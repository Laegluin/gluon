use os_pipe::{self, IntoStdio, PipeReader, PipeWriter};
use std::io;
use std::mem;
use std::process;
use std::sync::{Arc, Mutex};
use vm::api::IO;
use vm::thread::Thread;
use vm::{self, ExternModule};
use Compiler;

pub fn load(vm: &Thread) -> vm::Result<ExternModule> {
    vm.register_type::<GluonCommand>(stringify!(Command), &[])?;
    vm.register_type::<GluonChild>(stringify!(Child), &[])?;

    Compiler::new()
        .load_script(vm, "std.process.prim.types", PRIM_TYPES)
        .map_err(|err| err.to_string())?;

    let module = record! {
        cmd => primitive!(3 cmd),
        pipe => primitive!(1 pipe),
        join => primitive!(1 join),
        kill => primitive!(1 kill),
        exit_code_to_io => primitive!(1 exit_code_to_io),
        type Child => GluonChild,
        type Command => GluonCommand,
    };

    ExternModule::new(vm, module)
}

const PRIM_TYPES: &str = r#"
    type Stdio =
        | Pipe
        | Inherit
        | Null

    type CommandOption = 
        | Stdin Stdio
        | Stdout Stdio
        | Stderr Stdio
        | RedirectStdout
        | RedirectStderr
        | CurrentDir String
        | SetVars (Array (String, String))
        | RemoveVars (Array String)
        | ClearEnv

    { Stdio, CommandOption }
"#;

#[derive(Getable, VmType, Debug, PartialEq, Copy, Clone)]
#[gluon(vm_type = "std.process.prim.types.Stdio")]
enum Stdio {
    Pipe,
    Inherit,
    Null,
}

#[derive(Getable, VmType)]
#[gluon(vm_type = "std.process.prim.types.CommandOption")]
enum CommandOption {
    Stdin(Stdio),
    Stdout(Stdio),
    Stderr(Stdio),
    RedirectStdout,
    RedirectStderr,
    CurrentDir(String),
    SetVars(Vec<(String, String)>),
    RemoveVars(Vec<String>),
    ClearEnv,
}

#[derive(Userdata, Debug)]
struct GluonCommand(Arc<Command>);

#[derive(Default, Debug)]
struct Command {
    program: String,
    args: Vec<String>,
    stdin: Option<Stdio>,
    stdout: Option<Stdio>,
    stderr: Option<Stdio>,
    redirect_stdout: bool,
    redirect_stderr: bool,
    current_dir: Option<String>,
    set_vars: Vec<(String, String)>,
    remove_vars: Vec<String>,
    clear_env: bool,
}

#[derive(Userdata, Debug)]
struct GluonChild(Mutex<Child>);

#[derive(Debug)]
struct Child {
    stdin: Option<PipeWriter>,
    stdout: Option<PipeReader>,
    stderr: Option<PipeReader>,
    inner: process::Child,
}

type ExitCode = Option<i32>;

fn cmd(program: String, args: Vec<String>, opts: Vec<CommandOption>) -> GluonCommand {
    let mut cmd = Command {
        program,
        args,
        ..Command::default()
    };

    for opt in opts {
        match opt {
            CommandOption::Stdin(cfg) => cmd.stdin = Some(cfg),
            CommandOption::Stdout(cfg) => cmd.stdout = Some(cfg),
            CommandOption::Stderr(cfg) => cmd.stderr = Some(cfg),
            CommandOption::RedirectStdout => cmd.redirect_stdout = true,
            CommandOption::RedirectStderr => cmd.redirect_stderr = true,
            CommandOption::CurrentDir(dir) => cmd.current_dir = Some(dir),
            CommandOption::SetVars(vars) => cmd.set_vars = vars,
            CommandOption::RemoveVars(vars) => cmd.remove_vars = vars,
            CommandOption::ClearEnv => cmd.clear_env = true,
        }
    }

    GluonCommand(Arc::new(cmd))
}

fn pipe(commands: Vec<&GluonCommand>) -> IO<Vec<GluonChild>> {
    let num_commands = commands.len();
    let mut prev_stdout = None;

    commands
        .into_iter()
        .enumerate()
        .map(|(i, GluonCommand(cmd))| {
            // always pipe stdin and stdout if not set by the user, except for the first
            // and last processes, where stdin is set to inherit or stdout to inherit,
            // respectively
            let default_stdin = if i == 0 { Stdio::Inherit } else { Stdio::Pipe };
            let default_stdout = if i == (num_commands - 1) {
                Stdio::Inherit
            } else {
                Stdio::Pipe
            };

            let mut child = spawn_child(&cmd, prev_stdout.take(), default_stdin, default_stdout)?;
            prev_stdout = child.stdout.take();
            Ok(GluonChild(Mutex::new(child)))
        })
        .collect::<io::Result<Vec<_>>>()
        .into()
}

enum OutStream {
    Null,
    ParentStdout,
    ParentStderr,
    Pipe(PipeWriter),
}

impl OutStream {
    fn try_clone(&self) -> io::Result<OutStream> {
        match self {
            OutStream::Null => Ok(OutStream::Null),
            OutStream::ParentStdout => Ok(OutStream::ParentStdout),
            OutStream::ParentStderr => Ok(OutStream::ParentStderr),
            OutStream::Pipe(writer) => writer.try_clone().map(OutStream::Pipe),
        }
    }

    fn into_stdio(self) -> io::Result<process::Stdio> {
        match self {
            OutStream::Null => Ok(process::Stdio::null()),
            OutStream::ParentStdout => os_pipe::parent_stdout(),
            OutStream::ParentStderr => os_pipe::parent_stderr(),
            OutStream::Pipe(writer) => Ok(writer.into_stdio()),
        }
    }
}

fn join(GluonChild(process): &GluonChild) -> IO<ExitCode> {
    let mut process = process.lock().expect("Non-poisoned mutex");
    mem::drop(process.stdin.take());
    process.inner.wait().map(|exit| exit.code()).into()
}

fn kill(GluonChild(process): &GluonChild) -> IO<()> {
    process
        .lock()
        .expect("Non-poisoned mutex")
        .inner
        .kill()
        .into()
}

fn exit_code_to_io(code: ExitCode) -> IO<()> {
    match code {
        Some(code) => if code == 0 {
            IO::Value(())
        } else {
            IO::Exception(format!("process exited with code '{}'", code))
        },
        None => IO::Exception("process was terminated by signal".to_owned()),
    }
}

fn spawn_child(
    cmd: &Command,
    pipe: Option<PipeReader>,
    stdin_default: Stdio,
    stdout_default: Stdio,
) -> io::Result<Child> {
    let mut child = process::Command::new(&cmd.program);
    child.args(&cmd.args);

    // set standard config
    for (name, val) in &cmd.set_vars {
        child.env(name, val);
    }

    for var in &cmd.remove_vars {
        child.env_remove(var);
    }

    if cmd.clear_env {
        child.env_clear();
    }

    if let Some(dir) = &cmd.current_dir {
        child.current_dir(dir);
    }

    // configure the std streams
    let (stdin_read, stdin_write) = match cmd.stdin.unwrap_or(stdin_default) {
        Stdio::Null => (process::Stdio::null(), None),
        Stdio::Inherit => (os_pipe::parent_stdin()?, None),
        // if the stdin is piped, prefer the passed pipe handle over creating a new one
        Stdio::Pipe => match pipe {
            Some(pipe) => (pipe.into_stdio(), None),
            None => {
                let (reader, writer) = os_pipe::pipe()?;
                (reader.into_stdio(), Some(writer))
            }
        },
    };

    let (stdout_read, stdout_write) = match cmd.stdout.unwrap_or(stdout_default) {
        Stdio::Null => (None, OutStream::Null),
        Stdio::Inherit => (None, OutStream::ParentStdout),
        Stdio::Pipe => {
            let (reader, writer) = os_pipe::pipe()?;
            (Some(reader), OutStream::Pipe(writer))
        }
    };

    let (stderr_read, stderr_write) = match cmd.stderr.unwrap_or(Stdio::Inherit) {
        Stdio::Null => (None, OutStream::Null),
        Stdio::Inherit => (None, OutStream::ParentStderr),
        Stdio::Pipe => {
            let (reader, writer) = os_pipe::pipe()?;
            (Some(reader), OutStream::Pipe(writer))
        }
    };

    // if both stdout and stderr are redirected, simply flip the handles
    if cmd.redirect_stdout && cmd.redirect_stderr {
        child
            .stdout(stderr_write.into_stdio()?)
            .stderr(stdout_write.into_stdio()?);
    // otherwise duplicate the handle of the one being redirected to
    } else if cmd.redirect_stdout {
        child
            .stdout(stderr_write.try_clone()?.into_stdio()?)
            .stderr(stderr_write.into_stdio()?);
    } else if cmd.redirect_stderr {
        child
            .stdout(stdout_write.try_clone()?.into_stdio()?)
            .stderr(stdout_write.into_stdio()?);
    // default simply gets the correct handles
    } else {
        child
            .stdout(stdout_write.into_stdio()?)
            .stderr(stderr_write.into_stdio()?);
    }

    child.stdin(stdin_read).spawn().map(|child| Child {
        stdin: stdin_write,
        stdout: stdout_read,
        stderr: stderr_read,
        inner: child,
    })
}
