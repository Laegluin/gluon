use os_pipe::{self, PipeReader, PipeWriter};
use std::io::{self, BufRead, BufReader, ErrorKind, Read, Write};
use std::mem;
use std::process;
use std::sync::Mutex;
use vm::api::IO;
use vm::thread::Thread;
use vm::{self, ExternModule};

pub fn load(vm: &Thread) -> vm::Result<ExternModule> {
    vm.register_type::<StdStreamWriter>(stringify!(StdStreamWriter), &[])?;
    vm.register_type::<StdStreamReader>(stringify!(StdStreamReader), &[])?;
    vm.register_type::<Handle>(stringify!(Handle), &[])?;

    let module = record! {
        command => primitive!(3, std::process::prim::command),
        pipe => primitive!(1, std::process::prim::pipe),
        join => primitive!(2, std::process::prim::join),
        kill => primitive!(1, std::process::prim::kill),
        success_or_exception => primitive!(1, std::process::prim::success_or_exception),
        is_writer_available => primitive!(1, std::process::prim::is_writer_available),
        is_reader_available => primitive!(1, std::process::prim::is_reader_available),
        read => primitive!(1, std::process::prim::read),
        read_to_end => primitive!(1, std::process::prim::read_to_end),
        read_line => primitive!(1, std::process::prim::read_line),
        write => primitive!(2, std::process::prim::write),
        write_all => primitive!(2, std::process::prim::write_all),
        flush => primitive!(1, std::process::prim::flush),
        type Child => Child,
        type Command => Command,
        type StdStreamWriter => StdStreamWriter,
        type StdStreamReader => StdStreamReader,
        type Handle => Handle,
    };

    ExternModule::new(vm, module)
}

mod std {
    pub mod process {
        pub use process as prim;
    }
}

#[derive(Default, Debug, VmType, Getable, Pushable)]
#[gluon(crate_name = "::vm")]
struct Command {
    program: String,
    args: Vec<String>,
    stdin: Option<Stdio>,
    stdout: Option<Stdio>,
    stderr: Option<Stdio>,
    redirect_stdout: bool,
    redirect_stderr: bool,
    current_dir: Option<String>,
    set_env: Vec<(String, String)>,
    remove_env: Vec<String>,
    clear_env: bool,
}

#[derive(Getable, VmType)]
#[gluon(vm_type = "std.process.types.CommandOption")]
#[gluon(crate_name = "::vm")]
enum CommandOption {
    Stdin(Stdio),
    Stdout(Stdio),
    Stderr(Stdio),
    RedirectStdout,
    RedirectStderr,
    CurrentDir(String),
    SetEnv(Vec<(String, String)>),
    RemoveEnv(Vec<String>),
    ClearEnv,
}

#[derive(VmType, Getable, Pushable, Debug, PartialEq, Copy, Clone)]
#[gluon(vm_type = "std.process.types.Stdio")]
#[gluon(crate_name = "::vm")]
enum Stdio {
    Pipe,
    Inherit,
    Null,
}

#[derive(VmType, Pushable)]
#[gluon(crate_name = "::vm")]
struct Child {
    stdin: StdStreamWriter,
    stdout: StdStreamReader,
    stderr: StdStreamReader,
    handle: Handle,
}

#[derive(Userdata, Debug)]
#[gluon(crate_name = "::vm")]
struct StdStreamWriter(Mutex<Option<PipeWriter>>);

#[derive(Userdata, Debug)]
#[gluon(crate_name = "::vm")]
struct StdStreamReader(Mutex<Option<BufReader<PipeReader>>>);

#[derive(Userdata, Debug)]
#[gluon(crate_name = "::vm")]
struct Handle(Mutex<process::Child>);

#[derive(VmType, Getable, Pushable)]
#[gluon(vm_type = "std.process.types.ExitCode")]
#[gluon(crate_name = "::vm")]
struct ExitCode(Option<i32>);

fn is_writer_available(writer: &StdStreamWriter) -> bool {
    writer.0.lock().expect("non-poisoned Mutex").is_some()
}

fn is_reader_available(reader: &StdStreamReader) -> bool {
    reader.0.lock().expect("non-poisoned Mutex").is_some()
}

fn write(writer: &StdStreamWriter, buf: &[u8]) -> IO<usize> {
    if let Some(ref mut writer) = *writer.0.lock().expect("non-poisoned Mutex") {
        writer.write(buf).into()
    } else {
        IO::Value(0)
    }
}

fn write_all(writer: &StdStreamWriter, buf: &[u8]) -> IO<()> {
    if let Some(ref mut writer) = *writer.0.lock().expect("non-poisoned Mutex") {
        writer.write_all(buf).into()
    } else {
        IO::Exception(format!("cannot write because a writer is not available",))
    }
}

fn flush(writer: &StdStreamWriter) -> IO<()> {
    if let Some(ref mut writer) = *writer.0.lock().expect("non-poisoned Mutex") {
        writer.flush().into()
    } else {
        IO::Value(())
    }
}

fn read(reader: &StdStreamReader) -> IO<Vec<u8>> {
    fn read_inner(reader: &StdStreamReader) -> io::Result<Vec<u8>> {
        let mut reader = reader.0.lock().expect("non-poisoned Mutex");

        if let Some(ref mut reader) = *reader {
            let mut buf = vec![0; 4096];
            let bytes_read = reader.read(&mut buf)?;
            buf.resize(bytes_read, 0);
            Ok(buf)
        } else {
            Ok(vec![])
        }
    }

    read_inner(reader).into()
}

fn read_to_end(reader: &StdStreamReader) -> IO<Vec<u8>> {
    let mut reader = reader.0.lock().expect("non-poisoned Mutex");

    if let Some(ref mut reader) = *reader {
        let mut buf = Vec::new();
        reader.read_to_end(&mut buf).map(|_| buf).into()
    } else {
        IO::Value(Vec::new())
    }
}

fn read_line(reader: &StdStreamReader) -> IO<Option<String>> {
    let mut reader = reader.0.lock().expect("non-poisoned Mutex");
    let mut buf = String::new();

    if let Some(ref mut reader) = *reader {
        match reader.read_line(&mut buf) {
            Ok(_) => IO::Value(Some(buf)),
            Err(err) => match err.kind() {
                ErrorKind::InvalidData => IO::Value(None),
                _ => IO::from(Err(err)),
            },
        }
    } else {
        IO::Value(Some(buf))
    }
}

fn command(program: String, args: Vec<String>, opts: Vec<CommandOption>) -> Command {
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
            CommandOption::SetEnv(vars) => cmd.set_env = vars,
            CommandOption::RemoveEnv(vars) => cmd.remove_env = vars,
            CommandOption::ClearEnv => cmd.clear_env = true,
        }
    }

    cmd
}

fn pipe(commands: Vec<Command>) -> IO<Vec<Child>> {
    fn pipe_inner(commands: Vec<Command>) -> io::Result<Vec<Child>> {
        let mut children: Vec<Child> = Vec::with_capacity(commands.len());

        for (i, command) in commands.iter().enumerate() {
            // always pipe stdin and stdout if not set by the user, except for the first
            // and last processes, where stdin is set to inherit or stdout to inherit,
            // respectively
            let default_stdin = if i == 0 { Stdio::Inherit } else { Stdio::Pipe };
            let default_stdout = if i == (commands.len() - 1) {
                Stdio::Inherit
            } else {
                Stdio::Pipe
            };

            // extract the stdout handle from the previous process if it is needed for piping
            // it to the new process' stdin
            let prev_stdout = if command.stdin.unwrap_or(default_stdin) == Stdio::Pipe && i > 0 {
                children.get_mut(i - 1).and_then(|child| {
                    child
                        .stdout
                        .0
                        .lock()
                        .expect("non-poisoned Mutex")
                        .take()
                        .map(BufReader::into_inner)
                })
            } else {
                None
            };

            let child = spawn_child(&command, prev_stdout, default_stdin, default_stdout)?;
            children.push(child);
        }

        Ok(children)
    }

    pipe_inner(commands).into()
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
            OutStream::ParentStdout => os_pipe::dup_stdout().map(|out| out.into()),
            OutStream::ParentStderr => os_pipe::dup_stderr().map(|err| err.into()),
            OutStream::Pipe(writer) => Ok(writer.into()),
        }
    }
}

fn join(stdin: &StdStreamWriter, handle: &Handle) -> IO<ExitCode> {
    mem::drop(stdin.0.lock().expect("non-poisoned Mutex").take());
    handle
        .0
        .lock()
        .expect("Non-poisoned mutex")
        .wait()
        .map(|exit| ExitCode(exit.code()))
        .into()
}

fn kill(handle: &Handle) -> IO<()> {
    handle.0.lock().expect("Non-poisoned mutex").kill().into()
}

fn success_or_exception(ExitCode(code): ExitCode) -> IO<()> {
    match code {
        Some(code) => if code == 0 {
            IO::Value(())
        } else {
            IO::Exception(format!("process exited with {}", code))
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

    // configure environment
    for (name, val) in &cmd.set_env {
        child.env(name, val);
    }

    for var in &cmd.remove_env {
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
        Stdio::Inherit => (os_pipe::dup_stdin()?.into(), None),
        // if the stdin is piped, prefer the passed pipe handle over creating a new one
        Stdio::Pipe => match pipe {
            Some(pipe) => (pipe.into(), None),
            None => {
                let (reader, writer) = os_pipe::pipe()?;
                (reader.into(), Some(writer))
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
        stdin: StdStreamWriter(Mutex::new(stdin_write)),
        stdout: StdStreamReader(Mutex::new(stdout_read.map(BufReader::new))),
        stderr: StdStreamReader(Mutex::new(stderr_read.map(BufReader::new))),
        handle: Handle(Mutex::new(child)),
    })
}
