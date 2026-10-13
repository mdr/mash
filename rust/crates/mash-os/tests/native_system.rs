//! The native platform's users, groups, processes and disks, against the real OS. These only read, except for
//! killing a child process the test starts itself; the clipboard, `open` and the terminal aren't exercised.

use std::os::unix::process::ExitStatusExt;
use std::process::{Command, Stdio};
use std::rc::Rc;

use mash_core::eval::{Ctx, render, with_big_stack};
use mash_core::platform::{Platform, Signal};
use mash_os::NativePlatform;

fn eval(code: &str) -> String {
    let code = code.to_string();
    with_big_stack(move || {
        let mut ctx = Ctx::new(Rc::new(NativePlatform));
        match ctx.run("test", &code) {
            Ok(value) => render(&value),
            Err(e) => format!("error: {e}"),
        }
    })
}

#[test]
fn current_user_is_a_user() {
    let platform = NativePlatform;
    let name = platform.current_user().expect("current user");
    let users = platform.users().expect("users");
    let me = users.iter().find(|u| u.name == name).expect("current user is listed");
    assert_eq!(me.uid, nix::unistd::getuid().as_raw());
    assert_eq!(me.gid, nix::unistd::getgid().as_raw());
    let groups = platform.groups().expect("groups");
    assert!(groups.iter().any(|g| g.gid == me.gid), "primary group is listed");
    assert_eq!(eval("user.name"), name);
    assert_eq!(eval("user.uid"), me.uid.to_string());
    assert_eq!(eval("user.name.uid.name"), name);
    assert_eq!(eval("user.home"), me.home);
    assert_eq!(eval("(users | where (.name == user.name) | length)"), "1");
    assert_eq!(eval("user.groups.first == user.primaryGroup"), "true");
    assert_eq!(eval("user.primaryGroup.gid"), me.gid.to_string());
}

#[test]
fn own_process_is_listed() {
    let platform = NativePlatform;
    let pid = std::process::id();
    let processes = platform.processes().expect("processes");
    let me = processes.iter().find(|p| p.pid == pid).expect("own process is listed");
    assert_eq!(me.parent_pid, Some(std::os::unix::process::parent_id()));
    assert_eq!(me.owner, Some(platform.current_user().expect("current user")));
    assert!(me.resident_size > 0);
    assert_eq!(platform.process(pid).expect("process").map(|p| p.pid), Some(pid));
    assert_eq!(
        eval(&format!("(ps | where (.pid == {pid})).first.pid")),
        pid.to_string()
    );
    assert_eq!(
        eval(&format!("(ps | where (.pid == {pid})).first.parent.pid")),
        std::os::unix::process::parent_id().to_string()
    );
    assert_eq!(
        eval(&format!("(ps | where (.pid == {pid})).first.owner == user.name")),
        "true"
    );
}

#[test]
fn disk_space_includes_the_root() {
    let disks = NativePlatform.disk_space().expect("disk space");
    let root = disks.iter().find(|d| d.mount_point == "/").expect("/ is mounted");
    assert!(root.total > 0);
    assert!(root.used <= root.total && root.available <= root.total);
    assert_eq!(eval("(df | where (.mountPoint == '/')).length"), "1");
}

#[test]
fn kill_signals_a_child() {
    let mut child = Command::new("sleep")
        .arg("30")
        .stdin(Stdio::null())
        .spawn()
        .expect("sleep starts");
    let pid = child.id();
    assert_eq!(eval(&format!("ps | where (.pid == {pid}) | .name")), "[sleep]");
    assert_eq!(eval(&format!("kill --signal='USR1' {pid}")), "");
    let status = child.wait().expect("child exits");
    assert_eq!(status.signal(), Some(nix::sys::signal::Signal::SIGUSR1 as i32));
}

#[test]
fn kill_by_number_and_errors() {
    let mut child = Command::new("sleep")
        .arg("30")
        .stdin(Stdio::null())
        .spawn()
        .expect("sleep starts");
    let pid = child.id();
    // Invalid signals are rejected (while the child is still ours, so no other process can be signalled)
    assert!(NativePlatform.kill(pid, &Signal::Number(12345)).is_err());
    assert!(NativePlatform.kill(pid, &Signal::Named("NOPE".into())).is_err());
    // Names may have SIG, as `kill -s` accepts
    assert!(NativePlatform.kill(pid, &Signal::Named("NOPE".into())).is_err());
    assert_eq!(eval(&format!("kill --signal='SIGCONT' {pid}; 0")), "0");
    // Signal 0 checks the process exists, sending nothing
    NativePlatform.kill(pid, &Signal::Number(0)).expect("signal 0");
    assert!(child.try_wait().expect("child").is_none(), "still running");
    NativePlatform
        .kill(pid, &Signal::Number(nix::sys::signal::Signal::SIGTERM as i32))
        .expect("kill");
    assert_eq!(
        child.wait().expect("child exits").signal(),
        Some(nix::sys::signal::Signal::SIGTERM as i32)
    );
}
