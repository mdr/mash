//! The browser's platform and shell, natively, with a fake bridge standing in for the page.

use std::cell::Cell;
use std::rc::Rc;

use mash_core::eval::{Ctx, RunError, with_big_stack};
use mash_web::bridge::FakeBridge;
use mash_web::platform::WebPlatform;
use mash_web::shell::Shell;

fn eval(bridge: &Rc<FakeBridge>, source: &str) -> Result<String, String> {
    let platform = Rc::new(WebPlatform::new(bridge.clone(), Rc::new(Cell::new((30, 100)))));
    let mut ctx = Ctx::new(platform);
    match ctx.run("test", source) {
        Ok(value) => Ok(ctx.stringify(&value).unwrap()),
        Err(RunError::Eval(e)) => Err(e.message),
        Err(e) => Err(e.to_string()),
    }
}

fn bridge() -> Rc<FakeBridge> {
    Rc::new(FakeBridge {
        now: Cell::new(1_791_547_200_000.0),
        ..FakeBridge::default()
    })
}

#[test]
fn the_home_directory_has_the_sample_files() {
    with_big_stack(|| {
        let bridge = bridge();
        assert_eq!(eval(&bridge, "pwd").unwrap(), "/home/guest");
        assert_eq!(eval(&bridge, "ls | map (_.path)").unwrap(), "[README.md, examples]");
        assert_eq!(
            eval(&bridge, "ls 'examples' | map (p => [p.name, p.owner, p.group])").unwrap(),
            "[[bookshop.json, guest, guest], [fib.mash, guest, guest], [planets.json, guest, guest], [sales.csv, guest, guest]]"
        );
        assert_eq!(
            eval(&bridge, "mash.source 'examples/fib.mash'; fibs 10").unwrap(),
            "[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]"
        );
        assert_eq!(
            eval(
                &bridge,
                "json.read 'examples/planets.json' | where (_.rings) | map (_.name)"
            )
            .unwrap(),
            "[Jupiter, Saturn, Uranus, Neptune]"
        );
        // The bookshop's nested data, as the README explores it
        assert_eq!(
            eval(
                &bridge,
                "shop = json.read 'examples/bookshop.json'; \
                 sold = shop.orders | flatMap (o => o.items | map (i => i + { order: o.id })); \
                 totals = (sold.innerJoin shop.books (s b => s.book == b.id) | groupBy (_.order)) \
                   .transformValues (_.sumBy (x => x.quantity * x.pricePence)); \
                 [shop.books | where (_.stock.Edinburgh == 0) | map (_.title), totals.values | max]"
            )
            .unwrap(),
            "[[Emma, Moby-Dick, The Picture of Dorian Gray], 3097]"
        );
        // Files can be written (in memory), and are timestamped by the bridge's clock
        assert_eq!(
            eval(
                &bridge,
                "write 'new.txt' 'hi'; [read 'new.txt', (ls | where (_.path == 'new.txt') | first).lastModified]"
            )
            .unwrap(),
            "[hi, 2026-10-09T12:00:00Z]"
        );
    });
}

#[test]
fn the_system_has_one_user() {
    with_big_stack(|| {
        let bridge = bridge();
        assert_eq!(
            eval(
                &bridge,
                "[user.name, users | map (_.name), groups | map (_.name), env.HOME]"
            )
            .unwrap(),
            "[guest, [guest], [guest], /home/guest]"
        );
        assert_eq!(eval(&bridge, "terminal").unwrap(), "{ rows: 30, columns: 100 }");
    });
}

#[test]
fn what_the_browser_cant_do_fails_cleanly() {
    with_big_stack(|| {
        let bridge = bridge();
        assert_eq!(
            eval(&bridge, "run 'ls'").unwrap_err(),
            "Could not run 'ls': running programs is not possible in the browser"
        );
        assert!(
            eval(&bridge, "ps")
                .unwrap_err()
                .contains("listing processes is not possible in the browser")
        );
        assert!(eval(&bridge, "clipboard").unwrap_err().contains("paste with Ctrl-V"));
        assert!(
            eval(&bridge, "open 'README.md'")
                .unwrap_err()
                .contains("only web addresses")
        );
    });
}

#[test]
fn the_page_handles_output_http_the_clipboard_and_urls() {
    with_big_stack(|| {
        let bridge = bridge();
        eval(
            &bridge,
            "print 'a\nb'; 'copied' | clipboard; open 'https://example.com'",
        )
        .unwrap();
        assert_eq!(*bridge.output.borrow(), "a\r\nb\r\n");
        assert_eq!(bridge.clipboard.borrow().as_deref(), Some("copied"));
        assert_eq!(*bridge.opened.borrow(), ["https://example.com"]);
        assert_eq!(eval(&bridge, "(http.get 'https://example.com/x').body").unwrap(), "ok");
        assert_eq!(*bridge.requests.borrow(), ["GET https://example.com/x"]);
    });
}

#[test]
fn time_is_in_the_pages_time_zone() {
    with_big_stack(|| {
        let bridge = Rc::new(FakeBridge {
            time_zone: Some("Asia/Tokyo".into()),
            ..FakeBridge::default()
        });
        assert_eq!(eval(&bridge, "now.hour").unwrap(), "9");
        let unknown = Rc::new(FakeBridge {
            time_zone: Some("Nowhere/Special".into()),
            ..FakeBridge::default()
        });
        assert_eq!(eval(&unknown, "now.hour").unwrap(), "0");
    });
}

#[test]
fn the_page_can_interrupt_evaluation() {
    with_big_stack(|| {
        let bridge = bridge();
        bridge.interrupt.set(true);
        let error = eval(&bridge, "1.to 1000000 | map (n => n + 1) | sum").unwrap_err();
        assert!(error.contains("nterrupted"), "{error}");
    });
}

fn type_line(shell: &mut Shell, line: &str) {
    for c in line.chars() {
        assert!(shell.key(&c.to_string(), "", false, false, false, false));
    }
    shell.key("Enter", "Enter", false, false, false, false);
}

#[test]
fn an_interactive_session() {
    with_big_stack(|| {
        let bridge = bridge();
        let earlier = r#"{"sessionId":"e5a6c37d-6bd9-489e-a1f9-4cc6b52a2644","commandNumber":1,"timestamp":"2026-10-08T11:39:04Z","command":"earlier","mish":false,"workingDirectory":"/home/guest"}"#;
        let mut shell = Shell::new(bridge.clone(), 24, 80, &[earlier.to_string(), "not json".into()]);
        shell
            .repl()
            .ctx()
            .run("test", "config.cli.showStartupTips = false")
            .unwrap();
        shell.start();
        assert!(bridge.output.borrow().contains(" $ "), "{:?}", bridge.output.borrow());
        bridge.output.borrow_mut().clear();
        type_line(&mut shell, "40 + 2");
        assert!(bridge.output.borrow().contains("42"), "{:?}", bridge.output.borrow());
        assert_eq!(*bridge.running.borrow(), [true, false]);
        let saved = bridge.history.borrow();
        assert_eq!(saved.len(), 1);
        assert!(saved[0].contains(r#""command":"40 + 2""#), "{}", saved[0]);
        drop(saved);
        // The page's history is there to recall
        bridge.output.borrow_mut().clear();
        shell.key("ArrowUp", "ArrowUp", false, false, false, false);
        shell.key("ArrowUp", "ArrowUp", false, false, false, false);
        assert!(
            bridge.output.borrow().contains("earlier"),
            "{:?}",
            bridge.output.borrow()
        );
        // Ctrl-D on an empty line leaves
        shell.key("c", "KeyC", true, false, false, false);
        assert!(!shell.key("d", "KeyD", true, false, false, false));
        assert!(!shell.paste("more"));
    });
}

#[test]
fn output_while_a_command_runs_gets_carriage_returns() {
    with_big_stack(|| {
        let bridge = bridge();
        let mut shell = Shell::new(bridge.clone(), 24, 80, &[]);
        shell
            .repl()
            .ctx()
            .run("test", "config.cli.showStartupTips = false")
            .unwrap();
        shell.start();
        bridge.output.borrow_mut().clear();
        type_line(&mut shell, r#"print "x`ny""#);
        assert!(
            bridge.output.borrow().contains("x\r\ny\r\n"),
            "{:?}",
            bridge.output.borrow()
        );
    });
}

fn quiet_shell(bridge: &Rc<FakeBridge>, history: &[String]) -> Shell {
    let mut shell = Shell::new(bridge.clone(), 24, 80, history);
    shell
        .repl()
        .ctx()
        .run("test", "config.cli.showStartupTips = false")
        .unwrap();
    shell.set_page_url("https://mash-shell.org/try/#run=old");
    shell.start();
    shell
}

#[test]
fn share_makes_a_link_to_this_sessions_commands() {
    with_big_stack(|| {
        let bridge = bridge();
        let earlier = r#"{"sessionId":"e5a6c37d-6bd9-489e-a1f9-4cc6b52a2644","commandNumber":1,"timestamp":"2026-10-08T11:39:04Z","command":"earlier","mish":false,"workingDirectory":"/home/guest"}"#;
        let mut shell = quiet_shell(&bridge, &[earlier.to_string()]);
        assert!(shell.run_command("x = 6 * 7"));
        assert!(shell.run_command("json.read \"examples/planets.json\" | where (_.rings) | map (_.name)"));
        assert!(shell.run_command("{\n  a: 1 & 2\n}"));
        bridge.output.borrow_mut().clear();
        assert!(shell.run_command("share"));
        let all = "https://mash-shell.org/try/#run=x%20%3D%206%20*%207\
                   &run=json.read%20%22examples/planets.json%22%20%7C%20where%20(_.rings)%20%7C%20map%20(_.name)\
                   &run=%7B%0A%20%20a:%201%20%26%202%0A%7D";
        assert!(bridge.output.borrow().contains(all), "{:?}", bridge.output.borrow());
        assert_eq!(bridge.clipboard.borrow().as_deref(), Some(all));
        // The last n (the earlier `share` is a command of the session too)
        let ctx = shell.repl().ctx();
        let link = ctx.run("test", "share 2").unwrap();
        assert_eq!(
            ctx.stringify(&link).unwrap(),
            "https://mash-shell.org/try/#run=%7B%0A%20%20a:%201%20%26%202%0A%7D&run=share"
        );
    });
}

#[test]
fn run_command_runs_as_if_typed_and_waits_for_the_object_browser() {
    with_big_stack(|| {
        let bridge = bridge();
        let mut shell = quiet_shell(&bridge, &[]);
        assert!(shell.at_prompt());
        bridge.output.borrow_mut().clear();
        assert!(shell.run_command("40 + 2"));
        assert!(bridge.output.borrow().contains("42"), "{:?}", bridge.output.borrow());
        assert_eq!(bridge.history.borrow().len(), 1);
        assert!(shell.at_prompt());
        // Output too big for the screen opens the object browser; the REPL waits until it's closed
        assert!(shell.run_command("1.to 200 | map (n => { n: n, square: n * n })"));
        assert!(shell.in_object_browser());
        assert!(!shell.at_prompt());
        shell.key("q", "KeyQ", false, false, false, false);
        assert!(shell.at_prompt());
        // Something typed means it isn't waiting for a command
        shell.key("x", "KeyX", false, false, false, false);
        assert!(!shell.at_prompt());
    });
}
