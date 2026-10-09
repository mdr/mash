//! The JavaScript interface (wasm only): [`WebShell`] for the page's worker, [`evaluate`] for running code
//! without a terminal, and the bridge object the worker supplies.

use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use js_sys::{Reflect, Uint8Array};
use mash_core::eval::{Ctx, RunError};
use mash_core::platform::{HttpRequest, HttpResponse};
use mash_core::value::Value;
use wasm_bindgen::prelude::*;

use crate::bridge::Bridge;
use crate::platform::WebPlatform;
use crate::shell::Shell;

#[wasm_bindgen]
extern "C" {
    /// The worker's side of [`Bridge`] (see `www/worker.js`)
    pub type JsBridge;

    #[wasm_bindgen(method)]
    fn write(this: &JsBridge, text: &str);
    #[wasm_bindgen(method, js_name = nowMillis)]
    fn now_millis(this: &JsBridge) -> f64;
    #[wasm_bindgen(method, js_name = timeZone)]
    fn time_zone(this: &JsBridge) -> Option<String>;
    #[wasm_bindgen(method, js_name = randomU32)]
    fn random_u32(this: &JsBridge) -> u32;
    #[wasm_bindgen(method, js_name = interruptRequested)]
    fn interrupt_requested(this: &JsBridge) -> bool;
    #[wasm_bindgen(method)]
    fn sleep(this: &JsBridge, millis: f64);
    #[wasm_bindgen(method, js_name = commandRunning)]
    fn command_running(this: &JsBridge, running: bool);
    #[wasm_bindgen(method, js_name = saveHistory)]
    fn save_history(this: &JsBridge, entry: &str);
    #[wasm_bindgen(method, catch, js_name = setClipboard)]
    fn set_clipboard(this: &JsBridge, text: &str) -> Result<(), JsValue>;
    #[wasm_bindgen(method, catch, js_name = openUrl)]
    fn open_url(this: &JsBridge, url: &str) -> Result<(), JsValue>;
    /// Returns `{ status, headers: [name, value, name, value, …], body: Uint8Array }`
    #[wasm_bindgen(method, catch, js_name = httpRequest)]
    fn http_request(
        this: &JsBridge,
        method: &str,
        url: &str,
        headers: Vec<String>,
        body: Option<Vec<u8>>,
    ) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(js_namespace = console, js_name = error)]
    fn console_error(message: &str);
}

/// [`JsBridge`] as a [`Bridge`]
struct JsBridgeAdapter(JsBridge);

impl std::fmt::Debug for JsBridgeAdapter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("JsBridge")
    }
}

/// An exception's message
fn message(error: &JsValue) -> String {
    error
        .dyn_ref::<js_sys::Error>()
        .map(|e| String::from(e.message()))
        .or_else(|| error.as_string())
        .unwrap_or_else(|| format!("{error:?}"))
}

fn get(object: &JsValue, key: &str) -> Result<JsValue, JsValue> {
    Reflect::get(object, &JsValue::from_str(key))
}

impl Bridge for JsBridgeAdapter {
    fn write(&self, text: &str) {
        self.0.write(text);
    }

    fn now_millis(&self) -> f64 {
        self.0.now_millis()
    }

    fn time_zone(&self) -> Option<String> {
        self.0.time_zone()
    }

    fn random_u32(&self) -> u32 {
        self.0.random_u32()
    }

    fn interrupt_requested(&self) -> bool {
        self.0.interrupt_requested()
    }

    fn sleep(&self, millis: f64) {
        self.0.sleep(millis);
    }

    fn command_running(&self, running: bool) {
        self.0.command_running(running);
    }

    fn save_history(&self, entry: &str) {
        self.0.save_history(entry);
    }

    fn set_clipboard(&self, text: &str) -> io::Result<()> {
        self.0.set_clipboard(text).map_err(|e| io::Error::other(message(&e)))
    }

    fn open_url(&self, url: &str) -> io::Result<()> {
        self.0.open_url(url).map_err(|e| io::Error::other(message(&e)))
    }

    fn http_request(&self, request: &HttpRequest<'_>) -> io::Result<HttpResponse> {
        let headers = request
            .all_headers()
            .into_iter()
            .flat_map(|(name, value)| [name, value])
            .collect();
        let body = request.body.map(<[u8]>::to_vec);
        let response = self
            .0
            .http_request(request.method, request.url, headers, body)
            .map_err(|e| io::Error::other(message(&e)))?;
        let parse = || -> Result<HttpResponse, JsValue> {
            let status = get(&response, "status")?.as_f64().unwrap_or(0.0) as u16;
            let flat: Vec<String> = js_sys::Array::from(&get(&response, "headers")?)
                .iter()
                .map(|v| v.as_string().unwrap_or_default())
                .collect();
            let headers = flat
                .chunks(2)
                .filter_map(|pair| match pair {
                    [name, value] => Some((name.clone(), value.clone())),
                    _ => None,
                })
                .collect();
            let body = Uint8Array::new(&get(&response, "body")?).to_vec();
            Ok(HttpResponse {
                status,
                headers,
                cookies: vec![],
                body,
            })
        };
        parse().map_err(|e| io::Error::other(format!("unexpected response from the page: {}", message(&e))))
    }
}

thread_local! {
    static LAST_PANIC: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Report panics on the console, and keep the message for [`last_panic`]: a panic aborts, which the worker
/// sees as an exception without the message
fn install_panic_hook() {
    std::panic::set_hook(Box::new(|info| {
        let message = info.to_string();
        console_error(&message);
        LAST_PANIC.with(|p| *p.borrow_mut() = Some(message));
    }));
}

/// The message of the panic that stopped Mash, if it panicked
#[wasm_bindgen(js_name = lastPanic)]
pub fn last_panic() -> Option<String> {
    LAST_PANIC.with(|p| p.borrow().clone())
}

/// An interactive session, driven by the page's key presses
#[wasm_bindgen]
#[derive(Debug)]
pub struct WebShell(Shell);

#[wasm_bindgen]
impl WebShell {
    /// A session in a terminal of `rows` and `columns`, with the history entries the page kept (JSON strings)
    #[wasm_bindgen(constructor)]
    pub fn new(bridge: JsBridge, rows: u16, columns: u16, history: Vec<String>) -> WebShell {
        install_panic_hook();
        WebShell(Shell::new(Rc::new(JsBridgeAdapter(bridge)), rows, columns, &history))
    }

    pub fn start(&mut self) {
        self.0.start();
    }

    /// Handle a `keydown` event. Returns false once the user has left the shell.
    pub fn key(&mut self, key: &str, code: &str, ctrl: bool, alt: bool, shift: bool, meta: bool) -> bool {
        self.0.key(key, code, ctrl, alt, shift, meta)
    }

    pub fn paste(&mut self, text: &str) -> bool {
        self.0.paste(text)
    }

    pub fn resize(&mut self, rows: u16, columns: u16) -> bool {
        self.0.resize(rows, columns)
    }
}

/// Evaluate Mash code in a new interpreter, returning the result as Mash's `toString` renders it, or the error
/// (as reported in the shell)
#[wasm_bindgen]
pub fn evaluate(bridge: JsBridge, source: &str) -> Result<String, String> {
    install_panic_hook();
    let size = Rc::new(std::cell::Cell::new((24, 80)));
    let platform = Rc::new(WebPlatform::new(Rc::new(JsBridgeAdapter(bridge)), size));
    let mut ctx = Ctx::new(platform);
    let value = ctx.run("<evaluate>", source).map_err(|error| match error {
        RunError::Syntax(_) => format!("syntax error: {error}"),
        RunError::Eval(error) => error.report(),
    })?;
    if matches!(value, Value::Unit) {
        return Ok(String::new());
    }
    ctx.stringify(&value).map_err(|error| error.report())
}
