//! The `xml` namespace: XML as Mash objects, using the shape rules of org.json's `XML.toJSONObject` (which
//! Scala Mash used): an element becomes a field named after it; attributes and child elements become fields of
//! its object, and its text a `content` field; an element with only text is just that text, and an empty one
//! the empty string; repeated elements become a list. Unlike org.json, text and attribute values stay strings
//! (no conversion of `42` or `true`). Reading files (`xml.read`) needs the OS and lives in `mash-os`.

use mash_macros::builtin;
use quick_xml::Reader;
use quick_xml::events::{BytesStart, Event};

use crate::eval::{MashError, R};
use crate::function::BuiltinFunction;
use crate::value::{Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&FromStringFunction];

/// Read the given string and parse it as XML
#[builtin(namespace = "xml")]
pub fn from_string(#[param("String to parse as XML")] string: Str) -> R<Object> {
    parse_xml(string.as_str())
}

fn invalid(e: impl std::fmt::Display) -> MashError {
    MashError::new(format!("Invalid XML: {e}"))
}

/// org.json's `JSONObject.accumulate`: a repeated field becomes a list of the values
fn accumulate(object: &Object, field: &str, value: Value) {
    match object.get_str(field) {
        None => object.set_str(field, value),
        Some(Value::List(existing)) => existing.push(value),
        Some(existing) => object.set_str(field, Value::list(vec![existing, value])),
    }
}

/// An element being read: its name, and the object of its attributes, text and children
struct Element {
    name: String,
    object: Object,
}

impl Element {
    fn start(e: &BytesStart) -> R<Element> {
        let name = String::from_utf8_lossy(e.name().as_ref()).into_owned();
        let object = Object::new();
        for attribute in e.attributes() {
            let attribute = attribute.map_err(invalid)?;
            let key = String::from_utf8_lossy(attribute.key.as_ref()).into_owned();
            let value = attribute.unescape_value().map_err(invalid)?;
            accumulate(&object, &key, Value::str(&value));
        }
        Ok(Element { name, object })
    }

    /// The value this element contributes to its parent
    fn finish(self) -> (String, Value) {
        let value = match self.object.len() {
            0 => Value::str(""),
            1 => match self.object.get_str("content") {
                Some(content) => content,
                None => Value::Object(self.object),
            },
            _ => Value::Object(self.object),
        };
        (self.name, value)
    }
}

pub(crate) fn parse_xml(text: &str) -> R<Object> {
    let mut reader = Reader::from_str(text);
    reader.config_mut().trim_text(false);
    let root = Object::new();
    let mut stack: Vec<Element> = vec![];
    loop {
        let event = reader.read_event().map_err(invalid)?;
        match event {
            Event::Start(e) => stack.push(Element::start(&e)?),
            Event::Empty(e) => {
                let (name, value) = Element::start(&e)?.finish();
                let parent = stack.last().map_or(&root, |p| &p.object);
                accumulate(parent, &name, value);
            }
            Event::End(_) => {
                let element = stack.pop().ok_or_else(|| invalid("unexpected end tag"))?;
                let (name, value) = element.finish();
                let parent = stack.last().map_or(&root, |p| &p.object);
                accumulate(parent, &name, value);
            }
            Event::Text(e) => {
                let text = e.unescape().map_err(invalid)?;
                let text = text.trim();
                if !text.is_empty() {
                    match stack.last() {
                        Some(element) => accumulate(&element.object, "content", Value::str(text)),
                        None => return Err(invalid("text outside the root element")),
                    }
                }
            }
            Event::CData(e) => {
                let text = String::from_utf8_lossy(&e.into_inner()).into_owned();
                if let Some(element) = stack.last()
                    && !text.is_empty()
                {
                    accumulate(&element.object, "content", Value::Str(Str::new(text)));
                }
            }
            Event::Eof => break,
            Event::Comment(_) | Event::Decl(_) | Event::PI(_) | Event::DocType(_) => {}
        }
    }
    if let Some(element) = stack.last() {
        return Err(invalid(format!("unclosed element <{}>", element.name)));
    }
    Ok(root)
}
