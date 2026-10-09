use std::fmt;

/// A Mash number. Opaque, so that an integer/float split can be added later without touching callers.
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct Number(f64);

impl Number {
    pub const ZERO: Number = Number(0.0);

    pub fn new(n: f64) -> Self {
        Number(n)
    }

    pub fn as_f64(self) -> f64 {
        self.0
    }

    /// The value as an integer, if it is a whole number in range.
    pub fn as_i64(self) -> Option<i64> {
        let n = self.0;
        // i64::MAX as f64 rounds up to 2^63, which is out of range
        (n.fract() == 0.0 && n >= i64::MIN as f64 && n < i64::MAX as f64).then_some(n as i64)
    }

    pub fn is_integer(self) -> bool {
        self.as_i64().is_some()
    }

    /// Bits used for hashing and equality when a number is an object key: `-0.0` is the same key as `0.0`,
    /// and all NaNs are one key.
    pub(crate) fn key_bits(self) -> u64 {
        if self.0.is_nan() {
            f64::NAN.to_bits()
        } else if self.0 == 0.0 {
            0
        } else {
            self.0.to_bits()
        }
    }

    /// Total order (as used for sorting): -0.0 sorts before 0.0, NaN after everything.
    pub fn total_cmp(self, other: Number) -> std::cmp::Ordering {
        self.0.total_cmp(&other.0)
    }

    /// Parse as `parseNumber`/`toNumber` do: surrounding whitespace is ignored; `Infinity`, `-Infinity` and
    /// `NaN` are accepted.
    pub fn parse(s: &str) -> Option<Number> {
        let s = s.trim();
        match s {
            "Infinity" | "+Infinity" => Some(Number(f64::INFINITY)),
            "-Infinity" => Some(Number(f64::NEG_INFINITY)),
            "NaN" => Some(Number(f64::NAN)),
            // Rust also accepts "inf", "infinity" and "nan" in any case; Mash doesn't
            _ if s.chars().any(|c| c.is_ascii_alphabetic() && !matches!(c, 'e' | 'E')) => None,
            _ => s.parse().ok().map(Number),
        }
    }
}

impl From<f64> for Number {
    fn from(n: f64) -> Self {
        Number(n)
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Number(n as f64)
    }
}

impl From<usize> for Number {
    fn from(n: usize) -> Self {
        Number(n as f64)
    }
}

/// Shortest representation that round-trips; whole numbers have no decimal point; infinities are spelled
/// `Infinity`.
impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_infinite() {
            f.write_str(if self.0 > 0.0 { "Infinity" } else { "-Infinity" })
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display() {
        let cases = [
            (1.0, "1"),
            (-0.0, "-0"),
            (0.5, "0.5"),
            (0.1 + 0.2, "0.30000000000000004"),
            (1e21, "1000000000000000000000"),
            (f64::INFINITY, "Infinity"),
            (f64::NEG_INFINITY, "-Infinity"),
            (f64::NAN, "NaN"),
        ];
        for (n, expected) in cases {
            assert_eq!(Number(n).to_string(), expected);
        }
    }

    #[test]
    fn parse() {
        assert_eq!(Number::parse(" 42 ").map(Number::as_f64), Some(42.0));
        assert_eq!(Number::parse("1e3").map(Number::as_f64), Some(1000.0));
        assert_eq!(Number::parse("-Infinity").map(Number::as_f64), Some(f64::NEG_INFINITY));
        assert!(Number::parse("NaN").is_some_and(|n| n.as_f64().is_nan()));
        assert_eq!(Number::parse("inf"), None);
        assert_eq!(Number::parse("1d"), None);
        assert_eq!(Number::parse("abc"), None);
        assert_eq!(Number::parse(""), None);
    }

    #[test]
    fn as_i64() {
        assert_eq!(Number(3.0).as_i64(), Some(3));
        assert_eq!(Number(-3.0).as_i64(), Some(-3));
        assert_eq!(Number(3.5).as_i64(), None);
        assert_eq!(Number(f64::NAN).as_i64(), None);
        assert_eq!(Number(f64::INFINITY).as_i64(), None);
        assert_eq!(Number(1e300).as_i64(), None);
        assert_eq!(Number(5_000_000_000.0).as_i64(), Some(5_000_000_000));
    }
}
