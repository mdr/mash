//! The `maths` namespace.

use crate::builtins::constructor::FieldsConstructor;
use crate::infer::Type;
use mash_macros::builtin;

use super::core::OBJECT_CLASS;
use crate::eval::R;
use crate::function::{Args, BuiltinFunction};
use crate::value::{BuiltinClass, ClassRef, Field, Number, Object, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&LogFunction, &SquareRootFunction, &StatsFunction];

static STATS_NEW: FieldsConstructor =
    FieldsConstructor::new("maths.Stats", &STATS_CLASS, "Construct a new Stats object");

pub static STATS_CLASS: BuiltinClass = BuiltinClass {
    static_methods: &[&STATS_NEW],
    summary: "Statistics for a data set",
    fields: &[
        Field {
            name: "min",
            summary: "Smallest value in data set",
            ty: || Type::NUMBER,
        },
        Field {
            name: "max",
            summary: "Largest value in data set",
            ty: || Type::NUMBER,
        },
        Field {
            name: "mean",
            summary: "Average value in the data set",
            ty: || Type::NUMBER,
        },
        Field {
            name: "standardDeviation",
            summary: "Standard deviation",
            ty: || Type::NUMBER,
        },
        Field {
            name: "median",
            summary: "Median value of the data set",
            ty: || Type::NUMBER,
        },
        Field {
            name: "count",
            summary: "Number of elements in the data set",
            ty: || Type::NUMBER,
        },
    ],
    ..BuiltinClass::new("maths", "Stats", Some(&OBJECT_CLASS))
};

/// Calculate the logarithm of a given value
#[builtin(namespace = "maths", returns = crate::infer::Type::NUMBER)]
pub fn log(
    #[param("Number to take the logarithm of")] n: f64,
    #[param("base", default = std::f64::consts::E)] base: f64,
) -> R<f64> {
    Ok(if base == 10.0 {
        n.log10()
    } else if base == std::f64::consts::E {
        n.ln()
    } else {
        n.ln() / base.ln()
    })
}

/// Calculate the square root of a given value
#[builtin(namespace = "maths", aliases = ["maths.sqrt"], returns = crate::infer::Type::NUMBER)]
pub fn square_root(#[param("Number to take the square root of")] n: f64) -> R<f64> {
    Ok(n.sqrt())
}

/// Compute standard statistics (mean, standard deviation etc) on a collection of numbers
#[builtin(namespace = "maths", returns = crate::infer::Type::Instance(&STATS_CLASS))]
pub fn stats(args: &Args, #[param("Sequence of numbers on which to compute statistics")] sequence: Value) -> R<Object> {
    let Value::List(xs) = &sequence else {
        return Err(args.invalid(
            "sequence",
            &format!("Invalid sequence of type {}", sequence.type_name()),
        ));
    };
    let mut numbers = vec![];
    for item in xs.snapshot() {
        match item {
            Value::Number(n, _) => numbers.push(n.as_f64()),
            other => return Err(args.invalid("sequence", &format!("Invalid item of type {}", other.type_name()))),
        }
    }
    let stats = Stats::of(&numbers);
    let object = Object::with_class(Some(ClassRef::Builtin(&STATS_CLASS)));
    let number = |n: f64| Value::Number(Number::new(n), None);
    object.set_str("min", number(stats.min));
    object.set_str("max", number(stats.max));
    object.set_str("mean", number(stats.mean));
    object.set_str("standardDeviation", number(stats.standard_deviation));
    object.set_str("median", number(stats.median));
    object.set_str("count", Value::num(numbers.len()));
    Ok(object)
}

/// Descriptive statistics, as computed by Apache Commons Math's `DescriptiveStatistics` (which Scala Mash
/// used): the sample standard deviation, and the median as the 50th percentile by its default ("legacy")
/// estimation. Everything is NaN for no data.
#[derive(Debug, PartialEq)]
struct Stats {
    min: f64,
    max: f64,
    mean: f64,
    standard_deviation: f64,
    median: f64,
}

impl Stats {
    fn of(numbers: &[f64]) -> Stats {
        let n = numbers.len();
        if n == 0 {
            return Stats {
                min: f64::NAN,
                max: f64::NAN,
                mean: f64::NAN,
                standard_deviation: f64::NAN,
                median: f64::NAN,
            };
        }
        let min = numbers.iter().copied().fold(f64::INFINITY, f64::min);
        let max = numbers.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        let mean = numbers.iter().sum::<f64>() / n as f64;
        let standard_deviation = if n == 1 {
            0.0
        } else {
            let squares: f64 = numbers.iter().map(|x| (x - mean).powi(2)).sum();
            (squares / (n - 1) as f64).sqrt()
        };
        let mut sorted = numbers.to_vec();
        sorted.sort_by(f64::total_cmp);
        Stats {
            min,
            max,
            mean,
            standard_deviation,
            median: percentile(&sorted, 50.0),
        }
    }
}

/// Commons Math's legacy percentile estimate of sorted data
fn percentile(sorted: &[f64], p: f64) -> f64 {
    let n = sorted.len() as f64;
    let position = p * (n + 1.0) / 100.0;
    if position < 1.0 {
        return sorted[0];
    }
    if position >= n {
        return sorted[sorted.len() - 1];
    }
    let floor = position.floor();
    let lower = sorted[floor as usize - 1];
    let upper = sorted[floor as usize];
    lower + (position - floor) * (upper - lower)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stats_match_commons_math() {
        // Expected values from Scala Mash
        let s = Stats::of(&[1.0, 2.0, 3.0, 4.0]);
        assert_eq!((s.min, s.max, s.mean, s.median), (1.0, 4.0, 2.5, 2.5));
        assert_eq!(s.standard_deviation, 1.2909944487358056);
        let s = Stats::of(&[3.0, 1.0, 2.0]);
        assert_eq!((s.mean, s.standard_deviation, s.median), (2.0, 1.0, 2.0));
        let s = Stats::of(&[5.0]);
        assert_eq!((s.standard_deviation, s.median), (0.0, 5.0));
        assert!(Stats::of(&[]).mean.is_nan());
    }
}
