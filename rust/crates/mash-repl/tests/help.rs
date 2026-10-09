//! Ported from `view/render/help/*Test.scala` and `view/render/CallingSyntaxRendererTest.scala`. Scala's tests
//! render a `TestPointClass` fixture; here the fixture is the equivalent help descriptions.

use mash_core::function::Param;
use mash_repl::screen::{Style, StyledString, default_colours};
use mash_repl::view::calling_syntax;
use mash_repl::view::help::{
    ClassDoc, FieldDoc, FunctionDoc, MemberDoc, MethodDoc, class_help, field_help, function_help, method_help,
};

fn text(lines: &[StyledString]) -> String {
    lines
        .iter()
        .map(StyledString::forget_styling)
        .collect::<Vec<_>>()
        .join("\n")
}

/// Scala's `TestPointClass`
fn point_class() -> ClassDoc {
    ClassDoc {
        full_name: "geometry.Point".into(),
        summary: Some("A point in 2D space".into()),
        description: Some("Has an x and y coordinate".into()),
        parent: Some("core.Object".into()),
        fields: vec![MemberDoc::new("x", Some("The x coordinate"))],
        static_methods: vec![MemberDoc::new("random", Some("Generate a random point"))],
        methods: vec![MemberDoc::new("norm", Some("Calculate the norm of the point"))],
    }
}

/// The field `x` of `TestPointClass`
fn point_x_field() -> FieldDoc {
    FieldDoc {
        name: "x".into(),
        summary: Some("The x coordinate".into()),
        description: Some("The horizontal coordinate. Examples:\n<mash>\n  point.x\n</mash>".into()),
        class_name: "geometry.Point".into(),
        type_name: Some("core.String".into()),
    }
}

#[test]
fn class_help_with_all_information() {
    assert_eq!(
        text(&class_help(&point_class())),
        "CLASS
    geometry.Point - A point in 2D space

DESCRIPTION
    Has an x and y coordinate

PARENT
    core.Object

FIELDS
    x - The x coordinate

STATIC METHODS
    random - Generate a random point

METHODS
    norm - Calculate the norm of the point"
    );
}

#[test]
fn field_help_with_all_information() {
    assert_eq!(
        text(&field_help(&point_x_field())),
        "FIELD
    x - The x coordinate

CLASS
    geometry.Point

TYPE
    core.String

DESCRIPTION
    The horizontal coordinate. Examples:
      point.x"
    );
}

#[test]
fn field_help_with_information_omitted() {
    let field = FieldDoc {
        summary: None,
        description: None,
        ..point_x_field()
    };
    assert_eq!(
        text(&field_help(&field)),
        "FIELD
    x

CLASS
    geometry.Point

TYPE
    core.String"
    );
}

#[test]
fn function_help_with_all_information() {
    let function = FunctionDoc {
        full_name: "maths.squareRoot".into(),
        aliases: vec!["sqrt".into()],
        name: "squareRoot".into(),
        summary: Some("Take the square root of a number".into()),
        description: Some("The number must not be negative".into()),
        params: vec![
            // A boolean flag without a default, as in Scala's fixture
            Param {
                flag: true,
                boolean_flag: true,
                ..Param::named("goFaster").summary("Compute it faster")
            },
            Param::named("n")
                .summary("Number to take square root of")
                .description("Must not be negative"),
        ],
        source: Some("def squareRoot n = findSquareRoot n".into()),
    };
    assert_eq!(
        text(&function_help(&function)),
        "FUNCTION
    maths.squareRoot, sqrt - Take the square root of a number

CALLING SYNTAX
    squareRoot --goFaster <n>

PARAMETERS
    --goFaster - Compute it faster

    n - Number to take square root of
        Must not be negative

DESCRIPTION
    The number must not be negative

SOURCE
    def squareRoot n = findSquareRoot n"
    );
}

#[test]
fn function_help_omits_calling_syntax_without_parameters() {
    let function = FunctionDoc {
        full_name: "os.currentDirectory".into(),
        name: "currentDirectory".into(),
        ..FunctionDoc::default()
    };
    assert_eq!(text(&function_help(&function)), "FUNCTION\n    os.currentDirectory");
}

#[test]
fn method_help_with_all_information() {
    let method = MethodDoc {
        name: "norm".into(),
        summary: Some("Calculate the norm of the point".into()),
        class_name: "geometry.Point".into(),
        target_name: "point".into(),
        ..MethodDoc::default()
    };
    assert_eq!(
        text(&method_help(&method)),
        "METHOD
    norm - Calculate the norm of the point

CLASS
    geometry.Point"
    );
}

#[test]
fn help_styles() {
    let lines = class_help(&point_class());
    let title = Style::fg(default_colours::YELLOW).with_bold();
    assert!(lines[0].chars.iter().all(|c| c.style == title));
    // The class name is bold, its summary plain
    assert_eq!(lines[1].chars[4].style, Style::DEFAULT.with_bold());
    assert_eq!(lines[1].chars[lines[1].len() - 1].style, Style::DEFAULT);
    // Member names are cyan and bold
    let fields = lines.iter().position(|l| l.forget_styling() == "FIELDS").unwrap();
    assert_eq!(
        lines[fields + 1].chars[4].style,
        Style::fg(default_colours::CYAN).with_bold()
    );
}

#[test]
fn mash_markup_in_descriptions_is_highlighted() {
    let lines = field_help(&point_x_field());
    let code = lines.last().unwrap();
    assert_eq!(code.forget_styling(), "      point.x");
    assert_eq!(code.chars[6].style, Style::fg(default_colours::FOREGROUND));
}

// ------------------------------------------------------------------------------------------------ Calling syntax

fn positional() -> Param {
    Param::named("positional")
}

fn optional() -> Param {
    Param::named("optional").default_value(|| false.into())
}

fn variadic() -> Param {
    Param::named("variadic").variadic()
}

#[test]
fn calling_syntax_of_positional_parameters() {
    let params = [positional(), optional(), variadic()];
    assert_eq!(
        calling_syntax::parameters(&params).forget_styling(),
        "<positional> [<optional>] <variadic>..."
    );
}

#[test]
fn calling_syntax_of_variadic_parameters_needing_at_least_one_argument() {
    let params = [Param::named("variadicAtLeastOne").variadic().at_least_one()];
    assert_eq!(
        calling_syntax::parameters(&params).forget_styling(),
        "<variadicAtLeastOne>+..."
    );
}

#[test]
fn calling_syntax_of_flag_parameters() {
    let params = [
        Param::named("flag").flag(),
        Param::named("flagWithShortName").boolean_flag().short('f'),
        Param::named("flagWithCustomValueName").flag().flag_value_name("name"),
        Param::named("mandatoryValueFlag").flag().mandatory_flag_value(),
        Param::named("booleanFlag").boolean_flag(),
    ];
    assert_eq!(
        calling_syntax::parameters(&params).forget_styling(),
        "--flag[=<value>] (--flagWithShortName | -f) --flagWithCustomValueName[=<name>] \
         --mandatoryValueFlag=<value> --booleanFlag"
    );
}

#[test]
fn calling_syntax_of_methods() {
    let params = [positional()];
    assert_eq!(
        calling_syntax::method("point", "move", &params).forget_styling(),
        "point.move <positional>"
    );
    assert_eq!(
        calling_syntax::method("point", "norm", &[]).forget_styling(),
        "point.norm"
    );
    assert_eq!(calling_syntax::example_target_name("DateTime"), "dateTime");
}
