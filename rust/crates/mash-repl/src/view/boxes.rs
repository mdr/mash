//! The characters tables are drawn with (Scala `BoxCharacterSupplier`).

/// Characters for drawing a table: a double-line outer border with single-line inner dividers.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct BoxChars {
    pub double_top_left: char,
    pub double_top_right: char,
    pub double_bottom_right: char,
    pub double_bottom_left: char,
    pub double_horizontal_single_down: char,
    pub double_vertical: char,
    pub single_vertical: char,
    pub double_horizontal: char,
    pub double_vertical_single_right: char,
    pub single_horizontal: char,
    pub single_horizontal_single_down: char,
    pub single_intersect: char,
    pub double_vertical_single_left: char,
    pub double_horizontal_single_up: char,
}

impl BoxChars {
    /// Unicode box-drawing characters (what tables are printed with)
    pub const UNICODE: BoxChars = BoxChars {
        double_top_left: '╔',
        double_top_right: '╗',
        double_bottom_right: '╝',
        double_bottom_left: '╚',
        double_horizontal_single_down: '╤',
        double_vertical: '║',
        single_vertical: '│',
        double_horizontal: '═',
        double_vertical_single_right: '╟',
        single_horizontal: '─',
        single_horizontal_single_down: '┬',
        single_intersect: '┼',
        double_vertical_single_left: '╢',
        double_horizontal_single_up: '╧',
    };

    /// Plain ASCII
    pub const ASCII: BoxChars = BoxChars {
        double_top_left: '+',
        double_top_right: '+',
        double_bottom_right: '+',
        double_bottom_left: '+',
        double_horizontal_single_down: '-',
        double_vertical: '|',
        single_vertical: '|',
        double_horizontal: '-',
        double_vertical_single_right: '|',
        single_horizontal: '-',
        single_horizontal_single_down: '-',
        single_intersect: '+',
        double_vertical_single_left: '|',
        double_horizontal_single_up: '-',
    };
}
