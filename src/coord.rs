//! Coordinate System with Absolute Positioning and Shape Rendering
//!
//! Provides:
//! - Absolute (x, y) coordinate system
//! - Unicode-aware character width calculations
//! - Shape rendering with optimal character selection

use core::fmt;
use std::fmt::Display;

// ===== UNICODE WIDTH CALCULATION =====

/// Calculate actual terminal display width for text with Unicode characters
pub fn unicode_width(text: &str) -> usize {
    text.chars()
        .map(|c| match c {
            // Wide emoji and symbols
            '✅' | '❌' | '⚠' | '️' | '🎨' | '📏' | '📐' | '🧪' => 2,

            // Box drawing characters (single width)
            '─' | '│' | '┌' | '┐' | '└' | '┘' | '├' | '┤' | '┬' | '┴' | '┼' => {
                1
            }
            '═' | '║' | '╔' | '╗' | '╚' | '╝' | '╠' | '╣' | '╦' | '╩' | '╬' => {
                1
            }
            '▀' | '▄' | '█' | '░' | '▒' | '▓' | '■' | '□' | '▪' | '▫' => 1,
            '▲' | '▼' | '◄' | '►' | '◆' | '◇' | '○' | '●' | '◎' | '◉' => 1,
            '★' | '☆' | '◐' | '◑' | '◒' | '◓' => 1,

            // Zero-width characters
            '\u{200b}' | '\u{200c}' | '\u{200d}' | '\u{feff}' => 0,

            // ASCII (single width)
            c if c.is_ascii() => 1,

            // CJK and full-width characters (East Asian Width property)
            '\u{1100}'..='\u{115F}'
            | '\u{2329}'
            | '\u{232A}'
            | '\u{2E80}'..='\u{303E}'
            | '\u{3040}'..='\u{A4CF}'
            | '\u{AC00}'..='\u{D7A3}'
            | '\u{F900}'..='\u{FAFF}'
            | '\u{FE10}'..='\u{FE19}'
            | '\u{FE30}'..='\u{FE6F}'
            | '\u{FF00}'..='\u{FF60}'
            | '\u{FFE0}'..='\u{FFE6}' => 2,

            // Default to single width for other Unicode
            _ => 1,
        })
        .sum()
}

// ===== POINT AND COORDINATE SYSTEM =====

/// Absolute coordinate point (x, y)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Point {
    pub x: usize,
    pub y: usize,
}

impl Point {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    pub fn origin() -> Self {
        Self { x: 0, y: 0 }
    }

    pub fn translate(&self, dx: i32, dy: i32) -> Self {
        Self {
            x: (self.x as i32 + dx).max(0) as usize,
            y: (self.y as i32 + dy).max(0) as usize,
        }
    }

    pub fn distance_to(&self, other: &Point) -> f64 {
        let dx = self.x as f64 - other.x as f64;
        let dy = self.y as f64 - other.y as f64;
        (dx * dx + dy * dy).sqrt()
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

/// Rectangle defined by top-left and bottom-right points
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rect {
    pub top_left: Point,
    pub bottom_right: Point,
}

impl Rect {
    pub fn new(x1: usize, y1: usize, x2: usize, y2: usize) -> Self {
        Self {
            top_left: Point::new(x1.min(x2), y1.min(y2)),
            bottom_right: Point::new(x1.max(x2), y1.max(y2)),
        }
    }

    pub fn from_points(p1: Point, p2: Point) -> Self {
        Self::new(p1.x, p1.y, p2.x, p2.y)
    }

    pub fn width(&self) -> usize {
        self.bottom_right.x.saturating_sub(self.top_left.x)
    }

    pub fn height(&self) -> usize {
        self.bottom_right.y.saturating_sub(self.top_left.y)
    }

    pub fn area(&self) -> usize {
        self.width() * self.height()
    }

    pub fn contains(&self, point: &Point) -> bool {
        point.x >= self.top_left.x
            && point.x < self.bottom_right.x
            && point.y >= self.top_left.y
            && point.y < self.bottom_right.y
    }

    pub fn intersects(&self, other: &Rect) -> bool {
        self.top_left.x < other.bottom_right.x
            && self.bottom_right.x > other.top_left.x
            && self.top_left.y < other.bottom_right.y
            && self.bottom_right.y > other.top_left.y
    }

    pub fn center(&self) -> Point {
        Point::new(
            (self.top_left.x + self.bottom_right.x) / 2,
            (self.top_left.y + self.bottom_right.y) / 2,
        )
    }
}

// ===== SHAPE SYSTEM =====

/// Shape types for rendering
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShapeType {
    Rectangle,
    Circle,
    Triangle,
    Line,
    Ellipse,
    Diamond,
    Custom,
}

/// Shape rendering configuration
#[derive(Debug, Clone)]
pub struct Shape {
    pub shape_type: ShapeType,
    pub bounds: Rect,
    pub filled: bool,
    pub char_set: ShapeCharSet,
}

/// Character sets for rendering different shapes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShapeCharSet {
    /// Box drawing characters
    Box {
        horizontal: char,
        vertical: char,
        top_left: char,
        top_right: char,
        bottom_left: char,
        bottom_right: char,
        fill: char,
    },
    /// Block characters for filled shapes
    Block {
        full: char,
        half_top: char,
        half_bottom: char,
        half_left: char,
        half_right: char,
        quarter: char,
    },
    /// ASCII art characters
    Ascii {
        horizontal: char,
        vertical: char,
        corner: char,
        fill: char,
    },
    /// Circle-specific characters
    Circle { outline: char, fill: char },
    /// Custom character
    Custom(char),
}

impl Default for ShapeCharSet {
    fn default() -> Self {
        ShapeCharSet::Box {
            horizontal: '─',
            vertical: '│',
            top_left: '┌',
            top_right: '┐',
            bottom_left: '└',
            bottom_right: '┘',
            fill: ' ',
        }
    }
}

impl ShapeCharSet {
    pub fn box_drawing() -> Self {
        Self::default()
    }

    pub fn double_line() -> Self {
        ShapeCharSet::Box {
            horizontal: '═',
            vertical: '║',
            top_left: '╔',
            top_right: '╗',
            bottom_left: '╚',
            bottom_right: '╝',
            fill: ' ',
        }
    }

    pub fn blocks() -> Self {
        ShapeCharSet::Block {
            full: '█',
            half_top: '▀',
            half_bottom: '▄',
            half_left: '▌',
            half_right: '▐',
            quarter: '▪',
        }
    }

    pub fn ascii() -> Self {
        ShapeCharSet::Ascii {
            horizontal: '-',
            vertical: '|',
            corner: '+',
            fill: '#',
        }
    }

    pub fn circle_chars() -> Self {
        ShapeCharSet::Circle {
            outline: '○',
            fill: '●',
        }
    }
}

impl Shape {
    pub fn new(shape_type: ShapeType, bounds: Rect) -> Self {
        let char_set = match shape_type {
            ShapeType::Circle | ShapeType::Ellipse => ShapeCharSet::circle_chars(),
            ShapeType::Rectangle => ShapeCharSet::box_drawing(),
            ShapeType::Triangle | ShapeType::Diamond => ShapeCharSet::blocks(),
            _ => ShapeCharSet::default(),
        };

        Self {
            shape_type,
            bounds,
            filled: false,
            char_set,
        }
    }

    pub fn rectangle(x1: usize, y1: usize, x2: usize, y2: usize) -> Self {
        Self::new(ShapeType::Rectangle, Rect::new(x1, y1, x2, y2))
    }

    pub fn circle(center_x: usize, center_y: usize, radius: usize) -> Self {
        Self::new(
            ShapeType::Circle,
            Rect::new(
                center_x.saturating_sub(radius),
                center_y.saturating_sub(radius),
                center_x + radius,
                center_y + radius,
            ),
        )
    }

    pub fn triangle(x1: usize, y1: usize, x2: usize, y2: usize) -> Self {
        Self::new(ShapeType::Triangle, Rect::new(x1, y1, x2, y2))
    }

    pub fn diamond(x1: usize, y1: usize, x2: usize, y2: usize) -> Self {
        Self::new(ShapeType::Diamond, Rect::new(x1, y1, x2, y2))
    }

    pub fn line(x1: usize, y1: usize, x2: usize, y2: usize) -> Self {
        Self::new(ShapeType::Line, Rect::new(x1, y1, x2, y2))
    }

    pub fn with_filled(mut self, filled: bool) -> Self {
        self.filled = filled;
        self
    }

    pub fn with_char_set(mut self, char_set: ShapeCharSet) -> Self {
        self.char_set = char_set;
        self
    }

    /// Calculate optimal character for a point in the shape
    pub fn char_at(&self, point: &Point) -> Option<char> {
        if !self.bounds.contains(point) {
            return None;
        }

        let relative_x = point.x - self.bounds.top_left.x;
        let relative_y = point.y - self.bounds.top_left.y;
        let width = self.bounds.width();
        let height = self.bounds.height();

        match self.shape_type {
            ShapeType::Rectangle => self.rectangle_char_at(relative_x, relative_y, width, height),
            ShapeType::Circle => self.circle_char_at(relative_x, relative_y, width, height),
            ShapeType::Triangle => self.triangle_char_at(relative_x, relative_y, width, height),
            ShapeType::Diamond => self.diamond_char_at(relative_x, relative_y, width, height),
            ShapeType::Line => Some(self.line_char()),
            _ => Some(' '),
        }
    }

    fn rectangle_char_at(&self, x: usize, y: usize, width: usize, height: usize) -> Option<char> {
        if width == 0 || height == 0 {
            return None;
        }

        match &self.char_set {
            ShapeCharSet::Box {
                horizontal,
                vertical,
                top_left,
                top_right,
                bottom_left,
                bottom_right,
                fill,
            } => {
                if y == 0 && x == 0 {
                    Some(*top_left)
                } else if y == 0 && x == width - 1 {
                    Some(*top_right)
                } else if y == height - 1 && x == 0 {
                    Some(*bottom_left)
                } else if y == height - 1 && x == width - 1 {
                    Some(*bottom_right)
                } else if y == 0 || y == height - 1 {
                    Some(*horizontal)
                } else if x == 0 || x == width - 1 {
                    Some(*vertical)
                } else if self.filled {
                    Some(*fill)
                } else {
                    Some(' ')
                }
            }
            ShapeCharSet::Block { full, .. } if self.filled => Some(*full),
            ShapeCharSet::Ascii {
                horizontal,
                vertical,
                corner,
                fill,
            } => {
                if (y == 0 || y == height - 1) && (x == 0 || x == width - 1) {
                    Some(*corner)
                } else if y == 0 || y == height - 1 {
                    Some(*horizontal)
                } else if x == 0 || x == width - 1 {
                    Some(*vertical)
                } else if self.filled {
                    Some(*fill)
                } else {
                    Some(' ')
                }
            }
            ShapeCharSet::Custom(c) => Some(*c),
            _ => Some(' '),
        }
    }

    fn circle_char_at(&self, x: usize, y: usize, width: usize, height: usize) -> Option<char> {
        if width == 0 || height == 0 {
            return None;
        }

        let center_x = width as f64 / 2.0;
        let center_y = height as f64 / 2.0;
        let radius_x = width as f64 / 2.0;
        let radius_y = height as f64 / 2.0;

        let dx = (x as f64 - center_x) / radius_x;
        let dy = (y as f64 - center_y) / radius_y;
        let distance = (dx * dx + dy * dy).sqrt();

        match &self.char_set {
            ShapeCharSet::Circle { outline, fill } => {
                if distance <= 0.3 && self.filled {
                    Some(*fill)
                } else if distance <= 1.0 && distance > 0.8 {
                    Some(*outline)
                } else if distance < 0.8 && self.filled {
                    Some(*fill)
                } else if distance < 1.0 {
                    Some(' ')
                } else {
                    None
                }
            }
            ShapeCharSet::Block { full, quarter, .. } => {
                if distance <= 1.0 {
                    if self.filled || distance > 0.8 {
                        Some(*quarter)
                    } else {
                        Some(' ')
                    }
                } else {
                    None
                }
            }
            _ => {
                if distance <= 1.0 {
                    Some('o')
                } else {
                    None
                }
            }
        }
    }

    fn triangle_char_at(&self, x: usize, y: usize, width: usize, height: usize) -> Option<char> {
        if width == 0 || height == 0 {
            return None;
        }

        // Upward pointing triangle
        let center_x = width / 2;
        let max_width_at_y = ((height - y) * width) / height;
        let left_bound = center_x.saturating_sub(max_width_at_y / 2);
        let right_bound = center_x + max_width_at_y / 2;

        if x >= left_bound && x <= right_bound {
            match &self.char_set {
                ShapeCharSet::Block { full, quarter, .. } => {
                    if self.filled {
                        Some(*full)
                    } else if x == left_bound || x == right_bound || y == height - 1 {
                        Some(*quarter)
                    } else {
                        Some(' ')
                    }
                }
                _ => Some('▲'),
            }
        } else {
            None
        }
    }

    fn diamond_char_at(&self, x: usize, y: usize, width: usize, height: usize) -> Option<char> {
        if width == 0 || height == 0 {
            return None;
        }

        let center_x = width / 2;
        let center_y = height / 2;

        let dx = if x > center_x {
            x - center_x
        } else {
            center_x - x
        };
        let dy = if y > center_y {
            y - center_y
        } else {
            center_y - y
        };

        // Diamond equation: |x - cx| / (w/2) + |y - cy| / (h/2) <= 1
        let normalized = (dx as f64 / (width as f64 / 2.0)) + (dy as f64 / (height as f64 / 2.0));

        if normalized <= 1.0 {
            match &self.char_set {
                ShapeCharSet::Block { full, quarter, .. } => {
                    if self.filled {
                        Some(*full)
                    } else if normalized > 0.8 {
                        Some(*quarter)
                    } else {
                        Some(' ')
                    }
                }
                _ => Some('◆'),
            }
        } else {
            None
        }
    }

    fn line_char(&self) -> char {
        let dx = self.bounds.bottom_right.x as i32 - self.bounds.top_left.x as i32;
        let dy = self.bounds.bottom_right.y as i32 - self.bounds.top_left.y as i32;

        if dx.abs() > dy.abs() * 2 {
            '─' // Horizontal
        } else if dy.abs() > dx.abs() * 2 {
            '│' // Vertical
        } else if (dx > 0 && dy > 0) || (dx < 0 && dy < 0) {
            '\\' // Diagonal down-right or up-left
        } else {
            '/' // Diagonal up-right or down-left
        }
    }

    /// Generate all points that are part of this shape
    pub fn generate_points(&self) -> Vec<Point> {
        let mut points = Vec::new();

        for y in self.bounds.top_left.y..self.bounds.bottom_right.y {
            for x in self.bounds.top_left.x..self.bounds.bottom_right.x {
                let point = Point::new(x, y);
                if self.char_at(&point).is_some() {
                    points.push(point);
                }
            }
        }

        points
    }

    /// Render the shape to a string grid
    pub fn render(&self) -> String {
        let mut output = String::new();

        for y in self.bounds.top_left.y..self.bounds.bottom_right.y {
            for x in self.bounds.top_left.x..self.bounds.bottom_right.x {
                let point = Point::new(x, y);
                if let Some(ch) = self.char_at(&point) {
                    output.push(ch);
                } else {
                    output.push(' ');
                }
            }
            output.push('\n');
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unicode_width() {
        assert_eq!(unicode_width("hello"), 5);
        assert_eq!(unicode_width("✅"), 2);
        assert_eq!(unicode_width("─"), 1);
        assert_eq!(unicode_width("hello ✅"), 8); // "hello " (6) + "✅" (2) = 8
    }

    #[test]
    fn test_point() {
        let p = Point::new(5, 10);
        assert_eq!(p.x, 5);
        assert_eq!(p.y, 10);

        let translated = p.translate(2, -3);
        assert_eq!(translated.x, 7);
        assert_eq!(translated.y, 7);
    }

    #[test]
    fn test_rect() {
        let rect = Rect::new(0, 0, 10, 10);
        assert_eq!(rect.width(), 10);
        assert_eq!(rect.height(), 10);
        assert_eq!(rect.area(), 100);

        let p1 = Point::new(5, 5);
        assert!(rect.contains(&p1));

        let p2 = Point::new(15, 15);
        assert!(!rect.contains(&p2));
    }

    #[test]
    fn test_shape_rectangle() {
        let shape = Shape::rectangle(0, 0, 5, 3);
        assert_eq!(shape.shape_type, ShapeType::Rectangle);
        assert_eq!(shape.bounds.width(), 5);
        assert_eq!(shape.bounds.height(), 3);

        let points = shape.generate_points();
        assert!(!points.is_empty());
    }

    #[test]
    fn test_shape_circle() {
        let shape = Shape::circle(5, 5, 3).with_filled(true);
        assert_eq!(shape.shape_type, ShapeType::Circle);

        let points = shape.generate_points();
        assert!(!points.is_empty());
    }
}
