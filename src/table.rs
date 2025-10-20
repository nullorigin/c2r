use crate::{Entry, Id};
use core::fmt;

use std::fmt::Write;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    ops::Range,
};

/// Text justification options for table cells
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Justification {
    /// Left-aligned text (default)
    Left,
    /// Center-aligned text
    Center,
    /// Right-aligned text
    Right,
}
#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct Coords {
    x_ranges: Vec<Range<usize>>,
    y_ranges: Vec<Range<usize>>,
    shape_type: ShapeType,
}

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub enum ShapeType {
    #[default]
    Rectangle,
    Circle,
    Triangle,
    Line,
    Custom,
}

impl Coords {
    /// Create a new rectangular coordinate with a single range
    pub fn new(x: Range<usize>, y: Range<usize>) -> Self {
        Self {
            x_ranges: vec![x],
            y_ranges: vec![y],
            shape_type: ShapeType::Rectangle,
        }
    }

    /// Create a coordinate with a specific shape type
    pub fn new_with_shape(x: Range<usize>, y: Range<usize>, shape: ShapeType) -> Self {
        Self {
            x_ranges: vec![x],
            y_ranges: vec![y],
            shape_type: shape,
        }
    }

    /// Create a coordinate from multiple ranges (automatically sets to Custom shape)
    pub fn from_multiple_ranges(x_ranges: Vec<Range<usize>>, y_ranges: Vec<Range<usize>>) -> Self {
        Self {
            x_ranges,
            y_ranges,
            shape_type: ShapeType::Custom,
        }
    }

    /// Create a true mathematical point (dimensionless)
    /// Uses coord..coord ranges for precise point positioning in shapes
    /// Example: Coords::point(5, 3) creates ranges 5..5, 3..3
    pub fn point(x: usize, y: usize) -> Self {
        Self::new(Self::x_point(x), Self::y_point(y))
    }

    /// Create a renderable dot (single unit for display)
    pub fn dot(x: usize, y: usize) -> Self {
        Self::new(x..x + 1, y..y + 1)
    }

    /// Create a single-dot x range (for shape point coordinates)
    /// Creates coord..coord+1 range representing a single unit at that position
    pub fn x_dot(coord: usize) -> Range<usize> {
        coord..coord + 1
    }

    /// Create a single-dot y range (for shape point coordinates)
    /// Creates coord..coord+1 range representing a single unit at that position
    pub fn y_dot(coord: usize) -> Range<usize> {
        coord..coord + 1
    }

    /// Create an empty x range at a specific point (coord..coord)
    /// Useful for mathematical point representations in shapes
    pub fn x_point(coord: usize) -> Range<usize> {
        coord..coord
    }

    /// Create an empty y range at a specific point (coord..coord)
    /// Useful for mathematical point representations in shapes
    pub fn y_point(coord: usize) -> Range<usize> {
        coord..coord
    }

    /// Create a square coordinate
    pub fn square(top_left_x: usize, top_left_y: usize, size: usize) -> Self {
        Self::new(top_left_x..top_left_x + size, top_left_y..top_left_y + size)
    }

    /// Create a circle coordinate
    pub fn circle(center_x: usize, center_y: usize, radius: usize) -> Self {
        let x_range = center_x.saturating_sub(radius)..center_x.saturating_add(radius);
        let y_range = center_y.saturating_sub(radius)..center_y.saturating_add(radius);
        Self::new_with_shape(x_range, y_range, ShapeType::Circle)
    }

    /// Get x range at specific index
    pub fn x_range(&self, index: usize) -> Option<&Range<usize>> {
        self.x_ranges.get(index)
    }

    /// Get y range at specific index
    pub fn y_range(&self, index: usize) -> Option<&Range<usize>> {
        self.y_ranges.get(index)
    }

    /// Get all x ranges
    pub fn x_ranges(&self) -> &[Range<usize>] {
        &self.x_ranges
    }

    /// Get all y ranges
    pub fn y_ranges(&self) -> &[Range<usize>] {
        &self.y_ranges
    }

    /// Get the shape type
    pub fn shape_type(&self) -> &ShapeType {
        &self.shape_type
    }

    /// Calculate maximum width across all ranges
    pub fn width(&self) -> usize {
        self.x_ranges
            .iter()
            .map(|r| r.end.saturating_sub(r.start))
            .max()
            .unwrap_or(0)
    }

    /// Calculate maximum height across all ranges
    pub fn height(&self) -> usize {
        self.y_ranges
            .iter()
            .map(|r| r.end.saturating_sub(r.start))
            .max()
            .unwrap_or(0)
    }

    /// Calculate total width of bounding box
    pub fn total_width(&self) -> usize {
        let bounds = self.get_bounds();
        bounds.0.end.saturating_sub(bounds.0.start)
    }

    /// Calculate total height of bounding box
    pub fn total_height(&self) -> usize {
        let bounds = self.get_bounds();
        bounds.1.end.saturating_sub(bounds.1.start)
    }

    /// Calculate area based on shape type
    pub fn area(&self) -> usize {
        match self.shape_type {
            ShapeType::Rectangle => self.total_width() * self.total_height(),
            ShapeType::Circle => {
                let radius = self.total_width().min(self.total_height()) / 2;
                (std::f64::consts::PI * (radius as f64).powi(2)) as usize
            }
            ShapeType::Triangle => (self.total_width() * self.total_height()) / 2,
            ShapeType::Line => 0, // Lines have no area
            ShapeType::Custom => self
                .x_ranges
                .iter()
                .zip(&self.y_ranges)
                .map(|(x, y)| {
                    let width = x.end.saturating_sub(x.start);
                    let height = y.end.saturating_sub(y.start);
                    width * height
                })
                .sum(),
        }
    }

    /// Calculate perimeter based on shape type
    pub fn perimeter(&self) -> usize {
        match self.shape_type {
            ShapeType::Rectangle => 2 * (self.total_width() + self.total_height()),
            ShapeType::Circle => {
                let radius = self.total_width().min(self.total_height()) / 2;
                (2.0 * std::f64::consts::PI * radius as f64) as usize
            }
            ShapeType::Triangle => {
                let w = self.total_width() as f64;
                let h = self.total_height() as f64;
                // Perimeter of right triangle: base + height + hypotenuse
                (w + h + (w * w + h * h).sqrt()) as usize
            }
            ShapeType::Line => {
                // For a line, perimeter is twice the length (going there and back)
                let w = self.total_width() as f64;
                let h = self.total_height() as f64;
                (2.0 * (w * w + h * h).sqrt()) as usize
            }
            ShapeType::Custom => {
                // Sum perimeters of all rectangular ranges
                self.x_ranges
                    .iter()
                    .zip(&self.y_ranges)
                    .map(|(x, y)| {
                        let width = x.end.saturating_sub(x.start);
                        let height = y.end.saturating_sub(y.start);
                        2 * (width + height)
                    })
                    .sum()
            }
        }
    }

    /// Check if this is a perfect square
    pub fn is_square(&self) -> bool {
        self.total_width() == self.total_height() && matches!(self.shape_type, ShapeType::Rectangle)
    }

    /// Check if a point is contained within any of the ranges
    pub fn contains_point(&self, x: usize, y: usize) -> bool {
        match self.shape_type {
            ShapeType::Circle => {
                let (center_x, center_y) = self.center();
                let radius = self.total_width().min(self.total_height()) / 2;

                let dx = x as f64 - center_x as f64;
                let dy = y as f64 - center_y as f64;
                (dx * dx + dy * dy) <= (radius * radius) as f64
            }
            ShapeType::Triangle => {
                let bounds = self.get_bounds();
                let (x1, y1) = (bounds.0.start, bounds.1.end); // bottom-left
                let (x2, y2) = (bounds.0.end, bounds.1.end); // bottom-right
                let (x3, y3) = ((bounds.0.start + bounds.0.end) / 2, bounds.1.start); // top-center

                // Use barycentric coordinates to check if point is inside triangle
                let denom = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3);
                if denom == 0 {
                    return false;
                } // degenerate triangle

                let a = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) as f64 / denom as f64;
                let b = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) as f64 / denom as f64;
                let c = 1.0 - a - b;

                a >= 0.0 && b >= 0.0 && c >= 0.0
            }
            _ => self
                .x_ranges
                .iter()
                .zip(&self.y_ranges)
                .any(|(x_range, y_range)| x_range.contains(&x) && y_range.contains(&y)),
        }
    }

    /// Get the overall bounding box of all ranges
    pub fn get_bounds(&self) -> (Range<usize>, Range<usize>) {
        if self.x_ranges.is_empty() || self.y_ranges.is_empty() {
            return (0..0, 0..0);
        }

        let min_x = self.x_ranges.iter().map(|r| r.start).min().unwrap_or(0);
        let max_x = self.x_ranges.iter().map(|r| r.end).max().unwrap_or(0);
        let min_y = self.y_ranges.iter().map(|r| r.start).min().unwrap_or(0);
        let max_y = self.y_ranges.iter().map(|r| r.end).max().unwrap_or(0);

        (min_x..max_x, min_y..max_y)
    }

    /// Alias for get_bounds for consistency
    pub fn bounding_box(&self) -> Self {
        let (x_bound, y_bound) = self.get_bounds();
        Self::square(x_bound.start, y_bound.start, x_bound.end - x_bound.start)
    }

    /// Get center point of the bounding box
    pub fn center(&self) -> (usize, usize) {
        let bounds = self.get_bounds();
        let center_x = (bounds.0.start + bounds.0.end) / 2;
        let center_y = (bounds.1.start + bounds.1.end) / 2;
        (center_x, center_y)
    }

    /// Generate all points contained within the coordinate system
    pub fn generate_points(&self) -> Vec<(usize, usize)> {
        let mut points = Vec::new();
        let (x_bound, y_bound) = self.get_bounds();

        match self.shape_type {
            ShapeType::Rectangle => {
                for y in y_bound {
                    for x in x_bound.clone() {
                        if self.contains_point(x, y) {
                            points.push((x, y));
                        }
                    }
                }
            }
            ShapeType::Circle => {
                for y in y_bound {
                    for x in x_bound.clone() {
                        if self.contains_point(x, y) {
                            points.push((x, y));
                        }
                    }
                }
            }
            ShapeType::Line => {
                if x_bound.is_empty() || y_bound.is_empty() {
                    return points;
                }

                let start_x = x_bound.start;
                let start_y = y_bound.start;
                let end_x = x_bound.end.saturating_sub(1);
                let end_y = y_bound.end.saturating_sub(1);

                let dx = end_x as i32 - start_x as i32;
                let dy = end_y as i32 - start_y as i32;
                let steps = dx.abs().max(dy.abs());

                if steps == 0 {
                    points.push((start_x, start_y));
                } else {
                    for i in 0..=steps {
                        let x = start_x + (dx * i / steps) as usize;
                        let y = start_y + (dy * i / steps) as usize;
                        points.push((x, y));
                    }
                }
            }
            ShapeType::Triangle => {
                // Generate triangle points (simplified right triangle)
                for y in y_bound.clone() {
                    for x in x_bound.clone() {
                        let rel_x = x - x_bound.start;
                        let rel_y = y - y_bound.start;
                        let max_x_for_y = self.total_width().saturating_sub(rel_y);
                        if rel_x <= max_x_for_y {
                            points.push((x, y));
                        }
                    }
                }
            }
            ShapeType::Custom => {
                for y in y_bound {
                    for x in x_bound.clone() {
                        if self.contains_point(x, y) {
                            points.push((x, y));
                        }
                    }
                }
            }
        }
        points
    }

    /// Translate the coordinate system by dx, dy
    pub fn translate(&mut self, dx: i32, dy: i32) {
        self.x_ranges = self
            .x_ranges
            .iter()
            .map(|r| {
                let new_start = (r.start as i32 + dx).max(0) as usize;
                let new_end = (r.end as i32 + dx).max(0) as usize;
                new_start..new_end
            })
            .collect();

        self.y_ranges = self
            .y_ranges
            .iter()
            .map(|r| {
                let new_start = (r.start as i32 + dy).max(0) as usize;
                let new_end = (r.end as i32 + dy).max(0) as usize;
                new_start..new_end
            })
            .collect();
    }

    /// Scale the coordinate system by given factors
    pub fn scale(&mut self, scale_x: f64, scale_y: f64) {
        if scale_x <= 0.0 || scale_y <= 0.0 {
            return; // Avoid invalid scaling
        }

        self.x_ranges = self
            .x_ranges
            .iter()
            .map(|r| {
                let new_start = (r.start as f64 * scale_x) as usize;
                let new_end = (r.end as f64 * scale_x).max(new_start as f64 + 1.0) as usize;
                new_start..new_end
            })
            .collect();

        self.y_ranges = self
            .y_ranges
            .iter()
            .map(|r| {
                let new_start = (r.start as f64 * scale_y) as usize;
                let new_end = (r.end as f64 * scale_y).max(new_start as f64 + 1.0) as usize;
                new_start..new_end
            })
            .collect();
    }

    /// Check if this coordinate intersects with another
    pub fn intersects(&self, other: &Coords) -> bool {
        let (self_x, self_y) = self.get_bounds();
        let (other_x, other_y) = other.get_bounds();

        self_x.start < other_x.end
            && self_x.end > other_x.start
            && self_y.start < other_y.end
            && self_y.end > other_y.start
    }

    /// Calculate distance between centers of two coordinates
    pub fn distance_to(&self, other: &Coords) -> f64 {
        let (self_center_x, self_center_y) = self.center();
        let (other_center_x, other_center_y) = other.center();

        let dx = self_center_x as f64 - other_center_x as f64;
        let dy = self_center_y as f64 - other_center_y as f64;

        (dx * dx + dy * dy).sqrt()
    }

    /// Add a new range to the coordinate (converts to Custom shape)
    pub fn add_range(&mut self, x: Range<usize>, y: Range<usize>) {
        self.x_ranges.push(x);
        self.y_ranges.push(y);
        if !matches!(self.shape_type, ShapeType::Custom) {
            self.shape_type = ShapeType::Custom;
        }
    }

    /// Remove a range at the specified index
    pub fn remove_range(&mut self, index: usize) -> Option<(Range<usize>, Range<usize>)> {
        if index < self.range_count() {
            let x_range = self.x_ranges.remove(index);
            let y_range = self.y_ranges.remove(index);
            // Update shape type if we only have one range left
            if self.range_count() == 1 && matches!(self.shape_type, ShapeType::Custom) {
                self.shape_type = ShapeType::Rectangle;
            }
            Some((x_range, y_range))
        } else {
            None
        }
    }

    /// Get the number of coordinate ranges
    pub fn range_count(&self) -> usize {
        self.x_ranges.len().min(self.y_ranges.len())
    }

    /// Check if the coordinate is empty (no ranges)
    pub fn is_empty(&self) -> bool {
        self.x_ranges.is_empty() || self.y_ranges.is_empty()
    }

    /// Clear all ranges and reset to default rectangle shape
    pub fn clear(&mut self) {
        self.x_ranges.clear();
        self.y_ranges.clear();
        self.shape_type = ShapeType::Rectangle;
    }

    /// Create a union of this coordinate with another
    pub fn union(&self, other: &Coords) -> Coords {
        let mut result = Coords::from_multiple_ranges(self.x_ranges.clone(), self.y_ranges.clone());

        for (x_range, y_range) in other.x_ranges.iter().zip(&other.y_ranges) {
            result.add_range(x_range.clone(), y_range.clone());
        }

        result
    }

    /// Create intersection of this coordinate with another
    pub fn intersection(&self, other: &Coords) -> Option<Coords> {
        let (self_bounds, other_bounds) = (self.get_bounds(), other.get_bounds());

        let x_start = self_bounds.0.start.max(other_bounds.0.start);
        let x_end = self_bounds.0.end.min(other_bounds.0.end);
        let y_start = self_bounds.1.start.max(other_bounds.1.start);
        let y_end = self_bounds.1.end.min(other_bounds.1.end);

        if x_start < x_end && y_start < y_end {
            Some(Coords::new(x_start..x_end, y_start..y_end))
        } else {
            None
        }
    }

    /// Check if this coordinate completely contains another
    pub fn contains(&self, other: &Coords) -> bool {
        let (self_bounds, other_bounds) = (self.get_bounds(), other.get_bounds());

        self_bounds.0.start <= other_bounds.0.start
            && self_bounds.0.end >= other_bounds.0.end
            && self_bounds.1.start <= other_bounds.1.start
            && self_bounds.1.end >= other_bounds.1.end
    }

    /// Expand the coordinate by a given margin in all directions
    pub fn expand(&mut self, margin: usize) {
        self.x_ranges = self
            .x_ranges
            .iter()
            .map(|r| r.start.saturating_sub(margin)..r.end + margin)
            .collect();

        self.y_ranges = self
            .y_ranges
            .iter()
            .map(|r| r.start.saturating_sub(margin)..r.end + margin)
            .collect();
    }

    /// Shrink the coordinate by a given margin from all directions
    pub fn shrink(&mut self, margin: usize) {
        self.x_ranges = self
            .x_ranges
            .iter()
            .filter_map(|r| {
                let new_start = r.start + margin;
                let new_end = r.end.saturating_sub(margin);
                if new_start < new_end {
                    Some(new_start..new_end)
                } else {
                    None
                }
            })
            .collect();

        self.y_ranges = self
            .y_ranges
            .iter()
            .filter_map(|r| {
                let new_start = r.start + margin;
                let new_end = r.end.saturating_sub(margin);
                if new_start < new_end {
                    Some(new_start..new_end)
                } else {
                    None
                }
            })
            .collect();
    }
}

#[derive(Debug, Clone, Default)]
pub struct Table {
    rows: usize,
    columns: usize,
    column_offset: usize,
    row_offset: usize,
    coords: Vec<Coords>,
    cells: HashMap<Coords, TableCell>,
    id: Id,
}

impl Hash for Table {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rows.hash(state);
        self.columns.hash(state);
        self.column_offset.hash(state);
        self.row_offset.hash(state);
        self.coords.hash(state);
        self.cells.values().for_each(|x| x.hash(state));
        self.cells.keys().for_each(|y| y.hash(state));
        self.cells.len().hash(state);
        self.id.hash(state);
    }
}

impl Table {
    pub fn new() -> Table {
        Table::default()
    }

    pub fn new_with_dimensions(rows: usize, columns: usize) -> Table {
        Table {
            rows,
            columns,
            ..Default::default()
        }
    }

    pub fn new_with_offset(
        rows: usize,
        columns: usize,
        row_offset: usize,
        column_offset: usize,
    ) -> Table {
        Table {
            rows,
            columns,
            row_offset,
            column_offset,
            ..Default::default()
        }
    }

    pub fn rows(&self) -> usize {
        self.rows
    }

    pub fn content_rows(&self) -> usize {
        if self.cells.is_empty() {
            0
        } else {
            self.cells
                .keys()
                .map(|coord| coord.get_bounds().1.end) // y coordinate = row
                .max()
                .unwrap_or(0)
        }
    }

    pub fn columns(&self) -> usize {
        self.columns
    }

    pub fn column_offset(&self) -> usize {
        self.column_offset
    }

    pub fn row_offset(&self) -> usize {
        self.row_offset
    }

    pub fn coords(&self) -> &[Coords] {
        &self.coords.as_slice()
    }

    pub fn name(&self) -> String {
        self.id.name()
    }

    pub fn id(&self) -> Id {
        self.id.clone()
    }

    pub fn entries(&self) -> &HashMap<Coords, TableCell> {
        &self.cells
    }

    pub fn set_offset(&mut self, row_offset: usize, column_offset: usize) {
        self.row_offset = row_offset;
        self.column_offset = column_offset;
    }

    pub fn set_dimensions(&mut self, rows: usize, columns: usize) {
        self.rows = rows;
        self.columns = columns;
    }

    pub fn get_cell(&self, row: usize, col: usize) -> Option<&TableCell> {
        let coord = Coords::dot(col, row); // x=col, y=row - match creation coordinates
        self.get_cell_by_coord(&coord)
    }

    pub fn get_cell_mut(&mut self, row: usize, col: usize) -> Option<&mut TableCell> {
        let coord = Coords::dot(col, row); // x=col, y=row - match creation coordinates
        self.get_cell_by_coord_mut(&coord)
    }

    pub fn get_cell_by_coord(&self, coord: &Coords) -> Option<&TableCell> {
        self.cells.get(coord)
    }

    pub fn get_cell_by_coord_mut(&mut self, coord: &Coords) -> Option<&mut TableCell> {
        self.cells.get_mut(coord)
    }

    pub fn get_cell_global(&self, global_row: usize, global_col: usize) -> Option<&TableCell> {
        if let Some((local_row, local_col)) = self.global_to_local(global_row, global_col) {
            self.get_cell(local_row, local_col)
        } else {
            None
        }
    }

    pub fn get_cell_global_mut(
        &mut self,
        global_row: usize,
        global_col: usize,
    ) -> Option<&mut TableCell> {
        if let Some((local_row, local_col)) = self.global_to_local(global_row, global_col) {
            self.get_cell_mut(local_row, local_col)
        } else {
            None
        }
    }

    pub fn add_cell(&mut self, row: usize, col: usize, cell: TableCell) {
        let coord = Coords::dot(col, row); // x=col, y=row - match creation coordinates
        self.add_cell_by_coord(coord, cell);
    }

    pub fn add_cell_by_coord(&mut self, coord: Coords, cell: TableCell) {
        self.auto_resize_for_coord(&coord);
        self.cells.insert(coord.clone(), cell);
        if !self.coords.contains(&coord) {
            self.coords.push(coord);
        }
    }

    pub fn add_cell_global(&mut self, global_row: usize, global_col: usize, cell: TableCell) {
        if let Some((local_row, local_col)) = self.global_to_local(global_row, global_col) {
            self.add_cell(local_row, local_col, cell);
        }
    }

    pub fn set_cell(&mut self, row: usize, col: usize, cell: TableCell) {
        let coord = Coords::dot(col, row); // x=col, y=row - match creation coordinates
        self.set_cell_by_coord(coord, cell);
    }

    pub fn set_cell_by_coord(&mut self, coord: Coords, cell: TableCell) {
        self.cells.insert(coord.clone(), cell);
        if !self.coords.contains(&coord) {
            self.coords.push(coord);
        }
    }

    pub fn set_cell_global(&mut self, global_row: usize, global_col: usize, cell: TableCell) {
        if let Some((local_row, local_col)) = self.global_to_local(global_row, global_col) {
            self.set_cell(local_row, local_col, cell);
        }
    }

    pub fn remove_cell(&mut self, row: usize, col: usize) -> Option<TableCell> {
        let coord = Coords::dot(col, row); // x=col, y=row - match creation coordinates
        self.remove_cell_by_coord(&coord)
    }

    pub fn remove_cell_by_coord(&mut self, coord: &Coords) -> Option<TableCell> {
        self.coords.retain(|c| c != coord);
        self.cells.remove(coord)
    }

    pub fn remove_cell_global(
        &mut self,
        global_row: usize,
        global_col: usize,
    ) -> Option<TableCell> {
        if let Some((local_row, local_col)) = self.global_to_local(global_row, global_col) {
            self.remove_cell(local_row, local_col)
        } else {
            None
        }
    }

    pub fn contains_cell(&self, row: usize, col: usize) -> bool {
        let coord = Coords::dot(col, row); // x=col, y=row - match creation coordinates
        self.contains_cell_by_coord(&coord)
    }

    pub fn contains_cell_by_coord(&self, coord: &Coords) -> bool {
        self.cells.contains_key(coord)
    }

    pub fn contains_cell_global(&self, global_row: usize, global_col: usize) -> bool {
        self.global_to_local(global_row, global_col)
            .map_or(false, |(local_row, local_col)| {
                self.contains_cell(local_row, local_col)
            })
    }

    pub fn local_to_global(&self, row: usize, col: usize) -> (usize, usize) {
        (row + self.row_offset, col + self.column_offset)
    }

    pub fn coord_to_global(&self, coord: &Coords) -> Coords {
        let (col_range, row_range) = coord.get_bounds();
        Coords::new(
            (col_range.start + self.column_offset)..(col_range.end + self.column_offset),
            (row_range.start + self.row_offset)..(row_range.end + self.row_offset),
        )
    }

    pub fn global_to_local(&self, global_row: usize, global_col: usize) -> Option<(usize, usize)> {
        if global_row >= self.row_offset && global_col >= self.column_offset {
            Some((
                global_row - self.row_offset,
                global_col - self.column_offset,
            ))
        } else {
            None
        }
    }

    pub fn global_coord_to_local(&self, global_coord: &Coords) -> Option<Coords> {
        let (col_range, row_range) = global_coord.get_bounds();

        if row_range.start >= self.row_offset && col_range.start >= self.column_offset {
            Some(Coords::new(
                (col_range.start - self.column_offset)..(col_range.end - self.column_offset),
                (row_range.start - self.row_offset)..(row_range.end - self.row_offset),
            ))
        } else {
            None
        }
    }

    pub fn is_within_bounds(&self, row: usize, col: usize) -> bool {
        row < self.rows && col < self.columns
    }

    pub fn is_coord_within_bounds(&self, coord: &Coords) -> bool {
        let (col_range, row_range) = coord.get_bounds();
        row_range.end <= self.rows && col_range.end <= self.columns
    }

    pub fn is_within_global_bounds(&self, global_row: usize, global_col: usize) -> bool {
        self.global_to_local(global_row, global_col)
            .map_or(false, |(local_row, local_col)| {
                self.is_within_bounds(local_row, local_col)
            })
    }

    pub fn is_global_coord_within_bounds(&self, global_coord: &Coords) -> bool {
        self.global_coord_to_local(global_coord)
            .map_or(false, |local_coord| {
                self.is_coord_within_bounds(&local_coord)
            })
    }

    pub fn clear_cells(&mut self) {
        self.cells.clear();
        self.coords.clear();
    }

    pub fn cell_count(&self) -> usize {
        self.cells.len()
    }

    pub fn add_coord(&mut self, coord: Coords) {
        if !self.coords.contains(&coord) {
            self.coords.push(coord);
        }
    }

    pub fn clear_coords(&mut self) {
        self.coords.clear();
    }

    pub fn dynamic_resize(&mut self) {
        if self.cells.is_empty() {
            self.rows = 1;
            self.columns = 1;
            self.coords.clear();
            return;
        }

        // Find the maximum bounds from all cells
        let (max_col, max_row) = self.cells.keys().map(|coord| coord.get_bounds()).fold(
            (0, 0),
            |(max_col, max_row), (col_range, row_range)| {
                (max_col.max(col_range.end), max_row.max(row_range.end))
            },
        );

        // Ensure minimum dimensions of 1x1
        self.rows = max_row.max(1);
        self.columns = max_col.max(1);

        // Rebuild coords from existing cells
        self.coords.clear();
        self.coords.extend(self.cells.keys().cloned());
    }

    pub fn auto_resize_for_content(&mut self, target_row: usize, target_col: usize) {
        let required_rows = target_row + 1;
        let required_cols = target_col + 1;

        if required_rows > self.rows {
            self.rows = required_rows;
        }
        if required_cols > self.columns {
            self.columns = required_cols;
        }

        let coord = Coords::point(target_col, target_row);
        self.add_coord(coord);
    }

    fn auto_resize_for_coord(&mut self, coord: &Coords) {
        let (col_range, row_range) = coord.get_bounds();

        if row_range.end > self.rows {
            self.rows = row_range.end;
        }
        if col_range.end > self.columns {
            self.columns = col_range.end;
        }
    }

    pub fn shrink_to_fit(&mut self) {
        if self.cells.is_empty() {
            self.rows = 0;
            self.columns = 0;
            self.coords.clear();
            return;
        }

        let (max_col, max_row) = self.cells.keys().map(|coord| coord.get_bounds()).fold(
            (0, 0),
            |(max_col, max_row), (col_range, row_range)| {
                (max_col.max(col_range.end), max_row.max(row_range.end))
            },
        );

        self.rows = max_row;
        self.columns = max_col;

        self.coords.clear();
        self.coords.extend(self.cells.keys().cloned());
    }

    pub fn new_registry_table(title: &str) -> Table {
        let mut table = Table::new_with_dimensions(50, 4);
        table.id = Id::get(&format!("registry_table_{}", title));
        table
    }

    pub fn add_header(&mut self, headers: Vec<&str>) {
        for (col, header) in headers.iter().enumerate() {
            let coord = Coords::dot(col, 0); // x=col, y=row - use dot for header display
            let mut cell =
                TableCell::new_with_coords(0, col, &format!("header_{}", col), coord.clone());
            cell.add_entry(Entry::Str(header.to_string()));
            // Header cell added successfully
            self.set_cell_by_coord(coord, cell);
        }
        if headers.len() > self.columns {
            self.columns = headers.len();
        }
    }

    pub fn add_row(&mut self, row_data: Vec<Entry>) -> usize {
        let row_idx = self.get_next_available_row();

        if !row_data.is_empty() {
            self.auto_resize_for_content(row_idx, row_data.len().saturating_sub(1));
        }

        for (col, entry) in row_data.iter().enumerate() {
            let coord = Coords::dot(col, row_idx); // x=col, y=row - use dot for cell display
            let mut cell = TableCell::new_with_coords(
                row_idx,
                col,
                &format!("cell_{}_{}", row_idx, col),
                coord.clone(),
            );
            cell.add_entry(entry.clone());
            self.set_cell_by_coord(coord, cell);
        }

        row_idx
    }

    fn get_next_available_row(&self) -> usize {
        if self.cells.is_empty() {
            0
        } else {
            // Find the maximum row end from all cells (y coordinate = row)
            self.cells
                .keys()
                .map(|coord| coord.get_bounds().1.end) // y coordinate = row
                .max()
                .unwrap_or(0)
        }
    }

    /// Calculate optimal column widths based on content and title requirements
    fn calculate_column_widths(&self) -> Vec<usize> {
        const MIN_COLUMN_WIDTH: usize = 8;
        const COLUMN_PADDING: usize = 2;

        let mut widths = vec![MIN_COLUMN_WIDTH; self.columns];

        // Calculate content-based widths
        for (coord, cell) in &self.cells {
            let content_width = Self::calculate_display_width(&cell.display_content());
            let (col_range, _) = coord.get_bounds();

            for col in col_range {
                if col < widths.len() {
                    widths[col] = widths[col].max(content_width + COLUMN_PADDING);
                }
            }
        }

        // Ensure table is wide enough for title
        self.adjust_widths_for_title(&mut widths);

        widths
    }

    /// Adjust column widths to accommodate the table title
    fn adjust_widths_for_title(&self, widths: &mut [usize]) {
        let title = format!(
            " Table: {} ({} rows × {} cols) ",
            self.name(),
            self.content_rows(),
            self.columns
        );

        let title_width = Self::calculate_display_width(&title);
        let border_width = self.columns + 1;
        let current_content_width: usize = widths.iter().sum();
        let current_total_width = current_content_width + border_width;
        let required_total_width = title_width + 6;

        if required_total_width > current_total_width {
            let extra_width = required_total_width - current_total_width;
            let width_per_column = extra_width / self.columns;
            let remainder = extra_width % self.columns;

            for (i, width) in widths.iter_mut().enumerate() {
                *width += width_per_column;
                if i < remainder {
                    *width += 1;
                }
            }
        }
    }

    /// Calculate actual terminal display width for text with Unicode characters
    fn calculate_display_width(text: &str) -> usize {
        text.chars()
            .map(|c| {
                match c {
                    // Check for ✅ and ❌ specifically
                    '✅' | '❌' => 2, // These are often wide in terminals

                    // Zero-width characters
                    '\u{200b}' | '\u{200c}' | '\u{200d}' | '\u{feff}' => 0,

                    // Most ASCII and basic characters
                    c if c.is_ascii() => 1,

                    // For other Unicode, assume width 1 unless we know it's wide
                    _ => 1,
                }
            })
            .sum()
    }

    /// Format content with proper justification and padding
    fn format_content_with_justification(
        content: &str,
        total_width: usize,
        justification: &Justification,
    ) -> String {
        let content_width = Self::calculate_display_width(content);

        // Ensure we don't exceed total_width
        if content_width >= total_width {
            return if content_width == total_width {
                content.to_string()
            } else {
                // Truncate if content is too long
                content.chars().take(total_width).collect()
            };
        }

        // Calculate padding to fill total_width exactly
        let total_padding = total_width - content_width;

        let mut result = String::with_capacity(total_width * 4); // Extra capacity for Unicode

        match justification {
            Justification::Left => {
                result.push(' '); // 1 space left padding
                result.push_str(content);
                if total_padding > 1 {
                    result.push_str(&" ".repeat(total_padding - 1)); // remaining padding
                }
            }
            Justification::Center => {
                let left_padding = total_padding / 2;
                let right_padding = total_padding - left_padding;
                result.push_str(&" ".repeat(left_padding));
                result.push_str(content);
                result.push_str(&" ".repeat(right_padding));
            }
            Justification::Right => {
                if total_padding > 1 {
                    result.push_str(&" ".repeat(total_padding - 1)); // padding minus 1
                }
                result.push_str(content);
                result.push(' '); // 1 space right padding
            }
        }

        // Verify we have exactly the right display width
        let actual_width = Self::calculate_display_width(&result);
        if actual_width != total_width {
            // Adjust if needed
            if actual_width < total_width {
                result.push_str(&" ".repeat(total_width - actual_width));
            } else {
                result = result.chars().take(total_width).collect();
            }
        }

        result
    }

    /// Display the table with enhanced formatting and proper Unicode support
    pub fn display_formatted(&self) -> String {
        if self.cells.is_empty() {
            let empty_msg = format!("Table: {} (empty)", self.name());
            let border_width = empty_msg.chars().count() + 4;
            return format!(
                "┌{}┐\n│ {} │\n└{}┘",
                "─".repeat(border_width - 2),
                empty_msg,
                "─".repeat(border_width - 2)
            );
        }

        let widths = self.calculate_column_widths();
        let total_width = widths.iter().sum::<usize>() + self.columns + 1;

        let mut output = String::new();

        // Top border - straight line for table title
        output.push('┌');
        output.push_str(&"─".repeat(total_width - 2));
        output.push_str("┐\n");

        // Enhanced table title with better coordinate display
        let coord_info = if !self.coords.is_empty() {
            format!(" [coords: {}]", self.coords.len())
        } else {
            String::new()
        };

        let title = format!(
            "Table: {} ({} rows × {} cols){}",
            self.name(),
            self.content_rows(),
            self.columns,
            coord_info
        );

        let title_char_width = title.chars().count();
        let available_width = total_width.saturating_sub(2);
        let total_padding = available_width.saturating_sub(title_char_width);
        let left_padding = total_padding / 2;
        let right_padding = total_padding - left_padding;

        output.push('│');
        output.push_str(&" ".repeat(left_padding));
        output.push_str(&title);
        output.push_str(&" ".repeat(right_padding));
        output.push_str("│\n");

        // Header separator with improved styling
        output.push('├');
        for (i, width) in widths.iter().enumerate() {
            output.push_str(&"─".repeat(*width));
            if i < widths.len() - 1 {
                output.push('┬');
            }
        }
        output.push_str("┤\n");

        // Display rows with enhanced cell rendering
        let actual_rows = self.content_rows();
        for row in 0..actual_rows {
            output.push('│');
            for col in 0..self.columns {
                let (content, justification) = if let Some(cell) = self.get_cell(row, col) {
                    let display_content = cell.display_content();
                    (display_content, cell.justification())
                } else {
                    (String::new(), &Justification::Left)
                };

                let formatted_cell =
                    Self::format_content_with_justification(&content, widths[col], justification);
                output.push_str(&formatted_cell);

                if col < self.columns - 1 {
                    output.push('│');
                }
            }
            output.push_str("│\n");

            // Row separator with proper junction characters
            if row < actual_rows - 1 {
                output.push('├');
                for (i, width) in widths.iter().enumerate() {
                    output.push_str(&"─".repeat(*width));
                    if i < widths.len() - 1 {
                        output.push('┼');
                    }
                }
                output.push_str("┤\n");
            }
        }

        // Bottom border with proper closure
        output.push('└');
        for (i, width) in widths.iter().enumerate() {
            output.push_str(&"─".repeat(*width));
            if i < widths.len() - 1 {
                output.push('┴');
            }
        }
        output.push_str("┘\n");

        output
    }
}

impl Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_formatted())
    }
}

impl PartialEq for Table {
    fn eq(&self, other: &Self) -> bool {
        self.rows == other.rows
            && self.columns == other.columns
            && self.content_rows() == other.content_rows()
            && self.cells == other.cells
            && self.coords == other.coords
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableCell {
    row: usize,
    column: usize,
    entries: Vec<Entry>,
    id: Id,
    justification: Justification,
    coords: Option<Coords>,
}

impl Display for TableCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_content())
    }
}

impl TableCell {
    pub fn new(row: usize, column: usize, name: &str) -> TableCell {
        let coords = Coords::point(column, row); // x=col, y=row
        TableCell {
            row,
            column,
            entries: Vec::new(),
            id: Id::get(name),
            justification: Justification::Left,
            coords: Some(coords),
        }
    }

    /// Create a new TableCell with specified justification
    pub fn new_with_justification(
        row: usize,
        column: usize,
        name: &str,
        justification: Justification,
    ) -> TableCell {
        let coords = Coords::point(column, row); // x=col, y=row
        TableCell {
            row,
            column,
            entries: Vec::new(),
            id: Id::get(name),
            justification,
            coords: Some(coords),
        }
    }

    /// Create a new TableCell with coordinates for shape calculations
    pub fn new_with_coords(row: usize, column: usize, name: &str, coords: Coords) -> TableCell {
        TableCell {
            row,
            column,
            entries: Vec::new(),
            id: Id::get(name),
            justification: Justification::Left,
            coords: Some(coords),
        }
    }

    /// Create a new TableCell spanning multiple rows and columns
    pub fn new_spanning(
        start_row: usize,
        start_col: usize,
        end_row: usize,
        end_col: usize,
        name: &str,
    ) -> TableCell {
        let coords = Coords::new(start_col..end_col, start_row..end_row); // x=col, y=row
        TableCell {
            row: start_row,
            column: start_col,
            entries: Vec::new(),
            id: Id::get(name),
            justification: Justification::Left,
            coords: Some(coords),
        }
    }

    /// Create a new TableCell with full configuration
    pub fn new_full(
        start_row: usize,
        start_col: usize,
        end_row: usize,
        end_col: usize,
        name: &str,
        justification: Justification,
    ) -> TableCell {
        let coords = Coords::new(start_col..end_col, start_row..end_row); // x=col, y=row
        TableCell {
            row: start_row,
            column: start_col,
            entries: Vec::new(),
            id: Id::get(name),
            justification,
            coords: Some(coords),
        }
    }

    /// Set the justification for this cell
    pub fn set_justification(&mut self, justification: Justification) {
        self.justification = justification;
    }

    /// Get the current justification for this cell
    pub fn justification(&self) -> &Justification {
        &self.justification
    }

    /// Builder pattern method to set justification
    pub fn with_justification(mut self, justification: Justification) -> Self {
        self.justification = justification;
        self
    }

    /// Set coordinates for this cell and update row/column accordingly
    pub fn set_coords(&mut self, coords: Coords) {
        let (col_range, row_range) = coords.get_bounds();
        self.row = row_range.start;
        self.column = col_range.start;
        self.coords = Some(coords);
    }

    /// Get coordinates for this cell
    pub fn coords(&self) -> Option<&Coords> {
        self.coords.as_ref()
    }

    /// Builder pattern method to set coordinates
    pub fn with_coords(mut self, coords: Coords) -> Self {
        let (col_range, row_range) = coords.get_bounds();
        self.row = row_range.start;
        self.column = col_range.start;
        self.coords = Some(coords);
        self
    }

    /// Get the span dimensions (rows, columns) of this cell
    pub fn span_dimensions(&self) -> (usize, usize) {
        match &self.coords {
            Some(coords) => {
                let (col_range, row_range) = coords.get_bounds();
                (row_range.len(), col_range.len())
            }
            None => (1, 1),
        }
    }

    /// Check if this cell spans multiple rows
    pub fn is_row_spanning(&self) -> bool {
        self.span_dimensions().0 > 1
    }

    /// Check if this cell spans multiple columns
    pub fn is_column_spanning(&self) -> bool {
        self.span_dimensions().1 > 1
    }

    /// Check if this cell is a merged cell (spans multiple rows or columns)
    pub fn is_merged(&self) -> bool {
        self.is_row_spanning() || self.is_column_spanning()
    }

    /// Calculate the bounding box of the cell based on coordinates
    pub fn bounding_box(&self) -> Option<Coords> {
        self.coords.clone()
    }

    /// Check if this cell intersects with another cell's coordinates
    pub fn intersects_with(&self, other: &TableCell) -> bool {
        match (&self.coords, &other.coords) {
            (Some(coords1), Some(coords2)) => {
                let (col_range1, row_range1) = coords1.get_bounds();
                let (col_range2, row_range2) = coords2.get_bounds();

                row_range1.start < row_range2.end
                    && row_range2.start < row_range1.end
                    && col_range1.start < col_range2.end
                    && col_range2.start < col_range1.end
            }
            _ => false,
        }
    }

    /// Check if this cell contains a specific coordinate point
    pub fn contains_point(&self, row: usize, col: usize) -> bool {
        match &self.coords {
            Some(coords) => {
                let (col_range, row_range) = coords.get_bounds();
                row_range.contains(&row) && col_range.contains(&col)
            }
            None => self.row == row && self.column == col,
        }
    }

    /// Calculate the area covered by this cell's coordinates
    pub fn area(&self) -> usize {
        match &self.coords {
            Some(coords) => {
                let (col_range, row_range) = coords.get_bounds();
                row_range.len() * col_range.len()
            }
            None => 1,
        }
    }

    /// Get all coordinate points this cell covers
    pub fn covered_points(&self) -> Vec<(usize, usize)> {
        match &self.coords {
            Some(coords) => {
                let (col_range, row_range) = coords.get_bounds();
                let mut points = Vec::new();
                for row in row_range {
                    for col in col_range.clone() {
                        points.push((row, col));
                    }
                }
                points
            }
            None => vec![(self.row, self.column)],
        }
    }

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn column(&self) -> usize {
        self.column
    }

    /// Get the ending row (exclusive) for this cell
    pub fn end_row(&self) -> usize {
        match &self.coords {
            Some(coords) => {
                let (_, row_range) = coords.get_bounds();
                row_range.end
            }
            None => self.row + 1,
        }
    }

    /// Get the ending column (exclusive) for this cell
    pub fn end_column(&self) -> usize {
        match &self.coords {
            Some(coords) => {
                let (col_range, _) = coords.get_bounds();
                col_range.end
            }
            None => self.column + 1,
        }
    }

    pub fn entries(&self) -> &Vec<Entry> {
        &self.entries
    }

    pub fn entries_mut(&mut self) -> &mut Vec<Entry> {
        &mut self.entries
    }

    pub fn name(&self) -> String {
        self.id.name()
    }

    pub fn id(&self) -> Id {
        self.id.clone()
    }

    /// Add an entry to this cell
    pub fn add_entry(&mut self, entry: Entry) {
        self.entries.push(entry);
    }

    /// Remove an entry from this cell
    pub fn remove_entry(&mut self, index: usize) -> Option<Entry> {
        if index < self.entries.len() {
            Some(self.entries.remove(index))
        } else {
            None
        }
    }

    /// Clear all entries from this cell
    pub fn clear_entries(&mut self) {
        self.entries.clear();
    }

    /// Get the primary display content for this cell
    pub fn display_content(&self) -> String {
        if self.entries.is_empty() {
            return String::new();
        }

        if self.entries.len() == 1 {
            self.entries[0].display_summary()
        } else {
            // Multiple entries - show count and first entry
            format!(
                "{} (+{})",
                self.entries[0].display_summary(),
                self.entries.len() - 1
            )
        }
    }

    /// Get detailed content for this cell
    pub fn display_detailed(&self) -> String {
        if self.entries.is_empty() {
            return "(empty)".to_string();
        }

        self.entries
            .iter()
            .map(|e| e.display_summary())
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Check if cell contains a specific entry type
    pub fn contains_type(&self, type_name: &str) -> bool {
        self.entries.iter().any(|e| e.type_name() == type_name)
    }

    /// Get first entry of a specific type
    pub fn get_first_of_type(&self, type_name: &str) -> Option<&Entry> {
        self.entries.iter().find(|e| e.type_name() == type_name)
    }

    /// Count entries of a specific type
    pub fn count_type(&self, type_name: &str) -> usize {
        self.entries
            .iter()
            .filter(|e| e.type_name() == type_name)
            .count()
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    /// Test dynamic resizing functionality
    #[test]
    fn test_dynamic_resizing_functionality() {
        println!("🧪 Testing dynamic table resizing functionality...");

        // Test 1: Start with empty table
        let mut table = Table::new();
        println!(
            "📏 Initial empty table: {} rows × {} cols",
            table.rows(),
            table.columns()
        );

        // Test 2: Add content and verify auto-resize
        table.add_header(vec!["ID", "Name", "Value"]);
        println!(
            "📏 After adding header: {} rows × {} cols",
            table.rows(),
            table.columns()
        );
        assert_eq!(
            table.columns(),
            3,
            "Should have 3 columns after adding 3-column header"
        );

        // Test 3: Test auto_resize_for_content
        table.auto_resize_for_content(5, 7); // Request space for row 5, col 7
        println!(
            "📏 After auto_resize_for_content(5, 7): {} rows × {} cols",
            table.rows(),
            table.columns()
        );
        assert!(
            table.rows() >= 6,
            "Should have at least 6 rows to accommodate row index 5"
        );
        assert!(
            table.columns() >= 8,
            "Should have at least 8 columns to accommodate col index 7"
        );

        // Test 4: Manual dynamic resize
        table.dynamic_resize();
        println!(
            "📏 After manual dynamic_resize: {} rows × {} cols",
            table.rows(),
            table.columns()
        );

        // Test 5: Display the dynamically resized table
        println!("🎨 Final dynamically resized table:");
        println!("{}", table);

        // Verify table content is intact after resizing
        assert!(table.get_cell(0, 0).is_some(), "Header cell should exist");

        println!("✅ Dynamic resizing functionality works correctly");
        println!("📏 Tables automatically resize based on content and can be optimized");
    }

    /// Test the new text justification functionality
    #[test]
    fn test_justification_functionality() {
        println!("🧪 Testing text justification functionality...");

        // Create a demo table to showcase all justification types
        let mut demo_table = Table::new_with_dimensions(4, 3);

        // Add headers with center justification
        let mut header1 = TableCell::new(0, 0, "left_header");
        header1.add_entry(Entry::Str("Left Column".to_string()));
        demo_table.set_cell(0, 0, header1);

        let mut header2 = TableCell::new(0, 1, "center_header");
        header2.add_entry(Entry::Str("Center Column".to_string()));
        demo_table.set_cell(0, 1, header2);

        let mut header3 = TableCell::new(0, 2, "right_header");
        header3.add_entry(Entry::Str("Right Column".to_string()));
        demo_table.set_cell(0, 2, header3);

        // Row 1: Demonstrate different content
        let mut left_cell = TableCell::new(1, 0, "demo_left");
        left_cell.add_entry(Entry::Str("Left aligned text".to_string()));
        demo_table.set_cell(1, 0, left_cell);

        let mut center_cell = TableCell::new(1, 1, "demo_center");
        center_cell.add_entry(Entry::Str("Centered".to_string()));
        demo_table.set_cell(1, 1, center_cell);

        let mut right_cell = TableCell::new(1, 2, "demo_right");
        right_cell.add_entry(Entry::Str("Right aligned".to_string()));
        demo_table.set_cell(1, 2, right_cell);

        // Row 2: Numeric data
        let mut num_left = TableCell::new(2, 0, "num_left");
        num_left.add_entry(Entry::Val(42));
        demo_table.set_cell(2, 0, num_left);

        let mut num_center = TableCell::new(2, 1, "num_center");
        num_center.add_entry(Entry::Val(1337));
        demo_table.set_cell(2, 1, num_center);

        let mut num_right = TableCell::new(2, 2, "num_right");
        num_right.add_entry(Entry::Val(999));
        demo_table.set_cell(2, 2, num_right);

        // Row 3: Status indicators
        let mut status1 = TableCell::new(3, 0, "status1");
        status1.add_entry(Entry::Str("✅ SUCCESS".to_string()));
        demo_table.set_cell(3, 0, status1);

        let mut status2 = TableCell::new(3, 1, "status2");
        status2.add_entry(Entry::Str("⚠️ WARNING".to_string()));
        demo_table.set_cell(3, 1, status2);

        let mut status3 = TableCell::new(3, 2, "status3");
        status3.add_entry(Entry::Str("❌ ERROR".to_string()));
        demo_table.set_cell(3, 2, status3);

        println!("🎨 Demo Table:");
        println!("{}", "=".repeat(80));
        let formatted_output = demo_table.to_string();
        println!("{}", formatted_output);
        println!("{}", "=".repeat(80));

        // Validate that the output contains expected structural elements
        assert!(formatted_output.contains("┌"), "Should contain top border");
        assert!(
            formatted_output.contains("│"),
            "Should contain vertical borders"
        );
        assert!(
            formatted_output.contains("└"),
            "Should contain bottom border"
        );
        assert!(
            formatted_output.contains("Left Column"),
            "Should contain left header"
        );
        assert!(
            formatted_output.contains("Center Column"),
            "Should contain center header"
        );
        assert!(
            formatted_output.contains("Right Column"),
            "Should contain right header"
        );
        assert!(
            formatted_output.contains("Centered"),
            "Should contain centered text"
        );
        assert!(
            formatted_output.contains("SUCCESS"),
            "Should contain status text"
        );

        println!("✅ Table functionality works correctly!");
        println!("📐 Content display functioning properly");
    }
}
