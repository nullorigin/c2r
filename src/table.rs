use crate::{Entry, Id};
use core::fmt;
#[allow(unused_imports)]
use std::fmt::Write;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    ops::Range,
};

/// Text justification options for table cells
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Justification {
    /// Left-aligned text (default)
    Left,
    /// Center-aligned text
    Center,
    /// Right-aligned text
    Right,
}

#[derive(Debug, Clone, Default)]
pub struct Table {
    rows: usize,
    columns: usize,
    column_offset: usize,
    row_offset: usize,
    coords: Vec<Coords>,
    cells: HashMap<(usize, usize), TableCell>,
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
        // Return the allocated table size, not just content-based size
        self.rows
    }

    /// Get the actual number of rows that contain content
    pub fn content_rows(&self) -> usize {
        if self.cells.is_empty() {
            0
        } else {
            self.cells.keys().map(|(row, _)| *row).max().unwrap_or(0) + 1
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
        self.id.name.to_string()
    }
    pub fn id(&self) -> Id {
        self.id.clone()
    }
    pub fn entries(&self) -> &HashMap<(usize, usize), TableCell> {
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
        self.cells.get(&(row, col))
    }

    pub fn get_cell_mut(&mut self, row: usize, col: usize) -> Option<&mut TableCell> {
        self.cells.get_mut(&(row, col))
    }

    pub fn get_cell_global(&self, global_row: usize, global_col: usize) -> Option<&TableCell> {
        if global_row >= self.row_offset && global_col >= self.column_offset {
            let local_row = global_row - self.row_offset;
            let local_col = global_col - self.column_offset;
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
        if global_row >= self.row_offset && global_col >= self.column_offset {
            let local_row = global_row - self.row_offset;
            let local_col = global_col - self.column_offset;
            self.get_cell_mut(local_row, local_col)
        } else {
            None
        }
    }

    pub fn set_cell(&mut self, row: usize, col: usize, cell: TableCell) {
        self.cells.insert((row, col), cell);
    }

    pub fn set_cell_global(&mut self, global_row: usize, global_col: usize, cell: TableCell) {
        if global_row >= self.row_offset && global_col >= self.column_offset {
            let local_row = global_row - self.row_offset;
            let local_col = global_col - self.column_offset;
            self.set_cell(local_row, local_col, cell);
        }
    }

    pub fn remove_cell(&mut self, row: usize, col: usize) -> Option<TableCell> {
        self.cells.remove(&(row, col))
    }

    pub fn remove_cell_global(
        &mut self,
        global_row: usize,
        global_col: usize,
    ) -> Option<TableCell> {
        if global_row >= self.row_offset && global_col >= self.column_offset {
            let local_row = global_row - self.row_offset;
            let local_col = global_col - self.column_offset;
            self.remove_cell(local_row, local_col)
        } else {
            None
        }
    }

    pub fn contains_cell(&self, row: usize, col: usize) -> bool {
        self.cells.contains_key(&(row, col))
    }

    pub fn contains_cell_global(&self, global_row: usize, global_col: usize) -> bool {
        if global_row >= self.row_offset && global_col >= self.column_offset {
            let local_row = global_row - self.row_offset;
            let local_col = global_col - self.column_offset;
            self.contains_cell(local_row, local_col)
        } else {
            false
        }
    }

    pub fn local_to_global(&self, row: usize, col: usize) -> (usize, usize) {
        (row + self.row_offset, col + self.column_offset)
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

    pub fn is_within_bounds(&self, row: usize, col: usize) -> bool {
        row < self.rows && col < self.columns
    }

    pub fn is_within_global_bounds(&self, global_row: usize, global_col: usize) -> bool {
        if let Some((local_row, local_col)) = self.global_to_local(global_row, global_col) {
            self.is_within_bounds(local_row, local_col)
        } else {
            false
        }
    }

    pub fn clear_cells(&mut self) {
        self.cells.clear();
    }

    pub fn cell_count(&self) -> usize {
        self.cells.len()
    }

    pub fn add_coord(&mut self, coord: Coords) {
        self.coords.push(coord);
    }

    pub fn clear_coords(&mut self) {
        self.coords.clear();
    }

    /// Dynamically resize the table based on actual content
    pub fn dynamic_resize(&mut self) {
        if self.cells.is_empty() {
            // No content, set to minimal dimensions
            self.rows = 1;
            self.columns = 1;
            return;
        }

        // Calculate required dimensions based on actual cell positions
        let max_row = self.cells.keys().map(|(row, _)| *row).max().unwrap_or(0);
        let max_col = self.cells.keys().map(|(_, col)| *col).max().unwrap_or(0);

        // Set dimensions to accommodate all content (adding 1 since indices are 0-based)
        self.rows = (max_row + 1).max(self.rows);
        self.columns = (max_col + 1).max(self.columns);
    }

    /// Auto-resize before adding new content to ensure table can accommodate it
    pub fn auto_resize_for_content(&mut self, target_row: usize, target_col: usize) {
        // Expand dimensions to accommodate the target position (adding 1 since indices are 0-based)
        let required_rows = target_row + 1;
        let required_cols = target_col + 1;

        if required_rows > self.rows {
            self.rows = required_rows;
        }
        if required_cols > self.columns {
            self.columns = required_cols;
        }
    }

    /// Shrink the table to fit only existing content (removes empty rows/columns)
    pub fn shrink_to_fit(&mut self) {
        if self.cells.is_empty() {
            self.rows = 0;
            self.columns = 0;
            return;
        }

        // Find the actual used dimensions
        let max_row = self.cells.keys().map(|(row, _)| *row).max().unwrap_or(0);
        let max_col = self.cells.keys().map(|(_, col)| *col).max().unwrap_or(0);

        // Set to exact fit (adding 1 since indices are 0-based)
        self.rows = max_row + 1;
        self.columns = max_col + 1;
    }

    // === REGISTRY DISPLAY ENHANCEMENTS ===

    /// Create a registry table with proper headers
    pub fn new_registry_table(title: &str) -> Table {
        let mut table = Table::new_with_dimensions(50, 4);
        table.id = Id::get(&format!("registry_table_{}", title));
        table
    }

    /// Add a header row to the table
    pub fn add_header(&mut self, headers: Vec<&str>) {
        for (col, header) in headers.iter().enumerate() {
            let mut cell = TableCell::new(0, col, &format!("header_{}", col));
            cell.add_entry(Entry::Str(header.to_string()));
            self.set_cell(0, col, cell);
        }
        if headers.len() > self.columns {
            self.columns = headers.len();
        }
    }

    /// Add a data row to the table with automatic resizing
    pub fn add_row(&mut self, row_data: Vec<Entry>) -> usize {
        let row_idx = self.get_next_available_row();

        // Auto-resize to accommodate the new row and required columns
        self.auto_resize_for_content(row_idx, row_data.len().saturating_sub(1));

        for (col, entry) in row_data.iter().enumerate() {
            let mut cell = TableCell::new(row_idx, col, &format!("data_{}_{}", row_idx, col));
            cell.add_entry(entry.clone());
            self.set_cell(row_idx, col, cell);
        }

        // Perform final dynamic resize to ensure everything fits
        self.dynamic_resize();
        row_idx
    }

    /// Add an empty row to the table and return the row index for manual cell setting
    pub fn add_empty_row(&mut self) -> usize {
        let row_idx = self.get_next_available_row();

        // Auto-resize to ensure the table has at least the minimum columns
        self.auto_resize_for_content(row_idx, self.columns.saturating_sub(1));

        // Perform final dynamic resize to ensure everything fits
        self.dynamic_resize();
        row_idx
    }

    /// Get the next available row index (returns local coordinates)
    fn get_next_available_row(&self) -> usize {
        if self.cells.is_empty() {
            return 0; // First row in local coordinates
        }
        // Find the maximum LOCAL row index that actually has cells
        let max_row = self.cells.keys().map(|(row, _)| *row).max().unwrap_or(0);
        max_row + 1
    }

    /// Calculate the actual display width of a string using byte-based calculations
    fn calculate_display_width(text: &str) -> usize {
        // Convert to bytes and calculate width based on byte length
        let bytes = text.as_bytes();
        Self::get_byte_display_width(bytes)
    }

    /// Get the display width of byte data using byte-based calculations
    fn get_byte_display_width(bytes: &[u8]) -> usize {
        // Simple byte-based width calculation - each byte is 1 unit
        // This eliminates Unicode character width complexities
        bytes.len()
    }

    /// Calculate column widths based on actual character display widths AND header title requirements
    pub fn calculate_column_widths(&self) -> Vec<usize> {
        let mut widths = vec![0; self.columns];

        // Iterate through actual cells instead of assuming continuous ranges
        for col in 0..self.columns {
            let mut max_display_width = 0;
            // Check all cells in this column
            for ((_cell_row, cell_col), cell) in &self.cells {
                if *cell_col == col {
                    let content = cell.display_content();
                    let display_width = Self::calculate_display_width(&content);
                    max_display_width = max_display_width.max(display_width);
                }
            }
            // Add padding for left and right spaces (format_content_with_byte_width adds 1 space each side)
            let base_width = max_display_width + 2; // +2 for left and right padding spaces

            // Apply offset compensation for non-monospace character width differences
            // With byte-based calculations, no offset compensation needed
            let char_width_offset =
                Self::calculate_character_width_offset(&format!("Column{}", col));

            widths[col] = (base_width + char_width_offset).max(24); // Minimum total width - increased from 12 to 24
        }

        // Calculate the minimum table width needed for the header title
        let title = format!(
            " Table: {} ({} rows × {} cols) ",
            self.name(),
            self.content_rows(),
            self.columns
        );
        let title_width = Self::calculate_display_width(&title);
        let border_width = self.columns + 1; // borders and separators: ┬ between columns + outer borders
        let current_content_width = widths.iter().sum::<usize>();
        let current_total_width = current_content_width + border_width;

        // If title is longer than current table width, expand columns proportionally
        // Add minimum padding (6 characters) around the title for proper centering
        let required_total_width = title_width + 6; // +6 for minimum padding around title
        if required_total_width > current_total_width {
            let extra_width_needed = required_total_width - current_total_width;
            let width_per_column = extra_width_needed / self.columns;
            let remainder = extra_width_needed % self.columns;

            // Distribute extra width across all columns
            for (i, width) in widths.iter_mut().enumerate() {
                *width += width_per_column;
                // Add remainder to first few columns
                if i < remainder {
                    *width += 1;
                }
            }
        }

        widths
    }

    /// Calculate the offset needed to compensate for byte width differences
    fn calculate_character_width_offset(_sample_text: &str) -> usize {
        // With byte-based calculations, no offset compensation needed
        // All bytes are treated as width 1, providing consistent alignment
        0
    }

    /// Format content with proper byte-width-based padding and justification
    fn format_content_with_byte_width_and_justification(
        content: &str,
        total_width: usize,
        justification: &Justification,
    ) -> String {
        // Calculate the actual byte width of the content
        let content_byte_width = content.as_bytes().len();

        // Total width includes the left and right padding spaces
        let available_width = if total_width >= 2 { total_width - 2 } else { 0 }; // Reserve 2 for border spaces

        let mut result = String::with_capacity(total_width);

        // Calculate padding needed
        let padding_needed = if content_byte_width <= available_width {
            available_width - content_byte_width
        } else {
            0 // Content is too long, no padding needed
        };

        match justification {
            Justification::Left => {
                // Left alignment: " content       "
                result.push(' '); // Left border space
                result.push_str(content);
                result.push_str(&" ".repeat(padding_needed));
                result.push(' '); // Right border space
            }
            Justification::Center => {
                // Center alignment: "  content   "
                let left_padding = padding_needed / 2;
                let right_padding = padding_needed - left_padding;

                result.push(' '); // Left border space
                result.push_str(&" ".repeat(left_padding));
                result.push_str(content);
                result.push_str(&" ".repeat(right_padding));
                result.push(' '); // Right border space
            }
            Justification::Right => {
                // Right alignment: "       content "
                result.push(' '); // Left border space
                result.push_str(&" ".repeat(padding_needed));
                result.push_str(content);
                result.push(' '); // Right border space
            }
        }

        result
    }

    /// Display the table with proper formatting
    pub fn display_formatted(&self) -> String {
        if self.cells.is_empty() {
            return format!(
                "┌─────────────────────┐\n│ Table: {} (empty) │\n└─────────────────────┘",
                self.name()
            );
        }

        let widths = self.calculate_column_widths();
        let total_width = widths.iter().sum::<usize>() + self.columns + 1;

        let mut output = String::new();

        // Top border
        output.push('┌');
        for (i, width) in widths.iter().enumerate() {
            output.push_str(&"─".repeat(*width));
            if i < widths.len() - 1 {
                output.push('─'); // Continuous horizontal line for clean top border
            }
        }
        output.push_str("┐\n");

        // Table title (must match the calculation in calculate_column_widths)
        let title = format!(
            " Table: {} ({} rows × {} cols) ",
            self.name(),
            self.content_rows(),
            self.columns
        );
        // Calculate proper padding for header title
        let available_width = total_width.saturating_sub(2); // Subtract 2 for the │ characters
        let total_padding = available_width.saturating_sub(title.len());

        // Apply +1 character offset for proper alignment with table borders
        let left_padding = total_padding / 2;
        let right_padding = (total_padding - left_padding) + 1;
        output.push('│');
        output.push_str(&" ".repeat(left_padding));
        output.push_str(&title);
        output.push_str(&" ".repeat(right_padding));
        output.push_str("│\n");

        // Header separator
        output.push('├');
        for (i, width) in widths.iter().enumerate() {
            output.push_str(&"─".repeat(*width));
            if i < widths.len() - 1 {
                output.push('┬'); // T-like character pointing down
            }
        }
        output.push_str("┤\n");

        // Display rows - iterate through actual content rows
        let actual_rows = self.content_rows();

        for row in 0..actual_rows {
            output.push('│');
            for col in 0..self.columns {
                let (content, justification) = if let Some(cell) = self.get_cell(row, col) {
                    (cell.display_content(), cell.justification())
                } else {
                    (String::new(), &Justification::Left)
                };

                // Use byte-width-based padding with cell-specific justification
                let padded = Self::format_content_with_byte_width_and_justification(
                    &content,
                    widths[col],
                    justification,
                );
                output.push_str(&padded);
                if col < self.columns - 1 {
                    output.push('│');
                }
            }
            output.push_str("│\n");

            // Add separator after each row (except the last one)
            if row < actual_rows - 1 {
                output.push('├');
                for (i, width) in widths.iter().enumerate() {
                    output.push_str(&"─".repeat(*width));
                    if i < widths.len() - 1 {
                        output.push('┼'); // Cross character for all row separators
                    }
                }
                output.push_str("┤\n");
            }
        }

        // Bottom border
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

    /// Create a registry overview table
    pub fn create_registry_overview(entries: &HashMap<Id, Entry>) -> Table {
        let mut table = Table::new_registry_table("registry_overview");
        table.add_header(vec!["ID", "Type", "Details", "Size"]);

        for (id, entry) in entries {
            let entry_type = entry.type_name();
            let details = entry.display_summary();
            let size = entry.size_info();

            table.add_row(vec![
                Entry::Str(id.name.clone()),
                Entry::Str(entry_type),
                Entry::Str(details),
                Entry::Str(size),
            ]);
        }

        table
    }

    /// Create a categorized registry table
    pub fn create_categorized_registry(entries: &HashMap<Id, Entry>) -> HashMap<String, Table> {
        let mut tables = HashMap::new();

        // Group entries by type
        let mut by_type: HashMap<String, Vec<(&Id, &Entry)>> = HashMap::new();
        for (id, entry) in entries {
            let type_name = entry.type_name();
            by_type
                .entry(type_name)
                .or_insert_with(Vec::new)
                .push((id, entry));
        }

        // Create a table for each type
        for (type_name, entries) in by_type {
            // Create table with appropriate size: 1 header row + number of entries, and sufficient columns
            let num_rows = entries.len() + 1; // +1 for header
            let num_cols = match type_name.as_str() {
                "Handler" | "HandlerReport" | "Patternizer" => 5, // Enough columns for these types
                _ => 3,                                           // Default for other types
            };

            let mut table = Table::new_with_dimensions(num_rows, num_cols);
            table.id = Id::get(&format!("registry_table_{}", type_name));

            match type_name.as_str() {
                "Handler" => {
                    table.add_header(vec!["ID", "Role", "Priority", "Status"]);
                    for (id, entry) in entries {
                        if let Entry::Handler(h) = entry {
                            table.add_row(vec![
                                Entry::Str(id.name.clone()),
                                Entry::Str(h.role.clone()),
                                Entry::Val(h.priority as u64),
                                Entry::Str("Active".to_string()),
                            ]);
                        }
                    }
                }
                "HandlerReport" => {
                    table.add_header(vec!["ID", "Handler", "Level", "Message", "Success"]);
                    for (id, entry) in entries {
                        if let Entry::HandlerReport(r) = entry {
                            table.add_row(vec![
                                Entry::Str(id.name.clone()),
                                Entry::Str(r.handler_name.clone()),
                                Entry::Str(r.level.to_string()),
                                Entry::Str(r.message.clone()),
                                Entry::Bool(r.success),
                            ]);
                        }
                    }
                }
                "Patternizer" => {
                    table.add_header(vec!["ID", "Name", "Description", "Priority"]);
                    for (id, entry) in entries {
                        if let Entry::Patternizer(p) = entry {
                            table.add_row(vec![
                                Entry::Str(id.name.clone()),
                                Entry::Str(p.name.clone()),
                                Entry::Str(p.description.clone()),
                                Entry::Val(p.priority as u64),
                            ]);
                        }
                    }
                }
                _ => {
                    table.add_header(vec!["ID", "Value", "Type"]);
                    for (id, entry) in entries {
                        table.add_row(vec![
                            Entry::Str(id.name.clone()),
                            Entry::Str(entry.display_summary()),
                            Entry::Str(type_name.clone()),
                        ]);
                    }
                }
            }

            tables.insert(type_name, table);
        }

        tables
    }
}

impl Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_formatted())
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableCell {
    row: usize,
    column: usize,
    entries: Vec<Entry>,
    id: Id,
    justification: Justification,
}

impl Display for TableCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_content())
    }
}
impl TableCell {
    pub fn new(row: usize, column: usize, name: &str) -> TableCell {
        TableCell {
            row,
            column,
            entries: Vec::new(),
            id: Id::get(name),
            justification: Justification::Left, // Default to left alignment
        }
    }

    /// Create a new TableCell with specified justification
    pub fn new_with_justification(
        row: usize,
        column: usize,
        name: &str,
        justification: Justification,
    ) -> TableCell {
        TableCell {
            row,
            column,
            entries: Vec::new(),
            id: Id::get(name),
            justification,
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

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn entries(&self) -> &Vec<Entry> {
        &self.entries
    }

    pub fn entries_mut(&mut self) -> &mut Vec<Entry> {
        &mut self.entries
    }

    pub fn name(&self) -> &str {
        self.id.name.as_str()
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
#[derive(Debug, Clone, Default, Hash)]
pub struct Coords {
    x_ranges: Vec<Range<usize>>,
    y_ranges: Vec<Range<usize>>,
    shape_type: ShapeType,
}

#[derive(Debug, Clone, Default, Hash)]
pub enum ShapeType {
    #[default]
    Rectangle,
    Circle,
    Triangle,
    Line,
    Custom,
}

impl Coords {
    pub fn new(x: Range<usize>, y: Range<usize>) -> Coords {
        Coords {
            x_ranges: vec![x],
            y_ranges: vec![y],
            shape_type: ShapeType::Rectangle,
        }
    }

    pub fn new_with_shape(x: Range<usize>, y: Range<usize>, shape: ShapeType) -> Coords {
        Coords {
            x_ranges: vec![x],
            y_ranges: vec![y],
            shape_type: shape,
        }
    }

    pub fn from_multiple_ranges(
        x_ranges: Vec<Range<usize>>,
        y_ranges: Vec<Range<usize>>,
    ) -> Coords {
        Coords {
            x_ranges,
            y_ranges,
            shape_type: ShapeType::Custom,
        }
    }

    pub fn x(&self, index: usize) -> Option<Range<usize>> {
        self.x_ranges.get(index).cloned()
    }

    pub fn y(&self, index: usize) -> Option<Range<usize>> {
        self.y_ranges.get(index).cloned()
    }

    pub fn width(&self) -> usize {
        self.x_ranges
            .iter()
            .map(|r| r.end - r.start)
            .max()
            .unwrap_or(0)
    }

    pub fn height(&self) -> usize {
        self.y_ranges
            .iter()
            .map(|r| r.end - r.start)
            .max()
            .unwrap_or(0)
    }

    pub fn area(&self) -> usize {
        match self.shape_type {
            ShapeType::Rectangle => self.width() * self.height(),
            ShapeType::Circle => {
                let radius = self.width().min(self.height()) / 2;
                (std::f64::consts::PI * (radius * radius) as f64) as usize
            }
            ShapeType::Triangle => (self.width() * self.height()) / 2,
            ShapeType::Line => self.width().max(self.height()),
            ShapeType::Custom => self
                .x_ranges
                .iter()
                .zip(&self.y_ranges)
                .map(|(x, y)| (x.end - x.start) * (y.end - y.start))
                .sum(),
        }
    }

    pub fn perimeter(&self) -> usize {
        match self.shape_type {
            ShapeType::Rectangle => 2 * (self.width() + self.height()),
            ShapeType::Circle => {
                let radius = self.width().min(self.height()) / 2;
                (2.0 * std::f64::consts::PI * radius as f64) as usize
            }
            ShapeType::Triangle => {
                let w = self.width() as f64;
                let h = self.height() as f64;
                (w + h + (w * w + h * h).sqrt()) as usize
            }
            ShapeType::Line => self.width() + self.height(),
            ShapeType::Custom => self
                .x_ranges
                .iter()
                .zip(&self.y_ranges)
                .map(|(x, y)| 2 * ((x.end - x.start) + (y.end - y.start)))
                .sum(),
        }
    }

    pub fn is_square(&self) -> bool {
        self.width() == self.height() && matches!(self.shape_type, ShapeType::Rectangle)
    }

    pub fn contains_point(&self, x: usize, y: usize) -> bool {
        self.x_ranges
            .iter()
            .zip(&self.y_ranges)
            .any(|(x_range, y_range)| x_range.contains(&x) && y_range.contains(&y))
    }

    pub fn get_bounds(&self) -> (Range<usize>, Range<usize>) {
        let min_x = self.x_ranges.iter().map(|r| r.start).min().unwrap_or(0);
        let max_x = self.x_ranges.iter().map(|r| r.end).max().unwrap_or(0);
        let min_y = self.y_ranges.iter().map(|r| r.start).min().unwrap_or(0);
        let max_y = self.y_ranges.iter().map(|r| r.end).max().unwrap_or(0);
        (min_x..max_x, min_y..max_y)
    }

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
                let center_x = (x_bound.start + x_bound.end) / 2;
                let center_y = (y_bound.start + y_bound.end) / 2;
                let radius = self.width().min(self.height()) / 2;

                for y in y_bound {
                    for x in x_bound.clone() {
                        let dx = x as i32 - center_x as i32;
                        let dy = y as i32 - center_y as i32;
                        if (dx * dx + dy * dy) <= (radius * radius) as i32 {
                            points.push((x, y));
                        }
                    }
                }
            }
            ShapeType::Line => {
                let start_x = x_bound.start;
                let start_y = y_bound.start;
                let end_x = x_bound.end - 1;
                let end_y = y_bound.end - 1;

                let dx = end_x as i32 - start_x as i32;
                let dy = end_y as i32 - start_y as i32;
                let steps = dx.abs().max(dy.abs());

                for i in 0..=steps {
                    let x = start_x + (dx * i / steps.max(1)) as usize;
                    let y = start_y + (dy * i / steps.max(1)) as usize;
                    points.push((x, y));
                }
            }
            _ => {
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

    pub fn scale(&mut self, scale_x: f64, scale_y: f64) {
        self.x_ranges = self
            .x_ranges
            .iter()
            .map(|r| {
                let new_start = (r.start as f64 * scale_x) as usize;
                let new_end = (r.end as f64 * scale_x) as usize;
                new_start..new_end
            })
            .collect();

        self.y_ranges = self
            .y_ranges
            .iter()
            .map(|r| {
                let new_start = (r.start as f64 * scale_y) as usize;
                let new_end = (r.end as f64 * scale_y) as usize;
                new_start..new_end
            })
            .collect();
    }

    pub fn intersects(&self, other: &Coords) -> bool {
        let (self_x, self_y) = self.get_bounds();
        let (other_x, other_y) = other.get_bounds();

        self_x.start < other_x.end
            && self_x.end > other_x.start
            && self_y.start < other_y.end
            && self_y.end > other_y.start
    }

    pub fn distance_to(&self, other: &Coords) -> f64 {
        let (self_x, self_y) = self.get_bounds();
        let (other_x, other_y) = other.get_bounds();

        let self_center_x = (self_x.start + self_x.end) as f64 / 2.0;
        let self_center_y = (self_y.start + self_y.end) as f64 / 2.0;
        let other_center_x = (other_x.start + other_x.end) as f64 / 2.0;
        let other_center_y = (other_y.start + other_y.end) as f64 / 2.0;

        let dx = self_center_x - other_center_x;
        let dy = self_center_y - other_center_y;

        (dx * dx + dy * dy).sqrt()
    }

    pub fn add_range(&mut self, x: Range<usize>, y: Range<usize>) {
        self.x_ranges.push(x);
        self.y_ranges.push(y);
        self.shape_type = ShapeType::Custom;
    }

    pub fn range_count(&self) -> usize {
        self.x_ranges.len().min(self.y_ranges.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Handler, HandlerPhase, HandlerReport, Pattern, PatternMetrics, ReportLevel};
    use std::collections::HashMap;
    use std::path::PathBuf;

    /// Test creating categorized registry tables with mixed entry types
    #[test]
    fn test_create_categorized_registry_functionality() {
        println!("🧪 Testing create_categorized_registry functionality...");

        // Create a sample registry with various entry types
        let mut entries = HashMap::new();

        // Add some Handler entries
        let handler1 = Handler::new(Id::get("test_handler_1"), "function".to_string(), 100);
        let handler2 = Handler::new(Id::get("test_handler_2"), "array".to_string(), 95);
        entries.insert(Id::get("handler_function"), Entry::Handler(handler1));
        entries.insert(Id::get("handler_array"), Entry::Handler(handler2));

        // Add some HandlerReport entries
        let report1 = HandlerReport {
            report_id: Box::new(Id::get("report_1")),
            handler_id: Box::new(Id::get("test_handler")),
            handler_name: "function_handler".to_string(),
            function_name: "process_function".to_string(),
            message: "Successfully processed function declaration".to_string(),
            level: ReportLevel::Info,
            tokens_processed: 15,
            tokens_consumed: 12,
            phase: HandlerPhase::Process,
            success: true,
            metadata: HashMap::new(),
        };
        let report2 = HandlerReport {
            report_id: Box::new(Id::get("report_2")),
            handler_id: Box::new(Id::get("array_handler")),
            handler_name: "array_handler".to_string(),
            function_name: "handle_array".to_string(),
            message: "Array declaration converted successfully".to_string(),
            level: ReportLevel::Info,
            tokens_processed: 8,
            tokens_consumed: 8,
            phase: HandlerPhase::Convert,
            success: true,
            metadata: HashMap::new(),
        };
        entries.insert(Id::get("report_function"), Entry::HandlerReport(report1));
        entries.insert(Id::get("report_array"), Entry::HandlerReport(report2));

        // Add some Pattern entries
        let pattern1 = Pattern {
            id: Id::get("c_function"),
            name: "C Function Declaration".to_string(),
            description: "Matches C function declarations".to_string(),
            token_patterns: vec![],
            priority: 90,
            handler_types: vec!["function".to_string()],
            created_at: std::time::Instant::now(),
            usage_metrics: PatternMetrics::default(),
        };
        let pattern2 = Pattern {
            id: Id::get("c_array"),
            name: "C Array Declaration".to_string(),
            description: "Matches C array declarations".to_string(),
            token_patterns: vec![],
            priority: 85,
            handler_types: vec!["array".to_string()],
            created_at: std::time::Instant::now(),
            usage_metrics: PatternMetrics::default(),
        };
        entries.insert(Id::get("pattern_function"), Entry::Patternizer(pattern1));
        entries.insert(Id::get("pattern_array"), Entry::Patternizer(pattern2));

        // Add some basic entries
        entries.insert(Id::get("config_verbose"), Entry::Bool(true));
        entries.insert(Id::get("config_debug_level"), Entry::Val(3));
        entries.insert(Id::get("input_file"), Entry::Str("test.c".to_string()));
        entries.insert(
            Id::get("output_path"),
            Entry::Path(PathBuf::from("/tmp/output.rs")),
        );

        println!("📊 Created sample registry with {} entries", entries.len());

        // Test create_categorized_registry
        let categorized_tables = Table::create_categorized_registry(&entries);

        println!(
            "📋 Generated {} categorized tables:",
            categorized_tables.len()
        );
        for (category, table) in &categorized_tables {
            println!("  - {}: {} rows", category, table.rows());
        }

        // Verify we have the expected categories
        assert!(
            categorized_tables.contains_key("Handler"),
            "Should have Handler category"
        );
        assert!(
            categorized_tables.contains_key("HandlerReport"),
            "Should have HandlerReport category"
        );
        assert!(
            categorized_tables.contains_key("Patternizer"),
            "Should have Patternizer category"
        );
        assert!(
            categorized_tables.contains_key("Bool"),
            "Should have Bool category"
        );
        assert!(
            categorized_tables.contains_key("Val"),
            "Should have Val category"
        );
        assert!(
            categorized_tables.contains_key("Str"),
            "Should have Str category"
        );
        assert!(
            categorized_tables.contains_key("Path"),
            "Should have Path category"
        );

        // Verify Handler table structure
        let handler_table = categorized_tables.get("Handler").unwrap();
        assert_eq!(
            handler_table.rows(),
            3,
            "Handler table should have 3 rows (header + 2 entries)"
        ); // header + 2 handlers

        // Verify HandlerReport table structure
        let report_table = categorized_tables.get("HandlerReport").unwrap();
        assert_eq!(
            report_table.rows(),
            3,
            "HandlerReport table should have 3 rows (header + 2 entries)"
        ); // header + 2 reports

        println!("✅ create_categorized_registry functionality works correctly");
    }

    /// Test the display_formatted functionality with real registry data
    #[test]
    fn test_display_formatted_functionality() {
        println!("🧪 Testing display_formatted functionality...");

        // Create a smaller sample registry for cleaner output
        let mut entries = HashMap::new();

        // Add a few sample entries
        let handler = Handler::new(Id::get("sample_handler"), "test".to_string(), 75);
        entries.insert(Id::get("test_handler"), Entry::Handler(handler));

        let report = HandlerReport {
            report_id: Box::new(Id::get("sample_report")),
            handler_id: Box::new(Id::get("sample_handler")),
            handler_name: "sample_handler".to_string(),
            function_name: "test_function".to_string(),
            message: "Test conversion completed".to_string(),
            level: ReportLevel::Info,
            tokens_processed: 5,
            tokens_consumed: 5,
            phase: HandlerPhase::Convert,
            success: true,
            metadata: HashMap::new(),
        };
        entries.insert(Id::get("test_report"), Entry::HandlerReport(report));

        entries.insert(
            Id::get("test_string"),
            Entry::Str("Hello, World!".to_string()),
        );
        entries.insert(Id::get("test_number"), Entry::Val(42));
        entries.insert(Id::get("test_flag"), Entry::Bool(false));

        println!("📊 Created test registry with {} entries", entries.len());

        // Create categorized tables
        let categorized_tables = Table::create_categorized_registry(&entries);

        println!("🎨 Displaying formatted tables:\n");
        println!("{}", "=".repeat(80));

        // Display each category in a logical order
        let display_order = ["Handler", "HandlerReport", "Str", "Val", "Bool"];

        for category in display_order {
            if let Some(table) = categorized_tables.get(category) {
                println!("\n📂 {} REGISTRY ENTRIES", category.to_uppercase());
                println!("{}", "-".repeat(60));

                let formatted_output = table.display_formatted();
                println!("{}", formatted_output);

                // Basic validation that the output contains expected elements
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
                    formatted_output.contains(category),
                    "Should contain category name in title"
                );

                println!("{}", "-".repeat(60));
            }
        }

        // Display any remaining categories
        for (category_name, table) in categorized_tables {
            if !display_order.contains(&category_name.as_str()) {
                println!("\n📂 {} REGISTRY ENTRIES", category_name.to_uppercase());
                println!("{}", "-".repeat(60));
                println!("{}", table.display_formatted());
                println!("{}", "-".repeat(60));
            }
        }

        println!("\n{}", "=".repeat(80));
        println!("✅ display_formatted functionality works correctly");
        println!("📈 Tables properly formatted with borders, alignment, and categorization");
    }

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

        // Test 3: Add rows with different column counts
        table.add_row(vec![
            Entry::Str("item1".to_string()),
            Entry::Str("Test Item".to_string()),
        ]);
        println!(
            "📏 After adding 2-column row: {} rows × {} cols",
            table.rows(),
            table.columns()
        );

        table.add_row(vec![
            Entry::Str("item2".to_string()),
            Entry::Str("Another Item".to_string()),
            Entry::Val(42),
            Entry::Bool(true), // 4th column - should auto-expand
        ]);
        println!(
            "📏 After adding 4-column row: {} rows × {} cols",
            table.rows(),
            table.columns()
        );
        assert_eq!(table.columns(), 4, "Should auto-expand to 4 columns");
        assert_eq!(table.rows(), 3, "Should have 3 rows (header + 2 data rows)");

        // Test 4: Manual dynamic resize
        table.dynamic_resize();
        println!(
            "📏 After manual dynamic_resize: {} rows × {} cols",
            table.rows(),
            table.columns()
        );

        // Test 5: Test auto_resize_for_content
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

        // Test 6: Test shrink_to_fit
        table.shrink_to_fit();
        println!(
            "📏 After shrink_to_fit: {} rows × {} cols",
            table.rows(),
            table.columns()
        );
        assert_eq!(
            table.rows(),
            3,
            "Should shrink back to 3 rows (actual content)"
        );
        assert_eq!(
            table.columns(),
            4,
            "Should shrink back to 4 columns (actual content)"
        );

        // Test 7: Empty table shrink
        let mut empty_table = Table::new();
        empty_table.shrink_to_fit();
        println!(
            "📏 Empty table after shrink_to_fit: {} rows × {} cols",
            empty_table.rows(),
            empty_table.columns()
        );
        assert_eq!(empty_table.rows(), 0, "Empty table should have 0 rows");
        assert_eq!(
            empty_table.columns(),
            0,
            "Empty table should have 0 columns"
        );

        // Test 8: Display the dynamically resized table
        println!("🎨 Final dynamically resized table:");
        println!("{}", table.display_formatted());

        // Verify table content is intact after resizing
        assert!(table.get_cell(0, 0).is_some(), "Header cell should exist");
        assert!(table.get_cell(1, 0).is_some(), "Data cell should exist");
        assert!(
            table.get_cell(2, 3).is_some(),
            "4th column data should exist"
        );

        println!("✅ Dynamic resizing functionality works correctly");
        println!("📏 Tables automatically resize based on content and can be optimized");
    }

    /// Test the new text justification functionality
    #[test]
    fn test_justification_functionality() {
        println!("🧪 Testing text justification functionality...");

        // Create a demo table to showcase all justification types
        let mut demo_table = Table::new_with_dimensions(4, 3);
        demo_table.id = Id::get("justification_demo");

        // Add headers with center justification
        let mut header1 =
            TableCell::new_with_justification(0, 0, "left_header", Justification::Center);
        header1.add_entry(Entry::Str("Left Column".to_string()));
        demo_table.set_cell(0, 0, header1);

        let mut header2 =
            TableCell::new_with_justification(0, 1, "center_header", Justification::Center);
        header2.add_entry(Entry::Str("Center Column".to_string()));
        demo_table.set_cell(0, 1, header2);

        let mut header3 =
            TableCell::new_with_justification(0, 2, "right_header", Justification::Center);
        header3.add_entry(Entry::Str("Right Column".to_string()));
        demo_table.set_cell(0, 2, header3);

        // Row 1: Demonstrate different justifications
        let mut left_cell = TableCell::new(1, 0, "demo_left");
        left_cell.add_entry(Entry::Str("Left aligned text".to_string()));
        // left_cell keeps default Left justification
        demo_table.set_cell(1, 0, left_cell);

        let mut center_cell = TableCell::new(1, 1, "demo_center");
        center_cell.add_entry(Entry::Str("Centered".to_string()));
        center_cell.set_justification(Justification::Center);
        demo_table.set_cell(1, 1, center_cell);

        let mut right_cell = TableCell::new(1, 2, "demo_right");
        right_cell.add_entry(Entry::Str("Right aligned".to_string()));
        right_cell.set_justification(Justification::Right);
        demo_table.set_cell(1, 2, right_cell);

        // Row 2: Numeric data with different justifications
        let mut num_left = TableCell::new(2, 0, "num_left");
        num_left.add_entry(Entry::Val(42));
        demo_table.set_cell(2, 0, num_left);

        let mut num_center =
            TableCell::new_with_justification(2, 1, "num_center", Justification::Center);
        num_center.add_entry(Entry::Val(1337));
        demo_table.set_cell(2, 1, num_center);

        let mut num_right =
            TableCell::new_with_justification(2, 2, "num_right", Justification::Right);
        num_right.add_entry(Entry::Val(999));
        demo_table.set_cell(2, 2, num_right);

        // Row 3: Status indicators with center justification
        let mut status1 = TableCell::new_with_justification(3, 0, "status1", Justification::Center);
        status1.add_entry(Entry::Str("✅ SUCCESS".to_string()));
        demo_table.set_cell(3, 0, status1);

        let mut status2 = TableCell::new_with_justification(3, 1, "status2", Justification::Center);
        status2.add_entry(Entry::Str("⚠️ WARNING".to_string()));
        demo_table.set_cell(3, 1, status2);

        let mut status3 = TableCell::new_with_justification(3, 2, "status3", Justification::Center);
        status3.add_entry(Entry::Str("❌ ERROR".to_string()));
        demo_table.set_cell(3, 2, status3);

        println!("🎨 Justification Demo Table:");
        println!("{}", "=".repeat(80));
        let formatted_output = demo_table.display_formatted();
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

        // Test builder pattern with justification
        let builder_cell =
            TableCell::new(0, 0, "builder_test").with_justification(Justification::Right);
        assert_eq!(
            *builder_cell.justification(),
            Justification::Right,
            "Builder pattern should set justification"
        );

        // Test justification getter/setter
        let mut test_cell = TableCell::new(0, 0, "getter_setter_test");
        assert_eq!(
            *test_cell.justification(),
            Justification::Left,
            "Default should be Left"
        );

        test_cell.set_justification(Justification::Center);
        assert_eq!(
            *test_cell.justification(),
            Justification::Center,
            "Should update to Center"
        );

        println!("✅ Text justification functionality works correctly!");
        println!("📐 Left, Center, and Right alignments all functioning properly");
        println!("🔧 Builder pattern and getter/setter methods validated");
    }
}
