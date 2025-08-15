/// Represents a flag used in the C to Rust conversion process
///
/// Flags can be used to control aspects of the conversion process
/// or to mark specific features or behaviors that should be applied.
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Flag(pub u128, pub String);

impl Flag {
    /// Create a new flag with the given numeric value and name
    ///
    /// # Arguments
    ///
    /// * `value` - The numeric value of the flag
    /// * `name` - The name of the flag
    pub fn new(value: u128, name: String) -> Self {
        Flag(value, name)
    }

    /// Gets the value of the flag
    pub fn value(&self) -> u128 {
        self.0
    }

    /// Gets the name of the flag
    pub fn name(&self) -> &str {
        &self.1
    }
}
