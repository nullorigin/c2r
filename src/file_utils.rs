use crate::error::Error;
use crate::{Kind, Reason, Result};
use core::result::Result::{Err, Ok};
use std::fs;
use std::path::{Path, PathBuf};

/// File utility to handle I/O operations for the C2R converter
pub struct FileUtils;

impl FileUtils {
    /// Read a file and return its contents as a string
    pub fn read_file(path: &str) -> Result<String> {
        let result = fs::read_to_string(path);
        match result {
            Ok(content) => Ok(content),
            Err(err) => Err(Error::new(
                Kind::Io,
                Reason::Read("error"),
                Some(format!("Failed to read file {}: {}", path, err)),
            )),
        }
    }

    /// Check if a file exists
    pub fn file_exists(path: &str) -> bool {
        Path::new(path).exists()
    }

    /// Resolve a relative path from a base directory
    pub fn resolve_path(base_dir: &str, relative_path: &str) -> PathBuf {
        let base = PathBuf::from(base_dir);
        if base.is_dir() || base_dir.ends_with('/') {
            // It's a directory, join directly
            base.join(relative_path)
        } else {
            // If base_dir is a file path, use its parent directory
            base.parent()
                .unwrap_or_else(|| Path::new("/"))
                .join(relative_path)
        }
    }

    /// Search for a file in multiple include directories
    pub fn find_in_include_dirs(file_name: &str, include_dirs: &[String]) -> Option<PathBuf> {
        // First check if the file exists as specified
        let direct_path = Path::new(file_name);
        if direct_path.exists() {
            return Some(direct_path.to_path_buf());
        }

        // Then search in include directories
        for dir in include_dirs {
            let full_path = Path::new(dir).join(file_name);
            if full_path.exists() {
                return Some(full_path);
            }
        }

        None
    }

    /// Read file content from a path
    pub fn read_file_content(path: &Path) -> Result<String> {
        match fs::read_to_string(path) {
            Ok(content) => Ok(content),
            Err(err) => Err(Error::new(
                Kind::Io,
                Reason::Read("io error"),
                Some(format!("Failed to read file {:?}: {}", path, err)),
            )),
        }
    }

    /// Get all files with a specific extension in a directory (recursive)
    pub fn get_files_with_extension(dir: &str, extension: &str) -> Result<Vec<PathBuf>> {
        let mut result = Vec::new();
        Self::get_files_with_extension_recursive(Path::new(dir), extension, &mut result)?;
        Ok(result)
    }

    /// Helper for recursive file finding
    fn get_files_with_extension_recursive(
        dir: &Path,
        extension: &str,
        result: &mut Vec<PathBuf>,
    ) -> Result<()> {
        if !dir.is_dir() {
            return Ok(());
        }

        let entries = match fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(err) => {
                return Err(Error::new(
                    Kind::Io,
                    Reason::Read("io error"),
                    Some(format!("Failed to read directory {:?}: {}", dir, err)),
                ));
            }
        };

        for entry in entries {
            let entry = match entry {
                Ok(e) => e,
                Err(err) => {
                    return Err(Error::new(
                        Kind::Io,
                        Reason::Read("error"),
                        Some(format!("Failed to read directory entry: {}", err)),
                    ));
                }
            };

            let path = entry.path();
            if path.is_dir() {
                Self::get_files_with_extension_recursive(&path, extension, result)?;
            } else if let Some(ext) = path.extension() {
                if ext == extension {
                    result.push(path);
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env::temp_dir;
    use std::fs::File;
    use std::io::Write;

    #[test]
    fn test_file_exists() {
        let temp_dir = temp_dir();
        let file_path = temp_dir.join("test_file.txt");

        // File should not exist yet
        if FileUtils::file_exists(file_path.to_str().unwrap()) {
            fs::remove_file(&file_path).expect("Failed to remove test file");
        }
        assert!(!FileUtils::file_exists(file_path.to_str().unwrap()));
        // Create the file
        let mut file = File::create(&file_path).expect("Failed to create test file");
        write!(file, "test content").expect("Failed to write to test file");

        // Now the file should exist
        assert!(FileUtils::file_exists(file_path.to_str().unwrap()));
    }

    #[test]
    fn test_resolve_path() {
        // Mock the Path::is_dir() behavior since we can't test with actual directories in unit tests
        let path: PathBuf = FileUtils::resolve_path("/home/user/", "file.txt");
        assert_eq!(path, PathBuf::from("/home/user/file.txt"));

        // When base is a file
        let path: PathBuf = FileUtils::resolve_path("/home/user/base.c", "file.h");
        assert_eq!(path, PathBuf::from("/home/user/file.h"));
    }
}
