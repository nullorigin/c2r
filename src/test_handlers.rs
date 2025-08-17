use c2r::handler_test::HandlerTest;
use c2r::handlers::DefineHandler;
use c2r::{
    config::{self, Config, VERBOSITY_DEBUG},
    handler::{HandlerResult, ParserContext, TokenHandler},
    handlers::array_handler::ArrayHandler,
    handlers::composite_handler::CompositeHandler,
    handlers::define_const_handler::DefineConstHandler,
    handlers::define_macro_handler::DefineMacroHandler,
    handlers::enum_handler::EnumHandler,
    handlers::function_call::FunctionCallHandler,
    handlers::ifdef_handler::IfdefHandler,
    handlers::include_handler::IncludeHandler,
    handlers::pointer_declaration::PointerDeclarationHandler,
    handlers::struct_member_access::StructMemberAccessHandler,
    handlers::type_cast_handler::TypeCastHandler,
    handlers::typedef_handler::TypedefHandler,
    handlers::union_handler::UnionHandler,
    log,
    token_parser::Token
};
use std::env;

fn main() {
    // Parse command line arguments and create config
    let config = parse_arguments();
    
    // Apply the config globally
    config.apply();
    
    // Default to debug level for tests if not specified
    if config::get_verbosity() < config::VERBOSITY_DEBUG {
        config::set_verbosity(config::VERBOSITY_DEBUG);
    }

    log!(info, "=== C to Rust Handler Tests ===\n");
    log!(info, "Using configuration:");
    log!(info, "  Base directory: {}", config.base_dir);
    log!(info, "  Include directories: {:?}", config.include_dirs);
    log!(info, "  Process system includes: {}", config.process_system_includes);
    
    run_handler_tests(&config);
    
    log!(info, "\n=== All Tests Complete ===");
}

/// Parse command-line arguments and create a Config object
fn parse_arguments() -> Config {
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter();
    // Skip the program name
    args_iter.next();
    
    // Default configuration
    let mut config = Config::new();
    // Set the base directory to the current directory by default
    config = config.with_base_dir(".");
    
    // Parse arguments
    while let Some(arg) = args_iter.next() {
        match arg.as_str() {
            "-v" | "--verbose" => {
                // Look for a numeric value after -v
                if let Some(level_str) = args_iter.next() {
                    if let Ok(level) = level_str.parse::<u8>() {
                        config = config.with_verbosity(level);
                    } else {
                        config = config.with_verbosity(VERBOSITY_DEBUG); // Default to level 2 if no valid number
                    }
                } else {
                    config = config.with_verbosity(VERBOSITY_DEBUG); // Default to level 2 if no value provided
                }
            },
            "-I" | "--include" => {
                // Add an include directory
                if let Some(include_dir) = args_iter.next() {
                    config.add_include_dir(include_dir);
                    log!(debug, "Added include directory: {}", include_dir);
                }
            },
            "--base-dir" => {
                // Set the base directory
                if let Some(base_dir) = args_iter.next() {
                    config = config.with_base_dir(base_dir);
                    log!(debug, "Set base directory: {}", base_dir);
                }
            },
            "--system-includes" => {
                // Enable processing of system includes
                config = config.with_system_includes(true);
                log!(debug, "System include processing enabled");
            },
            _ => {
                // Ignore other arguments
            }
        }
    }
    
    config
}

/// Run all handler tests with the given config
fn run_handler_tests(config: &Config) {
    test_pointer_declaration();
    test_struct_member_access();
    test_function_call();
    test_array_handler();
    test_composite_handler();
    test_type_cast_handler();
    test_define_const_handler();
    test_define_macro_handler();
    test_typedef_handler();
    test_ifdef_handler();
    test_enum_handler();
    test_union_handler();
    test_include_handler(config);
}

/// Test the pointer declaration handler
fn test_pointer_declaration() {
    log!(info, "\nTesting PointerDeclarationHandler...");
    
    let handler = PointerDeclarationHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("int *ptr", "let mut ptr: *mut i32"),
        ("FILE *fp", "let mut fp: *mut std::fs::File"),
        ("const char *str", "let str: *const i8"),
        ("void *data", "let mut data: *mut c_void"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the struct member access handler
fn test_struct_member_access() {
    log!(info, "\nTesting StructMemberAccessHandler...");
    
    let handler = StructMemberAccessHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("point.x", "point.x"),
        ("ptr->value", "(*ptr).value"),
        ("obj.member.field", "obj.member.field"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the function call handler
fn test_function_call() {
    log!(info, "\nTesting FunctionCallHandler...");
    
    let handler = FunctionCallHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("printf(\"Hello, %d\\n\", 42)", "println!(\"Hello, {}\", 42)"),
        ("malloc(size)", "unsafe { std::alloc::alloc_zeroed(std::alloc::Layout::from_size_align(size, std::mem::align_of::<u8>()).unwrap()) as *mut u8 }"),
        ("strlen(str)", "str.len()"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the array handler
fn test_array_handler() {
    log!(info, "\nTesting ArrayHandler...");
    
    let handler = ArrayHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("int numbers[10]", "let mut numbers: [i32; 10] = [0; 10];"),
        ("char buffer[100]", "let mut buffer: [i8; 100] = ['\\0'; 100];"),
        ("numbers[i]", "numbers[i]"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the composite handler
fn test_composite_handler() {
    log!(info, "\nTesting CompositeHandler...");
    
    // Create a composite handler with struct member access
    let member_handler = StructMemberAccessHandler::new();
    let mut composite = CompositeHandler::new("struct_member_composite");
    composite.add_handler(Box::new(member_handler));
    
    let mut tester = HandlerTest::new(Box::new(composite));
    
    // Test direct delegation to struct member access handler
    let tests = vec![
        ("ptr->value", "(*ptr).value"),
    ];
    
    tester.run_tests(&tests);
    
    // Create a composite handler with function call and struct member access
    let member_handler = StructMemberAccessHandler::new();
    let function_handler = FunctionCallHandler::new();
    let mut composite = CompositeHandler::new("combined_composite");
    composite.add_handler(Box::new(member_handler));
    composite.add_handler(Box::new(function_handler));
    
    // Test with a more complex expression involving both handlers
    struct TestTokenHandler {}
    
    impl TokenHandler for TestTokenHandler {
        fn can_handle(&self, _tokens: &[Token], _context: &ParserContext) -> bool {
            true
        }
        
        fn handle(&self, _tokens: &[Token], _context: &mut ParserContext) -> Result<HandlerResult, c2r::ConversionError> {
            Ok(HandlerResult::RustCode("println!(\"Value: {}\", obj.value)".to_string()))
        }
    }
    
    let test_handler = TestTokenHandler {};
    composite.add_handler(Box::new(test_handler));
    
    let mut tester = HandlerTest::new(Box::new(composite));
    
    // Test with a complex expression
    let tests = vec![
        ("printf(\"Value: %d\\n\", obj.value)", "println!(\"Value: {}\", obj.value)"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the type cast handler
fn test_type_cast_handler() {
    log!(info, "\nTesting TypeCastHandler...");
    
    let handler = TypeCastHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("(int)x", "x as i32"),
        ("(double)value", "value as f64"),
        ("(unsigned int)size", "size as u32"),
        ("(int*)ptr", "ptr as *mut i32"),
        ("(char*)buffer", "buffer as *mut i8"),
        ("(const void*)data", "data as *const c_void"),
        ("(int*)(p + offset)", "(p + offset) as *mut i32"),
        ("(unsigned long)(result >> 8)", "(result >> 8) as u64"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the define const handler
fn test_define_const_handler() {
    log!(info, "\nTesting DefineConstHandler...");
    
    let handler = DefineConstHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("#define MAX_SIZE 100", "pub const MAX_SIZE: i32 = 100;"),
        ("#define VERSION \"1.0\"", "pub const VERSION: &'static str = \"1.0\";"),
        ("#define DEBUG 1", "pub const DEBUG: bool = true;"),
        ("#define DISABLED 0", "pub const DISABLED: bool = false;"),
        ("#define PI 3.14159", "pub const PI: f64 = 3.14159;"),
        ("#define NULL_PTR NULL", "pub const NULL_PTR: *const std::ffi::c_void = std::ptr::null();"),
        ("#define FEATURE_FLAG", "pub const FEATURE_FLAG: bool = true;"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the define macro handler
fn test_define_macro_handler() {
    log!(info, "\nTesting DefineMacroHandler...");
    
    let handler = DefineMacroHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        // Expression macros without parameters
        ("#define MIN_VALUE 1 + 2", "macro_rules! MIN_VALUE { () => { 1 + 2 }; }"),
        ("#define TWICE_MAX MAX_SIZE * 2", "macro_rules! TWICE_MAX { () => { MAX_SIZE * 2 }; }"),
        
        // Function-like macros with parameters
        ("#define MAX(a, b) ((a) > (b) ? (a) : (b))", "macro_rules! MAX { ($a:expr, $b:expr) => { if $a > $b { $a } else { $b } }; }"),
        ("#define SQUARE(x) ((x) * (x))", "macro_rules! SQUARE { ($x:expr) => { $x * $x }; }"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the legacy define handler
fn test_define_handler() {
    log!(info, "\nTesting DefineHandler...");
    
    let handler = DefineHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("#define MAX_SIZE 100", "pub const MAX_SIZE: i32 = 100;"),
        ("#define DEBUG 1", "pub const DEBUG: bool = true;"),
        ("#define VERSION \"1.0\"", "pub const VERSION: &'static str = \"1.0\";"),
        ("#define DISABLED 0", "pub const DISABLED: bool = false;"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the ifdef/endif handler
fn test_ifdef_handler() {
    log!(info, "\nTesting IfdefHandler...");
    
    let handler = IfdefHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("#ifdef DEBUG", "#[cfg(feature = \"DEBUG\")]\n{"),
        ("#endif", "}"),
        ("#ifdef FEATURE_FLAG", "#[cfg(feature = \"FEATURE_FLAG\")]\n{"),
        ("#ifdef RELEASE", "#[cfg(feature = \"RELEASE\")]\n{"),
        ("#ifndef DEBUG", "#[cfg(not(feature = \"DEBUG\"))]\n{"),
        ("#ifndef RELEASE", "#[cfg(not(feature = \"RELEASE\"))]\n{"),
        // Alternative format with separate # token
        ("# ifndef NDEBUG", "#[cfg(not(feature = \"NDEBUG\"))]\n{"),
        // Test #else directive
        ("#else", "} else {"),
        ("# else", "} else {"),
        ("#else  // comment", "} else {"),
        ("#ifdef DEBUG\nprintf(\"Debug mode\\n\");\n#endif", "#[cfg(feature = \"DEBUG\")]\nprintln!(\"Debug mode\");"),
        ("#ifndef RELEASE\nlog_message(\"Development build\");\n#endif", "#[cfg(not(feature = \"RELEASE\"))]\nlog_message(\"Development build\");"),
        ("#ifdef WINDOWS\nwin_api_call();\n#else\nunix_api_call();\n#endif", 
         "#[cfg(feature = \"WINDOWS\")]\nwin_api_call();\n#[cfg(not(feature = \"WINDOWS\"))]\nunix_api_call();"),
        ("#ifdef FEATURE_A\nuse_feature_a();\n#elif defined(FEATURE_B)\nuse_feature_b();\n#else\nuse_default();\n#endif",
         "#[cfg(feature = \"FEATURE_A\")]\nuse_feature_a();\n#[cfg(all(not(feature = \"FEATURE_A\"), feature = \"FEATURE_B\"))]\nuse_feature_b();\n#[cfg(all(not(feature = \"FEATURE_A\"), not(feature = \"FEATURE_B\")))]\nuse_default();"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the enum handler
fn test_enum_handler() {
    log!(info, "\nTesting EnumHandler...");
    
    let handler = EnumHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("enum Color { RED, GREEN, BLUE };", 
         "#[repr(C)]\nenum Color {\n    RED = 0,\n    GREEN = 1,\n    BLUE = 2\n}"),
        
        ("enum Status { OK = 0, ERROR = 1, PENDING = 2 };", 
         "#[repr(C)]\nenum Status {\n    OK = 0,\n    ERROR = 1,\n    PENDING = 2\n}"),
         
        ("enum Direction { UP, DOWN, LEFT = 10, RIGHT };", 
         "#[repr(C)]\nenum Direction {\n    UP = 0,\n    DOWN = 1,\n    LEFT = 10,\n    RIGHT = 11\n}"),
         
        // Test explicit type representation (though this test might be challenging with the current parser)
        ("enum Flags : uint8_t { NONE = 0, READ = 1, WRITE = 2, EXEC = 4 };", 
         "#[repr(u8)]\nenum Flags {\n    NONE = 0,\n    READ = 1,\n    WRITE = 2,\n    EXEC = 4\n}"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the union handler
fn test_union_handler() {
    log!(info, "\nTesting UnionHandler...");
    
    let handler = UnionHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));
    
    let tests = vec![
        ("union Data { int i; float f; };", 
         "#[repr(C)]\nunion Data {\n    i: i32,\n    f: f32\n}\n\nimpl Data {\n    pub fn get_i(&self) -> i32 {\n        unsafe { self.i }\n    }\n    \n    pub fn set_i(&mut self, val: i32) {\n        unsafe { self.i = val; }\n    }\n    \n    pub fn get_f(&self) -> f32 {\n        unsafe { self.f }\n    }\n    \n    pub fn set_f(&mut self, val: f32) {\n        unsafe { self.f = val; }\n    }\n}"),
         
        ("union IntBytes { int value; char bytes[4]; };", 
         "#[repr(C)]\nunion IntBytes {\n    value: i32,\n    bytes: [i8; 4]\n}\n\nimpl IntBytes {\n    pub fn get_value(&self) -> i32 {\n        unsafe { self.value }\n    }\n    \n    pub fn set_value(&mut self, val: i32) {\n        unsafe { self.value = val; }\n    }\n    \n    pub fn get_bytes(&self) -> [i8; 4] {\n        unsafe { self.bytes }\n    }\n    \n    pub fn set_bytes(&mut self, val: [i8; 4]) {\n        unsafe { self.bytes = val; }\n    }\n}"),
    ];
    
    tester.run_tests(&tests);
}

/// Test the typedef handler
fn test_typedef_handler() {
    log!(info, "\n=== Testing TypedefHandler ===");
    
    let mut test_runner = HandlerTest::new(Box::new(TypedefHandler {}));
    
    // Test basic typedefs
    test_runner.test("typedef int Integer;", "type Integer = i32;").unwrap();
    test_runner.test("typedef unsigned int UInteger;", "type UInteger = u32;").unwrap();
    test_runner.test("typedef float Real;", "type Real = f32;").unwrap();
    test_runner.test("typedef double Double;", "type Double = f64;").unwrap();
    
    // Test pointer typedefs
    test_runner.test("typedef char* String;", "type String = *mut i8;").unwrap();
    test_runner.test("typedef const char* ConstString;", "type ConstString = *const i8;").unwrap();
    test_runner.test("typedef void* VoidPtr;", "type VoidPtr = *mut std::ffi::c_void;").unwrap();
    
    // Test custom type typedefs
    test_runner.test("typedef struct Point Point;", "type Point = struct Point;").unwrap();
    test_runner.test("typedef enum Color ColorType;", "type ColorType = enum Color;").unwrap();
    
    // Test size_t and similar types
    test_runner.test("typedef size_t Size;", "type Size = usize;").unwrap();
    test_runner.test("typedef ssize_t SignedSize;", "type SignedSize = isize;").unwrap();
    
    log!(info, "=== TypedefHandler Tests Complete ===\n");
}

/// Test the include handler
fn test_include_handler(config: &Config) {
    log!(info, "\nTesting IncludeHandler...");
    
    let handler = IncludeHandler::new(config.clone());
    let mut tester = HandlerTest::new_with_config(Box::new(handler), config.clone());
    
    let tests = vec![
        ("#include \"stdio.h\"", "// Module import: stdio\nuse std::io;\nuse std::io::{Read, Write};\n"),
        ("#include <stdlib.h>", "// System import: stdlib\nuse std::alloc;\nuse std::mem;\n"),
        ("#include \"utils/helper.h\"", "// Module import: utils::helper\nmod utils {\n    pub mod helper;\n}\nuse utils::helper::*;\n"),
    ];
    
    tester.run_tests(&tests);
}
