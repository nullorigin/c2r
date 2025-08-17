use crate::config::Config;
use crate::handler_test::HandlerTest;
use crate::handlers::{
    ArrayHandler, CompositeHandler, FunctionCallHandler, IfdefHandler, IncludeHandler,
    MultilineMacroHandler, PointerDeclarationHandler, StructMemberAccessHandler, TypeCastHandler,
    TypedefHandler,
};

/// Run all handler tests
pub fn run_handler_tests() -> bool {
    println!("\n=== Running Handler Tests ===\n");

    let mut passed = true;

    passed &= test_pointer_declaration();
    passed &= test_struct_member_access();
    passed &= test_function_call();
    passed &= test_array_handler();
    passed &= test_composite_handler();
    passed &= test_type_cast_handler();
    passed &= test_ifdef_handler();
    passed &= test_typedef_handler();
    passed &= test_multiline_macro_handler();
    passed &= test_include_handler();

    println!("\n=== Handler Tests Complete ===\n");

    passed
}

/// Create a test configuration with standard settings
fn create_test_config() -> Config {
    let mut config = Config::new();
    // Add the test directory as an include directory
    config.add_include_dir("test");
    // Also add current directory for tests
    config.add_include_dir(".");
    // Set base directory to current directory
    config = config.with_base_dir(".");
    // Set verbosity to debug for tests
    config = config.with_verbosity(2);

    config
}

/// Test the pointer declaration handler
fn test_pointer_declaration() -> bool {
    println!("\nTesting PointerDeclarationHandler...");

    let handler = PointerDeclarationHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        ("int *ptr", "let mut ptr: *mut i32"),
        ("FILE *fp", "let mut fp: *mut std::fs::File"),
        ("const char *str", "let str: *const i8"),
        ("void *data", "let mut data: *mut c_void"),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the struct member access handler
fn test_struct_member_access() -> bool {
    println!("\nTesting StructMemberAccessHandler...");

    let handler = StructMemberAccessHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        ("point.x", "point.x"),
        ("ptr->value", "(*ptr).value"),
        ("obj.member.field", "obj.member.field"),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the function call handler
fn test_function_call() -> bool {
    println!("\nTesting FunctionCallHandler...");

    // Create a specialized handler for printf with struct member access
    let mut handler = FunctionCallHandler::new();
    handler.register_special_function("printf");
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        (
            "printf(\"Hello, %d\\n\", 42)",
            "println!(\"Hello, {}\", 42)",
        ),
        (
            "printf(\"Value: %d\\n\", obj.value)",
            "println!(\"Value: {}\", obj.value)",
        ),
        (
            "malloc(size)",
            "unsafe { std::alloc::alloc_zeroed(std::alloc::Layout::from_size_align(size, std::mem::align_of::<u8>()).unwrap()) as *mut u8 }",
        ),
        ("strlen(str)", "str.len()"),
        (
            "printf(\"Value: %d\\n\", obj.member.value)",
            "println!(\"Value: {}\", obj.member.value)",
        ),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the array handler
fn test_array_handler() -> bool {
    println!("\nTesting ArrayHandler...");

    let handler = ArrayHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        ("int numbers[10]", "let mut numbers: [i32; 10] = [0; 10];"),
        (
            "char buffer[100]",
            "let mut buffer: [i8; 100] = ['\\0'; 100];",
        ),
        ("numbers[i]", "numbers[i]"),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the composite handler
fn test_composite_handler() -> bool {
    println!("\nTesting CompositeHandler...");

    // Create a new composite handler
    let mut composite = CompositeHandler::new("TestComposite");

    composite.add_handler(Box::new(StructMemberAccessHandler::new()));

    let mut tester = HandlerTest::new(Box::new(composite));

    let tests = vec![("ptr->value", "(*ptr).value")];

    tester.run_tests(&tests);

    true
}

/// Test the type cast handler
fn test_type_cast_handler() -> bool {
    println!("\nTesting TypeCastHandler...");

    let handler = TypeCastHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        // Simple type casts
        ("(int)x", "x as i32"),
        ("(double)value", "value as f64"),
        ("(unsigned int)size", "size as u32"),
        // Pointer casts
        ("(int*)ptr", "ptr as *mut i32"),
        ("(char*)buffer", "buffer as *mut i8"),
        ("(const void*)data", "data as *const c_void"),
        // Complex expressions
        ("(int*)(p + offset)", "(p + offset) as *mut i32"),
        ("(unsigned long)(result >> 8)", "(result >> 8) as u64"),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the ifdef handler
fn test_ifdef_handler() -> bool {
    println!("\nTesting IfdefHandler...");

    let handler = IfdefHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        ("#ifdef DEBUG", "#[cfg(feature = \"debug\")]"),
        ("#ifndef RELEASE", "#[cfg(not(feature = \"release\"))]"),
        ("#ifdef _WIN32", "#[cfg(target_os = \"windows\")]"),
        ("#ifdef __linux__", "#[cfg(target_os = \"linux\")]"),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the TypedefHandler
fn test_typedef_handler() -> bool {
    println!("\nTesting TypedefHandler...");

    let handler = TypedefHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        ("typedef int Integer", "type Integer = i32;"),
        ("typedef unsigned long ulong", "type ulong = u64;"),
        ("typedef struct Point Point", "type Point = Point;"),
        (
            "typedef void (*Callback)(int, int)",
            "type Callback = fn(i32, i32) -> ();",
        ),
        (
            "typedef int (*CompareFn)(const void*, const void*)",
            "type CompareFn = fn(*const c_void, *const c_void) -> i32;",
        ),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the MultilineMacroHandler
fn test_multiline_macro_handler() -> bool {
    println!("\nTesting MultilineMacroHandler...");

    let handler = MultilineMacroHandler::new();
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        (
            "#define SWAP(a, b) do { \\\n    int temp = a; \\\n    a = b; \\\n    b = temp; \\\n} while (0)",
            "#[macro_export]\nmacro_rules! SWAP {\n    ($a:expr, $b:expr) => {\n        {\n            let temp = $a;\n            $a = $b;\n            $b = temp;\n        }\n    };\n}",
        ),
        (
            "#define LOG_ERROR(fmt, ...) \\\n    fprintf(stderr, \"[ERROR] \" fmt \"\\n\", ##__VA_ARGS__)",
            "#[macro_export]\nmacro_rules! LOG_ERROR {\n    ($fmt:expr $(, $args:expr)*) => {\n        eprintln!(concat!(\"[ERROR] \", $fmt), $($args),*);\n    };\n}",
        ),
    ];

    tester.run_tests(&tests);

    true
}

/// Test the IncludeHandler
fn test_include_handler() -> bool {
    println!("\nTesting IncludeHandler...");

    // Create a configuration for the handler
    let config = create_test_config();

    let handler = IncludeHandler::new(config);
    let mut tester = HandlerTest::new(Box::new(handler));

    let tests = vec![
        (
            "#include \"stdio.h\"",
            "// Module import: stdio\nuse std::io;\nuse std::io::{Read, Write};\n",
        ),
        (
            "#include <stdlib.h>",
            "// System import: stdlib\nuse std::alloc;\nuse std::mem;\n",
        ),
        (
            "#include \"utils/helper.h\"",
            "// Module import: utils::helper\nmod utils {\n    pub mod helper;\n}\nuse utils::helper::*;\n",
        ),
    ];

    tester.run_tests(&tests);

    true
}
