#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Global variables
int global_counter = 0;
static int static_global = 42;
extern int external_var;

// Simple function declarations
int add(int a, int b);
void print_message(const char* msg);
double calculate_average(int* values, int count);

// Function with various parameter types
int complex_function(int x, float y, char* str, const int* arr, size_t len) {
    if (x > 0 && y > 0.0) {
        printf("Processing %s with %zu elements\n", str, len);
        return x + (int)y;
    }
    return -1;
}

// Static and inline functions
static int private_helper(int value) {
    return value * 2;
}

inline int fast_multiply(int a, int b) {
    return a * b;
}

// Simple struct definition
struct Point {
    double x;
    double y;
};

// Complex struct with nested types
struct Rectangle {
    struct Point top_left;
    struct Point bottom_right;
    int color;
    char label[32];
};

// Struct with function pointer
struct Calculator {
    int (*add)(int, int);
    int (*subtract)(int, int);
    double (*multiply)(double, double);
};

// Union definition
union Data {
    int i;
    float f;
    char str[16];
};

// Simple enum
enum Color {
    RED,
    GREEN,
    BLUE
};

// Enum with explicit values
enum Status {
    ERROR = -1,
    SUCCESS = 0,
    PENDING = 1,
    COMPLETE = 100
};

// Typedef declarations
typedef struct Point Point2D;
typedef int (*BinaryOperation)(int, int);
typedef enum Color ColorType;

// Array declarations and initializations
int numbers[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
char* fruits[] = {"apple", "banana", "orange", NULL};
struct Point points[3] = {{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}};

// Function implementations
int add(int a, int b) {
    return a + b;
}

void print_message(const char* msg) {
    if (msg != NULL) {
        printf("Message: %s\n", msg);
    }
}

double calculate_average(int* values, int count) {
    if (values == NULL || count <= 0) {
        return 0.0;
    }
    
    int sum = 0;
    for (int i = 0; i < count; i++) {
        sum += values[i];
    }
    
    return (double)sum / count;
}

// Function with control flow
int process_data(int* data, int size) {
    int processed = 0;
    
    for (int i = 0; i < size; i++) {
        switch (data[i] % 3) {
            case 0:
                data[i] *= 2;
                processed++;
                break;
            case 1:
                data[i] += 10;
                processed++;
                break;
            case 2:
                data[i] = 0;
                break;
            default:
                break;
        }
    }
    
    return processed;
}

// Function with while and do-while loops
void count_operations(int limit) {
    int counter = 0;
    
    // While loop
    while (counter < limit) {
        global_counter++;
        counter++;
    }
    
    // Do-while loop
    do {
        global_counter--;
    } while (global_counter > 0 && counter > 0);
}

// Function with complex conditionals
int validate_input(const char* input, int min_len, int max_len) {
    if (input == NULL) {
        return -1;
    }
    
    int len = strlen(input);
    
    if (len < min_len || len > max_len) {
        return 0;
    }
    
    // Check for valid characters
    for (int i = 0; i < len; i++) {
        if (!(input[i] >= 'a' && input[i] <= 'z') && 
            !(input[i] >= 'A' && input[i] <= 'Z') && 
            !(input[i] >= '0' && input[i] <= '9')) {
            return 0;
        }
    }
    
    return 1;
}

// Main function
int main(int argc, char* argv[]) {
    printf("Comprehensive C Test Program\n");
    
    // Test basic operations
    int result = add(5, 3);
    printf("5 + 3 = %d\n", result);
    
    // Test struct usage
    struct Point p1 = {10.5, 20.3};
    struct Rectangle rect = {{0, 0}, {100, 50}, 255, "main_rect"};
    
    // Test array operations
    int test_data[] = {1, 2, 3, 4, 5};
    double avg = calculate_average(test_data, 5);
    printf("Average: %.2f\n", avg);
    
    // Test enum usage
    enum Color current_color = RED;
    enum Status app_status = SUCCESS;
    
    return 0;
}
