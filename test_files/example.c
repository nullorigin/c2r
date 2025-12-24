// Test C file for c2r converter validation
#include <stdio.h>
#include <stdlib.h>

// Global variables
int global_counter = 0;
const char* program_name = "c2r_test";
static float internal_value = 3.14f;

// Type definitions
typedef int MyInt;
typedef struct Point {
    int x;
    int y;
} Point;

// Enumeration
enum Status {
    OK = 0,
    ERROR = 1,
    PENDING = 2
};

// Function declarations
int add_numbers(int a, int b);
void print_point(Point* p);
MyInt calculate_square(MyInt n);

// Function implementations
int add_numbers(int a, int b) {
    return a + b;
}

void print_point(Point* p) {
    if (p != NULL) {
        printf("Point: (%d, %d)\n", p->x, p->y);
    }
}

MyInt calculate_square(MyInt n) {
    return n * n;
}

// Main function with control flow
int main() {
    Point origin = {0, 0};
    Point destination = {10, 20};
    
    // Array declaration
    int numbers[5] = {1, 2, 3, 4, 5};
    
    // Control flow examples
    for (int i = 0; i < 5; i++) {
        if (numbers[i] % 2 == 0) {
            printf("Even: %d\n", numbers[i]);
        } else {
            printf("Odd: %d\n", numbers[i]);
        }
    }
    
    // Function calls
    int sum = add_numbers(destination.x, destination.y);
    print_point(&origin);
    
    // Switch statement
    enum Status current_status = OK;
    switch (current_status) {
        case OK:
            printf("Everything is OK\n");
            break;
        case ERROR:
            printf("An error occurred\n");
            break;
        case PENDING:
            printf("Operation pending\n");
            break;
    }
    
    return 0;
}
