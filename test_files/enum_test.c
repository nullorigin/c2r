// Simple enum test file
#include <stdio.h>

// Basic enum
enum Color {
    RED,
    GREEN,
    BLUE
};

// Enum with explicit values
enum Status {
    ERROR = -1,
    SUCCESS = 0,
    PENDING = 1
};

// Enum with mixed values
enum Priority {
    LOW = 1,
    MEDIUM,     // Should be 2
    HIGH = 10,
    CRITICAL    // Should be 11
};

// Function using enums
void print_color(enum Color c) {
    switch(c) {
        case RED:
            printf("Red\n");
            break;
        case GREEN:
            printf("Green\n");
            break;
        case BLUE:
            printf("Blue\n");
            break;
    }
}

int main() {
    enum Color favorite = RED;
    enum Status app_status = SUCCESS;
    
    print_color(favorite);
    printf("Status: %d\n", app_status);
    
    return 0;
}
