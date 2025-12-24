// Simple struct test file
#include <stdio.h>

// Basic struct definition
struct Point {
    int x;
    int y;
};

// Struct with different data types
struct Person {
    char name[50];
    int age;
    float height;
    double salary;
};

// Nested struct
struct Address {
    char street[100];
    char city[50];
    int zip_code;
};

struct Employee {
    struct Person info;
    struct Address address;
    int employee_id;
};

// Function that uses structs
struct Point create_point(int x, int y) {
    struct Point p;
    p.x = x;
    p.y = y;
    return p;
}

void print_point(struct Point p) {
    printf("Point: (%d, %d)\n", p.x, p.y);
}

int main() {
    struct Point origin = {0, 0};
    struct Point p1 = create_point(10, 20);
    
    print_point(origin);
    print_point(p1);
    
    return 0;
}
