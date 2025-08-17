/**
 * Test file for enum and union conversions
 *
 * This file contains various examples of C enum and union declarations
 * to test the functionality of the EnumHandler and UnionHandler in our C-to-Rust converter.
 */

#include <stdint.h>
#include <stdio.h>

// Simple enum example
enum Color {
    RED,
    GREEN,
    BLUE
};

// Enum with explicit values
enum Status {
    OK = 0,
    ERROR = 1,
    PENDING = 2,
    CANCELLED = 3
};

// Enum with non-sequential values
enum Flags {
    FLAG_NONE = 0x00,
    FLAG_READ = 0x01,
    FLAG_WRITE = 0x02,
    FLAG_EXECUTE = 0x04
};

// Enum with mixed assignment
enum Direction {
    UP,               // Implicit 0
    DOWN,             // Implicit 1
    LEFT = 10,        // Explicit 10
    RIGHT             // Implicit 11
};

// Enum with explicit integer type
enum PowerLevel : uint16_t {
    LOW = 1,
    MEDIUM = 500,
    HIGH = 1000,
    MAX = 65535
};

// Simple union example
union Data {
    int i;
    float f;
    char str[20];
};

// Union with nested structs
union ComplexData {
    int simple;
    struct {
        int x;
        int y;
    } point;
    struct {
        char name[10];
        int id;
    } record;
};

// Packed union with memory layout optimization
#pragma pack(1)
union PackedData {
    uint32_t value;
    struct {
        uint8_t byte1;
        uint8_t byte2;
        uint8_t byte3;
        uint8_t byte4;
    } bytes;
} __attribute__((packed));
#pragma pack()

// Using the enum and union in functions
void print_color(enum Color color) {
    switch (color) {
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

void use_union(union Data* data) {
    // Access union members
    data->i = 10;
    printf("Integer: %d\n", data->i);
    
    data->f = 3.14;
    printf("Float: %f\n", data->f);
    
    sprintf(data->str, "Hello");
    printf("String: %s\n", data->str);
}

int main() {
    // Using enums
    enum Color my_color = BLUE;
    print_color(my_color);
    
    enum Flags permissions = FLAG_READ | FLAG_WRITE;
    if (permissions & FLAG_READ) {
        printf("Read permission is granted\n");
    }
    
    // Using unions
    union Data data;
    use_union(&data);
    
    union PackedData packed;
    packed.value = 0x12345678;
    printf("Bytes: %02x %02x %02x %02x\n", 
           packed.bytes.byte1, packed.bytes.byte2, 
           packed.bytes.byte3, packed.bytes.byte4);
    
    return 0;
}
