#include <stdio.h>

int main() {
    // Array declarations
    int numbers[10];
    char buffer[100];
    float data[5];
    
    // Multi-dimensional arrays
    int matrix[3][3];
    
    // Array initialization
    int initialized[5] = {1, 2, 3, 4, 5};
    char string[] = "Hello";
    
    // Array access
    numbers[0] = 42;
    numbers[1] = 43;
    
    // Array access with expressions
    int i = 2;
    numbers[i] = 44;
    numbers[i+1] = 45;
    
    // Array in struct
    struct Person {
        char name[50];
        int age;
    };
    
    struct Person people[10];
    people[0].age = 30;
    
    // Print array elements
    printf("First number: %d\n", numbers[0]);
    printf("Second number: %d\n", numbers[1]);
    
    // Loop through array
    for (i = 0; i < 5; i++) {
        printf("numbers[%d] = %d\n", i, numbers[i]);
    }
    
    // Use sizeof with arrays
    int array_size = sizeof(numbers) / sizeof(int);
    printf("Array size: %d\n", array_size);
    
    return 0;
}
