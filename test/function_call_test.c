#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    // Standard library function calls
    int *data = malloc(10 * sizeof(int));
    char *str = "Hello, World!";
    int len = strlen(str);
    
    // I/O function calls
    printf("Length of string: %d\n", len);
    printf("Multiple args: %d, %s, %f\n", 42, "test", 3.14);
    
    FILE *file = fopen("test.txt", "w");
    if (file) {
        fprintf(file, "Test data\n");
        fclose(file);
    }
    
    // Memory operations
    memset(data, 0, 10 * sizeof(int));
    int *copy = malloc(10 * sizeof(int));
    memcpy(copy, data, 10 * sizeof(int));
    
    // String conversions
    int num = atoi("123");
    double val = atof("3.14");
    
    // Custom function calls
    int result = add(5, 10);
    printf("Result: %d\n", result);
    
    // Cleanup
    free(data);
    free(copy);
    
    return 0;
}
