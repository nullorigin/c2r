#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fptr;
    int *ptr;
    char *str;
    
    fptr = fopen("test.txt", "r");
    ptr = malloc(sizeof(int) * 10);
    str = "Hello";
    
    return 0;
}
