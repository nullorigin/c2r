#include <stdio.h>
#include <string.h>

// Test function with while(true) loop - should become a Rust 'loop'
void infinite_loop_test() {
    int i = 0;
    while (1) {
        printf("This is an infinite loop, iteration: %d\n", i);
        i++;
        if (i > 10) break;
    }
}

// Test function with a C-style for loop
void standard_for_loop() {
    for (int i = 0; i < 5; i++) {
        printf("Standard for loop: %d\n", i);
    }
}

// Test function with a C-style for loop with external counter
void external_counter_for_loop() {
    int j;
    for (j = 10; j >= 0; j--) {
        printf("Countdown: %d\n", j);
    }
}

// Test function with a do-while loop
void do_while_test() {
    int k = 0;
    do {
        printf("Do-while loop: %d\n", k);
        k++;
    } while (k < 5);
}

// Complex for-loop that can't easily be translated to a range-based loop
void complex_for_loop() {
    char str[] = "12345";
    int sum = 0;
    
    for (int i = 0; i < strlen(str); i++) {
        sum += str[i] - '0';
    }
    
    printf("Sum of digits: %d\n", sum);
}

int main() {
    infinite_loop_test();
    standard_for_loop();
    external_counter_for_loop();
    do_while_test();
    complex_for_loop();
    
    return 0;
}
