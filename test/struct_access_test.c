#include <stdio.h>

struct Point {
    int x;
    int y;
};

struct Line {
    struct Point start;
    struct Point end;
};

int main() {
    struct Point p1;
    struct Point *p_ptr;
    struct Line line;
    
    // Direct member access
    p1.x = 10;
    p1.y = 20;
    
    // Pointer member access
    p_ptr = &p1;
    p_ptr->x = 30;
    p_ptr->y = 40;
    
    // Nested member access
    line.start.x = 0;
    line.start.y = 0;
    line.end.x = 100;
    line.end.y = 100;
    
    // Mixed member access
    printf("Point: (%d, %d)\n", p1.x, p1.y);
    printf("Point via pointer: (%d, %d)\n", p_ptr->x, p_ptr->y);
    printf("Line: (%d, %d) to (%d, %d)\n", 
           line.start.x, line.start.y, 
           line.end.x, line.end.y);
    
    return 0;
}
