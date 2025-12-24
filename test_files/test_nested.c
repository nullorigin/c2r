// Test file for nested structure processing
#include <stdio.h>

// Macro with nested struct
#define CONTAINER_STRUCT struct { \
    int x; \
    struct inner { \
        float y; \
        enum status { OK, ERROR } state; \
    } nested; \
}

// Function with nested enum
int process_data() {
    enum result { SUCCESS = 0, FAILURE = 1 } status;
    struct {
        int id;
        char name[50];
    } record;
    return 0;
}

// Global variable with inline struct
static struct {
    int count;
    enum priority { LOW, MEDIUM, HIGH } level;
} global_config = { 0, LOW };

// Typedef with function pointer
typedef int (*callback_t)(struct event {
    int type;
    void *data;
} *evt);

// Array with nested initialization
int matrix[3][3] = {
    {1, 2, 3},
    {func_call(struct temp { int x; } t), 5, 6},
    {7, 8, 9}
};

// Enum with function calls in values
enum codes {
    CODE_A = calculate_value(),
    CODE_B = get_constant(struct param { int val; } p),
    CODE_C = 100
};
