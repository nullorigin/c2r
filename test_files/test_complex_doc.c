// Test file for document callback system
#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 100
#define DEBUG_PRINT(x) printf("Debug: %s\n", x)
#ifdef FEATURE_ENABLED
#define FEATURE_FLAG 1
#endif

// Global variables for testing
static const int global_constant = 42;
extern volatile int hardware_register;
int shared_counter = 0;

// Complex typedef for function pointer
typedef int (*ProcessFunction)(const char* input, size_t length);

// Enum with explicit values
typedef enum {
    STATE_IDLE = 0,
    STATE_RUNNING = 1,
    STATE_PAUSED = 2,
    STATE_ERROR = 99
} SystemState;

// Struct definition
typedef struct {
    char name[64];
    int id;
    SystemState state;
    ProcessFunction processor;
} ProcessorConfig;

// Array declarations
static const char* error_messages[10];
int buffer[MAX_SIZE];

// Function declarations and definitions
extern int external_function(void);

static inline void debug_function(const char* message) {
    DEBUG_PRINT(message);
}

ProcessorConfig* create_processor(const char* name, int id) {
    ProcessorConfig* config = malloc(sizeof(ProcessorConfig));
    if (config) {
        strcpy(config->name, name);
        config->id = id;
        config->state = STATE_IDLE;
        config->processor = NULL;
    }
    return config;
}

/* Multi-line comment block
   TODO: Add error handling
   FIXME: Memory leak possible
   NOTE: This is a complex structure
*/
void process_data(ProcessorConfig* config, const char* data) {
    if (!config || !data) return;
    
    // Inline comment about state check
    if (config->state != STATE_RUNNING) {
        config->state = STATE_ERROR;
        return;
    }
    
    if (config->processor) {
        config->processor(data, strlen(data));
    }
}
