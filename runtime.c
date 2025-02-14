#include <stdlib.h>
#include <stdio.h>

void match_error() {
    printf("runtime error: non-exhaustive pattern matching\n");
    exit(1);
}

void division_error() {
    printf("runtime error: division by zero\n");
    exit(1);
}

struct toplevel_closure {
    void* fun_ptr; // toplevel does not use it
    void* env;     // toplevel saves it in its environment
};

int64_t yafl_toplevel(void* clo, int64_t arg);

int main() {
    struct toplevel_closure closure = {
        .fun_ptr = NULL,
        .env = NULL
    };
    return yafl_toplevel((void*) &closure, 0);
}
