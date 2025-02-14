#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

void match_error() {
    printf("runtime error: non-exhaustive pattern matching\n");
    exit(1);
}

void division_error() {
    printf("runtime error: division by zero\n");
    exit(1);
}

int64_t print_int(void* _, int64_t n) {
    printf("%" PRId64, n);
    return 0;
}

int64_t read_int(void* _1, int64_t _2) {
    int64_t n;
    scanf("%" PRId64, &n);
    return n;
}

int64_t print_newline(void* _, int64_t n) {
    printf("\n");
    return 0;
}

int64_t print_space(void* _, int64_t n) {
    printf(" ");
    return 0;
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
