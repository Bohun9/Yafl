#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#define UNIT 0

void match_error() {
    fprintf(stderr, "runtime error: non-exhaustive pattern matching\n");
    exit(1);
}

void division_error() {
    fprintf(stderr, "runtime error: division by zero\n");
    exit(1);
}

int64_t print_int(void* clo, int64_t n) {
    printf("%" PRId64, n);
    return UNIT;
}

int64_t read_int(void* clo, int64_t unit) {
    int64_t n;
    scanf("%" PRId64, &n);
    return n;
}

int64_t print_newline(void* clo, int64_t unit) {
    printf("\n");
    return UNIT;
}

int64_t print_space(void* clo, int64_t unit) {
    printf(" ");
    return UNIT;
}

struct toplevel_closure {
    void* fun_ptr; // toplevel does not use it
    void* env;     // toplevel saves it in its environment
};

int64_t __yafl_toplevel(void* clo, int64_t arg);

int main() {
    struct toplevel_closure closure = {
        .fun_ptr = NULL,
        .env = NULL
    };
    return __yafl_toplevel((void*) &closure, 0);
}
