#include <stdlib.h>
#include <stdio.h>

void match_error() {
    printf("runtime error: non-exhaustive pattern matching\n");
    exit(1);
}

int64_t yafl();

int main() {
    return yafl();
}
