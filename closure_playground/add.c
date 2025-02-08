#include<stdio.h>
#include <stdlib.h>

/*
let add x =
  let f y = 
    x + y
  in 
  f
in
print (add 1 2)
*/

struct top_level_env {};

struct add_env {
    struct top_level_env* enclosing_env;
    int x;
};

// The closure structure for functions of the type int -> int
struct f_clo {
    int (*code)(void*, int);
    void* enclosing_env;
};

int f_code(void* _f, int y) {
    struct f_clo* f = (struct f_clo*) _f;
    struct add_env* env = (struct add_env*) f->enclosing_env;
    return env->x + y;
}

// The closure structure for functions of the type int -> (int -> int)
struct add_clo {
    struct f_clo* (*code)(void*, int);
    void* enclosing_env;
};

struct f_clo* add_code(void* _add, int x) {
    struct add_clo* add = (struct add_clo*) _add;
    struct add_env* cur_env = malloc(sizeof(struct add_env));
    cur_env->enclosing_env = add->enclosing_env;
    cur_env->x = x;

    struct f_clo* f = malloc(sizeof(struct f_clo));
    f->code = f_code;
    f->enclosing_env = cur_env;
    return f;
}

int main() {
    struct top_level_env* cur_env = malloc(sizeof(struct top_level_env));

    struct add_clo* add = malloc(sizeof(struct add_clo));
    add->code = add_code;
    add->enclosing_env = cur_env;
    
    struct f_clo* add1 = add->code((void*) add, 1);
    int res = add1->code((void*) add1, 2);

    printf("%d\n", res);
}
