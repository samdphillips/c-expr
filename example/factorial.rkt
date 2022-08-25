#lang c-expr c-expr/example/lang

fn factorial(i) {
    factorial_help(1, i)
}

fn factorial_help(acc, i) {
    if (i == 1) {
        acc
    } else {
        factorial_help(acc * i, i - 1)
    }
}
