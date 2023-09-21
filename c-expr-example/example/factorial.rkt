#lang c-expr c-expr/example/lang
// #lang c-expr/read

fn factorial_help(acc, i) {
    if (i == 1) {
        acc
    } else {
        factorial_help(acc * i, i - 1)
    }
}

// fn factorial_help(a, b) { b }

fn factorial(i) {
    factorial_help(1, i)
}

if (1 == 0) {
  factorial(10)
} else {
  factorial(20)
}

