fun f(a: int, Cons(x, _xs), Just(Cons(_, Cons(y, _)))) {
    a + x + y
}

fun main() {
    f(2, [1], [2, 3]).println
}