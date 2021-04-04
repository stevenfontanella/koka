fun f(a: int, Cons(x, _xs), Just(Cons(_, Cons(y, _))) : maybe<list<int>>) {
    a + x + y
}

fun g(_, _) {
    1
}

fun main() {
    f(2, [1], Just([2, 3])).println
}
