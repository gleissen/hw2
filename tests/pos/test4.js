function minus(a, b) {
    assume(a >= 0);
    assume(b >= 0);
    assume(a >= b);
    var i = b;
    var r = a;
    while (i >= 1) {
        r = r - 1;
        i = i - 1;
    }
    assert(i == 0);
    assert(r == a - b);
}
