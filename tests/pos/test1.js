function foo(n, y) {
    assume(n>=0);
    assume(y>=0);
    var x = 0;
    var z = 0;
    while (x <= n-1) {
        x = x + 1;
        if (x+y>=0) {
            z = z + x;
        }
    }
    assert(z >= x);
}
