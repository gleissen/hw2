function foo(n, y) {
    assume(n>=0);
    var x = 0;
    var z = 0;
    var b = 0;
    while (x <= n-1) {
        x = x + 1;
        b = b + 2;
        if (y>=1) {
            z = z + x;
        }
        if (y<=0) {
            z = z + b;
        }
    }
    assert(z >= 0);
}
