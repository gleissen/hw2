function foo(f) {
    var x = 0;
    var y = 0;
    while (x <= n) {
        x = x + 1;
        if (f==1) {
            y = y + 1;
        }
        else {
            y = y - 1;
        }
    }
    assert(f!=1 || y>=0);
}
