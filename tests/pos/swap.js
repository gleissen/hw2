function swap(n, z) {
    assume(n>=0);
    var x = n;
    var y = 3;
    while (x >=1) {
        x = x - 1;
        if (z==1) {
            y = 1;
            z = 3;
        }
        else {
            if (y==1) {
                y = 3;
                z = 1;
            }
        }
    }
    assert(z==3 || y==3);
}
