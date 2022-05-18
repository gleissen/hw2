function foo() {
    var x = 0;
    var y = 50;
    while (x <= 99) {
        x = x + 1;
        pred(x==y);
        pred(x<=50);
        pred(y==100);
        if (x >= 51) {
            y = y + 1;
        }
    }
    assert (y == 100);
}
