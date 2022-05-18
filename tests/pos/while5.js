// Example done in class
function foo() {
    var x = 0;
    while (x <= 5) {
        pred(x <= 6);
        pred(x >= 6);
        x = x + 1;
    }
    assert(x == 6);
}
