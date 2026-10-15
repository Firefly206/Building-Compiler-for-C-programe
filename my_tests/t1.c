int main(void) {
    int a = 1 + 2 * 3;
    int b;
    b = a;
    for (int i = 0; i < 3; i = i + 1) {
        if (i == 1) b = b + 1; else ;
    }
    while (b > 0) b = b - 1;
    return -a;
}
