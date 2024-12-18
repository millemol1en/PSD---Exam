void main()
{
    print(fac(12));
    print(facT(12, 1));
    //  print numCalls;
    //  print numTailCalls;
}
int fac(int n)
{
    if (n == 0)
        return 1;
    else
        return n * fac(n - 1);
}
int facT(int n, int acc)
{
    if (n == 0)
        return acc;
    else
        return facT(n - 1, n * acc);
}