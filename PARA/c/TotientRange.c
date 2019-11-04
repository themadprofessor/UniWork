// TotientRange.c - Sequential Euler Totient Function (C Version)
// compile: gcc -Wall -O -o TotientRange TotientRange.c
// run:     ./TotientRange lower_num upper_num

// Greg Michaelson 14/10/2003
// Patrick Maier   29/01/2010 [enforced ANSI C compliance]

// This program calculates the sum of the totients between a lower and an
// upper limit using C longs. It is based on earlier work by:
// Phil Trinder, Nathan Charles, Hans-Wolfgang Loidl and Colin Runciman

// The comments provide (executable) Haskell specifications of the functions

#include <stdio.h>
#include <omp.h>

// hcf x 0 = x
// hcf x y = hcf y (rem x y)

long hcf(long x, long y)
{
    long t;

    while (y != 0) {
        t = x % y;
        x = y;
        y = t;
    }
    return x;
}


// relprime x y = hcf x y == 1

int relprime(long x, long y)
{
    return hcf(x, y) == 1;
}


// euler n = length (filter (relprime n) [1 .. n-1])

long euler(long n)
{
    long length, i;

    length = 0;
#pragma omp for simd
    for (i = 1; i < n; i++)
        if (relprime(n, i))
            length++;
    return length;
}


// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

long sumTotient(long lower, long upper)
{
    long sum, i;

    sum = 0;
#pragma omp target teams distribute parallel for default(none) shared(lower, upper) reduction(+:sum) schedule(static,1)
    for (i = lower; i <= upper; i++)
        sum += euler(i);
    return sum;
}


int main(int argc, char ** argv)
{
    long lower, upper;
    double start, end;

    if (argc != 3) {
        printf("not 2 arguments\n");
        return 1;
    }
    sscanf(argv[1], "%ld", &lower);
    sscanf(argv[2], "%ld", &upper);
    start = omp_get_wtime();
    printf("C: Sum of Totients  between [%ld..%ld] is %ld\n",
           lower, upper, sumTotient(lower, upper));
    end = omp_get_wtime();
    printf("Took %f seconds\n", (end - start));
    return 0;
}
