# Test for-loop

proc main():
    int i = 0
    int n = 10
    int sum = 0
    for i = 0 to 10:
        write(i)
    .

    for i = 0 to n:
        sum = sum + i
    .
    write(sum)
.