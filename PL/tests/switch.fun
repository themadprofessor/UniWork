# Test switch statement

proc main():
    int x = 5

    switch x:
        case 1:
            write(1)
        case 2:
            write(2)
        default:
            write(0)
    .

    switch x:
        case 1..3:
            write(1)
        default:
            write(0)
    .
.