proc main():
    int x = 2

# No default
    switch x:
        case 1:
            write(0)
    .

# Non-literal case
    switch 1:
        case x:
        default:
    .

# Non int range
    switch x:
        case true..false:
        default:
.