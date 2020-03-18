proc main():
    int x = 1

# Invalid case type
    switch x:
        case true:
        default:
    .

# Equal cases
    switch x:
        case 1:
        case 1:
        default:
    .

# Overlapping cases
    switch x:
        case 1..3:
        case 2..4:
        case 3:
        default:
    .

# Invalid range
    switch x:
        case 3..1:
        default:
    .
.