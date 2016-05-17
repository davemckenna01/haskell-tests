var y = 0;

function incr (x) {
    if (y !== 0) {
        return x + 2
    }

    return x + 1
}

incr (1)

// MIGHT be the same as:

1 + 1

// But it could also be

1 + 2

// if y is changed

// By allowing mutable state, we've destroyed purity, which
// destroys referential transparency.