/*
Mutable local variables are bad because they introduce complexity to the
structure of the method. The complexity is in the form of dependencies
between the lines of code in the method. Those dependencies make the code
brittle: if you try to move something or change it, you break some other code
that is depending on its side effects.
*/

function calculateTax (amt) {
    var hst,
        assholeTax;

    hst = 0.13;
    assholeTax = 10;

    amt = amt * (1 + hst);
    amt = amt + assholeTax; // MUTATING A LOCAL VAR (amt)!! BAD!!

    return amt;
}

// change the order, BAD things happen.
function calculateTax2 (amt) {
    var hst,
        assholeTax;

    hst = 0.13;
    assholeTax = 10;

    amt = amt + assholeTax;
    amt = amt * (1 + hst);

    return amt;
}

console.log(
    calculateTax(10) // 21.30
)

console.log(
    calculateTax2(10) // 22.60 ... deceptively similar
)

function calculateTaxImmutable (amt) {
    var hst,
        assholeTax,

        withHst,
        withAssholeTax;

    hst = 0.13;
    assholeTax = 10;

    withHst        = amt * (1 + hst);
    withAssholeTax = withHst + assholeTax;

    return withAssholeTax;
}

function calculateTaxImmutable2 (amt) {
    var hst,
        assholeTax,

        withHst,
        withAssholeTax;

    hst = 0.13;
    assholeTax = 10;

    // flipped.
    withAssholeTax = withHst + assholeTax;
    withHst        = amt * (1 + hst);

    return withAssholeTax;
}

console.log(
    calculateTaxImmutable(10) // 21.30
)

console.log(
    calculateTaxImmutable2(10) // NaN... it's clear something's wrong.
                               // (But NaN/error is just an imperative result of
                               //  a functional principle. In a functional
                               //  language flipping order makes no difference
                               //  b/c of lazy evaluation)
)
