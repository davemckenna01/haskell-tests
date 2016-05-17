-- mutability for local vars is simply not possible in Haskell

-- immutable
calculateTax amt = 
    let hst = 0.13
        assholeTax = 10
        withHst = amt * (1 + hst)
        withAssholeTax = withHst + assholeTax
    in withAssholeTax


-- this won't compile
calculateTax_FAIL amt = 
    let hst = 0.13
        assholeTax = 10
        -- which is it?!?! Make up your mind, amt.
        amt = amt * (1 + hst)
        amt = amt + assholeTax
    in amt


-- and b/c of lazy evaluation, the order of the let statements doesn't matter
calculateTax2 amt = 
    let hst = 0.13
        assholeTax = 10
        -- flipped, but it doesn't matter!
        withAssholeTax = withHst + assholeTax
        withHst = amt * (1 + hst)
    in withAssholeTax