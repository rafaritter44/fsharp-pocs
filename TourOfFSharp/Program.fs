module BasicFunctions =
    let f1 x = x*x + 3
    let result1 = f1 4573
    printfn $"result1: %d{result1}"

    let f2 (x:int) = 2*x*x - x/5 + 3
    let result2 = f2 (7 + 4)
    printfn $"result2: %d{result2}"

    let f3 x =
        if x < 100.0 then
            2.0*x*x - x/5.0 + 3.0
        else
            2.0*x*x + x/5.0 - 37.0
    let result3 = f3 (6.5 + 4.5)
    printfn $"result3: %f{result3}"

module Immutability =
    let number = 2
    printfn $"number: {number}"

    let mutable otherNumber = 2
    printfn $"otherNumber: {otherNumber}"
    otherNumber <- otherNumber + 1
    printfn $"otherNumber after mutation: {otherNumber}"