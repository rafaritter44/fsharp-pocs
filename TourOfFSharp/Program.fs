module BasicFunctions =
    let f1 x = x*x + 3
    let result1 = f1 4573
    printfn $"result1: %d{result1}"

    let f2 (x:int) = 2*x*x - x/5 + 3
    let result2 = f2 (7 + 4)
    printfn $"result2: %d{result2}"