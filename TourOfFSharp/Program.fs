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

module IntegersAndNumbers =
    let int1 = 176
    printfn $"int1: {int1}"
    let aDouble = 4.1
    printfn $"aDouble: {aDouble}"
    let int2 = (int1/4 + 5 - 7) * 4 + int aDouble
    printfn $"int2: {int2}"

    let numbers = [ 0 .. 99 ]
    printfn $"numbers:\n{numbers}"
    let tableOfSquares = [ for i in 0 .. 99 -> (i, i*i) ]
    printfn $"tableOfSquares:\n{tableOfSquares}"

module Booleans =
    let bool1 = true
    let bool2 = false
    let bool3 = not bool1 && (bool2 || false)
    printfn $"bool3: %b{bool3}"

module StringManipulation =
    let str1 = "Hello"
    let str2 = "World"
    let helloWorld = str1 + ", " + str2 + "!"
    printfn "%s" helloWorld

    let substring = helloWorld[0..6]
    printfn $"{substring}"

    let str3 = @"C:\Program Files\"
    printfn $"{str3}"
    let str4 = """The computer said "hello world" when I told it to!"""
    printfn $"{str4}"