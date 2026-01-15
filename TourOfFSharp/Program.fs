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

module Tuples =
    let tuple1 = (1, 2, 3)
    let tuple2 = (1, "hello", 3.1415)
    printfn $"tuple1: {tuple1}\ttuple2: {tuple2}"

    let swapElems (a, b) = (b, a)
    printfn $"The result of swapping (1, 2) is: {(swapElems (1,2))}"

    let structTuple = struct (1, 2)
    // let thisWillNotCompile: (int*int) = struct (1, 2)

    let convertFromStructTuple (struct(a, b)) = (a, b)
    printfn $"Struct tuple: {structTuple}\nReference tuple made from the struct tuple: {(structTuple |> convertFromStructTuple)}"
    let convertToStructTuple (a, b) = struct(a, b)
    printfn $"Reference tuple: {(1,2)}\nStruct tuple made from the reference tuple: {((1,2) |> convertToStructTuple)}"

module PipelinesAndComposition =
    let square x = x * x
    let addOne x = x + 1
    let isOdd x = x % 2 <> 0

    let numbers = [ 1; 2; 3; 4; 5 ]

    let squareOddValuesAndAddOne values =
        let odds = List.filter isOdd values
        let squares = List.map square odds
        let result = List.map addOne squares
        result
    printfn $"Processing {numbers} through 'squareOddValuesAndAddOne': {squareOddValuesAndAddOne numbers}"

    let squareOddValuesAndAddOneNested values =
        List.map addOne (List.map square (List.filter isOdd values))
    printfn $"Processing {numbers} through 'squareOddValuesAndAddOneNested': {squareOddValuesAndAddOneNested numbers}"

    let squareOddValuesAndAddOnePipeline values =
        values
        |> List.filter isOdd
        |> List.map square
        |> List.map addOne
    printfn $"Processing {numbers} through 'squareOddValuesAndAddOnePipeline': {squareOddValuesAndAddOnePipeline numbers}"

    let squareOddValuesAndAddOneShorterPipeline values =
        values
        |> List.filter isOdd
        |> List.map(fun x -> x |> square |> addOne)
    printfn $"Processing {numbers} through 'squareOddValuesAndAddOneShorterPipeline': {squareOddValuesAndAddOneShorterPipeline numbers}"

    let squareOddValuesAndAddOneComposition =
        List.filter isOdd >> List.map (square >> addOne)
    printfn $"Processing {numbers} through 'squareOddValuesAndAddOneComposition': {squareOddValuesAndAddOneComposition numbers}"

module Lists =
    let list1 = []
    let list2 = [1; 2; 3]
    let list3 = [
        1
        2
        3
    ]
    printfn $"Lists:\n{list1}\n{list2}\n{list3}"

    let numberList = [1..1000]
    let squares =
        numberList
        |> List.map (fun x -> x*x)
    printfn $"Squares: {squares}"
    let sumOfSquares =
        numberList
        |> List.filter (fun x -> x % 3 = 0)
        |> List.sumBy (fun x -> x * x)
    printfn $"Sum of squares of numbers divisible by 3 up to 1000: %d{sumOfSquares}"

    let daysList =
        [ for month in 1..12 do
            for day in 1..System.DateTime.DaysInMonth(2026, month) do
                yield System.DateTime(2026, month, day) ]
    printfn $"The first 5 days of 2026 are: {daysList |> List.take 5}"

    let blackSquares =
        [ for i in 0..7 do
            for j in 0..7 do
                if (i+j) % 2 = 1 then
                    yield (i, j) ]
    printfn $"Chess board black squares:\n{blackSquares}"

module Arrays =
    let array1 = [||]
    let array2 = [|"hello"; "world"; "and"; "hello"; "world"; "again"|]
    let array3 = [|1..1000|]
    let array4 =
        [| for word in array2 do
            if word.Contains("l") then
                yield word |]
    let evenNumbers = Array.init 1001 (fun n -> n * 2)
    let evenNumbersSlice = evenNumbers[0..500]
    array2[1] <- "WORLD!"
    printfn $"Arrays:\n%A{array1}\n%A{array2}\n%A{array3}\n%A{array4}\n%A{evenNumbers}\n%A{evenNumbersSlice}"

    for word in array4 do
        printfn $"Word: {word}"

    let sumOfLengthsOfWords =
        array2
        |> Array.filter (fun x -> x.StartsWith "h")
        |> Array.sumBy (fun x -> x.Length)
    printfn $"sumOfLengthsOfWords: %d{sumOfLengthsOfWords}"

module Sequences =
    let seq1 = Seq.empty
    let seq2 = seq { yield "hello"; yield "world"; yield "and"; yield "hello"; yield "world"; yield "again" }
    let seq3 =
        seq { for word in seq2 do
                if word.Contains("l") then
                    yield word }
    let seq4 = seq {1..1000}
    let seq5 = Seq.init 1001 (fun n -> n * 2)
    printfn $"Sequences:\n%A{seq1}\n%A{seq2}\n%A{seq3}\n%A{seq4}\n%A{seq5}"

    let rnd = System.Random()
    let rec randomWalk x =
        seq {
            yield x
            yield! randomWalk (x + rnd.NextDouble() - 0.5)
        }
    let first100ValuesOfRandomWalk =
        randomWalk 5.0
        |> Seq.truncate 100
        |> Seq.toList
    printfn $"first100ValuesOfRandomWalk: {first100ValuesOfRandomWalk}"

module RecursiveFunctions =
    let rec factorial n =
        if n = 0 then 1 else n * factorial (n-1)
    printfn $"Factorial of 6 is: %d{factorial 6}"

    let rec greatestCommonFactor a b =
        if a = 0 then b
        elif a < b then greatestCommonFactor a (b - a)
        else greatestCommonFactor (a - b) b
    printfn $"The greatest common factor of 300 and 620 is %d{greatestCommonFactor 300 620}"

    let rec sumList xs =
        match xs with
        | []    -> 0
        | y::ys -> y + sumList ys
    printfn $"The sum of 1-10 is: %d{sumList [1..10]}"

    let rec private sumListTailRecHelper accumulator xs =
        match xs with
        | []    -> accumulator
        | y::ys -> sumListTailRecHelper (accumulator+y) ys
    let sumListTailRecursive xs = sumListTailRecHelper 0 xs
    printfn $"The sum of 1-10 is: %d{sumListTailRecursive [1..10]}"

module RecordTypes =
    type ContactCard =
        { Name     : string
          Phone    : string
          Verified : bool }
    let contact1 =
        { Name = "Alf"
          Phone = "(206) 555-0157"
          Verified = false }
    let contactOnSameLine = { Name = "Alf"; Phone = "(206) 555-0157"; Verified = false }
    printfn $"Structural equality: {contact1 = contactOnSameLine}"

    let contact2 =
        { contact1 with
            Phone = "(206) 555-0112"
            Verified = true }
    printfn $"Record created by copy-and-update:\n{contact2}"

    let showContactCard (c: ContactCard) =
        c.Name + " Phone: " + c.Phone + (if not c.Verified then " (unverified)" else "")
    printfn $"Alf's contact card: {showContactCard contact1}"