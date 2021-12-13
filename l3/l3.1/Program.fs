open System

let f x = Math.Exp(3.0 * x)

let f' x = 3.0 * Math.Exp(3.0 * x)

let f'' x = 9.0 * Math.Exp(3.0 * x)

let reverseTable table = table |> List.map (fun (x, y) -> (y, x))

let getGeneralTable a h m f = [0 .. m] |> List.map (fun i -> (a + i * h, f(a + i * h)))

let firstDerivative (table : list<float * float>) (h : float) =
    let (x, fx) = (table |> List.map fst, table |> List.map snd)
    let maxIndex = table.Length - 1
    let rec fillData iter data =
        match iter with
        | 0 -> fillData (iter + 1) (((-3.0 * fx.[iter] + 4.0 * fx.[iter + 1] - fx.[iter + 2]) / (2.0 * h)) :: data)
        | _ when iter = maxIndex -> ((3.0 * fx.[iter] - 4.0 * fx.[iter - 1] + fx.[iter - 2]) / (2.0 * h)) :: data
        | _ -> fillData (iter + 1) (((fx.[iter + 1] - fx.[iter - 1]) / (2.0 * h)) :: data)
    [] |> fillData 0 |> List.rev 


let secondDerivative (table : list<float * float>) (h : float) = 
    let (x, fx) = (table |> List.map fst, table |> List.map snd)
    let hSquare = Math.Pow(h, 2.0)
    let rec fillData iter (data : list<float>) =
        match iter with
        | 0 -> fillData (iter + 1) [0.0]
        | _ when iter = table.Length - 1 -> 0.0 :: data
        | _ -> fillData (iter + 1) (((fx.[iter + 1] - 2.0 * fx.[iter] + fx.[iter - 1]) / hSquare) :: data)
    [] |> fillData 0 |> List.rev


// Prints one raw of the table.
let printString (data : list<float>) = 
    let symbolsForRaw = 25
    let addSpace s = s + " "
    let content = data |> List.map (fun el -> string el)
    let rec getRaw (content : list<string>) (acc : string) =
        match content with
        | [] -> acc
        | head :: tail -> let l = head.Length
                          let rec getElement (res : string) = 
                              match res with
                              | _ when res.Length = symbolsForRaw -> res + "|"
                              | _ -> getElement (res + " ")
                          getRaw tail (acc + (getElement head))
    let result = getRaw content ""
    printfn "%s" result
    ()


let getResult (a : float) (h : float) (m : int) = 
    let x = [for i in 0 .. m do a + (float)i * h]
    let fx = x |> List.map f
    let table = List.zip x fx
    let correctResultOfF1 = x |> List.map f'
    let correctResultOfF2 = x |> List.map f''
    let first = firstDerivative table h
    let second = secondDerivative table h
    printfn "x fx f' f''" 
    for i in 0 .. x.Length - 1 do
        printString [x.[i]; fx.[i]; first.[i]; correctResultOfF1.[i]; second.[i]; correctResultOfF2.[i]]
    ()
    

[<EntryPoint>]
let main (arcg : string[]) =
    printfn "Lab 3.2. Variant number 1."
    printfn "Finding Derivatives of a table-specified function"
    printfn "Using numerical differentiation formulas"
    printfn "Function: exp(3x)"
    printfn "Input first point (a):"
    let a = Console.ReadLine() |> float
    printfn "Input step (h):"
    let h = Console.ReadLine() |> float
    printfn "Input count of steps (m):"
    let m = Console.ReadLine() |> int
    getResult a h m
    let s = "abc"
    0
