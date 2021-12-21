open System

let pi = 3.1415926535

let Split (delta : decimal) (interval : decimal * decimal) =
    [fst interval .. delta .. snd interval] |> List.fold (fun acc x -> (x, x + delta) :: acc) [] |> List.tail

let GetIntervals delta f (interval : decimal * decimal) =
    interval |> Split 0.03M |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0) 

let SecantMethod f interval accuracy =
    let rec sub i (prev : decimal) (next : decimal) =
        if i > 99 then failwith "Something is wrong"
        if Math.Abs(next - prev) < accuracy then (next, i) else sub (i + 1) next (next - (f next) * (next - prev) / (f next - f prev))
    let newInterval = (interval |> fst |> decimal, interval |> snd |> decimal)
    newInterval |> Split 0.03M |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0M)
                |> List.map (fun x -> (sub 0 (fst x) (snd x)))
                |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.000000001M) then acc else x :: acc) []

let legandre (x : decimal) (n : int) =
    let rec sub (i : int) =
        match i with
        | 0 -> 1M
        | 1 -> x
        | 2 -> x * x * 1.5M - 0.5M
        | 3 -> 0.5M * x * (5.0M * x * x - 3.0M)
        | _ -> x * ((decimal (2 * n - 1)) / decimal n) * sub (i - 1) - ((decimal (n - 1)) / decimal n) * sub (i - 2)
    sub n

let getCoeffsForGauss (xk : decimal) n =
    let legandreResult = legandre xk (n - 1) 
    2.0 * (1.0 - float (xk * xk)) / (float n *  float n * Math.Pow(float legandreResult, 2.0))

// Mehler method.
let Mehler f n = 
    let rec sub sum k =
        match k with
        | _ when k > n -> sum
        | _ -> let valueX = cos(pi * (2.0 * (float k) - 1.0) / (2.0 * (float n)))
               sub (sum + f(valueX)) (k + 1)
    let sum = sub 0.0 1
    pi * sum / (float n)


let a = legandre 1.0M 3
printfn "%A" a

let f x = cos(x)

let g x = sin(x) / x

let Gauss a b N =
    let legandreRoots = (SecantMethod (fun x -> legandre x N) (-1.0, 1.0) 0.0000000001M) |> List.map fst
    let coeffs = legandreRoots |> List.map (fun xk -> getCoeffsForGauss xk N)
    for i in 0 .. coeffs.Length - 1 do printfn "x: %A Ak: %A" legandreRoots.[i] coeffs.[i]
    0
    
let names =
    [
        "6x^5 + 4x^3 + 2x"
    ]

let polynomes =
    [
        ((fun (x: float) -> 6.0 * Math.Pow(x, 5.0) + 4.0 * Math.Pow(x, 3.0) + 2.0 * x + 1.0), (fun (x : float) -> Math.Pow(x, 6.0) + Math.Pow(x, 4.0) + x * x + x))
    ]

let rec ProcessGauss () =
    for i in 1 .. 8 do
        let legandreRoots = (SecantMethod (fun x -> legandre x i) (-1.0, 1.0) 0.0000000000001M) |> List.map fst
        let coeffs = legandreRoots |> List.map (fun xk -> getCoeffsForGauss xk i)
        printfn "N = %A" i
        for j in 0 .. coeffs.Length - 1 do
            printfn "x: %A coeff: %A" legandreRoots.[j] coeffs.[j]
        printfn "Check Sum: %A" (coeffs |> List.fold (fun acc x -> acc + x) 0.0)
        
    for i in 0 .. polynomes.Length - 1 do
        let legandreRoots = (SecantMethod (fun x -> legandre x 3) (-1.0, 1.0) 0.0000000000001M) |> List.map fst
        let coeffs = legandreRoots |> List.map (fun xk -> getCoeffsForGauss xk 3)
        let data = List.zip legandreRoots coeffs
        let currentPolynome = polynomes.[i] |> fst
        let currentFPolynome = polynomes.[i] |> snd
        let correct = (currentFPolynome 1.0) - (currentFPolynome -1.0)
        let integral = data |> List.fold (fun acc x -> acc + (snd x) * currentPolynome(x |> fst |> float)) 0.0
        printfn "Polynome Integral: %A Correct: %A Delta: %A" integral correct (Math.Abs(integral - correct))

    printfn "Input A:"
    let a = Console.ReadLine() |> float
    printfn "Input B"
    let b = Console.ReadLine() |> float
    printfn "Input N:"
    let N = Console.ReadLine() |> int
//    Gauss a b N
    ()
    (* let legandreRoots = (SecantMethod (fun x -> legandre x 3) (-2.0, 2.0) 0.0000000001) |> List.map fst
    let coeffs = legandreRoots |> List.map (fun x -> getCoeffsForGauss x legandreRoots.Length)
    let data = List.zip legandreRoots coeffs
    printfn "%A %A" legandreRoots (sqrt(3.0 / 5.0))
    let integralGauss = data |> List.fold (fun acc (xk, Ak) -> acc + Ak * g(xk)) 0.0
    for (xk, Ak) in data do printfn "%A -> %A -> %A" xk Ak (Ak * g(xk))
    printfn "Gauss Integral: %A" integralGauss
    printfn "Data %A " data
    *)

let rec ProcessMehler () =
    printfn "Input n1:"
    let n1 = Console.ReadLine() |> int
    printfn "Input n2"
    let n2 = Console.ReadLine() |> int
    printfn "Input n3"
    let n3 = Console.ReadLine() |> int
    
    printfn ""
    printfn "N = %A" n1
    let xValues1 = [for k in 0 .. n1 do cos(pi * (2.0 * (float k) - 1.0) / (2.0 * (float n1)))]
    for i in 0 .. xValues1.Length - 1 do printfn "x: %A coeff: %A" xValues1.[i] (pi / float n1) 
    printfn "Mehler integral: %A" (Mehler f n1)

    printfn ""
    printfn "N = %A" n2
    let xValues2 = [for k in 0 .. n2 do cos(pi * (2.0 * (float k) - 1.0) / (2.0 * (float n2)))]
    for i in 0 .. xValues2.Length - 1 do printfn "x: %A coeff: %A" xValues2.[i] (pi / float n2) 
    printfn "Mehler integral: %A" (Mehler f n2)

    printfn ""
    printfn "N = %A" n3
    let xValues3 = [for k in 0 .. n3 do cos(pi * (2.0 * (float k) - 1.0) / (2.0 * (float n3)))]
    for i in 0 .. xValues3.Length - 1 do printfn "x: %A coeff: %A" xValues3.[i] (pi / float n3)
    printfn "Mehler integral: %A" (Mehler f n3)

    printfn "Continue?[y]"
    let resp = Console.ReadLine()
    if resp = "y" then ProcessMehler() else ()

(*
printfn "%A" (5F/3F)
printfn "%A" (double 1/ double 3)
printfn "%A" (1M/3M)
*)

printfn "Lab 5. Gauss and Mehler quadratic formulas"
printfn "%A" (legandre (decimal 0.774596669) 3)
printfn "1 -- Gauss"
printfn "2 -- Mehler"
let res = Console.ReadLine()
match res with | "1" -> ProcessGauss() | "2" -> ProcessMehler() | _ -> ()
