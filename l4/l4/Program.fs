open System

// 0
let quadLeftRect (a : float) (b : float) f = (b - a) * (f a)

// 0
let quadRightRect (a : float) (b : float) f = (b - a) * (f b)

// 1
let quadInterRect (a : float) (b : float) f = (b - a) * f ((a + b) / 2.0)

// 1
let quadInterTrap (a : float) (b : float) f = (b - a) * (f(a) + f(b)) / 2.0

// 3
let quadSimpson (a : float) (b : float) f = (b - a) * (f(a) + 4.0 * f((a + b) / 2.0) + f(b)) / 6.0

// 3
let threeAndEight (a : float) (b : float) f =
    let h = (b - a) / 3.0
    (b - a) * (f(a) + 3.0 * f(a + h) + 3.0 * f(a + 2.0 * h) + f(b)) / 8.0


let formulas = ["Left rectangle"; "Right rectangle"; "Inter rectangle"; "Trapezoid"; "Simpson"; "3/8"]

let quads = [quadLeftRect; quadRightRect; quadInterRect; quadInterTrap; quadSimpson; threeAndEight]

let polynomes = 
    [
        ((fun (x : float) -> 5.0), fun (x : float) -> 5.0 * x);
        ((fun (x : float) -> 2.0 * x), fun (x : float) -> x * x);
        ((fun (x : float) -> 3.0 * x * x + 2.0 * x), fun (x : float) -> x * x * x + x * x);
        ((fun (x : float) -> 4.0 * x * x * x + 3.0 * x * x + 2.0 * x), fun (x : float) -> x * x * x * x + x * x * x + x * x)
    ]

let polynomeNames =
    [
        "y = 5.0";
        "y = 2x";
        "y = 3x^2 + 2x";
        "y = 4x^3 + 3x^2 + 2x"
    ]

let getResults a b =
    for i in 0 .. formulas.Length - 1 do
        printfn "%s" formulas.[i]
        for j in 0 .. polynomes.Length - 1 do
            let result = quads.[i] a b (fst polynomes.[j])
            let correct = ((snd polynomes.[j]) b) - ((snd polynomes.[j]) a)
            printfn "Function: %s Result: %A Correct: %A Delta %f" polynomeNames.[j] result correct (Math.Abs(result - correct))
        printfn ""

let rec Process () =
    printfn "Input left boundary"
    let a = Console.ReadLine() |> float
    printfn "Input right boundary"
    let b = Console.ReadLine() |> float
   (* let quads = [quadLeftRect; quadRightRect; quadInterRect; quadInterTrap; quadSimpson; quadSimpson; threeAndEight; threeAndEight; threeAndEight]
    let quadNames = ["LeftRect"; "RightRect"; "InterRect"; "QuadTrap"; "Simpson"; "Simpson"; "3/8"; "3/8"; "3/8"]
    let data = sampleFunctionsForTestinng |> List.map snd |> List.zip quads
    let results = data |> List.map (fun (q, f) -> q a b f)
    let correctResults = integrals |> List.map (fun f -> (f b) - (f a))
    for i in 0 .. sampleFunctionsForTestinng.Length - 1 do
        printfn "Func: %s Form: %s Result: %A Correct: %A; Delta: %A" (sampleFunctionsForTestinng.[i] |> fst) (quadNames.[i]) (results.[i]) (correctResults.[i]) (Math.Abs(results.[i] - correctResults.[i])) 
    *)
    getResults a b
    printfn "Continue?[y]"
    let resp = Console.ReadLine()
    if resp = "y" then Process () else ()

printfn "Lab 4.1. Integration using quadratic formulas. Variant 1."
Process ()