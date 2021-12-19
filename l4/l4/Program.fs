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

let sampleFunctionsForTestinng = 
    [
        ("5", fun (x : float) -> 5.0);
        ("5", fun (x : float) -> 5.0);
        ("2x", fun (x : float) -> 2.0 * x);
        ("2x", fun (x : float) -> 2.0 * x)
        ("3x^2 + x", fun (x : float) -> 3.0 * x * x + x);
        ("4x^3 + 3x^2", fun (x : float) -> 4.0 * x * x * x + 3.0 * x * x);
        ("2x", fun (x : float) -> 2.0 * x);
        ("3x^2 + x", fun (x : float) -> 3.0 * x * x + x);
        ("4x^3 + 3x^2", fun (x : float) -> 4.0 * x * x * x + 3.0 * x * x) 
    ]

let integrals =
    [
        fun (x : float) -> 5.0 * x;
        fun (x : float) -> 5.0 * x;
        fun (x : float) -> x * x;
        fun (x : float) -> x * x;
        fun (x : float) -> x * x * x + 0.5 * x * x;
        fun (x : float) -> x * x * x * x + x * x * x;
        fun (x : float) -> x * x;
        fun (x : float) -> x * x * x + 0.5 * x * x;
        fun (x : float) -> x * x * x * x + x * x * x
    ]


let rec Process () =
    printfn "Lab 4.1. Integration using quadratic formulas. Variant 1."
    printfn "Input left boundary"
    let a = Console.ReadLine() |> float
    printfn "Input right boundary"
    let b = Console.ReadLine() |> float
    let quads = [quadLeftRect; quadRightRect; quadInterRect; quadInterTrap; quadSimpson; quadSimpson; threeAndEight; threeAndEight; threeAndEight]
    let quadNames = ["LeftRect"; "RightRect"; "InterRect"; "QuadTrap"; "Simpson"; "Simpson"; "3/8"; "3/8"; "3/8"]
    let data = sampleFunctionsForTestinng |> List.map snd |> List.zip quads
    let results = data |> List.map (fun (q, f) -> q a b f)
    let correctResults = integrals |> List.map (fun f -> (f b) - (f a))
    for i in 0 .. sampleFunctionsForTestinng.Length - 1 do
        printfn "Func: %s Form: %s Result: %A Correct: %A; Delta: %A" (sampleFunctionsForTestinng.[i] |> fst) (quadNames.[i]) (results.[i]) (correctResults.[i]) (Math.Abs(results.[i] - correctResults.[i])) 
    ()

Process ()