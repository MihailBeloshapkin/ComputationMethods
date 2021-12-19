open System

// 0
let quadLeftRect (a : float) (b : float) n h f = 
    let rec sub sum i =
        if i = n then sum else sub (sum + f(a + (float i) * h)) (i + 1)
    sub 0.0 0

// 0
let quadRightRect (a : float) (b : float) n h f = 
    let rec sub sum i =
        if i = n + 1 then sum else sub (sum + f(a + (float i) * h)) (i + 1)
    sub 0.0 1

// 1
let quadInterRect (a : float) (b : float) n h f = 
    // (b - a) * f ((a + b) / 2.0)
    let rec sub sum i =
        let xa = a + (float i) * h
        let xb = a + (float (i + 1)) * h
        if i = n - 1 then sum else sub (sum + f((xa + xb) / 2.0)) (i + 1)
    sub 0.0 0

// 1
let quadInterTrap (a : float) (b : float) n h f = 
    // (b - a) * (f(a) + f(b)) / 2.0
    let rec sub sum i =
        let fxa = f(a + (float i) * h)
        let fxb = f(a + (float (i + 1)) * h)
        if i = n - 1 then sum else sub (sum + (fxa + fxb) / 2.0) (i + 1)
    sub 0.0 0


// 3
let quadSimpson (a : float) (b : float) n h f = 
    // (b - a) * (f(a) + 4.0 * f((a + b) / 2.0) + f(b)) / 6.0
    let rec sub sum i =
        match i with
        | _ when i = n - 1 -> sum
        | _ -> let xa = a + (float i) * h
               let xb = a + (float (i + 1)) * h
               let localSum = (xb - xa) * (f(xa) + 4.0 * f((xa + xb) / 2.0) + f(xb)) / 6.0
               sub (sum + localSum) (i + 1)  
    sub 0.0 0

// 3
let threeAndEight (a : float) (b : float)  n h f =
//    let h = (b - a) / 3.0
//    (b - a) * (f(a) + 3.0 * f(a + h) + 3.0 * f(a + 2.0 * h) + f(b)) / 8.0
    let rec sub sum i =
        match i with
        | _ when i = n - 1 -> sum
        | _ -> let xa = a + (float i) * h
               let xb = a + (float (i + 1)) * h
               let localH = (xb - xa) / 3.0
               let localSum = (xb - xa) * (f(xa) + 3.0 * f(xa + localH) + 3.0 * f(xa + 2.0 * localH) + f(xb)) / 8.0
               sub (sum + localH) (i + 1)  
    sub 0.0 0


// Find maximum module of function.
let findMax (f : float -> float) (a : float) (n : int) (h : float) =
    let rec sub (localMax : float) i =
        if i = n then localMax else sub (Math.Max(localMax, Math.Abs(f(a + h * (float i))))) (i + 1)
    sub 0.0 0

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
    printfn "Lab 4.2 .Approximate calculation of the integral by compound quadrature formulas"
    printfn "Input A (left boundary of integration):"
    let a = Console.ReadLine() |> float
    printfn "Input B (right boundary of integration):"
    let b = Console.ReadLine() |> float
    printfn "Input m:"
    let m = Console.ReadLine() |> int
    let h = (b - a) / (float m)
    printfn "h: %A" h


Process ()