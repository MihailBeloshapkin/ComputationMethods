open System

let F x = -cos(x) + 0.75 * Math.Pow(x, 4.0) + 0.5 * Math.Exp(2.0 * x)

let f x = sin(x) + 3.0 * Math.Pow(x, 3.0) + Math.Exp(2.0 * x) 

let f' x = cos(x) + 9.0 * Math.Pow(x, 2.0) + 2.0 * Math.Exp(2.0 * x)

let f'' x = -sin(x) + 18.0 * x + 4.0 * Math.Exp(2.0 * x)

let f''' x = -cos(x) + 18.0 + 8.0 * Math.Exp(2.0 * x)

let f'''' x = sin(x) + 16.0 * Math.Exp(2.0 * x)


// 0
let quadLeftRect (a : float) (b : float) n h f = 
    let rec sub sum i =
        if i = n then sum else sub (sum + f(a + (float i) * h)) (i + 1)
    h * (sub 0.0 0)

// 0
let quadRightRect (a : float) (b : float) n h f = 
    let rec sub sum i =
        if i = n + 1 then sum else sub (sum + f(a + (float i) * h)) (i + 1)
    h * (sub 0.0 1)

// 1
let quadInterRect (a : float) (b : float) n h f = 
    // (b - a) * f ((a + b) / 2.0)
    let rec sub sum i =
        let xa = a + (float i) * h
        let xb = a + (float (i + 1)) * h
        if i = n then sum else sub (sum + f((xa + xb) / 2.0)) (i + 1)
    h * (sub 0.0 0)

// 1
let quadInterTrap (a : float) (b : float) n h f = 
    // (b - a) * (f(a) + f(b)) / 2.0
    let rec sub sum i =
        let fxa = f(a + (float i) * h)
        let fxb = f(a + (float (i + 1)) * h)
        if i = n then sum else sub (sum + (fxa + fxb) / 2.0) (i + 1)
    h * (sub 0.0 0)


// 3
let quadSimpson (a : float) (b : float) n h f = 
    // (b - a) * (f(a) + 4.0 * f((a + b) / 2.0) + f(b)) / 6.0
    let rec sub sum i =
        match i with
        | _ when i = n -> sum
        | _ -> let xa = a + (float i) * h
               let xb = a + (float (i + 1)) * h
               let localSum = (xb - xa) * (f(xa) + 4.0 * f((xa + xb) / 2.0) + f(xb)) / 6.0
               sub (sum + localSum) (i + 1)  
    (sub 0.0 0)


let threeAndEight (a : float) (b : float)  n h f =
//    let h = (b - a) / 3.0
//    (b - a) * (f(a) + 3.0 * f(a + h) + 3.0 * f(a + 2.0 * h) + f(b)) / 8.0
    let rec sub sum i =
        match i with
        | _ when i = n -> sum
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

let formulas = ["Left rectangle"; "Right rectangle"; "Inter rectangle"; "Trapezoid"; "Simpson"; (*"3/8"*)]

let quads = [quadLeftRect; quadRightRect; quadInterRect; quadInterTrap; quadSimpson; (*threeAndEight*)]

let polynomes = 
    [
        ((fun (x : float) -> 5.0), fun (x : float) -> 5.0 * x);
        ((fun (x : float) -> 2.0 * x), fun (x : float) -> x * x);
        ((fun (x : float) -> 3.0 * x * x + 2.0 * x), fun (x : float) -> x * x * x + x * x);
        ((fun (x : float) -> 4.0 * x * x * x + 3.0 * x * x + 2.0 * x), fun (x : float) -> x * x * x * x + x * x * x + x * x);
        (f, F)
    ]

let derivatives =
    [
        [ 
          (fun (x : float) -> 0.0); 
          (fun (x : float) -> 2.0); 
          (fun (x : float) -> 6.0 * x + 2.0); 
          (fun (x : float) -> 12.0 * x * x + 6.0 * x + 2.0); 
          f'
        ];
        
        [ (fun (x : float) -> 0.0);
          (fun (x : float) -> 0.0); 
          (fun (x : float) -> 6.0);
          (fun (x : float) -> 24.0 * x + 6.0);
          f''
        ];
        [
          (fun (x : float) -> 0.0);
          (fun (x : float) -> 0.0); 
          (fun (x : float) -> 0.0);
          (fun (x : float) -> 24.0);
          f'''
        ];
        [
          (fun (x : float) -> 0.0);
          (fun (x : float) -> 0.0); 
          (fun (x : float) -> 0.0);
          (fun (x : float) -> 0.0);
          f''''
        ]
    ]

let polynomeNames =
    [
        "y = 5.0";
        "y = 2x";
        "y = 3x^2 + 2x";
        "y = 4x^3 + 3x^2 + 2x"
        "y = sin(x) + 3 * x^3 + Exp(2.0 * x)"
    ]


let getTheoreticlaDelta f derivative (a : float) (b : float) n (h : float) d constant =
    let m = findMax derivative a n h
    constant * (b - a) * Math.Pow(h, float d + 1.0) * m

// let Rhongee l h d J =
//    (Math.Pow(l, float d + 1.0) J())
    

let getResults a b n h =
    for i in 0 .. formulas.Length - 1 do
        printfn "%s" formulas.[i]
        for j in 0 .. polynomes.Length - 1 do
            let result = quads.[i] a b n h (fst polynomes.[j])
            let correct = ((snd polynomes.[j]) b) - ((snd polynomes.[j]) a)
            let ast = match i with | 0 -> 0 | 1 -> 0 | 2 -> 1 | 3 -> 1 | 4 -> 3
            let coeff = match i with 0 -> 0.5 | 1 -> 0.5 | 2 -> 1.0 / 24.0 | 3 -> 1.0 / 12.0 | 4 -> 1.0 / 2880.0
            let theorDelta = getTheoreticlaDelta (fst polynomes.[j]) derivatives.[ast].[j] a b n h ast coeff
            printfn "Function: %s Result J(h): %A Correct: %A Delta J(h) %A Theoretical delta: %A" polynomeNames.[j] result correct (Math.Abs(result - correct)) theorDelta
        printfn ""

let getResultsWithL a b n h l =
    for i in 0 .. formulas.Length - 1 do
        printfn "%s" formulas.[i]
        for j in 0 .. polynomes.Length - 1 do
            let Jh = quads.[i] a b n h (fst polynomes.[j])
            let correct = ((snd polynomes.[j]) b) - ((snd polynomes.[j]) a)
            let ast = match i with | 0 -> 0 | 1 -> 0 | 2 -> 1 | 3 -> 1 | 4 -> 3
            let coeff = match i with 0 -> 0.5 | 1 -> 0.5 | 2 -> 1.0 / 24.0 | 3 -> 1.0 / 12.0 | 4 -> 1.0 / 2880.0
            
            let newN = n * l
            let newH = (b - a) / float newN
            let Jhl = quads.[i] a b newN newH (fst polynomes.[j])
            let _J = (Math.Pow(float l, float ast + 1.0) * Jhl - Jh) / (Math.Pow(float l, float ast + 1.0) - 1.0)
            printfn "Function: %s Result J(h/l): %A Correct: %A Delta J(hl) %A J_ : %A" polynomeNames.[j] Jhl correct (Math.Abs(Jhl - correct)) _J
            
            
        printfn ""

 
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
    getResults a b m h
    printfn "Input l:"
    let l = Console.ReadLine() |> int
    let newH = h / float l
    printfn "New h: %A" newH
    getResultsWithL a b m h l
    
    printfn "Restart?[y]"
    let response = Console.ReadLine()
    if response = "y" then Process () else ()

Process ()