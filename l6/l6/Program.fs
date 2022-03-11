open System

let f x = sin(x)

let rho x = Math.Sqrt(x)


let Split (delta : float) (interval : float * float) =
    [fst interval .. delta .. snd interval] |> List.fold (fun acc x -> (x, x + delta) :: acc) [] |> List.tail

let GetIntervals delta f (interval : float * float) =
    interval |> Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0) 

let SecantMethod f interval accuracy =
    let rec sub i (prev : float) (next : float) =
        if i > 99 then failwith "Something is wrong"
        if Math.Abs(next - prev) < accuracy then (next, i) else sub (i + 1) next (next - (f next) * (next - prev) / (f next - f prev))
    interval |> Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0)
             |> List.map (fun x -> (sub 0 (fst x) (snd x)))
             |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.000000001) then acc else x :: acc) []

let legandre (x : float) (n : int) =
    let rec sub (i : int) =
        match i with
        | 0 -> 1.0
        | 1 -> x
        | 2 -> x * x * 1.5 - 0.5
        | 3 -> 0.5 * x * (5.0 * x * x - 3.0)
        | 4 -> 0.125 * (35.0 * Math.Pow(x, 4.0) - 30.0 * x * x + 3.0)
        | 5 -> 0.125 * (63.0 * Math.Pow(x, 5.0) - 70.0 * Math.Pow(x, 3.0) + 15.0 * x)
        | 6 -> (231.0 * Math.Pow(x, 6.0) - 315.0 * Math.Pow(x, 4.0) + 105.0 * x * x - 5.0) * 1.0 / 16.0
    //    | 7 -> (429.0 * Math.Pow(x, 7.0) - 639.9 * Math.Pow(x, 5.0) + 315.0 * Math.Pow(x, 3.0) - 35.0 * x) * 1.0 / 16.0
        | _ -> x * ((float (2 * n - 1)) / float n) * sub (i - 1) - ((float (n - 1)) / float n) * sub (i - 2)
    sub n

let getCoeffsForGauss (xk : float) n =
    let legandreResult = legandre xk (n - 1) 
    2.0 * (1.0 - float (xk * xk)) / (float (n * n) * Math.Pow(float legandreResult, 2.0))

let ProcessGauss (a : float) (b : float) =
    let sum data func =
        let rec sub acc l =
            match l with
            | [] -> acc
            | head :: tail -> let sl = (snd head) * func(fst head)
                              // printfn "xk: %A Ak: %A %A" (fst head) (snd head) sl
                              sub (acc + sl) tail
        sub 0.0 data
    
    printfn "Input N (count od intervals):"
    let N = Console.ReadLine() |> int
    
    printfn "Input count of knots (M):"
    let M = Console.ReadLine() |> int 

    let roots = (SecantMethod (fun x -> legandre x M) (-1.0, 1.0) 0.0000000000001) |> List.map fst
    let coeffs = roots |> List.map (fun xk -> getCoeffsForGauss xk M)

    for i in 0 .. roots.Length - 1 do printfn "x: %A -> Ak: %A" roots.[i] coeffs.[i]
    printfn "Sum: %.12f" (coeffs |> List.fold (fun acc x -> acc + x) 0.0)
    let intervals = [for i in 0 .. N do a + float i * (b - a) / (float N)]
    let changeVariable a b x = x * (b - a) / 2.0 + (a + b) / 2.0

    let rec getFinalIntegral iter acc =
        match iter with
        | _ when iter = intervals.Length - 1 -> acc
        | _ -> let localA = intervals.[iter]
               let localB = intervals.[iter + 1]
               let newFunction x = x |> changeVariable localA localB |> (fun x -> (f x) * (rho x))
               let localIntegral = (sum (List.zip roots coeffs) newFunction) * (localB - localA) / 2.0
               getFinalIntegral (iter + 1) (acc + localIntegral)
         
    
//    let newFunction x = x |> changeVariable a b |> g
//    let integral = (sum (List.zip roots coeffs) newFunction) * (b - a) / 2.0
    let result = getFinalIntegral 0 0.0
    printfn "Integral value: %.12f" result
    result


let CompositeGauss () =
    printfn ""

let getKnots a b k  =
    let integral x = Math.Pow(x, float k + 1.5) / (float k + 1.5)
    (integral b) - (integral a)

let testFunction x = x * x * x

let knotsForTestFunction a b k = 
    let integral x = Math.Pow(x, float k + 1.0) / (float k + 1.0)
    (integral b) - (integral a)

let ComputeQuadFormula a b N func (knotsFunction : float -> float -> int -> float) = 
    let mu = [for k in 0 .. 2 * N - 1 do knotsFunction a b k]
    for i in 0 .. mu.Length - 1 do printfn "mu %A -> %A" i mu.[i]
    let a1 = (mu.[0] * mu.[3] - mu.[2] * mu.[1]) / (mu.[1] * mu.[1] - mu.[2] * mu.[0])
    let a2 = (mu.[2] * mu.[2] - mu.[3] * mu.[1]) / (mu.[1] * mu.[1] - mu.[2] * mu.[0])
    printfn "Polynom: x^2 + %A x + %A" a1 a2
    let x1 = (-a1 + Math.Sqrt(a1 * a1 - 4.0 * a2)) / 2.0
    let x2 = (-a1 - Math.Sqrt(a1 * a1 - 4.0 * a2)) / 2.0
    printfn "x1 = %A x2 = %A" x1 x2 
    let A1 = (mu.[1] - x2 * mu.[0]) / (x1 - x2)
    let A2 = (mu.[1] - x1 * mu.[0]) / (x2 - x1)
    let integral = A1 * func(x1) + A2 * func(x2)
    printfn "Integral: %.12f" integral
    integral

let rec ProcessTypeGauss a b =
    printfn "Test?[y]"
    let resp1 = Console.ReadLine() |> string
    if resp1 = "y" then 
        printfn "\n=============== Test =============\nFunction: y = x^3 a = 0.0 b = 4.0"
        let result = ComputeQuadFormula 0.0 4.0 2 testFunction knotsForTestFunction
        let testIntegr x = Math.Pow(x, 4.0) / 4.0
        let correct = (testIntegr 4.0) - (testIntegr 0.0)
        printfn "Correct: %A Delta: %.12f" correct (Math.Abs(result - correct)) 
        printfn "==================================\n"
    let N = 2
    ComputeQuadFormula a b N f getKnots
    


let rec Process () =
    printfn "Input A:"
    let a = Console.ReadLine() |> float
    printfn "Input B:"
    let b = Console.ReadLine() |> float
    printfn "\nComposite Gauss\n"
    let compositeGaussResult = ProcessGauss a b
    printfn "\nType Gauss\n"
    let typeGaussResult = ProcessTypeGauss a b
    printf "\n\n Delta: %.12f \n\n" (Math.Abs(compositeGaussResult - typeGaussResult))
    printfn "Continue?[y]"
    let resp = Console.ReadLine()
    if resp = "y" then Process() else ()


[<EntryPoint>]
let main (argc : string[]) =
    printfn "Lab 6. Approximate calculation of integrals using quadrature formulas"
    printfn "f(x) = sin(x); rho(x) = sqrt(x)"
    Process() 
    0