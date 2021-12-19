open System

let Split delta interval =
    [fst interval .. delta .. snd interval] |> List.fold (fun acc x -> (x, x + delta) :: acc) [] |> List.tail

let GetIntervals delta f (interval : double * double) =
    interval |> Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0) 

let SecantMethod f interval accuracy =
    let rec sub i (prev : float) (next : float) =
        if i > 99 then failwith "Something is wrong"
        if Math.Abs(next - prev) < accuracy then (next, i) else sub (i + 1) next (next - (f next) * (next - prev) / (f next - f prev))
    interval |> Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0)
             |> List.map (fun x -> (sub 0 (fst x) (snd x)))
             |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.0001) then acc else x :: acc) []

let legandre (x : float) (n : int) =
    let rec sub (i : int) =
        match i with
        | 0 -> 1.0
        | 1 -> x
        | 2 -> Math.Pow(x, 2.0) * 1.5 - 0.5
        | _ -> x * (float (2 * n - 1) / float n) * sub (i - 1) - (float (n - 1) / float n) * sub (i - 2)
    sub n

let getCoeffsForGauss xk n =
    let legandreResult = legandre xk (n - 1) 
    2.0 * (1.0 - xk * xk) / (float n *  float n * Math.Pow(legandreResult, 2.0))

// Mehler method.
let Mehler f n = 
    let pi = 3.1415926535
    let rec sub sum k =
        match k with
        | _ when k > n -> sum
        | _ -> let valueX = cos(pi * (2.0 * (float k) - 1.0) / (2.0 * (float n)))
               sub (sum + f(valueX)) (k + 1)
    let sum = sub 0.0 1
    pi * sum / (float n)
        

let a = legandre 1.0 3
printfn "%A" a

let f x = cos(x)

let g x = sin(x) / x 

let Process () =
    printfn "Lab 5. Gauss and Mehler quadratic formulas"
    let mehlerResult = Mehler f 10
    printfn "Mehler result: %A" mehlerResult
    let legandreRoots = (SecantMethod (fun x -> legandre x 3) (-2.0, 2.0) 0.0000000001) |> List.map fst
    let coeffs = legandreRoots |> List.map (fun x -> getCoeffsForGauss x legandreRoots.Length)
    let data = List.zip legandreRoots coeffs
    printfn "%A %A" legandreRoots (sqrt(3.0 / 5.0))
    printfn "Data %A " data

Process ()