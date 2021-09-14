open System

type Computation () =
    /// Split the interval.    
    static member Split delta interval =
        [fst interval .. delta .. snd interval] |> List.fold (fun acc x -> (x, x + delta) :: acc) [] |> List.tail

    static member Process sub (interval : double * double) f =
        interval |> Computation.Split 0.1 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0) 
                 |> List.map (fun x -> sub (fst x) (snd x)) 
                 |> List.fold (fun acc (x : float) -> if acc |> List.exists (fun a -> Math.Abs(a - x) < 0.0001) then acc else x :: acc) []

    static member GetIntervals delta f (interval : double * double) =
        interval |> Computation.Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0) 
        

    /// Bisection method.
    static member BisectionMethod f interval (accuracy : float) =
        let rec sub i la lb =
            if f la = 0.0 then (la, i)
            elif f lb = 0.0 then (lb, i)
            elif lb - la < accuracy then ((lb + la) / 2.0, i)
            elif (f la) * (f ((la + lb) / 2.0)) < 0.0 then sub (i + 1) la ((la + lb) / 2.0)
            else sub (i + 1) ((la + lb) / 2.0) lb 
        interval |> Computation.GetIntervals 0.03 f |> List.map (fun x -> sub 0 (fst x) (snd x))  
                 |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.0001) then acc else x :: acc) []
           
    /// Newton method.
    static member NewtonMethod f derF (interval : float * float) (accuracy : float) = 
        let rec sub i (prev : float) next = 
            if Math.Abs(next - prev) < accuracy then (next, i) else sub (i + 1) next (next - (f next) / (derF next))
        interval |> Computation.GetIntervals 0.03 f |> List.map (fun x -> sub 0 (fst x) (snd x)) 
                 |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.0001) then acc else x :: acc) []
           
    // Advanced newton method
    static member AdvncedNewtonMethod f derF (interval : float * float) (accuracy : float) = 
        let rec sub i x0 (prev : float) next = 
            if Math.Abs(next - prev) < accuracy then (next, i) else sub (i + 1) x0 next (next - (f next) / x0)
        interval |> Computation.Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0) 
                 |> List.map (fun x -> sub 0 (derF (fst x)) (fst x) (snd x)) 
                 |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.0001) then acc else x :: acc) []
           
    
    /// Secant method.
    static member SecantMethod f interval accuracy =
        let rec sub i (prev : float) (next : float) =
            if i > 99 then failwith "Something is wrong"
            if Math.Abs(next - prev) < accuracy then (next, i) else sub (i + 1) next (next - (f next) * (next - prev) / (f next - f prev))
        interval |> Computation.Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0)
                 |> List.map (fun x -> (sub 0 (fst x) (snd x)))
                 |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.0001) then acc else x :: acc) []
           
let f0 x = x - 10.0 * sin(x)
let derF0 x = 1.0 - 10.0 * cos(x)
let rightResults = [-2.865390124; 0.0; 2.865390124]
/// let getRes (a : float * int) f =  printfn "%A" (fst a, snd a, Math.Abs((fst a) - b))
let newGetRes (a : float *  int) = printfn "%A" (fst a, snd a, (a |> fst |> f0))
let epsilon = 0.000001

printfn "Functon: y = x - 10sin(x)" 
printfn "First value is x, second value is count of steps, third is delta"

printfn "Intervals: %A" (Computation.GetIntervals 0.11 f0 (-5.0, 3.0)) 

printfn "Bisection method" 
(List.map newGetRes (Computation.BisectionMethod f0 (-5.0, 3.0) epsilon)) |> ignore

printfn "Secant method" 
(List.map newGetRes (Computation.SecantMethod f0 (-5.0, 3.0) epsilon)) |> ignore

printfn "Newton method:"
(List.map newGetRes (Computation.NewtonMethod f0 derF0 (-5.0, 3.0) epsilon)) |> ignore

printfn "Advanced newton method:" 
(List.map newGetRes (Computation.AdvncedNewtonMethod f0 derF0 (-5.0, 3.0) epsilon)) |> ignore
