open System

let Split delta interval =
    [fst interval .. delta .. snd interval] |> List.fold (fun acc x -> (x, x + delta) :: acc) [] |> List.tail

let GetIntervals delta f (interval : double * double) =
    interval |> Split 0.03 |> List.filter (fun x -> (x |> fst |> f) * (x |> snd |> f) <= 0.0) 
        

    /// Bisection method.
let BisectionMethod f interval (accuracy : float) =
    let rec sub i la lb =
        if f la = 0.0 then (la, i)
        elif f lb = 0.0 then (lb, i)
        elif lb - la < accuracy then ((lb + la) / 2.0, i)
        elif (f la) * (f ((la + lb) / 2.0)) < 0.0 then sub (i + 1) la ((la + lb) / 2.0)
        else sub (i + 1) ((la + lb) / 2.0) lb 
    interval |> GetIntervals 0.03 f |> List.map (fun x -> sub 0 (fst x) (snd x))  
             |> List.fold (fun acc x -> if acc |> List.exists (fun a -> Math.Abs((fst a) - (fst x)) < 0.0001) then acc else x :: acc) []


let multiplication x (data : list<float>) = data |> List.map (fun el -> x - el) |> List.fold (fun acc x -> acc * x) 1.0

let getTerm x k (data : list<float * float>) = 
    let (xk, fxk) = (fst data.[k], snd data.[k]) 
    let numerator = data |> List.map fst |> List.filter (fun a -> a <> xk) |> multiplication x
    let denumerator = data |> List.map fst |> List.filter (fun a -> a <> xk) |> multiplication xk
    fxk * numerator / denumerator

let Lagrange x (data : list<float * float>) =
    let terms = [0 .. List.length data - 1] |> List.map (fun i  -> getTerm x i data)
    terms |> List.fold (fun acc x -> acc + x) 0.0

printfn "%A" ((Lagrange 1.0 [(-1.0, 4.0); (0.0, 1.0); (2.0, 1.0)]) - 0.0) 