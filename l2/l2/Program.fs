open System
open System.Collections
open System.Collections.Generic

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


let getNewDeltas (li : list<float>) (xValues : list<float>) iter = 
    [for i in 1 .. li.Length - 1 do (li.[i] - li.[i - 1]) / (xValues.[i + iter] - xValues.[i - 1])]

let getLists (table : list<list<float>>) =
    let xValues = table |> List.rev |> List.head 
    let rec sub (l : list<list<float>>) iter =
        match l with
        | [] -> failwith "List should be not empty"
        | head::tail -> if head.Length = 1 then l else sub ((getNewDeltas head xValues iter) :: head :: tail) (iter + 1)
    sub table 0

// Newton interpolation method realization.
let Newton x n (table : list<float * float>) =
    // Sort table by x - x_n
    let sortedTable = table |> List.sortBy (fun element -> Math.Abs((fst element) - x)) 
    printfn "%A" sortedTable
    let generalTable = [for i in 0 .. n - 1 do sortedTable.[i]]
    printfn "%A" generalTable
    let listOfDeltas = [generalTable |> List.map snd; generalTable |> List.map fst] |> getLists
    printfn "%A" listOfDeltas





let getXMultiplication (xValues : list<int>) x = 
    let rec sub values acc =
        match values with
        | [] -> acc
        | head :: tail -> sub tail (acc * (x - head))
    sub xValues x

// let ex = [1; 2; 4; 9]
// let ex1 = [for i in 1 .. ex.Length - 1 do ex.[i] - ex.[i - 1]]
Newton -1.2 4 [(-1.0, 9.0); (1.0, 3.0); (2.0, 3.0); (3.0, 5.0)]
// printfn "%A" (getLists ex)
printfn "Lab number 2" |> ignore
printfn "%A" ((Lagrange 1.0 [(-1.0, 4.0); (0.0, 1.0); (2.0, 1.0)]) - 0.0) 