open System
open System.Collections
open System.Collections.Generic

let multiplication x (data : list<float>) = data |> List.map (fun el -> x - el) |> List.fold (fun acc x -> acc * x) 1.0

let getTerm x k (data : list<float * float>) = 
    let (xk, fxk) = (fst data.[k], snd data.[k]) 
    let numerator = data |> List.map fst |> List.filter (fun a -> a <> xk) |> multiplication x
    let denumerator = data |> List.map fst |> List.filter (fun a -> a <> xk) |> multiplication xk
    fxk * numerator / denumerator

let Lagrange x n (data : list<float * float>) =
    let sortedTable = data |> List.sortBy (fun element -> Math.Abs((fst element) - x)) 
    let generalTable = [for i in 0 .. n - 1 do sortedTable.[i]]
    let terms = [0 .. List.length generalTable - 1] |> List.map (fun i  -> getTerm x i generalTable)
    terms |> List.fold (fun acc x -> acc + x) 0.0


let getNewDeltas (li : list<float>) (xValues : list<float>) iter = 
    [for i in 1 .. li.Length - 1 do (li.[i] - li.[i - 1]) / (xValues.[i + iter] - xValues.[i - 1])]

(*
let rec getResult res (coefs : list<float>) values n (i : int) =
    match (i : int) with
    | n -> res
    | _ -> getResult (res + coefs.[i] * (multiplication x [for j in 0 .. i do values.[j]])) coefs values n i
    
*)

// Newton interpolation method realization.
let Newton x n (table : list<float * float>) =
    let xValues = table |> List.map fst
        
    // Here we get table with deltas.
    let getLists (tableOfDeltas : list<list<float>>) =
        let rec sub (l : list<list<float>>) iter =
            match l with
            | [] -> failwith "List should be not empty"
            | head::tail -> if head.Length = 1 then l else sub ((getNewDeltas head xValues iter) :: head :: tail) (iter + 1)
        sub tableOfDeltas 0

    // Sort table by x - x_n
    let sortedTable = table |> List.sortBy (fun element -> Math.Abs((fst element) - x)) 
    printfn "%A" sortedTable
    let generalTable = [for i in 0 .. n - 1 do sortedTable.[i]]
    printfn "%A" generalTable
    let listOfDeltas = [generalTable |> List.map snd; generalTable |> List.map fst] |> getLists |> List.rev |> List.tail
    let coefficents = listOfDeltas |> List.map (fun el -> if el.IsEmpty |> not then el.[0] else failwith "empty list in the delta table")
    printfn "%A" listOfDeltas
    
 //   getResult 0 coefficents xValues 0




let getXMultiplication (xValues : list<int>) x = 
    let rec sub values acc =
        match values with
        | [] -> acc
        | head :: tail -> sub tail (acc * (x - head))
    sub xValues x

let f (x : float) = sin(x) + x * x / 2.0
let a = 0.0
let b = 1.0
let m = 15
let xValues = [for i in 0 .. m do a + (float)i * (b - a) / (float)m]
let yValues = xValues |> List.map (fun x -> f x)
let table = List.zip xValues yValues
let x = 0.55
let lagrangeRes : float = Lagrange x 5 table
let fRes : float = f x
let delta (a : float) b = Math.Abs(a - b)
printfn "Lagrange: %A f: %A delta: %A" lagrangeRes fRes (delta lagrangeRes fRes)