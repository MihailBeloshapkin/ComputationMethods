open System


type Computation () =
    /// Bisection method.
    static member BisectionMethod f (a : float)  (b : float) (accuracy : float) =
        let rec sub la lb =
            if f la = 0.0 then [la]
            elif f lb = 0.0 then [lb]
            elif (f la) * (f lb) < 0.0 then
                match lb - la < accuracy with
                | true -> [(la + lb) / 2.0] 
                | _ -> (sub la ((la + lb) / 2.0)) @ (sub ((la + lb) / 2.0) lb)
            else []
        sub a b     

    /// Newton method.
    static member NewtinMethod f derF (interval : float * float) (accuracy : float) = 
        let rec sub (prev : float) next = 
            if Math.Abs(next - prev) < accuracy then next else sub next (next - (f next) / (derF next))
        sub 0.0 (interval |> fst) 

    /// Secant method.
    static member SecantMethod f interval accuracy =
        let rec sub (prev : float) (next : float) =
            if Math.Abs(next - prev) < accuracy then next else sub next (next - (f next) * (next - prev) / (f next - f prev)) 
        sub 0.0 (fst interval - snd interval) / 2.0



let a = sin >> (-)0.5
let b = tan >> (-)2.0
