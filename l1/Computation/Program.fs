open System


type Computation () =
    
    static member Split interval delta =
        [fst interval .. delta .. snd interval] |> List.fold (fun acc x -> (x, x + delta) :: acc) [] |> List.tail

    /// Bisection method.
    static member BisectionMethod f (a : float)  (b : float) (accuracy : float) =
        let rec sub la lb step =
            if f la = 0.0 then la
            elif f lb = 0.0 then lb
            elif lb - la < accuracy then (lb + la) / 2.0
            elif (f la) * (f ((la + lb) / 2.0)) < 0.0 then sub la ((la + lb) / 2.0) (step + 1)
            else sub ((la + lb) / 2.0) lb (step + 1)
        sub a b 0

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



let i = Computation.Split (1.0, 2.0) 0.2
let a = sin >> (-)0.5
let b = tan >> (-)2.0
