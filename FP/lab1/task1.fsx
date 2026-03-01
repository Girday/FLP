open System

let a = 0.0
let b = 0.5
let segments = 10
let eps = 1e-8

let builtin x =
    2.0 * (cos x * cos x - 1.0)

let rec factorial n =
    if n <= 1 then 1.0
    else float n * factorial (n - 1)

let taylorTermNaive x n =
    let sign = if n % 2 = 0 then 1.0 else -1.0
    sign * pown (2.0 * x) (2 * n) / factorial (2 * n)

let taylorNaive x eps =
    let rec loop n sum terms =
        let term = taylorTermNaive x n
        if abs term < eps then
            sum, terms
        else
            loop (n + 1) (sum + term) (terms + 1)

    loop 1 0.0 0

let taylorSmart x eps =
    let firstTerm = -((2.0 * x) * (2.0 * x)) / 2.0

    let rec loop n term sum terms =
        if abs term < eps then
            sum, terms
        else
            let nextTerm =
                term
                * (-(4.0 * x * x))
                / (float (2 * n + 1) * float (2 * n + 2))

            loop (n + 1) nextTerm (sum + term) (terms + 1)

    loop 1 firstTerm 0.0 0

let printTable =
    printfn "%6s | %14s | %14s | %7s | %14s | %7s" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor" "# terms"
    printfn "-----------------------------------------------------------------------------------------------"

    for i in 0 .. segments do
        let x = a + (float i) * (b - a) / float segments
        let fx = builtin x
        let smart, smartTerms = taylorSmart x eps
        let dumb, dumbTerms = taylorNaive x eps

        printfn "%6.3f | %14.10f | %14.10f | %7d | %14.10f | %7d"
            x fx smart smartTerms dumb dumbTerms

printTable
