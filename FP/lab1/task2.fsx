open System

let eps = 1e-8
let maxIterations = 10_000

let dichotomy f a b eps maxIter =
    let fa = f a
    let fb = f b

    if fa * fb > 0.0 then
        invalidArg "a,b" "Dichotomy requires f(a) and f(b) with different signs."

    let rec loop left right fLeft iter =
        let mid = (left + right) / 2.0
        let fMid = f mid

        if iter >= maxIter || abs fMid < eps || abs (right - left) / 2.0 < eps then
            mid, iter + 1
        elif fLeft * fMid <= 0.0 then
            loop left mid fLeft (iter + 1)
        else
            loop mid right fMid (iter + 1)

    loop a b fa 0

let iterations phi x0 eps maxIter =
    let rec loop x iter =
        let next = phi x

        if iter >= maxIter || abs (next - x) < eps then
            next, iter + 1
        else
            loop next (iter + 1)

    loop x0 0

let newton f f' x0 eps maxIter =
    let phi x = x - f x / f' x
    iterations phi x0 eps maxIter

let f x = sqrt (1.0 - x) - tan x

let f' x =
    -1.0 / (2.0 * sqrt (1.0 - x))
    - 1.0 / ((cos x) * (cos x))

let phi x = atan (sqrt (1.0 - x))

let a = 0.0
let b = 1.0
let x0 = 0.5

let dRoot, dIters = dichotomy f a b eps maxIterations
let iRoot, iIters = iterations phi x0 eps maxIterations
let nRoot, nIters = newton f f' x0 eps maxIterations

printfn "Equation: sqrt(1 - x) - tg(x) = 0"
printfn "Interval: [%.1f, %.1f], table value: 0.5768" a b
printfn ""
printfn "%-12s | %-12s | %-10s" "Method" "Root" "# iter"
printfn "---------------------------------------------"
printfn "%-12s | %12.8f | %10d" "Dichotomy" dRoot dIters
printfn "%-12s | %12.8f | %10d" "Iterations" iRoot iIters
printfn "%-12s | %12.8f | %10d" "Newton" nRoot nIters
