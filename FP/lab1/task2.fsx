open System

let eps = 1e-8
let maxIterations = 10_000

let dichotomy f a b eps maxIter =
    let fa = f a

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

// 5: sqrt(1 - x) - tg(x) = 0, [0, 1]
let f1 x = sqrt (1.0 - x) - tan x
let f1' x = -1.0 / (2.0 * sqrt (1.0 - x)) - 1.0 / ((cos x) * (cos x))
let phi1 x = atan (sqrt (1.0 - x))
let a1, b1, x01, table1 = 0.0, 1.0, 0.5, 0.5768

// 6: x + cos(x^0.52 + 2) = 0, [0.5, 1]
let f2 x = x + cos (x ** 0.52 + 2.0)
let f2' x = 1.0 - sin (x ** 0.52 + 2.0) * 0.52 * (x ** -0.48)
let phi2 x = -cos (x ** 0.52 + 2.0)
let a2, b2, x02, table2 = 0.5, 1.0, 0.75, 0.9892

// 7: 3*ln^2(x) + 6*ln(x) - 5 = 0, [1, 3]
let f3 x = 3.0 * (log x) ** 2.0 + 6.0 * log x - 5.0
let f3' x = (6.0 * log x + 6.0) / x
let phi3 x = exp ((5.0 - 3.0 * (log x) ** 2.0) / 6.0)
let a3, b3, x03, table3 = 1.0, 3.0, 2.0, 1.8832

let solveOne f f' phi a b x0 =
    let dRoot, dIters = dichotomy f a b eps maxIterations
    let iRoot, iIters = iterations phi x0 eps maxIterations
    let nRoot, nIters = newton f f' x0 eps maxIterations
    (dRoot, dIters, iRoot, iIters, nRoot, nIters)

let s1 = solveOne f1 f1' phi1 a1 b1 x01
let s2 = solveOne f2 f2' phi2 a2 b2 x02
let s3 = solveOne f3 f3' phi3 a3 b3 x03

printfn ""
printfn "%-3s | %-12s | %-12s | %-12s | %-8s" "#" "Dichotomy" "Iterations" "Newton" "Table"
printfn "------------------------------------------------------------------------"

let d1, _, i1, _, n1, _ = s1
let d2, _, i2, _, n2, _ = s2
let d3, _, i3, _, n3, _ = s3

printfn "%-3d | %12.8f | %12.8f | %12.8f | %8.4f" 5 d1 i1 n1 table1
printfn "%-3d | %12.8f | %12.8f | %12.8f | %8.4f" 6 d2 i2 n2 table2
printfn "%-3d | %12.8f | %12.8f | %12.8f | %8.4f" 7 d3 i3 n3 table3


printfn ""
printfn "%-3s | %-12s | %-12s | %-12s" "#" "Dichotomy" "Iterations" "Newton"
printfn "---------------------------------------------------------------"

let _, dIt1, _, iIt1, _, nIt1 = s1
let _, dIt2, _, iIt2, _, nIt2 = s2
let _, dIt3, _, iIt3, _, nIt3 = s3

printfn "%-3d | %12d | %12d | %12d" 5 dIt1 iIt1 nIt1
printfn "%-3d | %12d | %12d | %12d" 6 dIt2 iIt2 nIt2
printfn "%-3d | %12d | %12d | %12d" 7 dIt3 iIt3 nIt3
