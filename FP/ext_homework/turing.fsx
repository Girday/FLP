open System

type alpha = char
type state = int

type command =
    | Left
    | Right
    | Noop
    | Halt

type rule = state * alpha * alpha * command * state
type program = Map<state * alpha, alpha * command * state>
type ribbon = alpha list * alpha * alpha list

let blank = ' '

let compile (rules: rule list) : program =
    rules
    |> List.fold
        (fun p (q, a, b, move, q') ->
            let key = q, a

            if Map.containsKey key p then
                failwithf "Duplicate rule for q%d '%c'" q a

            Map.add key (b, move, q') p)
        Map.empty

let mkRibbon (s: string) : ribbon =
    match Seq.toList s with
    | [] -> [], blank, []
    | h :: t -> [], h, t

let write b ((l, _, r): ribbon) : ribbon = l, b, r

let move cmd ((l, h, r): ribbon) : ribbon =
    match cmd with
    | Left ->
        match l with
        | x :: xs -> xs, x, h :: r
        | [] -> [], blank, h :: r
    | Right ->
        match r with
        | x :: xs -> h :: l, x, xs
        | [] -> h :: l, blank, []
    | Noop
    | Halt -> l, h, r

let cells ((l, h, r): ribbon) : alpha list = List.rev l @ [ h ] @ r

let text (rib: ribbon) =
    let rec trim =
        function
        | x :: xs when x = blank -> trim xs
        | xs -> xs

    rib |> cells |> trim |> List.rev |> trim |> List.rev |> List.toArray |> String

let run limit (prog: program) (start: state) (rib0: ribbon) : state * ribbon =
    let rec loop n ((q, rib): state * ribbon) =
        if n >= limit then
            failwith "Step limit exceeded"

        let (_, a, _) = rib

        match Map.tryFind (q, a) prog with
        | None -> failwithf "No rule for q%d '%c'" q a
        | Some(b, Halt, q') -> q', write b rib
        | Some(b, cmd, q') -> loop (n + 1) (q', move cmd (write b rib))

    loop 0 (start, rib0)

let add1 =
    [ 0, blank, blank, Right, 1
      1, '0', '0', Right, 1
      1, '1', '1', Right, 1
      1, blank, blank, Left, 2
      2, '0', '1', Halt, 2
      2, blank, '1', Halt, 2
      2, '1', '0', Left, 2 ]
    |> compile

let normalize name (s: string) =
    let s = s.Trim()

    if s = "" then
        failwithf "%s is empty" name

    if s |> Seq.exists (fun c -> c <> '0' && c <> '1') then
        failwithf "%s must be binary" name

    let s = s.TrimStart('0')
    if s = "" then "0" else s

let pair a b =
    match a, b with
    | '0', '0' -> 'A'
    | '0', '1' -> 'B'
    | '1', '0' -> 'C'
    | '1', '1' -> 'D'
    | _ -> failwith "Impossible bit pair"

let columns x y =
    let xs = normalize "Left" x |> Seq.rev |> Seq.toList
    let ys = normalize "Right" y |> Seq.rev |> Seq.toList
    let n = max (List.length xs) (List.length ys)

    let pad bits =
        bits @ List.replicate (n - List.length bits) '0'

    List.zip (pad xs) (pad ys) |> List.map (fun (a, b) -> pair a b)

let addBinaryProgram =
    [ 0, 'A', '0', Right, 0
      0, 'B', '1', Right, 0
      0, 'C', '1', Right, 0
      0, 'D', '0', Right, 1
      1, 'A', '1', Right, 0
      1, 'B', '0', Right, 1
      1, 'C', '0', Right, 1
      1, 'D', '1', Right, 1
      0, blank, blank, Halt, 0
      1, blank, '1', Halt, 0 ]
    |> compile

let binaryOf rib =
    let bits = cells rib |> List.takeWhile (fun c -> c = '0' || c = '1')

    match bits with
    | [] -> "0"
    | _ ->
        let s = bits |> List.rev |> List.toArray |> String
        let s = s.TrimStart('0')
        if s = "" then "0" else s

let addBinary x y =
    columns x y
    |> List.toArray
    |> String
    |> mkRibbon
    |> run 10000 addBinaryProgram 0
    |> snd
    |> binaryOf

let incResult = run 100 add1 1 (mkRibbon "0111") |> snd |> text
let sumResult = addBinary "1101" "1011"

printfn "0111 -> %s" incResult
printfn "1101 + 1011 = %s" sumResult
