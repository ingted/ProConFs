open System

let n = Console.ReadLine() |> int
printfn "%d" (316 + (n - 1) * 52)

(*
open System

let [|n;m|] = Console.ReadLine().Split(' ') |> Array.map int;;
let gr =
  let es =
    [ for i in 1..m do
        let [|a; b; c|] = Console.ReadLine().Split(' ') |> Array.map int
        yield (a, (b, c))
        ]
  es |> List.sort |> Seq.groupBy fst |> Map.ofSeq;;

printfn "%A" gr;;

//*)