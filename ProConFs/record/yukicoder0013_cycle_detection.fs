//AC <http://yukicoder.me/submissions/55153>

open System
open System.Text
open System.Collections.Generic

module Str =
  let split (delim: string) (s: string) =
    s.Split ([| delim |], StringSplitOptions.RemoveEmptyEntries)

[<EntryPoint>]
let main argv =
  let [| w; h |] =
      Console.ReadLine () |> Str.split " " |> Array.map (Int32.Parse)
  let board =
    [| for i in 0..(h - 1) do
        let s = Console.ReadLine ()
        yield s |> Str.split " " |> Array.map (Int32.Parse)
        |]

  let containsCycle () =
    let neighbors4 = [(1, 0); (0, 1); (-1, 0); (0, -1)]
    let visited = Array2D.init h w (fun _ _ -> false)

    let rec dfs backstep (i0, j0) =
        if visited.[i0, j0]
        then true
        else
          visited.[i0, j0] <- true
          Seq.exists id (seq {
            for (di, dj) in neighbors4 do
              let (i, j) = (i0 + di, j0 + dj)
              if  0 <= i && i < h && 0 <= j && j < w
                && (i, j) <> backstep 
                && board.[i].[j] = board.[fst backstep].[snd backstep]
              then
                yield dfs (i0, j0) (i, j)
            })

    Seq.exists id (seq {
      for i in 0..(h - 1) do
        for j in 0..(w - 1) do
          if not visited.[i, j] then
            yield dfs (i, j) (i, j)
      })

  printfn "%s"
    <| if containsCycle () then "possible" else "impossible"

  //exit code
  0
