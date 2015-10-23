// AC <http://yukicoder.me/submissions/55493>

open System
open System.Collections.Generic

let combi =
  let M = 100
  let m = Array2D.zeroCreate M M: int64 [,]
  let rec loop n r =
    if r = 0 || r = n then
      1L
    else if 0 < r && r < n then
      if m.[n, r] = 0L then
        let v = loop (n - 1) (r - 1) + loop (n - 1) r
        m.[n, r] <- v
        v
      else
        m.[n, r]
    else
      0L
  loop

let enumCombi n k =
  let u = (1L <<< n) - 1L
  let s = (1L <<< k) - 1L
  let gen s =
    if s &&& ~~~u = 0L
    then
      let t = (s ||| (s - 1L)) + 1L
      Some (s, t ||| ((((t &&& -t) / (s &&& -s)) >>> 1) - 1L))
    else None
  Seq.unfold gen s

let bitsFromInt len n =
  List.init len (fun i -> (n >>> (len - i - 1)) &&& 1L <> 0L)

let superFizzBuzz i =
  // n 桁以上の SFB 数の i 番目
  let rec loop i n =
    let k_max = n / 3
    let count =
      [ for k in 1..k_max do
          let countFives = k * 3
          let countThrees = n - countFives
          yield combi (n - 1) countThrees ]
      |> List.sum
    if i >= count then
        loop (i - count) (n + 1)
    else
        // 全列挙
        // リストでは間に合わない
        seq {
          for k in 1..k_max do
            let countFives = k * 3
            yield! enumCombi (n - 1) (countFives - 1)  // 1の位の 5 は除外
          }
        |> Seq.sort
        |> Seq.nth (int i)
        |> (fun bs ->
              bitsFromInt n ((bs <<< 1) ||| 1L)  // 1の位は 5
              |> List.map (function | true -> 5 | false -> 3)
              |> (fun ls -> String.Join("", ls))
              )
  loop i 0

[<EntryPoint>]
let main argv =
  let i = Console.ReadLine() |> int
  printfn "%s" (superFizzBuzz (i - 1 |> int64))

  //exit code
  0
