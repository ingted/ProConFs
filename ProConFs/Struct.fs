module Struct

open Misc

// (cf. <http://qiita.com/nida_001/items/bc244187f3c1b97b8984>)
module LeftistTree =
  type private Heap<'a when 'a: comparison> =
      Node<'a> option

  and Node<'a when 'a: comparison> =
    private
      { Rank: int
      ; Elem: 'a
      ; Left:  Heap<'a>
      ; Right: Heap<'a> }

  let empty = None
  let isEmpty = Option.isNone

  let singleton x =
      Some { Rank = 1; Elem = x; Left = empty; Right = empty }

  let rank = function
      | None -> 0
      | Some {Rank = n} -> n

  /// 最小値
  /// O(1)
  let head h =
      h |> Option.map (function | {Elem = x} -> x)

  /// マージの補助関数
  /// 要素と子要素候補2つから、頂点を作成。ランクの大きいほうを左側におく。
  /// 返される頂点のランクは、右のランク+1。
  let private makeT x l r =
      if rank l >= rank r
      then Some { Rank = rank r + 1; Elem = x; Left = l; Right = r }
      else Some { Rank = rank l + 1; Elem = x; Left = r; Right = l }

  /// マージ
  let rec merge h1 h2 =
      (h1, h2) ||> Option.operate (fun l r ->
        if l.Elem <= r.Elem
        then makeT (l.Elem) (l.Left) (merge (l.Right) h2)
        else makeT (r.Elem) (r.Left) (merge h1 (r.Right))
        )

  /// 挿入
  /// O(log n)
  let insert x =
      merge (singleton x)

  /// 最小値の除去
  /// O(log n)
  let tail h =
      Option.bind (fun t -> merge (t.Left) (t.Right)) h

  /// fmap
  let rec map f =
      let m = function
          |    { Rank = n; Elem =   x; Left =       l; Right =       r }
            -> { Rank = n; Elem = f x; Left = map f l; Right = map f r }
      Option.map m

  /// fold
  let rec fold f s h =
      match head h with
      | None -> s
      | Some x -> fold f (f s x) (tail h)

  let ofSeq s =
      s |> List.ofSeq
        |> List.sort
        |> (fun s -> List.foldBack insert s empty)

  let toList h =
      h |> fold (fun s x -> x :: s) [] |> List.rev
