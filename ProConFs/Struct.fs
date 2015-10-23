module Struct

open Misc

/// Union Find Forest
module UFF =
  type Node<'a> =
    { Value: 'a
    /// None iff this node is root
    ; Parent: Node<'a> option ref
    ; Rank: int ref
    }
    with
      static member value {Value = v} = v

  type private UnionFindForest<'a when 'a: comparison> =
      Map<'a, Node<'a>>

  let ofSeq s =
      let gen x =
          (x, {Value = x; Parent = ref None; Rank = ref 1})
      new UnionFindForest<'a> (s |> Seq.map gen)
    
  let init n f = Seq.init n f |> ofSeq

  let rec private findRootNode x uff =
      let n = uff |> Map.find x
      match (! n.Parent) with
      | None -> n
      | Some p ->
          let root = uff |> findRootNode (p.Value)
          n.Parent := Some root  // compress
          root

  let root x uff =
      findRootNode x uff |> Node<_>.value

  let connects x y uff =
      let x = root x uff
      let y = root y uff
      (x = y)

  let merge x y uff =
      let n1 = findRootNode x uff
      let n2 = findRootNode y uff
      if n1 <> n2 then
          if (! n1.Rank) < (! n2.Rank) then
              n1.Parent := Some n2
          else
              n2.Parent := Some n1
              if (! n1.Rank) = (! n2.Rank) then
                  n1.Rank := (! n1.Rank) + 1

  let groups uff =
      uff |> Map.toSeq |> Seq.map snd
          |> Seq.groupBy (fun n -> uff |> root (n.Value))
          |> Seq.map (fun (_, g) -> g |> Seq.map Node<_>.value)

  /// Sequence of representative elements
  let reprs uff =
      uff |> Map.filter (fun _ n -> (! n.Parent) = None)
          |> Map.toSeq |> Seq.map (snd >> Node<_>.value)

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
