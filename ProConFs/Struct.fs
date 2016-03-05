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

[<RequireQualifiedAccess>]
module BinomialHeap =
  type Node<'T> =
    internal
      {
        Rank      : int
        Value     : 'T
        Children  : Node<'T> list
      }

  [<RequireQualifiedAccess>]
  module internal Node =
    let rank (n: Node<_>) =
      n.Rank

    let value (n: Node<_>) =
      n.Value

    let children (n: Node<_>) =
      n.Children

    let length n =
      1 <<< (rank n)

    let singleton x =
      {
        Rank      = 0
        Value     = x
        Children  = []
      }

    let rec map f n =
      { n with
          Value     = f (value n)
          Children  = n |> children |> List.map (map f)
      }

    /// Link two nodes with the same rank
    let rec link l r =
      assert (rank l = rank r)
      if value l <= value r then
        {
          Rank      = rank l + 1
          Value     = value l
          Children  = r :: children l
        }
      else
        link r l

  type Heap<'T> =
    internal
    | Heap of Node<'T> list

  let empty =
    Heap []

  let isEmpty (Heap ns) =
    ns |> List.isEmpty

  let length (Heap ns) =
    ns |> List.sumBy Node.length

  let rec insertTree n (Heap ns) =
    match ns with
    | [] -> Heap [n]
    | n' :: ns' ->
        if Node.rank n < Node.rank n' then
          Heap (n :: ns)
        else
          assert (Node.rank n = Node.rank n')
          insertTree (Node.link n n') (Heap ns')

  let insert x =
    insertTree (Node.singleton x)

  let rec merge (Heap l) (Heap r) =
    match (l, r) with
    | (l, []) -> Heap l
    | ([], r) -> Heap r
    | (lh :: lt, rh :: rt) ->
        if Node.rank lh < Node.rank rh then
          let (Heap m) = merge (Heap lt) (Heap r)
          Heap (lh :: m)
        elif Node.rank lh > Node.rank rh then
          let (Heap m) = merge (Heap l) (Heap rt)
          Heap (rh :: m)
        else
          insertTree (Node.link lh rh) (merge (Heap lt) (Heap rt))

  let rec removeMinTree (Heap ns) =
    match ns with
    | [] -> None
    | [n] -> Some (n, Heap [])
    | n :: ns ->
        removeMinTree (Heap ns)
        |> Option.map (fun (n', Heap ns') ->
            if Node.value n <= Node.value n' 
            then (n, Heap ns)
            else (n', Heap ns')
            )

  let head h =
    removeMinTree h
    |> Option.map (fst >> Node.value)

  let tail h =
    match removeMinTree h with
    | None -> h
    | Some (n, h') ->
        merge (n |> Node.children |> List.rev |> Heap) h'

  let rec toList h =
    match head h with
    | None -> []
    | Some x ->
        x :: toList (tail h)

  let rec ofList =
    function
    | [] -> empty
    | x :: t ->
        ofList t |> insert x

  let map f (Heap ns) =
    ns |> List.map (Node.map f) |> Heap
