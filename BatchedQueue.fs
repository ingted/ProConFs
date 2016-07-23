namespace Utility

type BatchedQueue<'T> =
  internal
  | BatchedQueue of forwardList: list<'T> * reversedList: list<'T>

[<RequireQualifiedAccess>]
module BatchedQueue =
  let internal unwrap (BatchedQueue(l, r)) =
    (l, r)

  // 前方リストが空なら、後方リストを反転して前方リストにする
  let internal rebuild self =
    self
    |> unwrap
    |> function
      | ([], r) -> BatchedQueue (r |> List.rev, [])
      | _ -> self

  let empty =
    BatchedQueue ([], [])

  let add x self =
    self
    |> unwrap
    |> function (l, r) -> BatchedQueue (l, x :: r)
    |> rebuild

  let tryUncons self =
    self
    |> unwrap
    |> function
      | ([], _) -> None
      | (x :: l, r) -> Some (x, BatchedQueue (l, r) |> rebuild)

  let tryHead self =
    self |> tryUncons |> Option.map fst

  let tryTail self =
    self |> tryUncons |> Option.map snd

  let toList self =
    self
    |> unwrap
    |> function (f, r) -> List.append f (List.rev r)

  let ofList self =
    BatchedQueue (self, [])

  let toSeq self = self |> toList |> List.toSeq
  let ofSeq self = self |> Seq.toList |> ofList

  // Note: キューが空でないときは常に、前方リストが空でない。
  let isEmpty (l, _) =
    List.isEmpty l

  let length self =
    self
    |> unwrap
    |> function (l, r) -> List.length l + List.length r

  let map f self =
    self
    |> unwrap
    |> function (l, r) -> BatchedQueue (List.map f l, List.map f r)

  let fold f s self =
    self |> toList |> List.fold f s

  let append l r =
    r |> fold (fun self x -> self |> add x) l
