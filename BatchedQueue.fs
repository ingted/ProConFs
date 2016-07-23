namespace Utility

type BatchedQueue<'T> =
  internal
  | BatchedQueue of forwardList: list<'T> * reversedList: list<'T>

[<RequireQualifiedAccess>]
module BatchedQueue =
  let internal unwrap (BatchedQueue(l, r)) =
    (l, r)

  // �O�����X�g����Ȃ�A������X�g�𔽓]���đO�����X�g�ɂ���
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

  // Note: �L���[����łȂ��Ƃ��͏�ɁA�O�����X�g����łȂ��B
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
