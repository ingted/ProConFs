module Misc

open System
open System.Collections.Generic

module Option =
  /// None を単位元とみなして演算する。
  let operate f l r =
      match (l, r) with
      | (None, x)
      | (x, None) -> x
      | (Some l, Some r) -> f l r

[<AutoOpen>]
module Ord =
  [<StructuralEquality>]
  [<CustomComparison>]
  /// Value with reversed order
  type Down<'a when 'a: comparison> = | Down of 'a
    with
      member this.Value =
          match this with
          | Down x -> x
      interface IComparable with
        member this.CompareTo (rhs) =
            match rhs with
            | :? Down<'a> as rhs -> (this :> IComparable<Down<'a>>).CompareTo(rhs)
            | _ -> invalidArg "rhs" "Type mismatch"
      interface IComparable<Down<'a>> with
        member this.CompareTo (rhs) = - compare (this.Value) (rhs.Value)
