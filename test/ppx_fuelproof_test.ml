(* Test generated code *)

[@@@disable_unused_warnings]

type f = unit -> unit

(* ppx_fuelproof works by annotating each contained field as crossing
   the desired mode. If this succeeds, then the overall type is allowed
   to cross.
*)

module%test Portable = struct
  [@@@expand_inline
    type%fuelproof _ t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { mutable y : int
          ; z : int
          }
          -> unit t]

  type _ t =
    | A : int t
    | B : int -> bool t
    | C : { x : int } -> string t
    | D :
        { mutable y : int
        ; z : int
        }
        -> unit t
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

module%test Contended = struct
  [@@@expand_inline
    type%fuelproof _ t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : int
          ; z : int
          }
          -> unit t]

  type _ t =
    | A : int t
    | B : int -> bool t
    | C : { x : int } -> string t
    | D :
        { y : int
        ; z : int
        }
        -> unit t
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

module%test Unyielding = struct
  [@@@expand_inline
    type%fuelproof _ t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f
          ; z : int
          }
          -> unit t]

  type _ t =
    | A : int t
    | B : int -> bool t
    | C : { x : int } -> string t
    | D :
        { y : f
        ; z : int
        }
        -> unit t
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

module%test Many = struct
  [@@@expand_inline
    type%fuelproof _ t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f
          ; z : int
          }
          -> unit t]

  type _ t =
    | A : int t
    | B : int -> bool t
    | C : { x : int } -> string t
    | D :
        { y : f
        ; z : int
        }
        -> unit t
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

(* Modalities discharge the requirement that the field mode-cross. *)

module%test Modalities = struct
  [@@@expand_inline
    type%fuelproof _ t =
      | A : int t
      | B : f * int -> bool t
      | C :
          { x : f
          ; y : int
          }
          -> string t
      | D :
          { mutable y : int
          ; mutable y' : f
          ; z : int
          }
          -> unit t]

  type _ t =
    | A : int t
    | B : f * int -> bool t
    | C :
        { x : f
        ; y : int
        }
        -> string t
    | D :
        { mutable y : int
        ; mutable y' : f
        ; z : int
        }
        -> unit t
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

module%test Modalities2 = struct
  type mut = { mutable field : int }
  type mut_f = { mutable f_field : unit -> unit }

  [@@@expand_inline
    type%fuelproof _ t =
      | A : int t
      | B : f * int -> bool t
      | C :
          { x : f
          ; y : mut
          ; z : mut_f
          }
          -> string t]

  type _ t =
    | A : int t
    | B : f * int -> bool t
    | C :
        { x : f
        ; y : mut
        ; z : mut_f
        }
        -> string t
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

(* ppx_fuelproof can cause existential variables to have
   more restrictive mode crossing behavior inferred for them.
   See also [test_errors.mdx].
*)
module%test Existentials = struct
  [@@@expand_inline
    type%fuelproof t =
      | A : _ -> t
      | B : _ list -> t
      | C : _ iarray -> t]

  type t =
    | A : _ -> t
    | B : _ list -> t
    | C : _ iarray -> t
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]
end

module%test Kind_abbreviations = struct
  [@@@expand_inline
    type%fuelproof _ t =
      { x : int
      ; y : int t
      }]

  type _ t =
    { x : int
    ; y : int t
    }
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

module%test Kind_abbreviations2 = struct
  [@@@expand_inline
    type%fuelproof _ t =
      { mutable x : int
      ; mutable y : int t
      }]

  type _ t =
    { mutable x : int
    ; mutable y : int t
    }
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

module%test Recursive_unboxed = struct
  [@@@expand_inline
    type%fuelproof t =
      | Nil of int
      | Rec of u

    and u = { more : v } [@@unboxed]
    and v = { x : t }]

  type t =
    | Nil of int
    | Rec of u
  [@@unsafe_allow_any_mode_crossing]

  and u = { more : v } [@@unboxed] [@@unsafe_allow_any_mode_crossing]
  and v = { x : t } [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross_portability : t -> t = fun x -> x
  let cross_contention : t -> t = fun x -> x
end

(* When recursive uses of a type under definition appear under immutable_data type
   constructors, the constraint is pushed down to the recursive use. This
   appears to help the type-checker infer useful kinds in more places.
*)
module%test Recursive_with_immutable_data = struct
  module Iarray = struct
    type 'a t = 'a iarray
  end

  [@@@expand_inline
    type%fuelproof t =
      | Both of (t * t) list Iarray.t
      | Other of (int * t) iarray option]

  type t =
    | Both of (t * t) list Iarray.t
    | Other of (int * t) iarray option
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]

  let cross_portability : t -> t = fun x -> x
  let cross_contention : t -> t = fun x -> x
end
