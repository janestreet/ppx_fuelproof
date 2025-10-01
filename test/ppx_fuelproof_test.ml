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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__001_ = struct
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
      end
    end

    type 'a__002_ t = 'a__002_ Check__001_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { mutable y : int
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__005_ = struct
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
      end
    end

    type 'a__006_ t = 'a__006_ Check__005_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : int
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__009_ = struct
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
      end
    end

    type 'a__010_ t = 'a__010_ Check__009_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__013_ = struct
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
      end
    end

    type 'a__014_ t = 'a__014_ Check__013_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__017_ = struct
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
      end
    end

    type 'a__018_ t = 'a__018_ Check__017_.t =
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
  end

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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__021_ = struct
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
      end
    end

    type 'a__022_ t = 'a__022_ Check__021_.t =
      | A : int t
      | B : f * int -> bool t
      | C :
          { x : f
          ; y : mut
          ; z : mut_f
          }
          -> string t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

(* See also [test_errors.mdx]. *)
module%test Existentials = struct
  [@@@expand_inline
    type%fuelproof t =
      | A : 'a. 'a -> t
      | B : 'a. 'a list -> t
      | C : 'a. 'a iarray -> t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__025_ = struct
        type t =
          | A : 'a. 'a -> t
          | B : 'a. 'a Ppx_fuelproof_runtime.list -> t
          | C : 'a. 'a Ppx_fuelproof_runtime.Iarray.t -> t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t = Check__025_.t =
      | A : 'a. 'a -> t
      | B : 'a. 'a list -> t
      | C : 'a. 'a iarray -> t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

module%test Kind_abbreviations = struct
  [@@@expand_inline
    type%fuelproof _ t =
      { x : int
      ; y : int t
      }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__027_ = struct
        type _ t =
          { x : int
          ; y : int t
          }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__028_ t = 'a__028_ Check__027_.t =
      { x : int
      ; y : int t
      }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t -> _ t = fun x -> x
end

module%test Kind_abbreviations2 = struct
  [@@@expand_inline
    type%fuelproof _ t =
      { mutable x : int
      ; mutable y : int t
      }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__031_ = struct
        type _ t =
          { mutable x : int
          ; mutable y : int t
          }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__032_ t = 'a__032_ Check__031_.t =
      { mutable x : int
      ; mutable y : int t
      }
    [@@unsafe_allow_any_mode_crossing]
  end

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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__035_ = struct
        type t =
          | Nil of int
          | Rec of u
        [@@unsafe_allow_any_mode_crossing]

        and u = { more : v } [@@unboxed] [@@unsafe_allow_any_mode_crossing]
        and v = { x : t } [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t = Check__035_.t =
      | Nil of int
      | Rec of u
    [@@unsafe_allow_any_mode_crossing]

    and u = Check__035_.u = { more : v } [@@unboxed] [@@unsafe_allow_any_mode_crossing]
    and v = Check__035_.v = { x : t } [@@unsafe_allow_any_mode_crossing]
  end

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

  include struct
    open struct
      [@@@warning "-34"]

      module Check__037_ = struct
        type t =
          | Both of (t * t) Ppx_fuelproof_runtime.list Ppx_fuelproof_runtime.Iarray.t
          | Other of (int * t) Ppx_fuelproof_runtime.Iarray.t Ppx_fuelproof_runtime.option
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t = Check__037_.t =
      | Both of (t * t) list Iarray.t
      | Other of (int * t) iarray option
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross_portability : t -> t = fun x -> x
  let cross_contention : t -> t = fun x -> x
end

(* [%fuelproof] doesn't error as long as at least one type in the knot has
   an annotation.
*)
module%test Recursive_knot = struct
  [@@@expand_inline
    type%fuelproof t = { u : u }
    and u = U : int -> u
    and v
    and w = int]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__039_ = struct
        type t = { u : u }
        and u = U : int -> u [@@unsafe_allow_any_mode_crossing]
        and v
        and w = int
      end
    end

    type t = Check__039_.t = { u : u }
    and u = Check__039_.u = U : int -> u [@@unsafe_allow_any_mode_crossing]
    and v = Check__039_.v
    and w = int
  end

  [@@@end]
end

module Deriving_ppxes : sig
  [@@@expand_inline:
    type%fuelproof t =
      | T :
          { x : int u
          ; y : int option
          }
          -> t

    and 'a u = U : 'a. 'a -> 'a u [@@deriving sexp_of]]

  type t =
    | T :
        { x : int u
        ; y : int Ppx_fuelproof_runtime.option
        }
        -> t
  [@@unsafe_allow_any_mode_crossing]

  and 'a u = U : 'a. 'a -> 'a u [@@deriving sexp_of] [@@unsafe_allow_any_mode_crossing]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val sexp_of_u : ('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  [@@@end]
end = struct
  let sexp_of_int = Base.Int.sexp_of_t

  [@@@expand_inline
    type%fuelproof t =
      | T :
          { x : int u
          ; y : int option [@option]
          }
          -> t

    and 'a u = U : 'a. 'a -> 'a u [@@deriving sexp_of]]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__041_ = struct
        type t =
          | T :
              { x : int u
              ; y : int Ppx_fuelproof_runtime.option
              }
              -> t
        [@@unsafe_allow_any_mode_crossing]

        and 'a u = U : 'a. 'a -> 'a u [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t = Check__041_.t =
      | T :
          { x : int u
          ; y : int option [@option]
          }
          -> t
    [@@unsafe_allow_any_mode_crossing]

    and 'a u = 'a Check__041_.u = U : 'a. 'a -> 'a u
    [@@deriving sexp_of] [@@unsafe_allow_any_mode_crossing]

    include struct
      let _ = fun (_ : t) -> ()
      let _ = fun (_ : 'a u) -> ()

      let rec sexp_of_t =
        (fun (T { x = x__043_; y = y__045_ }) ->
           let bnds__042_ = ([] : _ Stdlib.List.t) in
           let bnds__042_ =
             match y__045_ with
             | Stdlib.Option.None -> bnds__042_
             | Stdlib.Option.Some v__046_ ->
               let arg__048_ = sexp_of_int v__046_ in
               let bnd__047_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "y"; arg__048_ ] in
               (bnd__047_ :: bnds__042_ : _ Stdlib.List.t)
           in
           let bnds__042_ =
             let arg__044_ = sexp_of_u sexp_of_int x__043_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__044_ ] :: bnds__042_
              : _ Stdlib.List.t)
           in
           Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "T" :: bnds__042_)
         : t -> Sexplib0.Sexp.t)

      and sexp_of_u : 'a. ('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t =
        fun (type a__052_)
          : ((a__052_ -> Sexplib0.Sexp.t) -> a__052_ u -> Sexplib0.Sexp.t) ->
        fun _of_a__049_ (U arg0__050_) ->
        let res0__051_ = _of_a__049_ arg0__050_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "U"; res0__051_ ]
      ;;

      let _ = sexp_of_t
      and _ = sexp_of_u
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end

  [@@@end]
end

module Attributes_preserved : sig
  [@@@expand_inline: type%fuelproof t = { mutable x : int [@atomic] }]

  type t = { mutable x : int [@atomic] } [@@unsafe_allow_any_mode_crossing]

  [@@@end]
end = struct
  [@@@expand_inline type%fuelproof t = { mutable x : int [@atomic] }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__076_ = struct
        type t = { mutable x : int [@atomic] } [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t = Check__076_.t = { mutable x : int [@atomic] }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

(* Fuelproof handles recursive knots even when it is not responsible for checking
   all records in the knot (e.g. because it involves [unsafe_allow_any_mode_crossing].
*)
module Manifest : sig
  [@@@expand_inline:
    type%fuelproof t = { f : unit -> unit } [@@unsafe_allow_any_mode_crossing]
    and u = { x : t }]

  type t = { f : unit -> unit } [@@unsafe_allow_any_mode_crossing]
  and u = { x : t } [@@unsafe_allow_any_mode_crossing]

  [@@@end]
end = struct
  [@@@expand_inline
    type%fuelproof t = { f : unit -> unit } [@@unsafe_allow_any_mode_crossing]
    and u = { x : t }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__078_ = struct
        type t = { f : unit -> unit } [@@unsafe_allow_any_mode_crossing]
        and u = { x : t } [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t = Check__078_.t = { f : unit -> unit } [@@unsafe_allow_any_mode_crossing]
    and u = Check__078_.u = { x : t } [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end
