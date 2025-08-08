open Base
open Ppxlib

module Supported_axis = struct
  module T = struct
    type t =
      | Portable
      | Contended
      | Unyielding
      | Many
    [@@deriving compare ~localize, sexp_of]
  end

  include Comparable.Make (T)
  include T

  let of_mod : Ppxlib_jane.mode -> t Or_error.t = function
    | Mode "portable" -> Ok Portable
    | Mode "contended" -> Ok Contended
    | Mode "unyielding" -> Ok Unyielding
    | Mode "many" -> Ok Many
    | Mode axis -> Or_error.errorf "Modifier not supported by fuelproof: %s" axis
  ;;

  let to_axis = function
    | Portable -> "portable"
    | Contended -> "contended"
    | Many -> "many"
    | Unyielding -> "unyielding"
  ;;
end

let immutable_data ~loc =
  [ Supported_axis.Contended; Many; Portable; Unyielding ]
  |> List.map ~f:(fun txt -> { txt; loc })
;;

let mutable_data ~loc =
  [ Supported_axis.Many; Portable; Unyielding ] |> List.map ~f:(fun txt -> { txt; loc })
;;

let axes_to_ignore modalities =
  List.filter_map
    modalities
    ~f:
      (fun
        (Ppxlib_jane.Shim.Modality.Modality field_modality) : Supported_axis.t option ->
      match field_modality with
      | "portable" -> Some Portable
      | "contended" -> Some Contended
      | "many" -> Some Many
      | "unyielding" -> Some Unyielding
      | _ -> None)
  |> Set.of_list (module Supported_axis)
;;

let crossing_axes_is_implied_by_immutable_data axes =
  List.for_all axes ~f:(fun { txt = m; loc = _ } ->
    match (m : Supported_axis.t) with
    | Portable | Contended | Unyielding | Many -> true)
;;

let type_with_builtin_cross_checking ty ~axes_to_cross ~axes_to_ignore =
  let axes_to_check_crossing =
    List.filter axes_to_cross ~f:(fun { txt = axis_to_cross; _ } ->
      not (Set.mem axes_to_ignore axis_to_cross))
  in
  let loc = { ty.ptyp_loc with loc_ghost = true } in
  match axes_to_check_crossing with
  | [] -> ty
  | _ :: _ ->
    let wrap_ty_in_check ty =
      Ppxlib_jane.Ast_builder.Default.ptyp_alias
        ty
        None
        (Some
           { pjkind_desc =
               Mod
                 ( { pjkind_desc = Abbreviation "any"; pjkind_loc = loc }
                 , List.map axes_to_check_crossing ~f:(fun axis ->
                     { txt = Ppxlib_jane.Shim.Mode.Mode (Supported_axis.to_axis axis.txt)
                     ; loc = { axis.loc with loc_ghost = true }
                     }) )
           ; pjkind_loc = loc
           })
        ~loc
    in
    (* The following code block sidesteps an issue with ppx_fuelproof's
       method of ensuring mode crossing. The issue presents when the
       recursive instance of the type being defined appears under a type
       constructor, like [list] or [iarray]. In these cases, mode crossing
       inference is made happier when the wrapping check appears closer
       to the type being defined.

       Engineering effort will be paid toward obviating ppx_fuelproof rather than making
       this check better.
    *)
    let rec wrap_loop ty =
      match ty with
      | [%type: [%t? ty] list]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] list]
      | [%type: [%t? ty] iarray]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] iarray]
      | [%type: [%t? ty] Iarray.t]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] Iarray.t]
      | [%type: [%t? ty1] * [%t? ty2]]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty1] * [%t wrap_loop ty2]]
      | [%type: [%t? ty] option]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] option]
      | _ -> wrap_ty_in_check ty
    in
    wrap_loop ty
;;

let rewrite_fields original_fields ~axes_to_cross =
  let open Or_error.Let_syntax in
  let check_crossing_contention =
    List.exists axes_to_cross ~f:(fun axis ->
      [%compare.equal: Supported_axis.t] axis.txt Contended)
  in
  List.map original_fields ~f:(fun field ->
    let modalities, _ = Ppxlib_jane.Shim.Label_declaration.extract_modalities field in
    let axes_to_ignore = axes_to_ignore modalities in
    let%bind () =
      match field.pld_mutable with
      | Mutable ->
        if check_crossing_contention
        then Or_error.errorf "Type with a mutable field can't cross contention"
        else Ok ()
      | Immutable -> Ok ()
    in
    return
      { field with
        pld_type =
          type_with_builtin_cross_checking field.pld_type ~axes_to_cross ~axes_to_ignore
      })
  |> Result.all
;;

let rewrite_arg arg ~axes_to_cross =
  let modalities, arg_type = Ppxlib_jane.Shim.Pcstr_tuple_arg.extract_modalities arg in
  let axes_to_ignore = axes_to_ignore modalities in
  Ppxlib_jane.Shim.Pcstr_tuple_arg.create
    ~loc:(Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg).ptyp_loc
    ~modalities
    ~type_:(type_with_builtin_cross_checking arg_type ~axes_to_cross ~axes_to_ignore)
;;

let rewrite_constructors original_constructors ~axes_to_cross =
  let open Or_error.Let_syntax in
  List.map original_constructors ~f:(fun ctor ->
    let%bind args =
      match ctor.pcd_args with
      | Pcstr_record fields ->
        let%bind fields = rewrite_fields fields ~axes_to_cross in
        return (Pcstr_record fields)
      | Pcstr_tuple args ->
        let args = List.map args ~f:(fun arg -> rewrite_arg arg ~axes_to_cross) in
        return (Pcstr_tuple args)
    in
    return { ctor with pcd_args = args })
  |> Result.all
;;

let rewrite_tydecls (tydecls : type_declaration list) =
  let open Or_error.Let_syntax in
  List.map tydecls ~f:(fun t ->
    let%bind axes_to_cross =
      match Ppxlib_jane.Shim.Type_declaration.extract_jkind_annotation t with
      | None -> Ok []
      | Some { pjkind_desc = Mod ({ pjkind_desc = Abbreviation "value"; _ }, mods); _ } ->
        List.map mods ~f:(fun mod_ ->
          let%map axis = Supported_axis.of_mod mod_.txt in
          { mod_ with txt = axis })
        |> Result.all
      | Some { pjkind_desc = Abbreviation "immutable_data"; pjkind_loc = loc } ->
        Ok (immutable_data ~loc)
      | Some { pjkind_desc = Abbreviation "mutable_data"; pjkind_loc = loc } ->
        Ok (mutable_data ~loc)
      | Some _ -> Or_error.error_string "Unsupported kind annotation for %fuelproof"
    in
    let%bind new_ptype_kind =
      match t.ptype_kind with
      | Ptype_record fields ->
        let%bind fields = rewrite_fields fields ~axes_to_cross in
        return (Ptype_record fields)
      | Ptype_variant constructors ->
        let%bind constructors = rewrite_constructors constructors ~axes_to_cross in
        return (Ptype_variant constructors)
      | _ -> Or_error.error_string "Can only write %fuelproof on records and variants"
    in
    let loc = { t.ptype_loc with loc_ghost = true } in
    Ok
      { t with
        ptype_kind = new_ptype_kind
      ; ptype_attributes =
          t.ptype_attributes
          @ [ { attr_loc = loc
              ; attr_name = { loc; txt = "unsafe_allow_any_mode_crossing" }
              ; attr_payload = PStr []
              }
            ]
      })
  |> Result.all
;;

let extension_str =
  Extension.declare
    "fuelproof"
    Structure_item
    Ast_pattern.(pstr (pstr_type __ __ ^:: nil))
    (fun ~loc ~path:_ rec_flag tydecls ->
      match rewrite_tydecls tydecls with
      | Ok tydecls -> Ast_builder.Default.pstr_type rec_flag tydecls ~loc
      | Error err ->
        List.iter ~f:Attribute.explicitly_drop#type_declaration tydecls;
        let err = Location.error_extensionf ~loc "%s" (Error.to_string_hum err) in
        Ast_builder.Default.pstr_extension err [] ~loc)
;;

let extension_sig =
  Extension.declare
    "fuelproof"
    Signature_item
    Ast_pattern.(
      psig
        (map_value
           ~f:(fun s -> (Ppxlib_jane.Shim.Signature.of_parsetree s).psg_items)
           (psig_type __ __ ^:: nil)))
    (fun ~loc ~path:_ rec_flag tydecls ->
      match rewrite_tydecls tydecls with
      | Ok tydecls -> Ast_builder.Default.psig_type rec_flag tydecls ~loc
      | Error err ->
        List.iter ~f:Attribute.explicitly_drop#type_declaration tydecls;
        let err = Location.error_extensionf ~loc "%s" (Error.to_string_hum err) in
        Ast_builder.Default.psig_extension err [] ~loc)
;;

let () =
  Driver.register_transformation
    "fuelproof"
    ~rules:
      [ Context_free.Rule.extension extension_str
      ; Context_free.Rule.extension extension_sig
      ]
;;
