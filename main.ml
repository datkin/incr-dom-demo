open Core_kernel
open Async_kernel
open Incr_dom
open Incr.Let_syntax

module App : sig
  include App_intf.S_simple
  val initial_model : Model.t
end = struct
  module Model = struct
    type t = int list
    let cutoff = phys_equal
  end

  module Action = struct
    type t =
      | Increment of { idx : int }
      | Reset of { idx : int }
    [@@deriving sexp_of]
    let should_log (_ : t) = true
  end

  module State = struct
    type t = unit
  end

  let initial_model : Model.t = List.init 100 ~f:(fun _ -> Random.int 100)

  let apply_action (action : Action.t) (model : Model.t) (_ : State.t) =
    let update ~idx ~f =
      List.mapi model ~f:(fun i c -> if i = idx then f c else c)
    in
    match action with
    | Increment { idx } -> update ~idx ~f:((+) 1)
    | Reset { idx } -> update ~idx ~f:(const 0)
  ;;

  let view (model : Model.t Incr.t) ~inject =
    let open Vdom in
    let%map model = model in
    let rows =
      List.mapi model ~f:(fun idx count ->
        let color =
          if count < 10
          then "green"
          else if count < 100
          then "blue"
          else "red"
        in
        Node.tr
          [
            Attr.on_click (fun _ev -> inject (Action.Increment { idx }));
            Attr.on_double_click (fun _ev -> inject (Action.Reset { idx }));
          ]
          [
            Node.td 
              []
              [ Node.text (Int.to_string (idx + 1)) ];
            Node.td 
              [ Attr.style [ ("color", color) ] ]
              [ Node.text (Int.to_string count) ];
          ])
    in
    Node.body
      []
      [
        Node.h3 [] [ Node.text "My app" ];
        Node.table [] rows;
      ]
  ;;

  let on_startup ~schedule:_ (_ : Model.t) = Deferred.return ()

  let update_visibility (model : Model.t) = model

  let on_display ~old:(_ : Model.t) (_ : Model.t) (_ : State.t) = ()
end

let () =
  Start_app.simple
    (module App)
    ~initial_model:App.initial_model
 
