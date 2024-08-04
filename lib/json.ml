open Kind
open Side
open Order

module Json = struct
  exception Invalid_kind
  exception Invalid_side
  exception Invalid_order

  let kind_to_json = function
    | Kind.Market -> `String "Market"
    | Kind.Limit -> `String "Limit"
    | Kind.Stop -> `String "Stop"
  ;;

  let kind_of_json = function
    | `String "Market" -> Kind.Market
    | `String "Limit" -> Kind.Limit
    | `String "Stop" -> Kind.Stop
    | _ -> raise Invalid_kind
  ;;

  let side_to_json = function
    | Side.Bid -> `String "Bid"
    | Side.Ask -> `String "Ask"
  ;;

  let side_of_json = function
    | `String "Bid" -> Side.Bid
    | `String "Ask" -> Side.Ask
    | _ -> raise Invalid_side
  ;;

  let order_to_json t =
    `Assoc
      [ "id", `Int t.Order.id
      ; "kind", kind_to_json t.Order.kind
      ; "side", side_to_json t.Order.side
      ; "price", `Int t.Order.price
      ; "quantity", `Int t.Order.quantity
      ]
  ;;

  let order_of_json = function
    | `Assoc
        [ ("id", `Int id)
        ; ("kind", kind_json)
        ; ("side", side_json)
        ; ("price", `Int price)
        ; ("quantity", `Int quantity)
        ] ->
      let kind = kind_of_json kind_json in
      let side = side_of_json side_json in
      Order.{ id; kind; side; price; quantity }
    | _ -> raise Invalid_order
  ;;
end
