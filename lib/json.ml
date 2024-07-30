open Order
open Level

module Json = struct
  exception InvalidKindJson
  exception InvalidSideJson
  exception InvalidOrderIdJson
  exception InvalidOrderPriceJson
  exception InvalidOrderQuantityJson
  exception InvalidOrderJson
  exception InvalidLevelPriceJson
  exception InvalidLevelQuantityJson
  exception InvalidLevelJson
  exception InvalidOrderPairJson
  exception InvalidOrderMapJson

  let kind_of_yojson = function
    | `String "Market" -> Order.Market
    | `String "Limit" -> Order.Limit
    | `String "Stop" -> Order.Stop
    | _ -> raise InvalidKindJson
  ;;

  let yojson_of_kind = function
    | Order.Market -> `String "Market"
    | Order.Limit -> `String "Limit"
    | Order.Stop -> `String "Stop"
  ;;

  let side_of_yojson = function
    | `String "Bid" -> Order.Bid
    | `String "Ask" -> Order.Ask
    | _ -> raise InvalidSideJson
  ;;

  let yojson_of_side = function
    | Order.Bid -> `String "Bid"
    | Order.Ask -> `String "Ask"
  ;;

  let assoc_field name fields =
    try List.assoc name fields with
    | Not_found -> raise InvalidOrderJson
  ;;

  let int_of_yojson = function
    | `Int x -> x
    | _ -> raise InvalidOrderIdJson
  ;;

  let float_of_yojson = function
    | `Float x -> x
    | _ -> raise InvalidOrderPriceJson
  ;;

  let order_of_yojson = function
    | `Assoc fields ->
      { Order.id = int_of_yojson (assoc_field "id" fields)
      ; kind = kind_of_yojson (assoc_field "kind" fields)
      ; side = side_of_yojson (assoc_field "side" fields)
      ; price = float_of_yojson (assoc_field "price" fields)
      ; quantity = int_of_yojson (assoc_field "quantity" fields)
      }
    | _ -> raise InvalidOrderJson
  ;;

  let yojson_of_order (t : Order.t) =
    `Assoc
      [ "id", `Int t.id
      ; "kind", yojson_of_kind t.kind
      ; "side", yojson_of_side t.side
      ; "price", `Float t.price
      ; "quantity", `Int t.quantity
      ]
  ;;

  let json_of_order (t : Order.t) = Yojson.Safe.pretty_to_string (yojson_of_order t)

  module OrderMap = Map.Make (Int)

  let order_pair_of_yojson = function
    | `List [ `Int k; v ] -> k, order_of_yojson v
    | _ -> raise InvalidOrderPairJson
  ;;

  let order_pair_to_yojson (k, v) = `List [ `Int k; yojson_of_order v ]

  let orders_of_yojson = function
    | `List lst ->
      List.fold_left
        (fun map entry ->
          let k, v = order_pair_of_yojson entry in
          OrderMap.add k v map)
        OrderMap.empty
        lst
    | _ -> raise InvalidOrderMapJson
  ;;

  let orders_to_yojson orders =
    `List (OrderMap.fold (fun k v acc -> order_pair_to_yojson (k, v) :: acc) orders [])
  ;;

  let level_of_yojson = function
    | `Assoc fields ->
      { Level.price = float_of_yojson (assoc_field "price" fields)
      ; quantity = int_of_yojson (assoc_field "quantity" fields)
      ; orders = orders_of_yojson (assoc_field "orders" fields)
      }
    | _ -> raise InvalidLevelJson
  ;;

  let yojson_of_level (t : Level.t) =
    `Assoc
      [ "price", `Float t.price
      ; "quantity", `Int t.quantity
      ; "orders", orders_to_yojson t.orders
      ]
  ;;

  let json_of_level (t : Level.t) = Yojson.Safe.pretty_to_string (yojson_of_level t)
end
