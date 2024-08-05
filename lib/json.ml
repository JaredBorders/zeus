open Olympus

module Json = struct
  let id_to_json (id : Id.t) : Yojson.Basic.t = `Int id

  let kind_to_json (kind : Kind.t) : Yojson.Basic.t =
    `String
      (match kind with
       | Kind.Market -> "Market"
       | Kind.Limit -> "Limit"
       | Kind.Stop -> "Stop")
  ;;

  let price_to_json (price : Price.t) : Yojson.Basic.t =
    `Int price
  ;;

  let quantity_to_json (quantity : Quantity.t)
    : Yojson.Basic.t
    =
    `Int quantity
  ;;

  let side_to_json (side : Side.t) : Yojson.Basic.t =
    `String
      (match side with
       | Side.Bid -> "Bid"
       | Side.Ask -> "Ask")
  ;;

  let order_to_json (order : Order.t) : Yojson.Basic.t =
    `Assoc
      [ "id", id_to_json order.id
      ; "kind", kind_to_json order.kind
      ; "side", side_to_json order.side
      ; "price", price_to_json order.price
      ; "quantity", quantity_to_json order.quantity
      ]
  ;;

  let orders_to_json (orders : (int * Order.t) list)
    : Yojson.Basic.t
    =
    `List
      (List.map
         (fun (_, order) -> order_to_json order)
         orders)
  ;;

  let level_to_json (level : Level.t) : Yojson.Basic.t =
    `Assoc
      [ "price", price_to_json level.price
      ; "quantity", quantity_to_json level.quantity
      ; "bids", orders_to_json (Level.bids level)
      ; "asks", orders_to_json (Level.asks level)
      ]
  ;;

  let levels_to_json (levels : Level.t array)
    : Yojson.Basic.t
    =
    `List (Array.to_list (Array.map level_to_json levels))
  ;;

  let trade_to_json (trade : Trade.t) : Yojson.Basic.t =
    `Assoc
      [ "bid", id_to_json trade.bid
      ; "ask", id_to_json trade.ask
      ]
  ;;

  let orderbook_to_json (orderbook : Orderbook.t)
    : Yojson.Basic.t
    =
    `Assoc
      [ "ticks", `Int orderbook.ticks
      ; "levels", levels_to_json orderbook.levels
      ; ( "ids"
        , `Assoc
            (Hashtbl.fold
               (fun key value acc ->
                 (string_of_int key, `Int value) :: acc)
               orderbook.ids
               []) )
      ]
  ;;

  let order_to_string (order : Order.t) : string =
    order_to_json order |> Yojson.Basic.pretty_to_string
  ;;

  let level_to_string (level : Level.t) : string =
    level_to_json level |> Yojson.Basic.pretty_to_string
  ;;

  let trade_to_string (trade : Trade.t) : string =
    trade_to_json trade |> Yojson.Basic.pretty_to_string
  ;;

  let orderbook_to_string (orderbook : Orderbook.t) : string
    =
    orderbook_to_json orderbook
    |> Yojson.Basic.pretty_to_string
  ;;
end
