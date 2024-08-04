open Price
open Quantity
open Order
open Side

module Level = struct
  exception Wrong_price
  exception Absent_order of int

  module OrderMap = Map.Make (Int)

  type t =
    { price : Price.t
    ; quantity : Quantity.t
    ; bids : Order.t OrderMap.t
    ; asks : Order.t OrderMap.t
    }

  let bids t = OrderMap.bindings t.bids
  let asks t = OrderMap.bindings t.asks

  let create price =
    { price
    ; quantity = 0
    ; bids = OrderMap.empty
    ; asks = OrderMap.empty
    }
  ;;

  let add t (order : Order.t) =
    if order.price <> t.price then raise Wrong_price;
    let quantity = t.quantity + order.quantity in
    match order.side with
    | Side.Bid ->
      { t with
        quantity
      ; bids = OrderMap.add order.id order t.bids
      }
    | Side.Ask ->
      { t with
        quantity
      ; asks = OrderMap.add order.id order t.asks
      }
  ;;

  let remove t id =
    let order =
      match OrderMap.find_opt id t.bids with
      | Some order -> order
      | None ->
        (match OrderMap.find_opt id t.asks with
         | Some order -> order
         | None -> raise (Absent_order id))
    in
    let quantity = t.quantity - order.quantity in
    match order.side with
    | Side.Bid ->
      let bids = OrderMap.remove id t.bids in
      { t with quantity; bids }
    | Side.Ask ->
      let asks = OrderMap.remove id t.asks in
      { t with quantity; asks }
  ;;
end
