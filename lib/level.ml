open Order

module Level = struct
  exception Wrong_side
  exception Wrong_price
  exception Absent_order of int

  module OrderMap = Map.Make (Int)

  type t =
    { price : float
    ; quantity : int
    ; bids : Order.t OrderMap.t
    ; asks : Order.t OrderMap.t
    }

  let create price =
    { price; quantity = 0; orders = OrderMap.empty }
  ;;

  let add (order : Order.t) level =
    if order.Order.price <> level.price
    then raise Wrong_price;
    let orders =
      OrderMap.add order.Order.id order level.orders
    in
    let quantity = level.quantity + order.Order.quantity in
    { level with quantity; orders }
  ;;

  let remove id level =
    let order =
      try OrderMap.find id level.orders with
      | Not_found -> raise (Absent_order id)
    in
    let orders = OrderMap.remove id level.orders in
    let quantity = level.quantity - order.Order.quantity in
    { level with quantity; orders }
  ;;
end
