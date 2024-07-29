open Order

module Level = struct
  exception WrongSide
  exception WrongPrice
  exception AbsentOrder of int

  module OrderMap = Map.Make (Int)

  type t =
    { price : float
    ; side : Order.side
    ; quantity : int
    ; orders : Order.t OrderMap.t
    }

  let create price (side : Order.side) =
    { price; side; quantity = 0; orders = OrderMap.empty }
  ;;

  let add (order : Order.t) level =
    if order.Order.side <> level.side then raise WrongSide;
    if order.Order.price <> level.price then raise WrongPrice;
    let orders = OrderMap.add order.Order.id order level.orders in
    let quantity = level.quantity + order.Order.quantity in
    { level with quantity; orders }
  ;;

  let remove id level =
    let order =
      try OrderMap.find id level.orders with
      | Not_found -> raise (AbsentOrder id)
    in
    let orders = OrderMap.remove id level.orders in
    let quantity = level.quantity - order.Order.quantity in
    { level with quantity; orders }
  ;;
end
