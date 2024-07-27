open Zeus.Order

let%test "order create" =
  let order = Order.create 1 GoodTillCancel Buy 100.0 100 in
  order.id = 1
  && order.kind = GoodTillCancel
  && order.side = Buy
  && order.price = 100.0
  && order.quantity = 100
;;

let%test "order string_of_order" =
  let order = Order.create 2 Market Sell 190.23 999 in
  Order.string_of_order order = "Order 2: Market Sell 190.230000 999"
;;
