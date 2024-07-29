open Zeus.Order

module OrderTests = struct
  let%test "order create bid" =
    let order = Order.create 1 Limit Bid 100.0 100 in
    order.id = 1
    && order.kind = Limit
    && order.side = Bid
    && order.price = 100.0
    && order.quantity = 100
  ;;

  let%test "order create ask" =
    let order = Order.create 1 Limit Ask 100.0 100 in
    order.id = 1
    && order.kind = Limit
    && order.side = Ask
    && order.price = 100.0
    && order.quantity = 100
  ;;
end
