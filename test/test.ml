open Zeus.Order
open Zeus.Level

module OrderTests = struct
  let%test "order create bid" =
    let order = Order.create 1 Market Bid 100 100 in
    order.id = 1
    && order.kind = Market
    && order.side = Bid
    && order.price = 100
    && order.quantity = 100
  ;;

  let%test "order create ask" =
    let order = Order.create 1 Market Ask 100 100 in
    order.id = 1
    && order.kind = Market
    && order.side = Ask
    && order.price = 100
    && order.quantity = 100
  ;;

  let%test "order modify" =
    let order = Order.create 1 Market Bid 100 100 in
    let order' = Order.modify order Limit 200 200 in
    order'.kind = Limit
    && order'.price = 200
    && order'.quantity = 200
  ;;

  let%test "order fill full" =
    let order = Order.create 1 Market Bid 100 100 in
    let order' = Order.fill order 100 in
    order'.quantity = 0
  ;;

  let%test "order fill partial" =
    let order = Order.create 1 Market Bid 100 100 in
    let order' = Order.fill order 50 in
    order'.quantity = 50
  ;;

  let%test "order filled false" =
    let order = Order.create 1 Market Bid 100 100 in
    Order.filled order = false
  ;;

  let%test "order filled true" =
    let order = Order.create 1 Market Bid 100 100 in
    let order' = Order.fill order 100 in
    Order.filled order' = true
  ;;
end

module LevelTests = struct
  let%test "level create" =
    let level = Level.create 100 in
    level.price = 100
  ;;

  let%test "level add order" = true
  let%test "level add orders" = true
  let%test "level add order wrong side" = true
  let%test "level add order wrong price" = true
  let%test "level remove order" = true
  let%test "level remove orders" = true
  let%test "level remove all orders" = true
  let%test "level remove absent order" = true
end
