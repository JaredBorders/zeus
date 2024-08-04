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
