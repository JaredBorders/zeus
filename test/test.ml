open Zeus.Order
open Zeus.Level
open Zeus.Trade

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

  let%test "order modify" =
    let order = Order.create 1 Limit Bid 100.0 100 in
    let order' = Order.modify order Market 99.99 50 in
    order'.id = 1
    && order'.kind = Market
    && order'.side = Bid
    && order'.price = 99.99
    && order'.quantity = 50
  ;;

  let%test "order fill partial" =
    let order = Order.create 1 Limit Bid 100.0 100 in
    let order' = Order.fill order 50 in
    order'.id = 1
    && order'.kind = Limit
    && order'.side = Bid
    && order'.price = 100.0
    && order'.quantity = 50
  ;;

  let%test "order fill full" =
    let order = Order.create 1 Limit Bid 100.0 100 in
    let order' = Order.fill order 100 in
    order'.id = 1
    && order'.kind = Limit
    && order'.side = Bid
    && order'.price = 100.0
    && order'.quantity = 0
  ;;

  let%test "order fill excess" =
    let order = Order.create 1 Limit Bid 100.0 100 in
    try
      let _ = Order.fill order 101 in
      false
    with
    | Order.Invalid_quantity -> true
    | _ -> false
  ;;

  let%test "order filled false" =
    let order = Order.create 1 Limit Bid 100.0 100 in
    Order.filled order = false
  ;;

  let%test "order filled true" =
    let order = Order.create 1 Limit Bid 100.0 0 in
    Order.filled order = true
  ;;
end

module LevelTests = struct
  let%test "level create" =
    let level = Level.create 100.0 in
    level.price = 100.0 && level.quantity = 0 && Level.OrderMap.is_empty level.orders
  ;;

  let%test "level add order" =
    let level = Level.create 99.99 in
    let order = Order.create 1 Limit Bid 99.99 100 in
    let level' = Level.add order level in
    level'.price = 99.99
    && level'.quantity = 100
    && Level.OrderMap.cardinal level'.orders = 1
    && Level.OrderMap.find 1 level'.orders = order
  ;;

  let%test "level add orders" =
    let level = Level.create 99.99 in
    let orders =
      [ Order.create 1 Limit Bid 99.99 100
      ; Order.create 2 Limit Bid 99.99 80
      ; Order.create 3 Limit Bid 99.99 20
      ]
    in
    let level' = List.fold_left (fun acc order -> Level.add order acc) level orders in
    level'.price = 99.99
    && level'.quantity = 200
    && Level.OrderMap.cardinal level'.orders = 3
    && Level.OrderMap.find 1 level'.orders = List.nth orders 0
    && Level.OrderMap.find 2 level'.orders = List.nth orders 1
    && Level.OrderMap.find 3 level'.orders = List.nth orders 2
  ;;

  let%test "level add order wrong price" =
    let level = Level.create 99.99 in
    let order = Order.create 1 Limit Bid 100.0 100 in
    try
      let _ = Level.add order level in
      false
    with
    | Level.Wrong_price -> true
    | _ -> false
  ;;

  let%test "level remove order" =
    let level = Level.create 99.99 in
    let orders =
      [ Order.create 1 Limit Bid 99.99 100
      ; Order.create 2 Limit Bid 99.99 80
      ; Order.create 3 Limit Bid 99.99 20
      ]
    in
    let level' = List.fold_left (fun acc order -> Level.add order acc) level orders in
    let level'' = Level.remove 2 level' in
    level''.price = 99.99
    && level''.quantity = 120
    && Level.OrderMap.cardinal level''.orders = 2
    && Level.OrderMap.find 1 level''.orders = List.nth orders 0
    && Level.OrderMap.find 3 level''.orders = List.nth orders 2
  ;;

  let%test "level remove orders" =
    let level = Level.create 99.99 in
    let orders =
      [ Order.create 1 Limit Bid 99.99 100
      ; Order.create 2 Limit Bid 99.99 80
      ; Order.create 3 Limit Bid 99.99 20
      ]
    in
    let level' = List.fold_left (fun acc order -> Level.add order acc) level orders in
    let level'' = Level.remove 2 level' in
    let level''' = Level.remove 1 level'' in
    level'''.price = 99.99
    && level'''.quantity = 20
    && Level.OrderMap.cardinal level'''.orders = 1
    && Level.OrderMap.find 3 level'''.orders = List.nth orders 2
  ;;

  let%test "level remove all orders" =
    let level = Level.create 99.99 in
    let orders =
      [ Order.create 1 Limit Bid 99.99 100
      ; Order.create 2 Limit Bid 99.99 80
      ; Order.create 3 Limit Bid 99.99 20
      ]
    in
    let level' = List.fold_left (fun acc order -> Level.add order acc) level orders in
    let level'' = Level.remove 2 level' in
    let level''' = Level.remove 1 level'' in
    let level'''' = Level.remove 3 level''' in
    level''''.price = 99.99
    && level''''.quantity = 0
    && Level.OrderMap.is_empty level''''.orders
  ;;

  let%test "level remove absent order" =
    let level = Level.create 99.99 in
    try
      let _ = Level.remove 1 level in
      false
    with
    | Level.Absent_order 1 -> true
    | _ -> false
  ;;
end

module TradeTests = struct
  let%test "trade create" =
    let bid = Order.create 1 Limit Bid 100.0 100 in
    let ask = Order.create 2 Limit Ask 100.0 100 in
    let trade = Trade.create bid ask in
    trade.bid = bid && trade.ask = ask
  ;;

  let%test "trade create invalid bid" =
    let bid = Order.create 1 Limit Ask 100.0 100 in
    let ask = Order.create 2 Limit Ask 100.0 100 in
    try
      let _ = Trade.create bid ask in
      false
    with
    | Trade.Invalid_bid -> true
    | _ -> false
  ;;

  let%test "trade create invalid ask" =
    let bid = Order.create 1 Limit Bid 100.0 100 in
    let ask = Order.create 2 Limit Bid 100.0 100 in
    try
      let _ = Trade.create bid ask in
      false
    with
    | Trade.Invalid_ask -> true
    | _ -> false
  ;;
end
