module Order = struct
  type kind =
    | GoodTillCancel
    | FillAndKill
    | FillOrKill
    | GoodForDay
    | Market

  type side =
    | Buy
    | Sell

  type t =
    { id : int
    ; kind : kind
    ; side : side
    ; price : float
    ; quantity : int
    }

  let create id kind side price quantity = { id; kind; side; price; quantity }

  let string_of_order t =
    Printf.sprintf
      "Order %d: %s %s %f %d"
      t.id
      (match t.kind with
       | GoodTillCancel -> "GoodTillCancel"
       | FillAndKill -> "FillAndKill"
       | FillOrKill -> "FillOrKill"
       | GoodForDay -> "GoodForDay"
       | Market -> "Market")
      (match t.side with
       | Buy -> "Buy"
       | Sell -> "Sell")
      t.price
      t.quantity
  ;;
end
