module Order = struct
  exception Invalid_quantity

  type kind =
    | Market
    | Limit
    | Stop

  type side =
    | Bid
    | Ask

  type t =
    { id : int
    ; kind : kind
    ; side : side
    ; price : float
    ; quantity : int
    }

  let create id kind side price quantity =
    { id; kind; side; price; quantity }
  ;;

  let modify t kind price quantity =
    { t with kind; price; quantity }
  ;;

  let fill t quantity =
    if quantity > t.quantity then raise Invalid_quantity;
    { t with quantity = t.quantity - quantity }
  ;;

  let filled t = t.quantity = 0
end
