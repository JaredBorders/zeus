module Order = struct
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

  let create id kind side price quantity = { id; kind; side; price; quantity }
  let modify t kind side price quantity = { t with kind; side; price; quantity }
end
