open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Order = struct
  type kind =
    | Market
    | Limit
    | Stop
  [@@deriving yojson]

  type side =
    | Bid
    | Ask
  [@@deriving yojson]

  type t =
    { id : int
    ; kind : kind
    ; side : side
    ; price : float
    ; quantity : int
    }
  [@@deriving yojson]

  let create id kind side price quantity = { id; kind; side; price; quantity }
  let modify t kind side price quantity = { t with kind; side; price; quantity }
  let json t = Yojson.Safe.to_string (yojson_of_t t)
end
