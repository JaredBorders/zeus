module Side = struct
  type t =
    | Bid
    | Ask

  let compare = compare

  let to_string = function
    | Bid -> "Bid"
    | Ask -> "Ask"
  ;;
end
