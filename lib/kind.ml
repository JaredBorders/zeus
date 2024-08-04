module Kind = struct
  type t =
    | Market
    | Limit
    | Stop

  let compare = compare

  let to_string = function
    | Market -> "Market"
    | Limit -> "Limit"
    | Stop -> "Stop"
  ;;
end
