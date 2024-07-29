open Zeus.Order
open Zeus.Level
open Zeus.Json

let orders =
  [ Order.create 1 Limit Bid 99.99 100
  ; Order.create 2 Limit Bid 99.99 80
  ; Order.create 3 Limit Bid 99.99 20
  ]
;;

let level =
  List.fold_left (fun acc order -> Level.add order acc) (Level.create 99.99 Bid) orders
;;

let () =
  print_endline "\n<><> zeus.0.0.1 ><><><><><><><><><><><><><><><><>\n";
  print_endline (Json.json_of_level level);
  print_endline "\n"
;;
