open Zeus.Order

let order = Order.create 1 Market Bid 100.0 100

let () =
  print_endline "\n<><> zeus.0.0.1 ><><><><><><><><><><><><><><><><>\n";
  print_endline (Order.json order);
  print_endline "\n"
;;
