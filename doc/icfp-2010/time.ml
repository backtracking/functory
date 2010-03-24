
let communication_percentage t seq_time net_time =
  let b = ceil (float t /. 8.) in
  let task_computation = seq_time /. float t in
  let computation = b *. task_computation in
  100. *. (net_time -. computation) /. net_time
