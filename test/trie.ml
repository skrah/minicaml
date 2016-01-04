(* Trie benchmark *)

type trie = { next : forest }
and forest = trie array
type word = int array
type randseed = { mutable seed : int }

let alphabet_len = 30
let word_len = 10

let new_trie () = { next = [nil] ** 30 }
let new_word () = [0] ** 10

let rec loop1 (t, w, i) =
  if t = nil then ()
  else if i < 10 then
    let j = w.(i) in
      if t.next.(j) = nil then
        (t.next.(j) <- new_trie ();
         loop1 (t.next.(j), w, i+1))
      else
        loop1 (t.next.(j), w, i+1)
  else ()

let insert (t, w) = loop1 (t, w, 0)

let rec loop2 (t, w, i) =
  if t = nil then false
  else if i = 10 then true
  else if t.next.(w.(i)) <> nil && i < 10 then
    loop2 (t.next.(w.(i)), w, i+1)
  else false

let lookup (t, w) = loop2 (t, w, 0)

let xmod (x, y) = let z = x / y in x - z * y

let next = { seed = 1 }
let factor = 461168623 * 191203711 * 11 * 5
let rand (r, factor) =
  let n = r.seed * factor + 12345 in
  let x = n / (65536 * 65536) in
  let y = x / (65536 * 32768) in
  let z = x - y in
  let zz = if z < 0 then -z else z in
    r.seed <- n; zz

let t = new_trie ()

let _ =
  for i = 0 to 100000 do
    let w = new_word () in
      for j = 0 to word_len-1 do
        w.(j) <- xmod (rand(next, factor), alphabet_len)
      done;
      insert (t, w)
  done;

  next.seed <- 1;

  for i = 0 to 100000 do
    let w = new_word () in
      for j = 0 to word_len-1 do
        w.(j) <- xmod (rand(next, factor), alphabet_len)
      done;
      if lookup (t, w) then ()
      else print_string ("Not_found\n")
  done


