
type 'a expr = 'a Semi_symbolic.expr

let const = Semi_symbolic.const
let get_const = Semi_symbolic.get_const
let add (a, b) = Semi_symbolic.add a b
let sub (a, b) =
  Semi_symbolic.add a (Semi_symbolic.mul (Semi_symbolic.const (-1.)) b)
let mul (a, b) = Semi_symbolic.mul a b
let div (a, b) = Semi_symbolic.div a b
let exp = Semi_symbolic.exp
let eq (a, b) = Semi_symbolic.eq a b
let lt (a, b) = Semi_symbolic.lt a b
let pair = Semi_symbolic.pair
let split = Semi_symbolic.split
let array = Semi_symbolic.array
let get_array = Semi_symbolic.get_array
let matrix = Semi_symbolic.matrix
let ite = Semi_symbolic.ite
let lst = Semi_symbolic.lst
let get_lst = Semi_symbolic.get_lst
let mat_add (a, b) = Semi_symbolic.mat_add a b
let mat_scalar_mult (a, b) = Semi_symbolic.mat_scalar_mult a b
let mat_dot (a, b) = Semi_symbolic.mat_dot a b
let vec_get (a, b) = Semi_symbolic.vec_get a b
let int_to_float = Semi_symbolic.int_to_float
let gaussian (a, b) = Semi_symbolic.gaussian a b
let beta (a, b) = Semi_symbolic.beta a b
let bernoulli = Semi_symbolic.bernoulli
let binomial (a, b) = Semi_symbolic.binomial a b
let beta_binomial (a, b) = Semi_symbolic.beta_binomial a b
let negative_binomial (a, b) = Semi_symbolic.negative_binomial a b
let exponential = Semi_symbolic.exponential
let gamma (a, b) = Semi_symbolic.gamma a b
let poisson = Semi_symbolic.poisson
(* let mv_gaussian (a, b) = Semi_symbolic.mv_gaussian a b *)
(* let sampler (a, b) = Semi_symbolic.sampler a b *)
(* let categorical (a, b, c) = Semi_symbolic.categorical ~lower: a ~upper: b c *)
let sample = Semi_symbolic.sample
let make_marginal = Semi_symbolic.make_marginal
(* TODO: observe's score needs to be added to weight when sampling *)
let observe d v =
  (fun d v -> 
    let _ =  Semi_symbolic.observe 0. d (get_const v) in
    ()) d v
let value x = const (Semi_symbolic.eval_sample x)
let pp_approx_status = Semi_symbolic.pp_approx_status

let get_marginal_expr = Semi_symbolic.get_marginal_expr

let pp_distribution = Semi_symbolic.pp_distribution
let mean_float = Semi_symbolic.mean_float
let mean_int = Semi_symbolic.mean_int
let mean_bool = Semi_symbolic.mean_bool

let infer f output =
  (* TODO: particle filter *)
  let res = f () in
  let marginal_res = Semi_symbolic.get_marginal_expr res in
  match output with
  | Some output -> 
    let _ = output marginal_res in
    marginal_res
  | None -> marginal_res

let int_of_float_det f =
  Semi_symbolic.const (int_of_float (get_const f))
let lt_det (a, b) = a < b
let eq_det (a, b) = 
  Semi_symbolic.const (get_const a = get_const b)
let add_int (x, y) = x + y
let sub_int (x, y) = x - y
let add_float (x, y) = x +. y
let sub_float (x, y) = x -. y
let div_float (x, y) = x /. y
let pow (x, y) = x ** y
let exit code = exit code
let concat (a, b) = a ^ b

let read file =
  let file = get_const file in
  let data = ref [] in
  let ic = open_in file in
  let first = ref true in
  try
    while true do
      let line = input_line ic in
      if !first then first := false
      else
        let line_list = String.split_on_char ',' line in
        let line_list = List.map (fun x -> 
          Semi_symbolic.const (float_of_string x)) line_list in
        data := Semi_symbolic.lst(line_list) :: !data
    done;
    Semi_symbolic.lst (!data)
  with
  | End_of_file ->
      close_in ic;
      Semi_symbolic.lst (!data)
  | e ->
      close_in_noerr ic;
      raise e

module List = struct
  let length l = Semi_symbolic.const(List.length l)
  let nil = (fun () -> Semi_symbolic.lst [])
  let hd l =
    let l = Semi_symbolic.get_lst l in
    List.hd l
  let tl l = 
    let l = Semi_symbolic.get_lst l in
    Semi_symbolic.lst(List.tl l)
  let cons (x, l) = 
    let l = Semi_symbolic.get_lst l in
    Semi_symbolic.lst(x :: l)
  let empty l = 
    let l = Semi_symbolic.get_lst l in
    Semi_symbolic.const(List.length l = 0)
  let rev l = 
    let l = Semi_symbolic.get_lst l in
    Semi_symbolic.lst(List.rev_append l [])
  let filter (f, l) = 
    let l = Semi_symbolic.get_lst l in
    Semi_symbolic.lst(List.filter f l)
  let init (n, f) = 
    let n = get_const n in
    Semi_symbolic.lst(List.init n (fun i ->
      let i = Semi_symbolic.const i in
      f i))
  let append (a, b) = 
    let a = Semi_symbolic.get_lst a in
    let b = Semi_symbolic.get_lst b in
    Semi_symbolic.lst(List.append a b)
  let map (f, l) = 
    let l = Semi_symbolic.get_lst l in
    Semi_symbolic.lst(List.map f l)
  let iter (f, l) = 
    let l = Semi_symbolic.get_lst l in
    List.iter f l
  let fold (f, a, l) = 
    let l = Semi_symbolic.get_lst l in
    List.fold_left (fun a b -> f (a, b)) a l
  let zip (a, b) = 
    let a = Semi_symbolic.get_lst a in
    let b = Semi_symbolic.get_lst b in
    Semi_symbolic.lst(List.map2 (fun x y -> Semi_symbolic.pair x y) a b)

  let iter2 (f, l1, l2) =
    let l1 = Semi_symbolic.get_lst l1 in
    let l2 = Semi_symbolic.get_lst l2 in
    let f a b = f (a, b) in
    List.iter2 f l1 l2
end

module Array = struct
  let empty = (fun _ -> Semi_symbolic.array [||])
  let init (n, f) = 
    Semi_symbolic.array (
      Array.init (get_const n) (fun x -> 
        f (Semi_symbolic.const x)))
  let get (a, x) = 
    let a = Semi_symbolic.get_array a in
    Array.get a (get_const x)
end

module Print = struct
  let print_float (f) =
    let f = get_const f in
    Format.printf "%f" f
  let print_string (s) =
    let s = get_const s in
    Format.printf "%s" s
  let print_endline (s) =
    let s = get_const s in
    Format.printf "%s\n" s
  let print_float_list : float list Semi_symbolic.expr -> unit =
  fun l ->
    let l = Semi_symbolic.get_lst l in
    let rec aux l =
      match l with
      | [] -> ()
      | [ x ] -> Format.printf "%f" (get_const x)
      | x :: xs ->
        Format.printf "%f, " (get_const x);
        aux xs
    in
    Format.printf "[@[";
    aux l;
    Format.printf "@]]"
end