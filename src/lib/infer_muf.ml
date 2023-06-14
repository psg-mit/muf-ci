(* Semi-symbolic inference interface *)
module SSI = Semi_symbolic.Semi_symbolic_impl
open Utils

type 'a expr = 'a Utils.expr

let const = SSI.const
let add (a, b) = SSI.add a b
let sub (a, b) =
  SSI.add a (SSI.mul (SSI.const (-1.)) b)
let mul (a, b) = SSI.mul a b
let div (a, b) = SSI.div a b
let exp = SSI.exp
let eq (a, b) = SSI.eq a b
let lt (a, b) = SSI.lt a b
let pair = SSI.pair
let split = Utils.split

let get_const = Utils.get_const

let array = SSI.array
let matrix = SSI.matrix
let ite = SSI.ite
let lst = SSI.lst
let mat_add (a, b) = SSI.mat_add a b
let mat_scalar_mult (a, b) = SSI.mat_scalar_mult a b
let mat_dot (a, b) = SSI.mat_dot a b
let vec_get (a, b) = SSI.vec_get a b
let int_to_float = SSI.int_to_float
let gaussian (a, b) = SSI.gaussian a b
let beta (a, b) = SSI.beta a b
let bernoulli = SSI.bernoulli
let binomial (a, b) = SSI.binomial a b
let beta_binomial (a, b) = SSI.beta_binomial a b
let negative_binomial (a, b) = SSI.negative_binomial a b
let exponential = SSI.exponential
let gamma (a, b) = SSI.gamma a b
let poisson = SSI.poisson
let delta = SSI.delta
let mixture = SSI.mixture
(* let mv_gaussian (a, b) = SSI.mv_gaussian a b *)
(* let sampler (a, b) = SSI.sampler a b *)
let categorical (lower, upper, f) = SSI.categorical ~lower ~upper f
let uniform_int (a, b) = 
  let a = Utils.get_const a in
  let b = Utils.get_const b in
  SSI.categorical ~lower:a ~upper:b (fun _ -> 1./.(float_of_int (b-a+1)))

let value v = const (SSI.eval_sample v)

let pp_approx_status = SSI.pp_approx_status

let get_marginal_expr = Utils.get_marginal_expr


(* Inference *)
let sample name dist k (prob : 'b prob) =
  let v = SSI.sample name dist in
  k v prob

let factor score k prob =
  let particle = prob.particles.(prob.id) in
  prob.particles.(prob.id) <- { particle with score = particle.score +. score };
  k (const ()) prob

let observe d v =
  let score = SSI.observe 0. d (Utils.get_const v) in
  factor score

let resample unitt k prob =
  (* resample takes unit *)
  let () = Utils.get_const unitt in

  let resample' particles = 
    let scores = Array.map (fun p -> p.score) particles in
    let probabilities = Utils.normalize scores in
    let values = Array.map (fun p -> {p with score = 0.}) particles in

    let used = Hashtbl.create (Array.length particles) in

    Array.init (Array.length particles) (fun _ ->
      let j = Owl_stats.categorical_rvs probabilities in
      let particle = 
        if Hashtbl.mem used j then
          {values.(j) with k = Utils.copy values.(j).k}
        else begin
          Hashtbl.add used j ();
          values.(j)
        end
      in
      particle
    )
  in

  (* At a resample operator for a given particle,
   pause (update k for the particle) and run the next one *)
  let particle = prob.particles.(prob.id) in
  prob.particles.(prob.id) <- { particle with k = k (const ()) };
  let prob = 
    (* Computed everything up until the resample operator,
       so resample and start from beginning *)

    if prob.id = Array.length prob.particles - 1 then
      { id = -1; particles = resample' prob.particles }
    else
      prob
  in
  
  Utils.run_next prob

(* scores is logscale *)
let infer =
fun n model output_function ->
  let init_particles = { value = None; score = 0.; k = (model ()) Utils.finish } in
  let prob = { id = -1; particles = Array.make n init_particles } in

  let prob = Utils.run_next prob in

  (* Check if value of particles is unit *)
  let values = Array.map (fun p -> 
    get_marginal_expr (Option.get p.value)) prob.particles in
  let scores = Array.map (fun p -> p.score) prob.particles in

  (* Normalize and make mixture distribution *)
  let distr = Utils.to_distribution values scores in

  match output_function with
  | Some output_function -> 
    let _ = output_function distr in
    distr
  | None -> distr

(* Other useful functions *)

let int_of_float_det f =
  const (int_of_float (Utils.get_const f))
let float_of_int_det i =
  const (float_of_int (Utils.get_const i))
let bool_of_float_det f =
  const (Utils.get_const f > 0.)
let float_of_bool_det b =
  const (if Utils.get_const b then 1. else 0.)
let eq_det (a, b) = 
  const (Utils.get_const a = Utils.get_const b)
let add_int (x, y) = 
  let x = Utils.get_const x in
  let y = Utils.get_const y in
  const(x + y)
let sub_int (x, y) = 
  let x = Utils.get_const x in
  let y = Utils.get_const y in
  const (x - y)
let exit code = exit code
let concat (a, b) = a ^ b

let read file =
  let file = Utils.get_const file in
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
          const (float_of_string x)) line_list in
        data := lst(line_list) :: !data
    done;
    lst (!data)
  with
  | End_of_file ->
      close_in ic;
      lst (!data)
  | e ->
      close_in_noerr ic;
      raise e

(* Output Related functions *)
let pp_mdistr : type a. a mdistr -> string expr =
fun d ->
  let rec get_string : type a. a mdistr -> string =
  fun d -> 
    let pp_distribution : type a. a distribution -> string =
    fun d ->
      match d with
      | Normal (ExConst(mu), ExConst(var)) ->
        Format.sprintf "Gaussian(%f, %f)" mu var
      | Beta (ExConst(a), ExConst(b)) ->
        Format.sprintf "Beta(%f, %f)" a b
      | Bernoulli (ExConst(p)) ->
        Format.sprintf "Bernoulli(%f)" p
      | Binomial (ExConst(n), ExConst(p)) ->
        Format.sprintf "Binomial(%d, %f)" n p
      | BetaBinomial (ExConst(n), ExConst(a), ExConst(b)) ->
        Format.sprintf "BetaBinomial(%d, %f, %f)" n a b
      | NegativeBinomial (ExConst(n), ExConst(p)) ->
        Format.sprintf "NegativeBinomial(%d, %f)" n p
      | Gamma (ExConst(a), ExConst(b)) ->
        Format.sprintf "Gamma(%f, %f)" a b
      | Poisson (ExConst(p)) ->
        Format.sprintf "Poisson(%f)" p
      | Delta (ExConst(_)) ->
        (* TODO: any way to print the value? *)
        Format.sprintf "Delta (_)" 
      | Mixture (l) -> 
        Format.sprintf "Mixture([%s])" 
          (String.concat "; " (List.map (fun (e, p) -> 
            Format.sprintf "(%s, %f)" (get_string e) p) l))
      | Sampler _ | MvNormal _ | Categorical _ -> failwith "not implemented"
      | _ -> failwith "pp_mdistr: not marginal"
    in
    match d with
    | ExConst _ -> Format.sprintf "Const (_)"
    | ExRand rv -> Format.sprintf "%s" (pp_distribution rv.distr)
    | ExAdd (e1, e2) -> Format.sprintf "Add(%s, %s)" (get_string e1) (get_string e2)
    | ExMul (e1, e2) -> Format.sprintf "Mul(%s, %s)" (get_string e1) (get_string e2)
    | ExDiv (e1, e2) -> Format.sprintf "Div(%s, %s)" (get_string e1) (get_string e2)
    | ExList l -> Format.sprintf "List([%s])" (String.concat "; " (List.map get_string l))
    | ExPair (e1, e2) -> Format.sprintf "Pair(%s, %s)" (get_string e1) (get_string e2)
    | ExArray a -> Format.sprintf "Array([%s])" 
      (String.concat "; " (Array.to_list (Array.map get_string a)))
    | _ -> failwith "pp_mdistr: not marginal"
  in
  const (get_string d)
  

let mean_int : int mdistr -> float expr =
fun e ->
  let e = Utils.get_marginal_expr e in
  let e = SSI.eval e in
  let rec mean_int' : int expr -> float =
  fun e ->
    match e with
    | ExConst c -> float_of_int c
    | ExRand rv -> Utils.mean_int_d rv.distr
    | ExIte (i, t, e) -> if Utils.get_const (value i) then (mean_int' t) else (mean_int' e)
    | _ -> failwith "mean_int: not marginal"
  in
  ExConst (mean_int' e)

let mean_float : float expr -> float expr =
  fun e ->
    let e = Utils.get_marginal_expr e in
    let e = SSI.eval e in
    let rec mean_float' : float expr -> float =
    fun e ->
      match e with
      | ExConst c -> c
      | ExRand rv -> Utils.mean_float_d rv.distr
      | ExAdd (e1, e2) -> mean_float' e1 +. mean_float' e2
      | ExMul (e1, e2) -> mean_float' e1 *. mean_float' e2
      | ExDiv (e1, e2) -> mean_float' e1 /. mean_float' e2
      | ExIte (i, t, e) -> 
        if Utils.get_const (value i) then (mean_float' t) else (mean_float' e)
      | ExUnop (Squared, e_inner) -> (mean_float' e_inner) ** 2.
      | ExUnop (SquareRoot, e_inner) -> Float.sqrt (mean_float' e_inner)
      | ExUnop (Exp, e_inner) -> Float.exp (mean_float' e_inner)
      | ExIntToFloat e_inner -> Utils.get_const (mean_int e_inner)
      | _ -> failwith "mean_float: not marginal"
    in 
    ExConst (mean_float' e)

let mean_bool : bool expr -> float expr =
  fun e ->
    let e = Utils.get_marginal_expr e in
    let e = SSI.eval e in
    let rec mean_bool' : bool expr -> float =
    fun e ->
      match e with
      | ExConst c -> if c then 1. else 0.
      | ExRand rv -> Utils.mean_bool_d rv.distr
      | ExIte (i, t, e) -> 
        if Utils.get_const (value i) then (mean_bool' t) else (mean_bool' e)
      | _ -> failwith "mean_bool: not marginal"
    in
    ExConst (mean_bool' e)

module List = struct
  let length l k = k (const(List.length (Utils.get_lst l)))
  let nil () k = k (lst([]))
  let hd l k =
    let l = Utils.get_lst l in
    k (List.hd l)
  let tl l k = 
    let l = Utils.get_lst l in
    k (lst(List.tl l))
  let cons (x, l) k = 
    let l = Utils.get_lst l in
    k (lst(x :: l))
  let empty l k = 
    let l = Utils.get_lst l in
    k (const(List.length l = 0))
  let rev l k = 
    let l = Utils.get_lst l in
    k (lst(List.rev_append l []))
  let filter (f, l) k = 
    let l = Utils.get_lst l in
    let rec traversal f l acc k =
      match l with
      | [] -> k (lst(List.rev acc))
      | h :: t -> (f h) (fun a -> if Utils.get_const a then traversal f t (h :: acc) k else traversal f t acc k)
    in
    traversal f l [] k
  let init (n, f) k = 
    let n = Utils.get_const n in
    let rec traversal n f acc k =
      if n = 0 then k (lst(List.rev acc))
      else (f (const n)) (fun a -> traversal (n - 1) f (a :: acc) k)
    in
    traversal n f [] k
  let append (a, b) k = 
    let a = Utils.get_lst a in
    let b = Utils.get_lst b in
    k (lst(List.append a b))
  let map (f, l) k = 
    let l = Utils.get_lst l in
    let rec traversal l acc k =
      match l with
      | [] -> k (lst(List.rev acc))
      | h :: t -> (f h) (fun a -> traversal t (a :: acc) k)
    in
    traversal l [] k

  let iter (f, l) k =
    let rec traversal f l k =
      match l with 
      | [] -> k (const ())
      | h :: t -> (f h) (fun _ : unit expr -> 
        traversal f t k)
    in
    let l = Utils.get_lst l in
    traversal f l k

  let fold (f, l, init) k =
    let rec traversal l acc k =
      match l with
      | [] -> k acc
      | h :: t -> (f (acc, h)) (fun acc -> traversal t acc k)
    in
    traversal (Utils.get_lst l) init k

  let fold_resample (f, l, init) k =
    let f' = (fun (acc, h) k -> 
      f (acc, h) (fun x -> 
        (resample (const ())) (fun _ -> k x))
    ) in
    fold (f', l, init) k
    
  let zip (a, b) k = 
    let a = Utils.get_lst a in
    let b = Utils.get_lst b in
    k (lst(List.map2 (fun x y -> pair x y) a b))

  let iter2 (f, l1, l2) k =
    let l1 = Utils.get_lst l1 in
    let l2 = Utils.get_lst l2 in
    let rec traversal f l1 l2 k =
      match l1, l2 with
      | [], [] -> k ()
      | h1 :: t1, h2 :: t2 -> f h1 h2 (fun () -> traversal f t1 t2 k)
      | _ -> failwith "iter2: lists have different lengths"
    in
    traversal f l1 l2 k

  let from_array (a) k = 
    let a = Utils.get_array a in
    k (lst(Array.to_list a))
end

module Array = struct
  let empty = (fun _ -> array [||])
  let init (n, f) k = 
    let n = Utils.get_const n in
    let rec traversal n acc k =
      if n = 0 then k (array (Array.of_list (Stdlib.List.rev acc)))
      else f (const (n - 1)) (fun a -> traversal (n - 1) (a :: acc) k)
    in
    traversal n [] k

  let get (a, x) k = 
    let a = Utils.get_array a in
    if Utils.is_const x then k (Array.get a (Utils.get_const x))
    else
      let rec traversal inner i =
        let inner = ite (eq (const i, x)) (Array.get a i) inner in
        if i = Array.length a - 1 then k inner
        else traversal inner (i + 1)
      in
      traversal (Array.get a 0) 1

  let from_list (l) k = 
    let l = Utils.get_lst l in
    k (array (Array.of_list l))
end

module Print = struct
  let print_float (f) =
    let f = Utils.get_const f in
    Format.printf "%f" f;
    const ()
  let print_string (s) =
    let s = Utils.get_const s in
    Format.printf "%s" s;
    const ()
  let print_endline (s) =
    let () = Utils.get_const s in
    Format.printf "\n";
    const ()
  let print_float_list =
  fun l ->
    let l = Utils.get_lst l in
    let rec aux l =
      match l with
      | [] -> ()
      | [ x ] -> 
        Format.printf "%f" (Utils.get_const (mean_float x))
      | x :: xs ->
        Format.printf "%f, " (Utils.get_const (mean_float x));
        aux xs
    in
    Format.printf "[@[";
    aux l;
    Format.printf "@]]";
    const ()
  let print_float_list2 =
  fun l ->
    let l = Utils.get_lst l in
    let rec aux l =
      match l with
      | [] -> ()
      | [ x ] -> 
        Format.printf "%f" (Utils.get_const (mean_float x))
      | x :: xs ->
        Format.printf "%f " (Utils.get_const (mean_float x));
        aux xs
    in
    Format.printf "@[";
    aux l;
    Format.printf "@]";
    const ()
  let print_float_list3 =
  fun l ->
    let l = Utils.get_lst l in
    let rec aux l =
      match l with
      | [] -> ()
      | [ x ] -> 
        Format.printf "%f" (Utils.get_const (mean_float x))
      | x :: xs ->
        Format.printf "%f\n" (Utils.get_const (mean_float x));
        aux xs
    in
    aux l;
    const ()

  let print_bool_list2 =
  fun l ->
    let l = Utils.get_lst l in
    let rec aux l =
      match l with
      | [] -> ()
      | [ x ] -> 
        Format.printf "%f" (Utils.get_const (mean_bool x))
      | x :: xs ->
        Format.printf "%f " (Utils.get_const (mean_bool x));
        aux xs
    in
    Format.printf "@[";
    aux l;
    Format.printf "@]";
    const ()
end