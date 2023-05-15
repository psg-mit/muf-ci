open Ztypes
open Probzelus

module type InferSig = sig
  type pstate
  type 'a ds_distribution
  type 'a expr

  val const : 'a -> 'a expr

  val add : float expr * float expr -> float expr
  val subtract : float expr * float expr -> float expr
  (* TODO sig conflict ds_streaming and semi_symb *)
  (* val ( +~ ) : float expr -> float expr -> float expr *)
  val mult : float expr * float expr -> float expr
  val div : float expr * float expr -> float expr
  val expp : float expr -> float expr
  (* val ( *~ ) : float expr -> float expr -> float expr *)
  val pair : 'a expr * 'b expr -> ('a * 'b) expr
  val array : 'a expr array -> 'a array expr
  val matrix : 'a expr array array -> 'a array array expr
  val lst : 'a expr list -> 'a list expr
  val ite : bool expr -> 'a expr -> 'a expr -> 'a expr
  val lt : 'a expr * 'a expr -> bool expr
  val mat_add : Owl.Mat.mat expr * Owl.Mat.mat expr -> Owl.Mat.mat expr
  (* val ( +@~ ) : Owl.Mat.mat expr -> Owl.Mat.mat expr -> Owl.Mat.mat expr *)
  val mat_scalar_mult : float expr * Owl.Mat.mat expr -> Owl.Mat.mat expr
  (* val ( $*~ ) : float expr -> Owl.Mat.mat expr -> Owl.Mat.mat expr *)
  val mat_dot : Owl.Mat.mat expr * Owl.Mat.mat expr -> Owl.Mat.mat expr
  (* val ( *@~ ) : Owl.Mat.mat expr -> Owl.Mat.mat expr -> Owl.Mat.mat expr *)
  val vec_get : Owl.Mat.mat expr * int -> float expr
  val eval : 'a expr -> 'a
  val of_distribution : 'a Distribution.t -> 'a ds_distribution
  val gaussian : float expr * float expr -> float ds_distribution
  val beta : float * float -> float ds_distribution
  val bernoulli : float expr -> bool ds_distribution
  val binomial : int * float expr -> int ds_distribution

  val mv_gaussian :
    Owl.Mat.mat expr * Owl.Mat.mat -> Owl.Mat.mat ds_distribution

  val sample : (pstate * (string * 'a ds_distribution), 'a expr) cnode
  val observe : (pstate * ('a ds_distribution * 'a), unit) cnode
  val factor : (pstate * float, unit) cnode

  val infer_marginal :
    int -> (pstate * 'a, 'b expr) cnode -> ('a, 'b Distribution.t) cnode
  val pp_approx_status : bool -> unit
end

module Make (Infer : InferSig) = struct
  include Infer

  let prob = ref (Obj.magic ())

  let sample =
    let (Cnode { alloc; reset; copy; step }) = Infer.sample in
    let step self (_, x) = step self (!prob, x) in
    Cnode { alloc; reset; copy; step }

  let observe =
    let (Cnode { alloc; reset; copy; step }) = Infer.observe in
    let step self (_, x) = step self (!prob, x) in
    Cnode { alloc; reset; copy; step }

  let infer n f =
    let (Cnode { alloc; reset; copy; step }) = f in
    let step self (proba, x) =
      prob := proba;
      step self (!prob, x)
    in
    let f = Cnode { alloc; reset; copy; step } in
    Infer.infer_marginal n f

  let ite (i, t, e) = Infer.ite i t e

  (* let abs x =
     Infer.ite
       (Infer.lt(x, Infer.const(0.)))
       (Infer.subtract(Infer.const(0.), x))
       x *)
  (* TODO missing lt and subtract *)

  let random_order len =
    let sample_fn _ =
      let arr = Array.init len (fun i -> i) in
      for n = Array.length arr - 1 downto 1 do
        let k = Random.int (n + 1) in
        let tmp = arr.(n) in
        arr.(n) <- arr.(k);
        arr.(k) <- tmp
      done;
      arr
    in
    let obs_fn _ = assert false in
    Infer.of_distribution (Types.Dist_sampler (sample_fn, obs_fn))
end

module Infer_semi_symbolic = Make (Infer_semi_symbolic)
(* TODO: add var name to sample for ds *)
(* module Infer_ds_streaming = Make (Infer_ds_streaming) *)

let lt_det (a, b) = a < b
let eq_det (a, b) = a = b
let add_int (x, y) = x + y
let sub_int (x, y) = x - y
let add_float (x, y) = x +. y
let sub_float (x, y) = x -. y
let div_float (x, y) = x /. y
let pow (x, y) = x ** y
let exit code = exit code
let concat (a, b) = a ^ b

let read file =
  let data = ref [] in
  let ic = open_in file in
  let first = ref true in
  try
    while true do
      let line = input_line ic in
      if !first then first := false
      else
        let line_list = String.split_on_char ',' line in
        let line_list = List.map float_of_string line_list in
        data := line_list :: !data
    done;
    !data
  with
  | End_of_file ->
      close_in ic;
      !data
  | e ->
      close_in_noerr ic;
      raise e

module List = struct
  let length l = List.length l
  let nil = []
  let nil2 = []
  let hd = List.hd
  let tl = List.tl
  let cons (x, l) = x :: l
  let empty l = if List.length l = 0 then true else false
  let rev l = List.rev_append l []
  let filter (f, l) = List.filter f l
  let init (n, f) = List.init n f
  let append (a, b) = List.append a b
  let map (f, l) = List.map f l
  let iter (f, l) = List.iter f l
  let fold (f, a, b) = List.fold_left (fun a b -> f (a, b)) a b
  let zip (a, b) = List.map2 (fun x y -> (x, y)) a b

  let iter2 (f, l1, l2) =
    let f a b = f (a, b) in
    List.iter2 f l1 l2

  let shuffle (order, l) =
    let l_arr = Array.of_list l in
    let new_arr =
      Array.init (Array.length l_arr) (fun i ->
          Array.get l_arr (Array.get order i))
    in
    Array.to_list new_arr

  let print_float_list l =
    let rec aux l =
      match l with
      | [] -> ()
      | [ x ] -> Format.printf "%f" x
      | x :: xs ->
          Format.printf "%f, " x;
          aux xs
    in
    Format.printf "[@[";
    aux l;
    Format.printf "@]]"
end

module Array = struct
  let empty = [||]
  let init (n, f) = Array.init n f
  let get (a, x) = Array.get a x
end
