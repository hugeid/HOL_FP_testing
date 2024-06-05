signature FP_test_base = sig
  val error : string -> 'a

  val hex_to_int : string -> IntInf.int
  val hex_to_bin : string -> string

  val triple_from_b32 : string -> bool * num * num
  val triple_from_b64 : string -> bool * num * num
  val single_of_hex : string -> term
  val double_of_hex : string -> term
  val make_signaling : int * int -> bool * num * num -> bool * num * num
  val parse_flags : string -> term option
  val fval: term -> term
  val freal: term -> term
  val ecr: term -> term
  val float_string : term -> string
  val get_active_flags : term option -> string list

  val check_fadd : term -> term -> term -> term -> term option-> bool * term * term option
  val check_fsub : term -> term -> term -> term -> term option -> bool * term * term option 
  val check_fmul : term -> term -> term -> term -> term option -> bool * term * term option
  val check_fdiv : term -> term -> term -> term -> term option -> bool * term * term option
  val check_fsqrt : term -> term -> term -> term option -> bool * term * term option
  val check_fmul_add : term -> term -> term -> term -> term -> term option -> bool * term * term option

  val check_feq : term -> term -> term -> bool * term * term option
  val check_flt : term -> term -> term -> bool * term * term option
  val check_fle : term -> term -> term -> bool * term * term option

  val fadd : term -> term -> term -> term
  val fsub : term -> term -> term -> term
  val fmul : term -> term -> term -> term
  val fdiv : term -> term -> term -> term
  val fsqrt : term -> term -> term
  val fmuladd : term -> term -> term -> term -> term

  val single_of_real : term -> term
  val double_of_real : term -> term
  val single_of_real_rm : term -> term -> term
  val double_of_real_rm : term -> term -> term
end