structure FP_test_base :> FP_test_base = struct
  open binary_ieeeSyntax binary_ieeeLib realLib
  
  fun error msg = (
    print("ERROR: " ^ msg ^ "\n");
    OS.Process.exit(OS.Process.failure)
  )
  

  fun hex_to_int s =
      case StringCvt.scanString (IntInf.scan StringCvt.HEX) s of
        NONE => error("No hex number " ^ s)
      | SOME i => i
    

  val hex_to_bin = (Arbnum.toBinString o Arbnum.fromHexString)


  fun pad_zeros n bstring = 
    let
      val l = explode bstring;
      fun add_zeros 0 = []
        | add_zeros n = #"0" :: add_zeros (n-1)
    in
      implode (add_zeros (n - (List.length l)) @ l)
  end


    
  fun triple_from_b32 s = 
    let
      val str_ = pad_zeros 32 s; 
      val (s,e,f) = (String.substring(str_,0,1), String.substring(str_,1,8), String.substring(str_,9,23))
    in
      (Arbnum.fromBinString s = Arbnum.one, Arbnum.fromBinString e, Arbnum.fromBinString f)
    end
    
  fun triple_from_b64 s = 
    let
      val str_ = pad_zeros 64 s; 
      val (s,e,f) = (String.substring(str_,0,1), String.substring(str_,1,11), String.substring(str_,12,52))
    in
      (Arbnum.fromBinString s = Arbnum.one, Arbnum.fromBinString e, Arbnum.fromBinString f)
    end
  
  fun make_signaling (fsize, esize) (sef as (s,e,f)) = 
    let
      val max_e =Arbnum.- (Arbnum.pow (Arbnum.two, Arbnum.fromInt esize), Arbnum.one)
      in 
        if (e = max_e andalso not (f = Arbnum.zero) andalso (Arbnum.log2 f) = (Arbnum.fromInt (fsize-1)))
        then (* it is qNaN *)
          (s,e,Arbnum.- (f, Arbnum.pow(Arbnum.two, Arbnum.fromInt (fsize-1))))
        else sef
      end
      
  fun float_of_hex format sef =
      (* first make all NaNs signaling *)
    let
      val sef = sef(* make_signaling format sef *)
    in
      float_of_triple (format, sef)
    end
      
      
        
  fun single_of_hex s = float_of_triple((23,8), ((triple_from_b32 o hex_to_bin) s))
  fun double_of_hex s = float_of_hex (52,11) ((triple_from_b64 o hex_to_bin) s)

  fun parse_flags "" =  NONE
    | parse_flags flagstring = 
    let
      val (underflow_before, invalid_op,divide_by_zero,overflow,underflow_after,inexact)= 
        case (hex_to_bin flagstring |> pad_zeros 6 |> explode |> map (fn x => if x = #"1" then ``T`` else ``F``)) of 
          [underflow_before, invalid_op,divide_by_zero,overflow,underflow_after,inexact] => (underflow_before, invalid_op,divide_by_zero,overflow,underflow_after,inexact)
        | _ => error("Invalid flag string " ^ flagstring)
    in 
      SOME ``<|DivideByZero := ^divide_by_zero; InvalidOp := ^invalid_op; Overflow := ^overflow; Precision := ^inexact;
                 Underflow_BeforeRounding := ^underflow_before; Underflow_AfterRounding := ^underflow_after|> : flags``
    end
  

  val ecr = (rhs o concl o EVAL)
  fun fval t = ecr ``float_value ^t``
  fun freal t = ecr ``float_to_real ^t``

  fun get_active_flags NONE = []
    | get_active_flags (SOME flags) = 
    let
      val flag_list = map (fn x => (ecr x) ~~ T) 
        [
          ``^flags.DivideByZero``, 
          ``^flags.InvalidOp``, 
          ``^flags.Overflow``, 
          ``^flags.Precision``, 
          ``^flags.Underflow_AfterRounding``,
          ``^flags.Underflow_BeforeRounding``
        ]
      fun get_active_flags_aux [] _ acc = acc
        | get_active_flags_aux (true::xs) (flag_string::ys) acc = get_active_flags_aux xs ys (flag_string::acc)
        | get_active_flags_aux (false::xs) (_::ys) acc = get_active_flags_aux xs ys acc
        | get_active_flags_aux _ _ _ = error "get_active_flags_aux: flag list and flag string list have different lengths"
          
    in
      get_active_flags_aux flag_list ["!DivideByZero ", "!InvalidOp ", "!Overflow ", "!Inexact ", "!Underflow_AfterRounding ", "!Underflow_BeforeRounding "] []
    end

  (* Arithmetic operations *)
  fun check_op op_ c flags = 
    let
      val res = fval ``SND $ ^op_``;
      val answ = fval c;
      val ret = aconv res answ;
      val (flag_ret, flag_res) = case flags of
        NONE => (true, NONE)
        | SOME flags => 
          let 
            val flags_result = ecr ``FST $ ^op_ ``
          in
            (aconv flags_result flags, SOME flags_result)
          end
    in
      (ret andalso flag_ret, res, flag_res)
    end
  
  

  fun check_fadd rm a b = check_op (mk_float_add (rm, a, b))
  fun check_fsub rm a b = check_op (mk_float_sub (rm, a, b)) 
  fun check_fmul rm a b = check_op (mk_float_mul (rm, a, b)) 
  fun check_fdiv rm a b = check_op (mk_float_div (rm, a, b))
  fun check_fsqrt rm a =  check_op (mk_float_sqrt (rm, a))
  fun check_fmul_add rm a b c = check_op (mk_float_mul_add (rm, a, b, c))

  (* Comparison operations *)
  fun check_comp op_ c = 
    let
      val res = ecr op_
      val ret = aconv res c
    in
      (ret, res, NONE)
    end

  fun check_feq a b = check_comp (mk_float_equal (a, b))
  fun check_flt a b = check_comp (mk_float_less_than (a, b))
  fun check_fle a b = check_comp (mk_float_less_equal (a, b))

  fun fadd rm a b = 
    let 
      val op_= mk_float_add (rm, a, b)
    in
      fval ``SND $ ^op_``
    end
  
  fun fsub rm a b = 
    let 
      val op_= mk_float_sub (rm, a, b)
    in
      fval ``SND $ ^op_``
    end
  
  fun fmul rm a b = 
    let 
      val op_= mk_float_mul (rm, a, b)
    in
      fval ``SND $ ^op_``
    end
  
  fun fdiv rm a b = 
    let 
      val op_= mk_float_div (rm, a, b)
    in
      fval ``SND $ ^op_``
    end
  
  fun fsqrt rm a = 
    let 
      val op_= mk_float_sqrt (rm, a)
    in
      fval ``SND $ ^op_``
    end
  
  fun fmuladd rm a b c = 
    let 
      val op_= mk_float_mul_add (rm, a, b, c)
    in
      fval ``SND $ ^op_``
    end
    
  fun single_of_real r = ecr ``real_to_float roundTiesToEven ^r: (23,8)float``
  fun single_of_real_rm rm r = ecr ``real_to_float ^rm ^r: (23,8)float``
  fun double_of_real r = ecr ``real_to_float roundTiesToEven ^r: (52,11)float``
  fun double_of_real_rm rm r = ecr ``real_to_float ^rm ^r: (52,11)float``

  fun float_string f = 
    case (term_to_string o fval) f of
      "NaN" => (case mk_float_is_signalling f |> ecr |> aconv T of
                  true => "sNaN"
                | _ => "qNaN") 
    | "Infinity" => let
                      val sign_ = mk_float_sign f |> ecr
                    in
                      if aconv (ecr ``^sign_ = 0w``) T then "+Inf" else "-Inf"
                    end

    | s => tl (String.tokens (fn c=> c = #" ") s) |> String.concat
end (*struct FP_test_base*)
