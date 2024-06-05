structure FP_test_parser = struct
  local
    open FP_test_base binary_ieeeSyntax
  in

    val xlate_hex_opr32 = single_of_hex
    val xlate_hex_opr64 = double_of_hex
  

    fun xlate_opr32 "0x0" = F
      | xlate_opr32 "0x1" = T
      | xlate_opr32 s = xlate_hex_opr32 s

    fun xlate_opr64 "0x0" = F
      | xlate_opr64 "0x1" = T
      | xlate_opr64 s = xlate_hex_opr64 s

    fun xlate_rm "0" = roundTowardZero_tm
      | xlate_rm "=0" = roundTiesToEven_tm
      | xlate_rm ">" = roundTowardPositive_tm
      | xlate_rm "<" = roundTowardNegative_tm
      | xlate_rm x = error ("xlate_rm: " ^ x)
    

    fun check_vector_aux with_flags v = let
      val (f, rm, args, flags) = case (String.tokens (Char.isSpace) v, with_flags) of
        ((f::rm::args), true) => (f, rm, butlast args, last args)
        | ((f::rm::args), false) => (f, rm, args, "")
      | _ => error("Bad vector: " ^ v);

      val rm = xlate_rm rm
      fun args32 () = map xlate_opr32 args
      fun args64 () = map xlate_opr64 args
      val flags = parse_flags flags

    in
      if String.isPrefix "b32" f then
        case (f,args32 ()) of
          ("b32+",[a,b,c])    => (check_fadd rm a b c flags, "+", rm, [a,b,c], flags)
        | ("b32-",[a,b,c])    => (check_fsub rm a b c flags, "-", rm, [a,b,c], flags)
        | ("b32*",[a,b,c])    => (check_fmul rm a b c flags, "*", rm, [a,b,c], flags)
        | ("b32mul",[a,b,c])  => (check_fmul rm a b c flags, "*", rm, [a,b,c], flags)
        | ("b32/",[a,b,c])    => (check_fdiv rm a b c flags, "/", rm, [a,b,c], flags)
        | ("b32V",[a,b])      => (check_fsqrt rm a b flags, "V", rm, [a,b], flags)
        | ("b32*+",[a,b,c,d]) => (check_fmul_add rm a b c d flags, "*+", rm, [a,b,c,d], flags)
        | ("b32eq",[a,b,c])    => (check_feq a b c, "==", rm, [a,b,c], flags)
        | ("b32lt",[a,b,c])    => (check_flt a b c, "<", rm, [a,b,c], flags)
        | ("b32le",[a,b,c])    => (check_fle a b c, "<=", rm, [a,b,c], flags)
        | (_,_) => error("Unsupported op: " ^ f)
      else
        case (f,args64 ()) of
          ("b64+",[a,b,c])    => (check_fadd rm a b c flags, "+", rm, [a,b,c], flags)
        | ("b64-",[a,b,c])    => (check_fsub rm a b c flags, "-", rm, [a,b,c], flags)
        | ("b64*",[a,b,c])    => (check_fmul rm a b c flags, "*", rm, [a,b,c], flags)
        | ("b64mul",[a,b,c])  => (check_fmul rm a b c flags, "*", rm, [a,b,c], flags)
        | ("b64/",[a,b,c])    => (check_fdiv rm a b c flags, "/", rm, [a,b,c], flags)
        | ("b64V",[a,b])      => (check_fsqrt rm a b flags, "V", rm, [a,b], flags)
        | ("b64*+",[a,b,c,d]) => (check_fmul_add rm a b c d flags, "*+", rm, [a,b,c,d], flags)
        | ("b64eq",[a,b,c])    => (check_feq a b c, "==", rm, [a,b,c], flags)
        | ("b64lt",[a,b,c])    => (check_flt a b c, "<", rm, [a,b,c], flags)
        | ("b64le",[a,b,c])    => (check_fle a b c, "<=", rm, [a,b,c], flags)
        | (_,_) => error("Unsupported op: " ^ f)

    end

    (* test parameters *)
    val filename = ref "";
    val verbose = ref false; (* print info for every test *)
    val progress_step = ref 100; 
    val save_to_file = ref false; 
    val with_flags = ref true; (* if test vectors have field for exception flags *)
    val silent = ref false; (* dont print info when running test*)
    val inverted = ref false; (* print passed tests instead of failed *)

    fun pstring f s = if !save_to_file then s else (f s)
    
    val failed = ref false;
    val failed_count = ref 0;
    val test_count = ref 0;
    val total_tests = ref 0;
    val cputimer = ref (Timer.startCPUTimer());

    fun stop_timer timer = 
    let
      val {nongc = {usr,...}, gc = {usr = gc,...}} = Timer.checkCPUTimes timer;
      val usr_s = "(" ^ Time.toString usr ^"s)      ";
      val gc_s = if Time.toReal usr > 0.000001 andalso
                    Time.toReal gc / Time.toReal usr > 0.20
                then
                  pstring testutils.boldred ("[GC = " ^ Time.toString gc ^ "] ")
                else ""
    in
        gc_s ^ usr_s
    end

    fun check_vector with_flags v = let

      fun fail v = (
        failed_count := !failed_count + 1;
        failed := true
      )
      fun op_string true _ (f1::f2::[]) (a::b::c::[]) ans _ flags = pstring testutils.boldgreen "OK " ^ "TEST " ^ Int.toString (!test_count + 1) ^ ": " ^ v ^ "\n\t" ^ a ^ " " ^ f1 ^ " " ^ b ^ " " ^ f2 ^ " " ^ c ^ " = " ^ ans ^ " " ^ flags ^ "\n"
        | op_string false result (f1::f2::[]) (a::b::c::[]) ans flag_result flags =  pstring testutils.boldred "FAILED " ^ "TEST " ^ Int.toString (!test_count + 1) ^ ": " ^ v ^ "\n\t" ^ a ^ " " ^ f1 ^ " " ^ b ^ " " ^ f2 ^ " " ^ c ^ " = " ^ ans ^ " " ^ flags ^ "\n\tGot: " ^ term_to_string result ^ " " ^ flag_result ^ "\n\n" 
        | op_string ret result (f::[]) (a::b::[]) ans flag_result flags = op_string ret result [f, ""] [a,b,""] ans flag_result flags
        | op_string ret result f (a::[]) ans flag_result flags = op_string ret result f [a,""] ans flag_result flags
        | op_string _ _ _ _ _ _ _ = error "op_string: unsupported case"

      fun print_test_info false _ _ _ _ _ _ _ = ()
        | print_test_info _ ret result "V" rm (a::b::[]) flag_result flags= print(op_string ret result [float_string a] ["sqrt"] (float_string b) flag_result flags)
        | print_test_info _ ret result "*+" rm (a::b::c::d::[]) flag_result flags= print(op_string ret result ["*", "+"] (map float_string [a,b,c]) (float_string d) flag_result flags)
        | print_test_info _ ret result (f as "==") rm (a::b::c::[]) flag_result flags = print(op_string ret result [f] (map float_string [a,b]) (term_to_string c) flag_result flags)
        | print_test_info _ ret result (f as "<") rm (a::b::c::[]) flag_result flags= print(op_string ret result [f] (map float_string [a,b]) (term_to_string c) flag_result flags)
        | print_test_info _ ret result (f as "<=") rm (a::b::c::[]) flag_result flags= print(op_string ret result [f] (map float_string [a,b]) (term_to_string c) flag_result flags)
        | print_test_info _ ret result f rm (a::b::c::[]) flag_result flags= print(op_string ret result [f] (map float_string [a,b]) (float_string c) flag_result flags)
        | print_test_info _ _ _ _ _ _ _ _= error "print_test_info: unsupported case"

      fun print_info true _ _ _ = print_test_info false false
        | print_info _ true _ true = print_test_info true true (* inverted + passed *)
        | print_info _ _ true passed = print_test_info true passed (* verbose + anything *)
        | print_info _ false _  false = print_test_info true false (* not inverted + fail *)
        | print_info _ _ _ _ = print_test_info false false
      
      val _ = 
      let
        val ((passed, result, flag_result), f, rm, args, flags) = check_vector_aux with_flags v
      in
        print_info (!silent) (!inverted) (!verbose) passed result f rm args ((String.concat o get_active_flags) flag_result) ((String.concat o get_active_flags) flags);
        (if passed then () else fail(v))
      end
      val _ = if !test_count mod (!progress_step) = 0 
              then
                print(pstring testutils.bold "Progress: " ^ Int.toString (!test_count) ^ "/" ^ Int.toString (!total_tests) ^ " " ^ stop_timer (!cputimer) ^ "\n")
              else ()

    in (test_count := !test_count + 1) end

    fun do_exit () =
      OS.Process.exit(if !failed then OS.Process.failure else OS.Process.success)
  end

  

  fun check_stdin () = case TextIO.inputLine TextIO.stdIn of
    NONE => ()
  | SOME s => (check_vector false s; check_stdin ())


    
  fun runtests "" _ =  (print(pstring testutils.bold "No tests to parse");do_exit ())
    | runtests file with_flags =
      let 
        val lines = TextIO.openIn file |> TextIO.inputAll |> String.tokens (fn c => c = #"\n");
      in
        cputimer := Timer.startCPUTimer();
        total_tests := length lines;
        (print("\nRunning " ^ Int.toString (!total_tests) ^ " tests from " ^ file ^ "...\n");
        map (check_vector with_flags) lines;
        print(pstring testutils.bold ("Ran " ^ Int.toString (!test_count) ^ " tests " ^ stop_timer (!cputimer) ^ "\n"));
        print(pstring testutils.boldred "Failed" ^ ": " ^ Int.toString (!failed_count) ^ "\n");
        print(pstring testutils.boldgreen "Passed" ^ ": " ^ Int.toString (!test_count - !failed_count) ^ "\n");
        do_exit ())
      end

  
  fun parse_extra [] = ()
    | parse_extra ("--testfile"::fname::rest) = (filename := fname; parse_extra rest)
    | parse_extra ("--verbose"::"true"::rest) = (verbose := true; parse_extra rest)
    | parse_extra ("--silent"::"true"::rest) = (silent := true; parse_extra rest)
    | parse_extra ("--inverted"::"true"::rest) = (inverted := true; parse_extra rest)
    | parse_extra ("--progress"::n::rest) = 
        (progress_step := (case (Int.fromString n) of 
                          NONE => 100
                          | SOME i => i)
                          ; parse_extra rest)
    | parse_extra ("--save"::"true"::rest) = (save_to_file := true; parse_extra rest)
    | parse_extra ("--flags"::"false"::rest) = (with_flags := false; parse_extra rest)
    | parse_extra (x::xs) = parse_extra xs

  fun main () = 
    let
      fun get_extra_args [] = []
        | get_extra_args ("--extra"::args::rest) = String.tokens (fn c=> c= #" ") args
        | get_extra_args (x::xs) = get_extra_args xs
    in
      (parse_extra o get_extra_args) (CommandLine.arguments());
      runtests (!filename) (!with_flags)
    end

  
end
