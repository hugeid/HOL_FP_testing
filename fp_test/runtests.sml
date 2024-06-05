val _ = load "RealArith";
val _ = RealArith.verbose_level := 0;
val _ = emit_MESG := false;
val _ = load "FP_test_parser";

val _ = FP_test_parser.main();