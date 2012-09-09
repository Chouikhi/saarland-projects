TRANSFORMATION_RESULT
    START c = 0; P01
    P01 t_0 = a + b; P01_0
    P01_0 t = t_0; P02
    P02 Pos(t) P03
    P02 Neg(t) P06
    P03 a = 4; P04
    P04 t_0 = a + b; P04_0
    P04_0 x = t_0; P05
    P05 ; P07
    P06 z = t_0; P07
    P07 t_1 = p + q; P07_0
    P07_0 f = t_1; P08
    P08 z = t_0; P09
    P09 gg = t_1; P10
    P10 Pos(z) P08
    P10 Neg(z) P11
    P11 foo = bar; P12
    P12 baz = foo + bar; P13
    P13 Pos(baz) P11
    P13 Neg(baz) END
