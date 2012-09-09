TRANSFORMATION_RESULT
    START x = 3; P1
    P1 y = 1; P2
    P2 t_1 = x + 5; P2_0
    P2_0 Pos(y < t_1) P3
    P2_0 Neg(y < t_1) P4
    P3 z = t_1; P4
    P4 v = t_1; END
