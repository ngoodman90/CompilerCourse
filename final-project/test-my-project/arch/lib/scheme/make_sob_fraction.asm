gcd_make:
PUSH(FP);
MOV(FP,SP);
PUSH(R1);
PUSH(R2);
PUSH(R3);
MOV(R1,FPARG(0));
MOV(R2,FPARG(1));
XOR(R3,R3);
lbl_gcd_loop:
  CMP(R1,0);
  JUMP_EQ(gc_finish_lbl);
  MOV(R3,R1);
  REM(R2,R1);
  MOV(R1,R2);
  MOV(R2,R3); 
  JUMP(lbl_gcd_loop);
gc_finish_lbl:
MOV(R0,R2);
POP(R3);
POP(R2);
POP(R1);
POP(FP);
RETURN;


MAKE_SOB_FRACTION:
    PUSH(FP);
    MOV(FP,SP);
    PUSH(FPARG(1));
    PUSH(FPARG(0));
    CALL(gcd_make);
    DROP(2);
    MOV(R1,FPARG(0));
    MOV(R2,FPARG(1));
    DIV(R1,R0);
    DIV(R2,R0);
    CMP(R2,IMM(0));
    JUMP_LT(lbl_Negative); 
    cont:
    CMP(R1,IMM(0));
    JUMP_EQ(bl_Zero_maker);
    CMP(R2,IMM(1)); //if we ge that the r2 1 then no need for fraction.. it's integer
    JUMP_NE(lbl_Fraction_maker);
    PUSH(R1);
    CALL(MAKE_SOB_INTEGER);
    DROP(1);
    POP(FP);
    RETURN;
    bl_Zero_maker:
	PUSH(R1);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(FP);
	RETURN;
    lbl_Fraction_maker:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0),T_FRACTION);
	MOV(INDD(R0,1),R1);
	MOV(INDD(R0,2),R2);
	POP(FP);
	RETURN;
    lbl_Negative:
	MUL(R1,-1);
	MUL(R2,-1);
	JUMP(cont); 





