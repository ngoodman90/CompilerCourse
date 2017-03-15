/* scheme/write_sob_void.asm
 * 
 * Programmer: Ibraheem Ibraheem, 2017
 */

WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  MOV(R0, FPARG(0)); // take the symbol object (T_symbol , pointerToString)
  MOV(R0,INDD(R0,1));  // take the pointerToString
  MOV(R1, INDD(R0, 1)); //length of the string
  MOV(R2, R0); // R2 have the string object (T_string, len , ch1 , ch2 ...)
  ADD(R2, IMM(2)); // R2 now have the charchters (ch1 , ch2 , ch3 , ...)
L_symbol_print_loop:
  CMP(R1, IMM(0)); 
  JUMP_EQ(L_sym_exit);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  INCR(R2);
  DECR(R1);
  JUMP(L_symbol_print_loop);
L_sym_exit:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
