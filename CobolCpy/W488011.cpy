000100 01  SALDO-W488011.                                                       
000200*                                 ANTAL SALDOPOSTER FRÅN PDP              
000300*                                 I DENNA ÖVERFÖRING                      
000400     03 FILLER               PIC X.                                       
000500     03 SALDO-IDPTYP-011     PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 FILLER               PIC X.                                       
000800     03 SALDO-KVSALDOPOST    PIC X(5).                                    
000900*                                 ANTAL SALDOPOSTER                       
001000     03 FILLER               PIC X(70).                                   
001100*** END COPY W488011CC0  LENGTH=80                                        
