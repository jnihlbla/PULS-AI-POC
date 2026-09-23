000100 01  4319-WDGX4319.                                                       
000200*                                 PACKNING AVVIKELSER                     
000300*                                 FYSISKA NOLLOR                          
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + TIREGDAT +                    
000600*                                  LOWVALUE            )                  
000700     03 4319-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 4319-TIREGDAT        PIC S9(7)           COMP-3.                  
001000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001100*                                 REGISTRATION DATE (YYMMDD)              
001200     03 4319-LOWVALUE        PIC X(22).                                   
001300*** END COPY WDGX4319C0  LENGTH=30                                        
