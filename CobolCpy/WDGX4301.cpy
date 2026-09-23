000100 01  4301-WDGX4301.                                                       
000200*                                 4301 LÅSNINGSREGISTER                   
000300*                                 NYCKEL: WDGXKEY                         
000400*                                         (IDHTYP, IDPRODNR,              
000500*                                          LOWVALUE)                      
000600     03 4301-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4301-IDPRODNR        PIC S9(7)           COMP-3.                  
000900*                                 PRODUKTIONSNUMMER                       
001000     03 4301-LOWVALUE        PIC X(22).                                   
001100*** END COPY WDGX4301    LENGTH=30                                        
