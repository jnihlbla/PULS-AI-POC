000100 01  4804-WDGX4804.                                                       
000200*                                 4804   DELSALDOÖVERFÖRING               
000300*                                                                         
000400*                                 SEGMENTET INNEHÅLLER INFO               
000500*                                 OM DE 4 SENASTE ÖVER-                   
000600*                                 FÖRINGARNA. INDEX = 1 ÄR                
000700*                                 DEN MEST AKTUELLA                       
000800     03 4804-KDSEGKEY        PIC X.                                       
000900*                                 TEKNISK SEGMENT-NYCKEL                  
001000     03 4804-RAD             OCCURS 4 TIMES.                              
001100*                                                                         
001200        05 4804-TIREGDAT-START                                            
001300                             PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500        05 4804-TIUPPTID-START                                            
001600                             PIC S9(9)           COMP-3.                  
001700*                                 UPPDATERINGSTID  (TTMMSSTH)             
001800        05 4804-TIUPPTID-KLAR                                             
001900                             PIC S9(9)           COMP-3.                  
002000*                                 UPPDATERINGSTID  (TTMMSSTH)             
002100        05 4804-KVSALDOPOST-PDP                                           
002200                             PIC S9(5)           COMP-3.                  
002300*                                 ANTAL SALDOPOSTER                       
002400        05 4804-KVSALDOPOST-IBM                                           
002500                             PIC S9(5)           COMP-3.                  
002600*                                 ANTAL SALDOPOSTER                       
002700        05 4804-KVSALDOPOST-FEL                                           
002800                             PIC S9(5)           COMP-3.                  
002900*                                 ANTAL SALDOPOSTER                       
003000        05 4804-KDTRSTAT     PIC S9              COMP-3.                  
003100*                                 TRANSAKTIONSSTATUS                      
003200     03 4804-KVSALDOPOST-FELTOTAL                                         
003300                             PIC S9(5)           COMP-3.                  
003400*                                 ANTAL SALDOPOSTER                       
003500*** END COPY WDGX4804C0  LENGTH=100                                       
