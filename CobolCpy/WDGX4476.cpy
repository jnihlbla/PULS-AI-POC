000100 01  4476-WDGX4476.                                                       
000200*                                 ≈TERSTARTS REGISTER PD90                
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4476-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4476-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4476-IDPRC.                                                       
001200*                                 PRODUKTIONSKANAL                        
001300*                                 PRODUCTION CHANNEL                      
001400        05 4476-IDPRCBAS     PIC X(3).                                    
001500*                                 PRC-BAS                                 
001600*                                 PRC-BASIC                               
001700        05 4476-IDPRCVAR     PIC X.                                       
001800*                                 PRC-VARIANT                             
001900*                                 PRC-VARIANT                             
002000     03 4476-TIUPPDAT        PIC S9(7)           COMP-3.                  
002100*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
002200*                                 UPDATING DATE     (YYMMDD)              
002300     03 4476-TIUPPTID        PIC S9(9)           COMP-3.                  
002400*                                 UPPDATERINGSTID  (TTMMSSTH)             
002500*                                 UPDATING TIME    (HHMMSSTH)             
002600     03 FILLER               PIC X(64).                                   
002700*** END COPY WDGX4476    LENGTH=80                                        
