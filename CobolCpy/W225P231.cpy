000100 01  W225P231.                                                            
000200*                                 POSTTYP 231 RESTORDERINFO               
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 KDCLPOST             PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERPOST                        
001000     03 CDC-INFO.                                                         
001100        05 KVROS-CDC-1-2     PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERSALDO                          
001300*                                 ORDERKLASS 1 OCH 2                      
001400        05 KVROS-CDC-3-4     PIC S9(7)           COMP-3.                  
001500*                                 RESTORDERSALDO                          
001600*                                 ORDERKLASS 3 - 4                        
001700        05 TIRODAT-ORDER-CDC PIC S9(5)           COMP-3.                  
001800*                                               TIRODAT-ORDER-002         
001900*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
002000        05 KVRORAD-KVAR-V1-CDC                                            
002100                             PIC S9(7)           COMP-3.                  
002200*                                 KVARVARANDE RESTORDERRADER              
002300*                                 VECKA 1           (KVRORAD-002)         
002400        05 KVRORAD-KVAR-IV-CDC                                            
002500                             PIC S9(7)           COMP-3.                  
002600*                                 KVARVARANDE RESTORDERRADER              
002700*                                 INNEVARANDE VECKA (KVRORAD-002)         
002800        05 KVRORAD-KVAR-P-CDC                                             
002900                             PIC S9(7)           COMP-3.                  
003000*                                 KVARVARANDE RESTORDERRADER              
003100*                                 UNDER PERIODEN    (KVRORAD-002)         
003200        05 KVRORAD-TOT-CDC   PIC S9(7)           COMP-3.                  
003300*                                                   (KVRORAD-002)         
003400*                                 TOTALT ANTAL RESTORDERRADER             
003500*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
