000100 01  W092P001-CTX.                                                        
000200*                                 SORTERINGS-BEGREPP GEMENSAMT            
000300*                                 FÖR ALLA TRANSAR UR TRATTEN             
000400     03 IDPTYP-S             PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 W092P001-001-GRP.                                                 
000700        05 IDDISTR-S         PIC S9(5).                                   
000800*                                 DISTRIKTNUMMER                          
000900        05 IDKUNDNR-S        PIC S9(7).                                   
001000*                                 KUNDNUMMER                              
001100        05 KDCLAGER-S        PIC S9.                                      
001200*                                 CENTRALLAGERKOD                         
001300        05 KDFRAKT-S         PIC S9(3).                                   
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500        05 KDFRAKT-X-FILLER REDEFINES KDFRAKT-S.                          
001600           07 KDFRAKT-X      PIC X(2).                                    
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800           07 FILLER         PIC X.                                       
001900        05 IDORDNR-S         PIC S9(5).                                   
002000*                                 ORDERNUMMER UTGÅR PD90                  
002100        05 IDORDNR-X REDEFINES IDORDNR-S                                  
002200                             PIC X(5).                                    
002300*                                 ORDERNUMMER UTGÅR PD90                  
002400     03 W092P001-002-GRP.                                                 
002500        05 KDORDKL           PIC S9.                                      
002600*                                 ORDERKLASS                              
002700        05 KDSORT1 REDEFINES KDORDKL                                      
002800                             PIC S9.                                      
002900*                                 SORTERINGSKOD                           
003000        05 SORTBGP           PIC S9(8).                                   
003100*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
003200        05 KDFELMRK          PIC S9.                                      
003300*                                 FELMARKERINGSKOD                        
003400        05 IDFELKOD          PIC 9(3).                                    
003500*                                 FELKOD                                  
003600        05 IDFELKODX REDEFINES IDFELKOD                                   
003700                             PIC X(3).                                    
003800*                                 FELKOD                                  
003900        05 FILLER2           PIC X(2).                                    
004000*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
