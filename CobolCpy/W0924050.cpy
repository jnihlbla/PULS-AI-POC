000100 01  W0924050.                                                            
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 IDPTYP-S             PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDORDER-S.                                                        
000700        05 IDDISTR-S         PIC S9(4).                                   
000800*                                 DISTRIKTNUMMER                          
000900        05 IDKUNDNR-S        PIC S9(6).                                   
001000*                                 KUNDNUMMER                              
001100        05 KDCLAGER-S        PIC S9.                                      
001200*                                 CENTRALLAGERKOD                         
001300        05 KDFRAKT-S         PIC S9(2).                                   
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500        05 KDFRAKT-X REDEFINES KDFRAKT-S                                  
001600                             PIC X(2).                                    
001700*                                 FRAKTSÄTT C1-C2 TILL KUND               
001800        05 IDORDNR-S         PIC S9(5).                                   
001900*                                 ORDERNUMMER                             
002000        05 IDORDNR-X REDEFINES IDORDNR-S                                  
002100                             PIC X(5).                                    
002200*                                 ORDERNUMMER                             
002300     03 PDATA.                                                            
002400        05 KDORDKL           PIC S9.                                      
002500*                                 ORDERKLASS                              
002600        05 SORTBGP           PIC S9(8).                                   
002700*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
002800        05 KDFELMRK          PIC S9.                                      
002900*                                 FELMARKERINGSKOD                        
003000        05 IDFELKOD          PIC S9(3).                                   
003100*                                 FELKOD                                  
003200        05 IDFELKODX REDEFINES IDFELKOD                                   
003300                             PIC X(3).                                    
003400*                                 FELKOD                                  
003500        05 FILLER2           PIC X(2).                                    
003600        05 KORTBILD          PIC X(80).                                   
003700*** END COPY W0924050C0  LENGTH=119                                       
