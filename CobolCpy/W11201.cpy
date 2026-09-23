000100 01  W11201.                                                              
000200*                                 COPYTEXT FÖR FILEN W11201               
000300     03 W092W001.                                                         
000400        05 IDPTYP            PIC X(3).                                    
000500*                                 POSTTYP                                 
000600        05 IDDISTR           PIC S9(4).                                   
000700*                                 DISTRIKTNUMMER                          
000800        05 IDKUNDNR          PIC S9(6).                                   
000900*                                 KUNDNUMMER                              
001000        05 KDCLAGER          PIC S9.                                      
001100*                                 CENTRALLAGERKOD                         
001200        05 KDFRAKT           PIC 9(2).                                    
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400        05 KDFRAKT-X REDEFINES KDFRAKT                                    
001500                             PIC X(2).                                    
001600*                                 FRAKTSÄTT DC TILL KUND                  
001700        05 IDORDNR           PIC S9(5).                                   
001800*                                 ORDERNUMMER                             
001900        05 IDORDNR-X REDEFINES IDORDNR                                    
002000                             PIC X(5).                                    
002100*                                 ORDERNUMMER                             
002200        05 KDORDKL           PIC S9.                                      
002300*                                 ORDERKLASS                              
002400        05 KDSORT1 REDEFINES KDORDKL                                      
002500                             PIC S9.                                      
002600*                                 SORTERINGSKOD                           
002700        05 SORTBGP           PIC S9(8).                                   
002800*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
002900        05 SORTBGP-X REDEFINES SORTBGP                                    
003000                             PIC X(8).                                    
003100*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
003200        05 KDFELMRK          PIC S9.                                      
003300*                                 FELMARKERINGSKOD                        
003400        05 IDFELKODX         PIC X(3).                                    
003500*                                 FELKOD                                  
003600        05 FILLER2           PIC X(2).                                    
003700     03 FELTEXT              PIC X(80).                                   
003800*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
