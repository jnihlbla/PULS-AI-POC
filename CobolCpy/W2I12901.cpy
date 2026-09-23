000100 01  W2I12901.                                                            
000200     03 IDARTNR-IN           PIC X(9).                                    
000300*                                 ARTIKELNUMMER                           
000400     03 IDARTNR-UT           PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 INFALT.                                                           
000700        05 C2-FAELT-SW       PIC X.                                       
000800*                                 FLAGGA OM C2 FINNS                      
000900        05 FLSKRSP-C1-VISN   PIC X.                                       
001000*                                 SKROTNING BEORDRAD AV ANSK              
001100        05 TISKROT-AUTO-IN   PIC 9(6).                                    
001200*                                 STOPDATE AUTO-SKROTNING                 
001300        05 KDERS             PIC X(2).                                    
001400*                                 ERSÄTTNINGSKOD                          
001500        05 KDCLAGER          PIC X.                                       
001600*                                 CENTRALLAGERKOD                         
001700        05 KVKVAR            PIC X(7).                                    
001800*                                 KVARLIGGANDE ANTAL                      
001900        05 IDKONTO           PIC X(10).                                   
002000*                                 KONTO                                   
002100        05 IDANALYS          PIC X(12).                                   
002200*                                 ANALYSNUMMER                            
002300        05 FLSKROT-CLASS     PIC X.                                       
002400*                                 SKROTNINGEN TILL CLASSIC                
002500        05 BELAGINS30        PIC X(30).                                   
002600*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
