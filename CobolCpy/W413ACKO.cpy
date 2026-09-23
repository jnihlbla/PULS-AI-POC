000100 01  ACKO-W413ACKO.                                                       
000200*                                 LÄNKAREA TILL W413ACKO -                
000300*                                 WOPS ACKORDSBEHANDLING                  
000400     03 ACKO-INDATA.                                                      
000500*                                 GRUPP MED INDATA                        
000600        05 ACKO-ADLAGOMR     PIC S9(3)           COMP-3.                  
000700*                                 LAGEROMRÅDE                             
000800        05 ACKO-FOERRA-ADLAGOMR                                           
000900                             PIC S9(3)           COMP-3.                  
001000*                                 LAGEROMRÅDE                             
001100        05 ACKO-NAESTA-ADLAGOMR                                           
001200                             PIC S9(3)           COMP-3.                  
001300*                                 LAGEROMRÅDE                             
001400        05 ACKO-IDDISTR      PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600        05 ACKO-KDFDKRAV     PIC S9(3)           COMP-3.                  
001700*                                 TRANSPORTFÖRPACKNINGSKOD                
001800        05 ACKO-KVRADER      PIC S9(5)           COMP-3.                  
001900*                                 ANTAL RADER                             
002000        05 ACKO-FOERRA-KVRADER                                            
002100                             PIC S9(5)           COMP-3.                  
002200*                                 ANTAL RADER                             
002300        05 ACKO-SUHANTTI     PIC S9(7)           COMP-3.                  
002400*                                 SUMMA HANTERINGSKOD TID                 
002500     03 ACKO-UTDATA.                                                      
002600*                                 GRUPP MED UTDATA                        
002700        05 ACKO-TIHANTTI     PIC S9(5)V9(2)      COMP-3.                  
002800*                                 HANTERINGSKOD TID                       
002900*** END COPY W413ACKOC0  LENGTH=25                                        
