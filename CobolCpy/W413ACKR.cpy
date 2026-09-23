000100 01  ACKR-W413ACKR.                                                       
000200*                                 LÄNKAREA TILL W413ACKR -                
000300*                                 WOPS ACKORDSBEHANDLING                  
000400     03 ACKR-INDATA.                                                      
000500*                                 GRUPP MED INDATA                        
000600        05 ACKR-ADLAGOMR     PIC S9(3)           COMP-3.                  
000700*                                 LAGEROMRÅDE                             
000800        05 ACKR-KDARTHNT     PIC S9(7)           COMP-3.                  
000900*                                 HANTERINGSKOD                           
001000        05 ACKR-KDFDKRAV     PIC S9(3)           COMP-3.                  
001100*                                 TRANSPORTFÖRPACKNINGSKOD                
001200        05 ACKR-KVBEART-Q    PIC S9(7)           COMP-3.                  
001300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001400        05 ACKR-KVQPACK-1    PIC S9(5)           COMP-3.                  
001500*                                 ANTAL I Q1 FÖRPACKNING                  
001600     03 ACKR-UTDATA.                                                      
001700*                                 GRUPP MED UTDATA                        
001800        05 ACKR-KVHANTTI     PIC S9(7)           COMP-3.                  
001900*                                 HANTERINGSKOD TID                       
002000*** END COPY W413ACKRC0  LENGTH=19                                        
