000100 01  5-W155L035.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W15503         
000300     03 5-IDKATRAD           PIC S9(5)           COMP-3.                  
000400*                                 RADNUMMER                               
000500     03 5-TIUPPDAT           PIC S9(7)           COMP-3.                  
000600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
000700     03 5-KDRADST            PIC X.                                       
000800*                                 RAD-     L = LÅNAD.                     
000900*                                 STATUS   Ä = ÄNDRAD.                    
001000*                                          N = NYREGISTRERAD.             
001100*                                          C = L,Ä,N EFTER OMBRYT         
001200*                                              FRAM TILL VADGEN.          
001300*                                      SPACE = OFÖRÄNDRAD.                
001400     03 5-BERUBTEXT.                                                      
001500*                                                                         
001600        05 5-BERUBTEXT-1     PIC X(30).                                   
001700*                                 RUBRIKTEXT                              
001800        05 5-BERUBTEXT-2     PIC X(30).                                   
001900*                                 RUBRIKTEXT                              
002000     03 FILLER REDEFINES 5-BERUBTEXT.                                     
002100*                                                                         
002200        05 5-TEKOL           PIC X(25).                                   
002300*                                 KOLUMNTEXT                              
002400        05 FILLER            PIC X(35).                                   
002500     03 FILLER REDEFINES 5-BERUBTEXT.                                     
002600*                                                                         
002700        05 5-TEKATANM        PIC X(23).                                   
002800*                                 ANMÄRKNINGSTEXT                         
002900        05 FILLER            PIC X(37).                                   
003000*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
