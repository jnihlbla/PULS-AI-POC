000100 01  MEAN-W418MEAN.                                                       
000200*                                 LÄNKAREA TILL W418MEAN   -              
000300*                                 SKAPAR MEMO TILL LEVERANS-              
000400*                                 ANMÄRKNINGSANSVARIG                     
000500     03 MEAN-IDMAIL          PIC X(60).                                   
000600*                                 MAIL ADRESS                             
000700     03 MEAN-RAD             OCCURS 13 TIMES.                             
000800        05 MEAN-IDDISTR      PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 MEAN-IDKUNDNR     PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 MEAN-IDRAPPNR     PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400        05 MEAN-IDARTNR      PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600        05 MEAN-IDRADNR      PIC S9(5)           COMP-3.                  
001700*                                 RADNUMMER                               
001800        05 MEAN-KDKREBEH     PIC X(3).                                    
001900*                                 BEHANDLINGSSTATUS                       
002000        05 MEAN-TEANMNOT-ADM-GRP.                                         
002100           07 MEAN-TEANMNOT-ADM                                           
002200                             OCCURS 3 TIMES                               
002300                             PIC X(70).                                   
002400*                                 FRI TEXT FRÅN ADMINISTRATION            
002500        05 MEAN-TEANMNOT-REM-GRP.                                         
002600           07 MEAN-TEANMNOT-REM                                           
002700                             OCCURS 3 TIMES                               
002800                             PIC X(70).                                   
002900*                                 FRI TEXT FRÅN REMISSINSTANS             
003000*** END OF VILMAII-COPY LENGTH= 5845 BYTES                                
