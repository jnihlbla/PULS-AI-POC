000100 01  4438-WDGX4438.                                                       
000200*                                 BESKRIVNING AV ARBETSTIDER              
000300*                                 FYSISK NYCKEL: DADATUM                  
000400     03 4438-DADATUM         PIC 9(8).                                    
000500*                                 DATUM ENLIGT KDDATFORM                  
000600*                                 DATE AS SPECIFIED BY KDDATFORM          
000700     03 4438-TISTAMIN-PAC    PIC S9(5)           COMP-3.                  
000800*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
000900*                                 WORKINGDAYS BEGINNING (PICKING)         
001000     03 4438-TISTOMIN-PAC    PIC S9(5)           COMP-3.                  
001100*                                 ARBETSDAGENS SLUT (PACKNING)            
001200*                                 WORKINGDAYS END (PICKING)               
001300     03 4438-TISTAMIN-ADM    PIC S9(5)           COMP-3.                  
001400*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
001500*                                 WORKINGDAYS BEGINNING(ORDEROFF)         
001600     03 4438-TISTOMIN-ADM    PIC S9(5)           COMP-3.                  
001700*                                 ARBETSDAGENS SLUT (ORDERKONT)           
001800*                                 WORKINGDAYS END (ORDEROFFICE)           
001900     03 4438-TISTAMIN-LAST   PIC S9(5)           COMP-3.                  
002000*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
002100*                                 WORKINGDAYS BEGINNING (LOADING)         
002200     03 4438-TISTOMIN-LAST   PIC S9(5)           COMP-3.                  
002300*                                 ARBETSDAGENS SLUT (LASTNING)            
002400*                                 WORKINGDAYS END (LOADING)               
002500*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
