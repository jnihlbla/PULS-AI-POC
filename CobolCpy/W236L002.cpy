000100 01  W236L002.                                                            
000200*                                 COPYTEXT FÖR LÄSNING AV AVROP           
000300*                                 OCH INLEVERANS.                         
000400     03 KDCALL               PIC S9(3)           COMP-3.                  
000500      88 LAS-AVROP           VALUE +103.                                  
000600      88 LAS-INLEVERANS      VALUE +104.                                  
000700     03 KDSVAR               PIC X.                                       
000800      88 AVROP-FINNS         VALUE ' '.                                   
000900      88 INLEVERANS-FINNS    VALUE ' '.                                   
001000*                                 SVAR FRÅN SUBPROGRAM                    
001100     03 IO-AREA.                                                          
001200*                                 AVROP-AREA  WDD905                      
001300        05 KDAVROP           PIC S9              COMP-3.                  
001400*                                 AVROPSKOD                               
001500        05 TIAVROP-AVS       PIC S9(5)           COMP-3.                  
001600*                                 AVSÄNDNINGSVECKA (PLANERAD)             
001700*                                 (ÅÅVV)                                  
001800        05 TIAVROP-INL       PIC S9(5)           COMP-3.                  
001900*                                 INLEVERANSDATUM (PLANERAD)              
002000*                                 (ÅÅVV)                                  
002100        05 KVAVROP           PIC S9(7)           COMP-3.                  
002200*                                 AVROPSKVANTITET                         
002300        05 IDLOPNRM-PL       PIC S9(9)           COMP-3.                  
002400*                                 AVBOKNINGSID, (ÅÅVVDLLLL)               
002500        05 KVAVROP-AVB       PIC S9(7)           COMP-3.                  
002600*                                 AVBOKAT ANTAL                           
002700        05 TID               PIC S9              COMP-3.                  
002800*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
002900*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
