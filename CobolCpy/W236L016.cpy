000100 01  W236L016.                                                            
000200*                                 COPYTEXT FÖR LÄSNING AV AVROP           
000300*                                 OCH INLEVERANS.  KOMM. MELLAN           
000400*                                 W2362000 W2362010.                      
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-AVROP           VALUE +103.                                  
000700      88 LAS-INLEVERANS      VALUE +104.                                  
000800     03 KDSVAR               PIC X.                                       
000900      88 AVROP-FINNS         VALUE ' '.                                   
001000      88 INLEVERANS-FINNS    VALUE ' '.                                   
001100*                                 SVAR FRÅN SUBPROGRAM                    
001200     03 IO-AREA.                                                          
001300*                                 AVROP-AREA  WDD905                      
001400        05 KDAVROP           PIC S9              COMP-3.                  
001500*                                 AVROPSKOD                               
001600        05 TIAVROP-AVS       PIC S9(5)           COMP-3.                  
001700*                                 AVSÄNDNINGSVECKA (PLANERAD)             
001800*                                 (ÅÅVV)                                  
001900        05 TIAVROP-INL       PIC S9(5)           COMP-3.                  
002000*                                 INLEVERANSDATUM (PLANERAD)              
002100*                                 (ÅÅVV)                                  
002200        05 KVAVROP           PIC S9(7)           COMP-3.                  
002300*                                 AVROPSKVANTITET                         
002400        05 IDLOPNRM-PL       PIC S9(9)           COMP-3.                  
002500*                                 AVBOKNINGSID, (ÅÅVVDLLLL)               
002600        05 KVAVROP-AVB       PIC S9(7)           COMP-3.                  
002700*                                 AVBOKAT ANTAL                           
002800*** END COPY W236L016C0  LENGTH=23                                        
