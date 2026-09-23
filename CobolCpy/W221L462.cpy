000100 01  W221L462.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT LEVPLANREG                   
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-AVROP           VALUE +462.                                  
000700*                                 ANROPSTYP     KDCALL-W221               
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IDLEVNR              PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 IOAREA.                                                           
001700*                                                                         
001800        05 TIAVROP-AVS       PIC S9(5)           COMP-3.                  
001900*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002000*                                 (ÅÅVV)                                  
002100        05 KDAVROP           PIC S9              COMP-3.                  
002200*                                 AVROPSKOD                               
002300        05 KVAVROP           PIC S9(7)           COMP-3.                  
002400*                                 AVROPSKVANTITET                         
002500*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
