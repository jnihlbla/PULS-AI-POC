000100 01  W221L223.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22122 MOT LEVPLANREG                   
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-AVROP          VALUE +301.                                  
000700      88 LAES-SATSORDERNR    VALUE +302.                                  
000800*                                 ANROPSTYP     KDCALL-W221               
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDARTNR              PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500     03 IDLEVNR              PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 TIAVROP-AVS          PIC S9(5)           COMP-3.                  
001800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
001900*                                 (ÅÅVV)                                  
002000     03 IOAREA.                                                           
002100*                                                                         
002200        05 KDAVROP           PIC S9              COMP-3.                  
002300*                                 AVROPSKOD                               
002400        05 KVAVROP           PIC S9(7)           COMP-3.                  
002500*                                 AVROPSKVANTITET                         
002600        05 IDORDNR           PIC S9(5)           COMP-3.                  
002700*                                 ORDERNUMMER UTGÅR PD90                  
002800*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
