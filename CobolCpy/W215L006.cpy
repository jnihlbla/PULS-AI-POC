000100 01  W215L006.                                                            
000200*                                 LÄNKAREA I W215 MOT LEVPLAN,            
000300*                                 SATSENS BEORDR. DATUM                   
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-SATSBEORDR-DATUM                                            
000700                             VALUE +16.                                   
000800      88 UPPDAT-SATSBEORDR-DATUM                                          
000900                             VALUE +17.                                   
001000      88 NYUPPL-SATSBEORDR-DATUM                                          
001100                             VALUE +18.                                   
001200     03 FLJANEJ-ANROP        PIC X.                                       
001300      88 ANROP-OK            VALUE 'J'.                                   
001400      88 ANROP-FEL           VALUE 'N'.                                   
001500*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001600     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER FÖR SATS                  
001800     03 IDLEVNR              PIC S9(5)           COMP-3.                  
001900*                                 LEVERANTÖRNUMMER                        
002000     03 TIAVROP-AVS          PIC S9(5)           COMP-3.                  
002100*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002200*                                 (ÅÅVV)                                  
002300     03 KDAVROP              PIC S9              COMP-3.                  
002400*                                 AVROPSKOD                               
002500     03 IOAREA.                                                           
002600*                                                                         
002700        05 IDORDNR           PIC S9(5)           COMP-3.                  
002800*                                 ORDERNUMMER                             
002900        05 TIBEODAT-SATS     PIC S9(5)           COMP-3.                  
003000*                                 BEORDRINGSDATUM SATS (ÅÅVV)             
003100*** END COPY W215L006C0  LENGTH=21                                        
