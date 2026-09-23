000100 01  W215L005.                                                            
000200*                                 LÄNKAREA I W215 MOT LEVPLAN,            
000300*                                 AVROP FOR SATS                          
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-FORST-LEVPLAN-AVRO                                          
000700                             VALUE +12.                                   
000800      88 LAES-NASTA-LEVPLAN-AVRO                                          
000900                             VALUE +13.                                   
001000      88 BORTTAG-LEVPLAN-AVROP                                            
001100                             VALUE +14.                                   
001200      88 UPPDATERA-LEVPLAN-AVROP                                          
001300                             VALUE +15.                                   
001400     03 FLJANEJ-ANROP        PIC X.                                       
001500      88 ANROP-OK            VALUE 'J'.                                   
001600      88 ANROP-FEL           VALUE 'N'.                                   
001700*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001800     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER FÖR SATS                  
002000     03 IDLEVNR              PIC S9(5)           COMP-3.                  
002100*                                 LEVERANTÖRNUMMER                        
002200     03 IOAREA.                                                           
002300*                                                                         
002400        05 TIAVROP-AVS       PIC S9(5)           COMP-3.                  
002500*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002600*                                 (ÅÅVV)                                  
002700        05 TIAVROP-DISP      PIC S9(5)           COMP-3.                  
002800*                                 DISPONIBELVECKA  (PLANERAD)             
002900*                                 (ÅÅVV)                                  
003000        05 KDAVROP           PIC S9              COMP-3.                  
003100*                                 AVROPSKOD                               
003200        05 KVAVROP           PIC S9(7)           COMP-3.                  
003300*                                 AVROPSKVANTITET                         
003400*** END COPY W215L005C0  LENGTH=22                                        
