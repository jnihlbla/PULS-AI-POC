000100 01  W213L323.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21332 MOT LEVERANSPLANREG              
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ART-INFO-PLAN  VALUE +301.                                  
000700      88 LAES-LEV-INFO-PLAN  VALUE +302.                                  
000800      88 DELETE-SEGM3-4-PLAN VALUE +303.                                  
000900      88 LAES-AVROP          VALUE +304.                                  
001000      88 DELETE-AVROP        VALUE +305.                                  
001100      88 REPLACE-AVROP       VALUE +306.                                  
001200      88 LAES-SATSORDERNR    VALUE +307.                                  
001300*                                 ANROPSTYP FÖR SYSTEM R2XX               
001400     03 FLJANEJ-ANROP        PIC X.                                       
001500      88 ANROP-OK            VALUE 'J'.                                   
001600      88 ANROP-FEL           VALUE 'N'.                                   
001700*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 IDLEVNR              PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 KDAVROP              PIC S9              COMP-3.                  
002300*                                 AVROPSKOD                               
002400     03 DAAVROP-AVS          PIC 9(6).                                    
002500*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002600*                                 (ÅÅÅÅVV)                                
002700     03 TILEVDAG             PIC S9              COMP-3.                  
002800*                                 AVSÄNDNINGSDAG INOM VECKA               
002900     03 IDORDNR              PIC S9(5)           COMP-3.                  
003000*                                 ORDERNUMMER UTGÅR PD90                  
003100     03 IOAREA.                                                           
003200*                                                                         
003300        05 KVBR              PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLNINGSREST                        
003500        05 KVAVROP           PIC S9(7)           COMP-3.                  
003600*                                 AVROPSKVANTITET                         
003700*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
