000100 01  W221L212.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22121 MOT LEVERANSPLANEREG             
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-OMSPEC-INFO     VALUE +201.                                  
000700      88 BORTTAG-OMSPEC-INFO VALUE +202.                                  
000800      88 UPPDAT-OMSPEC-INFO  VALUE +203.                                  
000900*                                 ANROPSTYP     KDCALL-W221               
001000     03 FLJANEJ-ANROP        PIC X.                                       
001100      88 ANROP-OK            VALUE 'J'.                                   
001200      88 ANROP-FEL           VALUE 'N'.                                   
001300*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001400     03 IDARTNR              PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600     03 IDLEVNR              PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 IOAREA.                                                           
001900*                                                                         
002000        05 KDLPORS-TAB       OCCURS 3 TIMES                               
002100                             PIC S9(3)           COMP-3.                  
002200*                                 LEVERANSPLANEORSAK                      
002300        05 KVBEST-PL         PIC S9(7)           COMP-3.                  
002400*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
002500        05 KDPLKOEP          PIC S9              COMP-3.                  
002600*                                 STATUS AVTALSKÖP (PLAN)                 
002700*                                 1=FÖRESLAGEN  2=GODKÄND                 
002800*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
