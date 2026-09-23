000100 01  W221L213.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22121 MOT LEVERANSPLANEREG             
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-LEV-INFO        VALUE +301.                                  
000700      88 UPPDAT-LEV-INFO     VALUE +302.                                  
000800*                                 ANROPSTYP     KDCALL-W221               
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDARTNR              PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500     03 IDLEVNR              PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 IOAREA.                                                           
001800*                                                                         
001900        05 KVBR              PIC S9(7)           COMP-3.                  
002000*                                 BESTÄLLNINGSREST                        
002100        05 TILEVPL           PIC S9(7)           COMP-3.                  
002200*                                 LEVERANSPLANEDATUM  (ÅÅMMDD)            
002300*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
