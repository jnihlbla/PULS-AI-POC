000100 01  W221L469.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT LEVPLANREG                   
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-BESTREST        VALUE +469.                                  
000700*                                 ANROPSTYP       KDCALL-W221-002         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                                                         
001600        05 IDLEVNR           PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800        05 TILEVPL           PIC S9(7)           COMP-3.                  
001900*                                 LEVERANSPLANEDATUM  (ÅÅMMDD)            
002000*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
