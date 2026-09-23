000100 01  W221L215.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22121 MOT WDGX                         
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ROT-2215        VALUE +501.                                  
000700      88 LAS-LEVNR           VALUE +502.                                  
000800*                                 ANROPSTYP       KDCALL-W221-003         
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 IOAREA.                                                           
001600*                                                                         
001700        05 KVANTEX           PIC S9              COMP-3.                  
001800*                                 ANTAL EXEMPLAR                          
001900        05 KDEDI             PIC X.                                       
002000*                                 ÖVERFÖRINGSSTANDARD                     
002100*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
