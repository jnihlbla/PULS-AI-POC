000100 01  W221L470-CTX.                                                        
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT LEVPLANREG                   
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-LEV-INFO        VALUE +470.                                  
000700*                                 ANROPSTYP       KDCALL-W221-002         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IDLEVNR              PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 KDLPORS-TAB          OCCURS 3 TIMES                               
001700                             PIC S9(3)           COMP-3.                  
001800*                                 LEVERANSPLANEORSAK                      
001900*** END OF VILMAII-COPY LENGTH= 19 BYTES                                  
