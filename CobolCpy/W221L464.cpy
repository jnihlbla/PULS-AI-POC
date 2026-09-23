000100 01  W221L464.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT SATSSTRUKTUREN WDJ1          
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ART-BESTAR-AV   VALUE +464.                                  
000700*                                 ANROPSTYP       KDCALL-W221-002         
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 FLJANEJ-ANROP        PIC X.                                       
001100      88 ANROP-OK            VALUE 'J'.                                   
001200      88 ANROP-FEL           VALUE 'N'.                                   
001300*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001400     03 IOAREA.                                                           
001500        05 IDARTNR-ING       PIC S9(9)           COMP-3.                  
001600*                                 INGÅENDE ARTIKELNUMMER                  
001700        05 KDISATS           PIC X.                                       
001800*                                 STATUSKOD I SATS                        
001900        05 REANTPSA          PIC S9(2)V9(3)      COMP-3.                  
002000*                                 ANTAL PER SATS                          
002100*** END COPY W221L464C0  LENGTH=17                                        
