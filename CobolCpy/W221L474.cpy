000100 01  W221L474.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT BYTES  (WDA8)                
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-BYTES-KVRETUR   VALUE +474.                                  
000700*                                 ANROPSTYP       KDCALL-W221-003         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 KVRETUR              PIC S9(7)           COMP-3.                  
001500*                                 ANTAL I RETUR                           
001600*** END COPY W221L474C0  LENGTH=12                                        
