000100 01  W221L472.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT MASTER (WDN6)                
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-MASTER-BEEMBLEM VALUE +472.                                  
000700*                                 ANROPSTYP       KDCALL-W221-002         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                 EMBLEM                                  
001600*                                                                         
001700        05 KAT-BEEMBLEM      OCCURS 12 TIMES                              
001800                             PIC X(5).                                    
001900*                                 EMBLEM                                  
002000*** END COPY W221L472C0  LENGTH=68                                        
