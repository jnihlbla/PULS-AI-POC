000100 01  W221L468.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT ERSÄTTNINGS                  
000400*                                 REGISTRET (ERSATT ARTIKEL) WDD7         
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAS-ART-ERSATTS-AV  VALUE +468.                                  
000800*                                 ANROPSTYP       KDCALL-W221-002         
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 FLJANEJ-ANROP        PIC X.                                       
001200      88 ANROP-OK            VALUE 'J'.                                   
001300      88 ANROP-FEL           VALUE 'N'.                                   
001400*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001500     03 IOAREA.                                                           
001600        05 FLTEXT            PIC X.                                       
001700         88 FLTEXT-JA        VALUE 'J'.                                   
001800         88 FLTEXT-NEJ       VALUE 'N'.                                   
001900*                                 FINNS TEXTINFORMATION ?                 
002000        05 IDARTNR-TILLK     PIC S9(9)           COMP-3.                  
002100*                                 TILLKOMMANDE ARTIKELNUMMER              
002200*** END COPY W221L468C0  LENGTH=14                                        
