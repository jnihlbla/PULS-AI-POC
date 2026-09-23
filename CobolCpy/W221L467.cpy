000100 01  W221L467.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT ERSÄTTNINGS                  
000400*                                 REGISTRET (TILLKOMMANDE                 
000500*                                 ARTIKEL) WDD8                           
000600*                                                                         
000700     03 KDCALL               PIC S9(3)           COMP-3.                  
000800      88 LAS-ART-ERSATTER    VALUE +467.                                  
000900*                                 ANROPSTYP       KDCALL-W221-002         
001000     03 IDARTNR-TILLK        PIC S9(9)           COMP-3.                  
001100*                                 TILLKOMMANDE ARTIKELNUMMER              
001200     03 FLJANEJ-ANROP        PIC X.                                       
001300      88 ANROP-OK            VALUE 'J'.                                   
001400      88 ANROP-FEL           VALUE 'N'.                                   
001500*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001600     03 IOAREA.                                                           
001700        05 IDARTNR           PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900*** END COPY W221L467C0  LENGTH=13                                        
