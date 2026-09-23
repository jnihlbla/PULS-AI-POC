000100 01  W221L465.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT SATSSTRUKTUREN WDJ1          
000400*                                 OCH ARTIKELREGISTRET WDK6               
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAS-ART-INGAR-I     VALUE +465.                                  
000800*                                 ANROPSTYP       KDCALL-W221-002         
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 FLJANEJ-ANROP        PIC X.                                       
001200      88 ANROP-OK            VALUE 'J'.                                   
001300      88 ANROP-FEL           VALUE 'N'.                                   
001400*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001500     03 IOAREA.                                                           
001600        05 IDARTNR-SATS      PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER FÖR SATS                  
001800        05 KDERS             PIC S9(3)           COMP-3.                  
001900*                                 ERSÄTTNINGSKOD                          
002000        05 REANTPSA          PIC S9(2)V9(3)      COMP-3.                  
002100*                                 ANTAL PER SATS                          
002200        05 IDLEVNR           PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 CLAGER            OCCURS 2 TIMES.                              
002500           07 KVPB-SEP       PIC S9(6)V9(1)      COMP-3.                  
002600*                                 SEPARAT PERIODBEHOV                     
002700*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
