000100 01  MID-W2I39201.                                                        
000200*                                 MID-COPYTEXT FÖR W2039200               
000300     03 MID-IDARTNR-IN       PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-REF-IN      PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-REF-UT      PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDTYPE-IN        PIC X.                                       
001200     03 MID-IDTYPE-UT        PIC X(5).                                    
001300     03 MID-IDPERSON-BUY-IN  PIC 9(3).                                    
001400*                                 PERSONKOD REFILLANSVARIG                
001500     03 MID-IDPERSON-BUY-UT  PIC X(3).                                    
001600*                                 PERSONKOD REFILLANSVARIG                
001700     03 MID-IDSTATUS-IN      PIC X.                                       
001800     03 MID-IDSTATUS-UT      PIC X(12).                                   
001900     03 MID-IDREFTYP-IN      PIC X.                                       
002000*                                 TYP AV REFILLORDER                      
002100     03 MID-IDREFTYP-UT      PIC X(5).                                    
002200     03 MID-INPUT.                                                        
002300        05 MID-KVPB-SEP      PIC X(8).                                    
002400*                                 PERIODBEHOV (PROGNOS)                   
002500        05 MID-FLAGGA-FCD    PIC X.                                       
002600        05 MID-FLREFBEO      PIC X.                                       
002700        05 MID-COMMENT-1     PIC X(36).                                   
002800        05 MID-PURCHQTY      PIC 9(7).                                    
002900        05 MID-COMMENT-2     PIC X(36).                                   
003000*** END OF VILMAII-COPY LENGTH= 142 BYTES                                 
