000100 01  MID-W2I35201.                                                        
000200*                                 MID-COPYTEXT FÖR W2035200               
000300     03 MID-IDARTNR-IN       PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDPERSON-BUY-IN  PIC 9(3).                                    
000800*                                 PERSONKOD REFILLANSVARIG                
000900     03 MID-IDPERSON-BUY-UT  PIC X(3).                                    
001000*                                 PERSONKOD REFILLANSVARIG                
001100     03 MID-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MID-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MID-IDTYPE-IN        PIC X.                                       
001600     03 MID-IDTYPE-UT        PIC X(5).                                    
001700     03 MID-IDSTATUS-IN      PIC X.                                       
001800     03 MID-IDSTATUS-UT      PIC X(12).                                   
001900     03 MID-IDREFTYP-IN      PIC X.                                       
002000*                                 TYP AV REFILLORDER                      
002100     03 MID-IDREFTYP-UT      PIC X(5).                                    
002200     03 MID-INPUT.                                                        
002300        05 MID-KVPB-REF      OCCURS 4 TIMES                               
002400                             PIC X(7).                                    
002500*                                 PERIODBEHOV (PROGNOS)                   
002600        05 MID-KVPBREOI      OCCURS 4 TIMES                               
002700                             PIC X(7).                                    
002800*                                 PERIODBEHOV (PROGNOS)                   
002900        05 MID-IDDC-FROM     OCCURS 4 TIMES                               
003000                             PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200        05 MID-PURCHQTY      OCCURS 4 TIMES                               
003300                             PIC X(7).                                    
003400        05 MID-FLREFBEO      OCCURS 4 TIMES                               
003500                             PIC X.                                       
003600        05 MID-FLFLYG        OCCURS 4 TIMES                               
003700                             PIC X.                                       
003800*                                 FLYGARTIKEL                             
003900        05 MID-COMMENT       PIC X(30).                                   
004000*** END OF VILMAII-COPY LENGTH= 183 BYTES                                 
