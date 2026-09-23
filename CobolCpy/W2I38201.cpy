000100 01  MID-W2I38201.                                                        
000200*                                 MID-COPYTEXT FÖR W2038200               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDTYPE-IN        PIC X.                                       
001200     03 MID-IDTYPE-UT        PIC X(5).                                    
001300     03 MID-IDSTATUS-IN      PIC X.                                       
001400     03 MID-IDSTATUS-UT      PIC X(12).                                   
001500     03 MID-IDREFTYP-IN      PIC X.                                       
001600*                                 TYP AV REFILLORDER                      
001700     03 MID-IDREFTYP-UT      PIC X(5).                                    
001800     03 MID-INPUT.                                                        
001900        05 MID-KVPB-REF      OCCURS 6 TIMES                               
002000                             PIC X(7).                                    
002100*                                 PERIODBEHOV (PROGNOS)                   
002200        05 MID-FLREFBEO      OCCURS 6 TIMES                               
002300                             PIC X.                                       
002400        05 MID-PURCHQTY      OCCURS 6 TIMES                               
002500                             PIC X(7).                                    
002600        05 MID-COMMENT       OCCURS 2 TIMES                               
002700                             PIC X(15).                                   
002800        05 MID-IDDC-FROM     OCCURS 6 TIMES                               
002900                             PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100*** END OF VILMAII-COPY LENGTH= 179 BYTES                                 
