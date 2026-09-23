000100 01  MID-W2I11101.                                                        
000200*                                 COPYTEXT FÖR MID W2I11101               
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-UT       PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDATTENT-ENTER   PIC 9(2).                                    
001200*                                 ATTENTION NUMMER                        
001300     03 MID-IDATTENT-NEXT    PIC 9(2).                                    
001400*                                 ATTENTION NUMMER                        
001500     03 MID-INPUT.                                                        
001600        05 MID-IDANSK-PG-IN  OCCURS 8 TIMES                               
001700                             PIC 9(3).                                    
001800*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
001900        05 MID-KVDAGAR-TTC1-IN                                            
002000                             PIC 9(2).                                    
002100*                                 DAGAR TULL- OCH TRANSPORT-TID           
002200*                                 C1                                      
002300        05 MID-DAG-POS-IN    OCCURS 5 TIMES                               
002400                             PIC X(2).                                    
002500        05 MID-KVDAGAR-AVIAVV-IN                                          
002600                             PIC X.                                       
002700*                                 TOLERANSAVVIKELSE FÖRAVISERING          
002800        05 MID-KVDAGAR-INLAVV-IN                                          
002900                             PIC X.                                       
003000*                                 TOLERANSAVVIKELSE INLEVERANS            
003100     03 MID-INPUT1.                                                       
003200        05 MID-KVVECKOR-LVAR-IN                                           
003300                             PIC X(4).                                    
003400*                                 VARIANS I LEDTIDEN                      
003500     03 MID-SPAR-DAG         OCCURS 5 TIMES                               
003600                             PIC X(2).                                    
003700*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
