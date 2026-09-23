000100 01  MID-W2I17201.                                                        
000200*                                 MID-COPYTEXT FÖR W2017200               
000300     03 MID-IDANSK-IN        PIC X(3).                                    
000400*                                 ANSKAFFARNUMMER                         
000500     03 MID-IDANSK-UT        PIC X(3).                                    
000600*                                 ANSKAFFARNUMMER                         
000700     03 MID-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MID-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MID-KDLARM-IN        PIC X(3).                                    
001200*                                 LARMORSAKSKOD                           
001300     03 MID-KDLARM-UT        PIC X(3).                                    
001400*                                 LARMORSAKSKOD                           
001500     03 MID-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-KDOTFREK-IN      PIC X.                                       
002000*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002100     03 MID-KDOTFREK-UT      PIC X.                                       
002200*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002300     03 MID-MID-INPUT.                                                    
002400*                                 TABINFORMATION                          
002500        05 MID-INFO-RAD      OCCURS 15 TIMES.                             
002600*                                 RADINFORMATION                          
002700           07 MID-SELECT-ARTIKEL                                          
002800                             PIC X.                                       
002900*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
