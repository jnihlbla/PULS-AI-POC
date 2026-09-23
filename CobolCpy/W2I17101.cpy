000100 01  MID-W2I17101.                                                        
000200*                                 MID-COPYTEXT FÖR W2017100               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDANSK-IN        PIC X(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MID-IDANSK-UT        PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MID-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MID-IDLEVNR-UT       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MID-KDLARM-IN        PIC X(3).                                    
001600*                                 LARMORSAKSKOD                           
001700     03 MID-KDLARM-UT        PIC X(3).                                    
001800*                                 LARMORSAKSKOD                           
001900     03 MID-KDOTFREK-IN      PIC X.                                       
002000*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002100     03 MID-KDOTFREK-UT      PIC X.                                       
002200*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002300     03 MID-TISENBEK-DAG-ENTER                                            
002400                             PIC 9(6).                                    
002500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002600     03 MID-TISENBEK-KL-ENTER                                             
002700                             PIC 9(6).                                    
002800*                                 TIM - MIN - SEK   (HHMMSS)              
002900     03 MID-KDLARM-ENTER     PIC 9(3).                                    
003000*                                 LARMORSAKSKOD                           
003100     03 MID-TISENBEK-DAG-NEXT                                             
003200                             PIC 9(6).                                    
003300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003400     03 MID-TISENBEK-KL-NEXT PIC 9(6).                                    
003500*                                 TIM - MIN - SEK   (HHMMSS)              
003600     03 MID-KDLARM-NEXT      PIC 9(3).                                    
003700*                                 LARMORSAKSKOD                           
003800     03 MID-INFO-RAD         OCCURS 14 TIMES.                             
003900*                                 RADINFORMATION                          
004000        05 MID-SELECT-ARTIKEL                                             
004100                             PIC X.                                       
004200        05 MID-FLNYLARM      PIC X.                                       
004300*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
004400        05 MID-KDLARM        PIC 9(3).                                    
004500*                                 LARMORSAKSKOD                           
004600        05 MID-TISENBEK-DAG-IN                                            
004700                             PIC 9(6).                                    
004800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004900        05 MID-TISENBEK-KL-IN                                             
005000                             PIC 9(6).                                    
005100*                                 TIM - MIN - SEK   (HHMMSS)              
005200*** END OF VILMAII-COPY LENGTH= 310 BYTES                                 
