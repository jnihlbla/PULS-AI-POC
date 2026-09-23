000100 01  MID-W2I12601.                                                        
000200*                                 MID-COPYTEXT F÷R W2012600               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDANSK-DOLD      PIC 9(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MID-TISENBEK-DAG-ENTER                                            
001000                             PIC 9(6).                                    
001100*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001200     03 MID-TISENBEK-KL-ENTER                                             
001300                             PIC 9(6).                                    
001400*                                 TIM - MIN - SEK   (HHMMSS)              
001500     03 MID-KDLARM-ENTER     PIC 9(3).                                    
001600*                                 LARMORSAKSKOD                           
001700     03 MID-TISENBEK-DAG-NEXT                                             
001800                             PIC 9(6).                                    
001900*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002000     03 MID-TISENBEK-KL-NEXT PIC 9(6).                                    
002100*                                 TIM - MIN - SEK   (HHMMSS)              
002200     03 MID-KDLARM-NEXT      PIC 9(3).                                    
002300*                                 LARMORSAKSKOD                           
002400     03 MID-INPUT.                                                        
002500*                                 RADINFORMATION                          
002600        05 MID-INFO-RAD      OCCURS 6 TIMES.                              
002700*                                 RADINFORMATION                          
002800           07 MID-KVART-IN   PIC 9(7).                                    
002900*                                 ANTAL ARTNR PER BRYTBEGREPP             
003000           07 MID-TITPO-IN   PIC 9(5).                                    
003100*                                 TPO-DATUM AAVVD  TITPO-002              
003200*** END OF VILMAII-COPY LENGTH= 123 BYTES                                 
