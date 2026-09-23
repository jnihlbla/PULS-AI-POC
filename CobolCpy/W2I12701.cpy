000100 01  MID-W2I12701.                                                        
000200*                                 MID-COPYTEXT FÖR W2012700               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDANSK-DOLD      PIC 9(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MID-TISENBEK-DAG-ENTER                                            
001000                             PIC 9(6).                                    
001100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001200*                                 FÅR EJ ANVÄNDAS                         
001300     03 MID-TISENBEK-KL-ENTER                                             
001400                             PIC 9(6).                                    
001500*                                 TIM - MIN - SEK   (HHMMSS)              
001600     03 MID-KDLARM-ENTER     PIC 9(3).                                    
001700*                                 LARMORSAKSKOD                           
001800     03 MID-TISENBEK-DAG-NEXT                                             
001900                             PIC 9(6).                                    
002000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002100*                                 FÅR EJ ANVÄNDAS                         
002200     03 MID-TISENBEK-KL-NEXT PIC 9(6).                                    
002300*                                 TIM - MIN - SEK   (HHMMSS)              
002400     03 MID-KDLARM-NEXT      PIC 9(3).                                    
002500*                                 LARMORSAKSKOD                           
002600     03 MID-INPUT.                                                        
002700*                                 RADINFORMATION                          
002800        05 MID-INFO-RAD      OCCURS 3 TIMES.                              
002900*                                 RADINFORMATION                          
003000           07 MID-KVART      PIC X(7).                                    
003100*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
003200           07 MID-KVVECKOR-TPO5                                           
003300                             PIC X(2).                                    
003400*                                 ANTAL VECKOR FÖR TPO5                   
003500*** END COPY W2I12701    LENGTH=78                                        
