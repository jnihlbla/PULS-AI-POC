000100 01  MID-W6I33801.                                                        
000200*                                 MID-COPYTEXT FÖR BILD 6338              
000300*                                 LEVERANS SPÄRRKOD + NOTERING            
000400*                                 LDC                                     
000500     03 MID-IDARTNR-IN       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDARTNR-UT       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDSKYLT-IN       PIC X(3).                                    
001000*                                 NATIONALITETSTECKEN IN                  
001100     03 MID-IDSKYLT-UT       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN UT                  
001300     03 MID-INPUT.                                                        
001400*                                                                         
001500        05 MID-RAD           OCCURS 14 TIMES.                             
001600*                                                                         
001700           07 MID-KDLEVSP    PIC 9(2).                                    
001800*                                 SPÄRRKOD LEVERANS                       
001900           07 MID-KVSPARR-KVAL                                            
002000                             PIC 9(7).                                    
002100*                                 SPÄRRAT ANTAL KVALITETSFEL              
002200           07 MID-TEKVAL     PIC X(10).                                   
002300*                                 KVALITETSNOTERING SPÄRR                 
002400*** END OF VILMAII-COPY LENGTH= 290 BYTES                                 
