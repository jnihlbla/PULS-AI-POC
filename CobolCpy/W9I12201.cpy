000100 01  MID-W9I12201.                                                        
000200*                                 MID-COPYTEXT PGM W90122                 
000300*                                 PROJECT REGISTRATION                    
000400     03 MID-IDPROJ-IN        PIC X(4).                                    
000500*                                 PARTS PROJEKTIDENTITET                  
000600     03 MID-IDPROJ-UT        PIC X(4).                                    
000700*                                 PARTS PROJEKTIDENTITET                  
000800     03 MID-KDBASLM-IN       PIC X(6).                                    
000900*                                 BASLAGERMARKNAD                         
001000     03 MID-KDBASLM-UT       PIC X(6).                                    
001100*                                 BASLAGERMARKNAD                         
001200     03 MID-IDSKYLT-IN       PIC X(3).                                    
001300*                                 NATIONALITETSTECKEN                     
001400     03 MID-IDSKYLT-UT       PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600     03 MID-IDARTNR-IN       PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MID-IDARTNR-UT       PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MID-KDPRODSL-UT      PIC X(2).                                    
002100*                                 PRODUKTSLAG                             
002200     03 MID-KDBPSR-MIN       PIC 9.                                       
002300*                                 BASLAGERFÖRSLAGSNIVÅ                    
002400     03 MID-KDBPSR-MAX       PIC 9.                                       
002500*                                 BASLAGERFÖRSLAGSNIVÅ                    
002600     03 MID-IDFKNGRP-MIN     PIC 9(4).                                    
002700*                                 FUNKTIONSGRUPP                          
002800     03 MID-KDDEALER         PIC X.                                       
002900*                                 BASLAGER KUNDKOD                        
003000     03 MID-KVBASLM          PIC X(7).                                    
003100*                                 BASLAGER TOTAL PER MARKNAD              
003200     03 MID-RAD              OCCURS 6 TIMES.                              
003300        05 MID-IDDISTR       PIC 9(4).                                    
003400*                                 DISTRIKTNUMMER                          
003500        05 MID-REBLFORD      PIC 9(2).                                    
003600*                                 PROCENTFÖRDELNING PER DISTRIKT          
003700        05 MID-KVBASLMD      PIC X(7).                                    
003800*                                 BASLAGER DISTRIKT KVANTITET             
003900     03 MID-KVBLKIT          PIC 9(3).                                    
004000*                                 ÅTERFÖRSÄLJARSATSER PER MARKNAD         
004100     03 MID-KVBASLKIT        PIC X(7).                                    
004200*                                 ANTAL I ÅTERFÖLSÄLJARSATS               
004300     03 MID-TEARTNOT-MARK    PIC X(40).                                   
004400*                                 ARTIKEL NOTERING                        
004500*** END COPY W9I12201    LENGTH=188                                       
