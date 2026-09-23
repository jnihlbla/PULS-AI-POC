000100 01  MID-W9I12101.                                                        
000200*                                 MID-COPYTEXT PGM W90121                 
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
001600     03 MID-KDBPSR-1-IN      PIC X.                                       
001700*                                 BASLAGERFÖRSLAGSNIVÅ                    
001800     03 MID-KDBPSR-1-UT      PIC X.                                       
001900*                                 BASLAGERFÖRSLAGSNIVÅ                    
002000     03 MID-KDBPSR-2-IN      PIC X.                                       
002100*                                 BASLAGERFÖRSLAGSNIVÅ                    
002200     03 MID-KDBPSR-2-UT      PIC X.                                       
002300*                                 BASLAGERFÖRSLAGSNIVÅ                    
002400     03 MID-KDPRODSL-IN      PIC X(2).                                    
002500*                                 PRODUKTSLAG                             
002600     03 MID-KDPRODSL-UT      PIC X(2).                                    
002700*                                 PRODUKTSLAG                             
002800     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
002900*                                 ARTIKELNUMMER                           
003000     03 MID-IDARTNR-PF8      PIC 9(9).                                    
003100*                                 ARTIKELNUMMER                           
003200     03 MID-IDFKNGRP-ENTER   PIC 9(4).                                    
003300*                                 FUNKTIONSGRUPP                          
003400     03 MID-IDFKNGRP-PF8     PIC 9(4).                                    
003500*                                 FUNKTIONSGRUPP                          
003600     03 MID-RAD              OCCURS 13 TIMES.                             
003700        05 MID-KDSVAR        PIC X.                                       
003800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003900        05 MID-IDARTNR       PIC 9(9).                                    
004000*                                 ARTIKELNUMMER                           
004100*** END COPY W9I12101    LENGTH=190                                       
