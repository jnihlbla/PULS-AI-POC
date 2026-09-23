000100 01  MID-W0I79101.                                                        
000200*                                 MID-COPYTEXT PGM W00791                 
000300*                                 BASIC STOCK LISTS                       
000400     03 MID-KDBASLM          PIC X(6).                                    
000500*                                 BASLAGERMARKNAD                         
000600     03 MID-KDPRODSL         PIC X(2).                                    
000700*                                 PRODUKTSLAG                             
000800     03 MID-IDPROJ           PIC X(4).                                    
000900*                                 PARTS PROJEKTIDENTITET                  
001000     03 MID-IDFKNGRP-1       PIC 9(4).                                    
001100*                                 FUNKTIONSGRUPP                          
001200     03 MID-IDFKNGRP-2       PIC 9(4).                                    
001300*                                 FUNKTIONSGRUPP                          
001400     03 MID-IDDISTR          PIC 9(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MID-KDBPSR-1         PIC 9.                                       
001700*                                 BASLAGERFÖRSLAGSNIVÅ                    
001800     03 MID-KDBPSR-2         PIC 9.                                       
001900*                                 BASLAGERFÖRSLAGSNIVÅ                    
002000     03 MID-IDSKYLT          PIC X(3).                                    
002100      88 MID-GODK-IDSKYLT    VALUE 'D  '                                  
002200                             'E  '                                        
002300                             'F  '                                        
002400                             'GB '                                        
002500                             'I  '                                        
002600                             'NL '                                        
002700                             'P  '                                        
002800                             'S  '                                        
002900                             'SF '                                        
003000                             'USA'.                                       
003100*                                 NATIONALITETSTECKEN                     
003200     03 MID-MARK-NOT         PIC X.                                       
003300     03 MID-MARK-QTY         PIC X.                                       
003400     03 MID-KDDEALER         PIC X.                                       
003500*                                 BASLAGER KUNDKOD                        
003600     03 MID-IDFKNGRP-SORT    PIC X.                                       
003700     03 MID-TISTOMREG-SORT   PIC X.                                       
003800     03 MID-IDARTNR-SORT     PIC X.                                       
003900     03 MID-KDPRODSL-SORT    PIC X.                                       
004000*** END COPY W0I79101    LENGTH=36                                        
