000100 01  MID-W6I30801.                                                        
000200*                                 MID-COPYTEXT FÖR BILD 6308              
000300*                                 LEVERANS SPÄRRKOD + NOTERING            
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDSKYLT-IN       PIC X(3).                                    
000900*                                 NATIONALITETSTECKEN IN                  
001000     03 MID-IDSKYLT-UT       PIC X(3).                                    
001100*                                 NATIONALITETSTECKEN UT                  
001200     03 MID-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MID-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MID-SHOW-IN          PIC X.                                       
001700     03 MID-SHOW-UT          PIC X.                                       
001800     03 MID-TYPE-IN          PIC X.                                       
001900     03 MID-TYPE-UT          PIC X.                                       
002000     03 MID-INPUT.                                                        
002100*                                                                         
002200        05 MID-FLMAIL        PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400        05 MID-KDLEVSP-GLOB  PIC 9(2).                                    
002500*                                 SPÄRRKOD LEVERANS                       
002600        05 MID-TEARTNOT-GLOB PIC X(25).                                   
002700        05 MID-KDLEVSP-DC11  PIC 9(2).                                    
002800*                                 SPÄRRKOD LEVERANS                       
002900        05 MID-KVSPARR-KVAL-DC11                                          
003000                             PIC 9(7).                                    
003100*                                 SPÄRRAT ANTAL KVALITETSFEL              
003200        05 MID-INPUT2        OCCURS 12 TIMES.                             
003300*                                                                         
003400           07 MID-KDLEVSP    PIC 9(2).                                    
003500*                                 SPÄRRKOD LEVERANS                       
003600           07 MID-KVSPARR-KVAL                                            
003700                             PIC 9(7).                                    
003800*                                 SPÄRRAT ANTAL KVALITETSFEL              
003900           07 MID-TEKVAL     PIC X(10).                                   
004000*                                 KVALITETSNOTERING SPÄRR                 
004100*** END OF VILMAII-COPY LENGTH= 297 BYTES                                 
