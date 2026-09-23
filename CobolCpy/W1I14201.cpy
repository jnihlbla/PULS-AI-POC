000100 01  MID-W1I14201.                                                        
000200*                                 MID-COPYTEXT FÖR W1014200               
000300     03 MID-IDBERED-IN       PIC X(2).                                    
000400*                                 BEREDARENUMMER                          
000500     03 MID-IDBERED-UT       PIC X(2).                                    
000600*                                 BEREDARENUMMER                          
000700     03 MID-IDAO-IN          PIC X(10).                                   
000800*                                 ÄNDRINGSORDERNUMMER                     
000900     03 MID-IDAO-UT          PIC X(10).                                   
001000*                                 ÄNDRINGSORDERNUMMER                     
001100     03 MID-IDPROJ-IN        PIC X(4).                                    
001200*                                 PARTS PROJEKTIDENTITET                  
001300     03 MID-IDPROJ-UT        PIC X(4).                                    
001400*                                 PARTS PROJEKTIDENTITET                  
001500     03 MID-IDAO-LO          PIC X(10).                                   
001600*                                 ÄNDRINGSORDERNUMMER                     
001700     03 MID-IDAO-HI          PIC X(10).                                   
001800*                                 ÄNDRINGSORDERNUMMER                     
001900     03 MID-IDARTNR-LO       PIC 9(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-IDARTNR-HI       PIC 9(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MID-INFO-RAD         OCCURS 13 TIMES.                             
002400*                                 RADINFORMATION                          
002500        05 MID-SELECT-ARTIKEL                                             
002600                             PIC X.                                       
002700        05 MID-IDARTNR       PIC 9(9).                                    
002800*                                 ARTIKELNUMMER                           
002900        05 MID-RS-IN         PIC X.                                       
003000*                                 RESERVDELSBEDÖMNINGSKOD                 
003100        05 MID-NEDB-IN       PIC X(6).                                    
003200*                                 SLUTTID NEDBRYTNING                     
003300        05 MID-NY-IDBERED-IN PIC X(2).                                    
003400*                                 BEREDARENUMMER                          
003500        05 MID-BORTTAG-IN    PIC X.                                       
003600*                                 ANGER ATT ARTIKELN ÄR UTGÅNGEN          
003700*** END COPY W1I14201C0  LENGTH=330                                       
