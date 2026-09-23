000100 01  REQU-WL0601I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0601             
000300*                                 LDC INBOUND LOCATION UPDATE             
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-ADLAGOMR-KEY    PIC 9(2).                                    
000800*                                 LAGEROMRÅDE                             
000900*                                 AREA                                    
001000     03 REQU-ADGANG-KEY      PIC 9(2).                                    
001100*                                 GÅNG                                    
001200*                                 AISLE                                   
001300     03 REQU-BAY-KEY         PIC 9(2).                                    
001400     03 REQU-TABELLRAD       OCCURS 10 TIMES.                             
001500*                                 GRUPP MED TABELLRADER                   
001600        05 REQU-ADLAGOMR-UPD PIC 9(2).                                    
001700*                                 LAGEROMRÅDE                             
001800*                                 AREA                                    
001900        05 REQU-ADGANG-UPD   PIC 9(2).                                    
002000*                                 GÅNG                                    
002100*                                 AISLE                                   
002200        05 REQU-BAY-UPD      PIC 9(2).                                    
002300        05 REQU-KVPLATS-UPD  PIC 9(3).                                    
002400*                                 NO OF ADDRESSES IN A BAY                
002500*                                 ANTAL LAGERPLATSER I ETT STÄLL          
002600     03 REQU-KVRADER         PIC 9(5).                                    
002700*                                 ANTAL RADER                             
002800*                                 NUMBER OF LINES                         
002900     03 REQU-TABELLRAD2      OCCURS 500 TIMES.                            
003000*                                 GRUPP MED TABELLRADER                   
003100        05 REQU-CMD-IN       PIC X(3).                                    
003200        05 REQU-ADLAGOMR-UT  PIC 9(2).                                    
003300*                                 LAGEROMRÅDE                             
003400*                                 AREA                                    
003500        05 REQU-ADGANG-UT    PIC 9(2).                                    
003600*                                 GÅNG                                    
003700*                                 AISLE                                   
003800        05 REQU-BAY-UT       PIC 9(2).                                    
003900        05 REQU-KVPLATS-IN   PIC 9(3).                                    
004000*                                 NO OF ADDRESSES IN A BAY                
004100*                                 ANTAL LAGERPLATSER I ETT STÄLL          
004200        05 REQU-KVPLATS-UT   PIC 9(3).                                    
004300*                                 NO OF ADDRESSES IN A BAY                
004400*                                 ANTAL LAGERPLATSER I ETT STÄLL          
004500*** END OF VILMAII-COPY LENGTH= 7603 BYTES                                
