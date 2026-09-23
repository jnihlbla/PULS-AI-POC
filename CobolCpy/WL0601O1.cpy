000100 01  RESP-WL0601O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0601         
000300*                                 LDC INBOUND LOCATION UPDATE             
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-ADLAGOMR-KEY    PIC 9(2).                                    
000800*                                 LAGEROMRÅDE                             
000900*                                 AREA                                    
001000     03 RESP-ADGANG-KEY      PIC 9(2).                                    
001100*                                 GÅNG                                    
001200*                                 AISLE                                   
001300     03 RESP-BAY-KEY         PIC 9(2).                                    
001400     03 RESP-TABELLRAD       OCCURS 10 TIMES.                             
001500*                                 GRUPP MED TABELLRADER                   
001600        05 RESP-ADLAGOMR-UPD PIC 9(2).                                    
001700*                                 LAGEROMRÅDE                             
001800*                                 AREA                                    
001900        05 RESP-ADGANG-UPD   PIC 9(2).                                    
002000*                                 GÅNG                                    
002100*                                 AISLE                                   
002200        05 RESP-BAY-UPD      PIC 9(2).                                    
002300        05 RESP-KVPLATS-UPD  PIC Z(2)9.                                   
002400*                                 NO OF ADDRESSES IN A BAY                
002500*                                 ANTAL LAGERPLATSER I ETT STÄLL          
002600     03 RESP-KVRADER         PIC Z(4)9.                                   
002700*                                 ANTAL RADER                             
002800*                                 NUMBER OF LINES                         
002900     03 RESP-TABELLRAD2      OCCURS 500 TIMES.                            
003000*                                 GRUPP MED TABELLRADER                   
003100        05 RESP-CMD-IN       PIC X(3).                                    
003200        05 RESP-ADLAGOMR-UT  PIC 9(2).                                    
003300*                                 LAGEROMRÅDE                             
003400*                                 AREA                                    
003500        05 RESP-ADGANG-UT    PIC 9(2).                                    
003600*                                 GÅNG                                    
003700*                                 AISLE                                   
003800        05 RESP-BAY-UT       PIC 9(2).                                    
003900        05 RESP-KVPLATS-IN   PIC Z(2)9.                                   
004000*                                 NO OF ADDRESSES IN A BAY                
004100*                                 ANTAL LAGERPLATSER I ETT STÄLL          
004200        05 RESP-KVPLATS-UT   PIC Z(2)9.                                   
004300*                                 NO OF ADDRESSES IN A BAY                
004400*                                 ANTAL LAGERPLATSER I ETT STÄLL          
004500*** END OF VILMAII-COPY LENGTH= 7603 BYTES                                
