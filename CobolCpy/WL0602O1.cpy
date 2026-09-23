000100 01  RESP-WL0602O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0602         
000300*                                 LDC INBOUND LOCATION QUERY              
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
001400     03 RESP-FL-NOEXIST      PIC X.                                       
001500*                                 JA/NEJ-FLAGGA                           
001600     03 RESP-FL-DEVIATION    PIC X.                                       
001700*                                 JA/NEJ-FLAGGA                           
001800     03 RESP-FL-COVER        PIC X.                                       
001900*                                 JA/NEJ-FLAGGA                           
002000     03 RESP-WEB-LAYOUT      PIC X.                                       
002100     03 RESP-KVRADER         PIC Z(4)9.                                   
002200*                                 ANTAL RADER                             
002300*                                 NUMBER OF LINES                         
002400     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002500*                                 GRUPP MED TABELLRADER                   
002600        05 RESP-IDARTNR-UT   PIC Z(7)9.                                   
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900        05 RESP-ADLAGOMR-UT  PIC 9(2).                                    
003000*                                 LAGEROMRÅDE                             
003100*                                 AREA                                    
003200        05 RESP-ADGANG-UT    PIC 9(2).                                    
003300*                                 GÅNG                                    
003400*                                 AISLE                                   
003500        05 RESP-ADPLATS-UT   PIC Z(4)9.                                   
003600*                                 LAGERPLATSNUMMER                        
003700*                                 LOCATION                                
003800        05 RESP-KVPLATS-UT   PIC Z(2)9.                                   
003900*                                 NO OF ADDRESSES IN A BAY                
004000*                                 ANTAL LAGERPLATSER I ETT STÄLL          
004100        05 RESP-KVANTART-UT  PIC Z(4)9.                                   
004200*                                 ANTAL-ARTIKLAR                          
004300*                                 QUANTITY PARTS                          
004400        05 RESP-RELOBEL-UT   PIC Z(2)9.9.                                 
004500*                                 BELÄGGNINGSPROCENT PER LO               
004600*                                 COVERING PERCENT PER LO                 
004700        05 RESP-REDCBEL-UT   PIC Z(2)9.9.                                 
004800*                                 BELÄGGNINGSPROCENT PER DC               
004900*                                 COVERING PERCENT PER DC                 
005000*** END OF VILMAII-COPY LENGTH= 17517 BYTES                               
