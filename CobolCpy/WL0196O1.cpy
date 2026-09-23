000100 01  RESP-WL0196O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0196         
000300*                                 LDC INBOUND                             
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-FREQPACK-KEY    PIC X.                                       
000800     03 RESP-KVRADER-PACK-KEY                                             
000900                             PIC Z(4)9.                                   
001000*                                 ANTAL RADER                             
001100*                                 NUMBER OF LINES                         
001200     03 RESP-ADLAGOMR-KEY    PIC Z9.                                      
001300*                                 LAGEROMRÅDE                             
001400*                                 AREA                                    
001500     03 RESP-KVRADER-PERIOD-KEY                                           
001600                             PIC Z(4)9.                                   
001700*                                 ANTAL RADER                             
001800*                                 NUMBER OF LINES                         
001900     03 RESP-KDSORT-KEY      PIC X(2).                                    
002000*                                 SORT-KOD                                
002100*                                 UNIT OF MEASURE                         
002200     03 RESP-KDSORT1-KEY     PIC X.                                       
002300*                                 SORTERINGSKOD                           
002400*                                 CODE FOR SORTING                        
002500     03 RESP-BEART-KEY       PIC X(25).                                   
002600*                                 ARTIKELBENÄMNING                        
002700*                                 PART DESCRIPTION                        
002800     03 RESP-IDSPRAK-KEY     PIC X(3).                                    
002900*                                 NATIONALITETSTECKEN                     
003000*                                 SPRÅKIDENTIFIKATION                     
003100*                                 NATIONALITY SIGN                        
003200*                                 LANGUAGE IDENTIFIER                     
003300     03 RESP-WEB-LAYOUT      PIC X.                                       
003400     03 RESP-KVRADER         PIC Z(4)9.                                   
003500*                                 ANTAL RADER                             
003600*                                 NUMBER OF LINES                         
003700     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
003800*                                 GRUPP MED TABELLRADER                   
003900        05 RESP-PACK-ANTAL-UT                                             
004000                             PIC Z(4)9.                                   
004100*                                 ANTAL RADER                             
004200*                                 NUMBER OF LINES                         
004300        05 RESP-IDARTNR-UT   PIC Z(7)9.                                   
004400*                                 ARTIKELNUMMER                           
004500*                                 PART NUMBER                             
004600        05 RESP-BEART-UT     PIC X(25).                                   
004700*                                 ARTIKELBENÄMNING                        
004800*                                 PART DESCRIPTION                        
004900        05 RESP-KDSORT-UT    PIC X(2).                                    
005000*                                 SORT-KOD                                
005100*                                 UNIT OF MEASURE                         
005200        05 RESP-KVLS-UT      PIC -(7)9.                                   
005300*                                 LAGERSALDO                              
005400*                                 STOCK BALANCE                           
005500        05 RESP-KVPB-REF-UT  PIC Z(5)9.9.                                 
005600*                                 PERIODBEHOV REFILLING                   
005700*                                 FORECAST REFILLING                      
005800        05 RESP-ADLAGOMR-UT  PIC 9(2).                                    
005900*                                 LAGEROMRÅDE                             
006000*                                 AREA                                    
006100        05 RESP-ADGANG-UT    PIC 9(2).                                    
006200*                                 GÅNG                                    
006300*                                 AISLE                                   
006400        05 RESP-ADPLATS-UT   PIC 9(5).                                    
006500*                                 LAGERPLATSNUMMER                        
006600*                                 LOCATION                                
006700        05 RESP-ADBUFFOMR-UT PIC 9(2).                                    
006800*                                 BUFFERTOMRÅDE                           
006900*                                 BUFFER AREA                             
007000        05 RESP-ADBUFFGANG-UT                                             
007100                             PIC 9(2).                                    
007200*                                 BUFFERT GÅNG                            
007300        05 RESP-ADBUFFPL-UT  PIC 9(5).                                    
007400*                                 BUFFERPLATSNUMMER                       
007500*                                 LOCATION IN BUFFER                      
007600        05 RESP-TIREFEFT-UT  PIC 9(6).                                    
007700*                                 DATUM SENAST EFTERFRÅGAD                
007800*                                 DATE LATEST DEMAND                      
007900*** END OF VILMAII-COPY LENGTH= 40052 BYTES                               
