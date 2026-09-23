000100 01  RESP-WL0106O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0106         
000300*                                 LDC LOCATION INQUIRY                    
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-KDCMDVAL        PIC X(3).                                    
000800*                                 GENERELL KOMMANDOKOD                    
000900*                                 GENERAL COMMAND-CODE                    
001000     03 RESP-ADLAGOMR-KEY    PIC X(3).                                    
001100*                                 LAGEROMRÅDE                             
001200*                                 AREA                                    
001300     03 RESP-ADGANG-KEY      PIC X(3).                                    
001400*                                 GÅNG                                    
001500*                                 AISLE                                   
001600     03 RESP-ADPLATS-KEY     PIC X(5).                                    
001700*                                 LAGERPLATSNUMMER                        
001800*                                 LOCATION                                
001900     03 RESP-KVRADER         PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100*                                 NUMBER OF LINES                         
002200     03 RESP-TABELLRAD       OCCURS 9999 TIMES.                           
002300*                                 GRUPP MED TABELLRADER                   
002400        05 RESP-IDARTNR      PIC Z(7)9.                                   
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700        05 RESP-BEART-ENG    PIC X(25).                                   
002800*                                 ENGELSK ARTIKELBENÄMNING                
002900        05 RESP-ADLAGOMR     PIC 9(2).                                    
003000*                                 LAGEROMRÅDE                             
003100*                                 AREA                                    
003200        05 RESP-ADGANG       PIC 9(2).                                    
003300*                                 GÅNG                                    
003400*                                 AISLE                                   
003500        05 RESP-ADPLATS      PIC X(5).                                    
003600*                                 LAGERPLATSNUMMER                        
003700*                                 LOCATION                                
003800        05 RESP-KVLS         PIC -Z(6)9.                                  
003900*                                 LAGERSALDO                              
004000*                                 STOCK BALANCE                           
004100        05 RESP-KVPB-REF     PIC Z(5)9.9.                                 
004200*                                 PERIODBEHOV REFILLING                   
004300*                                 FORECAST REFILLING                      
004400*** END OF VILMAII-COPY LENGTH= 57963 BYTES                               
