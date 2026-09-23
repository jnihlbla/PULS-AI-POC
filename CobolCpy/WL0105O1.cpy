000100 01  RESP-WL0105O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0105         
000300*                                 LDC BUFFER INFORMATION                  
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-IDARTNR-KEY     PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 RESP-KVBUFF-KEY      PIC X(7).                                    
001100*                                 FÖRÄDLAT BUFFERSALDO                    
001200*                                 BUFFER QUANTANTITY PREPARED             
001300     03 RESP-ADBUFFOMR-KEY   PIC X(2).                                    
001400*                                 BUFFERTOMRÅDE                           
001500*                                 BUFFER AREA                             
001600     03 RESP-ADBUFFGANG-KEY  PIC X(2).                                    
001700*                                 BUFFERT GÅNG                            
001800     03 RESP-ADBUFFPL-KEY    PIC X(5).                                    
001900*                                 BUFFERPLATSNUMMER                       
002000*                                 LOCATION IN BUFFER                      
002100     03 RESP-ADLAGOMR        PIC 9(2).                                    
002200*                                 LAGEROMRÅDE                             
002300*                                 AREA                                    
002400     03 RESP-ADGANG          PIC 9(2).                                    
002500*                                 GÅNG                                    
002600*                                 AISLE                                   
002700     03 RESP-ADPLATS         PIC 9(5).                                    
002800*                                 LAGERPLATSNUMMER                        
002900*                                 LOCATION                                
003000     03 RESP-BEART           PIC X(25).                                   
003100*                                 ARTIKELBENÄMNING                        
003200*                                 PART DESCRIPTION                        
003300     03 RESP-KVLS            PIC -(7)9.                                   
003400*                                 LAGERSALDO                              
003500*                                 STOCK BALANCE                           
003600     03 RESP-MOD-LEDTEXT     PIC X(37).                                   
003700     03 RESP-SUBUFF-F        PIC Z(6)9.                                   
003800*                                 FÖRÄDLAT BUFFERSALDO                    
003900*                                 BUFFER QUANTANTITY PREPARED             
004000     03 RESP-SUKOLLI-F       PIC Z(3)9.                                   
004100*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
004200*                                 Q PREPARED CASES IN BUFFER              
004300     03 RESP-ADBUFFOMR-UPD   PIC X(2).                                    
004400*                                 BUFFERTOMRÅDE                           
004500*                                 BUFFER AREA                             
004600     03 RESP-ADBUFFGANG-UPD  PIC X(2).                                    
004700*                                 BUFFERT GÅNG                            
004800     03 RESP-ADBUFFPL-UPD    PIC X(5).                                    
004900*                                 BUFFERPLATSNUMMER                       
005000*                                 LOCATION IN BUFFER                      
005100     03 RESP-KVBUFF-F-UPD    PIC X(7).                                    
005200*                                 FÖRÄDLAT BUFFERSALDO                    
005300*                                 BUFFER QUANTANTITY PREPARED             
005400     03 RESP-KVKOLLI-F-UPD   PIC X(4).                                    
005500*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
005600*                                 Q PREPARED CASES IN BUFFER              
005700     03 RESP-IDMSG-ERROR-UPD PIC X(3).                                    
005800*                                 FELMEDDELANDE ID                        
005900*                                 ERROR MESSAGE ID                        
006000     03 RESP-KVRADER         PIC 9(5).                                    
006100*                                 ANTAL RADER                             
006200*                                 NUMBER OF LINES                         
006300     03 RESP-RAD             OCCURS 500 TIMES.                            
006400        05 RESP-KDCMDVAL     PIC X(3).                                    
006500*                                 GENERELL KOMMANDOKOD                    
006600*                                 GENERAL COMMAND-CODE                    
006700        05 RESP-ADBUFFOMR    PIC X(2).                                    
006800*                                 BUFFERTOMRÅDE                           
006900*                                 BUFFER AREA                             
007000        05 RESP-ADBUFFGANG   PIC X(2).                                    
007100*                                 BUFFERT GÅNG                            
007200        05 RESP-ADBUFFPL     PIC X(5).                                    
007300*                                 BUFFERPLATSNUMMER                       
007400*                                 LOCATION IN BUFFER                      
007500        05 RESP-DABUFPAF     PIC Z(8).                                    
007600*                                 BUFFERT PÅFYLLNINGS DATUM               
007700*                                 BUFFERT REFILLING DATE                  
007800        05 RESP-KVBUFF-F     PIC Z(6)9.                                   
007900*                                 FÖRÄDLAT BUFFERSALDO                    
008000*                                 BUFFER QUANTANTITY PREPARED             
008100        05 RESP-KVBUFF-F-IN  PIC Z(7).                                    
008200*                                 FÖRÄDLAT BUFFERSALDO                    
008300*                                 BUFFER QUANTANTITY PREPARED             
008400        05 RESP-KVKOLLI-F    PIC Z(3)9.                                   
008500*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
008600*                                 Q PREPARED CASES IN BUFFER              
008700        05 RESP-KVKOLLI-F-IN PIC Z(4).                                    
008800*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
008900*                                 Q PREPARED CASES IN BUFFER              
009000        05 RESP-IDMSG-ERROR-LINE                                          
009100                             PIC X(3).                                    
009200*                                 FELMEDDELANDE ID                        
009300*                                 ERROR MESSAGE ID                        
009400*** END OF VILMAII-COPY LENGTH= 22645 BYTES                               
