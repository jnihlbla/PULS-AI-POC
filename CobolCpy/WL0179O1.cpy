000100 01  RESP-WL0179O1.                                                       
000200*                                 RESPONS FROM PGM WL0179                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-ADLAGOMR-KEY    PIC 9(2).                                    
000600*                                 LAGEROMRÅDE                             
000700     03 RESP-ADGANG-KEY      PIC 9(2).                                    
000800*                                 GÅNG                                    
000900     03 RESP-ADPLATS-KEY     PIC 9(5).                                    
001000*                                 LAGERPLATSNUMMER                        
001100     03 RESP-IDPRTOMG-KEY    PIC 9.                                       
001200*                                 PRINT OMGÅNG FÖR AUT.JUSTERING          
001300     03 RESP-FLINVSKR-KEY    PIC X.                                       
001400*                                 INVENTERINGSANMODAN UTSKRIVEN           
001500     03 RESP-WL0173I1        OCCURS 10 TIMES.                             
001600*                                 ARTIKEL-PRINT GRUPP                     
001700        05 RESP-IDARTNR-L173 PIC 9(8).                                    
001800*                                 ARTIKELNUMMER                           
001900        05 RESP-KDINVPRIO-L173                                            
002000                             PIC 9.                                       
002100*                                 INVENTERING PRIORITET                   
002200        05 RESP-KDINVKAT-L173                                             
002300                             PIC 9(2).                                    
002400*                                 INVENTERINGSKATEGORI                    
002500     03 RESP-KVINVSKR-5309   PIC Z(2).                                    
002600*                                 BEGÄRDA INVENTERINGSUNDERLAG            
002700     03 RESP-KVANT-ART       PIC Z(2)9.                                   
002800     03 RESP-KVRADER         PIC Z(4)9.                                   
002900*                                 ANTAL RADER                             
003000     03 RESP-RAD-INFO        OCCURS 500 TIMES.                            
003100*                                 RAD-INFO                                
003200        05 RESP-ADLAGOMR-UTSKR                                            
003300                             PIC 9(2).                                    
003400*                                 LAGEROMRÅDE                             
003500        05 RESP-ADGANG-UTSKR PIC 9(2).                                    
003600*                                 GÅNG                                    
003700        05 RESP-ADPLATS-UTSKR                                             
003800                             PIC 9(5).                                    
003900*                                 LAGERPLATSNUMMER                        
004000        05 RESP-KDINVPRIO-UTSKR                                           
004100                             PIC 9.                                       
004200*                                 INVENTERING PRIORITET                   
004300        05 RESP-KDVVKL-UTSKR PIC 9.                                       
004400*                                 VOLYMVÄRDESKLASS                        
004500        05 RESP-IDARTNR-UTSKR                                             
004600                             PIC Z(7)9.                                   
004700*                                 ARTIKELNUMMER                           
004800        05 RESP-KDINVKAT-UTSKR                                            
004900                             PIC Z9.                                      
005000*                                 INVENTERINGSKATEGORI                    
005100        05 RESP-FLINVSKR-UTSKR                                            
005200                             PIC X.                                       
005300*                                 INVENTERINGSANMODAN UTSKRIVEN           
005400        05 RESP-KDPRODSL-UTSKR                                            
005500                             PIC Z9.                                      
005600*                                 PRODUKTSLAG                             
005700        05 RESP-IDFKNGRP-UTSKR                                            
005800                             PIC Z(3)9.                                   
005900*                                 FUNKTIONSGRUPP                          
006000        05 RESP-TIREGDAT-UTSKR                                            
006100                             PIC X(6).                                    
006200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006300        05 RESP-LISTNR       PIC X(6).                                    
006400*** END OF VILMAII-COPY LENGTH= 20133 BYTES                               
