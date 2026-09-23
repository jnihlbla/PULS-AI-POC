000100 01  RESP-WL0175O1.                                                       
000200*                                 RESPONS FROM PGM WL0175                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-ADLAGOMR-KEY    PIC 9(2).                                    
000600*                                 LAGEROMRÅDE                             
000700     03 RESP-ADGANG-KEY      PIC 9(2).                                    
000800*                                 GÅNG                                    
000900     03 RESP-ADPLATS-KEY     PIC 9(5).                                    
001000*                                 LAGERPLATSNUMMER                        
001100     03 RESP-KDINVPRIO-KEY   PIC 9.                                       
001200*                                 INVENTERING PRIORITET                   
001300     03 RESP-KDVVKL-KEY      PIC 9.                                       
001400*                                 VOLYMVÄRDESKLASS                        
001500     03 RESP-KDINVKAT-KEY    PIC Z9.                                      
001600*                                 INVENTERINGSKATEGORI                    
001700     03 RESP-FLINVSKR-KEY    PIC X.                                       
001800*                                 INVENTERINGSANMODAN UTSKRIVEN           
001900     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
002000*                                 ARTIKELNUMMER                           
002100     03 RESP-KDPRODSL-KEY    PIC Z9.                                      
002200*                                 PRODUKTSLAG                             
002300     03 RESP-IDFKNGRP-KEY    PIC Z(3)9.                                   
002400*                                 FUNKTIONSGRUPP                          
002500     03 RESP-DAREGDAT-KEY    PIC X(10).                                   
002600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002700     03 RESP-WL0173I1        OCCURS 10 TIMES.                             
002800*                                 ARTIKEL-PRINT GRUPP                     
002900        05 RESP-IDARTNR-L173 PIC 9(8).                                    
003000*                                 ARTIKELNUMMER                           
003100        05 RESP-KDINVPRIO-L173                                            
003200                             PIC 9.                                       
003300*                                 INVENTERING PRIORITET                   
003400        05 RESP-KDINVKAT-L173                                             
003500                             PIC 9(2).                                    
003600*                                 INVENTERINGSKATEGORI                    
003700     03 RESP-WL0174I1        OCCURS 10 TIMES.                             
003800*                                 ARTIKEL-PRINT GRUPP                     
003900        05 RESP-IDARTNR-L174 PIC 9(8).                                    
004000*                                 ARTIKELNUMMER                           
004100        05 RESP-KDINVPRIO-L174                                            
004200                             PIC 9.                                       
004300*                                 INVENTERING PRIORITET                   
004400        05 RESP-KDINVKAT-L174                                             
004500                             PIC 9(2).                                    
004600*                                 INVENTERINGSKATEGORI                    
004700     03 RESP-KVINVSKR-5302   PIC Z(2).                                    
004800*                                 BEGÄRDA INVENTERINGSUNDERLAG            
004900     03 RESP-FLBUFDOC        PIC X.                                       
005000*                                 ALLMÄN FLAGGA                           
005100     03 RESP-FLURVAL-IN      PIC X.                                       
005200*                                 FLAGGA BEHANDLA HELA URVAET             
005300     03 RESP-KVANT-ART       PIC Z(2)9.                                   
005400     03 RESP-KVRADER-MAX     PIC 9(5).                                    
005500*                                 MAX INDEX KOPPLAT TILL OCCURS N         
005600*                                 EDAN.                                   
005700     03 RESP-RAD-INFO        OCCURS 1 TO 500 TIMES                        
005800                             DEPENDING ON RESP-KVRADER-MAX.               
005900*                                 RAD-INFO                                
006000        05 RESP-ADLAGOMR-UTSKR                                            
006100                             PIC X(2).                                    
006200*                                 LAGEROMRÅDE                             
006300        05 RESP-ADGANG-UTSKR PIC X(2).                                    
006400*                                 GÅNG                                    
006500        05 RESP-ADPLATS-UTSKR                                             
006600                             PIC X(5).                                    
006700*                                 LAGERPLATSNUMMER                        
006800        05 RESP-KDINVPRIO-UTSKR                                           
006900                             PIC 9.                                       
007000*                                 INVENTERING PRIORITET                   
007100        05 RESP-KDVVKL-UTSKR PIC 9.                                       
007200*                                 VOLYMVÄRDESKLASS                        
007300        05 RESP-IDARTNR-UTSKR                                             
007400                             PIC Z(7)9.                                   
007500*                                 ARTIKELNUMMER                           
007600        05 RESP-KDINVKAT-UTSKR                                            
007700                             PIC Z9.                                      
007800*                                 INVENTERINGSKATEGORI                    
007900        05 RESP-FLINVSKR-UTSKR                                            
008000                             PIC X.                                       
008100*                                 INVENTERINGSANMODAN UTSKRIVEN           
008200        05 RESP-KDPRODSL-UTSKR                                            
008300                             PIC Z9.                                      
008400*                                 PRODUKTSLAG                             
008500        05 RESP-IDFKNGRP-UTSKR                                            
008600                             PIC Z(3)9.                                   
008700*                                 FUNKTIONSGRUPP                          
008800        05 RESP-DAREGDAT-UTSKR                                            
008900                             PIC X(10).                                   
009000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
009100        05 RESP-LISTNR-UTSKR PIC 9(6).                                    
009200*                                 LISTNUMMER                              
009300*** END OF VILMAII-COPY LENGTH= 22272 BYTES                               
