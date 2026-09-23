000100 01  MOD-W5O30201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5302              
000300*                                 INVENTERING                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-ADLAGOMR-IN      PIC X(2).                                    
001300*                                 LAGEROMRÅDE                             
001400     03 MOD-ADLAGOMR-UT      PIC X(2).                                    
001500*                                 LAGEROMRÅDE                             
001600     03 MOD-ADGANG-IN        PIC X(2).                                    
001700*                                 GÅNG                                    
001800     03 MOD-ADGANG-UT        PIC X(2).                                    
001900*                                 GÅNG                                    
002000     03 MOD-ADPLATS-IN       PIC X(5).                                    
002100*                                 LAGERPLATSNUMMER                        
002200     03 MOD-ADPLATS-UT       PIC X(5).                                    
002300*                                 LAGERPLATSNUMMER                        
002400     03 MOD-KDINVPRIO-IN     PIC X.                                       
002500*                                 INVENTERING PRIORITET                   
002600     03 MOD-KDINVPRIO-UT     PIC X.                                       
002700*                                 INVENTERING PRIORITET                   
002800     03 MOD-KDVVKL-IN        PIC X.                                       
002900*                                 VOLYMVÄRDESKLASS                        
003000     03 MOD-KDVVKL-UT        PIC X.                                       
003100*                                 VOLYMVÄRDESKLASS                        
003200     03 MOD-KDINVKAT-IN      PIC X(2).                                    
003300*                                 INVENTERINGSKATEGORI                    
003400     03 MOD-KDINVKAT-UT      PIC X(2).                                    
003500*                                 INVENTERINGSKATEGORI                    
003600     03 MOD-FLINVSKR-IN      PIC X.                                       
003700*                                 INVENTERINGSANMODAN UTSKRIVEN           
003800     03 MOD-FLINVSKR-UT      PIC X.                                       
003900*                                 INVENTERINGSANMODAN UTSKRIVEN           
004000     03 MOD-KDPRODSL-IN      PIC X(2).                                    
004100*                                 PRODUKTSLAG                             
004200     03 MOD-KDPRODSL-UT      PIC X(2).                                    
004300*                                 PRODUKTSLAG                             
004400     03 MOD-IDFKNGRP-IN      PIC X(4).                                    
004500*                                 FUNKTIONSGRUPP                          
004600     03 MOD-IDFKNGRP-UT      PIC X(4).                                    
004700*                                 FUNKTIONSGRUPP                          
004800     03 MOD-TIREGDAT-IN      PIC X(6).                                    
004900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005000     03 MOD-TIREGDAT-UT      PIC X(6).                                    
005100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005200     03 MOD-IDARTNR-IN       PIC X(9).                                    
005300*                                 ARTIKELNUMMER                           
005400     03 MOD-IDLISTNR-ATTR    PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDLISTNR-UT      PIC X.                                       
005700*                                 ALLMÄN FLAGGA                           
005800     03 MOD-KVINVSKR-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-KVINVSKR         PIC Z9.                                      
006100*                                 BEGÄRDA INVENTERINGSUNDERLAG            
006200     03 MOD-IDARTNR-UT       PIC X(9).                                    
006300*                                 ARTIKELNUMMER                           
006400     03 MOD-IDNODE-ATTR      PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-IDNODE-UT        PIC X(8).                                    
006700*                                 VTAM NODE-NAMN                          
006800     03 MOD-KVANT-ART        PIC Z(5).                                    
006900*                                 ANTAL               KVANTAL-003         
007000     03 MOD-FLURVAL-IN       PIC X.                                       
007100*                                 FLAGGA BEHANDLA HELA URVAET             
007200     03 MOD-FLURVAL-UT       PIC X.                                       
007300*                                 FLAGGA BEHANDLA HELA URVAET             
007400     03 MOD-HEADING          PIC X(5).                                    
007500     03 MOD-RAD-INFO         OCCURS 10 TIMES.                             
007600*                                 RAD-INFO                                
007700        05 MOD-ADLAGOMR-UTSKR                                             
007800                             PIC Z9.                                      
007900*                                 LAGEROMRÅDE                             
008000        05 MOD-ADGANG-UTSKR  PIC Z9.                                      
008100*                                 GÅNG                                    
008200        05 MOD-ADPLATS-UTSKR PIC Z(4)9.                                   
008300*                                 LAGERPLATSNUMMER                        
008400        05 MOD-FLAGGA-FLER   PIC X.                                       
008500*                                 ALLMÄN FLAGGA                           
008600        05 MOD-KDINVPRIO-UTSKR                                            
008700                             PIC Z.                                       
008800*                                 INVENTERING PRIORITET                   
008900        05 MOD-KDVVKL-UTSKR  PIC 9.                                       
009000*                                 VOLYMVÄRDESKLASS                        
009100        05 MOD-IDARTNR-UTSKR PIC Z(9).                                    
009200*                                 ARTIKELNUMMER                           
009300        05 MOD-KDINVKAT-UTSKR                                             
009400                             PIC Z9.                                      
009500*                                 INVENTERINGSKATEGORI                    
009600        05 MOD-FLINVSKR-UTSKR                                             
009700                             PIC X.                                       
009800*                                 INVENTERINGSANMODAN UTSKRIVEN           
009900        05 MOD-KDPRODSL-UTSKR                                             
010000                             PIC Z9.                                      
010100*                                 PRODUKTSLAG                             
010200        05 MOD-IDFKNGRP-UTSKR                                             
010300                             PIC Z(3)9.                                   
010400*                                 FUNKTIONSGRUPP                          
010500        05 MOD-TIREGDAT-UTSKR                                             
010600                             PIC 9(6).                                    
010700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010800        05 MOD-KDPSLLOC-UTSKR                                             
010900                             PIC 9(2).                                    
011000*                                 PRODUKTSLAG LOKALT                      
011100     03 MOD-TEMFSINF         PIC X(55).                                   
011200*                                 INFORMATIONSMEDDELANDE                  
011300*** END OF VILMAII-COPY LENGTH= 582 BYTES                                 
