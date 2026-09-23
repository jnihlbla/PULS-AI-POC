000100 01  W9O11201-CTX.                                                        
000200*                                 COPYTEXT FÖR MOD W9O11201               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 STRECK               PIC X.                                       
001200     03 REKSIFFR             PIC X.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 IDDISTR-IN           PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 IDDISTR-UT           PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 BLADDRING-ANT        PIC X(3).                                    
001900*                                                                         
002000     03 W9O11201-001-GRP.                                                 
002100*                                                                         
002200        05 PRARTBTO-EXP      PIC Z(6)9.9(2).                              
002300*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
002400        05 KDVALISO          PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600        05 BEART-ENG         PIC X(25).                                   
002700*                                 ARTIKELBENÄMNING                        
002800        05 KDARTRAB          PIC 9(2).                                    
002900*                                 RABATTKOD (ARTIKELPRIS)                 
003000        05 BEART-FRA         PIC X(25).                                   
003100*                                 ARTIKELBENÄMNING                        
003200        05 KDARTKAM          PIC Z(4)9.                                   
003300*                                 TRANSFER KOD                            
003400        05 BEART-SPA         PIC X(25).                                   
003500*                                 ARTIKELBENÄMNING                        
003600        05 KDPRODSL          PIC Z9.                                      
003700*                                 PRODUKTSLAG                             
003800        05 BEART-TYS         PIC X(25).                                   
003900*                                 ARTIKELBENÄMNING                        
004000        05 IDFKNGRP          PIC Z(3)9.                                   
004100*                                 FUNKTIONSGRUPP                          
004200        05 BEART-SVE         PIC X(25).                                   
004300*                                 ARTIKELBENÄMNING                        
004400        05 KVPOINT           PIC Z(7).                                    
004500*                                 POINT VALUE                             
004600        05 KDSORT            PIC X(2).                                    
004700*                                 SORT-KOD                                
004800        05 TIFINLV           PIC Z(5).                                    
004900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
005000        05 KDBPSR            PIC Z.                                       
005100*                                 BASLAGERFÖRSLAGSNIVÅ                    
005200        05 FILLERX1          PIC X.                                       
005300        05 KDSRA             PIC Z9.                                      
005400*                                 SRA-KOD                                 
005500        05 KDARTURS          PIC X(2).                                    
005600*                                 ARTIKELURSPRUNGSKOD                     
005700        05 KDUART            PIC X.                                       
005800*                                 UNDANTAGSARTIKEL                        
005900        05 KDIART            PIC X.                                       
006000*                                 INGÅR I SATS                            
006100        05 IDBERED           PIC Z(2)9.                                   
006200*                                 BEREDARENUMMER                          
006300        05 FLLSRDEL          PIC X.                                       
006400*                                 LEVERERAS SOM RESDEL                    
006500        05 IDSTATNR          PIC Z(8)9.                                   
006600*                                 STATISTISKT NUMMER                      
006700*                                 1 = NORSKT                              
006800*                                 2 = ENGELSKT                            
006900*                                 3 = BELGISKT                            
007000*                                 4 = PERUANSKT                           
007100*                                 5 = SVENSKT                             
007200*                                 6 =                                     
007300        05 KDFARLIG          PIC Z9.                                      
007400*                                                    KDFARLIG-002         
007500*                                 KOD FÖR FARLIGT GODS                    
007600        05 IDKAT             OCCURS 3 TIMES                               
007700                             INDEXED IND                                  
007800                             PIC X(5).                                    
007900*                                 KATALOGBETECKNING                       
008000        05 KDAGE             PIC X.                                       
008100*                                 AGE-CODE                                
008200        05 VKART             PIC Z(6)9.                                   
008300*                                 ARTIKELVIKT (G)                         
008400        05 VLARTNTO          PIC Z(7)9.9.                                 
008500*                                 ARTIKELVOLYM NETTO (CM3)                
008600        05 KVQPACK-0         PIC Z(4)9.                                   
008700*                                 ANTAL I Q0 FÖRPACKNING                  
008800        05 KDERS             PIC Z(2)9.                                   
008900*                                 ERSÄTTNINGSKOD                          
009000        05 BEEMBLEM          OCCURS 28 TIMES                              
009100                             INDEXED IX                                   
009200                             PIC X(5).                                    
009300*                                 EMBLEM                                  
009400        05 MORE              PIC X(4).                                    
009500*                                 FÖR BLÄDDRING.                          
009600        05 TIERSDAT          PIC 9(5).                                    
009700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
009800        05 IDARTNR-TILLK     PIC Z(8)9.                                   
009900*                                 TILLKOMMANDE ARTIKELNUMMER              
010000        05 STRECK2           PIC X.                                       
010100        05 REKSIFFR-TILLK    PIC 9.                                       
010200*                                 TILLKOMMANDE KONTROLLSIFFRA             
010300        05 TEARTNOT-3        PIC X(40).                                   
010400*                                 ARTIKEL NOTERING                        
010500        05 TEARTNOT-7        PIC X(40).                                   
010600*                                 ARTIKEL NOTERING                        
010700     03 TEMFSINF             PIC X(55).                                   
010800*                                 INFORMATIONSMEDDELANDE                  
010900*** END OF VILMAII-COPY LENGTH= 599 BYTES                                 
