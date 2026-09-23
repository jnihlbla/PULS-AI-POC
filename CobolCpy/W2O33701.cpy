000100 01  MOD-W2O33701.                                                        
000200*                                 MOD-COPYTEXT FÖR W20337                 
000300*                                 PARTS WITH DELIVERY BLOCK               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDSPRGRP-IN      PIC X(10).                                   
000900*                                 SPÄRRADE GRUPPER                        
001000     03 MOD-IDSPRGRP-UT      PIC X(10).                                   
001100*                                 SPÄRRADE GRUPPER                        
001200     03 MOD-IDARTNR-IN       PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-IDARTNR-UT       PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 MOD-TISTADAT-GRP-ATTR                                             
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-TISTADAT-GRP     PIC 9(6).                                    
002000*                                 GENERELLT STARTDATUM                    
002100     03 MOD-FLAUTUPD-GRP     PIC X(3).                                    
002200     03 MOD-KDARTURS-ART     PIC X(2).                                    
002300*                                 ARTIKELURSPRUNGSKOD                     
002400     03 MOD-KDPRODSL-ART     PIC Z9.                                      
002500*                                 PRODUKTSLAG                             
002600     03 MOD-IDFKNGRP-ART     PIC Z(3)9.                                   
002700*                                 FUNKTIONSGRUPP                          
002800     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
002900*                                 GRUPP MED TABELL RADER                  
003000        05 MOD-KDCMD-A-ATTR  PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-KDCMD-A       PIC X.                                       
003300*                                 RAD-UPPDATERINGSKOMMANDO                
003400*                                  BLANK  = INGENTING                     
003500*                                  D , B  = DELETE                        
003600*                                  R , Ä  = REPLACE                       
003700*                                  I,N,A  = INSERT                        
003800*                                  S , V  = SELECT                        
003900*                                  P , P  = PRINT                         
004000*                                  C , K  = COPY                          
004100        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-IDARTNR       PIC Z(8)9.                                   
004400*                                 ARTIKELNUMMER                           
004500        05 MOD-TISTADAT-A-ATTR                                            
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-TISTADAT-A    PIC X(6).                                    
004900*                                 GENERELLT STARTDATUM                    
005000        05 MOD-KDCMD-U-ATTR  PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KDCMD-U       PIC X.                                       
005300*                                 RAD-UPPDATERINGSKOMMANDO                
005400*                                  BLANK  = INGENTING                     
005500*                                  D , B  = DELETE                        
005600*                                  R , Ä  = REPLACE                       
005700*                                  I,N,A  = INSERT                        
005800*                                  S , V  = SELECT                        
005900*                                  P , P  = PRINT                         
006000*                                  C , K  = COPY                          
006100        05 MOD-KDARTURS-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-KDARTURS      PIC X(2).                                    
006400*                                 ARTIKELURSPRUNGSKOD                     
006500        05 MOD-TISTADAT-U-ATTR                                            
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-TISTADAT-U    PIC X(6).                                    
006900*                                 GENERELLT STARTDATUM                    
007000        05 MOD-KDCMD-P-ATTR  PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-KDCMD-P       PIC X.                                       
007300*                                 RAD-UPPDATERINGSKOMMANDO                
007400*                                  BLANK  = INGENTING                     
007500*                                  D , B  = DELETE                        
007600*                                  R , Ä  = REPLACE                       
007700*                                  I,N,A  = INSERT                        
007800*                                  S , V  = SELECT                        
007900*                                  P , P  = PRINT                         
008000*                                  C , K  = COPY                          
008100        05 MOD-KDPRODSL-FOM-ATTR                                          
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-KDPRODSL-FOM  PIC Z9.                                      
008500*                                 PRODUKTSLAG                             
008600        05 MOD-KDPRODSL-TOM-ATTR                                          
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-KDPRODSL-TOM  PIC Z9.                                      
009000*                                 PRODUKTSLAG                             
009100        05 MOD-TISTADAT-P-ATTR                                            
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400        05 MOD-TISTADAT-P    PIC X(6).                                    
009500*                                 GENERELLT STARTDATUM                    
009600        05 MOD-KDCMD-F-ATTR  PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-KDCMD-F       PIC X.                                       
009900        05 MOD-IDFKNGRP-FOM-ATTR                                          
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-IDFKNGRP-FOM  PIC Z(3)9.                                   
010300*                                 FUNKTIONSGRUPP                          
010400        05 MOD-IDFKNGRP-TOM-ATTR                                          
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-IDFKNGRP-TOM  PIC Z(3)9.                                   
010800*                                 FUNKTIONSGRUPP                          
010900        05 MOD-TISTADAT-F-ATTR                                            
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200        05 MOD-TISTADAT-F    PIC X(6).                                    
011300*                                 GENERELLT STARTDATUM                    
011400     03 MOD-KDCMD-E-ATTR     PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-KDCMD-E          PIC X.                                       
011700*                                 RAD-UPPDATERINGSKOMMANDO                
011800*                                  BLANK  = INGENTING                     
011900*                                  D , B  = DELETE                        
012000*                                  R , Ä  = REPLACE                       
012100*                                  I,N,A  = INSERT                        
012200*                                  S , V  = SELECT                        
012300*                                  P , P  = PRINT                         
012400*                                  C , K  = COPY                          
012500     03 MOD-IDARTNR-E-ATTR   PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-IDARTNR-E        PIC Z(8)9.                                   
012800*                                 ARTIKELNUMMER                           
012900     03 MOD-TISTADAT-A-E-ATTR                                             
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-TISTADAT-A-E     PIC X(6).                                    
013300*                                 GENERELLT STARTDATUM                    
013400     03 MOD-KDARTURS-E-ATTR  PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600     03 MOD-KDARTURS-E       PIC X(2).                                    
013700*                                 ARTIKELURSPRUNGSKOD                     
013800     03 MOD-TISTADAT-U-E-ATTR                                             
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-TISTADAT-U-E     PIC X(6).                                    
014200*                                 GENERELLT STARTDATUM                    
014300     03 MOD-KDPRODSL-FOM-E-ATTR                                           
014400                             PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600     03 MOD-KDPRODSL-FOM-E   PIC Z9.                                      
014700*                                 PRODUKTSLAG                             
014800     03 MOD-KDPRODSL-TOM-E-ATTR                                           
014900                             PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100     03 MOD-KDPRODSL-TOM-E   PIC Z9.                                      
015200*                                 PRODUKTSLAG                             
015300     03 MOD-TISTADAT-P-E-ATTR                                             
015400                             PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600     03 MOD-TISTADAT-P-E     PIC X(6).                                    
015700*                                 GENERELLT STARTDATUM                    
015800     03 MOD-IDFKNGRP-FOM-E-ATTR                                           
015900                             PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100     03 MOD-IDFKNGRP-FOM-E   PIC Z(3)9.                                   
016200*                                 FUNKTIONSGRUPP                          
016300     03 MOD-IDFKNGRP-TOM-E-ATTR                                           
016400                             PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 MOD-IDFKNGRP-TOM-E   PIC Z(3)9.                                   
016700*                                 FUNKTIONSGRUPP                          
016800     03 MOD-TISTADAT-F-E-ATTR                                             
016900                             PIC X(2).                                    
017000*                                 MFS ATTRIBUTFÄLT                        
017100     03 MOD-TISTADAT-F-E     PIC X(6).                                    
017200*                                 GENERELLT STARTDATUM                    
017300     03 MOD-TEMFSINF         PIC X(55).                                   
017400*                                 INFORMATIONSMEDDELANDE                  
017500*** END OF VILMAII-COPY LENGTH= 1174 BYTES                                
