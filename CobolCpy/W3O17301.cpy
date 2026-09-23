000100 01  MOD-W3O17301.                                                        
000200*                                 MOD-COPYTEXT FÖR W3017300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDBYTRAP-IN      PIC X(7).                                    
001200*                                 RAPPORTNUMMER  BYTES                    
001300     03 MOD-IDBYTRAP-UT      PIC X(7).                                    
001400*                                 RAPPORTNUMMER  BYTES                    
001500     03 MOD-KDPRT-ATTR       PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-KDPRT            PIC X(3).                                    
001800*                                 PRINTERKOD                              
001900     03 MOD-IDARTNR-IN       PIC X(9).                                    
002000*                                 OBJEKTNUMMER                            
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 OBJEKTNUMMER                            
002300     03 MOD-IDTABNR-IN       PIC X(3).                                    
002400*                                 TABELLNUMMER                            
002500     03 MOD-IDTABNR-UT       PIC X(3).                                    
002600*                                 TABELLNUMMER                            
002700     03 MOD-IDBYTRAD-IN      PIC X(5).                                    
002800*                                 RADNUMMER                               
002900     03 MOD-IDBYTRAD-UT      PIC X(5).                                    
003000*                                 RADNUMMER                               
003100     03 MOD-IDPRODNR-LO      PIC 9(9).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDPRODNR-HI      PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500     03 MOD-BELEV-LO         PIC X(30).                                   
003600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003700     03 MOD-BEART-SVE        PIC X(25).                                   
003800*                                 SVENSK ARTIKELBENÄMNING                 
003900     03 MOD-KDPRODSL         PIC Z9.                                      
004000*                                 PRODUKTSLAG                             
004100     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
004200*                                 FUNKTIONSGRUPP                          
004300     03 MOD-IDDISTR-RENOV-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDDISTR-RENOV    PIC Z(3)9.                                   
004700*                                 DISTRIKTNUMMER                          
004800     03 MOD-KVBYTPKO         PIC 9(3).                                    
004900*                                 ANTAL BYTESOBJEKT PER PALL              
005000     03 MOD-ADLAGOMR         PIC Z(2).                                    
005100*                                 LAGEROMRÅDE                             
005200     03 MOD-ADGANG           PIC Z(2).                                    
005300*                                 GÅNG                                    
005400     03 MOD-ADPLATS          PIC Z(5).                                    
005500*                                 LAGERPLATSNUMMER                        
005600     03 MOD-KVLS-ATTR        PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-KVLS             PIC -(7)9.                                   
005900*                                 LAGERSALDO                              
006000     03 MOD-KVLS-MAXCORE     PIC -(7)9.                                   
006100*                                 LAGERSALDO MAXCORE                      
006200     03 MOD-BETFLEV-ATTR     PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-BETFLEV          PIC X(30).                                   
006500*                                 TILLFÄLLIG LEVERANTÖR                   
006600     03 MOD-DELAR-SAKNAS-ATTR                                             
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-DELAR-SAKNAS     PIC X(30).                                   
007000     03 MOD-TEBYTKVA1        PIC X(75).                                   
007100*                                 KVALITETSNOTERING BYTESOBJEKT           
007200     03 MOD-TEBYTKVA2        PIC X(75).                                   
007300*                                 KVALITETSNOTERING BYTESOBJEKT           
007400     03 MOD-TEBYTKVA3        PIC X(75).                                   
007500*                                 KVALITETSNOTERING BYTESOBJEKT           
007600     03 MOD-TEBYTKVA4        PIC X(75).                                   
007700*                                 KVALITETSNOTERING BYTESOBJEKT           
007800     03 MOD-BELEV            OCCURS 4 TIMES                               
007900                             PIC X(30).                                   
008000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
008100     03 MOD-IDARTNR          OCCURS 12 TIMES                              
008200                             PIC Z(7)9.                                   
008300*                                 ARTIKELNUMMER                           
008400     03 MOD-BELEV-HI         PIC X(30).                                   
008500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
008600     03 MOD-REG-FLAGGA-ATTR  PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-REG-FLAGGA       PIC X.                                       
008900*                                 ALLMÄN FLAGGA                           
009000     03 MOD-TEMFSINF         PIC X(55).                                   
009100*                                 INFORMATIONSMEDDELANDE                  
009200*** END OF VILMAII-COPY LENGTH= 888 BYTES                                 
