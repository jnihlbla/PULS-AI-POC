000100 01  MOD-W6O26301.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-STRECK           PIC X.                                       
001200     03 MOD-REKSIFFR         PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 MOD-GAMMAL-AREA.                                                  
001500*                                                                         
001600        05 MOD-ADLAGOMR-OLD-ATTR                                          
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-ADLAGOMR-OLD  PIC Z9.                                      
002000*                                 LAGEROMRÅDE                             
002100        05 MOD-ADGANG-OLD-ATTR                                            
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-ADGANG-OLD    PIC Z9.                                      
002500*                                 GÅNG                                    
002600        05 MOD-ADPLATS-OLD-ATTR                                           
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-ADPLATS-OLD   PIC Z(4)9.                                   
003000*                                 LAGERPLATSNUMMER                        
003100        05 MOD-VKART-OLD-ATTR                                             
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-VKART-OLD     PIC Z(6)9.                                   
003500*                                 ARTIKELVIKT (G)                         
003600        05 MOD-VLARTNTO-OLD-ATTR                                          
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-VLARTNTO-OLD  PIC Z(7)9.9.                                 
004000*                                 ARTIKELVOLYM NETTO (CM3)                
004100        05 MOD-KDVSOP-OLD-ATTR                                            
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KDVSOP-OLD    PIC X(3).                                    
004500*                                 VSOP-KOD                                
004600        05 MOD-KDSPEEMB-OLD-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KDSPEEMB-OLD  PIC 9.                                       
005000*                                 SPECIALEMBALLAGEKOD                     
005100        05 MOD-IDARTNR-EMBQ3-OLD-ATTR                                     
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-IDARTNR-EMBQ3-OLD                                          
005500                             PIC Z(8)9.                                   
005600*                                 EMBALLAGEARTIKELNR FÖR Q3               
005700        05 MOD-IDARTNR-EMBQ4-OLD-ATTR                                     
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-IDARTNR-EMBQ4-OLD                                          
006100                             PIC Z(8)9.                                   
006200*                                 EMBALLAGEARTIKELNR FÖR Q4               
006300        05 MOD-BUFF-ADR      OCCURS 3 TIMES.                              
006400           07 MOD-ADBUFFOMR-OLD-ATTR                                      
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700           07 MOD-ADBUFFOMR-OLD                                           
006800                             PIC Z9.                                      
006900*                                 BUFFERTOMRÅDE                           
007000           07 MOD-ADBUFFGANG-OLD-ATTR                                     
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300           07 MOD-ADBUFFGANG-OLD                                          
007400                             PIC Z9.                                      
007500*                                 BUFFERT GÅNG                            
007600           07 MOD-ADBUFFPL-OLD-ATTR                                       
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900           07 MOD-ADBUFFPL-OLD                                            
008000                             PIC Z(4)9.                                   
008100*                                 BUFFERPLATSNUMMER                       
008200     03 MOD-NY-AREA          OCCURS 9 TIMES.                              
008300        05 MOD-NEW-ATTR      PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-NEW           PIC X(2).                                    
008600*                                 MFS BEHANDLING AV INPUTFÄLT             
008700     03 MOD-ADINLOMR-PRT-ATTR                                             
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
009100*                                 PRINTERPLACERING                        
009200     03 MOD-TEMFSINF         PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
