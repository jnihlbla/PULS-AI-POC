000100 01  MOD-W6O16301.                                                        
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
001600        05 MOD-ADLAGOMR-CDC-OLD-ATTR                                      
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-ADLAGOMR-CDC-OLD                                           
002000                             PIC Z9.                                      
002100*                                 LAGEROMRÅDE                             
002200        05 MOD-ADGANG-CDC-OLD-ATTR                                        
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-ADGANG-CDC-OLD                                             
002600                             PIC Z9.                                      
002700*                                 GÅNG                                    
002800        05 MOD-ADPLATS-CDC-OLD-ATTR                                       
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-ADPLATS-CDC-OLD                                            
003200                             PIC Z(4)9.                                   
003300*                                 LAGERPLATSNUMMER                        
003400        05 MOD-KVMAXPL-OLD-ATTR                                           
003500                             PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-KVMAXPL-OLD   PIC Z(5)9.                                   
003800*                                 MAX ANTAL (STYCK) PÅ PLOCKPLATS         
003900        05 MOD-KVQPACK-3     PIC -(5)9.                                   
004000*                                 ANTAL I Q3 FÖRPACKNING                  
004100        05 MOD-KVREFPKT-PLOCK                                             
004200                             PIC Z(6)9.                                   
004300*                                 BERÄKNAD PÅFYLLNADSPUNKT-PLOCK          
004400        05 MOD-CDPLATS       OCCURS 4 TIMES.                              
004500*                                                                         
004600           07 MOD-ADLAGOMR-CD-OLD-ATTR                                    
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900           07 MOD-ADLAGOMR-CD-OLD                                         
005000                             PIC Z9.                                      
005100*                                 LAGEROMRÅDE                             
005200           07 MOD-ADGANG-CD-OLD-ATTR                                      
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500           07 MOD-ADGANG-CD-OLD                                           
005600                             PIC Z9.                                      
005700*                                 GÅNG                                    
005800           07 MOD-ADPLATS-CD-OLD-ATTR                                     
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100           07 MOD-ADPLATS-CD-OLD                                          
006200                             PIC Z(4)9.                                   
006300*                                 LAGERPLATSNUMMER                        
006400        05 MOD-VKART-OLD-ATTR                                             
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-VKART-OLD     PIC Z(6)9.                                   
006800*                                 ARTIKELVIKT (G)                         
006900        05 MOD-VLARTNTO-OLD-ATTR                                          
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-VLARTNTO-OLD  PIC Z(7)9.9.                                 
007300*                                 ARTIKELVOLYM NETTO (CM3)                
007400        05 MOD-KDVSOP-OLD-ATTR                                            
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 MOD-KDVSOP-OLD    PIC X(3).                                    
007800*                                 VSOP-KOD                                
007900        05 MOD-KDSPEEMB-OLD-ATTR                                          
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-KDSPEEMB-OLD  PIC 9.                                       
008300*                                 SPECIALEMBALLAGEKOD                     
008400        05 MOD-IDARTNR-EMBQ3-OLD-ATTR                                     
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 MOD-IDARTNR-EMBQ3-OLD                                          
008800                             PIC Z(8)9.                                   
008900*                                 EMBALLAGEARTIKELNR FÖR Q3               
009000        05 MOD-IDARTNR-EMBQ4-OLD-ATTR                                     
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-IDARTNR-EMBQ4-OLD                                          
009400                             PIC Z(8)9.                                   
009500*                                 EMBALLAGEARTIKELNR FÖR Q4               
009600        05 MOD-FLEJBUFF-OLD-ATTR                                          
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900        05 MOD-FLEJBUFF-OLD  PIC X.                                       
010000*                                 EJ BUFFERTSTYRNING                      
010100        05 MOD-ADINLOMR-BOA-OLD-ATTR                                      
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-ADINLOMR-BOA-OLD                                           
010500                             PIC X(4).                                    
010600*                                 BUFFERTOMRÅDE-ALTERNATIVT               
010700        05 MOD-BUFF-ADR      OCCURS 3 TIMES.                              
010800           07 MOD-ADBUFFOMR-OLD-ATTR                                      
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100           07 MOD-ADBUFFOMR-OLD                                           
011200                             PIC Z9.                                      
011300*                                 BUFFERTOMRÅDE                           
011400           07 MOD-ADBUFFGANG-OLD-ATTR                                     
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700           07 MOD-ADBUFFGANG-OLD                                          
011800                             PIC Z9.                                      
011900*                                 BUFFERT GÅNG                            
012000           07 MOD-ADBUFFPL-OLD-ATTR                                       
012100                             PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300           07 MOD-ADBUFFPL-OLD                                            
012400                             PIC Z(4)9.                                   
012500*                                 BUFFERPLATSNUMMER                       
012600     03 MOD-NY-AREA          OCCURS 26 TIMES.                             
012700        05 MOD-NEW-ATTR      PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900        05 MOD-NEW           PIC X(2).                                    
013000*                                 MFS BEHANDLING AV INPUTFÄLT             
013100     03 MOD-ADINLOMR-PRT-ATTR                                             
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
013500*                                 PRINTERPLACERING                        
013600     03 MOD-TEMFSINF         PIC X(55).                                   
013700*                                 INFORMATIONSMEDDELANDE                  
013800*** END OF VILMAII-COPY LENGTH= 430 BYTES                                 
