000100 01  MOD-W4O35601.                                                        
000200*                                 COPYTEXT FOR MOD W4O35601               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR7-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDORDNR7-UT      PIC X(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-ADLAGOMR-IN      PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-ADLAGOMR-UT      PIC X(2).                                    
002200*                                 LAGEROMRÅDE                             
002300     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDPRODNR-UT      PIC X(7).                                    
002600*                                 PRODUKTIONSNUMMER                       
002700     03 MOD-FLEKOD-IN        PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-FLEKOD-UT        PIC X.                                       
003000*                                 JA/NEJ-FLAGGA                           
003100     03 MOD-IDARTNR-IN       PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300     03 MOD-IDARTNR-UT       PIC X(9).                                    
003400*                                 ARTIKELNUMMER                           
003500     03 MOD-IDDC-IN          PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700     03 MOD-IDDC-UT          PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900     03 MOD-IDARTNR-ENTER    PIC X(9).                                    
004000*                                 ARTIKELNUMMER                           
004100     03 MOD-IDLOPNR-ENTER    PIC 9(3).                                    
004200*                                 LÖPNUMMER                               
004300     03 MOD-ADLAGOMR-ENTER   PIC X(2).                                    
004400*                                 LAGEROMRÅDE                             
004500     03 MOD-IDARTNR-NEXT     PIC X(9).                                    
004600*                                 ARTIKELNUMMER                           
004700     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
004800*                                 LÖPNUMMER                               
004900     03 MOD-ADLAGOMR-NEXT    PIC X(2).                                    
005000*                                 LAGEROMRÅDE                             
005100     03 MOD-IDORDER-SPAR     PIC X(7).                                    
005200*                                 VOLVO PARTS ORDERNUMMER                 
005300     03 MOD-RAD              OCCURS 14 TIMES.                             
005400*                                 LINES                                   
005500        05 MOD-IDSPECEMB-RAD-ATTR                                         
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-IDSPECEMB-RAD PIC Z(3)9.                                   
005900*                                 SPECIALEMBALLAGEID                      
006000        05 MOD-IDSPECEMB-NY-ATTR                                          
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDSPECEMB-NY  PIC Z(3)9.                                   
006400*                                 SPECIALEMBALLAGEID                      
006500        05 MOD-IDARTNR-RAD-ATTR                                           
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-IDARTNR-RAD   PIC Z(8)9.                                   
006900*                                 ARTIKELNUMMER                           
007000        05 MOD-IDLOPNR-RAD-ATTR                                           
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-IDLOPNR-RAD   PIC Z(2)9.                                   
007400*                                 LÖPNUMMER                               
007500        05 MOD-KDSPEEMB-RAD-ATTR                                          
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KDSPEEMB-RAD  PIC 9.                                       
007900*                                 SPECIALEMBALLAGEKOD                     
008000        05 MOD-ADLAGOMR-RAD-ATTR                                          
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-ADLAGOMR-RAD  PIC Z9.                                      
008400*                                 LAGEROMRÅDE                             
008500        05 MOD-ADGANG-RAD-ATTR                                            
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-ADGANG-RAD    PIC Z9.                                      
008900*                                 GÅNG                                    
009000        05 MOD-ADPLATS-RAD-ATTR                                           
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-ADPLATS-RAD   PIC Z(4)9.                                   
009400*                                 LAGERPLATSNUMMER                        
009500        05 MOD-KDARTURS-RAD-ATTR                                          
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-KDARTURS-RAD  PIC X(2).                                    
009900*                                 ARTIKELURSPRUNGSKOD                     
010000        05 MOD-KVBEART-RAD-ATTR                                           
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300        05 MOD-KVBEART-RAD   PIC Z(6).                                    
010400*                                 BESTÄLLT ANTAL STYCKEN                  
010500        05 MOD-BEART-RAD-ATTR                                             
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-BEART-RAD     PIC X(25).                                   
010900*                                 ARTIKELBENÄMNING                        
011000     03 MOD-ADPLATS-Q401     OCCURS 14 TIMES.                             
011100*                                 INPUT                                   
011200        05 MOD-ADPLATS-Q4    PIC 9(5).                                    
011300*                                 LAGERPLATSNUMMER                        
011400     03 MOD-TEMFSINF         PIC X(55).                                   
011500*                                 INFORMATIONSMEDDELANDE                  
011600*** END OF VILMAII-COPY LENGTH= 1448 BYTES                                
