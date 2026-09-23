000100 01  MOD-W2O35901-CTX.                                                    
000200*                                 COPYTEXT FÖR MOD W2I35901               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDARTNR-UT-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-IDDC-UT-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-BEART-ATTR       PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-BEART            PIC X(25).                                   
002600*                                 ARTIKELBENÄMNING                        
002700     03 MOD-IDDC-SEND-TO-ATTR                                             
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDDC-SEND-TO     PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MOD-KVRETUR-BEORD-ATTR                                            
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-KVRETUR-BEORD    PIC Z(7).                                    
003600*                                 ANTAL I RETUR                           
003700     03 MOD-TIRETUR-BEORD-ATTR                                            
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-TIRETUR-BEORD    PIC 9(6).                                    
004100*                                 RETURDATUM                              
004200     03 MOD-KVRETUR-TRANSFER-ATTR                                         
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-KVRETUR-TRANSFER PIC 9(6).                                    
004600*                                 ANTAL I RETUR                           
004700     03 MOD-KVSTOCK-ATTR     PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KVSTOCK          PIC Z(5)9.                                   
005000     03 MOD-KDORDKL-ATTR     PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KDORDKL          PIC X.                                       
005300*                                 ORDERKLASS                              
005400     03 MOD-KVAKS-PAV-ATTR   PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KVAKS-PAV        PIC Z(6)9.                                   
005700*                                 ANKOMSTSALDO                            
005800     03 MOD-KVAKS-SDC-ATTR   PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-KVAKS-SDC        PIC Z(6)9.                                   
006100*                                 ANKOMSTSALDO                            
006200     03 MOD-FLTOT-ATTR       PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-FLTOT            PIC X.                                       
006500*                                 ALLMÄN FLAGGA                           
006600     03 MOD-KVBEART-ATTR     PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KVBEART          PIC Z(5)9.                                   
006900*                                 BESTÄLLT ANTAL STYCKEN                  
007000     03 MOD-KDFRAKT-ATTR     PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-KDFRAKT          PIC Z9.                                      
007300*                                 FRAKTSÄTT DC TILL KUND                  
007400     03 MOD-KDERS-ATTR       PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-KDERS            PIC Z9.                                      
007700*                                 ERSÄTTNINGSKOD                          
007800     03 MOD-KVQPACK-1-ATTR   PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-KVQPACK-1        PIC Z(4)9.                                   
008100*                                 ANTAL I Q1 FÖRPACKNING                  
008200     03 MOD-TRANSFER-TEXT-1-ATTR                                          
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-TRANSFER-TEXT-1  PIC X(30).                                   
008600     03 MOD-TISKROT-AUTO-IN-ATTR                                          
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-TISKROT-AUTO-IN  PIC Z(6).                                    
009000*                                 STOPDATE AUTO-SKROTNING                 
009100     03 MOD-TISKROT-AUTO-UT-ATTR                                          
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-TISKROT-AUTO-UT  PIC Z(6).                                    
009500*                                 STOPDATE AUTO-SKROTNING                 
009600     03 MOD-TRANSFER-TEXT-2-ATTR                                          
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-TRANSFER-TEXT-2  PIC X(30).                                   
010000     03 MOD-KVSKROT-KVAR-ATTR                                             
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 MOD-KVSKROT-KVAR     PIC X(7).                                    
010400*                                 KVARLIGGANDE ANTAL                      
010500     03 MOD-VALUE-OF-SCRAP-QTY-ATTR                                       
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-VALUE-OF-SCRAP-QTY                                            
010900                             PIC Z(6)9.                                   
011000     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-IDKONTO          PIC Z(9)9.                                   
011300*                                 KONTO                                   
011400     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-IDANALYS         PIC X(12).                                   
011700*                                 ANALYSNUMMER                            
011800     03 MOD-TISKROT-ATTR     PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-TISKROT          PIC 9(6).                                    
012100*                                 SKROTNINGSDATUM                         
012200     03 MOD-KVSKROT-ATTR     PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-KVSKROT          PIC Z(6)9.                                   
012500*                                 ANTAL SENASTE SKROTORDER                
012600     03 MOD-SKROT-TEXT-ATTR  PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800     03 MOD-SKROT-TEXT       PIC X(20).                                   
012900     03 MOD-TISKROT-BEORD-ATTR                                            
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-TISKROT-BEORD    PIC 9(6).                                    
013300*                                 SKROTNINGSDATUM                         
013400     03 MOD-FLSKROT-AUTO-ATTR                                             
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 MOD-FLSKROT-AUTO     PIC X.                                       
013800*                                 SKROTNING AUTOMATISKT BEORDRAD          
013900     03 MOD-FLSKROT-BEORD-ATTR                                            
014000                             PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200     03 MOD-FLSKROT-BEORD    PIC X.                                       
014300*                                 SKROTNING BEORDRAD AV ANSK              
014400     03 MOD-TEMFSINF         PIC X(55).                                   
014500*                                 INFORMATIONSMEDDELANDE                  
014600*** END OF VILMAII-COPY LENGTH= 417 BYTES                                 
