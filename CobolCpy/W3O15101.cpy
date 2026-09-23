000100 01  MOD-W3O15101.                                                        
000200*                                 MOD-COPYTEXT FÖR W3015100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDPRODNR-LO      PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDPRODNR-HI      PIC 9(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-BELEV-LO         PIC X(30).                                   
001600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001700     03 MOD-BEART-SVE        PIC X(25).                                   
001800*                                 SVENSK ARTIKELBENÄMNING                 
001900     03 MOD-IDDISTR-RENOV    PIC Z(4)9.                                   
002000*                                 DISTRIKTNUMMER                          
002100     03 MOD-IDDISTR-NDC      PIC Z(4)9.                                   
002200*                                 DISTRIKTNUMMER                          
002300     03 MOD-IDDISTR-CAN      PIC Z(4)9.                                   
002400*                                 DISTRIKTNUMMER                          
002500     03 MOD-IDDISTR-PAC      PIC Z(4)9.                                   
002600*                                 DISTRIKTNUMMER                          
002700     03 MOD-IDDISTR-AUS      PIC Z(4)9.                                   
002800*                                 DISTRIKTNUMMER                          
002900     03 MOD-IDDISTR-CHN      PIC Z(4)9.                                   
003000*                                 DISTRIKTNUMMER                          
003100     03 MOD-IDDISTR-KOR      PIC Z(4)9.                                   
003200*                                 DISTRIKTNUMMER                          
003300     03 MOD-IDDISTR-MY       PIC Z(4)9.                                   
003400*                                 DISTRIKTNUMMER                          
003500     03 MOD-IDDISTR-TW       PIC Z(4)9.                                   
003600*                                 DISTRIKTNUMMER                          
003700     03 MOD-IDDISTR-TH       PIC Z(4)9.                                   
003800*                                 DISTRIKTNUMMER                          
003900     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
004000*                                 RADINFORMATION                          
004100        05 MOD-TEBYTNOT-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-TEBYTNOT      PIC X(30).                                   
004400*                                 BYTES ARTIKEL NOTERING                  
004500     03 MOD-BETFLEV-ATTR     PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-BETFLEV          PIC X(30).                                   
004800*                                 TILLFÄLLIG LEVERANTÖR                   
004900     03 MOD-KVLS             PIC -(7)9.                                   
005000*                                 LAGERSALDO                              
005100     03 MOD-KVLS-MAXCORE-UT  PIC -(7)9.                                   
005200*                                 LAGERSALDO MAXCORE                      
005300     03 MOD-KVLS-MAXCORE-IN-ATTR                                          
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KVLS-MAXCORE-IN  PIC -(7)9.                                   
005700*                                 LAGERSALDO MAXCORE                      
005800     03 MOD-BELEV            OCCURS 6 TIMES                               
005900                             PIC X(30).                                   
006000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006100     03 MOD-IDARTNR          OCCURS 18 TIMES                              
006200                             PIC Z(8)9.                                   
006300*                                 ARTIKELNUMMER                           
006400     03 MOD-UPPDATERINGSSORT-ATTR                                         
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-UPPDATERINGSSORT PIC X.                                       
006800     03 MOD-IDPRODNR-IN-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-IDPRODNR-IN      PIC X(9).                                    
007100*                                 ARTIKELNUMMER                           
007200     03 MOD-IDDISTR-RENOV-IN-ATTR                                         
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-IDDISTR-RENOV-IN PIC X(5).                                    
007600*                                 DISTRIKTNUMMER                          
007700     03 MOD-IDDISTR-NDC-IN-ATTR                                           
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-IDDISTR-NDC-IN   PIC X(5).                                    
008100*                                 DISTRIKTNUMMER                          
008200     03 MOD-IDDISTR-CAN-IN-ATTR                                           
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-IDDISTR-CAN-IN   PIC X(5).                                    
008600*                                 DISTRIKTNUMMER                          
008700     03 MOD-IDDISTR-PAC-IN-ATTR                                           
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDDISTR-PAC-IN   PIC X(5).                                    
009100*                                 DISTRIKTNUMMER                          
009200     03 MOD-IDDISTR-AUS-IN-ATTR                                           
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-IDDISTR-AUS-IN   PIC X(5).                                    
009600*                                 DISTRIKTNUMMER                          
009700     03 MOD-IDDISTR-CHN-IN-ATTR                                           
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-IDDISTR-CHN-IN   PIC X(5).                                    
010100*                                 DISTRIKTNUMMER                          
010200     03 MOD-IDDISTR-KOR-IN-ATTR                                           
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-IDDISTR-KOR-IN   PIC X(5).                                    
010600*                                 DISTRIKTNUMMER                          
010700     03 MOD-IDDISTR-MY-IN-ATTR                                            
010800                             PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000     03 MOD-IDDISTR-MY-IN    PIC X(5).                                    
011100*                                 DISTRIKTNUMMER                          
011200     03 MOD-IDDISTR-TW-IN-ATTR                                            
011300                             PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-IDDISTR-TW-IN    PIC X(5).                                    
011600*                                 DISTRIKTNUMMER                          
011700     03 MOD-IDDISTR-TH-IN-ATTR                                            
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-IDDISTR-TH-IN    PIC X(5).                                    
012100*                                 DISTRIKTNUMMER                          
012200     03 MOD-BELEV-HI         PIC X(30).                                   
012300*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
012400     03 MOD-BELEV-IN-ATTR    PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600     03 MOD-BELEV-IN         PIC X(30).                                   
012700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
012800     03 MOD-TEMFSINF         PIC X(55).                                   
012900*                                 INFORMATIONSMEDDELANDE                  
013000*** END OF VILMAII-COPY LENGTH= 907 BYTES                                 
