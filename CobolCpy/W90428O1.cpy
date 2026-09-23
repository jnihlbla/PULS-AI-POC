000100 01  MOD-W90428O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9042800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 FILLER               PIC X(9).                                    
001000     03 MOD-IDPRODNR-LO      PIC 9(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDPRODNR-HI      PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-BELEV-LO         PIC X(30).                                   
001500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001600     03 MOD-BEART-SVE        PIC X(25).                                   
001700*                                 SVENSK ARTIKELBENÄMNING                 
001800     03 MOD-IDDISTR-RENOV    PIC Z(4)9.                                   
001900*                                 DISTRIKTNUMMER                          
002000     03 MOD-IDDISTR-NDC      PIC Z(4)9.                                   
002100*                                 DISTRIKTNUMMER                          
002200     03 MOD-IDDISTR-CAN      PIC Z(4)9.                                   
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-IDDISTR-PAC      PIC Z(4)9.                                   
002500*                                 DISTRIKTNUMMER                          
002600     03 MOD-IDDISTR-AUS      PIC Z(4)9.                                   
002700*                                 DISTRIKTNUMMER                          
002800     03 MOD-IDDISTR-CHN      PIC Z(4)9.                                   
002900*                                 DISTRIKTNUMMER                          
003000     03 MOD-IDDISTR-KOR      PIC Z(4)9.                                   
003100*                                 DISTRIKTNUMMER                          
003200     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
003300*                                 RADINFORMATION                          
003400        05 MOD-TEBYTNOT-ATTR PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-TEBYTNOT      PIC X(30).                                   
003700*                                 BYTES ARTIKEL NOTERING                  
003800     03 MOD-BETFLEV-ATTR     PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-BETFLEV          PIC X(30).                                   
004100*                                 TILLFÄLLIG LEVERANTÖR                   
004200     03 MOD-BELEV            OCCURS 6 TIMES                               
004300                             PIC X(30).                                   
004400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004500     03 MOD-IDARTNR          OCCURS 18 TIMES                              
004600                             PIC Z(8)9.                                   
004700*                                 ARTIKELNUMMER                           
004800     03 MOD-UPPDATERINGSSORT-ATTR                                         
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-UPPDATERINGSSORT PIC X.                                       
005200     03 MOD-IDPRODNR-IN-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-IDPRODNR-IN      PIC X(9).                                    
005500*                                 ARTIKELNUMMER                           
005600     03 MOD-IDDISTR-RENOV-IN-ATTR                                         
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-IDDISTR-RENOV-IN PIC X(5).                                    
006000*                                 DISTRIKTNUMMER                          
006100     03 MOD-IDDISTR-NDC-IN-ATTR                                           
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-IDDISTR-NDC-IN   PIC X(5).                                    
006500*                                 DISTRIKTNUMMER                          
006600     03 MOD-IDDISTR-CAN-IN-ATTR                                           
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-IDDISTR-CAN-IN   PIC X(5).                                    
007000*                                 DISTRIKTNUMMER                          
007100     03 MOD-IDDISTR-PAC-IN-ATTR                                           
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-IDDISTR-PAC-IN   PIC X(5).                                    
007500*                                 DISTRIKTNUMMER                          
007600     03 MOD-IDDISTR-AUS-IN-ATTR                                           
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 MOD-IDDISTR-AUS-IN   PIC X(5).                                    
008000*                                 DISTRIKTNUMMER                          
008100     03 MOD-IDDISTR-CHN-IN-ATTR                                           
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-IDDISTR-CHN-IN   PIC X(5).                                    
008500*                                 DISTRIKTNUMMER                          
008600     03 MOD-IDDISTR-KOR-IN-ATTR                                           
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-IDDISTR-KOR-IN   PIC X(5).                                    
009000*                                 DISTRIKTNUMMER                          
009100     03 MOD-BELEV-IN-ATTR    PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 MOD-BELEV-IN         PIC X(30).                                   
009400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
009500     03 MOD-BELEV-HI         PIC X(30).                                   
009600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
009700     03 MOD-TEMFSINF         PIC X(55).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END OF VILMAII-COPY LENGTH= 845 BYTES                                 
