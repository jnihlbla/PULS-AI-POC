000100 01  MOD-W4O67601.                                                        
000200*                                 MODCOPYTEXT TILL W40676.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTRPTNR-IN      PIC X(3).                                    
000800*                                 TRANSPORTIDENTITET                      
000900     03 MOD-IDTRPTNR-UT      PIC X(3).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 MOD-IDLBBET-IN       PIC X(12).                                   
001200*                                 LASTBÄRARBETECKNING                     
001300     03 MOD-IDLBBET-UT       PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500     03 MOD-FLFARLIG-IN      PIC X.                                       
001600*                                 FARLIGT GODS-FLAGGA                     
001700     03 MOD-FLFARLIG-UT      PIC X.                                       
001800*                                 FARLIGT GODS-FLAGGA                     
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-IDDISTR          PIC Z(3)9.                                   
002400*                                 DISTRIKTNUMMER                          
002500     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700     03 MOD-FLSEPINV-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLSEPINV         PIC X.                                       
003000*                                 SEPARAT FAKTURA                         
003100     03 MOD-CURRENCY-MC-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-CURRENCY-MC      PIC X(11).                                   
003400     03 MOD-KDVALISO-MC-ATTR PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-KDVALISO-MC      PIC X(3).                                    
003700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003800     03 MOD-RADER.                                                        
003900*                                                                         
004000        05 MOD-PRFRAKT-ATTR  PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-PRFRAKT       PIC Z(6)9.9(2).                              
004300*                                 FRAKTKOSTNAD                            
004400        05 MOD-FLFRAKT-UPD-ATTR                                           
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-FLFRAKT-UPD   PIC X.                                       
004800        05 MOD-PRFOERS-ATTR  PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-PRFOERS       PIC Z(6)9.9(2).                              
005100*                                 FÖRSÄKRINGSPREMIE                       
005200        05 MOD-REFOERS-ATTR  PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-REFOERS       PIC Z9.9(3).                                 
005500*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
005600        05 MOD-REOVKOFF-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-REOVKOFF      PIC Z9.9.                                    
005900*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
006000        05 MOD-PRLEGKST-ATTR PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-PRLEGKST      PIC Z(6)9.9(2).                              
006300*                                 LEGALISERINSKOSTNAD                     
006400        05 MOD-RELEGKST-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-RELEGKST      PIC Z9.9.                                    
006700*                                 LEGALISERINGSKOSTNAD PROCENT            
006800        05 MOD-PREMBHNT-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-PREMBHNT      PIC Z(6)9.9(2).                              
007100*                                 EMBALLAGE O HANTERINGSKOST              
007200        05 MOD-REEMBHNT-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-REEMBHNT      PIC Z9.9.                                    
007500*                                 EMB OCH HANTERINGSKOST (%)              
007600        05 MOD-PRAVDRAG-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-PRAVDRAG      PIC Z(6)9.9(2).                              
007900*                                 AVDRAGSBELOPP                           
008000        05 MOD-REAVDRAG-ATTR PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-REAVDRAG      PIC Z9.9.                                    
008300*                                 AVDRAGSPROCENT                          
008400        05 MOD-FLCREDL-ATTR  PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-FLCREDL       PIC X.                                       
008700        05 MOD-KDVALUTA-MAN-ATTR                                          
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-KDVALUTA-MAN  PIC 9(2).                                    
009100*                                 VALUTAKOD MANUELL                       
009200        05 MOD-KDVALISO-MAN  PIC X(3).                                    
009300*                                 VALUTAKOD ENLIGT ISO-ST, ANGIVE         
009400*                                 N AV ANVÄNDARE                          
009500        05 MOD-PRKURS-MAN-ATTR                                            
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-PRKURS-MAN    PIC Z(5)9.9(4).                              
009900*                                 VALUTAKURS ANGIVEN AV ANVÄNDARE         
010000        05 MOD-IDSIGILL-ATTR PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-IDSIGILL      PIC X(15).                                   
010300*                                 FÄRDVÄG, DESTINATION                    
010400        05 MOD-TISKEPPN-MAN-ATTR                                          
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-TISKEPPN-MAN  PIC 9(6).                                    
010800*                                 SKEPPNINGSDATUM MAN  (ÅÅMMDD)           
010900        05 MOD-IDLC-ATTR     PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-IDLC          PIC X(15).                                   
011200*                                 LC-NUMMER                               
011300        05 MOD-IDLICENS-ATTR PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500        05 MOD-IDLICENS      PIC X(15).                                   
011600*                                 LICENSNUMMER                            
011700        05 MOD-IDBOKN-ATTR   PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900        05 MOD-IDBOKN        PIC X(15).                                   
012000*                                 BOKNINGSNUMMER                          
012100        05 MOD-IDVCERT-ATTR  PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300        05 MOD-IDVCERT       PIC X(16).                                   
012400*                                 VARUCERTIFIKATNUMMER                    
012500        05 MOD-BESLULEV-ATTR PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700        05 MOD-BESLULEV      PIC X(5).                                    
012800*                                 SLUTLEVERANS                            
012900        05 MOD-KDLEVVIL-ATTR PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100        05 MOD-KDLEVVIL      PIC 9.                                       
013200*                                 LEVERANSVILLKOR                         
013300        05 MOD-VKORDBTO-ATTR PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500        05 MOD-VKORDBTO      PIC Z(5)9.9.                                 
013600*                                 ORDERVIKT BRUTTO (KG)                   
013700        05 MOD-VLORDBTO-ATTR PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900        05 MOD-VLORDBTO      PIC Z(3)9.9(3).                              
014000*                                 ORDERVOLYM BRUTTO (M3)                  
014100        05 MOD-SUORDV-ATTR   PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300        05 MOD-SUORDV        PIC Z(8)9.9(2).                              
014400*                                 SUMMA ORDERVÄRDE                        
014500     03 MOD-TEMFSINF         PIC X(55).                                   
014600*                                 INFORMATIONSMEDDELANDE                  
014700*** END OF VILMAII-COPY LENGTH= 447 BYTES                                 
