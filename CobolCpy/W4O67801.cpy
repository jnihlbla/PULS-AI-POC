000100 01  MOD-W4O67801.                                                        
000200*                                 MODCOPYTEXT TILL W40678.                
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
004400        05 MOD-PRFOERS-ATTR  PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-PRFOERS       PIC Z(6)9.9(2).                              
004700*                                 FÖRSÄKRINGSPREMIE                       
004800        05 MOD-REFOERS-ATTR  PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-REFOERS       PIC Z9.9(3).                                 
005100*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
005200        05 MOD-REOVKOFF-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-REOVKOFF      PIC Z9.9.                                    
005500*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
005600        05 MOD-PRLEGKST-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-PRLEGKST      PIC Z(6)9.9(2).                              
005900*                                 LEGALISERINSKOSTNAD                     
006000        05 MOD-RELEGKST-ATTR PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-RELEGKST      PIC Z9.9.                                    
006300*                                 LEGALISERINGSKOSTNAD PROCENT            
006400        05 MOD-PREMBHNT-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-PREMBHNT      PIC Z(6)9.9(2).                              
006700*                                 EMBALLAGE O HANTERINGSKOST              
006800        05 MOD-REEMBHNT-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-REEMBHNT      PIC Z9.9.                                    
007100*                                 EMB OCH HANTERINGSKOST (%)              
007200        05 MOD-PRAVDRAG-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-PRAVDRAG      PIC Z(6)9.9(2).                              
007500*                                 AVDRAGSBELOPP                           
007600        05 MOD-REAVDRAG-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-REAVDRAG      PIC Z9.9.                                    
007900*                                 AVDRAGSPROCENT                          
008000        05 MOD-VKORDBTO-ATTR PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-VKORDBTO      PIC Z(5)9.9.                                 
008300*                                 ORDERVIKT BRUTTO (KG)                   
008400        05 MOD-VLORDBTO-ATTR PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-VLORDBTO      PIC Z(3)9.9(3).                              
008700*                                 ORDERVOLYM BRUTTO (M3)                  
008800        05 MOD-SUORDV-ATTR   PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-SUORDV        PIC Z(8)9.9(2).                              
009100*                                 SUMMA ORDERVÄRDE                        
009200     03 MOD-TEMFSINF         PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
009400*** END OF VILMAII-COPY LENGTH= 292 BYTES                                 
