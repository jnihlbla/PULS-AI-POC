000100 01  MOD-W4O41701.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-KDFRAKT-IN       PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MOD-KDFRAKT-UT       PIC X(2).                                    
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500     03 MOD-IDDISTR-IN       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-INFO-RAD         OCCURS 11 TIMES.                             
002400*                                 RADINFORMATION                          
002500        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700        05 MOD-KDTRPKAT      PIC X.                                       
002800*                                 TRANSPORTKATEGORI                       
002900        05 MOD-IDTRP-0.                                                   
003000*                                 TRANSPORTIDENTITET KLASS 0              
003100           07 MOD-IDTRPLOS-0 PIC X(3).                                    
003200*                                 TRANSPORTLÖSNING                        
003300           07 MOD-IDTRPVAR-0 PIC X(2).                                    
003400*                                 TRANSPORTLÖSNINGSGRUPP                  
003500        05 MOD-IDTRP-1.                                                   
003600*                                 TRANSPORTIDENTITET KLASS 1              
003700           07 MOD-IDTRPLOS-1 PIC X(3).                                    
003800*                                 TRANSPORTLÖSNING                        
003900           07 MOD-IDTRPVAR-1 PIC X(2).                                    
004000*                                 TRANSPORTLÖSNINGSGRUPP                  
004100        05 MOD-IDTRP-2.                                                   
004200*                                 TRANSPORTIDENTITET KLASS 2              
004300           07 MOD-IDTRPLOS-2 PIC X(3).                                    
004400*                                 TRANSPORTLÖSNING                        
004500           07 MOD-IDTRPVAR-2 PIC X(2).                                    
004600*                                 TRANSPORTLÖSNINGSGRUPP                  
004700        05 MOD-IDTRP-3.                                                   
004800*                                 TRANSPORTIDENTITET KLASS 3              
004900           07 MOD-IDTRPLOS-3 PIC X(3).                                    
005000*                                 TRANSPORTLÖSNING                        
005100           07 MOD-IDTRPVAR-3 PIC X(2).                                    
005200*                                 TRANSPORTLÖSNINGSGRUPP                  
005300        05 MOD-IDTRP-4.                                                   
005400*                                 TRANSPORTIDENTITET KLASS 4              
005500           07 MOD-IDTRPLOS-4 PIC X(3).                                    
005600*                                 TRANSPORTLÖSNING                        
005700           07 MOD-IDTRPVAR-4 PIC X(2).                                    
005800*                                 TRANSPORTLÖSNINGSGRUPP                  
005900        05 MOD-KDFDKRAV      PIC Z(2)9.                                   
006000*                                 TRANSPORTFÖRPACKNINGSKOD                
006100        05 MOD-KDGRANS       PIC Z(2)9.                                   
006200*                                 GRÄNSKOD                                
006300        05 MOD-REFOERS       PIC Z9.9(3).                                 
006400*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
006500        05 MOD-PRLEGKST      PIC Z(6)9.9(2).                              
006600*                                 LEGALISERINSKOSTNAD                     
006700     03 MOD-KDTRPKAT-UT      PIC X.                                       
006800*                                 TRANSPORTKATEGORI                       
006900     03 MOD-IDTRP-0-UT.                                                   
007000*                                 TRANSPORTIDENTITET KLASS 0              
007100        05 MOD-IDTRPLOS-0    PIC X(3).                                    
007200*                                 TRANSPORTLÖSNING                        
007300        05 MOD-IDTRPVAR-0    PIC X(2).                                    
007400*                                 TRANSPORTLÖSNINGSGRUPP                  
007500     03 MOD-IDTRP-1-UT.                                                   
007600*                                 TRANSPORTIDENTITET KLASS 1              
007700        05 MOD-IDTRPLOS-1    PIC X(3).                                    
007800*                                 TRANSPORTLÖSNING                        
007900        05 MOD-IDTRPVAR-1    PIC X(2).                                    
008000*                                 TRANSPORTLÖSNINGSGRUPP                  
008100     03 MOD-IDTRP-2-UT.                                                   
008200*                                 TRANSPORTIDENTITET KLASS 2              
008300        05 MOD-IDTRPLOS-2    PIC X(3).                                    
008400*                                 TRANSPORTLÖSNING                        
008500        05 MOD-IDTRPVAR-2    PIC X(2).                                    
008600*                                 TRANSPORTLÖSNINGSGRUPP                  
008700     03 MOD-IDTRP-3-UT.                                                   
008800*                                 TRANSPORTIDENTITET KLASS 3              
008900        05 MOD-IDTRPLOS-3    PIC X(3).                                    
009000*                                 TRANSPORTLÖSNING                        
009100        05 MOD-IDTRPVAR-3    PIC X(2).                                    
009200*                                 TRANSPORTLÖSNINGSGRUPP                  
009300     03 MOD-IDTRP-4-UT.                                                   
009400*                                 TRANSPORTIDENTITET KLASS 4              
009500        05 MOD-IDTRPLOS-4    PIC X(3).                                    
009600*                                 TRANSPORTLÖSNING                        
009700        05 MOD-IDTRPVAR-4    PIC X(2).                                    
009800*                                 TRANSPORTLÖSNINGSGRUPP                  
009900     03 MOD-KDFDKRAV-UT      PIC Z(2)9.                                   
010000*                                 TRANSPORTFÖRPACKNINGSKOD                
010100     03 MOD-KDGRANS-UT       PIC Z(2)9.                                   
010200*                                 GRÄNSKOD                                
010300     03 MOD-REFOERS-UT       PIC Z9.9(3).                                 
010400*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
010500     03 MOD-PRLEGKST-UT      PIC Z(6)9.9(2).                              
010600*                                 LEGALISERINSKOSTNAD                     
010700     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-KDCMD-IN         PIC X.                                       
011000*                                 RAD-UPPDATERINGSKOMMANDO                
011100     03 MOD-KUNDNR-IN-ATTR   PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 MOD-KUNDNR-IN        PIC X(7).                                    
011400*                                 KUNDNUMMER                              
011500     03 MOD-KDTRPKAT-IN-ATTR PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 MOD-KDTRPKAT-IN      PIC X.                                       
011800*                                 TRANSPORTKATEGORI                       
011900     03 MOD-IDTRP-0-IN-ATTR  PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-IDTRP-0-IN.                                                   
012200*                                 TRANSPORTIDENTITET KLASS 0              
012300        05 MOD-IDTRPLOS-0    PIC X(3).                                    
012400*                                 TRANSPORTLÖSNING                        
012500        05 MOD-IDTRPVAR-0    PIC X(2).                                    
012600*                                 TRANSPORTLÖSNINGSGRUPP                  
012700     03 MOD-IDTRP-1-IN-ATTR  PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900     03 MOD-IDTRP-1-IN.                                                   
013000*                                 TRANSPORTIDENTITET KLASS 1              
013100        05 MOD-IDTRPLOS-1    PIC X(3).                                    
013200*                                 TRANSPORTLÖSNING                        
013300        05 MOD-IDTRPVAR-1    PIC X(2).                                    
013400*                                 TRANSPORTLÖSNINGSGRUPP                  
013500     03 MOD-IDTRP-2-IN-ATTR  PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 MOD-IDTRP-2-IN.                                                   
013800*                                 TRANSPORTIDENTITET KLASS 2              
013900        05 MOD-IDTRPLOS-2    PIC X(3).                                    
014000*                                 TRANSPORTLÖSNING                        
014100        05 MOD-IDTRPVAR-2    PIC X(2).                                    
014200*                                 TRANSPORTLÖSNINGSGRUPP                  
014300     03 MOD-IDTRP-3-IN-ATTR  PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500     03 MOD-IDTRP-3-IN.                                                   
014600*                                 TRANSPORTIDENTITET KLASS 3              
014700        05 MOD-IDTRPLOS-3    PIC X(3).                                    
014800*                                 TRANSPORTLÖSNING                        
014900        05 MOD-IDTRPVAR-3    PIC X(2).                                    
015000*                                 TRANSPORTLÖSNINGSGRUPP                  
015100     03 MOD-IDTRP-4-IN-ATTR  PIC X(2).                                    
015200*                                 MFS ATTRIBUTFÄLT                        
015300     03 MOD-IDTRP-4-IN.                                                   
015400*                                 TRANSPORTIDENTITET KLASS 4              
015500        05 MOD-IDTRPLOS-4    PIC X(3).                                    
015600*                                 TRANSPORTLÖSNING                        
015700        05 MOD-IDTRPVAR-4    PIC X(2).                                    
015800*                                 TRANSPORTLÖSNINGSGRUPP                  
015900     03 MOD-KDFDKRAV-IN-ATTR PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100     03 MOD-KDFDKRAV-IN      PIC X(3).                                    
016200*                                 TRANSPORTFÖRPACKNINGSKOD                
016300     03 MOD-KDGRANS-IN-ATTR  PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500     03 MOD-KDGRANS-IN       PIC X(3).                                    
016600*                                 GRÄNSKOD                                
016700     03 MOD-REFOERS-IN-ATTR  PIC X(2).                                    
016800*                                 MFS ATTRIBUTFÄLT                        
016900     03 MOD-REFOERS-IN       PIC X(6).                                    
017000*                                 PROCENT             REFOERS-002         
017100     03 MOD-PRLEGKST-IN-ATTR PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300     03 MOD-PRLEGKST-IN      PIC X(10).                                   
017400*                                 LEGALISERINSKOSTNAD                     
017500     03 MOD-TEMFSINF         PIC X(55).                                   
017600*                                 INFORMATIONSMEDDELANDE                  
017700*** END OF VILMAII-COPY LENGTH= 849 BYTES                                 
