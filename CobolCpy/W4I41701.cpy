000100 01  MID-W4I41701.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-KDFRAKT-IN       PIC X(2).                                    
000800*                                 FRAKTSÄTT DC TILL KUND                  
000900     03 MID-KDFRAKT-UT       PIC X(2).                                    
001000*                                 FRAKTSÄTT DC TILL KUND                  
001100     03 MID-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MID-INPUT.                                                        
002000*                                                                         
002100        05 MID-KDCMD-IN      PIC X.                                       
002200*                                 RAD-UPPDATERINGSKOMMANDO                
002300        05 MID-KUNDNR-IN     PIC X(7).                                    
002400*                                 KUNDNUMMER                              
002500        05 MID-KDTRPKAT-IN   PIC X.                                       
002600*                                 TRANSPORTKATEGORI                       
002700        05 MID-IDTRP-0-IN.                                                
002800*                                 TRANSPORTIDENTITET KLASS 0              
002900           07 MID-IDTRPLOS-0 PIC X(3).                                    
003000*                                 TRANSPORTLÖSNING                        
003100           07 MID-IDTRPVAR-0 PIC X(2).                                    
003200*                                 TRANSPORTLÖSNINGSGRUPP                  
003300        05 MID-IDTRP-1-IN.                                                
003400*                                 TRANSPORTIDENTITET KLASS 1              
003500           07 MID-IDTRPLOS-1 PIC X(3).                                    
003600*                                 TRANSPORTLÖSNING                        
003700           07 MID-IDTRPVAR-1 PIC X(2).                                    
003800*                                 TRANSPORTLÖSNINGSGRUPP                  
003900        05 MID-IDTRP-2-IN.                                                
004000*                                 TRANSPORTIDENTITET KLASS 2              
004100           07 MID-IDTRPLOS-2 PIC X(3).                                    
004200*                                 TRANSPORTLÖSNING                        
004300           07 MID-IDTRPVAR-2 PIC X(2).                                    
004400*                                 TRANSPORTLÖSNINGSGRUPP                  
004500        05 MID-IDTRP-3-IN.                                                
004600*                                 TRANSPORTIDENTITET KLASS 3              
004700           07 MID-IDTRPLOS-3 PIC X(3).                                    
004800*                                 TRANSPORTLÖSNING                        
004900           07 MID-IDTRPVAR-3 PIC X(2).                                    
005000*                                 TRANSPORTLÖSNINGSGRUPP                  
005100        05 MID-IDTRP-4-IN.                                                
005200*                                 TRANSPORTIDENTITET KLASS 4              
005300           07 MID-IDTRPLOS-4 PIC X(3).                                    
005400*                                 TRANSPORTLÖSNING                        
005500           07 MID-IDTRPVAR-4 PIC X(2).                                    
005600*                                 TRANSPORTLÖSNINGSGRUPP                  
005700        05 MID-KDFDKRAV-IN   PIC X(3).                                    
005800*                                 TRANSPORTFÖRPACKNINGSKOD                
005900        05 MID-KDGRANS-IN    PIC X(3).                                    
006000*                                 GRÄNSKOD                                
006100        05 MID-REFOERS-IN    PIC X(6).                                    
006200*                                 PROCENT             REFOERS-002         
006300        05 MID-PRLEGKST-IN   PIC X(10).                                   
006400*                                 LEGALISERINSKOSTNAD                     
006500*** END OF VILMAII-COPY LENGTH= 84 BYTES                                  
