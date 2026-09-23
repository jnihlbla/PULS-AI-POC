000100 01  MOD-W4O45701.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-LINE.                                                         
001600*                                 GROUP FOR FREIGTH CODES                 
001700        05 MOD-KDFRAKT-AIR1  PIC Z9.                                      
001800*                                 FRAKTSÄTT DC TILL KUND                  
001900        05 MOD-KDFRAKT-AIR2  PIC Z9.                                      
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100        05 MOD-KDFRAKT-AIR3  PIC Z9.                                      
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300        05 MOD-KDFRAKT-AIR4  PIC Z9.                                      
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500        05 MOD-KDFRAKT-AIR5  PIC Z9.                                      
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700        05 MOD-KDFRAKT-AIR6  PIC Z9.                                      
002800*                                 FRAKTSÄTT DC TILL KUND                  
002900        05 MOD-KDFRAKT-BOAT1 PIC Z9.                                      
003000*                                 FRAKTSÄTT DC TILL KUND                  
003100        05 MOD-KDFRAKT-BOAT2 PIC Z9.                                      
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300        05 MOD-KDFRAKT-BOAT3 PIC Z9.                                      
003400*                                 FRAKTSÄTT DC TILL KUND                  
003500        05 MOD-KDFRAKT-BOAT4 PIC Z9.                                      
003600*                                 FRAKTSÄTT DC TILL KUND                  
003700        05 MOD-KDFRAKT-BOAT5 PIC Z9.                                      
003800*                                 FRAKTSÄTT DC TILL KUND                  
003900        05 MOD-KDFRAKT-BOAT6 PIC Z9.                                      
004000*                                 FRAKTSÄTT DC TILL KUND                  
004100        05 MOD-KDFRAKT-ROAD1 PIC Z9.                                      
004200*                                 FRAKTSÄTT DC TILL KUND                  
004300        05 MOD-KDFRAKT-ROAD2 PIC Z9.                                      
004400*                                 FRAKTSÄTT DC TILL KUND                  
004500        05 MOD-KDFRAKT-ROAD3 PIC Z9.                                      
004600*                                 FRAKTSÄTT DC TILL KUND                  
004700        05 MOD-KDFRAKT-ROAD4 PIC Z9.                                      
004800*                                 FRAKTSÄTT DC TILL KUND                  
004900        05 MOD-KDFRAKT-ROAD5 PIC Z9.                                      
005000*                                 FRAKTSÄTT DC TILL KUND                  
005100        05 MOD-KDFRAKT-AIR7  PIC Z9.                                      
005200*                                 FRAKTSÄTT DC TILL KUND                  
005300        05 MOD-KDFRAKT-AIR8  PIC Z9.                                      
005400*                                 FRAKTSÄTT DC TILL KUND                  
005500        05 MOD-KDFRAKT-AIR9  PIC Z9.                                      
005600*                                 FRAKTSÄTT DC TILL KUND                  
005700        05 MOD-KDFRAKT-AIR10 PIC Z9.                                      
005800*                                 FRAKTSÄTT DC TILL KUND                  
005900        05 MOD-KDFRAKT-AIR11 PIC Z9.                                      
006000*                                 FRAKTSÄTT DC TILL KUND                  
006100        05 MOD-KDFRAKT-AIR12 PIC Z9.                                      
006200*                                 FRAKTSÄTT DC TILL KUND                  
006300        05 MOD-KDFRAKT-BOAT7 PIC Z9.                                      
006400*                                 FRAKTSÄTT DC TILL KUND                  
006500        05 MOD-KDFRAKT-BOAT8 PIC Z9.                                      
006600*                                 FRAKTSÄTT DC TILL KUND                  
006700        05 MOD-KDFRAKT-BOAT9 PIC Z9.                                      
006800*                                 FRAKTSÄTT DC TILL KUND                  
006900        05 MOD-KDFRAKT-BOAT10                                             
007000                             PIC Z9.                                      
007100*                                 FRAKTSÄTT DC TILL KUND                  
007200        05 MOD-KDFRAKT-BOAT11                                             
007300                             PIC Z9.                                      
007400*                                 FRAKTSÄTT DC TILL KUND                  
007500        05 MOD-KDFRAKT-BOAT12                                             
007600                             PIC Z9.                                      
007700*                                 FRAKTSÄTT DC TILL KUND                  
007800        05 MOD-KDFRAKT-ROAD6 PIC Z9.                                      
007900*                                 FRAKTSÄTT DC TILL KUND                  
008000        05 MOD-KDFRAKT-ROAD7 PIC Z9.                                      
008100*                                 FRAKTSÄTT DC TILL KUND                  
008200        05 MOD-KDFRAKT-ROAD8 PIC Z9.                                      
008300*                                 FRAKTSÄTT DC TILL KUND                  
008400        05 MOD-KDFRAKT-ROAD9 PIC Z9.                                      
008500*                                 FRAKTSÄTT DC TILL KUND                  
008600        05 MOD-KDFRAKT-ROAD10                                             
008700                             PIC Z9.                                      
008800*                                 FRAKTSÄTT DC TILL KUND                  
008900     03 MOD-LINE             OCCURS 3 TIMES.                              
009000*                                 GRUPP MED RADER                         
009100        05 MOD-KDCMD-ATTR    PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-KDCMD         PIC X.                                       
009400*                                 RAD-UPPDATERINGSKOMMANDO                
009500*                                  BLANK  = INGENTING                     
009600*                                  D , B  = DELETE                        
009700*                                  R , Ä  = REPLACE                       
009800*                                  I,N,A  = INSERT                        
009900*                                  S , V  = SELECT                        
010000*                                  P , P  = PRINT                         
010100*                                  C , K  = COPY                          
010200        05 MOD-IDDISTR       PIC Z(3)9.                                   
010300*                                 DISTRIKTNUMMER                          
010400        05 MOD-RETRPFAC-AIR  PIC Z(2)9.9(2).                              
010500        05 MOD-AIR-PERCENT   PIC X.                                       
010600        05 MOD-PRWEIGHT-AIR  PIC Z(4)9.                                   
010700        05 MOD-PRHAZMAT-AIR  PIC Z(4)9.                                   
010800        05 MOD-RETRPFAC-BOAT PIC Z(2)9.9(2).                              
010900        05 MOD-BOAT-PERCENT  PIC X.                                       
011000        05 MOD-PRWEIGHT-BOAT PIC Z(4)9.                                   
011100        05 MOD-PRHAZMAT-BOAT PIC Z(4)9.                                   
011200        05 MOD-RETRPFAC-ROAD PIC Z(2)9.9(2).                              
011300        05 MOD-ROAD-PERCENT  PIC X.                                       
011400        05 MOD-PRWEIGHT-ROAD PIC Z(4)9.                                   
011500        05 MOD-PRHAZMAT-ROAD PIC Z(4)9.                                   
011600        05 MOD-REINSFAC-AIR  PIC 9.9(5).                                  
011700        05 MOD-REINSFAC-BOAT PIC 9.9(5).                                  
011800        05 MOD-REINSFAC-ROAD PIC 9.9(5).                                  
011900     03 MOD-UPD.                                                          
012000*                                 UPPDATERINGSRAD                         
012100        05 MOD-IDDISTR-UPD-ATTR                                           
012200                             PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400        05 MOD-IDDISTR-UPD   PIC Z(3)9.                                   
012500*                                 DISTRIKTNUMMER                          
012600        05 MOD-RETRPFAC-AIR-UPD-ATTR                                      
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900        05 MOD-RETRPFAC-AIR-UPD                                           
013000                             PIC Z(2)9.9(2).                              
013100        05 MOD-AIR-PERCENT-UPD-ATTR                                       
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400        05 MOD-AIR-PERCENT-UPD                                            
013500                             PIC X.                                       
013600        05 MOD-PRWEIGHT-AIR-UPD-ATTR                                      
013700                             PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900        05 MOD-PRWEIGHT-AIR-UPD                                           
014000                             PIC Z(4)9.                                   
014100        05 MOD-PRHAZMAT-AIR-UPD-ATTR                                      
014200                             PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400        05 MOD-PRHAZMAT-AIR-UPD                                           
014500                             PIC Z(4)9.                                   
014600        05 MOD-RETRPFAC-BOAT-UPD-ATTR                                     
014700                             PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900        05 MOD-RETRPFAC-BOAT-UPD                                          
015000                             PIC Z(2)9.9(2).                              
015100        05 MOD-PRWEIGHT-BOAT-UPD-ATTR                                     
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400        05 MOD-PRWEIGHT-BOAT-UPD                                          
015500                             PIC Z(4)9.                                   
015600        05 MOD-PRHAZMAT-BOAT-UPD-ATTR                                     
015700                             PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900        05 MOD-PRHAZMAT-BOAT-UPD                                          
016000                             PIC Z(4)9.                                   
016100        05 MOD-RETRPFAC-ROAD-UPD-ATTR                                     
016200                             PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400        05 MOD-RETRPFAC-ROAD-UPD                                          
016500                             PIC Z(2)9.9(2).                              
016600        05 MOD-PRWEIGHT-ROAD-UPD-ATTR                                     
016700                             PIC X(2).                                    
016800*                                 MFS ATTRIBUTFÄLT                        
016900        05 MOD-PRWEIGHT-ROAD-UPD                                          
017000                             PIC Z(4)9.                                   
017100        05 MOD-PRHAZMAT-ROAD-UPD-ATTR                                     
017200                             PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400        05 MOD-PRHAZMAT-ROAD-UPD                                          
017500                             PIC Z(4)9.                                   
017600        05 MOD-REINSFAC-AIR-UPD2-ATTR                                     
017700                             PIC X(2).                                    
017800*                                 MFS ATTRIBUTFÄLT                        
017900        05 MOD-REINSFAC-AIR-UPD2                                          
018000                             PIC 9.9(5).                                  
018100        05 MOD-REINSFAC-BOAT-UPD2-ATTR                                    
018200                             PIC X(2).                                    
018300*                                 MFS ATTRIBUTFÄLT                        
018400        05 MOD-REINSFAC-BOAT-UPD2                                         
018500                             PIC 9.9(5).                                  
018600        05 MOD-REINSFAC-ROAD-UPD2-ATTR                                    
018700                             PIC X(2).                                    
018800*                                 MFS ATTRIBUTFÄLT                        
018900        05 MOD-REINSFAC-ROAD-UPD2                                         
019000                             PIC 9.9(5).                                  
019100     03 MOD-TEMFSINF         PIC X(55).                                   
019200*                                 INFORMATIONSMEDDELANDE                  
019300*** END OF VILMAII-COPY LENGTH= 518 BYTES                                 
