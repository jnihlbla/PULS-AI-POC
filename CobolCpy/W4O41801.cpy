000100 01  MOD-W4O41801.                                                        
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
002700        05 MOD-BEGMRK-RAD1   PIC X(30).                                   
002800*                                 GODSMÄRKE  RAD1                         
002900        05 MOD-BEGMRK-RAD2   PIC X(30).                                   
003000*                                 GODSMÄRKE  RAD2                         
003100     03 MOD-BEGMRK-RAD1-UT   PIC X(30).                                   
003200*                                 GODSMÄRKE  RAD1                         
003300     03 MOD-BEGMRK-RAD2-UT   PIC X(30).                                   
003400*                                 GODSMÄRKE  RAD2                         
003500     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDCMD-IN         PIC X.                                       
003800*                                 RAD-UPPDATERINGSKOMMANDO                
003900     03 MOD-KUNDNR-IN-ATTR   PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KUNDNR-IN        PIC X(7).                                    
004200*                                 KUNDNUMMER                              
004300     03 MOD-BEGMRK-RAD1-IN-ATTR                                           
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-BEGMRK-RAD1-IN   PIC X(30).                                   
004700*                                 GODSMÄRKE  RAD1                         
004800     03 MOD-BEGMRK-RAD2-IN-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-BEGMRK-RAD2-IN   PIC X(30).                                   
005200*                                 GODSMÄRKE  RAD2                         
005300     03 MOD-TEMFSINF         PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*** END OF VILMAII-COPY LENGTH= 989 BYTES                                 
