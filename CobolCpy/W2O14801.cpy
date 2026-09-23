000100 01  MOD-W2O14801.                                                        
000200*                                 MOD-COPYTEXT FÖR W2014800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-RAD              OCCURS 10 TIMES.                             
000800*                                 RADINFORMATION                          
000900        05 MOD-RAD-CMD-ATTR  PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100        05 MOD-RAD-CMD       PIC X.                                       
001200*                                 BEHANDLINGSKOD                          
001300        05 MOD-RAD-KDPRODSL  PIC Z9.                                      
001400*                                 PRODUKTSLAG                             
001500        05 MOD-RAD-KVPERIOD-BERS                                          
001600                             PIC Z9.                                      
001700*                                 ANTAL PERIODER OI BEHOV ERSATT.         
001800        05 MOD-RAD-KVPERIOD-BTILLK                                        
001900                             PIC Z9.                                      
002000*                                 ANTAL PERIODER OI BEHOV TILLK.          
002100        05 MOD-RAD-KVPERIOD-VERS                                          
002200                             PIC Z9.                                      
002300*                                 ANTAL PERIODER OI VÄRDE ERSATT.         
002400        05 MOD-RAD-KVPERIOD-HERS                                          
002500                             PIC Z9.                                      
002600*                                 ANTAL PERIODER OI HIST. ERSATT.         
002700     03 MOD-KDPRODSL-IN-ATTR PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KDPRODSL-IN      PIC Z9.                                      
003000*                                 PRODUKTSLAG                             
003100     03 MOD-KVPERIOD-BERS-IN-ATTR                                         
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KVPERIOD-BERS-IN PIC Z9.                                      
003500*                                 ANTAL PERIODER OI BEHOV ERSATT.         
003600     03 MOD-KVPERIOD-BTILLK-IN-ATTR                                       
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-KVPERIOD-BTILLK-IN                                            
004000                             PIC Z9.                                      
004100*                                 ANTAL PERIODER OI BEHOV TILLK.          
004200     03 MOD-KVPERIOD-VERS-IN-ATTR                                         
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-KVPERIOD-VERS-IN PIC Z9.                                      
004600*                                 ANTAL PERIODER OI VÄRDE ERSATT.         
004700     03 MOD-KVPERIOD-HERS-IN-ATTR                                         
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-KVPERIOD-HERS-IN PIC Z9.                                      
005100*                                 ANTAL PERIODER OI HIST. ERSATT.         
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 249 BYTES                                 
