000100 01  MOD-W4O36201.                                                        
000200*                                 COPYTEXT FÖR MOD W4O36201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDHLOTAB-IN      PIC X(2).                                    
000800*                                 HLOTABELLSIDENTITET                     
000900     03 MOD-IDHLOTAB-UT      PIC X(2).                                    
001000*                                 HLOTABELLSIDENTITET                     
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDHLO-IN         PIC X(2).                                    
001600*                                 HUVUDLAGEROMRÅDE                        
001700     03 MOD-IDHLO-UT         PIC X(2).                                    
001800*                                 HUVUDLAGEROMRÅDE                        
001900     03 MOD-RAD              OCCURS 99 TIMES.                             
002000*                                 TABELL-RADER                            
002100        05 MOD-ADLAGOMR-RAD-ATTR                                          
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-ADLAGOMR-RAD  PIC Z9.                                      
002500*                                 HUVUDLAGEROMRÅDE                        
002600     03 MOD-RAD              OCCURS 99 TIMES.                             
002700*                                 TABELL-RADER                            
002800        05 MOD-IDHLO-RAD-ATTR                                             
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-IDHLO-RAD     PIC Z9.                                      
003200*                                 HUVUDLAGEROMRÅDE                        
003300     03 MOD-INPUT            OCCURS 5 TIMES.                              
003400*                                 INDATA FÖR UPPDATERING                  
003500        05 MOD-ADLAGOMR-UPDATE-ATTR                                       
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-ADLAGOMR-UPDATE                                            
003900                             PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100        05 MOD-IDHLO-UPDATE-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDHLO-UPDATE  PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-TEMFSINF         PIC X(55).                                   
004700*                                 INFORMATIONSMEDDELANDE                  
004800*** END OF VILMAII-COPY LENGTH= 943 BYTES                                 
