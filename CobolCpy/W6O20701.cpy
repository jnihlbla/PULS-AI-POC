000100 01  MOD-W6O20701.                                                        
000200*                                 COPYTEXT FÖR MOD W6020701               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDKR-IN          PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-IDKR-UT          PIC 9(5).                                    
001400*                                 KONTROLLRAPPORT NUMMER                  
001500*                                 INSPECTION REPORT NUMBER                
001600     03 MOD-FLAGGA-DEL-ATTR  PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-FLAGGA-DEL       PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000*                                 GENERAL FLAG                            
002100     03 MOD-RAD              OCCURS 15 TIMES.                             
002200        05 MOD-TEKRFEL-ATTR  PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-TEKRFEL       PIC X(66).                                   
002500     03 MOD-TEMFSINF         PIC X(55).                                   
002600*                                 INFORMATIONSMEDDELANDE                  
002700*                                 INFORMATION MESSAGE                     
002800*** END OF VILMAII-COPY LENGTH= 1129 BYTES                                
