000100 01  MOD-W5O16101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5161              
000300*                                 SOL-URVAL                               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDUSER-IN        PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000     03 MOD-IDUSER-UT        PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200     03 MOD-DATA-IN.                                                      
001300*                                 RAPPORTERINGS-FÄLT                      
001400        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600        05 MOD-IDARTNR       PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800        05 MOD-DATHISY       PIC X(4).                                    
001900        05 MOD-FLTHISYEAR-ATTR                                            
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200        05 MOD-FLTHISYEAR    PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400        05 MOD-DALASTY       PIC X(4).                                    
002500        05 MOD-FLLASTYEAR-ATTR                                            
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-FLLASTYEAR    PIC X.                                       
002900*                                 ALLMÄN FLAGGA                           
003000     03 MOD-TEMFSINF         PIC X(55).                                   
003100*                                 INFORMATIONSMEDDELANDE                  
003200*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 
