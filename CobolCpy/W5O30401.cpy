000100 01  MOD-W5O30401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5304              
000300*                                 INVENTERING                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-RAD              OCCURS 24 TIMES.                             
001300        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-IDARTNR       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700        05 MOD-KDINVPRIO-ATTR                                             
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-KDINVPRIO     PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200        05 MOD-TEINVANM-ATTR PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-TEINVANM      PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-TEMFSINF         PIC X(55).                                   
002700*                                 INFORMATIONSMEDDELANDE                  
