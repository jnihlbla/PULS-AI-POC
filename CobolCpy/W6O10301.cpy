000100 01  MOD-W6O10301.                                                        
000200*                                 MODCOPYTEXT TILL W60103.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-ADINLOMR-PRT-ATTR                                             
001200                             PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
001500*                                 PRINTERPLACERING                        
001600     03 MOD-RADER.                                                        
001700*                                 RADER                                   
001800        05 MOD-INKLATTR      OCCURS 12 TIMES.                             
001900*                                 ATTR + FÄLT                             
002000           07 MOD-IDLEVNR-KOLLI-ATTR                                      
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300           07 MOD-IDLEVNR-KOLLI                                           
002400                             PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER KOLLI                  
002600        05 MOD-INKLATTR      OCCURS 12 TIMES.                             
002700*                                 ATTR + FÄLT                             
002800           07 MOD-IDOKOLLI-ATTR                                           
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100           07 MOD-IDOKOLLI   PIC X(9).                                    
003200*                                 ODETTE KOLLINUMMER                      
003300     03 MOD-TEMFSINF         PIC X(55).                                   
003400*                                 INFORMATIONSMEDDELANDE                  
