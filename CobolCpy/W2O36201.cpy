000100 01  MOD-W2O36201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2036200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-SEND-IDDC-IN-ATTR                                             
000800                             PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 MOD-SEND-IDDC-IN     PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-RECV-IDDC-IN-ATTR                                             
001300                             PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-RECV-IDDC-IN     PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-FLFLYG-IN-ATTR   PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-FLFLYG-IN        PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDKAMPRF-IN-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-IDKAMPRF-IN      PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-UTFALT.                                                       
002600*                                 UTDATA UPPDATERINGSFÄLT                 
002700*                                 2362                                    
002800        05 MOD-BELAGINS-DEL-IN-ATTR                                       
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-BELAGINS-DEL-IN                                            
003200                             PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400        05 MOD-RAD           OCCURS 12 TIMES.                             
003500*                                 UTDATA ORDERRAD 2362                    
003600           07 MOD-IDARTNR-RAD-IN-ATTR                                     
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900           07 MOD-IDARTNR-RAD-IN                                          
004000                             PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200           07 MOD-KVBEART-RAD-IN-ATTR                                     
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500           07 MOD-KVBEART-RAD-IN                                          
004600                             PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800           07 MOD-BERADREF-RAD-IN-ATTR                                    
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100           07 MOD-BERADREF-RAD-IN                                         
005200                             PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400        05 MOD-FLSLUT-IN-ATTR                                             
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-FLSLUT-IN     PIC X(2).                                    
005800*                                 MFS BEHANDLING AV INPUTFÄLT             
005900     03 MOD-IDORDNR7-ATTR    PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-IDORDNR7         PIC Z(6)9.                                   
006200*                                 ORDERNUMMER                             
006300     03 MOD-TEMFSINF         PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 276 BYTES                                 
