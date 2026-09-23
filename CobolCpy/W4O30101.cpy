000100 01  MOD-W4O30101-CTX.                                                    
000200*                                 COPYTEXT FÖR MOD W4O30101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-W4O30101-001-GRP OCCURS 13 TIMES.                             
001200        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400        05 MOD-IDDISTR       PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600        05 MOD-IDPRODNR-ATTR PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 MOD-IDPRODNR      PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000        05 MOD-FLAVVPACK-ATTR                                             
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-FLAVVPACK     PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500        05 MOD-KDKOLLI-ATTR  PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-KDKOLLI       PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900        05 MOD-VKORDBTO-ATTR PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-VKORDBTO      PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300        05 MOD-KDEMBTYP-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KDEMBTYP      PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700        05 MOD-IDPRODNR-SAMP-ATTR                                         
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-IDPRODNR-SAMP PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200     03 MOD-TEMFSINF         PIC X(55).                                   
004300*                                 INFORMATIONSMEDDELANDE                  
004400*** END OF VILMAII-COPY LENGTH= 467 BYTES                                 
