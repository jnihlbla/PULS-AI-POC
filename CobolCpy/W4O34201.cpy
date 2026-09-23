000100 01  MOD-W4O34201.                                                        
000200*                                 MOD-COPYTEXT PGM W40342                 
000300*                                 SKAPA PACKAD ORDERLISTA                 
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPRODNR-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDPRODNR-UT      PIC X(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-TEMFSINF         PIC X(55).                                   
001700*                                 INFORMATIONSMEDDELANDE                  
