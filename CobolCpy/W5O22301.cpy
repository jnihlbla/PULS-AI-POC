000100 01  MOD-W5O22301.                                                        
000200*                                 MOD-COPYTEXT FÖR W50223                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDKONTO-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDKONTO-UT       PIC X(10).                                   
001100*                                 KONTO                                   
001200     03 MOD-IDFTG-UT         PIC X(2).                                    
001300*                                 FÖRETAGSID EKONOM REDOVISNING           
001400     03 MOD-TABELLRAD        OCCURS 15 TIMES.                             
001500*                                 GRUPP MED TABELL RADER                  
001600        05 MOD-CMD-ATTR      PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 MOD-CMD           PIC X.                                       
001900        05 MOD-IDPRCTR       PIC X(10).                                   
002000*                                 PROFIT CENTER                           
002100        05 MOD-IDANALYS      PIC X(12).                                   
002200*                                 ANALYSNUMMER                            
002300     03 MOD-TEMFSINF         PIC X(55).                                   
002400*                                 INFORMATIONSMEDDELANDE                  
002500*** END OF VILMAII-COPY LENGTH= 488 BYTES                                 
