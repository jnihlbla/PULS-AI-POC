000100 01  MOD-W2O33201.                                                        
000200*                                 MOD-COPYTEXT FÖR W20332                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-CMD-ATTR         PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-CMD              PIC X.                                       
001500     03 MOD-IDDIRGRP-E-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDDIRGRP-E       PIC X(10).                                   
001800*                                 DIREKTLEVERANSGRUPP                     
001900     03 MOD-TISTADAT-E-ATTR  PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-TISTADAT-E       PIC 9(6).                                    
002200*                                 GENERELLT STARTDATUM                    
002300     03 MOD-BELEV            PIC X(35).                                   
002400*                                 LEVERANTÖRSNAMN                         
002500     03 MOD-TABELLRAD        OCCURS 36 TIMES.                             
002600*                                 GRUPP MED TABELL RADER                  
002700        05 MOD-IDDIRGRP      PIC X(10).                                   
002800*                                 DIREKTLEVERANSGRUPP                     
002900        05 MOD-TISTADAT      PIC 9(6).                                    
003000*                                 GENERELLT STARTDATUM                    
003100     03 MOD-TEMFSINF         PIC X(55).                                   
003200*                                 INFORMATIONSMEDDELANDE                  
003300*** END OF VILMAII-COPY LENGTH= 740 BYTES                                 
