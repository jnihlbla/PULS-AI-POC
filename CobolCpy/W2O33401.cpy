000100 01  MOD-W2O33401.                                                        
000200*                                 MOD-COPYTEXT FÖR W20334                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-TABELLRAD        OCCURS 28 TIMES.                             
001300*                                 GRUPP MED TABELL RADER                  
001400        05 MOD-IDLEVNR       PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600        05 MOD-IDDIRGRP      PIC X(10).                                   
001700*                                 DIREKTLEVERANSGRUPP                     
001800        05 MOD-TISTADAT-GRP  PIC 9(6).                                    
001900*                                 GENERELLT STARTDATUM                    
002000        05 MOD-TISTADAT-ART  PIC 9(6).                                    
002100*                                 GENERELLT STARTDATUM                    
002200     03 MOD-TEMFSINF         PIC X(55).                                   
002300*                                 INFORMATIONSMEDDELANDE                  
002400*** END OF VILMAII-COPY LENGTH= 866 BYTES                                 
