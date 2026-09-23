000100 01  MOD-W2O33301.                                                        
000200*                                 MOD-COPYTEXT FÖR W20333                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-IDDIRGRP-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDIRGRP-UT      PIC X(10).                                   
001500*                                 DIREKTLEVERANSGRUPP                     
001600     03 MOD-IDARTNR-IN       PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDARTNR-UT       PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-CMD-ATTR         PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-CMD              PIC X.                                       
002300     03 MOD-IDARTNR-E-ATTR   PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-IDARTNR-E        PIC Z(8)9.                                   
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-TISTADAT-E-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-TISTADAT-E       PIC 9(6).                                    
003000*                                 GENERELLT STARTDATUM                    
003100     03 MOD-TISTADAT-GRP     PIC 9(6).                                    
003200*                                 GENERELLT STARTDATUM                    
003300     03 MOD-TABELLRAD        OCCURS 36 TIMES.                             
003400*                                 GRUPP MED TABELL RADER                  
003500        05 MOD-IDARTNR       PIC Z(8)9.                                   
003600*                                 ARTIKELNUMMER                           
003700        05 MOD-TISTADAT      PIC 9(6).                                    
003800*                                 GENERELLT STARTDATUM                    
003900     03 MOD-TEMFSINF         PIC X(55).                                   
004000*                                 INFORMATIONSMEDDELANDE                  
004100*** END OF VILMAII-COPY LENGTH= 697 BYTES                                 
