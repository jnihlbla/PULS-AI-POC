000100 01  MOD-W2O33801.                                                        
000200*                                 MOD-COPYTEXT FÖR W20338                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDDISTR-IN       PIC Z(3)9.                                   
001300*                                 DISTRIKTNUMMER                          
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-TABELLRAD        OCCURS 14 TIMES.                             
001700*                                 GRUPP MED TABELL RADER                  
001800        05 MOD-IDDIRGRP      PIC X(10).                                   
001900*                                 DIREKTLEVERANSGRUPP                     
002000        05 MOD-FLAUTUPD      PIC X.                                       
002100*                                 AUT. SPÄRR PER REGEL/ARTIKEL            
002200        05 MOD-TISTADAT-GRP-ATTR                                          
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-TISTADAT-GRP  PIC 9(6).                                    
002600*                                 GENERELLT STARTDATUM                    
002700        05 MOD-TISTADAT-ART-ATTR                                          
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-TISTADAT-ART  PIC 9(6).                                    
003100*                                 GENERELLT STARTDATUM                    
003200        05 MOD-TENOTE40      PIC X(40).                                   
003300     03 MOD-TEMFSINF         PIC X(55).                                   
003400*                                 INFORMATIONSMEDDELANDE                  
003500*** END OF VILMAII-COPY LENGTH= 1063 BYTES                                
