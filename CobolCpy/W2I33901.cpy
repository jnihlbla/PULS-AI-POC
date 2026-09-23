000100 01  MID-W2I33901.                                                        
000200*                                 MID-COPYTEXT FÖR W20339                 
000300     03 MID-IDSPRGRP-IN      PIC X(10).                                   
000400*                                 SPÄRRADE GRUPPER                        
000500     03 MID-IDDISTR-IN       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDSPRGRP-UT      PIC X(10).                                   
001000*                                 SPÄRRADE GRUPPER                        
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-TABELLRAD        OCCURS 11 TIMES.                             
001600*                                 GRUPP MED TABELL RADER                  
001700        05 MID-CMD           PIC X.                                       
001800        05 MID-IDDISTR-FOM   PIC Z(3)9.                                   
001900*                                 DISTRIKTNUMMER                          
002000        05 MID-IDDISTR-TOM   PIC Z(3)9.                                   
002100*                                 DISTRIKTNUMMER                          
002200        05 MID-IDKUNDNR-FOM  PIC Z(5)9.                                   
002300*                                 KUNDNUMMER                              
002400        05 MID-IDKUNDNR-TOM  PIC Z(5)9.                                   
002500*                                 KUNDNUMMER                              
002600     03 MID-CMD-E            PIC X.                                       
002700     03 MID-IDDISTR-FOM-E    PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MID-IDDISTR-TOM-E    PIC 9(4).                                    
003000*                                 DISTRIKTNUMMER                          
003100     03 MID-IDKUNDNR-FOM-E   PIC 9(6).                                    
003200*                                 KUNDNUMMER                              
003300     03 MID-IDKUNDNR-TOM-E   PIC 9(6).                                    
003400*                                 KUNDNUMMER                              
003500     03 MID-TISTADAT-E       OCCURS 5 TIMES                               
003600                             PIC 9(6).                                    
003700*                                 GENERELLT STARTDATUM                    
003800*** END OF VILMAII-COPY LENGTH= 322 BYTES                                 
