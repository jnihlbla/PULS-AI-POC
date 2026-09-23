000100 01  MID-W2I33501.                                                        
000200*                                 MID-COPYTEXT FÖR W20335                 
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-UT       PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-IDDIRGRP-IN      PIC X(10).                                   
000800*                                 DIREKTLEVERANSGRUPP                     
000900     03 MID-IDDIRGRP-UT      PIC X(10).                                   
001000*                                 DIREKTLEVERANSGRUPP                     
001100     03 MID-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MID-CMD-E            PIC X.                                       
002000     03 MID-IDDISTR-FOM-E    PIC 9(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 MID-IDDISTR-TOM-E    PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MID-IDKUNDNR-FOM-E   PIC X(6).                                    
002500*                                 KUNDNUMMER                              
002600     03 MID-IDKUNDNR-TOM-E   PIC X(6).                                    
002700*                                 KUNDNUMMER                              
002800     03 MID-KVBEART-MIN-E    OCCURS 5 TIMES                               
002900                             PIC 9(6).                                    
003000*                                 BESTÄLLT ANTAL MIN-KVANTITET            
003100     03 MID-KDDDGS-E         OCCURS 5 TIMES                               
003200                             PIC X.                                       
003300*                                 REGEL HUR DDGS ART LEVERERAS            
003400     03 MID-FLDDGS-E         OCCURS 5 TIMES                               
003500                             PIC X.                                       
003600*                                 VISAR OM DDGS REGEL ÖVERLAPPAS          
003700     03 MID-IDDC-E           OCCURS 5 TIMES                               
003800                             PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000     03 MID-TABELLRAD        OCCURS 6 TIMES.                              
004100*                                 GRUPP MED TABELL RADER                  
004200        05 MID-CMD           PIC X.                                       
004300*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
