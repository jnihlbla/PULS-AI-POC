000100 01  MID-W0I81501.                                                        
000200*                                 MID-COPYTEXT FÖR W08150                 
000300     03 MID-IDSYSMOT-IN      PIC X(10).                                   
000400*                                 PULS MOTTAGANDE SYSTEMNAMN              
000500     03 MID-IDDISTR-IN       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDSYSMOT-UT      PIC X(10).                                   
000800*                                 PULS MOTTAGANDE SYSTEMNAMN              
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-TABELLRAD        OCCURS 12 TIMES.                             
001200*                                 GRUPP MED TABELL RADER                  
001300        05 MID-CMD           PIC X.                                       
001400        05 MID-IDLANDX2      PIC X(2).                                    
001500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001600        05 MID-ADPOSTNR-FOM  PIC X(10).                                   
001700*                                 POSTNUMMER I ADRESS                     
001800        05 MID-ADPOSTNR-TOM  PIC X(10).                                   
001900*                                 POSTNUMMER I ADRESS                     
002000     03 MID-CMD-E            PIC X.                                       
002100     03 MID-IDLANDX2-E       PIC X(2).                                    
002200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002300     03 MID-ADPOSTNR-FOM-E   PIC X(10).                                   
002400*                                 POSTNUMMER I ADRESS                     
002500     03 MID-ADPOSTNR-TOM-E   PIC X(10).                                   
002600*                                 POSTNUMMER I ADRESS                     
002700     03 MID-IDDISTR-E        PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MID-IDKUNDNR-E       PIC 9(6).                                    
003000*                                 KUNDNUMMER                              
003100*** END OF VILMAII-COPY LENGTH= 337 BYTES                                 
