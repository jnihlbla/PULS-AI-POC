000100 01  MID-W4I26501.                                                        
000200*                                 MID-COPYTEXT FÖR W40265                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDKUNDRF-IN      PIC X(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDKUNDRF-UT      PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-IDLOPNR-ENTER    PIC 9(3).                                    
002200*                                 LÖPNUMMER                               
002300     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
002600*                                 LÖPNUMMER                               
002700     03 MID-INPUT.                                                        
002800*                                 INDATA FÖR UPPDATERING                  
002900        05 MID-CMD-UPDATE    OCCURS 7 TIMES                               
003000                             PIC X.                                       
003100*                                 BEHANDLINGSKOD-X                        
003200        05 MID-IDARTNR       OCCURS 7 TIMES                               
003300                             PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500        05 MID-IDLOPNR       OCCURS 7 TIMES                               
003600                             PIC 9(3).                                    
003700*                                 LÖPNUMMER                               
003800        05 MID-KVBEART-Q-U   OCCURS 7 TIMES                               
003900                             PIC X(7).                                    
004000*                                 ANTAL ARTNR PER BRYTBEGREPP             
004100        05 MID-PRARTNTO-U    OCCURS 7 TIMES                               
004200                             PIC X(10).                                   
004300*                                 ARTIKELPRIS NETTO                       
004400        05 MID-BEART-U       OCCURS 7 TIMES                               
004500                             PIC X(17).                                   
004600        05 MID-BERADREF-U    OCCURS 7 TIMES                               
004700                             PIC X(10).                                   
004800*                                 KUNDENS RADREFERENS                     
004900*** END OF VILMAII-COPY LENGTH= 475 BYTES                                 
