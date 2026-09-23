000100 01  MID-W4I52101.                                                        
000200*                                 MID-COPYTEXT FÖR W40521                 
000300     03 MID-KDPERSON-IN      PIC X(3).                                    
000400*                                 PERSONKOD                               
000500     03 MID-KDPERSON-UT      PIC X(3).                                    
000600*                                 PERSONKOD                               
000700     03 MID-KDFRAKT-IN       PIC X(2).                                    
000800*                                 FRAKTSÄTT C1-C2 TILL KUND               
000900     03 MID-KDFRAKT-UT       PIC X(2).                                    
001000*                                 FRAKTSÄTT C1-C2 TILL KUND               
001100     03 MID-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-FLUTSKR-IN       PIC X.                                       
002400*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
002500     03 MID-FLUTSKR-UT       PIC X.                                       
002600*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
002700     03 MID-SPARADE-NYCKLAR.                                              
002800*                                 INNEHÅLLER SPARADE NYCKLAR              
002900        05 MID-IDPRODNR-SPAR PIC 9(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100        05 MID-KDORDSTA-SPAR PIC 9.                                       
003200*                                 VOLVOORDERSTATUS                        
003300        05 MID-TIBEGPAC-SPAR PIC 9(6).                                    
003400*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
003500        05 MID-IDDISTR-SPAR  PIC 9(4).                                    
003600*                                 DISTRIKTNUMMER                          
003700        05 MID-IDKUNDNR-SPAR PIC 9(6).                                    
003800*                                 KUNDNUMMER                              
003900        05 MID-KDFRAKT-SPAR  PIC 9(2).                                    
004000*                                 FRAKTSÄTT C1-C2 TILL KUND               
004100*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
