000100 01  HTERM-MID-W6I12403.                                                  
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I12403                                
000400     03 HTERM-MID-IDINLVGN-UT                                             
000500                             PIC X(3).                                    
000600*                                 VAGNSIDENTITET                          
000700     03 HTERM-MID-ADINLOMR-UT                                             
000800                             PIC X(4).                                    
000900*                                 INLEVERANSOMRÅDE                        
001000     03 HTERM-MID-ADINLOMR-NXT-UT                                         
001100                             PIC X(4).                                    
001200*                                 INLEVERANSOMRÅDE NÄSTA                  
001300     03 HTERM-MID-FLPRIO     PIC X.                                       
001400*                                 PRIORITERAD                             
001500     03 HTERM-MID-FLSATS     PIC X.                                       
001600*                                 SATSARTIKEL                             
001700     03 HTERM-MID-RAD        OCCURS 12 TIMES.                             
001800*                                 LINES                                   
001900        05 HTERM-MID-IDARTNR PIC 9(8).                                    
002000*                                 ARTIKELNUMMER                           
002100        05 HTERM-MID-KVINLART                                             
002200                             PIC 9(6).                                    
002300*                                 ANTAL I PARTIRAD                        
002400        05 HTERM-MID-IDLEVNR PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600        05 HTERM-MID-IDOKOLLI                                             
002700                             PIC 9(9).                                    
002800*                                 ODETTE KOLLINUMMER                      
002900*** END OF VILMAII-COPY LENGTH= 349 BYTES                                 
