000100 01  MID-W6I12401.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I12401                                
000400     03 MID-IDINLVGN-IN      PIC X(3).                                    
000500*                                 VAGNSIDENTITET                          
000600     03 MID-ADINLOMR-IN      PIC X(4).                                    
000700*                                 INLEVERANSOMRÅDE                        
000800     03 MID-ADINLOMR-NXT-IN  PIC X(4).                                    
000900*                                 INLEVERANSOMRÅDE NÄSTA                  
001000     03 MID-IDDC-IN          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-IDINLVGN-UT      PIC X(3).                                    
001300*                                 VAGNSIDENTITET                          
001400     03 MID-ADINLOMR-UT      PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE                        
001600     03 MID-ADINLOMR-NXT-UT  PIC X(4).                                    
001700*                                 INLEVERANSOMRÅDE NÄSTA                  
001800     03 MID-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MID-SPAR-IDINLVGN    PIC 9(3).                                    
002100*                                 VAGNSIDENTITET                          
002200     03 MID-SPAR-ADINLOMR    PIC X(4).                                    
002300*                                 INLEVERANSOMRÅDE                        
002400     03 MID-SPAR-ADINLOMR-NXT                                             
002500                             PIC X(4).                                    
002600*                                 INLEVERANSOMRÅDE NÄSTA                  
002700     03 MID-FLPRIO           PIC X.                                       
002800*                                 PRIORITERAD                             
002900     03 MID-FLSATS           PIC X.                                       
003000*                                 SATSARTIKEL                             
003100     03 MID-RAD              OCCURS 12 TIMES.                             
003200*                                 LINES                                   
003300        05 MID-IDARTNR       PIC X(9).                                    
003400*                                 ARTIKELNUMMER                           
003500        05 MID-KVINLART      PIC X(6).                                    
003600*                                 ANTAL I PARTIRAD                        
003700        05 MID-IDLEVNR       PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900        05 MID-IDOKOLLI      PIC X(9).                                    
004000*                                 ODETTE KOLLINUMMER                      
