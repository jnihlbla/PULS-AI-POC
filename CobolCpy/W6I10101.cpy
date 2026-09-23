000100 01  MID-W6I10101.                                                        
000200*                                 MID-COPYTEXT FÖR W6010100               
000300     03 MID-IDINLVGN-IN      PIC X(3).                                    
000400*                                 VAGNSIDENTITET                          
000500     03 MID-IDINLVGN-UT      PIC X(3).                                    
000600*                                 VAGNSIDENTITET                          
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDINLVGN-ENTER   PIC 9(3).                                    
001200*                                 VAGNSIDENTITET                          
001300     03 MID-IDINLVGN-NEXT    PIC 9(3).                                    
001400*                                 VAGNSIDENTITET                          
001500     03 MID-RADER            OCCURS 12 TIMES.                             
001600*                                 TABELL RADER                            
001700        05 MID-IDINLVGN      PIC 9(3).                                    
001800*                                 VAGNSIDENTITET                          
001900     03 MID-INPUT.                                                        
002000*                                 INDATA FÖR UPPDATERING                  
002100        05 MID-KDCMDVAL-UPP  PIC X(3).                                    
002200*                                 GENERELL KOMMANDOKOD                    
002300        05 MID-IDINLVGN-UPP  PIC 9(3).                                    
002400*                                 VAGNSIDENTITET                          
002500        05 MID-ADINLOMR-UPP  PIC X(4).                                    
002600*                                 INLEVERANSOMRÅDE                        
002700        05 MID-ADINLOMR-NXT-UPP                                           
002800                             PIC X(4).                                    
002900*                                 INLEVERANSOMRÅDE NÄSTA                  
