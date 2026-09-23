000100 01  MOD-W6O12501.                                                        
000200*                                 COPYTEXT FOR MOD W6O12501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDINLVGN-IN      PIC X(3).                                    
000800*                                 VAGNSIDENTITET                          
000900     03 MOD-ADINLOMR-IN      PIC X(4).                                    
001000*                                 INLEVERANSOMRÅDE                        
001100     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
001200*                                 INLEVERANSOMRÅDE NÄSTA                  
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDINLVGN-UT      PIC X(3).                                    
001600*                                 VAGNSIDENTITET                          
001700     03 MOD-ADINLOMR-UT      PIC X(4).                                    
001800*                                 INLEVERANSOMRÅDE                        
001900     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
002000*                                 INLEVERANSOMRÅDE NÄSTA                  
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-SPAR-IDINLVGN    PIC 9(3).                                    
002400*                                 VAGNSIDENTITET                          
002500     03 MOD-SPAR-ADINLOMR    PIC X(4).                                    
002600*                                 INLEVERANSOMRÅDE                        
002700     03 MOD-SPAR-ADINLOMR-NXT                                             
002800                             PIC X(4).                                    
002900*                                 INLEVERANSOMRÅDE NÄSTA                  
003000     03 MOD-RAD              OCCURS 14 TIMES.                             
003100*                                 LINES                                   
003200        05 MOD-IDLOPNRM-ATTR PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-IDLOPNRM      PIC X(9).                                    
003500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003600*                                 (0VVDLLLLK)                             
003700        05 MOD-INFO          PIC X(40).                                   
003800     03 MOD-TEMFSINF         PIC X(55).                                   
003900*                                 INFORMATIONSMEDDELANDE                  
