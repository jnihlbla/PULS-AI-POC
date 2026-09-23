000100 01  REQU-WL0148I1.                                                       
000200*                                 REQUEST TO PGM WL0148                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 REQU-IDRTLOP-KEY     PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 REQU-INPUT.                                                       
001000*                                 INMATNINGSFÄLT                          
001100        05 REQU-LOSS.                                                     
001200*                                 INMATNINGSFÄLT                          
001300           07 REQU-KVKOLLI-LOSS-UPD                                       
001400                             PIC 9(4).                                    
001500*                                 ANTAL KOLLI                             
001600           07 REQU-IDANSTNR-LOSS-UPD                                      
001700                             PIC 9(5).                                    
001800*                                 ANSTÄLLNINGSNUMMER                      
001900           07 REQU-ADINLOMR-LOSS-UPD                                      
002000                             PIC X(4).                                    
002100*                                 INLEVERANSOMRÅDE                        
002200           07 REQU-IDFRASED-LOSS-UPD                                      
002300                             PIC X(15).                                   
002400*                                 FRAKTSEDELSNUMMER                       
002500        05 REQU-KOLLI.                                                    
002600*                                 INMATNINGSFÄLT                          
002700           07 REQU-IDKOLLI   PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900           07 REQU-IDDISTR   PIC 9(4).                                    
003000*                                 DISTRIKTNUMMER                          
003100           07 REQU-IDKOLLI-FOM                                            
003200                             PIC 9(5).                                    
003300*                                 KOLLINUMMER                             
003400           07 REQU-IDKOLLI-TOM                                            
003500                             PIC 9(5).                                    
003600*                                 KOLLINUMMER                             
003700        05 REQU-INPUT-RAD    OCCURS 8 TIMES.                              
003800*                                 INMATNINGSFÄLT                          
003900           07 REQU-IDKUNDNR  PIC 9(6).                                    
004000*                                 KUNDNUMMER                              
004100           07 REQU-IDRAPPNR  PIC 9(7).                                    
004200*                                 RAPPORT NUMMER                          
004300           07 REQU-KVKOLLI   PIC 9(4).                                    
004400*                                 ANTAL KOLLI                             
004500           07 REQU-IDFRASED  PIC X(15).                                   
004600*                                 FRAKTSEDELSNUMMER                       
004700           07 REQU-TERETNOT  PIC X(20).                                   
004800*                                 FRI NOTERING RETURER                    
004900*** END OF VILMAII-COPY LENGTH= 471 BYTES                                 
