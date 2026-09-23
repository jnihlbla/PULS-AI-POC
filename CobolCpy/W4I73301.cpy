000100 01  MID-W4I73301.                                                        
000200*                                 MID-COPYTEXT FÖR W40733                 
000300     03 MID-IDRT-IN          PIC X(3).                                    
000400*                                 RETURTERMINAL                           
000500     03 MID-IDRTLOP-IN       PIC X(3).                                    
000600*                                 RETUR TERMINAL LÖPNUMMER                
000700     03 MID-IDRT-UT          PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 MID-IDRTLOP-UT       PIC X(3).                                    
001000*                                 RETUR TERMINAL LÖPNUMMER                
001100     03 MID-INPUT.                                                        
001200*                                 INMATNINGSFÄLT                          
001300        05 MID-FLNYSND       PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500        05 MID-LOSS.                                                      
001600*                                 INMATNINGSFÄLT                          
001700           07 MID-KVKOLLI-LOSS                                            
001800                             PIC X(4).                                    
001900*                                 ANTAL KOLLI                             
002000           07 MID-IDANSTNR-LOSS                                           
002100                             PIC X(5).                                    
002200*                                 ANSTÄLLNINGSNUMMER                      
002300           07 MID-ADINLOMR-LOSS                                           
002400                             PIC X(4).                                    
002500*                                 INLEVERANSOMRÅDE                        
002600           07 MID-IDFRASED-LOSS                                           
002700                             PIC X(15).                                   
002800*                                 FRAKTSEDELSNUMMER                       
002900        05 MID-MOT.                                                       
003000*                                 INMATNINGSFÄLT                          
003100           07 MID-KVKOLLI-MOT                                             
003200                             PIC X(4).                                    
003300*                                 ANTAL KOLLI                             
003400           07 MID-IDANSTNR-MOT                                            
003500                             PIC X(5).                                    
003600*                                 ANSTÄLLNINGSNUMMER                      
003700           07 MID-ADINLOMR-MOT                                            
003800                             PIC X(4).                                    
003900*                                 INLEVERANSOMRÅDE                        
004000           07 MID-IDFRASED-MOT                                            
004100                             PIC X(15).                                   
004200*                                 FRAKTSEDELSNUMMER                       
004300        05 MID-KOLLI.                                                     
004400*                                 INMATNINGSFÄLT                          
004500           07 MID-IDKOLLI    PIC X(5).                                    
004600*                                 KOLLINUMMER                             
004700           07 MID-IDDISTR    PIC X(4).                                    
004800*                                 DISTRIKTNUMMER                          
004900           07 MID-IDKOLLI-FOM                                             
005000                             PIC X(5).                                    
005100*                                 KOLLINUMMER                             
005200           07 MID-IDKOLLI-TOM                                             
005300                             PIC X(5).                                    
005400*                                 KOLLINUMMER                             
005500        05 MID-INPUT-RAD     OCCURS 8 TIMES.                              
005600*                                 INMATNINGSFÄLT                          
005700           07 MID-IDKUNDNR   PIC X(6).                                    
005800*                                 KUNDNUMMER                              
005900           07 MID-IDRAPPNR   PIC X(7).                                    
006000*                                 RAPPORT NUMMER                          
006100           07 MID-KVKOLLI    PIC X(4).                                    
006200*                                 ANTAL KOLLI                             
006300           07 MID-IDFRASED   PIC X(15).                                   
006400*                                 FRAKTSEDELSNUMMER                       
006500           07 MID-TERETNOT   PIC X(20).                                   
006600*                                 FRI NOTERING RETURER                    
006700*** END OF VILMAII-COPY LENGTH= 504 BYTES                                 
