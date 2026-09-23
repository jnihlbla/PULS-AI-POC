000100 01  RESP-WL0148O1.                                                       
000200*                                 RESPONS FROM PGM WL0148                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 RESP-IDRTLOP-KEY     PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 RESP-OUTPUT.                                                      
001000*                                                                         
001100        05 RESP-KVKOLLI-MOT  PIC Z(4).                                    
001200*                                 ANTAL KOLLI                             
001300        05 RESP-IDANSTNR-MOT PIC Z(5).                                    
001400*                                 ANSTÄLLNINGSNUMMER                      
001500        05 RESP-ADINLOMR-MOT PIC X(4).                                    
001600*                                 INLEVERANSOMRÅDE                        
001700        05 RESP-IDFRASED-MOT PIC X(15).                                   
001800*                                 FRAKTSEDELSNUMMER                       
001900     03 RESP-INPUT.                                                       
002000*                                                                         
002100        05 RESP-SND.                                                      
002200*                                                                         
002300           07 RESP-KVKOLLI-LOSS-UPD                                       
002400                             PIC Z(4).                                    
002500*                                 ANTAL KOLLI                             
002600           07 RESP-IDANSTNR-LOSS-UPD                                      
002700                             PIC Z(5).                                    
002800*                                 ANSTÄLLNINGSNUMMER                      
002900           07 RESP-ADINLOMR-LOSS-UPD                                      
003000                             PIC X(4).                                    
003100*                                 INLEVERANSOMRÅDE                        
003200           07 RESP-IDFRASED-LOSS-UPD                                      
003300                             PIC X(15).                                   
003400*                                 FRAKTSEDELSNUMMER                       
003500        05 RESP-KOLLI.                                                    
003600*                                                                         
003700           07 RESP-IDKOLLI   PIC Z(5).                                    
003800*                                 KOLLINUMMER                             
003900           07 RESP-IDDISTR   PIC Z(4).                                    
004000*                                 DISTRIKTNUMMER                          
004100           07 RESP-IDKOLLI-FOM                                            
004200                             PIC Z(5).                                    
004300*                                 KOLLINUMMER                             
004400           07 RESP-IDKOLLI-TOM                                            
004500                             PIC Z(5).                                    
004600*                                 KOLLINUMMER                             
004700     03 RESP-RADER           OCCURS 8 TIMES.                              
004800*                                                                         
004900        05 RESP-IDKUNDNR     PIC Z(6).                                    
005000*                                 KUNDNUMMER                              
005100        05 RESP-IDRAPPNR     PIC Z(7).                                    
005200*                                 RAPPORT NUMMER                          
005300        05 RESP-KVKOLLI      PIC Z(4).                                    
005400*                                 ANTAL KOLLI                             
005500        05 RESP-IDFRASED     PIC X(15).                                   
005600*                                 FRAKTSEDELSNUMMER                       
005700        05 RESP-TERETNOT     PIC X(20).                                   
005800*                                 FRI NOTERING RETURER                    
005900        05 RESP-IDMSG-ERROR-LINE                                          
006000                             PIC X(3).                                    
006100*                                 FELMEDDELANDE ID                        
006200*** END OF VILMAII-COPY LENGTH= 523 BYTES                                 
