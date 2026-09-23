000100 01  W4O74101.                                                            
000200*                                 MODCOPYTEXT TILL W40741.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDDISTR-IN           PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 IDDISTR-UT           PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 INPUT.                                                            
001200*                                                                         
001300        05 RADER             OCCURS 14 TIMES.                             
001400*                                                                         
001500           07 IDKUNDNR-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700           07 IDKUNDNR       PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900           07 IDRAPPNR-ATTR  PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100           07 IDRAPPNR       PIC X(7).                                    
002200*                                 RAPPORT NUMMER                          
002300           07 KVKOLLI-AF-ATTR                                             
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600           07 KVKOLLI-AF     PIC X(4).                                    
002700*                                 ANTAL KOLLI                             
002800           07 IDFRASED-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000           07 IDFRASED       PIC X(15).                                   
003100*                                 FRAKTSEDELSNUMMER                       
003200           07 TERETNOT-ATTR  PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400           07 TERETNOT       PIC X(20).                                   
003500*                                 FRI NOTERING RETURER                    
003600     03 TEMFSINF             PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 975 BYTES                                 
