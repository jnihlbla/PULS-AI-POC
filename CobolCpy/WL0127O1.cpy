000100 01  RESP-WL0127O1.                                                       
000200*                                 RESPONS FROM PGM WL0127                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSTNR-KEY    PIC Z(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 RESP-IDDISTR-KEY     PIC Z(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 RESP-IDKUNDNR-KEY    PIC Z(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 RESP-IDORDNR-KEY     PIC Z(5).                                    
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 RESP-IDKOLLI-KEY     PIC Z(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDPRODNR-KEY    PIC Z(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700     03 RESP-IDRADNR-START-KEY                                            
001800                             PIC Z(3)9.                                   
001900*                                 RADNUMMER                               
002000     03 RESP-FLBACKA-ALLA    PIC X.                                       
002100*                                 ALLMÄN SVARSFLAGGA                      
002200     03 RESP-KVRADER         PIC Z(4)9.                                   
002300*                                 ANTAL RADER                             
002400     03 RESP-RAD             OCCURS 500 TIMES.                            
002500        05 RESP-IDRADNR      PIC Z(3)9.                                   
002600*                                 RADNUMMER                               
002700        05 RESP-KVLEVART     PIC Z(6)9.                                   
002800*                                 LEVERERAT ANTAL STYCK                   
002900        05 RESP-FLBACKA      PIC X.                                       
003000*                                 ALLMÄN SVARSFLAGGA                      
003100        05 RESP-IDMSG-ERROR-LINE                                          
003200                             PIC X(3).                                    
003300*                                 FELMEDDELANDE ID                        
003400*** END OF VILMAII-COPY LENGTH= 7544 BYTES                                
