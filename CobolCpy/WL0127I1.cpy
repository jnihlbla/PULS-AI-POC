000100 01  REQU-WL0127I1.                                                       
000200*                                 REQUEST TO PGM WL0127                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDANSTNR-KEY    PIC 9(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-IDORDNR-KEY     PIC 9(5).                                    
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 REQU-IDPRODNR-KEY    PIC 9(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700     03 REQU-IDRADNR-START-KEY                                            
001800                             PIC 9(4).                                    
001900*                                 RADNUMMER                               
002000     03 REQU-FLBACKA-ALLA    PIC X.                                       
002100*                                 ALLMÄN SVARSFLAGGA                      
002200     03 REQU-KVRADER         PIC 9(5).                                    
002300*                                 ANTAL RADER                             
002400     03 REQU-RAD             OCCURS 500 TIMES.                            
002500        05 REQU-IDRADNR      PIC 9(4).                                    
002600*                                 RADNUMMER                               
002700        05 REQU-KVLEVART     PIC 9(7).                                    
002800*                                 LEVERERAT ANTAL STYCK                   
002900        05 REQU-FLBACKA      PIC X.                                       
003000*                                 ALLMÄN SVARSFLAGGA                      
003100*** END OF VILMAII-COPY LENGTH= 6044 BYTES                                
