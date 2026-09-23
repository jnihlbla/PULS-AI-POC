000100 01  REQU-WL0120I1.                                                       
000200*                                 REQUEST TO PGM      WL0120              
000300*                                                                         
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 REQU-IDANSTNR-KEY    PIC 9(5).                                    
000700*                                 ANSTÄLLNINGSNUMMER                      
000800     03 REQU-FLPREPRINT-KEY  PIC X.                                       
000900*                                 FLAGGA PRE PRINT                        
001000     03 REQU-IDQUEUENR-KEY   PIC 9(3).                                    
001100*                                 PRE PRINT QUEUE NUMBER                  
001200     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 REQU-IDORDNR-KEY     PIC 9(5).                                    
001700*                                 ORDERNUMMER                             
001800     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
001900*                                 KOLLINUMMER                             
002000     03 REQU-IDPRODNR-KEY    PIC 9(7).                                    
002100*                                 PRODUKTIONSNUMMER                       
002200     03 REQU-KVRADER         PIC 9(5).                                    
002300*                                 ANTAL RADER                             
002400     03 REQU-RAD             OCCURS 500 TIMES.                            
002500*                                 REQUEST-COPYTEXT FÖR WL0120             
002600*                                                                         
002700        05 REQU-IDPRODNR     PIC 9(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900        05 REQU-IDRADNR-ORD-TOM                                           
003000                             PIC 9(4).                                    
003100*                                 RADNUMMER PÅ VOLVOORDER TOM             
003200        05 REQU-IDANSTNR-OLD PIC 9(5).                                    
003300*                                 ANSTÄLLNINGSNUMMER                      
003400        05 REQU-IDQUEUENR    PIC 9(3).                                    
003500*                                 PRE PRINT QUEUE NUMBER                  
003600        05 REQU-IDANSTNR-NEW PIC 9(5).                                    
003700*                                 ANSTÄLLNINGSNUMMER                      
003800*** END OF VILMAII-COPY LENGTH= 12043 BYTES                               
