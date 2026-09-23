000100 01  REQU-W40704I1.                                                       
000200*                                 MID-COPYTEXT FÖR W4070400               
000300     03 REQU-IDDISTR         PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 REQU-IDKUNDNR        PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 REQU-IDRAPPNR        PIC X(7).                                    
000800*                                 RAPPORT NUMMER                          
000900     03 REQU-KVRADER         PIC 9(5).                                    
001000*                                 ANTAL RADER                             
001100     03 REQU-INFO-RAD        OCCURS 1 TO 999 TIMES                        
001200                             DEPENDING ON REQU-KVRADER.                   
001300*                                 RADINFORMATION                          
001400        05 REQU-IDORDNR      PIC X(5).                                    
001500*                                 ORDERNUMMER UTGÅR PD90                  
001600        05 REQU-IDFAKT       PIC X(7).                                    
001700*                                 FAKTURANUMMER                           
001800        05 REQU-IDKOLLI      PIC X(5).                                    
001900*                                 KOLLINUMMER                             
002000        05 REQU-IDARTNR      PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200        05 REQU-KVLEVANM     PIC X(6).                                    
002300*                                 LEVERANSANMÄRKNINGSANTAL                
002400        05 REQU-KDANMORS     PIC X(2).                                    
002500*                                 ORSAK TILL LEVERANSANMÄRKNING           
002600*** END OF VILMAII-COPY LENGTH= 33988 BYTES                               
