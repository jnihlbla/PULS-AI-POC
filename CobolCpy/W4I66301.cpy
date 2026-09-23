000100 01  MID-W4I66301.                                                        
000200*                                 MID-COPYTEXT FÖR W40663                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 MID-IDTRPTNR-UT      PIC X(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 MID-IDLBBET-IN       PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 MID-IDLBBET-UT       PIC X(12).                                   
001000*                                 LASTBÄRARBETECKNING                     
001100     03 MID-FLFARLIG-IN      PIC X.                                       
001200*                                 FARLIGT GODS-FLAGGA                     
001300     03 MID-FLFARLIG-UT      PIC X.                                       
001400*                                 FARLIGT GODS-FLAGGA                     
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-FLSIDLAST        PIC X.                                       
002000*                                 LASTA HEL SIDA?                         
002100     03 MID-RADER            OCCURS 10 TIMES.                             
002200*                                 MID-COPYTEXT FÖR W40663                 
002300        05 MID-FLLASTA-RAD   PIC X.                                       
002400*                                 SKA ORDERN LASTAS ?                     
002500        05 MID-IDDISTR       PIC 9(4).                                    
002600*                                 DISTRIKTNUMMER                          
002700        05 MID-IDKUNDNR      PIC 9(6).                                    
002800*                                 KUNDNUMMER                              
002900        05 MID-IDORDNR7      PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100        05 MID-IDKOLLI       PIC 9(5).                                    
003200*                                 KOLLINUMMER                             
003300        05 MID-IDPRODNR      PIC 9(7).                                    
003400*                                 PRODUKTIONSNUMMER                       
003500*** END OF VILMAII-COPY LENGTH= 337 BYTES                                 
