000100 01  MID-W4I48101.                                                        
000200*                                                                         
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-FLSHOW           PIC X.                                       
001200     03 MID-INPUT.                                                        
001300*                                                                         
001400        05 MID-BEGMT-RAD1-NEW                                             
001500                             PIC X(35).                                   
001600*                                 GODSMOTTAGARNAMN RAD 1                  
001700        05 MID-BEGMT-RAD2-NEW                                             
001800                             PIC X(35).                                   
001900*                                 GODSMOTTAGARNAMN RAD 2                  
002000        05 MID-ADGMT-GATA-NEW                                             
002100                             PIC X(35).                                   
002200*                                 GODSMOTTAGARADRESS GATA                 
002300        05 MID-ADPOSTNR      PIC X(10).                                   
002400*                                 POSTNUMMER I ADRESS                     
002500        05 MID-KDPOSTNR      PIC X.                                       
002600*                                 OM/HUR POSTNUMMER JUSTERATS             
002700        05 MID-ADCITY        PIC X(25).                                   
002800*                                 BENÄMNING PÅ STAD                       
002900        05 MID-ADGMT-LAND-NEW                                             
003000                             PIC X(35).                                   
003100*                                 GODSMOTTAGARADRESS LAND                 
003200*** END OF VILMAII-COPY LENGTH= 197 BYTES                                 
