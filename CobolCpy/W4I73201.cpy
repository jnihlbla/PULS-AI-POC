000100 01  MID-W4I73201.                                                        
000200*                                 MID-COPYTEXT FÖR W40732                 
000300     03 MID-IDRT-IN          PIC X(3).                                    
000400*                                 RETURTERMINAL                           
000500     03 MID-IDRTLOP-IN       PIC 9(3).                                    
000600*                                 RETUR TERMINAL LÖPNUMMER                
000700     03 MID-IDRT-UT          PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 MID-IDRTLOP-UT       PIC 9(3).                                    
001000*                                 RETUR TERMINAL LÖPNUMMER                
001100     03 MID-IDKOLLI-IN       PIC X(5).                                    
001200*                                 KOLLINUMMER                             
001300     03 MID-IDKOLLI-UT       PIC X(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 MID-INPUT            OCCURS 13 TIMES.                             
001600*                                 INMATNINGSFÄLT                          
001700        05 MID-KDCMD         PIC X(4).                                    
001800        05 MID-ADINLOMR-UPD  PIC X(4).                                    
001900*                                 INLEVERANSOMRÅDE                        
002000     03 MID-KEYFIELD         OCCURS 13 TIMES.                             
002100*                                 NYCKELFÄLT PÅ RADEN                     
002200        05 MID-IDKOLLI       PIC 9(5).                                    
002300*                                 KOLLINUMMER                             
002400*** END OF VILMAII-COPY LENGTH= 191 BYTES                                 
