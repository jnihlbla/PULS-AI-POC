000100 01  MID-W4I73401.                                                        
000200*                                 MID-COPYTEXT FÖR W40734                 
000300     03 MID-IDANSV-IN        PIC X(6).                                    
000400     03 MID-IDANSV-UT        PIC X(6).                                    
000500     03 MID-IDRT-IN          PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 MID-IDRTLOP-IN       PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 MID-IDRT-UT          PIC X(3).                                    
001000*                                 RETURTERMINAL                           
001100     03 MID-IDRTLOP-UT       PIC 9(3).                                    
001200*                                 RETUR TERMINAL LÖPNUMMER                
001300     03 MID-IDKOLLI-IN       PIC X(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 MID-IDKOLLI-UT       PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MID-IDDISTR-IN       PIC 9(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MID-IDDISTR-UT       PIC 9(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MID-FLVISAAV-IN      PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300     03 MID-FLVISAAV-UT      PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MID-INPUT.                                                        
002600*                                 INMATNINGSFÄLT                          
002700        05 MID-IDANSTNR      PIC X(5).                                    
002800*                                 ANSTÄLLNINGSNUMMER                      
002900        05 MID-INPUTLINE     OCCURS 13 TIMES.                             
003000*                                 INMATNINGSFÄLT                          
003100           07 MID-KDCMD      PIC X(4).                                    
003200     03 MID-KEYFIELD         OCCURS 13 TIMES.                             
003300*                                 NYCKELFÄLT PÅ RADEN                     
003400        05 MID-IDRT          PIC X(3).                                    
003500*                                 RETURTERMINAL                           
003600        05 MID-IDRTLOP       PIC 9(3).                                    
003700*                                 RETUR TERMINAL LÖPNUMMER                
003800        05 MID-IDKOLLI       PIC X(5).                                    
003900*                                 KOLLINUMMER                             
004000*** END OF VILMAII-COPY LENGTH= 244 BYTES                                 
