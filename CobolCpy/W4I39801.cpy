000100 01  MID-W4I39801.                                                        
000200*                                 MID-COPYTEXT PGM W40398                 
000300*                                 KONTROLL EJ RAPPORTERAT                 
000400     03 MID-IDANSTNR-IN      PIC X(5).                                    
000500*                                 ANSTÄLLNINGSNUMMER                      
000600     03 MID-IDANSTNR-UT      PIC X(5).                                    
000700*                                 ANSTÄLLNINGSNUMMER                      
000800     03 MID-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MID-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MID-IDORDNR-IN       PIC X(5).                                    
001700*                                 ORDERNUMMER                             
001800     03 MID-IDORDNR-UT       PIC X(5).                                    
001900*                                 ORDERNUMMER                             
002000     03 MID-IDKOLLI-IN       PIC X(5).                                    
002100*                                 KOLLINUMMER                             
002200     03 MID-IDKOLLI-UT       PIC X(5).                                    
002300*                                 KOLLINUMMER                             
002400     03 MID-IDPRODNR-IN      PIC X(7).                                    
002500*                                 PRODUKTIONSNUMMER                       
002600     03 MID-IDPRODNR-UT      PIC X(7).                                    
002700*                                 PRODUKTIONSNUMMER                       
002800     03 MID-IDTRANS-START    PIC X(4).                                    
002900*                                 BILDNUMMER                              
003000     03 MID-RAD              OCCURS 26 TIMES.                             
003100        05 MID-IDRADNR       PIC 9(4).                                    
003200*                                 RADNUMMER                               
003300        05 MID-KVORAPP       PIC 9(6).                                    
003400*                                 EJ-RAPPORTERAT-ANTAL                    
003500     03 MID-FLSVAR           PIC X.                                       
003600*                                 ALLMÄN SVARSFLAGGA                      
003700*** END OF VILMAII-COPY LENGTH= 329 BYTES                                 
