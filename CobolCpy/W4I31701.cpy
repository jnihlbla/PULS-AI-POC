000100 01  MID-W4I31701.                                                        
000200*                                 MID-COPYTEXT PGM W40317                 
000300*                                 AVVIKELSE URSPRUNG                      
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
001700*                                 ORDERNUMMER UTGÅR PD90                  
001800     03 MID-IDORDNR-UT       PIC X(5).                                    
001900*                                 ORDERNUMMER UTGÅR PD90                  
002000     03 MID-IDKOLLI-IN       PIC X(5).                                    
002100*                                 KOLLINUMMER                             
002200     03 MID-IDKOLLI-UT       PIC X(5).                                    
002300*                                 KOLLINUMMER                             
002400     03 MID-IDPRODNR-IN      PIC X(7).                                    
002500*                                 PRODUKTIONSNUMMER                       
002600     03 MID-IDPRODNR-UT      PIC X(7).                                    
002700*                                 PRODUKTIONSNUMMER                       
002800     03 MID-IDDC-IN          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MID-IDDC-UT          PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MID-IDTRANS-START    PIC X(4).                                    
003300*                                 BILDNUMMER                              
003400     03 MID-FLSISTAK         PIC X.                                       
003500*                                 SISTA KOLLI I ORDERN?                   
003600     03 MID-IDRADNR-S        PIC 9(4).                                    
003700*                                 RADNUMMER                               
003800     03 MID-KDARTURS-S       PIC X(2).                                    
003900*                                 ARTIKELURSPRUNGSKOD                     
004000     03 MID-RAD              OCCURS 13 TIMES.                             
004100        05 MID-IDRADNR       PIC X(4).                                    
004200*                                 RADNUMMER                               
004300        05 MID-KDARTURS      PIC X(2).                                    
004400*                                 ARTIKELURSPRUNGSKOD                     
004500*** END OF VILMAII-COPY LENGTH= 157 BYTES                                 
