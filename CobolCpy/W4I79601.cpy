000100 01  MID-W4I79601.                                                        
000200*                                 MID-COPYTEXT FÖR W4079600               
000300     03 MID-IDDISTR          PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDRAPPNR         PIC X(7).                                    
000800*                                 RAPPORT NUMMER                          
000900     03 MID-RELANDCO         PIC X(6).                                    
001000*                                 LANDING COST PROCENT                    
001100     03 MID-PRFRAKT          PIC X(10).                                   
001200*                                 FRAKTKOSTNAD                            
001300     03 MID-PRFOERS          PIC X(10).                                   
001400*                                 FÖRSÄKRINGSPREMIE                       
001500     03 MID-PRLEGKST         PIC X(10).                                   
001600*                                 LEGALISERINSKOSTNAD                     
001700     03 MID-IDORDNR5         PIC X(5).                                    
001800*                                 ORDERNUMMER                             
001900     03 MID-IDFAKT           PIC X(7).                                    
002000*                                 FAKTURANUMMER                           
002100     03 MID-IDPRODNR         PIC X(7).                                    
002200*                                 PRODUKTIONSNUMMER                       
002300     03 MID-KOLLI-FROM-TO    OCCURS 13 TIMES.                             
002400*                                 KOLLI INTERVALL                         
002500        05 MID-IDKOLLI-FOM   PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700        05 MID-IDKOLLI-TOM   PIC X(5).                                    
002800*                                 KOLLINUMMER                             
002900     03 MID-OMSTART-NYCKLAR.                                              
003000*                                 NYCKLAR FÖR OMSTART AV PGM              
003100        05 MID-IDPLKLST      PIC X(3).                                    
003200*                                 PLOCKLISTNUMMER                         
003300        05 MID-IDPURAD       PIC X(5).                                    
003400*                                 RADNUMMER PÅ PACKUNDERLAG               
003500        05 MID-IDKOLLI       PIC X(5).                                    
003600*                                 KOLLINUMMER                             
003700        05 MID-RAD-IX        PIC X(3).                                    
003800*** END OF VILMAII-COPY LENGTH= 218 BYTES                                 
