000100 01  MID-W4I31201.                                                        
000200*                                 MID-COPYTEXT FÖR W40312                 
000300     03 MID-IDANSTNR-IN      PIC X(5).                                    
000400*                                 ANSTÄLLNINGSNUMMER                      
000500     03 MID-IDANSTNR-UT      PIC X(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 MID-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR-IN       PIC X(5).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-IDORDNR-UT       PIC X(5).                                    
001800*                                 ORDERNUMMER                             
001900     03 MID-IDKOLLI-IN       PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100     03 MID-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MID-IDPRODNR-IN      PIC X(7).                                    
002400*                                 PRODUKTIONSNUMMER                       
002500     03 MID-IDPRODNR-UT      PIC X(7).                                    
002600*                                 PRODUKTIONSNUMMER                       
002700     03 MID-IDDC-IN          PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MID-IDDC-UT          PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MID-IDPRODNR-NEXT    PIC 9(7).                                    
003200*                                 PRODUKTIONSNUMMER                       
003300     03 MID-IDPLKLST-NEXT    PIC 9(3).                                    
003400*                                 PLOCKLISTNUMMER                         
003500     03 MID-KDPRCGRP-NEXT    PIC X(5).                                    
003600*                                 PRODUKTIONSKANALSGRUPP                  
003700     03 MID-TIRFS-NEXT       PIC 9(10).                                   
003800*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
003900     03 MID-IDPRODNR-ENTER   PIC 9(7).                                    
004000*                                 PRODUKTIONSNUMMER                       
004100     03 MID-IDPLKLST-ENTER   PIC 9(3).                                    
004200*                                 PLOCKLISTNUMMER                         
004300     03 MID-KDPRCGRP-ENTER   PIC X(5).                                    
004400*                                 PRODUKTIONSKANALSGRUPP                  
004500     03 MID-TIRFS-ENTER      PIC 9(10).                                   
004600*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
004700     03 MID-KDPRCGRP-4324    PIC X(5).                                    
004800*                                 PRODUKTIONSKANALSGRUPP                  
004900     03 MID-RAD              OCCURS 14 TIMES.                             
005000*                                 MID-COPYTEXT FÖR W40312                 
005100        05 MID-IDPRODNR      PIC 9(7).                                    
005200*                                 PRODUKTIONSNUMMER                       
005300        05 MID-IDRADNR-ORD-TOM                                            
005400                             PIC 9(4).                                    
005500*                                 RADNUMMER PÅ VOLVOORDER TOM             
005600        05 MID-IDANSTNR      PIC 9(5).                                    
005700*                                 ANSTÄLLNINGSNUMMER                      
005800        05 MID-IDANSTNR-NEW  PIC 9(5).                                    
005900*                                 ANSTÄLLNINGSNUMMER                      
006000*** END OF VILMAII-COPY LENGTH= 417 BYTES                                 
