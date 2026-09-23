000100 01  MID-W3I21201.                                                        
000200*                                 COPYTEXT FÖR MID W3I21201               
000300*                                                                         
000400     03 MID-KDMARK-BUDG-ENTER                                             
000500                             PIC X(3).                                    
000600*                                 MARKNADSKOD BUDGET 96 MARKNADER         
000700     03 MID-KDPRODSL-ENTER   PIC X(2).                                    
000800*                                 PRODUKTSLAG                             
000900     03 MID-IDFKNGRP-ENTER   PIC X(4).                                    
001000*                                 FUNKTIONSGRUPP                          
001100     03 MID-KDMARK-BUDG-PFK8 PIC X(3).                                    
001200*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001300     03 MID-KDPRODSL-PFK8    PIC X(2).                                    
001400*                                 PRODUKTSLAG                             
001500     03 MID-IDFKNGRP-PFK8    PIC X(4).                                    
001600*                                 FUNKTIONSGRUPP                          
001700     03 MID-INRAD-KDMARK-BUDG                                             
001800                             PIC X(3).                                    
001900*                                 MARKNADSKOD BUDGET 96 MARKNADER         
002000     03 MID-INRAD-KDPRODSL   PIC X(2).                                    
002100*                                 PRODUKTSLAG                             
002200     03 MID-INRAD-IDFKNGRP   PIC X(4).                                    
002300*                                 FUNKTIONSGRUPP                          
002400     03 MID-INRAD-SUTOTFSG-BUDG                                           
002500                             PIC X(14).                                   
002600*                                 BUDGETERAT FÖRSÄLJNINGSVÄRDE            
002700     03 MID-INRAD-IDSKURVA   PIC X(2).                                    
002800*                                 SÄSONGSKURVA                            
002900     03 MID-INRAD-KDCMD      PIC X.                                       
003000      88 MID-KDCMD-INGENTING VALUE ' '.                                   
003100      88 MID-KDCMD-DELETE    VALUE 'D'                                    
003200                             'B'.                                         
003300      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
003400                             'Ä'.                                         
003500      88 MID-KDCMD-INSERT    VALUE 'I'                                    
003600                             'N'.                                         
003700*                                 RAD-UPPDATERINGSKOMMANDO                
003800*** END COPY W3I21201C0  LENGTH=44                                        
