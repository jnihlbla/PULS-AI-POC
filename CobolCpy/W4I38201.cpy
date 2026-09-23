000100 01  MID-W4I38201.                                                        
000200*                                 COPYTEXT FOR MID W4I38201               
000300     03 MID-KDPRCGRP-IN      PIC X(5).                                    
000400*                                 PRODUKTIONSKANALSGRUPP                  
000500     03 MID-KDPRCGRP-OUT     PIC X(5).                                    
000600*                                 PRODUKTIONSKANALSGRUPP                  
000700     03 MID-KDPRODKL-IN      PIC X.                                       
000800*                                 PRODUKTIONSKLASS                        
000900     03 MID-KDPRODKL-OUT     PIC X.                                       
001000*                                 PRODUKTIONSKLASS                        
001100     03 MID-IDPRC-IN.                                                     
001200*                                 PRODUKTIONSKANAL                        
001300        05 MID-IDPRCBAS      PIC X(3).                                    
001400*                                 PRC-BAS                                 
001500        05 MID-IDPRCVAR      PIC X.                                       
001600*                                 PRC-VARIANT                             
001700     03 MID-IDPRC-OUT.                                                    
001800*                                 PRODUKTIONSKANAL                        
001900        05 MID-IDPRCBAS      PIC X(3).                                    
002000*                                 PRC-BAS                                 
002100        05 MID-IDPRCVAR      PIC X.                                       
002200*                                 PRC-VARIANT                             
002300     03 MID-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MID-IDDC-OUT         PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MID-IDORDER-ENTER    PIC X(7).                                    
002800*                                 VOLVO PARTS ORDERNUMMER                 
002900     03 MID-TIRFS-ENTER      PIC X(10).                                   
003000*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
003100     03 MID-TILST-O-ENTER    PIC X(10).                                   
003200*                                 SENASTE STARTTIDPUNKT F÷R ORDER         
003300     03 MID-IDPRCVAR-ENTER   OCCURS 10 TIMES                              
003400                             PIC X.                                       
003500*                                 PRC-VARIANT                             
003600     03 MID-IDORDER-NEXT     PIC X(7).                                    
003700*                                 VOLVO PARTS ORDERNUMMER                 
003800     03 MID-TIRFS-NEXT       PIC X(10).                                   
003900*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
004000     03 MID-TILST-O-NEXT     PIC X(10).                                   
004100*                                 SENASTE STARTTIDPUNKT F÷R ORDER         
004200     03 MID-IDPRCVAR-NEXT    OCCURS 10 TIMES                              
004300                             PIC X.                                       
004400*                                 PRC-VARIANT                             
