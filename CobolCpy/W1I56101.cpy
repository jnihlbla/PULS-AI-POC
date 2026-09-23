000100 01  MID-W1I56101.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 ILLUSTRATIONSREGISTER                   
000400     03 MID-IDILLU-IN        PIC X(5).                                    
000500*                                 ILLUSTRATIONENS NR                      
000600     03 MID-IDILLU-UT        PIC X(5).                                    
000700*                                 ILLUSTRATIONENS NR                      
000800     03 MID-IDCATNR-NEXT     PIC 9(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MID-IDCATGRP-NEXT    PIC 9(2).                                    
001100*                                 KATALOG-GRUPP                           
001200     03 MID-IDCATAVS-NEXT    PIC 9(4).                                    
001300*                                 KATALOG-AVSNITT                         
001400     03 MID-KDCATPUB-NEXT    PIC X(6).                                    
001500*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
001600     03 MID-IDCATNR          PIC 9(5).                                    
001700*                                 KATALOG-ID                              
001800     03 MID-IDILLU-1         PIC 9(5).                                    
001900*                                 ILLUSTRATIONENS NR                      
002000     03 MID-IDILLU-2         PIC 9(5).                                    
002100*                                 ILLUSTRATIONENS NR                      
002200     03 MID-IDILLU-3         PIC 9(5).                                    
002300*                                 ILLUSTRATIONENS NR                      
002400     03 MID-IDSUBNR          PIC 9.                                       
002500*                                 SUBMAPPNUMBER                           
002600     03 MID-IDRUBNR          OCCURS 5 TIMES                               
002700                             PIC 9(5).                                    
002800*                                 RUBRIKNUMMER                            
002900     03 MID-TENOTE           PIC X(40).                                   
003000*                                 NOTERINGSFÄLT                           
003100*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
