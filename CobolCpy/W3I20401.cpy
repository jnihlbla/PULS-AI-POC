000100 01  MID-W3I20401.                                                        
000200*                                 MID-COPYTEXT FÖR W3020400               
000300     03 MID-IDFSGURV-IN      PIC X(8).                                    
000400*                                 URVALS IDENTITET                        
000500     03 MID-IDUSER-IN        PIC X(8).                                    
000600*                                 ANVÄNDARENS SÄKERHETS ID                
000700     03 MID-IDFSGURV-UT      PIC X(8).                                    
000800*                                 URVALS IDENTITET                        
000900     03 MID-IDUSER-UT        PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100     03 MID-KVART            PIC 9(7).                                    
001200*                                 ANTAL ARTNR PER BRYTBEGREPP             
001300     03 MID-IDPTYP           PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 MID-KDPRODSL         OCCURS 4 TIMES                               
001600                             PIC 9(2).                                    
001700*                                 PRODUKTSLAG                             
001800     03 MID-IDKONCNR         OCCURS 8 TIMES                               
001900                             PIC 9(3).                                    
002000*                                 KONCERNNUMMER                           
002100     03 MID-IDLEVNR          OCCURS 8 TIMES                               
002200                             PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400     03 MID-IDFKNGRP-FOM     PIC 9(4).                                    
002500*                                 FUNKTIONSGRUPP                          
002600     03 MID-IDFKNGRP-TOM     PIC 9(4).                                    
002700*                                 FUNKTIONSGRUPP                          
002800     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
002900*                                 RADINFORMATION                          
003000        05 MID-KDMARK-FOM    PIC 9(3).                                    
003100*                                 MARKNADSKOD                             
003200        05 MID-KDMARK-TOM    PIC 9(3).                                    
003300*                                 MARKNADSKOD                             
003400     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
003500*                                 RADINFORMATION                          
003600        05 MID-IDDISTR-FOM   PIC 9(4).                                    
003700*                                 DISTRIKTNUMMER                          
003800        05 MID-IDDISTR-TOM   PIC 9(4).                                    
003900*                                 DISTRIKTNUMMER                          
004000     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
004100*                                 RADINFORMATION                          
004200        05 MID-IDANSK-FOM    PIC 9(3).                                    
004300*                                 ANSKAFFARNUMMER                         
004400        05 MID-IDANSK-TOM    PIC 9(3).                                    
004500*                                 ANSKAFFARNUMMER                         
004600*** END OF VILMAII-COPY LENGTH= 202 BYTES                                 
