000100 01  MID-W4I35501.                                                        
000200*                                 MID-COPYTEXT PGM W40355                 
000300*                                 ANNULLATION AV UTSKRIVEN                
000400*                                 MJUKVARUORDER                           
000500*                                                                         
000600     03 MID-IDPTYP           PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 MID-IDDISTR          PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MID-IDKUNDNR         PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MID-IDORDNR7         PIC X(7).                                    
001300*                                 ORDERNUMMER                             
001400     03 MID-IDPRODNR         PIC X(7).                                    
001500*                                 PRODUKTIONSNUMMER                       
001600     03 MID-RAD              OCCURS 10 TIMES.                             
001700        05 MID-IDARTPRE      PIC X(3).                                    
001800*                                 IDENTIFIERARE ARTIKELSORTIMENT          
001900        05 MID-IDARTBET      PIC X(17).                                   
002000*                                 ARTIKELBETECKNING EFTERMARKNAD          
002100        05 MID-IDRADNR       PIC X(4).                                    
002200*                                 RADNUMMER                               
002300        05 MID-KDORDBEK      PIC X(2).                                    
002400*                                 ORDERBEKRÄFTELSEKOD                     
002500*** END OF VILMAII-COPY LENGTH= 287 BYTES                                 
