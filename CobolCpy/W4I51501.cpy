000100 01  MID-W4I51501.                                                        
000200*                                 COPYTEXT FÖR MID W4I51501               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MID-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MID-KDORDKL-IN       PIC X.                                       
001600*                                 ORDERKLASS                              
001700     03 MID-KDORDKL-UT       PIC X.                                       
001800*                                 ORDERKLASS                              
001900     03 MID-KDFRAKT-IN       PIC X(2).                                    
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 MID-KDFRAKT-UT       PIC X(2).                                    
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300     03 MID-KDORDSTA-IN      PIC X.                                       
002400*                                 VOLVOORDERSTATUS                        
002500     03 MID-KDORDSTA-UT      PIC X.                                       
002600*                                 VOLVOORDERSTATUS                        
270000     03 MID-INPUT            OCCURS 14 TIMES.                             
280000*                                 INMATNINGSFÄLT                          
290000        05 MID-IDTRANS       PIC X(4).                                    
300000*                                 BILDNUMMER                              
310000        05 MID-IDKUNDNR      PIC X(6).                                    
320000*                                 KUNDNUMMER                              
330000        05 MID-IDORDNR7      PIC X(7).                                    
340000*                                 ORDERNUMMER                             
003500*** END OF VILMAII-COPY LENGTH= 270 BYTES                                 
