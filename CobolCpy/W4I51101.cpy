000100 01  MID-W4I51101.                                                        
000200*                                 COPYTEXT FÖR MID W4I51101               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-FLSORT           PIC X.                                       
001000*                                 SORTERINGSFLAGGA (J/N)                  
001100     03 MID-FLSOFT           PIC X.                                       
001200*                                 FLAGGA SOFTVARA                         
001300     03 MID-FLPROF           PIC X.                                       
001400*                                 PROFORMA-MÄRKNING                       
001500     03 MID-INPUT            OCCURS 14 TIMES.                             
001600*                                 INMATNINGSFÄLT                          
001700        05 MID-IDTRANS       PIC X(4).                                    
001800*                                 BILDNUMMER                              
001900        05 MID-IDKUNDNR      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100        05 MID-IDORDNR7      PIC X(7).                                    
002200*                                 ORDERNUMMER                             
002300        05 MID-IDPRODNR      PIC X(7).                                    
002400*                                 PRODUKTIONSNUMMER                       
002500*** END OF VILMAII-COPY LENGTH= 351 BYTES                                 
