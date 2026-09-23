000100 01  MID-W4I57501.                                                        
000200*                                 COPYTEXT FÖR MID W4I57501               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-TIORDREG-IN      PIC X(6).                                    
001200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001300     03 MID-TIORDREG-UT      PIC X(6).                                    
001400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001500     03 MID-KDORDKL-IN       PIC X.                                       
001600*                                 ORDERKLASS                              
001700     03 MID-KDORDKL-UT       PIC X.                                       
001800*                                 ORDERKLASS                              
001900     03 MID-INPUT            OCCURS 14 TIMES.                             
002000*                                 INMATNINGSFÄLT                          
002100        05 MID-BILDNR        PIC X(4).                                    
002200*                                 BILDNUMMER                              
002300        05 MID-IDKUNDNR      PIC X(6).                                    
002400*                                 KUNDNUMMER                              
002500        05 MID-IDORDNR       PIC X(5).                                    
002600*                                 ORDERNUMMER                             
002700        05 MID-IDARTNR       PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900*** END OF VILMAII-COPY LENGTH= 370 BYTES                                 
