000100 01  MID-R02-W0IR0201.                                                    
000200*                                 COPYTEXT FÖR MID W0IR0201               
000300*                                                                         
000400     03 MID-R02-IDPTYP       PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 MID-R02-IDLEVNR      PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 MID-R02-PG-TABELL    OCCURS 8 TIMES.                              
000900        05 MID-R02-IDPLANGR-AG                                            
001000                             PIC X.                                       
001100*                                 PLANERINGSGRUPP ANSKAFFARE              
001200        05 MID-R02-IDANSK    PIC X(3).                                    
001300*                                 ANSKAFFARNUMMER                         
001400*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
