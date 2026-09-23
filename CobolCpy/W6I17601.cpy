000100 01  MID-W6I17601.                                                        
000200*                                 MID-COPYTEXT FÖR W60176                 
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-TISUPREF-IN      PIC X(6).                                    
000600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000700     03 MID-IDSUPREF-IN      PIC X(10).                                   
000800*                                 LEVERANTöRSREF.                         
000900     03 MID-IDDISTR-IN       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-INPUT            OCCURS 26 TIMES.                             
001200*                                 MID-COPYTEXT FÖR W60176                 
001300        05 MID-IDDISTR-UPD   PIC 9(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500        05 MID-IDKUNDNR-UPD  PIC 9(6).                                    
001600*                                 KUNDNUMMER                              
001700        05 MID-IDORDNR7-UPD  PIC 9(7).                                    
001800*                                 ORDERNUMMER                             
001900        05 MID-IDKOLLI-UPD   PIC 9(5).                                    
002000*                                 KOLLINUMMER                             
002100*** END OF VILMAII-COPY LENGTH= 597 BYTES                                 
