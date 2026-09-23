000100 01  MID-W0I51501.                                                        
000200*                                 MID-COPYTEXT FÖR W00515                 
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-INPUT.                                                        
000800        05 MID-IDTRANS-HOPP  OCCURS 12 TIMES                              
000900                             PIC X(4).                                    
001000*                                 BILDNUMMER                              
001100     03 MID-RAD-INFO         OCCURS 12 TIMES.                             
001200        05 MID-IDARTNR       PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400*** END OF VILMAII-COPY LENGTH= 167 BYTES                                 
