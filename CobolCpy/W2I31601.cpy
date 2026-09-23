000100 01  MID-W2I31601.                                                        
000200*                                 MID-COPYTEXT FÖR W2031600               
000300     03 MID-IDKAMP-IN        PIC X(7).                                    
000400*                                 SERVICEKAMPANJ                          
000500     03 MID-IDKAMP-UT        PIC X(7).                                    
000600*                                 SERVICEKAMPANJ                          
000700     03 MID-IDKAMP-GRP-IN    PIC X(7).                                    
000800*                                 ID FÖR KAMPANJGRUPPER                   
000900     03 MID-IDKAMP-GRP-UT    PIC X(7).                                    
001000*                                 ID FÖR KAMPANJGRUPPER                   
001100     03 MID-INPUT.                                                        
001200        05 MID-GRP           OCCURS 13 TIMES.                             
001300           07 MID-CMD        PIC X.                                       
001400           07 MID-IDKAMP     PIC X(7).                                    
001500*                                 SERVICEKAMPANJ                          
001600        05 MID-IDKAMP-NY     PIC X(7).                                    
001700*                                 SERVICEKAMPANJ                          
001800*** END OF VILMAII-COPY LENGTH= 139 BYTES                                 
