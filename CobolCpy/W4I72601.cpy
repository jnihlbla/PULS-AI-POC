000100 01  MID-W4I72601.                                                        
000200*                                 MID-COPYTEXT FÖR W40726                 
000300     03 MID-IDKNOTNR-IN      PIC X(7).                                    
000400*                                 KREDITNOTANUMMER                        
000500     03 MID-IDKNOTNR-UT      PIC X(7).                                    
000600*                                 KREDITNOTANUMMER                        
000700     03 MID-INPUT.                                                        
000800        05 MID-KDCMD         OCCURS 4 TIMES                               
000900                             PIC X(4).                                    
001000     03 MID-RAD-INFO         OCCURS 4 TIMES.                              
001100        05 MID-IDDISTR       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300        05 MID-IDKUNDNR      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500        05 MID-IDRAPPNR      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700*** END OF VILMAII-COPY LENGTH= 98 BYTES                                  
