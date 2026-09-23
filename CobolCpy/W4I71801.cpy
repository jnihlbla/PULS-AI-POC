000100 01  MID-W4I71801.                                                        
000200*                                 MID-COPYTEXT FÖR W40718                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDRAPPNR-IN      PIC X(7).                                    
000800*                                 RAPPORT NUMMER                          
000900     03 MID-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-KDKRENOT-IN      PIC X(2).                                    
001200*                                 TYP AV KREDITERING                      
001300     03 MID-INPUT.                                                        
001400*                                 INMATNINGSFÄLT                          
001500        05 MID-TEMEMO        OCCURS 5 TIMES                               
001600                             PIC X(66).                                   
001700*                                 TEXTRAD MAIL                            
001800*** END OF VILMAII-COPY LENGTH= 351 BYTES                                 
