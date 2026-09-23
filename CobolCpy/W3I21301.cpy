000100 01  MID-W3I21301.                                                        
000200*                                 COPYTEXT FÖR MID W3I21301               
000300*                                                                         
000400     03 MID-MOD-BYTE         PIC X.                                       
000500     03 MID-INRAD-IDSKURVA   PIC X(2).                                    
000600*                                 SÄSONGSKURVA                            
000700     03 MID-INRAD            OCCURS 12 TIMES.                             
000800        05 MID-INRAD-REFSGSIX                                             
000900                             PIC X(4).                                    
001000*                                 SÄSONGSINDEX FÖRSÄLJNING (%)            
001100*** END COPY W3I21301C0  LENGTH=51                                        
