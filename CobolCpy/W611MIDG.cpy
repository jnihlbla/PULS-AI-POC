000010*** EDIT ALLOWED                                                          
000100 01  AEDIRMID.                                                            
000200*                                 MID SEGMENTET I AVIEXP                  
000300*                                                                         
000400*                                                                         
000500     03 IDRT-MID             PIC X(3).                                    
000510     03 FILLER               PIC X(3).                                    
000600     03 MID-000-GRP.                                                      
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000        05 MID-1004          PIC X(8).                                    
001100        05 MID-2007          PIC X(6).                                    
001200        05 MID-2002          PIC X(4).                                    
001300        05 FILLER            OCCURS 56 TIMES                              
001400                             PIC X.                                       
001500*** END COPY AEDIRMIDC0  LENGTH=80                                        
