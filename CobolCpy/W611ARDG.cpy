000010*** EDIT ALLOWED                                                          
000100 01  AEDIRARD.                                                            
000200*                                 ARD SEGMENTET I AVIEXP                  
000300*                                                                         
000400*                                                                         
000500     03 IDRT-ARD             PIC X(3).                                    
000510     03 FILLER               PIC X(3).                                    
000600     03 ARD-000-GRP.                                                      
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000        05 ARD-7304          PIC X(8).                                    
001010        05 FILLER            PIC X(27).                                   
001020        05 FILLER            PIC X.                                       
001100        05 ARD-6270          PIC 9(10).                                   
001200        05 ARD-6410          PIC X(3).                                    
001300        05 ARD-1022          PIC X(12).                                   
001400        05 ARD-3239          PIC X(2).                                    
001500        05 FILLER            OCCURS 36 TIMES                              
001600                             PIC X.                                       
001700*** END COPY AEDIRARDC0  LENGTH=80                                        
