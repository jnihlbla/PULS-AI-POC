000010*** EDIT ALLOWED                                                          
000100 01  AEDIRDET.                                                            
000200*                                 DET SEGMENTET I AVIEXP                  
000300*                                                                         
000400*                                                                         
000500     03 IDRT-DET             PIC X(3).                                    
000510     03 FILLER               PIC X(3).                                    
000600     03 DET-000-GRP.                                                      
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000        05 DET-8067          PIC 9(2).                                    
001100        05 FILLER            OCCURS 72 TIMES                              
001200                             PIC X.                                       
001300*** END COPY AEDIRDETC0  LENGTH=80                                        
