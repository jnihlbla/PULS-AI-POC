000010*** EDIT ALLOWED                                                          
000100 01  AEDIRTCO.                                                            
000200*                                 TCO SEGMENTET I AVIEXP                  
000300*                                                                         
000400*                                                                         
000500     03 IDRT-TCO             PIC X(3).                                    
000510     03 FILLER               PIC X(3).                                    
000600     03 TCO-000-GRP.                                                      
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000        05 TCO-1906          PIC X(8).                                    
001100        05 FILLER            PIC X.                                       
001110        05 TCO-7224          PIC 9(6).                                    
001120        05 FILLER            PIC X.                                       
001200        05 TCO-6853          PIC 9(10).                                   
001300        05 FILLER            OCCURS 48 TIMES                              
001400                             PIC X.                                       
001500*** END COPY AEDIRTCOC0  LENGTH=80                                        
