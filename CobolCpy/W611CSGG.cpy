000010*** EDIT ALLOWED                                                          
000100 01  AEDIRCSG.                                                            
000200*                                 CSG SEGMENTET I AVIEXP                  
000300*                                                                         
000400*                                                                         
000500     03 IDRT-CSG             PIC X(3).                                    
000510     03 FILLER               PIC X(3).                                    
000600     03 CSG-000-GRP.                                                      
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000        05 CSG-3296          PIC X(5).                                    
001100        05 CSG-3921          PIC X(3).                                    
001200        05 FILLER            OCCURS 66 TIMES                              
001300                             PIC X.                                       
001400*** END COPY AEDIRCSGC0  LENGTH=80                                        
