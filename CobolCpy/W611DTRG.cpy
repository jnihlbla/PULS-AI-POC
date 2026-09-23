000010*** EDIT ALLOWED                                                          
000100 01  AEDIRDTR.                                                            
000200*                                 DTR SEGMENTET I AVIEXP                  
000300*                                                                         
000400*                                                                         
000500     03 IDRT-DTR             PIC X(3).                                    
000510     03 FILLER               PIC X(3).                                    
000600     03 DTR-000-GRP.                                                      
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000        05 DTR-8212          PIC X(17).                                   
001100        05 DTR-8164          PIC X(17).                                   
001200        05 DTR-8028          PIC X(9).                                    
001210        05 DTR-3296          PIC X(17).                                   
001300        05 FILLER            OCCURS 14 TIMES                              
001400                             PIC X.                                       
001500     03 DTR-001-GRP REDEFINES DTR-000-GRP.                                
001600*                                                                         
001700*                                                                         
001800*                                                                         
001900        05 DTR-3126-NAME  PIC X(35).                                      
002000        05 DTR-1188       PIC X(17).                                      
002100        05 FILLER            PIC X(22).                                   
002200*** END COPY AEDIRDTRC0  LENGTH=80                                        
