000010*** EDIT ALLOWED                                                          
000100******************************************************************        
000200*****        TABELL MED PROCENTFORDELNING PER LO             *****        
000300******************************************************************        
000400                                                                          
000500    01  W413FORD.                                                         
000600                                                                          
000700       03  VARDEN.                                                        
000800                                                                          
000900          05  FILLER      PIC X(13)      VALUE                            
001000                                    '1000100100265'.                      
001100          05  FILLER      PIC X(13)      VALUE                            
001200                                    '2000160100100'.                      
001300          05  FILLER      PIC X(13)      VALUE                            
001400                                    '3000172172212'.                      
001500          05  FILLER      PIC X(13)      VALUE                            
001600                                    '3500172172212'.                      
001700                                                                          
001800       03  FILLER REDEFINES VARDEN.                                       
001900                                                                          
002000          05  FORD-TABELLRAD  OCCURS  4  INDEXED BY FORD-INDX.            
002100                                                                          
002200            07  FORD-ADLAGOMR    PIC 9(2).                                
002300            07  FORD-DATA        PIC X(11).                               
