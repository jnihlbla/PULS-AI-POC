000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***  - INTERNA DISTRIKT                      
000400*                            ***                                          
000500*                            *************************************        
000600                                                                          
000700 01  DIST47-IDDISTR          PIC 9(5)     COMP-3.                         
000800*                                                                         
000900     88  DIST47-INTERNA      VALUE 0001 THRU 0099.                        
001000*                                                                         
001001     88  DIST47-INTERN-PV    VALUE 54.                                    
001002*                                                                         
001010     88  DIST47-INTERNA-DEL  VALUE 0070 0072 0074                         
001020                                   0078 0079                              
001030                                   0082 0087                              
001040                                   0096 0097.                             
001100*** END COPY WWDIST47    LENGTH=3                                         
