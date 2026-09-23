000010*** EDIT ALLOWED                                                          
000100 01  W4753101.                                                            
000200*                                                                         
000300*   LAYOUT OF ORIGIN TABLE                                                
000400*                                                                         
000500     03  IDPTYP          PIC X(6).                                        
000600*                TRANSACTION TYPE                                         
000700     03  D               PIC X(1).                                        
000800*                VALUE = D = ORIGIN CARD                                  
000900     03  SWORIG          PIC 9(2).                                        
001000*                SWEDISH ORIGIN                                           
001100     03  FILLER          PIC X(9).                                        
001200*                                                                         
001300     03  FRORIG          PIC 9(3).                                        
001400*                FRENCH  ORIGIN                                           
001500     03  FILLER          PIC X(59).                                       
001600*                                                                         
001700*** END COPY W4753101    LENGTH=80                                        
