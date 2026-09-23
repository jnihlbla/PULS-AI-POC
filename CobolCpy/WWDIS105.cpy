000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            *** -  SPECIAL DISTRICTS NA                  
000600*                            *************************************        
000700*                                                                         
000800 02  DIS105-IDDISTR          PIC 9(5)     COMP-3.                         
000900*                                                                         
001000     88  DIS105-NA-SPEC               VALUE  8460 8461 8462 8463          
001100                                             8470 8471                    
001100                                             8481 8482 8483               
001100                                             8600 THRU 8611               
001100                                             8615 THRU 8616               
001100                                             8618 THRU 8620.              
001100* KITTING                                                                 
001100     88  DIS105-NA-KITT               VALUE  8460.                        
001100* APM                                                                     
001100     88  DIS105-NA-APM                VALUE  8461.                        
001100* SERVICE OPERATIONS                                                      
001100     88  DIS105-NA-SERV               VALUE  8462.                        
001100* BLANK KEYS                                                              
001100     88  DIS105-NA-BLANK              VALUE  8463.                        
001100* DRILL BITS                                                              
001100     88  DIS105-NA-DRILL              VALUE  8470.                        
001100* PACKING MATERIAL                                                        
001100     88  DIS105-NA-PACK               VALUE  8471.                        
001100* MIXED STOCK                                                             
001100     88  DIS105-NA-MIX                VALUE  8483.                        
001100* AMER.BYTES                                                              
001100     88  DIS105-NA-BYT                VALUE  8600 THRU 8611               
001100                                             8615 THRU 8616               
001100                                             8618 THRU 8620.              
001500*                                                                         
001600*** END COPY WWDIS105    LENGTH=0                                         
