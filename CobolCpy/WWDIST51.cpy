000010*** EDIT ALLOWED                                                          
000020*                            *************************************        
000030*                            *** ANVÄNDS VID TEST AV:                     
000040*                            ***                                          
000050*                            ***  CASH ON DELIVERY                        
000060*                            *************************************        
000070                                                                          
000080 01  DIST51-IDDISTR          PIC 9(5)     COMP-3.                         
000090*                                                                         
000100       88  DIST51-COD               VALUE  1378 1478                      
000110                                           1778                           
000120                                           1822 1871                      
000200                                           2178 2364 2365                 
000300                                           2370 2371 2372                 
000400                                           2373 2374 2375 2376            
000410                                           2377 2697 2878                 
000411                                           3162                           
000412                                           5222 5627 5811                 
000413                                           6010 6124 6203 6225            
000414                                           6271 6281 6591 7051            
000415                                           7574 7575 7674 7838            
000416                                           8741 8742 8743 8751.           
000417*                                                                         
000418       88  DIST51-GB-COD            VALUE  1378.                          
000419*                                                                         
000420       88  DIST51-FRANCE-COD        VALUE  1478.                          
000421*                                                                         
000422       88  DIST51-IRLAND-COD        VALUE  1778.                          
000423*                                                                         
000424       88  DIST51-ITALY-COD         VALUE  1822 1871.                     
000425*                                                                         
000450       88  DIST51-ES-COD            VALUE  2178.                          
000460*                                                                         
000470       88  DIST51-AT-COD            VALUE  2370 THRU 2377.                
000480*                                                                         
000490       88  DIST51-PL-COD            VALUE  2878.                          
000500*                                                                         
000600       88  DIST51-US-COD            VALUE  7574 7575 8751.                
000700*                                                                         
000800       88  DIST51-CA-COD            VALUE  7674 8741 8742 8743.           
000900*                                                                         
000910       88  DIST51-AU-COD            VALUE  7838.                          
000920*                                                                         
000930       88  DIST51-JP-COD            VALUE  5222.                          
000940*                                                                         
000950       88  DIST51-CN-COD            VALUE  6271 6281.                     
000960*                                                                         
000961       88  DIST51-IN-COD            VALUE  6010.                          
000962*                                                                         
000963       88  DIST51-HU-COD            VALUE  2364.                          
000964*                                                                         
000965       88  DIST51-CZ-COD            VALUE  2365.                          
000966*                                                                         
000967       88  DIST51-TH-COD            VALUE  6225.                          
000968*                                                                         
000969       88  DIST51-TW-COD            VALUE  6203.                          
000970*                                                                         
000971       88  DIST51-KR-COD            VALUE  6124.                          
000972*                                                                         
000973       88  DIST51-MY-COD            VALUE  5627.                          
000974*                                                                         
000975       88  DIST51-RU-COD            VALUE  2697.                          
000976*                                                                         
000977       88  DIST51-TR-COD            VALUE  5811.                          
000978*                                                                         
000979       88  DIST51-BR-COD            VALUE  7051.                          
000980*                                                                         
000981       88  DIST51-MX-COD            VALUE  6591.                          
000982*                                                                         
000983       88  DIST51-ZA-COD            VALUE  3162.                          
000984*                                                                         
000990*** END COPY WWDIST51    LENGTH=3                                         
