000100*- EDIT ALLOWED                                                           
000110*- TABLE TO GET PROCUREMENT SEGMENT DESCRIPTIONS -*                       
000200 01  PSEGM-VALUES.                                                        
000201     03 FILLER PIC X(45) VALUE                                            
000202             '1000 KEY BUSINESS SMALL PHASE IN NORMAL'.                   
000203     03 FILLER PIC X(45) VALUE                                            
000204             '1001 KEY BUSINESS SMALL PHASE IN EXPIRE 1Y'.                
000205     03 FILLER PIC X(45) VALUE                                            
000206             '1003 KEY BUSINESS SMALL PHASE IN EXPIRE 3Y'.                
000207     03 FILLER PIC X(45) VALUE                                            
000208             '1004 KEY BUSINESS SMALL PHASE IN DG 4'.                     
000209     03 FILLER PIC X(45) VALUE                                            
000210             '1006 KEY BUSINESS SMALL PHASE IN DG 6'.                     
000211     03 FILLER PIC X(45) VALUE                                            
000212             '1010 KEY BUSINESS SMALL PRIME NORMAL'.                      
000213     03 FILLER PIC X(45) VALUE                                            
000214             '1011 KEY BUSINESS SMALL PRIME EXPIRE 1Y'.                   
000215     03 FILLER PIC X(45) VALUE                                            
000216             '1013 KEY BUSINESS SMALL PRIME EXPIRE 3Y'.                   
000217     03 FILLER PIC X(45) VALUE                                            
000218             '1014 KEY BUSINESS SMALL PRIME DG 4'.                        
000219     03 FILLER PIC X(45) VALUE                                            
000220             '1016 KEY BUSINESS SMALL PRIME DG 6'.                        
000221     03 FILLER PIC X(45) VALUE                                            
000222             '1020 KEY BUSINESS SMALL DECLINE NORMAL'.                    
000223     03 FILLER PIC X(45) VALUE                                            
000224             '1021 KEY BUSINESS SMALL DECLINE EXPIRE 1Y'.                 
000225     03 FILLER PIC X(45) VALUE                                            
000226             '1023 KEY BUSINESS SMALL DECLINE EXPIRE 3Y'.                 
000227     03 FILLER PIC X(45) VALUE                                            
000228             '1024 KEY BUSINESS SMALL DECLINE DG 4'.                      
000229     03 FILLER PIC X(45) VALUE                                            
000230             '1026 KEY BUSINESS SMALL DECLINE DG 6'.                      
000231     03 FILLER PIC X(45) VALUE                                            
000232             '1100 KEY BUSINESS MEDIUM PHASE IN NORMAL'.                  
000233     03 FILLER PIC X(45) VALUE                                            
000234             '1101 KEY BUSINESS MEDIUM PHASE IN EXPIRE 1Y'.               
000235     03 FILLER PIC X(45) VALUE                                            
000236             '1103 KEY BUSINESS MEDIUM PHASE IN EXPIRE 3Y'.               
000237     03 FILLER PIC X(45) VALUE                                            
000238             '1104 KEY BUSINESS MEDIUM PHASE IN DG 4'.                    
000239     03 FILLER PIC X(45) VALUE                                            
000240             '1106 KEY BUSINESS MEDIUM PHASE IN DG 6'.                    
000241     03 FILLER PIC X(45) VALUE                                            
000242             '1110 KEY BUSINESS MEDIUM PRIME NORMAL'.                     
000243     03 FILLER PIC X(45) VALUE                                            
000244             '1111 KEY BUSINESS MEDIUM PRIME EXPIRE 1Y'.                  
000245     03 FILLER PIC X(45) VALUE                                            
000246             '1113 KEY BUSINESS MEDIUM PRIME EXPIRE 3Y'.                  
000247     03 FILLER PIC X(45) VALUE                                            
000248             '1114 KEY BUSINESS MEDIUM PRIME DG 4'.                       
000249     03 FILLER PIC X(45) VALUE                                            
000250             '1116 KEY BUSINESS MEDIUM PRIME DG 6'.                       
000251     03 FILLER PIC X(45) VALUE                                            
000252             '1120 KEY BUSINESS MEDIUM DECLINE NORMAL'.                   
000253     03 FILLER PIC X(45) VALUE                                            
000254             '1121 KEY BUSINESS MEDIUM DECLINE EXPIRE 1Y'.                
000255     03 FILLER PIC X(45) VALUE                                            
000256             '1123 KEY BUSINESS MEDIUM DECLINE EXPIRE 3Y'.                
000257     03 FILLER PIC X(45) VALUE                                            
000258             '1124 KEY BUSINESS MEDIUM DECLINE DG 4'.                     
000259     03 FILLER PIC X(45) VALUE                                            
000260             '1126 KEY BUSINESS MEDIUM DECLINE DG 6'.                     
000261     03 FILLER PIC X(45) VALUE                                            
000262             '1200 KEY BUSINESS LARGE PHASE IN NORMAL'.                   
000263     03 FILLER PIC X(45) VALUE                                            
000264             '1201 KEY BUSINESS LARGE PHASE IN EXPIRE 1Y'.                
000265     03 FILLER PIC X(45) VALUE                                            
000266             '1203 KEY BUSINESS LARGE PHASE IN EXPIRE 3Y'.                
000267     03 FILLER PIC X(45) VALUE                                            
000268             '1204 KEY BUSINESS LARGE PHASE IN DG 4'.                     
000269     03 FILLER PIC X(45) VALUE                                            
000270             '1206 KEY BUSINESS LARGE PHASE IN DG 6'.                     
000271     03 FILLER PIC X(45) VALUE                                            
000272             '1210 KEY BUSINESS LARGE PRIME NORMAL'.                      
000273     03 FILLER PIC X(45) VALUE                                            
000274             '1211 KEY BUSINESS LARGE PRIME EXPIRE 1Y'.                   
000275     03 FILLER PIC X(45) VALUE                                            
000276             '1213 KEY BUSINESS LARGE PRIME EXPIRE 3Y'.                   
000277     03 FILLER PIC X(45) VALUE                                            
000278             '1214 KEY BUSINESS LARGE PRIME DG 4'.                        
000279     03 FILLER PIC X(45) VALUE                                            
000280             '1216 KEY BUSINESS LARGE PRIME DG 6'.                        
000281     03 FILLER PIC X(45) VALUE                                            
000282             '1220 KEY BUSINESS LARGE DECLINE NORMAL'.                    
000283     03 FILLER PIC X(45) VALUE                                            
000284             '1221 KEY BUSINESS LARGE DECLINE EXPIRE 1Y'.                 
000285     03 FILLER PIC X(45) VALUE                                            
000286             '1223 KEY BUSINESS LARGE DECLINE EXPIRE 3Y'.                 
000287     03 FILLER PIC X(45) VALUE                                            
000288             '1224 KEY BUSINESS LARGE DECLINE DG 4'.                      
000289     03 FILLER PIC X(45) VALUE                                            
000290             '1226 KEY BUSINESS LARGE DECLINE DG 6'.                      
000291     03 FILLER PIC X(45) VALUE                                            
000292             '2000 FUNCTION CRIT. SMALL PHASE IN NORMAL'.                 
000293     03 FILLER PIC X(45) VALUE                                            
000294             '2001 FUNCTION CRIT. SMALL PHASE IN EXPIRE 1Y'.              
000295     03 FILLER PIC X(45) VALUE                                            
000296             '2003 FUNCTION CRIT. SMALL PHASE IN EXPIRE 3Y'.              
000297     03 FILLER PIC X(45) VALUE                                            
000298             '2004 FUNCTION CRIT. SMALL PHASE IN DG 4'.                   
000299     03 FILLER PIC X(45) VALUE                                            
000300             '2006 FUNCTION CRIT. SMALL PHASE IN DG 6'.                   
000301     03 FILLER PIC X(45) VALUE                                            
000302             '2010 FUNCTION CRIT. SMALL PRIME NORMAL'.                    
000303     03 FILLER PIC X(45) VALUE                                            
000304             '2011 FUNCTION CRIT. SMALL PRIME EXPIRE 1Y'.                 
000305     03 FILLER PIC X(45) VALUE                                            
000306             '2013 FUNCTION CRIT. SMALL PRIME EXPIRE 3Y'.                 
000307     03 FILLER PIC X(45) VALUE                                            
000308             '2014 FUNCTION CRIT. SMALL PRIME DG 4'.                      
000309     03 FILLER PIC X(45) VALUE                                            
000310             '2016 FUNCTION CRIT. SMALL PRIME DG 6'.                      
000311     03 FILLER PIC X(45) VALUE                                            
000312             '2020 FUNCTION CRIT. SMALL DECLINE NORMAL'.                  
000313     03 FILLER PIC X(45) VALUE                                            
000314             '2021 FUNCTION CRIT. SMALL DECLINE EXPIRE 1Y'.               
000315     03 FILLER PIC X(45) VALUE                                            
000316             '2023 FUNCTION CRIT. SMALL DECLINE EXPIRE 3Y'.               
000317     03 FILLER PIC X(45) VALUE                                            
000318             '2024 FUNCTION CRIT. SMALL DECLINE DG 4'.                    
000319     03 FILLER PIC X(45) VALUE                                            
000320             '2026 FUNCTION CRIT. SMALL DECLINE DG 6'.                    
000321     03 FILLER PIC X(45) VALUE                                            
000322             '2100 FUNCTION CRIT. MEDIUM PHASE IN NORMAL'.                
000323     03 FILLER PIC X(45) VALUE                                            
000324             '2101 FUNCTION CRIT. MEDIUM PHASE IN EXPIRE 1Y'.             
000325     03 FILLER PIC X(45) VALUE                                            
000326             '2103 FUNCTION CRIT. MEDIUM PHASE IN EXPIRE 3Y'.             
000327     03 FILLER PIC X(45) VALUE                                            
000328             '2104 FUNCTION CRIT. MEDIUM PHASE IN DG 4'.                  
000329     03 FILLER PIC X(45) VALUE                                            
000330             '2106 FUNCTION CRIT. MEDIUM PHASE IN DG 6'.                  
000331     03 FILLER PIC X(45) VALUE                                            
000332             '2110 FUNCTION CRIT. MEDIUM PRIME NORMAL'.                   
000333     03 FILLER PIC X(45) VALUE                                            
000334             '2111 FUNCTION CRIT. MEDIUM PRIME EXPIRE 1Y'.                
000335     03 FILLER PIC X(45) VALUE                                            
000336             '2113 FUNCTION CRIT. MEDIUM PRIME EXPIRE 3Y'.                
000337     03 FILLER PIC X(45) VALUE                                            
000338             '2114 FUNCTION CRIT. MEDIUM PRIME DG 4'.                     
000339     03 FILLER PIC X(45) VALUE                                            
000340             '2116 FUNCTION CRIT. MEDIUM PRIME DG 6'.                     
000341     03 FILLER PIC X(45) VALUE                                            
000342             '2120 FUNCTION CRIT. MEDIUM DECLINE NORMAL'.                 
000343     03 FILLER PIC X(45) VALUE                                            
000344             '2121 FUNCTION CRIT. MEDIUM DECLINE EXPIRE 1Y'.              
000345     03 FILLER PIC X(45) VALUE                                            
000346             '2123 FUNCTION CRIT. MEDIUM DECLINE EXPIRE 3Y'.              
000347     03 FILLER PIC X(45) VALUE                                            
000348             '2124 FUNCTION CRIT. MEDIUM DECLINE DG 4'.                   
000349     03 FILLER PIC X(45) VALUE                                            
000350             '2126 FUNCTION CRIT. MEDIUM DECLINE DG 6'.                   
000351     03 FILLER PIC X(45) VALUE                                            
000352             '2200 FUNCTION CRIT. LARGE PHASE IN NORMAL'.                 
000353     03 FILLER PIC X(45) VALUE                                            
000354             '2201 FUNCTION CRIT. LARGE PHASE IN EXPIRE 1Y'.              
000355     03 FILLER PIC X(45) VALUE                                            
000356             '2203 FUNCTION CRIT. LARGE PHASE IN EXPIRE 3Y'.              
000357     03 FILLER PIC X(45) VALUE                                            
000358             '2204 FUNCTION CRIT. LARGE PHASE IN DG 4'.                   
000359     03 FILLER PIC X(45) VALUE                                            
000360             '2206 FUNCTION CRIT. LARGE PHASE IN DG 6'.                   
000361     03 FILLER PIC X(45) VALUE                                            
000362             '2210 FUNCTION CRIT. LARGE PRIME NORMAL'.                    
000363     03 FILLER PIC X(45) VALUE                                            
000364             '2211 FUNCTION CRIT. LARGE PRIME EXPIRE 1Y'.                 
000365     03 FILLER PIC X(45) VALUE                                            
000366             '2213 FUNCTION CRIT. LARGE PRIME EXPIRE 3Y'.                 
000367     03 FILLER PIC X(45) VALUE                                            
000368             '2214 FUNCTION CRIT. LARGE PRIME DG 4'.                      
000369     03 FILLER PIC X(45) VALUE                                            
000370             '2216 FUNCTION CRIT. LARGE PRIME DG 6'.                      
000371     03 FILLER PIC X(45) VALUE                                            
000372             '2220 FUNCTION CRIT. LARGE DECLINE NORMAL'.                  
000373     03 FILLER PIC X(45) VALUE                                            
000374             '2221 FUNCTION CRIT. LARGE DECLINE EXPIRE 1Y'.               
000375     03 FILLER PIC X(45) VALUE                                            
000376             '2223 FUNCTION CRIT. LARGE DECLINE EXPIRE 3Y'.               
000377     03 FILLER PIC X(45) VALUE                                            
000378             '2224 FUNCTION CRIT. LARGE DECLINE DG 4'.                    
000379     03 FILLER PIC X(45) VALUE                                            
000380             '2226 FUNCTION CRIT. LARGE DECLINE DG 6'.                    
000381     03 FILLER PIC X(45) VALUE                                            
000382             '3000 NORMAL SMALL PHASE IN NORMAL'.                         
000383     03 FILLER PIC X(45) VALUE                                            
000384             '3001 NORMAL SMALL PHASE IN EXPIRE 1Y'.                      
000385     03 FILLER PIC X(45) VALUE                                            
000386             '3003 NORMAL SMALL PHASE IN EXPIRE 3Y'.                      
000387     03 FILLER PIC X(45) VALUE                                            
000388             '3004 NORMAL SMALL PHASE IN DG 4'.                           
000389     03 FILLER PIC X(45) VALUE                                            
000390             '3006 NORMAL SMALL PHASE IN DG 6'.                           
000391     03 FILLER PIC X(45) VALUE                                            
000392             '3010 NORMAL SMALL PRIME NORMAL'.                            
000393     03 FILLER PIC X(45) VALUE                                            
000394             '3011 NORMAL SMALL PRIME EXPIRE 1Y'.                         
000395     03 FILLER PIC X(45) VALUE                                            
000396             '3013 NORMAL SMALL PRIME EXPIRE 3Y'.                         
000397     03 FILLER PIC X(45) VALUE                                            
000398             '3014 NORMAL SMALL PRIME DG 4'.                              
000399     03 FILLER PIC X(45) VALUE                                            
000400             '3016 NORMAL SMALL PRIME DG 6'.                              
000401     03 FILLER PIC X(45) VALUE                                            
000402             '3020 NORMAL SMALL DECLINE NORMAL'.                          
000403     03 FILLER PIC X(45) VALUE                                            
000404             '3021 NORMAL SMALL DECLINE EXPIRE 1Y'.                       
000405     03 FILLER PIC X(45) VALUE                                            
000406             '3023 NORMAL SMALL DECLINE EXPIRE 3Y'.                       
000407     03 FILLER PIC X(45) VALUE                                            
000408             '3024 NORMAL SMALL DECLINE DG 4'.                            
000409     03 FILLER PIC X(45) VALUE                                            
000410             '3026 NORMAL SMALL DECLINE DG 6'.                            
000411     03 FILLER PIC X(45) VALUE                                            
000412             '3100 NORMAL MEDIUM PHASE IN NORMAL'.                        
000413     03 FILLER PIC X(45) VALUE                                            
000414             '3101 NORMAL MEDIUM PHASE IN EXPIRE 1Y'.                     
000415     03 FILLER PIC X(45) VALUE                                            
000416             '3103 NORMAL MEDIUM PHASE IN EXPIRE 3Y'.                     
000417     03 FILLER PIC X(45) VALUE                                            
000418             '3104 NORMAL MEDIUM PHASE IN DG 4'.                          
000419     03 FILLER PIC X(45) VALUE                                            
000420             '3106 NORMAL MEDIUM PHASE IN DG 6'.                          
000421     03 FILLER PIC X(45) VALUE                                            
000422             '3110 NORMAL MEDIUM PRIME NORMAL'.                           
000423     03 FILLER PIC X(45) VALUE                                            
000424             '3111 NORMAL MEDIUM PRIME EXPIRE 1Y'.                        
000425     03 FILLER PIC X(45) VALUE                                            
000426             '3113 NORMAL MEDIUM PRIME EXPIRE 3Y'.                        
000427     03 FILLER PIC X(45) VALUE                                            
000428             '3114 NORMAL MEDIUM PRIME DG 4'.                             
000429     03 FILLER PIC X(45) VALUE                                            
000430             '3116 NORMAL MEDIUM PRIME DG 6'.                             
000431     03 FILLER PIC X(45) VALUE                                            
000432             '3120 NORMAL MEDIUM DECLINE NORMAL'.                         
000433     03 FILLER PIC X(45) VALUE                                            
000434             '3121 NORMAL MEDIUM DECLINE EXPIRE 1Y'.                      
000435     03 FILLER PIC X(45) VALUE                                            
000436             '3123 NORMAL MEDIUM DECLINE EXPIRE 3Y'.                      
000437     03 FILLER PIC X(45) VALUE                                            
000438             '3124 NORMAL MEDIUM DECLINE DG 4'.                           
000439     03 FILLER PIC X(45) VALUE                                            
000440             '3126 NORMAL MEDIUM DECLINE DG 6'.                           
000441     03 FILLER PIC X(45) VALUE                                            
000442             '3200 NORMAL LARGE PHASE IN NORMAL'.                         
000443     03 FILLER PIC X(45) VALUE                                            
000444             '3201 NORMAL LARGE PHASE IN EXPIRE 1Y'.                      
000445     03 FILLER PIC X(45) VALUE                                            
000446             '3203 NORMAL LARGE PHASE IN EXPIRE 3Y'.                      
000447     03 FILLER PIC X(45) VALUE                                            
000448             '3204 NORMAL LARGE PHASE IN DG 4'.                           
000449     03 FILLER PIC X(45) VALUE                                            
000450             '3206 NORMAL LARGE PHASE IN DG 6'.                           
000451     03 FILLER PIC X(45) VALUE                                            
000452             '3210 NORMAL LARGE PRIME NORMAL'.                            
000453     03 FILLER PIC X(45) VALUE                                            
000454             '3211 NORMAL LARGE PRIME EXPIRE 1Y'.                         
000455     03 FILLER PIC X(45) VALUE                                            
000456             '3213 NORMAL LARGE PRIME EXPIRE 3Y'.                         
000457     03 FILLER PIC X(45) VALUE                                            
000458             '3214 NORMAL LARGE PRIME DG 4'.                              
000459     03 FILLER PIC X(45) VALUE                                            
000460             '3216 NORMAL LARGE PRIME DG 6'.                              
000461     03 FILLER PIC X(45) VALUE                                            
000462             '3220 NORMAL LARGE DECLINE NORMAL'.                          
000463     03 FILLER PIC X(45) VALUE                                            
000464             '3221 NORMAL LARGE DECLINE EXPIRE 1Y'.                       
000465     03 FILLER PIC X(45) VALUE                                            
000466             '3223 NORMAL LARGE DECLINE EXPIRE 3Y'.                       
000467     03 FILLER PIC X(45) VALUE                                            
000468             '3224 NORMAL LARGE DECLINE DG 4'.                            
000469     03 FILLER PIC X(45) VALUE                                            
000470             '3226 NORMAL LARGE DECLINE DG 6'.                            
000471     03 FILLER PIC X(45) VALUE                                            
000472             '4000 ACCESSORIES SMALL PHASE IN NORMAL'.                    
000473     03 FILLER PIC X(45) VALUE                                            
000474             '4001 ACCESSORIES SMALL PHASE IN EXPIRE 1Y'.                 
000475     03 FILLER PIC X(45) VALUE                                            
000476             '4003 ACCESSORIES SMALL PHASE IN EXPIRE 3Y'.                 
000477     03 FILLER PIC X(45) VALUE                                            
000478             '4004 ACCESSORIES SMALL PHASE IN DG 4'.                      
000479     03 FILLER PIC X(45) VALUE                                            
000480             '4006 ACCESSORIES SMALL PHASE IN DG 6'.                      
000481     03 FILLER PIC X(45) VALUE                                            
000482             '4010 ACCESSORIES SMALL PRIME NORMAL'.                       
000483     03 FILLER PIC X(45) VALUE                                            
000484             '4011 ACCESSORIES SMALL PRIME EXPIRE 1Y'.                    
000485     03 FILLER PIC X(45) VALUE                                            
000486             '4013 ACCESSORIES SMALL PRIME EXPIRE 3Y'.                    
000487     03 FILLER PIC X(45) VALUE                                            
000488             '4014 ACCESSORIES SMALL PRIME DG 4'.                         
000489     03 FILLER PIC X(45) VALUE                                            
000490             '4016 ACCESSORIES SMALL PRIME DG 6'.                         
000491     03 FILLER PIC X(45) VALUE                                            
000492             '4020 ACCESSORIES SMALL DECLINE NORMAL'.                     
000493     03 FILLER PIC X(45) VALUE                                            
000494             '4021 ACCESSORIES SMALL DECLINE EXPIRE 1Y'.                  
000495     03 FILLER PIC X(45) VALUE                                            
000496             '4023 ACCESSORIES SMALL DECLINE EXPIRE 3Y'.                  
000497     03 FILLER PIC X(45) VALUE                                            
000498             '4024 ACCESSORIES SMALL DECLINE DG 4'.                       
000499     03 FILLER PIC X(45) VALUE                                            
000500             '4026 ACCESSORIES SMALL DECLINE DG 6'.                       
000501     03 FILLER PIC X(45) VALUE                                            
000502             '4100 ACCESSORIES MEDIUM PHASE IN NORMAL'.                   
000503     03 FILLER PIC X(45) VALUE                                            
000504             '4101 ACCESSORIES MEDIUM PHASE IN EXPIRE 1Y'.                
000505     03 FILLER PIC X(45) VALUE                                            
000506             '4103 ACCESSORIES MEDIUM PHASE IN EXPIRE 3Y'.                
000507     03 FILLER PIC X(45) VALUE                                            
000508             '4104 ACCESSORIES MEDIUM PHASE IN DG 4'.                     
000509     03 FILLER PIC X(45) VALUE                                            
000510             '4106 ACCESSORIES MEDIUM PHASE IN DG 6'.                     
000511     03 FILLER PIC X(45) VALUE                                            
000512             '4110 ACCESSORIES MEDIUM PRIME NORMAL'.                      
000513     03 FILLER PIC X(45) VALUE                                            
000514             '4111 ACCESSORIES MEDIUM PRIME EXPIRE 1Y'.                   
000515     03 FILLER PIC X(45) VALUE                                            
000516             '4113 ACCESSORIES MEDIUM PRIME EXPIRE 3Y'.                   
000517     03 FILLER PIC X(45) VALUE                                            
000518             '4114 ACCESSORIES MEDIUM PRIME DG 4'.                        
000519     03 FILLER PIC X(45) VALUE                                            
000520             '4116 ACCESSORIES MEDIUM PRIME DG 6'.                        
000521     03 FILLER PIC X(45) VALUE                                            
000522             '4120 ACCESSORIES MEDIUM DECLINE NORMAL'.                    
000523     03 FILLER PIC X(45) VALUE                                            
000524             '4121 ACCESSORIES MEDIUM DECLINE EXPIRE 1Y'.                 
000525     03 FILLER PIC X(45) VALUE                                            
000526             '4123 ACCESSORIES MEDIUM DECLINE EXPIRE 3Y'.                 
000527     03 FILLER PIC X(45) VALUE                                            
000528             '4124 ACCESSORIES MEDIUM DECLINE DG 4'.                      
000529     03 FILLER PIC X(45) VALUE                                            
000530             '4126 ACCESSORIES MEDIUM DECLINE DG 6'.                      
000531     03 FILLER PIC X(45) VALUE                                            
000532             '4200 ACCESSORIES LARGE PHASE IN NORMAL'.                    
000533     03 FILLER PIC X(45) VALUE                                            
000534             '4201 ACCESSORIES LARGE PHASE IN EXPIRE 1Y'.                 
000535     03 FILLER PIC X(45) VALUE                                            
000536             '4203 ACCESSORIES LARGE PHASE IN EXPIRE 3Y'.                 
000537     03 FILLER PIC X(45) VALUE                                            
000538             '4204 ACCESSORIES LARGE PHASE IN DG 4'.                      
000539     03 FILLER PIC X(45) VALUE                                            
000540             '4206 ACCESSORIES LARGE PHASE IN DG 6'.                      
000541     03 FILLER PIC X(45) VALUE                                            
000542             '4210 ACCESSORIES LARGE PRIME NORMAL'.                       
000543     03 FILLER PIC X(45) VALUE                                            
000544             '4211 ACCESSORIES LARGE PRIME EXPIRE 1Y'.                    
000545     03 FILLER PIC X(45) VALUE                                            
000546             '4213 ACCESSORIES LARGE PRIME EXPIRE 3Y'.                    
000547     03 FILLER PIC X(45) VALUE                                            
000548             '4214 ACCESSORIES LARGE PRIME DG 4'.                         
000549     03 FILLER PIC X(45) VALUE                                            
000550             '4216 ACCESSORIES LARGE PRIME DG 6'.                         
000551     03 FILLER PIC X(45) VALUE                                            
000552             '4220 ACCESSORIES LARGE DECLINE NORMAL'.                     
000553     03 FILLER PIC X(45) VALUE                                            
000554             '4221 ACCESSORIES LARGE DECLINE EXPIRE 1Y'.                  
000555     03 FILLER PIC X(45) VALUE                                            
000556             '4223 ACCESSORIES LARGE DECLINE EXPIRE 3Y'.                  
000557     03 FILLER PIC X(45) VALUE                                            
000558             '4224 ACCESSORIES LARGE DECLINE DG 4'.                       
000559     03 FILLER PIC X(45) VALUE                                            
000560             '4226 ACCESSORIES LARGE DECLINE DG 6'.                       
000561     03 FILLER PIC X(45) VALUE                                            
000562             '5000 PACKAGING SMALL PHASE IN NORMAL'.                      
000563     03 FILLER PIC X(45) VALUE                                            
000564             '5001 PACKAGING SMALL PHASE IN EXPIRE 1Y'.                   
000565     03 FILLER PIC X(45) VALUE                                            
000566             '5003 PACKAGING SMALL PHASE IN EXPIRE 3Y'.                   
000567     03 FILLER PIC X(45) VALUE                                            
000568             '5004 PACKAGING SMALL PHASE IN DG 4'.                        
000569     03 FILLER PIC X(45) VALUE                                            
000570             '5006 PACKAGING SMALL PHASE IN DG 6'.                        
000571     03 FILLER PIC X(45) VALUE                                            
000572             '5010 PACKAGING SMALL PRIME NORMAL'.                         
000573     03 FILLER PIC X(45) VALUE                                            
000574             '5011 PACKAGING SMALL PRIME EXPIRE 1Y'.                      
000575     03 FILLER PIC X(45) VALUE                                            
000576             '5013 PACKAGING SMALL PRIME EXPIRE 3Y'.                      
000577     03 FILLER PIC X(45) VALUE                                            
000578             '5014 PACKAGING SMALL PRIME DG 4'.                           
000579     03 FILLER PIC X(45) VALUE                                            
000580             '5016 PACKAGING SMALL PRIME DG 6'.                           
000581     03 FILLER PIC X(45) VALUE                                            
000582             '5020 PACKAGING SMALL DECLINE NORMAL'.                       
000583     03 FILLER PIC X(45) VALUE                                            
000584             '5021 PACKAGING SMALL DECLINE EXPIRE 1Y'.                    
000585     03 FILLER PIC X(45) VALUE                                            
000586             '5023 PACKAGING SMALL DECLINE EXPIRE 3Y'.                    
000587     03 FILLER PIC X(45) VALUE                                            
000588             '5024 PACKAGING SMALL DECLINE DG 4'.                         
000589     03 FILLER PIC X(45) VALUE                                            
000590             '5026 PACKAGING SMALL DECLINE DG 6'.                         
000591     03 FILLER PIC X(45) VALUE                                            
000592             '5100 PACKAGING MEDIUM PHASE IN NORMAL'.                     
000593     03 FILLER PIC X(45) VALUE                                            
000594             '5101 PACKAGING MEDIUM PHASE IN EXPIRE 1Y'.                  
000595     03 FILLER PIC X(45) VALUE                                            
000596             '5103 PACKAGING MEDIUM PHASE IN EXPIRE 3Y'.                  
000597     03 FILLER PIC X(45) VALUE                                            
000598             '5104 PACKAGING MEDIUM PHASE IN DG 4'.                       
000599     03 FILLER PIC X(45) VALUE                                            
000600             '5106 PACKAGING MEDIUM PHASE IN DG 6'.                       
000601     03 FILLER PIC X(45) VALUE                                            
000602             '5110 PACKAGING MEDIUM PRIME NORMAL'.                        
000603     03 FILLER PIC X(45) VALUE                                            
000604             '5111 PACKAGING MEDIUM PRIME EXPIRE 1Y'.                     
000605     03 FILLER PIC X(45) VALUE                                            
000606             '5113 PACKAGING MEDIUM PRIME EXPIRE 3Y'.                     
000607     03 FILLER PIC X(45) VALUE                                            
000608             '5114 PACKAGING MEDIUM PRIME DG 4'.                          
000609     03 FILLER PIC X(45) VALUE                                            
000610             '5116 PACKAGING MEDIUM PRIME DG 6'.                          
000611     03 FILLER PIC X(45) VALUE                                            
000612             '5120 PACKAGING MEDIUM DECLINE NORMAL'.                      
000613     03 FILLER PIC X(45) VALUE                                            
000614             '5121 PACKAGING MEDIUM DECLINE EXPIRE 1Y'.                   
000615     03 FILLER PIC X(45) VALUE                                            
000616             '5123 PACKAGING MEDIUM DECLINE EXPIRE 3Y'.                   
000617     03 FILLER PIC X(45) VALUE                                            
000618             '5124 PACKAGING MEDIUM DECLINE DG 4'.                        
000619     03 FILLER PIC X(45) VALUE                                            
000620             '5126 PACKAGING MEDIUM DECLINE DG 6'.                        
000621     03 FILLER PIC X(45) VALUE                                            
000622             '5200 PACKAGING LARGE PHASE IN NORMAL'.                      
000623     03 FILLER PIC X(45) VALUE                                            
000624             '5201 PACKAGING LARGE PHASE IN EXPIRE 1Y'.                   
000625     03 FILLER PIC X(45) VALUE                                            
000626             '5203 PACKAGING LARGE PHASE IN EXPIRE 3Y'.                   
000627     03 FILLER PIC X(45) VALUE                                            
000628             '5204 PACKAGING LARGE PHASE IN DG 4'.                        
000629     03 FILLER PIC X(45) VALUE                                            
000630             '5206 PACKAGING LARGE PHASE IN DG 6'.                        
000631     03 FILLER PIC X(45) VALUE                                            
000632             '5210 PACKAGING LARGE PRIME NORMAL'.                         
000633     03 FILLER PIC X(45) VALUE                                            
000634             '5211 PACKAGING LARGE PRIME EXPIRE 1Y'.                      
000635     03 FILLER PIC X(45) VALUE                                            
000636             '5213 PACKAGING LARGE PRIME EXPIRE 3Y'.                      
000637     03 FILLER PIC X(45) VALUE                                            
000638             '5214 PACKAGING LARGE PRIME DG 4'.                           
000639     03 FILLER PIC X(45) VALUE                                            
000640             '5216 PACKAGING LARGE PRIME DG 6'.                           
000641     03 FILLER PIC X(45) VALUE                                            
000642             '5220 PACKAGING LARGE DECLINE NORMAL'.                       
000643     03 FILLER PIC X(45) VALUE                                            
000644             '5221 PACKAGING LARGE DECLINE EXPIRE 1Y'.                    
000645     03 FILLER PIC X(45) VALUE                                            
000646             '5223 PACKAGING LARGE DECLINE EXPIRE 3Y'.                    
000647     03 FILLER PIC X(45) VALUE                                            
000648             '5224 PACKAGING LARGE DECLINE DG 4'.                         
000649     03 FILLER PIC X(45) VALUE                                            
000650             '5226 PACKAGING LARGE DECLINE DG 6'.                         
000651     03 FILLER PIC X(45) VALUE                                            
000652             '6000 TOOLS SMALL PHASE IN NORMAL'.                          
000653     03 FILLER PIC X(45) VALUE                                            
000654             '6001 TOOLS SMALL PHASE IN EXPIRE 1Y'.                       
000655     03 FILLER PIC X(45) VALUE                                            
000656             '6003 TOOLS SMALL PHASE IN EXPIRE 3Y'.                       
000657     03 FILLER PIC X(45) VALUE                                            
000658             '6004 TOOLS SMALL PHASE IN DG 4'.                            
000659     03 FILLER PIC X(45) VALUE                                            
000660             '6006 TOOLS SMALL PHASE IN DG 6'.                            
000661     03 FILLER PIC X(45) VALUE                                            
000662             '6010 TOOLS SMALL PRIME NORMAL'.                             
000663     03 FILLER PIC X(45) VALUE                                            
000664             '6011 TOOLS SMALL PRIME EXPIRE 1Y'.                          
000665     03 FILLER PIC X(45) VALUE                                            
000666             '6013 TOOLS SMALL PRIME EXPIRE 3Y'.                          
000667     03 FILLER PIC X(45) VALUE                                            
000668             '6014 TOOLS SMALL PRIME DG 4'.                               
000669     03 FILLER PIC X(45) VALUE                                            
000670             '6016 TOOLS SMALL PRIME DG 6'.                               
000671     03 FILLER PIC X(45) VALUE                                            
000672             '6020 TOOLS SMALL DECLINE NORMAL'.                           
000673     03 FILLER PIC X(45) VALUE                                            
000674             '6021 TOOLS SMALL DECLINE EXPIRE 1Y'.                        
000675     03 FILLER PIC X(45) VALUE                                            
000676             '6023 TOOLS SMALL DECLINE EXPIRE 3Y'.                        
000677     03 FILLER PIC X(45) VALUE                                            
000678             '6024 TOOLS SMALL DECLINE DG 4'.                             
000679     03 FILLER PIC X(45) VALUE                                            
000680             '6026 TOOLS SMALL DECLINE DG 6'.                             
000681     03 FILLER PIC X(45) VALUE                                            
000682             '6100 TOOLS MEDIUM PHASE IN NORMAL'.                         
000683     03 FILLER PIC X(45) VALUE                                            
000684             '6101 TOOLS MEDIUM PHASE IN EXPIRE 1Y'.                      
000685     03 FILLER PIC X(45) VALUE                                            
000686             '6103 TOOLS MEDIUM PHASE IN EXPIRE 3Y'.                      
000687     03 FILLER PIC X(45) VALUE                                            
000688             '6104 TOOLS MEDIUM PHASE IN DG 4'.                           
000689     03 FILLER PIC X(45) VALUE                                            
000690             '6106 TOOLS MEDIUM PHASE IN DG 6'.                           
000691     03 FILLER PIC X(45) VALUE                                            
000692             '6110 TOOLS MEDIUM PRIME NORMAL'.                            
000693     03 FILLER PIC X(45) VALUE                                            
000694             '6111 TOOLS MEDIUM PRIME EXPIRE 1Y'.                         
000695     03 FILLER PIC X(45) VALUE                                            
000696             '6113 TOOLS MEDIUM PRIME EXPIRE 3Y'.                         
000697     03 FILLER PIC X(45) VALUE                                            
000698             '6114 TOOLS MEDIUM PRIME DG 4'.                              
000699     03 FILLER PIC X(45) VALUE                                            
000700             '6116 TOOLS MEDIUM PRIME DG 6'.                              
000701     03 FILLER PIC X(45) VALUE                                            
000702             '6120 TOOLS MEDIUM DECLINE NORMAL'.                          
000703     03 FILLER PIC X(45) VALUE                                            
000704             '6121 TOOLS MEDIUM DECLINE EXPIRE 1Y'.                       
000705     03 FILLER PIC X(45) VALUE                                            
000706             '6123 TOOLS MEDIUM DECLINE EXPIRE 3Y'.                       
000707     03 FILLER PIC X(45) VALUE                                            
000708             '6124 TOOLS MEDIUM DECLINE DG 4'.                            
000709     03 FILLER PIC X(45) VALUE                                            
000710             '6126 TOOLS MEDIUM DECLINE DG 6'.                            
000711     03 FILLER PIC X(45) VALUE                                            
000712             '6200 TOOLS LARGE PHASE IN NORMAL'.                          
000713     03 FILLER PIC X(45) VALUE                                            
000714             '6201 TOOLS LARGE PHASE IN EXPIRE 1Y'.                       
000715     03 FILLER PIC X(45) VALUE                                            
000716             '6203 TOOLS LARGE PHASE IN EXPIRE 3Y'.                       
000717     03 FILLER PIC X(45) VALUE                                            
000718             '6204 TOOLS LARGE PHASE IN DG 4'.                            
000719     03 FILLER PIC X(45) VALUE                                            
000720             '6206 TOOLS LARGE PHASE IN DG 6'.                            
000721     03 FILLER PIC X(45) VALUE                                            
000722             '6210 TOOLS LARGE PRIME NORMAL'.                             
000723     03 FILLER PIC X(45) VALUE                                            
000724             '6211 TOOLS LARGE PRIME EXPIRE 1Y'.                          
000725     03 FILLER PIC X(45) VALUE                                            
000726             '6213 TOOLS LARGE PRIME EXPIRE 3Y'.                          
000727     03 FILLER PIC X(45) VALUE                                            
000728             '6214 TOOLS LARGE PRIME DG 4'.                               
000729     03 FILLER PIC X(45) VALUE                                            
000730             '6216 TOOLS LARGE PRIME DG 6'.                               
000731     03 FILLER PIC X(45) VALUE                                            
000732             '6220 TOOLS LARGE DECLINE NORMAL'.                           
000733     03 FILLER PIC X(45) VALUE                                            
000734             '6221 TOOLS LARGE DECLINE EXPIRE 1Y'.                        
000735     03 FILLER PIC X(45) VALUE                                            
000736             '6223 TOOLS LARGE DECLINE EXPIRE 3Y'.                        
000737     03 FILLER PIC X(45) VALUE                                            
000738             '6224 TOOLS LARGE DECLINE DG 4'.                             
000739     03 FILLER PIC X(45) VALUE                                            
000740             '6226 TOOLS LARGE DECLINE DG 6'.                             
054101                                                                          
054102 01  PSEGM-TAB           REDEFINES PSEGM-VALUES.                          
054103     03  PSEGM-TAB-RECORD OCCURS 270 TIMES                                
054104                          ASCENDING KEY IS PSEGM-ID                       
054105                          INDEXED BY SEGM-IX.                             
054106       05  PSEGM-ID            PIC 9(4).                                  
054107       05  FILLER              PIC X(1).                                  
054108       05  PSEGM-DESCRIPTION   PIC X(40).                                 
054109                                                                          
