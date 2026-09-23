000010*** EDIT ALLOWED                                                          
000020*                            *************************************        
000030*                            *** ANVÄNDS VID TEST AV:                     
000040*                            ***  - VILKA DISTRIKT SOM SKALL              
000050*                            ***    VARA MED I RESTORDERLISTA             
000060*                            ***    W4408X.                               
000070*                            *************************************        
000080 01  DIST99-IDDISTR          PIC S9(5)     COMP-3.                        
000090**                                                                        
000100     88  DIST99-RO-DIST      VALUE  0778 0878 0974                        
000200                                    0978 1090 1258                        
000300                                    1378 1478 1558 1578                   
000400                                    1678 1822 1920 1958                   
000500                                    2078 2178 2278                        
000600                                    2378 2697 3160 3162                   
000700                                    5120 5616 5619 5627                   
000800                                    6010 6124 6200 6203 6225              
000900                                    6222 6240 6251 6270 6271 6281         
001000                                    7574 7674 1110 1778 2365 2374         
001100                                    2380 2382 2383 2385 2386 2387         
001101                                    2388 2561 2601 2626 2634 2665         
001102                                    2615 2681 2688 2696 2715 2716         
001103                                    2867 2878                             
001104                                    3020 3130 3250 3680                   
001105                                    4400 4410 4553                        
001106                                    4810 4860 5314 5400 5415 5510         
001107                                    5620 5624 5811 5920                   
001108                                    6015 6017 6027 6028                   
001109                                    6210 6231 6233 6236 6247              
001110                                    6480 6591 6680 6790 7051 7060         
001111                                    7080 7190 7290 7472 7474 7480         
001112                                    7481 7656 7899                        
001113                                    5216 2635 5214 7490 7838.             
001114**                                                                        
001115                                                                          
001116     88  DIST99-RO-AT        VALUE  2378.                                 
001117     88  DIST99-RO-BE        VALUE  1258.                                 
001118     88  DIST99-RO-CA        VALUE  7674.                                 
001119     88  DIST99-RO-CH        VALUE  2078.                                 
001120     88  DIST99-RO-CN4       VALUE  6270 6271 6281.                       
001121     88  DIST99-RO-DE        VALUE  2278.                                 
001122     88  DIST99-RO-DK        VALUE  0974 0978.                            
001123     88  DIST99-RO-ES        VALUE  2178.                                 
001124     88  DIST99-RO-FI        VALUE  1090.                                 
001125     88  DIST99-RO-FR        VALUE  1478.                                 
001126     88  DIST99-RO-GB        VALUE  1378.                                 
001127     88  DIST99-RO-GR        VALUE  1558 1578.                            
001128     88  DIST99-RO-HK        VALUE  6240.                                 
001129     88  DIST99-RO-IL        VALUE  5120.                                 
001130     88  DIST99-RO-IT        VALUE  1822.                                 
001131     88  DIST99-RO-PT        VALUE  1920 1958.                            
001132     88  DIST99-RO-MY        VALUE  5619 5627.                            
001133     88  DIST99-RO-NL        VALUE  1678.                                 
001134     88  DIST99-RO-NO        VALUE  0878.                                 
001135     88  DIST99-RO-SE        VALUE  0778.                                 
001136     88  DIST99-RO-SG        VALUE  5616.                                 
001137     88  DIST99-RO-TH        VALUE  6251 6225.                            
001138     88  DIST99-RO-TW        VALUE  6200 6203 6222.                       
001139     88  DIST99-RO-US        VALUE  7574.                                 
001140     88  DIST99-RO-ZA        VALUE  3160 3162.                            
001141     88  DIST99-RO-IN        VALUE  6010.                                 
001142     88  DIST99-RO-KR        VALUE  6124.                                 
001143     88  DIST99-RO-RU        VALUE  2697 2601.                            
001144     88  DIST99-RO-IS        VALUE  1110.                                 
001145     88  DIST99-RO-IE        VALUE  1778.                                 
001146     88  DIST99-RO-CZ        VALUE  2365.                                 
001147     88  DIST99-RO-HU        VALUE  2374.                                 
001148     88  DIST99-RO-SI        VALUE  2380.                                 
001149     88  DIST99-RO-BA        VALUE  2382.                                 
001150     88  DIST99-RO-MK        VALUE  2383.                                 
001151     88  DIST99-RO-ME        VALUE  2385.                                 
001152     88  DIST99-RO-SK        VALUE  2386.                                 
001153     88  DIST99-RO-RS        VALUE  2387.                                 
001154     88  DIST99-RO-AL        VALUE  2388.                                 
001155     88  DIST99-RO-BG        VALUE  2561.                                 
001156     88  DIST99-RO-UA        VALUE  2615 2715 2716.                       
001157     88  DIST99-RO-UZ        VALUE  2626 2634.                            
001158     88  DIST99-RO-AZ        VALUE  2665.                                 
001159     88  DIST99-RO-RO        VALUE  2867.                                 
001160     88  DIST99-RO-MD        VALUE  2681.                                 
001161     88  DIST99-RO-GE        VALUE  2688.                                 
001162     88  DIST99-RO-AM        VALUE  2696.                                 
001163     88  DIST99-RO-PL        VALUE  2878.                                 
001164     88  DIST99-RO-MT        VALUE  3020.                                 
001165     88  DIST99-RO-AO        VALUE  3130.                                 
001166     88  DIST99-RO-EG        VALUE  3250.                                 
001167     88  DIST99-RO-MA        VALUE  3680.                                 
001168     88  DIST99-RO-TN        VALUE  4400 4410.                            
001169     88  DIST99-RO-MU        VALUE  4810.                                 
001170     88  DIST99-RO-SA        VALUE  4860.                                 
001171     88  DIST99-RO-JO        VALUE  5314.                                 
001172     88  DIST99-RO-KW        VALUE  5400.                                 
001173     88  DIST99-RO-QA        VALUE  5415.                                 
001174     88  DIST99-RO-LB        VALUE  5510.                                 
001175     88  DIST99-RO-MM        VALUE  5620.                                 
001176     88  DIST99-RO-BN        VALUE  5624.                                 
001177     88  DIST99-RO-TR        VALUE  5811.                                 
001178     88  DIST99-RO-ID        VALUE  5920.                                 
001179     88  DIST99-RO-PH        VALUE  6015 6017.                            
001180     88  DIST99-RO-BD        VALUE  6027.                                 
001181     88  DIST99-RO-LK        VALUE  6028.                                 
001182     88  DIST99-RO-CY        VALUE  6210.                                 
001183     88  DIST99-RO-YE        VALUE  6231.                                 
001184     88  DIST99-RO-OM        VALUE  6233.                                 
001185     88  DIST99-RO-BH        VALUE  6236.                                 
001186     88  DIST99-RO-AE        VALUE  6247.                                 
001187     88  DIST99-RO-CL        VALUE  6480.                                 
001188     88  DIST99-RO-MX        VALUE  6591.                                 
001189     88  DIST99-RO-PY        VALUE  4553 6680.                            
001190     88  DIST99-RO-PE        VALUE  6790.                                 
001191     88  DIST99-RO-BR        VALUE  7051.                                 
001192     88  DIST99-RO-GY        VALUE  7060.                                 
001193     88  DIST99-RO-SR        VALUE  7080.                                 
001194     88  DIST99-RO-CR        VALUE  7190.                                 
001195     88  DIST99-RO-PA        VALUE  7290.                                 
001196     88  DIST99-RO-DO        VALUE  7472.                                 
001197     88  DIST99-RO-PR        VALUE  7474.                                 
001198     88  DIST99-RO-EC        VALUE  7480 7656.                            
001199     88  DIST99-RO-CO        VALUE  7481.                                 
001200     88  DIST99-RO-UY        VALUE  7899.                                 
001201     88  DIST99-RO-KH        VALUE  5216.                                 
001202     88  DIST99-RO-LV        VALUE  2635.                                 
001203     88  DIST99-RO-VN        VALUE  5214.                                 
001204     88  DIST99-RO-SV        VALUE  7490.                                 
001205     88  DIST99-RO-AU        VALUE  7838.                                 
001206*                                                                         
001207*** END COPY WWDIST99    LENGTH=3                                         
