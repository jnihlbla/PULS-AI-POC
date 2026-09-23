000010*** EDIT ALLOWED                                                          
000020*                            *************************************        
000030*                            *** ANVÄNDS VID TEST AV:                     
000040*                            ***  - DISTRIKT SOM HAR OLIKA                
000050*                            ***    TRANSPORTÖRER SOM FÅR EDI FIL         
000060*                            ***                                          
000070*                            * OBS! VIKTIGT!                              
000080*                            ***  - NÄR NYA DISTRIKT LÄGGS UPP            
000090*                            ***    SKALL ÄVEN DIST87-DIV-TRANSP          
000091*                            ***    UPPDATERAS MEN BARA OM DESSA          
000092*                            ***    DISTRIKT ÄVEN GÄLLER FÖR DIR.         
000093*                            ***    LEVERANSER (SE PGM. W4039000)         
000094*                            * OBS! VIKTIGT!                              
000095*                            ***                                          
000096*                            *************************************        
000097                                                                          
000098 01  DIST87-IDDISTR          PIC 9(5)    COMP-3.                          
000099*                                                                         
000100   88  DIST87-DIV-TRANSP     VALUE  1420 1438 1478                        
000101                                    1558                                  
000102                                    1822                                  
000103                                    1832                                  
000104                                    1870 1871                             
000105                                    1958                                  
000106                                    2078                                  
000107                                    2120 2138 2178                        
000108                                    2278                                  
000109                                    2330                                  
000110                                    2364 2365                             
000111                                    2371 2374 2375 2377                   
000112                                    2378                                  
000113                                    2380 2382 2383 2385                   
000114                                    2386 2387 2388                        
000115                                    2445                                  
000116                                    2561                                  
000117                                    2635                                  
000118                                    2681                                  
000119                                    2837                                  
000120                                    2867                                  
000121                                    2878                                  
000122                                    3020                                  
000123                                    7703.                                 
000124*                                                                         
000125   88  DIST87-DANX-INT       VALUE  0054.                                 
000126                                                                          
000127   88  DIST87-DANX           VALUE  1090                                  
000128                                    2635.                                 
000129                                                                          
000130   88  DIST87-DANX-DK        VALUE  0974                                  
000131                                    0978.                                 
000132                                                                          
000133   88  DIST87-DANX-POLESTAR  VALUE  0069.                                 
000134                                                                          
000135   88  DIST87-DANX-SE        VALUE  0778.                                 
000136                                                                          
000139   88  DIST87-DANX-LYNK      VALUE  2638.                                 
000140                                                                          
000141   88  DIST87-DANX-POLEN     VALUE  2878.                                 
000142                                                                          
000153   88  DIST87-TRUCKWHEEL     VALUE  1420                                  
000154                                    1438                                  
000155                                    1478.                                 
000156                                                                          
000157   88  DIST87-BCUBE          VALUE  1822                                  
000158                                    1832                                  
000159                                    1870 1871.                            
000160                                                                          
000161   88  DIST87-BCUBE-PLUS     VALUE  1538                                  
000162                                    1558                                  
000163                                    3020.                                 
000164                                                                          
000165   88  DIST87-GALLIKER       VALUE  2078.                                 
000166                                                                          
000167   88  DIST87-NEOVIA-ES      VALUE  1958                                  
000168                                    2120                                  
000169                                    2138                                  
000170                                    2178                                  
000171                                    7703.                                 
000172                                                                          
000173   88  DIST87-NEOVIA-AFRIKA  VALUE  3130                                  
000174                                    3680                                  
000175                                    4400 4410.                            
000176                                                                          
000177   88  DIST87-NIGHT-PLUS     VALUE  2278.                                 
000180                                                                          
000181   88  DIST87-LAGERMAX       VALUE  2364 2365                             
000182                                    2371 2374 2375 2377                   
000183                                    2378                                  
000184                                    2380 2382 2383 2385                   
000185                                    2386 2387 2388                        
000186                                    2445                                  
000187                                    2561                                  
000188                                    2681                                  
000189                                    2867.                                 
000190                                                                          
000191   88  DIST87-LAGERMAX-LYNK  VALUE  2330                                  
000192                                    2837.                                 
000193                                                                          
000197   88  DIST87-SCHENKER       VALUE  8162.                                 
000400                                                                          
001162   88  DIST87-DHL            VALUE  0778.                                 
001163                                                                          
001164   88  DIST87-DHL-NO         VALUE  0878 8859.                            
001165                                                                          
001180*** END COPY WWDIST87    LENGTH=5                                         
