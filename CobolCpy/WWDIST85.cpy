001000*** EDIT ALLOWED                                                          
002000*                            *************************************        
003000*                            *** ANVÄNDS AV DISTRIKT OCH KUNDER           
004000*                            *** SOM SKALL HA AVISERING AV                
005000*                            *** PACKNINGSUNDERLAG VIA VR                 
006000*                            ***                                          
007000*                            *** (TYP SAMMA SOM TILL SVENSKA ÅF)          
008000*                            *************************************        
009000                                                                          
010000 01  DIST85-IDDISTR          PIC 9(4)   COMP-3.                           
020000*                                                                         
030000     88  DIST85-PU-VIA-VR    VALUE 1258 1378 1478                         
040000                                   1558 1578                              
050000                                   1678 1778 1822 1978                    
060000                                   2078                                   
070000                                   2278                                   
080000                                   2364 2365 2370 2375 2378               
090000                                   2602 2635 2697                         
100000                                   2878                                   
110000                                   3160 3162 3166 3167                    
120000                                   3250                                   
121000                                   4850                                   
121100                                   5120                                   
122000                                   5400 5410 5415                         
123000                                   5619 5627                              
124000                                   5810 5811                              
125000                                   6121 6124                              
126100                                   6200 6203 6210 6225 6247 6251          
127000                                   6320                                   
127100                                   6480                                   
127200                                   6589 6591                              
127300                                   6785 6790                              
127400                                   7026 7050 7051                         
127500                                   7474 7481 7482 7490                    
127600                                   7574                                   
127700                                   7674                                   
127800                                   7701 7702 7703.                        
127900*                                                                         
128000* ALLA DISTRIKT SOM LÄGGS TILL/TAS BORT NEDAN SKALL ÄVEN                  
128100* LÄGGAS TILL/TAS BORT I FÖRSTA 88-NIVÅN                                  
128200*                                                                         
128300     88  DIST85-BELGIEN      VALUE 1258 7702.                             
128400     88  DIST85-ENGLAND      VALUE 1378 7701.                             
128500     88  DIST85-FRANKRIKE    VALUE 1478.                                  
128600     88  DIST85-GREKLAND     VALUE 1558 1578.                             
128700     88  DIST85-HOLLAND      VALUE 1678.                                  
128800     88  DIST85-IRLAND       VALUE 1778.                                  
128900     88  DIST85-ITALIEN      VALUE 1822.                                  
129000     88  DIST85-PORTUGAL     VALUE 1978.                                  
129100     88  DIST85-SCHWEIZ      VALUE 2078.                                  
129200     88  DIST85-TYSKLAND     VALUE 2278.                                  
129300     88  DIST85-HUNGARY      VALUE 2364.                                  
129400     88  DIST85-TJECKIEN     VALUE 2365 2375.                             
129500     88  DIST85-SLOVENIEN    VALUE 2370.                                  
129600     88  DIST85-AUSTRIA      VALUE 2378.                                  
129700     88  DIST85-ESTONIA      VALUE 2635.                                  
129800     88  DIST85-LATVIA       VALUE 2635.                                  
129900     88  DIST85-LITHUANIA    VALUE 2635.                                  
130000     88  DIST85-RYSSLAND     VALUE 2602 2697.                             
130100     88  DIST85-POLEN        VALUE 2878.                                  
130200     88  DIST85-SYDAFRIKA    VALUE 3160 3162 3166 3167.                   
130300     88  DIST85-EGYPT        VALUE 3250.                                  
130400     88  DIST85-SAUDI        VALUE 4850.                                  
130500     88  DIST85-ISRAEL       VALUE 5120.                                  
130600     88  DIST85-KUWAIT       VALUE 5400 5410.                             
130700     88  DIST85-UAE          VALUE 5415 6247.                             
130800     88  DIST85-MALAYSIA     VALUE 5619 5627.                             
130900     88  DIST85-TURKIET      VALUE 5810 5811.                             
131000     88  DIST85-KOREA        VALUE 6121 6124.                             
131100     88  DIST85-TAIWAN       VALUE 6200 6203.                             
131200     88  DIST85-CYPERN       VALUE 6210.                                  
131300     88  DIST85-THAILAND     VALUE 6251 6225.                             
131400     88  DIST85-ARGENTINA    VALUE 6320.                                  
131500     88  DIST85-CHILE        VALUE 6480.                                  
131600     88  DIST85-MEXICO       VALUE 6589.                                  
131700     88  DIST85-PERU         VALUE 6785 6790.                             
131800     88  DIST85-BRASILIEN    VALUE 7026 7050 7051.                        
131900     88  DIST85-PUERTORICO   VALUE 7474.                                  
132000     88  DIST85-COLOMBIA     VALUE 7481 7482.                             
132100     88  DIST85-GUATEMALA    VALUE 7490.                                  
132200     88  DIST85-USA          VALUE 7574.                                  
132300     88  DIST85-CANADA       VALUE 7674.                                  
132400     88  DIST85-SPAIN        VALUE 7703.                                  
132500                                                                          
132600*                                                                         
132700*** END COPY WWDIST85    LENGTH=6                                         
