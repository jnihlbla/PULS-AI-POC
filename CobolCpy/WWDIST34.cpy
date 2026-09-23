000100*** EDIT ALLOWED                                                          
000201*                            *************************************        
000301*                            *** ANVÄNDS VID TEST AV:                     
000401*                            ***                                          
000501*                            ***  VILKA DISTRIKT SOM FINNS INOM           
000601*                            ***  ETT SDC:S EGET LAND (ANVÄNDS FÖR        
000701*                            ***  BLAND ANNAT MOMSBERÄKNING)              
000801*                            ***  ÄVEN INOM NDC OCH DDC  OCH LDC          
000901*                            ***                                          
001001*                            *************************************        
001101                                                                          
001201 01  DIST34-IDDISTR          PIC 9(5)     COMP-3.                         
001301*                                                                         
001401       88  DIST34-HOLLAND-SDC       VALUE  1600 THRU 1699.                
001501*                                                                         
001601       88  DIST34-FRANKRIKE-SDC     VALUE  1400 THRU 1499.                
001701*                                                                         
001801       88  DIST34-ENGLAND-SDC       VALUE  1778                           
001901                                           1300 THRU 1399                 
002001                                           8800.                          
002101*                                                                         
002201       88  DIST34-ITALIEN-SDC       VALUE  1800 THRU 1899.                
002301*                                                                         
002401       88  DIST34-SPANIEN-SDC       VALUE  2100 THRU 2199.                
002501*                                                                         
002601       88  DIST34-AUSTRIA-SDC       VALUE  2370 THRU 2378.                
002701*                                                                         
002801       88  DIST34-SWEDEN-LDC        VALUE  778                            
002901                                           8780 8790.                     
003001*                                                                         
003101       88  DIST34-NORWAY-LDC        VALUE  878                            
003201                                           8780 8790.                     
003301*                                                                         
003401       88  DIST34-FINLAND-LDC       VALUE  1090                           
003501                                           8880 8890.                     
003601*                                                                         
003701       88  DIST34-BELGIEN-LDC       VALUE  1258                           
003801                                           8880 8890.                     
003901*                                                                         
004001       88  DIST34-ENGLAND-LDC       VALUE  1378                           
004101                                           8880 8890.                     
004201*                                                                         
004301       88  DIST34-FRANCE-LDC        VALUE  1478                           
004401                                           8880 8890.                     
004501*                                                                         
004601       88  DIST34-HOLLAND-LDC       VALUE  1678                           
004701                                           8880 8890.                     
004801*                                                                         
004901       88  DIST34-ITALIEN-LDC       VALUE  1822                           
005001                                           8880 8890.                     
005101*                                                                         
005201       88  DIST34-SCHWEIZ-LDC       VALUE  2078                           
005301                                           8880 8890.                     
005401*                                                                         
005501       88  DIST34-TYSKLAND-LDC      VALUE  2278                           
005601                                           8880 8890.                     
005701*                                                                         
005801       88  DIST34-POLAND-LDC        VALUE  2878                           
005901                                           8880 8890.                     
006001*                                                                         
006101       88  DIST34-KINA-LDC          VALUE  6271 6281                      
006201                                           8980 8990.                     
006301*                                                                         
006401       88  DIST34-NDC-NA            VALUE  7500 THRU 7537                 
006501                                           7539 THRU 7638                 
006601                                           7640 THRU 7699.                
006701*                                                                         
006801       88  DIST34-USA-NDC           VALUE  7500 THRU 7537                 
006901                                           7539 THRU 7599.                
007001*                                                                         
007101       88  DIST34-KANADA-NDC        VALUE  7600 THRU 7638                 
007201                                           7640 THRU 7699.                
007301*                                                                         
007401       88  DIST34-NDC-BYPASS        VALUE  7510 7625.                     
007501*                                                                         
007601       88  DIST34-NDC-PACIFIC       VALUE  5222 6010 7838.                
007701*                                                                         
007801       88  DIST34-JAPAN-NDC         VALUE  5222.                          
007901*                                                                         
008001       88  DIST34-INDIA-NDC         VALUE  6010.                          
008101*                                                                         
008201       88  DIST34-AUSTRALIA-NDC     VALUE  7838.                          
008301*                                                                         
008401       88  DIST34-KINA-NDC          VALUE  6271 6281.                     
008501*                                                                         
008601       88  DIST34-THAILAND-NDC      VALUE  6225.                          
008701*                                                                         
008801       88  DIST34-TAIWAN-NDC        VALUE  6203.                          
008901*                                                                         
009001       88  DIST34-KOREA-NDC         VALUE  6124.                          
009101*                                                                         
009201       88  DIST34-TURKEY-NDC        VALUE  5811.                          
009301*                                                                         
009401       88  DIST34-MALAYSIA-NDC      VALUE  5627.                          
009501*                                                                         
009601       88  DIST34-RUSSIA-NDC        VALUE  2697.                          
009701*                                                                         
009801       88  DIST34-EMIRATES-NDC      VALUE  6247.                          
009901*                                                                         
010001       88  DIST34-MEXICO-NDC        VALUE  6591.                          
010101*                                                                         
010102       88  DIST34-BRAZIL-NDC        VALUE  7051.                          
010103*                                                                         
010201       88  DIST34-SOUTH-AFRICA-NDC  VALUE  3162.                          
010301*                                                                         
010401       88  DIST34-SVERIGE-DDC       VALUE     1 THRU  799                 
010501                                           8023.                          
010601*                                                                         
010701       88  DIST34-NORGE-DDC         VALUE   800 THRU  899                 
010801                                           1130 THRU 1169.                
010901*                                                                         
011001       88  DIST34-FINLAND-DDC       VALUE  1090 THRU 1099.                
011101*                                                                         
011201       88  DIST34-BELGIEN-DDC       VALUE  1200 THRU 1299.                
011301*                                                                         
011401       88  DIST34-FRANKRIKE-DDC     VALUE  1420 1478 1479.                
011501*                                                                         
011601       88  DIST34-TYSKLAND-DDC      VALUE  2259 THRU 2278.                
011701*                                                                         
011801       88  DIST34-POLAND-DDC        VALUE  2878.                          
011901*                                                                         
012001       88  DIST34-KOREA-DDC         VALUE  6121 6124.                     
012101*                                                                         
012201       88  DIST34-TURKEY-DDC        VALUE  5811.                          
012301*                                                                         
012302       88  DIST34-ENGLAND-DDC       VALUE  1300 THRU 1399.                
012303*                                                                         
012302       88  DIST34-AUSTRALIA-DDC     VALUE  7838.                          
012303*                                                                         
012401*      88  DIST34-HUNGARY-DDC       VALUE  ???? ????.                     
012501*                                                                         
012601*** END COPY WWDIST34    LENGTH=3                                         
