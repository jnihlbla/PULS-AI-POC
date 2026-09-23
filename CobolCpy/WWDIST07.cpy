000100*** EDIT ALLOWED                                                          
002000*                            *************************************        
003000*                            *** ANVÄNDS VID TEST AV:                     
004000*                            ***  - DISTRIKT SOM TILLHÖR                  
005000*                            ***    USA, KANADA, PACIFIK, KINA            
006000*                            ***                                          
007000*                            *************************************        
008000                                                                          
009000 01  DIST07-IDDISTR          PIC 9(5)     COMP-3.                         
010000*                                                                         
020000       88  DIST07-NA-CUSTOMERS        VALUE 7574 7575 7580 7579           
030000                                            7510                          
040000                                            7674 7680 7639.               
050000       88  DIST07-USA-CUSTOMERS       VALUE 7574 7575 7580 7579           
060000                                            7510.                         
070000       88  DIST07-USA-RETAILER        VALUE 7574 7575.                    
080000       88  DIST07-USA-RETAILER-DNOTE  VALUE 7574 7575.                    
090000       88  DIST07-USA-PRINT-DNOTE     VALUE 7574.                         
100000       88  DIST07-USA-RET-DISCR       VALUE 7580.                         
110000       88  DIST07-USA-SUPPL-FROM-CDC  VALUE 7579.                         
120000       88  DIST07-CAN-CUSTOMERS       VALUE 7674 7680 7639.               
121000       88  DIST07-CAN-RETAILER        VALUE 7674.                         
122000       88  DIST07-CAN-PRINT-DNOTE     VALUE 7674.                         
122100       88  DIST07-CAN-RET-DISCR       VALUE 7680.                         
122200       88  DIST07-CAN-SUPPL-FROM-CDC  VALUE 7639.                         
122500       88  DIST07-MEXICO              VALUE 6591.                         
122600       88  DIST07-MEXICO-RET-DISCR    VALUE 6382.                         
122610       88  DIST07-BRAZIL              VALUE 7051.                         
122620       88  DIST07-BRAZIL-RET-DISCR    VALUE 6380.                         
122700       88  DIST07-PACIFIC             VALUE 5222 7838.                    
122800       88  DIST07-JAPAN               VALUE 5222.                         
122900       88  DIST07-AUSTRALIEN          VALUE 7838.                         
123000       88  DIST07-KINA                VALUE 6271 6281.                    
123100       88  DIST07-KINA-RET-DISCR      VALUE 6280.                         
123200       88  DIST07-INDIEN              VALUE 6010 6081.                    
123300       88  DIST07-INDIEN-RET-DISCR    VALUE 6080.                         
123400       88  DIST07-THAILAND            VALUE 6225.                         
123500       88  DIST07-THAILAND-RET-DISCR  VALUE 6282.                         
123600       88  DIST07-TAIWAN              VALUE 6203.                         
123700       88  DIST07-TAIWAN-RET-DISCR    VALUE 6182.                         
123800       88  DIST07-KOREA               VALUE 6124 6181.                    
123900       88  DIST07-KOREA-RET-DISCR     VALUE 6180.                         
124000       88  DIST07-TURKEY              VALUE 5811.                         
124100       88  DIST07-TURKEY-RET-DISCR    VALUE 6486.                         
124200       88  DIST07-MALAYSIA            VALUE 5627.                         
124300       88  DIST07-MALAYSIA-RET-DISCR  VALUE 6082.                         
124400       88  DIST07-RUSSIA              VALUE 2697.                         
124500       88  DIST07-RUSSIA-RET-DISCR    VALUE 6293.                         
124602       88  DIST07-S-AFRICA            VALUE 3162.                         
124702       88  DIST07-S-AFRICA-RET-DISCR  VALUE 6484 6485.                    
124802                                                                          
124901*                                                                         
125001* 2697 (MOSCOW) IS NOT ADDED IN BELOW GROUP                               
125101* BECAUSE THEY ARE SOFTWARE DIST TODAY AND SHOULD BE STILL SEK            
125201       88  DIST07-NON-VCC-OWNED       VALUE 3162                          
125202                                            5627                          
125301                                            5811                          
125401                                            6010 6081                     
125501                                            6124 6181                     
125601                                            6203 6225                     
125701                                            6271 6281                     
125801                                            6591                          
125802                                            7051.                         
126000*** END COPY WWDIST07    LENGTH=3                                         
