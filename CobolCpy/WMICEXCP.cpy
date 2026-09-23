000001*** EDIT ALLOWED                                                          
000002*                            *************************************        
000003*                            *** ANVÄNDS VID TEST AV:                     
000004*                            ***  - ORDER SOM INTE SKALL SKICKAS          
000005*                            ***    TILL MIC                              
000006*                            ***                                          
000007*                            ***    ANVÄND SEARCH ALL FÖR ATT             
000008*                            ***    SÖKA I TABELLEN.                      
000009*                            ***                                          
000010*                            ***  OBS!!!                                  
000011*                            ***    TABELLEN MÅSTE VARA SORTERAD          
000012*                            ***    I DC-DSTRIKTS-ORDNING.                
000013*                            ***    ANPASSA OCCURS NN LÄNGST NER!!        
000014*                            *************************************        
000015*                                                                         
000016*   TABELL FÖR ATT SÖKA DC / DISTRIKT SOM INTE SKICKAS TILL MIC.          
000017*                                                                         
000018 01  MICEXCP-TABELL-VALUES.                                               
000019     03  FILLER             PIC X(8) VALUE 'BE 01257'.                    
000020     03  FILLER             PIC X(8) VALUE 'BE 01258'.                    
000021     03  FILLER             PIC X(8) VALUE 'DE 02278'.                    
000022     03  FILLER             PIC X(8) VALUE 'NO 00878'.                    
000023     03  FILLER             PIC X(8) VALUE 'PL 02878'.                    
000024     03  FILLER             PIC X(8) VALUE 'SE 00778'.                    
000025     03  FILLER             PIC X(8) VALUE '1A 00778'.                    
000026     03  FILLER             PIC X(8) VALUE '1B 00778'.                    
000027     03  FILLER             PIC X(8) VALUE '1C 00778'.                    
000028     03  FILLER             PIC X(8) VALUE '1D 00778'.                    
000029     03  FILLER             PIC X(8) VALUE '1E 00778'.                    
000030     03  FILLER             PIC X(8) VALUE '11 00778'.                    
000031     03  FILLER             PIC X(8) VALUE '2C 01378'.                    
000032     03  FILLER             PIC X(8) VALUE '2C 01379'.                    
000033     03  FILLER             PIC X(8) VALUE '2H 01378'.                    
000034     03  FILLER             PIC X(8) VALUE '2H 01379'.                    
000035     03  FILLER             PIC X(8) VALUE '2I 02259'.                    
000036     03  FILLER             PIC X(8) VALUE '2I 02278'.                    
000037     03  FILLER             PIC X(8) VALUE '2I 02282'.                    
000038     03  FILLER             PIC X(8) VALUE '2J 02259'.                    
000039     03  FILLER             PIC X(8) VALUE '2J 02278'.                    
000040     03  FILLER             PIC X(8) VALUE '2J 02282'.                    
000041     03  FILLER             PIC X(8) VALUE '2L 02259'.                    
000042     03  FILLER             PIC X(8) VALUE '2L 02278'.                    
000043     03  FILLER             PIC X(8) VALUE '2L 02282'.                    
000044     03  FILLER             PIC X(8) VALUE '2M 01620'.                    
000045     03  FILLER             PIC X(8) VALUE '2M 01678'.                    
000046     03  FILLER             PIC X(8) VALUE '2M 01679'.                    
000047     03  FILLER             PIC X(8) VALUE '21 01620'.                    
000048     03  FILLER             PIC X(8) VALUE '21 01678'.                    
000049     03  FILLER             PIC X(8) VALUE '21 01679'.                    
000050     03  FILLER             PIC X(8) VALUE '24 02178'.                    
000051     03  FILLER             PIC X(8) VALUE '24 02179'.                    
000052     03  FILLER             PIC X(8) VALUE '26 02378'.                    
000053     03  FILLER             PIC X(8) VALUE '3A 01348'.                    
000054     03  FILLER             PIC X(8) VALUE '3A 01378'.                    
000055     03  FILLER             PIC X(8) VALUE '3A 01379'.                    
000056     03  FILLER             PIC X(8) VALUE '3B 01348'.                    
000057     03  FILLER             PIC X(8) VALUE '3B 01378'.                    
000058     03  FILLER             PIC X(8) VALUE '3B 01379'.                    
000059     03  FILLER             PIC X(8) VALUE '3C 02259'.                    
000060     03  FILLER             PIC X(8) VALUE '3C 02278'.                    
000061     03  FILLER             PIC X(8) VALUE '3C 02282'.                    
000062     03  FILLER             PIC X(8) VALUE '3D 01822'.                    
000063     03  FILLER             PIC X(8) VALUE '3E 02259'.                    
000064     03  FILLER             PIC X(8) VALUE '3E 02278'.                    
000065     03  FILLER             PIC X(8) VALUE '3E 02282'.                    
000066     03  FILLER             PIC X(8) VALUE '3F 01822'.                    
000067     03  FILLER             PIC X(8) VALUE '3F 01870'.                    
000068     03  FILLER             PIC X(8) VALUE '3F 01871'.                    
000069     03  FILLER             PIC X(8) VALUE '3F 01879'.                    
000070     03  FILLER             PIC X(8) VALUE '3G 02259'.                    
000071     03  FILLER             PIC X(8) VALUE '3G 02278'.                    
000072     03  FILLER             PIC X(8) VALUE '3G 02282'.                    
000073     03  FILLER             PIC X(8) VALUE '3H 02059'.                    
000074     03  FILLER             PIC X(8) VALUE '3H 02078'.                    
000075     03  FILLER             PIC X(8) VALUE '3J 00870'.                    
000076     03  FILLER             PIC X(8) VALUE '3J 00878'.                    
000077     03  FILLER             PIC X(8) VALUE '3J 00879'.                    
000078     03  FILLER             PIC X(8) VALUE '3K 02259'.                    
000079     03  FILLER             PIC X(8) VALUE '3K 02278'.                    
000080     03  FILLER             PIC X(8) VALUE '3K 02282'.                    
000081     03  FILLER             PIC X(8) VALUE '3L 01257'.                    
000082     03  FILLER             PIC X(8) VALUE '3L 01258'.                    
000083     03  FILLER             PIC X(8) VALUE '3L 01259'.                    
000084     03  FILLER             PIC X(8) VALUE '3M 02259'.                    
000085     03  FILLER             PIC X(8) VALUE '3M 02278'.                    
000086     03  FILLER             PIC X(8) VALUE '3M 02282'.                    
000087     03  FILLER             PIC X(8) VALUE '3N 01620'.                    
000088     03  FILLER             PIC X(8) VALUE '3N 01678'.                    
000089     03  FILLER             PIC X(8) VALUE '3N 01679'.                    
000090     03  FILLER             PIC X(8) VALUE '3O 01090'.                    
000091     03  FILLER             PIC X(8) VALUE '3O 01091'.                    
000092     03  FILLER             PIC X(8) VALUE '3P 01420'.                    
000093     03  FILLER             PIC X(8) VALUE '3P 01478'.                    
000094     03  FILLER             PIC X(8) VALUE '3P 01479'.                    
000095     03  FILLER             PIC X(8) VALUE '3R 01620'.                    
000096     03  FILLER             PIC X(8) VALUE '3R 01678'.                    
000097     03  FILLER             PIC X(8) VALUE '3R 01679'.                    
000098     03  FILLER             PIC X(8) VALUE '3S 02870'.                    
000099     03  FILLER             PIC X(8) VALUE '3S 02878'.                    
000100     03  FILLER             PIC X(8) VALUE '3S 02879'.                    
000101     03  FILLER             PIC X(8) VALUE '3T 02259'.                    
000102     03  FILLER             PIC X(8) VALUE '3T 02278'.                    
000103     03  FILLER             PIC X(8) VALUE '3T 02282'.                    
000104     03  FILLER             PIC X(8) VALUE '41 07574'.                    
000105     03  FILLER             PIC X(8) VALUE '41 07575'.                    
000106     03  FILLER             PIC X(8) VALUE '41 07625'.                    
000107     03  FILLER             PIC X(8) VALUE '41 07674'.                    
000108     03  FILLER             PIC X(8) VALUE '43 07574'.                    
000109     03  FILLER             PIC X(8) VALUE '43 07575'.                    
000110     03  FILLER             PIC X(8) VALUE '43 07625'.                    
000111     03  FILLER             PIC X(8) VALUE '43 07674'.                    
000112     03  FILLER             PIC X(8) VALUE '44 07574'.                    
000113     03  FILLER             PIC X(8) VALUE '44 07575'.                    
000114     03  FILLER             PIC X(8) VALUE '44 07625'.                    
000115     03  FILLER             PIC X(8) VALUE '44 07674'.                    
000116     03  FILLER             PIC X(8) VALUE '45 07574'.                    
000117     03  FILLER             PIC X(8) VALUE '45 07575'.                    
000118     03  FILLER             PIC X(8) VALUE '45 07625'.                    
000119     03  FILLER             PIC X(8) VALUE '45 07674'.                    
000120     03  FILLER             PIC X(8) VALUE '46 07574'.                    
000121     03  FILLER             PIC X(8) VALUE '46 07575'.                    
000122     03  FILLER             PIC X(8) VALUE '46 07625'.                    
000123     03  FILLER             PIC X(8) VALUE '46 07674'.                    
000124     03  FILLER             PIC X(8) VALUE '47 07574'.                    
000125     03  FILLER             PIC X(8) VALUE '47 07575'.                    
000126     03  FILLER             PIC X(8) VALUE '47 07625'.                    
000127     03  FILLER             PIC X(8) VALUE '47 07674'.                    
000128     03  FILLER             PIC X(8) VALUE '51 07674'.                    
000129     03  FILLER             PIC X(8) VALUE '52 07051'.                    
000130     03  FILLER             PIC X(8) VALUE '53 06591'.                    
000131     03  FILLER             PIC X(8) VALUE '6A 05222'.                    
000132     03  FILLER             PIC X(8) VALUE '6A 08363'.                    
000133     03  FILLER             PIC X(8) VALUE '61 05222'.                    
000134     03  FILLER             PIC X(8) VALUE '61 08263'.                    
000135     03  FILLER             PIC X(8) VALUE '62 07830'.                    
000136     03  FILLER             PIC X(8) VALUE '62 07838'.                    
000137     03  FILLER             PIC X(8) VALUE '62 07899'.                    
000138     03  FILLER             PIC X(8) VALUE '63 06225'.                    
000139     03  FILLER             PIC X(8) VALUE '63 06251'.                    
000140     03  FILLER             PIC X(8) VALUE '64 06200'.                    
000141     03  FILLER             PIC X(8) VALUE '64 06203'.                    
000142     03  FILLER             PIC X(8) VALUE '65 06124'.                    
000143     03  FILLER             PIC X(8) VALUE '66 05619'.                    
000144     03  FILLER             PIC X(8) VALUE '66 05627'.                    
000145     03  FILLER             PIC X(8) VALUE '67 06010'.                    
000146     03  FILLER             PIC X(8) VALUE '71 06271'.                    
000147     03  FILLER             PIC X(8) VALUE '72 06271'.                    
000148     03  FILLER             PIC X(8) VALUE '73 06271'.                    
000149     03  FILLER             PIC X(8) VALUE '74 06271'.                    
000150     03  FILLER             PIC X(8) VALUE '85 03162'.                    
000151     03  FILLER             PIC X(8) VALUE '86 05811'.                    
000152*                                                                         
000153 01  MICEXCP-DC-DIST-TAB REDEFINES MICEXCP-TABELL-VALUES.                 
000154     03  MICEXCP-DC-DIST OCCURS 133 TIMES                                 
000155                          ASCENDING KEY IS MIC-SOK-DC-DIST                
000156                          INDEXED BY MICEXCP-IX.                          
000157       05  MIC-SOK-DC-DIST     PIC X(8).                                  
000158*                                                                         
000159 01  MICEXCP-IDDISTR           PIC 9(4).                                  
000160     88  MICEXCP-DISTRICT                VALUE 0030                       
000161                                               0048                       
000162                                               0054 0056 0057             
000163                                               0061 0069                  
000164                                               0070 THRU 0074             
000165                                               0077                       
000166                                               0081 0082 0083             
000167                                               0090 0095 0096 0098        
000168                                               0099                       
000169                                               5811                       
000170                                               6080 6081                  
000171                                               6180 6181                  
000172                                               6280 6281                  
000173                                               8033 8034                  
000174                                               8063 8064                  
000175                                               8073 8074                  
000176                                               8083 8084                  
000177                                               8093 8094                  
000178                                               8111                       
000179                                               8200 THRU 8205             
000180                                               8211                       
000181                                               8263                       
000182                                               8270 THRU 8279             
000183                                               8311 8312                  
000184                                               8320 THRU 8329             
000185                                               8330 THRU 8339             
000186                                               8341 THRU 8349             
000187                                               8363                       
000188                                               8371 THRU 8379             
000189                                               8462 8463                  
000190                                               8471                       
000191                                               8480 THRU 8483             
000192                                               8490 8497                  
000193                                               8570 THRU 8579             
000194                                               8600 THRU 8613             
000195                                               8615 THRU 8620             
000196                                               8650                       
000197                                               8700 THRU 8704             
000198                                               8730 THRU 8739             
000199                                               8741 8745 8746             
000200                                               8751                       
000201                                               8780                       
000202                                               8790                       
000203                                               8871 THRU 8879             
000204                                               8880 8887 8888             
000205                                               8890 8897 8899             
000206                                               9920 THRU 9999.            
000207*                                                                         
000208 01  MICEXCP-KDSORT            PIC X(2).                                  
000209     88  MICEXCP-SW                      VALUE 'SW'.                      
000210*                                                                         
000211 01  MICEXCP-IDFKNGRP          PIC 9(5).                                  
000212     88  MICEXCP-ZERO                    VALUE ZERO.                      
000213*                                                                         
