000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000210*                            ***                                          
000220*                            ***  TULLISTOR FÖR:                          
000300*                            ***  - HOLLÄNDSKA   DISTRIKT                 
000320*                            ***  - FRANSKA      DISTRIKT                 
000330*                            ***  - ITALIENSKA   DISTRIKT                 
000340*                            ***  - ENGELSKA     DISTRIKT                 
000350*                            ***  - SPANSKA      DISTRIKT                 
000360*                            ***  - ÖSTERRIKISKA DISTRIKT                 
000400*                            ***                                          
000500*                            *************************************        
000600                                                                          
000700 01  DIST01-IDDISTR          PIC 9(5)     COMP-3.                         
000800*                                                                         
000900       88  DIST01-HOLLAND-TULL      VALUE  1620 1678.                     
000951*                                                                         
000960       88  DIST01-FRANCE-TULL       VALUE  1470 1478.                     
000961*                                                                         
000962       88  DIST01-ITALIEN-TULL      VALUE  1820 1821 1822                 
000963                                           1870 1871.                     
000964*                                                                         
000965       88  DIST01-SCHWEIZ-TULL      VALUE  2078.                          
000966*                                                                         
000967       88  DIST01-ENGLAND-TULL      VALUE  1312 1378 1778.                
000970*                                                                         
000980       88  DIST01-SPANIEN-TULL      VALUE  2170 2178.                     
000981*                                                                         
000982       88  DIST01-AUSTRIA-TULL      VALUE  2370 THRU 2378.                
000990*                                                                         
000991       88  DIST01-POLEN-TULL        VALUE  2870 2878.                     
000992*                                                                         
001000*** END COPY WWDIST01    LENGTH=3                                         
