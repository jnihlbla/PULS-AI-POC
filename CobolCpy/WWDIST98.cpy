001000*** EDIT ALLOWED                                                          
010000*                            *************************************        
020000*                            *** ANVÄNDS VID TEST AV:                     
030037*                            ***  - LDC-DISTRIKT SOM INTE SKALL           
040037*                            ***    SKICKA RIX/RIY SAMT                   
050037*                            ***    RIT-TRANSAR TILL VIPS                 
070000*                            *************************************        
080037 02  DIST98-IDDISTR          PIC 9(5)     COMP-3.                         
090000**                                                                        
101039     88  DIST98-EJ-ONORDER   VALUE  1090 1258 1378                        
101040                                    1478 1678 1822                        
101040                                    2078 2278 2378.                       
140000**                                                                        
150037*** END COPY WWDIST98    LENGTH=3                                         
