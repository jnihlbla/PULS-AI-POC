000100*** EDIT ALLOWED                                                          
006400 WY2000P9  SECTION.                                                       
006500                                                                          
006510*---  ADJUST TWO DATE FIELDS IN 2-DIGIT YEAR FORMAT                       
006520*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006600                                                                          
006700     IF TMP1-YY < 50                                                      
006800       ADD 50 TO TMP1-YY                                                  
006900     ELSE                                                                 
006910       SUBTRACT 50 FROM TMP1-YY                                           
007100     END-IF                                                               
007200                                                                          
007300     IF TMP2-YY < 50                                                      
007400       ADD 50 TO TMP2-YY                                                  
007500     ELSE                                                                 
007600       SUBTRACT 50 FROM TMP2-YY                                           
007610     END-IF                                                               
007700     .                                                                    
