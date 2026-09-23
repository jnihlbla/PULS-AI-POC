000100*** EDIT ALLOWED                                                          
006400 WY2000P5  SECTION.                                                       
006500                                                                          
006510*---  ADJUST TWO DATE FIELDS IN YY-P FORMAT                               
006511*---  (1/8TH YEAR PROGRAM PERIOD - 3 DIGITS)                              
006520*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006600                                                                          
006610     IF TMP1-YYP NOT = ZERO AND                                           
006620        TMP1-YYP < 999                                                    
006700       IF TMP1-YYP < 500                                                  
006800         ADD 500 TO TMP1-YYP                                              
006900       ELSE                                                               
006910         SUBTRACT 500 FROM TMP1-YYP                                       
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YYP NOT = ZERO AND                                           
007220        TMP2-YYP < 999                                                    
007300       IF TMP2-YYP < 500                                                  
007400         ADD 500 TO TMP2-YYP                                              
007500       ELSE                                                               
007600         SUBTRACT 500 FROM TMP2-YYP                                       
007610       END-IF                                                             
007620     END-IF                                                               
007700     .                                                                    
