000100*** EDIT ALLOWED                                                          
006400 WY2000Q5  SECTION.                                                       
006500                                                                          
006510*---  ADJUST FOUR DATE FIELDS IN YY-P FORMAT                              
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
007611     END-IF                                                               
007620                                                                          
007621     IF TMP3-YYP NOT = ZERO AND                                           
007622        TMP3-YYP < 999                                                    
007630       IF TMP3-YYP < 500                                                  
007640         ADD 500 TO TMP3-YYP                                              
007650       ELSE                                                               
007660         SUBTRACT 500 FROM TMP3-YYP                                       
007670       END-IF                                                             
007671     END-IF                                                               
007680                                                                          
007681     IF TMP4-YYP NOT = ZERO AND                                           
007682        TMP4-YYP < 999                                                    
007690       IF TMP4-YYP < 500                                                  
007691         ADD 500 TO TMP4-YYP                                              
007692       ELSE                                                               
007693         SUBTRACT 500 FROM TMP4-YYP                                       
007694       END-IF                                                             
007695     END-IF                                                               
007700     .                                                                    
