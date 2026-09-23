000100*** EDIT ALLOWED                                                          
006400 WY2000Q4  SECTION.                                                       
006500                                                                          
006510*---  ADJUST FOUR DATE FIELDS IN YY-DDD FORMAT (5 DIGITS),                
006520*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006600                                                                          
006610     IF TMP1-YYDDD NOT = ZERO AND                                         
006620        TMP1-YYDDD < 99999                                                
006700       IF TMP1-YYDDD < 50000                                              
006800         ADD 50000 TO TMP1-YYDDD                                          
006900       ELSE                                                               
006910         SUBTRACT 50000 FROM TMP1-YYDDD                                   
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YYDDD NOT = ZERO AND                                         
007220        TMP2-YYDDD < 99999                                                
007300       IF TMP2-YYDDD < 50000                                              
007400         ADD 50000 TO TMP2-YYDDD                                          
007500       ELSE                                                               
007600         SUBTRACT 50000 FROM TMP2-YYDDD                                   
007610       END-IF                                                             
007611     END-IF                                                               
007620                                                                          
007621     IF TMP3-YYDDD NOT = ZERO AND                                         
007622        TMP3-YYDDD < 99999                                                
007630       IF TMP3-YYDDD < 50000                                              
007640         ADD 50000 TO TMP3-YYDDD                                          
007650       ELSE                                                               
007660         SUBTRACT 50000 FROM TMP3-YYDDD                                   
007670       END-IF                                                             
007671     END-IF                                                               
007680                                                                          
007681     IF TMP4-YYDDD NOT = ZERO AND                                         
007682        TMP4-YYDDD < 99999                                                
007690       IF TMP4-YYDDD < 50000                                              
007691         ADD 50000 TO TMP4-YYDDD                                          
007692       ELSE                                                               
007693         SUBTRACT 50000 FROM TMP4-YYDDD                                   
007694       END-IF                                                             
007695     END-IF                                                               
007700     .                                                                    
