000100*** EDIT ALLOWED                                                          
006400 WY2000Q3  SECTION.                                                       
006500                                                                          
006510*---  ADJUST FOUR DATE FIELDS IN YY-WW  FORMAT (4/5 DIGITS)               
006520*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006600                                                                          
006610     IF TMP1-YYWW NOT = ZERO AND                                          
006620        TMP1-YYWW < 9999                                                  
006700       IF TMP1-YYWW < 5000                                                
006800         ADD 5000 TO TMP1-YYWW                                            
006900       ELSE                                                               
006910         SUBTRACT 5000 FROM TMP1-YYWW                                     
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YYWW NOT = ZERO AND                                          
007220        TMP2-YYWW < 9999                                                  
007300       IF TMP2-YYWW < 5000                                                
007400         ADD 5000 TO TMP2-YYWW                                            
007500       ELSE                                                               
007600         SUBTRACT 5000 FROM TMP2-YYWW                                     
007610       END-IF                                                             
007611     END-IF                                                               
007620                                                                          
007621     IF TMP3-YYWW NOT = ZERO AND                                          
007622        TMP3-YYWW < 9999                                                  
007630       IF TMP3-YYWW < 5000                                                
007640         ADD 5000 TO TMP3-YYWW                                            
007650       ELSE                                                               
007660         SUBTRACT 5000 FROM TMP3-YYWW                                     
007670       END-IF                                                             
007671     END-IF                                                               
007680                                                                          
007681     IF TMP4-YYWW NOT = ZERO AND                                          
007682        TMP4-YYWW < 9999                                                  
007690       IF TMP4-YYWW < 5000                                                
007691         ADD 5000 TO TMP4-YYWW                                            
007692       ELSE                                                               
007693         SUBTRACT 5000 FROM TMP4-YYWW                                     
007694       END-IF                                                             
007695     END-IF                                                               
007700     .                                                                    
