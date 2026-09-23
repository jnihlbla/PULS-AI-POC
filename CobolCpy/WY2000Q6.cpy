000100*** EDIT ALLOWED                                                          
006400 WY2000Q6  SECTION.                                                       
006500                                                                          
006510*---  ADJUST FOUR DATE FIELDS IN YY-PP FORMAT                             
006511*---  (1/12TH YEAR PROGRAM PERIOD - 4/5 DIGITS)                           
006520*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006600                                                                          
006610     IF TMP1-YYPP NOT = ZERO AND                                          
006620        TMP1-YYPP < 9999                                                  
006700       IF TMP1-YYPP < 5000                                                
006800         ADD 5000 TO TMP1-YYPP                                            
006900       ELSE                                                               
006910         SUBTRACT 5000 FROM TMP1-YYPP                                     
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YYPP NOT = ZERO AND                                          
007220        TMP2-YYPP < 9999                                                  
007300       IF TMP2-YYPP < 5000                                                
007400         ADD 5000 TO TMP2-YYPP                                            
007500       ELSE                                                               
007600         SUBTRACT 5000 FROM TMP2-YYPP                                     
007610       END-IF                                                             
007620     END-IF                                                               
007630                                                                          
007640     IF TMP3-YYPP NOT = ZERO AND                                          
007650        TMP3-YYPP < 9999                                                  
007660       IF TMP3-YYPP < 5000                                                
007670         ADD 5000 TO TMP3-YYPP                                            
007680       ELSE                                                               
007690         SUBTRACT 5000 FROM TMP3-YYPP                                     
007691       END-IF                                                             
007692     END-IF                                                               
007693                                                                          
007694     IF TMP4-YYPP NOT = ZERO AND                                          
007695        TMP4-YYPP < 9999                                                  
007696       IF TMP4-YYPP < 5000                                                
007697         ADD 5000 TO TMP4-YYPP                                            
007698       ELSE                                                               
007699         SUBTRACT 5000 FROM TMP4-YYPP                                     
007700       END-IF                                                             
007710     END-IF                                                               
007800     .                                                                    
