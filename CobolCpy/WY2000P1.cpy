000100*** EDIT ALLOWED                                                          
006400 WY2000P1  SECTION.                                                       
006600                                                                          
006610*---  ADJUST TWO DATE FIELDS IN YY-MM-DD FORMAT (6/7 DIGITS)              
006620*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006621*---  VALUE ZERO IS CONSIDERED A NON-DATE AND NOT ADJUSTED.               
006630                                                                          
006700     IF TMP1-YYMMDD NOT = ZERO AND                                        
006701        TMP1-YYMMDD < 999999                                              
006710       IF TMP1-YYMMDD < 500000                                            
006800         ADD 500000 TO TMP1-YYMMDD                                        
006900       ELSE                                                               
006910         SUBTRACT 500000 FROM TMP1-YYMMDD                                 
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YYMMDD NOT = ZERO AND                                        
007220        TMP2-YYMMDD < 999999                                              
007300       IF TMP2-YYMMDD < 500000                                            
007400         ADD 500000 TO TMP2-YYMMDD                                        
007500       ELSE                                                               
007600         SUBTRACT 500000 FROM TMP2-YYMMDD                                 
007610       END-IF                                                             
007620     END-IF                                                               
007700     .                                                                    
