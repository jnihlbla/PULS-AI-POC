000100*** EDIT ALLOWED                                                          
006400 WY2000P7  SECTION.                                                       
006500                                                                          
006510*---  ADJUST TWO DATE FIELDS IN YY-RP FORMAT                              
006511*---  (1/12TH YEAR ACCOUNTING PERIOD - 4/5 DIGITS)                        
006520*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006600                                                                          
006700     IF TMP1-YYRP NOT = ZERO AND                                          
006701        TMP1-YYRP < 9999                                                  
006710       IF TMP1-YYRP < 5000                                                
006800         ADD 5000 TO TMP1-YYRP                                            
006900       ELSE                                                               
006910         SUBTRACT 5000 FROM TMP1-YYRP                                     
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YYRP NOT = ZERO AND                                          
007220        TMP2-YYRP < 9999                                                  
007300       IF TMP2-YYRP < 5000                                                
007400         ADD 5000 TO TMP2-YYRP                                            
007500       ELSE                                                               
007600         SUBTRACT 5000 FROM TMP2-YYRP                                     
007610       END-IF                                                             
007620     END-IF                                                               
007700     .                                                                    
