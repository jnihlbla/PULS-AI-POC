000100*** EDIT ALLOWED                                                          
006400 WY2000Q2  SECTION.                                                       
006500                                                                          
006510*---  ADJUST FOUR DATE FIELDS IN YY-WW-D FORMAT (5 DIGITS),               
006520*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
006600                                                                          
006610     IF TMP1-YYWWD  NOT = ZERO AND                                        
006620        TMP1-YYWWD  < 99999                                               
006700       IF TMP1-YYWWD < 50000                                              
006800         ADD 50000 TO TMP1-YYWWD                                          
006900       ELSE                                                               
006910         SUBTRACT 50000 FROM TMP1-YYWWD                                   
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YYWWD  NOT = ZERO AND                                        
007220        TMP2-YYWWD  < 99999                                               
007300       IF TMP2-YYWWD < 50000                                              
007400         ADD 50000 TO TMP2-YYWWD                                          
007500       ELSE                                                               
007600         SUBTRACT 50000 FROM TMP2-YYWWD                                   
007610       END-IF                                                             
007611     END-IF                                                               
007612                                                                          
007613     IF TMP3-YYWWD  NOT = ZERO AND                                        
007614        TMP3-YYWWD  < 99999                                               
007620       IF TMP3-YYWWD < 50000                                              
007630         ADD 50000 TO TMP3-YYWWD                                          
007640       ELSE                                                               
007650         SUBTRACT 50000 FROM TMP3-YYWWD                                   
007660       END-IF                                                             
007661     END-IF                                                               
007662                                                                          
007663     IF TMP4-YYWWD  NOT = ZERO AND                                        
007664        TMP4-YYWWD  < 99999                                               
007665       IF TMP4-YYWWD < 50000                                              
007666         ADD 50000 TO TMP4-YYWWD                                          
007667       ELSE                                                               
007668         SUBTRACT 50000 FROM TMP4-YYWWD                                   
007669       END-IF                                                             
007670     END-IF                                                               
007700     .                                                                    
