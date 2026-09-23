000100*** EDIT ALLOWED                                                          
006400 WY2000QA  SECTION.                                                       
006600                                                                          
006610*---  ADJUST TWO DATE FIELDS IN Y-WW FORMAT (NOTE 1-DIGIT YEAR)           
006620*---  SO DATES BETWEEN CURRENT YEAR +-5 COME IN ASCENDING ORDER           
006621*---  THE LIMITS USED BELOW, ARE IN THE FORMAT 'Y00', WHERE 'Y' IS        
006622*---  THE LAST DIGIT OF A YEAR.                                           
006623*---  DATES ON DIFFERENT SIDES OF YWW-LIMIT ARE REGARDED AS               
006624*---  BELONGING TO DIFFERENT DECADES. YWW-LIMIT2 IS THE                   
006625*---  1000-COMPLEMENT OF YWW-LIMIT                                        
006630                                                                          
006640     IF TMP1-YWW NOT = ZERO AND < 999                                     
006700       IF TMP1-YWW < YWW-LIMIT                                            
006800         ADD YWW-LIMIT2 TO TMP1-YWW                                       
006900       ELSE                                                               
006910         SUBTRACT YWW-LIMIT FROM TMP1-YWW                                 
007100       END-IF                                                             
007110     END-IF                                                               
007200                                                                          
007210     IF TMP2-YWW NOT = ZERO AND < 999                                     
007300       IF TMP2-YWW < YWW-LIMIT                                            
007400         ADD YWW-LIMIT2 TO TMP2-YWW                                       
007500       ELSE                                                               
007600         SUBTRACT YWW-LIMIT FROM TMP2-YWW                                 
007610       END-IF                                                             
007611     END-IF                                                               
007620                                                                          
007621     IF TMP3-YWW NOT = ZERO AND < 999                                     
007630       IF TMP3-YWW < YWW-LIMIT                                            
007640         ADD YWW-LIMIT2 TO TMP3-YWW                                       
007650       ELSE                                                               
007660         SUBTRACT YWW-LIMIT FROM TMP3-YWW                                 
007670       END-IF                                                             
007671     END-IF                                                               
007680                                                                          
007681     IF TMP4-YWW NOT = ZERO AND < 999                                     
007690       IF TMP4-YWW < YWW-LIMIT                                            
007691         ADD YWW-LIMIT2 TO TMP4-YWW                                       
007692       ELSE                                                               
007693         SUBTRACT YWW-LIMIT FROM TMP4-YWW                                 
007694       END-IF                                                             
007695     END-IF                                                               
007700     .                                                                    
