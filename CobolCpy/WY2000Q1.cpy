000010*** EDIT ALLOWED                                                          
000100 WY2000Q1  SECTION.                                                       
000200                                                                          
000300*---  ADJUST FIVE DATE FIELDS IN YY-MM-DD FORMAT (6/7 DIGITS)             
000400*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
000500                                                                          
000510     IF TMP1-YYMMDD NOT = ZERO AND                                        
000520        TMP1-YYMMDD < 999999                                              
000600       IF TMP1-YYMMDD < 500000                                            
000700         ADD 500000 TO TMP1-YYMMDD                                        
000800       ELSE                                                               
000900         SUBTRACT 500000 FROM TMP1-YYMMDD                                 
001000       END-IF                                                             
001010     END-IF                                                               
001100                                                                          
001110     IF TMP2-YYMMDD NOT = ZERO AND                                        
001120        TMP2-YYMMDD < 999999                                              
001200       IF TMP2-YYMMDD < 500000                                            
001300         ADD 500000 TO TMP2-YYMMDD                                        
001400       ELSE                                                               
001500         SUBTRACT 500000 FROM TMP2-YYMMDD                                 
001600       END-IF                                                             
001610     END-IF                                                               
001700                                                                          
001710     IF TMP3-YYMMDD NOT = ZERO AND                                        
001720        TMP3-YYMMDD < 999999                                              
001800       IF TMP3-YYMMDD < 500000                                            
001900         ADD 500000 TO TMP3-YYMMDD                                        
002000       ELSE                                                               
002100         SUBTRACT 500000 FROM TMP3-YYMMDD                                 
002200       END-IF                                                             
002201     END-IF                                                               
002210                                                                          
002211     IF TMP4-YYMMDD NOT = ZERO AND                                        
002212        TMP4-YYMMDD < 999999                                              
002220       IF TMP4-YYMMDD < 500000                                            
002230         ADD 500000 TO TMP4-YYMMDD                                        
002240       ELSE                                                               
002250         SUBTRACT 500000 FROM TMP4-YYMMDD                                 
002260       END-IF                                                             
002270     END-IF                                                               
002280                                                                          
002290     IF TMP5-YYMMDD NOT = ZERO AND                                        
002291        TMP5-YYMMDD < 999999                                              
002292       IF TMP5-YYMMDD < 500000                                            
002293         ADD 500000 TO TMP5-YYMMDD                                        
002294       ELSE                                                               
002295         SUBTRACT 500000 FROM TMP5-YYMMDD                                 
002296       END-IF                                                             
002297     END-IF                                                               
002300     .                                                                    
