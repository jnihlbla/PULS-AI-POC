000010*** EDIT ALLOWED                                                          
000100 WY2000PC  SECTION.                                                       
000200                                                                          
000300*---  ADJUST TWO DATE FIELDS IN YY-MM-DD (7 DIGITS) PLUS                  
000310*---  HH-MM (5 DIGITS) (DEFINED AS A GROUP WITH 2 PACKED FIELDS)          
000400*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
000410*---  ONLY THE YY-MM-DD PART NEEDS TO BE ADJUSTED HERE.                   
000500*---  VALUE ZERO IS CONSIDERED A NON-DATE AND IS NOT ADJUSTED.            
000600                                                                          
000700     IF TMP1-C-YYMMDD NOT = ZERO AND                                      
000710        TMP1-C-YYMMDD < 999999                                            
000800       IF TMP1-C-YYMMDD < 500000                                          
000900         ADD 500000 TO TMP1-C-YYMMDD                                      
001000       ELSE                                                               
001100         SUBTRACT 500000 FROM TMP1-C-YYMMDD                               
001200       END-IF                                                             
001300     END-IF                                                               
001400                                                                          
001500     IF TMP2-C-YYMMDD NOT = ZERO AND                                      
001510        TMP2-C-YYMMDD < 999999                                            
001600       IF TMP2-C-YYMMDD < 500000                                          
001700         ADD 500000 TO TMP2-C-YYMMDD                                      
001800       ELSE                                                               
001900         SUBTRACT 500000 FROM TMP2-C-YYMMDD                               
002000       END-IF                                                             
002100     END-IF                                                               
002200     .                                                                    
