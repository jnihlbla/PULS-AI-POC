000100*** EDIT ALLOWED                                                          
000200*---  COMPUTE LIMITS USED WHEN PROCESSING DATES IN Y-WW FORMAT.           
000300*---  THE LIMITS COMPUTED BELOW ARE IN THE FORMAT 'Y00', WHERE            
000310*---  'Y' IS COMPUTED FROM THE LAST DIGIT OF CURRENT YEAR.                
000400*---  DATES ON DIFFERENT SIDES OF YWW-LIMIT ARE REGARDED AS               
000500*---  BELONGING TO DIFFERENT DECADES. YWW-LIMIT2 IS THE                   
000510*---  1000-COMPLEMENT OF YWW-LIMIT.                                       
000600                                                                          
000700     MOVE FUNCTION CURRENT-DATE (4:1) TO YWW-LIMIT                        
000810     IF YWW-LIMIT < 5                                                     
000900       ADD 5 TO YWW-LIMIT                                                 
001000     ELSE                                                                 
001100       SUBTRACT 5 FROM YWW-LIMIT                                          
001200     END-IF                                                               
001210     MULTIPLY 100 BY YWW-LIMIT                                            
001300     COMPUTE YWW-LIMIT2 = 1000 - YWW-LIMIT                                
