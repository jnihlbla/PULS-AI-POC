000010*** EDIT ALLOWED                                                          
000100 S51-Y2K-TIOMBRYT   SECTION .                                             
000200     SKIP2                                                                
031610**********  KONVERTERA FÖR ÅR 2000                 ************           
031700     IF W-TIOMBRYT-6 (1:2) > 50                                           
031701**********                 ÅR 1990-1999            ************           
031711       MOVE W-TIOMBRYT-6     TO W-TIOMBRYT-7                              
031713       MOVE ZERO             TO W-TIOMBRYT-7 (1:1)                        
031714     ELSE                                                                 
031715**********                 ÅR 2000+                ************           
031716       MOVE W-TIOMBRYT-6     TO W-TIOMBRYT-7                              
031717       MOVE 1                TO W-TIOMBRYT-7 (1:1)                        
031722     END-IF                                                               
032100     .                                                                    
