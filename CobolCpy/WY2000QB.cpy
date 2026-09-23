000010*** EDIT ALLOWED                                                          
000100 WY2000QB  SECTION.                                                       
000200                                                                          
000300*---  ADJUST 4 DATE FIELDS IN YY-MM-DD-HH-MM FORMAT (10/11 DIG.)          
000400*---  SO DATES BETWEEN 1950-2049 COME IN ASCENDING SEQUENCE.              
000500*---  VALUE ZERO IS CONSIDERED A NON-DATE AND NOT ADJUSTED.               
000600                                                                          
000700     IF TMP1-YYMMDDHHMM NOT = ZERO   AND                                  
000710        TMP1-YYMMDDHHMM < 9999999999                                      
000800       IF TMP1-YYMMDDHHMM < 5000000000                                    
000900         ADD 5000000000 TO TMP1-YYMMDDHHMM                                
001000       ELSE                                                               
001100         SUBTRACT 5000000000 FROM TMP1-YYMMDDHHMM                         
001200       END-IF                                                             
001300     END-IF                                                               
001400                                                                          
001500     IF TMP2-YYMMDDHHMM NOT = ZERO   AND                                  
001510        TMP2-YYMMDDHHMM < 9999999999                                      
001600       IF TMP2-YYMMDDHHMM < 5000000000                                    
001700         ADD 5000000000 TO TMP2-YYMMDDHHMM                                
001800       ELSE                                                               
001900         SUBTRACT 5000000000 FROM TMP2-YYMMDDHHMM                         
002000       END-IF                                                             
002100     END-IF                                                               
002110                                                                          
002120     IF TMP3-YYMMDDHHMM NOT = ZERO   AND                                  
002121        TMP3-YYMMDDHHMM < 9999999999                                      
002130       IF TMP3-YYMMDDHHMM < 5000000000                                    
002140         ADD 5000000000 TO TMP3-YYMMDDHHMM                                
002150       ELSE                                                               
002160         SUBTRACT 5000000000 FROM TMP3-YYMMDDHHMM                         
002170       END-IF                                                             
002180     END-IF                                                               
002190                                                                          
002191     IF TMP4-YYMMDDHHMM NOT = ZERO   AND                                  
002192        TMP4-YYMMDDHHMM < 9999999999                                      
002193       IF TMP4-YYMMDDHHMM < 5000000000                                    
002194         ADD 5000000000 TO TMP4-YYMMDDHHMM                                
002195       ELSE                                                               
002196         SUBTRACT 5000000000 FROM TMP4-YYMMDDHHMM                         
002197       END-IF                                                             
002198     END-IF                                                               
002200     .                                                                    
