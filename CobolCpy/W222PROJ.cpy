000010*** EDIT ALLOWED                                                          
000100 01  W222PROG.                                                            
000200*                                 TABELL PROGNOSFAKTORER CDC.             
000300*                                 ANVÄNDS VID BERÄKNING AV NY             
000400*                                 PROGNOS. RAD MOTSVARAR PERIOD.          
000500*                                                                         
000700        05 FILLER PIC X(09) VALUE '030030030'.                            
000800        05 FILLER PIC X(09) VALUE '021021021'.                            
001000        05 FILLER PIC X(09) VALUE '015015015'.                            
001100        05 FILLER PIC X(09) VALUE '010010010'.                            
001200        05 FILLER PIC X(09) VALUE '007007007'.                            
001300        05 FILLER PIC X(09) VALUE '005005005'.                            
001400        05 FILLER PIC X(09) VALUE '004004004'.                            
001500        05 FILLER PIC X(09) VALUE '002002002'.                            
001600        05 FILLER PIC X(09) VALUE '002002002'.                            
001610        05 FILLER PIC X(09) VALUE '002002002'.                            
001700        05 FILLER PIC X(09) VALUE '001001001'.                            
001800        05 FILLER PIC X(09) VALUE '001001001'.                            
003300                                                                          
013209                                                                          
013210 01  FILLER REDEFINES W222PROG.                                           
013400        05 PERIOD            OCCURS 12.                                   
013500           07 PERIODTREND.                                                
013600               09 NORMAL          PIC 9V9(2).                             
013700               09 SVAG-TREND      PIC 9V9(2).                             
013800               09 STARK-TREND     PIC 9V9(2).                             
013900           07 FILLER REDEFINES PERIODTREND.                               
014000               09 TRENDFAKT      PIC 9V9(2) OCCURS 3.                     
014100                                                                          
