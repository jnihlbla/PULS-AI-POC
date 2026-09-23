000010*** EDIT ALLOWED                                                          
000100 01  PROGFAKT.                                                            
000200*                                 TABELL PROGNOSFAKTORER S-LAGER.         
000300*                                 ANVÄNDS VID BERÄKNING AV NY             
000400*                                 PROGNOS. RAD MOTSVARAR PERIOD.          
000500*                                                                         
000600     03 S1-PROGFAKT.                                                      
000700        05 FILLER PIC X(09) VALUE '035040045'.                            
000800        05 FILLER PIC X(09) VALUE '025025035'.                            
000900        05 FILLER PIC X(09) VALUE '020015010'.                            
001000        05 FILLER PIC X(09) VALUE '010010005'.                            
001100        05 FILLER PIC X(09) VALUE '006006003'.                            
001200        05 FILLER PIC X(09) VALUE '004004002'.                            
001300        05 FILLER PIC X(09) VALUE '000000000'.                            
001400        05 FILLER PIC X(09) VALUE '000000000'.                            
001500        05 FILLER PIC X(09) VALUE '000000000'.                            
001600        05 FILLER PIC X(09) VALUE '000000000'.                            
001700        05 FILLER PIC X(09) VALUE '000000000'.                            
001800        05 FILLER PIC X(09) VALUE '000000000'.                            
001900                                                                          
002000     03 S2-PROGFAKT.                                                      
002100        05 FILLER PIC X(09) VALUE '035040045'.                            
002200        05 FILLER PIC X(09) VALUE '025025035'.                            
002300        05 FILLER PIC X(09) VALUE '020015010'.                            
002400        05 FILLER PIC X(09) VALUE '010010005'.                            
002500        05 FILLER PIC X(09) VALUE '006006003'.                            
002600        05 FILLER PIC X(09) VALUE '004004002'.                            
002700        05 FILLER PIC X(09) VALUE '000000000'.                            
002800        05 FILLER PIC X(09) VALUE '000000000'.                            
002900        05 FILLER PIC X(09) VALUE '000000000'.                            
003000        05 FILLER PIC X(09) VALUE '000000000'.                            
003100        05 FILLER PIC X(09) VALUE '000000000'.                            
003200        05 FILLER PIC X(09) VALUE '000000000'.                            
003300                                                                          
003400                                                                          
003500 01  FILLER REDEFINES PROGFAKT.                                           
003600     03 S-PROGFAKT       OCCURS 2.                                        
003700        05 S-PERIOD          OCCURS 12.                                   
003800           07 S-PERIODTREND.                                              
003900               09 S-NORMAL       PIC 9V9(2).                              
004000               09 S-SVAG-TREND   PIC 9V9(2).                              
004100               09 S-STARK-TREND  PIC 9V9(2).                              
004200           07 FILLER REDEFINES S-PERIODTREND.                             
004300               09 S-TRENDFAKT   PIC 9V9(2) OCCURS 3.                      
004400                                                                          
004500*** END COPY PROGFAKT    LENGTH=216                                       
