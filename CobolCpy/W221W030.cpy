000100*** EDIT ALLOWED                                                          
000200 01  W221W030.                                                            
000300*             ***  VARIANS I LEDTID                        ****           
000400*                                                                         
000500     03  T30-MAXLEV-IX           PIC S9(3)  COMP-3  VALUE +5.             
000600*             ***  MAXVÄRDE FÖR INDEX                      ****           
000700     03  VARDEN-TABW030.                                                  
000800*             ***                                                         
000900* AVSER:                             LEVNR*LVAR                           
001000****************************************************************          
001100      05  FILLER PIC X(9)    VALUE  '12345*000'.                          
001200      05  FILLER PIC X(9)    VALUE  '12345*000'.                          
001300      05  FILLER PIC X(9)    VALUE  '12345*000'.                          
001400      05  FILLER PIC X(9)    VALUE  '12345*000'.                          
001500      05  FILLER PIC X(9)    VALUE  '12345*000'.                          
001600******************************************************************        
001700     03  TABW030   REDEFINES VARDEN-TABW030.                              
001800      04  FILLER   OCCURS 5.                                              
001900*        ***   INDEX                                                      
002000       05  T30-IDLEVNR           PIC X(5).                                
002100*                            ***                                          
002200       05  FILLER                PIC X.                                   
002300*                            ***                                          
002400       05  T30-KVVECKOR-LVAR     DISPLAY PIC 9(2)V9.                      
002500*                            ***                                          
002600*** END COPY W221W030    LENGTH=45    OLD LENGTH=45                       
