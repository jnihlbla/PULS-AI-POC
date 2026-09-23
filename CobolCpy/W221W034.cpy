000100*** EDIT ALLOWED                                                          
000200 01  W221W034.                                                            
000300*             ***  FAKTOR ORDERKOSTNAD / LEVERANTÖR        ****           
000400*                                                                         
000500     03  T34-MAXLEV-IX           PIC S9(3)  COMP-3  VALUE +5.             
000600*             ***  MAXVÄRDE FÖR INDEX                      ****           
000700     03  VARDEN-TABW034.                                                  
000800*             ***                                                         
000900* AVSER:                             LEVNR*OKOST                          
001000****************************************************************          
001100      05  FILLER PIC X(10)   VALUE  '12345*0100'.                         
001200      05  FILLER PIC X(10)   VALUE  '12345*0100'.                         
001300      05  FILLER PIC X(10)   VALUE  '12345*0100'.                         
001400      05  FILLER PIC X(10)   VALUE  '12345*0100'.                         
001500      05  FILLER PIC X(10)   VALUE  '12345*0100'.                         
001600******************************************************************        
001700     03  TABW034   REDEFINES VARDEN-TABW034.                              
001800      04  FILLER   OCCURS 5.                                              
001900*        ***   INDEX                                                      
002000       05  T34-IDLEVNR           PIC X(5).                                
002100*                            ***                                          
002200       05  FILLER                PIC X.                                   
002300*                            ***                                          
002400       05  T34-REOKOST-LEV       DISPLAY PIC 9(2)V9(2).                   
002500*                            ***                                          
002600*** END COPY W221W034    LENGTH=50    OLD LENGTH=50                       
