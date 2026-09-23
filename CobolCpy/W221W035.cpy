000100*** EDIT ALLOWED                                                          
000200 01  W221W035.                                                            
000300*             ***  FAKTOR ORDERKOSTNAD / FÖRPACKNINGSSÄTT  ****           
000400*                                                                         
000500     03  T35-MAXBEFT-IX          PIC S9(3)  COMP-3  VALUE +5.             
000600*             ***  MAXVÄRDE FÖR INDEX                      ****           
000700     03  VARDEN-TABW035.                                                  
000800*             ***                                                         
000900* AVSER:                            BEFT*OKOST                            
001000****************************************************************          
001100      05  FILLER PIC X(8)    VALUE  '123*0100'.                           
001200      05  FILLER PIC X(8)    VALUE  '123*0100'.                           
001300      05  FILLER PIC X(8)    VALUE  '123*0100'.                           
001400      05  FILLER PIC X(8)    VALUE  '123*0100'.                           
001500      05  FILLER PIC X(8)    VALUE  '123*0100'.                           
001600******************************************************************        
001700     03  TABW035   REDEFINES VARDEN-TABW035.                              
001800      04  FILLER   OCCURS 5.                                              
001900*        ***   INDEX                                                      
002000       05  T35-BEFT              PIC 9(3).                                
002100*                            ***                                          
002200       05  FILLER                PIC X.                                   
002300*                            ***                                          
002400       05  T35-REOKOST-BEFT      DISPLAY PIC 9(2)V9(2).                   
002500*                            ***                                          
002600*** END COPY W221W035    LENGTH=40    OLD LENGTH=40                       
