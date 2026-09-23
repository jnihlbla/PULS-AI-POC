000100*** EDIT ALLOWED                                                          
000200 01  W221W036.                                                            
000300*             ***  FAKTOR ORDERKOSTNAD / LAGEROMRÅDE       ****           
000300*             ***  FAKTOR VOLYMKOSTNAD / LAGEROMRÅDE       ****           
000400*                                                                         
000500     03  T36-MAXADLAGOMR-IX      PIC S9(3)  COMP-3  VALUE +5.             
000600*             ***  MAXVÄRDE FÖR INDEX                      ****           
000700     03  VARDEN-TABW036.                                                  
000800*             ***                                                         
000900* AVSER:                             OMR*ORDK*VOLK                        
001000****************************************************************          
001100      05  FILLER PIC X(13)   VALUE  '123*0100*0100'.                      
001200      05  FILLER PIC X(13)   VALUE  '123*0100*0100'.                      
001300      05  FILLER PIC X(13)   VALUE  '123*0100*0100'.                      
001400      05  FILLER PIC X(13)   VALUE  '123*0100*0100'.                      
001500      05  FILLER PIC X(13)   VALUE  '123*0100*0100'.                      
001600******************************************************************        
001700     03  TABW036   REDEFINES VARDEN-TABW036.                              
001800      04  FILLER   OCCURS 5.                                              
001900*        ***   INDEX                                                      
002000       05  T36-ADLAGOMR          PIC 9(3).                                
002100*                            ***                                          
002200       05  FILLER                PIC X.                                   
002300*                            ***                                          
002400       05  T36-REOKOST-OMR       DISPLAY PIC 9(2)V9(2).                   
002500*                            ***                                          
002200       05  FILLER                PIC X.                                   
002300*                            ***                                          
002400       05  T36-REVKOST-OMR       DISPLAY PIC 9(2)V9(2).                   
002500*                            ***                                          
002600*** END COPY W221W036    LENGTH=65    OLD LENGTH=65                       
