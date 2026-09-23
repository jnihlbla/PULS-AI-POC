000010*** EDIT ALLOWED                                                          
000100*     SENASTE UPPDATERING         75036       8.22.44.3                   
000200*                            *** POSTLÄNGD 80               ***           
000300 01  M109.                                                                
000400*    *** R31 R32 R33 R34  ARTIKEL MED VTH = 2               ***           
000500     03  FILLER          PIC X(9)    VALUE '   LÖPNR '.                   
000600     03  IDLOPNRM        PIC 9(8)    DISPLAY.                             
000700     03  FILLER          PIC X(6)    VALUE '  LEV '.                      
000800     03  IDLEVNR         PIC X(5).                                        
000900     03  FILLER          PIC X(6)    VALUE '  ANT '.                      
001000     03   KVAVIS         PIC +++++++   DISPLAY.                           
001100     03  FILLER          PIC X(5)    VALUE '  RT '.                       
001200     03  KDRT            PIC 99      DISPLAY.                             
001300     03  FILLER          PIC X(6)    VALUE '  AVI '.                      
001400     03  IDAVINR         PIC 9(7)    DISPLAY.                             
001500     03  FILLER          PIC X(8)    VALUE '  DATUM'.                     
001600     03  TIAVSDAT        PIC 9(6)    DISPLAY.                             
001700     03  FILLER          PIC X(7)    VALUE SPACE.                         
001800*** END COPY W211M109   LENGTH=0                                          
