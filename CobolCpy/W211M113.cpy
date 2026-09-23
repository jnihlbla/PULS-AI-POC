000010*** EDIT ALLOWED                                                          
000100*                            *** POSTLÄNGD 80               ***           
000200 01  M113.                                                                
000300*    ***  R31-R34  INLEVERANS AV ERSATT ARTIKEL             ***           
000400     03  FILLER          PIC X(9)    VALUE '   LÖPNR '.                   
000500     03  IDLOPNRM        PIC 9(8)    DISPLAY.                             
000600     03  FILLER          PIC X(5)    VALUE '  AVI'.                       
000700     03  IDAVINR         PIC Z9(6)   DISPLAY.                             
000800     03  FILLER          PIC X(5)    VALUE '  LEV'.                       
000900     03  IDLEVNR         PIC X(5).                                        
001000     03  FILLER          PIC X(5)    VALUE '  RT '.                       
001100     03  KDRT            PIC 99      DISPLAY.                             
001200     03  FILLER          PIC X(5)    VALUE '  ANT'.                       
001300     03  KVANTAL         PIC S9(7)   DISPLAY.                             
001400     03   FILLER         PIC X(5)    VALUE '  EK '.                       
001500     03  KDERS           PIC 99      DISPLAY.                             
001600     03  FILLER          PIC X(6)    VALUE '  LTK '.                      
001700     03  KDLTK           PIC 9       DISPLAY.                             
001800     03  FILLER          PIC X(6)    VALUE ' ANSK '.                      
001900     03  IDANSKNR        PIC 9(3)    DISPLAY.                             
002000*** END COPY W211M113C0  LENGTH=80    OLD LENGTH=                         
