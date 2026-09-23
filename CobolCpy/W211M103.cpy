000010*** EDIT ALLOWED                                                          
000100*                            *** POSTLÄNGD 80               ***           
000200 01  M103.                                                                
000300*    *** R32  AVVIKELSE MOTTAGET-AVISERAT                  ***            
000400     03  FILLER          PIC X(8)    VALUE '  LÖPNR '.                    
000500     03  IDLOPNRM        PIC 9(8)    DISPLAY.                             
000600     03  FILLER          PIC X(6)    VALUE ' ANSK '.                      
000700     03  IDANSKNR        PIC 9(3)    DISPLAY.                             
000800     03  FILLER          PIC X(5)    VALUE ' LEV '.                       
000900     03  IDLEVNR         PIC 9(5)    DISPLAY.                             
001000     03  FILLER          PIC X(5)    VALUE ' DIFF'.                       
001100     03  KVDIFF          PIC +++++++   DISPLAY.                           
001200     03  FILLER          PIC X(8)    VALUE ' ANTAVV '.                    
001300     03  KDANTAV         PIC 9       DISPLAY.                             
001400     03  FILLER          PIC X(3)    VALUE ' IP'.                         
001500     03  PRINK           PIC Z(4)9V99  DISPLAY.                           
001600     03  FILLER          PIC X(3)    VALUE ' SP'.                         
001700     03  PRARTSTD        PIC Z(4)9V99    DISPLAY.                         
001800     03  FILLER          PIC X(4)    VALUE SPACE.                         
001900*** END COPY W211M103C0  LENGTH=80    OLD LENGTH=80                       
