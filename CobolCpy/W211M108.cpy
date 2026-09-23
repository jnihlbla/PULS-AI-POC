000010*** EDIT ALLOWED                                                          
000100*                            *** POSTLÄNGD 80               ***           
000200 01  M108.                                                                
000300*    *** R32 R34  ARTIKELVIKT EJ REGISTRERAD                ***           
000400     03  FILLER          PIC X(9)    VALUE '   LÖPNR '.                   
000500     03  IDLOPNRM        PIC 9(8)    DISPLAY.                             
000600     03  FILLER          PIC X(17)  VALUE '  ARTIKELADRESS  '.            
000700     03  ARTADR.                                                          
000800         05  ADLAGOMR    PIC 9(2)    DISPLAY.                             
000900         05  ADGANG      PIC 9(2)    DISPLAY.                             
001000         05  ADPLATS     PIC 9(5)    DISPLAY.                             
001100     03  FILLER          PIC X(14)   VALUE '  VIKT SAKNAS '.              
001200     03  FILLER          PIC X(26)   VALUE SPACE.                         
001300*** END COPY W211M108C0  LENGTH=83    OLD LENGTH=                         
