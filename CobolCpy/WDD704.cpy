000100 01  WDD704.                                                              
000200*                                 ERSÄTTNINGSINFORMATION                  
000300*                                 SÖKARGUMENT TIERSDAT (-REG)             
000400     03 KDSTATUS-C1          PIC S9(3)           COMP-3.                  
000500*                                 STATUSKOD C1                            
000600     03 KDSTATUS-C2          PIC S9(3)           COMP-3.                  
000700*                                 STATUSKOD C2                            
000800     03 TIERSDAT-REG         PIC S9(5)           COMP-3.                  
000900*                                 REGISTRERINGSDATUM  (ÅÅVVD)             
001000     03 TIERSDAT-PREL-C1     PIC S9(5)           COMP-3.                  
001100*                                 PREL ERSÄTTNINGSDATUM C1  ÅÅVVD         
001200     03 TIERSDAT-PREL-C2     PIC S9(5)           COMP-3.                  
001300*                                 PREL ERSÄTTNINGSDATUM C2  ÅÅVVD         
001400*** END COPY WDD704CCC0  LENGTH=13                                        
