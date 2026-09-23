000100 01  TRCK-WDK728.                                                         
000200*                                 CUSTOMS TRACKING SEGMENT                
000300*                                 FYSISK NYCKEL DAINLEV                   
000400     03 TRCK-DAINLEV         PIC 9(16).                                   
000500*                                 INLEVERANS NUMMER                       
000600*                                 CONSIGNMENT IDENTITY                    
000700*                                 (YYYYMMDD+HHMMSSTH)                     
000800     03 TRCK-KVANTMOT        PIC S9(7)           COMP-3.                  
000900*                                 ANTAL MOTTAGET                          
001000*                                 QUANTITY RECEIVED                       
001100     03 TRCK-KVAVIS          PIC S9(7)           COMP-3.                  
001200*                                 AVISERAT ANTAL                          
001300*                                 QUANTITY NOTIFIED                       
001400     03 TRCK-KVTRACK-KVAR    PIC S9(7)           COMP-3.                  
001500*                                 ANTAL KVAR PER TRACKING-ID              
001600*                                 REMAINING QTY OF TRACKING-ID            
001700     03 TRCK-IDTRACK         PIC X(25).                                   
001800*                                 TRACKING ID FROM CUSTOMS                
001900*                                 CUSTOMS TRACKING ID                     
002000*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  
