000100 01  W480W005.                                                            
000200     03 IX                   PIC S9(4)           COMP SYNC                
000300                             VALUE ZEROS.                                 
000400     03 MAX-IX               PIC S9(4)           COMP SYNC                
000500                             VALUE +12.                                   
000600     03 LAGER                OCCURS 12 TIMES.                             
000700        05 ANTALRADER        PIC S9(7)           COMP-3.                  
000800        05 VKARTTOT          PIC S9(17)          COMP-3.                  
000900        05 PRARTSTDTOT       PIC S9(15)V9(2)     COMP-3.                  
001000        05 MOPRISTOT         PIC S9(15)V9(2)     COMP-3.                  
001100        05 FILLER            PIC X(5).                                    
001200*** END COPY W480W005C0  LENGTH=436                                       
