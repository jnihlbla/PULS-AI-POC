000100 01  A-W234L60A.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W23460         
000300*                                 LÄSNING AV AVROP                        
000400     03 A-TIAVROP-AVS        PIC S9(5)           COMP-3                   
000500                             VALUE ZEROS.                                 
000600*                                 AVSÄNDNINGSVECKA (PLANERAD)             
000700*                                 (ÅÅVV)                                  
000800     03 A-KVAVROP            PIC S9(7)           COMP-3                   
000900                             VALUE ZEROS.                                 
001000*                                 AVROPSKVANTITET                         
001100     03 A-KDAVROP            PIC S9              COMP-3                   
001200                             VALUE ZERO.                                  
001300*                                 AVROPSKOD                               
001400*** END COPY W234L60AC0  LENGTH=8                                         
