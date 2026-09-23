000100 01  B-W234L60B.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W23460         
000300*                                 LÄSNING AV LEVERANSBESKED               
000400     03 B-TILEVBSK-AVS       PIC S9(5)           COMP-3                   
000500                             VALUE ZEROS.                                 
000600*                                 AVSÄNDNINGSVECKA     (ÅÅVV)             
000700*                                 ENL LEVERANSBESKED                      
000800     03 B-KVAVIS-BSKKVAR     PIC S9(7)           COMP-3                   
000900                             VALUE ZEROS.                                 
001000*                                 LEVERANSBESKEDANTAL EFTER               
001100*                                 AVBOKNING                               
001200*** END COPY W234L60BC0  LENGTH=7                                         
