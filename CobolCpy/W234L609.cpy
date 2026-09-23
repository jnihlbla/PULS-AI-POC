000100 01  9-W234L609.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W23460         
000300*                                 LÄSNING AV AVROP FÖR EXTRA-             
000400*                                 LEVERANSER                              
000500     03 9-TIEXLEV            PIC S9(5)           COMP-3                   
000600                             VALUE ZEROS.                                 
000700*                                 AVSÄNDNINGSVECKA EXTRALEVERANS          
000800*                                 (ÅÅVV)                                  
000900     03 9-KVEXLEV            PIC S9(7)           COMP-3                   
001000                             VALUE ZEROS.                                 
001100*                                 EXTRALEVERANSKVANTITET                  
001200*** END COPY W234L609C0  LENGTH=7                                         
