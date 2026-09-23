000100 01  4-W234L604.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W23460         
000300*                                 LÄSNING AV AVIDATA                      
000400     03 4-IDAVINR-SEN        PIC S9(7)           COMP-3                   
000500                             VALUE ZEROS.                                 
000600*                                 AVINUMMER SENASTE INLEVERANS            
000700     03 4-KVAVIS-SEN         PIC S9(7)           COMP-3                   
000800                             VALUE ZEROS.                                 
000900*                                 SENAST AVISERAT ANTAL                   
001000     03 4-TIAVIDAT-SEN       PIC S9(7)           COMP-3                   
001100                             VALUE ZEROS.                                 
001200*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
001300*** END COPY W234L604C0  LENGTH=12                                        
