000100 01  9305-WDGX9305.                                                       
000200*                                 STYRANDE VALUTA                         
000300*                                 FYSISK NYCKEL: WDGXKEY                  
000400*                                 (IDHTYP + KDVALISO-HUV +                
000500*                                  KDVALTYP + L-V)                        
000600     03 9305-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 9305-KDVALISO-HUV    PIC X(3).                                    
000900*                                 HUVUDVALUTA                             
001000*                                 MAIN CURRENCY                           
001100     03 9305-KDVALTYP        PIC X.                                       
001200*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
001300*                                 CURRENCY PER YEAR/MONTH/DAY             
001400     03 9305-LOWVALUE        PIC X(22).                                   
001500*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
