000100 01  MID-W4I75201.                                                        
000200*                                 COPYTEXT FOR MID W4I75201               
000300     03 MID-INPUT            OCCURS 11 TIMES.                             
000400*                                 OCCURS CLAUSE FOR W4I75201 COPY         
000500*                                 TEXT                                    
000600        05 MID-KDCMDVAL      PIC X.                                       
000700*                                 GENERAL COMMAND-CODE                    
000800        05 MID-BESORTRT      PIC X(20).                                   
000900*                                 SORT ALLOWED FOR RETURNS                
001000        05 MID-ANMORS-BEH    OCCURS 14 TIMES.                             
001100*                                 OCCURS CLAUSE FOR W4I75201 COPY         
001200*                                 TEXT                                    
001300           07 MID-KDRETBEH   PIC X.                                       
001400*                                 ACTION TYPE FOR A RETURN CODE           
001500     03 MID-BESORTRT-UP      PIC X(20).                                   
001600*                                 SORT ALLOWED FOR RETURNS                
001700     03 MID-ANMORS-BEH-UP    OCCURS 14 TIMES.                             
001800*                                 OCCURS CLAUSE FOR W4I75201 COPY         
001900*                                 TEXT                                    
002000        05 MID-KDRETBEH-UP   PIC X.                                       
002100*                                 ACTION TYPE FOR A RETURN CODE           
002200*** END OF VILMAII-COPY LENGTH= 419 BYTES                                 
