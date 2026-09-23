000100 01  MOD-W4O75201.                                                        
000200*                                 COPYTEXT FOR MOD W4O75201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 SCREEN NUMBER                           
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS ERROR MESSAGE                       
000700     03 MOD-OUTPUT           OCCURS 11 TIMES.                             
000800*                                 OCCURS CLAUSE FOR W4O75201 COPY         
000900*                                 TEXT                                    
001000        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
001100        05 MOD-KDCMDVAL      PIC X.                                       
001200*                                 GENERAL COMMAND-CODE                    
001300        05 MOD-BESORTRT-ATTR PIC X(2).                                    
001400        05 MOD-BESORTRT      PIC X(20).                                   
001500*                                 SORT ALLOWED FOR RETURNS                
001600        05 MOD-ANMORS-BEH    OCCURS 14 TIMES.                             
001700*                                 OCCURS CLAUSE FOR W4O75201 COPY         
001800*                                 TEXT                                    
001900           07 MOD-KDRETBEH-ATTR                                           
002000                             PIC X(2).                                    
002100           07 MOD-KDRETBEH   PIC X.                                       
002200*                                 ACTION TYPE FOR A RETURN CODE           
002300     03 MOD-BESORTRT-UP-ATTR PIC X(2).                                    
002400     03 MOD-BESORTRT-UP      PIC X(20).                                   
002500*                                 SORT ALLOWED FOR RETURNS                
002600     03 MOD-ANMORS-BEH-UP    OCCURS 14 TIMES.                             
002700*                                 OCCURS CLAUSE FOR W4O75201 COPY         
002800*                                 TEXT                                    
002900        05 MOD-KDRETBEH-UP-ATTR                                           
003000                             PIC X(2).                                    
003100        05 MOD-KDRETBEH-UP   PIC X.                                       
003200*                                 ACTION TYPE FOR A RETURN CODE           
003300     03 MOD-TEMFSINF         PIC X(55).                                   
003400*                                 INFORMATION MESSAGE                     
003500*** END OF VILMAII-COPY LENGTH= 900 BYTES                                 
