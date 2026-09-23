000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***     FIKTIV TRAILER MELLAN                
000400*                            ***     LAGREN C1 OCH C2.                    
000500*                            *************************************        
000600 01  FTRL-IDLBFIKT                PIC S9(3)    COMP-3.                    
000700*                                                                         
000800       88  FTRL-NORMAL-C1         VALUE    +1 THRU  +399.                 
000900       88  FTRL-OVERLEV-C1        VALUE  +400 THRU  +409.                 
001000       88  FTRL-CLEAR-C1          VALUE  +410 THRU  +419.                 
001100       88  FTRL-BYTES-C1          VALUE  +420 THRU  +499.                 
001200       88  FTRL-NORMAL-C2         VALUE  +500 THRU  +899.                 
001300       88  FTRL-OVERLEV-C2        VALUE  +900 THRU  +909.                 
001400       88  FTRL-CLEAR-C2          VALUE  +910 THRU  +919.                 
001500       88  FTRL-BYTES-C2          VALUE  +920 THRU  +999.                 
001600*** END COPY WWFTRL01C0  LENGTH=0     OLD LENGTH=                         
