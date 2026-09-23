000100 01  WDD904.                                                              
000200*                                 LEVERANSPLANEREGISTER                   
000300*                                 OMSPEC-INFORMATION                      
000400*                                 FYSISK NYCKEL  DASPECST                 
000500     03 DASPECST             PIC 9(6).                                    
000600*                                 SPECAD FR.O.M DATUM   (ÅÅÅÅVV)          
000700     03 KDLPORS-TAB          OCCURS 3 TIMES                               
000800                             PIC S9(3)           COMP-3.                  
000900*                                 LEVERANSPLANEORSAK                      
001000     03 KVBEST-PL            PIC S9(7)           COMP-3.                  
001100*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
001200     03 KDPLKOEP             PIC S9              COMP-3.                  
001300*                                 STATUS AVTALSKÖP (PLAN)                 
001400*                                 1=FÖRESLAGEN  2=GODKÄND                 
001500*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
