000100 01  W221LI42.                                                            
000200*                                 POST FÖR OMSPEC AV LEVERANSPLAN         
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 KDLPORS-TAB          OCCURS 3 TIMES                               
000700                             PIC S9(3)           COMP-3.                  
000800*                                 LEVERANSPLANEORSAK                      
000900     03 KVBEST-PL            PIC S9(7)           COMP-3.                  
001000*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
001100     03 KDPLKOEP             PIC S9              COMP-3.                  
001200*                                 STATUS AVTALSKÖP (PLAN)                 
001300*                                 1=FÖRESLAGEN  2=GODKÄND                 
001400*** END COPY W221LI42C0  LENGTH=16                                        
