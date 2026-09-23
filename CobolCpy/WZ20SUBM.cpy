000100 01  SUBM-WZ20SUBM.                                                       
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ20SUBM (SUBMIT JCL)           
000400*                                 CALLS:                                  
000500*                                    USING SUBM-WZ20SUBM                  
000600*                                                                         
000700*                                 IDCALL=1-9 DEPENDING ON WHICH           
000800*                                       OUTPUT "CHANNEL" IS USED.         
000900*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001000*                                 LINE=ONE 80 BYTES RECORD                
001100*                                      (ONLY USED IN PUT CALL)            
001200     03 SUBM-IDCALL          PIC S9(9)           COMP.                    
001300*                                 ID OF CALL SEQUENCE                     
001400     03 SUBM-KDFUNC          PIC X(10).                                   
001500*                                 FUNCTION CODE                           
001600     03 SUBM-LINE            PIC X(80).                                   
001700*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
