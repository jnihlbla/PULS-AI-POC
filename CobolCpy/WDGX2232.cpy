000100 01  2232-WDGX2232.                                                       
000200*                                 ÖVERSÄTTNING ANSK-LARM                  
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDANSK + LOW-VALUE)                    
000500     03 2232-IDANSK          PIC S9(3)           COMP-3.                  
000600*                                 ANSKAFFARNUMMER                         
000700*                                 PROCURER NO.                            
000800     03 2232-LOW-VALUE       PIC X(3).                                    
000900     03 2232-IDANSK-LARM     PIC S9(3)           COMP-3.                  
001000*                                 LARMMOTTAGANDE ANSKAFFARENUMMER         
001100*                                 ALARMRECEVED PROCURER NO.               
001200     03 2232-FILLER          PIC X(13).                                   
001300*** END COPY WDGX2232C0  LENGTH=20                                        
