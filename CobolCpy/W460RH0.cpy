000100 01  START-W460RH0.                                                       
000200*                                 1:ST CARD FROM VIPS TO  NOAC            
000300*                                 RECORD TYPE RH0                         
000400     03 START-IDPTYP         PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 START-IDDISTR        PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 START-TIFILDAT       PIC 9(6).                                    
000900*                                 FILE CREATION DATE  (YYMMDD)            
001000     03 START-TIHHMMSS       PIC 9(6).                                    
001100*                                 HOUR - MINUTE - SEC (HHMMSS)            
001200     03 FILLER               PIC X(61).                                   
001300*** END COPY W460RH0CC0  LENGTH=80                                        
