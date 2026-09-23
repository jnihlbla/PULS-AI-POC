000100 01  START-W461RI0N-CTX.                                                  
000200*                                 1:ST CARD FROM NOAC TO VIPS             
000300*                                 RECORD TYPE RI0                         
000400     03 START-IDPTYP         PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 START-IDDISTR        PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 START-TIFILDAT       PIC 9(6).                                    
000900*                                 FILE CREATION DATE  (YYMMDD)            
001000     03 START-TIHHMMSS       PIC 9(6).                                    
001100*                                 HOUR - MINUTE - SEC (HHMMSS)            
001200     03 START-IDDC           PIC X(2).                                    
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 START-FILLERX59      PIC X(59).                                   
001500*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
