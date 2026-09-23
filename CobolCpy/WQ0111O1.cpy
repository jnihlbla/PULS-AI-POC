000100 01  RESP-WQ0111O1.                                                       
000200*                                 MOD-COPYTEXT FOR WQ011100               
000300     03 RESP-KVRADER         PIC Z(4)9.                                   
000400*                                 ANTAL RADER                             
000500*                                 NUMBER OF LINES                         
000600     03 RESP-DATA-TABLE      OCCURS 50 TIMES.                             
000700*                                 A TABLE OF LINES WITH DATA              
000800*                                 IN "EXCEL" INPUT FORMAT                 
000900        05 RESP-TESSVDATA    PIC X(800).                                  
001000*                                 SEMIKOLON-SEP. DATA (EXCEL FMT)         
001100*                                 SEMICOLON SEP VALUES FOR EXCEL          
001200*** END OF VILMAII-COPY LENGTH= 40005 BYTES                               
