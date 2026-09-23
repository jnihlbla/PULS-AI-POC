000100 01  WXTR9B-CTX.                                                          
000200*                                 PRIMARY EXTRACT                         
000300*                                                                         
000400*                                 VOLUME PLANNIG INFO KINA                
000500*                                                                         
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 PART NUMBER                             
000800     03 KDERS                PIC S9(3)           COMP-3.                  
000900*                                 SUPERSESSION CODE                       
001000     03 IDARTNR-TILLK        PIC S9(9)           COMP-3.                  
001100*                                 REPLACEMENT PART NO.                    
001200     03 IDDC                 PIC X(2).                                    
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 BEART-ENG            PIC X(25).                                   
001500     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001600*                                 FUNCTION GROUP                          
001700     03 IDLEVNR              PIC X(5).                                    
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 TIFINLV              PIC S9(5)           COMP-3.                  
002000*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
002100     03 KVDLTID-TOT          PIC S9(3)           COMP-3.                  
002200*                                 TOTAL CALENDAR DAYS LEAD TIME           
002300     03 STOCKOH              PIC S9(7)           COMP-3.                  
002400*                                 STOCK BALANCE                           
002500     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
002600*                                 PART OF AK IN THE SDC                   
002700     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002800*                                 PART OF AK ON ITS WAY                   
002900     03 KVROS-BULK           PIC S9(7)           COMP-3.                  
003000*                                 BACK ORDER BALANCE, CLASS 2-4           
003100     03 KVROS-DAG            PIC S9(7)           COMP-3.                  
003200*                                 BACK ORDER BALANCE, CLASS 1             
003300     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 AVERAGE COST FOREIGN CURRENCY           
003500*** END OF VILMAII-COPY LENGTH= 77 BYTES                                  
