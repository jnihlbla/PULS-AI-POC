000100 01  W42811.                                                              
000200*                                 UPPFÖLJNINGS FIL                        
000300*                                 LEVERANSANMÄRKNINGAR                    
000400*                                 FOLLOW UP FILE FOR                      
000500*                                 DISCREPANCIES             .             
000600     03 IDFTG                PIC 9(2).                                    
000700*                                 FÖRETAGSID EKONOM REDOVISNING           
000800*                                 COMPANY IDENTITY ACCOUNTING             
000900     03 IDDC-RET             PIC X(2).                                    
001000*                                 MOTTAGANDE LAGER FÖR RETURER            
001100*                                 RECEIVING WAREHOUSE FOR RETURNS         
001200     03 IDDISTR              PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 IDKUNDNR             PIC 9(6).                                    
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 IDRAPPNR             PIC 9(7).                                    
001900*                                 RAPPORT NUMMER                          
002000*                                 DISCREPANCY REPORT NUMBER               
002100     03 KDANMORS             PIC X(2).                                    
002200*                                 ORSAK TILL LEVERANSANMÄRKNING           
002300*                                 DISCREPANCY REPORT REASON CODE          
002400     03 TISAAPP-INLINL.                                                   
002500*                                 TIINLINL I ANNAT DATUMFORMAT            
002600        05 TISAAPP-INLINL-TISEKEL                                         
002700                             PIC 9(2).                                    
002800*                                 SEKEL I ÅRTALET                         
002900*                                 CENTURY                                 
003000        05 TISAAPP-INLINL-TIAAPP                                          
003100                             PIC 9(4).                                    
003200*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
003300*                                 12 PER ÅR                               
003400*                                 YEAR - PLANNING PERIOD (YYPP)           
003500*                                 12 PER YEAR                             
003600     03 TISAAVV-INLINL.                                                   
003700*                                 TIINLINL I ANNAT DATUMFORMAT            
003800        05 TISAAVV-INLINL-TISEKEL                                         
003900                             PIC 9(2).                                    
004000*                                 SEKEL I ÅRTALET                         
004100*                                 CENTURY                                 
004200        05 TISAAVV-INLINL-TIAAVV                                          
004300                             PIC 9(4).                                    
004400*                                 ÅR - VECKA  (ÅÅVV)                      
004500*                                 YEAR - WEEK  (YYWW)                     
004600     03 KVDAGAR-RET          PIC 9(3).                                    
004700*                                 NO. OF DAYS RETURN HANDLING FOR         
004800*                                 A DESCRAPENCY REPORT                    
004900     03 KVRETINL             PIC 9(6).                                    
005000*                                 INLAGT ANTAL VID RETUR                  
005100*                                 RECEIVED QUANTITY ON RETURN             
005200     03 KVRETINL-SKR         PIC 9(6).                                    
005300*                                 INRPT ANTAL SOM SKROTATS                
005400*                                 REPORTED QTY SCRAPPED                   
005500     03 KVAVV-KVANT          PIC 9(7).                                    
005600*                                 ANTALSAVVIKELSE KVANTITET               
005700*                                 QUANTITYDEVIATION QUANTITY              
005800     03 KVAVV-KVAL           PIC 9(7).                                    
005900*                                 ANTALSAVVIKELSE KVALITET                
006000*                                 QUANTITYDEVIATION QUALITY               
006100*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
