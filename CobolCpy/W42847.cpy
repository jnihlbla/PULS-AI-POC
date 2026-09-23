000100 01  W42847.                                                              
000200*                                 UPPFÖLJNINGS FIL                        
000300*                                 LEVERANSANMÄRKNINGAR                    
000400*                                 SELECTED EXTRACT ABOUT                  
000500*                                 DESCREPANCYS              .             
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 IDKUNDNR             PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 IDDC-RET             PIC X(2).                                    
001600*                                 MOTTAGANDE LAGER FÖR RETURER            
001700*                                 RECEIVING WAREHOUSE FOR RETURNS         
001800     03 IDFTG                PIC 9(2).                                    
001900*                                 FÖRETAGSID EKONOM REDOVISNING           
002000*                                 COMPANY IDENTITY ACCOUNTING             
002100     03 KDORDKL              PIC 9.                                       
002200*                                 ORDERKLASS                              
002300*                                 ORDER CLASS                             
002400     03 KVRADER              PIC 9(5).                                    
002500*                                 ANTAL RADER                             
002600*                                 NUMBER OF LINES                         
002700     03 PRARTNTO             PIC 9(7)V9(2).                               
002800*                                 ARTIKELPRIS NETTO                       
002900*                                 NET PRICE EACH   (FOB NET)              
003000     03 TIFAKT-SAAPP.                                                     
003100*                                 TIFAKT I ANNAT DATUMFORMAT              
003200        05 TIFAKT-TISEKEL-PER                                             
003300                             PIC 9(2).                                    
003400*                                 SEKEL I ÅRTALET                         
003500*                                 CENTURY                                 
003600        05 TIFAKT-TIAAPP     PIC 9(4).                                    
003700*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
003800*                                 12 PER ÅR                               
003900*                                 NUMERA ÄR DETTA "PV-PERIOD"             
004000*                                 YEAR - PLANNING PERIOD (YYPP)           
004100*                                 12 PER YEAR                             
004200*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
