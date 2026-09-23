000100 01  WXTR8F.                                                              
000200*                                 PRIMÄREXTRAKT                           
000300*                                 UPPFÖLJNINGS FIL                        
000400*                                 KREDITERINGAR                           
000500*                                 PRIMARY EXTRACT                         
000600*                                 INFORMATION ABOUT                       
000700*                                 CREDITED LINES                          
000800*                                 DESCREPANCYS              .             
000900     03 IDDISTR              PIC 9(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200     03 IDKUNDNR             PIC 9(6).                                    
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500     03 IDDC                 PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 IDDC-RET             PIC X(2).                                    
001900*                                 MOTTAGANDE LAGER FÖR RETURER            
002000*                                 RECEIVING WAREHOUSE FOR RETURNS         
002100     03 IDFTG                PIC 9(2).                                    
002200*                                 FÖRETAGSID EKONOM REDOVISNING           
002300*                                 COMPANY IDENTITY ACCOUNTING             
002400     03 KDORDKL              PIC 9.                                       
002500*                                 ORDERKLASS                              
002600*                                 ORDER CLASS                             
002700     03 KDANMORS             PIC X(2).                                    
002800*                                 ORSAK TILL LEVERANSANMÄRKNING           
002900*                                 DISCREPANCY REPORT REASON CODE          
003000     03 SUKRENOT             PIC 9(7)V9(2).                               
003100*                                 KREDITNOTASUMMA                         
003200*                                 CREDIT NOTE TOTAL                       
003300     03 TIFAKT-SAAPP.                                                     
003400*                                 TIFAKT I ANNAT DATUMFORMAT              
003500        05 TIFAKT-TISEKEL-PER                                             
003600                             PIC 9(2).                                    
003700*                                 SEKEL I ÅRTALET                         
003800*                                 CENTURY                                 
003900        05 TIFAKT-TIAAPP     PIC 9(4).                                    
004000*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
004100*                                 12 PER ÅR                               
004200*                                 YEAR - PLANNING PERIOD (YYPP)           
004300*                                 12 PER YEAR                             
004400     03 TIKNOTA-SAAPP.                                                    
004500*                                 TIKNOTA I ANNAT DATUMFORMAT             
004600        05 TIKNOTA-TISEKEL-PER                                            
004700                             PIC 9(2).                                    
004800*                                 SEKEL I ÅRTALET                         
004900*                                 CENTURY                                 
005000        05 TIKNOTA-TIAAPP    PIC 9(4).                                    
005100*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
005200*                                 12 PER ÅR                               
005300*                                 YEAR - PLANNING PERIOD (YYPP)           
005400*                                 12 PER YEAR                             
005500*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
