000100 01  GMTD-WDB701.                                                         
000200*                                 KUNDREGISTER                            
000300*                                 DEALER INFO                             
000400*                                 FYSISK NYCKEL: IDGMT                    
000500*                                 (IDDISTR + IDKUNDNR)                    
000600     03 GMTD-IDGMT.                                                       
000700*                                 GODSMOTTAGARE                           
000800*                                 GOODS RECEIVER                          
000900        05 GMTD-IDDISTR      PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200        05 GMTD-IDKUNDNR     PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500     03 GMTD-IDDEALER-VIPS   PIC X(6).                                    
001600*                                 VIPS ÅTERFÖRSÄLJARE                     
001700*                                 VIPS DEALER                             
001800     03 GMTD-IDDEALER-VIPSINV                                             
001900                             PIC X(6).                                    
002000*                                 FINANSIELL ÅTERFÖRSÄLJARE               
002100*                                 FINANCIAL REPORTING DEALER              
002200     03 GMTD-BEDEALER-VIPSINV                                             
002300                             PIC X(30).                                   
002400*                                 VIPS ÅTERFÖRSÄLJARNAMN                  
002500*                                 VIPS DEALER NAME                        
002600     03 GMTD-ADDEALER-INVRAD1                                             
002700                             PIC X(30).                                   
002800*                                 KUNDADRESS 1                            
002900*                                 DEALER ADDRESS LINE 1                   
003000     03 GMTD-ADDEALER-INVRAD2                                             
003100                             PIC X(30).                                   
003200*                                 KUNDADRESS 2                            
003300*                                 DEALER ADDRESS LINE 1                   
003400     03 GMTD-ADPOSTNR-INV    PIC X(10).                                   
003500*                                 FAKURAMOTTAGARENS POSTNUMMER            
003600*                                 INVOICE RECEIVER POSTAL CODE            
003700     03 GMTD-ADCITY-INV      PIC X(20).                                   
003800*                                 FAKTURAMOTTAGERENS STAD                 
003900*                                 INVOICE RECEIVER CITY                   
004000     03 GMTD-BEDEALER-VIPSGMT                                             
004100                             PIC X(30).                                   
004200*                                 VIPS ÅTERFÖRSÄLJARNAMN                  
004300*                                 VIPS DEALER NAME                        
004400     03 GMTD-ADDEALER-GMTRAD1                                             
004500                             PIC X(30).                                   
004600*                                 GODSMOTTAGANDE KUND 1                   
004700*                                 RECEIVER DEALER ADDRESS LINE 1          
004800     03 GMTD-ADDEALER-GMTRAD2                                             
004900                             PIC X(30).                                   
005000*                                 GODSMOTTAGANDE KUND 2                   
005100*                                 RECEIVER DEALER ADDRESS LINE 2          
005200     03 GMTD-ADPOSTNR-GMT    PIC X(10).                                   
005300*                                 GODSMOTTAGARENS POSTNUMMER              
005400*                                 GOODS RECEIVER POSTAL CODE              
005500     03 GMTD-ADCITY-GMT      PIC X(20).                                   
005600*                                 GODSMOTTAGERENS STAD                    
005700*                                 GOODS RECEIVER CITY                     
005800     03 GMTD-KDKNDSTA        PIC X.                                       
005900*                                 KUNDSTATUSKOD                           
006000*                                 CUSTOMER STATUS CODE                    
006100     03 GMTD-DAREGDAT        PIC 9(8).                                    
006200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
006300*                                 REGISTRATION DATE (YYYYMMDD)            
006400     03 GMTD-DASTADAT        PIC 9(8).                                    
006500*                                 GENERELLT STARTDATUM                    
006600*                                 GENERAL START DATE                      
006700     03 GMTD-DASTODAT        PIC 9(8).                                    
006800*                                 GENERELLT STOPPDATUM                    
006900*                                 GENERAL STOP DATE                       
007000     03 GMTD-KDCREDIT        PIC X.                                       
007100*                                 KREDITVÄRDIGHETSKOD                     
007200*                                 CREDIT ALLOWED CODE                     
007300     03 GMTD-FLDIRAFF        PIC X.                                       
007400*                                 DIRECT BUSINESS?                        
007500*                                 DIRECT BUSINESS ?                       
007600     03 GMTD-IDLANDX2        PIC X(2).                                    
007700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
007800*                                 2-LETTER CODE FOR COUNTRY               
007900     03 GMTD-KDKNDKAT        PIC X(2).                                    
008000*                                 KUNDKATEGORIKOD                         
008100*                                 CUSTOMER CATEGORI CODE                  
008200     03 GMTD-TIREGDAT        PIC S9(7)           COMP-3.                  
008300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008400*                                 REGISTRATION DATE (YYMMDD)              
008500*** END OF VILMAII-COPY LENGTH= 294 BYTES                                 
