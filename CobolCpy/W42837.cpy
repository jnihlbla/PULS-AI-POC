000100 01  W42837.                                                              
000200*                                 UPPF÷LJNINGS FIL                        
000300*                                 RETURER WEB-LDC/NDC                     
000400*                                 FOLLOW UP FILE FOR                      
000500*                                 RETURNS WEB-LDC/NDC       .             
000600     03 IDFTG                PIC 9(2).                                    
000700*                                 F÷RETAGSID EKONOM REDOVISNING           
000800*                                 COMPANY IDENTITY ACCOUNTING             
000900     03 IDDC-RET             PIC X(2).                                    
001000*                                 MOTTAGANDE LAGER F÷R RETURER            
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
002200*                                 ORSAK TILL LEVERANSANMƒRKNING           
002300*                                 DISCREPANCY REPORT REASON CODE          
002400     03 DARETANK             PIC 9(8).                                    
002500*                                 ANKOMSTDATUM (≈≈≈≈MMDD)                 
002600*                                 DATE GOODS RECEIVING(YYYYMMDD)          
002700     03 TIINLINL             PIC 9(6).                                    
002800*                                 RAPPORTERINGSDATUM INLAGD (R32)         
002900*                                 DATE OF REPORTED IN STOCK (R32)         
003000     03 KVRETINL             PIC 9(6).                                    
003100*                                 INLAGT ANTAL VID RETUR                  
003200*                                 RECEIVED QUANTITY ON RETURN             
003300     03 KVAVV-KVANT          PIC 9(7).                                    
003400*                                 ANTALSAVVIKELSE KVANTITET               
003500*                                 QUANTITYDEVIATION QUANTITY              
003600     03 KVRETINL-SKR         PIC 9(6).                                    
003700*                                 INRPT ANTAL SOM SKROTATS                
003800*                                 REPORTED QTY SCRAPPED                   
003900     03 KVAVV-KVAL           PIC 9(7).                                    
004000*                                 ANTALSAVVIKELSE KVALITET                
004100*                                 QUANTITYDEVIATION QUALITY               
004200     03 KVDAGAR-INL          PIC 9(2).                                    
004300*                                 NO. OF DAYS BINNING FOR                 
004400*                                 A DESCRAPENCY REPORT                    
004500*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
