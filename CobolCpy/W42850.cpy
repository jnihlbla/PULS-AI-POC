000100 01  W42850.                                                              
000200*                                 UPPFÖLJNINGS FIL                        
000300*                                 RETURER WEB-LDC/NDC                     
000400*                                 FOLLOW UP FILE FOR                      
000500*                                 RETURNS WEB-LDC/NDC       .             
000600     03 KDMFUP               PIC X(2).                                    
000700*                                 RAPPORTGRUPP  MA/CN/PF/NA               
000800*                                 REPORT GROUP  MA/CN/PF/NA               
000900     03 IDDC-RET             PIC X(2).                                    
001000*                                 MOTTAGANDE LAGER FÖR RETURER            
001100*                                 RECEIVING WAREHOUSE FOR RETURNS         
001200     03 ADCITY               PIC X(20).                                   
001300     03 IDDISTR              PIC 9(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 IDKUNDNR             PIC 9(6).                                    
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900     03 IDRAPPNR             PIC 9(7).                                    
002000*                                 RAPPORT NUMMER                          
002100*                                 DISCREPANCY REPORT NUMBER               
002200     03 KDANMORS             PIC X(2).                                    
002300*                                 ORSAK TILL LEVERANSANMÄRKNING           
002400*                                 DISCREPANCY REPORT REASON CODE          
002500     03 IDARTNR              PIC 9(8).                                    
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 DARETANK             PIC 9(8).                                    
002900*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
003000*                                 DATE GOODS RECEIVING(YYYYMMDD)          
003100     03 TIINLINL             PIC 9(6).                                    
003200*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003300*                                 DATE OF REPORTED IN STOCK (R32)         
003400     03 TISAAVV-INLINL.                                                   
003500*                                 TIINLINL I ANNAT DATUMFORMAT            
003600        05 TISAAVV-INLINL-TISEKEL                                         
003700                             PIC 9(2).                                    
003800*                                 SEKEL I ÅRTALET                         
003900*                                 CENTURY                                 
004000        05 TISAAVV-INLINL-TIAAVV                                          
004100                             PIC 9(4).                                    
004200*                                 ÅR - VECKA  (ÅÅVV)                      
004300*                                 YEAR - WEEK  (YYWW)                     
004400     03 TISAAPP-INLINL.                                                   
004500*                                 TIINLINL I ANNAT DATUMFORMAT            
004600        05 TISAAPP-INLINL-TISEKEL                                         
004700                             PIC 9(2).                                    
004800*                                 SEKEL I ÅRTALET                         
004900*                                 CENTURY                                 
005000        05 TISAAPP-INLINL-TIAAPP                                          
005100                             PIC 9(4).                                    
005200*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
005300*                                 12 PER ÅR                               
005400*                                 NUMERA ÄR DETTA "PV-PERIOD"             
005500*                                 YEAR - PLANNING PERIOD (YYPP)           
005600*                                 12 PER YEAR                             
005700     03 KVRETINL             PIC 9(6).                                    
005800*                                 INLAGT ANTAL VID RETUR                  
005900*                                 RECEIVED QUANTITY ON RETURN             
006000     03 KVAVV-KVANT          PIC 9(7).                                    
006100*                                 ANTALSAVVIKELSE KVANTITET               
006200*                                 QUANTITYDEVIATION QUANTITY              
006300     03 KVRETINL-SKR         PIC 9(6).                                    
006400*                                 INRPT ANTAL SOM SKROTATS                
006500*                                 REPORTED QTY SCRAPPED                   
006600     03 KVAVV-KVAL           PIC 9(7).                                    
006700*                                 ANTALSAVVIKELSE KVALITET                
006800*                                 QUANTITYDEVIATION QUALITY               
006900     03 KVDAGAR-INL          PIC 9(2).                                    
007000*                                 NO. OF DAYS BINNING FOR                 
007100*                                 A DESCRAPENCY REPORT                    
007200     03 SUARTSTD             PIC 9(8)V9(2).                               
007300*                                 SUMMA STANDARDPRIS RADVÄRDE             
007400*                                 SUM LINEVALUE STANDARD PRICE            
007500*** END OF VILMAII-COPY LENGTH= 115 BYTES                                 
