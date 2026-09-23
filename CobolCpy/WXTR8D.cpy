000100 01  WXTR8D.                                                              
000200*                                 PRIMÄREXTRAKT                           
000300*                                 LEVERANSANMÄRKNINGAR                    
000400*                                 PRIMARY EXTRACT                         
000500*                                 DISCREPANCY INFORMATION   .             
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 IDKUNDNR             PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 IDRAPPNR             PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400*                                 DISCREPANCY REPORT NUMBER               
001500     03 IDARTNR              PIC 9(8).                                    
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800     03 IDFKNGRP             PIC 9(4).                                    
001900*                                 FUNKTIONSGRUPP                          
002000*                                 FUNCTION GROUP                          
002100     03 IDFTG                PIC 9(2).                                    
002200*                                 FÖRETAGSID EKONOM REDOVISNING           
002300*                                 COMPANY IDENTITY ACCOUNTING             
002400     03 IDDC                 PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700     03 IDDC-RET             PIC X(2).                                    
002800*                                 MOTTAGANDE LAGER FÖR RETURER            
002900*                                 RECEIVING WAREHOUSE FOR RETURNS         
003000     03 ADLAGOMR             PIC 9(2).                                    
003100*                                 LAGEROMRÅDE                             
003200*                                 AREA                                    
003300     03 ADGANG               PIC 9(2).                                    
003400*                                 GÅNG                                    
003500*                                 AISLE                                   
003600     03 ADPLATS              PIC 9(5).                                    
003700*                                 LAGERPLATSNUMMER                        
003800*                                 LOCATION                                
003900     03 KDANMORS             PIC X(2).                                    
004000*                                 ORSAK TILL LEVERANSANMÄRKNING           
004100*                                 DISCREPANCY REPORT REASON CODE          
004200     03 KDANMTYP             PIC 9.                                       
004300*                                 TYP AV LEVERANSANMÄRKNING               
004400*                                 TYPE OF DESCRAPANCIE REPORT             
004500     03 KDFRAKT              PIC 9(2).                                    
004600*                                 FRAKTSÄTT DC TILL KUND                  
004700*                                 FREIGHT CODE                            
004800     03 KDKREBEH             PIC X(3).                                    
004900*                                 BEHANDLINGSSTATUS                       
005000*                                 TREATMENT STATUS                        
005100     03 IDANALYS             PIC X(12).                                   
005200*                                 ANALYSNUMMER                            
005300*                                 ANALYSIS NUMBER                         
005400     03 FLDIRLEV             PIC X.                                       
005500*                                 DIREKTLEVERANS ?                        
005600*                                 DIRECT DELIVERY ?                       
005700     03 TISAAPP-LEVANM.                                                   
005800*                                 TILEVANM I ANNAT DATUMFORMAT            
005900        05 TISAAPP-LEVANM-TISEKEL                                         
006000                             PIC 9(2).                                    
006100*                                 SEKEL I ÅRTALET                         
006200*                                 CENTURY                                 
006300        05 TISAAPP-LEVANM-TIAAPP                                          
006400                             PIC 9(4).                                    
006500*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
006600*                                 12 PER ÅR                               
006700*                                 YEAR - PLANNING PERIOD (YYPP)           
006800*                                 12 PER YEAR                             
006900     03 TISAAPP-RETILL.                                                   
007000*                                 TIRETILL I ANNAT DATUMFORMAT            
007100        05 TISAAPP-RETILL-TISEKEL                                         
007200                             PIC 9(2).                                    
007300*                                 SEKEL I ÅRTALET                         
007400*                                 CENTURY                                 
007500        05 TISAAPP-RETILL-TIAAPP                                          
007600                             PIC 9(4).                                    
007700*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
007800*                                 12 PER ÅR                               
007900*                                 YEAR - PLANNING PERIOD (YYPP)           
008000*                                 12 PER YEAR                             
008100     03 TISAAPP-RETANK.                                                   
008200*                                 TIRETANK I ANNAT DATUMFORMAT            
008300        05 TISAAPP-RETANK-TISEKEL                                         
008400                             PIC 9(2).                                    
008500*                                 SEKEL I ÅRTALET                         
008600*                                 CENTURY                                 
008700        05 TISAAPP-RETANK-TIAAPP                                          
008800                             PIC 9(4).                                    
008900*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
009000*                                 12 PER ÅR                               
009100*                                 YEAR - PLANNING PERIOD (YYPP)           
009200*                                 12 PER YEAR                             
009300     03 TISAAPP-INLINL.                                                   
009400*                                 TIINLINL I ANNAT DATUMFORMAT            
009500        05 TISAAPP-INLINL-TISEKEL                                         
009600                             PIC 9(2).                                    
009700*                                 SEKEL I ÅRTALET                         
009800*                                 CENTURY                                 
009900        05 TISAAPP-INLINL-TIAAPP                                          
010000                             PIC 9(4).                                    
010100*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
010200*                                 12 PER ÅR                               
010300*                                 YEAR - PLANNING PERIOD (YYPP)           
010400*                                 12 PER YEAR                             
010500     03 TISAAPP-KNOTA.                                                    
010600*                                 TIKNOTA I ANNAT DATUMFORMAT             
010700        05 TISAAPP-KNOTA-TISEKEL                                          
010800                             PIC 9(2).                                    
010900*                                 SEKEL I ÅRTALET                         
011000*                                 CENTURY                                 
011100        05 TISAAPP-KNOTA-TIAAPP                                           
011200                             PIC 9(4).                                    
011300*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
011400*                                 12 PER ÅR                               
011500*                                 YEAR - PLANNING PERIOD (YYPP)           
011600*                                 12 PER YEAR                             
011700     03 TISAAVV-LEVANM.                                                   
011800*                                 TILEVANM I ANNAT DATUMFORMAT            
011900        05 TISAAVV-LEVANM-TISEKEL                                         
012000                             PIC 9(2).                                    
012100*                                 SEKEL I ÅRTALET                         
012200*                                 CENTURY                                 
012300        05 TISAAVV-LEVANM-TIAAVV                                          
012400                             PIC 9(4).                                    
012500*                                 ÅR - VECKA  (ÅÅVV)                      
012600*                                 YEAR - WEEK  (YYWW)                     
012700     03 TISAAVV-RETILL.                                                   
012800*                                 TIRETILL I ANNAT DATUMFORMAT            
012900        05 TISAAVV-RETILL-TISEKEL                                         
013000                             PIC 9(2).                                    
013100*                                 SEKEL I ÅRTALET                         
013200*                                 CENTURY                                 
013300        05 TISAAVV-RETILL-TIAAVV                                          
013400                             PIC 9(4).                                    
013500*                                 ÅR - VECKA  (ÅÅVV)                      
013600*                                 YEAR - WEEK  (YYWW)                     
013700     03 TISAAVV-RETANK.                                                   
013800*                                 TIRETANK I ANNAT DATUMFORMAT            
013900        05 TISAAVV-RETANK-TISEKEL                                         
014000                             PIC 9(2).                                    
014100*                                 SEKEL I ÅRTALET                         
014200*                                 CENTURY                                 
014300        05 TISAAVV-RETANK-TIAAVV                                          
014400                             PIC 9(4).                                    
014500*                                 ÅR - VECKA  (ÅÅVV)                      
014600*                                 YEAR - WEEK  (YYWW)                     
014700     03 TISAAVV-INLINL.                                                   
014800*                                 TIINLINL I ANNAT DATUMFORMAT            
014900        05 TISAAVV-INLINL-TISEKEL                                         
015000                             PIC 9(2).                                    
015100*                                 SEKEL I ÅRTALET                         
015200*                                 CENTURY                                 
015300        05 TISAAVV-INLINL-TIAAVV                                          
015400                             PIC 9(4).                                    
015500*                                 ÅR - VECKA  (ÅÅVV)                      
015600*                                 YEAR - WEEK  (YYWW)                     
015700     03 TISAAVV-KNOTA.                                                    
015800*                                 TIKNOTA I ANNAT DATUMFORMAT             
015900        05 TISAAVV-KNOTA-TISEKEL                                          
016000                             PIC 9(2).                                    
016100*                                 SEKEL I ÅRTALET                         
016200*                                 CENTURY                                 
016300        05 TISAAVV-KNOTA-TIAAVV                                           
016400                             PIC 9(4).                                    
016500*                                 ÅR - VECKA  (ÅÅVV)                      
016600*                                 YEAR - WEEK  (YYWW)                     
016700     03 KVLEVANM             PIC 9(6).                                    
016800*                                 LEVERANSANMÄRKNINGSANTAL                
016900*                                 DISCREPANCY REPORT QTY                  
017000     03 KVRETINL             PIC 9(6).                                    
017100*                                 INLAGT ANTAL VID RETUR                  
017200*                                 RECEIVED QUANTITY ON RETURN             
017300     03 KVRETINL-SKR         PIC 9(6).                                    
017400*                                 INRPT ANTAL SOM SKROTATS                
017500*                                 REPORTED QTY SCRAPPED                   
017600     03 KVAVV-KVANT          PIC 9(7).                                    
017700*                                 ANTALSAVVIKELSE KVANTITET               
017800*                                 QUANTITYDEVIATION QUANTITY              
017900     03 KVAVV-KVAL           PIC 9(7).                                    
018000*                                 ANTALSAVVIKELSE KVALITET                
018100*                                 QUANTITYDEVIATION QUALITY               
018200     03 SUARTBTO             PIC 9(7)V9(2).                               
018300*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
018400     03 SUARTBTO-RET         PIC 9(7)V9(2).                               
018500*                                 SUMMA FÖRSÄLJNINGSVÄRDE FÖR RET         
018600*                                 URER                                    
018700     03 SUARTSTD             PIC 9(8)V9(2).                               
018800*                                 SUMMA STANDARDPRIS RADVÄRDE             
018900*                                 SUM LINEVALUE STANDARD PRICE            
019000     03 SUARTSTD-INL         PIC 9(8)V9(2).                               
019100*                                 SUMMA STANDARDPRIS RADVÄRDE             
019200*                                 SUM LINEVALUE STANDARD PRICE            
019300     03 SUARTSTD-SKR         PIC 9(8)V9(2).                               
019400*                                 SUMMA STANDARDPRIS RADVÄRDE             
019500*                                 SUM LINEVALUE STANDARD PRICE            
019600     03 SUARTSTD-KVANT       PIC 9(8)V9(2).                               
019700*                                 SUMMA STANDARDPRIS RADVÄRDE             
019800*                                 SUM LINEVALUE STANDARD PRICE            
019900     03 SUARTSTD-KVAL        PIC 9(8)V9(2).                               
020000*                                 SUMMA STANDARDPRIS RADVÄRDE             
020100*                                 SUM LINEVALUE STANDARD PRICE            
020200     03 VKART                PIC 9(7).                                    
020300*                                 ARTIKELVIKT (G)                         
020400*                                 PART WEIGHT (G)                         
020500     03 VKART-KRED           PIC 9(7).                                    
020600*                                 VIKT KREDITERADE ARTIKLAR               
020700*                                 WEIGHT CREDITED PARTS                   
020800     03 VKART-KVANT          PIC 9(7).                                    
020900*                                 VIKT AVVIKELSE   ARTIKLAR               
021000*                                 WEIGHT QTY.DEV. PARTS                   
021100     03 VLARTNTO             PIC 9(8)V9(1).                               
021200*                                 ARTIKELVOLYM NETTO (CM3)                
021300*                                 PART NET VOLUME    (CM3)                
021400     03 VLARTNTO-KRED        PIC 9(8)V9(1).                               
021500*                                 ARTIKELVOLYM NETTO (CM3)                
021600*                                 PART NET VOLUME    (CM3)                
021700     03 VLARTNTO-KVANT       PIC 9(8)V9(1).                               
021800*                                 ARTIKELVOLYM NETTO (CM3)                
021900*                                 PART NET VOLUME    (CM3)                
022000     03 KDARBTYP-ADM         PIC X(8).                                    
022100*                                 ANSVARIG LEVERANSANMÄRKNINGSAVD         
022200*                                 RESPONSIBLE AT DISCREPANCYDEPT          
022300     03 IDPERSON-ADM         PIC 9(3).                                    
022400*                                 PERSONKOD LEVANM                        
022500*                                 STAFF CODE DISCREPANCY                  
022600     03 KDARBTYP-RET         PIC X(8).                                    
022700*                                 ANSVARIG RETURAVDELNINGEN               
022800*                                 RESPONSIBLE AT RETURNDEPARTMENT         
022900     03 IDPERSON-RET         PIC 9(3).                                    
023000*                                 PERSONKOD RETURAVD.                     
023100*                                 STAFF CODE RETURN DEPT.                 
023200     03 IDANSTNR-RET         PIC 9(5).                                    
023300*                                 ANSTÄLLNINGSNUMMER INLÄGGARE.           
023400*                                 EMPLOYEE NUMBER BINNER.                 
023500     03 KDORDKL              PIC 9.                                       
023600*                                 ORDERKLASS                              
023700*                                 ORDER CLASS                             
023800     03 IDUSER-PACK          PIC X(8).                                    
023900*                                 ANSVARIGT USERID PACKARE                
024000*                                 RESPONSIBLE USERID PACKER               
024100*** END OF VILMAII-COPY LENGTH= 309 BYTES                                 
