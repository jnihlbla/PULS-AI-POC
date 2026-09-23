000100 01  W4765001.                                                            
000200*                                 SAVE-IT FRÅN BILLIT                     
000300*                                 POST FÖR INFO TILL OLIKA SYSTEM         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 W476FAKT.                                                         
000800*                                 FAKTURATRANSAR FRÅN BILL-IT             
000900        05 IDPRODNR          PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100*                                 PRODUCTION NUMBER                       
001200        05 IDKOLLI           PIC S9(5)           COMP-3.                  
001300*                                 KOLLINUMMER                             
001400*                                 CASE NUMBER                             
001500        05 IDPURAD           PIC S9(5)           COMP-3.                  
001600*                                 RADNUMMER PÅ PACKUNDERLAG               
001700*                                 LINENO IN PACKINGDOCUMENT               
001800        05 BEART             PIC X(25).                                   
001900*                                 ARTIKELBENÄMNING                        
002000*                                 PART DESCRIPTION                        
002100        05 DAFINDOC          PIC 9(8).                                    
002200*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
002300*                                 INVOICING DATE   (YYYYMMDD)             
002400        05 IDDC              PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700        05 IDDISTR           PIC S9(5)           COMP-3.                  
002800*                                 DISTRIKTNUMMER                          
002900*                                 DISTRICT NUMBER                         
003000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
003100*                                 KUNDNUMMER                              
003200*                                 CUSTOMER NO                             
003300        05 IDORDNR7          PIC S9(7)           COMP-3.                  
003400*                                 ORDERNUMMER                             
003500*                                 ORDER NUMBER                            
003600        05 IDFAKT            PIC S9(7)           COMP-3.                  
003700*                                 FAKTURANUMMER                           
003800*                                 INVOICE NO.                             
003900        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
004000*                                 FUNKTIONSGRUPP                          
004100*                                 FUNCTION GROUP                          
004200        05 IDPARTNR          PIC X(9).                                    
004300*                                 FINANCIELL KUND                         
004400*                                 FINANCIAL CUST                          
004500        05 IDSHIPM           PIC 9(7).                                    
004600*                                 SKEPPNINGSNUMMER                        
004700*                                 SHIPMENT NO                             
004800        05 KDARTRAB          PIC 9(2).                                    
004900*                                 RABATTKOD (ARTIKELPRIS)                 
005000*                                 PURCHASE DISCOUNT CODE                  
005100        05 KDPRODSL          PIC S9(3)           COMP-3.                  
005200*                                 PRODUKTSLAG                             
005300*                                 PRODUCT GROUP                           
005400        05 KDVALISO-FAKT     PIC X(3).                                    
005500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005600*                                 CURRENCY CODE BY ISO-STANDARD.          
005700        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
005800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005900*                                 AVERAGE COST FOREIGN CURRENCY           
006000        05 PRAVCOST-CORE     PIC S9(7)V9(2)      COMP-3.                  
006100*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
006200*                                 UTL.VALUTA                              
006300*                                 AV. COST PRICE OF THE CORE IN F         
006400*                                 OR. CURR.                               
006500        05 SUBTO-LINE        PIC S9(11)V9(2)     COMP-3.                  
006600*                                 TOTAL SALES AMOUNT PER LINE INC         
006700*                                 L. VAT                                  
006800        05 SUBTO-TOT         PIC S9(11)V9(2)     COMP-3.                  
006900*                                 TOTAL SALES AMOUNT INCL. VAT            
007000        05 SUNTO-LINE        PIC S9(11)V9(2)     COMP-3.                  
007100*                                 TOTAL SALES AMOUNT PER LINE EXC         
007200*                                 L. VAT                                  
007300        05 SUNTO-TOT         PIC S9(11)V9(2)     COMP-3.                  
007400*                                 TOTAL SALES AMOUNT EXCL. VAT            
007500        05 SUVAT-FAKT        PIC S9(11)V9(2)     COMP-3.                  
007600*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
007700*                                 EDITNOTA                                
007800*                                 TOTAL VAT VALUE PER INVOICE/CRE         
007900*                                 DIT                                     
008000        05 SUVAT-LINE        PIC S9(11)V9(2)     COMP-3.                  
008100*                                 MOMSVÄRDE PER FAKTURARAD                
008200*                                 VAT VALUE PER INVOICE LINE              
008300        05 TIFINDOC          PIC S9(7)           COMP-3.                  
008400*                                 DOKUMENT KLOCKSLAG (TTMMSS)             
008500*                                 DOCUMENT TIME (HHMMSS)                  
008600        05 TISKEPPN          PIC S9(7)           COMP-3.                  
008700*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
008800*                                 SHIPPING DATE    (YYMMDD)               
008900        05 VKARTNTO          PIC S9(4)V9(3)      COMP-3.                  
009000*                                 ARTIKELVIKT NETTO (KG) MED EMB          
009100*                                 PART NET WEIGHT (KG) W/ PACKAGE         
009200        05 KDLEVVIL          PIC S9              COMP-3.                  
009300*                                 LEVERANSVILLKOR                         
009400*                                 TERMS OF DELIVERY                       
009500        05 KDVALISO-BET      PIC X(3).                                    
009600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009700*                                 CURRENCY CODE BY ISO-STANDARD.          
009800        05 PRKURS-BET        PIC S9(6)V9(5)      COMP-3.                  
009900*                                 VALUTAKURS                              
010000*                                 CURRENCY EXCHANGE RATE                  
010100        05 PRKURS-FAKT       PIC S9(6)V9(5)      COMP-3.                  
010200*                                 VALUTAKURS                              
010300*                                 CURRENCY EXCHANGE RATE                  
010400        05 PRKURS-FIKTIV     PIC S9(6)V9(5)      COMP-3.                  
010500*                                 VALUTAKURS                              
010600*                                 CURRENCY EXCHANGE RATE                  
010700        05 FLCOD             PIC X.                                       
010800*                                 KONTANTBETALANDE KUND                   
010900*                                 CASH ON DELIVERY CUSTOMER               
011000        05 IDLEVNR-ART       PIC X(5).                                    
011100*                                 LEVERANTÖRNR PÅ ARTIKEL                 
011200*                                 PART SUPPLIER NUMBER                    
011300        05 KDFAKSTA-EXP      PIC X.                                       
011400*                                 DUBBELFAKTURERING STATUS                
011500*                                 STATUS CODE DOUBLE INVOICING            
011600        05 IDVAT-LEG         PIC X(17).                                   
011700*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
011800*                                 ÄLJARE                                  
011900*                                 VAT REGISTRATION LEGAL PAYER            
012000        05 IDVAT-RESP        PIC X(17).                                   
012100*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
012200*                                 G AVD                                   
012300*                                 VAT REGISTRATION RESPONSIBLE DP         
012400*                                 T                                       
012500        05 IDVAT-BET         PIC X(17).                                   
012600*                                 MOMSREGISTRERINGSNUMMER BETALAR         
012700*                                 E                                       
012800*                                 VAT REGISTRATION NUMBER PAYER           
012900        05 IDVAT-AGENT       PIC X(17).                                   
013000*                                 MOMSREGISTRERINGSNUMMER AGENT           
013100*                                 VAT REGISTRATION VAT AGENT              
013200        05 IDVAT-DDGS-RESP   PIC X(17).                                   
013300*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
013400*                                 G AVD                                   
013500*                                 VAT REGISTRATION RESPONSIBLE DP         
013600*                                 T                                       
013700*                                 VAT REG NO WHEN DIRECT DELIVERY         
013800*                                 T                                       
013900        05 SUNTO-PART-LOC    PIC S9(11)V9(2)     COMP-3.                  
014000*                                 TOTAL SALES AMOUNT PARTS EXCL.          
014100*                                 VAT                                     
014200*                                 NET TOT FOR PART DNI                    
014300*                                                                         
014400        05 SUVAT-BILLIT-TOT-PART-L                                        
014500                             PIC S9(11)V9(2)     COMP-3.                  
014600*                                 SUMMERAT NET TOT VAT FÖR ARTIKE         
014700*                                 L DNI                                   
014800*                                 TOTAL VAT VALUE PER DNI                 
014900*                                 NET TOT VAT FOR PART DNI                
015000        05 SUBTO-TOT-PART-LOC                                             
015100                             PIC S9(11)V9(2)     COMP-3.                  
015200*                                 TOTAL SALES AMOUNT INCL. VAT            
015300*                                 NET TOT DNI AMOUNT INCL. VAT            
015400        05 SUNTO-TOT-LOC     PIC S9(11)V9(2)     COMP-3.                  
015500*                                 TOTAL SALES AMOUNT EXCL. VAT            
015600*                                 NET TOT DNI                             
015700        05 SUVAT-BILLIT-TOT-LOC                                           
015800                             PIC S9(11)V9(2)     COMP-3.                  
015900*                                 SUMMERAT MOMSVÄRDE LOC                  
016000*                                 NET TOT VAT DNI                         
016100*                                 TOTAL VAT VALUE                         
016200        05 SUBTO-TOT-LOC     PIC S9(11)V9(2)     COMP-3.                  
016300*                                 TOTAL SALES AMOUNT INCL. VAT            
016400*                                 BRUTTO TOT DNI                          
016500        05 KDVALISO-LOC      PIC X(3).                                    
016600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
016700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
016800*                                 CURRENCY CODE BY ISO-STANDARD.          
016900        05 PRKURS-LOC        PIC S9(6)V9(5)      COMP-3.                  
017000*                                 VALUTAKURS                              
017100*                                 CURRENCY EXCHANGE RATE                  
017200*                                 CURRENCY RATE DNI                       
017300        05 KDTECKEN-LOC      PIC X.                                       
017400*                                 PLUS ELLER MINUS (+ -)                  
017500*                                 PLUS OR MINUS                           
017600        05 SUNTO-LOCC        PIC S9(11)V9(2)     COMP-3.                  
017700*                                 TOTAL SALES AMOUNT EXCL. VAT            
017800*                                 CURRENCY CONVERSION AMOUNT DNI          
017900        05 SUNTO-PART-RECALC PIC S9(11)V9(2)     COMP-3.                  
018000*                                 TOTAL SALES AMOUNT PARTS EXCL.          
018100*                                 VAT                                     
018200*                                 NET TOT FOR PART RECALCULATION          
018300*                                 VAT                                     
018400        05 SUVAT-BILLIT-TOT-PART-R                                        
018500                             PIC S9(11)V9(2)     COMP-3.                  
018600*                                 SUMMERAT MOMSVÄRDE                      
018700*                                 TOTAL VAT VALUE                         
018800*                                 NET TOT VAT FOR PART RECALCULAT         
018900*                                 ION                                     
019000        05 SUBTO-TOT-PART-RECALC                                          
019100                             PIC S9(11)V9(2)     COMP-3.                  
019200*                                 TOTAL SALES AMOUNT INCL. VAT            
019300*                                  BRUTTO TOT FOR PART RECALCULAT         
019400*                                 ION                                     
019500        05 SUNTO-TOT-RECALC  PIC S9(11)V9(2)     COMP-3.                  
019600*                                 TOTAL SALES AMOUNT EXCL. VAT            
019700*                                 NET TOT RECALCULATION EXCL.VAT          
019800        05 SUVAT-BILLIT-TOT-RECALC                                        
019900                             PIC S9(11)V9(2)     COMP-3.                  
020000*                                 SUMMERAT MOMSVÄRDE                      
020100*                                 TOTAL VAT VALUE                         
020200        05 SUBTO-TOT-RECALC  PIC S9(11)V9(2)     COMP-3.                  
020300*                                 TOTAL SALES AMOUNT INCL. VAT            
020400*                                 BRUTTO TOT RECALCULATION                
020500        05 KDVALISO-RECALC   PIC X(3).                                    
020600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
020700*                                 CURRENCY CODE BY ISO-STANDARD.          
020800        05 PRKURS-RECALC     PIC S9(6)V9(5)      COMP-3.                  
020900*                                 VALUTAKURS                              
021000*                                 CURRENCY EXCHANGE RATE                  
021100*                                 CURRENCY RATE RECALCULATION             
021200        05 KDTECKEN-RECALC   PIC X.                                       
021300*                                 PLUS ELLER MINUS (+ -)                  
021400*                                 PLUS OR MINUS                           
021500*                                 CURRENCY CONVERSION SIGN RECALC         
021600*                                 ULATION                                 
021700        05 SUNTO-LOCC-RECALC PIC S9(11)V9(2)     COMP-3.                  
021800*                                 TOTAL SALES AMOUNT EXCL. VAT            
021900*                                 CURRENCY CONVERSION AMOUNT RECA         
022000*                                 LCULATION                               
022100        05 PRAVCOST-BILLIT   PIC S9(7)V9(2)      COMP-3.                  
022200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
022300*                                 AVERAGE COST FOREIGN CURRENCY           
022400        05 KDVALISO-AVC      PIC X(3).                                    
022500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
022600*                                 CURRENCY CODE BY ISO-STANDARD.          
022700*                                 AVERAGE COST CURRENCY                   
022800        05 PRARTNTO          PIC S9(7)V9(2)      COMP-3.                  
022900*                                 ARTIKELPRIS NETTO                       
023000*                                 NET PRICE EACH   (FOB NET)              
023100        05 PRARTNTO-LOC      PIC S9(7)V9(2)      COMP-3.                  
023200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
023300*                                 NET PRICE EACH LOCAL CURRENCY           
023400        05 KDVALISO-NTO      PIC X(3).                                    
023500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
023600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
023700*                                 CURRENCY CODE BY ISO-STANDARD.          
023800        05 IDDC-BILLIT       PIC X(2).                                    
023900*                                 IDENTIFIERARE LAGER                     
024000*                                 WAREHOUSE IDENTIFIER                    
024100        05 KVLEVART          PIC S9(7)           COMP-3.                  
024200*                                 LEVERERAT ANTAL STYCK                   
024300*                                 DELIVERED QUANTITY                      
024400        05 KDARTURS          PIC X(2).                                    
024500*                                 ARTIKELURSPRUNGSKOD                     
024600*                                 COUNTRY OF ORIGIN                       
024700        05 FLPCOO            PIC X.                                       
024800*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
024900*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
025000*** END OF VILMAII-COPY LENGTH= 415 BYTES                                 
