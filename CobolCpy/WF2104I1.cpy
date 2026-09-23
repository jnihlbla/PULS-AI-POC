000100 01  WF2104I1.                                                            
000200*                                 FEEDBACK TO SYSTEM PULS-W476 (O         
000300*                                 RDER)                                   
000400     03 DAEXDAT              PIC 9(8).                                    
000500*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000600*                                 EXECUTION DATE (YYYYMMDD)               
000700     03 TIEXTID              PIC 9(6).                                    
000800*                                 EXEKVERINGSTIDPUNKT                     
000900*                                 EXECUTION TIME                          
001000     03 DAFINDOC             PIC 9(8).                                    
001100*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001200*                                 INVOICING DATE   (YYYYMMDD)             
001300     03 IDFINDOC             PIC 9(9).                                    
001400*                                 FINANSIELLT DOKUMENT ID                 
001500*                                 FINANCIAL DOCUMENT ID                   
001600     03 KDVALISO             PIC X(3).                                    
001700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001800*                                 CURRENCY CODE BY ISO-STANDARD.          
001900     03 IDPARTNR             PIC X(9).                                    
002000*                                 FINANCIELL KUND                         
002100*                                 FINANCIAL CUST                          
002200     03 SUNTO-PART           PIC 9(11)V9(2).                              
002300*                                 TOTAL SALES AMOUNT PARTS EXCL.          
002400*                                 VAT                                     
002500     03 SUVAT-BILLIT-TOT     PIC 9(11)V9(2).                              
002600*                                 SUMMERAT MOMSVÄRDE                      
002700*                                 TOTAL VAT VALUE                         
002800     03 SUBTO-TOT            PIC 9(11)V9(2).                              
002900*                                 TOTAL SALES AMOUNT INCL. VAT            
003000     03 IDBUNDLE             PIC X(15).                                   
003100*                                 BUNDLE ID                               
003200*                                 BUNDLE ID                               
003300     03 IDEXCUST-1           PIC X(15).                                   
003400*                                 EXTERNT KUNDID                          
003500*                                 EXTERNAL CUSTOMER ID                    
003600     03 IDEXCUST-2           PIC X(15).                                   
003700*                                 EXTERNT KUNDID                          
003800*                                 EXTERNAL CUSTOMER ID                    
003900     03 IDOPTION-1           PIC X(15).                                   
004000*                                 BRYTBEGREPP                             
004100*                                 OPTIONAL ID                             
004200     03 IDOPTION-2           PIC X(15).                                   
004300*                                 BRYTBEGREPP                             
004400*                                 OPTIONAL ID                             
004500     03 IDOPTION-3           PIC X(15).                                   
004600*                                 BRYTBEGREPP                             
004700*                                 OPTIONAL ID                             
004800     03 IDOPTION-4           PIC X(15).                                   
004900*                                 BRYTBEGREPP                             
005000*                                 OPTIONAL ID                             
005100     03 IDOPTION-5           PIC X(15).                                   
005200*                                 BRYTBEGREPP                             
005300*                                 OPTIONAL ID                             
005400     03 IDARTNR-FINANCE      PIC X(50).                                   
005500*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
005600*                                 UK                                      
005700*                                 PART NUMBER FOR FINANCIAL USE           
005800     03 BEART                PIC X(25).                                   
005900*                                 ARTIKELBENÄMNING                        
006000*                                 PART DESCRIPTION                        
006100     03 VKARTNTO             PIC 9(4)V9(3).                               
006200*                                 ARTIKELVIKT NETTO (KG) MED EMB          
006300*                                 PART NET WEIGHT (KG) W/ PACKAGE         
006400     03 SUBTO                PIC 9(11)V9(2).                              
006500*                                 TOTAL SALES AMOUNT INCL. VAT            
006600     03 SUNTO                PIC 9(11)V9(2).                              
006700*                                 TOTAL SALES AMOUNT EXCL. VAT            
006800     03 SUVAT-BILLIT         PIC 9(11)V9(2).                              
006900*                                 SUMMERAT MOMSVÄRDE PER RAD              
007000*                                 TOTAL VAT VALUE PER LINE                
007100     03 KDVALISO-BET         PIC X(3).                                    
007200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007300*                                 CURRENCY CODE BY ISO-STANDARD.          
007400     03 PRKURS-BET           PIC 9(6)V9(5).                               
007500*                                 VALUTAKURS                              
007600*                                 CURRENCY EXCHANGE RATE                  
007700     03 PRKURS               PIC 9(6)V9(5).                               
007800*                                 VALUTAKURS                              
007900*                                 CURRENCY EXCHANGE RATE                  
008000     03 PRKURS-FAKBET        PIC 9(6)V9(5).                               
008100*                                 VALUTAKURS                              
008200*                                 CURRENCY EXCHANGE RATE                  
008300     03 KDVALISO-SND         PIC X(3).                                    
008400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008500*                                 CURRENCY CODE BY ISO-STANDARD.          
008600     03 PRKURS-SND           PIC 9(6)V9(5).                               
008700*                                 VALUTAKURS                              
008800*                                 CURRENCY EXCHANGE RATE                  
008900     03 IDLEVNR-ART          PIC X(5).                                    
009000*                                 LEVERANTÖRNR PÅ ARTIKEL                 
009100*                                 PART SUPPLIER NUMBER                    
009200     03 IDVAT-LEG            PIC X(17).                                   
009300*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
009400*                                 ÄLJARE                                  
009500*                                 VAT REGISTRATION LEGAL PAYER            
009600     03 IDVAT-RESP           PIC X(17).                                   
009700*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
009800*                                 G AVD                                   
009900*                                 VAT REGISTRATION RESPONSIBLE DP         
010000*                                 T                                       
010100     03 IDVAT-BET            PIC X(17).                                   
010200*                                 MOMSREGISTRERINGSNUMMER BETALAR         
010300*                                 E                                       
010400*                                 VAT REGISTRATION NUMBER PAYER           
010500     03 IDVAT-AGENT          PIC X(17).                                   
010600*                                 MOMSREGISTRERINGSNUMMER AGENT           
010700*                                 VAT REGISTRATION VAT AGENT              
010800     03 IDVAT-DDGS-RESP      PIC X(17).                                   
010900*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
011000*                                 G AVD                                   
011100*                                 VAT REGISTRATION RESPONSIBLE DP         
011200*                                 T                                       
011300*                                 VAT REG NO WHEN DIRECT DELIVERY         
011400*                                 T                                       
011500     03 SUNTO-PART-LOC       PIC 9(11)V9(2).                              
011600*                                 TOTAL SALES AMOUNT PARTS EXCL.          
011700*                                 VAT                                     
011800*                                 NET TOT FOR PART DNI                    
011900*                                                                         
012000     03 SUVAT-BILLIT-TOT-PART-L                                           
012100                             PIC 9(11)V9(2).                              
012200*                                 SUMMERAT NET TOT VAT FÖR ARTIKE         
012300*                                 L DNI                                   
012400*                                 TOTAL VAT VALUE PER DNI                 
012500*                                 NET TOT VAT FOR PART DNI                
012600     03 SUBTO-TOT-PART-LOC   PIC 9(11)V9(2).                              
012700*                                 TOTAL SALES AMOUNT INCL. VAT            
012800*                                 NET TOT DNI AMOUNT INCL. VAT            
012900     03 SUNTO-TOT-LOC        PIC 9(11)V9(2).                              
013000*                                 TOTAL SALES AMOUNT EXCL. VAT            
013100*                                 NET TOT DNI                             
013200     03 SUVAT-BILLIT-TOT-LOC PIC 9(11)V9(2).                              
013300*                                 SUMMERAT MOMSVÄRDE LOC                  
013400*                                 NET TOT VAT DNI                         
013500*                                 TOTAL VAT VALUE                         
013600     03 SUBTO-TOT-LOC        PIC 9(11)V9(2).                              
013700*                                 TOTAL SALES AMOUNT INCL. VAT            
013800*                                 BRUTTO TOT DNI                          
013900     03 KDVALISO-LOC         PIC X(3).                                    
014000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
014100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
014200*                                 CURRENCY CODE BY ISO-STANDARD.          
014300     03 PRKURS-LOC           PIC 9(6)V9(5).                               
014400*                                 VALUTAKURS                              
014500*                                 CURRENCY EXCHANGE RATE                  
014600*                                 CURRENCY RATE DNI                       
014700     03 SUNTO-LOCC           PIC 9(11)V9(2).                              
014800*                                 TOTAL SALES AMOUNT EXCL. VAT            
014900*                                 CURRENCY CONVERSION AMOUNT DNI          
015000     03 KDSIGN-LOCC          PIC X.                                       
015100*                                 DATA SIGN                               
015200*                                 DATA SIGN                               
015300     03 SUNTO-PART-RECALC    PIC 9(11)V9(2).                              
015400*                                 TOTAL SALES AMOUNT PARTS EXCL.          
015500*                                 VAT                                     
015600*                                 NET TOT FOR PART RECALCULATION          
015700*                                 VAT                                     
015800     03 SUVAT-BILLIT-TOT-PART-R                                           
015900                             PIC 9(11)V9(2).                              
016000*                                 SUMMERAT MOMSVÄRDE                      
016100*                                 TOTAL VAT VALUE                         
016200*                                 NET TOT VAT FOR PART RECALCULAT         
016300*                                 ION                                     
016400     03 SUBTO-TOT-PART-RECALC                                             
016500                             PIC 9(11)V9(2).                              
016600*                                 TOTAL SALES AMOUNT INCL. VAT            
016700*                                  BRUTTO TOT FOR PART RECALCULAT         
016800*                                 ION                                     
016900     03 SUNTO-TOT-RECALC     PIC 9(11)V9(2).                              
017000*                                 TOTAL SALES AMOUNT EXCL. VAT            
017100*                                 NET TOT RECALCULATION EXCL.VAT          
017200     03 SUVAT-BILLIT-TOT-RECALC                                           
017300                             PIC 9(11)V9(2).                              
017400*                                 SUMMERAT MOMSVÄRDE                      
017500*                                 TOTAL VAT VALUE                         
017600     03 SUBTO-TOT-RECALC     PIC 9(11)V9(2).                              
017700*                                 TOTAL SALES AMOUNT INCL. VAT            
017800*                                 BRUTTO TOT RECALCULATION                
017900     03 KDVALISO-RECALC      PIC X(3).                                    
018000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
018100*                                 CURRENCY CODE BY ISO-STANDARD.          
018200     03 PRKURS-RECALC        PIC 9(6)V9(5).                               
018300*                                 VALUTAKURS                              
018400*                                 CURRENCY EXCHANGE RATE                  
018500*                                 CURRENCY RATE RECALCULATION             
018600     03 SUNTO-RECALC         PIC 9(11)V9(2).                              
018700*                                 TOTAL SALES AMOUNT EXCL. VAT            
018800*                                 CURRENCY CONVERSION AMOUNT DNI          
018900     03 KDSIGN-RECALC        PIC X.                                       
019000*                                 DATA SIGN                               
019100*                                 DATA SIGN                               
019200     03 PRAVCOST             PIC Z(6)9.9(2).                              
019300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
019400*                                 AVERAGE COST FOREIGN CURRENCY           
019500     03 KDVALISO-AVC         PIC X(3).                                    
019600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
019700*                                 CURRENCY CODE BY ISO-STANDARD.          
019800*                                 AVERAGE COST CURRENCY                   
019900     03 PRARTNTO             PIC Z(6)9.9(2).                              
020000*                                 ARTIKELPRIS NETTO                       
020100*                                 NET PRICE EACH   (FOB NET)              
020200     03 KDVALISO-NTO         PIC X(3).                                    
020300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
020400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
020500*                                 CURRENCY CODE BY ISO-STANDARD.          
020600     03 PRARTNTO-LOC         PIC Z(6)9.9(2).                              
020700*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
020800*                                 NET PRICE EACH LOCAL CURRENCY           
020900     03 IDDC                 PIC X(2).                                    
021000*                                 IDENTIFIERARE LAGER                     
021100*                                 WAREHOUSE IDENTIFIER                    
021200     03 KVLEVART             PIC Z(6)9.                                   
021300*                                 LEVERERAT ANTAL STYCK                   
021400*                                 DELIVERED QUANTITY                      
021500     03 KDARTURS             PIC X(2).                                    
021600*                                 ARTIKELURSPRUNGSKOD                     
021700*                                 COUNTRY OF ORIGIN                       
021800     03 FLPCOO               PIC X.                                       
021900*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
022000*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
022100*** END OF VILMAII-COPY LENGTH= 723 BYTES                                 
