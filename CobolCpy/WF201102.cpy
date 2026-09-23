000100 01  LINE-WF201102.                                                       
000200*                                 DOCUMENT LINE DATA                      
000300     03 LINE-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 LINE-DAFINDOC        PIC 9(8).                                    
000700*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
000800*                                 INVOICING DATE   (YYYYMMDD)             
000900     03 LINE-IDFINDOC        PIC S9(9)           COMP-3.                  
001000*                                 FINANSIELLT DOKUMENT ID                 
001100*                                 FINANCIAL DOCUMENT ID                   
001200     03 LINE-IDLOPNR         PIC S9(5)           COMP-3.                  
001300*                                 LÖPNUMMER          IDLOPNR-002          
001400     03 LINE-IDLEGSEL        PIC X(4).                                    
001500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001600*                                 LEGAL SELLER IDENTITY                   
001700     03 LINE-BEFORMS         PIC X(15).                                   
001800*                                 BENÄMNING PÅ DOKUMENTFORMAT             
001900*                                                                         
002000*                                 DESCRIPTION OF DOCUMENT FORMAT          
002100*                                                                         
002200     03 LINE-DAEXDAT         PIC 9(8).                                    
002300*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
002400*                                 EXECUTION DATE (YYYYMMDD)               
002500     03 LINE-TIEXTID         PIC S9(7)           COMP-3.                  
002600*                                 EXEKVERINGSTIDPUNKT                     
002700*                                 EXECUTION TIME                          
002800     03 LINE-KDVALISO        PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000*                                 CURRENCY CODE BY ISO-STANDARD.          
003100     03 LINE-IDLANDX3-SEND   PIC X(3).                                    
003200*                                 LANDKOD SÄNDANDE LAND                   
003300*                                 COUNTRY CODE SENDING COUNTRY            
003400     03 LINE-IDLEVNR         PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003700     03 LINE-IDPARTNR        PIC X(9).                                    
003800*                                 FINANCIELL KUND                         
003900*                                 FINANCIAL CUST                          
004000     03 LINE-KDFINDOC        PIC X(4).                                    
004100*                                 TYP FINANSIELLT DOKUMENT                
004200*                                 FINANCIAL DOCUMENT TYPE                 
004300     03 LINE-FLSOFT          PIC X.                                       
004400*                                 FLAGGA SOFTVARA                         
004500*                                 SOFTWARE MARK                           
004600     03 LINE-FLFREE          PIC X.                                       
004700*                                 GRATISFATURA                            
004800*                                 FREE INVOICE                            
004900     03 LINE-IDBREAK-1       PIC X(8).                                    
005000*                                 BRYTVÄRDE                               
005100*                                 BREAK VALUE                             
005200     03 LINE-IDBREAK-2       PIC X(8).                                    
005300*                                 BRYTVÄRDE                               
005400*                                 BREAK VALUE                             
005500     03 LINE-IDLANDX3-REC    PIC X(3).                                    
005600*                                 LANDKOD MOTTAGANDE LAND                 
005700*                                 COUNTRY CODE RECEIVING COUNTRY          
005800     03 LINE-IDBUNDLE        PIC X(15).                                   
005900*                                 BUNDLE ID                               
006000*                                 BUNDLE ID                               
006100     03 LINE-BEVOLREF        PIC X(10).                                   
006200*                                 VOLVO REFERENS                          
006300*                                 VOLVO REFERENCE                         
006400     03 LINE-IDREF           PIC X(15).                                   
006500*                                 REFERENS ID                             
006600*                                 REFERENCE ID                            
006700     03 LINE-DAREFDAT        PIC 9(8).                                    
006800*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
006900*                                 REFERENCE DATE(YYYYMMDD)                
007000     03 LINE-IDACCNT-1       PIC X(15).                                   
007100*                                 KONTOFÄLT                               
007200*                                 ACCOUNT FIELD                           
007300     03 LINE-IDACCNT-2       PIC X(15).                                   
007400*                                 KONTOFÄLT                               
007500*                                 ACCOUNT FIELD                           
007600     03 LINE-IDACCNT-3       PIC X(15).                                   
007700*                                 KONTOFÄLT                               
007800*                                 ACCOUNT FIELD                           
007900     03 LINE-IDACCNT-4       PIC X(15).                                   
008000*                                 KONTOFÄLT                               
008100*                                 ACCOUNT FIELD                           
008200     03 LINE-IDEXCUST-1      PIC X(15).                                   
008300*                                 EXTERNT KUNDID                          
008400*                                 EXTERNAL CUSTOMER ID                    
008500     03 LINE-IDEXCUST-2      PIC X(15).                                   
008600*                                 EXTERNT KUNDID                          
008700*                                 EXTERNAL CUSTOMER ID                    
008800     03 LINE-IDEXCUST-3      PIC X(15).                                   
008900*                                 EXTERNT KUNDID                          
009000*                                 EXTERNAL CUSTOMER ID                    
009100     03 LINE-IDOPTION-1      PIC X(15).                                   
009200*                                 BRYTBEGREPP                             
009300*                                 OPTIONAL ID                             
009400     03 LINE-IDOPTION-2      PIC X(15).                                   
009500*                                 BRYTBEGREPP                             
009600*                                 OPTIONAL ID                             
009700     03 LINE-IDOPTION-3      PIC X(15).                                   
009800*                                 BRYTBEGREPP                             
009900*                                 OPTIONAL ID                             
010000     03 LINE-IDOPTION-4      PIC X(15).                                   
010100*                                 BRYTBEGREPP                             
010200*                                 OPTIONAL ID                             
010300     03 LINE-IDOPTION-5      PIC X(15).                                   
010400*                                 BRYTBEGREPP                             
010500*                                 OPTIONAL ID                             
010600     03 LINE-IDAPPEND        PIC X(8).                                    
010700*                                 APPENDIXVÄRDE                           
010800*                                 APPENDIX ITEM                           
010900     03 LINE-IDARTNR-FINANCE PIC X(50).                                   
011000*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
011100*                                 UK                                      
011200*                                 PART NUMBER FOR FINANCIAL USE           
011300     03 LINE-BEART           PIC X(25).                                   
011400*                                 ARTIKELBENÄMNING                        
011500*                                 PART DESCRIPTION                        
011600     03 LINE-IDSTATNR        PIC S9(9)           COMP-3.                  
011700*                                 STATISTISKT NUMMER                      
011800*                                 1 = NORSKT                              
011900*                                 2 = ENGELSKT                            
012000*                                 3 = BELGISKT                            
012100*                                 4 = PERUANSKT                           
012200*                                 5 = SVENSKT                             
012300*                                 6 =                                     
012400*                                 STATISTICAL NO.                         
012500     03 LINE-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
012600*                                 ORDERVIKT BRUTTO PER KOLLI              
012700*                                 ORDER WEIGHT GROSS PER CASE             
012800     03 LINE-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
012900*                                 ARTIKELVIKT NETTO (KG) MED EMB          
013000*                                 PART NET WEIGHT (KG) W/ PACKAGE         
013100     03 LINE-PRARTBTO        PIC S9(7)V9(2)      COMP-3.                  
013200*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
013300*                                 GROSS SALES PRICE (SEK)                 
013400     03 LINE-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
013500*                                 ARTIKELPRIS NETTO                       
013600*                                 NET PRICE EACH   (FOB NET)              
013700     03 LINE-REARTRAB        PIC S9(2)V9(2)      COMP-3.                  
013800*                                 ARTIKELRABATT                           
013900*                                 PARTS DISCOUNT PERCENT                  
014000     03 LINE-KVBEART         PIC S9(7)           COMP-3.                  
014100*                                 BESTÄLLT ANTAL STYCKEN                  
014200*                                 ORDERED QUANTITY                        
014300     03 LINE-KVLEVART        PIC S9(7)           COMP-3.                  
014400*                                 LEVERERAT ANTAL STYCK                   
014500*                                 DELIVERED QUANTITY                      
014600     03 LINE-FLSPECPR        PIC X.                                       
014700*                                 SPECIALPRISFLAGGA                       
014800*                                 SPECIAL PRICE FLAG                      
014900     03 LINE-REVAT           PIC S9(3)V9(2)      COMP-3.                  
015000*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
015100*                                 VAT FACTOR                              
015200     03 LINE-KDARTURS        PIC X(2).                                    
015300*                                 ARTIKELURSPRUNGSKOD                     
015400*                                 COUNTRY OF ORIGIN                       
015500     03 LINE-KDANMORS        PIC X(2).                                    
015600*                                 ORSAK TILL LEVERANSANMÄRKNING           
015700*                                 DISCREPANCY REPORT REASON CODE          
015800     03 LINE-IDFAKREF        PIC S9(9)           COMP-3.                  
015900*                                 URSPRUNGLIGT FAKTURANUMMER              
016000*                                 ORIGINAL INVOICE NUMBER                 
016100     03 LINE-DAFAKREF        PIC 9(8).                                    
016200*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
016300*                                 MMDD)                                   
016400*                                 ORIGINAL INVOICING DATE (YYYYMM         
016500*                                 DD)                                     
016600     03 LINE-IDDC            PIC X(2).                                    
016700*                                 IDENTIFIERARE LAGER                     
016800*                                 WAREHOUSE IDENTIFIER                    
016900     03 LINE-KDFRAKT         PIC S9(3)           COMP-3.                  
017000*                                 FRAKTSÄTT DC TILL KUND                  
017100*                                 FREIGHT CODE                            
017200     03 LINE-BELEVVIL        PIC X(35).                                   
017300*                                 LEVERANSVILLKOR                         
017400*                                 DELIVERY TERMS                          
017500     03 LINE-SUNTO           PIC S9(11)V9(2)     COMP-3.                  
017600*                                 TOTAL SALES AMOUNT EXCL. VAT            
017700     03 LINE-SUBTO           PIC S9(11)V9(2)     COMP-3.                  
017800*                                 TOTAL SALES AMOUNT INCL. VAT            
017900     03 LINE-SUVAT-BILLIT    PIC S9(11)V9(2)     COMP-3.                  
018000*                                 SUMMERAT MOMSVÄRDE PER RAD              
018100*                                 TOTAL VAT VALUE PER LINE                
018200     03 LINE-KDVAT           PIC X(2).                                    
018300*                                 MOMSKOD                                 
018400*                                 VAT CODE                                
018500     03 LINE-BEVAT           PIC X(50).                                   
018600*                                 MOMSKODSBENÄMNING R3                    
018700*                                 VAT CODE DESCRIPTION R3                 
018800     03 LINE-IDARTNR-CNTRL   PIC X(2).                                    
018900*                                 KONTROLLSIFFRA FÖR ARTIKELNUMME         
019000*                                 R                                       
019100*                                 CHECK FIGURE FOR PART NUMBER            
019200*                                                                         
019300     03 LINE-FLCURRND        PIC X.                                       
019400*                                 ANGER OM BELOPP AVRUNDAS                
019500*                                 INDICATES IF AMOUNT ROUNDED             
019600     03 LINE-FLPCOO          PIC X.                                       
019700*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
019800*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
019900     03 LINE-IDLEVNR-ART     PIC X(5).                                    
020000*                                 LEVERANTÖRNR PÅ ARTIKEL                 
020100*                                 PART SUPPLIER NUMBER                    
020200     03 LINE-IDTRACK-1       PIC X(25).                                   
020300*                                 TRACKING ID FROM CUSTOMS                
020400*                                 CUSTOMS TRACKING ID                     
020500     03 LINE-KVANT-TRACK-1   PIC S9(7)           COMP-3.                  
020600*                                 TILLHÖR AV EN VISS ARTIKEL              
020700*                                 QUANTITY WITH RESPECT TO TRACK-         
020800*                                 ID                                      
020900     03 LINE-IDTRACK-2       PIC X(25).                                   
021000*                                 TRACKING ID FROM CUSTOMS                
021100*                                 CUSTOMS TRACKING ID                     
021200     03 LINE-KVANT-TRACK-2   PIC S9(7)           COMP-3.                  
021300*                                 TILLHÖR AV EN VISS ARTIKEL              
021400*                                 QUANTITY WITH RESPECT TO TRACK-         
021500*                                 ID                                      
021600     03 LINE-IDTRACK-3       PIC X(25).                                   
021700*                                 TRACKING ID FROM CUSTOMS                
021800*                                 CUSTOMS TRACKING ID                     
021900     03 LINE-KVANT-TRACK-3   PIC S9(7)           COMP-3.                  
022000*                                 TILLHÖR AV EN VISS ARTIKEL              
022100*                                 QUANTITY WITH RESPECT TO TRACK-         
022200*                                 ID                                      
022300     03 LINE-IDTRACK-4       PIC X(25).                                   
022400*                                 TRACKING ID FROM CUSTOMS                
022500*                                 CUSTOMS TRACKING ID                     
022600     03 LINE-KVANT-TRACK-4   PIC S9(7)           COMP-3.                  
022700*                                 TILLHÖR AV EN VISS ARTIKEL              
022800*                                 QUANTITY WITH RESPECT TO TRACK-         
022900*                                 ID                                      
023000     03 LINE-IDTRACK-5       PIC X(25).                                   
023100*                                 TRACKING ID FROM CUSTOMS                
023200*                                 CUSTOMS TRACKING ID                     
023300     03 LINE-KVANT-TRACK-5   PIC S9(7)           COMP-3.                  
023400*                                 TILLHÖR AV EN VISS ARTIKEL              
023500*                                 QUANTITY WITH RESPECT TO TRACK-         
023600*                                 ID                                      
023700*** END OF VILMAII-COPY LENGTH= 727 BYTES                                 
