000100 01  LINE-WF2011T2.                                                       
000200*                                 DOCUMENT LINE DATA                      
000300     03 LINE-IDPTYP          OCCURS 100 TIMES                             
000400                             PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 LINE-DAFINDOC        OCCURS 100 TIMES                             
000800                             PIC 9(8).                                    
000900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001000*                                 INVOICING DATE   (YYYYMMDD)             
001100     03 LINE-IDFINDOC        OCCURS 100 TIMES                             
001200                             PIC S9(9)           COMP-3.                  
001300*                                 FINANSIELLT DOKUMENT ID                 
001400*                                 FINANCIAL DOCUMENT ID                   
001500     03 LINE-IDLOPNR         OCCURS 100 TIMES                             
001600                             PIC S9(5)           COMP-3.                  
001700*                                 LÖPNUMMER          IDLOPNR-002          
001800     03 LINE-IDLEGSEL        OCCURS 100 TIMES                             
001900                             PIC X(4).                                    
002000*                                 FAKTURERANDE FÖRETAG TEX VCCS           
002100*                                 LEGAL SELLER IDENTITY                   
002200     03 LINE-BEFORMS         OCCURS 100 TIMES                             
002300                             PIC X(15).                                   
002400*                                 BENÄMNING PÅ DOKUMENTFORMAT             
002500*                                                                         
002600*                                 DESCRIPTION OF DOCUMENT FORMAT          
002700*                                                                         
002800     03 LINE-DAEXDAT         OCCURS 100 TIMES                             
002900                             PIC 9(8).                                    
003000*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
003100*                                 EXECUTION DATE (YYYYMMDD)               
003200     03 LINE-TIEXTID         OCCURS 100 TIMES                             
003300                             PIC S9(7)           COMP-3.                  
003400*                                 EXEKVERINGSTIDPUNKT                     
003500*                                 EXECUTION TIME                          
003600     03 LINE-KDVALISO        OCCURS 100 TIMES                             
003700                             PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900*                                 CURRENCY CODE BY ISO-STANDARD.          
004000     03 LINE-IDLANDX3-SEND   OCCURS 100 TIMES                             
004100                             PIC X(3).                                    
004200*                                 LANDKOD SÄNDANDE LAND                   
004300*                                 COUNTRY CODE SENDING COUNTRY            
004400     03 LINE-IDLEVNR         OCCURS 100 TIMES                             
004500                             PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 LINE-IDPARTNR        OCCURS 100 TIMES                             
004900                             PIC X(9).                                    
005000*                                 PARTNERNUMMER                           
005100*                                 PARTNER NO                              
005200     03 LINE-KDFINDOC        OCCURS 100 TIMES                             
005300                             PIC X(4).                                    
005400*                                 TYP FINANSIELLT DOKUMENT                
005500*                                 FINANCIAL DOCUMENT TYPE                 
005600     03 LINE-FLSOFT          OCCURS 100 TIMES                             
005700                             PIC X.                                       
005800*                                 FLAGGA SOFTVARA                         
005900*                                 SOFTWARE MARK                           
006000     03 LINE-FLFREE          OCCURS 100 TIMES                             
006100                             PIC X.                                       
006200*                                 GRATISFATURA                            
006300*                                 FREE INVOICE                            
006400     03 LINE-IDBREAK-1       OCCURS 100 TIMES                             
006500                             PIC X(8).                                    
006600*                                 BRYTVÄRDE                               
006700*                                 BREAK VALUE                             
006800     03 LINE-IDBREAK-2       OCCURS 100 TIMES                             
006900                             PIC X(8).                                    
007000*                                 BRYTVÄRDE                               
007100*                                 BREAK VALUE                             
007200     03 LINE-IDLANDX3-REC    OCCURS 100 TIMES                             
007300                             PIC X(3).                                    
007400*                                 LANDKOD MOTTAGANDE LAND                 
007500*                                 COUNTRY CODE RECEIVING COUNTRY          
007600     03 LINE-IDBUNDLE        OCCURS 100 TIMES                             
007700                             PIC X(15).                                   
007800*                                 BUNDLE ID                               
007900*                                 BUNDLE ID                               
008000     03 LINE-BEVOLREF        OCCURS 100 TIMES                             
008100                             PIC X(10).                                   
008200*                                 VOLVO REFERENS                          
008300*                                 VOLVO REFERENCE                         
008400     03 LINE-IDREF           OCCURS 100 TIMES                             
008500                             PIC X(15).                                   
008600*                                 REFERENS ID                             
008700*                                 REFERENCE ID                            
008800     03 LINE-DAREFDAT        OCCURS 100 TIMES                             
008900                             PIC 9(8).                                    
009000*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
009100*                                 REFERENCE DATE(YYYYMMDD)                
009200     03 LINE-IDACCNT-1       OCCURS 100 TIMES                             
009300                             PIC X(15).                                   
009400*                                 KONTOFÄLT                               
009500*                                 ACCOUNT FIELD                           
009600     03 LINE-IDACCNT-2       OCCURS 100 TIMES                             
009700                             PIC X(15).                                   
009800*                                 KONTOFÄLT                               
009900*                                 ACCOUNT FIELD                           
010000     03 LINE-IDACCNT-3       OCCURS 100 TIMES                             
010100                             PIC X(15).                                   
010200*                                 KONTOFÄLT                               
010300*                                 ACCOUNT FIELD                           
010400     03 LINE-IDACCNT-4       OCCURS 100 TIMES                             
010500                             PIC X(15).                                   
010600*                                 KONTOFÄLT                               
010700*                                 ACCOUNT FIELD                           
010800     03 LINE-IDEXCUST-1      PIC X(15).                                   
010900*                                 EXTERNT KUNDID                          
011000*                                 EXTERNAL CUSTOMER ID                    
011100     03 LINE-IDEXCUST-2      OCCURS 100 TIMES                             
011200                             PIC X(15).                                   
011300*                                 EXTERNT KUNDID                          
011400*                                 EXTERNAL CUSTOMER ID                    
011500     03 LINE-IDEXCUST-3      OCCURS 100 TIMES                             
011600                             PIC X(15).                                   
011700*                                 EXTERNT KUNDID                          
011800*                                 EXTERNAL CUSTOMER ID                    
011900     03 LINE-IDOPTION-1      OCCURS 100 TIMES                             
012000                             PIC X(15).                                   
012100*                                 BRYTBEGREPP                             
012200*                                 OPTIONAL ID                             
012300     03 LINE-IDOPTION-2      OCCURS 100 TIMES                             
012400                             PIC X(15).                                   
012500*                                 BRYTBEGREPP                             
012600*                                 OPTIONAL ID                             
012700     03 LINE-IDOPTION-3      OCCURS 100 TIMES                             
012800                             PIC X(15).                                   
012900*                                 BRYTBEGREPP                             
013000*                                 OPTIONAL ID                             
013100     03 LINE-IDOPTION-4      OCCURS 100 TIMES                             
013200                             PIC X(15).                                   
013300*                                 BRYTBEGREPP                             
013400*                                 OPTIONAL ID                             
013500     03 LINE-IDOPTION-5      OCCURS 100 TIMES                             
013600                             PIC X(15).                                   
013700*                                 BRYTBEGREPP                             
013800*                                 OPTIONAL ID                             
013900     03 LINE-IDAPPEND        OCCURS 100 TIMES                             
014000                             PIC X(8).                                    
014100*                                 APPENDIXVÄRDE                           
014200*                                 APPENDIX ITEM                           
014300     03 LINE-IDARTNR-FINANCE OCCURS 100 TIMES                             
014400                             PIC X(50).                                   
014500*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
014600*                                 UK                                      
014700*                                 PART NUMBER FOR FINANCIAL USE           
014800     03 LINE-BEART           OCCURS 100 TIMES                             
014900                             PIC X(25).                                   
015000*                                 ARTIKELBENÄMNING                        
015100*                                 PART DESCRIPTION                        
015200     03 LINE-IDSTATNR        OCCURS 100 TIMES                             
015300                             PIC S9(9)           COMP-3.                  
015400*                                 STATISTISKT NUMMER                      
015500*                                 1 = NORSKT                              
015600*                                 2 = ENGELSKT                            
015700*                                 3 = BELGISKT                            
015800*                                 4 = PERUANSKT                           
015900*                                 5 = SVENSKT                             
016000*                                 6 =                                     
016100*                                 STATISTICAL NO.                         
016200     03 LINE-VKORDBTO-KOLLI  OCCURS 100 TIMES                             
016300                             PIC S9(6)V9(1)      COMP-3.                  
016400*                                 ORDERVIKT BRUTTO PER KOLLI              
016500*                                 ORDER WEIGHT GROSS PER CASE             
016600     03 LINE-VKARTNTO        OCCURS 100 TIMES                             
016700                             PIC S9(4)V9(3)      COMP-3.                  
016800*                                 ARTIKELVIKT NETTO (KG)                  
016900*                                 PART NET WEIGHT (KG)                    
017000     03 LINE-PRARTBTO        OCCURS 100 TIMES                             
017100                             PIC S9(7)V9(2)      COMP-3.                  
017200*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
017300*                                 GROSS SALES PRICE (SEK)                 
017400     03 LINE-PRARTNTO        OCCURS 100 TIMES                             
017500                             PIC S9(7)V9(2)      COMP-3.                  
017600*                                 ARTIKELPRIS NETTO                       
017700*                                 NET PRICE EACH   (FOB NET)              
017800     03 LINE-REARTRAB        OCCURS 100 TIMES                             
017900                             PIC S9(2)V9(2)      COMP-3.                  
018000*                                 ARTIKELRABATT                           
018100*                                 PARTS DISCOUNT PERCENT                  
018200     03 LINE-KVBEART         OCCURS 100 TIMES                             
018300                             PIC S9(7)           COMP-3.                  
018400*                                 BESTÄLLT ANTAL STYCKEN                  
018500*                                 ORDERED QUANTITY                        
018600     03 LINE-KVLEVART        OCCURS 100 TIMES                             
018700                             PIC S9(7)           COMP-3.                  
018800*                                 LEVERERAT ANTAL STYCK                   
018900*                                 DELIVERED QUANTITY                      
019000     03 LINE-FLSPECPR        OCCURS 100 TIMES                             
019100                             PIC X.                                       
019200*                                 SPECIALPRISFLAGGA                       
019300*                                 SPECIAL PRICE FLAG                      
019400     03 LINE-REVAT           OCCURS 100 TIMES                             
019500                             PIC S9(3)V9(2)      COMP-3.                  
019600*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
019700*                                 VAT FACTOR                              
019800     03 LINE-KDARTURS        OCCURS 100 TIMES                             
019900                             PIC X(2).                                    
020000*                                 ARTIKELURSPRUNGSKOD                     
020100*                                 COUNTRY OF ORIGIN                       
020200     03 LINE-KDANMORS        OCCURS 100 TIMES                             
020300                             PIC X(2).                                    
020400*                                 ORSAK TILL LEVERANSANMÄRKNING           
020500*                                 DISCREPANCY REPORT REASON CODE          
020600     03 LINE-IDFAKREF        OCCURS 100 TIMES                             
020700                             PIC S9(9)           COMP-3.                  
020800*                                 URSPRUNGLIGT FAKTURANUMMER              
020900*                                 ORIGINAL INVOICE NUMBER                 
021000     03 LINE-DAFAKREF        OCCURS 100 TIMES                             
021100                             PIC 9(8).                                    
021200*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
021300*                                 MMDD)                                   
021400*                                 ORIGINAL INVOICING DATE (YYYYMM         
021500*                                 DD)                                     
021600     03 LINE-IDDC            OCCURS 100 TIMES                             
021700                             PIC X(2).                                    
021800*                                 IDENTIFIERARE LAGER                     
021900*                                 WAREHOUSE IDENTIFIER                    
022000     03 LINE-KDFRAKT         OCCURS 100 TIMES                             
022100                             PIC S9(3)           COMP-3.                  
022200*                                 FRAKTSÄTT DC TILL KUND                  
022300*                                 FREIGHT CODE                            
022400     03 LINE-BELEVVIL        OCCURS 100 TIMES                             
022500                             PIC X(35).                                   
022600*                                 LEVERANSVILLKOR                         
022700*                                 DELIVERY TERMS                          
022800     03 LINE-SUNTO           OCCURS 100 TIMES                             
022900                             PIC S9(11)V9(2)     COMP-3.                  
023000*                                 TOTAL SALES AMOUNT EXCL. VAT            
023100     03 LINE-SUBTO           OCCURS 100 TIMES                             
023200                             PIC S9(11)V9(2)     COMP-3.                  
023300*                                 TOTAL SALES AMOUNT INCL. VAT            
023400     03 LINE-SUVAT-BILLIT    OCCURS 100 TIMES                             
023500                             PIC S9(11)V9(2)     COMP-3.                  
023600*                                 SUMMERAT MOMSVÄRDE PER RAD              
023700*                                 TOTAL VAT VALUE PER LINE                
023800     03 LINE-KDVAT           OCCURS 100 TIMES                             
023900                             PIC X(2).                                    
024000*                                 MOMSKOD                                 
024100*                                 VAT CODE                                
024200     03 LINE-BEVAT           OCCURS 100 TIMES                             
024300                             PIC X(50).                                   
024400*                                 MOMSKODSBENÄMNING R3                    
024500*                                 VAT CODE DESCRIPTION R3                 
024600     03 LINE-IDARTNR-CNTRL   OCCURS 100 TIMES                             
024700                             PIC X(2).                                    
024800*                                 KONTROLLSIFFRA FÖR ARTIKELNUMME         
024900*                                 R                                       
025000*                                 CHECK FIGURE FOR PART NUMBER            
025100*                                                                         
025200     03 LINE-FLPCOO          OCCURS 100 TIMES                             
025300                             PIC X.                                       
025400*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
025500*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
025600     03 LINE-IDLEVNR-ART     OCCURS 100 TIMES                             
025700                             PIC X(5).                                    
025800*                                 LEVERANTÖRNR PÅ ARTIKEL                 
025900*                                 PART SUPPLIER NUMBER                    
026000*** END OF VILMAII-COPY LENGTH= 56615 BYTES                               
