000100 01  T01SLINT.                                                            
000200*                                 MULTIFETCH TABELL TILL T01SLIN          
000300     03 IDLEGSEL             OCCURS 100 TIMES                             
000400                             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 DAEXDAT              OCCURS 100 TIMES                             
000800                             PIC X(8).                                    
000900     03 TIEXTID              OCCURS 100 TIMES                             
001000                             PIC S9(7)           COMP-3.                  
001100*                                 EXEKVERINGSTIDPUNKT                     
001200*                                 EXECUTION TIME                          
001300     03 KDVALISO             OCCURS 100 TIMES                             
001400                             PIC X(3).                                    
001500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001600*                                 CURRENCY CODE BY ISO-STANDARD.          
001700     03 IDLANDX3-SEND        OCCURS 100 TIMES                             
001800                             PIC X(3).                                    
001900*                                 LANDKOD SÄNDANDE LAND                   
002000*                                 COUNTRY CODE SENDING COUNTRY            
002100     03 IDLEVNR              OCCURS 100 TIMES                             
002200                             PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500     03 IDPARTNR             OCCURS 100 TIMES                             
002600                             PIC X(9).                                    
002700*                                 FINANCIELL KUND                         
002800*                                 FINANCIAL CUST                          
002900     03 KDFINDOC             OCCURS 100 TIMES                             
003000                             PIC X(4).                                    
003100*                                 TYP FINANSIELLT DOKUMENT                
003200*                                 FINANCIAL DOCUMENT TYPE                 
003300     03 FLSOFT               OCCURS 100 TIMES                             
003400                             PIC X.                                       
003500*                                 FLAGGA SOFTVARA                         
003600*                                 SOFTWARE MARK                           
003700     03 FLFREE               OCCURS 100 TIMES                             
003800                             PIC X.                                       
003900*                                 GRATISFATURA                            
004000*                                 FREE INVOICE                            
004100     03 FLPRIV               OCCURS 100 TIMES                             
004200                             PIC X.                                       
004300*                                 KÖPARE ÄR EN PRIVATPERSON               
004400*                                 PURCHASER IS A PRIVATE PERSON           
004500     03 IDBREAK-1            OCCURS 100 TIMES                             
004600                             PIC X(8).                                    
004700*                                 BRYTVÄRDE                               
004800*                                 BREAK VALUE                             
004900     03 IDBREAK-2            OCCURS 100 TIMES                             
005000                             PIC X(8).                                    
005100*                                 BRYTVÄRDE                               
005200*                                 BREAK VALUE                             
005300     03 IDLOPNR              OCCURS 100 TIMES                             
005400                             PIC S9(5)           COMP-3.                  
005500*                                 LÖPNUMMER          IDLOPNR-002          
005600     03 IDAPPEND             OCCURS 100 TIMES                             
005700                             PIC X(8).                                    
005800*                                 APPENDIXVÄRDE                           
005900*                                 APPENDIX ITEM                           
006000     03 IDREF                OCCURS 100 TIMES                             
006100                             PIC X(15).                                   
006200*                                 REFERENS ID                             
006300*                                 REFERENCE ID                            
006400     03 DAREFDAT             OCCURS 100 TIMES                             
006500                             PIC 9(8).                                    
006600*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
006700*                                 REFERENCE DATE(YYYYMMDD)                
006800     03 IDREFRAD             OCCURS 100 TIMES                             
006900                             PIC S9(5)           COMP-3.                  
007000*                                 REFERENSRADSNR                          
007100*                                 REFERENCE LINE NUMBER                   
007200     03 BEVOLREF             OCCURS 100 TIMES                             
007300                             PIC X(10).                                   
007400*                                 VOLVO REFERENS                          
007500*                                 VOLVO REFERENCE                         
007600     03 IDSEQ-1              OCCURS 100 TIMES                             
007700                             PIC X(8).                                    
007800*                                 SEKVENSVÄRDE                            
007900*                                 SEQUENCE VALUE                          
008000     03 IDSEQ-2              OCCURS 100 TIMES                             
008100                             PIC X(8).                                    
008200*                                 SEKVENSVÄRDE                            
008300*                                 SEQUENCE VALUE                          
008400     03 IDSEQ-3              OCCURS 100 TIMES                             
008500                             PIC X(8).                                    
008600*                                 SEKVENSVÄRDE                            
008700*                                 SEQUENCE VALUE                          
008800     03 IDLANDX3-REC         OCCURS 100 TIMES                             
008900                             PIC X(3).                                    
009000*                                 LANDKOD MOTTAGANDE LAND                 
009100*                                 COUNTRY CODE RECEIVING COUNTRY          
009200     03 IDEXCUST-1           OCCURS 100 TIMES                             
009300                             PIC X(15).                                   
009400*                                 EXTERNT KUNDID                          
009500*                                 EXTERNAL CUSTOMER ID                    
009600     03 IDEXCUST-2           OCCURS 100 TIMES                             
009700                             PIC X(15).                                   
009800*                                 EXTERNT KUNDID                          
009900*                                 EXTERNAL CUSTOMER ID                    
010000     03 IDEXCUST-3           OCCURS 100 TIMES                             
010100                             PIC X(15).                                   
010200*                                 EXTERNT KUNDID                          
010300*                                 EXTERNAL CUSTOMER ID                    
010400     03 IDBUNDLE             OCCURS 100 TIMES                             
010500                             PIC X(15).                                   
010600*                                 BUNDLE ID                               
010700*                                 BUNDLE ID                               
010800     03 IDOPTION-1           OCCURS 100 TIMES                             
010900                             PIC X(15).                                   
011000*                                 BRYTBEGREPP                             
011100*                                 OPTIONAL ID                             
011200     03 IDOPTION-2           OCCURS 100 TIMES                             
011300                             PIC X(15).                                   
011400*                                 BRYTBEGREPP                             
011500*                                 OPTIONAL ID                             
011600     03 IDOPTION-3           OCCURS 100 TIMES                             
011700                             PIC X(15).                                   
011800*                                 BRYTBEGREPP                             
011900*                                 OPTIONAL ID                             
012000     03 IDOPTION-4           OCCURS 100 TIMES                             
012100                             PIC X(15).                                   
012200*                                 BRYTBEGREPP                             
012300*                                 OPTIONAL ID                             
012400     03 IDOPTION-5           OCCURS 100 TIMES                             
012500                             PIC X(15).                                   
012600*                                 BRYTBEGREPP                             
012700*                                 OPTIONAL ID                             
012800     03 IDARTNR-FINANCE      OCCURS 100 TIMES                             
012900                             PIC X(50).                                   
013000*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
013100*                                 UK                                      
013200*                                 PART NUMBER FOR FINANCIAL USE           
013300     03 BEART                OCCURS 100 TIMES                             
013400                             PIC X(25).                                   
013500*                                 ARTIKELBENÄMNING                        
013600*                                 PART DESCRIPTION                        
013700     03 KDVAT                OCCURS 100 TIMES                             
013800                             PIC X(2).                                    
013900*                                 MOMSKOD                                 
014000*                                 VAT CODE                                
014100     03 FLSPECPR             OCCURS 100 TIMES                             
014200                             PIC X.                                       
014300*                                 SPECIALPRISFLAGGA                       
014400*                                 SPECIAL PRICE FLAG                      
014500     03 PRARTBTO             OCCURS 100 TIMES                             
014600                             PIC S9(7)V9(2)      COMP-3.                  
014700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
014800*                                 GROSS SALES PRICE (SEK)                 
014900     03 PRARTNTO             OCCURS 100 TIMES                             
015000                             PIC S9(7)V9(2)      COMP-3.                  
015100*                                 ARTIKELPRIS NETTO                       
015200*                                 NET PRICE EACH   (FOB NET)              
015300     03 REARTRAB             OCCURS 100 TIMES                             
015400                             PIC S9(2)V9(2)      COMP-3.                  
015500*                                 ARTIKELRABATT                           
015600*                                 PARTS DISCOUNT PERCENT                  
015700     03 KVBEART              OCCURS 100 TIMES                             
015800                             PIC S9(7)           COMP-3.                  
015900*                                 BESTÄLLT ANTAL STYCKEN                  
016000*                                 ORDERED QUANTITY                        
016100     03 KVLEVART             OCCURS 100 TIMES                             
016200                             PIC S9(7)           COMP-3.                  
016300*                                 LEVERERAT ANTAL STYCK                   
016400*                                 DELIVERED QUANTITY                      
016500     03 KDANMORS             OCCURS 100 TIMES                             
016600                             PIC X(2).                                    
016700*                                 ORSAK TILL LEVERANSANMÄRKNING           
016800*                                 DISCREPANCY REPORT REASON CODE          
016900     03 IDFAKREF             OCCURS 100 TIMES                             
017000                             PIC S9(9)           COMP-3.                  
017100*                                 URSPRUNGLIGT FAKTURANUMMER              
017200*                                 ORIGINAL INVOICE NUMBER                 
017300     03 DAFAKREF             OCCURS 100 TIMES                             
017400                             PIC 9(8).                                    
017500*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
017600*                                 MMDD)                                   
017700*                                 ORIGINAL INVOICING DATE (YYYYMM         
017800*                                 DD)                                     
017900     03 IDDC                 OCCURS 100 TIMES                             
018000                             PIC X(2).                                    
018100*                                 IDENTIFIERARE LAGER                     
018200*                                 WAREHOUSE IDENTIFIER                    
018300     03 KDFRAKT              OCCURS 100 TIMES                             
018400                             PIC S9(3)           COMP-3.                  
018500*                                 FRAKTSÄTT DC TILL KUND                  
018600*                                 FREIGHT CODE                            
018700     03 BELEVVIL             OCCURS 100 TIMES                             
018800                             PIC X(35).                                   
018900*                                 LEVERANSVILLKOR                         
019000*                                 DELIVERY TERMS                          
019100     03 IDACCNT-1            OCCURS 100 TIMES                             
019200                             PIC X(15).                                   
019300*                                 KONTOFÄLT                               
019400*                                 ACCOUNT FIELD                           
019500     03 IDACCNT-2            OCCURS 100 TIMES                             
019600                             PIC X(15).                                   
019700*                                 KONTOFÄLT                               
019800*                                 ACCOUNT FIELD                           
019900     03 IDACCNT-3            OCCURS 100 TIMES                             
020000                             PIC X(15).                                   
020100*                                 KONTOFÄLT                               
020200*                                 ACCOUNT FIELD                           
020300     03 IDACCNT-4            OCCURS 100 TIMES                             
020400                             PIC X(15).                                   
020500*                                 KONTOFÄLT                               
020600*                                 ACCOUNT FIELD                           
020700     03 IDSYSTEM-SEND        OCCURS 100 TIMES                             
020800                             PIC X(4).                                    
020900*                                 VOLVO SÄNDANDE SYSTEM                   
021000*                                 VOLVO SENDING SYSTEM                    
021100     03 IDSYSTEM-REC         OCCURS 100 TIMES                             
021200                             PIC X(4).                                    
021300*                                 VOLVO MOTTAGANDE SYSTEM                 
021400*                                 VOLVO RECEIVING SYSTEM                  
021500     03 VKORDBTO-KOLLI       OCCURS 100 TIMES                             
021600                             PIC S9(6)V9(1)      COMP-3.                  
021700*                                 ORDERVIKT BRUTTO PER KOLLI              
021800*                                 ORDER WEIGHT GROSS PER CASE             
021900     03 VKARTNTO             OCCURS 100 TIMES                             
022000                             PIC S9(4)V9(3)      COMP-3.                  
022100*                                 ARTIKELVIKT NETTO (KG) MED EMB          
022200*                                 PART NET WEIGHT (KG) W/ PACKAGE         
022300     03 KDARTURS             OCCURS 100 TIMES                             
022400                             PIC X(2).                                    
022500*                                 ARTIKELURSPRUNGSKOD                     
022600*                                 COUNTRY OF ORIGIN                       
022700     03 FILLER               OCCURS 100 TIMES                             
022800                             PIC X(100).                                  
022900     03 BEANST               OCCURS 100 TIMES                             
023000                             PIC X(25).                                   
023100*                                 ANSTÄLLDS NAMN                          
023200*                                 NAME OF EMPLOYED                        
023300     03 IDUSER               OCCURS 100 TIMES                             
023400                             PIC X(8).                                    
023500*                                 ANVÄNDARENS SÄKERHETS ID                
023600*                                 USER SECURITY-IDENTITY                  
023700     03 BETEXT               OCCURS 100 TIMES                             
023800                             PIC X(125).                                  
023900     03 BETEXT-CRE           OCCURS 100 TIMES                             
024000                             PIC X(100).                                  
024100     03 IDARTNR-CNTRL        OCCURS 100 TIMES                             
024200                             PIC X(2).                                    
024300*                                 KONTROLLSIFFRA FÖR ARTIKELNUMME         
024400*                                 R                                       
024500*                                 CHECK FIGURE FOR PART NUMBER            
024600*                                                                         
024700     03 FLPCOO               OCCURS 100 TIMES                             
024800                             PIC X.                                       
024900*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
025000*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
025100     03 IDLEVNR-ART          OCCURS 100 TIMES                             
025200                             PIC X(5).                                    
025300*                                 LEVERANTÖRNR PÅ ARTIKEL                 
025400*                                 PART SUPPLIER NUMBER                    
025500     03 IDTRACK-1            OCCURS 100 TIMES                             
025600                             PIC X(25).                                   
025700*                                 TRACKING ID FROM CUSTOMS                
025800*                                 CUSTOMS TRACKING ID                     
025900     03 KVANT-TRACK-1        OCCURS 100 TIMES                             
026000                             PIC S9(7)           COMP-3.                  
026100*                                 TILLHÖR AV EN VISS ARTIKEL              
026200*                                 QUANTITY WITH RESPECT TO TRACK-         
026300*                                 ID                                      
026400     03 IDTRACK-2            OCCURS 100 TIMES                             
026500                             PIC X(25).                                   
026600*                                 TRACKING ID FROM CUSTOMS                
026700*                                 CUSTOMS TRACKING ID                     
026800     03 KVANT-TRACK-2        OCCURS 100 TIMES                             
026900                             PIC S9(7)           COMP-3.                  
027000*                                 TILLHÖR AV EN VISS ARTIKEL              
027100*                                 QUANTITY WITH RESPECT TO TRACK-         
027200*                                 ID                                      
027300     03 IDTRACK-3            OCCURS 100 TIMES                             
027400                             PIC X(25).                                   
027500*                                 TRACKING ID FROM CUSTOMS                
027600*                                 CUSTOMS TRACKING ID                     
027700     03 KVANT-TRACK-3        OCCURS 100 TIMES                             
027800                             PIC S9(7)           COMP-3.                  
027900*                                 TILLHÖR AV EN VISS ARTIKEL              
028000*                                 QUANTITY WITH RESPECT TO TRACK-         
028100*                                 ID                                      
028200     03 IDTRACK-4            OCCURS 100 TIMES                             
028300                             PIC X(25).                                   
028400*                                 TRACKING ID FROM CUSTOMS                
028500*                                 CUSTOMS TRACKING ID                     
028600     03 KVANT-TRACK-4        OCCURS 100 TIMES                             
028700                             PIC S9(7)           COMP-3.                  
028800*                                 TILLHÖR AV EN VISS ARTIKEL              
028900*                                 QUANTITY WITH RESPECT TO TRACK-         
029000*                                 ID                                      
029100     03 IDTRACK-5            OCCURS 100 TIMES                             
029200                             PIC X(25).                                   
029300*                                 TRACKING ID FROM CUSTOMS                
029400*                                 CUSTOMS TRACKING ID                     
029500     03 KVANT-TRACK-5        OCCURS 100 TIMES                             
029600                             PIC S9(7)           COMP-3.                  
029700*                                 TILLHÖR AV EN VISS ARTIKEL              
029800*                                 QUANTITY WITH RESPECT TO TRACK-         
029900*                                 ID                                      
030000     03 KDPRMOD              OCCURS 100 TIMES                             
030100                             PIC X(2).                                    
030200*                                 VILKEN PRISMODELL SOM ANVÄNDS           
030300*                                 WHAT PRICE MODEL THAT IS USED           
030400*** END OF VILMAII-COPY LENGTH= 101200 BYTES                              
