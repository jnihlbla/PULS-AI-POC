000100 01  T01TRAWT.                                                            
000200*                                 MULTIFETCH TABELL TILL T01TRAW          
000300     03 IDLEGSEL             OCCURS 100 TIMES                             
000400                             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 IDBUNDLE             OCCURS 100 TIMES                             
000800                             PIC X(15).                                   
000900*                                 BUNDLE ID                               
001000*                                 BUNDLE ID                               
001100     03 DAREGDAT             OCCURS 100 TIMES                             
001200                             PIC X(8).                                    
001300     03 TIREGTID             OCCURS 100 TIMES                             
001400                             PIC S9(10)          COMP-3.                  
001500*                                 REGISTRERINGSTID                        
001600*                                 GENERAL REGISTRATION TIME               
001700     03 IDREF                OCCURS 100 TIMES                             
001800                             PIC X(15).                                   
001900*                                 REFERENS ID                             
002000*                                 REFERENCE ID                            
002100     03 DAREFDAT             OCCURS 100 TIMES                             
002200                             PIC X(8).                                    
002300     03 IDREFRAD             OCCURS 100 TIMES                             
002400                             PIC S9(5)           COMP-3.                  
002500*                                 REFERENSRADSNR                          
002600*                                 REFERENCE LINE NUMBER                   
002700     03 IDAPPEND             OCCURS 100 TIMES                             
002800                             PIC X(8).                                    
002900*                                 APPENDIXVÄRDE                           
003000*                                 APPENDIX ITEM                           
003100     03 BEVOLREF             OCCURS 100 TIMES                             
003200                             PIC X(10).                                   
003300*                                 VOLVO REFERENS                          
003400*                                 VOLVO REFERENCE                         
003500     03 IDLANDX3-SEND        OCCURS 100 TIMES                             
003600                             PIC X(3).                                    
003700*                                 LANDKOD SÄNDANDE LAND                   
003800*                                 COUNTRY CODE SENDING COUNTRY            
003900     03 IDLANDX3-REC         OCCURS 100 TIMES                             
004000                             PIC X(3).                                    
004100*                                 LANDKOD MOTTAGANDE LAND                 
004200*                                 COUNTRY CODE RECEIVING COUNTRY          
004300     03 IDLEVNR              OCCURS 100 TIMES                             
004400                             PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004700     03 IDPARTNR             OCCURS 100 TIMES                             
004800                             PIC X(9).                                    
004900*                                 FINANCIELL KUND                         
005000*                                 FINANCIAL CUST                          
005100     03 IDEXCUST-1           OCCURS 100 TIMES                             
005200                             PIC X(15).                                   
005300*                                 EXTERNT KUNDID                          
005400*                                 EXTERNAL CUSTOMER ID                    
005500     03 IDEXCUST-2           OCCURS 100 TIMES                             
005600                             PIC X(15).                                   
005700*                                 EXTERNT KUNDID                          
005800*                                 EXTERNAL CUSTOMER ID                    
005900     03 IDEXCUST-3           OCCURS 100 TIMES                             
006000                             PIC X(15).                                   
006100*                                 EXTERNT KUNDID                          
006200*                                 EXTERNAL CUSTOMER ID                    
006300     03 IDOPTION-1           OCCURS 100 TIMES                             
006400                             PIC X(15).                                   
006500*                                 BRYTBEGREPP                             
006600*                                 OPTIONAL ID                             
006700     03 IDOPTION-2           OCCURS 100 TIMES                             
006800                             PIC X(15).                                   
006900*                                 BRYTBEGREPP                             
007000*                                 OPTIONAL ID                             
007100     03 IDOPTION-3           OCCURS 100 TIMES                             
007200                             PIC X(15).                                   
007300*                                 BRYTBEGREPP                             
007400*                                 OPTIONAL ID                             
007500     03 IDOPTION-4           OCCURS 100 TIMES                             
007600                             PIC X(15).                                   
007700*                                 BRYTBEGREPP                             
007800*                                 OPTIONAL ID                             
007900     03 IDOPTION-5           OCCURS 100 TIMES                             
008000                             PIC X(15).                                   
008100*                                 BRYTBEGREPP                             
008200*                                 OPTIONAL ID                             
008300     03 IDARTNR-FINANCE      OCCURS 100 TIMES                             
008400                             PIC X(50).                                   
008500*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
008600*                                 UK                                      
008700*                                 PART NUMBER FOR FINANCIAL USE           
008800     03 IDSTATNR             OCCURS 100 TIMES                             
008900                             PIC S9(9)           COMP-3.                  
009000*                                 STATISTISKT NUMMER                      
009100*                                 1 = NORSKT                              
009200*                                 2 = ENGELSKT                            
009300*                                 3 = BELGISKT                            
009400*                                 4 = PERUANSKT                           
009500*                                 5 = SVENSKT                             
009600*                                 6 =                                     
009700*                                 STATISTICAL NO.                         
009800     03 VKORDBTO-KOLLI       OCCURS 100 TIMES                             
009900                             PIC S9(6)V9(1)      COMP-3.                  
010000*                                 ORDERVIKT BRUTTO PER KOLLI              
010100*                                 ORDER WEIGHT GROSS PER CASE             
010200     03 VKARTNTO             OCCURS 100 TIMES                             
010300                             PIC S9(4)V9(3)      COMP-3.                  
010400*                                 ARTIKELVIKT NETTO (KG) MED EMB          
010500*                                 PART NET WEIGHT (KG) W/ PACKAGE         
010600     03 PRARTBTO             OCCURS 100 TIMES                             
010700                             PIC S9(7)V9(2)      COMP-3.                  
010800*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
010900*                                 GROSS SALES PRICE (SEK)                 
011000     03 PRARTNTO             OCCURS 100 TIMES                             
011100                             PIC S9(7)V9(2)      COMP-3.                  
011200*                                 ARTIKELPRIS NETTO                       
011300*                                 NET PRICE EACH   (FOB NET)              
011400     03 REARTRAB             OCCURS 100 TIMES                             
011500                             PIC S9(2)V9(2)      COMP-3.                  
011600*                                 ARTIKELRABATT                           
011700*                                 PARTS DISCOUNT PERCENT                  
011800     03 KVBEART              OCCURS 100 TIMES                             
011900                             PIC S9(7)           COMP-3.                  
012000*                                 BESTÄLLT ANTAL STYCKEN                  
012100*                                 ORDERED QUANTITY                        
012200     03 KVLEVART             OCCURS 100 TIMES                             
012300                             PIC S9(7)           COMP-3.                  
012400*                                 LEVERERAT ANTAL STYCK                   
012500*                                 DELIVERED QUANTITY                      
012600     03 BEART                OCCURS 100 TIMES                             
012700                             PIC X(25).                                   
012800*                                 ARTIKELBENÄMNING                        
012900*                                 PART DESCRIPTION                        
013000     03 FLSOFT               OCCURS 100 TIMES                             
013100                             PIC X.                                       
013200*                                 FLAGGA SOFTVARA                         
013300*                                 SOFTWARE MARK                           
013400     03 FLSPECPR             OCCURS 100 TIMES                             
013500                             PIC X.                                       
013600*                                 SPECIALPRISFLAGGA                       
013700*                                 SPECIAL PRICE FLAG                      
013800     03 FLFREE               OCCURS 100 TIMES                             
013900                             PIC X.                                       
014000*                                 GRATISFATURA                            
014100*                                 FREE INVOICE                            
014200     03 FLPRIV               OCCURS 100 TIMES                             
014300                             PIC X.                                       
014400*                                 KÖPARE ÄR EN PRIVATPERSON               
014500*                                 PURCHASER IS A PRIVATE PERSON           
014600     03 KDVAT                OCCURS 100 TIMES                             
014700                             PIC X(2).                                    
014800*                                 MOMSKOD                                 
014900*                                 VAT CODE                                
015000     03 KDVALISO             OCCURS 100 TIMES                             
015100                             PIC X(3).                                    
015200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
015300*                                 CURRENCY CODE BY ISO-STANDARD.          
015400     03 KDINVFRQ             OCCURS 100 TIMES                             
015500                             PIC X(4).                                    
015600*                                 FAKTURERINGSFREKVENS                    
015700*                                 INVOICE FREQUENCE                       
015800     03 KDFINDOC             OCCURS 100 TIMES                             
015900                             PIC X(4).                                    
016000*                                 TYP FINANSIELLT DOKUMENT                
016100*                                 FINANCIAL DOCUMENT TYPE                 
016200     03 IDBREAK-1            OCCURS 100 TIMES                             
016300                             PIC X(8).                                    
016400*                                 BRYTVÄRDE                               
016500*                                 BREAK VALUE                             
016600     03 IDBREAK-2            OCCURS 100 TIMES                             
016700                             PIC X(8).                                    
016800*                                 BRYTVÄRDE                               
016900*                                 BREAK VALUE                             
017000     03 IDSEQ-1              OCCURS 100 TIMES                             
017100                             PIC X(8).                                    
017200*                                 SEKVENSVÄRDE                            
017300*                                 SEQUENCE VALUE                          
017400     03 IDSEQ-2              OCCURS 100 TIMES                             
017500                             PIC X(8).                                    
017600*                                 SEKVENSVÄRDE                            
017700*                                 SEQUENCE VALUE                          
017800     03 IDSEQ-3              OCCURS 100 TIMES                             
017900                             PIC X(8).                                    
018000*                                 SEKVENSVÄRDE                            
018100*                                 SEQUENCE VALUE                          
018200     03 KDARTURS             OCCURS 100 TIMES                             
018300                             PIC X(2).                                    
018400*                                 ARTIKELURSPRUNGSKOD                     
018500*                                 COUNTRY OF ORIGIN                       
018600     03 KDANMORS             OCCURS 100 TIMES                             
018700                             PIC X(2).                                    
018800*                                 ORSAK TILL LEVERANSANMÄRKNING           
018900*                                 DISCREPANCY REPORT REASON CODE          
019000     03 IDFAKREF             OCCURS 100 TIMES                             
019100                             PIC S9(9)           COMP-3.                  
019200*                                 URSPRUNGLIGT FAKTURANUMMER              
019300*                                 ORIGINAL INVOICE NUMBER                 
019400     03 DAFAKREF             OCCURS 100 TIMES                             
019500                             PIC X(8).                                    
019600     03 IDDC                 OCCURS 100 TIMES                             
019700                             PIC X(2).                                    
019800*                                 IDENTIFIERARE LAGER                     
019900*                                 WAREHOUSE IDENTIFIER                    
020000     03 KDFRAKT              OCCURS 100 TIMES                             
020100                             PIC S9(3)           COMP-3.                  
020200*                                 FRAKTSÄTT DC TILL KUND                  
020300*                                 FREIGHT CODE                            
020400     03 BELEVVIL             OCCURS 100 TIMES                             
020500                             PIC X(35).                                   
020600*                                 LEVERANSVILLKOR                         
020700*                                 DELIVERY TERMS                          
020800     03 IDACCNT-1            OCCURS 100 TIMES                             
020900                             PIC X(15).                                   
021000*                                 KONTOFÄLT                               
021100*                                 ACCOUNT FIELD                           
021200     03 IDACCNT-2            OCCURS 100 TIMES                             
021300                             PIC X(15).                                   
021400*                                 KONTOFÄLT                               
021500*                                 ACCOUNT FIELD                           
021600     03 IDACCNT-3            OCCURS 100 TIMES                             
021700                             PIC X(15).                                   
021800*                                 KONTOFÄLT                               
021900*                                 ACCOUNT FIELD                           
022000     03 IDACCNT-4            OCCURS 100 TIMES                             
022100                             PIC X(15).                                   
022200*                                 KONTOFÄLT                               
022300*                                 ACCOUNT FIELD                           
022400     03 IDSYSTEM-SEND        OCCURS 100 TIMES                             
022500                             PIC X(4).                                    
022600*                                 VOLVO SÄNDANDE SYSTEM                   
022700*                                 VOLVO SENDING SYSTEM                    
022800     03 IDSYSTEM-REC         OCCURS 100 TIMES                             
022900                             PIC X(4).                                    
023000*                                 VOLVO MOTTAGANDE SYSTEM                 
023100*                                 VOLVO RECEIVING SYSTEM                  
023200     03 FILLER               OCCURS 100 TIMES                             
023300                             PIC X(100).                                  
023400     03 IDFELKOD             OCCURS 100 TIMES                             
023500                             PIC X(3).                                    
023600*                                 FELKOD                                  
023700*                                 ERROR CODE                              
023800     03 BEFEL                OCCURS 100 TIMES                             
023900                             PIC X(50).                                   
024000*                                 FELTEXT                                 
024100*                                 ERROR TEXT                              
024200     03 BEANST               OCCURS 100 TIMES                             
024300                             PIC X(25).                                   
024400*                                 ANSTÄLLDS NAMN                          
024500*                                 NAME OF EMPLOYED                        
024600     03 IDUSER               OCCURS 100 TIMES                             
024700                             PIC X(8).                                    
024800*                                 ANVÄNDARENS SÄKERHETS ID                
024900*                                 USER SECURITY-IDENTITY                  
025000     03 BETEXT               OCCURS 100 TIMES                             
025100                             PIC X(125).                                  
025200     03 BETEXT-CRE           OCCURS 100 TIMES                             
025300                             PIC X(100).                                  
025400     03 IDARTNR-CNTRL        OCCURS 100 TIMES                             
025500                             PIC X(2).                                    
025600*                                 KONTROLLSIFFRA FÖR ARTIKELNUMME         
025700*                                 R                                       
025800*                                 CHECK FIGURE FOR PART NUMBER            
025900*                                                                         
026000     03 FLPCOO               OCCURS 100 TIMES                             
026100                             PIC X.                                       
026200*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
026300*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
026400     03 IDLEVNR-ART          OCCURS 100 TIMES                             
026500                             PIC X(5).                                    
026600*                                 LEVERANTÖRNR PÅ ARTIKEL                 
026700*                                 PART SUPPLIER NUMBER                    
026800     03 IDTRACK-1            OCCURS 100 TIMES                             
026900                             PIC X(25).                                   
027000*                                 TRACKING ID FROM CUSTOMS                
027100*                                 CUSTOMS TRACKING ID                     
027200     03 KVANT-TRACK-1        OCCURS 100 TIMES                             
027300                             PIC S9(7)           COMP-3.                  
027400*                                 TILLHÖR AV EN VISS ARTIKEL              
027500*                                 QUANTITY WITH RESPECT TO TRACK-         
027600*                                 ID                                      
027700     03 IDTRACK-2            OCCURS 100 TIMES                             
027800                             PIC X(25).                                   
027900*                                 TRACKING ID FROM CUSTOMS                
028000*                                 CUSTOMS TRACKING ID                     
028100     03 KVANT-TRACK-2        OCCURS 100 TIMES                             
028200                             PIC S9(7)           COMP-3.                  
028300*                                 TILLHÖR AV EN VISS ARTIKEL              
028400*                                 QUANTITY WITH RESPECT TO TRACK-         
028500*                                 ID                                      
028600     03 IDTRACK-3            OCCURS 100 TIMES                             
028700                             PIC X(25).                                   
028800*                                 TRACKING ID FROM CUSTOMS                
028900*                                 CUSTOMS TRACKING ID                     
029000     03 KVANT-TRACK-3        OCCURS 100 TIMES                             
029100                             PIC S9(7)           COMP-3.                  
029200*                                 TILLHÖR AV EN VISS ARTIKEL              
029300*                                 QUANTITY WITH RESPECT TO TRACK-         
029400*                                 ID                                      
029500     03 IDTRACK-4            OCCURS 100 TIMES                             
029600                             PIC X(25).                                   
029700*                                 TRACKING ID FROM CUSTOMS                
029800*                                 CUSTOMS TRACKING ID                     
029900     03 KVANT-TRACK-4        OCCURS 100 TIMES                             
030000                             PIC S9(7)           COMP-3.                  
030100*                                 TILLHÖR AV EN VISS ARTIKEL              
030200*                                 QUANTITY WITH RESPECT TO TRACK-         
030300*                                 ID                                      
030400     03 IDTRACK-5            OCCURS 100 TIMES                             
030500                             PIC X(25).                                   
030600*                                 TRACKING ID FROM CUSTOMS                
030700*                                 CUSTOMS TRACKING ID                     
030800     03 KVANT-TRACK-5        OCCURS 100 TIMES                             
030900                             PIC S9(7)           COMP-3.                  
031000*                                 TILLHÖR AV EN VISS ARTIKEL              
031100*                                 QUANTITY WITH RESPECT TO TRACK-         
031200*                                 ID                                      
031300     03 KDPRMOD              OCCURS 100 TIMES                             
031400                             PIC X(2).                                    
031500*                                 VILKEN PRISMODELL SOM ANVÄNDS           
031600*                                 WHAT PRICE MODEL THAT IS USED           
031700*** END OF VILMAII-COPY LENGTH= 107300 BYTES                              
