000100 01  T01ALINT.                                                            
000200*                                 MULTIFETCH TABELL TILL T01ALIN          
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
002700     03 BEVOLREF             OCCURS 100 TIMES                             
002800                             PIC X(10).                                   
002900*                                 VOLVO REFERENS                          
003000*                                 VOLVO REFERENCE                         
003100     03 IDAPPEND             OCCURS 100 TIMES                             
003200                             PIC X(8).                                    
003300*                                 APPENDIXVÄRDE                           
003400*                                 APPENDIX ITEM                           
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
023400     03 DASTADAT             OCCURS 100 TIMES                             
023500                             PIC X(8).                                    
023600     03 BEANST               OCCURS 100 TIMES                             
023700                             PIC X(25).                                   
023800*                                 ANSTÄLLDS NAMN                          
023900*                                 NAME OF EMPLOYED                        
024000     03 IDUSER               OCCURS 100 TIMES                             
024100                             PIC X(8).                                    
024200*                                 ANVÄNDARENS SÄKERHETS ID                
024300*                                 USER SECURITY-IDENTITY                  
024400     03 BETEXT               OCCURS 100 TIMES                             
024500                             PIC X(125).                                  
024600     03 BETEXT-CRE           OCCURS 100 TIMES                             
024700                             PIC X(100).                                  
024800     03 IDARTNR-CNTRL        OCCURS 100 TIMES                             
024900                             PIC X(2).                                    
025000*                                 KONTROLLSIFFRA FÖR ARTIKELNUMME         
025100*                                 R                                       
025200*                                 CHECK FIGURE FOR PART NUMBER            
025300*                                                                         
025400     03 FLPCOO               OCCURS 100 TIMES                             
025500                             PIC X.                                       
025600*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
025700*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
025800     03 IDLEVNR-ART          OCCURS 100 TIMES                             
025900                             PIC X(5).                                    
026000*                                 LEVERANTÖRNR PÅ ARTIKEL                 
026100*                                 PART SUPPLIER NUMBER                    
026200     03 IDTRACK-1            OCCURS 100 TIMES                             
026300                             PIC X(25).                                   
026400*                                 TRACKING ID FROM CUSTOMS                
026500*                                 CUSTOMS TRACKING ID                     
026600     03 KVANT-TRACK-1        OCCURS 100 TIMES                             
026700                             PIC S9(7)           COMP-3.                  
026800*                                 TILLHÖR AV EN VISS ARTIKEL              
026900*                                 QUANTITY WITH RESPECT TO TRACK-         
027000*                                 ID                                      
027100     03 IDTRACK-2            OCCURS 100 TIMES                             
027200                             PIC X(25).                                   
027300*                                 TRACKING ID FROM CUSTOMS                
027400*                                 CUSTOMS TRACKING ID                     
027500     03 KVANT-TRACK-2        OCCURS 100 TIMES                             
027600                             PIC S9(7)           COMP-3.                  
027700*                                 TILLHÖR AV EN VISS ARTIKEL              
027800*                                 QUANTITY WITH RESPECT TO TRACK-         
027900*                                 ID                                      
028000     03 IDTRACK-3            OCCURS 100 TIMES                             
028100                             PIC X(25).                                   
028200*                                 TRACKING ID FROM CUSTOMS                
028300*                                 CUSTOMS TRACKING ID                     
028400     03 KVANT-TRACK-3        OCCURS 100 TIMES                             
028500                             PIC S9(7)           COMP-3.                  
028600*                                 TILLHÖR AV EN VISS ARTIKEL              
028700*                                 QUANTITY WITH RESPECT TO TRACK-         
028800*                                 ID                                      
028900     03 IDTRACK-4            OCCURS 100 TIMES                             
029000                             PIC X(25).                                   
029100*                                 TRACKING ID FROM CUSTOMS                
029200*                                 CUSTOMS TRACKING ID                     
029300     03 KVANT-TRACK-4        OCCURS 100 TIMES                             
029400                             PIC S9(7)           COMP-3.                  
029500*                                 TILLHÖR AV EN VISS ARTIKEL              
029600*                                 QUANTITY WITH RESPECT TO TRACK-         
029700*                                 ID                                      
029800     03 IDTRACK-5            OCCURS 100 TIMES                             
029900                             PIC X(25).                                   
030000*                                 TRACKING ID FROM CUSTOMS                
030100*                                 CUSTOMS TRACKING ID                     
030200     03 KVANT-TRACK-5        OCCURS 100 TIMES                             
030300                             PIC S9(7)           COMP-3.                  
030400*                                 TILLHÖR AV EN VISS ARTIKEL              
030500*                                 QUANTITY WITH RESPECT TO TRACK-         
030600*                                 ID                                      
030700     03 KDPRMOD              OCCURS 100 TIMES                             
030800                             PIC X(2).                                    
030900*                                 VILKEN PRISMODELL SOM ANVÄNDS           
031000*                                 WHAT PRICE MODEL THAT IS USED           
031100*** END OF VILMAII-COPY LENGTH= 102800 BYTES                              
