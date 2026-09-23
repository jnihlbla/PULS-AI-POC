000100 01  T01DLINT.                                                            
000200*                                 MULTIFETCH TABELL TILL T01DLIN          
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
005600     03 IDLANDX3-REC         OCCURS 100 TIMES                             
005700                             PIC X(3).                                    
005800*                                 LANDKOD MOTTAGANDE LAND                 
005900*                                 COUNTRY CODE RECEIVING COUNTRY          
006000     03 IDEXCUST-1           OCCURS 100 TIMES                             
006100                             PIC X(15).                                   
006200*                                 EXTERNT KUNDID                          
006300*                                 EXTERNAL CUSTOMER ID                    
006400     03 IDEXCUST-2           OCCURS 100 TIMES                             
006500                             PIC X(15).                                   
006600*                                 EXTERNT KUNDID                          
006700*                                 EXTERNAL CUSTOMER ID                    
006800     03 IDEXCUST-3           OCCURS 100 TIMES                             
006900                             PIC X(15).                                   
007000*                                 EXTERNT KUNDID                          
007100*                                 EXTERNAL CUSTOMER ID                    
007200     03 IDBUNDLE             OCCURS 100 TIMES                             
007300                             PIC X(15).                                   
007400*                                 BUNDLE ID                               
007500*                                 BUNDLE ID                               
007600     03 IDREF                OCCURS 100 TIMES                             
007700                             PIC X(15).                                   
007800*                                 REFERENS ID                             
007900*                                 REFERENCE ID                            
008000     03 IDREFRAD             OCCURS 100 TIMES                             
008100                             PIC S9(5)           COMP-3.                  
008200*                                 REFERENSRADSNR                          
008300*                                 REFERENCE LINE NUMBER                   
008400     03 DAREFDAT             OCCURS 100 TIMES                             
008500                             PIC 9(8).                                    
008600*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
008700*                                 REFERENCE DATE(YYYYMMDD)                
008800     03 BEVOLREF             OCCURS 100 TIMES                             
008900                             PIC X(10).                                   
009000*                                 VOLVO REFERENS                          
009100*                                 VOLVO REFERENCE                         
009200     03 IDOPTION-1           OCCURS 100 TIMES                             
009300                             PIC X(15).                                   
009400*                                 BRYTBEGREPP                             
009500*                                 OPTIONAL ID                             
009600     03 IDOPTION-2           OCCURS 100 TIMES                             
009700                             PIC X(15).                                   
009800*                                 BRYTBEGREPP                             
009900*                                 OPTIONAL ID                             
010000     03 IDOPTION-3           OCCURS 100 TIMES                             
010100                             PIC X(15).                                   
010200*                                 BRYTBEGREPP                             
010300*                                 OPTIONAL ID                             
010400     03 IDOPTION-4           OCCURS 100 TIMES                             
010500                             PIC X(15).                                   
010600*                                 BRYTBEGREPP                             
010700*                                 OPTIONAL ID                             
010800     03 IDOPTION-5           OCCURS 100 TIMES                             
010900                             PIC X(15).                                   
011000*                                 BRYTBEGREPP                             
011100*                                 OPTIONAL ID                             
011200     03 IDARTNR-FINANCE      OCCURS 100 TIMES                             
011300                             PIC X(50).                                   
011400*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
011500*                                 UK                                      
011600*                                 PART NUMBER FOR FINANCIAL USE           
011700     03 BEART                OCCURS 100 TIMES                             
011800                             PIC X(25).                                   
011900*                                 ARTIKELBENÄMNING                        
012000*                                 PART DESCRIPTION                        
012100     03 IDSTATNR             OCCURS 100 TIMES                             
012200                             PIC S9(9)           COMP-3.                  
012300*                                 STATISTISKT NUMMER                      
012400*                                 1 = NORSKT                              
012500*                                 2 = ENGELSKT                            
012600*                                 3 = BELGISKT                            
012700*                                 4 = PERUANSKT                           
012800*                                 5 = SVENSKT                             
012900*                                 6 =                                     
013000*                                 STATISTICAL NO.                         
013100     03 VKORDBTO-KOLLI       OCCURS 100 TIMES                             
013200                             PIC S9(6)V9(1)      COMP-3.                  
013300*                                 ORDERVIKT BRUTTO PER KOLLI              
013400*                                 ORDER WEIGHT GROSS PER CASE             
013500     03 VKARTNTO             OCCURS 100 TIMES                             
013600                             PIC S9(4)V9(3)      COMP-3.                  
013700*                                 ARTIKELVIKT NETTO (KG) MED EMB          
013800*                                 PART NET WEIGHT (KG) W/ PACKAGE         
013900     03 KDARTURS             OCCURS 100 TIMES                             
014000                             PIC X(2).                                    
014100*                                 ARTIKELURSPRUNGSKOD                     
014200*                                 COUNTRY OF ORIGIN                       
014300     03 KVBEART              OCCURS 100 TIMES                             
014400                             PIC S9(7)           COMP-3.                  
014500*                                 BESTÄLLT ANTAL STYCKEN                  
014600*                                 ORDERED QUANTITY                        
014700     03 KVLEVART             OCCURS 100 TIMES                             
014800                             PIC S9(7)           COMP-3.                  
014900*                                 LEVERERAT ANTAL STYCK                   
015000*                                 DELIVERED QUANTITY                      
015100     03 PRARTBTO             OCCURS 100 TIMES                             
015200                             PIC S9(7)V9(2)      COMP-3.                  
015300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
015400*                                 GROSS SALES PRICE (SEK)                 
015500     03 PRARTNTO             OCCURS 100 TIMES                             
015600                             PIC S9(7)V9(2)      COMP-3.                  
015700*                                 ARTIKELPRIS NETTO                       
015800*                                 NET PRICE EACH   (FOB NET)              
015900     03 REARTRAB             OCCURS 100 TIMES                             
016000                             PIC S9(2)V9(2)      COMP-3.                  
016100*                                 ARTIKELRABATT                           
016200*                                 PARTS DISCOUNT PERCENT                  
016300     03 KDVAT                OCCURS 100 TIMES                             
016400                             PIC X(2).                                    
016500*                                 MOMSKOD                                 
016600*                                 VAT CODE                                
016700     03 FLSPECPR             OCCURS 100 TIMES                             
016800                             PIC X.                                       
016900*                                 SPECIALPRISFLAGGA                       
017000*                                 SPECIAL PRICE FLAG                      
017100     03 KDANMORS             OCCURS 100 TIMES                             
017200                             PIC X(2).                                    
017300*                                 ORSAK TILL LEVERANSANMÄRKNING           
017400*                                 DISCREPANCY REPORT REASON CODE          
017500     03 IDFAKREF             OCCURS 100 TIMES                             
017600                             PIC S9(9)           COMP-3.                  
017700*                                 URSPRUNGLIGT FAKTURANUMMER              
017800*                                 ORIGINAL INVOICE NUMBER                 
017900     03 DAFAKREF             OCCURS 100 TIMES                             
018000                             PIC X(8).                                    
018100*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
018200*                                 MMDD)                                   
018300*                                 ORIGINAL INVOICING DATE (YYYYMM         
018400*                                 DD)                                     
018500     03 IDDC                 OCCURS 100 TIMES                             
018600                             PIC X(2).                                    
018700*                                 IDENTIFIERARE LAGER                     
018800*                                 WAREHOUSE IDENTIFIER                    
018900     03 KDFRAKT              OCCURS 100 TIMES                             
019000                             PIC S9(3)           COMP-3.                  
019100*                                 FRAKTSÄTT DC TILL KUND                  
019200*                                 FREIGHT CODE                            
019300     03 BELEVVIL             OCCURS 100 TIMES                             
019400                             PIC X(35).                                   
019500*                                 LEVERANSVILLKOR                         
019600*                                 DELIVERY TERMS                          
019700     03 IDACCNT-1            OCCURS 100 TIMES                             
019800                             PIC X(15).                                   
019900*                                 KONTOFÄLT                               
020000*                                 ACCOUNT FIELD                           
020100     03 IDACCNT-2            OCCURS 100 TIMES                             
020200                             PIC X(15).                                   
020300*                                 KONTOFÄLT                               
020400*                                 ACCOUNT FIELD                           
020500     03 IDACCNT-3            OCCURS 100 TIMES                             
020600                             PIC X(15).                                   
020700*                                 KONTOFÄLT                               
020800*                                 ACCOUNT FIELD                           
020900     03 IDACCNT-4            OCCURS 100 TIMES                             
021000                             PIC X(15).                                   
021100*                                 KONTOFÄLT                               
021200*                                 ACCOUNT FIELD                           
021300     03 FILLER               OCCURS 100 TIMES                             
021400                             PIC X(100).                                  
021500     03 SUNTO                OCCURS 100 TIMES                             
021600                             PIC S9(11)V9(2)     COMP-3.                  
021700*                                 TOTAL SALES AMOUNT EXCL. VAT            
021800     03 REVAT                OCCURS 100 TIMES                             
021900                             PIC S9(3)V9(2)      COMP-3.                  
022000*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
022100*                                 VAT FACTOR                              
022200     03 SUVAT-BILLIT         OCCURS 100 TIMES                             
022300                             PIC S9(11)V9(2)     COMP-3.                  
022400*                                 SUMMERAT MOMSVÄRDE PER RAD              
022500*                                 TOTAL VAT VALUE PER LINE                
022600     03 SUBTO                OCCURS 100 TIMES                             
022700                             PIC S9(11)V9(2)     COMP-3.                  
022800*                                 TOTAL SALES AMOUNT INCL. VAT            
022900     03 BEVAT                OCCURS 100 TIMES                             
023000                             PIC X(50).                                   
023100*                                 MOMSKODSBENÄMNING R3                    
023200*                                 VAT CODE DESCRIPTION R3                 
023300     03 IDAPPEND             OCCURS 100 TIMES                             
023400                             PIC X(8).                                    
023500*                                 APPENDIXVÄRDE                           
023600*                                 APPENDIX ITEM                           
023700     03 IDARTNR-CNTRL        OCCURS 100 TIMES                             
023800                             PIC X(2).                                    
023900*                                 KONTROLLSIFFRA FÖR ARTIKELNUMME         
024000*                                 R                                       
024100*                                 CHECK FIGURE FOR PART NUMBER            
024200*                                                                         
024300     03 FLPCOO               OCCURS 100 TIMES                             
024400                             PIC X.                                       
024500*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
024600*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
024700     03 IDLEVNR-ART          OCCURS 100 TIMES                             
024800                             PIC X(5).                                    
024900*                                 LEVERANTÖRNR PÅ ARTIKEL                 
025000*                                 PART SUPPLIER NUMBER                    
025100     03 IDTRACK-1            OCCURS 100 TIMES                             
025200                             PIC X(25).                                   
025300*                                 TRACKING ID FROM CUSTOMS                
025400*                                 CUSTOMS TRACKING ID                     
025500     03 KVANT-TRACK-1        OCCURS 100 TIMES                             
025600                             PIC S9(7)           COMP-3.                  
025700*                                 TILLHÖR AV EN VISS ARTIKEL              
025800*                                 QUANTITY WITH RESPECT TO TRACK-         
025900*                                 ID                                      
026000     03 IDTRACK-2            OCCURS 100 TIMES                             
026100                             PIC X(25).                                   
026200*                                 TRACKING ID FROM CUSTOMS                
026300*                                 CUSTOMS TRACKING ID                     
026400     03 KVANT-TRACK-2        OCCURS 100 TIMES                             
026500                             PIC S9(7)           COMP-3.                  
026600*                                 TILLHÖR AV EN VISS ARTIKEL              
026700*                                 QUANTITY WITH RESPECT TO TRACK-         
026800*                                 ID                                      
026900     03 IDTRACK-3            OCCURS 100 TIMES                             
027000                             PIC X(25).                                   
027100*                                 TRACKING ID FROM CUSTOMS                
027200*                                 CUSTOMS TRACKING ID                     
027300     03 KVANT-TRACK-3        OCCURS 100 TIMES                             
027400                             PIC S9(7)           COMP-3.                  
027500*                                 TILLHÖR AV EN VISS ARTIKEL              
027600*                                 QUANTITY WITH RESPECT TO TRACK-         
027700*                                 ID                                      
027800     03 IDTRACK-4            OCCURS 100 TIMES                             
027900                             PIC X(25).                                   
028000*                                 TRACKING ID FROM CUSTOMS                
028100*                                 CUSTOMS TRACKING ID                     
028200     03 KVANT-TRACK-4        OCCURS 100 TIMES                             
028300                             PIC S9(7)           COMP-3.                  
028400*                                 TILLHÖR AV EN VISS ARTIKEL              
028500*                                 QUANTITY WITH RESPECT TO TRACK-         
028600*                                 ID                                      
028700     03 IDTRACK-5            OCCURS 100 TIMES                             
028800                             PIC X(25).                                   
028900*                                 TRACKING ID FROM CUSTOMS                
029000*                                 CUSTOMS TRACKING ID                     
029100     03 KVANT-TRACK-5        OCCURS 100 TIMES                             
029200                             PIC S9(7)           COMP-3.                  
029300*                                 TILLHÖR AV EN VISS ARTIKEL              
029400*                                 QUANTITY WITH RESPECT TO TRACK-         
029500*                                 ID                                      
029600     03 KDPRMOD              OCCURS 100 TIMES                             
029700                             PIC X(2).                                    
029800*                                 VILKEN PRISMODELL SOM ANVÄNDS           
029900*                                 WHAT PRICE MODEL THAT IS USED           
030000*** END OF VILMAII-COPY LENGTH= 80100 BYTES                               
