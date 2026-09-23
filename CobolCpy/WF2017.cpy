000100 01  FEED-WF2017.                                                         
000200*                                 FEEDBACK DATA                           
000300     03 FEED-DAEXDAT         PIC 9(8).                                    
000400*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000500*                                 EXECUTION DATE (YYYYMMDD)               
000600     03 FEED-TIEXTID         PIC S9(7)           COMP-3.                  
000700*                                 EXEKVERINGSTIDPUNKT                     
000800*                                 EXECUTION TIME                          
000900     03 FEED-IDLEGSEL        PIC X(4).                                    
001000*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001100*                                 LEGAL SELLER IDENTITY                   
001200     03 FEED-KDVALISO        PIC X(3).                                    
001300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001400*                                 CURRENCY CODE BY ISO-STANDARD.          
001500     03 FEED-PRKURS          PIC S9(6)V9(5)      COMP-3.                  
001600*                                 VALUTAKURS                              
001700*                                 CURRENCY EXCHANGE RATE                  
001800     03 FEED-IDLANDX3-SEND   PIC X(3).                                    
001900*                                 LANDKOD SÄNDANDE LAND                   
002000*                                 COUNTRY CODE SENDING COUNTRY            
002100     03 FEED-IDLANDX3-REC    PIC X(3).                                    
002200*                                 LANDKOD MOTTAGANDE LAND                 
002300*                                 COUNTRY CODE RECEIVING COUNTRY          
002400     03 FEED-IDLEVNR         PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002700     03 FEED-IDPARTNR        PIC X(9).                                    
002800*                                 FINANCIELL KUND                         
002900*                                 FINANCIAL CUST                          
003000     03 FEED-KDFINDOC        PIC X(4).                                    
003100*                                 TYP FINANSIELLT DOKUMENT                
003200*                                 FINANCIAL DOCUMENT TYPE                 
003300     03 FEED-FLSOFT          PIC X.                                       
003400*                                 FLAGGA SOFTVARA                         
003500*                                 SOFTWARE MARK                           
003600     03 FEED-FLFREE          PIC X.                                       
003700*                                 GRATISFATURA                            
003800*                                 FREE INVOICE                            
003900     03 FEED-DAFINDOC        PIC 9(8).                                    
004000*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
004100*                                 INVOICING DATE   (YYYYMMDD)             
004200     03 FEED-IDFINDOC        PIC S9(9)           COMP-3.                  
004300*                                 FINANSIELLT DOKUMENT ID                 
004400*                                 FINANCIAL DOCUMENT ID                   
004500     03 FEED-IDSYSTEM-SEND   PIC X(4).                                    
004600*                                 VOLVO SÄNDANDE SYSTEM                   
004700*                                 VOLVO SENDING SYSTEM                    
004800     03 FEED-IDSYSTEM-REC    PIC X(4).                                    
004900*                                 VOLVO MOTTAGANDE SYSTEM                 
005000*                                 VOLVO RECEIVING SYSTEM                  
005100     03 FEED-IDSPRAK         PIC X(2).                                    
005200*                                 2-STÄLLIG ISO SPRÅKKOD                  
005300*                                 2-LETTER ISO LANGUAGE CODE              
005400     03 FEED-KDBETALV        PIC X(4).                                    
005500*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
005600*                                 TERMS OF PAYMENT                        
005700     03 FEED-BEBETVIL        PIC X(30).                                   
005800*                                 BETALNINGSVILLKORSTEXT                  
005900*                                 TERMS OF PAYMENT TEXT                   
006000     03 FEED-BELEGRAD-1      PIC X(35).                                   
006100*                                 DEL AV LEGAL SELLER NAMN                
006200*                                 PART OF LEGAL SELLER NAME               
006300     03 FEED-BELEGRAD-2      PIC X(35).                                   
006400*                                 DEL AV LEGAL SELLER NAMN                
006500*                                 PART OF LEGAL SELLER NAME               
006600     03 FEED-ADLEG-STREET    PIC X(35).                                   
006700*                                 LEGAL SELLER GATUADRESS                 
006800*                                 LEGAL SELLER STREET ADDRESS             
006900     03 FEED-ADLEG-BOX       PIC X(10).                                   
007000*                                 BOXADRESS LEGAL SELLER                  
007100*                                 LEGAL SELLER BOX ADDRESS                
007200     03 FEED-ADLEG-CITY      PIC X(35).                                   
007300*                                 LEGAL SÄLJARES ADRESS STAD              
007400*                                 LEGAL SELLER ADDRESS CITY               
007500     03 FEED-ADLEG-PCODE     PIC X(10).                                   
007600*                                 LEGAL SELLER ADRESS POSTNR              
007700*                                 LEGAL SELLER POSTAL CODE                
007800     03 FEED-IDLANDX3-LEG    PIC X(3).                                    
007900*                                 LANDKOD LEGAL SÄLJARE                   
008000*                                 COUNTRY CODE LEGAL SELLER               
008100     03 FEED-IDVAT-LEG       PIC X(17).                                   
008200*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
008300*                                 ÄLJARE                                  
008400*                                 VAT REGISTRATION LEGAL PAYER            
008500     03 FEED-IDTFN-LEG       PIC X(20).                                   
008600*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
008700*                                 JARE                                    
008800*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
008900*                                  SELLER                                 
009000     03 FEED-IDTFX-LEG       PIC X(20).                                   
009100*                                 FAXNUMMER LEGAL SÄLJARE                 
009200*                                 FAXNUMBER LEGAL SELLER                  
009300     03 FEED-IDMAIL-LEG      PIC X(60).                                   
009400*                                 MAIL ADRESS LEGAL SÄLJARE               
009500*                                 MAIL ADDRESS LEGAL SELLER               
009600     03 FEED-BECONT-LEG      PIC X(35).                                   
009700*                                 KONTAKTPERSON LEGAL SÄLJARE             
009800*                                 CONTACT PERSON LEGAL SELLER             
009900     03 FEED-IDBG-LEG        PIC X(15).                                   
010000*                                 BANKGIRO LEGAL SÄLJARE                  
010100*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
010200*                                 R                                       
010300     03 FEED-IDPG-LEG        PIC X(15).                                   
010400*                                 POSTGIRO LEGAL SÄLJARE                  
010500*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
010600*                                 LER                                     
010700     03 FEED-BERESPRA-1      PIC X(35).                                   
010800*                                 DEL AV ANSV AVDELNINGS NAMN             
010900*                                 PART OF RESP DEPT  NAME                 
011000     03 FEED-BERESPRA-2      PIC X(35).                                   
011100*                                 DEL AV ANSV AVDELNINGS NAMN             
011200*                                 PART OF RESP DEPT  NAME                 
011300     03 FEED-ADRESP-STREET   PIC X(35).                                   
011400*                                 ANSVARIG AVDELNINGS GATUADRESS          
011500*                                 STREET ADDRESS OF RESPONSIBLE D         
011600*                                 PT.                                     
011700     03 FEED-ADRESP-BOX      PIC X(10).                                   
011800*                                 BOX ADRESS ANSVARIG AVDELNING           
011900*                                 BOX ADDRESS RESPONSIBLE DPT.            
012000     03 FEED-ADRESP-CITY     PIC X(35).                                   
012100*                                 ANSVARIG AVDELNINGS STAD (ELLER         
012200*                                  LIKNANDE)                              
012300*                                 CITY OF REPSONSIBLE DPT (OR SIM         
012400*                                 ILAR)                                   
012500     03 FEED-ADRESP-PCODE    PIC X(10).                                   
012600*                                 ANSVARIG AVDELNINGS POSTNUMMER          
012700*                                 RESPONSIBLE DPT. POSTAL CODE            
012800     03 FEED-IDLANDX3-RESP   PIC X(3).                                    
012900*                                 LANDKOD ANSVARIG AVD ETC                
013000*                                 COUNTRY CODE RESPONSIBLE DPT ET         
013100*                                 C                                       
013200     03 FEED-IDVAT-RESP      PIC X(17).                                   
013300*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
013400*                                 G AVD                                   
013500*                                 VAT REGISTRATION RESPONSIBLE DP         
013600*                                 T                                       
013700     03 FEED-IDTFN-RESP      PIC X(20).                                   
013800*                                 TELEFONNUMMER EXTERNT ANSVARIG          
013900*                                 AVD                                     
014000*                                 TELEPHONE NUMBER EXTERNAL RESPO         
014100*                                 NSIBLE DPT                              
014200     03 FEED-IDTFX-RESP      PIC X(20).                                   
014300*                                 FAXNUMMER ANSVARIG AVD                  
014400*                                 FAXNUMBER RESPONSIBLE DPT               
014500     03 FEED-IDMAIL-RESP     PIC X(60).                                   
014600*                                 MAIL ADRESS ANSVARIG AVD                
014700*                                 MAIL ADDRESS RESPONSIBLE DPT            
014800     03 FEED-BECONT-RESP     PIC X(35).                                   
014900*                                 KONTAKTPERSON ANSVARIG AVD              
015000*                                 CONTACT PERSON RESPONSIBLE DPT          
015100     03 FEED-IDBG-RESP       PIC X(15).                                   
015200*                                 BANKGIRO ANSVARIG AVD                   
015300*                                 BANC CHEQUE ACCOUNT RESPONSIBLE         
015400*                                  DPT                                    
015500     03 FEED-IDPG-RESP       PIC X(15).                                   
015600*                                 POSTGIRO ANSVARIG AVD                   
015700*                                 POSTAL CHEQUE ACCOUNT RESPONSIB         
015800*                                 LE DPT                                  
015900     03 FEED-BEBET-NAME1     PIC X(35).                                   
016000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
016100*                                 PART OF FINANCIAL CUSTOMER NAME         
016200     03 FEED-BEBET-NAME2     PIC X(35).                                   
016300*                                 DEL AV BETALNINGSANSVARIGS NAMN         
016400*                                 PART OF FINANCIAL CUSTOMER NAME         
016500     03 FEED-ADBET-STREET    PIC X(35).                                   
016600*                                 BETALARENS GATUADRESS                   
016700*                                 PAYER ADDRESS STREET                    
016800     03 FEED-ADBET-BOX       PIC X(10).                                   
016900*                                 BOXADRESS BETALNINGSANSVARIG            
017000*                                 PAYER BOX ADDRESS                       
017100     03 FEED-ADBET-CITY      PIC X(35).                                   
017200*                                 BETALARENS STADSADRESS                  
017300*                                 PAYER ADDRESS CITY                      
017400     03 FEED-ADBET-PCODE     PIC X(10).                                   
017500*                                 BETALARENS STADSADRESS POSTNR           
017600*                                 PAYER ADDRESS POSTAL CODE               
017700     03 FEED-IDVAT-BET       PIC X(17).                                   
017800*                                 MOMSREGISTRERINGSNUMMER BETALAR         
017900*                                 E                                       
018000*                                 VAT REGISTRATION NUMBER PAYER           
018100     03 FEED-IDLANDX3-BET    PIC X(3).                                    
018200*                                 LANDKOD BETALANDE KUND ETC              
018300*                                 COUNTRY CODE PAYING CUSTOMER ET         
018400*                                 C                                       
018500     03 FEED-KDTRADP         PIC X(4).                                    
018600*                                 TRADING PARTNER                         
018700*                                 TRADING PARTNER                         
018800     03 FEED-SUNTO-SERV      PIC S9(11)V9(2)     COMP-3.                  
018900*                                 TOTAL SALES AMOUNT SERVICES EXC         
019000*                                 L. VAT                                  
019100     03 FEED-SUNTO-PART      PIC S9(11)V9(2)     COMP-3.                  
019200*                                 TOTAL SALES AMOUNT PARTS EXCL.          
019300*                                 VAT                                     
019400     03 FEED-SUBTO-SERV      PIC S9(11)V9(2)     COMP-3.                  
019500*                                 TOTAL SALES AMOUNT SERVICES INC         
019600*                                 L. VAT                                  
019700     03 FEED-SUBTO-PART      PIC S9(11)V9(2)     COMP-3.                  
019800*                                 TOTAL SALES AMOUNT PARTS INCL.          
019900*                                 VAT                                     
020000     03 FEED-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
020100*                                 TOTAL SALES AMOUNT EXCL. VAT            
020200     03 FEED-SUBTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
020300*                                 TOTAL SALES AMOUNT INCL. VAT            
020400     03 FEED-SUVAT-BILLIT-TOT                                             
020500                             PIC S9(11)V9(2)     COMP-3.                  
020600*                                 SUMMERAT MOMSVÄRDE                      
020700*                                 TOTAL VAT VALUE                         
020800     03 FEED-IDBUNDLE        PIC X(15).                                   
020900*                                 BUNDLE ID                               
021000*                                 BUNDLE ID                               
021100     03 FEED-BEVOLREF        PIC X(10).                                   
021200*                                 VOLVO REFERENS                          
021300*                                 VOLVO REFERENCE                         
021400     03 FEED-IDREF           PIC X(15).                                   
021500*                                 REFERENS ID                             
021600*                                 REFERENCE ID                            
021700     03 FEED-IDREFRAD        PIC 9(5)            COMP-3.                  
021800*                                 REFERENSRADSNR                          
021900*                                 REFERENCE LINE NUMBER                   
022000     03 FEED-DAREFDAT        PIC 9(8).                                    
022100*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
022200*                                 REFERENCE DATE(YYYYMMDD)                
022300     03 FEED-IDEXCUST        OCCURS 3 TIMES                               
022400                             PIC X(15).                                   
022500*                                 EXTERNT KUNDID                          
022600*                                 EXTERNAL CUSTOMER ID                    
022700     03 FEED-IDOPTION        OCCURS 5 TIMES                               
022800                             PIC X(15).                                   
022900*                                 BRYTBEGREPP                             
023000*                                 OPTIONAL ID                             
023100     03 FEED-IDAPPEND        PIC X(8).                                    
023200*                                 APPENDIXVÄRDE                           
023300*                                 APPENDIX ITEM                           
023400     03 FEED-IDARTNR-FINANCE PIC X(50).                                   
023500*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
023600*                                 UK                                      
023700*                                 PART NUMBER FOR FINANCIAL USE           
023800     03 FEED-BEART           PIC X(25).                                   
023900*                                 ARTIKELBENÄMNING                        
024000*                                 PART DESCRIPTION                        
024100     03 FEED-IDSTATNR        PIC S9(9)           COMP-3.                  
024200*                                 STATISTISKT NUMMER                      
024300*                                 1 = NORSKT                              
024400*                                 2 = ENGELSKT                            
024500*                                 3 = BELGISKT                            
024600*                                 4 = PERUANSKT                           
024700*                                 5 = SVENSKT                             
024800*                                 6 =                                     
024900*                                 STATISTICAL NO.                         
025000     03 FEED-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
025100*                                 ORDERVIKT BRUTTO PER KOLLI              
025200*                                 ORDER WEIGHT GROSS PER CASE             
025300     03 FEED-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
025400*                                 ARTIKELVIKT NETTO (KG) MED EMB          
025500*                                 PART NET WEIGHT (KG) W/ PACKAGE         
025600     03 FEED-PRARTBTO        PIC S9(7)V9(2)      COMP-3.                  
025700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
025800*                                 GROSS SALES PRICE (SEK)                 
025900     03 FEED-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
026000*                                 ARTIKELPRIS NETTO                       
026100*                                 NET PRICE EACH   (FOB NET)              
026200     03 FEED-REARTRAB        PIC S9(2)V9(2)      COMP-3.                  
026300*                                 ARTIKELRABATT                           
026400*                                 PARTS DISCOUNT PERCENT                  
026500     03 FEED-KVBEART         PIC S9(7)           COMP-3.                  
026600*                                 BESTÄLLT ANTAL STYCKEN                  
026700*                                 ORDERED QUANTITY                        
026800     03 FEED-KVLEVART        PIC S9(7)           COMP-3.                  
026900*                                 LEVERERAT ANTAL STYCK                   
027000*                                 DELIVERED QUANTITY                      
027100     03 FEED-FLSPECPR        PIC X.                                       
027200*                                 SPECIALPRISFLAGGA                       
027300*                                 SPECIAL PRICE FLAG                      
027400     03 FEED-REVAT           PIC S9(3)V9(2)      COMP-3.                  
027500*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
027600*                                 VAT FACTOR                              
027700     03 FEED-KDVAT           PIC X(2).                                    
027800*                                 MOMSKOD                                 
027900*                                 VAT CODE                                
028000     03 FEED-KDARTURS        PIC X(2).                                    
028100*                                 ARTIKELURSPRUNGSKOD                     
028200*                                 COUNTRY OF ORIGIN                       
028300     03 FEED-KDANMORS        PIC X(2).                                    
028400*                                 ORSAK TILL LEVERANSANMÄRKNING           
028500*                                 DISCREPANCY REPORT REASON CODE          
028600     03 FEED-IDFAKREF        PIC S9(9)           COMP-3.                  
028700*                                 URSPRUNGLIGT FAKTURANUMMER              
028800*                                 ORIGINAL INVOICE NUMBER                 
028900     03 FEED-DAFAKREF        PIC 9(8).                                    
029000*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
029100*                                 MMDD)                                   
029200*                                 ORIGINAL INVOICING DATE (YYYYMM         
029300*                                 DD)                                     
029400     03 FEED-IDDC            PIC X(2).                                    
029500*                                 IDENTIFIERARE LAGER                     
029600*                                 WAREHOUSE IDENTIFIER                    
029700     03 FEED-KDFRAKT         PIC S9(3)           COMP-3.                  
029800*                                 FRAKTSÄTT DC TILL KUND                  
029900*                                 FREIGHT CODE                            
030000     03 FEED-BELEVVIL        PIC X(35).                                   
030100*                                 LEVERANSVILLKOR                         
030200*                                 DELIVERY TERMS                          
030300     03 FEED-IDACCNT         OCCURS 4 TIMES                               
030400                             PIC X(15).                                   
030500*                                 KONTOFÄLT                               
030600*                                 ACCOUNT FIELD                           
030700     03 FEED-SUNTO           PIC S9(11)V9(2)     COMP-3.                  
030800*                                 TOTAL SALES AMOUNT EXCL. VAT            
030900     03 FEED-SUBTO           PIC S9(11)V9(2)     COMP-3.                  
031000*                                 TOTAL SALES AMOUNT INCL. VAT            
031100     03 FEED-SUVAT-BILLIT    PIC S9(11)V9(2)     COMP-3.                  
031200*                                 SUMMERAT MOMSVÄRDE PER RAD              
031300*                                 TOTAL VAT VALUE PER LINE                
031400     03 FEED-BETEXT          PIC X(100).                                  
031500     03 FEED-KDVALISO-BET    PIC X(3).                                    
031600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
031700*                                 CURRENCY CODE BY ISO-STANDARD.          
031800     03 FEED-PRKURS-BET      PIC S9(6)V9(5)      COMP-3.                  
031900*                                 VALUTAKURS                              
032000*                                 CURRENCY EXCHANGE RATE                  
032100     03 FEED-BEANST          PIC X(25).                                   
032200*                                 ANSTÄLLDS NAMN                          
032300*                                 NAME OF EMPLOYED                        
032400     03 FEED-KDPARTTY        PIC X(3).                                    
032500*                                 TYP AV BETALARE                         
032600*                                 TYPE OF FIN.CUSTOMER                    
032700     03 FEED-KDPARTGR        PIC X(15).                                   
032800*                                 GRUPP AV BETALARE                       
032900*                                 FIN.CUSTOMER GROUP                      
033000     03 FEED-PRKURS-FAKBET   PIC S9(6)V9(5)      COMP-3.                  
033100*                                 VALUTAKURS                              
033200*                                 CURRENCY EXCHANGE RATE                  
033300     03 FEED-KDVALISO-SND    PIC X(3).                                    
033400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
033500*                                 CURRENCY CODE BY ISO-STANDARD.          
033600     03 FEED-PRKURS-SND      PIC S9(6)V9(5)      COMP-3.                  
033700*                                 VALUTAKURS                              
033800*                                 CURRENCY EXCHANGE RATE                  
033900     03 FEED-IDLEVNR-ART     PIC X(5).                                    
034000*                                 LEVERANTÖRNR PÅ ARTIKEL                 
034100*                                 PART SUPPLIER NUMBER                    
034200     03 FEED-KDTRADP-SC      PIC X(4).                                    
034300*                                 TRADING PARTNER                         
034400*                                 TRADING PARTNER                         
034500     03 FEED-IDVAT-AGENT     PIC X(17).                                   
034600*                                 MOMSREGISTRERINGSNUMMER AGENT           
034700*                                 VAT REGISTRATION VAT AGENT              
034800     03 FEED-IDVAT-DDGS-RESP PIC X(17).                                   
034900*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
035000*                                 G AVD                                   
035100*                                 VAT REGISTRATION RESPONSIBLE DP         
035200*                                 T                                       
035300*                                 VAT REG NO WHEN DIRECT DELIVERY         
035400*                                 T                                       
035500     03 FEED-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
035600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
035700*                                 AVERAGE COST FOREIGN CURRENCY           
035800     03 FEED-KDVALISO-AVC    PIC X(3).                                    
035900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
036000*                                 CURRENCY CODE BY ISO-STANDARD.          
036100*                                 AVERAGE COST CURRENCY                   
036200     03 FEED-FLPCOO          PIC X.                                       
036300*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
036400*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
036500     03 FEED-KDVALISO-RECALC PIC X(3).                                    
036600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
036700*                                 CURRENCY CODE BY ISO-STANDARD.          
036800     03 FEED-SUNTO-TOT-RECALC                                             
036900                             PIC S9(11)V9(2)     COMP-3.                  
037000*                                 TOTAL SALES AMOUNT EXCL. VAT            
037100*                                 NET TOT RECALCULATION EXCL.VAT          
037200     03 FEED-SUBTO-TOT-RECALC                                             
037300                             PIC S9(11)V9(2)     COMP-3.                  
037400*                                 TOTAL SALES AMOUNT INCL. VAT            
037500*                                 BRUTTO TOT RECALCULATION                
037600     03 FEED-SUVAT-BILLIT-TOT-REC                                         
037700                             PIC S9(11)V9(2)     COMP-3.                  
037800*                                 SUMMERAT MOMSVÄRDE                      
037900*                                 TOTAL VAT VALUE                         
038000     03 FEED-KDPRMOD         PIC X(2).                                    
038100*                                 VILKEN PRISMODELL SOM ANVÄNDS           
038200*                                 WHAT PRICE MODEL THAT IS USED           
038300*** END OF VILMAII-COPY LENGTH= 1707 BYTES                                
