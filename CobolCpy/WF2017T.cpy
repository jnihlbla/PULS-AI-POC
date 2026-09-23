000100 01  FEED-WF2017T.                                                        
000200*                                 FEEDBACK DATA                           
000300     03 FEED-DAEXDAT         OCCURS 100 TIMES                             
000400                             PIC 9(8).                                    
000500*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000600*                                 EXECUTION DATE (YYYYMMDD)               
000700     03 FEED-TIEXTID         OCCURS 100 TIMES                             
000800                             PIC S9(7)           COMP-3.                  
000900*                                 EXEKVERINGSTIDPUNKT                     
001000*                                 EXECUTION TIME                          
001100     03 FEED-IDLEGSEL        OCCURS 100 TIMES                             
001200                             PIC X(4).                                    
001300*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001400*                                 LEGAL SELLER IDENTITY                   
001500     03 FEED-KDVALISO        OCCURS 100 TIMES                             
001600                             PIC X(3).                                    
001700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001800*                                 CURRENCY CODE BY ISO-STANDARD.          
001900     03 FEED-PRKURS          OCCURS 100 TIMES                             
002000                             PIC S9(6)V9(5)      COMP-3.                  
002100*                                 VALUTAKURS                              
002200*                                 CURRENCY EXCHANGE RATE                  
002300     03 FEED-IDLANDX3-SEND   OCCURS 100 TIMES                             
002400                             PIC X(3).                                    
002500*                                 LANDKOD SÄNDANDE LAND                   
002600*                                 COUNTRY CODE SENDING COUNTRY            
002700     03 FEED-IDLANDX3-REC    OCCURS 100 TIMES                             
002800                             PIC X(3).                                    
002900*                                 LANDKOD MOTTAGANDE LAND                 
003000*                                 COUNTRY CODE RECEIVING COUNTRY          
003100     03 FEED-IDLEVNR         OCCURS 100 TIMES                             
003200                             PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003500     03 FEED-IDPARTNR        OCCURS 100 TIMES                             
003600                             PIC X(9).                                    
003700*                                 FINANCIELL KUND                         
003800*                                 FINANCIAL CUST                          
003900     03 FEED-KDFINDOC        OCCURS 100 TIMES                             
004000                             PIC X(4).                                    
004100*                                 TYP FINANSIELLT DOKUMENT                
004200*                                 FINANCIAL DOCUMENT TYPE                 
004300     03 FEED-FLSOFT          OCCURS 100 TIMES                             
004400                             PIC X.                                       
004500*                                 FLAGGA SOFTVARA                         
004600*                                 SOFTWARE MARK                           
004700     03 FEED-FLFREE          OCCURS 100 TIMES                             
004800                             PIC X.                                       
004900*                                 GRATISFATURA                            
005000*                                 FREE INVOICE                            
005100     03 FEED-DAFINDOC        OCCURS 100 TIMES                             
005200                             PIC 9(8).                                    
005300*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
005400*                                 INVOICING DATE   (YYYYMMDD)             
005500     03 FEED-IDFINDOC        OCCURS 100 TIMES                             
005600                             PIC S9(9)           COMP-3.                  
005700*                                 FINANSIELLT DOKUMENT ID                 
005800*                                 FINANCIAL DOCUMENT ID                   
005900     03 FEED-IDSYSTEM-SEND   OCCURS 100 TIMES                             
006000                             PIC X(4).                                    
006100*                                 VOLVO SÄNDANDE SYSTEM                   
006200*                                 VOLVO SENDING SYSTEM                    
006300     03 FEED-IDSYSTEM-REC    OCCURS 100 TIMES                             
006400                             PIC X(4).                                    
006500*                                 VOLVO MOTTAGANDE SYSTEM                 
006600*                                 VOLVO RECEIVING SYSTEM                  
006700     03 FEED-IDSPRAK         OCCURS 100 TIMES                             
006800                             PIC X(2).                                    
006900*                                 2-STÄLLIG ISO SPRÅKKOD                  
007000*                                 2-LETTER ISO LANGUAGE CODE              
007100     03 FEED-KDBETALV        OCCURS 100 TIMES                             
007200                             PIC X(4).                                    
007300*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
007400*                                 TERMS OF PAYMENT                        
007500     03 FEED-BEBETVIL        OCCURS 100 TIMES                             
007600                             PIC X(30).                                   
007700*                                 BETALNINGSVILLKORSTEXT                  
007800*                                 TERMS OF PAYMENT TEXT                   
007900     03 FEED-BELEGRAD-1      OCCURS 100 TIMES                             
008000                             PIC X(35).                                   
008100*                                 DEL AV LEGAL SELLER NAMN                
008200*                                 PART OF LEGAL SELLER NAME               
008300     03 FEED-BELEGRAD-2      OCCURS 100 TIMES                             
008400                             PIC X(35).                                   
008500*                                 DEL AV LEGAL SELLER NAMN                
008600*                                 PART OF LEGAL SELLER NAME               
008700     03 FEED-ADLEG-STREET    OCCURS 100 TIMES                             
008800                             PIC X(35).                                   
008900*                                 LEGAL SELLER GATUADRESS                 
009000*                                 LEGAL SELLER STREET ADDRESS             
009100     03 FEED-ADLEG-BOX       OCCURS 100 TIMES                             
009200                             PIC X(10).                                   
009300*                                 BOXADRESS LEGAL SELLER                  
009400*                                 LEGAL SELLER BOX ADDRESS                
009500     03 FEED-ADLEG-CITY      OCCURS 100 TIMES                             
009600                             PIC X(35).                                   
009700*                                 LEGAL SÄLJARES ADRESS STAD              
009800*                                 LEGAL SELLER ADDRESS CITY               
009900     03 FEED-ADLEG-PCODE     OCCURS 100 TIMES                             
010000                             PIC X(10).                                   
010100*                                 LEGAL SELLER ADRESS POSTNR              
010200*                                 LEGAL SELLER POSTAL CODE                
010300     03 FEED-IDLANDX3-LEG    OCCURS 100 TIMES                             
010400                             PIC X(3).                                    
010500*                                 LANDKOD LEGAL SÄLJARE                   
010600*                                 COUNTRY CODE LEGAL SELLER               
010700     03 FEED-IDVAT-LEG       OCCURS 100 TIMES                             
010800                             PIC X(17).                                   
010900*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
011000*                                 ÄLJARE                                  
011100*                                 VAT REGISTRATION LEGAL PAYER            
011200     03 FEED-IDTFN-LEG       OCCURS 100 TIMES                             
011300                             PIC X(20).                                   
011400*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
011500*                                 JARE                                    
011600*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
011700*                                  SELLER                                 
011800     03 FEED-IDTFX-LEG       OCCURS 100 TIMES                             
011900                             PIC X(20).                                   
012000*                                 FAXNUMMER LEGAL SÄLJARE                 
012100*                                 FAXNUMBER LEGAL SELLER                  
012200     03 FEED-IDMAIL-LEG      OCCURS 100 TIMES                             
012300                             PIC X(60).                                   
012400*                                 MAIL ADRESS LEGAL SÄLJARE               
012500*                                 MAIL ADDRESS LEGAL SELLER               
012600     03 FEED-BECONT-LEG      OCCURS 100 TIMES                             
012700                             PIC X(35).                                   
012800*                                 KONTAKTPERSON LEGAL SÄLJARE             
012900*                                 CONTACT PERSON LEGAL SELLER             
013000     03 FEED-IDBG-LEG        OCCURS 100 TIMES                             
013100                             PIC X(15).                                   
013200*                                 BANKGIRO LEGAL SÄLJARE                  
013300*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
013400*                                 R                                       
013500     03 FEED-IDPG-LEG        OCCURS 100 TIMES                             
013600                             PIC X(15).                                   
013700*                                 POSTGIRO LEGAL SÄLJARE                  
013800*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
013900*                                 LER                                     
014000     03 FEED-BERESPRA-1      OCCURS 100 TIMES                             
014100                             PIC X(35).                                   
014200*                                 DEL AV ANSV AVDELNINGS NAMN             
014300*                                 PART OF RESP DEPT  NAME                 
014400     03 FEED-BERESPRA-2      OCCURS 100 TIMES                             
014500                             PIC X(35).                                   
014600*                                 DEL AV ANSV AVDELNINGS NAMN             
014700*                                 PART OF RESP DEPT  NAME                 
014800     03 FEED-ADRESP-STREET   OCCURS 100 TIMES                             
014900                             PIC X(35).                                   
015000*                                 ANSVARIG AVDELNINGS GATUADRESS          
015100*                                 STREET ADDRESS OF RESPONSIBLE D         
015200*                                 PT.                                     
015300     03 FEED-ADRESP-BOX      OCCURS 100 TIMES                             
015400                             PIC X(10).                                   
015500*                                 BOX ADRESS ANSVARIG AVDELNING           
015600*                                 BOX ADDRESS RESPONSIBLE DPT.            
015700     03 FEED-ADRESP-CITY     OCCURS 100 TIMES                             
015800                             PIC X(35).                                   
015900*                                 ANSVARIG AVDELNINGS STAD (ELLER         
016000*                                  LIKNANDE)                              
016100*                                 CITY OF REPSONSIBLE DPT (OR SIM         
016200*                                 ILAR)                                   
016300     03 FEED-ADRESP-PCODE    OCCURS 100 TIMES                             
016400                             PIC X(10).                                   
016500*                                 ANSVARIG AVDELNINGS POSTNUMMER          
016600*                                 RESPONSIBLE DPT. POSTAL CODE            
016700     03 FEED-IDLANDX3-RESP   OCCURS 100 TIMES                             
016800                             PIC X(3).                                    
016900*                                 LANDKOD ANSVARIG AVD ETC                
017000*                                 COUNTRY CODE RESPONSIBLE DPT ET         
017100*                                 C                                       
017200     03 FEED-IDVAT-RESP      OCCURS 100 TIMES                             
017300                             PIC X(17).                                   
017400*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
017500*                                 G AVD                                   
017600*                                 VAT REGISTRATION RESPONSIBLE DP         
017700*                                 T                                       
017800     03 FEED-IDTFN-RESP      OCCURS 100 TIMES                             
017900                             PIC X(20).                                   
018000*                                 TELEFONNUMMER EXTERNT ANSVARIG          
018100*                                 AVD                                     
018200*                                 TELEPHONE NUMBER EXTERNAL RESPO         
018300*                                 NSIBLE DPT                              
018400     03 FEED-IDTFX-RESP      OCCURS 100 TIMES                             
018500                             PIC X(20).                                   
018600*                                 FAXNUMMER ANSVARIG AVD                  
018700*                                 FAXNUMBER RESPONSIBLE DPT               
018800     03 FEED-IDMAIL-RESP     OCCURS 100 TIMES                             
018900                             PIC X(60).                                   
019000*                                 MAIL ADRESS ANSVARIG AVD                
019100*                                 MAIL ADDRESS RESPONSIBLE DPT            
019200     03 FEED-BECONT-RESP     OCCURS 100 TIMES                             
019300                             PIC X(35).                                   
019400*                                 KONTAKTPERSON ANSVARIG AVD              
019500*                                 CONTACT PERSON RESPONSIBLE DPT          
019600     03 FEED-IDBG-RESP       OCCURS 100 TIMES                             
019700                             PIC X(15).                                   
019800*                                 BANKGIRO ANSVARIG AVD                   
019900*                                 BANC CHEQUE ACCOUNT RESPONSIBLE         
020000*                                  DPT                                    
020100     03 FEED-IDPG-RESP       OCCURS 100 TIMES                             
020200                             PIC X(15).                                   
020300*                                 POSTGIRO ANSVARIG AVD                   
020400*                                 POSTAL CHEQUE ACCOUNT RESPONSIB         
020500*                                 LE DPT                                  
020600     03 FEED-BEBET-NAME1     OCCURS 100 TIMES                             
020700                             PIC X(35).                                   
020800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
020900*                                 PART OF FINANCIAL CUSTOMER NAME         
021000     03 FEED-BEBET-NAME2     OCCURS 100 TIMES                             
021100                             PIC X(35).                                   
021200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
021300*                                 PART OF FINANCIAL CUSTOMER NAME         
021400     03 FEED-ADBET-STREET    OCCURS 100 TIMES                             
021500                             PIC X(35).                                   
021600*                                 BETALARENS GATUADRESS                   
021700*                                 PAYER ADDRESS STREET                    
021800     03 FEED-ADBET-BOX       OCCURS 100 TIMES                             
021900                             PIC X(10).                                   
022000*                                 BOXADRESS BETALNINGSANSVARIG            
022100*                                 PAYER BOX ADDRESS                       
022200     03 FEED-ADBET-CITY      OCCURS 100 TIMES                             
022300                             PIC X(35).                                   
022400*                                 BETALARENS STADSADRESS                  
022500*                                 PAYER ADDRESS CITY                      
022600     03 FEED-ADBET-PCODE     OCCURS 100 TIMES                             
022700                             PIC X(10).                                   
022800*                                 BETALARENS STADSADRESS POSTNR           
022900*                                 PAYER ADDRESS POSTAL CODE               
023000     03 FEED-IDVAT-BET       OCCURS 100 TIMES                             
023100                             PIC X(17).                                   
023200*                                 MOMSREGISTRERINGSNUMMER BETALAR         
023300*                                 E                                       
023400*                                 VAT REGISTRATION NUMBER PAYER           
023500     03 FEED-IDLANDX3-BET    OCCURS 100 TIMES                             
023600                             PIC X(3).                                    
023700*                                 LANDKOD BETALANDE KUND ETC              
023800*                                 COUNTRY CODE PAYING CUSTOMER ET         
023900*                                 C                                       
024000     03 FEED-KDTRADP         OCCURS 100 TIMES                             
024100                             PIC X(4).                                    
024200*                                 TRADING PARTNER                         
024300*                                 TRADING PARTNER                         
024400     03 FEED-SUNTO-SERV      OCCURS 100 TIMES                             
024500                             PIC S9(11)V9(2)     COMP-3.                  
024600*                                 TOTAL SALES AMOUNT SERVICES EXC         
024700*                                 L. VAT                                  
024800     03 FEED-SUNTO-PART      OCCURS 100 TIMES                             
024900                             PIC S9(11)V9(2)     COMP-3.                  
025000*                                 TOTAL SALES AMOUNT PARTS EXCL.          
025100*                                 VAT                                     
025200     03 FEED-SUBTO-SERV      OCCURS 100 TIMES                             
025300                             PIC S9(11)V9(2)     COMP-3.                  
025400*                                 TOTAL SALES AMOUNT SERVICES INC         
025500*                                 L. VAT                                  
025600     03 FEED-SUBTO-PART      OCCURS 100 TIMES                             
025700                             PIC S9(11)V9(2)     COMP-3.                  
025800*                                 TOTAL SALES AMOUNT PARTS INCL.          
025900*                                 VAT                                     
026000     03 FEED-SUNTO-TOT       OCCURS 100 TIMES                             
026100                             PIC S9(11)V9(2)     COMP-3.                  
026200*                                 TOTAL SALES AMOUNT EXCL. VAT            
026300     03 FEED-SUBTO-TOT       OCCURS 100 TIMES                             
026400                             PIC S9(11)V9(2)     COMP-3.                  
026500*                                 TOTAL SALES AMOUNT INCL. VAT            
026600     03 FEED-SUVAT-BILLIT-TOT                                             
026700                             OCCURS 100 TIMES                             
026800                             PIC S9(11)V9(2)     COMP-3.                  
026900*                                 SUMMERAT MOMSVÄRDE                      
027000*                                 TOTAL VAT VALUE                         
027100     03 FEED-IDBUNDLE        OCCURS 100 TIMES                             
027200                             PIC X(15).                                   
027300*                                 BUNDLE ID                               
027400*                                 BUNDLE ID                               
027500     03 FEED-BEVOLREF        OCCURS 100 TIMES                             
027600                             PIC X(10).                                   
027700*                                 VOLVO REFERENS                          
027800*                                 VOLVO REFERENCE                         
027900     03 FEED-IDREF           OCCURS 100 TIMES                             
028000                             PIC X(15).                                   
028100*                                 REFERENS ID                             
028200*                                 REFERENCE ID                            
028300     03 FEED-IDREFRAD        OCCURS 100 TIMES                             
028400                             PIC 9(5)            COMP-3.                  
028500*                                 REFERENSRADSNR                          
028600*                                 REFERENCE LINE NUMBER                   
028700     03 FEED-DAREFDAT        OCCURS 100 TIMES                             
028800                             PIC 9(8).                                    
028900*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
029000*                                 REFERENCE DATE(YYYYMMDD)                
029100     03 FEED-IDAPPEND        OCCURS 100 TIMES                             
029200                             PIC X(8).                                    
029300*                                 APPENDIXVÄRDE                           
029400*                                 APPENDIX ITEM                           
029500     03 FEED-IDARTNR-FINANCE OCCURS 100 TIMES                             
029600                             PIC X(50).                                   
029700*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
029800*                                 UK                                      
029900*                                 PART NUMBER FOR FINANCIAL USE           
030000     03 FEED-BEART           OCCURS 100 TIMES                             
030100                             PIC X(25).                                   
030200*                                 ARTIKELBENÄMNING                        
030300*                                 PART DESCRIPTION                        
030400     03 FEED-IDSTATNR        OCCURS 100 TIMES                             
030500                             PIC S9(9)           COMP-3.                  
030600*                                 STATISTISKT NUMMER                      
030700*                                 1 = NORSKT                              
030800*                                 2 = ENGELSKT                            
030900*                                 3 = BELGISKT                            
031000*                                 4 = PERUANSKT                           
031100*                                 5 = SVENSKT                             
031200*                                 6 =                                     
031300*                                 STATISTICAL NO.                         
031400     03 FEED-VKORDBTO-KOLLI  OCCURS 100 TIMES                             
031500                             PIC S9(6)V9(1)      COMP-3.                  
031600*                                 ORDERVIKT BRUTTO PER KOLLI              
031700*                                 ORDER WEIGHT GROSS PER CASE             
031800     03 FEED-VKARTNTO        OCCURS 100 TIMES                             
031900                             PIC S9(4)V9(3)      COMP-3.                  
032000*                                 ARTIKELVIKT NETTO (KG) MED EMB          
032100*                                 PART NET WEIGHT (KG) W/ PACKAGE         
032200     03 FEED-PRARTBTO        OCCURS 100 TIMES                             
032300                             PIC S9(7)V9(2)      COMP-3.                  
032400*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
032500*                                 GROSS SALES PRICE (SEK)                 
032600     03 FEED-PRARTNTO        OCCURS 100 TIMES                             
032700                             PIC S9(7)V9(2)      COMP-3.                  
032800*                                 ARTIKELPRIS NETTO                       
032900*                                 NET PRICE EACH   (FOB NET)              
033000     03 FEED-REARTRAB        OCCURS 100 TIMES                             
033100                             PIC S9(2)V9(2)      COMP-3.                  
033200*                                 ARTIKELRABATT                           
033300*                                 PARTS DISCOUNT PERCENT                  
033400     03 FEED-KVBEART         OCCURS 100 TIMES                             
033500                             PIC S9(7)           COMP-3.                  
033600*                                 BESTÄLLT ANTAL STYCKEN                  
033700*                                 ORDERED QUANTITY                        
033800     03 FEED-KVLEVART        OCCURS 100 TIMES                             
033900                             PIC S9(7)           COMP-3.                  
034000*                                 LEVERERAT ANTAL STYCK                   
034100*                                 DELIVERED QUANTITY                      
034200     03 FEED-FLSPECPR        OCCURS 100 TIMES                             
034300                             PIC X.                                       
034400*                                 SPECIALPRISFLAGGA                       
034500*                                 SPECIAL PRICE FLAG                      
034600     03 FEED-REVAT           OCCURS 100 TIMES                             
034700                             PIC S9(3)V9(2)      COMP-3.                  
034800*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
034900*                                 VAT FACTOR                              
035000     03 FEED-KDVAT           OCCURS 100 TIMES                             
035100                             PIC X(2).                                    
035200*                                 MOMSKOD                                 
035300*                                 VAT CODE                                
035400     03 FEED-KDARTURS        OCCURS 100 TIMES                             
035500                             PIC X(2).                                    
035600*                                 ARTIKELURSPRUNGSKOD                     
035700*                                 COUNTRY OF ORIGIN                       
035800     03 FEED-KDANMORS        OCCURS 100 TIMES                             
035900                             PIC X(2).                                    
036000*                                 ORSAK TILL LEVERANSANMÄRKNING           
036100*                                 DISCREPANCY REPORT REASON CODE          
036200     03 FEED-IDFAKREF        OCCURS 100 TIMES                             
036300                             PIC S9(9)           COMP-3.                  
036400*                                 URSPRUNGLIGT FAKTURANUMMER              
036500*                                 ORIGINAL INVOICE NUMBER                 
036600     03 FEED-DAFAKREF        OCCURS 100 TIMES                             
036700                             PIC 9(8).                                    
036800*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
036900*                                 MMDD)                                   
037000*                                 ORIGINAL INVOICING DATE (YYYYMM         
037100*                                 DD)                                     
037200     03 FEED-IDDC            OCCURS 100 TIMES                             
037300                             PIC X(2).                                    
037400*                                 IDENTIFIERARE LAGER                     
037500*                                 WAREHOUSE IDENTIFIER                    
037600     03 FEED-KDFRAKT         OCCURS 100 TIMES                             
037700                             PIC S9(3)           COMP-3.                  
037800*                                 FRAKTSÄTT DC TILL KUND                  
037900*                                 FREIGHT CODE                            
038000     03 FEED-BELEVVIL        OCCURS 100 TIMES                             
038100                             PIC X(35).                                   
038200*                                 LEVERANSVILLKOR                         
038300*                                 DELIVERY TERMS                          
038400     03 FEED-SUNTO           OCCURS 100 TIMES                             
038500                             PIC S9(11)V9(2)     COMP-3.                  
038600*                                 TOTAL SALES AMOUNT EXCL. VAT            
038700     03 FEED-SUBTO           OCCURS 100 TIMES                             
038800                             PIC S9(11)V9(2)     COMP-3.                  
038900*                                 TOTAL SALES AMOUNT INCL. VAT            
039000     03 FEED-SUVAT-BILLIT    OCCURS 100 TIMES                             
039100                             PIC S9(11)V9(2)     COMP-3.                  
039200*                                 SUMMERAT MOMSVÄRDE PER RAD              
039300*                                 TOTAL VAT VALUE PER LINE                
039400     03 FEED-BETEXT          OCCURS 100 TIMES                             
039500                             PIC X(100).                                  
039600     03 FEED-KDVALISO-BET    OCCURS 100 TIMES                             
039700                             PIC X(3).                                    
039800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
039900*                                 CURRENCY CODE BY ISO-STANDARD.          
040000     03 FEED-PRKURS-BET      OCCURS 100 TIMES                             
040100                             PIC S9(6)V9(5)      COMP-3.                  
040200*                                 VALUTAKURS                              
040300*                                 CURRENCY EXCHANGE RATE                  
040400     03 FEED-BEANST          OCCURS 100 TIMES                             
040500                             PIC X(25).                                   
040600*                                 ANSTÄLLDS NAMN                          
040700*                                 NAME OF EMPLOYED                        
040800     03 FEED-KDPARTTY        OCCURS 100 TIMES                             
040900                             PIC X(3).                                    
041000*                                 TYP AV BETALARE                         
041100*                                 TYPE OF FIN.CUSTOMER                    
041200     03 FEED-KDPARTGR        OCCURS 100 TIMES                             
041300                             PIC X(15).                                   
041400*                                 GRUPP AV BETALARE                       
041500*                                 FIN.CUSTOMER GROUP                      
041600     03 FEED-PRKURS-FAKBET   OCCURS 100 TIMES                             
041700                             PIC S9(6)V9(5)      COMP-3.                  
041800*                                 VALUTAKURS                              
041900*                                 CURRENCY EXCHANGE RATE                  
042000     03 FEED-TABELLRAD       OCCURS 100 TIMES.                            
042100*                                 GRUPP MED TABELLRADER                   
042200        05 FEED-IDEXCUST     OCCURS 3 TIMES                               
042300                             PIC X(15).                                   
042400*                                 EXTERNT KUNDID                          
042500*                                 EXTERNAL CUSTOMER ID                    
042600        05 FEED-IDOPTION     OCCURS 5 TIMES                               
042700                             PIC X(15).                                   
042800*                                 BRYTBEGREPP                             
042900*                                 OPTIONAL ID                             
043000        05 FEED-IDACCNT      OCCURS 4 TIMES                               
043100                             PIC X(15).                                   
043200*                                 KONTOFÄLT                               
043300*                                 ACCOUNT FIELD                           
043400     03 FEED-KDVALISO-SND    OCCURS 100 TIMES                             
043500                             PIC X(3).                                    
043600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
043700*                                 CURRENCY CODE BY ISO-STANDARD.          
043800     03 FEED-PRKURS-SND      OCCURS 100 TIMES                             
043900                             PIC S9(6)V9(5)      COMP-3.                  
044000*                                 VALUTAKURS                              
044100*                                 CURRENCY EXCHANGE RATE                  
044200     03 FEED-IDLEVNR-ART     OCCURS 100 TIMES                             
044300                             PIC X(5).                                    
044400*                                 LEVERANTÖRNR PÅ ARTIKEL                 
044500*                                 PART SUPPLIER NUMBER                    
044600     03 FEED-IDVAT-AGENT     OCCURS 100 TIMES                             
044700                             PIC X(17).                                   
044800*                                 MOMSREGISTRERINGSNUMMER AGENT           
044900*                                 VAT REGISTRATION VAT AGENT              
045000     03 FEED-IDVAT-DDGS-RESP OCCURS 100 TIMES                             
045100                             PIC X(17).                                   
045200*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
045300*                                 G AVD                                   
045400*                                 VAT REGISTRATION RESPONSIBLE DP         
045500*                                 T                                       
045600*                                 VAT REG NO WHEN DIRECT DELIVERY         
045700*                                 T                                       
045800     03 FEED-PRAVCOST        OCCURS 100 TIMES                             
045900                             PIC S9(7)V9(2)      COMP-3.                  
046000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
046100*                                 AVERAGE COST FOREIGN CURRENCY           
046200     03 FEED-KDVALISO-AVC    OCCURS 100 TIMES                             
046300                             PIC X(3).                                    
046400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
046500*                                 CURRENCY CODE BY ISO-STANDARD.          
046600*                                 AVERAGE COST CURRENCY                   
046700     03 FEED-FLPCOO          OCCURS 100 TIMES                             
046800                             PIC X.                                       
046900*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
047000*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
047100     03 FEED-KDVALISO-RECALC OCCURS 100 TIMES                             
047200                             PIC X(3).                                    
047300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
047400*                                 CURRENCY CODE BY ISO-STANDARD.          
047500     03 FEED-SUNTO-TOT-RECALC                                             
047600                             OCCURS 100 TIMES                             
047700                             PIC S9(11)V9(2)     COMP-3.                  
047800*                                 TOTAL SALES AMOUNT EXCL. VAT            
047900*                                 NET TOT RECALCULATION EXCL.VAT          
048000     03 FEED-SUBTO-TOT-RECALC                                             
048100                             OCCURS 100 TIMES                             
048200                             PIC S9(11)V9(2)     COMP-3.                  
048300*                                 TOTAL SALES AMOUNT INCL. VAT            
048400*                                 BRUTTO TOT RECALCULATION                
048500     03 FEED-SUVAT-BILLIT-TOT-REC                                         
048600                             OCCURS 100 TIMES                             
048700                             PIC S9(11)V9(2)     COMP-3.                  
048800*                                 SUMMERAT MOMSVÄRDE                      
048900*                                 TOTAL VAT VALUE                         
049000     03 FEED-KDPRMOD         OCCURS 100 TIMES                             
049100                             PIC X(2).                                    
049200*                                 VILKEN PRISMODELL SOM ANVÄNDS           
049300*                                 WHAT PRICE MODEL THAT IS USED           
049400*** END OF VILMAII-COPY LENGTH= 170300 BYTES                              
