000100 01  HEAD-WF2011T1.                                                       
000200*                                 DOCUMENT HEADER DATA                    
000300     03 HEAD-IDPTYP          OCCURS 100 TIMES                             
000400                             PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 HEAD-DAFINDOC        OCCURS 100 TIMES                             
000800                             PIC 9(8).                                    
000900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001000*                                 INVOICING DATE   (YYYYMMDD)             
001100     03 HEAD-IDFINDOC        OCCURS 100 TIMES                             
001200                             PIC S9(9)           COMP-3.                  
001300*                                 FINANSIELLT DOKUMENT ID                 
001400*                                 FINANCIAL DOCUMENT ID                   
001500     03 HEAD-IDLOPNR         OCCURS 100 TIMES                             
001600                             PIC S9(5)           COMP-3.                  
001700*                                 LÖPNUMMER          IDLOPNR-002          
001800     03 HEAD-IDLEGSEL        OCCURS 100 TIMES                             
001900                             PIC X(4).                                    
002000*                                 FAKTURERANDE FÖRETAG TEX VCCS           
002100*                                 LEGAL SELLER IDENTITY                   
002200     03 HEAD-BEFORMS         OCCURS 100 TIMES                             
002300                             PIC X(15).                                   
002400*                                 BENÄMNING PÅ DOKUMENTFORMAT             
002500*                                                                         
002600*                                 DESCRIPTION OF DOCUMENT FORMAT          
002700*                                                                         
002800     03 HEAD-DAEXDAT         OCCURS 100 TIMES                             
002900                             PIC 9(8).                                    
003000*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
003100*                                 EXECUTION DATE (YYYYMMDD)               
003200     03 HEAD-TIEXTID         OCCURS 100 TIMES                             
003300                             PIC S9(7)           COMP-3.                  
003400*                                 EXEKVERINGSTIDPUNKT                     
003500*                                 EXECUTION TIME                          
003600     03 HEAD-KDVALISO        OCCURS 100 TIMES                             
003700                             PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900*                                 CURRENCY CODE BY ISO-STANDARD.          
004000     03 HEAD-IDLANDX3-SEND   OCCURS 100 TIMES                             
004100                             PIC X(3).                                    
004200*                                 LANDKOD SÄNDANDE LAND                   
004300*                                 COUNTRY CODE SENDING COUNTRY            
004400     03 HEAD-IDLEVNR         OCCURS 100 TIMES                             
004500                             PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 HEAD-IDPARTNR        OCCURS 100 TIMES                             
004900                             PIC X(9).                                    
005000*                                 PARTNERNUMMER                           
005100*                                 PARTNER NO                              
005200     03 HEAD-KDFINDOC        OCCURS 100 TIMES                             
005300                             PIC X(4).                                    
005400*                                 TYP FINANSIELLT DOKUMENT                
005500*                                 FINANCIAL DOCUMENT TYPE                 
005600     03 HEAD-FLSOFT          OCCURS 100 TIMES                             
005700                             PIC X.                                       
005800*                                 FLAGGA SOFTVARA                         
005900*                                 SOFTWARE MARK                           
006000     03 HEAD-FLFREE          OCCURS 100 TIMES                             
006100                             PIC X.                                       
006200*                                 GRATISFATURA                            
006300*                                 FREE INVOICE                            
006400     03 HEAD-IDBREAK-1       OCCURS 100 TIMES                             
006500                             PIC X(8).                                    
006600*                                 BRYTVÄRDE                               
006700*                                 BREAK VALUE                             
006800     03 HEAD-IDBREAK-2       OCCURS 100 TIMES                             
006900                             PIC X(8).                                    
007000*                                 BRYTVÄRDE                               
007100*                                 BREAK VALUE                             
007200     03 HEAD-IDSPRAK         OCCURS 100 TIMES                             
007300                             PIC X(2).                                    
007400*                                 2-STÄLLIG ISO SPRÅKKOD                  
007500*                                 2-LETTER ISO LANGUAGE CODE              
007600     03 HEAD-BEBETVIL        OCCURS 100 TIMES                             
007700                             PIC X(30).                                   
007800*                                 BETALNINGSVILLKORSTEXT                  
007900*                                 TERMS OF PAYMENT TEXT                   
008000     03 HEAD-BELEGRAD-1      OCCURS 100 TIMES                             
008100                             PIC X(35).                                   
008200*                                 DEL AV LEGAL SELLER NAMN                
008300*                                 PART OF LEGAL SELLER NAME               
008400     03 HEAD-BELEGRAD-2      OCCURS 100 TIMES                             
008500                             PIC X(35).                                   
008600*                                 DEL AV LEGAL SELLER NAMN                
008700*                                 PART OF LEGAL SELLER NAME               
008800     03 HEAD-ADLEG-STREET    OCCURS 100 TIMES                             
008900                             PIC X(35).                                   
009000*                                 LEGAL SELLER GATUADRESS                 
009100*                                 LEGAL SELLER STREET ADDRESS             
009200     03 HEAD-ADLEG-BOX       OCCURS 100 TIMES                             
009300                             PIC X(10).                                   
009400*                                 BOXADRESS LEGAL SELLER                  
009500*                                 LEGAL SELLER BOX ADDRESS                
009600     03 HEAD-ADLEG-CITY      OCCURS 100 TIMES                             
009700                             PIC X(35).                                   
009800*                                 LEGAL SÄLJARES ADRESS STAD              
009900*                                 LEGAL SELLER ADDRESS CITY               
010000     03 HEAD-ADLEG-PCODE     OCCURS 100 TIMES                             
010100                             PIC X(10).                                   
010200*                                 LEGAL SELLER ADRESS POSTNR              
010300*                                 LEGAL SELLER POSTAL CODE                
010400     03 HEAD-IDLANDX3-LEG    OCCURS 100 TIMES                             
010500                             PIC X(3).                                    
010600*                                 LANDKOD LEGAL SÄLJARE                   
010700*                                 COUNTRY CODE LEGAL SELLER               
010800     03 HEAD-IDVAT-LEG       OCCURS 100 TIMES                             
010900                             PIC X(17).                                   
011000*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
011100*                                 ÄLJARE                                  
011200*                                 VAT REGISTRATION LEGAL PAYER            
011300     03 HEAD-IDTFN-LEG       OCCURS 100 TIMES                             
011400                             PIC X(20).                                   
011500*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
011600*                                 JARE                                    
011700*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
011800*                                  SELLER                                 
011900     03 HEAD-IDTFX-LEG       OCCURS 100 TIMES                             
012000                             PIC X(20).                                   
012100*                                 FAXNUMMER LEGAL SÄLJARE                 
012200*                                 FAXNUMBER LEGAL SELLER                  
012300     03 HEAD-IDMAIL-LEG      OCCURS 100 TIMES                             
012400                             PIC X(60).                                   
012500*                                 MAIL ADRESS LEGAL SÄLJARE               
012600*                                 MAIL ADDRESS LEGAL SELLER               
012700     03 HEAD-BECONT-LEG      OCCURS 100 TIMES                             
012800                             PIC X(35).                                   
012900*                                 KONTAKTPERSON LEGAL SÄLJARE             
013000*                                 CONTACT PERSON LEGAL SELLER             
013100     03 HEAD-IDBG-LEG        OCCURS 100 TIMES                             
013200                             PIC X(15).                                   
013300*                                 BANKGIRO LEGAL SÄLJARE                  
013400*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
013500*                                 R                                       
013600     03 HEAD-IDPG-LEG        OCCURS 100 TIMES                             
013700                             PIC X(15).                                   
013800*                                 POSTGIRO LEGAL SÄLJARE                  
013900*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
014000*                                 LER                                     
014100     03 HEAD-BELAND-LEG      OCCURS 100 TIMES                             
014200                             PIC X(35).                                   
014300*                                 LANDSBETECKNING                         
014400*                                 NAME OF COUNTRY                         
014500     03 HEAD-BERESPRA-1      OCCURS 100 TIMES                             
014600                             PIC X(35).                                   
014700*                                 DEL AV ANSV AVDELNINGS NAMN             
014800*                                 PART OF RESP DEPT  NAME                 
014900     03 HEAD-BERESPRA-2      OCCURS 100 TIMES                             
015000                             PIC X(35).                                   
015100*                                 DEL AV ANSV AVDELNINGS NAMN             
015200*                                 PART OF RESP DEPT  NAME                 
015300     03 HEAD-ADRESP-STREET   OCCURS 100 TIMES                             
015400                             PIC X(35).                                   
015500*                                 ANSVARIG AVDELNINGS GATUADRESS          
015600*                                 STREET ADDRESS OF RESPONSIBLE D         
015700*                                 PT.                                     
015800     03 HEAD-ADRESP-BOX      OCCURS 100 TIMES                             
015900                             PIC X(10).                                   
016000*                                 BOX ADRESS ANSVARIG AVDELNING           
016100*                                 BOX ADDRESS RESPONSIBLE DPT.            
016200     03 HEAD-ADRESP-CITY     OCCURS 100 TIMES                             
016300                             PIC X(35).                                   
016400*                                 ANSVARIG AVDELNINGS STAD (ELLER         
016500*                                  LIKNANDE)                              
016600*                                 CITY OF REPSONSIBLE DPT (OR SIM         
016700*                                 ILAR)                                   
016800     03 HEAD-ADRESP-PCODE    OCCURS 100 TIMES                             
016900                             PIC X(10).                                   
017000*                                 ANSVARIG AVDELNINGS POSTNUMMER          
017100*                                 RESPONSIBLE DPT. POSTAL CODE            
017200     03 HEAD-IDLANDX3-RESP   OCCURS 100 TIMES                             
017300                             PIC X(3).                                    
017400*                                 LANDKOD ANSVARIG AVD ETC                
017500*                                 COUNTRY CODE RESPONSIBLE DPT ET         
017600*                                 C                                       
017700     03 HEAD-IDVAT-RESP      OCCURS 100 TIMES                             
017800                             PIC X(17).                                   
017900*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
018000*                                 G AVD                                   
018100*                                 VAT REGISTRATION RESPONSIBLE DP         
018200*                                 T                                       
018300     03 HEAD-IDTFN-RESP      OCCURS 100 TIMES                             
018400                             PIC X(20).                                   
018500*                                 TELEFONNUMMER EXTERNT ANSVARIG          
018600*                                 AVD                                     
018700*                                 TELEPHONE NUMBER EXTERNAL RESPO         
018800*                                 NSIBLE DPT                              
018900     03 HEAD-IDTFX-RESP      OCCURS 100 TIMES                             
019000                             PIC X(20).                                   
019100*                                 FAXNUMMER ANSVARIG AVD                  
019200*                                 FAXNUMBER RESPONSIBLE DPT               
019300     03 HEAD-IDMAIL-RESP     OCCURS 100 TIMES                             
019400                             PIC X(60).                                   
019500*                                 MAIL ADRESS ANSVARIG AVD                
019600*                                 MAIL ADDRESS RESPONSIBLE DPT            
019700     03 HEAD-BECONT-RESP     OCCURS 100 TIMES                             
019800                             PIC X(35).                                   
019900*                                 KONTAKTPERSON ANSVARIG AVD              
020000*                                 CONTACT PERSON RESPONSIBLE DPT          
020100     03 HEAD-IDBG-RESP       OCCURS 100 TIMES                             
020200                             PIC X(15).                                   
020300*                                 BANKGIRO ANSVARIG AVD                   
020400*                                 BANC CHEQUE ACCOUNT RESPONSIBLE         
020500*                                  DPT                                    
020600     03 HEAD-IDPG-RESP       OCCURS 100 TIMES                             
020700                             PIC X(15).                                   
020800*                                 POSTGIRO ANSVARIG AVD                   
020900*                                 POSTAL CHEQUE ACCOUNT RESPONSIB         
021000*                                 LE DPT                                  
021100     03 HEAD-BELAND-RESP     OCCURS 100 TIMES                             
021200                             PIC X(35).                                   
021300*                                 LANDSBETECKNING                         
021400*                                 NAME OF COUNTRY                         
021500     03 HEAD-BEBET-NAME1     OCCURS 100 TIMES                             
021600                             PIC X(35).                                   
021700*                                 DEL AV BETALNINGSANSVARIGS NAMN         
021800*                                 PART OF FINANCIAL CUSTOMER NAME         
021900     03 HEAD-BEBET-NAME2     OCCURS 100 TIMES                             
022000                             PIC X(35).                                   
022100*                                 DEL AV BETALNINGSANSVARIGS NAMN         
022200*                                 PART OF FINANCIAL CUSTOMER NAME         
022300     03 HEAD-ADBET-STREET    OCCURS 100 TIMES                             
022400                             PIC X(35).                                   
022500*                                 BETALARENS GATUADRESS                   
022600*                                 PAYER ADDRESS STREET                    
022700     03 HEAD-ADBET-BOX       OCCURS 100 TIMES                             
022800                             PIC X(10).                                   
022900*                                 BOXADRESS BETALNINGSANSVARIG            
023000*                                 PAYER BOX ADDRESS                       
023100     03 HEAD-ADBET-CITY      OCCURS 100 TIMES                             
023200                             PIC X(35).                                   
023300*                                 BETALARENS STADSADRESS                  
023400*                                 PAYER ADDRESS CITY                      
023500     03 HEAD-ADBET-PCODE     OCCURS 100 TIMES                             
023600                             PIC X(10).                                   
023700*                                 BETALARENS STADSADRESS POSTNR           
023800*                                 PAYER ADDRESS POSTAL CODE               
023900     03 HEAD-IDVAT-BET       OCCURS 100 TIMES                             
024000                             PIC X(17).                                   
024100*                                 MOMSREGISTRERINGSNUMMER BETALAR         
024200*                                 E                                       
024300*                                 VAT REGISTRATION NUMBER PAYER           
024400     03 HEAD-IDLANDX3-BET    OCCURS 100 TIMES                             
024500                             PIC X(3).                                    
024600*                                 LANDKOD BETALANDE KUND ETC              
024700*                                 COUNTRY CODE PAYING CUSTOMER ET         
024800*                                 C                                       
024900     03 HEAD-BELAND-BET      OCCURS 100 TIMES                             
025000                             PIC X(35).                                   
025100*                                 LANDSBETECKNING                         
025200*                                 NAME OF COUNTRY                         
025300     03 HEAD-SUNTO-SERV      OCCURS 100 TIMES                             
025400                             PIC S9(11)V9(2)     COMP-3.                  
025500*                                 TOTAL SALES AMOUNT SERVICES EXC         
025600*                                 L. VAT                                  
025700     03 HEAD-SUNTO-PART      OCCURS 100 TIMES                             
025800                             PIC S9(11)V9(2)     COMP-3.                  
025900*                                 TOTAL SALES AMOUNT PARTS EXCL.          
026000*                                 VAT                                     
026100     03 HEAD-SUBTO-SERV      OCCURS 100 TIMES                             
026200                             PIC S9(11)V9(2)     COMP-3.                  
026300*                                 TOTAL SALES AMOUNT SERVICES INC         
026400*                                 L. VAT                                  
026500     03 HEAD-SUBTO-PART      OCCURS 100 TIMES                             
026600                             PIC S9(11)V9(2)     COMP-3.                  
026700*                                 TOTAL SALES AMOUNT PARTS INCL.          
026800*                                 VAT                                     
026900     03 HEAD-SUNTO-TOT       OCCURS 100 TIMES                             
027000                             PIC S9(11)V9(2)     COMP-3.                  
027100*                                 TOTAL SALES AMOUNT EXCL. VAT            
027200     03 HEAD-SUBTO-TOT       OCCURS 100 TIMES                             
027300                             PIC S9(11)V9(2)     COMP-3.                  
027400*                                 TOTAL SALES AMOUNT INCL. VAT            
027500     03 HEAD-SUVAT-BILLIT-TOT                                             
027600                             OCCURS 100 TIMES                             
027700                             PIC S9(11)V9(2)     COMP-3.                  
027800*                                 SUMMERAT MOMSVÄRDE                      
027900*                                 TOTAL VAT VALUE                         
028000     03 HEAD-SUNTO-TOT-LOC   OCCURS 100 TIMES                             
028100                             PIC S9(11)V9(2)     COMP-3.                  
028200*                                 TOTAL SALES AMOUNT EXCL. VAT            
028300     03 HEAD-SUBTO-TOT-LOC   OCCURS 100 TIMES                             
028400                             PIC S9(11)V9(2)     COMP-3.                  
028500*                                 TOTAL SALES AMOUNT INCL. VAT            
028600     03 HEAD-SUVAT-BILLIT-TOT-LOC                                         
028700                             OCCURS 100 TIMES                             
028800                             PIC S9(11)V9(2)     COMP-3.                  
028900*                                 SUMMERAT MOMSVÄRDE                      
029000*                                 TOTAL VAT VALUE                         
029100     03 HEAD-KDVALISO-LOC    OCCURS 100 TIMES                             
029200                             PIC X(3).                                    
029300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
029400*                                 CURRENCY CODE BY ISO-STANDARD.          
029500     03 HEAD-PRKURS-LOC      OCCURS 100 TIMES                             
029600                             PIC S9(6)V9(5)      COMP-3.                  
029700*                                 VALUTAKURS                              
029800*                                 CURRENCY EXCHANGE RATE                  
029900     03 HEAD-PRKURS          OCCURS 100 TIMES                             
030000                             PIC S9(6)V9(5)      COMP-3.                  
030100*                                 VALUTAKURS                              
030200*                                 CURRENCY EXCHANGE RATE                  
030300     03 HEAD-BEANST          OCCURS 100 TIMES                             
030400                             PIC X(25).                                   
030500*                                 ANSTÄLLDS NAMN                          
030600*                                 NAME OF EMPLOYED                        
030700     03 HEAD-IDUSER          OCCURS 100 TIMES                             
030800                             PIC X(8).                                    
030900*                                 ANVÄNDARENS SÄKERHETS ID                
031000*                                 USER SECURITY-IDENTITY                  
031100     03 HEAD-SUDOCLIM        OCCURS 100 TIMES                             
031200                             PIC S9(11)          COMP-3.                  
031300*                                 MINIMUM VALUE FOR PPRINTOUT OF          
031400*                                 FINANCIAL DOCUMENT (INVOICE ETC         
031500*                                 .)                                      
031600     03 HEAD-IDVAT-AGENT     OCCURS 100 TIMES                             
031700                             PIC X(17).                                   
031800*                                 MOMSREGISTRERINGSNUMMER                 
031900*                                 VAT REGISTRATION NUMBER                 
032000     03 HEAD-BETEXT-1        OCCURS 100 TIMES                             
032100                             PIC X(50).                                   
032200     03 HEAD-BETEXT-2        OCCURS 100 TIMES                             
032300                             PIC X(50).                                   
032400     03 HEAD-BETEXT-3        OCCURS 100 TIMES                             
032500                             PIC X(50).                                   
032600     03 HEAD-BETEXT-4        OCCURS 100 TIMES                             
032700                             PIC X(50).                                   
032800     03 HEAD-BETEXT-5        OCCURS 100 TIMES                             
032900                             PIC X(50).                                   
033000     03 HEAD-BETEXT-6        OCCURS 100 TIMES                             
033100                             PIC X(50).                                   
033200     03 HEAD-BETEXT-7        OCCURS 100 TIMES                             
033300                             PIC X(50).                                   
033400     03 HEAD-BETEXT-8        OCCURS 100 TIMES                             
033500                             PIC X(50).                                   
033600     03 HEAD-BETEXT-9        OCCURS 100 TIMES                             
033700                             PIC X(50).                                   
033800     03 HEAD-BETEXT-10       OCCURS 100 TIMES                             
033900                             PIC X(50).                                   
034000     03 HEAD-BETEXT-11       OCCURS 100 TIMES                             
034100                             PIC X(50).                                   
034200     03 HEAD-BETEXT-12       OCCURS 100 TIMES                             
034300                             PIC X(50).                                   
034400     03 HEAD-BETEXT-13       OCCURS 100 TIMES                             
034500                             PIC X(50).                                   
034600     03 HEAD-BETEXT-14       OCCURS 100 TIMES                             
034700                             PIC X(50).                                   
034800     03 HEAD-BETEXT-15       OCCURS 100 TIMES                             
034900                             PIC X(50).                                   
035000     03 HEAD-BETEXT-16       OCCURS 100 TIMES                             
035100                             PIC X(50).                                   
035200     03 HEAD-BETEXT-17       OCCURS 100 TIMES                             
035300                             PIC X(50).                                   
035400     03 HEAD-BETEXT-18       OCCURS 100 TIMES                             
035500                             PIC X(50).                                   
035600     03 HEAD-BETEXT-19       OCCURS 100 TIMES                             
035700                             PIC X(50).                                   
035800     03 HEAD-BETEXT-20       OCCURS 100 TIMES                             
035900                             PIC X(50).                                   
036000     03 HEAD-BETEXT-21       OCCURS 100 TIMES                             
036100                             PIC X(50).                                   
036200     03 HEAD-BETEXT-22       OCCURS 100 TIMES                             
036300                             PIC X(50).                                   
036400     03 HEAD-BETEXT-23       OCCURS 100 TIMES                             
036500                             PIC X(50).                                   
036600     03 HEAD-BETEXT-24       OCCURS 100 TIMES                             
036700                             PIC X(50).                                   
036800     03 HEAD-BETEXT          OCCURS 100 TIMES                             
036900                             PIC X(125).                                  
037000     03 HEAD-REVALUTA        OCCURS 100 TIMES                             
037100                             PIC S9(5)           COMP-3.                  
037200*                                 OMRÄKNINGSTAL FÖR VALUTA                
037300*                                 CONVERT VALUE FOR CURRENCY CODE         
037400     03 HEAD-BETEXT-CRE      OCCURS 100 TIMES                             
037500                             PIC X(100).                                  
037600*** END OF VILMAII-COPY LENGTH= 266800 BYTES                              
