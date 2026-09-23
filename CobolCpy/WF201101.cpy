000100 01  HEAD-WF201101.                                                       
000200*                                 DOCUMENT HEADER DATA                    
000300     03 HEAD-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 HEAD-DAFINDOC        PIC 9(8).                                    
000700*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
000800*                                 INVOICING DATE   (YYYYMMDD)             
000900     03 HEAD-IDFINDOC        PIC S9(9)           COMP-3.                  
001000*                                 FINANSIELLT DOKUMENT ID                 
001100*                                 FINANCIAL DOCUMENT ID                   
001200     03 HEAD-IDLOPNR         PIC S9(5)           COMP-3.                  
001300*                                 LÖPNUMMER          IDLOPNR-002          
001400     03 HEAD-IDLEGSEL        PIC X(4).                                    
001500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001600*                                 LEGAL SELLER IDENTITY                   
001700     03 HEAD-BEFORMS         PIC X(15).                                   
001800*                                 BENÄMNING PÅ DOKUMENTFORMAT             
001900*                                                                         
002000*                                 DESCRIPTION OF DOCUMENT FORMAT          
002100*                                                                         
002200     03 HEAD-DAEXDAT         PIC 9(8).                                    
002300*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
002400*                                 EXECUTION DATE (YYYYMMDD)               
002500     03 HEAD-TIEXTID         PIC S9(7)           COMP-3.                  
002600*                                 EXEKVERINGSTIDPUNKT                     
002700*                                 EXECUTION TIME                          
002800     03 HEAD-KDVALISO        PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000*                                 CURRENCY CODE BY ISO-STANDARD.          
003100     03 HEAD-IDLANDX3-SEND   PIC X(3).                                    
003200*                                 LANDKOD SÄNDANDE LAND                   
003300*                                 COUNTRY CODE SENDING COUNTRY            
003400     03 HEAD-IDLEVNR         PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003700     03 HEAD-IDPARTNR        PIC X(9).                                    
003800*                                 FINANCIELL KUND                         
003900*                                 FINANCIAL CUST                          
004000     03 HEAD-KDFINDOC        PIC X(4).                                    
004100*                                 TYP FINANSIELLT DOKUMENT                
004200*                                 FINANCIAL DOCUMENT TYPE                 
004300     03 HEAD-FLSOFT          PIC X.                                       
004400*                                 FLAGGA SOFTVARA                         
004500*                                 SOFTWARE MARK                           
004600     03 HEAD-FLFREE          PIC X.                                       
004700*                                 GRATISFATURA                            
004800*                                 FREE INVOICE                            
004900     03 HEAD-IDBREAK-1       PIC X(8).                                    
005000*                                 BRYTVÄRDE                               
005100*                                 BREAK VALUE                             
005200     03 HEAD-IDBREAK-2       PIC X(8).                                    
005300*                                 BRYTVÄRDE                               
005400*                                 BREAK VALUE                             
005500     03 HEAD-IDSPRAK         PIC X(2).                                    
005600*                                 2-STÄLLIG ISO SPRÅKKOD                  
005700*                                 2-LETTER ISO LANGUAGE CODE              
005800     03 HEAD-BEBETVIL        PIC X(30).                                   
005900*                                 BETALNINGSVILLKORSTEXT                  
006000*                                 TERMS OF PAYMENT TEXT                   
006100     03 HEAD-BELEGRAD-1      PIC X(35).                                   
006200*                                 DEL AV LEGAL SELLER NAMN                
006300*                                 PART OF LEGAL SELLER NAME               
006400     03 HEAD-BELEGRAD-2      PIC X(35).                                   
006500*                                 DEL AV LEGAL SELLER NAMN                
006600*                                 PART OF LEGAL SELLER NAME               
006700     03 HEAD-ADLEG-STREET    PIC X(35).                                   
006800*                                 LEGAL SELLER GATUADRESS                 
006900*                                 LEGAL SELLER STREET ADDRESS             
007000     03 HEAD-ADLEG-BOX       PIC X(10).                                   
007100*                                 BOXADRESS LEGAL SELLER                  
007200*                                 LEGAL SELLER BOX ADDRESS                
007300     03 HEAD-ADLEG-CITY      PIC X(35).                                   
007400*                                 LEGAL SÄLJARES ADRESS STAD              
007500*                                 LEGAL SELLER ADDRESS CITY               
007600     03 HEAD-ADLEG-PCODE     PIC X(10).                                   
007700*                                 LEGAL SELLER ADRESS POSTNR              
007800*                                 LEGAL SELLER POSTAL CODE                
007900     03 HEAD-IDLANDX3-LEG    PIC X(3).                                    
008000*                                 LANDKOD LEGAL SÄLJARE                   
008100*                                 COUNTRY CODE LEGAL SELLER               
008200     03 HEAD-IDVAT-LEG       PIC X(17).                                   
008300*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
008400*                                 ÄLJARE                                  
008500*                                 VAT REGISTRATION LEGAL PAYER            
008600     03 HEAD-IDTFN-LEG       PIC X(20).                                   
008700*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
008800*                                 JARE                                    
008900*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
009000*                                  SELLER                                 
009100     03 HEAD-IDTFX-LEG       PIC X(20).                                   
009200*                                 FAXNUMMER LEGAL SÄLJARE                 
009300*                                 FAXNUMBER LEGAL SELLER                  
009400     03 HEAD-IDMAIL-LEG      PIC X(60).                                   
009500*                                 MAIL ADRESS LEGAL SÄLJARE               
009600*                                 MAIL ADDRESS LEGAL SELLER               
009700     03 HEAD-BECONT-LEG      PIC X(35).                                   
009800*                                 KONTAKTPERSON LEGAL SÄLJARE             
009900*                                 CONTACT PERSON LEGAL SELLER             
010000     03 HEAD-IDBG-LEG        PIC X(15).                                   
010100*                                 BANKGIRO LEGAL SÄLJARE                  
010200*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
010300*                                 R                                       
010400     03 HEAD-IDPG-LEG        PIC X(15).                                   
010500*                                 POSTGIRO LEGAL SÄLJARE                  
010600*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
010700*                                 LER                                     
010800     03 HEAD-BELAND-LEG      PIC X(35).                                   
010900*                                 LANDSBETECKNING                         
011000*                                 NAME OF COUNTRY                         
011100     03 HEAD-BERESPRA-1      PIC X(35).                                   
011200*                                 DEL AV ANSV AVDELNINGS NAMN             
011300*                                 PART OF RESP DEPT  NAME                 
011400     03 HEAD-BERESPRA-2      PIC X(35).                                   
011500*                                 DEL AV ANSV AVDELNINGS NAMN             
011600*                                 PART OF RESP DEPT  NAME                 
011700     03 HEAD-ADRESP-STREET   PIC X(35).                                   
011800*                                 ANSVARIG AVDELNINGS GATUADRESS          
011900*                                 STREET ADDRESS OF RESPONSIBLE D         
012000*                                 PT.                                     
012100     03 HEAD-ADRESP-BOX      PIC X(10).                                   
012200*                                 BOX ADRESS ANSVARIG AVDELNING           
012300*                                 BOX ADDRESS RESPONSIBLE DPT.            
012400     03 HEAD-ADRESP-CITY     PIC X(35).                                   
012500*                                 ANSVARIG AVDELNINGS STAD (ELLER         
012600*                                  LIKNANDE)                              
012700*                                 CITY OF REPSONSIBLE DPT (OR SIM         
012800*                                 ILAR)                                   
012900     03 HEAD-ADRESP-PCODE    PIC X(10).                                   
013000*                                 ANSVARIG AVDELNINGS POSTNUMMER          
013100*                                 RESPONSIBLE DPT. POSTAL CODE            
013200     03 HEAD-IDLANDX3-RESP   PIC X(3).                                    
013300*                                 LANDKOD ANSVARIG AVD ETC                
013400*                                 COUNTRY CODE RESPONSIBLE DPT ET         
013500*                                 C                                       
013600     03 HEAD-IDVAT-RESP      PIC X(17).                                   
013700*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
013800*                                 G AVD                                   
013900*                                 VAT REGISTRATION RESPONSIBLE DP         
014000*                                 T                                       
014100     03 HEAD-IDTFN-RESP      PIC X(20).                                   
014200*                                 TELEFONNUMMER EXTERNT ANSVARIG          
014300*                                 AVD                                     
014400*                                 TELEPHONE NUMBER EXTERNAL RESPO         
014500*                                 NSIBLE DPT                              
014600     03 HEAD-IDTFX-RESP      PIC X(20).                                   
014700*                                 FAXNUMMER ANSVARIG AVD                  
014800*                                 FAXNUMBER RESPONSIBLE DPT               
014900     03 HEAD-IDMAIL-RESP     PIC X(60).                                   
015000*                                 MAIL ADRESS ANSVARIG AVD                
015100*                                 MAIL ADDRESS RESPONSIBLE DPT            
015200     03 HEAD-BECONT-RESP     PIC X(35).                                   
015300*                                 KONTAKTPERSON ANSVARIG AVD              
015400*                                 CONTACT PERSON RESPONSIBLE DPT          
015500     03 HEAD-IDBG-RESP       PIC X(15).                                   
015600*                                 BANKGIRO ANSVARIG AVD                   
015700*                                 BANC CHEQUE ACCOUNT RESPONSIBLE         
015800*                                  DPT                                    
015900     03 HEAD-IDPG-RESP       PIC X(15).                                   
016000*                                 POSTGIRO ANSVARIG AVD                   
016100*                                 POSTAL CHEQUE ACCOUNT RESPONSIB         
016200*                                 LE DPT                                  
016300     03 HEAD-BELAND-RESP     PIC X(35).                                   
016400*                                 LANDSBETECKNING                         
016500*                                 NAME OF COUNTRY                         
016600     03 HEAD-BEBET-NAME1     PIC X(35).                                   
016700*                                 DEL AV BETALNINGSANSVARIGS NAMN         
016800*                                 PART OF FINANCIAL CUSTOMER NAME         
016900     03 HEAD-BEBET-NAME2     PIC X(35).                                   
017000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
017100*                                 PART OF FINANCIAL CUSTOMER NAME         
017200     03 HEAD-ADBET-STREET    PIC X(35).                                   
017300*                                 BETALARENS GATUADRESS                   
017400*                                 PAYER ADDRESS STREET                    
017500     03 HEAD-ADBET-BOX       PIC X(10).                                   
017600*                                 BOXADRESS BETALNINGSANSVARIG            
017700*                                 PAYER BOX ADDRESS                       
017800     03 HEAD-ADBET-CITY      PIC X(35).                                   
017900*                                 BETALARENS STADSADRESS                  
018000*                                 PAYER ADDRESS CITY                      
018100     03 HEAD-ADBET-PCODE     PIC X(10).                                   
018200*                                 BETALARENS STADSADRESS POSTNR           
018300*                                 PAYER ADDRESS POSTAL CODE               
018400     03 HEAD-IDVAT-BET       PIC X(17).                                   
018500*                                 MOMSREGISTRERINGSNUMMER BETALAR         
018600*                                 E                                       
018700*                                 VAT REGISTRATION NUMBER PAYER           
018800     03 HEAD-IDLANDX3-BET    PIC X(3).                                    
018900*                                 LANDKOD BETALANDE KUND ETC              
019000*                                 COUNTRY CODE PAYING CUSTOMER ET         
019100*                                 C                                       
019200     03 HEAD-BELAND-BET      PIC X(35).                                   
019300*                                 LANDSBETECKNING                         
019400*                                 NAME OF COUNTRY                         
019500     03 HEAD-SUNTO-SERV      PIC S9(11)V9(2)     COMP-3.                  
019600*                                 TOTAL SALES AMOUNT SERVICES EXC         
019700*                                 L. VAT                                  
019800     03 HEAD-SUNTO-PART      PIC S9(11)V9(2)     COMP-3.                  
019900*                                 TOTAL SALES AMOUNT PARTS EXCL.          
020000*                                 VAT                                     
020100     03 HEAD-SUBTO-SERV      PIC S9(11)V9(2)     COMP-3.                  
020200*                                 TOTAL SALES AMOUNT SERVICES INC         
020300*                                 L. VAT                                  
020400     03 HEAD-SUBTO-PART      PIC S9(11)V9(2)     COMP-3.                  
020500*                                 TOTAL SALES AMOUNT PARTS INCL.          
020600*                                 VAT                                     
020700     03 HEAD-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
020800*                                 TOTAL SALES AMOUNT EXCL. VAT            
020900     03 HEAD-SUBTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
021000*                                 TOTAL SALES AMOUNT INCL. VAT            
021100     03 HEAD-SUVAT-BILLIT-TOT                                             
021200                             PIC S9(11)V9(2)     COMP-3.                  
021300*                                 SUMMERAT MOMSVÄRDE                      
021400*                                 TOTAL VAT VALUE                         
021500     03 HEAD-SUNTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
021600*                                 TOTAL SALES AMOUNT EXCL. VAT            
021700     03 HEAD-SUBTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
021800*                                 TOTAL SALES AMOUNT INCL. VAT            
021900     03 HEAD-SUVAT-BILLIT-TOT-LOC                                         
022000                             PIC S9(11)V9(2)     COMP-3.                  
022100*                                 SUMMERAT MOMSVÄRDE                      
022200*                                 TOTAL VAT VALUE                         
022300     03 HEAD-KDVALISO-LOC    PIC X(3).                                    
022400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
022500*                                 CURRENCY CODE BY ISO-STANDARD.          
022600     03 HEAD-PRKURS-LOC      PIC S9(6)V9(5)      COMP-3.                  
022700*                                 VALUTAKURS                              
022800*                                 CURRENCY EXCHANGE RATE                  
022900     03 HEAD-PRKURS          PIC S9(6)V9(5)      COMP-3.                  
023000*                                 VALUTAKURS                              
023100*                                 CURRENCY EXCHANGE RATE                  
023200     03 HEAD-BEANST          PIC X(25).                                   
023300*                                 ANSTÄLLDS NAMN                          
023400*                                 NAME OF EMPLOYED                        
023500     03 HEAD-IDUSER          PIC X(8).                                    
023600*                                 ANVÄNDARENS SÄKERHETS ID                
023700*                                 USER SECURITY-IDENTITY                  
023800     03 HEAD-SUDOCLIM        PIC S9(11)          COMP-3.                  
023900*                                 MINIMUM VALUE FOR PPRINTOUT OF          
024000*                                 FINANCIAL DOCUMENT (INVOICE ETC         
024100*                                 .)                                      
024200     03 HEAD-IDVAT-AGENT     PIC X(17).                                   
024300*                                 MOMSREGISTRERINGSNUMMER                 
024400*                                 VAT REGISTRATION NUMBER                 
024500     03 HEAD-BETEXT-1        PIC X(50).                                   
024600     03 HEAD-BETEXT-2        PIC X(50).                                   
024700     03 HEAD-BETEXT-3        PIC X(50).                                   
024800     03 HEAD-BETEXT-4        PIC X(50).                                   
024900     03 HEAD-BETEXT-5        PIC X(100).                                  
025000     03 HEAD-BETEXT-6        PIC X(100).                                  
025100     03 HEAD-BETEXT-7        PIC X(100).                                  
025200     03 HEAD-BETEXT-8        PIC X(100).                                  
025300     03 HEAD-BETEXT-9        PIC X(100).                                  
025400     03 HEAD-BETEXT-10       PIC X(100).                                  
025500     03 HEAD-BETEXT-11       PIC X(100).                                  
025600     03 HEAD-BETEXT-12       PIC X(100).                                  
025700     03 HEAD-BETEXT-13       PIC X(100).                                  
025800     03 HEAD-BETEXT-14       PIC X(100).                                  
025900     03 HEAD-BETEXT-15       PIC X(100).                                  
026000     03 HEAD-BETEXT-16       PIC X(100).                                  
026100     03 HEAD-BETEXT-17       PIC X(100).                                  
026200     03 HEAD-BETEXT-18       PIC X(100).                                  
026300     03 HEAD-BETEXT-19       PIC X(100).                                  
026400     03 HEAD-BETEXT-20       PIC X(100).                                  
026500     03 HEAD-BETEXT-21       PIC X(100).                                  
026600     03 HEAD-BETEXT-22       PIC X(100).                                  
026700     03 HEAD-BETEXT-23       PIC X(100).                                  
026800     03 HEAD-BETEXT-24       PIC X(100).                                  
026900     03 HEAD-BETEXT          PIC X(125).                                  
027000     03 HEAD-REVALUTA        PIC S9(5)           COMP-3.                  
027100*                                 OMRÄKNINGSTAL FÖR VALUTA                
027200*                                 CONVERT VALUE FOR CURRENCY CODE         
027300     03 HEAD-BETEXT-CRE      PIC X(100).                                  
027400     03 HEAD-FLFINFIL        PIC X.                                       
027500*                                 FINACIELL INFO FIL TILL KUND            
027600*                                 FINACIAL INFO FILE TO FINANCIAL         
027700*                                  CUSTOMER                               
027800     03 HEAD-KDTRADP         PIC X(4).                                    
027900*                                 TRADING PARTNER                         
028000*                                 TRADING PARTNER                         
028100     03 HEAD-KDFRAKT         PIC S9(3)           COMP-3.                  
028200*                                 FRAKTSÄTT DC TILL KUND                  
028300*                                 FREIGHT CODE                            
028400     03 HEAD-KDPARTGR        PIC X(15).                                   
028500*                                 GRUPP AV BETALARE                       
028600*                                 FIN.CUSTOMER GROUP                      
028700     03 HEAD-FLDIRVAT        PIC X.                                       
028800*                                 OM VAT FÖR DIRLEV                       
028900*                                 IF VAT FOR DDGS                         
029000     03 HEAD-IDSYSTEM-SEND   PIC X(4).                                    
029100*                                 VOLVO SÄNDANDE SYSTEM                   
029200*                                 VOLVO SENDING SYSTEM                    
029300     03 HEAD-BETEXT-25       PIC X(100).                                  
029400     03 HEAD-BETEXT-26       PIC X(100).                                  
029500     03 HEAD-BETEXT-27       PIC X(100).                                  
029600     03 HEAD-BETEXT-28       PIC X(100).                                  
029700     03 HEAD-BETEXT-29       PIC X(100).                                  
029800     03 HEAD-BETEXT-30       PIC X(100).                                  
029900     03 HEAD-BETEXT-31       PIC X(100).                                  
030000     03 HEAD-BETEXT-32       PIC X(100).                                  
030100     03 HEAD-BETEXT-33       PIC X(100).                                  
030200     03 HEAD-BETEXT-34       PIC X(100).                                  
030300     03 HEAD-BETEXT-35       PIC X(100).                                  
030400     03 HEAD-BETEXT-36       PIC X(100).                                  
030500     03 HEAD-BETEXT-37       PIC X(100).                                  
030600     03 HEAD-BETEXT-38       PIC X(100).                                  
030700     03 HEAD-BETEXT-39       PIC X(100).                                  
030800     03 HEAD-BETEXT-40       PIC X(100).                                  
030900     03 HEAD-BETEXT-41       PIC X(100).                                  
031000     03 HEAD-BETEXT-42       PIC X(100).                                  
031100     03 HEAD-BETEXT-43       PIC X(100).                                  
031200     03 HEAD-BETEXT-44       PIC X(100).                                  
031300*** END OF VILMAII-COPY LENGTH= 5695 BYTES                                
