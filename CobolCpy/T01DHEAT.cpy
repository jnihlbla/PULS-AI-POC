000100 01  T01DHEAT.                                                            
000200*                                 MULTIFETCH TABELL TILL T01DHEA          
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
002700*                                 PARTNERNUMMER                           
002800*                                 PARTNER NO                              
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
005300     03 IDFINDOC             OCCURS 100 TIMES                             
005400                             PIC S9(9)           COMP-3.                  
005500*                                 FINANSIELLT DOKUMENT ID                 
005600*                                 FINANCIAL DOCUMENT ID                   
005700     03 DAFINDOC             OCCURS 100 TIMES                             
005800                             PIC 9(8).                                    
005900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
006000*                                 INVOICING DATE   (YYYYMMDD)             
006100     03 IDSYSTEM-SEND        OCCURS 100 TIMES                             
006200                             PIC X(4).                                    
006300*                                 VOLVO SÄNDANDE SYSTEM                   
006400*                                 VOLVO SENDING SYSTEM                    
006500     03 IDSYSTEM-REC         OCCURS 100 TIMES                             
006600                             PIC X(4).                                    
006700*                                 VOLVO MOTTAGANDE SYSTEM                 
006800*                                 VOLVO RECEIVING SYSTEM                  
006900     03 IDSPRAK              OCCURS 100 TIMES                             
007000                             PIC X(2).                                    
007100*                                 2-STÄLLIG ISO SPRÅKKOD                  
007200*                                 2-LETTER ISO LANGUAGE CODE              
007300     03 KDBETALV             OCCURS 100 TIMES                             
007400                             PIC X(4).                                    
007500*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
007600*                                 TERMS OF PAYMENT                        
007700     03 BEBETVIL             OCCURS 100 TIMES                             
007800                             PIC X(30).                                   
007900*                                 BETALNINGSVILLKORSTEXT                  
008000*                                 TERMS OF PAYMENT TEXT                   
008100     03 BELEGRAD-1           OCCURS 100 TIMES                             
008200                             PIC X(35).                                   
008300*                                 DEL AV LEGAL SELLER NAMN                
008400*                                 PART OF LEGAL SELLER NAME               
008500     03 BELEGRAD-2           OCCURS 100 TIMES                             
008600                             PIC X(35).                                   
008700*                                 DEL AV LEGAL SELLER NAMN                
008800*                                 PART OF LEGAL SELLER NAME               
008900     03 ADLEG-STREET         OCCURS 100 TIMES                             
009000                             PIC X(35).                                   
009100*                                 LEGAL SELLER GATUADRESS                 
009200*                                 LEGAL SELLER STREET ADDRESS             
009300     03 ADLEG-BOX            OCCURS 100 TIMES                             
009400                             PIC X(10).                                   
009500*                                 BOXADRESS LEGAL SELLER                  
009600*                                 LEGAL SELLER BOX ADDRESS                
009700     03 ADLEG-CITY           OCCURS 100 TIMES                             
009800                             PIC X(35).                                   
009900*                                 LEGAL SÄLJARES ADRESS STAD              
010000*                                 LEGAL SELLER ADDRESS CITY               
010100     03 ADLEG-PCODE          OCCURS 100 TIMES                             
010200                             PIC X(10).                                   
010300*                                 LEGAL SELLER ADRESS POSTNR              
010400*                                 LEGAL SELLER POSTAL CODE                
010500     03 IDLANDX3-LEG         OCCURS 100 TIMES                             
010600                             PIC X(3).                                    
010700*                                 LANDKOD LEGAL SÄLJARE                   
010800*                                 COUNTRY CODE LEGAL SELLER               
010900     03 IDTFN-LEG            OCCURS 100 TIMES                             
011000                             PIC X(20).                                   
011100*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
011200*                                 JARE                                    
011300*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
011400*                                  SELLER                                 
011500     03 IDTFX-LEG            OCCURS 100 TIMES                             
011600                             PIC X(20).                                   
011700*                                 FAXNUMMER LEGAL SÄLJARE                 
011800*                                 FAXNUMBER LEGAL SELLER                  
011900     03 IDMAIL-LEG           OCCURS 100 TIMES                             
012000                             PIC X(60).                                   
012100*                                 MAIL ADRESS LEGAL SÄLJARE               
012200*                                 MAIL ADDRESS LEGAL SELLER               
012300     03 BECONT-LEG           OCCURS 100 TIMES                             
012400                             PIC X(35).                                   
012500*                                 KONTAKTPERSON LEGAL SÄLJARE             
012600*                                 CONTACT PERSON LEGAL SELLER             
012700     03 IDVAT-LEG            OCCURS 100 TIMES                             
012800                             PIC X(17).                                   
012900*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
013000*                                 ÄLJARE                                  
013100*                                 VAT REGISTRATION LEGAL PAYER            
013200     03 IDBG-LEG             OCCURS 100 TIMES                             
013300                             PIC X(15).                                   
013400*                                 BANKGIRO LEGAL SÄLJARE                  
013500*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
013600*                                 R                                       
013700     03 IDPG-LEG             OCCURS 100 TIMES                             
013800                             PIC X(15).                                   
013900*                                 POSTGIRO LEGAL SÄLJARE                  
014000*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
014100*                                 LER                                     
014200     03 BERESPRA-1           OCCURS 100 TIMES                             
014300                             PIC X(35).                                   
014400*                                 DEL AV ANSV AVDELNINGS NAMN             
014500*                                 PART OF RESP DEPT  NAME                 
014600     03 BERESPRA-2           OCCURS 100 TIMES                             
014700                             PIC X(35).                                   
014800*                                 DEL AV ANSV AVDELNINGS NAMN             
014900*                                 PART OF RESP DEPT  NAME                 
015000     03 ADRESP-STREET        OCCURS 100 TIMES                             
015100                             PIC X(35).                                   
015200*                                 ANSVARIG AVDELNINGS GATUADRESS          
015300*                                 STREET ADDRESS OF RESPONSIBLE D         
015400*                                 PT.                                     
015500     03 ADRESP-BOX           OCCURS 100 TIMES                             
015600                             PIC X(10).                                   
015700*                                 BOX ADRESS ANSVARIG AVDELNING           
015800*                                 BOX ADDRESS RESPONSIBLE DPT.            
015900     03 ADRESP-CITY          OCCURS 100 TIMES                             
016000                             PIC X(35).                                   
016100*                                 ANSVARIG AVDELNINGS STAD (ELLER         
016200*                                  LIKNANDE)                              
016300*                                 CITY OF REPSONSIBLE DPT (OR SIM         
016400*                                 ILAR)                                   
016500     03 ADRESP-PCODE         OCCURS 100 TIMES                             
016600                             PIC X(10).                                   
016700*                                 ANSVARIG AVDELNINGS POSTNUMMER          
016800*                                 RESPONSIBLE DPT. POSTAL CODE            
016900     03 IDLANDX3-RESP        OCCURS 100 TIMES                             
017000                             PIC X(3).                                    
017100*                                 LANDKOD ANSVARIG AVD ETC                
017200*                                 COUNTRY CODE RESPONSIBLE DPT ET         
017300*                                 C                                       
017400     03 IDTFN-RESP           OCCURS 100 TIMES                             
017500                             PIC X(20).                                   
017600*                                 TELEFONNUMMER EXTERNT ANSVARIG          
017700*                                 AVD                                     
017800*                                 TELEPHONE NUMBER EXTERNAL RESPO         
017900*                                 NSIBLE DPT                              
018000     03 IDTFX-RESP           OCCURS 100 TIMES                             
018100                             PIC X(20).                                   
018200*                                 FAXNUMMER ANSVARIG AVD                  
018300*                                 FAXNUMBER RESPONSIBLE DPT               
018400     03 IDMAIL-RESP          OCCURS 100 TIMES                             
018500                             PIC X(60).                                   
018600*                                 MAIL ADRESS ANSVARIG AVD                
018700*                                 MAIL ADDRESS RESPONSIBLE DPT            
018800     03 BECONT-RESP          OCCURS 100 TIMES                             
018900                             PIC X(35).                                   
019000*                                 KONTAKTPERSON ANSVARIG AVD              
019100*                                 CONTACT PERSON RESPONSIBLE DPT          
019200     03 IDVAT-RESP           OCCURS 100 TIMES                             
019300                             PIC X(17).                                   
019400*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
019500*                                 G AVD                                   
019600*                                 VAT REGISTRATION RESPONSIBLE DP         
019700*                                 T                                       
019800     03 IDBG-RESP            OCCURS 100 TIMES                             
019900                             PIC X(15).                                   
020000*                                 BANKGIRO ANSVARIG AVD                   
020100*                                 BANC CHEQUE ACCOUNT RESPONSIBLE         
020200*                                  DPT                                    
020300     03 IDPG-RESP            OCCURS 100 TIMES                             
020400                             PIC X(15).                                   
020500*                                 POSTGIRO ANSVARIG AVD                   
020600*                                 POSTAL CHEQUE ACCOUNT RESPONSIB         
020700*                                 LE DPT                                  
020800     03 BEBET-NAME1          OCCURS 100 TIMES                             
020900                             PIC X(35).                                   
021000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
021100*                                 PART OF FINANCIAL CUSTOMER NAME         
021200     03 BEBET-NAME2          OCCURS 100 TIMES                             
021300                             PIC X(35).                                   
021400*                                 DEL AV BETALNINGSANSVARIGS NAMN         
021500*                                 PART OF FINANCIAL CUSTOMER NAME         
021600     03 ADBET-STREET         OCCURS 100 TIMES                             
021700                             PIC X(35).                                   
021800*                                 BETALARENS GATUADRESS                   
021900*                                 PAYER ADDRESS STREET                    
022000     03 ADBET-BOX            OCCURS 100 TIMES                             
022100                             PIC X(10).                                   
022200*                                 BOXADRESS BETALNINGSANSVARIG            
022300*                                 PAYER BOX ADDRESS                       
022400     03 ADBET-CITY           OCCURS 100 TIMES                             
022500                             PIC X(35).                                   
022600*                                 BETALARENS STADSADRESS                  
022700*                                 PAYER ADDRESS CITY                      
022800     03 ADBET-PCODE          OCCURS 100 TIMES                             
022900                             PIC X(10).                                   
023000*                                 BETALARENS STADSADRESS POSTNR           
023100*                                 PAYER ADDRESS POSTAL CODE               
023200     03 IDLANDX3-BET         OCCURS 100 TIMES                             
023300                             PIC X(3).                                    
023400*                                 LANDKOD BETALANDE KUND ETC              
023500*                                 COUNTRY CODE PAYING CUSTOMER ET         
023600*                                 C                                       
023700     03 IDVAT-BET            OCCURS 100 TIMES                             
023800                             PIC X(17).                                   
023900*                                 MOMSREGISTRERINGSNUMMER BETALAR         
024000*                                 E                                       
024100*                                 VAT REGISTRATION NUMBER PAYER           
024200     03 SUNTO-PART           OCCURS 100 TIMES                             
024300                             PIC S9(11)V9(2)     COMP-3.                  
024400*                                 TOTAL SALES AMOUNT PARTS EXCL.          
024500*                                 VAT                                     
024600     03 SUNTO-SERV           OCCURS 100 TIMES                             
024700                             PIC S9(11)V9(2)     COMP-3.                  
024800*                                 TOTAL SALES AMOUNT SERVICES EXC         
024900*                                 L. VAT                                  
025000     03 SUBTO-PART           OCCURS 100 TIMES                             
025100                             PIC S9(11)V9(2)     COMP-3.                  
025200*                                 TOTAL SALES AMOUNT PARTS INCL.          
025300*                                 VAT                                     
025400     03 SUBTO-SERV           OCCURS 100 TIMES                             
025500                             PIC S9(11)V9(2)     COMP-3.                  
025600*                                 TOTAL SALES AMOUNT SERVICES INC         
025700*                                 L. VAT                                  
025800     03 SUNTO-TOT            OCCURS 100 TIMES                             
025900                             PIC S9(11)V9(2)     COMP-3.                  
026000*                                 TOTAL SALES AMOUNT EXCL. VAT            
026100     03 SUVAT-BILLIT-TOT     OCCURS 100 TIMES                             
026200                             PIC S9(11)V9(2)     COMP-3.                  
026300*                                 SUMMERAT MOMSVÄRDE                      
026400*                                 TOTAL VAT VALUE                         
026500     03 SUBTO-TOT            OCCURS 100 TIMES                             
026600                             PIC S9(11)V9(2)     COMP-3.                  
026700*                                 TOTAL SALES AMOUNT INCL. VAT            
026800     03 KDTRADP              OCCURS 100 TIMES                             
026900                             PIC X(4).                                    
027000*                                 TRADING PARTNER                         
027100*                                 TRADING PARTNER                         
027200     03 PRKURS               OCCURS 100 TIMES                             
027300                             PIC S9(6)V9(5)      COMP-3.                  
027400*                                 VALUTAKURS                              
027500*                                 CURRENCY EXCHANGE RATE                  
027600     03 BEANST               OCCURS 100 TIMES                             
027700                             PIC X(25).                                   
027800*                                 ANSTÄLLDS NAMN                          
027900*                                 NAME OF EMPLOYED                        
028000     03 IDUSER               OCCURS 100 TIMES                             
028100                             PIC X(8).                                    
028200*                                 ANVÄNDARENS SÄKERHETS ID                
028300*                                 USER SECURITY-IDENTITY                  
028400     03 BETEXT-1             OCCURS 100 TIMES                             
028500                             PIC X(50).                                   
028600     03 BETEXT-2             OCCURS 100 TIMES                             
028700                             PIC X(50).                                   
028800     03 BETEXT-3             OCCURS 100 TIMES                             
028900                             PIC X(50).                                   
029000     03 BETEXT-4             OCCURS 100 TIMES                             
029100                             PIC X(50).                                   
029200     03 IDVAT-AGENT          OCCURS 100 TIMES                             
029300                             PIC X(17).                                   
029400*                                 MOMSREGISTRERINGSNUMMER AGENT           
029500*                                 VAT REGISTRATION VAT AGENT              
029600     03 BETEXT               OCCURS 100 TIMES                             
029700                             PIC X(125).                                  
029800     03 BETEXT-CRE           OCCURS 100 TIMES                             
029900                             PIC X(100).                                  
030000*** END OF VILMAII-COPY LENGTH= 152000 BYTES                              
