000100 01  DP-INSUR-WF2101.                                                     
000200*                                 DOCUMENT DATA APPENDIX                  
000300     03 DP-INSUR-IDAFPRCD    PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 DP-INSUR-IDLEGSEL    PIC X(4).                                    
000700*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000800*                                 LEGAL SELLER IDENTITY                   
000900     03 DP-INSUR-BEFORMS     PIC X(15).                                   
001000*                                 BENÄMNING PÅ DOKUMENTFORMAT             
001100*                                                                         
001200*                                 DESCRIPTION OF DOCUMENT FORMAT          
001300*                                                                         
001400     03 DP-INSUR-IDLANDX3-SEND                                            
001500                             PIC X(3).                                    
001600*                                 LANDKOD SÄNDANDE LAND                   
001700*                                 COUNTRY CODE SENDING COUNTRY            
001800     03 DP-INSUR-IDLANDX3-REC                                             
001900                             PIC X(3).                                    
002000*                                 LANDKOD MOTTAGANDE LAND                 
002100*                                 COUNTRY CODE RECEIVING COUNTRY          
002200     03 DP-INSUR-IDLEVNR     PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500     03 DP-INSUR-IDPARTNR    PIC X(9).                                    
002600*                                 FINANCIELL KUND                         
002700*                                 FINANCIAL CUST                          
002800     03 DP-INSUR-IDFINDOC    PIC Z(8)9.                                   
002900*                                 FINANSIELLT DOKUMENT ID                 
003000*                                 FINANCIAL DOCUMENT ID                   
003100     03 DP-INSUR-DAFINDOC    PIC 9(8).                                    
003200*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003300*                                 INVOICING DATE   (YYYYMMDD)             
003400     03 DP-INSUR-BELEGRAD-1  PIC X(35).                                   
003500*                                 DEL AV LEGAL SELLER NAMN                
003600*                                 PART OF LEGAL SELLER NAME               
003700     03 DP-INSUR-BELEGRAD-2  PIC X(35).                                   
003800*                                 DEL AV LEGAL SELLER NAMN                
003900*                                 PART OF LEGAL SELLER NAME               
004000     03 DP-INSUR-ADLEG-STREET                                             
004100                             PIC X(35).                                   
004200*                                 LEGAL SELLER GATUADRESS                 
004300*                                 LEGAL SELLER STREET ADDRESS             
004400     03 DP-INSUR-ADLEG-BOX   PIC X(10).                                   
004500*                                 BOXADRESS LEGAL SELLER                  
004600*                                 LEGAL SELLER BOX ADDRESS                
004700     03 DP-INSUR-ADLEG-CITY  PIC X(35).                                   
004800*                                 LEGAL SÄLJARES ADRESS STAD              
004900*                                 LEGAL SELLER ADDRESS CITY               
005000     03 DP-INSUR-ADLEG-PCODE PIC X(10).                                   
005100*                                 LEGAL SELLER ADRESS POSTNR              
005200*                                 LEGAL SELLER POSTAL CODE                
005300     03 DP-INSUR-BELAND-LEG  PIC X(35).                                   
005400*                                 LANDSBETECKNING                         
005500*                                 NAME OF COUNTRY                         
005600     03 DP-INSUR-IDTFN-LEG   PIC X(20).                                   
005700*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
005800*                                 JARE                                    
005900*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
006000*                                  SELLER                                 
006100     03 DP-INSUR-IDTFX-LEG   PIC X(20).                                   
006200*                                 FAXNUMMER LEGAL SÄLJARE                 
006300*                                 FAXNUMBER LEGAL SELLER                  
006400     03 DP-INSUR-IDBG-LEG    PIC X(15).                                   
006500*                                 BANKGIRO LEGAL SÄLJARE                  
006600*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
006700*                                 R                                       
006800     03 DP-INSUR-IDPG-LEG    PIC X(15).                                   
006900*                                 POSTGIRO LEGAL SÄLJARE                  
007000*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
007100*                                 LER                                     
007200     03 DP-INSUR-IDVAT-LEG   PIC X(17).                                   
007300*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
007400*                                 ÄLJARE                                  
007500*                                 VAT REGISTRATION LEGAL PAYER            
007600     03 DP-INSUR-IDVAT-LEG-REG-NO                                         
007700                             PIC X(17).                                   
007800*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
007900*                                 ÄLJARE                                  
008000*                                 VAT REGISTRATION LEGAL PAYER            
008100     03 DP-INSUR-IDVAT-RESP  PIC X(17).                                   
008200*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
008300*                                 G AVD                                   
008400*                                 VAT REGISTRATION RESPONSIBLE DP         
008500*                                 T                                       
008600     03 DP-INSUR-KDVALISO    PIC X(3).                                    
008700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008800*                                 CURRENCY CODE BY ISO-STANDARD.          
008900     03 DP-INSUR-PRKURS      PIC Z(5)9.9(5).                              
009000*                                 VALUTAKURS                              
009100*                                 CURRENCY EXCHANGE RATE                  
009200     03 DP-INSUR-BEBETVIL    PIC X(30).                                   
009300*                                 BETALNINGSVILLKORSTEXT                  
009400*                                 TERMS OF PAYMENT TEXT                   
009500     03 DP-INSUR-BEBET-NAME1 PIC X(35).                                   
009600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009700*                                 PART OF FINANCIAL CUSTOMER NAME         
009800     03 DP-INSUR-BEBET-NAME2 PIC X(35).                                   
009900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
010000*                                 PART OF FINANCIAL CUSTOMER NAME         
010100     03 DP-INSUR-ADBET-STREET                                             
010200                             PIC X(35).                                   
010300*                                 BETALARENS GATUADRESS                   
010400*                                 PAYER ADDRESS STREET                    
010500     03 DP-INSUR-ADBET-BOX   PIC X(10).                                   
010600*                                 BOXADRESS BETALNINGSANSVARIG            
010700*                                 PAYER BOX ADDRESS                       
010800     03 DP-INSUR-ADBET-CITY  PIC X(35).                                   
010900*                                 BETALARENS STADSADRESS                  
011000*                                 PAYER ADDRESS CITY                      
011100     03 DP-INSUR-ADBET-PCODE PIC X(10).                                   
011200*                                 BETALARENS STADSADRESS POSTNR           
011300*                                 PAYER ADDRESS POSTAL CODE               
011400     03 DP-INSUR-BELAND-BET  PIC X(35).                                   
011500*                                 LANDSBETECKNING                         
011600*                                 NAME OF COUNTRY                         
011700     03 DP-INSUR-IDVAT-BET   PIC X(17).                                   
011800*                                 MOMSREGISTRERINGSNUMMER BETALAR         
011900*                                 E                                       
012000*                                 VAT REGISTRATION NUMBER PAYER           
012100     03 DP-INSUR-SUNTO-PART  PIC Z(10)9.9(2).                             
012200*                                 TOTAL SALES AMOUNT PARTS EXCL.          
012300*                                 VAT                                     
012400     03 DP-INSUR-SUBTO-PART  PIC Z(10)9.9(2).                             
012500*                                 TOTAL SALES AMOUNT PARTS INCL.          
012600*                                 VAT                                     
012700     03 DP-INSUR-IDEXCUST-1  PIC X(15).                                   
012800*                                 EXTERNT KUNDID                          
012900*                                 EXTERNAL CUSTOMER ID                    
013000     03 DP-INSUR-BELEVVIL    PIC X(35).                                   
013100*                                 LEVERANSVILLKOR                         
013200*                                 DELIVERY TERMS                          
013300     03 DP-INSUR-IDEXCUST-2  PIC X(15).                                   
013400*                                 EXTERNT KUNDID                          
013500*                                 EXTERNAL CUSTOMER ID                    
013600     03 DP-INSUR-IDDC        PIC X(2).                                    
013700*                                 IDENTIFIERARE LAGER                     
013800*                                 WAREHOUSE IDENTIFIER                    
013900     03 DP-INSUR-BEANST      PIC X(25).                                   
014000*                                 ANSTÄLLDS NAMN                          
014100*                                 NAME OF EMPLOYED                        
014200     03 DP-INSUR-IDUSER      PIC X(8).                                    
014300*                                 ANVÄNDARENS SÄKERHETS ID                
014400*                                 USER SECURITY-IDENTITY                  
014500     03 DP-INSUR-BEVAT       PIC X(50).                                   
014600*                                 MOMSKODSBENÄMNING R3                    
014700*                                 VAT CODE DESCRIPTION R3                 
014800     03 DP-INSUR-KDAPPEND    PIC X(4).                                    
014900*                                 APPENDIX KOD                            
015000*                                 APPENDIX CODE                           
015100     03 DP-INSUR-IDAPPEND    PIC X(8).                                    
015200*                                 APPENDIXVÄRDE                           
015300*                                 APPENDIX ITEM                           
015400     03 DP-INSUR-SUNTO-APP   PIC Z(10)9.9(2).                             
015500*                                 SALES AMOUNT FOR APPENDIX EXCL.         
015600*                                  VAT                                    
015700     03 DP-INSUR-SUVAT-BILLIT-APP                                         
015800                             PIC Z(10)9.9(2).                             
015900*                                 MOMSVÄRDE FÖR APPENDIX                  
016000*                                 VAT VALUE FOR APPENDIX                  
016100     03 DP-INSUR-SUBTO-APP   PIC Z(10)9.9(2).                             
016200*                                 SALES AMOUNT FOR APPENDIX INCL.         
016300*                                  VAT                                    
016400     03 DP-INSUR-IDVAT-AGENT PIC X(17).                                   
016500*                                 MOMSREGISTRERINGSNUMMER                 
016600*                                 VAT REGISTRATION NUMBER                 
016700     03 DP-INSUR-BERESPRA-1  PIC X(35).                                   
016800*                                 DEL AV ANSV AVDELNINGS NAMN             
016900*                                 PART OF RESP DEPT  NAME                 
017000     03 DP-INSUR-IDTFN-RESP  PIC X(20).                                   
017100*                                 TELEFONNUMMER EXTERNT ANSVARIG          
017200*                                 AVD                                     
017300*                                 TELEPHONE NUMBER EXTERNAL RESPO         
017400*                                 NSIBLE DPT                              
017500     03 DP-INSUR-IDTFX-RESP  PIC X(20).                                   
017600*                                 FAXNUMMER ANSVARIG AVD                  
017700*                                 FAXNUMBER RESPONSIBLE DPT               
017800     03 DP-INSUR-BETEXT-9    PIC X(100).                                  
017900     03 DP-INSUR-BETEXT-10   PIC X(100).                                  
018000     03 DP-INSUR-BETEXT-11   PIC X(100).                                  
018100     03 DP-INSUR-BETEXT-12   PIC X(100).                                  
018200     03 DP-INSUR-BETEXT-13   PIC X(100).                                  
018300     03 DP-INSUR-BETEXT-14   PIC X(100).                                  
018400     03 DP-INSUR-BETEXT-15   PIC X(100).                                  
018500     03 DP-INSUR-BETEXT-16   PIC X(100).                                  
018600     03 DP-INSUR-BETEXT-17   PIC X(100).                                  
018700     03 DP-INSUR-BETEXT-18   PIC X(100).                                  
018800     03 DP-INSUR-BETEXT-19   PIC X(100).                                  
018900     03 DP-INSUR-BETEXT-20   PIC X(100).                                  
019000     03 DP-INSUR-BETEXT-21   PIC X(100).                                  
019100     03 DP-INSUR-BETEXT-22   PIC X(100).                                  
019200     03 DP-INSUR-BETEXT-23   PIC X(100).                                  
019300     03 DP-INSUR-BETEXT-24   PIC X(100).                                  
019400*** END OF VILMAII-COPY LENGTH= 2563 BYTES                                
