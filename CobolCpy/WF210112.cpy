000100 01  DP-ADD-WF2101.                                                       
000200*                                 DOCUMENT DATA APPENDIX                  
000300     03 DP-ADD-IDAFPRCD      PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 DP-ADD-IDLEGSEL      PIC X(4).                                    
000700*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000800*                                 LEGAL SELLER IDENTITY                   
000900     03 DP-ADD-BEFORMS       PIC X(15).                                   
001000*                                 BENÄMNING PÅ DOKUMENTFORMAT             
001100*                                                                         
001200*                                 DESCRIPTION OF DOCUMENT FORMAT          
001300*                                                                         
001400     03 DP-ADD-IDLANDX3-SEND PIC X(3).                                    
001500*                                 LANDKOD SÄNDANDE LAND                   
001600*                                 COUNTRY CODE SENDING COUNTRY            
001700     03 DP-ADD-IDLANDX3-REC  PIC X(3).                                    
001800*                                 LANDKOD MOTTAGANDE LAND                 
001900*                                 COUNTRY CODE RECEIVING COUNTRY          
002000     03 DP-ADD-IDLEVNR       PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300     03 DP-ADD-IDPARTNR      PIC X(9).                                    
002400*                                 FINANCIELL KUND                         
002500*                                 FINANCIAL CUST                          
002600     03 DP-ADD-IDFINDOC      PIC Z(8)9.                                   
002700*                                 FINANSIELLT DOKUMENT ID                 
002800*                                 FINANCIAL DOCUMENT ID                   
002900     03 DP-ADD-DAFINDOC      PIC 9(8).                                    
003000*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003100*                                 INVOICING DATE   (YYYYMMDD)             
003200     03 DP-ADD-BELEGRAD-1    PIC X(35).                                   
003300*                                 DEL AV LEGAL SELLER NAMN                
003400*                                 PART OF LEGAL SELLER NAME               
003500     03 DP-ADD-BELEGRAD-2    PIC X(35).                                   
003600*                                 DEL AV LEGAL SELLER NAMN                
003700*                                 PART OF LEGAL SELLER NAME               
003800     03 DP-ADD-ADLEG-STREET  PIC X(35).                                   
003900*                                 LEGAL SELLER GATUADRESS                 
004000*                                 LEGAL SELLER STREET ADDRESS             
004100     03 DP-ADD-ADLEG-BOX     PIC X(10).                                   
004200*                                 BOXADRESS LEGAL SELLER                  
004300*                                 LEGAL SELLER BOX ADDRESS                
004400     03 DP-ADD-ADLEG-CITY    PIC X(35).                                   
004500*                                 LEGAL SÄLJARES ADRESS STAD              
004600*                                 LEGAL SELLER ADDRESS CITY               
004700     03 DP-ADD-ADLEG-PCODE   PIC X(10).                                   
004800*                                 LEGAL SELLER ADRESS POSTNR              
004900*                                 LEGAL SELLER POSTAL CODE                
005000     03 DP-ADD-BELAND-LEG    PIC X(35).                                   
005100*                                 LANDSBETECKNING                         
005200*                                 NAME OF COUNTRY                         
005300     03 DP-ADD-IDTFN-LEG     PIC X(20).                                   
005400*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
005500*                                 JARE                                    
005600*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
005700*                                  SELLER                                 
005800     03 DP-ADD-IDTFX-LEG     PIC X(20).                                   
005900*                                 FAXNUMMER LEGAL SÄLJARE                 
006000*                                 FAXNUMBER LEGAL SELLER                  
006100     03 DP-ADD-IDBG-LEG      PIC X(15).                                   
006200*                                 BANKGIRO LEGAL SÄLJARE                  
006300*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
006400*                                 R                                       
006500     03 DP-ADD-IDPG-LEG      PIC X(15).                                   
006600*                                 POSTGIRO LEGAL SÄLJARE                  
006700*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
006800*                                 LER                                     
006900     03 DP-ADD-IDVAT-LEG     PIC X(17).                                   
007000*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
007100*                                 ÄLJARE                                  
007200*                                 VAT REGISTRATION LEGAL PAYER            
007300     03 DP-ADD-IDVAT-LEG-REG-NO                                           
007400                             PIC X(17).                                   
007500*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
007600*                                 ÄLJARE                                  
007700*                                 VAT REGISTRATION LEGAL PAYER            
007800     03 DP-ADD-IDVAT-RESP    PIC X(17).                                   
007900*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
008000*                                 G AVD                                   
008100*                                 VAT REGISTRATION RESPONSIBLE DP         
008200*                                 T                                       
008300     03 DP-ADD-KDVALISO      PIC X(3).                                    
008400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008500*                                 CURRENCY CODE BY ISO-STANDARD.          
008600     03 DP-ADD-PRKURS        PIC Z(5)9.9(5).                              
008700*                                 VALUTAKURS                              
008800*                                 CURRENCY EXCHANGE RATE                  
008900     03 DP-ADD-BEBETVIL      PIC X(30).                                   
009000*                                 BETALNINGSVILLKORSTEXT                  
009100*                                 TERMS OF PAYMENT TEXT                   
009200     03 DP-ADD-BEBET-NAME1   PIC X(35).                                   
009300*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009400*                                 PART OF FINANCIAL CUSTOMER NAME         
009500     03 DP-ADD-BEBET-NAME2   PIC X(35).                                   
009600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009700*                                 PART OF FINANCIAL CUSTOMER NAME         
009800     03 DP-ADD-ADBET-STREET  PIC X(35).                                   
009900*                                 BETALARENS GATUADRESS                   
010000*                                 PAYER ADDRESS STREET                    
010100     03 DP-ADD-ADBET-BOX     PIC X(10).                                   
010200*                                 BOXADRESS BETALNINGSANSVARIG            
010300*                                 PAYER BOX ADDRESS                       
010400     03 DP-ADD-ADBET-CITY    PIC X(35).                                   
010500*                                 BETALARENS STADSADRESS                  
010600*                                 PAYER ADDRESS CITY                      
010700     03 DP-ADD-ADBET-PCODE   PIC X(10).                                   
010800*                                 BETALARENS STADSADRESS POSTNR           
010900*                                 PAYER ADDRESS POSTAL CODE               
011000     03 DP-ADD-BELAND-BET    PIC X(35).                                   
011100*                                 LANDSBETECKNING                         
011200*                                 NAME OF COUNTRY                         
011300     03 DP-ADD-IDVAT-BET     PIC X(17).                                   
011400*                                 MOMSREGISTRERINGSNUMMER BETALAR         
011500*                                 E                                       
011600*                                 VAT REGISTRATION NUMBER PAYER           
011700     03 DP-ADD-SUNTO-PART    PIC Z(10)9.9(2).                             
011800*                                 TOTAL SALES AMOUNT PARTS EXCL.          
011900*                                 VAT                                     
012000     03 DP-ADD-SUBTO-PART    PIC Z(10)9.9(2).                             
012100*                                 TOTAL SALES AMOUNT PARTS INCL.          
012200*                                 VAT                                     
012300     03 DP-ADD-IDEXCUST-1    PIC X(15).                                   
012400*                                 EXTERNT KUNDID                          
012500*                                 EXTERNAL CUSTOMER ID                    
012600     03 DP-ADD-BELEVVIL      PIC X(35).                                   
012700*                                 LEVERANSVILLKOR                         
012800*                                 DELIVERY TERMS                          
012900     03 DP-ADD-IDEXCUST-2    PIC X(15).                                   
013000*                                 EXTERNT KUNDID                          
013100*                                 EXTERNAL CUSTOMER ID                    
013200     03 DP-ADD-IDDC          PIC X(2).                                    
013300*                                 IDENTIFIERARE LAGER                     
013400*                                 WAREHOUSE IDENTIFIER                    
013500     03 DP-ADD-BEANST        PIC X(25).                                   
013600*                                 ANSTÄLLDS NAMN                          
013700*                                 NAME OF EMPLOYED                        
013800     03 DP-ADD-IDUSER        PIC X(8).                                    
013900*                                 ANVÄNDARENS SÄKERHETS ID                
014000*                                 USER SECURITY-IDENTITY                  
014100     03 DP-ADD-BEVAT         PIC X(50).                                   
014200*                                 MOMSKODSBENÄMNING R3                    
014300*                                 VAT CODE DESCRIPTION R3                 
014400     03 DP-ADD-KDAPPEND      PIC X(4).                                    
014500*                                 APPENDIX KOD                            
014600*                                 APPENDIX CODE                           
014700     03 DP-ADD-IDAPPEND      PIC X(8).                                    
014800*                                 APPENDIXVÄRDE                           
014900*                                 APPENDIX ITEM                           
015000     03 DP-ADD-SUNTO-APP     PIC Z(10)9.9(2).                             
015100*                                 SALES AMOUNT FOR APPENDIX EXCL.         
015200*                                  VAT                                    
015300     03 DP-ADD-SUVAT-BILLIT-APP                                           
015400                             PIC Z(10)9.9(2).                             
015500*                                 MOMSVÄRDE FÖR APPENDIX                  
015600*                                 VAT VALUE FOR APPENDIX                  
015700     03 DP-ADD-SUBTO-APP     PIC Z(10)9.9(2).                             
015800*                                 SALES AMOUNT FOR APPENDIX INCL.         
015900*                                  VAT                                    
016000     03 DP-ADD-IDVAT-AGENT   PIC X(17).                                   
016100*                                 MOMSREGISTRERINGSNUMMER                 
016200*                                 VAT REGISTRATION NUMBER                 
016300     03 DP-ADD-BERESPRA-1    PIC X(35).                                   
016400*                                 DEL AV ANSV AVDELNINGS NAMN             
016500*                                 PART OF RESP DEPT  NAME                 
016600     03 DP-ADD-IDTFN-RESP    PIC X(20).                                   
016700*                                 TELEFONNUMMER EXTERNT ANSVARIG          
016800*                                 AVD                                     
016900*                                 TELEPHONE NUMBER EXTERNAL RESPO         
017000*                                 NSIBLE DPT                              
017100     03 DP-ADD-IDTFX-RESP    PIC X(20).                                   
017200*                                 FAXNUMMER ANSVARIG AVD                  
017300*                                 FAXNUMBER RESPONSIBLE DPT               
017400     03 DP-ADD-BETEXT-25     PIC X(100).                                  
017500     03 DP-ADD-BETEXT-26     PIC X(100).                                  
017600     03 DP-ADD-BETEXT-27     PIC X(100).                                  
017700     03 DP-ADD-BETEXT-28     PIC X(100).                                  
017800     03 DP-ADD-BETEXT-29     PIC X(100).                                  
017900     03 DP-ADD-BETEXT-30     PIC X(100).                                  
018000     03 DP-ADD-BETEXT-31     PIC X(100).                                  
018100     03 DP-ADD-BETEXT-32     PIC X(100).                                  
018200     03 DP-ADD-BETEXT-33     PIC X(100).                                  
018300     03 DP-ADD-BETEXT-34     PIC X(100).                                  
018400     03 DP-ADD-BETEXT-35     PIC X(100).                                  
018500     03 DP-ADD-BETEXT-36     PIC X(100).                                  
018600     03 DP-ADD-BETEXT-37     PIC X(100).                                  
018700     03 DP-ADD-BETEXT-38     PIC X(100).                                  
018800     03 DP-ADD-BETEXT-39     PIC X(100).                                  
018900     03 DP-ADD-BETEXT-40     PIC X(100).                                  
019000     03 DP-ADD-BETEXT-41     PIC X(100).                                  
019100     03 DP-ADD-BETEXT-42     PIC X(100).                                  
019200     03 DP-ADD-BETEXT-43     PIC X(100).                                  
019300     03 DP-ADD-BETEXT-44     PIC X(100).                                  
019400*** END OF VILMAII-COPY LENGTH= 2963 BYTES                                
