000100 01  DP-DADR-WF2101.                                                      
000200*                                 DOCUMENT DATA APPENDIX                  
000300     03 DP-DADR-IDAFPRCD     PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 DP-DADR-IDLEGSEL     PIC X(4).                                    
000700*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000800*                                 LEGAL SELLER IDENTITY                   
000900     03 DP-DADR-BEFORMS      PIC X(15).                                   
001000*                                 BENÄMNING PÅ DOKUMENTFORMAT             
001100*                                                                         
001200*                                 DESCRIPTION OF DOCUMENT FORMAT          
001300*                                                                         
001400     03 DP-DADR-IDLANDX3-SEND                                             
001500                             PIC X(3).                                    
001600*                                 LANDKOD SÄNDANDE LAND                   
001700*                                 COUNTRY CODE SENDING COUNTRY            
001800     03 DP-DADR-IDLANDX3-REC PIC X(3).                                    
001900*                                 LANDKOD MOTTAGANDE LAND                 
002000*                                 COUNTRY CODE RECEIVING COUNTRY          
002100     03 DP-DADR-IDLEVNR      PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002400     03 DP-DADR-IDPARTNR     PIC X(9).                                    
002500*                                 PARTNERNUMMER                           
002600*                                 PARTNER NO                              
002700     03 DP-DADR-IDFINDOC     PIC Z(8)9.                                   
002800*                                 FINANSIELLT DOKUMENT ID                 
002900*                                 FINANCIAL DOCUMENT ID                   
003000     03 DP-DADR-DAFINDOC     PIC 9(8).                                    
003100*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003200*                                 INVOICING DATE   (YYYYMMDD)             
003300     03 DP-DADR-BELEGRAD-1   PIC X(35).                                   
003400*                                 DEL AV LEGAL SELLER NAMN                
003500*                                 PART OF LEGAL SELLER NAME               
003600     03 DP-DADR-BELEGRAD-2   PIC X(35).                                   
003700*                                 DEL AV LEGAL SELLER NAMN                
003800*                                 PART OF LEGAL SELLER NAME               
003900     03 DP-DADR-ADLEG-STREET PIC X(35).                                   
004000*                                 LEGAL SELLER GATUADRESS                 
004100*                                 LEGAL SELLER STREET ADDRESS             
004200     03 DP-DADR-ADLEG-BOX    PIC X(10).                                   
004300*                                 BOXADRESS LEGAL SELLER                  
004400*                                 LEGAL SELLER BOX ADDRESS                
004500     03 DP-DADR-ADLEG-CITY   PIC X(35).                                   
004600*                                 LEGAL SÄLJARES ADRESS STAD              
004700*                                 LEGAL SELLER ADDRESS CITY               
004800     03 DP-DADR-ADLEG-PCODE  PIC X(10).                                   
004900*                                 LEGAL SELLER ADRESS POSTNR              
005000*                                 LEGAL SELLER POSTAL CODE                
005100     03 DP-DADR-BELAND-LEG   PIC X(35).                                   
005200*                                 LANDSBETECKNING                         
005300*                                 NAME OF COUNTRY                         
005400     03 DP-DADR-IDTFN-LEG    PIC X(20).                                   
005500*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
005600*                                 JARE                                    
005700*                                 TELEPHONE NUMBER EXTERNAL LEGAL         
005800*                                  SELLER                                 
005900     03 DP-DADR-IDTFX-LEG    PIC X(20).                                   
006000*                                 FAXNUMMER LEGAL SÄLJARE                 
006100*                                 FAXNUMBER LEGAL SELLER                  
006200     03 DP-DADR-IDBG-LEG     PIC X(15).                                   
006300*                                 BANKGIRO LEGAL SÄLJARE                  
006400*                                 BANC CHEQUE ACCOUNT LEGAL SELLE         
006500*                                 R                                       
006600     03 DP-DADR-IDPG-LEG     PIC X(15).                                   
006700*                                 POSTGIRO LEGAL SÄLJARE                  
006800*                                 POSTAL CHEQUE ACCOUNT LEGAL SEL         
006900*                                 LER                                     
007000     03 DP-DADR-IDVAT-LEG    PIC X(17).                                   
007100*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
007200*                                 ÄLJARE                                  
007300*                                 VAT REGISTRATION LEGAL PAYER            
007400     03 DP-DADR-IDVAT-LEG-REG-NO                                          
007500                             PIC X(17).                                   
007600*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
007700*                                 ÄLJARE                                  
007800*                                 VAT REGISTRATION LEGAL PAYER            
007900     03 DP-DADR-IDVAT-RESP   PIC X(17).                                   
008000*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
008100*                                 G AVD                                   
008200*                                 VAT REGISTRATION RESPONSIBLE DP         
008300*                                 T                                       
008400     03 DP-DADR-KDVALISO     PIC X(3).                                    
008500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008600*                                 CURRENCY CODE BY ISO-STANDARD.          
008700     03 DP-DADR-PRKURS       PIC Z(5)9.9(5).                              
008800*                                 VALUTAKURS                              
008900*                                 CURRENCY EXCHANGE RATE                  
009000     03 DP-DADR-BEBETVIL     PIC X(30).                                   
009100*                                 BETALNINGSVILLKORSTEXT                  
009200*                                 TERMS OF PAYMENT TEXT                   
009300     03 DP-DADR-BEBET-NAME1  PIC X(35).                                   
009400*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009500*                                 PART OF FINANCIAL CUSTOMER NAME         
009600     03 DP-DADR-BEBET-NAME2  PIC X(35).                                   
009700*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009800*                                 PART OF FINANCIAL CUSTOMER NAME         
009900     03 DP-DADR-ADBET-STREET PIC X(35).                                   
010000*                                 BETALARENS GATUADRESS                   
010100*                                 PAYER ADDRESS STREET                    
010200     03 DP-DADR-ADBET-BOX    PIC X(10).                                   
010300*                                 BOXADRESS BETALNINGSANSVARIG            
010400*                                 PAYER BOX ADDRESS                       
010500     03 DP-DADR-ADBET-CITY   PIC X(35).                                   
010600*                                 BETALARENS STADSADRESS                  
010700*                                 PAYER ADDRESS CITY                      
010800     03 DP-DADR-ADBET-PCODE  PIC X(10).                                   
010900*                                 BETALARENS STADSADRESS POSTNR           
011000*                                 PAYER ADDRESS POSTAL CODE               
011100     03 DP-DADR-BELAND-BET   PIC X(35).                                   
011200*                                 LANDSBETECKNING                         
011300*                                 NAME OF COUNTRY                         
011400     03 DP-DADR-IDVAT-BET    PIC X(17).                                   
011500*                                 MOMSREGISTRERINGSNUMMER BETALAR         
011600*                                 E                                       
011700*                                 VAT REGISTRATION NUMBER PAYER           
011800     03 DP-DADR-SUNTO-PART   PIC Z(10)9.9(2).                             
011900*                                 TOTAL SALES AMOUNT PARTS EXCL.          
012000*                                 VAT                                     
012100     03 DP-DADR-SUBTO-PART   PIC Z(10)9.9(2).                             
012200*                                 TOTAL SALES AMOUNT PARTS INCL.          
012300*                                 VAT                                     
012400     03 DP-DADR-IDEXCUST-1   PIC X(15).                                   
012500*                                 EXTERNT KUNDID                          
012600*                                 EXTERNAL CUSTOMER ID                    
012700     03 DP-DADR-BELEVVIL     PIC X(35).                                   
012800*                                 LEVERANSVILLKOR                         
012900*                                 DELIVERY TERMS                          
013000     03 DP-DADR-IDEXCUST-2   PIC X(15).                                   
013100*                                 EXTERNT KUNDID                          
013200*                                 EXTERNAL CUSTOMER ID                    
013300     03 DP-DADR-IDDC         PIC X(2).                                    
013400*                                 IDENTIFIERARE LAGER                     
013500*                                 WAREHOUSE IDENTIFIER                    
013600     03 DP-DADR-BEANST       PIC X(25).                                   
013700*                                 ANSTÄLLDS NAMN                          
013800*                                 NAME OF EMPLOYED                        
013900     03 DP-DADR-IDUSER       PIC X(8).                                    
014000*                                 ANVÄNDARENS SÄKERHETS ID                
014100*                                 USER SECURITY-IDENTITY                  
014200     03 DP-DADR-BEVAT        PIC X(50).                                   
014300*                                 MOMSKODSBENÄMNING R3                    
014400*                                 VAT CODE DESCRIPTION R3                 
014500     03 DP-DADR-KDAPPEND     PIC X(4).                                    
014600*                                 APPENDIX KOD                            
014700*                                 APPENDIX CODE                           
014800     03 DP-DADR-IDAPPEND     PIC X(8).                                    
014900*                                 APPENDIXVÄRDE                           
015000*                                 APPENDIX ITEM                           
015100     03 DP-DADR-SUNTO-APP    PIC Z(10)9.9(2).                             
015200*                                 SALES AMOUNT FOR APPENDIX EXCL.         
015300*                                  VAT                                    
015400     03 DP-DADR-SUVAT-BILLIT-APP                                          
015500                             PIC Z(10)9.9(2).                             
015600*                                 MOMSVÄRDE FÖR APPENDIX                  
015700*                                 VAT VALUE FOR APPENDIX                  
015800     03 DP-DADR-SUBTO-APP    PIC Z(10)9.9(2).                             
015900*                                 SALES AMOUNT FOR APPENDIX INCL.         
016000*                                  VAT                                    
016100     03 DP-DADR-IDVAT-AGENT  PIC X(17).                                   
016200*                                 MOMSREGISTRERINGSNUMMER                 
016300*                                 VAT REGISTRATION NUMBER                 
016400     03 DP-DADR-BERESPRA-1   PIC X(35).                                   
016500*                                 DEL AV ANSV AVDELNINGS NAMN             
016600*                                 PART OF RESP DEPT  NAME                 
016700     03 DP-DADR-IDTFN-RESP   PIC X(20).                                   
016800*                                 TELEFONNUMMER EXTERNT ANSVARIG          
016900*                                 AVD                                     
017000*                                 TELEPHONE NUMBER EXTERNAL RESPO         
017100*                                 NSIBLE DPT                              
017200     03 DP-DADR-IDTFX-RESP   PIC X(20).                                   
017300*                                 FAXNUMMER ANSVARIG AVD                  
017400*                                 FAXNUMBER RESPONSIBLE DPT               
017500     03 DP-DADR-BETEXT       PIC X(125).                                  
017600*** END OF VILMAII-COPY LENGTH= 1088 BYTES                                
