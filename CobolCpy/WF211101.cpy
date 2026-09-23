000100 01  FI-HEAD-WF2111.                                                      
000200*                                 DOCUMENT DATA HEADER                    
000300     03 FI-HEAD-IDAFPRCD     PIC X(10)                                    
000400                             VALUE SPACES.                                
000500*                                 AFP-BLANKETT POSTTYP                    
000600*                                 AFP FORMS RECORD TYPE                   
000700     03 FI-HEAD-HORIZTAB     PIC X                                        
000800                             VALUE X'05'.                                 
000900*                                 HORIZTAB                                
001000*                                 HORIZTAB                                
001100     03 FI-HEAD-IDLEGSEL     PIC X(4)                                     
001200                             VALUE SPACES.                                
001300*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001400*                                 LEGAL SELLER IDENTITY                   
001500     03 FI-HEAD-HORIZTAB     PIC X                                        
001600                             VALUE X'05'.                                 
001700*                                 HORIZTAB                                
001800*                                 HORIZTAB                                
001900     03 FI-HEAD-BEFORMS      PIC X(15)                                    
002000                             VALUE SPACES.                                
002100*                                 BENÄMNING PÅ DOKUMENTFORMAT             
002200*                                                                         
002300*                                 DESCRIPTION OF DOCUMENT FORMAT          
002400*                                                                         
002500     03 FI-HEAD-HORIZTAB     PIC X                                        
002600                             VALUE X'05'.                                 
002700*                                 HORIZTAB                                
002800*                                 HORIZTAB                                
002900     03 FI-HEAD-IDLANDX3-SEND                                             
003000                             PIC X(3)                                     
003100                             VALUE SPACES.                                
003200*                                 LANDKOD SÄNDANDE LAND                   
003300*                                 COUNTRY CODE SENDING COUNTRY            
003400     03 FI-HEAD-HORIZTAB     PIC X                                        
003500                             VALUE X'05'.                                 
003600*                                 HORIZTAB                                
003700*                                 HORIZTAB                                
003800     03 FI-HEAD-IDLANDX3-BET PIC X(3)                                     
003900                             VALUE SPACES.                                
004000*                                 LANDKOD BETALANDE KUND ETC              
004100*                                 COUNTRY CODE PAYING CUSTOMER ET         
004200*                                 C                                       
004300     03 FI-HEAD-HORIZTAB     PIC X                                        
004400                             VALUE X'05'.                                 
004500*                                 HORIZTAB                                
004600*                                 HORIZTAB                                
004700     03 FI-HEAD-IDLEVNR      PIC X(5)                                     
004800                             VALUE SPACES.                                
004900*                                 LEVERANTÖRNUMMER                        
005000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005100     03 FI-HEAD-HORIZTAB     PIC X                                        
005200                             VALUE X'05'.                                 
005300*                                 HORIZTAB                                
005400*                                 HORIZTAB                                
005500     03 FI-HEAD-IDPARTNR     PIC X(9)                                     
005600                             VALUE SPACES.                                
005700*                                 PARTNERNUMMER                           
005800*                                 PARTNER NO                              
005900     03 FI-HEAD-HORIZTAB     PIC X                                        
006000                             VALUE X'05'.                                 
006100*                                 HORIZTAB                                
006200*                                 HORIZTAB                                
006300     03 FI-HEAD-IDFINDOC     PIC Z(8)9                                    
006400                             VALUE ZEROS.                                 
006500*                                 FINANSIELLT DOKUMENT ID                 
006600*                                 FINANCIAL DOCUMENT ID                   
006700     03 FI-HEAD-HORIZTAB     PIC X                                        
006800                             VALUE X'05'.                                 
006900*                                 HORIZTAB                                
007000*                                 HORIZTAB                                
007100     03 FI-HEAD-DAFINDOC     PIC 9(8)                                     
007200                             VALUE ZEROS.                                 
007300*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
007400*                                 INVOICING DATE   (YYYYMMDD)             
007500     03 FI-HEAD-HORIZTAB     PIC X                                        
007600                             VALUE X'05'.                                 
007700*                                 HORIZTAB                                
007800*                                 HORIZTAB                                
007900     03 FI-HEAD-IDVAT-LEG    PIC X(17)                                    
008000                             VALUE SPACES.                                
008100*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
008200*                                 ÄLJARE                                  
008300*                                 VAT REGISTRATION LEGAL PAYER            
008400     03 FI-HEAD-HORIZTAB     PIC X                                        
008500                             VALUE X'05'.                                 
008600*                                 HORIZTAB                                
008700*                                 HORIZTAB                                
008800     03 FI-HEAD-IDVAT-RESP   PIC X(17)                                    
008900                             VALUE SPACES.                                
009000*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
009100*                                 G AVD                                   
009200*                                 VAT REGISTRATION RESPONSIBLE DP         
009300*                                 T                                       
009400     03 FI-HEAD-HORIZTAB     PIC X                                        
009500                             VALUE X'05'.                                 
009600*                                 HORIZTAB                                
009700*                                 HORIZTAB                                
009800     03 FI-HEAD-KDVALISO     PIC X(3)                                     
009900                             VALUE SPACES.                                
010000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010100*                                 CURRENCY CODE BY ISO-STANDARD.          
010200     03 FI-HEAD-HORIZTAB     PIC X                                        
010300                             VALUE X'05'.                                 
010400*                                 HORIZTAB                                
010500*                                 HORIZTAB                                
010600     03 FI-HEAD-PRKURS       PIC Z(5)9.9(5)                               
010700                             VALUE ZEROS.                                 
010800*                                 VALUTAKURS                              
010900*                                 CURRENCY EXCHANGE RATE                  
011000     03 FI-HEAD-HORIZTAB     PIC X                                        
011100                             VALUE X'05'.                                 
011200*                                 HORIZTAB                                
011300*                                 HORIZTAB                                
011400     03 FI-HEAD-BEBETVIL     PIC X(30)                                    
011500                             VALUE SPACES.                                
011600*                                 BETALNINGSVILLKORSTEXT                  
011700*                                 TERMS OF PAYMENT TEXT                   
011800     03 FI-HEAD-HORIZTAB     PIC X                                        
011900                             VALUE X'05'.                                 
012000*                                 HORIZTAB                                
012100*                                 HORIZTAB                                
012200     03 FI-HEAD-IDVAT-BET    PIC X(17)                                    
012300                             VALUE SPACES.                                
012400*                                 MOMSREGISTRERINGSNUMMER BETALAR         
012500*                                 E                                       
012600*                                 VAT REGISTRATION NUMBER PAYER           
012700     03 FI-HEAD-HORIZTAB     PIC X                                        
012800                             VALUE X'05'.                                 
012900*                                 HORIZTAB                                
013000*                                 HORIZTAB                                
013100     03 FI-HEAD-SUNTO-PART   PIC Z(10)9.9(2)                              
013200                             VALUE ZEROS.                                 
013300*                                 TOTAL SALES AMOUNT PARTS EXCL.          
013400*                                 VAT                                     
013500     03 FI-HEAD-HORIZTAB     PIC X                                        
013600                             VALUE X'05'.                                 
013700*                                 HORIZTAB                                
013800*                                 HORIZTAB                                
013900     03 FI-HEAD-SUBTO-PART   PIC Z(10)9.9(2)                              
014000                             VALUE ZEROS.                                 
014100*                                 TOTAL SALES AMOUNT PARTS INCL.          
014200*                                 VAT                                     
014300     03 FI-HEAD-HORIZTAB     PIC X                                        
014400                             VALUE X'05'.                                 
014500*                                 HORIZTAB                                
014600*                                 HORIZTAB                                
014700     03 FI-HEAD-IDEXCUST-1   PIC X(15)                                    
014800                             VALUE SPACES.                                
014900*                                 EXTERNT KUNDID                          
015000*                                 EXTERNAL CUSTOMER ID                    
015100     03 FI-HEAD-HORIZTAB     PIC X                                        
015200                             VALUE X'05'.                                 
015300*                                 HORIZTAB                                
015400*                                 HORIZTAB                                
015500     03 FI-HEAD-BELEVVIL     PIC X(35)                                    
015600                             VALUE SPACES.                                
015700*                                 LEVERANSVILLKOR                         
015800*                                 DELIVERY TERMS                          
015900     03 FI-HEAD-HORIZTAB     PIC X                                        
016000                             VALUE X'05'.                                 
016100*                                 HORIZTAB                                
016200*                                 HORIZTAB                                
016300     03 FI-HEAD-IDEXCUST-2   PIC X(15)                                    
016400                             VALUE SPACES.                                
016500*                                 EXTERNT KUNDID                          
016600*                                 EXTERNAL CUSTOMER ID                    
016700     03 FI-HEAD-HORIZTAB     PIC X                                        
016800                             VALUE X'05'.                                 
016900*                                 HORIZTAB                                
017000*                                 HORIZTAB                                
017100     03 FI-HEAD-IDDC         PIC X(2)                                     
017200                             VALUE SPACES.                                
017300*                                 IDENTIFIERARE LAGER                     
017400*                                 WAREHOUSE IDENTIFIER                    
017500     03 FI-HEAD-HORIZTAB     PIC X                                        
017600                             VALUE X'05'.                                 
017700*                                 HORIZTAB                                
017800*                                 HORIZTAB                                
017900     03 FI-HEAD-BEANST       PIC X(25)                                    
018000                             VALUE SPACES.                                
018100*                                 ANSTÄLLDS NAMN                          
018200*                                 NAME OF EMPLOYED                        
018300     03 FI-HEAD-HORIZTAB     PIC X                                        
018400                             VALUE X'05'.                                 
018500*                                 HORIZTAB                                
018600*                                 HORIZTAB                                
018700     03 FI-HEAD-IDUSER       PIC X(8)                                     
018800                             VALUE SPACES.                                
018900*                                 ANVÄNDARENS SÄKERHETS ID                
019000*                                 USER SECURITY-IDENTITY                  
019100     03 FI-HEAD-HORIZTAB     PIC X                                        
019200                             VALUE X'05'.                                 
019300*                                 HORIZTAB                                
019400*                                 HORIZTAB                                
019500     03 FI-HEAD-BEVAT        PIC X(50)                                    
019600                             VALUE SPACES.                                
019700*                                 MOMSKODSBENÄMNING R3                    
019800*                                 VAT CODE DESCRIPTION R3                 
019900     03 FI-HEAD-HORIZTAB     PIC X                                        
020000                             VALUE X'05'.                                 
020100*                                 HORIZTAB                                
020200*                                 HORIZTAB                                
020300     03 FI-HEAD-IDVAT-AGENT  PIC X(17)                                    
020400                             VALUE SPACES.                                
020500*                                 MOMSREGISTRERINGSNUMMER                 
020600*                                 VAT REGISTRATION NUMBER                 
020700     03 FI-HEAD-HORIZTAB     PIC X                                        
020800                             VALUE X'05'.                                 
020900*                                 HORIZTAB                                
021000*                                 HORIZTAB                                
021100     03 FI-HEAD-BETEXT-5     PIC X(50)                                    
021200                             VALUE SPACES.                                
021300     03 FI-HEAD-HORIZTAB     PIC X                                        
021400                             VALUE X'05'.                                 
021500*                                 HORIZTAB                                
021600*                                 HORIZTAB                                
021700     03 FI-HEAD-BETEXT-6     PIC X(50)                                    
021800                             VALUE SPACES.                                
021900     03 FI-HEAD-HORIZTAB     PIC X                                        
022000                             VALUE X'05'.                                 
022100*                                 HORIZTAB                                
022200*                                 HORIZTAB                                
022300     03 FI-HEAD-BETEXT-7     PIC X(50)                                    
022400                             VALUE SPACES.                                
022500     03 FI-HEAD-HORIZTAB     PIC X                                        
022600                             VALUE X'05'.                                 
022700*                                 HORIZTAB                                
022800*                                 HORIZTAB                                
022900     03 FI-HEAD-BETEXT-8     PIC X(50)                                    
023000                             VALUE SPACES.                                
023100*** END OF VILMAII-COPY LENGTH= 585 BYTES                                 
