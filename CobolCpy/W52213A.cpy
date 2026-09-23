000100 01  W52213A.                                                             
000200*                                 INTRASTAT DATA FOR DISTRIBUTION         
000300     03 TIAAAA               PIC 9(4)                                     
000400                             VALUE ZEROS.                                 
000500*                                 ÅRTAL (ÅÅÅÅ)                            
000600*                                 YEAR  (YYYY)                            
000700     03 SEMICOLON            PIC X                                        
000800                             VALUE ';'.                                   
000900*                                 SEMIKOLON                               
001000*                                 SEMICOLON                               
001100     03 TIMM                 PIC 9(2)                                     
001200                             VALUE ZEROS.                                 
001300*                                 MÅNAD (MM)                              
001400*                                 MONTH (MM)                              
001500     03 SEMICOLON            PIC X                                        
001600                             VALUE ';'.                                   
001700*                                 SEMIKOLON                               
001800*                                 SEMICOLON                               
001900     03 IDLANDX3-SEND        PIC X(3)                                     
002000                             VALUE SPACES.                                
002100*                                 LANDKOD SÄNDANDE LAND                   
002200*                                 COUNTRY CODE SENDING COUNTRY            
002300     03 SEMICOLON            PIC X                                        
002400                             VALUE ';'.                                   
002500*                                 SEMIKOLON                               
002600*                                 SEMICOLON                               
002700     03 IDLANDX3-REC         PIC X(3)                                     
002800                             VALUE SPACES.                                
002900*                                 LANDKOD MOTTAGANDE LAND                 
003000*                                 COUNTRY CODE RECEIVING COUNTRY          
003100     03 SEMICOLON            PIC X                                        
003200                             VALUE ';'.                                   
003300*                                 SEMIKOLON                               
003400*                                 SEMICOLON                               
003500     03 KDVALISO             PIC X(3)                                     
003600                             VALUE SPACES.                                
003700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003800*                                 CURRENCY CODE BY ISO-STANDARD.          
003900     03 SEMICOLON            PIC X                                        
004000                             VALUE ';'.                                   
004100*                                 SEMIKOLON                               
004200*                                 SEMICOLON                               
004300     03 PRKURS               PIC Z(5)9.9(5)                               
004400                             VALUE ZEROS.                                 
004500*                                 VALUTAKURS                              
004600*                                 CURRENCY EXCHANGE RATE                  
004700     03 SEMICOLON            PIC X                                        
004800                             VALUE ';'.                                   
004900*                                 SEMIKOLON                               
005000*                                 SEMICOLON                               
005100     03 KDINTTYP-OLD         PIC 9                                        
005200                             VALUE ZERO.                                  
005300     03 SEMICOLON            PIC X                                        
005400                             VALUE ';'.                                   
005500*                                 SEMIKOLON                               
005600*                                 SEMICOLON                               
005700     03 DAFINDOC             PIC 9(8)                                     
005800                             VALUE ZEROS.                                 
005900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
006000*                                 INVOICING DATE   (YYYYMMDD)             
006100     03 SEMICOLON            PIC X                                        
006200                             VALUE ';'.                                   
006300*                                 SEMIKOLON                               
006400*                                 SEMICOLON                               
006500     03 IDFINDOC             PIC 9(9)                                     
006600                             VALUE ZEROS.                                 
006700*                                 FINANSIELLT DOKUMENT ID                 
006800*                                 FINANCIAL DOCUMENT ID                   
006900     03 SEMICOLON            PIC X                                        
007000                             VALUE ';'.                                   
007100*                                 SEMIKOLON                               
007200*                                 SEMICOLON                               
007300     03 IDSTATNR             PIC 9(8)                                     
007400                             VALUE ZEROS.                                 
007500*                                 STATISTISKT NUMMER                      
007600*                                 1 = NORSKT                              
007700*                                 2 = ENGELSKT                            
007800*                                 3 = BELGISKT                            
007900*                                 4 = PERUANSKT                           
008000*                                 5 = SVENSKT                             
008100*                                 6 =                                     
008200*                                 STATISTICAL NO.                         
008300     03 SEMICOLON            PIC X                                        
008400                             VALUE ';'.                                   
008500*                                 SEMIKOLON                               
008600*                                 SEMICOLON                               
008700     03 IDARTNR              PIC 9(8)                                     
008800                             VALUE ZEROS.                                 
008900*                                 ARTIKELNUMMER                           
009000*                                 PART NUMBER                             
009100     03 SEMICOLON            PIC X                                        
009200                             VALUE ';'.                                   
009300*                                 SEMIKOLON                               
009400*                                 SEMICOLON                               
009500     03 BEART                PIC X(25)                                    
009600                             VALUE SPACES.                                
009700*                                 ARTIKELBENÄMNING                        
009800*                                 PART DESCRIPTION                        
009900     03 SEMICOLON            PIC X                                        
010000                             VALUE ';'.                                   
010100*                                 SEMIKOLON                               
010200*                                 SEMICOLON                               
010300     03 VKORDNTO             PIC Z(4)9.9                                  
010400                             VALUE ZEROS.                                 
010500*                                 ORDERVIKT NETTO (KG)                    
010600*                                 WEIGHT PER ORDER NETTO (KG)             
010700     03 SEMICOLON            PIC X                                        
010800                             VALUE ';'.                                   
010900*                                 SEMIKOLON                               
011000*                                 SEMICOLON                               
011100     03 KVLEVART             PIC Z(5)9                                    
011200                             VALUE ZEROS.                                 
011300*                                 LEVERERAT ANTAL STYCK                   
011400*                                 DELIVERED QUANTITY                      
011500     03 SEMICOLON            PIC X                                        
011600                             VALUE ';'.                                   
011700*                                 SEMIKOLON                               
011800*                                 SEMICOLON                               
011900     03 KDARTURS             PIC X(2)                                     
012000                             VALUE SPACES.                                
012100*                                 ARTIKELURSPRUNGSKOD                     
012200*                                 COUNTRY OF ORIGIN                       
012300     03 SEMICOLON            PIC X                                        
012400                             VALUE ';'.                                   
012500*                                 SEMIKOLON                               
012600*                                 SEMICOLON                               
012700     03 SUNTO                PIC -(10)9.9(2)                              
012800                             VALUE ZEROS.                                 
012900*                                 TOTAL SALES AMOUNT EXCL. VAT            
013000     03 SEMICOLON            PIC X                                        
013100                             VALUE ';'.                                   
013200*                                 SEMIKOLON                               
013300*                                 SEMICOLON                               
013400     03 BELEVVIL             PIC X(35)                                    
013500                             VALUE SPACES.                                
013600*                                 LEVERANSVILLKOR                         
013700*                                 DELIVERY TERMS                          
013800     03 SEMICOLON            PIC X                                        
013900                             VALUE ';'.                                   
014000*                                 SEMIKOLON                               
014100*                                 SEMICOLON                               
014200     03 KDBEH                PIC 9                                        
014300                             VALUE ZERO.                                  
014400*                                 BEHANDLINGSKOD                          
014500*                                 TREATMENT STATUS CODE                   
014600     03 SEMICOLON            PIC X                                        
014700                             VALUE ';'.                                   
014800*                                 SEMIKOLON                               
014900*                                 SEMICOLON                               
015000     03 IDDISTR              PIC 9(4)                                     
015100                             VALUE ZEROS.                                 
015200*                                 DISTRIKTNUMMER                          
015300*                                 DISTRICT NUMBER                         
015400     03 SEMICOLON            PIC X                                        
015500                             VALUE ';'.                                   
015600*                                 SEMIKOLON                               
015700*                                 SEMICOLON                               
015800     03 IDKUNDNR             PIC 9(6)                                     
015900                             VALUE ZEROS.                                 
016000*                                 KUNDNUMMER                              
016100*                                 CUSTOMER NO                             
016200     03 SEMICOLON            PIC X                                        
016300                             VALUE ';'.                                   
016400*                                 SEMIKOLON                               
016500*                                 SEMICOLON                               
016600     03 IDVAT                PIC X(17)                                    
016700                             VALUE SPACES.                                
016800*                                 MOMSREGISTRERINGSNUMMER                 
016900*                                 VAT REGISTRATION NUMBER                 
017000     03 SEMICOLON            PIC X                                        
017100                             VALUE ';'.                                   
017200*                                 SEMIKOLON                               
017300*                                 SEMICOLON                               
017400     03 KDINTTYP             PIC 9(2)                                     
017500                             VALUE ZEROS.                                 
017600*                                 AFFÄRSHÄNDELSEKOD                       
017700*                                 BUSINESS ACTION CODE                    
017800     03 SEMICOLON            PIC X                                        
017900                             VALUE ';'.                                   
018000*                                 SEMIKOLON                               
018100*                                 SEMICOLON                               
018200     03 VKORDNTO-3DEC        PIC Z(5)9.9(3)                               
018300                             VALUE ZEROS.                                 
018400*                                 ORDERVIKT NETTO (KG)                    
018500*                                 WEIGHT PER ORDER NETTO (KG)             
018600     03 SEMICOLON            PIC X                                        
018700                             VALUE ';'.                                   
018800*                                 SEMIKOLON                               
018900*                                 SEMICOLON                               
019000     03 IDVAT-BET            PIC X(17)                                    
019100                             VALUE SPACES.                                
019200*                                 MOMSREGISTRERINGSNUMMER BETALAR         
019300*                                 E                                       
019400*                                 VAT REGISTRATION NUMBER PAYER           
019500     03 SEMICOLON            PIC X                                        
019600                             VALUE ';'.                                   
019700*                                 SEMIKOLON                               
019800*                                 SEMICOLON                               
019900     03 IDVAT-RESP           PIC X(17)                                    
020000                             VALUE SPACES.                                
020100*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
020200*                                 G AVD                                   
020300*                                 VAT REGISTRATION RESPONSIBLE DP         
020400*                                 T                                       
020500     03 SEMICOLON            PIC X                                        
020600                             VALUE ';'.                                   
020700*                                 SEMIKOLON                               
020800*                                 SEMICOLON                               
020900     03 IDVAT-LEG            PIC X(17)                                    
021000                             VALUE SPACES.                                
021100*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
021200*                                 ÄLJARE                                  
021300*                                 VAT REGISTRATION LEGAL PAYER            
021400     03 SEMICOLON            PIC X                                        
021500                             VALUE ';'.                                   
021600*                                 SEMIKOLON                               
021700*                                 SEMICOLON                               
021800     03 IDVAT-AGENT          PIC X(17)                                    
021900                             VALUE SPACES.                                
022000*                                 MOMSREGISTRERINGSNUMMER AGENT           
022100*                                 VAT REGISTRATION VAT AGENT              
022200     03 FILLER               PIC X(420)                                   
022300                             VALUE SPACES.                                
022400*** END OF VILMAII-COPY LENGTH= 704 BYTES                                 
