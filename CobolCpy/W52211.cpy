000100 01  W52211.                                                              
000200*                                 INTRASTAT DATA FOR DISTRIBUTION         
000300     03 TIAAAA               PIC 9(4)                                     
000400                             VALUE ZEROS.                                 
000500*                                 ÅRTAL (ÅÅÅÅ)                            
000600*                                 YEAR  (YYYY)                            
000700     03 TIMM                 PIC 9(2)                                     
000800                             VALUE ZEROS.                                 
000900*                                 MÅNAD (MM)                              
001000*                                 MONTH (MM)                              
001100     03 SEMICOLON            PIC X                                        
001200                             VALUE ';'.                                   
001300*                                 SEMIKOLON                               
001400*                                 SEMICOLON                               
001500     03 IDLANDX3-SEND        PIC X(3)                                     
001600                             VALUE SPACES.                                
001700*                                 LANDKOD SÄNDANDE LAND                   
001800*                                 COUNTRY CODE SENDING COUNTRY            
001900     03 SEMICOLON            PIC X                                        
002000                             VALUE ';'.                                   
002100*                                 SEMIKOLON                               
002200*                                 SEMICOLON                               
002300     03 IDLANDX3-REC         PIC X(3)                                     
002400                             VALUE SPACES.                                
002500*                                 LANDKOD MOTTAGANDE LAND                 
002600*                                 COUNTRY CODE RECEIVING COUNTRY          
002700     03 SEMICOLON            PIC X                                        
002800                             VALUE ';'.                                   
002900*                                 SEMIKOLON                               
003000*                                 SEMICOLON                               
003100     03 KDVALISO             PIC X(3)                                     
003200                             VALUE SPACES.                                
003300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003400*                                 CURRENCY CODE BY ISO-STANDARD.          
003500     03 SEMICOLON            PIC X                                        
003600                             VALUE ';'.                                   
003700*                                 SEMIKOLON                               
003800*                                 SEMICOLON                               
003900     03 PRKURS               PIC 9(6)V9(5)                                
004000                             VALUE ZEROS.                                 
004100*                                 VALUTAKURS                              
004200*                                 CURRENCY EXCHANGE RATE                  
004300     03 SEMICOLON            PIC X                                        
004400                             VALUE ';'.                                   
004500*                                 SEMIKOLON                               
004600*                                 SEMICOLON                               
004700     03 KDINTTYP-OLD         PIC 9                                        
004800                             VALUE ZERO.                                  
004900     03 SEMICOLON            PIC X                                        
005000                             VALUE ';'.                                   
005100*                                 SEMIKOLON                               
005200*                                 SEMICOLON                               
005300     03 DAFINDOC             PIC 9(8)                                     
005400                             VALUE ZEROS.                                 
005500*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
005600*                                 INVOICING DATE   (YYYYMMDD)             
005700     03 SEMICOLON            PIC X                                        
005800                             VALUE ';'.                                   
005900*                                 SEMIKOLON                               
006000*                                 SEMICOLON                               
006100     03 IDFINDOC             PIC 9(9)                                     
006200                             VALUE ZEROS.                                 
006300*                                 FINANSIELLT DOKUMENT ID                 
006400*                                 FINANCIAL DOCUMENT ID                   
006500     03 SEMICOLON            PIC X                                        
006600                             VALUE ';'.                                   
006700*                                 SEMIKOLON                               
006800*                                 SEMICOLON                               
006900     03 IDSTATNR             PIC 9(8)                                     
007000                             VALUE ZEROS.                                 
007100*                                 STATISTISKT NUMMER                      
007200*                                 1 = NORSKT                              
007300*                                 2 = ENGELSKT                            
007400*                                 3 = BELGISKT                            
007500*                                 4 = PERUANSKT                           
007600*                                 5 = SVENSKT                             
007700*                                 6 =                                     
007800*                                 STATISTICAL NO.                         
007900     03 SEMICOLON            PIC X                                        
008000                             VALUE ';'.                                   
008100*                                 SEMIKOLON                               
008200*                                 SEMICOLON                               
008300     03 IDARTNR              PIC 9(8)                                     
008400                             VALUE ZEROS.                                 
008500*                                 ARTIKELNUMMER                           
008600*                                 PART NUMBER                             
008700     03 SEMICOLON            PIC X                                        
008800                             VALUE ';'.                                   
008900*                                 SEMIKOLON                               
009000*                                 SEMICOLON                               
009100     03 BEART                PIC X(25)                                    
009200                             VALUE SPACES.                                
009300*                                 ARTIKELBENÄMNING                        
009400*                                 PART DESCRIPTION                        
009500     03 SEMICOLON            PIC X                                        
009600                             VALUE ';'.                                   
009700*                                 SEMIKOLON                               
009800*                                 SEMICOLON                               
009900     03 VKORDNTO             PIC 9(5)V9(1)                                
010000                             VALUE ZEROS.                                 
010100*                                 ORDERVIKT NETTO (KG)                    
010200*                                 WEIGHT PER ORDER NETTO (KG)             
010300     03 SEMICOLON            PIC X                                        
010400                             VALUE ';'.                                   
010500*                                 SEMIKOLON                               
010600*                                 SEMICOLON                               
010700     03 KVLEVART             PIC 9(7)                                     
010800                             VALUE ZEROS.                                 
010900*                                 LEVERERAT ANTAL STYCK                   
011000*                                 DELIVERED QUANTITY                      
011100     03 SEMICOLON            PIC X                                        
011200                             VALUE ';'.                                   
011300*                                 SEMIKOLON                               
011400*                                 SEMICOLON                               
011500     03 KDARTURS             PIC X(2)                                     
011600                             VALUE SPACES.                                
011700*                                 ARTIKELURSPRUNGSKOD                     
011800*                                 COUNTRY OF ORIGIN                       
011900     03 SEMICOLON            PIC X                                        
012000                             VALUE ';'.                                   
012100*                                 SEMIKOLON                               
012200*                                 SEMICOLON                               
012300     03 SUNTO                PIC +9(10)V9(2)                              
012400                             VALUE ZEROS.                                 
012500*                                 TOTAL SALES AMOUNT EXCL. VAT            
012600     03 SEMICOLON            PIC X                                        
012700                             VALUE ';'.                                   
012800*                                 SEMIKOLON                               
012900*                                 SEMICOLON                               
013000     03 BELEVVIL             PIC X(35)                                    
013100                             VALUE SPACES.                                
013200*                                 LEVERANSVILLKOR                         
013300*                                 DELIVERY TERMS                          
013400     03 SEMICOLON            PIC X                                        
013500                             VALUE ';'.                                   
013600*                                 SEMIKOLON                               
013700*                                 SEMICOLON                               
013800     03 KDBEH                PIC 9                                        
013900                             VALUE ZERO.                                  
014000*                                 BEHANDLINGSKOD                          
014100*                                 TREATMENT STATUS CODE                   
014200     03 SEMICOLON            PIC X                                        
014300                             VALUE ';'.                                   
014400*                                 SEMIKOLON                               
014500*                                 SEMICOLON                               
014600     03 IDDISTR              PIC 9(4)                                     
014700                             VALUE ZEROS.                                 
014800*                                 DISTRIKTNUMMER                          
014900*                                 DISTRICT NUMBER                         
015000     03 SEMICOLON            PIC X                                        
015100                             VALUE ';'.                                   
015200*                                 SEMIKOLON                               
015300*                                 SEMICOLON                               
015400     03 IDKUNDNR             PIC 9(6)                                     
015500                             VALUE ZEROS.                                 
015600*                                 KUNDNUMMER                              
015700*                                 CUSTOMER NO                             
015800     03 SEMICOLON            PIC X                                        
015900                             VALUE ';'.                                   
016000*                                 SEMIKOLON                               
016100*                                 SEMICOLON                               
016200     03 IDVAT                PIC X(17)                                    
016300                             VALUE SPACES.                                
016400*                                 MOMSREGISTRERINGSNUMMER                 
016500*                                 VAT REGISTRATION NUMBER                 
016600     03 SEMICOLON            PIC X                                        
016700                             VALUE ';'.                                   
016800*                                 SEMIKOLON                               
016900*                                 SEMICOLON                               
017000     03 KDINTTYP             PIC 9(2)                                     
017100                             VALUE ZEROS.                                 
017200*                                 AFFÄRSHÄNDELSEKOD                       
017300*                                 BUSINESS ACTION CODE                    
017400     03 SEMICOLON            PIC X                                        
017500                             VALUE ';'.                                   
017600*                                 SEMIKOLON                               
017700*                                 SEMICOLON                               
017800     03 VKORDNTO-3DEC        PIC 9(5)V9(3)                                
017900                             VALUE ZEROS.                                 
018000*                                 ORDERVIKT NETTO (KG)                    
018100*                                 WEIGHT PER ORDER NETTO (KG)             
018200     03 SEMICOLON            PIC X                                        
018300                             VALUE ';'.                                   
018400*                                 SEMIKOLON                               
018500*                                 SEMICOLON                               
018600     03 IDVAT-BET            PIC X(17)                                    
018700                             VALUE SPACES.                                
018800*                                 MOMSREGISTRERINGSNUMMER BETALAR         
018900*                                 E                                       
019000*                                 VAT REGISTRATION NUMBER PAYER           
019100     03 SEMICOLON            PIC X                                        
019200                             VALUE ';'.                                   
019300*                                 SEMIKOLON                               
019400*                                 SEMICOLON                               
019500     03 IDVAT-RESP           PIC X(17)                                    
019600                             VALUE SPACES.                                
019700*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
019800*                                 G AVD                                   
019900*                                 VAT REGISTRATION RESPONSIBLE DP         
020000*                                 T                                       
020100     03 SEMICOLON            PIC X                                        
020200                             VALUE ';'.                                   
020300*                                 SEMIKOLON                               
020400*                                 SEMICOLON                               
020500     03 IDVAT-LEG            PIC X(17)                                    
020600                             VALUE SPACES.                                
020700*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
020800*                                 ÄLJARE                                  
020900*                                 VAT REGISTRATION LEGAL PAYER            
021000     03 SEMICOLON            PIC X                                        
021100                             VALUE ';'.                                   
021200*                                 SEMIKOLON                               
021300*                                 SEMICOLON                               
021400     03 IDVAT-AGENT          PIC X(17)                                    
021500                             VALUE SPACES.                                
021600*                                 MOMSREGISTRERINGSNUMMER AGENT           
021700*                                 VAT REGISTRATION VAT AGENT              
021800     03 SEMICOLON            PIC X                                        
021900                             VALUE ';'.                                   
022000*                                 SEMIKOLON                               
022100*                                 SEMICOLON                               
022200*** END OF VILMAII-COPY LENGTH= 280 BYTES                                 
