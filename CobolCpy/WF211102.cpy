000100 01  FI-LINE-WF2111.                                                      
000200*                                 DOCUMENT DATA LINE                      
000300     03 FI-LINE-IDAFPRCD     PIC X(10)                                    
000400                             VALUE SPACES.                                
000500*                                 AFP-BLANKETT POSTTYP                    
000600*                                 AFP FORMS RECORD TYPE                   
000700     03 FI-LINE-HORIZTAB     PIC X                                        
000800                             VALUE X'05'.                                 
000900*                                 HORIZTAB                                
001000*                                 HORIZTAB                                
001100     03 FI-LINE-IDEXCUST-1   PIC X(15)                                    
001200                             VALUE SPACES.                                
001300*                                 EXTERNT KUNDID                          
001400*                                 EXTERNAL CUSTOMER ID                    
001500     03 FI-LINE-HORIZTAB     PIC X                                        
001600                             VALUE X'05'.                                 
001700*                                 HORIZTAB                                
001800*                                 HORIZTAB                                
001900     03 FI-LINE-IDEXCUST-2   PIC X(15)                                    
002000                             VALUE SPACES.                                
002100*                                 EXTERNT KUNDID                          
002200*                                 EXTERNAL CUSTOMER ID                    
002300     03 FI-LINE-HORIZTAB     PIC X                                        
002400                             VALUE X'05'.                                 
002500*                                 HORIZTAB                                
002600*                                 HORIZTAB                                
002700     03 FI-LINE-IDBUNDLE     PIC X(15)                                    
002800                             VALUE SPACES.                                
002900*                                 BUNDLE ID                               
003000*                                 BUNDLE ID                               
003100     03 FI-LINE-HORIZTAB     PIC X                                        
003200                             VALUE X'05'.                                 
003300*                                 HORIZTAB                                
003400*                                 HORIZTAB                                
003500     03 FI-LINE-IDREF        PIC X(15)                                    
003600                             VALUE SPACES.                                
003700*                                 REFERENS ID                             
003800*                                 REFERENCE ID                            
003900     03 FI-LINE-HORIZTAB     PIC X                                        
004000                             VALUE X'05'.                                 
004100*                                 HORIZTAB                                
004200*                                 HORIZTAB                                
004300     03 FI-LINE-BEVOLREF     PIC X(10)                                    
004400                             VALUE SPACES.                                
004500*                                 VOLVO REFERENS                          
004600*                                 VOLVO REFERENCE                         
004700     03 FI-LINE-HORIZTAB     PIC X                                        
004800                             VALUE X'05'.                                 
004900*                                 HORIZTAB                                
005000*                                 HORIZTAB                                
005100     03 FI-LINE-BERADREF     PIC X(10)                                    
005200                             VALUE SPACES.                                
005300     03 FI-LINE-HORIZTAB     PIC X                                        
005400                             VALUE X'05'.                                 
005500*                                 HORIZTAB                                
005600*                                 HORIZTAB                                
005700     03 FI-LINE-IDARTNR-FINANCE                                           
005800                             PIC X(50)                                    
005900                             VALUE SPACES.                                
006000*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
006100*                                 UK                                      
006200*                                 PART NUMBER FOR FINANCIAL USE           
006300     03 FI-LINE-HORIZTAB     PIC X                                        
006400                             VALUE X'05'.                                 
006500*                                 HORIZTAB                                
006600*                                 HORIZTAB                                
006700     03 FI-LINE-BEART        PIC X(25)                                    
006800                             VALUE SPACES.                                
006900*                                 ARTIKELBENÄMNING                        
007000*                                 PART DESCRIPTION                        
007100     03 FI-LINE-HORIZTAB     PIC X                                        
007200                             VALUE X'05'.                                 
007300*                                 HORIZTAB                                
007400*                                 HORIZTAB                                
007500     03 FI-LINE-KVBEART      PIC Z(6)                                     
007600                             VALUE ZEROS.                                 
007700*                                 BESTÄLLT ANTAL STYCKEN                  
007800*                                 ORDERED QUANTITY                        
007900     03 FI-LINE-HORIZTAB     PIC X                                        
008000                             VALUE X'05'.                                 
008100*                                 HORIZTAB                                
008200*                                 HORIZTAB                                
008300     03 FI-LINE-KVLEVART     PIC Z(6)9                                    
008400                             VALUE ZEROS.                                 
008500*                                 LEVERERAT ANTAL STYCK                   
008600*                                 DELIVERED QUANTITY                      
008700     03 FI-LINE-HORIZTAB     PIC X                                        
008800                             VALUE X'05'.                                 
008900*                                 HORIZTAB                                
009000*                                 HORIZTAB                                
009100     03 FI-LINE-PRARTBTO     PIC Z(6)9.9(2)                               
009200                             VALUE ZEROS.                                 
009300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
009400*                                 GROSS SALES PRICE (SEK)                 
009500     03 FI-LINE-HORIZTAB     PIC X                                        
009600                             VALUE X'05'.                                 
009700*                                 HORIZTAB                                
009800*                                 HORIZTAB                                
009900     03 FI-LINE-PRARTNTO     PIC Z(6)9.9(2)                               
010000                             VALUE ZEROS.                                 
010100*                                 ARTIKELPRIS NETTO                       
010200*                                 NET PRICE EACH   (FOB NET)              
010300     03 FI-LINE-HORIZTAB     PIC X                                        
010400                             VALUE X'05'.                                 
010500*                                 HORIZTAB                                
010600*                                 HORIZTAB                                
010700     03 FI-LINE-REARTRAB     PIC Z9.9(2)                                  
010800                             VALUE ZEROS.                                 
010900*                                 ARTIKELRABATT                           
011000*                                 PARTS DISCOUNT PERCENT                  
011100     03 FI-LINE-HORIZTAB     PIC X                                        
011200                             VALUE X'05'.                                 
011300*                                 HORIZTAB                                
011400*                                 HORIZTAB                                
011500     03 FI-LINE-FLSPECPR     PIC X                                        
011600                             VALUE SPACE.                                 
011700*                                 SPECIALPRISFLAGGA                       
011800*                                 SPECIAL PRICE FLAG                      
011900     03 FI-LINE-HORIZTAB     PIC X                                        
012000                             VALUE X'05'.                                 
012100*                                 HORIZTAB                                
012200*                                 HORIZTAB                                
012300     03 FI-LINE-SUNTO        PIC Z(10)9.9(2)                              
012400                             VALUE ZEROS.                                 
012500*                                 TOTAL SALES AMOUNT EXCL. VAT            
012600     03 FI-LINE-HORIZTAB     PIC X                                        
012700                             VALUE X'05'.                                 
012800*                                 HORIZTAB                                
012900*                                 HORIZTAB                                
013000     03 FI-LINE-REVAT        PIC Z(2)9.9(2)                               
013100                             VALUE ZEROS.                                 
013200*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
013300*                                 VAT FACTOR                              
013400     03 FI-LINE-HORIZTAB     PIC X                                        
013500                             VALUE X'05'.                                 
013600*                                 HORIZTAB                                
013700*                                 HORIZTAB                                
013800     03 FI-LINE-SUVAT-BILLIT PIC Z(10)9.9(2)                              
013900                             VALUE ZEROS.                                 
014000*                                 SUMMERAT MOMSVÄRDE PER RAD              
014100*                                 TOTAL VAT VALUE PER LINE                
014200     03 FI-LINE-HORIZTAB     PIC X                                        
014300                             VALUE X'05'.                                 
014400*                                 HORIZTAB                                
014500*                                 HORIZTAB                                
014600     03 FI-LINE-SUBTO        PIC Z(10)9.9(2)                              
014700                             VALUE ZEROS.                                 
014800*                                 TOTAL SALES AMOUNT INCL. VAT            
014900     03 FI-LINE-HORIZTAB     PIC X                                        
015000                             VALUE X'05'.                                 
015100*                                 HORIZTAB                                
015200*                                 HORIZTAB                                
015300     03 FI-LINE-VKARTNTO     PIC Z(3)9.9(3)                               
015400                             VALUE ZEROS.                                 
015500*                                 ARTIKELVIKT NETTO (KG)                  
015600*                                 PART NET WEIGHT (KG)                    
015700     03 FI-LINE-HORIZTAB     PIC X                                        
015800                             VALUE X'05'.                                 
015900*                                 HORIZTAB                                
016000*                                 HORIZTAB                                
016100     03 FI-LINE-VKORDBTO-KOLLI                                            
016200                             PIC Z(5)9.9                                  
016300                             VALUE ZEROS.                                 
016400*                                 ORDERVIKT BRUTTO PER KOLLI              
016500*                                 ORDER WEIGHT GROSS PER CASE             
016600     03 FI-LINE-HORIZTAB     PIC X                                        
016700                             VALUE X'05'.                                 
016800*                                 HORIZTAB                                
016900*                                 HORIZTAB                                
017000     03 FI-LINE-KDARTURS     PIC X(2)                                     
017100                             VALUE SPACES.                                
017200*                                 ARTIKELURSPRUNGSKOD                     
017300*                                 COUNTRY OF ORIGIN                       
017400     03 FI-LINE-HORIZTAB     PIC X                                        
017500                             VALUE X'05'.                                 
017600*                                 HORIZTAB                                
017700*                                 HORIZTAB                                
017800     03 FI-LINE-IDSTATNR     PIC Z(9)                                     
017900                             VALUE ZEROS.                                 
018000*                                 STATISTISKT NUMMER                      
018100*                                 1 = NORSKT                              
018200*                                 2 = ENGELSKT                            
018300*                                 3 = BELGISKT                            
018400*                                 4 = PERUANSKT                           
018500*                                 5 = SVENSKT                             
018600*                                 6 =                                     
018700*                                 STATISTICAL NO.                         
018800     03 FI-LINE-HORIZTAB     PIC X                                        
018900                             VALUE X'05'.                                 
019000*                                 HORIZTAB                                
019100*                                 HORIZTAB                                
019200     03 FI-LINE-KDFRAKT      PIC Z9                                       
019300                             VALUE ZEROS.                                 
019400*                                 FRAKTSÄTT DC TILL KUND                  
019500*                                 FREIGHT CODE                            
019600     03 FI-LINE-HORIZTAB     PIC X                                        
019700                             VALUE X'05'.                                 
019800*                                 HORIZTAB                                
019900*                                 HORIZTAB                                
020000     03 FI-LINE-IDACCNT-1    PIC X(15)                                    
020100                             VALUE SPACES.                                
020200*                                 KONTOFÄLT                               
020300*                                 ACCOUNT FIELD                           
020400     03 FI-LINE-HORIZTAB     PIC X                                        
020500                             VALUE X'05'.                                 
020600*                                 HORIZTAB                                
020700*                                 HORIZTAB                                
020800     03 FI-LINE-IDACCNT-2    PIC X(15)                                    
020900                             VALUE SPACES.                                
021000*                                 KONTOFÄLT                               
021100*                                 ACCOUNT FIELD                           
021200     03 FI-LINE-HORIZTAB     PIC X                                        
021300                             VALUE X'05'.                                 
021400*                                 HORIZTAB                                
021500*                                 HORIZTAB                                
021600     03 FI-LINE-IDACCNT-3    PIC X(15)                                    
021700                             VALUE SPACES.                                
021800*                                 KONTOFÄLT                               
021900*                                 ACCOUNT FIELD                           
022000     03 FI-LINE-HORIZTAB     PIC X                                        
022100                             VALUE X'05'.                                 
022200*                                 HORIZTAB                                
022300*                                 HORIZTAB                                
022400     03 FI-LINE-KDANMORS     PIC X(2)                                     
022500                             VALUE SPACES.                                
022600*                                 ORSAK TILL LEVERANSANMÄRKNING           
022700*                                 DISCREPANCY REPORT REASON CODE          
022800     03 FI-LINE-HORIZTAB     PIC X                                        
022900                             VALUE X'05'.                                 
023000*                                 HORIZTAB                                
023100*                                 HORIZTAB                                
023200     03 FI-LINE-IDFAKREF     PIC Z(9)                                     
023300                             VALUE ZEROS.                                 
023400*                                 URSPRUNGLIGT FAKTURANUMMER              
023500*                                 ORIGINAL INVOICE NUMBER                 
023600     03 FI-LINE-HORIZTAB     PIC X                                        
023700                             VALUE X'05'.                                 
023800*                                 HORIZTAB                                
023900*                                 HORIZTAB                                
024000     03 FI-LINE-DAFAKREF     PIC Z(8)                                     
024100                             VALUE ZEROS.                                 
024200*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
024300*                                 MMDD)                                   
024400*                                 ORIGINAL INVOICING DATE (YYYYMM         
024500*                                 DD)                                     
024600     03 FI-LINE-HORIZTAB     PIC X                                        
024700                             VALUE X'05'.                                 
024800*                                 HORIZTAB                                
024900*                                 HORIZTAB                                
025000     03 FI-LINE-DAREFDAT     PIC 9(8)                                     
025100                             VALUE ZEROS.                                 
025200*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
025300*                                 REFERENCE DATE(YYYYMMDD)                
025400     03 FI-LINE-HORIZTAB     PIC X                                        
025500                             VALUE X'05'.                                 
025600*                                 HORIZTAB                                
025700*                                 HORIZTAB                                
025800     03 FI-LINE-IDDC         PIC X(2)                                     
025900                             VALUE SPACES.                                
026000*                                 IDENTIFIERARE LAGER                     
026100*                                 WAREHOUSE IDENTIFIER                    
026200     03 FI-LINE-HORIZTAB     PIC X                                        
026300                             VALUE X'05'.                                 
026400*                                 HORIZTAB                                
026500*                                 HORIZTAB                                
026600     03 FI-LINE-BELEVVIL     PIC X(35)                                    
026700                             VALUE SPACES.                                
026800*                                 LEVERANSVILLKOR                         
026900*                                 DELIVERY TERMS                          
027000     03 FI-LINE-HORIZTAB     PIC X                                        
027100                             VALUE X'05'.                                 
027200*                                 HORIZTAB                                
027300*                                 HORIZTAB                                
027400     03 FI-LINE-BEANST       PIC X(25)                                    
027500                             VALUE SPACES.                                
027600*                                 ANSTÄLLDS NAMN                          
027700*                                 NAME OF EMPLOYED                        
027800     03 FI-LINE-HORIZTAB     PIC X                                        
027900                             VALUE X'05'.                                 
028000*                                 HORIZTAB                                
028100*                                 HORIZTAB                                
028200     03 FI-LINE-IDUSER       PIC X(8)                                     
028300                             VALUE SPACES.                                
028400*                                 ANVÄNDARENS SÄKERHETS ID                
028500*                                 USER SECURITY-IDENTITY                  
028600     03 FI-LINE-HORIZTAB     PIC X                                        
028700                             VALUE X'05'.                                 
028800*                                 HORIZTAB                                
028900*                                 HORIZTAB                                
029000     03 FI-LINE-FLPCOO       PIC X                                        
029100                             VALUE SPACE.                                 
029200*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
029300*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
029400     03 FI-LINE-HORIZTAB     PIC X                                        
029500                             VALUE X'05'.                                 
029600*                                 HORIZTAB                                
029700*                                 HORIZTAB                                
029800     03 FI-LINE-IDOPTION-1   PIC X(15)                                    
029900                             VALUE SPACES.                                
030000*                                 BRYTBEGREPP                             
030100*                                 OPTIONAL ID                             
030200     03 FI-LINE-HORIZTAB     PIC X                                        
030300                             VALUE X'05'.                                 
030400*                                 HORIZTAB                                
030500*                                 HORIZTAB                                
030600     03 FI-LINE-IDOPTION-2   PIC X(15)                                    
030700                             VALUE SPACES.                                
030800*                                 BRYTBEGREPP                             
030900*                                 OPTIONAL ID                             
031000     03 FI-LINE-HORIZTAB     PIC X                                        
031100                             VALUE X'05'.                                 
031200*                                 HORIZTAB                                
031300*                                 HORIZTAB                                
031400     03 FI-LINE-IDOPTION-3   PIC X(15)                                    
031500                             VALUE SPACES.                                
031600*                                 BRYTBEGREPP                             
031700*                                 OPTIONAL ID                             
031800     03 FI-LINE-HORIZTAB     PIC X                                        
031900                             VALUE X'05'.                                 
032000*                                 HORIZTAB                                
032100*                                 HORIZTAB                                
032200     03 FI-LINE-IDOPTION-4   PIC X(15)                                    
032300                             VALUE SPACES.                                
032400*                                 BRYTBEGREPP                             
032500*                                 OPTIONAL ID                             
032600     03 FI-LINE-HORIZTAB     PIC X                                        
032700                             VALUE X'05'.                                 
032800*                                 HORIZTAB                                
032900*                                 HORIZTAB                                
033000     03 FI-LINE-IDOPTION-5   PIC X(15)                                    
033100                             VALUE SPACES.                                
033200*                                 BRYTBEGREPP                             
033300*                                 OPTIONAL ID                             
033400*** END OF VILMAII-COPY LENGTH= 539 BYTES                                 
