000100 01  LINESW-WF029206.                                                     
000200*                                 MAIL-HEADER FOR PGM WF0292              
000300     03 LINESW-IDLEGSEL      PIC X(4)                                     
000400                             VALUE SPACES.                                
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 LINESW-SEMICOLON     PIC X                                        
000800                             VALUE ';'.                                   
000900*                                 SEMIKOLON                               
001000*                                 SEMICOLON                               
001100     03 LINESW-BETEXT        PIC X(20)                                    
001200                             VALUE SPACES.                                
001300     03 LINESW-SEMICOLON     PIC X                                        
001400                             VALUE ';'.                                   
001500*                                 SEMIKOLON                               
001600*                                 SEMICOLON                               
001700     03 LINESW-DAREGDAT      PIC Z(8)                                     
001800                             VALUE ZEROS.                                 
001900*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002000*                                 REGISTRATION DATE (YYYYMMDD)            
002100     03 LINESW-SEMICOLON     PIC X                                        
002200                             VALUE ';'.                                   
002300*                                 SEMIKOLON                               
002400*                                 SEMICOLON                               
002500     03 LINESW-IDFINDOC      PIC Z(8)9                                    
002600                             VALUE ZEROS.                                 
002700*                                 FINANSIELLT DOKUMENT ID                 
002800*                                 FINANCIAL DOCUMENT ID                   
002900     03 LINESW-SEMICOLON     PIC X                                        
003000                             VALUE ';'.                                   
003100*                                 SEMIKOLON                               
003200*                                 SEMICOLON                               
003300     03 LINESW-IDARTNR       PIC Z(8)9                                    
003400                             VALUE ZEROS.                                 
003500*                                 ARTIKELNUMMER                           
003600*                                 PART NUMBER                             
003700     03 LINESW-SEMICOLON     PIC X                                        
003800                             VALUE ';'.                                   
003900*                                 SEMIKOLON                               
004000*                                 SEMICOLON                               
004100     03 LINESW-BEART         PIC X(25)                                    
004200                             VALUE SPACES.                                
004300*                                 ARTIKELBENƒMNING                        
004400*                                 PART DESCRIPTION                        
004500     03 LINESW-SEMICOLON     PIC X                                        
004600                             VALUE ';'.                                   
004700*                                 SEMIKOLON                               
004800*                                 SEMICOLON                               
004900     03 LINESW-IDPARTNR      PIC X(9)                                     
005000                             VALUE SPACES.                                
005100*                                 PARTNERNUMMER                           
005200*                                 PARTNER NO                              
005300     03 LINESW-SEMICOLON     PIC X                                        
005400                             VALUE ';'.                                   
005500*                                 SEMIKOLON                               
005600*                                 SEMICOLON                               
005700     03 LINESW-IDEXCUST-1    PIC X(15)                                    
005800                             VALUE SPACES.                                
005900*                                 EXTERNT KUNDID                          
006000*                                 EXTERNAL CUSTOMER ID                    
006100     03 LINESW-SEMICOLON     PIC X                                        
006200                             VALUE ';'.                                   
006300*                                 SEMIKOLON                               
006400*                                 SEMICOLON                               
006500     03 LINESW-IDEXCUST-2    PIC X(15)                                    
006600                             VALUE SPACES.                                
006700*                                 EXTERNT KUNDID                          
006800*                                 EXTERNAL CUSTOMER ID                    
006900     03 LINESW-SEMICOLON     PIC X                                        
007000                             VALUE ';'.                                   
007100*                                 SEMIKOLON                               
007200*                                 SEMICOLON                               
007300     03 LINESW-IDREF         PIC X(15)                                    
007400                             VALUE SPACES.                                
007500*                                 REFERENS ID                             
007600*                                 REFERENCE ID                            
007700     03 LINESW-SEMICOLON     PIC X                                        
007800                             VALUE ';'.                                   
007900*                                 SEMIKOLON                               
008000*                                 SEMICOLON                               
008100     03 LINESW-KDFINDOC      PIC X(4)                                     
008200                             VALUE SPACES.                                
008300*                                 TYP FINANSIELLT DOKUMENT                
008400*                                 FINANCIAL DOCUMENT TYPE                 
008500     03 LINESW-SEMICOLON     PIC X                                        
008600                             VALUE ';'.                                   
008700*                                 SEMIKOLON                               
008800*                                 SEMICOLON                               
008900     03 LINESW-FLSOFT        PIC X                                        
009000                             VALUE SPACE.                                 
009100*                                 FLAGGA SOFTVARA                         
009200*                                 SOFTWARE MARK                           
009300     03 LINESW-SEMICOLON     PIC X                                        
009400                             VALUE ';'.                                   
009500*                                 SEMIKOLON                               
009600*                                 SEMICOLON                               
009700     03 LINESW-FLFREE        PIC X                                        
009800                             VALUE SPACE.                                 
009900*                                 GRATISFATURA                            
010000*                                 FREE INVOICE                            
010100     03 LINESW-SEMICOLON     PIC X                                        
010200                             VALUE ';'.                                   
010300*                                 SEMIKOLON                               
010400*                                 SEMICOLON                               
010500     03 LINESW-REARTRAB      PIC Z9.9(2)                                  
010600                             VALUE ZEROS.                                 
010700*                                 ARTIKELRABATT                           
010800*                                 PARTS DISCOUNT PERCENT                  
010900     03 LINESW-SEMICOLON     PIC X                                        
011000                             VALUE ';'.                                   
011100*                                 SEMIKOLON                               
011200*                                 SEMICOLON                               
011300     03 LINESW-PRARTNTO-SEK  PIC Z(6)9.9(2)                               
011400                             VALUE ZEROS.                                 
011500*                                 ARTIKELPRIS NETTO                       
011600*                                 NET PRICE EACH   (FOB NET)              
011700     03 LINESW-SEMICOLON     PIC X                                        
011800                             VALUE ';'.                                   
011900*                                 SEMIKOLON                               
012000*                                 SEMICOLON                               
012100     03 LINESW-SUNTO-SEK     PIC Z(10)9.9(2)                              
012200                             VALUE ZEROS.                                 
012300*                                 TOTAL SALES AMOUNT EXCL. VAT            
012400     03 LINESW-SEMICOLON     PIC X                                        
012500                             VALUE ';'.                                   
012600*                                 SEMIKOLON                               
012700*                                 SEMICOLON                               
012800     03 LINESW-KDVALISO      PIC X(3)                                     
012900                             VALUE SPACES.                                
013000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013100*                                 CURRENCY CODE BY ISO-STANDARD.          
013200     03 LINESW-SEMICOLON     PIC X                                        
013300                             VALUE ';'.                                   
013400*                                 SEMIKOLON                               
013500*                                 SEMICOLON                               
013600     03 LINESW-PRARTNTO      PIC Z(6)9.9(2)                               
013700                             VALUE ZEROS.                                 
013800*                                 ARTIKELPRIS NETTO                       
013900*                                 NET PRICE EACH   (FOB NET)              
014000     03 LINESW-SEMICOLON     PIC X                                        
014100                             VALUE ';'.                                   
014200*                                 SEMIKOLON                               
014300*                                 SEMICOLON                               
014400     03 LINESW-SUNTO         PIC Z(10)9.9(2)                              
014500                             VALUE ZEROS.                                 
014600*                                 TOTAL SALES AMOUNT EXCL. VAT            
014700     03 LINESW-SEMICOLON     PIC X                                        
014800                             VALUE ';'.                                   
014900*                                 SEMIKOLON                               
015000*                                 SEMICOLON                               
015100     03 LINESW-KVLEVART      PIC Z(6)9                                    
015200                             VALUE ZEROS.                                 
015300*                                 LEVERERAT ANTAL STYCK                   
015400*                                 DELIVERED QUANTITY                      
015500     03 LINESW-SEMICOLON     PIC X                                        
015600                             VALUE ';'.                                   
015700*                                 SEMIKOLON                               
015800*                                 SEMICOLON                               
015900     03 LINESW-IDSTATNR      PIC Z(8)9                                    
016000                             VALUE ZEROS.                                 
016100*                                 STATISTISKT NUMMER                      
016200*                                 1 = NORSKT                              
016300*                                 2 = ENGELSKT                            
016400*                                 3 = BELGISKT                            
016500*                                 4 = PERUANSKT                           
016600*                                 5 = SVENSKT                             
016700*                                 6 =                                     
016800*                                 STATISTICAL NO.                         
016900     03 LINESW-SEMICOLON     PIC X                                        
017000                             VALUE ';'.                                   
017100*                                 SEMIKOLON                               
017200*                                 SEMICOLON                               
017300     03 LINESW-IDLEVNR       PIC X(5)                                     
017400                             VALUE SPACES.                                
017500*                                 LEVERANT÷RNUMMER                        
017600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
017700     03 LINESW-SEMICOLON     PIC X                                        
017800                             VALUE ';'.                                   
017900*                                 SEMIKOLON                               
018000*                                 SEMICOLON                               
018100     03 LINESW-DAFINDOC      PIC 9(8)                                     
018200                             VALUE ZEROS.                                 
018300*                                 DOKUMENT DATUM (≈≈≈≈MMDD)               
018400*                                 INVOICING DATE   (YYYYMMDD)             
018500*** END OF VILMAII-COPY LENGTH= 242 BYTES                                 
