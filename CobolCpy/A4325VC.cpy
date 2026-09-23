000100 01  A4325VC.                                                             
000200*                                 FELSKICK I DISPLAYFORMAT                
000300     03 IDPTYP               PIC X(3)                                     
000400                             VALUE SPACES.                                
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 SEMICOLON-1          PIC X                                        
000800                             VALUE ';'.                                   
000900*                                 SEMIKOLON                               
001000*                                 SEMICOLON                               
001100     03 IDLOPNRM             PIC Z(7)9                                    
001200                             VALUE ZEROS.                                 
001300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001400*                                 (0VVDLLLLK)                             
001500*                                 SERIAL NO RECEIVING REPORT              
001600*                                 (0WWDLLLLC)                             
001700     03 SEMICOLON-2          PIC X                                        
001800                             VALUE ';'.                                   
001900*                                 SEMIKOLON                               
002000*                                 SEMICOLON                               
002100     03 IDARTNR              PIC Z(8)9                                    
002200                             VALUE ZEROS.                                 
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 SEMICOLON-3          PIC X                                        
002600                             VALUE ';'.                                   
002700*                                 SEMIKOLON                               
002800*                                 SEMICOLON                               
002900     03 IDLEVNR              PIC X(6)                                     
003000                             VALUE SPACES.                                
003100*                                 LEVERANTÖRNUMMER                        
003200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003300     03 SEMICOLON-4          PIC X                                        
003400                             VALUE ';'.                                   
003500*                                 SEMIKOLON                               
003600*                                 SEMICOLON                               
003700     03 BESTPREF             PIC Z(2)9                                    
003800                             VALUE ZEROS.                                 
003900     03 SEMICOLON-5          PIC X                                        
004000                             VALUE ';'.                                   
004100*                                 SEMIKOLON                               
004200*                                 SEMICOLON                               
004300     03 BESTLOPNR            PIC Z(5)9                                    
004400                             VALUE ZEROS.                                 
004500     03 SEMICOLON-6          PIC X                                        
004600                             VALUE ';'.                                   
004700*                                 SEMIKOLON                               
004800*                                 SEMICOLON                               
004900     03 BESTSUFF             PIC Z(2)9                                    
005000                             VALUE ZEROS.                                 
005100     03 SEMICOLON-7          PIC X                                        
005200                             VALUE ';'.                                   
005300*                                 SEMIKOLON                               
005400*                                 SEMICOLON                               
005500     03 PACKNR               PIC Z(5)9                                    
005600                             VALUE ZEROS.                                 
005700     03 SEMICOLON-8          PIC X                                        
005800                             VALUE ';'.                                   
005900*                                 SEMIKOLON                               
006000*                                 SEMICOLON                               
006100     03 DATUM-AVS            PIC X(8)                                     
006200                             VALUE SPACES.                                
006300     03 SEMICOLON-9          PIC X                                        
006400                             VALUE ';'.                                   
006500*                                 SEMIKOLON                               
006600*                                 SEMICOLON                               
006700     03 KVAVIS               PIC -(7)9                                    
006800                             VALUE ZEROS.                                 
006900*                                 AVISERAT ANTAL                          
007000*                                 QUANTITY NOTIFIED                       
007100     03 SEMICOLON-10         PIC X                                        
007200                             VALUE ';'.                                   
007300*                                 SEMIKOLON                               
007400*                                 SEMICOLON                               
007500     03 KDSORT               PIC X(2)                                     
007600                             VALUE SPACES.                                
007700*                                 SORT-KOD                                
007800*                                 UNIT OF MEASURE                         
007900     03 SEMICOLON-11         PIC X                                        
008000                             VALUE ';'.                                   
008100*                                 SEMIKOLON                               
008200*                                 SEMICOLON                               
008300     03 PRARTBES             PIC -(7)9.9(2)                               
008400                             VALUE ZEROS.                                 
008500*                                 BESTÄLLNINGSPRIS I KRONOR               
008600*                                 ORDER PRICE SWEDISH CURRENCY            
008700     03 SEMICOLON-12         PIC X                                        
008800                             VALUE ';'.                                   
008900*                                 SEMIKOLON                               
009000*                                 SEMICOLON                               
009100     03 PRARTBEL-PR          PIC Z(7)9.9(3)                               
009200                             VALUE ZEROS.                                 
009300*                                 DETTA BESTÄLLNINGSPRIS                  
009400*                                 (I LEVERANTÖRENS VALUTA)                
009500     03 SEMICOLON-13         PIC X                                        
009600                             VALUE ';'.                                   
009700*                                 SEMIKOLON                               
009800*                                 SEMICOLON                               
009900     03 KDANTENH             PIC X                                        
010000                             VALUE SPACE.                                 
010100*                                 KOD ANTAL PER ARTIKEL                   
010200*                                 CODE FOR PART NUMBERS PER LINE          
010300     03 SEMICOLON-14         PIC X                                        
010400                             VALUE ';'.                                   
010500*                                 SEMIKOLON                               
010600*                                 SEMICOLON                               
010700     03 TIPPAT               PIC 9                                        
010800                             VALUE ZERO.                                  
010900     03 SEMICOLON-15         PIC X                                        
011000                             VALUE ';'.                                   
011100*                                 SEMIKOLON                               
011200*                                 SEMICOLON                               
011300     03 PRARTSTD             PIC -(7)9.9(2)                               
011400                             VALUE ZEROS.                                 
011500*                                 ARTIKELSTANDARDPRIS                     
011600*                                 STANDARD PRICE                          
011700     03 SEMICOLON-16         PIC X                                        
011800                             VALUE ';'.                                   
011900*                                 SEMIKOLON                               
012000*                                 SEMICOLON                               
012100     03 BEL-BEST             PIC -(7)9.9(2)                               
012200                             VALUE ZEROS.                                 
012300     03 SEMICOLON-17         PIC X                                        
012400                             VALUE ';'.                                   
012500*                                 SEMIKOLON                               
012600*                                 SEMICOLON                               
012700     03 HKTO                 PIC X(10)                                    
012800                             VALUE SPACES.                                
012900     03 SEMICOLON-18         PIC X                                        
013000                             VALUE ';'.                                   
013100*                                 SEMIKOLON                               
013200*                                 SEMICOLON                               
013300     03 UKTO-DD              PIC X(2)                                     
013400                             VALUE SPACES.                                
013500     03 SEMICOLON-19         PIC X                                        
013600                             VALUE ';'.                                   
013700*                                 SEMIKOLON                               
013800*                                 SEMICOLON                               
013900     03 UKTO-DC              PIC X(2)                                     
014000                             VALUE SPACES.                                
014100     03 SEMICOLON-20         PIC X                                        
014200                             VALUE ';'.                                   
014300*                                 SEMIKOLON                               
014400*                                 SEMICOLON                               
014500     03 IDFTG                PIC X(2)                                     
014600                             VALUE SPACES.                                
014700*                                 FÖRETAGSID EKONOM REDOVISNING           
014800*                                 COMPANY IDENTITY ACCOUNTING             
014900     03 SEMICOLON-21         PIC X                                        
015000                             VALUE ';'.                                   
015100*                                 SEMIKOLON                               
015200*                                 SEMICOLON                               
015300     03 PRODKOD              PIC Z(3)9                                    
015400                             VALUE ZEROS.                                 
015500     03 SEMICOLON-22         PIC X                                        
015600                             VALUE ';'.                                   
015700*                                 SEMIKOLON                               
015800*                                 SEMICOLON                               
015900     03 SYSTKOD              PIC X(2)                                     
016000                             VALUE SPACES.                                
016100     03 SEMICOLON-23         PIC X                                        
016200                             VALUE ';'.                                   
016300*                                 SEMIKOLON                               
016400*                                 SEMICOLON                               
016500     03 KDVALISO             PIC X(3)                                     
016600                             VALUE SPACES.                                
016700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
016800*                                 CURRENCY CODE BY ISO-STANDARD.          
016900     03 SEMICOLON-24         PIC X                                        
017000                             VALUE ';'.                                   
017100*                                 SEMIKOLON                               
017200*                                 SEMICOLON                               
017300     03 TULLKURS             PIC Z(2)9.9(3)                               
017400                             VALUE ZEROS.                                 
017500     03 SEMICOLON-25         PIC X                                        
017600                             VALUE ';'.                                   
017700*                                 SEMIKOLON                               
017800*                                 SEMICOLON                               
017900     03 TULLFAKT             PIC 9.9(4)                                   
018000                             VALUE ZEROS.                                 
018100     03 SEMICOLON-26         PIC X                                        
018200                             VALUE ';'.                                   
018300*                                 SEMIKOLON                               
018400*                                 SEMICOLON                               
018500     03 KDINLAVV             PIC X                                        
018600                             VALUE SPACE.                                 
018700*                                 TYP AV AVVIKELSE                        
018800*                                 TYPE OF DEVIATION                       
018900     03 SEMICOLON-27         PIC X                                        
019000                             VALUE ';'.                                   
019100*                                 SEMIKOLON                               
019200*                                 SEMICOLON                               
019300     03 IDPRCTR              PIC X(10)                                    
019400                             VALUE SPACES.                                
019500*                                 PROFIT CENTER                           
019600*                                 PROFIT CENTER                           
019700     03 SEMICOLON-28         PIC X                                        
019800                             VALUE ';'.                                   
019900*                                 SEMIKOLON                               
020000*                                 SEMICOLON                               
020100     03 COSTCTR-FREIGHT      PIC X(12)                                    
020200                             VALUE SPACES.                                
020300     03 SEMICOLON-29         PIC X                                        
020400                             VALUE ';'.                                   
020500*                                 SEMIKOLON                               
020600*                                 SEMICOLON                               
020700     03 FLAGGA1F             PIC X                                        
020800                             VALUE SPACE.                                 
020900     03 SEMICOLON-30         PIC X                                        
021000                             VALUE ';'.                                   
021100*                                 SEMIKOLON                               
021200*                                 SEMICOLON                               
021300     03 COSTCTR-PRICE-DIFF   PIC X(12)                                    
021400                             VALUE SPACES.                                
021500     03 SEMICOLON-31         PIC X                                        
021600                             VALUE ';'.                                   
021700*                                 SEMIKOLON                               
021800*                                 SEMICOLON                               
021900     03 FLAGGA2D             PIC X                                        
022000                             VALUE SPACE.                                 
022100     03 SEMICOLON-32         PIC X                                        
022200                             VALUE ';'.                                   
022300*                                 SEMIKOLON                               
022400*                                 SEMICOLON                               
022500     03 COSTCTR-EXCH-DIFF    PIC X(12)                                    
022600                             VALUE SPACES.                                
022700     03 SEMICOLON-33         PIC X                                        
022800                             VALUE ';'.                                   
022900*                                 SEMIKOLON                               
023000*                                 SEMICOLON                               
023100     03 FLAGGA3VD            PIC X                                        
023200                             VALUE SPACE.                                 
023300     03 SEMICOLON-34         PIC X                                        
023400                             VALUE ';'.                                   
023500*                                 SEMIKOLON                               
023600*                                 SEMICOLON                               
023700     03 IDDISTR              PIC Z(3)9                                    
023800                             VALUE ZEROS.                                 
023900*                                 DISTRIKTNUMMER                          
024000*                                 DISTRICT NUMBER                         
024100     03 SEMICOLON-35         PIC X                                        
024200                             VALUE ';'.                                   
024300*                                 SEMIKOLON                               
024400*                                 SEMICOLON                               
024500     03 LOPNRFR1             PIC Z(6)9                                    
024600                             VALUE ZEROS.                                 
024700     03 SEMICOLON-36         PIC X                                        
024800                             VALUE ';'.                                   
024900*                                 SEMIKOLON                               
025000*                                 SEMICOLON                               
025100     03 LOPNRTM1             PIC Z(6)9                                    
025200                             VALUE ZEROS.                                 
025300     03 SEMICOLON-37         PIC X                                        
025400                             VALUE ';'.                                   
025500*                                 SEMIKOLON                               
025600*                                 SEMICOLON                               
025700     03 LOPNRFR2             PIC Z(6)9                                    
025800                             VALUE ZEROS.                                 
025900     03 SEMICOLON-38         PIC X                                        
026000                             VALUE ';'.                                   
026100*                                 SEMIKOLON                               
026200*                                 SEMICOLON                               
026300     03 LOPNRTM2             PIC Z(6)9                                    
026400                             VALUE ZEROS.                                 
026500     03 SEMICOLON-39         PIC X                                        
026600                             VALUE ';'.                                   
026700*                                 SEMIKOLON                               
026800*                                 SEMICOLON                               
026900     03 FLAVVINL             PIC X                                        
027000                             VALUE SPACE.                                 
027100*                                 AVVIKELSE FÖR INLEVERANS                
027200*                                 DEVIATION INBOUND                       
027300     03 SEMICOLON-40         PIC X                                        
027400                             VALUE ';'.                                   
027500*                                 SEMIKOLON                               
027600*                                 SEMICOLON                               
027700     03 PACKNRTOT            PIC Z(10)9                                   
027800                             VALUE ZEROS.                                 
027900     03 SEMICOLON-41         PIC X                                        
028000                             VALUE ';'.                                   
028100*                                 SEMIKOLON                               
028200*                                 SEMICOLON                               
028300     03 IDTECKEN             PIC X                                        
028400                             VALUE SPACE.                                 
028500*                                 TECKEN                                  
028600*                                 SIGN                                    
028700     03 FILLER               PIC X(350)                                   
028800                             VALUE SPACES.                                
028900*** END OF VILMAII-COPY LENGTH= 633 BYTES                                 
