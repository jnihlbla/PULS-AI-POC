000100 01  W55375.                                                              
000200*                                 COPYTEXT FÖR WARNINGS LIST              
000300     03 IDARTNR              PIC Z(8)9                                    
000400                             VALUE ZEROS.                                 
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 SEMICOLON            PIC X                                        
000800                             VALUE ';'.                                   
000900*                                 SEMIKOLON                               
001000*                                 SEMICOLON                               
001100     03 BETEXT               PIC X(40)                                    
001200                             VALUE SPACES.                                
001300     03 SEMICOLON            PIC X                                        
001400                             VALUE ';'.                                   
001500*                                 SEMIKOLON                               
001600*                                 SEMICOLON                               
001700     03 BEART                PIC X(15)                                    
001800                             VALUE SPACES.                                
001900*                                 ARTIKELBENÄMNING      BEART-002         
002000     03 SEMICOLON            PIC X                                        
002100                             VALUE ';'.                                   
002200*                                 SEMIKOLON                               
002300*                                 SEMICOLON                               
002400     03 IDLEVNR              PIC X(5)                                     
002500                             VALUE SPACES.                                
002600*                                 LEVERANTÖRNUMMER                        
002700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002800     03 SEMICOLON            PIC X                                        
002900                             VALUE ';'.                                   
003000*                                 SEMIKOLON                               
003100*                                 SEMICOLON                               
003200     03 PRARTBEL             PIC Z(7)9.9(5)                               
003300                             VALUE ZEROS.                                 
003400*                                 BESTPRIS LEVERANTÖRENS VALUTA           
003500*                                 ORDER PRICE SUPL.CUR                    
003600     03 SEMICOLON            PIC X                                        
003700                             VALUE ';'.                                   
003800*                                 SEMIKOLON                               
003900*                                 SEMICOLON                               
004000     03 KDVALISO             PIC X(3)                                     
004100                             VALUE SPACES.                                
004200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004300*                                 CURRENCY CODE BY ISO-STANDARD.          
004400     03 SEMICOLON            PIC X                                        
004500                             VALUE ';'.                                   
004600*                                 SEMIKOLON                               
004700*                                 SEMICOLON                               
004800     03 TIPRLIST             PIC X(6)                                     
004900                             VALUE SPACES.                                
005000*                                 PRISLISTEDATUM (AAMMDD)                 
005100     03 SEMICOLON            PIC X                                        
005200                             VALUE ';'.                                   
005300*                                 SEMIKOLON                               
005400*                                 SEMICOLON                               
005500     03 PRARTBES             PIC Z(6)9.9(2)                               
005600                             VALUE ZEROS.                                 
005700*                                 BESTÄLLNINGSPRIS I KRONOR               
005800*                                 ORDER PRICE SWEDISH CURRENCY            
005900     03 SEMICOLON            PIC X                                        
006000                             VALUE ';'.                                   
006100*                                 SEMIKOLON                               
006200*                                 SEMICOLON                               
006300     03 KVPB                 PIC Z(5)9.9                                  
006400                             VALUE ZEROS.                                 
006500*                                 PERIODBEHOV (PROGNOS)                   
006600*                                 PERIOD REQUIREMENTS                     
006700     03 SEMICOLON            PIC X                                        
006800                             VALUE ';'.                                   
006900*                                 SEMIKOLON                               
007000*                                 SEMICOLON                               
007100     03 KVLS                 PIC Z(6)9                                    
007200                             VALUE ZEROS.                                 
007300*                                 LAGERSALDO                              
007400*                                 STOCK BALANCE                           
007500     03 SEMICOLON            PIC X                                        
007600                             VALUE ';'.                                   
007700*                                 SEMIKOLON                               
007800*                                 SEMICOLON                               
007900     03 IDDC                 PIC X(2)                                     
008000                             VALUE SPACES.                                
008100*                                 IDENTIFIERARE LAGER                     
008200*                                 WAREHOUSE IDENTIFIER                    
008300     03 SEMICOLON            PIC X                                        
008400                             VALUE ';'.                                   
008500*                                 SEMIKOLON                               
008600*                                 SEMICOLON                               
008700     03 IDNAMN               PIC X(40)                                    
008800                             VALUE SPACES.                                
008900*                                 NAMN                                    
009000     03 SEMICOLON            PIC X                                        
009100                             VALUE ';'.                                   
009200*                                 SEMIKOLON                               
009300*                                 SEMICOLON                               
009400     03 IDINK                PIC Z(2)9                                    
009500                             VALUE ZEROS.                                 
009600*                                 INKÖPARNUMMER                           
009700*                                 PURCHASE IDENTIFICATION NUMBER          
009800     03 SEMICOLON            PIC X                                        
009900                             VALUE ';'.                                   
010000*                                 SEMIKOLON                               
010100*                                 SEMICOLON                               
010200     03 IDMAIL               PIC X(60)                                    
010300                             VALUE SPACES.                                
010400*                                 MAIL ADRESS                             
010500*                                 MAIL ADDRESS                            
010600*** END OF VILMAII-COPY LENGTH= 235 BYTES                                 
