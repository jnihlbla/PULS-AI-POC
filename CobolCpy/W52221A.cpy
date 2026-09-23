000100 01  W52221A.                                                             
000200*                                 VAT DATA FOR DISTRIBUTION               
000300     03 TIAAAA               PIC 9(4)                                     
000400                             VALUE ZEROS.                                 
000500*                                 ÅRTAL (ÅÅÅÅ)                            
000600*                                 YEAR  (YYYY)                            
000700     03 SEMICOLON            PIC X                                        
000800                             VALUE ';'.                                   
000900*                                 SEMIKOLON                               
001000*                                 SEMICOLON                               
001100     03 TIRP                 PIC 9(2)                                     
001200                             VALUE ZEROS.                                 
001300*                                 REDOVISNINGSPERIOD                      
001400*                                 12 PER ÅR                               
001500*                                 ACCOUNTING PERIOD                       
001600*                                 12 PER YEAR                             
001700     03 SEMICOLON            PIC X                                        
001800                             VALUE ';'.                                   
001900*                                 SEMIKOLON                               
002000*                                 SEMICOLON                               
002100     03 IDLANDX3-SEND        PIC X(3)                                     
002200                             VALUE SPACES.                                
002300*                                 LANDKOD SÄNDANDE LAND                   
002400*                                 COUNTRY CODE SENDING COUNTRY            
002500     03 SEMICOLON            PIC X                                        
002600                             VALUE ';'.                                   
002700*                                 SEMIKOLON                               
002800*                                 SEMICOLON                               
002900     03 IDVAT-SEND           PIC X(17)                                    
003000                             VALUE SPACES.                                
003100*                                 MOMSREGISTRERINGSNUMMER                 
003200*                                 VAT REGISTRATION NUMBER                 
003300     03 SEMICOLON            PIC X                                        
003400                             VALUE ';'.                                   
003500*                                 SEMIKOLON                               
003600*                                 SEMICOLON                               
003700     03 IDLANDX3-BET         PIC X(3)                                     
003800                             VALUE SPACES.                                
003900*                                 LANDKOD BETALANDE KUND ETC              
004000*                                 COUNTRY CODE PAYING CUSTOMER ET         
004100*                                 C                                       
004200     03 SEMICOLON            PIC X                                        
004300                             VALUE ';'.                                   
004400*                                 SEMIKOLON                               
004500*                                 SEMICOLON                               
004600     03 IDVAT-REC            PIC X(17)                                    
004700                             VALUE SPACES.                                
004800*                                 MOMSREGISTRERINGSNUMMER                 
004900*                                 VAT REGISTRATION NUMBER                 
005000     03 SEMICOLON            PIC X                                        
005100                             VALUE ';'.                                   
005200*                                 SEMIKOLON                               
005300*                                 SEMICOLON                               
005400     03 IDDISTR              PIC 9(4)                                     
005500                             VALUE ZEROS.                                 
005600*                                 DISTRIKTNUMMER                          
005700*                                 DISTRICT NUMBER                         
005800     03 SEMICOLON            PIC X                                        
005900                             VALUE ';'.                                   
006000*                                 SEMIKOLON                               
006100*                                 SEMICOLON                               
006200     03 IDKUNDNR             PIC 9(6)                                     
006300                             VALUE ZEROS.                                 
006400*                                 KUNDNUMMER                              
006500*                                 CUSTOMER NO                             
006600     03 SEMICOLON            PIC X                                        
006700                             VALUE ';'.                                   
006800*                                 SEMIKOLON                               
006900*                                 SEMICOLON                               
007000     03 TIFAKT               PIC 9(6)                                     
007100                             VALUE ZEROS.                                 
007200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
007300*                                 INVOICING DATE   (YYMMDD)               
007400     03 SEMICOLON            PIC X                                        
007500                             VALUE ';'.                                   
007600*                                 SEMIKOLON                               
007700*                                 SEMICOLON                               
007800     03 IDFAKT               PIC 9(7)                                     
007900                             VALUE ZEROS.                                 
008000*                                 FAKTURANUMMER                           
008100*                                 INVOICE NO.                             
008200     03 SEMICOLON            PIC X                                        
008300                             VALUE ';'.                                   
008400*                                 SEMIKOLON                               
008500*                                 SEMICOLON                               
008600     03 SUFKTTOT-LOC         PIC -(10)9.9(2)                              
008700                             VALUE ZEROS.                                 
008800*                                 SUMMA FAKTURERAT BELOPP                 
008900*                                 TOTAL INVOICED AMOUNT                   
009000     03 SEMICOLON            PIC X                                        
009100                             VALUE ';'.                                   
009200*                                 SEMIKOLON                               
009300*                                 SEMICOLON                               
009400     03 SUVAT-FAKT-LOC       PIC -(10)9.9(2)                              
009500                             VALUE ZEROS.                                 
009600*                                 MOMSVÄRDE PER MOMSKOD                   
009700*                                 VAT VALUE PER VAT CODE                  
009800     03 FILLER               PIC X(150)                                   
009900                             VALUE SPACES.                                
010000*** END OF VILMAII-COPY LENGTH= 258 BYTES                                 
