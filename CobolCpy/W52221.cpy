000100 01  W52221.                                                              
000200*                                 VAT DATA FOR DISTRIBUTION               
000300     03 TIAAAA               PIC 9(4)                                     
000400                             VALUE ZEROS.                                 
000500*                                 ÅRTAL (ÅÅÅÅ)                            
000600*                                 YEAR  (YYYY)                            
000700     03 TIRP                 PIC 9(2)                                     
000800                             VALUE ZEROS.                                 
000900*                                 REDOVISNINGSPERIOD                      
001000*                                 12 PER ÅR                               
001100*                                 ACCOUNTING PERIOD                       
001200*                                 12 PER YEAR                             
001300     03 SEMICOLON            PIC X                                        
001400                             VALUE ';'.                                   
001500*                                 SEMIKOLON                               
001600*                                 SEMICOLON                               
001700     03 IDLANDX3-SEND        PIC X(3)                                     
001800                             VALUE SPACES.                                
001900*                                 LANDKOD SÄNDANDE LAND                   
002000*                                 COUNTRY CODE SENDING COUNTRY            
002100     03 SEMICOLON            PIC X                                        
002200                             VALUE ';'.                                   
002300*                                 SEMIKOLON                               
002400*                                 SEMICOLON                               
002500     03 IDVAT-SEND           PIC X(17)                                    
002600                             VALUE SPACES.                                
002700*                                 MOMSREGISTRERINGSNUMMER                 
002800*                                 VAT REGISTRATION NUMBER                 
002900     03 SEMICOLON            PIC X                                        
003000                             VALUE ';'.                                   
003100*                                 SEMIKOLON                               
003200*                                 SEMICOLON                               
003300     03 IDLANDX3-BET         PIC X(3)                                     
003400                             VALUE SPACES.                                
003500*                                 LANDKOD BETALANDE KUND ETC              
003600*                                 COUNTRY CODE PAYING CUSTOMER ET         
003700*                                 C                                       
003800     03 SEMICOLON            PIC X                                        
003900                             VALUE ';'.                                   
004000*                                 SEMIKOLON                               
004100*                                 SEMICOLON                               
004200     03 IDVAT-REC            PIC X(17)                                    
004300                             VALUE SPACES.                                
004400*                                 MOMSREGISTRERINGSNUMMER                 
004500*                                 VAT REGISTRATION NUMBER                 
004600     03 SEMICOLON            PIC X                                        
004700                             VALUE ';'.                                   
004800*                                 SEMIKOLON                               
004900*                                 SEMICOLON                               
005000     03 IDDISTR              PIC 9(4)                                     
005100                             VALUE ZEROS.                                 
005200*                                 DISTRIKTNUMMER                          
005300*                                 DISTRICT NUMBER                         
005400     03 SEMICOLON            PIC X                                        
005500                             VALUE ';'.                                   
005600*                                 SEMIKOLON                               
005700*                                 SEMICOLON                               
005800     03 IDKUNDNR             PIC 9(6)                                     
005900                             VALUE ZEROS.                                 
006000*                                 KUNDNUMMER                              
006100*                                 CUSTOMER NO                             
006200     03 SEMICOLON            PIC X                                        
006300                             VALUE ';'.                                   
006400*                                 SEMIKOLON                               
006500*                                 SEMICOLON                               
006600     03 TIFAKT               PIC 9(6)                                     
006700                             VALUE ZEROS.                                 
006800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
006900*                                 INVOICING DATE   (YYMMDD)               
007000     03 SEMICOLON            PIC X                                        
007100                             VALUE ';'.                                   
007200*                                 SEMIKOLON                               
007300*                                 SEMICOLON                               
007400     03 IDFAKT               PIC 9(7)                                     
007500                             VALUE ZEROS.                                 
007600*                                 FAKTURANUMMER                           
007700*                                 INVOICE NO.                             
007800     03 SEMICOLON            PIC X                                        
007900                             VALUE ';'.                                   
008000*                                 SEMIKOLON                               
008100*                                 SEMICOLON                               
008200     03 SUFKTTOT-LOC         PIC +9(10)V9(2)                              
008300                             VALUE ZEROS.                                 
008400*                                 SUMMA FAKTURERAT BELOPP                 
008500*                                 TOTAL INVOICED AMOUNT                   
008600     03 SEMICOLON            PIC X                                        
008700                             VALUE ';'.                                   
008800*                                 SEMIKOLON                               
008900*                                 SEMICOLON                               
009000     03 SUVAT-FAKT-LOC       PIC +9(10)V9(2)                              
009100                             VALUE ZEROS.                                 
009200*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
009300*                                 EDITNOTA                                
009400*                                 TOTAL VAT VALUE PER INVOICE/CRE         
009500*                                 DIT                                     
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
010000*** END OF VILMAII-COPY LENGTH= 106 BYTES                                 
