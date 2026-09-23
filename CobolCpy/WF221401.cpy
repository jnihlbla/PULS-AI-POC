000100 01  DP-HEAD-WF2214.                                                      
000200*                                 EXCEL DATA HEADER - NSC                 
000300     03 DP-HEAD-IDPTYP       PIC X(6)                                     
000400                             VALUE SPACES.                                
000500*                                 POSTTYP              IDPTYP-006         
000600*                                 RECORD TYPE          IDPTYP-006         
000700     03 DP-HEAD-HORIZTAB     PIC X                                        
000800                             VALUE X'05'.                                 
000900*                                 HORIZTAB                                
001000*                                 HORIZTAB                                
001100     03 DP-HEAD-IDPARTNR     PIC X(9)                                     
001200                             VALUE SPACES.                                
001300*                                 PARTNERNUMMER                           
001400*                                 PARTNER NO                              
001500     03 DP-HEAD-HORIZTAB     PIC X                                        
001600                             VALUE X'05'.                                 
001700*                                 HORIZTAB                                
001800*                                 HORIZTAB                                
001900     03 DP-HEAD-IDLANDX2-BET PIC X(2)                                     
002000                             VALUE SPACES.                                
002100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002200*                                 2-LETTER CODE FOR COUNTRY               
002300     03 DP-HEAD-HORIZTAB     PIC X                                        
002400                             VALUE X'05'.                                 
002500*                                 HORIZTAB                                
002600*                                 HORIZTAB                                
002700     03 DP-HEAD-IDVAT-BET    PIC X(17)                                    
002800                             VALUE SPACES.                                
002900*                                 MOMSREGISTRERINGSNUMMER BETALAR         
003000*                                 E                                       
003100*                                 VAT REGISTRATION NUMBER PAYER           
003200     03 DP-HEAD-HORIZTAB     PIC X                                        
003300                             VALUE X'05'.                                 
003400*                                 HORIZTAB                                
003500*                                 HORIZTAB                                
003600     03 DP-HEAD-KVDAGAR      PIC 9(3)                                     
003700                             VALUE ZEROS.                                 
003800*                                 ANTAL DAGAR                             
003900     03 DP-HEAD-HORIZTAB     PIC X                                        
004000                             VALUE X'05'.                                 
004100*                                 HORIZTAB                                
004200*                                 HORIZTAB                                
004300     03 DP-HEAD-KDFINDOC     PIC X(4)                                     
004400                             VALUE SPACES.                                
004500*                                 TYP FINANSIELLT DOKUMENT                
004600*                                 FINANCIAL DOCUMENT TYPE                 
004700     03 DP-HEAD-HORIZTAB     PIC X                                        
004800                             VALUE X'05'.                                 
004900*                                 HORIZTAB                                
005000*                                 HORIZTAB                                
005100     03 DP-HEAD-IDFINDOC     PIC 9(9)                                     
005200                             VALUE ZEROS.                                 
005300*                                 FINANSIELLT DOKUMENT ID                 
005400*                                 FINANCIAL DOCUMENT ID                   
005500     03 DP-HEAD-HORIZTAB     PIC X                                        
005600                             VALUE X'05'.                                 
005700*                                 HORIZTAB                                
005800*                                 HORIZTAB                                
005900     03 DP-HEAD-DAFINDOC     PIC 9(8)                                     
006000                             VALUE ZEROS.                                 
006100*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
006200*                                 INVOICING DATE   (YYYYMMDD)             
006300     03 DP-HEAD-HORIZTAB     PIC X                                        
006400                             VALUE X'05'.                                 
006500*                                 HORIZTAB                                
006600*                                 HORIZTAB                                
006700     03 DP-HEAD-KDVALISO     PIC X(3)                                     
006800                             VALUE SPACES.                                
006900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007000*                                 CURRENCY CODE BY ISO-STANDARD.          
007100     03 DP-HEAD-HORIZTAB     PIC X                                        
007200                             VALUE X'05'.                                 
007300*                                 HORIZTAB                                
007400*                                 HORIZTAB                                
007500     03 DP-HEAD-SUNTO-TOT    PIC 9(11)V9(2)                               
007600                             VALUE ZEROS.                                 
007700*                                 TOTAL SALES AMOUNT EXCL. VAT            
007800     03 DP-HEAD-HORIZTAB     PIC X                                        
007900                             VALUE X'05'.                                 
008000*                                 HORIZTAB                                
008100*                                 HORIZTAB                                
008200     03 DP-HEAD-SUVAT-BILLIT-TOT                                          
008300                             PIC 9(11)V9(2)                               
008400                             VALUE ZEROS.                                 
008500*                                 SUMMERAT MOMSVÄRDE                      
008600*                                 TOTAL VAT VALUE                         
008700     03 DP-HEAD-HORIZTAB     PIC X                                        
008800                             VALUE X'05'.                                 
008900*                                 HORIZTAB                                
009000*                                 HORIZTAB                                
009100     03 DP-HEAD-SUBTO-TOT    PIC 9(11)V9(2)                               
009200                             VALUE ZEROS.                                 
009300*                                 TOTAL SALES AMOUNT INCL. VAT            
009400     03 DP-HEAD-HORIZTAB     PIC X                                        
009500                             VALUE X'05'.                                 
009600*                                 HORIZTAB                                
009700*                                 HORIZTAB                                
009800     03 DP-HEAD-IDVAT-LEG    PIC X(17)                                    
009900                             VALUE SPACES.                                
010000*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
010100*                                 ÄLJARE                                  
010200*                                 VAT REGISTRATION LEGAL PAYER            
010300     03 DP-HEAD-HORIZTAB     PIC X                                        
010400                             VALUE X'05'.                                 
010500*                                 HORIZTAB                                
010600*                                 HORIZTAB                                
010700*** END OF VILMAII-COPY LENGTH= 130 BYTES                                 
