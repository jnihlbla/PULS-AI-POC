000100 01  DP-LINE-WF2214.                                                      
000200*                                 EXCEL DATA LINE - NSC                   
000300     03 DP-LINE-IDPTYP       PIC X(6)                                     
000400                             VALUE SPACES.                                
000500*                                 POSTTYP              IDPTYP-006         
000600*                                 RECORD TYPE          IDPTYP-006         
000700     03 DP-LINE-HORIZTAB     PIC X                                        
000800                             VALUE X'05'.                                 
000900*                                 HORIZTAB                                
001000*                                 HORIZTAB                                
001100     03 DP-LINE-IDPARTNR     PIC X(9)                                     
001200                             VALUE SPACES.                                
001300*                                 PARTNERNUMMER                           
001400*                                 PARTNER NO                              
001500     03 DP-LINE-HORIZTAB     PIC X                                        
001600                             VALUE X'05'.                                 
001700*                                 HORIZTAB                                
001800*                                 HORIZTAB                                
001900     03 DP-LINE-IDLANDX2-BET PIC X(2)                                     
002000                             VALUE SPACES.                                
002100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002200*                                 2-LETTER CODE FOR COUNTRY               
002300     03 DP-LINE-HORIZTAB     PIC X                                        
002400                             VALUE X'05'.                                 
002500*                                 HORIZTAB                                
002600*                                 HORIZTAB                                
002700     03 DP-LINE-KDFINDOC     PIC X(4)                                     
002800                             VALUE SPACES.                                
002900*                                 TYP FINANSIELLT DOKUMENT                
003000*                                 FINANCIAL DOCUMENT TYPE                 
003100     03 DP-LINE-HORIZTAB     PIC X                                        
003200                             VALUE X'05'.                                 
003300*                                 HORIZTAB                                
003400*                                 HORIZTAB                                
003500     03 DP-LINE-IDFINDOC     PIC 9(9)                                     
003600                             VALUE ZEROS.                                 
003700*                                 FINANSIELLT DOKUMENT ID                 
003800*                                 FINANCIAL DOCUMENT ID                   
003900     03 DP-LINE-HORIZTAB     PIC X                                        
004000                             VALUE X'05'.                                 
004100*                                 HORIZTAB                                
004200*                                 HORIZTAB                                
004300     03 DP-LINE-IDREF        PIC X(15)                                    
004400                             VALUE SPACES.                                
004500*                                 REFERENS ID                             
004600*                                 REFERENCE ID                            
004700     03 DP-LINE-HORIZTAB     PIC X                                        
004800                             VALUE X'05'.                                 
004900*                                 HORIZTAB                                
005000*                                 HORIZTAB                                
005100     03 DP-LINE-IDEXCUST-2   PIC X(10)                                    
005200                             VALUE SPACES.                                
005300     03 DP-LINE-HORIZTAB     PIC X                                        
005400                             VALUE X'05'.                                 
005500*                                 HORIZTAB                                
005600*                                 HORIZTAB                                
005700     03 DP-LINE-IDEXCUST-1   PIC X(10)                                    
005800                             VALUE SPACES.                                
005900     03 DP-LINE-HORIZTAB     PIC X                                        
006000                             VALUE X'05'.                                 
006100*                                 HORIZTAB                                
006200*                                 HORIZTAB                                
006300     03 DP-LINE-IDOPTION-1   PIC X(17)                                    
006400                             VALUE SPACES.                                
006500     03 DP-LINE-HORIZTAB     PIC X                                        
006600                             VALUE X'05'.                                 
006700*                                 HORIZTAB                                
006800*                                 HORIZTAB                                
006900     03 DP-LINE-IDEXCUST-3   PIC X(10)                                    
007000                             VALUE SPACES.                                
007100     03 DP-LINE-HORIZTAB     PIC X                                        
007200                             VALUE X'05'.                                 
007300*                                 HORIZTAB                                
007400*                                 HORIZTAB                                
007500     03 DP-LINE-IDARTNR-FINANCE                                           
007600                             PIC X(46)                                    
007700                             VALUE SPACES.                                
007800*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
007900*                                 UK                                      
008000*                                 PART NUMBER FOR FINANCIAL USE           
008100     03 DP-LINE-HORIZTAB     PIC X                                        
008200                             VALUE X'05'.                                 
008300*                                 HORIZTAB                                
008400*                                 HORIZTAB                                
008500     03 DP-LINE-TIMM         PIC X(2)                                     
008600                             VALUE SPACES.                                
008700*                                 MÅNAD (MM)                              
008800*                                 MONTH (MM)                              
008900     03 DP-LINE-HORIZTAB     PIC X                                        
009000                             VALUE X'05'.                                 
009100*                                 HORIZTAB                                
009200*                                 HORIZTAB                                
009300     03 DP-LINE-SUNTO        PIC 9(11)V9(2)                               
009400                             VALUE ZEROS.                                 
009500*                                 TOTAL SALES AMOUNT EXCL. VAT            
009600     03 DP-LINE-HORIZTAB     PIC X                                        
009700                             VALUE X'05'.                                 
009800*                                 HORIZTAB                                
009900*                                 HORIZTAB                                
010000     03 DP-LINE-SUVAT-BILLIT PIC 9(11)V9(2)                               
010100                             VALUE ZEROS.                                 
010200*                                 SUMMERAT MOMSVÄRDE PER RAD              
010300*                                 TOTAL VAT VALUE PER LINE                
010400     03 DP-LINE-HORIZTAB     PIC X                                        
010500                             VALUE X'05'.                                 
010600*                                 HORIZTAB                                
010700*                                 HORIZTAB                                
010800     03 DP-LINE-SUBTO        PIC 9(11)V9(2)                               
010900                             VALUE ZEROS.                                 
011000*                                 TOTAL SALES AMOUNT INCL. VAT            
011100     03 DP-LINE-HORIZTAB     PIC X                                        
011200                             VALUE X'05'.                                 
011300*                                 HORIZTAB                                
011400*                                 HORIZTAB                                
011500     03 DP-LINE-REVAT        PIC 9(3)V9(2)                                
011600                             VALUE ZEROS.                                 
011700*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
011800*                                 VAT FACTOR                              
011900     03 DP-LINE-HORIZTAB     PIC X                                        
012000                             VALUE X'05'.                                 
012100*                                 HORIZTAB                                
012200*                                 HORIZTAB                                
012300*** END OF VILMAII-COPY LENGTH= 200 BYTES                                 
