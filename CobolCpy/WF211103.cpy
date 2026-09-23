000100 01  FI-FOOT-WF2111.                                                      
000200*                                 DOCUMENT DATA FOOTER                    
000300     03 FI-FOOT-IDAFPRCD     PIC X(10)                                    
000400                             VALUE SPACES.                                
000500*                                 AFP-BLANKETT POSTTYP                    
000600*                                 AFP FORMS RECORD TYPE                   
000700     03 FI-FOOT-HORIZTAB     PIC X                                        
000800                             VALUE X'05'.                                 
000900*                                 HORIZTAB                                
001000*                                 HORIZTAB                                
001100     03 FI-FOOT-SUNTO-TOT    PIC Z(10)9.9(2)                              
001200                             VALUE ZEROS.                                 
001300*                                 TOTAL SALES AMOUNT EXCL. VAT            
001400     03 FI-FOOT-HORIZTAB     PIC X                                        
001500                             VALUE X'05'.                                 
001600*                                 HORIZTAB                                
001700*                                 HORIZTAB                                
001800     03 FI-FOOT-SUVAT-BILLIT-TOT                                          
001900                             PIC Z(10)9.9(2)                              
002000                             VALUE ZEROS.                                 
002100*                                 SUMMERAT MOMSVÄRDE                      
002200*                                 TOTAL VAT VALUE                         
002300     03 FI-FOOT-HORIZTAB     PIC X                                        
002400                             VALUE X'05'.                                 
002500*                                 HORIZTAB                                
002600*                                 HORIZTAB                                
002700     03 FI-FOOT-SUBTO-TOT    PIC Z(10)9.9(2)                              
002800                             VALUE ZEROS.                                 
002900*                                 TOTAL SALES AMOUNT INCL. VAT            
003000     03 FI-FOOT-HORIZTAB     PIC X                                        
003100                             VALUE X'05'.                                 
003200*                                 HORIZTAB                                
003300*                                 HORIZTAB                                
003400     03 FI-FOOT-KDVALISO     PIC X(3)                                     
003500                             VALUE SPACES.                                
003600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003700*                                 CURRENCY CODE BY ISO-STANDARD.          
003800     03 FI-FOOT-HORIZTAB     PIC X                                        
003900                             VALUE X'05'.                                 
004000*                                 HORIZTAB                                
004100*                                 HORIZTAB                                
004200     03 FI-FOOT-SUNTO-TOT-LOC                                             
004300                             PIC Z(10)9.9(2)                              
004400                             VALUE ZEROS.                                 
004500*                                 TOTAL SALES AMOUNT EXCL. VAT            
004600     03 FI-FOOT-HORIZTAB     PIC X                                        
004700                             VALUE X'05'.                                 
004800*                                 HORIZTAB                                
004900*                                 HORIZTAB                                
005000     03 FI-FOOT-SUVAT-BILLIT-TOT-LOC                                      
005100                             PIC Z(10)9.9(2)                              
005200                             VALUE ZEROS.                                 
005300*                                 SUMMERAT MOMSVÄRDE                      
005400*                                 TOTAL VAT VALUE                         
005500     03 FI-FOOT-HORIZTAB     PIC X                                        
005600                             VALUE X'05'.                                 
005700*                                 HORIZTAB                                
005800*                                 HORIZTAB                                
005900     03 FI-FOOT-SUBTO-TOT-LOC                                             
006000                             PIC Z(10)9.9(2)                              
006100                             VALUE ZEROS.                                 
006200*                                 TOTAL SALES AMOUNT INCL. VAT            
006300     03 FI-FOOT-HORIZTAB     PIC X                                        
006400                             VALUE X'05'.                                 
006500*                                 HORIZTAB                                
006600*                                 HORIZTAB                                
006700     03 FI-FOOT-KDVALISO-LOC PIC X(3)                                     
006800                             VALUE SPACES.                                
006900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007000*                                 CURRENCY CODE BY ISO-STANDARD.          
007100     03 FI-FOOT-HORIZTAB     PIC X                                        
007200                             VALUE X'05'.                                 
007300*                                 HORIZTAB                                
007400*                                 HORIZTAB                                
007500     03 FI-FOOT-SUNTO-TOT-SND                                             
007600                             PIC Z(10)9.9(2)                              
007700                             VALUE ZEROS.                                 
007800*                                 TOTAL SALES AMOUNT EXCL. VAT            
007900     03 FI-FOOT-HORIZTAB     PIC X                                        
008000                             VALUE X'05'.                                 
008100*                                 HORIZTAB                                
008200*                                 HORIZTAB                                
008300     03 FI-FOOT-SUVAT-BILLIT-TOT-SND                                      
008400                             PIC Z(10)9.9(2)                              
008500                             VALUE ZEROS.                                 
008600*                                 SUMMERAT MOMSVÄRDE                      
008700*                                 TOTAL VAT VALUE                         
008800     03 FI-FOOT-HORIZTAB     PIC X                                        
008900                             VALUE X'05'.                                 
009000*                                 HORIZTAB                                
009100*                                 HORIZTAB                                
009200     03 FI-FOOT-SUBTO-TOT-SND                                             
009300                             PIC Z(10)9.9(2)                              
009400                             VALUE ZEROS.                                 
009500*                                 TOTAL SALES AMOUNT INCL. VAT            
009600     03 FI-FOOT-HORIZTAB     PIC X                                        
009700                             VALUE X'05'.                                 
009800*                                 HORIZTAB                                
009900*                                 HORIZTAB                                
010000     03 FI-FOOT-KDVALISO-SND PIC X(3)                                     
010100                             VALUE SPACES.                                
010200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010300*                                 CURRENCY CODE BY ISO-STANDARD.          
010400     03 FI-FOOT-HORIZTAB     PIC X                                        
010500                             VALUE X'05'.                                 
010600*                                 HORIZTAB                                
010700*                                 HORIZTAB                                
010800     03 FI-FOOT-PRKURS-SND   PIC Z(5)9.9(5)                               
010900                             VALUE ZEROS.                                 
011000*                                 VALUTAKURS                              
011100*                                 CURRENCY EXCHANGE RATE                  
011200     03 FI-FOOT-HORIZTAB     PIC X                                        
011300                             VALUE X'05'.                                 
011400*                                 HORIZTAB                                
011500*                                 HORIZTAB                                
011600     03 FI-FOOT-KDVALISO-TXT PIC X(3)                                     
011700                             VALUE SPACES.                                
011800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011900*                                 CURRENCY CODE BY ISO-STANDARD.          
012000*** END OF VILMAII-COPY LENGTH= 174 BYTES                                 
