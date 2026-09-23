000010*COMPOPT STDSUB=YES                                                       
000200                                                                          
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                 W300AFOM.                                    
000800 AUTHOR.                     ERIK KÅREBY                                  
000900 DATE-WRITTEN.               DEC 1983.                                    
000910 DATE-COMPILED.                                                           
001000                                                                          
001100*    REMARKS.                                                             
001200*        PROGRAMMET ADDERAR PERIODDATA TILL AF1 - AF5                     
001300*        AF1 = RULLANDE 8 PERIODER ETT ÅR TILLBAKA                        
001400*        AF2 = RULLANDE 8 PERIODER                                        
001500*        AF3 = FRÅN FÖREGÅENDE ÅRS 1 PERIOD TILL DAGENS PERIOD            
001600*              FÖREGÅENDE ÅR                                              
001700*        AF4 = PERIOD 1 TILL DAGENS PERIODINNEVARANDE ÅR                  
001800*        AF5 = AKTUELL PERIOD                                             
001900*        AF6 = START-PERIOD TILL STOPP-PERIOD                             
002000                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002710*    -COPY WY2000W9                                                       
002800     SKIP3                                                                
003400 77  IDPGM                   PIC X(8)    VALUE 'W300AFOM'.                
003420 77  IND                     PIC S9(9)   COMP  SYNC.                      
003500 77  FG-AR-PERIOD            PIC S9(3)   COMP-3 VALUE ZERO.               
003600 77  INV-AR-PERIOD           PIC S9(3)   COMP-3 VALUE ZERO.               
003700 77  MIN-PER                 PIC S9(3)   COMP-3 VALUE ZERO.               
003800 77  MAX-PER                 PIC S9(3)   COMP-3 VALUE ZERO.               
003900     SKIP3                                                                
004000 01  DIVERSE-TIDSVAR.                                                     
004100     03  NUV-TIAAP           PIC 9(3)    VALUE ZERO.                      
004200     03  FILLER              REDEFINES   NUV-TIAAP.                       
004300       05  NUV-TIAA          PIC 99.                                      
004400       05  NUV-TIP           PIC 9.                                       
004500     03  MIN-TIAAP           PIC 9(3)    VALUE ZERO.                      
004600     03  FILLER              REDEFINES   MIN-TIAAP.                       
004700       05  MIN-TIAA          PIC 99.                                      
004800       05  MIN-TIP           PIC 9.                                       
004900     03  MAX-TIAAP           PIC 9(3)    VALUE ZERO.                      
005000     03  FILLER              REDEFINES   MAX-TIAAP.                       
005100       05  MAX-TIAA          PIC 99.                                      
005200       05  MAX-TIP           PIC 9.                                       
005300     EJECT                                                                
005400 LINKAGE SECTION.                                                         
005500                                                                          
005600*01  AREA -COPY W009W030 -PRE IN-                                         
005800     EJECT                                                                
005900 PROCEDURE DIVISION USING IN-AREA.                                        
006000 MAIN SECTION.                                                            
006200                                                                          
006300     PERFORM A-INIT                                                       
006400                                                                          
006500     MOVE +1 TO IND                                                       
006600                                                                          
006620     PERFORM UNTIL                                                        
006800      ( IND > IN-TIANTPER )                                               
006820       IF IN-TIFSGPER(IND) NOT < MIN-PER                                  
007000       AND IN-TIFSGPER(IND) NOT > MAX-PER                                 
007100         ADD IN-KVLEVART(IND) TO IN-AF-KVLEVART(6)                        
007200         ADD IN-PRARTNTO(IND) TO IN-AF-PRARTNTO(6)                        
007300         ADD IN-PRARTSJK(IND) TO IN-AF-PRARTSJK(6)                        
007400       END-IF                                                             
007500       IF IN-TIFSGPER(IND) = +16                                          
007600         ADD IN-KVLEVART(IND) TO IN-AF-KVLEVART(5)                        
007700         ADD IN-PRARTNTO(IND) TO IN-AF-PRARTNTO(5)                        
007800         ADD IN-PRARTSJK(IND) TO IN-AF-PRARTSJK(5)                        
007900       END-IF                                                             
007920       IF IN-TIFSGPER(IND) NOT < INV-AR-PERIOD                            
008100       AND IN-TIFSGPER(IND) NOT > +16                                     
008200         ADD IN-KVLEVART(IND) TO IN-AF-KVLEVART(4)                        
008300         ADD IN-PRARTNTO(IND) TO IN-AF-PRARTNTO(4)                        
008400         ADD IN-PRARTSJK(IND) TO IN-AF-PRARTSJK(4)                        
008500       END-IF                                                             
008520       IF IN-TIFSGPER(IND) NOT < FG-AR-PERIOD                             
008700       AND IN-TIFSGPER(IND) NOT > +8                                      
008800         ADD IN-KVLEVART(IND) TO IN-AF-KVLEVART(3)                        
008900         ADD IN-PRARTNTO(IND) TO IN-AF-PRARTNTO(3)                        
009000         ADD IN-PRARTSJK(IND) TO IN-AF-PRARTSJK(3)                        
009100       END-IF                                                             
009120       IF IN-TIFSGPER(IND) > +8                                           
009300       AND IN-TIFSGPER(IND) NOT > +16                                     
009400         ADD IN-KVLEVART(IND) TO IN-AF-KVLEVART(2)                        
009500         ADD IN-PRARTNTO(IND) TO IN-AF-PRARTNTO(2)                        
009600         ADD IN-PRARTSJK(IND) TO IN-AF-PRARTSJK(2)                        
009700       END-IF                                                             
009720       IF IN-TIFSGPER(IND) > +0                                           
009900       AND IN-TIFSGPER(IND) NOT > +8                                      
010000         ADD IN-KVLEVART(IND) TO IN-AF-KVLEVART(1)                        
010100         ADD IN-PRARTNTO(IND) TO IN-AF-PRARTNTO(1)                        
010200         ADD IN-PRARTSJK(IND) TO IN-AF-PRARTSJK(1)                        
010300       END-IF                                                             
010400       ADD +1 TO IND                                                      
010500                                                                          
010600     END-PERFORM                                                          
010700     MOVE ZERO TO RETURN-CODE                                             
010800     GOBACK                                                               
010900     .                                                                    
011000     EJECT                                                                
011100 A-INIT SECTION.                                                          
011200                                                                          
011300     MOVE +1 TO IND                                                       
011400                                                                          
011500     PERFORM UNTIL                                                        
011600      NOT ( IND < +7 )                                                    
011700       MOVE ZERO TO IN-AF-KVLEVART(IND)                                   
011800                    IN-AF-PRARTNTO(IND)                                   
011900                    IN-AF-PRARTSJK(IND)                                   
012000       ADD +1 TO IND                                                      
012100     END-PERFORM                                                          
012200     MOVE IN-NUV-TIAAP TO NUV-TIAAP                                       
012300                                                                          
012400     COMPUTE INV-AR-PERIOD = 16 - NUV-TIP + 1                             
012500                                                                          
012600     COMPUTE FG-AR-PERIOD  = 8  - NUV-TIP + 1                             
012700                                                                          
012800     MOVE IN-TISTART-AAP TO MIN-TIAAP                                     
012900     MOVE IN-TISTOPP-AAP TO MAX-TIAAP                                     
013000                                                                          
013001     MOVE NUV-TIAA TO TMP1-YY                                             
013010     MOVE MIN-TIAA TO TMP2-YY                                             
013030     PERFORM WY2000P9                                                     
013040                                                                          
013100     COMPUTE MIN-PER =                                                    
013200     16 - ((TMP1-YY * 8 + NUV-TIP) - (TMP2-YY * 8 + MIN-TIP))             
013300                                                                          
013310     MOVE NUV-TIAA TO TMP1-YY                                             
013330     MOVE MAX-TIAA TO TMP2-YY                                             
013340     PERFORM WY2000P9                                                     
013350                                                                          
013400     COMPUTE MAX-PER =                                                    
013500     16 - ((TMP1-YY * 8 + NUV-TIP) - (TMP2-YY * 8 + MAX-TIP))             
013600     .                                                                    
013610     EJECT                                                                
013700*    -COPY WY2000P9                                                       
