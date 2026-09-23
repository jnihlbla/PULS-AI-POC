000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W460DIS1.                                                
000300*AUTHOR.         KARIN OLSSON.                                            
000400*DATE-WRITTEN.   92/07/20.                                                
000500                                                                          
000600                                                                          
000700*    REMARKS.                                                             
000800*    FUNKTION:                                                            
000900*        SUBPROGRAMMET KONTROLLERAR OM ETT DISTRIKT SOM GES VIA           
001000*        PARM ÄR ETT NOAC-DISTRIKT.                                       
001100*                                                                         
001200*        OM NOAC SÅ SÄTTS KDSVAR TILL J                                   
001300*                         IDLANDX2 TILL ISO TVÅ-STÄLLIG LANDSKOD          
001400*                                                                         
001500*        ANNARS  SÅ SÄTTS KDSVAR TILL N                                   
001600*                         IDLANDX2 BLANKAS                                
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 DATA DIVISION.                                                           
002300     SKIP2                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
002800 77   PROGRAM-NAMN           VALUE 'W460DIS1'                             
002900                                 PIC X(8).                                
003000     SKIP2                                                                
003100 01  GENERELLA-KONSTANTER.                                                
003200*                                                                         
003300     03  JA                      PIC X(1)    VALUE 'J'.                   
003400     03  NEJ                     PIC X(1)    VALUE 'N'.                   
003500                                                                          
003600     EJECT                                                                
003700*01  -COPY W460LISO                                                       
003800     EJECT                                                                
003900 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
004000*01  FILLER -COPY WWDIST52 -RED TEST-IDDISTR.                             
004100     EJECT                                                                
004200 LINKAGE SECTION.                                                         
004300                                                                          
004400 01  -COPY W460DIS1                                                       
004500     EJECT                                                                
004600 PROCEDURE DIVISION USING DIS1-W460DIS1.                                  
004700     SKIP2                                                                
004800                                                                          
004900     MOVE DIS1-IDDISTR TO TEST-IDDISTR                                    
005000                                                                          
005100     EVALUATE TRUE                                                        
005200                                                                          
005300       WHEN DIST52-FRANCE                                                 
005400         MOVE JA TO DIS1-KDSVAR                                           
005500         MOVE ISO-FRANKRIKE TO DIS1-IDLANDX2                              
005600                                                                          
005700       WHEN DIST52-DANMARK                                                
005800         MOVE JA TO DIS1-KDSVAR                                           
005900         MOVE ISO-DANMARK TO DIS1-IDLANDX2                                
006000                                                                          
006100       WHEN DIST52-VOLVOBIL                                               
006200         MOVE JA TO DIS1-KDSVAR                                           
006300         MOVE ISO-SVERIGE TO DIS1-IDLANDX2                                
006400                                                                          
006500       WHEN DIST52-NORGE                                                  
006600         MOVE JA TO DIS1-KDSVAR                                           
006700         MOVE ISO-NORGE TO DIS1-IDLANDX2                                  
006800                                                                          
006900       WHEN DIST52-ITALIEN                                                
007000         MOVE JA TO DIS1-KDSVAR                                           
007100         MOVE ISO-ITALIEN TO DIS1-IDLANDX2                                
007200                                                                          
007300       WHEN DIST52-BELGIEN                                                
007400         MOVE JA TO DIS1-KDSVAR                                           
007500         MOVE ISO-BELGIEN TO DIS1-IDLANDX2                                
007600                                                                          
007700       WHEN DIST52-FINLAND                                                
007800         MOVE JA TO DIS1-KDSVAR                                           
007900         MOVE ISO-FINLAND TO DIS1-IDLANDX2                                
008000                                                                          
008100       WHEN DIST52-TYSKLAND                                               
008200         MOVE JA TO DIS1-KDSVAR                                           
008300         MOVE ISO-TYSKLAND TO DIS1-IDLANDX2                               
008400                                                                          
008500       WHEN DIST52-SPANIEN                                                
008600         MOVE JA TO DIS1-KDSVAR                                           
008700         MOVE ISO-SPANIEN TO DIS1-IDLANDX2                                
008800                                                                          
008900       WHEN DIST52-ENGLAND                                                
009000         MOVE JA TO DIS1-KDSVAR                                           
009100         MOVE ISO-ENGLAND TO DIS1-IDLANDX2                                
009200                                                                          
009300       WHEN DIST52-IRLAND                                                 
009400         MOVE JA TO DIS1-KDSVAR                                           
009500         MOVE ISO-IRLAND TO DIS1-IDLANDX2                                 
009600                                                                          
009700       WHEN DIST52-POLEN                                                  
009800         MOVE JA TO DIS1-KDSVAR                                           
009900         MOVE ISO-POLEN TO DIS1-IDLANDX2                                  
010000                                                                          
010100       WHEN DIST52-HOLLAND                                                
010200         MOVE JA TO DIS1-KDSVAR                                           
010300         MOVE ISO-HOLLAND TO DIS1-IDLANDX2                                
010400                                                                          
010500       WHEN DIST52-USA                                                    
010600         MOVE JA TO DIS1-KDSVAR                                           
010700         MOVE ISO-USA TO DIS1-IDLANDX2                                    
010800                                                                          
010900       WHEN DIST52-OSTERRIKE                                              
011000         MOVE JA TO DIS1-KDSVAR                                           
011100         MOVE ISO-OSTERRIKE TO DIS1-IDLANDX2                              
011200                                                                          
011300       WHEN DIST52-PERU                                                   
011400         MOVE JA TO DIS1-KDSVAR                                           
011500         MOVE ISO-PERU TO DIS1-IDLANDX2                                   
011600                                                                          
011700       WHEN DIST52-BRASILIEN                                              
011800         MOVE JA TO DIS1-KDSVAR                                           
011900         MOVE ISO-BRASILIEN TO DIS1-IDLANDX2                              
012000                                                                          
012100       WHEN DIST52-BRASILIEN-NEW                                          
012200         MOVE JA TO DIS1-KDSVAR                                           
012300         MOVE ISO-BRASILIEN TO DIS1-IDLANDX2                              
012400                                                                          
012500       WHEN DIST52-JAPAN                                                  
012600         MOVE JA TO DIS1-KDSVAR                                           
012700         MOVE ISO-JAPAN TO DIS1-IDLANDX2                                  
012800                                                                          
012900       WHEN DIST52-KOREA                                                  
013000         MOVE JA TO DIS1-KDSVAR                                           
013100         MOVE ISO-KOREA TO DIS1-IDLANDX2                                  
013200                                                                          
013300       WHEN DIST52-PORTUGAL                                               
013400         MOVE JA TO DIS1-KDSVAR                                           
013500         MOVE ISO-PORTUGAL TO DIS1-IDLANDX2                               
013600                                                                          
013700       WHEN DIST52-PORTUGAL-NY                                            
013800         MOVE JA TO DIS1-KDSVAR                                           
013900         MOVE ISO-PORTUGAL2 TO DIS1-IDLANDX2                              
014000                                                                          
014100       WHEN DIST52-SAUDI                                                  
014200         MOVE JA TO DIS1-KDSVAR                                           
014300         MOVE ISO-SAUDI TO DIS1-IDLANDX2                                  
014400                                                                          
014500       WHEN DIST52-SCHWEIZ                                                
014600         MOVE JA TO DIS1-KDSVAR                                           
014700         MOVE ISO-SCHWEIZ TO DIS1-IDLANDX2                                
014800                                                                          
014900       WHEN DIST52-AUSTRALIEN                                             
015000         MOVE JA TO DIS1-KDSVAR                                           
015100         MOVE ISO-AUSTRALIEN TO DIS1-IDLANDX2                             
015200                                                                          
015300       WHEN DIST52-TAIWAN                                                 
015400         MOVE JA TO DIS1-KDSVAR                                           
015500         MOVE ISO-TAIWAN TO DIS1-IDLANDX2                                 
015600                                                                          
015700       WHEN DIST52-TAIWAN2                                                
015800         MOVE JA TO DIS1-KDSVAR                                           
015900         MOVE ISO-TAIWAN2 TO DIS1-IDLANDX2                                
016000                                                                          
016100       WHEN DIST52-THAILAND                                               
016200         MOVE JA TO DIS1-KDSVAR                                           
016300         MOVE ISO-THAILAND TO DIS1-IDLANDX2                               
016400                                                                          
016500       WHEN DIST52-MALAYSIA                                               
016600         MOVE JA TO DIS1-KDSVAR                                           
016700         MOVE ISO-MALAYSIA TO DIS1-IDLANDX2                               
016800                                                                          
016900       WHEN DIST52-USA-NEW                                                
017000         MOVE JA TO DIS1-KDSVAR                                           
017100         MOVE ISO-USA      TO DIS1-IDLANDX2                               
017200                                                                          
017300       WHEN DIST52-CANADA                                                 
017400         MOVE JA TO DIS1-KDSVAR                                           
017500         MOVE ISO-CANADA   TO DIS1-IDLANDX2                               
017600                                                                          
017700       WHEN DIST52-MEXICO                                                 
017800         MOVE JA TO DIS1-KDSVAR                                           
017900         MOVE ISO-MEXICO   TO DIS1-IDLANDX2                               
018000                                                                          
018100       WHEN DIST52-TURKIET                                                
018200         MOVE JA TO DIS1-KDSVAR                                           
018300         MOVE ISO-TURKIET  TO DIS1-IDLANDX2                               
018400                                                                          
018500       WHEN DIST52-RYSSLAND                                               
018600         MOVE JA TO DIS1-KDSVAR                                           
018700         MOVE ISO-RYSSLAND TO DIS1-IDLANDX2                               
018800                                                                          
018900       WHEN DIST52-KINA                                                   
019000         MOVE JA TO DIS1-KDSVAR                                           
019100*** KINA-C1 BÖR RENSAS OCH ANVÄNDA CN NÄR OLD 6214/6238 RENSAS            
019200         MOVE ISO-KINA-C1 TO DIS1-IDLANDX2                                
019300                                                                          
019400       WHEN DIST52-SYDAFRIKA                                              
019500         MOVE JA TO DIS1-KDSVAR                                           
019600         MOVE ISO-SYDAFRIKA TO DIS1-IDLANDX2                              
019700                                                                          
019710       WHEN DIST52-INDIEN                                                 
019720         MOVE JA TO DIS1-KDSVAR                                           
019730         MOVE ISO-INDIEN    TO DIS1-IDLANDX2                              
019740                                                                          
019750       WHEN DIST52-TJECKIEN                                               
019760         MOVE JA TO DIS1-KDSVAR                                           
019770         MOVE ISO-TJECKIEN  TO DIS1-IDLANDX2                              
019780                                                                          
019790       WHEN DIST52-UNGERN                                                 
019791         MOVE JA TO DIS1-KDSVAR                                           
019792         MOVE ISO-UNGERN    TO DIS1-IDLANDX2                              
019793                                                                          
019794       WHEN DIST52-EMIRATES                                               
019795         MOVE NEJ TO DIS1-KDSVAR                                          
019796         MOVE ISO-EMIRATERNA TO DIS1-IDLANDX2                             
019797                                                                          
019800       WHEN OTHER                                                         
019900         MOVE NEJ TO DIS1-KDSVAR                                          
020000         MOVE SPACE TO DIS1-IDLANDX2                                      
020100     END-EVALUATE                                                         
020200                                                                          
020300     MOVE ZERO TO RETURN-CODE                                             
020400     GOBACK                                                               
020500     .                                                                    
020600     EJECT                                                                
