000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4184900.                                    
000300*AUTHOR.                     BOO HAMMARIN CGLI.                           
000400*INSTALLATION.               W418    UTSKRIFT AV RETURTILLSTÅND           
000500*DATE-WRITTEN.               FEB     1992.                                
000600*REMARKS:                                                                 
000700*                                                                         
000800*  FUNKTION:                                                              
000900*  PROGRAMMET ÄR ETT  I M S - PROGRAM SOM                                 
001000*  - LÄSER EN FIL OCH                                                     
001100*  - SKRIVER RETURTILLSTÅND, SKICKAS TILL D&P VIA DAP3                    
001200*                                                                         
001300*  INDATA: W41849                                                         
001400*                                                                         
001500*  POSTTYPER:                                                             
001600*  76A      BILDAR RETURTILLSTÅND-HUVUD                                   
001700*  76B      BILDAR RETURTILLSTÅND-RAD                                     
001800*                                                                         
001900*  UTDATA: W4189A  RETURTILLSTÅND TILL D&P                                
002000*                                                                         
002100*  E-TRACKER : 1658417  060302                                            
002200*  E-TRACKER : 8872616  NOV 2013 (RT TILL D&P)                            
002300*                                                                         
002400*                                                                         
002500*  SUBPROGR:   DATKORT                                                    
002600                                                                          
002700*  RETURKODER: 0000    NORMALT SLUT                                       
002800                                                                          
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300                                                                          
003400     SELECT   W41848   ASSIGN TO  W41849D1.                               
003500     SELECT   W4184A   ASSIGN TO  W41849D2.                               
003600                                                                          
003700                                                                          
003800 DATA DIVISION.                                                           
003900 FILE SECTION.                                                            
004000                                                                          
004100 FD  W41848                                                               
004200     RECORDING V                                                          
004300     BLOCK 0.                                                             
004400 01  W41848-POST.                                                         
004500*    03  -COPY W41876A  -PRE IN-.                                         
004600 01  W41848B-POST.                                                        
004700*    03  -COPY W41876B  -PRE INB-.                                        
004800                                                                          
004900 FD  W4184A                                                               
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200 01  DOP-POST  PIC X(150).                                                
005300                                                                          
005400                                                                          
005500                                                                          
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000 77  CURRENT-SECTION         PIC X(16) VALUE SPACE.                       
006100                                                                          
006200 77  RKOD                    PIC S9(4)  COMP  SYNC  VALUE ZERO.           
006300 77  FLTA                    PIC X(6)           VALUE 'W41849'.           
006400 77  FLTB                    PIC X(6)           VALUE 'WDATUM'.           
006500 77  W-TOTBELOPP-1           PIC S9(8)V99   COMP-3   VALUE ZERO.          
006600 77  W-TOTBELOPP-2           PIC S9(8)V99   COMP-3   VALUE ZERO.          
006700 77  W-TOTBELOPP-3           PIC S9(8)V99   COMP-3   VALUE ZERO.          
006800 77  W-TOTPRLANDCO           PIC S9(8)V99   COMP-3   VALUE ZERO.          
006900 77  W-TOTPREMBHNT           PIC S9(8)V99   COMP-3   VALUE ZERO.          
007000 77  W-TOTPRFRAKT            PIC S9(8)V99   COMP-3   VALUE ZERO.          
007100 77  W-TOTPRHAMNAV           PIC S9(8)V99   COMP-3   VALUE ZERO.          
007200 77  W-TOTPRLEGKST           PIC S9(8)V99   COMP-3   VALUE ZERO.          
007300 77  W-TOTPRFOERS            PIC S9(8)V99   COMP-3   VALUE ZERO.          
007400 77  W-TOTPRMOMS             PIC S9(8)V99   COMP-3   VALUE ZERO.          
007500 77  W-REEMBHNT              PIC S9(2)V9    COMP-3   VALUE ZERO.          
007600 77  W-RELANDCO              PIC S9(3)V99   COMP-3   VALUE ZERO.          
007700 77  W-RADBELOPP             PIC S9(8)V99   COMP-3   VALUE ZERO.          
007800                                                                          
007810 77  W-IDDISTR               PIC 9(5)                VALUE ZERO.          
007811 77  W-IDKUNDNR              PIC 9(7)                VALUE ZERO.          
007820                                                                          
007900*01  -COPY W0004.                                                         
008000     EJECT                                                                
008100 01  DATUMKORT.                                                           
008200     03  FILLER              PIC X(11).                                   
008300     03  DATUM               PIC 9(6).                                    
008400     03  FILLER              PIC X(63).                                   
008500                                                                          
008600 01  SUBPROGRAM.                                                          
008700     03  DATKORT             PIC X(8)       VALUE 'DATKORT'.              
008800     03  W418OKOD            PIC X(8)       VALUE 'W418OKOD'.             
008900                                                                          
009000 01  SWITCHAR.                                                            
009100     03  EOF-SW          PIC X              VALUE 'N'.                    
009200         88  EOF                            VALUE 'J'.                    
009300     03  SW-FIRST-TIME-76A PIC X            VALUE 'J'.                    
009400         88  FIRST-TIME-76A                 VALUE 'J'.                    
009500                                                                          
009600                                                                          
009700 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
009800*01  FILLER  -COPY WWDIST45 -RED TEST-IDDISTR.                            
009900                                                                          
010000*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
010100                                                                          
010200 01  IFYLLDA-FALT.                                                        
010300     03  W-ADVANCE          PIC S9(9)      COMP SYNC.                     
010400     03  W-SIDR             PIC S9(5)      COMP-3   VALUE ZERO.           
010500     03  W-RADR             PIC S9(5)      COMP-3   VALUE ZERO.           
010600                                                                          
010700 01  RADER-TILL-RETURTILLSTAND.                                           
010800     03  RUBRAD4-76A.                                                     
010900         05  FILLER           PIC X(12)    VALUE SPACE.                   
011000         05  BEKOPARE-1-76A   PIC X(27).                                  
011100         05  FILLER           PIC X(30)    VALUE SPACE.                   
011200         05  IDDISTR-76A      PIC Z(04)9.                                 
011300         05  FILLER           PIC X(04)    VALUE SPACE.                   
011400         05  IDKUNDNR-76A     PIC Z(07).                                  
011500         05  FILLER           PIC X(04)    VALUE SPACE.                   
011600         05  IDRAPPNR-76A     PIC Z(06)9.                                 
011700         05  FILLER           PIC X(13)    VALUE SPACE.                   
011800         05  SIDR-76A         PIC Z(03)9.                                 
011900     03  RUBRAD5-76A.                                                     
012000         05  FILLER           PIC X(12)    VALUE SPACE.                   
012100         05  BEKOPARE-2-76A   PIC X(27).                                  
012200         05  FILLER           PIC X(74)    VALUE SPACE.                   
012300     03  RUBRAD6-76A.                                                     
012400         05  FILLER           PIC X(12)    VALUE SPACE.                   
012500         05  ADKOPARE-1-76A   PIC X(27).                                  
012600         05  FILLER           PIC X(74)    VALUE SPACE.                   
012700     03  RUBRAD7-76A.                                                     
012800         05  FILLER           PIC X(12)    VALUE SPACE.                   
012900         05  ADKOPARE-2-76A   PIC X(27).                                  
013000         05  FILLER           PIC X(38)    VALUE SPACE.                   
013100         05  TILEVANM-76A     PIC 9(06).                                  
013200         05  FILLER           PIC X(24)    VALUE SPACE.                   
013300         05  DATUM-76A        PIC 9(06).                                  
013400                                                                          
013500     03  DETRAD-76B.                                                      
013600         05  FILLER           PIC X(04)    VALUE SPACE.                   
013700         05  IDORDNR-76B      PIC Z(05).                                  
013800         05  FILLER           PIC X(01)    VALUE SPACE.                   
013900         05  IDARTNR-76B      PIC Z(07)9.                                 
014000         05  FILLER           PIC X(01)    VALUE '-'.                     
014100         05  REKSIFFR-76B     PIC 9(01).                                  
014200         05  FILLER           PIC X(01)    VALUE SPACE.                   
014300         05  BEART-76B        PIC X(15).                                  
014400         05  FILLER           PIC X(04)    VALUE SPACE.                   
014500         05  KVLEVANM-76B     PIC Z(06)9.                                 
014600         05  FILLER           PIC X(12)    VALUE SPACE.                   
014700         05  KDANMORS-76B     PIC 9(02).                                  
014800         05  FILLER           PIC X(01)    VALUE SPACE.                   
014900         05  PRARTBTO-76B     PIC Z(07).Z(02).                            
015000         05  RADBELOPP-76B    PIC Z(07).Z(02).                            
015100         05  IDRADNR-76B      PIC Z(04)9.                                 
015200         05  FILLER           PIC X(30)    VALUE SPACE.                   
015300                                                                          
015400     03  TOTRAD32-76B.                                                    
015500         05  FILLER           PIC X(69)   VALUE SPACE.                    
015600         05  TOTBELOPP-1-76B  PIC Z(07).Z(02).                            
015700         05  FILLER           PIC X(34)   VALUE SPACE.                    
015800     03  TOTRAD33-76B.                                                    
015900         05  FILLER           PIC X(69)   VALUE SPACE.                    
016000         05  PRLANDCO-76B     PIC Z(07).Z(02).                            
016100         05  FILLER           PIC X(34)   VALUE SPACE.                    
016200     03  TOTRAD34-76B.                                                    
016300         05  FILLER           PIC X(69)   VALUE SPACE.                    
016400         05  PREMBHNT-76B     PIC Z(07).Z(02).                            
016500         05  FILLER           PIC X(34)   VALUE SPACE.                    
016600     03  TOTRAD35-76B.                                                    
016700         05  FILLER           PIC X(69)   VALUE SPACE.                    
016800         05  PRFRAKT-76B      PIC Z(07).Z(02).                            
016900         05  FILLER           PIC X(34)   VALUE SPACE.                    
017000     03  TOTRAD36-76B.                                                    
017100         05  FILLER           PIC X(69)   VALUE SPACE.                    
017200         05  PRLEGKST-76B     PIC Z(07).Z(02).                            
017300         05  FILLER           PIC X(34)   VALUE SPACE.                    
017400     03  TOTRAD37-76B.                                                    
017500         05  FILLER           PIC X(69)   VALUE SPACE.                    
017600         05  PRFOERS-76B      PIC Z(07).Z(02).                            
017700         05  FILLER           PIC X(34)   VALUE SPACE.                    
017800     03  TOTRAD38-76B.                                                    
017900         05  FILLER           PIC X(82)   VALUE SPACE.                    
018000     03  TOTRAD39-76B.                                                    
018100         05  FILLER           PIC X(69)   VALUE SPACE.                    
018200         05  PRMOMS-76B       PIC Z(07).Z(02).                            
018300         05  FILLER           PIC X(34)   VALUE SPACE.                    
018400     03  TOTRAD40-76B.                                                    
018500         05  FILLER           PIC X(69)   VALUE SPACE.                    
018600         05  TOTBELOPP-2-76B  PIC Z(07).Z(02).                            
018700         05  FILLER           PIC X(34)   VALUE SPACE.                    
018800                                                                          
018900 01  BLANKRADER.                                                          
019000     03  BLANKRAD.                                                        
019100         05  FILLER           PIC X(150)   VALUE SPACE.                   
019200                                                                          
019210 01  W001-DAP.                                                            
019220     03  FILLER                  PIC X(150)  VALUE SPACE.                 
019300                                                                          
019400*            * * * * * * * * * * *                                        
019500*            *   I N F I L E N   *                                        
019600*            * * * * * * * * * * *                                        
019700                                                                          
019800 01  FILLER                     PIC X(05) VALUE '*76A*'.                  
019900*01  POST         -COPY W41876A     -PRE  76A-                            
020000                                                                          
020100 01  FILLER                     PIC X(05) VALUE '*76B*'.                  
020200*01  POST         -COPY W41876B     -PRE  76B-                            
020300                                                                          
020400                                                                          
020500*    ---  LÄNKAREA TILL W418OKOD                                          
020600                                                                          
020700*    03 -COPY W418OKOD           -PRE OKOD-.                              
020800                                                                          
020900                                                                          
021000 PROCEDURE DIVISION.                                                      
021100                                                                          
021200     PERFORM A-INIT                                                       
021300     PERFORM S01-LAES-INFIL                                               
021400                                                                          
021500     PERFORM UNTIL EOF                                                    
021600       MOVE IN-IDDISTR                 TO TEST-IDDISTR                    
021700       IF DIST45-EJ-UTSKRIFT                                              
021800          CONTINUE                                                        
021900       ELSE                                                               
022000          PERFORM B-BEHANDLA                                              
022100       END-IF                                                             
022200       PERFORM S01-LAES-INFIL                                             
022300     END-PERFORM                                                          
022400                                                                          
022500     IF NOT FIRST-TIME-76A                                                
022600        PERFORM C-BYGG-SIDSLUT-76A                                        
022700     END-IF                                                               
022800                                                                          
022900     PERFORM Z-FINIT                                                      
023000                                                                          
023100     MOVE ZERO                         TO RETURN-CODE                     
023200     GOBACK                                                               
023300     .                                                                    
023400                                                                          
023410                                                                          
023500 A-INIT  SECTION.                                                         
023510     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
023600                                                                          
023700     CALL DATKORT USING FLTA FLTB DATUMKORT                               
023800                                                                          
023900     OPEN INPUT   W41848                                                  
024000          OUTPUT  W4184A                                                  
024100                                                                          
024200     PERFORM AA-INIT-AV-AREOR                                             
024300     .                                                                    
024310                                                                          
024320                                                                          
024400 AA-INIT-AV-AREOR SECTION.                                                
024410     MOVE 'AA-INIT-AV-AREOR' TO CURRENT-SECTION                           
024500                                                                          
024600     MOVE +1                           TO W-SIDR                          
024700     MOVE +10                          TO W-RADR                          
024800                                                                          
024900     MOVE 'J'                          TO SW-FIRST-TIME-76A               
025000                                                                          
025100     MOVE ZERO                         TO W-TOTBELOPP-1                   
025200                                          W-TOTBELOPP-2                   
025300                                          W-TOTBELOPP-3                   
025400                                          W-TOTPRLANDCO                   
025500                                          W-TOTPREMBHNT                   
025600                                          W-TOTPRFRAKT                    
025700                                          W-TOTPRLEGKST                   
025800                                          W-TOTPRHAMNAV                   
025900                                          W-TOTPRFOERS                    
026000                                          W-TOTPRMOMS                     
026100     .                                                                    
026200                                                                          
026210                                                                          
026300 B-BEHANDLA  SECTION.                                                     
026310     MOVE 'B-BEHANDLA      ' TO CURRENT-SECTION                           
026400                                                                          
026500     EVALUATE IN-IDPTYP (3:1)                                             
026600     WHEN 'A'                                                             
026700       IF NOT FIRST-TIME-76A                                              
026800          PERFORM C-BYGG-SIDSLUT-76A                                      
026900       ELSE                                                               
027000          MOVE 'N'                    TO SW-FIRST-TIME-76A                
027100       END-IF                                                             
027200       MOVE W41848-POST               TO 76A-POST                         
027300       MOVE  +1                       TO W-SIDR                           
027400       MOVE +10                       TO W-RADR                           
027420       IF IN-IDDISTR  NOT = W-IDDISTR                                     
027430       OR IN-IDKUNDNR NOT = W-IDKUNDNR                                    
027440          PERFORM BA-DOP-BRYTNING                                         
027510       END-IF                                                             
027520       PERFORM CB-BYGG-HUVUD-76A                                          
027600     WHEN 'B'                                                             
027700       MOVE W41848B-POST               TO 76B-POST                        
027800       IF W-RADR = 31                                                     
027900          ADD +1                      TO W-SIDR                           
028000          PERFORM CB-BYGG-HUVUD-76A                                       
028100          MOVE +10                    TO W-RADR                           
028200       END-IF                                                             
028300       PERFORM CC-BYGG-RAD-76B                                            
028400       ADD +1                         TO W-RADR                           
028500     END-EVALUATE                                                         
028600     .                                                                    
028700                                                                          
028710                                                                          
028800 BA-DOP-BRYTNING SECTION.                                                 
028810     MOVE 'BA-DOP-BRYTNING ' TO CURRENT-SECTION                           
028900                                                                          
028901     MOVE ' ¤DAPRETURN-PERMW418' TO W001-DAP                              
028902     WRITE DOP-POST FROM W001-DAP                                         
028903                                                                          
028904     MOVE SPACE                 TO W001-DAP                               
028905     MOVE IN-IDDISTR            TO W-IDDISTR                              
028906     MOVE IN-IDKUNDNR           TO W-IDKUNDNR                             
028907     STRING ' ¤DAP' W-IDDISTR W-IDKUNDNR                                  
028908     DELIMITED BY SIZE INTO W001-DAP                                      
028909     WRITE DOP-POST FROM W001-DAP                                         
028910     .                                                                    
028911                                                                          
028920                                                                          
028930 C-BYGG-SIDSLUT-76A SECTION.                                              
028940     MOVE 'C-SIDSLUT-76A   ' TO CURRENT-SECTION                           
028950                                                                          
029000     MOVE W-TOTBELOPP-1               TO TOTBELOPP-1-76B                  
029100     COMPUTE W-TOTPRLANDCO ROUNDED =                                      
029200             W-TOTBELOPP-3 * W-RELANDCO / 100                             
029300     MOVE W-TOTPRLANDCO               TO PRLANDCO-76B                     
029400     COMPUTE W-TOTPREMBHNT ROUNDED =                                      
029500             W-TOTBELOPP-1 * W-REEMBHNT / 100                             
029600     MOVE W-TOTPREMBHNT               TO PREMBHNT-76B                     
029700     MOVE W-TOTPRFRAKT                TO PRFRAKT-76B                      
029800     MOVE W-TOTPRLEGKST               TO PRLEGKST-76B                     
029900     MOVE W-TOTPRFOERS                TO PRFOERS-76B                      
030000     MOVE W-TOTPRMOMS                 TO PRMOMS-76B                       
030100                                                                          
030200     COMPUTE W-TOTBELOPP-2 =             W-TOTBELOPP-1 +                  
030300                                         W-TOTPRLANDCO +                  
030400                                         W-TOTPREMBHNT +                  
030500                                         W-TOTPRFRAKT  +                  
030600                                         W-TOTPRLEGKST +                  
030700                                         W-TOTPRFOERS  +                  
030800                                         W-TOTPRMOMS                      
030900                                                                          
031000     MOVE W-TOTBELOPP-2               TO TOTBELOPP-2-76B                  
031100                                                                          
031200     COMPUTE W-ADVANCE = 32 - W-RADR + 1                                  
031300                                                                          
031400     WRITE DOP-POST FROM TOTRAD32-76B AFTER W-ADVANCE                     
031500     WRITE DOP-POST FROM TOTRAD33-76B AFTER 1                             
031600     WRITE DOP-POST FROM TOTRAD34-76B AFTER 1                             
031700     WRITE DOP-POST FROM TOTRAD35-76B AFTER 1                             
031800     WRITE DOP-POST FROM TOTRAD36-76B AFTER 1                             
031900     WRITE DOP-POST FROM TOTRAD37-76B AFTER 1                             
032000     WRITE DOP-POST FROM TOTRAD38-76B AFTER 1                             
032100     WRITE DOP-POST FROM TOTRAD39-76B AFTER 1                             
032200     WRITE DOP-POST FROM TOTRAD40-76B AFTER 1                             
032300                                                                          
032400     PERFORM CAA-NOLLST-76B                                               
032500     .                                                                    
032600                                                                          
032610                                                                          
032700 CAA-NOLLST-76B    SECTION.                                               
032710     MOVE 'CAA-NOLLST-76B  ' TO CURRENT-SECTION                           
032720                                                                          
032800     MOVE ZERO                        TO W-TOTBELOPP-1                    
032900                                         W-TOTBELOPP-2                    
033000                                         W-TOTBELOPP-3                    
033100                                         W-TOTPRLANDCO                    
033200                                         W-TOTPREMBHNT                    
033300                                         W-TOTPRFRAKT                     
033400                                         W-TOTPRLEGKST                    
033500                                         W-TOTPRFOERS                     
033600                                         W-TOTPRMOMS                      
033700     .                                                                    
033800                                                                          
033810                                                                          
033900 CB-BYGG-HUVUD-76A SECTION.                                               
033910     MOVE 'CB-HUVUD-76A    ' TO CURRENT-SECTION                           
034000                                                                          
034100     MOVE 76A-BEKOPARE-1              TO BEKOPARE-1-76A                   
034200     MOVE 76A-IDDISTR                 TO IDDISTR-76A                      
034300     MOVE 76A-IDKUNDNR                TO IDKUNDNR-76A                     
034400     MOVE 76A-IDRAPPNR                TO IDRAPPNR-76A                     
034500*    MOVE 76A-IDDC (1:1)              TO IDDC-76A                         
034600     MOVE W-SIDR                      TO SIDR-76A                         
034700     MOVE 76A-BEKOPARE-2              TO BEKOPARE-2-76A                   
034800     MOVE 76A-ADKOPARE-1              TO ADKOPARE-1-76A                   
034900     MOVE 76A-ADKOPARE-2              TO ADKOPARE-2-76A                   
035000     MOVE 76A-TILEVANM                TO TILEVANM-76A                     
035100     MOVE 76A-DATUM                   TO DATUM-76A                        
035200     MOVE 76A-REEMBHNT                TO W-REEMBHNT                       
035300     MOVE 76A-RELANDCO                TO W-RELANDCO                       
035400     MOVE 76A-PRFRAKT                 TO W-TOTPRFRAKT                     
035500     MOVE 76A-PRFOERS                 TO W-TOTPRFOERS                     
035600     MOVE 76A-PRLEGKST                TO W-TOTPRLEGKST                    
035700                                                                          
035800     WRITE DOP-POST FROM BLANKRAD    AFTER PAGE                           
035880                                                                          
035900     WRITE DOP-POST FROM RUBRAD4-76A AFTER 3                              
036000     WRITE DOP-POST FROM RUBRAD5-76A AFTER 1                              
036100     WRITE DOP-POST FROM RUBRAD6-76A AFTER 1                              
036200     WRITE DOP-POST FROM RUBRAD7-76A AFTER 1                              
036300     .                                                                    
036400                                                                          
036410                                                                          
036500 CC-BYGG-RAD-76B SECTION.                                                 
036510     MOVE 'CC-BYGG-RAD-76B ' TO CURRENT-SECTION                           
036600                                                                          
036700     MOVE 76B-IDORDNR                 TO IDORDNR-76B                      
036800     MOVE 76B-IDARTNR                 TO IDARTNR-76B                      
036900     MOVE 76B-REKSIFFR                TO REKSIFFR-76B                     
037000     MOVE 76B-BEART                   TO BEART-76B                        
037100     MOVE 76B-KDANMORS                TO KDANMORS-76B                     
037200                                                                          
037300     IF KDANMORS-76B = 27                                                 
037400       MOVE 22                        TO KDANMORS-76B                     
037500     END-IF                                                               
037600                                                                          
037700     MOVE 76B-KVLEVANM                TO KVLEVANM-76B                     
037800     MOVE 76B-IDRADNR                 TO IDRADNR-76B                      
037900                                                                          
038000     IF DIST79-DEALER-PRICE OR                                            
038020        DIST79-ECOM-PRICE                                                 
038100       MOVE 76B-PRARTBTO-LOC          TO PRARTBTO-76B                     
038200                                                                          
038300       COMPUTE W-RADBELOPP =                                              
038400                    76B-PRARTBTO-LOC * 76B-KVLEVANM                       
038500     ELSE                                                                 
038600       MOVE 76B-PRARTBTO              TO PRARTBTO-76B                     
038700                                                                          
038800       COMPUTE W-RADBELOPP = 76B-PRARTBTO * 76B-KVLEVANM                  
038900     END-IF                                                               
039000                                                                          
039100     MOVE W-RADBELOPP                 TO RADBELOPP-76B                    
039200                                                                          
039300     COMPUTE W-TOTBELOPP-1 = W-TOTBELOPP-1 + W-RADBELOPP                  
039400                                                                          
039500     MOVE 76B-KDANMORS  TO OKOD-KDANMORS                                  
039600     CALL W418OKOD USING OKOD-W418OKOD                                    
039700     IF OKOD-FL-KOD-SOM-BAER-TK  = 'J' OR                                 
039800        OKOD-FL-INTERNUPPACKNING = 'J'                                    
039900        COMPUTE W-TOTBELOPP-3 = W-TOTBELOPP-3 + W-RADBELOPP               
040000     END-IF                                                               
040100                                                                          
040630     IF W-RADR = 10                                                       
040640        WRITE DOP-POST FROM DETRAD-76B AFTER 3                            
040650     ELSE                                                                 
040660        WRITE DOP-POST FROM DETRAD-76B AFTER 1                            
040670     END-IF                                                               
040700     .                                                                    
040800                                                                          
040810                                                                          
040900 Z-FINIT SECTION.                                                         
040910     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
041000                                                                          
041100     CLOSE  W41848 W4184A                                                 
041200     .                                                                    
041300                                                                          
041310                                                                          
041400 S01-LAES-INFIL SECTION.                                                  
041500                                                                          
041600     READ  W41848                                                         
041700                     AT END     MOVE 'J' TO EOF-SW                        
041800     END-READ                                                             
041900     .                                                                    
