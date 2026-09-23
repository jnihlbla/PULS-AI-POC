000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W0300200.                                                 
000400*AUTHOR.        TOMMY JOHANSSON                                           
000500*DATE-WRITTEN.  NOV. 1986                                                 
000600                                                                          
000700*REMARKS.                                                                 
000800                                                                          
000900*    FUNKTION:                                                            
001000                                                                          
001100*      - LÄSER IN DATUMKORT FRÅN SYSIN.                                   
001200*      - TESTAR OM DET FINNS PÅ DATUMFILEN.                               
001300*      - SYSINKORTET ÄR DET SOM ÄR GÄLLANDE, DVS GAMMALT                  
001400*        KORT BLIR AVKORTAT MOTSVARANDE ANTAL DAGAR SOM                   
001500*        SYSIN ÖVERLAPPAR BEFINTLIGT DATUMKORT.                           
001600*      - DE DATUMKORT SOM ÄR INAKTUELLA SLÄNGS.                           
001700                                                                          
001800*    SUBPROGRAM:                                                          
001900*        WDATKONV - DAT-WDATAREA, PROGRAMMET KONVERTERAR DATUM            
002000*                 TILL OLIKA FORMAT.                                      
002100                                                                          
002200*    ABENDKODER:                                                          
002300*        U0016 - VID FELAKTIG ANGIVNING AV DATUM.                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*--- INFILER:                                                             
003200                                                                          
003300     SELECT IN-DATUMFIL              ASSIGN TO  W03002D1.                 
003400                                                                          
003500     SELECT IN-SYSINFIL              ASSIGN TO  W03002D2.                 
003600                                                                          
003700*--- UTFILER:                                                             
003800                                                                          
003900     SELECT UT-DATUMFIL              ASSIGN TO  W03002D3.                 
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400 FD  IN-DATUMFIL                                                          
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800 01  IN-FIL                      PIC X(80).                               
004900     EJECT                                                                
005000 FD  IN-SYSINFIL                                                          
005100     RECORDING      F                                                     
005200     BLOCK CONTAINS 0.                                                    
005300     SKIP2                                                                
005400 01  IN-SYSIN                    PIC X(80).                               
005500     EJECT                                                                
005600 FD  UT-DATUMFIL                                                          
005700     RECORDING      F                                                     
005800     BLOCK CONTAINS 0.                                                    
005900     SKIP2                                                                
006000 01  UT-FIL                      PIC X(80).                               
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400*    -COPY WY2000W4                                                       
006500     SKIP3                                                                
006600 77  JA                          PIC X(1)    VALUE 'J'.                   
006700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006800 77  FEL                         PIC X(1)    VALUE 'F'.                   
006900 77  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
007000 77  SYSIN-EOF                   PIC X(1)    VALUE 'N'.                   
007100 77  W-ANT-DAG                   PIC 9(3).                                
007200 77  W-SYSIN-SLUT-IOCS           PIC 9(5).                                
007300 77  W-SYSIN-START-IOCS          PIC 9(5).                                
007400 77  W-IN-SLUT-IOCS              PIC 9(5).                                
007500 77  W-DAGENS-IOCSDAT            PIC 9(5).                                
007600*                                                                         
007700 01  W-IN-DATUM.                                                          
007800     03  W-IN-DATUM-X            PIC X(4).                                
007900     03  W-IN-DATUM-N            PIC 9(1).                                
008000*                                                                         
008100 01  W-SYSIN-DATUM.                                                       
008200     03  W-SYSIN-DATUM-X         PIC X(4).                                
008300     03  W-SYSIN-DATUM-N         PIC 9(1).                                
008400*                                                                         
008500 01  W-ARSKIFTE.                                                          
008600     03  W-ARSKIFTE-AR           PIC 9(2).                                
008700     03  W-ARSKIFTE-DAG          PIC 9(3).                                
008800*                                                                         
008900 01  RETURKODER.                                                          
009000*                                                                         
009100     03  RKOD                    PIC S9(4)   COMP SYNC VALUE ZERO.        
009200     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP SYNC VALUE +16.         
009300     EJECT                                                                
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500*                                                                         
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009800     EJECT                                                                
009900 01  IN-AREA                     PIC X(80).                               
010000     SKIP3                                                                
010100 01  IN-DATUMKORT         REDEFINES IN-AREA.                              
010200     03  IN-DATUM                PIC X(5).                                
010300     03  IN-FAELT2               PIC X(6).                                
010400     03  FILLER                  PIC X(64).                               
010500     03  IN-IOCSDAT              PIC 9(5).                                
010600     EJECT                                                                
010700 01  SYSIN-AREA                  PIC X(80).                               
010800     SKIP3                                                                
010900 01  SYSIN-KORT           REDEFINES SYSIN-AREA.                           
011000     03  SYSIN-DATUM             PIC X(5).                                
011100     03  SYSIN-FAELT2            PIC X(6).                                
011200     03  FILLER                  PIC X(64).                               
011300     03  SYSIN-IOCSDAT           PIC 9(5).                                
011400     EJECT                                                                
011500 01  UT-AREA                     PIC X(80).                               
011600     SKIP3                                                                
011700 01  UT-DATUMKORT         REDEFINES UT-AREA.                              
011800     03  UT-DATUM                PIC X(5).                                
011900     03  UT-FAELT2               PIC X(6).                                
012000     03  FILLER                  PIC X(64).                               
012100     03  UT-IOCSDAT              PIC 9(5).                                
012200     EJECT                                                                
012300*01             -COPY WDATAREA.                                           
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700 01  EXEC-PARM.                                                           
012800     03  PARM-LENGD              PIC S9(4) COMP.                          
012900     03  PARM-IOCSDAT            PIC X(5).                                
013000     EJECT                                                                
013100 PROCEDURE DIVISION USING EXEC-PARM.                                      
013200                                                                          
013300                                                                          
013400 STYR SECTION.                                                            
013500                                                                          
013600     PERFORM A-INIT                                                       
013700     PERFORM S02-LAES-SYSIN                                               
013800     PERFORM S01-LAES-INFIL                                               
013900     PERFORM UNTIL                                                        
014000       IN-FAELT2 = HIGH-VALUE AND SYSIN-FAELT2 = HIGH-VALUE               
014100                                                                          
014200       EVALUATE TRUE                                                      
014300       WHEN SYSIN-FAELT2 < IN-FAELT2                                      
014400         PERFORM S03-SKRIV-SYSIN-KORT                                     
014500         PERFORM S02-LAES-SYSIN                                           
014600                                                                          
014700       WHEN SYSIN-FAELT2 = IN-FAELT2                                      
014800         PERFORM B-KONTROLLERA-MATCHNING                                  
014900         PERFORM S03-SKRIV-SYSIN-KORT                                     
015000         PERFORM S02-LAES-SYSIN                                           
015100                                                                          
015200       WHEN SYSIN-FAELT2 > IN-FAELT2                                      
015300         PERFORM S04-TESTA-SKRIV-INFIL                                    
015400         PERFORM S01-LAES-INFIL                                           
015500       END-EVALUATE                                                       
015600                                                                          
015700     END-PERFORM                                                          
015800     PERFORM Z-FINIT                                                      
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     OPEN INPUT  IN-DATUMFIL                                              
016600                 IN-SYSINFIL                                              
016700          OUTPUT UT-DATUMFIL                                              
016800                                                                          
016900     MOVE SPACE TO UT-AREA                                                
017000                                                                          
017100     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
017200     CALL WDATKONV USING DAT-KDDATFORM                                    
017300                         DAT-I-TIDATUM                                    
017400                         DAT-O-TIDATUM                                    
017500                         DAT-KDSVAR                                       
017600     MOVE DAT-TIAADDD TO W-DAGENS-IOCSDAT                                 
017700                                                                          
017800     EVALUATE TRUE                                                        
017900     WHEN PARM-IOCSDAT = 'SYSIN'                                          
018000         MOVE ZERO TO W-SYSIN-START-IOCS                                  
018100     WHEN PARM-IOCSDAT NOT NUMERIC OR PARM-IOCSDAT = ZERO                 
018200         MOVE W-DAGENS-IOCSDAT TO W-SYSIN-START-IOCS                      
018300     WHEN OTHER                                                           
018400         MOVE PARM-IOCSDAT TO W-SYSIN-START-IOCS                          
018500     END-EVALUATE                                                         
018600     .                                                                    
018700     EJECT                                                                
018800 B-KONTROLLERA-MATCHNING  SECTION.                                        
018900                                                                          
019000     MOVE W-IN-SLUT-IOCS  TO TMP1-YYDDD                                   
019100     MOVE SYSIN-IOCSDAT   TO TMP2-YYDDD                                   
019200     PERFORM WY2000P4                                                     
019300     PERFORM UNTIL SYSIN-FAELT2 NOT = IN-FAELT2                           
019400       OR TMP1-YYDDD >=  TMP2-YYDDD                                       
019500                                                                          
019600       PERFORM S04-TESTA-SKRIV-INFIL                                      
019700       PERFORM S01-LAES-INFIL                                             
019710                                                                          
019800       MOVE W-IN-SLUT-IOCS  TO TMP1-YYDDD                                 
019900       MOVE SYSIN-IOCSDAT   TO TMP2-YYDDD                                 
020000       PERFORM WY2000P4                                                   
020100     END-PERFORM                                                          
020200                                                                          
020300     MOVE IN-IOCSDAT      TO TMP1-YYDDD                                   
020400     MOVE SYSIN-IOCSDAT   TO TMP2-YYDDD                                   
020500     PERFORM WY2000P4                                                     
020600     IF SYSIN-FAELT2 = IN-FAELT2                                          
020700       AND TMP1-YYDDD < TMP2-YYDDD                                        
020710                                                                          
020800*        -- IN-POST ÖVERLAPPAR START AV SYSIN-POST                        
020900*        -- FÖRKORTA IN-POSTENS VARAKTIGHET                               
020910       MOVE SYSIN-IOCSDAT       TO TMP1-YYDDD                             
020920       MOVE IN-IOCSDAT          TO TMP2-YYDDD                             
020930       PERFORM WY2000P4                                                   
021000       COMPUTE W-ANT-DAG = TMP1-YYDDD - TMP2-YYDDD - 1                    
021100       IF W-ANT-DAG >= 635                                                
021110*        -- ÅRSSKARV -------------                                        
021200         SUBTRACT 635 FROM W-ANT-DAG                                      
021300       END-IF                                                             
021310                                                                          
021400       MOVE W-ANT-DAG TO W-IN-DATUM-N                                     
021500       MOVE W-IN-DATUM TO IN-DATUM                                        
021600       PERFORM S10-BERAKNA-IN-SLUT-IOCS                                   
021700       PERFORM S04-TESTA-SKRIV-INFIL                                      
021800       PERFORM S01-LAES-INFIL                                             
021900     END-IF                                                               
021901                                                                          
021910     MOVE W-IN-SLUT-IOCS      TO TMP1-YYDDD                               
021920     MOVE W-SYSIN-SLUT-IOCS   TO TMP2-YYDDD                               
021930     PERFORM WY2000P4                                                     
022100     PERFORM UNTIL SYSIN-FAELT2 NOT = IN-FAELT2                           
022200             OR TMP1-YYDDD >  TMP2-YYDDD                                  
022210                                                                          
022300       DISPLAY 'GAMMALT ' IN-FAELT2 ' '                                   
022400               IN-IOCSDAT '-' W-IN-SLUT-IOCS '  ERSATT MED NYTT'          
022500       PERFORM S01-LAES-INFIL                                             
022501                                                                          
022510       MOVE W-IN-SLUT-IOCS      TO TMP1-YYDDD                             
022520       MOVE W-SYSIN-SLUT-IOCS   TO TMP2-YYDDD                             
022530       PERFORM WY2000P4                                                   
022600     END-PERFORM                                                          
022610                                                                          
022700     MOVE IN-IOCSDAT          TO TMP1-YYDDD                               
022800     MOVE W-SYSIN-SLUT-IOCS   TO TMP2-YYDDD                               
022900     PERFORM WY2000P4                                                     
023000     IF SYSIN-FAELT2 = IN-FAELT2                                          
023100       AND TMP1-YYDDD NOT > TMP2-YYDDD                                    
023110                                                                          
023200*        -- IN-POST ÖVERLAPPAR SLUT AV SYSIN-POST                         
023300*        -- ÄNDRA IN-POSTENS START-IOCS OCH VARAKTIGHET,                  
023400*        -- MEN SKRIV INTE UT DEN ÄN, EFTERSOM NÄSTA                      
023500*        -- SYSIN-POST KANSKE OCKSÅ PÅVERKAR DENNA IN-POST                
023600       COMPUTE IN-IOCSDAT = W-SYSIN-SLUT-IOCS + 1                         
023610       MOVE W-IN-SLUT-IOCS      TO TMP1-YYDDD                             
023620       MOVE W-SYSIN-SLUT-IOCS   TO TMP2-YYDDD                             
023630       PERFORM WY2000P4                                                   
023700       COMPUTE W-ANT-DAG = TMP1-YYDDD - TMP2-YYDDD - 1                    
023800       IF W-ANT-DAG >= 635                                                
023810*        -- ÅRSSKARV -------------                                        
023900         SUBTRACT 635 FROM W-ANT-DAG                                      
024000       END-IF                                                             
024010                                                                          
024100       MOVE W-ANT-DAG TO W-IN-DATUM-N                                     
024200       MOVE W-IN-DATUM TO IN-DATUM                                        
024300       PERFORM S10-BERAKNA-IN-SLUT-IOCS                                   
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 Z-FINIT SECTION.                                                         
024800                                                                          
024900     CLOSE IN-DATUMFIL                                                    
025000           IN-SYSINFIL                                                    
025100           UT-DATUMFIL                                                    
025200     .                                                                    
025300     EJECT                                                                
025400 S01-LAES-INFIL  SECTION.                                                 
025500                                                                          
025600     READ IN-DATUMFIL INTO IN-AREA                                        
025700              AT END                                                      
025800                 MOVE JA TO INFIL-EOF                                     
025900                 MOVE HIGH-VALUE TO IN-FAELT2                             
026000     END-READ                                                             
026100                                                                          
026200     IF INFIL-EOF = NEJ                                                   
026300       MOVE IN-DATUM TO W-IN-DATUM                                        
026400       PERFORM S10-BERAKNA-IN-SLUT-IOCS                                   
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 S02-LAES-SYSIN  SECTION.                                                 
026900                                                                          
027000     READ IN-SYSINFIL INTO SYSIN-AREA                                     
027100              AT END                                                      
027200                 MOVE JA TO SYSIN-EOF                                     
027300                 MOVE HIGH-VALUE TO SYSIN-FAELT2                          
027400     END-READ                                                             
027500                                                                          
027600     IF SYSIN-EOF = NEJ                                                   
027700       IF W-SYSIN-START-IOCS NOT = ZERO                                   
027800         MOVE W-SYSIN-START-IOCS TO SYSIN-IOCSDAT                         
027900       END-IF                                                             
028000       MOVE SYSIN-DATUM TO W-SYSIN-DATUM                                  
028100       PERFORM S11-BERAKNA-SYSIN-SLUT-IOCS                                
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 S03-SKRIV-SYSIN-KORT   SECTION.                                          
028600                                                                          
028700     MOVE SYSIN-AREA TO UT-AREA                                           
028800     DISPLAY 'NYTT    ' SYSIN-FAELT2 ' '                                  
028900             SYSIN-IOCSDAT '-' W-SYSIN-SLUT-IOCS                          
029000                                                                          
029100     WRITE UT-FIL FROM UT-AREA                                            
029200     .                                                                    
029300                                                                          
029400     EJECT                                                                
029500 S04-TESTA-SKRIV-INFIL  SECTION.                                          
029600                                                                          
029700     MOVE W-IN-SLUT-IOCS     TO TMP1-YYDDD                                
029800     MOVE W-DAGENS-IOCSDAT   TO TMP2-YYDDD                                
029900     PERFORM WY2000P4                                                     
030000     IF TMP1-YYDDD < TMP2-YYDDD                                           
030100*        DÅ ÄR DATUMKORTET INAKTUELLT OCH SKA RENSAS                      
030200*        FRÅN DATUM-FILEN                                                 
030300       DISPLAY 'GAMMALT ' IN-FAELT2 ' '                                   
030400               IN-IOCSDAT '-' W-IN-SLUT-IOCS '  BORTTAGET'                
030500     ELSE                                                                 
030600       DISPLAY 'GAMMALT ' IN-FAELT2 ' '                                   
030700               IN-IOCSDAT '-' W-IN-SLUT-IOCS '  KVAR'                     
030800       MOVE IN-AREA TO UT-AREA                                            
030900       WRITE UT-FIL FROM UT-AREA                                          
031000     END-IF                                                               
031100     .                                                                    
031200                                                                          
031300     EJECT                                                                
031400 S10-BERAKNA-IN-SLUT-IOCS  SECTION.                                       
031500                                                                          
031700     COMPUTE W-IN-SLUT-IOCS = IN-IOCSDAT + W-IN-DATUM-N                   
031800     IF W-IN-SLUT-IOCS (3:3) > '365'                                      
031900*      -- ÅRSSKARV -------------                                          
032000       ADD 635 TO W-IN-SLUT-IOCS                                          
032100     END-IF                                                               
032200     .                                                                    
032300                                                                          
032400     SKIP3                                                                
032500 S11-BERAKNA-SYSIN-SLUT-IOCS  SECTION.                                    
032600                                                                          
032800     COMPUTE W-SYSIN-SLUT-IOCS = SYSIN-IOCSDAT + W-SYSIN-DATUM-N          
032900     IF W-SYSIN-SLUT-IOCS (3:3) > '365'                                   
033000*      -- ÅRSSKARV -------------                                          
033100       ADD 635 TO W-SYSIN-SLUT-IOCS                                       
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500*    -COPY WY2000P4                                                       
