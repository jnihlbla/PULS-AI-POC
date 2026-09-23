000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2712400.                                                
000400 AUTHOR.         STEFAN ÅSGÅRSDEN.                                        
000500 DATE-WRITTEN.   25 JUNI 2002.                                            
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        MATCHAR KOMPLETT VECKOFIL MED VISS REFILLDATA                    
001100*        MED EV. FÖRÄNDRINGAR UNDER VECKAN                                
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- EV. FÖRÄNDRINGAR UNDER VECKAN                              
002600     SELECT W271IN1                    ASSIGN TO W271IND1.                
002700     SKIP2                                                                
002800*          --- KOMPLETT VECKOFIL                                          
002900     SELECT W271IN2                    ASSIGN TO W271IND2.                
003000     SKIP2                                                                
003100*          --- KOMPLETT FIL FÖR EXTRAKTLAGERBANDEN                        
003200     SELECT W271UT1                    ASSIGN TO W271UTD1.                
003300*          --- KOMPLETT FIL FÖR EXTRAKTLAGERBANDEN                        
003400     SELECT W271UT2                    ASSIGN TO W271UTD2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W271IN1                                                              
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W27120   -PRE  IN1-  -L.                                  
004500                                                                          
004600     SKIP3                                                                
004700 FD  W271IN2                                                              
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W27120   -PRE  IN2-  -L.                                  
005200     SKIP3                                                                
005300 FD  W271UT1                                                              
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W27120   -PRE  UT-  -L.                                   
005800     EJECT                                                                
005900 FD  W271UT2                                                              
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W27120X  -PRE  UT2- -L.                                   
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600     SKIP2                                                                
006700                                                                          
006800*    -- CHECKED BY WY2000                                                 
006900 77  IDPGM                       PIC X(8)    VALUE 'W2712400'.            
007000 77  JA                          PIC X       VALUE 'J'.                   
007100 77  NEJ                         PIC X       VALUE 'N'.                   
007200 77  FL-USA-FINNS                PIC X.                                   
007300 77  WS-FLSALDO-USA              PIC X.                                   
007400 77  WS-FLSALDO-CAN              PIC X.                                   
007500 77  WS-FLERSATT-USA             PIC X.                                   
007600 77  WS-FLERSATT-CAN             PIC X.                                   
007700 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
007800 77  WS-FL-TYP1                  PIC X.                                   
007900     SKIP2                                                                
008000 01 WS-KDERS-NUM                PIC 9(2).                                 
008100 01 WS-KDERS                             REDEFINES WS-KDERS-NUM.          
008200     03 WS-KDERSPOS1            PIC X.                                    
008300     03 WS-KDERSPOS2            PIC X.                                    
008400                                                                          
008500 01  WS-IDARTNR                 PIC S9(9)  VALUE ZERO COMP-3.             
008600 01  WS-IDUSER                  PIC X(8)   VALUE SPACE.                   
008700 01  WS-DASTADAT-9KOMPL         PIC S9(9)  VALUE ZERO COMP-3.             
008800 01  WS-ANTAL-ARTIKEL-C         PIC 9(9)   VALUE ZERO.                    
008900 01  WS-ANTAL-DISPLAY           PIC 9(9)   VALUE ZERO.                    
009000                                                                          
009100 01  WS-IDARTNR-TILLK.                                                    
009200     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
009300     03 WS-IDARTNR-TILLK-NUM     PIC 9(9).                                
009400                                                                          
009500 01  WS-TABELL.                                                           
009600     03 WS-TAB-ARTNR             PIC S9(9) VALUE ZERO                     
009700                                 OCCURS 50.                               
009800     03 WS-TAB-MAX               PIC 9(2)  VALUE ZERO.                    
010400 01 WS-ANTAL-DUBBLETTER         PIC 9(9)   VALUE ZERO.                    
010500 01 WS-ANTAL-IN1                PIC 9(9)   VALUE ZERO.                    
010600 01 WS-ANTAL-IN2                PIC 9(9)   VALUE ZERO.                    
010700 01 WS-ANTAL-MATCH              PIC 9(9)   VALUE ZERO.                    
010800 01 WS-ADPLATS.                                                           
010900  03 WS-SEKTION                 PIC 9(3)   VALUE ZERO.                    
011000  03 WS-NIVA                    PIC 9      VALUE ZERO.                    
011100  03 WS-PLATS                   PIC 9      VALUE ZERO.                    
011200                                                                          
011300                                                                          
011400 01  FELTEXT.                                                             
011500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011700                                                                          
011800 77  W271IN1-EOF-SW              PIC X       VALUE 'N'.                   
011900     88  END-OF-W271IN1                      VALUE 'J'.                   
012000                                                                          
012100 77  W271IN2-EOF-SW              PIC X       VALUE 'N'.                   
012200     88  END-OF-W271IN2                      VALUE 'J'.                   
012300                                                                          
012400     EJECT                                                                
012500 01  DYNAMISKA-SUBPROGRAM.                                                
012600*                                                                         
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013100     EJECT                                                                
013200*    ---- PARAMETRAR TILL WDATKONV                                        
013300 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
013400*01  -COPY WDATAREA                                                       
013500*    --- PARAMETRAR TILL POSTSUM                                          
013600*                                                                         
013700*01  -COPY W0005   -PRE  POSTSUM-                                         
013800     EJECT                                                                
013900 01  IN1-AREA-START               PIC X(24)   VALUE                       
014000                                             'IN1-AREA-START'.            
014100     SKIP2                                                                
014200                                                                          
014300*01  AREA -COPY W27120       -PRE IN1-                                    
014400                                                                          
014500     EJECT                                                                
014600 01  IN2-AREA-START               PIC X(24)   VALUE                       
014700                                             'IN2-AREA-START'.            
014800     SKIP2                                                                
014900                                                                          
015000                                                                          
015100*01  AREA -COPY W27120       -PRE IN2-                                    
015200                                                                          
015300     EJECT                                                                
015400 01  UT-AREA-START               PIC X(24)   VALUE                        
015500                                             'UT-AREA-START'.             
015600     SKIP2                                                                
015700*01  AREA -COPY W27120       -PRE UT-                                     
015800                                                                          
015900     EJECT                                                                
015910 01  UT2-AREA-START              PIC X(24)   VALUE                        
015920                                             'UT2-AREA-START'.            
015930     SKIP2                                                                
015940*01  AREA -COPY W27120X      -PRE UT2-                                    
015950                                                                          
015960     EJECT                                                                
016000 LINKAGE SECTION.                                                         
016100                                                                          
016200 PROCEDURE DIVISION.                                                      
016300                                                                          
016400     SKIP2                                                                
016500     PERFORM A-INIT                                                       
016600     PERFORM S01-LAES-W271IN1                                             
016700     PERFORM S02-LAES-W271IN2                                             
016800                                                                          
016900     PERFORM UNTIL END-OF-W271IN1                                         
017000     AND           END-OF-W271IN2                                         
017100       PERFORM B-BEARBETA                                                 
017200     END-PERFORM                                                          
017300                                                                          
017400     PERFORM Z-FINIT                                                      
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100                                                                          
018200     OPEN INPUT  W271IN1                                                  
018300                 W271IN2                                                  
018400                                                                          
018500     OPEN OUTPUT W271UT1                                                  
018600                 W271UT2                                                  
018700                                                                          
018800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018900     .                                                                    
019000     EJECT                                                                
019100 B-BEARBETA SECTION.                                                      
019200                                                                          
019300     IF END-OF-W271IN1                                                    
019400     OR  IN1-IDARTNR > IN2-IDARTNR                                        
019500     OR (IN1-IDARTNR = IN2-IDARTNR                                        
019600     AND IN1-IDDC    > IN2-IDDC)                                          
019700       MOVE IN2-AREA         TO UT-AREA                                   
019800       PERFORM S11-SKRIV-W271UT1                                          
019810       PERFORM S12-SKRIV-W271UT2                                          
019900       ADD 1                 TO WS-ANTAL-IN2                              
020000       PERFORM S02-LAES-W271IN2                                           
020100                                                                          
020200     ELSE                                                                 
020300       IF END-OF-W271IN2                                                  
020400       OR  IN2-IDARTNR > IN1-IDARTNR                                      
020500       OR (IN2-IDARTNR = IN1-IDARTNR                                      
020600       AND IN2-IDDC    > IN1-IDDC)                                        
020700                                                                          
020800         MOVE IN1-AREA       TO UT-AREA                                   
020900         PERFORM S11-SKRIV-W271UT1                                        
020910         PERFORM S12-SKRIV-W271UT2                                        
021000         ADD 1               TO WS-ANTAL-IN1                              
021100         PERFORM S01-LAES-W271IN1                                         
021200                                                                          
021300       ELSE                                                               
021400*                                                                         
021500***** MATCHNING                                                           
021600*                                                                         
021700                                                                          
021800         MOVE IN1-AREA       TO UT-AREA                                   
021900         PERFORM S11-SKRIV-W271UT1                                        
021910         PERFORM S12-SKRIV-W271UT2                                        
022000         ADD 1               TO WS-ANTAL-MATCH                            
022100         PERFORM S01-LAES-W271IN1                                         
022200         PERFORM S02-LAES-W271IN2                                         
022300       END-IF                                                             
022400     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700                                                                          
022800 Z-FINIT SECTION.                                                         
022900                                                                          
023200     DISPLAY 'ENBART FRÅN DAGFIL   : '    WS-ANTAL-IN1                    
023300     DISPLAY 'ENBART FRÅN VECKOFIL : '    WS-ANTAL-IN2                    
023400     DISPLAY 'MATCH                : '    WS-ANTAL-MATCH                  
023510                                                                          
023520     MOVE 'S' TO POSTSUM-OPKOD                                            
023530     CALL POSTSUM USING POSTSUM-PARM                                      
023540                                                                          
023600     CLOSE W271IN1                                                        
023700           W271IN2                                                        
023800           W271UT1                                                        
023810           W271UT2                                                        
023900     .                                                                    
024000     EJECT                                                                
024100 S01-LAES-W271IN1 SECTION.                                                
024200                                                                          
024300     READ W271IN1         INTO IN1-AREA                                   
024400     AT END                                                               
024500        SET END-OF-W271IN1 TO TRUE                                        
024600        MOVE 999999999    TO IN1-IDARTNR                                  
024700     NOT AT END                                                           
024800        MOVE 'W27124'   TO POSTSUM-FDNAMN                                 
024810        MOVE 'W271IND1' TO POSTSUM-DDNAMN2                                
024820        CALL POSTSUM USING POSTSUM-PARM                                   
025000     END-READ                                                             
025100     .                                                                    
025200     EJECT                                                                
025300 S02-LAES-W271IN2 SECTION.                                                
025400                                                                          
025500     READ W271IN2         INTO IN2-AREA                                   
025600     AT END                                                               
025700        SET END-OF-W271IN2 TO TRUE                                        
025800        MOVE 999999999    TO IN2-IDARTNR                                  
025900     NOT AT END                                                           
025910        MOVE 'W27124'   TO POSTSUM-FDNAMN                                 
025920        MOVE 'W271IND2' TO POSTSUM-DDNAMN2                                
025930        CALL POSTSUM USING POSTSUM-PARM                                   
026200     END-READ                                                             
026300     .                                                                    
026400     EJECT                                                                
026500 S11-SKRIV-W271UT1 SECTION.                                               
026600                                                                          
026700     WRITE UT-POST        FROM UT-AREA                                    
026900                                                                          
026920     MOVE 'W27124'   TO POSTSUM-FDNAMN                                    
026930     MOVE 'W271UTD1' TO POSTSUM-DDNAMN2                                   
026940     CALL POSTSUM USING POSTSUM-PARM                                      
027000     .                                                                    
027010                                                                          
027100 S12-SKRIV-W271UT2 SECTION.                                               
027200                                                                          
027210     MOVE UT-IDARTNR  TO UT2-IDARTNR                                      
027220     MOVE UT-IDDC     TO UT2-IDDC                                         
027230     MOVE UT-KDPRODSL TO UT2-KDPRODSL                                     
027240     MOVE UT-KDREFSTA TO UT2-KDREFSTA                                     
027241     MOVE UT-KLASS    TO UT2-KLASS                                        
027242     MOVE UT-IDREFTAB TO UT2-IDREFTAB                                     
027243     MOVE UT-FLWILSON TO UT2-FLWILSON                                     
027244     MOVE UT-PRARTBES TO UT2-PRARTBES                                     
027245     MOVE UT-PRARTSTD TO UT2-PRARTSTD                                     
027246     MOVE UT-PRMATRL  TO UT2-PRMATRL                                      
027250                                                                          
027300     WRITE UT2-POST        FROM UT2-AREA                                  
027500                                                                          
027520     MOVE 'W27124'   TO POSTSUM-FDNAMN                                    
027530     MOVE 'W271UTD2' TO POSTSUM-DDNAMN2                                   
027540     CALL POSTSUM USING POSTSUM-PARM                                      
027600     .                                                                    
