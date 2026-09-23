000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMW2000.                                                
000400*AUTHOR.         KARIN OLSSON.                                            
000500*DATE-WRITTEN.   91/12/09.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PLOCKA UT DATA FRÅN ETT COBOL-PROGRAM. DATAT ANVÄNDS FÖR         
001100*        ATT SKAPA FÖDDATA TILL DMR.                                      
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ETT COBOLPROGRAM                                           
002700     SELECT RCOBIN                     ASSIGN TO WDMR20D1.                
002800*          --- DATA FÖR ATT SKAPA FÖDDATA TILL DMR                        
002900     SELECT RCOBUT                     ASSIGN TO WDMR20D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  RCOBIN                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     EJECT                                                                
003900 01  FILLER          PIC X(80).                                           
004000     SKIP3                                                                
004100 FD  RCOBUT                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     EJECT                                                                
004500 01  UT-POST         PIC X(128).                                          
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004801                                                                          
004810*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'WDMW2000'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 77  GEMENER                     PIC X(29)                                
005400                value 'abcdefghijklmnopqrstuvwxyzåäö'.                    
005500 77  VERSALER                    PIC X(29)                                
005600                VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.                    
005700 01  WS-SECTION                  PIC X       VALUE 'N'.                   
005900 01  END-OF-OPEN-SATS            PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  RCOBIN-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-RCOBIN                       VALUE 'J'.                   
006300 01  ORD-TYP                     PIC X       VALUE SPACE.                 
006400 01  RAD.                                                                 
006500     03  FILLER                  PIC X(3).                                
006600     03  END-COPY                PIC X(10).                               
006700     03  FILLER                  PIC X(11).                               
006800     03  LENGTH-X                PIC X(8).                                
006900     03  FILLER                  PIC X(5).                                
007000     03  OLD-LENGTH-X            PIC X(12).                               
007100     03  FILLER                  PIC X(17).                               
007200 01  RADX2                       PIC X(2).                                
007300 01  RADX20                      PIC X(20).                               
007400 01  ORD1                        PIC X(20)   VALUE SPACE.                 
007500 01  ORD2                        PIC X(20)   VALUE SPACE.                 
007600 01  ORD3                        PIC X(20)   VALUE SPACE.                 
007700 01  ORD4                        PIC X(20)   VALUE SPACE.                 
007800 01  ORD5                        PIC X(20)   VALUE SPACE.                 
007900 01  ORD6                        PIC X(20)   VALUE SPACE.                 
008000 01  ORD7                        PIC X(20)   VALUE SPACE.                 
008100 01  ORD8                        PIC X(20)   VALUE SPACE.                 
008200 01  ORDX4                       PIC X(4)    VALUE SPACE.                 
008300                                                                          
008400 01  OPEN-TYP                    PIC X(6)    VALUE SPACE.                 
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE                        
008700                                            'IN-AREA-START'.              
008800 01  IN-AREA.                                                             
008900     03  FILLER                  PIC X(6).                                
009000     03  IN-RAD                  PIC X(66).                               
009100     03  FILLER                  PIC X(8).                                
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE                        
009400                                            'UT-AREA-START'.              
009500 01  UT-AREA.                                                             
009600     03  UT-TYP                  PIC X(8)    VALUE SPACE.                 
009700     03  UT-DATA                 PIC X(120)  VALUE SPACE.                 
009800 01  UT-AREA2  REDEFINES UT-AREA.                                         
009900     03  FILLER                  PIC X(8).                                
010000     03  UT-DATA-ORD1            PIC X(20).                               
010100     03  UT-DATA-ORD2            PIC X(20).                               
010200     03  UT-DATA-ORD3            PIC X(20).                               
010300     03  UT-DATA-ORD4            PIC X(20).                               
010400     03  UT-DATA-ORD5            PIC X(20).                               
010500     03  UT-DATA-ORD6            PIC X(20).                               
010600     SKIP2                                                                
010700 77  REM-TYP                     PIC X(8)    VALUE 'REM'.                 
010800 77  SEL-TYP                     PIC X(8)    VALUE 'SEL'.                 
010900 77  FD-TYP                      PIC X(8)    VALUE 'FD'.                  
011000 77  SD-TYP                      PIC X(8)    VALUE 'SD'.                  
011100 77  FILCTX-TYP                  PIC X(8)    VALUE 'FILCTX'.              
011200 77  OPNIN-TYP                   PIC X(8)    VALUE 'OPNIN'.               
011300 77  OPNOUT-TYP                  PIC X(8)    VALUE 'OPNOUT'.              
011400 77  CALL-TYP                    PIC X(8)    VALUE 'CALL'.                
011500 77  CTX-TYP                     PIC X(8)    VALUE 'CTX'.                 
011600 77  DB2-TYP                     PIC X(8)    VALUE 'DB2'.                 
011700 77  CTXMEM-TYP                  PIC X(8)    VALUE 'CTXMEM'.              
011800 77  USNG-TYP                    PIC X(8)    VALUE 'USING'.               
011900 77  GIV-TYP                     PIC X(8)    VALUE 'GIVING'.              
012000 77  W-END-COPY                  PIC X(10)   VALUE ' END COPY '.          
012100 77  W-LENGTH-X                  PIC X(8)    VALUE ' LENGTH='.            
012200 77  W-OLD-LENGTH-X              PIC X(12)   VALUE ' OLD LENGTH='.        
012300     EJECT                                                                
012400 01  DYNAMISKA-SUBPROGRAM.                                                
012500*                                                                         
012600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012700     03  WCOBORD                 PIC X(8)    VALUE 'WCOBORD'.             
012800     SKIP2                                                                
012900*    --- PARAMETRAR TILL ABEND                                            
013000                                                                          
013100 77  RKOD                        PIC S9(4)   COMP VALUE +0.               
013200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013400     SKIP2                                                                
013500 01  FELTEXT.                                                             
013600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013800     EJECT                                                                
013900 PROCEDURE DIVISION.                                                      
014000     SKIP2                                                                
014100     PERFORM A-INIT                                                       
014200     PERFORM S01-LAES-RCOBIN                                              
014300     PERFORM UNTIL END-OF-RCOBIN                                          
014400                                                                          
014500       IF RADX20 NOT = '*' AND RADX2 NOT = '*-'                           
014600         INSPECT RAD CONVERTING GEMENER TO VERSALER                       
014700         CALL WCOBORD USING ORD-TYP RAD                                   
014800                   ORD1 ORD2 ORD3 ORD4 ORD5 ORD6 ORD7 ORD8                
014900                                                                          
015000         IF ORD-TYP NOT = SPACE                                           
015100           EVALUATE TRUE                                                  
015200                                                                          
015300             WHEN ORD-TYP = 'S'                                           
015400               IF RADX2 NOT = '*' AND '**' AND WS-SECTION = NEJ           
015500                 PERFORM B-ANALYSERA-SELECT                               
015600               END-IF                                                     
015700               PERFORM S01-LAES-RCOBIN                                    
015800                                                                          
015900             WHEN ORD-TYP = 'F'                                           
016000               IF RADX2 NOT = '*' AND '**'                                
016100                 PERFORM C-ANALYSERA-FD                                   
016200               END-IF                                                     
016300               PERFORM S01-LAES-RCOBIN                                    
016400                                                                          
016500             WHEN ORD-TYP = 'A'                                           
016600               IF RADX2 NOT = '*' AND '**'                                
016700                 PERFORM D-ANALYSERA-SD                                   
016800               END-IF                                                     
016900               PERFORM S01-LAES-RCOBIN                                    
017000                                                                          
017100             WHEN ORD-TYP = 'C'                                           
017200               IF ORD1 NOT = '*++INCLUDE' AND '++INCLUDE'                 
017300                 PERFORM E-ANALYSERA-COPY                                 
017400               END-IF                                                     
017500               PERFORM S01-LAES-RCOBIN                                    
017600                                                                          
017700             WHEN ORD-TYP = 'W'                                           
017800               IF RADX2 NOT = '*' AND '**'                                
017900                 MOVE JA TO WS-SECTION                                    
018000               END-IF                                                     
018100               PERFORM S01-LAES-RCOBIN                                    
018200                                                                          
018300             WHEN ORD-TYP = 'I' OR 'O'                                    
018400               IF RADX2 NOT = '*' AND '**'                                
018500                                                                          
018600                 IF ORD2 = 'PROCEDURE' OR ORD3 = 'PROCEDURE'              
018700                 OR ORD4 = 'PROCEDURE' OR ORD5 = 'PROCEDURE'              
018800                   PERFORM S01-LAES-RCOBIN                                
018900                 ELSE                                                     
019000                   PERFORM F-ANALYSERA-OPEN                               
019100                 END-IF                                                   
025200                                                                          
031600               ELSE                                                       
031700                 PERFORM S01-LAES-RCOBIN                                  
031800               END-IF                                                     
044300                                                                          
044400             WHEN ORD-TYP = 'P'                                           
044500               IF RADX2 NOT = '*' AND '**'                                
044510                 PERFORM G-ANALYSERA-CALL                                 
046800               END-IF                                                     
046900               PERFORM S01-LAES-RCOBIN                                    
047000                                                                          
047100             WHEN ORD-TYP = 'D'                                           
047200               IF RADX2 NOT = '*' AND '**'                                
047210                 PERFORM H-ANALYSERA-DB2                                  
047600               END-IF                                                     
047700               PERFORM S01-LAES-RCOBIN                                    
047800                                                                          
047900             WHEN ORD-TYP = 'U'                                           
048000               IF RADX2 NOT = '*' AND '**'                                
048010                 PERFORM I-ANALYSERA-USING                                
050200               END-IF                                                     
050300               PERFORM S01-LAES-RCOBIN                                    
050400                                                                          
050500             WHEN ORD-TYP = 'G'                                           
050600               IF RADX2 NOT = '*' AND '**'                                
050610                 PERFORM J-ANALYSERA-GIVING                               
052400               END-IF                                                     
052500               PERFORM S01-LAES-RCOBIN                                    
052600                                                                          
052700             WHEN OTHER                                                   
052800               DISPLAY 'Unknown type of line: ' ORD-TYP                   
052900               PERFORM S99-ABEND                                          
053000           END-EVALUATE                                                   
053100                                                                          
053200         ELSE                                                             
053300           IF WS-SECTION = NEJ                                            
053400             IF END-COPY = W-END-COPY AND LENGTH-X = W-LENGTH-X           
053500                 AND OLD-LENGTH-X = W-OLD-LENGTH-X                        
053600               MOVE CTXMEM-TYP TO UT-TYP                                  
053700               MOVE SPACE TO UT-DATA                                      
053800               PERFORM S11-SKRIV-RCOBUT                                   
053900             END-IF                                                       
054000           END-IF                                                         
054100           PERFORM S01-LAES-RCOBIN                                        
054200         END-IF                                                           
054300       ELSE                                                               
054400         PERFORM S01-LAES-RCOBIN                                          
054500       END-IF                                                             
054600     END-PERFORM                                                          
054700                                                                          
054800     PERFORM Z-FINIT                                                      
054900                                                                          
055000     MOVE RKOD TO RETURN-CODE                                             
055100     GOBACK                                                               
055200     .                                                                    
055300     EJECT                                                                
055400 A-INIT SECTION.                                                          
055500     SKIP2                                                                
055600     OPEN INPUT  RCOBIN                                                   
055700     OPEN OUTPUT RCOBUT                                                   
055800     MOVE ZERO TO RKOD                                                    
055900     .                                                                    
056000     EJECT                                                                
056100 B-ANALYSERA-SELECT  SECTION.                                             
056200     SKIP2                                                                
056300     MOVE SEL-TYP TO UT-TYP                                               
056400     MOVE SPACE TO UT-DATA                                                
056500     MOVE ORD2 TO UT-DATA-ORD1                                            
056600     IF ORD4 = 'TO'                                                       
056700       MOVE ORD5 TO UT-DATA-ORD2                                          
056800     ELSE                                                                 
056900       MOVE ORD4 TO UT-DATA-ORD2                                          
057000     END-IF                                                               
057100     PERFORM S11-SKRIV-RCOBUT                                             
057200     .                                                                    
057300     EJECT                                                                
057400 C-ANALYSERA-FD  SECTION.                                                 
057500     SKIP2                                                                
057600     MOVE FD-TYP TO UT-TYP                                                
057700     MOVE SPACE TO UT-DATA                                                
057800     MOVE ORD2 TO UT-DATA-ORD1                                            
057900     PERFORM S11-SKRIV-RCOBUT                                             
058000     .                                                                    
058100     EJECT                                                                
058200 D-ANALYSERA-SD  SECTION.                                                 
058300     SKIP2                                                                
058400     MOVE SD-TYP TO UT-TYP                                                
058500     MOVE SPACE TO UT-DATA                                                
058600     MOVE ORD2 TO UT-DATA-ORD1                                            
058700     PERFORM S11-SKRIV-RCOBUT                                             
058800     .                                                                    
058900     EJECT                                                                
059000 E-ANALYSERA-COPY  SECTION.                                               
059100     SKIP2                                                                
059200     IF WS-SECTION = JA                                                   
059300       MOVE CTX-TYP TO UT-TYP                                             
059400     ELSE                                                                 
059500       MOVE FILCTX-TYP TO UT-TYP                                          
059600     END-IF                                                               
059700     MOVE SPACE TO UT-DATA                                                
059800     EVALUATE TRUE                                                        
059900       WHEN ORD1 = '-COPY'                                                
060000         MOVE ORD2 TO UT-DATA-ORD1                                        
060100         MOVE ORD4 TO UT-DATA-ORD2                                        
060200                                                                          
060300       WHEN ORD2 = '-COPY'                                                
060400         MOVE ORD3 TO UT-DATA-ORD1                                        
060500         MOVE ORD5 TO UT-DATA-ORD2                                        
060600                                                                          
060700       WHEN ORD3 = '-COPY'                                                
060800         MOVE ORD4 TO UT-DATA-ORD1                                        
060900         MOVE ORD6 TO UT-DATA-ORD2                                        
061000                                                                          
061100       WHEN ORD4 = '-COPY'                                                
061200         MOVE ORD5 TO UT-DATA-ORD1                                        
061300         MOVE ORD7 TO UT-DATA-ORD2                                        
061400                                                                          
061500       WHEN ORD5 = '-COPY'                                                
061600         MOVE ORD6 TO UT-DATA-ORD1                                        
061700         MOVE ORD8 TO UT-DATA-ORD2                                        
061800                                                                          
061900       WHEN ORD6 = '-COPY'                                                
062000         MOVE ORD7  TO UT-DATA-ORD1                                       
062100         MOVE SPACE TO UT-DATA-ORD2                                       
062200                                                                          
062300       WHEN ORD7 = '-COPY'                                                
062400         MOVE ORD8  TO UT-DATA-ORD1                                       
062500         MOVE SPACE TO UT-DATA-ORD2                                       
062600                                                                          
062700       WHEN OTHER                                                         
062800         MOVE 'UNKNOWN' TO UT-DATA-ORD1                                   
062900         MOVE SPACE     TO UT-DATA-ORD2                                   
063000     END-EVALUATE                                                         
063100     PERFORM S11-SKRIV-RCOBUT                                             
063200     .                                                                    
063300     EJECT                                                                
063400 F-ANALYSERA-OPEN  SECTION.                                               
063500     SKIP2                                                                
063600     IF ORD-TYP = 'I'                                                     
063700       MOVE OPNIN-TYP TO UT-TYP                                           
063800       MOVE 'INPUT' TO OPEN-TYP                                           
063900     ELSE                                                                 
064000       MOVE OPNOUT-TYP TO UT-TYP                                          
064100       MOVE 'OUTPUT' TO OPEN-TYP                                          
064200     END-IF                                                               
064300     MOVE SPACE TO UT-DATA                                                
064400     PERFORM FA-ANALYSERA-OPEN-RAD                                        
064500                                                                          
064600     PERFORM S01-LAES-RCOBIN                                              
064700     INSPECT RAD CONVERTING GEMENER TO VERSALER                           
064800     MOVE NEJ TO END-OF-OPEN-SATS                                         
064900     PERFORM UNTIL END-OF-RCOBIN OR RAD = SPACE                           
065000             OR END-OF-OPEN-SATS = JA                                     
065100       IF RADX2 NOT = '* ' AND '**' AND '*-'                              
065200         MOVE 'U' TO ORD-TYP                                              
065300         CALL WCOBORD USING ORD-TYP RAD                                   
065400            ORD1 ORD2 ORD3 ORD4 ORD5 ORD6 ORD7 ORD8                       
065500         MOVE ORD1 TO ORDX4                                               
065600         EVALUATE TRUE                                                    
065700           WHEN ORD-TYP NOT = SPACE                                       
065800             MOVE JA TO END-OF-OPEN-SATS                                  
065900                                                                          
066000           WHEN ORDX4 = 'SKIP'                                            
066100             MOVE JA TO END-OF-OPEN-SATS                                  
066200                                                                          
066300           WHEN ORD1 = '.' OR 'EJECT' OR 'PERFORM'                        
066400               OR 'IF' OR 'ELSE' OR 'END-IF'                              
066500               OR 'READ' OR 'WRITE' OR 'MOVE'                             
066600               OR 'PERFORM' OR 'END-PERFORM'                              
066700               OR 'EVALUATE' OR 'END-EVALUATE'                            
066800               OR 'STRING' OR 'UNSTRING' OR 'GOBACK'                      
066900               OR 'INSPECT' OR 'CALL' OR 'WHEN'                           
066910               OR 'SORT' OR 'SEARCH' OR 'SET'                             
067000             MOVE JA TO END-OF-OPEN-SATS                                  
067100                                                                          
067200           WHEN ORD2 = 'SECTION.'                                         
067300             MOVE JA TO END-OF-OPEN-SATS                                  
067400                                                                          
067500           WHEN OTHER                                                     
067600             PERFORM FB-ANALYSERA-OPEN-RAD                                
070300             PERFORM S01-LAES-RCOBIN                                      
070400             INSPECT RAD CONVERTING GEMENER                               
070500                      TO VERSALER                                         
070600                                                                          
070700         END-EVALUATE                                                     
070800       ELSE                                                               
070900         PERFORM S01-LAES-RCOBIN                                          
071000         INSPECT RAD CONVERTING GEMENER TO VERSALER                       
071100       END-IF                                                             
071200     END-PERFORM                                                          
071400     .                                                                    
071500     EJECT                                                                
071600 FA-ANALYSERA-OPEN-RAD  SECTION.                                          
071700     SKIP2                                                                
071800     EVALUATE TRUE                                                        
071900       WHEN ORD1 = OPEN-TYP                                               
072000         IF ORD2 NOT = SPACE                                              
072100           MOVE ORD2  TO UT-DATA-ORD1                                     
072200           PERFORM S11-SKRIV-RCOBUT                                       
072300         END-IF                                                           
072400         IF ORD3 NOT = SPACE                                              
072500           MOVE ORD3  TO UT-DATA-ORD1                                     
072600           PERFORM S11-SKRIV-RCOBUT                                       
072700         END-IF                                                           
072800         IF ORD4 NOT = SPACE                                              
072900           MOVE ORD4  TO UT-DATA-ORD1                                     
073000           PERFORM S11-SKRIV-RCOBUT                                       
073100         END-IF                                                           
073200         IF ORD5 NOT = SPACE                                              
073300           MOVE ORD5  TO UT-DATA-ORD1                                     
073400           PERFORM S11-SKRIV-RCOBUT                                       
073500         END-IF                                                           
073600         IF ORD6 NOT = SPACE                                              
073700           MOVE ORD6  TO UT-DATA-ORD1                                     
073800           PERFORM S11-SKRIV-RCOBUT                                       
073900         END-IF                                                           
073910         IF ORD7 NOT = SPACE                                              
073920           MOVE ORD7  TO UT-DATA-ORD1                                     
073930           PERFORM S11-SKRIV-RCOBUT                                       
073940         END-IF                                                           
073950         IF ORD8 NOT = SPACE                                              
073960           MOVE ORD8  TO UT-DATA-ORD1                                     
073970           PERFORM S11-SKRIV-RCOBUT                                       
073980         END-IF                                                           
074000                                                                          
074100       WHEN ORD2 = OPEN-TYP                                               
074200         IF ORD3 NOT = SPACE                                              
074300           MOVE ORD3  TO UT-DATA-ORD1                                     
074400           PERFORM S11-SKRIV-RCOBUT                                       
074500         END-IF                                                           
074600         IF ORD4 NOT = SPACE                                              
074700           MOVE ORD4  TO UT-DATA-ORD1                                     
074800           PERFORM S11-SKRIV-RCOBUT                                       
074900         END-IF                                                           
075000         IF ORD5 NOT = SPACE                                              
075100           MOVE ORD5  TO UT-DATA-ORD1                                     
075200           PERFORM S11-SKRIV-RCOBUT                                       
075300         END-IF                                                           
075400         IF ORD6 NOT = SPACE                                              
075500           MOVE ORD6  TO UT-DATA-ORD1                                     
075600           PERFORM S11-SKRIV-RCOBUT                                       
075700         END-IF                                                           
075710         IF ORD7 NOT = SPACE                                              
075720           MOVE ORD7  TO UT-DATA-ORD1                                     
075730           PERFORM S11-SKRIV-RCOBUT                                       
075740         END-IF                                                           
075750         IF ORD8 NOT = SPACE                                              
075760           MOVE ORD8  TO UT-DATA-ORD1                                     
075770           PERFORM S11-SKRIV-RCOBUT                                       
075780         END-IF                                                           
075800                                                                          
075900       WHEN ORD3 = OPEN-TYP                                               
076000         IF ORD4 NOT = SPACE                                              
076100           MOVE ORD4  TO UT-DATA-ORD1                                     
076200           PERFORM S11-SKRIV-RCOBUT                                       
076300         END-IF                                                           
076400         IF ORD5 NOT = SPACE                                              
076500           MOVE ORD5  TO UT-DATA-ORD1                                     
076600           PERFORM S11-SKRIV-RCOBUT                                       
076700         END-IF                                                           
076800         IF ORD6 NOT = SPACE                                              
076900           MOVE ORD6  TO UT-DATA-ORD1                                     
077000           PERFORM S11-SKRIV-RCOBUT                                       
077100         END-IF                                                           
077110         IF ORD7 NOT = SPACE                                              
077120           MOVE ORD7  TO UT-DATA-ORD1                                     
077130           PERFORM S11-SKRIV-RCOBUT                                       
077140         END-IF                                                           
077150         IF ORD8 NOT = SPACE                                              
077160           MOVE ORD8  TO UT-DATA-ORD1                                     
077170           PERFORM S11-SKRIV-RCOBUT                                       
077180         END-IF                                                           
077200                                                                          
077300       WHEN ORD4 = OPEN-TYP                                               
077400         IF ORD5 NOT = SPACE                                              
077500           MOVE ORD5  TO UT-DATA-ORD1                                     
077600           PERFORM S11-SKRIV-RCOBUT                                       
077700         END-IF                                                           
077800         IF ORD6 NOT = SPACE                                              
077900           MOVE ORD6  TO UT-DATA-ORD1                                     
078000           PERFORM S11-SKRIV-RCOBUT                                       
078100         END-IF                                                           
078110         IF ORD7 NOT = SPACE                                              
078120           MOVE ORD7  TO UT-DATA-ORD1                                     
078130           PERFORM S11-SKRIV-RCOBUT                                       
078140         END-IF                                                           
078150         IF ORD8 NOT = SPACE                                              
078160           MOVE ORD8  TO UT-DATA-ORD1                                     
078170           PERFORM S11-SKRIV-RCOBUT                                       
078180         END-IF                                                           
078200                                                                          
078300       WHEN OTHER                                                         
078400         IF ORD6 NOT = SPACE                                              
078500           MOVE ORD6  TO UT-DATA-ORD1                                     
078600           PERFORM S11-SKRIV-RCOBUT                                       
078700         END-IF                                                           
078710         IF ORD7 NOT = SPACE                                              
078720           MOVE ORD7  TO UT-DATA-ORD1                                     
078730           PERFORM S11-SKRIV-RCOBUT                                       
078740         END-IF                                                           
078750         IF ORD8 NOT = SPACE                                              
078760           MOVE ORD8  TO UT-DATA-ORD1                                     
078770           PERFORM S11-SKRIV-RCOBUT                                       
078780         END-IF                                                           
078800                                                                          
078900                                                                          
079000     END-EVALUATE                                                         
079201     .                                                                    
079202     EJECT                                                                
079203 FB-ANALYSERA-OPEN-RAD  SECTION.                                          
079204     SKIP2                                                                
079210     MOVE ORD1 TO UT-DATA-ORD1                                            
079220     PERFORM S11-SKRIV-RCOBUT                                             
079230     IF ORD2 NOT = SPACE                                                  
079240       MOVE ORD2  TO UT-DATA-ORD1                                         
079250       PERFORM S11-SKRIV-RCOBUT                                           
079260     END-IF                                                               
079270     IF ORD3 NOT = SPACE                                                  
079280       MOVE ORD3  TO UT-DATA-ORD1                                         
079290       PERFORM S11-SKRIV-RCOBUT                                           
079291     END-IF                                                               
079292     IF ORD4 NOT = SPACE                                                  
079293       MOVE ORD4  TO UT-DATA-ORD1                                         
079294       PERFORM S11-SKRIV-RCOBUT                                           
079295     END-IF                                                               
079296     IF ORD5 NOT = SPACE                                                  
079297       MOVE ORD5  TO UT-DATA-ORD1                                         
079298       PERFORM S11-SKRIV-RCOBUT                                           
079299     END-IF                                                               
079300     IF ORD6 NOT = SPACE                                                  
079301       MOVE ORD6  TO UT-DATA-ORD1                                         
079302       PERFORM S11-SKRIV-RCOBUT                                           
079303     END-IF                                                               
079304     IF ORD7 NOT = SPACE                                                  
079305       MOVE ORD7  TO UT-DATA-ORD1                                         
079306       PERFORM S11-SKRIV-RCOBUT                                           
079307     END-IF                                                               
079308     IF ORD8 NOT = SPACE                                                  
079309       MOVE ORD8  TO UT-DATA-ORD1                                         
079310       PERFORM S11-SKRIV-RCOBUT                                           
079311     END-IF                                                               
079312     .                                                                    
079313     EJECT                                                                
079314 G-ANALYSERA-CALL  SECTION.                                               
079315     SKIP2                                                                
079316     MOVE CALL-TYP TO UT-TYP                                              
079317     MOVE SPACE TO UT-DATA                                                
079318     EVALUATE TRUE                                                        
079319       WHEN ORD1 = 'CALL'                                                 
079320         MOVE ORD2 TO UT-DATA-ORD1                                        
079321                                                                          
079322       WHEN ORD2 = 'CALL'                                                 
079323         MOVE ORD3 TO UT-DATA-ORD1                                        
079324                                                                          
079325       WHEN ORD3 = 'CALL'                                                 
079326         MOVE ORD4 TO UT-DATA-ORD1                                        
079327                                                                          
079328       WHEN ORD4 = 'CALL'                                                 
079329         MOVE ORD5 TO UT-DATA-ORD1                                        
079330                                                                          
079331       WHEN ORD5 = 'CALL'                                                 
079332         MOVE ORD6 TO UT-DATA-ORD1                                        
079333                                                                          
079334       WHEN OTHER                                                         
079335         MOVE ORD7 TO UT-DATA-ORD1                                        
079336     END-EVALUATE                                                         
079337     PERFORM S11-SKRIV-RCOBUT                                             
079338     .                                                                    
079339     EJECT                                                                
079340 H-ANALYSERA-DB2  SECTION.                                                
079341     SKIP2                                                                
079342     MOVE DB2-TYP TO UT-TYP                                               
079343     MOVE SPACE TO UT-DATA                                                
079344     PERFORM S11-SKRIV-RCOBUT                                             
079345     .                                                                    
079346     EJECT                                                                
079347 I-ANALYSERA-USING  SECTION.                                              
079348     SKIP2                                                                
079349     MOVE USNG-TYP TO UT-TYP                                              
079350     MOVE SPACE TO UT-DATA                                                
079351     MOVE ORD1 TO UT-DATA-ORD1                                            
079352     EVALUATE TRUE                                                        
079353       WHEN ORD1 = 'USING'                                                
079354         MOVE ORD2 TO UT-DATA-ORD2                                        
079355                                                                          
079356       WHEN ORD2 = 'USING'                                                
079357         MOVE ORD3 TO UT-DATA-ORD2                                        
079358                                                                          
079359       WHEN ORD3 = 'USING'                                                
079360         MOVE ORD4 TO UT-DATA-ORD2                                        
079361                                                                          
079362       WHEN ORD4 = 'USING'                                                
079363         MOVE ORD5 TO UT-DATA-ORD2                                        
079364                                                                          
079365       WHEN OTHER                                                         
079366         MOVE ORD6 TO UT-DATA-ORD2                                        
079367     END-EVALUATE                                                         
079368                                                                          
079369     PERFORM S11-SKRIV-RCOBUT                                             
079370     .                                                                    
079371     EJECT                                                                
079372 J-ANALYSERA-GIVING  SECTION.                                             
079373     SKIP2                                                                
079374     MOVE GIV-TYP TO UT-TYP                                               
079375     MOVE SPACE TO UT-DATA                                                
079376     EVALUATE TRUE                                                        
079377       WHEN ORD1 = 'GIVING'                                               
079378         MOVE ORD2 TO UT-DATA-ORD1                                        
079379                                                                          
079380       WHEN ORD2 = 'GIVING'                                               
079381         MOVE ORD3 TO UT-DATA-ORD1                                        
079382                                                                          
079383       WHEN ORD3 = 'GIVING'                                               
079384         MOVE ORD4 TO UT-DATA-ORD1                                        
079385                                                                          
079386       WHEN OTHER                                                         
079387         MOVE ORD5 TO UT-DATA-ORD1                                        
079388     END-EVALUATE                                                         
079389                                                                          
079390     PERFORM S11-SKRIV-RCOBUT                                             
079391     .                                                                    
079392     EJECT                                                                
079393 Z-FINIT SECTION.                                                         
079400     SKIP2                                                                
079500     CLOSE RCOBIN                                                         
079600           RCOBUT                                                         
079700     .                                                                    
079800     EJECT                                                                
079900 S01-LAES-RCOBIN   SECTION.                                               
080000     SKIP2                                                                
080100     READ RCOBIN INTO IN-AREA                                             
080200       AT END SET END-OF-RCOBIN TO TRUE                                   
080300       NOT AT END MOVE IN-RAD TO RAD RADX2 RADX20                         
080400     END-READ                                                             
080500     .                                                                    
080600     EJECT                                                                
080700 S11-SKRIV-RCOBUT  SECTION.                                               
080800     SKIP2                                                                
080900     WRITE UT-POST FROM UT-AREA                                           
081000     .                                                                    
081100     EJECT                                                                
081200 S99-ABEND SECTION.                                                       
081300                                                                          
081400     SKIP2                                                                
081500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
081600     .                                                                    
