000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5128900.                                                
000301 AUTHOR.         BHAT ARCHANA.                                            
000401 DATE-WRITTEN.   19/12/03.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*                                                                         
000801*    FUNCTION:                                                            
000901*        SALES REPORT FOR NON VCC DC                                      
001001*                                                                         
001101*                                                                         
001201*    ABENDCODES:                                                          
001301*        U0016 -  . . . .                                                 
001401*        U1000 -  . . . .                                                 
001501*                                                                         
001601                                                                          
001701     SKIP3                                                                
001801 ENVIRONMENT DIVISION.                                                    
001901     SKIP2                                                                
002001 INPUT-OUTPUT SECTION.                                                    
002101                                                                          
002201 FILE-CONTROL.                                                            
002301     SKIP2                                                                
002401*          --- BOOKING EVENTS                                             
002501     SELECT W51266                     ASSIGN TO W51289D1.                
002601     SKIP2                                                                
002701*          --- SALES EVENTS FILE                                          
002801     SELECT W51289                     ASSIGN TO W51289D2.                
002901     EJECT                                                                
003001 DATA DIVISION.                                                           
003101     SKIP3                                                                
003201 FILE SECTION.                                                            
003301     SKIP3                                                                
003401 FD  W51266                                                               
003501     RECORDING       F                                                    
003601     BLOCK CONTAINS  0.                                                   
003701                                                                          
003801*01  -COPY WDR801      -L.                                                
003901     SKIP3                                                                
004001 FD  W51289                                                               
004101     RECORDING       V                                                    
004201     BLOCK CONTAINS  0.                                                   
004301 01  W51289-001                  PIC X(999).                              
004401     SKIP3                                                                
004501                                                                          
004801 WORKING-STORAGE SECTION.                                                 
004901                                                                          
005001 77  IDPGM                       PIC X(8)    VALUE 'W5128900'.            
005101 77  YES                         PIC X       VALUE 'J'.                   
005201 77  NOO                         PIC X       VALUE 'N'.                   
005301 77  WS-PREV-KDTRADP             PIC X(4)    VALUE SPACES.                
005401                                                                          
005501 77  W51266-EOF-SW               PIC X       VALUE 'N'.                   
005601     88  END-OF-W51266                       VALUE 'J'.                   
006001 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006101 01  FILLER REDEFINES TODAYS-DATE.                                        
006201     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006301     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006401     03  TODAYS-DATE-DAY         PIC 9(2).                                
006501     EJECT                                                                
006601 01  GENERAL-SUBPROGRAMS.                                                 
006701*                                                                         
006801     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006902     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
007001     SKIP2                                                                
007101*    --- PARAMETERS TO ABEND                                              
007201                                                                          
007301 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007401 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007501 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007601     SKIP2                                                                
007701 01  ERROR-TEXT.                                                          
007801     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007901     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008001     EJECT                                                                
008101 01  W001-DAP.                                                            
008201     03  FILLER                  PIC X(165)  VALUE SPACE.                 
008301     EJECT                                                                
008401*01  -COPY WWDC99                                                         
008501*    --- PARAMETRAR TILL POSTSUM                                          
008601*                                                                         
008701*01  -COPY W0005   -PRE  POSTSUM-                                         
008801     EJECT                                                                
008901 01  IN-AREA-START               PIC X(24)   VALUE                        
009001                                 'IN-AREA-START  '.                       
009101     SKIP2                                                                
009201                                                                          
009301*01  AREA -COPY WDR801     -PRE IN-                                       
009401*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
009501     EJECT                                                                
009601 01  TEXT-AREA.                                                           
009701     03  HEAD-LINE.                                                       
009801         05  FILLER          PIC X(11)  VALUE                             
009901            'PART NUMBER'.                                                
010001         05  FILLER          PIC X(1)   VALUE ';'.                        
010101         05  FILLER          PIC X(8)   VALUE                             
010201            'PACKAGES'.                                                   
010301         05  FILLER          PIC X(1)   VALUE ';'.                        
010401         05  FILLER          PIC X(5)  VALUE                              
010501            'PRICE'.                                                      
010601         05  FILLER          PIC X(1)    VALUE ';'.                       
010701         05  FILLER          PIC X(8)  VALUE                              
010801            'QUANTITY'.                                                   
010901         05  FILLER          PIC X(1)   VALUE ';'.                        
011001         05  FILLER          PIC X(12)  VALUE                             
011101            'TOTAL AMOUNT'.                                               
011201         05  FILLER          PIC X(1)    VALUE ';'.                       
011301         05  FILLER          PIC X(21)  VALUE                             
011401            'INVOICE/CREDIT NUMBER'.                                      
011501         05  FILLER          PIC X(1)    VALUE ';'.                       
011601         05  FILLER          PIC X(10)  VALUE                             
011701            'MAIN EVENT'.                                                 
011801         05  FILLER          PIC X(1)    VALUE ';'.                       
011901         05  FILLER          PIC X(9)   VALUE                             
012001            'SUB EVENT'.                                                  
012101         05  FILLER          PIC X(1)    VALUE ';'.                       
012201         05  FILLER          PIC X(11)  VALUE                             
012301            'EVENT LEVEL'.                                                
012401         05  FILLER          PIC X(1)    VALUE ';'.                       
012501         05  FILLER          PIC X(11)  VALUE                             
012601            'POSTING KEY'.                                                
012700         05  FILLER          PIC X(1)    VALUE ';'.                       
012800         05  FILLER          PIC X(12)   VALUE                            
012900            'ORDER NUMBER'.                                               
013000         05  FILLER          PIC X(1)    VALUE ';'.                       
013100         05  FILLER          PIC X(11)  VALUE                             
013200            'CUSTOMER NO'.                                                
013300         05  FILLER          PIC X(1)    VALUE ';'.                       
013400         05  FILLER          PIC X(11)  VALUE                             
013500            'DISTRICT NO'.                                                
013600         05  FILLER          PIC X(1)    VALUE ';'.                       
013701         05  FILLER          PIC X(11)  VALUE                             
013801            'PRODUCT GRP'.                                                
013901         05  FILLER          PIC X(1)    VALUE ';'.                       
014001         05  FILLER          PIC X(13)  VALUE                             
014101            'DELIVERY DATE'.                                              
014201         05  FILLER          PIC X(1)    VALUE ';'.                       
014300                                                                          
014400     03 ROW-LINE.                                                         
014501*        05   -COPY W51289                                                
014600     EJECT                                                                
014700 PROCEDURE DIVISION.                                                      
014800 MAIN SECTION.                                                            
014900     SKIP2                                                                
015000                                                                          
015100     PERFORM A-INIT                                                       
015200     PERFORM S01-READ-W51266                                              
015300     PERFORM UNTIL END-OF-W51266                                          
015400       PERFORM B-PROCESS-EVENTS                                           
015500       PERFORM S01-READ-W51266                                            
015600     END-PERFORM                                                          
015700                                                                          
015800                                                                          
015900     PERFORM Z-FINIT                                                      
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     OPEN INPUT  W51266                                                   
016800                                                                          
016901     OPEN OUTPUT W51289                                                   
017000     SKIP2                                                                
017100     ACCEPT TODAYS-DATE  FROM DATE                                        
017200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017210     MOVE SPACES TO WS-PREV-KDTRADP                                       
017300     .                                                                    
017400     EJECT                                                                
017500 B-PROCESS-EVENTS SECTION.                                                
017600                                                                          
018300     IF ((IN-EKH-KDEKHHT = '203'                                          
018400     AND  IN-EKH-KDEKSHT = '201')                                         
018500     OR  (IN-EKH-KDEKHHT = '204'                                          
018600     AND  IN-EKH-KDEKSHT = '201')                                         
018700     OR  (IN-EKH-KDEKHHT = '303'                                          
018800     AND  IN-EKH-KDEKSHT = '311')                                         
018900     OR  (IN-EKH-KDEKHHT = '303'                                          
019000     AND  IN-EKH-KDEKSHT = '391'))                                        
019101                                                                          
019201       PERFORM BA-WRITE-HEADER                                            
019301       MOVE IN-EKH-PRARTSTD    TO UT-PRARTSTD                             
019401       COMPUTE UT-SUBEL = IN-EKH-PRARTSTD * IN-EKH-KVANTAL                
019501       IF IN-EKH-KDEKHHT = '303'                                          
019601       AND IN-EKH-KDEKSHT = '391'                                         
019701         MOVE '-'              TO UT-KDTECKEN                             
019801       ELSE                                                               
019901         MOVE '+'              TO UT-KDTECKEN                             
020001       END-IF                                                             
020101       PERFORM BB-MOVE-FIELDS                                             
020200     END-IF                                                               
020300                                                                          
020401     IF IN-EKH-KDEKHHT = '302'                                            
020501     AND IN-EKH-KDEKSHT = '302'                                           
020601       PERFORM BA-WRITE-HEADER                                            
020701       MOVE IN-EKH-PRARTSTD    TO UT-PRARTSTD                             
020801       COMPUTE UT-SUBEL = IN-EKH-PRARTSTD * IN-EKH-KVANTAL                
020901       IF IN-FIL-IDPGM = 'W4079700'                                       
021001         MOVE '+'              TO UT-KDTECKEN                             
021101       ELSE                                                               
021201         MOVE '-'              TO UT-KDTECKEN                             
021301       END-IF                                                             
021401       PERFORM BB-MOVE-FIELDS                                             
021501     END-IF                                                               
021601                                                                          
021700     IF IN-EKH-KDEKHHT = '204'                                            
021800     AND IN-EKH-KDEKSHT = '301'                                           
021901       IF IN-EKH-KDEKNIVA = 'DET'                                         
022201         PERFORM BA-WRITE-HEADER                                          
022301         MOVE IN-EKH-PRARTSTD  TO UT-PRARTSTD                             
022401         COMPUTE UT-SUBEL = IN-EKH-PRARTSTD * IN-EKH-KVANTAL              
022501         PERFORM BB-MOVE-FIELDS                                           
022701       END-IF                                                             
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023101 BA-WRITE-HEADER SECTION.                                                 
023201                                                                          
023601     IF IN-EKH-KDTRADP NOT = WS-PREV-KDTRADP                              
023602     AND IN-EKH-KDTRADP(1:2) NOT = WS-PREV-KDTRADP(1:2)                   
023603       MOVE IN-EKH-KDTRADP   TO WS-PREV-KDTRADP                           
023801       PERFORM S10-WRITE-DAP                                              
024001       WRITE W51289-001   FROM HEAD-LINE                                  
024102     END-IF                                                               
024301     .                                                                    
024401     EJECT                                                                
024501 BB-MOVE-FIELDS SECTION.                                                  
024600                                                                          
025300     MOVE IN-EKH-IDARTNR   TO UT-IDARTNR                                  
025400     MOVE IN-EKH-IDDISTR   TO UT-IDDISTR                                  
025500     MOVE IN-EKH-IDKUNDNR  TO UT-IDKUNDNR                                 
025600     MOVE IN-EKH-IDVERGL   TO UT-IDVERGL                                  
025700     MOVE IN-EKH-KDEKHHT   TO UT-KDEKHHT                                  
025800     MOVE IN-EKH-KDEKSHT   TO UT-KDEKSHT                                  
025900     MOVE IN-EKH-KDEKNIVA  TO UT-KDEKNIVA                                 
026000     MOVE IN-EKH-KVANTAL   TO UT-KVANTAL                                  
026100     MOVE IN-EKH-IDORDNR5  TO UT-IDORDNR5                                 
026201     MOVE IN-EKH-KDPRODSL  TO UT-KDPRODSL                                 
026301     MOVE IN-EKH-DAVERDAT  TO UT-DAVERDAT                                 
026400     MOVE ZERO             TO UT-IDKOLLI                                  
026500     MOVE ';'              TO UT-SEMICOLON-1                              
026600                              UT-SEMICOLON-2                              
026700                              UT-SEMICOLON-3                              
026800                              UT-SEMICOLON-4                              
026900                              UT-SEMICOLON-5                              
027000                              UT-SEMICOLON-6                              
027100                              UT-SEMICOLON-7                              
027200                              UT-SEMICOLON-8                              
027300                              UT-SEMICOLON-9                              
027400                              UT-SEMICOLON-10                             
027500                              UT-SEMICOLON-11                             
027600                              UT-SEMICOLON-12                             
027701                              UT-SEMICOLON-13                             
027801                              UT-SEMICOLON-14                             
027901                              UT-SEMICOLON-15                             
028001     WRITE W51289-001 FROM ROW-LINE                                       
028100     .                                                                    
028200     EJECT                                                                
028300 Z-FINIT SECTION.                                                         
028400     CLOSE W51266                                                         
028501           W51289                                                         
028600     SKIP2                                                                
028700     MOVE 'S' TO POSTSUM-OPKOD                                            
028800     CALL POSTSUM USING POSTSUM-PARM                                      
028900     .                                                                    
029000     EJECT                                                                
029100 S01-READ-W51266  SECTION.                                                
029200     READ W51266 INTO IN-AREA                                             
029300     AT END                                                               
029400        MOVE HIGH-VALUE TO IN-AREA                                        
029500        SET END-OF-W51266 TO TRUE                                         
029600                                                                          
029700     NOT AT END                                                           
029800        MOVE 'W51266' TO POSTSUM-FDNAMN                                   
029901        MOVE 'W51289D1' TO POSTSUM-DDNAMN2                                
030000        MOVE SPACES    TO POSTSUM-TRANSTYP                                
030100        CALL POSTSUM USING POSTSUM-PARM                                   
030200     END-READ                                                             
030300     .                                                                    
030400     EJECT                                                                
030500                                                                          
030501 S10-WRITE-DAP SECTION.                                                   
030601                                                                          
030701     MOVE ' ¤DAPW51289-001' TO W001-DAP                                   
030801     WRITE W51289-001 FROM W001-DAP                                       
030802                                                                          
030803     MOVE SPACES            TO W001-DAP                                   
030804                                                                          
030805     STRING ' ¤DAP' WS-PREV-KDTRADP                                       
030806            DELIMITED BY SIZE INTO W001-DAP                               
030807     WRITE W51289-001 FROM W001-DAP                                       
030808     MOVE SPACES            TO W001-DAP                                   
031101     .                                                                    
031102     EJECT                                                                
031201                                                                          
