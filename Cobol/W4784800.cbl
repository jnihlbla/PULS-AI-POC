000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W4784800.                                               
000400 AUTHOR.          FRANK THORBURN.                                         
000500 DATE-WRITTEN.    NOV 1988.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        ***************  SB-PROGRAM    **************                    
001200*                                                                         
001300*        PROGRAMMET LÄSER IGENOM WDJ1 (LOGISKT WLSATA)                    
001400*        OCH LÄSER NED SEGMENTEN 01 OCH 11 PÅ UTFILEN                     
001500*        W47848.                                                          
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP3                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*- - - - - - - - - - - - - - UTFIL:                                       
002500                                                                          
002600     SELECT W47847                       ASSIGN TO W47848D1.              
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100 FD  W47847                                                               
003200     LABEL RECORD   STANDARD                                              
003300     RECORDING  F                                                         
003400     BLOCK CONTAINS 0.                                                    
003500     SKIP2                                                                
003600 01  UT-POST                 PIC X(16).                                   
003700*                                                                         
003800                                                                          
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004001*    -COPY WY2000W3                                                       
004010     SKIP3                                                                
004100*- - - - - - - - - - - - - - GENERERAT PROGRAMNAMN                        
004200 77   PROGRAM-NAMN           VALUE 'W4784800'                             
004300                                 PIC X(8).                                
004400     SKIP2                                                                
004500*- - - - - - - - - - - - - - GENERELLA KONSTANTER                         
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  SW-SKRIV-POST               PIC X.                                   
005000     88  SKRIV-POST                          VALUE 'J'.                   
005100 77  WS-STOPPDATUM               PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300*- - - - - - - - - - - - - - HJÄLPFÄLT                                    
005400                                                                          
005500 01  AKT-DATUM                   PIC 9(6)    VALUE ZERO.                  
005600                                                                          
005700 01  DATUM-GRP.                                                           
005800   03  DATUM-AAVV                PIC 9(4)    VALUE ZERO.                  
005900   03  DATUM-D                   PIC 9(1)    VALUE ZERO.                  
006000                                                                          
006100 01  DATUM-START-GRP.                                                     
006200   03  DATUM-START-AAVV          PIC 9(4)    VALUE ZERO.                  
006300   03  DATUM-START-D             PIC 9(1)    VALUE ZERO.                  
006400                                                                          
006500 01  DATUM-STOPP-GRP.                                                     
006600   03  DATUM-STOPP-AAVV          PIC 9(4)    VALUE ZERO.                  
006700   03  DATUM-STOPP-D             PIC 9(1)    VALUE ZERO.                  
006800                                                                          
006900 01  SPAR-AREA.                                                           
007000   03  SPAR-IDARTNR              PIC S9(9) COMP-3 VALUE ZERO.             
007100   03  SPAR-TIBORT               PIC S9(5) COMP-3 VALUE ZERO.             
007200                                                                          
007300 01  IMS-WS.                                                              
007400                                                                          
007500   03  STATUS-WS                 PIC X(2).                                
007600       88  SEGMENT-SLUT                      VALUE 'GB'.                  
007700                                                                          
007800   03  GODK-STATUSKODER.                                                  
007900       05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX  PIC X(2).           
008000                                                                          
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
008300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008500   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
008600     SKIP3                                                                
008700*- - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                      
008800                                                                          
008900*    -COPY W0005       -PRE POSTSUM-                                      
009100     EJECT                                                                
009200*    -COPY W0003                                                          
009400     EJECT                                                                
009500*    -COPY WDATAREA                                                       
009700     EJECT                                                                
009800*- - - - - - - - - - - - - -  UT-AREA                                     
009900 01  FILLER                   PIC X(24) VALUE 'UT-AREA    '.              
010000                                                                          
010100 01  UT-AREA.                                                             
010200                                                                          
010300   03 UT-IDARTNR            PIC S9(9)          COMP-3.                    
010400   03 UT-IDARTNR-ING        PIC S9(9)          COMP-3.                    
010500   03 UT-REANTPSA           PIC S9(2)V9(3)     COMP-3.                    
010600   03 UT-TIBORT             PIC S9(5)          COMP-3.                    
010700                                                                          
010800     EJECT                                                                
010900 01  IO-AREA.                                                             
011000   03 IO-AREA1                   PIC X(230).                              
011100                                                                          
011200*  03 01-AREA -COPY WDJ101     -PRE SATB01- -RED IO-AREA1.                
011400     EJECT                                                                
011500*  03 03-AREA -COPY WDJ111     -PRE SATB11- -RED IO-AREA1.                
011700     EJECT                                                                
011800 LINKAGE SECTION.                                                         
011900*01              -COPY W0008        -PRE SATB-                            
012100     05  FILLER                  PIC X.                                   
012200     EJECT                                                                
012300 PROCEDURE DIVISION USING SATB-PCB.                                       
012400     ENTRY 'DLITCBL' USING SATB-PCB.                                      
012500                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM IMS-GET-WDJ1                                                 
012800     PERFORM UNTIL SEGMENT-SLUT                                           
012900                                                                          
013000         MOVE NEJ                     TO SW-SKRIV-POST                    
013100         EVALUATE SATB-SEG-NAME-FB                                        
013200           WHEN 'WDJ101'                                                  
013300                  PERFORM B-BEHANDLA-SATS                                 
013400           WHEN 'WDJ111'                                                  
013500                  PERFORM C-BEHANDLA-GAELLANDE-ARTIKLAR                   
013600         END-EVALUATE                                                     
013700                                                                          
013800         IF SKRIV-POST                                                    
013900           WRITE UT-POST FROM UT-AREA                                     
014000                                                                          
014100           MOVE SPACE TO POSTSUM-TRANSTYP                                 
014200           MOVE 'UT' TO POSTSUM-FDNAMN                                    
014300           MOVE 'W47848D1' TO POSTSUM-DDNAMN2                             
014400           CALL POSTSUM USING POSTSUM-PARM                                
014410           MOVE NEJ TO SW-SKRIV-POST                                      
014500         END-IF                                                           
014600         PERFORM IMS-GET-WDJ1                                             
014700     END-PERFORM                                                          
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500     SKIP2                                                                
015600     INITIALIZE UT-AREA                                                   
015700     ACCEPT AKT-DATUM FROM DATE                                           
015800     MOVE AKT-DATUM               TO DAT-I-TIDATUM                        
015900     PERFORM S01-CALL-WDATKONV                                            
016000     IF DAT-KDSVAR-OK                                                     
016100       MOVE DAT-TIAAVVD-GRP       TO DATUM-GRP                            
016200     END-IF                                                               
016300     OPEN OUTPUT W47847                                                   
016400     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
016500     EJECT                                                                
016600                                                                          
016700     .                                                                    
016800 B-BEHANDLA-SATS SECTION.                                                 
016900                                                                          
017000     IF SATB01-STR-IDARTNR < +100000000 AND                               
017100        SATB01-STR-IDLEVNR = '1002 '                                      
017110        IF SATB01-STR-TIBORT > ZERO                                       
017120           MOVE ZERO                 TO SPAR-IDARTNR                      
017130        ELSE                                                              
017200           MOVE SATB01-STR-IDARTNR   TO SPAR-IDARTNR                      
017201           MOVE ZERO                 TO SPAR-TIBORT                       
017210        END-IF                                                            
017710     ELSE                                                                 
017720        MOVE ZERO                    TO SPAR-IDARTNR                      
017800     END-IF                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 C-BEHANDLA-GAELLANDE-ARTIKLAR SECTION.                                   
018200                                                                          
018210     IF SPAR-IDARTNR > ZERO                                               
018300        MOVE SPAR-IDARTNR               TO UT-IDARTNR                     
018400        MOVE SPAR-TIBORT                TO UT-TIBORT                      
018500        MOVE SATB11-RAD-REANTPSA        TO UT-REANTPSA                    
018600                                                                          
018700        MOVE SATB11-RAD-TISTADAT TO DAT-I-TIDATUM                         
018800        PERFORM S01-CALL-WDATKONV                                         
018900        IF DAT-KDSVAR-OK                                                  
019000           MOVE DAT-TIAAVVD-GRP  TO DATUM-START-GRP                       
019100        END-IF                                                            
019200        MOVE NEJ TO WS-STOPPDATUM                                         
019300        IF SATB11-RAD-TISTODAT = +0999999                                 
019400           MOVE JA TO WS-STOPPDATUM                                       
019500        ELSE                                                              
019600           MOVE SATB11-RAD-TISTODAT TO DAT-I-TIDATUM                      
019700           PERFORM S01-CALL-WDATKONV                                      
019800           IF DAT-KDSVAR-OK                                               
019900              MOVE DAT-TIAAVVD-GRP  TO DATUM-STOPP-GRP                    
020000           END-IF                                                         
020100        END-IF                                                            
020200                                                                          
020201        MOVE DATUM-START-AAVV   TO TMP1-YYWW                              
020202        MOVE DATUM-AAVV         TO TMP2-YYWW                              
020203        MOVE DATUM-STOPP-AAVV   TO TMP3-YYWW                              
020210        PERFORM WY2000Q3                                                  
020300        IF (TMP1-YYWW < TMP2-YYWW) OR                                     
020400           (TMP1-YYWW = TMP2-YYWW) AND                                    
020500           (TMP3-YYWW > TMP2-YYWW OR WS-STOPPDATUM = JA)                  
020600            MOVE SATB11-RAD-IDARTNR   TO UT-IDARTNR-ING                   
020700            MOVE JA  TO SW-SKRIV-POST                                     
020800        ELSE                                                              
020900            MOVE NEJ TO SW-SKRIV-POST                                     
021000        END-IF                                                            
021010     ELSE                                                                 
021020        MOVE NEJ TO SW-SKRIV-POST                                         
021030     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 S01-CALL-WDATKONV SECTION.                                               
021400                                                                          
021500     MOVE 'AAMMDD'                    TO DAT-KDDATFORM                    
021600     CALL WDATKONV USING DAT-KDDATFORM                                    
021700                         DAT-I-TIDATUM                                    
021800                         DAT-O-TIDATUM                                    
021900                         DAT-KDSVAR                                       
022000                                                                          
022100     .                                                                    
022200     EJECT                                                                
022300 Z-FINIT SECTION.                                                         
022400     SKIP2                                                                
022500     CLOSE W47847                                                         
022600                                                                          
022700*- - - - - - - - - - - - - - - -  SKRIV UT ANTAL SKRIVNA POSTER           
022800                                                                          
022900     MOVE 'S' TO POSTSUM-OPKOD                                            
023000     CALL POSTSUM USING POSTSUM-PARM                                      
023100     .                                                                    
023200     EJECT                                                                
023300 IMS-GET-WDJ1 SECTION.                                                    
023400                                                                          
023500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
023600     CALL CBLTDLI USING GN SATB-PCB IO-AREA1                              
023700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
023800     PERFORM IMS-STATUSKONTROLL                                           
023900                                                                          
024000     .                                                                    
024100 IMS-STATUSKONTROLL SECTION.                                              
024200                                                                          
024300     SET STATUS-IX TO 1                                                   
024400     SEARCH GODK-STATUS AT END CALL FELLOG                                
024500     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
024600     END-SEARCH                                                           
024700     .                                                                    
024710     EJECT                                                                
024800*    -COPY WY2000Q3                                                       
