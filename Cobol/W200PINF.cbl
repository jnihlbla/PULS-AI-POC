000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W200PINF.                                                
000400 AUTHOR.         SATHISH THIRUVENGADAM.                                   
000500 DATE-WRITTEN.   24/JAN/2023.                                             
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SUBPROGRAM TO GET THE FOLLOWING :                                
001100*        -PART DESCRIPTION AS IN 1101                                     
001200*        -SUPERSESSION CODE AS IN 1102                                    
001300*        -KIT PART NO,QUANTITY AS IN 1213                                 
001400*        -AGREEMENT,PROCURER NO AS IN 2103                                
001500*        -PROCURER NAME AS IN 6108                                        
001600*        -DANG. GOODS CODE AS IN 1118                                     
001700*        -MFG, SHP AS IN 2103                                             
001800*                                                                         
001900*        PROGRAMMET READS     WDD3                                        
002000*                             WDK6                                        
002100*                             WDJ1                                        
002200*                                                                         
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W1                                                       
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W200PINF'.            
004100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004200 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500     EJECT                                                                
004600                                                                          
004700                                                                          
004800 01  WORKING-FIELDS.                                                      
004900*                                                                         
005000     03  INDX                    PIC  9(3)   VALUE ZERO.                  
005100     03  MAX-INDX                PIC  9(3)   VALUE 100.                   
005200*                                                                         
005300*                                                                         
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
005700     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
005800     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
005900     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM'.              
006000     SKIP2                                                                
006100*    --- PARAMETERS TO ABEND                                              
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006600*                                                                         
006700 01  TODAYS-DATE             PIC 9(6).                                    
006800*SWITCHES                                                                 
006900 77  KIT-DATA-SW                 PIC X(01)   VALUE 'J'.                   
007000     88  KIT-DATA-OK                         VALUE 'J'.                   
007100     88  KIT-DATA-FEL                        VALUE 'N'.                   
007200                                                                          
007300 77  INPUT-DATA-SW               PIC X(01)   VALUE 'J'.                   
007400     88  INPUT-DATA-OK                       VALUE 'J'.                   
007500     88  INPUT-DATA-FEL                      VALUE 'N'.                   
007600*                                                                         
007700                                                                          
007800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100 01  KEYS-TO-DLI.                                                         
008200     03  W-IDARTNR-X.                                                     
008300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008400     03  W-IDSKYLT-X.                                                     
008500         05  W-IDSKYLT           PIC X(3)   VALUE 'GB '.                  
008600     03  W-KDARBTYP-X.                                                    
008700         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
008800     03  W-IDPERSON-X.                                                    
008900         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
009000                                                                          
009100*    --- --- IMS FUNCTION CODES                                           
009200*01  -COPY W0003                                                          
009300                                                                          
009400*    ---  DLI INPUT-OUTPUT AREA                                           
009500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
009600 01  DLI-IO-WDD311.                                                       
009700*    03  -COPY WDD311                                                     
009800                                                                          
009900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010000 01  DLI-IO-WDK601.                                                       
010100*    03  -COPY WDK601                                                     
010200                                                                          
010300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010400 01  DLI-IO-WDK611.                                                       
010500*    03  -COPY WDK611                                                     
010600                                                                          
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ101'.                      
010800 01  DLI-IO-WDJ101.                                                       
010900*    03  -COPY WDJ101                                                     
011000                                                                          
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ111'.                      
011200 01  DLI-IO-WDJ111.                                                       
011300*    03  -COPY WDJ111                                                     
011400                                                                          
011500                                                                          
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
011700 01  DLI-IO-WDP311.                                                       
011800*    03  -COPY WDP311                                                     
011900                                                                          
012000*    --- STATUS-CODE FROM IMS                                             
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FOUND                       VALUE '  '.                  
012300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012500     88  END-OF-BASE                         VALUE 'GB'.                  
012600     SKIP2                                                                
012700 01  GOOD-STATUSCODES.                                                    
012800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900                                                                          
013000 01  ALL-SSA.                                                             
013100    03 SSA1                      PIC X(160).                              
013200    03 SSA2                      PIC X(96).                               
013300                                                                          
013400                                                                          
013500 LINKAGE SECTION.                                                         
013600*    -COPY W200PINF                                                       
013700                                                                          
013800     EJECT                                                                
013900*01  -COPY W0008      -PRE WDD3-                                          
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200*01  -COPY W0008      -PRE WDK6-                                          
014300     05  FILLER                  PIC X.                                   
014400*01  -COPY W0008      -PRE WDJ1-                                          
014500     05  FILLER                  PIC X.                                   
014600*01  -COPY W0008      -PRE WDP3-                                          
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900                                                                          
015000                                                                          
015100 PROCEDURE DIVISION  USING PINF-W200PINF WDD3-PCB WDK6-PCB                
015200                                         WDJ1-PCB WDP3-PCB.               
015300                                                                          
015400     PERFORM A-INIT                                                       
015500     PERFORM B-VALIDATE-INPUT                                             
015600     IF INPUT-DATA-OK                                                     
015700        PERFORM C-GET-PART-DESCRIPTION                                    
015800        PERFORM D-GET-WDK6                                                
015900        PERFORM E-GET-KIT-DATA                                            
016000        IF PINF-IDANSK > 0                                                
016100           PERFORM F-GET-PROCURER-NAME                                    
016200        END-IF                                                            
016300     END-IF                                                               
016400                                                                          
016500     GOBACK                                                               
016600     .                                                                    
016700     EJECT                                                                
016800 A-INIT SECTION.                                                          
016900     MOVE 'A-INIT         ' TO CURRENT-SECTION                            
017000                                                                          
017100     INITIALIZE PINF-UTDATA                                               
017200     MOVE JA  TO INPUT-DATA-SW                                            
017300                                                                          
017400     MOVE SPACES            TO PINF-KDSVAR                                
017500                               PINF-FEL-TEXT                              
017600                                                                          
017700     ACCEPT TODAYS-DATE        FROM DATE                                  
017800     .                                                                    
017900     EJECT                                                                
018000                                                                          
018100 B-VALIDATE-INPUT SECTION.                                                
018200     MOVE 'B-VALIDATE-    ' TO CURRENT-SECTION                            
018300                                                                          
018400     IF  PINF-IDARTNR-IN  IS NUMERIC                                      
018500     AND PINF-IDARTNR-IN    > ZERO                                        
018600         MOVE PINF-IDARTNR-IN    TO W-IDARTNR                             
018700     ELSE                                                                 
018800         MOVE NEJ                TO INPUT-DATA-SW                         
018900*                                                                         
019000         SET  PINF-KDSVAR-FEL     TO TRUE                                 
019100         MOVE '022'               TO PINF-IDMSG-ERROR                     
019200         MOVE 'IDARTNR'           TO PINF-IDELMT-ERROR                    
019300         MOVE 'INVALID PART     ' TO PINF-FEL-TEXT                        
019400*                                                                         
019500     END-IF                                                               
019600     .                                                                    
019700 C-GET-PART-DESCRIPTION SECTION.                                          
019800     MOVE 'C-GET-PART    ' TO CURRENT-SECTION                             
019900                                                                          
020000     PERFORM IMS-GU-WDD311                                                
020100     IF SEGMENT-FOUND                                                     
020200        MOVE TEXT-BEART          TO PINF-BEART-ENG                        
020300     ELSE                                                                 
020400        MOVE SPACES              TO PINF-BEART-ENG                        
020500     END-IF                                                               
020600     .                                                                    
020700 D-GET-WDK6              SECTION.                                         
020800*TO GET SUPERSESSION CODE,AGREEMENT CODE,PROCURER NO,                     
020900*DANG.GOODS CODE                                                          
021000     MOVE 'D-GET-WDK6    ' TO CURRENT-SECTION                             
021100                                                                          
021200     PERFORM IMS-GU-WDK601                                                
021300     IF SEGMENT-FOUND                                                     
021400       MOVE ART-KDERS-UTG  TO PINF-KDERS                                  
021500       MOVE ART-IDLEVNR    TO PINF-IDLEVNR-MFG                            
021600       PERFORM IMS-GNP-WDK611                                             
021700       IF  SEGMENT-FOUND                                                  
021800         MOVE CLAG-KDERS   TO PINF-KDERS                                  
021900         MOVE CLAG-KDAVT   TO PINF-KDAVT                                  
022000         MOVE CLAG-IDANSK  TO PINF-IDANSK                                 
022100         MOVE CLAG-KDFARLIG                                               
022200                           TO PINF-KDFARLIG                               
022300         MOVE CLAG-IDLEVNR-SHIP                                           
022400                           TO PINF-IDLEVNR-SHIP                           
022500       END-IF                                                             
022600     ELSE                                                                 
022700        SET  PINF-KDSVAR-FEL     TO TRUE                                  
022800        MOVE '025'               TO PINF-IDMSG-ERROR                      
022900        MOVE 'IDARTNR'           TO PINF-IDELMT-ERROR                     
023000        MOVE 'MISSING IN WDK601' TO PINF-FEL-TEXT                         
023100     END-IF                                                               
023200     .                                                                    
023300 E-GET-KIT-DATA          SECTION.                                         
023400*TO GET KIT PART NUMBERS,QUANTITY                                         
023500*                                                                         
023600     MOVE 'E-GET-KIT     ' TO CURRENT-SECTION                             
023700                                                                          
023800     PERFORM IMS-GU-WDJ101                                                
023900     IF SEGMENT-FOUND                                                     
024000       PERFORM IMS-GNP-WDJ111                                             
024100       MOVE +1                     TO INDX                                
024200       PERFORM UNTIL ( INDX > MAX-INDX ) OR SEGMENT-MISSING               
024300         PERFORM EA-VALIDATE-KIT-DATA                                     
024400         IF KIT-DATA-OK                                                   
024500            MOVE RAD-IDARTNR   TO PINF-IDARTNR-SATS (INDX)                
024600            MOVE RAD-REANTPSA  TO PINF-REANTPSA (INDX)                    
024700            ADD +1             TO INDX                                    
024800         END-IF                                                           
024900         PERFORM IMS-GNP-WDJ111                                           
025000       END-PERFORM                                                        
025100     END-IF                                                               
025200     .                                                                    
025300 EA-VALIDATE-KIT-DATA    SECTION.                                         
025400*TO GET KIT PART NUMBERS,QUANTITY                                         
025500*                                                                         
025600     MOVE 'EA-VALIDATE   ' TO CURRENT-SECTION                             
025700                                                                          
025800     MOVE NEJ TO KIT-DATA-SW                                              
025900                                                                          
026000     IF RAD-KDISATS = 'N' OR 'T' OR 'E'                                   
026100       MOVE JA                 TO KIT-DATA-SW                             
026200     ELSE                                                                 
026300       MOVE RAD-TISTODAT       TO TMP1-YYMMDD                             
026400       MOVE TODAYS-DATE        TO TMP2-YYMMDD                             
026500       PERFORM WY2000P1                                                   
026600       IF  ( RAD-KDISATS = 'U' OR ' ')                                    
026700       AND TMP1-YYMMDD > TMP2-YYMMDD                                      
026800         MOVE JA               TO KIT-DATA-SW                             
026900       END-IF                                                             
027000     END-IF                                                               
027100     .                                                                    
027200 F-GET-PROCURER-NAME SECTION.                                             
027300     MOVE 'F-GET-PROCURER' TO CURRENT-SECTION                             
027400                                                                          
027500     MOVE 'ANSK    '           TO W-KDARBTYP                              
027600     MOVE PINF-IDANSK          TO W-IDPERSON                              
027700     PERFORM IMS-GU-WDP311                                                
027800     IF SEGMENT-FOUND                                                     
027900        MOVE PERS-IDNAMN         TO PINF-IDNAMN-ANSK                      
028000     ELSE                                                                 
028100        MOVE SPACES              TO PINF-IDNAMN-ANSK                      
028200     END-IF                                                               
028300     .                                                                    
028400* ---                                                                     
028500* --- IMS SECTIONS  ---                                                   
028600* ---                                                                     
028700                                                                          
028800 IMS-GU-WDD311 SECTION.                                                   
028900     MOVE 'IMS-GU-WDD311 ' TO CURR-IMS-SECTION                            
029000                                                                          
029100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
029200             DELIMITED BY SIZE INTO SSA1                                  
029300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
029400              DELIMITED BY SIZE INTO SSA2                                 
029500     MOVE '  GE' TO GOOD-STATUSCODES                                      
029600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
029700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSCHECK                                              
029900     .                                                                    
030000     EJECT                                                                
030100 IMS-GU-WDK601  SECTION.                                                  
030200     MOVE 'IMS-GU-WDK601 ' TO CURR-IMS-SECTION                            
030300                                                                          
030400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
030500          DELIMITED BY SIZE INTO SSA1                                     
030600     MOVE '  GE' TO GOOD-STATUSCODES                                      
030700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
030800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
030900     PERFORM IMS-STATUSCHECK                                              
031000     .                                                                    
031100     SKIP3                                                                
031200 IMS-GNP-WDK611 SECTION.                                                  
031300     MOVE 'IMS-GNP-WDK611' TO CURR-IMS-SECTION                            
031400                                                                          
031500     MOVE 'WDK611    '      TO SSA1                                       
031600     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
031700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
031800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031900     PERFORM IMS-STATUSCHECK                                              
032000     .                                                                    
032100     EJECT                                                                
032200 IMS-GU-WDJ101  SECTION.                                                  
032300     MOVE 'IMS-GU-WDJ101 ' TO CURR-IMS-SECTION                            
032400                                                                          
032500     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
032600          DELIMITED BY SIZE INTO SSA1                                     
032700     MOVE '  GE'   TO GOOD-STATUSCODES                                    
032800     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-WDJ101 SSA1                    
032900     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
033000     PERFORM IMS-STATUSCHECK                                              
033100     .                                                                    
033200     SKIP3                                                                
033300 IMS-GNP-WDJ111 SECTION.                                                  
033400     MOVE 'IMS-GNP-WDJ111' TO CURR-IMS-SECTION                            
033500                                                                          
033600     MOVE 'WDJ111    '      TO SSA1                                       
033700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
033800     CALL CBLTDLI USING GNP WDJ1-PCB DLI-IO-WDJ111 SSA1                   
033900     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
034000     PERFORM IMS-STATUSCHECK                                              
034100     .                                                                    
034200     EJECT                                                                
034300 IMS-GU-WDP311    SECTION.                                                
034400     MOVE 'IMS-GU-WDP311' TO CURR-IMS-SECTION                             
034500                                                                          
034600     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
034700       DELIMITED BY SIZE INTO SSA1                                        
034800     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
034900       DELIMITED BY SIZE INTO SSA2                                        
035000     MOVE '  GE' TO GOOD-STATUSCODES                                      
035100     CALL CBLTDLI             USING GU  WDP3-PCB                          
035200                                    DLI-IO-WDP311                         
035300                                    SSA1 SSA2                             
035400     MOVE WDP3-STATUS-CODE       TO STATUS-WS                             
035500     PERFORM IMS-STATUSCHECK                                              
035600     .                                                                    
035700     SKIP2                                                                
035800 IMS-STATUSCHECK SECTION.                                                 
035900                                                                          
036000     SET STATUS-IX TO 1                                                   
036100     SEARCH GOOD-STATUS                                                   
036200       AT END                                                             
036300         CALL FELLOG                                                      
036400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
036500         CONTINUE                                                         
036600     END-SEARCH                                                           
036700     .                                                                    
036800*    -COPY WY2000P1                                                       
