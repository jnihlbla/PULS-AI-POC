000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5126100.                                                
000300 AUTHOR.         DADHICH PRERNA.                                          
000400 DATE-WRITTEN.   18/12/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000810*        READS WDK6 EXTRACT (W01160) AND WDK7 EXTRACT (W01184)            
000820*        AND CREATES OUTPUT RECORD WITH COMPANY CODE AS 57                
000900*                                                                         
001000*        THE PROGRAM READS     WDB6                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002310*          --- EXTRACT OF WDK6                                            
002320     SELECT W01160                     ASSIGN TO W51261D1.                
002330     SKIP2                                                                
002400*          --- EXTRACT OF WDK7                                            
002500     SELECT W01184                     ASSIGN TO W51261D2.                
002900     SKIP2                                                                
003000*          --- OUTPUT FILE WITH IDFTG=57                                  
003100     SELECT W51261                     ASSIGN TO W51261D3.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W01160                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W01160   -L.                                                   
004200     SKIP3                                                                
004300 FD  W01184                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W01184   -L.                                                   
004800     SKIP3                                                                
004900 FD  W51261                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  RECORD -COPY W51261 -PRE  UT-  -L.                                   
005470     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W5126100'.            
005800 77  YES                         PIC X       VALUE 'J'.                   
005900 77  NOO                         PIC X       VALUE 'N'.                   
006000 77  WS-IDDC-CDC                 PIC X(2)    VALUE '11'.                  
006100                                                                          
006200 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W01160                       VALUE 'J'.                   
006400                                                                          
006500 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W01184                       VALUE 'J'.                   
006700     EJECT                                                                
006800                                                                          
006900 77  WS-IDFTG-VALUE              PIC X(2).                                
007000     88  VALID-IDFTG                         VALUE '57'.                  
007100     EJECT                                                                
007200                                                                          
007300 01  WS-IDFTG-TABEL.                                                      
007400     03 WS-SAVE-IDFTG OCCURS 99  INDEXED BY WS-IDFTG-IX.                  
007500        05 WS-IDDC               PIC X(2).                                
007600        05 WS-IDFTG              PIC X(2).                                
007700                                                                          
007800 01  WS-IDARTNR-SPAR             PIC S9(9)   VALUE ZERO COMP-3.           
007900*                                                                         
008000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES TODAYS-DATE.                                        
008200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008400     03  TODAYS-DATE-DAY         PIC 9(2).                                
008500     EJECT                                                                
008600 01  GENERAL-SUBPROGRAMS.                                                 
008800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009200     SKIP2                                                                
009300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
009400                                                                          
009500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009800     SKIP2                                                                
009900 01  ERROR-TEXT.                                                          
010000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
010100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
010200     EJECT                                                                
010300 01  FELTEXT.                                                             
010400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010700*    --- PARAMETRAR TILL POSTSUM                                          
010900*01  -COPY W0005   -PRE  POSTSUM-                                         
011000     EJECT                                                                
011010                                                                          
011100 01  W01160-AREA-START           PIC X(24)   VALUE                        
011200                                 'W01160-AREA      '.                     
011300     SKIP2                                                                
011400                                                                          
011500*01  AREA -COPY W01160     -PRE W01160-                                   
011600     EJECT                                                                
011700 01  W01184-AREA-START           PIC X(24)   VALUE                        
011800                                 'W01184-AREA      '.                     
011900     SKIP2                                                                
012000                                                                          
012100*01  AREA -COPY W01184     -PRE W01184-                                   
012200     EJECT                                                                
012300 01  UT-AREA-START               PIC X(24)   VALUE                        
012400                                 'UT-AREA-START  '.                       
012500     SKIP2                                                                
012600                                                                          
012700*01  AREA -COPY W51261     -PRE UT-                                       
013140     EJECT                                                                
013150*    --- AREAS FOR IMS-SECTIONS                                           
013160*                                                                         
013170     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013300     SKIP3                                                                
013400 01  KEYS-FOR-DLI.                                                        
013500     03  W-IDDC-X.                                                        
013600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013700     SKIP2                                                                
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FOUND                       VALUE '  '.                  
014100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014400     SKIP2                                                                
014500 01  GOOD-STATUSCODES.                                                    
014600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                        PIC X(64).                               
014900 01  SSA2                        PIC X(64).                               
015000     EJECT                                                                
015100*    --- IMS FUNCTION CODES                                               
015200*01  -COPY W0003                                                          
015300     EJECT                                                                
015400*    ---  DLI INPUT-OUTPUT AREA                                           
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
015600 01  DLI-IO-WDB601.                                                       
015700*    03  -COPY WDB601                                                     
015800     EJECT                                                                
015900 LINKAGE SECTION.                                                         
016000                                                                          
016100*01  -COPY W0008  -PRE WDB6-                                              
016200     05  FILLER                  PIC X.                                   
016300     EJECT                                                                
016400 PROCEDURE DIVISION  USING WDB6-PCB.                                      
016500 MAIN SECTION.                                                            
016600     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
016700                                                                          
016800     PERFORM A-INIT                                                       
016900     PERFORM S01-READ-W01160                                              
017000     PERFORM S02-READ-W01184                                              
017100                                                                          
017200     PERFORM UNTIL END-OF-W01160                                          
017210       PERFORM B-CHECK-IDFTG-CDC                                          
017300         PERFORM UNTIL END-OF-W01184 OR                                   
017400                       W01184-SLAG-IDARTNR > W01160-CLAG-IDARTNR          
017500           IF W01184-SLAG-IDARTNR = W01160-CLAG-IDARTNR                   
017600              PERFORM B-CHECK-IDFTG                                       
017700           END-IF                                                         
017800           PERFORM S02-READ-W01184                                        
017900         END-PERFORM                                                      
018000       PERFORM S01-READ-W01160                                            
018100     END-PERFORM                                                          
018200                                                                          
018300     PERFORM Z-FINIT                                                      
018400                                                                          
018500     MOVE ZERO TO RETURN-CODE                                             
018600     GOBACK                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 A-INIT SECTION.                                                          
019000                                                                          
019100     OPEN INPUT  W01160                                                   
019200                 W01184                                                   
019300                                                                          
019400     OPEN OUTPUT W51261                                                   
019500                                                                          
019600     ACCEPT TODAYS-DATE  FROM DATE                                        
019800     PERFORM AA-LOAD-IDFTG-TABEL                                          
019810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019900     .                                                                    
020000     EJECT                                                                
020100                                                                          
020200 AA-LOAD-IDFTG-TABEL  SECTION.                                            
020300                                                                          
020400     INITIALIZE WS-IDFTG-TABEL                                            
020500     SET WS-IDFTG-IX TO +1                                                
020600     PERFORM IMS-GN-WDB601                                                
020700     PERFORM UNTIL SEGMENT-SLUT                                           
020800        MOVE DCS-IDDC            TO WS-IDDC (WS-IDFTG-IX)                 
020900        MOVE DCS-IDFTG           TO WS-IDFTG(WS-IDFTG-IX)                 
021000        PERFORM IMS-GN-WDB601                                             
021100        SET WS-IDFTG-IX UP BY +1                                          
021200        IF WS-IDFTG-IX      > 99                                          
021300           MOVE 'IDFTG TABLE FULL'                                        
021400                                 TO FELTEXT                               
021500           CALL FELLOG                                                    
021600        END-IF                                                            
021700     END-PERFORM                                                          
021800     .                                                                    
021900     EJECT                                                                
022000 B-CHECK-IDFTG SECTION.                                                   
022100     SET WS-IDFTG-IX TO  1                                                
022200     SEARCH WS-SAVE-IDFTG                                                 
022300       AT END                                                             
022400         CONTINUE                                                         
022500       WHEN ((WS-IDDC(WS-IDFTG-IX) = W01184-SLAG-IDDC))                   
022600         MOVE WS-IDFTG(WS-IDFTG-IX) TO WS-IDFTG-VALUE                     
022700         IF  VALID-IDFTG                                                  
022910           PERFORM BB-PROCESS-SDC-LDC-INFO                                
023000         END-IF                                                           
023100     END-SEARCH                                                           
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023410 B-CHECK-IDFTG-CDC SECTION.                                               
023420     SET WS-IDFTG-IX TO  1                                                
023430     SEARCH WS-SAVE-IDFTG                                                 
023440       AT END                                                             
023450         CONTINUE                                                         
023460       WHEN ((WS-IDDC(WS-IDFTG-IX) = WS-IDDC-CDC))                        
023470         MOVE WS-IDFTG(WS-IDFTG-IX) TO WS-IDFTG-VALUE                     
023480         IF  VALID-IDFTG                                                  
023490           PERFORM BA-PROCESS-CDC-INFO                                    
023492         END-IF                                                           
023493     END-SEARCH                                                           
023494     .                                                                    
023495     EJECT                                                                
023500 BA-PROCESS-CDC-INFO SECTION.                                             
023600                                                                          
023700     IF WS-IDARTNR-SPAR          NOT = W01160-CLAG-IDARTNR                
023800        MOVE W01160-CLAG-IDARTNR    TO WS-IDARTNR-SPAR                    
023900*                                                                         
024000        MOVE W01160-CLAG-IDARTNR    TO UT-IDARTNR                         
024100        MOVE WS-IDDC-CDC            TO UT-IDDC                            
024200        MOVE W01160-CLAG-IDLEVNR    TO UT-IDLEVNR                         
024300        MOVE W01160-CLAG-KDPRODSL   TO UT-KDPRODSL                        
024400        MOVE W01160-CLAG-PRARTSTD   TO UT-PRARTSTD                        
024500        MOVE W01160-CLAG-KVEFRS     TO UT-KVEFRS                          
024600        MOVE W01160-CLAG-KVLS       TO UT-KVLS                            
024700        MOVE W01160-CLAG-KVAKS-CDC  TO UT-KVAKS                           
024800        MOVE W01160-CLAG-TIFINLV    TO UT-TIFINLV                         
024900        MOVE W01160-CLAG-KDERS      TO UT-KDERS                           
025000        MOVE W01160-CLAG-KDKG       TO UT-KDKG                            
025100        MOVE W01160-CLAG-KVRESS     TO UT-KVRESS                          
025200        MOVE W01160-CLAG-KVPB-SEP   TO UT-KVPB-SEP                        
025300        MOVE W01160-CLAG-KVPB-SATS  TO UT-KVPB-SATS                       
025310        MOVE W01160-CLAG-KVAKS-PAV  TO UT-KVAKS-PAV                       
025400                                                                          
025500        PERFORM S11-WRITE-W51261                                          
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900                                                                          
026000 BB-PROCESS-SDC-LDC-INFO SECTION.                                         
026100                                                                          
026200     MOVE W01184-SLAG-IDARTNR    TO UT-IDARTNR                            
026300     MOVE W01184-SLAG-IDDC       TO UT-IDDC                               
026400     MOVE W01160-CLAG-IDLEVNR    TO UT-IDLEVNR                            
026500     MOVE W01160-CLAG-KDPRODSL   TO UT-KDPRODSL                           
026600     MOVE W01160-CLAG-PRARTSTD   TO UT-PRARTSTD                           
026700     MOVE W01184-SLAG-KVEFRS     TO UT-KVEFRS                             
026800     MOVE W01184-SLAG-KVLS       TO UT-KVLS                               
026900     MOVE W01184-SLAG-KVAKS-SDC  TO UT-KVAKS                              
027000     MOVE W01160-CLAG-TIFINLV    TO UT-TIFINLV                            
027100     MOVE W01160-CLAG-KDERS      TO UT-KDERS                              
027200     MOVE W01160-CLAG-KDKG       TO UT-KDKG                               
027300     MOVE W01184-SLAG-KVRESS     TO UT-KVRESS                             
027400     MOVE W01160-CLAG-KVPB-SEP   TO UT-KVPB-SEP                           
027500     MOVE W01160-CLAG-KVPB-SATS  TO UT-KVPB-SATS                          
027510     MOVE W01184-SLAG-KVAKS-PAV  TO UT-KVAKS-PAV                          
027600                                                                          
027700     PERFORM S11-WRITE-W51261                                             
027800     .                                                                    
027900     EJECT                                                                
028020                                                                          
028100 Z-FINIT SECTION.                                                         
028200     CLOSE W01160                                                         
028300           W01184                                                         
028400           W51261                                                         
028410                                                                          
028420     MOVE 'S'                TO POSTSUM-OPKOD                             
028430     CALL POSTSUM         USING POSTSUM-PARM                              
028500     SKIP2                                                                
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100 S01-READ-W01160  SECTION.                                                
029200     READ W01160               INTO W01160-AREA                           
029300     AT END                                                               
029400        MOVE HIGH-VALUE          TO W01160-AREA                           
029500        SET END-OF-W01160        TO TRUE                                  
029700     NOT AT END                                                           
029800        MOVE 'W01160'            TO POSTSUM-FDNAMN                        
029900        MOVE 'W51261D1'          TO POSTSUM-DDNAMN2                       
030000        MOVE '       '           TO POSTSUM-TRANSTYP                      
030110        CALL POSTSUM USING POSTSUM-PARM                                   
030200     END-READ                                                             
030300     .                                                                    
030400     EJECT                                                                
030500                                                                          
030600 S02-READ-W01184  SECTION.                                                
030700     READ W01184              INTO W01184-AREA                            
030800     AT END                                                               
030900        MOVE HIGH-VALUE          TO W01184-AREA                           
031000        SET END-OF-W01184        TO TRUE                                  
031200     NOT AT END                                                           
031300        MOVE 'W01184'            TO POSTSUM-FDNAMN                        
031400        MOVE 'W51261D2'          TO POSTSUM-DDNAMN2                       
031410        MOVE '       '           TO POSTSUM-TRANSTYP                      
031500        CALL POSTSUM  USING POSTSUM-PARM                                  
031600     END-READ                                                             
031700     .                                                                    
031800     EJECT                                                                
032000                                                                          
032100 S11-WRITE-W51261 SECTION.                                                
032200                                                                          
032300     WRITE UT-RECORD          FROM UT-AREA                                
032400                                                                          
032500     MOVE 'W51261'              TO POSTSUM-FDNAMN                         
032600     MOVE 'W51261D3'            TO POSTSUM-DDNAMN2                        
032700     CALL POSTSUM            USING POSTSUM-PARM                           
032800     .                                                                    
032900     EJECT                                                                
033000                                                                          
033100 S99-ABEND SECTION.                                                       
033200                                                                          
033300     SKIP2                                                                
033400     MOVE 'S'                   TO POSTSUM-OPKOD                          
033500     CALL POSTSUM            USING POSTSUM-PARM                           
033600     CALL ABEND              USING RKOD-ABEND                             
033700     .                                                                    
033800     EJECT                                                                
033900* --- IMS SECTIONS  ---                                                   
034000                                                                          
034100     EJECT                                                                
034200 IMS-GN-WDB601    SECTION.                                                
034300     MOVE 'WDB601  '            TO SSA1                                   
034400     MOVE '  GB'                TO GOOD-STATUSCODES                       
034500     CALL CBLTDLI            USING GN WDB6-PCB DLI-IO-WDB601 SSA1         
034600     MOVE WDB6-STATUS-CODE      TO STATUS-WS                              
034700     PERFORM IMS-STATUSCHECK                                              
034800     .                                                                    
034900     EJECT                                                                
035000                                                                          
035100 IMS-STATUSCHECK SECTION.                                                 
035200                                                                          
035300     SET STATUS-IX               TO 1                                     
035400     SEARCH GOOD-STATUS                                                   
035500       AT END                                                             
035600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035700           DELIMITED BY SIZE INTO ERROR-TEXT                              
035800         DISPLAY ERROR-TEXT                                               
035900         CALL FELLOG                                                      
036000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
036100         CONTINUE                                                         
036200     END-SEARCH                                                           
036300     .                                                                    
