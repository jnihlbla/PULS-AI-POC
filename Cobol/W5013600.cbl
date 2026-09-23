000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5013600.                                                
000300 AUTHOR.         MAMATHA SHETTY.                                          
000400 DATE-WRITTEN.   FEB 2023.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES UPDATES OF LCF FOR THAILAND                 
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDB6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W5T136                                              
001400*        MID:         W5I13601                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W5O136N1                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W5013600'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  YES                         PIC X       VALUE 'J'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200 77  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
003300 77  WS-START-DATE               PIC 9(6)    VALUE ZERO.                  
003400 77  WS-PERCENT-FROM             PIC 9(3)V9(3) VALUE ZERO.                
003500 77  WS-PERCENT                  PIC 9(4)V9(2) VALUE ZERO.                
003600 77  WS-RELANDCO-PERC            PIC 9(3)V9(1) VALUE ZERO.                
003700 77  WS-RELANDCO-PG-FROM-PERC    PIC 9(3)V9(3) VALUE ZERO.                
003800 77  WS-RELANDCO-PG-TO-PERC      PIC 9(3)V9(3) VALUE ZERO.                
003900 77  WS-RELANDCO-PG-FROM         PIC 9(3)V9(3) VALUE ZERO.                
004000 77  WS-RELANDCO-PG-TO           PIC 9(3)V9(3) VALUE ZERO.                
004100 77  WS-KDPRODSL                 PIC X(2) VALUE SPACES.                   
004200 77  W-SAVE-KDPRODSL             PIC S9(3)   VALUE ZERO COMP-3.           
004300 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004400 77  WS-IDFRIDATA                PIC X(25).                               
004500 77  WS-CNT                      PIC 9(2)    VALUE ZERO.                  
004600 77  WS-PGAD-KDPRODSL            PIC X(3)    VALUE SPACE.                 
004700                                                                          
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005100     88  INDATA-OK                           VALUE 'J'.                   
005200     88  INDATA-WRONG                        VALUE 'N'.                   
005300                                                                          
005400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005500     88  KEYS-OK                             VALUE 'J'.                   
005600     88  KEYS-WRONG                          VALUE 'N'.                   
005700                                                                          
005800 77  INVALID-SW                  PIC X       VALUE 'J'.                   
005900     88  IDFTG-YES                           VALUE 'J'.                   
006000     88  INVALID-NO                          VALUE 'N'.                   
006100                                                                          
006200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006300     88  OWN-MID                             VALUE '5136'.                
006400     88  GOOD-MID                            VALUE '5136'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700 01  W-DAGENS-DATUM-ONE-YEAR            PIC 9(6) VALUE ZERO.              
006800 01  FILLER REDEFINES W-DAGENS-DATUM-ONE-YEAR.                            
006900     03  W-DAGENS-DATUM-ONE-YEAR-YY     PIC 9(2).                         
007000     03  W-DAGENS-DATUM-ONE-YEAR-MM     PIC 9(2).                         
007100     03  W-DAGENS-DATUM-ONE-YEAR-DD     PIC 9(2).                         
007200                                                                          
007300 01  WORK-DATE                   PIC 9(6)   VALUE ZERO.                   
007400 01  FILLER REDEFINES WORK-DATE.                                          
007500     03  WORK-DATUM-YY                  PIC 9(2).                         
007600     03  WORK-DATUM-MM                  PIC 9(2).                         
007700     03  WORK-DATUM-DD                  PIC 9(2).                         
007800                                                                          
007900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008000 01  GENERAL-SUBPROGRAMS.                                                 
008100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008600     EJECT                                                                
008700*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
008800*01  -COPY WDECAREA                                                       
008900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009000*01 -COPY WMEDAREA                                                        
009100*01 -COPY WWDCKONS                                                        
009200*01  -COPY WDATAREA                                                       
009300 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
009400     SKIP3                                                                
009500*    -COPY WZ20DATE                                                       
009600     EJECT                                                                
009700*                                                                         
009800 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
009900     SKIP3                                                                
010000*    -COPY WZ20DAYS                                                       
010100     EJECT                                                                
010200*                                                                         
010300     EJECT                                                                
010400     SKIP3                                                                
010500 01  MESSAGE-CODES.                                                       
010600     03  ERR-CORR-HILITE-FLDS     PIC X(3)    VALUE '001'.                
010700     03  ERR-PF11-AND-NO-DATA     PIC X(3)    VALUE '011'.                
010800     03  INF-PRESS-PF11           PIC X(3)    VALUE '003'.                
010900     03  INF-UPDATE-DONE          PIC X(3)    VALUE '101'.                
011000     03  ERR-WRONG-KEY            PIC X(3)    VALUE '401'.                
011100     03  ERR-INVALID-VALUE        PIC X(3)    VALUE '492'.                
011200     03  INF-FIRST-PAGE           PIC X(3)    VALUE '006'.                
011300     03  ERR-FUTURE-DATE          PIC X(3)    VALUE '363'.                
011400     03  LAST-PAGE                PIC X(3)    VALUE '106'.                
011500     03  INF-MORE-LINE            PIC X(3)    VALUE '402'.                
011600     03  ERR-USER-NOT-AUTH        PIC X(3)    VALUE '405'.                
011700     EJECT                                                                
011800*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012100     SKIP3                                                                
012200*01 -COPY WMSGINIT                                                        
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012500     SKIP3                                                                
012600*01  MID -COPY W5I13601                                                   
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012900     SKIP3                                                                
013000*01  -COPY WMSGAREA                                                       
013100     EJECT                                                                
013200     03  MOD REDEFINES MSG-AREA.                                          
013300*      05  -COPY W5O13601                                                 
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013600     SKIP3                                                                
013700*01  -COPY WMFSAREA                                                       
013800     EJECT                                                                
013900*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014200     SKIP3                                                                
014300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
014400 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
014500 01  KEYS-FOR-DLI.                                                        
014600     03  W-KDSEGKEY-X.                                                    
014700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
014800     03  W-IDDC-X.                                                        
014900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015000     03  W-KDPRODSL-X.                                                    
015100         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
015200     03  W-KDPRODSL-MIN-X.                                                
015300         05  W-KDPRODSL-MIN      PIC S9(3)   VALUE ZERO COMP-3.           
015400     03  W-KDPRODSL-MAX-X.                                                
015500         05  W-KDPRODSL-MAX      PIC S9(3)   VALUE ZERO COMP-3.           
015600     03  W-WDP701KY-X.                                                    
015700         05  W-IDUSER            PIC X(8)     VALUE SPACE.                
015800     03  W-IDFTG-X.                                                       
015900         05  W-IDFTG             PIC X(2)    VALUE SPACE.                 
016000     SKIP2                                                                
016100*    --- STATUS CODES FROM IMS                                            
016200 01  SAVE-AREA.                                                           
016300     03  SAVE-IDTRANS             PIC X(4)    VALUE '5136'.               
016400     03  SAVE-KDPRODSL-ENTER      PIC S9(3)   VALUE ZERO COMP-3.          
016500     03  SAVE-KDPRODSL-NEXT       PIC S9(3)   VALUE ZERO COMP-3.          
016600 01  STATUS-WS                    PIC XX.                                 
016700     88  SEGMENT-FOUND                       VALUE '  '.                  
016800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017000     88  END-OF-DATABASE                     VALUE 'GB'.                  
017100     SKIP2                                                                
017200 01  GOOD-STATUSCODES.                                                    
017300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017400     SKIP3                                                                
017500 01  SSA1                        PIC X(64).                               
017600 01  SSA2                        PIC X(64).                               
017700 01  SSA3                        PIC X(121).                              
017800     EJECT                                                                
017900*    --- IMS FUNCTION CODES                                               
018000 01  DLI-IO-WDP701.                                                       
018100*    03  -COPY WDP701                                                     
018200     EJECT                                                                
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500*    ---  DLI INPUT-OUTPUT AREA                                           
018600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
018700 01  DLI-IO-WDB617.                                                       
018800*    03  -COPY WDB617                                                     
018900     EJECT                                                                
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB621'.                      
019100 01  DLI-IO-WDB621.                                                       
019200*    03  -COPY WDB621                                                     
019300     EJECT                                                                
019400 LINKAGE SECTION.                                                         
019500*01  -COPY W0009   -PRE MSG-                                              
019600*01  -COPY W0008   -PRE WDP7-                                             
019700     05  FILLER                  PIC X.                                   
019800                                                                          
019900*01  -COPY W0008  -PRE WDB6-                                              
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
020300 MAIN SECTION.                                                            
020400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
020500                                                                          
020600     PERFORM IMS-GET-MSG                                                  
020700     IF SEGMENT-FOUND                                                     
020800       PERFORM A-INIT                                                     
020900       PERFORM B-CHECK-KEYS                                               
021000       IF KEYS-OK                                                         
021100         IF MFS-UPDATE                                                    
021200           PERFORM G-CHECK-INPUT                                          
021300           IF INDATA-OK                                                   
021400             PERFORM H-UPDATE                                             
021500           END-IF                                                         
021600         ELSE                                                             
021700           IF MFS-FIRST                                                   
021800             PERFORM C-FIRST-PAGE                                         
021900           ELSE                                                           
022000             IF MFS-NEXT                                                  
022100               PERFORM D-NEXT-PAGE                                        
022200             ELSE                                                         
022300               PERFORM E-SAME-PAGE                                        
022400             END-IF                                                       
022500           END-IF                                                         
022600         END-IF                                                           
022700         PERFORM F-READ-SHOW-INFO                                         
022800       END-IF                                                             
022900       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O13601 + 4                      
023000       PERFORM IMS-INSERT-MSG                                             
023100     END-IF                                                               
023200                                                                          
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK                                                               
023500     .                                                                    
023600     EJECT                                                                
023700 A-INIT SECTION.                                                          
023800     IF MSG-DOUBLE-TRANSACTIONS                                           
023900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W5I13601                 
024000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024200     ELSE                                                                 
024300       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W5I13601                  
024400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
024500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024600     END-IF                                                               
024700                                                                          
024800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025100                                                                          
025200     MOVE LOW-VALUE TO MSG-AREA                                           
025300     MOVE 'W5O136N1' TO MFS-IDMOD                                         
025400     MOVE '5136' TO MOD-IDTRANS                                           
025500     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
025600                                                                          
025700     IF OWN-MID  OR HELP-MID                                              
025800       CONTINUE                                                           
025900     ELSE                                                                 
026000       MOVE SPACE TO MFS-KDTRTYP                                          
026100       MOVE '7'   TO MFS-IDPFK                                            
026200     END-IF                                                               
026300     ACCEPT TODAYS-DATE FROM DATE                                         
026400     ACCEPT WORK-DATE    FROM DATE                                        
026500                                                                          
026600     MOVE WORK-DATE             TO DATE-TIDATE                            
026700     MOVE ZERO TO INDX                                                    
026800     .                                                                    
026900     EJECT                                                                
027000 B-CHECK-KEYS SECTION.                                                    
027100                                                                          
027200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
027300     MOVE '001'             TO MSGI-KDCALL                                
027400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027600     MOVE '5136'            TO MSGI-IDTRANS                               
027700     IF GOOD-MID                                                          
027800        MOVE MID-KDPRODSL-IN     TO MSGI-KDPRODSL                         
027900     END-IF                                                               
028000     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
028100     MOVE MSGI-SPAR-AREA TO  SAVE-AREA                                    
028200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
028300                                                                          
028400     MOVE YES               TO KEYS-SW                                    
028500                                                                          
028600** DC WITH FTG CODE ONLY 63ALLOWED **                                     
028700     MOVE MSGI-IDUSER TO W-IDUSER                                         
028800     MOVE MSGI-IDFTG        TO W-IDFTG                                    
028900                               MOD-IDFTG                                  
029000     MOVE MSGI-IDDC         TO W-IDDC                                     
029100                                                                          
029200     IF MSGI-IDFTG = 63                                                   
029300       MOVE YES TO KEYS-SW                                                
029400     ELSE                                                                 
029500       MOVE NOO TO KEYS-SW                                                
029600     END-IF                                                               
029700     IF KEYS-WRONG                                                        
029800       MOVE ERR-USER-NOT-AUTH TO MED-IDMFSFEL                             
029900       CALL WMEDKONV     USING   MED-WMEDAREA                             
030000       MOVE MED-TEMFSFEL   TO    MOD-TEMFSFEL                             
030100     ELSE                                                                 
030200       PERFORM BA-CHECK-KDPRODSL                                          
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 BA-CHECK-KDPRODSL SECTION.                                               
030700                                                                          
030800     MOVE MFS-ERASE-FIELD   TO MOD-KDPRODSL-IN                            
030900     IF MID-KDPRODSL-IN  NOT  = ALL '+'                                   
031000        MOVE MID-KDPRODSL-IN  TO WS-KDPRODSL                              
031100                                 MOD-KDPRODSL-UT                          
031200        MOVE SPACE TO MFS-KDTRTYP                                         
031300        MOVE '7'   TO MFS-IDPFK                                           
031400     ELSE                                                                 
031500        MOVE MSGI-KDPRODSL    TO WS-KDPRODSL                              
031600                                 MOD-KDPRODSL-UT                          
031700     END-IF                                                               
031800     IF WS-KDPRODSL IS NUMERIC                                            
031900       MOVE YES                  TO KEYS-SW                               
032000       MOVE WS-KDPRODSL          TO W-KDPRODSL-MIN                        
032100                                    W-KDPRODSL-MAX                        
032200     ELSE                                                                 
032300       MOVE YES                  TO KEYS-SW                               
032400       MOVE SPACES               TO MOD-KDPRODSL-UT                       
032500       MOVE LOW-VALUE            TO W-KDPRODSL-MIN-X                      
032600       MOVE HIGH-VALUE           TO W-KDPRODSL-MAX-X                      
032700     END-IF                                                               
032800     IF INVALID-NO                                                        
032900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
033000       CALL WMEDKONV USING   MED-WMEDAREA                                 
033100       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 C-FIRST-PAGE SECTION.                                                    
033600                                                                          
033700     PERFORM MFS-ERASE-FIELD-IN                                           
033800     .                                                                    
033900     EJECT                                                                
034000 E-SAME-PAGE SECTION.                                                     
034100                                                                          
034200       IF MID-INPUT = ALL '+'                                             
034300         PERFORM MFS-ERASE-FIELD-IN                                       
034400       ELSE                                                               
034500         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
034600         CALL WMEDKONV  USING MED-WMEDAREA                                
034700         MOVE MED-MFSFEL   TO MOD-TEMFSFEL                                
034800         PERFORM MFS-READ-IN-AGAIN                                        
034900         PERFORM EA-MID-INDATA-TILL-MOD                                   
035000       END-IF                                                             
035100                                                                          
035200     IF SAVE-IDTRANS = '5136' OR '0551'                                   
035300       IF SAVE-KDPRODSL-ENTER  NUMERIC                                    
035400         MOVE SAVE-KDPRODSL-ENTER  TO W-KDPRODSL-MIN                      
035500                                      W-KDPRODSL                          
035600       ELSE                                                               
035700         MOVE LOW-VALUE          TO W-KDPRODSL-MIN-X                      
035800         MOVE HIGH-VALUE         TO W-KDPRODSL-MAX-X                      
035900       END-IF                                                             
036000     ELSE                                                                 
036100         PERFORM MFS-ERASE-FIELD-IN                                       
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 D-NEXT-PAGE SECTION.                                                     
036600                                                                          
036700     IF SAVE-IDTRANS = '5136'                                             
036800       IF SAVE-KDPRODSL-NEXT  NUMERIC                                     
036900         MOVE SAVE-KDPRODSL-NEXT  TO W-KDPRODSL-MIN                       
037000                                     W-KDPRODSL                           
037100       ELSE                                                               
037200         MOVE ZERO               TO W-KDPRODSL-MIN                        
037300                                     W-KDPRODSL                           
037400                                                                          
037500       END-IF                                                             
037600     ELSE                                                                 
037700       PERFORM MFS-ERASE-FIELD-IN                                         
037800     END-IF                                                               
037900                                                                          
038000     .                                                                    
038100     EJECT                                                                
038200 EA-MID-INDATA-TILL-MOD SECTION.                                          
038300                                                                          
038400     IF MID-KDPRODSL = ALL '+'                                            
038500       MOVE MFS-ERASE-FIELD         TO MOD-KDPRODSL-UPD                   
038600     ELSE                                                                 
038700       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KDPRODSL-UPD                   
038800     END-IF                                                               
038900                                                                          
039000     IF MID-RELANDCO-PG-FROM = ALL '+'                                    
039100       MOVE MFS-ERASE-FIELD        TO MOD-RELANDCO-PG-FROM-UPD            
039200     ELSE                                                                 
039300       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-PG-FROM-UPD            
039400     END-IF                                                               
039500                                                                          
039600     IF MID-TILANDCO = ALL '+'                                            
039700       MOVE MFS-ERASE-FIELD        TO MOD-TILANDCO-UPD                    
039800     ELSE                                                                 
039900       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TILANDCO-UPD                    
040000     END-IF                                                               
040100                                                                          
040200     IF MID-KDCMD  = ALL '+'                                              
040300       MOVE MFS-ERASE-FIELD         TO MOD-KDCMD-UPD                      
040400     ELSE                                                                 
040500       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KDCMD-UPD                      
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900                                                                          
041000 F-READ-SHOW-INFO SECTION.                                                
041100                                                                          
041200     PERFORM IMS-GU-WDB617                                                
041300     PERFORM IMS-GHNP-WDB621                                              
041400     MOVE +1                     TO INDX                                  
041500     PERFORM UNTIL INDX > MAX-INDX                                        
041600       IF SEGMENT-FOUND                                                   
041700         IF INDX = 1                                                      
041800            MOVE PGAD-KDPRODSL       TO SAVE-KDPRODSL-ENTER               
041900         END-IF                                                           
042000         MOVE PGAD-KDPRODSL          TO WS-PGAD-KDPRODSL                  
042100         MOVE WS-PGAD-KDPRODSL(2:2)  TO MOD-KDPRODSL(INDX)                
042200         COMPUTE WS-RELANDCO-PG-FROM =                                    
042300                  (PGAD-RELANDCO-PG-FROM) * 100                           
042400         MOVE WS-RELANDCO-PG-FROM TO MOD-RELANDCO-PG-FROM(INDX)           
042500         COMPUTE WS-RELANDCO-PG-TO   =                                    
042600                  (PGAD-RELANDCO-PG-TO  ) * 100                           
042700         MOVE WS-RELANDCO-PG-TO   TO MOD-RELANDCO-PG-TO(INDX)             
042800         MOVE PGAD-TILANDCO          TO MOD-TILANDCO(INDX)                
042900         MOVE PGAD-TIUPPDAT          TO MOD-TIUPPDAT(INDX)                
043000         MOVE PGAD-IDUSER            TO MOD-IDUSER(INDX)                  
043100         PERFORM IMS-GHNP-WDB621                                          
043200       ELSE                                                               
043300         PERFORM MFS-ERASE-FIELD-OUT                                      
043400       END-IF                                                             
043500       ADD  +1 TO INDX                                                    
043600     END-PERFORM                                                          
043700       IF SEGMENT-FOUND                                                   
043800         MOVE PGAD-KDPRODSL     TO SAVE-KDPRODSL-NEXT                     
043900         MOVE INF-MORE-LINE TO MED-IDMFSINF                               
044000         CALL WMEDKONV USING MED-WMEDAREA                                 
044100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
044200       ELSE                                                               
044300         MOVE LAST-PAGE TO MED-IDMFSINF                                   
044400         CALL WMEDKONV USING MED-WMEDAREA                                 
044500         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
044600       END-IF                                                             
044700       MOVE '002' TO MSGI-KDCALL                                          
044800       MOVE '5136' TO SAVE-IDTRANS                                        
044900       MOVE SAVE-AREA TO MSGI-SPAR-AREA                                   
045000       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
045100     .                                                                    
045200     EJECT                                                                
045300 G-CHECK-INPUT SECTION.                                                   
045400                                                                          
045500     MOVE YES  TO INDATA-SW                                               
045600       IF  (MID-INPUT     = ALL '+' OR SPACES)                            
045700       AND (MID-KDCMD     = ALL '+' OR SPACES)                            
045800          MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                      
045900          CALL WMEDKONV           USING MED-WMEDAREA                      
046000          MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                      
046100          PERFORM MFS-ERASE-FIELD-IN                                      
046200          PERFORM MFS-ERASE-FIELD-UPD                                     
046300          MOVE NOO                   TO INDATA-SW                         
046400       ELSE                                                               
046500        IF MID-KDCMD  = ALL '+' OR SPACES                                 
046600          PERFORM GD-CHECK-KDPRODSL                                       
046700          PERFORM GA-CHECK-RELANDCO                                       
046800          PERFORM GD-CHECK-TILANDCO                                       
046900        ELSE                                                              
047000         IF MID-KDCMD = 'D'                                               
047100           MOVE MFS-NUM-FIELD-OK    TO MOD-KDCMD-UPD-ATTR                 
047200           PERFORM GE-CHECK-KDPRODSL                                      
047300         ELSE                                                             
047400           MOVE MFS-NUM-FIELD-WRONG TO MOD-KDCMD-UPD-ATTR                 
047500           MOVE MFS-NUM-FIELD-WRONG TO MOD-KDPRODSL-UPD-ATTR              
047600           MOVE MID-KDPRODSL        TO W-KDPRODSL                         
047700           MOVE NOO               TO INDATA-SW                            
047800         END-IF                                                           
047900        END-IF                                                            
048000          IF INDATA-OK                                                    
048100             CONTINUE                                                     
048200          ELSE                                                            
048300           IF MED-TEMFSFEL = ERR-FUTURE-DATE                              
048400              MOVE ERR-FUTURE-DATE        TO MED-IDMFSFEL                 
048500           END-IF                                                         
048600                                                                          
048700           IF MED-TEMFSFEL NOT = ERR-FUTURE-DATE                          
048800             MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                  
048900           END-IF                                                         
049000            CALL WMEDKONV      USING MED-WMEDAREA                         
049100            MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                            
049200            PERFORM MFS-DONT-TOUCH-FIELD-OUT                              
049300            PERFORM MFS-DONT-TOUCH-FIELD-IN                               
049400          END-IF                                                          
049500       END-IF                                                             
049600     .                                                                    
049700     EJECT                                                                
049800 GD-CHECK-KDPRODSL SECTION.                                               
049900                                                                          
050000     IF MID-KDPRODSL NOT = ALL '+'                                        
050100     AND (MID-KDCMD  = ALL '+' OR SPACES)                                 
050200       INSPECT MID-KDPRODSL REPLACING LEADING SPACE                       
050300                               BY ZERO                                    
050400       IF MID-KDPRODSL NUMERIC                                            
050500         MOVE MFS-NUM-FIELD-OK    TO MOD-KDPRODSL-UPD-ATTR                
050600       ELSE                                                               
050700         MOVE MFS-NUM-FIELD-WRONG TO MOD-KDPRODSL-UPD-ATTR                
050800         MOVE NOO                 TO INDATA-SW                            
050900       END-IF                                                             
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 GE-CHECK-KDPRODSL SECTION.                                               
051400                                                                          
051500     IF MID-KDPRODSL NOT = ALL '+'                                        
051600       INSPECT MID-KDPRODSL REPLACING LEADING SPACE                       
051700                               BY ZERO                                    
051800       IF MID-KDPRODSL NUMERIC                                            
051900         MOVE MFS-NUM-FIELD-OK    TO MOD-KDPRODSL-UPD-ATTR                
052000       ELSE                                                               
052100         MOVE MFS-NUM-FIELD-WRONG TO MOD-KDPRODSL-UPD-ATTR                
052200         MOVE NOO                 TO INDATA-SW                            
052300       END-IF                                                             
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 GA-CHECK-RELANDCO SECTION.                                               
052800                                                                          
052900     IF MID-RELANDCO-PG-FROM NOT = ALL '+'  AND MID-KDCMD = SPACES        
053000       MOVE MFS-NUM-FIELD-OK      TO MOD-RELANDCO-PG-FROM-UPD-ATTR        
053100     ELSE                                                                 
053200       INSPECT MID-RELANDCO-PG-FROM TALLYING WS-CNT                       
053300        FOR ALL '-'                                                       
053400        IF WS-CNT > 0                                                     
053500         MOVE MFS-NUM-FIELD-WRONG TO MOD-RELANDCO-PG-FROM-UPD-ATTR        
053600          MOVE NOO                    TO INDATA-SW                        
053700        ELSE                                                              
053800          MOVE MID-RELANDCO-PG-FROM TO WS-IDFRIDATA                       
053900          MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                    
054000          MOVE 3                      TO DEC-KVHELTAL                     
054100          MOVE 1                      TO DEC-KVDECIMAL                    
054200           CALL WDECEDIT            USING DEC-WDECAREA                    
054300                                                                          
054400           IF DEC-KDSVAR-OK                                               
054500            MOVE DEC-IDEDITDATA      TO WS-RELANDCO-PG-FROM               
054600            MOVE MFS-NUM-FIELD-OK TO MOD-RELANDCO-PG-FROM-UPD-ATTR        
054700           ELSE                                                           
054800            MOVE MFS-NUM-FIELD-WRONG TO                                   
054900                                    MOD-RELANDCO-PG-FROM-UPD-ATTR         
055000             MOVE NOO                TO INDATA-SW                         
055100           END-IF                                                         
055200        END-IF                                                            
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 GD-CHECK-TILANDCO SECTION.                                               
055700                                                                          
055800     IF MID-TILANDCO NOT = ALL '+'                                        
055900     AND (MID-KDCMD  = ALL '+' OR SPACES)                                 
056000       INSPECT MID-TILANDCO REPLACING LEADING SPACE                       
056100                                   BY ZERO                                
056200       IF MID-TILANDCO NUMERIC                                            
056300         MOVE MID-TILANDCO          TO WS-START-DATE                      
056400         MOVE WORK-DATE               TO DATE-TIDATE                      
056500         MOVE 'YYMMDD'                TO DATE-KDDATFMT                    
056600         CALL WZ20DATE USING DATE-WZ20DATE                                
056700          IF DATE-KDRC = 0                                                
056800            IF WS-START-DATE    <  DATE-TIDATE                            
056900              MOVE ERR-FUTURE-DATE     TO MED-TEMFSFEL                    
057000              MOVE MFS-NUM-FIELD-WRONG TO MOD-TILANDCO-UPD-ATTR           
057100              MOVE NOO                 TO INDATA-SW                       
057200            ELSE                                                          
057300              MOVE MFS-NUM-FIELD-OK    TO MOD-TILANDCO-UPD-ATTR           
057400            END-IF                                                        
057500          END-IF                                                          
057600       ELSE                                                               
057700         MOVE MFS-NUM-FIELD-WRONG TO MOD-TILANDCO-UPD-ATTR                
057800         MOVE NOO                 TO INDATA-SW                            
057900       END-IF                                                             
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400 H-UPDATE SECTION.                                                        
058500                                                                          
058600     MOVE WC-NDC-TH   TO W-IDDC                                           
058700     MOVE MID-KDPRODSL     TO W-KDPRODSL                                  
058800     PERFORM IMS-GHU-WDB621                                               
058900     IF SEGMENT-FOUND                                                     
059000       IF MID-KDCMD  = 'D' AND MID-KDPRODSL NOT = ALL '+'                 
059100        PERFORM IMS-DLET-WDB621                                           
059200       ELSE                                                               
059300        PERFORM HB-MOVE-FIELDS                                            
059400        PERFORM IMS-REPL-WDB621                                           
059500       END-IF                                                             
059600     ELSE                                                                 
059700       IF  (MID-KDCMD  = ALL '+' OR SPACES)                               
059800        PERFORM HA-INIT-FIELDS                                            
059900        PERFORM HB-MOVE-FIELDS-ISRT                                       
060000        PERFORM IMS-ISRT-WDB621                                           
060100       END-IF                                                             
060200     END-IF                                                               
060300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
060400     CALL WMEDKONV USING MED-WMEDAREA                                     
060500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
060600     PERFORM MFS-FORM-ATTR                                                
060700     PERFORM MFS-ERASE-FIELD-IN                                           
060800     .                                                                    
060900     EJECT                                                                
061000 HA-INIT-FIELDS SECTION.                                                  
061100     INITIALIZE    PGAD-RELANDCO-PG-TO                                    
061200                   PGAD-TILANDCO                                          
061300                   PGAD-TIUPPDAT                                          
061400                   PGAD-KDPRODSL                                          
061500                   PGAD-IDUSER                                            
061600                   PGAD-RELANDCO-PG-FROM                                  
061700     MOVE TODAYS-DATE            TO PGAD-TIUPPDAT                         
061800     MOVE MSGI-IDUSER            TO PGAD-IDUSER                           
061900     .                                                                    
062000     EJECT                                                                
062100 HB-MOVE-FIELDS-ISRT SECTION.                                             
062200                                                                          
062300     IF MID-KDPRODSL NOT = ALL '+'                                        
062400       MOVE MID-KDPRODSL           TO PGAD-KDPRODSL                       
062500                                      W-KDPRODSL                          
062600     END-IF                                                               
062700                                                                          
062800     IF MID-TILANDCO NOT = ALL '+'                                        
062900       MOVE MID-TILANDCO           TO PGAD-TILANDCO                       
063000     ELSE                                                                 
063100       MOVE TODAYS-DATE            TO PGAD-TILANDCO                       
063200     END-IF                                                               
063300                                                                          
063400     PERFORM IMS-GU-WDB617                                                
063500     IF SEGMENT-FOUND                                                     
063600       IF MID-RELANDCO-PG-FROM NOT = ALL '+'                              
063700         MOVE PROC-RELANDCO-TO  TO PGAD-RELANDCO-PG-TO                    
063800         COMPUTE WS-RELANDCO-PG-FROM-PERC =                               
063900                                    WS-RELANDCO-PG-FROM / 100             
064000         MOVE WS-RELANDCO-PG-FROM-PERC TO PGAD-RELANDCO-PG-FROM           
064100                                          PGAD-RELANDCO-PG-TO             
064200       END-IF                                                             
064300     END-IF                                                               
064400     MOVE TODAYS-DATE            TO PGAD-TIUPPDAT                         
064500     MOVE MSGI-IDUSER            TO PGAD-IDUSER                           
064600     .                                                                    
064700     EJECT                                                                
064800 HB-MOVE-FIELDS SECTION.                                                  
064900                                                                          
065000     IF MID-KDPRODSL NOT = ALL '+'                                        
065100       MOVE MID-KDPRODSL          TO PGAD-KDPRODSL                        
065200                                     W-KDPRODSL                           
065300     END-IF                                                               
065400                                                                          
065500     IF MID-TILANDCO NOT = ALL '+'                                        
065600       MOVE MID-TILANDCO           TO PGAD-TILANDCO                       
065700     ELSE                                                                 
065800       MOVE TODAYS-DATE            TO PGAD-TILANDCO                       
065900     END-IF                                                               
066000                                                                          
066100     IF MID-RELANDCO-PG-FROM NOT = ALL '+'                                
066200       COMPUTE WS-RELANDCO-PG-FROM-PERC =                                 
066300                                     WS-RELANDCO-PG-FROM / 100            
066400       MOVE PGAD-RELANDCO-PG-FROM TO PGAD-RELANDCO-PG-TO                  
066500       MOVE WS-RELANDCO-PG-FROM-PERC TO PGAD-RELANDCO-PG-FROM             
066600     END-IF                                                               
066700                                                                          
066800     MOVE TODAYS-DATE            TO PGAD-TIUPPDAT                         
066900     MOVE MSGI-IDUSER            TO PGAD-IDUSER                           
067000     .                                                                    
067100     EJECT                                                                
067200 MFS-ERASE-FIELD-UPD SECTION.                                             
067300                                                                          
067400*    --- ALLA UTDATA-FÄLT                                                 
067500     MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL-UPD                             
067600                             MOD-RELANDCO-PG-FROM-UPD                     
067700                             MOD-TILANDCO-UPD                             
067800                             MOD-KDPRODSL-UT                              
067900                                                                          
068000     .                                                                    
068100     SKIP3                                                                
068200 MFS-ERASE-FIELD-OUT SECTION.                                             
068300     MOVE MFS-ERASE-FIELD   TO MOD-RELANDCO-PG-FROM(INDX)                 
068400                               MOD-TILANDCO(INDX)                         
068500                               MOD-KDPRODSL(INDX)                         
068600                               MOD-TIUPPDAT(INDX)                         
068700                               MOD-IDUSER(INDX)                           
068800                               MOD-RELANDCO-PG-TO(INDX)                   
068900     .                                                                    
069000 MFS-ERASE-FIELD-IN SECTION.                                              
069100                                                                          
069200     MOVE ZERO TO INDX                                                    
069300     IF INDX > MAX-INDX                                                   
069400       PERFORM UNTIL INDX > MAX-INDX                                      
069500         MOVE MFS-ERASE-FIELD   TO MOD-RELANDCO-PG-FROM(INDX)             
069600                                   MOD-TILANDCO(INDX)                     
069700                                   MOD-KDPRODSL(INDX)                     
069800                                   MOD-TIUPPDAT(INDX)                     
069900                                   MOD-IDUSER(INDX)                       
070000                                   MOD-RELANDCO-PG-TO(INDX)               
070100         ADD +1 TO INDX                                                   
070200       END-PERFORM                                                        
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
070700                                                                          
070800*    --- ALLA UTDATA-FÄLT                                                 
070900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRODSL-UPD                      
071000                                    MOD-KDPRODSL-UT                       
071100                                    MOD-RELANDCO-PG-FROM-UPD              
071200                                    MOD-TILANDCO-UPD                      
071300                                    MOD-KDCMD-UPD                         
071400     .                                                                    
071500     SKIP3                                                                
071600 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
071700                                                                          
071800*    --- ALLA INDATA-FÄLT                                                 
071900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRODSL-UPD                      
072000                                    MOD-KDCMD-UPD                         
072100                                    MOD-RELANDCO-PG-FROM-UPD              
072200                                    MOD-TILANDCO-UPD                      
072300     .                                                                    
072400     EJECT                                                                
072500 MFS-FORM-ATTR SECTION.                                                   
072600                                                                          
072700*    --- ALL INDATA-FIELDS                                                
072800     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDPRODSL-UPD-ATTR                
072900                                     MOD-RELANDCO-PG-FROM-UPD-ATTR        
073000                                     MOD-TILANDCO-UPD-ATTR                
073100                                     MOD-KDCMD-UPD-ATTR                   
073200     .                                                                    
073300     SKIP2                                                                
073400* --- IMS SECTIONS ---                                                    
073500     SKIP3                                                                
073600 MFS-READ-IN-AGAIN SECTION.                                               
073700                                                                          
073800*    --- ALL INDATA-FIELDS                                                
073900     MOVE MFS-ADD-READ-FIELD TO MOD-KDPRODSL-UPD-ATTR                     
074000                                MOD-RELANDCO-PG-FROM-UPD-ATTR             
074100                                MOD-TILANDCO-UPD-ATTR                     
074200                                MOD-KDCMD-UPD-ATTR                        
074300     .                                                                    
074400     EJECT                                                                
074500* --- IMS SECTIONS ---                                                    
074600     SKIP3                                                                
074700 IMS-GET-MSG SECTION.                                                     
074800                                                                          
074900     MOVE '  QC' TO GOOD-STATUSCODES                                      
075000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075200     PERFORM IMS-STATUSCHECK                                              
075300     .                                                                    
075400     SKIP3                                                                
075500 IMS-INSERT-MSG SECTION.                                                  
075600                                                                          
075700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075800     MOVE SPACE TO GOOD-STATUSCODES                                       
075900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076100     PERFORM IMS-STATUSCHECK                                              
076200     .                                                                    
076300     EJECT                                                                
076400 IMS-GU-WDB617 SECTION.                                                   
076500                                                                          
076600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
076700          DELIMITED BY SIZE INTO SSA1                                     
076800     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
076900          DELIMITED BY SIZE INTO SSA2                                     
077000     MOVE '    ' TO GOOD-STATUSCODES                                      
077100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB617 SSA1 SSA2               
077200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
077300     PERFORM IMS-STATUSCHECK                                              
077400     .                                                                    
077500     SKIP3                                                                
077600 IMS-GHNP-WDB621 SECTION.                                                 
077700                                                                          
077800     STRING 'WDB621  (KDPRODSL>=' W-KDPRODSL-MIN-X                        
077900                    '&KDPRODSL<=' W-KDPRODSL-MAX-X ')'                    
078000          DELIMITED BY SIZE INTO SSA1                                     
078100     MOVE '  GE' TO GOOD-STATUSCODES                                      
078200     CALL CBLTDLI USING GHNP WDB6-PCB DLI-IO-WDB621 SSA1                  
078300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
078400     PERFORM IMS-STATUSCHECK                                              
078500     .                                                                    
078600     SKIP3                                                                
078700 IMS-ISRT-WDB621 SECTION.                                                 
078800                                                                          
078900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
079000          DELIMITED BY SIZE INTO SSA1                                     
079100     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
079200          DELIMITED BY SIZE INTO SSA2                                     
079300     MOVE 'WDB621  '   TO SSA3                                            
079400     MOVE '  ' TO GOOD-STATUSCODES                                        
079500     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB621 SSA1 SSA2 SSA3        
079600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
079700     PERFORM IMS-STATUSCHECK                                              
079800     .                                                                    
079900     SKIP3                                                                
080000 IMS-GHU-WDB621 SECTION.                                                  
080100                                                                          
080200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
080300          DELIMITED BY SIZE INTO SSA1                                     
080400     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
080500          DELIMITED BY SIZE INTO SSA2                                     
080600     STRING 'WDB621  (KDPRODSL =' W-KDPRODSL-X ')'                        
080700          DELIMITED BY SIZE INTO SSA3                                     
080800     MOVE '  GE' TO GOOD-STATUSCODES                                      
080900     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB621 SSA1 SSA2 SSA3         
081000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
081100     PERFORM IMS-STATUSCHECK                                              
081200     .                                                                    
081300     SKIP3                                                                
081400 IMS-REPL-WDB621 SECTION.                                                 
081500                                                                          
081600     MOVE '  ' TO GOOD-STATUSCODES                                        
081700     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB621                       
081800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
081900     PERFORM IMS-STATUSCHECK                                              
082000     .                                                                    
082100     SKIP3                                                                
082200 IMS-DLET-WDB621 SECTION.                                                 
082300                                                                          
082400     MOVE '   ' TO GOOD-STATUSCODES                                       
082500     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB621                       
082600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
082700     PERFORM IMS-STATUSCHECK                                              
082800     .                                                                    
082900     SKIP3                                                                
083000 IMS-STATUSCHECK SECTION.                                                 
083100                                                                          
083200     SET STATUS-IX TO 1                                                   
083300     SEARCH GOOD-STATUS                                                   
083400       AT END                                                             
083500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
083600         DELIMITED BY SIZE INTO ERROR-TEXT                                
083700         CALL FELLOG                                                      
083800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
083900         CONTINUE                                                         
084000     END-SEARCH                                                           
084100     .                                                                    
