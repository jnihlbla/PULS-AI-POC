000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2044100.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/11/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS SCREEN SHOWS QUEUE OF NEW PARTS FROM PARTS PLANNING         
000900*        FOR REGISTRATION BY PROCURER.                                    
001000*                                                                         
001100*        THE PROGRAM READS     WDC9                                       
001200*        THE PROGRAM READS     WDK6                                       
001300*        THE PROGRAM READS     WDK7                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W2T441                                              
001700*        MID:         W2I44101                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W2O44101                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W2044100'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  YES                         PIC X       VALUE 'J'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- INDEX FOR SCROLL LINES                                           
003700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004000                                                                          
004100                                                                          
004200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004300     88  KEYS-OK                             VALUE 'J'.                   
004400     88  KEYS-WRONG                          VALUE 'N'.                   
004500                                                                          
004600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004700     88  INDATA-OK                           VALUE 'J'.                   
004800     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
004901 77  INPUT-SW                    PIC X       VALUE 'N'.                   
004902     88  INPUT-YES                           VALUE 'J'.                   
004903     88  INPUT-NO                            VALUE 'N'.                   
004904                                                                          
004910 77  WDK6-SW                     PIC X       VALUE 'N'.                   
004920     88  WDK6-OK                             VALUE 'J'.                   
004930     88  WDK6-WRONG                          VALUE 'N'.                   
004940                                                                          
005000 77  KDCMDVAL-SW                 PIC X       VALUE 'J'.                   
005100     88  KDCMDVAL-OK                         VALUE 'J'.                   
005200     88  NO-KDCMDVAL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  OWN-MID                             VALUE '2441'.                
005600     88  GOOD-MID                            VALUE '2441' '2442'          
005700                                                   '2443' '2444'          
005800                                                   '2445' '2446'          
005900                                                   '2447' '2448'          
006000                                                   '2449'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300 77  WS-IDANSK-FROM              PIC X(3)    VALUE ZERO.                  
006400 77  WS-IDANSK-TOM               PIC X(3)    VALUE ZERO.                  
006500 77  WS-IDANSK                   PIC X(3)    VALUE ZERO.                  
006600 77  WS-IDPROJ                   PIC X(4)    VALUE SPACE.                 
006700 01  WS-AAAAMMDD                 PIC 9(8).                                
006800 01  FILLER REDEFINES WS-AAAAMMDD.                                        
006900     03  FILLER                  PIC 9(2).                                
007000     03  WS-AAMMDD               PIC 9(6).                                
007100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007200 01  GENERAL-SUBPROGRAMS.                                                 
007300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007800     EJECT                                                                
007900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008000*01 -COPY WMEDAREA                                                        
008100     SKIP3                                                                
008200 01  MESSAGE-CODES.                                                       
008300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008700     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
008800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008900     03  ERR-HILITE-FIELDS-WRONG PIC X(3)    VALUE '409'.                 
009000                                                                          
009100     EJECT                                                                
009200*01  -COPY WDATAREA                                                       
009300     EJECT                                                                
009400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009700     SKIP3                                                                
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
010000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010100*                                                                         
010200 01  SAVE-AREA.                                                           
010300     03  SAVE-IDTRANS            PIC X(4)    VALUE '2441'.                
010400     03  SAVE-IDANSK-ENTER       PIC S9(3)        COMP-3.                 
010500     03  SAVE-IDANSK-NEXT        PIC S9(3)        COMP-3.                 
010600     03  SAVE-IDARTNR-ENTER      PIC S9(9)        COMP-3.                 
010700     03  SAVE-IDARTNR-NEXT       PIC S9(9)        COMP-3.                 
010800     EJECT                                                                
010900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200     SKIP3                                                                
011300*01  MID -COPY W2I44101                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W2O44101                                                 
012100     EJECT                                                                
012200 01  FILLER                    PIC X(16) VALUE 'PROG-TO-PROG-SW'.         
012300*    -- P-WS-LL                                                           
012400 01  W-PROG-TO-PROG-SW.                                                   
012500     03  P-WS-LL               PIC S9(4)  COMP SYNC.                      
012600     03  P-WS-Z1-Z2            PIC X(2)   VALUE LOW-VALUE.                
012700     03  KDTRANS-WS            PIC X(8)   VALUE 'W2T433  '.               
012800     03  P-IDTRANS             PIC X(4)   VALUE '2441'.                   
012900     03  P-KDMFSFOR            PIC X(1)   VALUE '1'.                      
013000                                                                          
013100*    03  MID -COPY W2I44101 -PRE PROGSW-.                                 
013200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013300     SKIP3                                                                
013400*01  -COPY WMFSAREA                                                       
013500     EJECT                                                                
013600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013900     SKIP3                                                                
014000 01  KEYS-FOR-DLI.                                                        
014100*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
014200     03  W-WDC9A1KY-GU-X.                                                 
014300         05  W-KDANSKQ-GU-X.                                              
014400             07  W-KDANSKQ-GU    PIC X       VALUE '1'.                   
014500         05  W-IDANSK-GU-X.                                               
014600             07  W-IDANSK-GU     PIC S9(3)   VALUE ZERO COMP-3.           
014700         05  W-IDARTNR-GU-X.                                              
014800             07  W-IDARTNR-GU    PIC S9(9)   VALUE ZERO COMP-3.           
014900         05  W-IDDC-GU-X.                                                 
015000             07  W-IDDC-GU       PIC X(2)    VALUE '71'.                  
015100                                                                          
015200     03  W-WDC9A1KY-MIN-X.                                                
015300         05  W-KDANSKQ-MIN-X.                                             
015400             07  W-KDANSKQ-MIN   PIC X       VALUE '1'.                   
015500         05  W-IDANSK-MIN-X.                                              
015600             07  W-IDANSK-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
015700         05  FILLER              PIC X(7)    VALUE LOW-VALUES.            
016000                                                                          
016100     03  W-WDC9A1KY-MAX-X.                                                
016200         05  W-KDANSKQ-MAX-X.                                             
016300             07  W-KDANSKQ-MAX   PIC X       VALUE '1'.                   
016400         05  W-IDANSK-MAX-X.                                              
016500             07  W-IDANSK-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
016600         05  FILLER              PIC X(7)    VALUE HIGH-VALUES.           
016900                                                                          
017000     03  W-IDARTNR-X.                                                     
017100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017200     03  W-IDDC-X.                                                        
017300         05  W-IDDC              PIC X(2)    VALUE '71'.                  
017400     03  W-IDLAND-X.                                                      
017500         05  W-IDLANDX           PIC X(2)    VALUE 'CN'.                  
017600     03  W-KDSEGKEY-X.                                                    
017700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
017800     SKIP2                                                                
017900*    --- STATUS CODES FROM IMS                                            
018000 01  STATUS-WS                   PIC XX.                                  
018100     88  SEGMENT-FOUND                       VALUE '  '.                  
018200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018310     88  SEGMENT-END                         VALUE 'GB'.                  
018400     SKIP2                                                                
018500 01  GOOD-STATUSCODES.                                                    
018600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018700     SKIP3                                                                
018800 01  SSA1                        PIC X(64).                               
018900 01  SSA2                        PIC X(64).                               
019000     EJECT                                                                
019100*    --- IMS FUNCTION CODES                                               
019200*01  -COPY W0003                                                          
019300     EJECT                                                                
019400*    ---  DLI INPUT-OUTPUT AREA                                           
019500                                                                          
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
019700 01  DLI-IO-WDC901.                                                       
019800*    03  -COPY WDC901                                                     
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC9A1'.                      
020000 01  DLI-IO-WDC9A1.                                                       
020100*    03  -COPY WDC9A1                                                     
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
020300 01  DLI-IO-WDK6.                                                         
020400*    03  -COPY WDK601                                                     
020500*    03  -COPY WDK611                                                     
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
020700 01  DLI-IO-WDK701.                                                       
020800*    03  -COPY WDK701                                                     
020900     EJECT                                                                
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
021100 01  DLI-IO-WDK711.                                                       
021200*    03  -COPY WDK711                                                     
021300     EJECT                                                                
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
021500 01  DLI-IO-WDK712.                                                       
021600*    03  -COPY WDK712                                                     
021700     EJECT                                                                
021800 LINKAGE SECTION.                                                         
021900*01  -COPY W0009   -PRE MSG-                                              
022000*01  -COPY W0008   -PRE ALT-                                              
022100     05  FILLER                  PIC X.                                   
022200*01  -COPY W0008   -PRE WDP7-                                             
022300     05  FILLER                  PIC X.                                   
022400                                                                          
022500*01  -COPY W0008  -PRE WDC9-                                              
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008  -PRE WDC9A-                                             
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE WDK6-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400*01  -COPY W0008  -PRE WDK7-                                              
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDC9-PCB              
023800                           WDC9A-PCB WDK6-PCB WDK7-PCB.                   
023900 MAIN SECTION.                                                            
024000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDC9-PCB              
024100                           WDC9A-PCB WDK6-PCB WDK7-PCB.                   
024200                                                                          
024300     PERFORM IMS-GET-MSG                                                  
024400     IF SEGMENT-FOUND                                                     
024500       PERFORM A-INIT                                                     
024600       PERFORM B-CHECK-KEYS                                               
024700       IF KEYS-OK                                                         
024800         EVALUATE TRUE                                                    
024900           WHEN MFS-UPDATE                                                
025000             PERFORM G-CHECK-INPUT                                        
025100             IF INDATA-OK                                                 
025200                PERFORM H-UPDATE                                          
025300             END-IF                                                       
025400           WHEN MFS-FIRST                                                 
025500             PERFORM C-FIRST-PAGE                                         
025600           WHEN MFS-NEXT                                                  
025700             PERFORM D-NEXT-PAGE                                          
025800           WHEN MFS-ENTER                                                 
025900             PERFORM E-SAME-PAGE                                          
026000           WHEN MFS-SPLIT                                                 
026100             PERFORM J-SWITCH-TO-2433                                     
026200         END-EVALUATE                                                     
026300                                                                          
026400         IF MFS-SPLIT                                                     
026500            IF KDCMDVAL-OK                                                
026600               MOVE '002'        TO MSGI-KDCALL                           
026700               MOVE '2441'       TO SAVE-IDTRANS                          
026800               MOVE SAVE-AREA    TO MSGI-SPAR-AREA                        
026900               CALL W005INIT  USING MSGI-WMSGINIT WDP7-PCB                
027000               MOVE MID-W2I44101 TO PROGSW-MID-W2I44101                   
027100               COMPUTE P-WS-LL = LENGTH OF PROGSW-MID + 17                
027200               PERFORM IMS-INSERT-ALT-MSG                                 
027300            END-IF                                                        
027400         ELSE                                                             
027500            PERFORM F-READ-SHOW-INFO                                      
027600         END-IF                                                           
027700       END-IF                                                             
027800                                                                          
027900       IF NOT MFS-SPLIT                                                   
028000       OR ( MFS-SPLIT AND NO-KDCMDVAL )                                   
028100          COMPUTE MSG-KVLL = LENGTH OF MOD-W2O44101 + 4                   
028200          PERFORM IMS-INSERT-MSG                                          
028300       END-IF                                                             
028400     END-IF                                                               
028500                                                                          
028600     MOVE ZERO TO RETURN-CODE                                             
028700     GOBACK                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 A-INIT SECTION.                                                          
029100                                                                          
029200     IF MSG-DOUBLE-TRANSACTIONS                                           
029300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I44101                 
029400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
029500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029600     ELSE                                                                 
029700       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I44101                  
029800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
029900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030000     END-IF                                                               
030100                                                                          
030200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
030300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
030400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030500                                                                          
030600     MOVE LOW-VALUE TO MSG-AREA                                           
030700     MOVE 'W2O441N1' TO MFS-IDMOD                                         
030800     MOVE '2441' TO MOD-IDTRANS                                           
030900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
031000                                                                          
031100     IF OWN-MID OR HELP-MID                                               
031200       CONTINUE                                                           
031300     ELSE                                                                 
031400       MOVE SPACE TO MFS-KDTRTYP                                          
031500       MOVE '7' TO MFS-IDPFK                                              
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 B-CHECK-KEYS SECTION.                                                    
032000                                                                          
032100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032200     MOVE '001'             TO MSGI-KDCALL                                
032300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
032400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032500     MOVE '2441'            TO MSGI-IDTRANS                               
032600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
032700     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
032800                                                                          
032900*    - LANGUAGE TO BE USED BY MEDKONV                                     
033000     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
033100                                                                          
033200     MOVE YES               TO KEYS-SW                                    
033300                                                                          
033400                                                                          
033500*    -- CHECK OF IDANSK-FROM                                              
033600     MOVE MFS-ERASE-FIELD        TO MOD-IDANSK-FROM-IN                    
033700                                                                          
033800     IF MID-IDANSK-FROM-IN = ALL '+'                                      
033900       MOVE MID-IDANSK-FROM-UT   TO WS-IDANSK-FROM                        
034000     ELSE                                                                 
034100       MOVE MID-IDANSK-FROM-IN   TO WS-IDANSK-FROM                        
034200       MOVE '7'                  TO MFS-IDPFK                             
034300       MOVE SPACE                TO MFS-KDTRTYP                           
034400     END-IF                                                               
034500                                                                          
034600     INSPECT WS-IDANSK-FROM REPLACING LEADING SPACE BY ZERO               
034700     IF WS-IDANSK-FROM NUMERIC                                            
034800       IF WS-IDANSK-FROM > ZERO                                           
034900         MOVE WS-IDANSK-FROM     TO W-IDANSK-MIN                          
035000       ELSE                                                               
035100         MOVE ZERO               TO W-IDANSK-MIN                          
035200                                    WS-IDANSK-FROM                        
035300       END-IF                                                             
035400     ELSE                                                                 
035500       MOVE NOO                  TO KEYS-SW                               
035600     END-IF                                                               
035700                                                                          
035800*    -- CHECK OF IDANSK-TOM                                               
035900     MOVE MFS-ERASE-FIELD        TO MOD-IDANSK-TOM-IN                     
036000                                                                          
036100     IF MID-IDANSK-TOM-IN = ALL '+'                                       
036110       IF MID-IDANSK-FROM-IN NOT = ALL '+'                                
036120          MOVE MID-IDANSK-FROM-IN TO WS-IDANSK-TOM                        
036130       ELSE                                                               
036200          MOVE MID-IDANSK-TOM-UT TO WS-IDANSK-TOM                         
036210       END-IF                                                             
036300     ELSE                                                                 
036400       MOVE MID-IDANSK-TOM-IN    TO WS-IDANSK-TOM                         
036500       MOVE '7'                  TO MFS-IDPFK                             
036600       MOVE SPACE                TO MFS-KDTRTYP                           
036700     END-IF                                                               
036800                                                                          
036900     INSPECT WS-IDANSK-TOM REPLACING LEADING SPACE BY ZERO                
037000     IF WS-IDANSK-TOM NUMERIC                                             
037100       IF WS-IDANSK-TOM > ZERO                                            
037200         MOVE WS-IDANSK-TOM      TO W-IDANSK-MAX                          
037300       ELSE                                                               
037400         MOVE ZERO               TO W-IDANSK-MAX                          
037500                                    WS-IDANSK-TOM                         
037600       END-IF                                                             
037700     ELSE                                                                 
037800       MOVE NOO                  TO KEYS-SW                               
037900     END-IF                                                               
038000                                                                          
038100*    -- CHECK OF IDPROJ                                                   
038200     MOVE MFS-ERASE-FIELD        TO MOD-IDPROJ-IN                         
038300                                                                          
038400     IF MID-IDPROJ-IN = ALL '+'                                           
038500       MOVE MID-IDPROJ-UT        TO WS-IDPROJ                             
038600     ELSE                                                                 
038700       MOVE MID-IDPROJ-IN        TO WS-IDPROJ                             
038800       MOVE '7'                  TO MFS-IDPFK                             
038900       MOVE SPACE                TO MFS-KDTRTYP                           
039000     END-IF                                                               
039100                                                                          
039200     IF GOOD-MID OR KEYS-OK                                               
039300       MOVE WS-IDANSK-FROM       TO MOD-IDANSK-FROM-UT                    
039400       MOVE WS-IDANSK-TOM        TO MOD-IDANSK-TOM-UT                     
039500       MOVE WS-IDPROJ            TO MOD-IDPROJ-UT                         
039600     ELSE                                                                 
039700       MOVE MFS-ERASE-FIELD      TO MOD-IDANSK-FROM-UT                    
039800                                    MOD-IDANSK-TOM-UT                     
039900                                    MOD-IDPROJ-UT                         
040000     END-IF                                                               
040100                                                                          
040200     IF KEYS-WRONG                                                        
040300       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
040400       CALL WMEDKONV          USING MED-WMEDAREA                          
040500       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
040600       PERFORM MFS-ERASE-FIELD-IN                                         
040700       PERFORM MFS-ERASE-FIELD-OUT                                        
040800     END-IF                                                               
040900     .                                                                    
041000     EJECT                                                                
041100 C-FIRST-PAGE SECTION.                                                    
041200                                                                          
041300     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
041400     CALL WMEDKONV            USING MED-WMEDAREA                          
041500     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
041600                                                                          
041700     PERFORM MFS-ERASE-FIELD-IN                                           
041800     .                                                                    
041900     EJECT                                                                
042000 D-NEXT-PAGE SECTION.                                                     
042100                                                                          
042200     IF SAVE-IDTRANS = '2441'                                             
042300       MOVE SAVE-IDANSK-NEXT     TO W-IDANSK-GU                           
042400       MOVE SAVE-IDARTNR-NEXT    TO W-IDARTNR-GU                          
042500     ELSE                                                                 
042600       PERFORM MFS-ERASE-FIELD-IN                                         
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 E-SAME-PAGE SECTION.                                                     
043100                                                                          
043200     IF SAVE-IDTRANS = '2441' OR '0551'                                   
043300       MOVE SAVE-IDANSK-ENTER    TO W-IDANSK-GU                           
043400       MOVE SAVE-IDARTNR-ENTER   TO W-IDARTNR-GU                          
043510       PERFORM VARYING INDX FROM 1 BY 1                                   
043520       UNTIL INDX > MAX-INDX OR INPUT-YES                                 
043530         IF MID-NEW-IDANSK(INDX) = ALL '+'                                
043601           MOVE NOO              TO INPUT-SW                              
043602         ELSE                                                             
043603           MOVE YES              TO INPUT-SW                              
043610         END-IF                                                           
043620       END-PERFORM                                                        
043630       IF INPUT-YES                                                       
043800         MOVE INF-PRESS-PF11     TO MED-IDMFSINF                          
043900         CALL WMEDKONV        USING MED-WMEDAREA                          
044000         MOVE MED-MFSINF         TO MOD-TEMFSFEL                          
044100         PERFORM EA-MID-INDATA-FOR-MOD                                    
044110       ELSE                                                               
044120         PERFORM MFS-ERASE-FIELD-IN                                       
044200       END-IF                                                             
044300     ELSE                                                                 
044400       PERFORM MFS-ERASE-FIELD-IN                                         
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 EA-MID-INDATA-FOR-MOD SECTION.                                           
044900                                                                          
045000     IF MID-IDANSK-FROM-IN = ALL '+'                                      
045100       MOVE MFS-ERASE-FIELD       TO MOD-IDANSK-FROM-IN                   
045200     ELSE                                                                 
045300       MOVE MID-IDANSK-FROM-IN    TO MOD-IDANSK-FROM-IN                   
045400*      MOVE MFS-ADD-READ-FIELD    TO MOD-IDANSK-FROM-IN-ATTR              
045500     END-IF                                                               
045600                                                                          
045700     IF MID-IDANSK-TOM-IN = ALL '+'                                       
045800       MOVE MFS-ERASE-FIELD       TO MOD-IDANSK-TOM-IN                    
045900     ELSE                                                                 
046000       MOVE MID-IDANSK-TOM-IN     TO MOD-IDANSK-TOM-IN                    
046100*      MOVE MFS-ADD-READ-FIELD    TO MOD-IDANSK-TOM-IN-ATTR               
046200     END-IF                                                               
046300                                                                          
046400     IF MID-IDPROJ-IN = ALL '+'                                           
046500       MOVE MFS-ERASE-FIELD       TO MOD-IDPROJ-IN                        
046600     ELSE                                                                 
046700       MOVE MID-IDPROJ-IN         TO MOD-IDPROJ-IN                        
046800*      MOVE MFS-ADD-READ-FIELD    TO MOD-IDPROJ-IN-ATTR                   
046900     END-IF                                                               
047000                                                                          
047100     MOVE +1                        TO INDX                               
047200     PERFORM UNTIL INDX > MAX-INDX                                        
047300       IF MID-KDCMD(INDX) = ALL '+'                                       
047400         MOVE MFS-ERASE-FIELD       TO MOD-KDCMD(INDX)                    
047500       ELSE                                                               
047600         MOVE MID-KDCMD(INDX)       TO MOD-KDCMD(INDX)                    
047700         MOVE MFS-ADD-READ-FIELD    TO MOD-KDCMD-ATTR(INDX)               
047800       END-IF                                                             
047900                                                                          
048000       IF MID-NEW-IDANSK(INDX) = ALL '+'                                  
048100         MOVE MFS-ERASE-FIELD       TO MOD-NEW-IDANSK(INDX)               
048200       ELSE                                                               
048300         MOVE MID-NEW-IDANSK(INDX)  TO MOD-NEW-IDANSK(INDX)               
048400         MOVE MFS-ADD-READ-FIELD    TO MOD-NEW-IDANSK-ATTR(INDX)          
048500       END-IF                                                             
048600       ADD +1                       TO INDX                               
048700     END-PERFORM                                                          
048800     .                                                                    
048900     EJECT                                                                
049000 F-READ-SHOW-INFO SECTION.                                                
049100                                                                          
049200     IF MFS-IDPFK = '7' OR MFS-UPDATE                                     
049300       PERFORM IMS-GN-WDC9A1                                              
049400     ELSE                                                                 
049500       PERFORM IMS-GU-WDC9A1                                              
049600     END-IF                                                               
049700                                                                          
049800     IF SEGMENT-MISSING                                                   
049900       MOVE 'KEYS NOT FOUND'     TO MOD-TEMFSFEL                          
050000       PERFORM MFS-ERASE-FIELD-OUT                                        
050100     ELSE                                                                 
050200       MOVE SEQA-IDANSK          TO SAVE-IDANSK-ENTER                     
050300       MOVE SEQA-IDARTNR         TO SAVE-IDARTNR-ENTER                    
050400       MOVE +1                   TO INDX                                  
050500       PERFORM FA-READ-LINEDATA                                           
050601       PERFORM UNTIL INDX > MAX-INDX                                      
050610         PERFORM IMS-GN-WDC9A1                                            
050700         IF SEGMENT-FOUND                                                 
050800           PERFORM FA-READ-LINEDATA                                       
051000         ELSE                                                             
051100           PERFORM MFS-ERASE-LINE-FIELD-OUT                               
051200           ADD +1                TO INDX                                  
051300         END-IF                                                           
051400       END-PERFORM                                                        
051500                                                                          
051510       MOVE WDC9A-STATUS-CODE    TO STATUS-WS                             
051600       IF SEGMENT-FOUND                                                   
051700         MOVE SEQA-IDANSK        TO SAVE-IDANSK-NEXT                      
051800         MOVE SEQA-IDARTNR       TO SAVE-IDARTNR-NEXT                     
051900         MOVE INF-MORE-INFO-EXISTS                                        
052000                                 TO MED-IDMFSINF                          
052100         CALL WMEDKONV        USING MED-WMEDAREA                          
052200         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
052300       ELSE                                                               
052400         MOVE ZERO               TO SAVE-IDANSK-NEXT                      
052500                                    SAVE-IDARTNR-NEXT                     
052600       END-IF                                                             
052700                                                                          
052800       MOVE '002'                TO MSGI-KDCALL                           
052900       MOVE '2441'               TO SAVE-IDTRANS                          
053000       MOVE SAVE-AREA            TO MSGI-SPAR-AREA                        
053100       CALL W005INIT USING MSGI-WMSGINIT wdp7-PCB                         
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 FA-READ-LINEDATA SECTION.                                                
053600                                                                          
053700     MOVE SEQA-IDARTNR           TO W-IDARTNR                             
053701     PERFORM IMS-GU-WDK611                                                
053702     IF SEGMENT-FOUND                                                     
053703        MOVE YES                 TO WDK6-SW                               
053710        IF WS-IDPROJ = SPACE                                              
053720        OR WS-IDPROJ = CLAG-IDPROJ                                        
053730                                                                          
053910*WDK601                                                                   
053920           MOVE ART-IDAO(1)      TO MOD-IDAO(INDX)                        
053930                                                                          
053940           MOVE ART-TIREGDAT     TO DAT-I-TIDATUM                         
053950           PERFORM S01-CALL-WDATKONV                                      
053960           MOVE DAT-TIAAVV-GRP   TO MOD-TIREGDAT(INDX)                    
053970                                                                          
053980           MOVE ART-IDFKNGRP     TO MOD-IDFKNGRP(INDX)                    
053990*WDK611                                                                   
053991           MOVE CLAG-IDPROJ      TO MOD-IDPROJ(INDX)                      
053992           PERFORM FAA-MOVE-WDC9-FIELDS                                   
054000        END-IF                                                            
054100     ELSE                                                                 
054101        MOVE NOO                 TO WDK6-SW                               
054110        IF WS-IDPROJ = SPACE                                              
054120           PERFORM FAA-MOVE-WDC9-FIELDS                                   
054130        END-IF                                                            
054140     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
057901 FAA-MOVE-WDC9-FIELDS SECTION.                                            
057910*WDC9A                                                                    
057920     MOVE SEQA-IDARTNR     TO MOD-IDARTNR(INDX)                           
057921     MOVE SEQA-IDDC        TO MOD-IDDC   (INDX)                           
057930     MOVE SEQA-IDANSK      TO MOD-IDANSK(INDX)                            
057940*    MOVE MFS-ERASE-FIELD  TO MOD-NEW-IDANSK(INDX)                        
057996                                                                          
057997*To be decided                                                            
057998*    MOVE FLCN             TO MOD-FLCN(INDX)                              
057999     MOVE SPACES           TO MOD-FLCN(INDX)                              
058000                                                                          
058001     PERFORM IMS-GU-WDK712                                                
058002     IF SEGMENT-MISSING                                                   
058003     OR LART-DAPUBL = ZERO                                                
058004       IF WDK6-OK                                                         
058005          IF ART-TIFINLV > ZERO                                           
058006             MOVE ART-TIFINLV                                             
058007                           TO MOD-TIFINLV(INDX)                           
058008          ELSE                                                            
058009             MOVE MFS-ERASE-FIELD                                         
058010                           TO MOD-TIFINLV(INDX)                           
058011          END-IF                                                          
058012       ELSE                                                               
058013         MOVE MFS-ERASE-FIELD                                             
058014                           TO MOD-TIFINLV(INDX)                           
058015       END-IF                                                             
058016     ELSE                                                                 
058017       MOVE LART-DAPUBL    TO WS-AAAAMMDD                                 
058018       MOVE WS-AAMMDD      TO DAT-I-TIDATUM                               
058019       PERFORM S01-CALL-WDATKONV                                          
058020       MOVE DAT-TIAAVVD    TO MOD-TIFINLV(INDX)                           
058021     END-IF                                                               
058022     ADD +1                TO INDX                                        
058023     .                                                                    
058024     EJECT                                                                
058030 G-CHECK-INPUT SECTION.                                                   
058100                                                                          
058200     MOVE +1                     TO INDX                                  
058300     PERFORM UNTIL INDX > MAX-INDX                                        
058400       IF MID-KDCMD(INDX) NOT = ALL '+'                                   
058500          IF MID-KDCMD(INDX) = 'S' OR 'D'                                 
058600             CONTINUE                                                     
058700          ELSE                                                            
058800             MOVE NOO            TO INDATA-SW                             
058810             MOVE MID-KDCMD(INDX) TO MOD-KDCMD(INDX)                      
058900             MOVE MFS-ALPHA-FIELD-WRONG                                   
059000                                 TO MOD-KDCMD-ATTR(INDX)                  
059100          END-IF                                                          
059200       END-IF                                                             
059300                                                                          
059400       IF MID-NEW-IDANSK(INDX) NOT = ALL '+'                              
059500          MOVE MID-NEW-IDANSK(INDX)                                       
059600                                 TO WS-IDANSK                             
059700          INSPECT WS-IDANSK REPLACING LEADING SPACE BY ZERO               
059800          IF WS-IDANSK NUMERIC                                            
059900          AND MID-IDARTNR(INDX) NOT = ALL '+'                             
059910             IF WS-IDANSK > ZERO AND WS-IDANSK < 1000                     
060000                CONTINUE                                                  
060100             END-IF                                                       
060200          ELSE                                                            
060300             MOVE NOO            TO INDATA-SW                             
060310             MOVE MID-NEW-IDANSK(INDX)                                    
060320                                 TO MOD-NEW-IDANSK(INDX)                  
060400             MOVE MFS-NUM-FIELD-WRONG                                     
060500                                 TO MOD-NEW-IDANSK-ATTR(INDX)             
060600          END-IF                                                          
060700       END-IF                                                             
060800       ADD +1                    TO INDX                                  
060900     END-PERFORM                                                          
061000                                                                          
061100     IF INDATA-FEL                                                        
061200       MOVE ERR-HILITE-FIELDS-WRONG                                       
061300                                 TO MED-IDMFSFEL                          
061400       CALL WMEDKONV          USING MED-WMEDAREA                          
061500       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
061600     END-IF                                                               
061700                                                                          
061800     .                                                                    
061900     EJECT                                                                
062000 H-UPDATE SECTION.                                                        
062100                                                                          
062200     PERFORM VARYING INDX FROM 1 BY 1                                     
062300     UNTIL INDX > MAX-INDX                                                
062310                                                                          
062401       MOVE MID-IDARTNR(INDX)          TO W-IDARTNR                       
062403                                                                          
062410       IF MID-KDCMD(INDX) = 'D'                                           
062411          PERFORM IMS-GHU-WDC901                                          
062420          PERFORM IMS-DLET-WDC901                                         
062430       ELSE                                                               
062500         IF MID-NEW-IDANSK(INDX) NOT = ALL '+'                            
062600           PERFORM IMS-GHU-WDC901                                         
063100           MOVE MID-NEW-IDANSK(INDX)   TO KART-IDANSK                     
063200           PERFORM IMS-REPL-WDC901                                        
063430         END-IF                                                           
063440       END-IF                                                             
063500                                                                          
063600     END-PERFORM                                                          
063700                                                                          
063800     MOVE INF-UPDATE-DONE              TO MED-IDMFSINF                    
063900     CALL WMEDKONV                  USING MED-WMEDAREA                    
064000     MOVE MED-TEMFSINF                 TO MOD-TEMFSINF                    
064100     .                                                                    
064200     EJECT                                                                
064300 J-SWITCH-TO-2433 SECTION.                                                
064400     MOVE YES                    TO KDCMDVAL-SW                           
064500     PERFORM VARYING INDX FROM 1 BY 1                                     
064600     UNTIL INDX > MAX-INDX                                                
064700        IF MID-KDCMD(INDX) = '+'                                          
064800           MOVE NOO              TO KDCMDVAL-SW                           
064900           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
065000           PERFORM MFS-FORM-ATTR                                          
065100           MOVE 'NO LINE SELECTED FOR JUMPING TO 2433'                    
065200                                 TO MOD-TEMFSFEL                          
065300        END-IF                                                            
065400     END-PERFORM                                                          
065500                                                                          
065600     .                                                                    
065700     EJECT                                                                
065800 S01-CALL-WDATKONV SECTION.                                               
065900                                                                          
066000     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
066100                                                                          
066200     CALL WDATKONV            USING DAT-KDDATFORM                         
066300                                    DAT-I-TIDATUM                         
066400                                    DAT-O-TIDATUM                         
066500                                    DAT-KDSVAR                            
066600                                                                          
066700     IF DAT-KDSVAR-OK                                                     
066800       CONTINUE                                                           
066900     ELSE                                                                 
067000       MOVE 'BAD RETURN CODE FROM WDATKONV' TO ERROR-TEXT                 
067100       CALL FELLOG                                                        
067200     END-IF                                                               
067300                                                                          
067400     .                                                                    
067500     EJECT                                                                
067600 MFS-ERASE-FIELD-OUT SECTION.                                             
067700                                                                          
067800     PERFORM VARYING INDX FROM 1 BY 1                                     
067900     UNTIL INDX > MAX-INDX                                                
068000       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
068100     END-PERFORM                                                          
068200     .                                                                    
068300     SKIP3                                                                
068400 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
068500                                                                          
068600     MOVE MFS-ERASE-FIELD TO MOD-KDCMD(INDX)                              
068710                             MOD-IDARTNR(INDX)                            
068720                             MOD-IDDC   (INDX)                            
068800                             MOD-IDPROJ(INDX)                             
068900                             MOD-IDAO(INDX)                               
069000                             MOD-TIFINLV(INDX)                            
069100                             MOD-TIREGDAT(INDX)                           
069200                             MOD-IDFKNGRP(INDX)                           
069300                             MOD-IDANSK(INDX)                             
069400                             MOD-NEW-IDANSK(INDX)                         
069500                             MOD-FLCN(INDX)                               
069600                                                                          
069700     .                                                                    
069800     SKIP3                                                                
069900 MFS-ERASE-FIELD-IN SECTION.                                              
070000                                                                          
070100     MOVE MFS-ERASE-FIELD   TO MOD-IDANSK-FROM-IN                         
070200                               MOD-IDANSK-TOM-IN                          
070300                               MOD-IDPROJ-IN                              
070400                                                                          
070500     MOVE +1                TO INDX                                       
070600     PERFORM UNTIL INDX > MAX-INDX                                        
070700       MOVE MFS-ERASE-FIELD TO MOD-KDCMD   (INDX)                         
070800                               MOD-NEW-IDANSK(INDX)                       
070900       ADD +1               TO INDX                                       
071000     END-PERFORM                                                          
071100     .                                                                    
071200     EJECT                                                                
071300 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
071400                                                                          
071500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDANSK-FROM-UT                    
071600                                    MOD-IDANSK-TOM-UT                     
071700                                    MOD-IDPROJ-UT                         
071800                                                                          
071900     MOVE +1                     TO INDX                                  
072000     PERFORM UNTIL INDX > MAX-INDX                                        
072100       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
072200       ADD +1                    TO INDX                                  
072300     END-PERFORM                                                          
072400     .                                                                    
072500     EJECT                                                                
072600 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
072700                                                                          
072900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR(INDX)                     
072901                                    MOD-IDDC(INDX)                        
072910                                    MOD-IDPROJ(INDX)                      
073000                                    MOD-IDAO(INDX)                        
073100                                    MOD-TIFINLV(INDX)                     
073200                                    MOD-TIREGDAT(INDX)                    
073300                                    MOD-IDFKNGRP(INDX)                    
073400                                    MOD-IDANSK(INDX)                      
073500                                    MOD-NEW-IDANSK(INDX)                  
073600                                    MOD-FLCN(INDX)                        
073700     .                                                                    
073800     EJECT                                                                
073900 MFS-FORM-ATTR SECTION.                                                   
074000                                                                          
074100*    --- ALL INDATA-FIELDS                                                
074200     MOVE +1                TO INDX                                       
074300     PERFORM UNTIL INDX > MAX-INDX                                        
074400       MOVE MFS-FORMAT-DEFAULT-ATTR                                       
074500                            TO MOD-NEW-IDANSK(INDX)                       
074600       ADD +1               TO INDX                                       
074700     END-PERFORM                                                          
074800     .                                                                    
074900* --- IMS SECTIONS ---                                                    
075000     SKIP3                                                                
075100 IMS-GET-MSG SECTION.                                                     
075200                                                                          
075300     MOVE '  QC' TO GOOD-STATUSCODES                                      
075400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075600     PERFORM IMS-STATUSCHECK                                              
075700     .                                                                    
075800     SKIP3                                                                
075900 IMS-INSERT-MSG SECTION.                                                  
076000                                                                          
076100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
076200     MOVE SPACE TO GOOD-STATUSCODES                                       
076300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076500     PERFORM IMS-STATUSCHECK                                              
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-INSERT-ALT-MSG Section.                                              
076900                                                                          
077000     IF ENGLISH-TEXT                                                      
077100       MOVE '2' TO P-KDMFSFOR                                             
077200     END-IF                                                               
077300     MOVE SPACE TO GOOD-STATUSCODES                                       
077400     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
077500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
077600     PERFORM IMS-STATUSCHECK                                              
077700     .                                                                    
077800     EJECT                                                                
077900 IMS-GU-WDC9A1 SECTION.                                                   
078000                                                                          
078010     MOVE SPACES    TO SSA1                                               
078100     STRING 'WDC9A1  (WDC9A1KY =' W-WDC9A1KY-GU-X ')'                     
078200          DELIMITED BY SIZE INTO SSA1                                     
078300     MOVE '  GE' TO GOOD-STATUSCODES                                      
078400     CALL CBLTDLI USING GU WDC9A-PCB DLI-IO-WDC9A1 SSA1                   
078500     MOVE WDC9A-STATUS-CODE TO STATUS-WS                                  
078600     PERFORM IMS-STATUSCHECK                                              
078700     .                                                                    
078800     EJECT                                                                
078900 IMS-GN-WDC9A1 SECTION.                                                   
079000                                                                          
079010     MOVE SPACES    TO SSA1                                               
079100     STRING 'WDC9A1  (WDC9A1KY>=' W-WDC9A1KY-MIN-X                        
079200                    '&WDC9A1KY<=' W-WDC9A1KY-MAX-X                        
079210                    '&IDDC     =' W-IDDC-X ')'                            
079300             DELIMITED BY SIZE INTO SSA1                                  
079400     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
079500     CALL CBLTDLI USING GN WDC9A-PCB DLI-IO-WDC9A1 SSA1                   
079600     MOVE WDC9A-STATUS-CODE TO STATUS-WS                                  
079700     PERFORM IMS-STATUSCHECK                                              
079800     .                                                                    
079900     EJECT                                                                
080000 IMS-GHU-WDC901 SECTION.                                                  
080100                                                                          
080110     MOVE SPACES    TO SSA1                                               
080200     STRING 'WDC901  (IDARTNR  =' W-IDARTNR-X                             
080300                    '&IDDC     =' W-IDDC-X ')'                            
080400             DELIMITED BY SIZE INTO SSA1                                  
080500     MOVE '  '  TO GOOD-STATUSCODES                                       
080600     CALL CBLTDLI USING GHU WDC9-PCB DLI-IO-WDC901 SSA1                   
080700     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
080800     PERFORM IMS-STATUSCHECK                                              
080900     .                                                                    
081000     EJECT                                                                
081100 IMS-REPL-WDC901 SECTION.                                                 
081200                                                                          
081300     MOVE 'WDC901  '   TO SSA1                                            
081400     MOVE '  '  TO GOOD-STATUSCODES                                       
081500     CALL CBLTDLI USING REPL WDC9-PCB DLI-IO-WDC901 SSA1                  
081600     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
081700     PERFORM IMS-STATUSCHECK                                              
081800     .                                                                    
081900     EJECT                                                                
082000 IMS-DLET-WDC901 SECTION.                                                 
082100                                                                          
082200     MOVE 'WDC901  '   TO SSA1                                            
082300     MOVE '  '  TO GOOD-STATUSCODES                                       
082400     CALL CBLTDLI USING DLET WDC9-PCB DLI-IO-WDC901 SSA1                  
082500     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
082600     PERFORM IMS-STATUSCHECK                                              
082700     .                                                                    
082800     EJECT                                                                
082900 IMS-GU-WDK611 SECTION.                                                   
083000                                                                          
083010     MOVE SPACES    TO SSA1 SSA2                                          
083100     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
083200             DELIMITED BY SIZE INTO SSA1                                  
083300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
083400             DELIMITED BY SIZE INTO SSA2                                  
083500     MOVE '  GE' TO GOOD-STATUSCODES                                      
083600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK6 SSA1 SSA2                 
083700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
083800     PERFORM IMS-STATUSCHECK                                              
083900     .                                                                    
084000     EJECT                                                                
084100 IMS-GU-WDK712 SECTION.                                                   
084200                                                                          
084210     MOVE SPACES    TO SSA1 SSA2                                          
084300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
084400             DELIMITED BY SIZE INTO SSA1                                  
084500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
084600             DELIMITED BY SIZE INTO SSA2                                  
084700     MOVE '  GE' TO GOOD-STATUSCODES                                      
084800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
084900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
085000     PERFORM IMS-STATUSCHECK                                              
085100     .                                                                    
085200     EJECT                                                                
085300 IMS-STATUSCHECK SECTION.                                                 
085400                                                                          
085500     SET STATUS-IX TO 1                                                   
085600     SEARCH GOOD-STATUS                                                   
085700       AT END                                                             
085800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
085900         DELIMITED BY SIZE INTO ERROR-TEXT                                
086000         CALL FELLOG                                                      
086100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
086200         CONTINUE                                                         
086300     END-SEARCH                                                           
086400     .                                                                    
