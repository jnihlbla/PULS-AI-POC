000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2044600.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   12/11/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN THAT SHOW REGISTERED PARTS FROM THE PROCURER QUEUE        
000900*        WHICH ARE NOT POSSIBLE TO SEND TO SI+ DUE TO MISSING             
001000*        PACKAGE CODE.                                                    
001100*                                                                         
001200*        THE PROGRAM READS     WDC9                                       
001300*        THE PROGRAM READS     WDK6                                       
001400*        THE PROGRAM READS     WDK7                                       
001500*        THE PROGRAM READS     WDB6                                       
001600*        THE PROGRAM READS     WDD2                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: W2T446                                              
002000*        MID:         W2I44601                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        MOD:         W2O44601                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W2044600'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  WS-DAPUBL-FOM               PIC X(5)    VALUE ZERO.                  
004000 77  WS-DAPUBL-TOM               PIC X(5)    VALUE ZERO.                  
004100 77  WS-IDPROJ                   PIC X(4)    VALUE SPACE.                 
004200 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004210 77  WS-DC-OK                    PIC X(1).                                
004300                                                                          
004400 01  WS-AAAAMMDD                 PIC 9(8).                                
004500 01  FILLER REDEFINES WS-AAAAMMDD.                                        
004600     03  FILLER                  PIC 9(2).                                
004700     03  WS-AAMMDD               PIC 9(6).                                
004800                                                                          
004900 01  WS-AAVVD                    PIC 9(5).                                
005000                                                                          
005100*    --- INDEX FOR SCROLL LINES                                           
005200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005300 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
005400*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005500                                                                          
005600                                                                          
005700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005800     88  KEYS-OK                             VALUE 'J'.                   
005900     88  KEYS-WRONG                          VALUE 'N'.                   
006000                                                                          
006100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006200     88  OWN-MID                             VALUE '2446'.                
006300     88  GOOD-MID                            VALUE '2446'.                
006800     88  HELP-MID                            VALUE '0551'.                
006900     EJECT                                                                
006910 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
006920*01 -COPY WWIDFTG                                                         
006930     EJECT                                                                
007000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007100 01  GENERAL-SUBPROGRAMS.                                                 
007200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007700     EJECT                                                                
007800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007900*01 -COPY WMEDAREA                                                        
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008500     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
008600     EJECT                                                                
008700*01  -COPY WDATAREA                                                       
008800     EJECT                                                                
008900*01  -COPY WY2000W2                                                       
009000     EJECT                                                                
009100*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009200*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009400     SKIP3                                                                
009500*01 -COPY WMSGINIT                                                        
009600     EJECT                                                                
009700*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009800*                                                                         
009900 01  SAVE-AREA.                                                           
010000     03  SAVE-IDTRANS            PIC X(4)    VALUE '2446'.                
010100     03  SAVE-IDANSK-ENTER       PIC S9(3)        COMP-3.                 
010200     03  SAVE-IDANSK-NEXT        PIC S9(3)        COMP-3.                 
010300     03  SAVE-IDARTNR-ENTER      PIC S9(9)        COMP-3.                 
010400     03  SAVE-IDARTNR-NEXT       PIC S9(9)        COMP-3.                 
010500     03  SAVE-IDDC-ENTER         PIC X(2).                                
010600     03  SAVE-IDDC-NEXT          PIC X(2).                                
010700     EJECT                                                                
010800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011100     SKIP3                                                                
011200*01  MID -COPY W2I44601                                                   
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011500     SKIP3                                                                
011600*01  -COPY WMSGAREA                                                       
011700     EJECT                                                                
011800     03  MOD REDEFINES MSG-AREA.                                          
011900*      05  -COPY W2O44601                                                 
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012200     SKIP3                                                                
012300*01  -COPY WMFSAREA                                                       
012400     EJECT                                                                
012500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012800     SKIP3                                                                
012900 01  KEYS-FOR-DLI.                                                        
013000*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
013100     03  W-WDC9A1KY-GU-X.                                                 
013200         05  W-KDANSKQ-GU-X.                                              
013300             07  W-KDANSKQ-GU    PIC X       VALUE '4'.                   
013400         05  W-IDANSK-GU-X.                                               
013500             07  W-IDANSK-GU     PIC S9(3)   VALUE ZERO COMP-3.           
013600         05  W-IDARTNR-GU-X.                                              
013700             07  W-IDARTNR-GU    PIC S9(9)   VALUE ZERO COMP-3.           
013800         05  W-IDDC-GU-X.                                                 
013900             07  W-IDDC-GU       PIC X(2)    VALUE SPACE.                 
014000                                                                          
014100     03  W-WDC9A1KY-MIN-X.                                                
014200         05  W-KDANSKQ-MIN-X.                                             
014300             07  W-KDANSKQ-MIN   PIC X       VALUE '4'.                   
014400         05  FILLER              PIC X(9)    VALUE LOW-VALUES.            
014500                                                                          
014600     03  W-WDC9A1KY-MAX-X.                                                
014700         05  W-KDANSKQ-MAX-X.                                             
014800             07  W-KDANSKQ-MAX   PIC X       VALUE '4'.                   
014900         05  FILLER              PIC X(9)    VALUE HIGH-VALUES.           
015000                                                                          
015100     03  W-IDDC-MIN-X.                                                    
015200         05  FILLER              PIC X(1).                                
015210         05  W-IDDC2-MIN         PIC X(1).                                
015300     03  W-IDDC-MAX-X.                                                    
015400         05  FILLER              PIC X(1).                                
015410         05  W-IDDC2-MAX         PIC X(1).                                
015500                                                                          
015600     03  W-WDC901KY-X.                                                    
015700         05  W-IDDC-X.                                                    
015800             07  W-IDDC          PIC X(2)    VALUE SPACE.                 
015900         05  W-IDARTNR-X.                                                 
016000             07  W-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.           
016100                                                                          
016200     03  W-KDSEGKEY-X.                                                    
016300         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016400     03  W-IDLAND-X.                                                      
016500         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
016600     SKIP2                                                                
016700*    --- STATUS CODES FROM IMS                                            
016800 01  STATUS-WS                   PIC XX.                                  
016900     88  SEGMENT-FOUND                       VALUE '  '.                  
017000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017110     88  SEGMENT-END                         VALUE 'GB'.                  
017200     SKIP2                                                                
017300 01  GOOD-STATUSCODES.                                                    
017400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(128).                              
017700 01  SSA2                        PIC X(64).                               
017800 01  SSA3                        PIC X(64).                               
017900     EJECT                                                                
018000*    --- IMS FUNCTION CODES                                               
018100*01  -COPY W0003                                                          
018200     EJECT                                                                
018300*    ---  DLI INPUT-OUTPUT AREA                                           
018400                                                                          
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC9A1'.                      
018600 01  DLI-IO-WDC9A1.                                                       
018700*    03  -COPY WDC9A1                                                     
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
019000 01  DLI-IO-WDC901.                                                       
019100*    03  -COPY WDC901                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
019400 01  DLI-IO-WDK601.                                                       
019500*    03  -COPY WDK601                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
019800 01  DLI-IO-WDK611.                                                       
019900*    03  -COPY WDK611                                                     
020000     EJECT                                                                
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
020200 01  DLI-IO-WDK701.                                                       
020300*    03  -COPY WDK701                                                     
020400     EJECT                                                                
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
020600 01  DLI-IO-WDK712.                                                       
020700*    03  -COPY WDK712                                                     
020800     EJECT                                                                
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
021000 01  DLI-IO-WDB601.                                                       
021100*    03  -COPY WDB601                                                     
021200     EJECT                                                                
021300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
021400 01  DLI-IO-WDD201.                                                       
021500*    03  -COPY WDD201                                                     
021600     EJECT                                                                
021700 LINKAGE SECTION.                                                         
021800*01  -COPY W0009   -PRE MSG-                                              
021900*01  -COPY W0008   -PRE WDP7-                                             
022000     05  FILLER                  PIC X.                                   
022100                                                                          
022200*01  -COPY W0008  -PRE WDC9A-                                             
022300     05  FILLER                  PIC X.                                   
022400                                                                          
022410*01  -COPY W0008  -PRE WDC9-                                              
022420     05  FILLER                  PIC X.                                   
022430                                                                          
022500*01  -COPY W0008  -PRE WDK6-                                              
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008  -PRE WDK7-                                              
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE WDB6-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023310*01  -COPY W0008  -PRE WDB6A-                                             
023320     05  FILLER                  PIC X.                                   
023330                                                                          
023400*01  -COPY W0008  -PRE WDD2-                                              
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDC9A-PCB WDC9-PCB            
023800     WDK6-PCB WDK7-PCB WDB6-PCB WDB6A-PCB WDD2-PCB.                       
023900 MAIN SECTION.                                                            
024000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDC9A-PCB WDC9-PCB            
024010     WDK6-PCB WDK7-PCB WDB6-PCB WDB6A-PCB WDD2-PCB.                       
024200                                                                          
024300*------------------------                                                 
024400     PERFORM IMS-GET-MSG                                                  
024500     IF SEGMENT-FOUND                                                     
024600       PERFORM A-INIT                                                     
024700       PERFORM B-CHECK-KEYS                                               
024800       IF KEYS-OK                                                         
024900         IF MFS-FIRST                                                     
025000           PERFORM C-FIRST-PAGE                                           
025100         ELSE                                                             
025200           IF MFS-NEXT                                                    
025300             PERFORM D-NEXT-PAGE                                          
025400           ELSE                                                           
025500             PERFORM E-SAME-PAGE                                          
025600           END-IF                                                         
025700         END-IF                                                           
025800         PERFORM F-READ-SHOW-INFO                                         
025900       END-IF                                                             
026000       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O44601 + 4                      
026100       PERFORM IMS-INSERT-MSG                                             
026200     END-IF                                                               
026300                                                                          
026400     MOVE ZERO                   TO RETURN-CODE                           
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900                                                                          
027000     IF MSG-DOUBLE-TRANSACTIONS                                           
027100       MOVE MSG-INDATA-MINUS-2-TRANSACT                                   
027200                                 TO MID-W2I44601                          
027300       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
027400       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
027500     ELSE                                                                 
027600       MOVE MSG-INDATA-MINUS-1-TRANSACT                                   
027700                                 TO MID-W2I44601                          
027800       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
027900       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
028000     END-IF                                                               
028100                                                                          
028200     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
028300     MOVE MSG-IDPFK              TO MFS-IDPFK                             
028400     MOVE MFS-IDTRANS            TO W-IDTRANS                             
028500                                                                          
028600     MOVE LOW-VALUE              TO MSG-AREA                              
028700     MOVE 'W2O446N1'             TO MFS-IDMOD                             
028800     MOVE '2446'                 TO MOD-IDTRANS                           
028900     MOVE MFS-ERASE-FIELD        TO MOD-TEMFSFEL                          
029000                                    MOD-TEMFSINF                          
029100                                                                          
029200     IF OWN-MID OR HELP-MID                                               
029300       CONTINUE                                                           
029400     ELSE                                                                 
029500       MOVE SPACE                TO MFS-KDTRTYP                           
029600       MOVE '7'                  TO MFS-IDPFK                             
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 B-CHECK-KEYS SECTION.                                                    
030100                                                                          
030200     MOVE ALL '+'                TO MSGI-WMSGINIT                         
030300     MOVE '001'                  TO MSGI-KDCALL                           
030400     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
030500     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
030600     MOVE '2446'                 TO MSGI-IDTRANS                          
030700     CALL W005INIT            USING MSGI-WMSGINIT                         
030800                                    WDP7-PCB                              
030900     MOVE MSGI-SPAR-AREA         TO SAVE-AREA                             
031000                                                                          
031100*    - LANGUAGE TO BE USED BY MEDKONV                                     
031200     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
031300                                                                          
031400     MOVE YES                    TO KEYS-SW                               
031500                                                                          
031600*    -- CHECK OF DAPUBL-FOM                                               
031700     MOVE MFS-ERASE-FIELD        TO MOD-DAPUBL-FOM-IN                     
031800                                                                          
031900     IF MID-DAPUBL-FOM-IN = ALL '+'                                       
032000       MOVE MID-DAPUBL-FOM-UT    TO WS-DAPUBL-FOM                         
032100     ELSE                                                                 
032200       MOVE MID-DAPUBL-FOM-IN    TO WS-DAPUBL-FOM                         
032300       MOVE '7'                  TO MFS-IDPFK                             
032400       MOVE SPACE                TO MFS-KDTRTYP                           
032500     END-IF                                                               
032600                                                                          
032700     INSPECT WS-DAPUBL-FOM REPLACING LEADING SPACE BY ZERO                
032800     IF WS-DAPUBL-FOM NUMERIC AND                                         
032900        WS-DAPUBL-FOM > ZERO                                              
033000       MOVE WS-DAPUBL-FOM        TO DAT-I-TIDATUM                         
033100       PERFORM BA-CHECK-DATE                                              
033200       IF DAT-KDSVAR-OK                                                   
033300         CONTINUE                                                         
033400       ELSE                                                               
033500         MOVE NOO                TO KEYS-SW                               
033600       END-IF                                                             
033700     ELSE                                                                 
033800       MOVE NOO                  TO KEYS-SW                               
033900     END-IF                                                               
034000                                                                          
034100*    -- CHECK OF DAPUBL-TOM                                               
034200     MOVE MFS-ERASE-FIELD        TO MOD-DAPUBL-TOM-IN                     
034300                                                                          
034400     IF MID-DAPUBL-TOM-IN = ALL '+'                                       
034500       MOVE MID-DAPUBL-TOM-UT    TO WS-DAPUBL-TOM                         
034600     ELSE                                                                 
034700       MOVE MID-DAPUBL-TOM-IN    TO WS-DAPUBL-TOM                         
034800       MOVE '7'                  TO MFS-IDPFK                             
034900       MOVE SPACE                TO MFS-KDTRTYP                           
035000     END-IF                                                               
035100                                                                          
035200     INSPECT WS-DAPUBL-TOM REPLACING LEADING SPACE BY ZERO                
035300     IF WS-DAPUBL-TOM NUMERIC AND                                         
035400        WS-DAPUBL-TOM > ZERO                                              
035500       MOVE WS-DAPUBL-TOM        TO DAT-I-TIDATUM                         
035600       PERFORM BA-CHECK-DATE                                              
035700       IF DAT-KDSVAR-OK                                                   
035800         CONTINUE                                                         
035900       ELSE                                                               
036000         MOVE NOO                TO KEYS-SW                               
036100       END-IF                                                             
036200     ELSE                                                                 
036300       MOVE NOO                  TO KEYS-SW                               
036400     END-IF                                                               
036500                                                                          
036600*    -- CHECK OF IDPROJ                                                   
036700     MOVE MFS-ERASE-FIELD        TO MOD-IDPROJ-IN                         
036800                                                                          
036900     IF MID-IDPROJ-IN = ALL '+'                                           
037000       MOVE MID-IDPROJ-UT        TO WS-IDPROJ                             
037100     ELSE                                                                 
037200       MOVE MID-IDPROJ-IN        TO WS-IDPROJ                             
037300       MOVE '7'                  TO MFS-IDPFK                             
037400       MOVE SPACE                TO MFS-KDTRTYP                           
037500     END-IF                                                               
037600                                                                          
037700*    -- CHECK OF IDDC                                                     
037800     MOVE MFS-ERASE-FIELD        TO MOD-IDDC-IN                           
037900                                                                          
038000     IF MID-IDDC-IN = ALL '+'                                             
038100       MOVE MID-IDDC-UT          TO WS-IDDC                               
038200     ELSE                                                                 
038300       MOVE MID-IDDC-IN          TO WS-IDDC                               
038400       MOVE '7'                  TO MFS-IDPFK                             
038500       MOVE SPACE                TO MFS-KDTRTYP                           
038600     END-IF                                                               
038700                                                                          
038710     IF WS-IDDC = SPACE                                                   
038720        MOVE MSGI-IDFTG          TO WS-IDFTG                              
038730        IF IDFTG-US                                                       
038740           MOVE '41'             TO W-IDDC-MIN-X                          
038750           MOVE '49'             TO W-IDDC-MAX-X                          
038751           MOVE '40'             TO WS-IDDC                               
038760        ELSE                                                              
038770           IF IDFTG-CN                                                    
038780              MOVE '71'          TO W-IDDC-MIN-X                          
038790              MOVE '79'          TO W-IDDC-MAX-X                          
038791              MOVE '70'          TO WS-IDDC                               
038792           ELSE                                                           
038793              MOVE NOO           TO KEYS-SW                               
038795           END-IF                                                         
038796        END-IF                                                            
038797     ELSE                                                                 
038798        MOVE WS-IDDC             TO W-IDDC-MIN-X                          
038799                                    W-IDDC-MAX-X                          
038800        IF WS-IDDC(2:1) = '0'                                             
038801           MOVE '1'              TO W-IDDC2-MIN                           
038802           MOVE '9'              TO W-IDDC2-MAX                           
038803        END-IF                                                            
038804     END-IF                                                               
038805                                                                          
038806     MOVE NOO                    TO WS-DC-OK                              
038810     PERFORM IMS-GU-WDB601-MIN-MAX                                        
038811     PERFORM UNTIL SEGMENT-MISSING                                        
038812                OR SEGMENT-END                                            
038813                OR WS-DC-OK = YES                                         
038816        IF DCS-NDC-CN                                                     
038817        OR (DCS-NDC-NA AND DCS-USA)                                       
038820           MOVE DCS-IDLANDX2     TO W-IDLAND                              
038823           IF W-IDDC-MIN-X = W-IDDC-MAX-X                                 
038830             MOVE DCS-IDLEVNR-DC TO MOD-IDLEVNR-DC-UT                     
038831           END-IF                                                         
038840           MOVE YES              TO WS-DC-OK                              
038850        END-IF                                                            
038860        PERFORM IMS-GN-WDB601-MIN-MAX                                     
038870     END-PERFORM                                                          
038880                                                                          
038890     IF WS-DC-OK = NOO                                                    
038900        MOVE NOO                 TO KEYS-SW                               
039000     END-IF                                                               
039100                                                                          
039200     IF GOOD-MID OR KEYS-OK                                               
039300       MOVE WS-DAPUBL-FOM        TO MOD-DAPUBL-FOM-UT                     
039400       INSPECT MOD-DAPUBL-FOM-UT REPLACING LEADING ZERO BY SPACE          
039500       MOVE WS-DAPUBL-TOM        TO MOD-DAPUBL-TOM-UT                     
039600       INSPECT MOD-DAPUBL-TOM-UT REPLACING LEADING ZERO BY SPACE          
039700       MOVE WS-IDPROJ            TO MOD-IDPROJ-UT                         
039800       MOVE WS-IDDC              TO MOD-IDDC-UT                           
040300     ELSE                                                                 
040400       MOVE MFS-ERASE-FIELD      TO MOD-DAPUBL-FOM-UT                     
040500                                    MOD-DAPUBL-TOM-UT                     
040600                                    MOD-IDPROJ-UT                         
040700                                    MOD-IDDC-UT                           
040800                                    MOD-IDLEVNR-DC-UT                     
040900     END-IF                                                               
041000                                                                          
041100     IF KEYS-WRONG                                                        
041200       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
041300       CALL WMEDKONV          USING MED-WMEDAREA                          
041400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
041500       PERFORM MFS-ERASE-FIELD-OUT                                        
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 BA-CHECK-DATE SECTION.                                                   
042000                                                                          
042100     MOVE 'AAVVD'                TO DAT-KDDATFORM                         
042200                                                                          
042300     CALL WDATKONV            USING DAT-KDDATFORM                         
042400                                    DAT-I-TIDATUM                         
042500                                    DAT-O-TIDATUM                         
042600                                    DAT-KDSVAR                            
042700                                                                          
042800     .                                                                    
042900     EJECT                                                                
043000 C-FIRST-PAGE SECTION.                                                    
043100                                                                          
043200     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
043300     CALL WMEDKONV            USING MED-WMEDAREA                          
043400     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
043500                                                                          
043600     .                                                                    
043700     EJECT                                                                
043800 D-NEXT-PAGE SECTION.                                                     
043900                                                                          
044000     IF SAVE-IDTRANS = '2446'                                             
044100       MOVE SAVE-IDANSK-NEXT     TO W-IDANSK-GU                           
044200       MOVE SAVE-IDARTNR-NEXT    TO W-IDARTNR-GU                          
044300       MOVE SAVE-IDDC-NEXT       TO W-IDDC-GU                             
044400     END-IF                                                               
044500     IF SAVE-IDARTNR-NEXT = ZERO                                          
044600       MOVE '7'                  TO MFS-IDPFK                             
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 E-SAME-PAGE SECTION.                                                     
045100                                                                          
045200     IF SAVE-IDTRANS = '2446' OR '0551'                                   
045300       MOVE SAVE-IDANSK-ENTER    TO W-IDANSK-GU                           
045400       MOVE SAVE-IDARTNR-ENTER   TO W-IDARTNR-GU                          
045500       MOVE SAVE-IDDC-ENTER      TO W-IDDC-GU                             
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 F-READ-SHOW-INFO SECTION.                                                
046000                                                                          
046600     IF MFS-IDPFK = '7'                                                   
046700       PERFORM IMS-GN-WDC9A1                                              
046800     ELSE                                                                 
046900       PERFORM IMS-GU-WDC9A1                                              
047000     END-IF                                                               
047100                                                                          
047200     IF SEGMENT-MISSING                                                   
047300       MOVE ERR-PART-MISSING     TO MED-IDMFSFEL                          
047400       CALL WMEDKONV          USING MED-WMEDAREA                          
047500       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
047600       PERFORM MFS-ERASE-FIELD-OUT                                        
047700     ELSE                                                                 
047800       MOVE SEQA-IDANSK          TO SAVE-IDANSK-ENTER                     
047900       MOVE SEQA-IDARTNR         TO SAVE-IDARTNR-ENTER                    
048000       MOVE SEQA-IDDC            TO SAVE-IDDC-ENTER                       
048100       MOVE +1                   TO INDX                                  
048200       PERFORM UNTIL INDX > MAX-INDX                                      
048300         IF SEGMENT-FOUND                                                 
048400           PERFORM FA-READ-LINEDATA                                       
048500           PERFORM IMS-GN-WDC9A1                                          
048600         ELSE                                                             
048700           PERFORM MFS-ERASE-FIELD-LINE-OUT                               
048800           ADD +1                TO INDX                                  
048900         END-IF                                                           
049000       END-PERFORM                                                        
049100                                                                          
049200       IF SEGMENT-FOUND                                                   
049300         MOVE SEQA-IDANSK        TO SAVE-IDANSK-NEXT                      
049400         MOVE SEQA-IDARTNR       TO SAVE-IDARTNR-NEXT                     
049500         MOVE SEQA-IDDC          TO SAVE-IDDC-NEXT                        
049600         MOVE INF-MORE-INFO-EXISTS                                        
049700                                 TO MED-IDMFSINF                          
049800         CALL WMEDKONV        USING MED-WMEDAREA                          
049900         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
050000       ELSE                                                               
050100         MOVE ZERO               TO SAVE-IDANSK-NEXT                      
050200                                    SAVE-IDARTNR-NEXT                     
050300         MOVE SPACE              TO SAVE-IDDC-NEXT                        
050400       END-IF                                                             
050500                                                                          
050600       MOVE '002'                TO MSGI-KDCALL                           
050700       MOVE '2446'               TO SAVE-IDTRANS                          
050800       MOVE SAVE-AREA            TO MSGI-SPAR-AREA                        
050900       CALL W005INIT          USING MSGI-WMSGINIT                         
051000                                    WDP7-PCB                              
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 FA-READ-LINEDATA SECTION.                                                
051500                                                                          
051600     MOVE SEQA-IDARTNR           TO W-IDARTNR                             
051700     MOVE SEQA-IDDC              TO W-IDDC                                
051800                                                                          
051900     PERFORM IMS-GET-WDK712                                               
052000     IF SEGMENT-MISSING OR                                                
052100        LART-DAPUBL = ZERO                                                
052200       PERFORM IMS-GET-WDK601                                             
052300       IF SEGMENT-FOUND AND                                               
052400          ART-TIFINLV > ZERO                                              
052500         MOVE ART-TIFINLV        TO WS-AAVVD                              
052600       ELSE                                                               
052700         MOVE ZERO               TO WS-AAVVD                              
052800       END-IF                                                             
052900     ELSE                                                                 
053000       MOVE LART-DAPUBL          TO WS-AAAAMMDD                           
053100       MOVE WS-AAMMDD            TO DAT-I-TIDATUM                         
053200       PERFORM S01-CALL-WDATKONV                                          
053300       MOVE DAT-TIAAVVD          TO WS-AAVVD                              
053400     END-IF                                                               
053500                                                                          
053600     MOVE WS-AAVVD               TO TMP1-YYWWD                            
053700     MOVE WS-DAPUBL-FOM          TO TMP2-YYWWD                            
053800     PERFORM WY2000P2                                                     
053900     IF TMP1-YYWWD >= TMP2-YYWWD                                          
054000       MOVE WS-DAPUBL-TOM        TO TMP2-YYWWD                            
054100       PERFORM WY2000P2                                                   
054200       IF TMP1-YYWWD <= TMP2-YYWWD                                        
054300         PERFORM FAA-FILL-LINEDATA                                        
054400       END-IF                                                             
054500     END-IF                                                               
054600                                                                          
054700     .                                                                    
054800     EJECT                                                                
054900 FAA-FILL-LINEDATA SECTION.                                               
055000                                                                          
055100     PERFORM IMS-GET-WDK611                                               
055200     IF SEGMENT-FOUND                                                     
055300       IF (WS-IDPROJ = SPACE OR                                           
055400           WS-IDPROJ = CLAG-IDPROJ)                                       
055500*WDC9A                                                                    
055600         MOVE SEQA-IDARTNR       TO MOD-IDARTNR (INDX)                    
055700*WDK712 / WDK601                                                          
055800         IF WS-AAVVD > ZERO                                               
055900           MOVE WS-AAVVD         TO MOD-DAPUBL  (INDX)                    
056000         ELSE                                                             
056100           MOVE MFS-ERASE-FIELD  TO MOD-DAPUBL  (INDX)                    
056200         END-IF                                                           
056300*WDK611                                                                   
056400         MOVE CLAG-IDPROJ        TO MOD-IDPROJ  (INDX)                    
056500         MOVE CLAG-KDEMBKOD-2    TO MOD-KDEMBKOD-2 (INDX)                 
056600                                                                          
056700         PERFORM IMS-GET-WDC901                                           
056800         IF KART-TIREGDAT > ZERO                                          
056900           MOVE KART-TIREGDAT    TO MOD-TIREGDAT (INDX)                   
057000         ELSE                                                             
057100           MOVE MFS-ERASE-FIELD  TO MOD-TIREGDAT (INDX)                   
057200         END-IF                                                           
057300                                                                          
057400         PERFORM IMS-GET-WDD201                                           
057500         IF SEGMENT-FOUND                                                 
057600           MOVE ART-FLPISK       TO MOD-FLPISK  (INDX)                    
057700         ELSE                                                             
057800           MOVE MFS-ERASE-FIELD  TO MOD-FLPISK  (INDX)                    
057900         END-IF                                                           
058000                                                                          
058100         PERFORM IMS-GET-WDB601                                           
058200         IF SEGMENT-FOUND                                                 
058300           MOVE DCS-IDLEVNR-DC   TO MOD-IDLEVNR-DC (INDX)                 
058400         ELSE                                                             
058500           MOVE MFS-ERASE-FIELD  TO MOD-IDLEVNR-DC (INDX)                 
058600         END-IF                                                           
058700                                                                          
058800                                                                          
058900         ADD +1 TO INDX                                                   
059000       END-IF                                                             
059100     END-IF                                                               
059200                                                                          
059300     .                                                                    
059400     EJECT                                                                
059500 S01-CALL-WDATKONV SECTION.                                               
059600                                                                          
059700     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
059800                                                                          
059900     CALL WDATKONV            USING DAT-KDDATFORM                         
060000                                    DAT-I-TIDATUM                         
060100                                    DAT-O-TIDATUM                         
060200                                    DAT-KDSVAR                            
060300                                                                          
060400     IF DAT-KDSVAR-OK                                                     
060500       CONTINUE                                                           
060600     ELSE                                                                 
060700       MOVE 'BAD RETURN CODE FROM WDATKONV' TO ERROR-TEXT                 
060800       CALL FELLOG                                                        
060900     END-IF                                                               
061000                                                                          
061100     .                                                                    
061200     EJECT                                                                
061300 MFS-ERASE-FIELD-OUT SECTION.                                             
061400                                                                          
061500     PERFORM                                                              
061600     VARYING INDX FROM +1 BY +1                                           
061700       UNTIL INDX > MAX-INDX                                              
061800       PERFORM MFS-ERASE-FIELD-LINE-OUT                                   
061900     END-PERFORM                                                          
062000     .                                                                    
062100     SKIP3                                                                
062200 MFS-ERASE-FIELD-LINE-OUT SECTION.                                        
062300                                                                          
062400     MOVE MFS-ERASE-FIELD        TO MOD-IDARTNR (INDX)                    
062500                                    MOD-IDPROJ (INDX)                     
062600                                    MOD-KDEMBKOD-2 (INDX)                 
062700                                    MOD-DAPUBL (INDX)                     
062800                                    MOD-FLPISK (INDX)                     
062900                                    MOD-TIREGDAT (INDX)                   
063000                                    MOD-IDLEVNR-DC (INDX)                 
063100     .                                                                    
063200     SKIP3                                                                
063300* --- IMS SECTIONS ---                                                    
063400     SKIP3                                                                
063500 IMS-GET-MSG SECTION.                                                     
063600                                                                          
063700     MOVE '  QC'                 TO GOOD-STATUSCODES                      
063800     CALL CBLTDLI             USING GU                                    
063900                                    MSG-PCB                               
064000                                    MSG-IO-AREA                           
064100     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
064200     PERFORM IMS-STATUSCHECK                                              
064300     .                                                                    
064400     SKIP3                                                                
064500 IMS-INSERT-MSG SECTION.                                                  
064600                                                                          
064700     MOVE LOW-VALUE              TO MSG-KDZ1 MSG-KDZ2                     
064800     MOVE SPACE                  TO GOOD-STATUSCODES                      
064900     CALL CBLTDLI             USING ISRT                                  
065000                                    MSG-PCB                               
065100                                    MSG-IO-AREA                           
065200                                    MFS-IDMOD                             
065300     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
065400     PERFORM IMS-STATUSCHECK                                              
065500     .                                                                    
065600     EJECT                                                                
065700 IMS-GU-WDC9A1 SECTION.                                                   
065800                                                                          
065900     STRING 'WDC9A1  (WDC9A1KY =' W-WDC9A1KY-GU-X ')'                     
066000             DELIMITED BY SIZE INTO SSA1                                  
066100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
066200     CALL CBLTDLI             USING GU                                    
066300                                    WDC9A-PCB                             
066400                                    DLI-IO-WDC9A1                         
066500                                    SSA1                                  
066600     MOVE WDC9A-STATUS-CODE      TO STATUS-WS                             
066700     PERFORM IMS-STATUSCHECK                                              
066800     .                                                                    
066900     EJECT                                                                
067000 IMS-GN-WDC9A1 SECTION.                                                   
067100                                                                          
067200     STRING 'WDC9A1  (WDC9A1KY>=' W-WDC9A1KY-MIN-X                        
067300                    '&WDC9A1KY<=' W-WDC9A1KY-MAX-X                        
067400                    '&IDDC    >=' W-IDDC-MIN-X                            
067500                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
067600             DELIMITED BY SIZE INTO SSA1                                  
067700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
067800     CALL CBLTDLI             USING GN                                    
067900                                    WDC9A-PCB                             
068000                                    DLI-IO-WDC9A1                         
068100                                    SSA1                                  
068200     MOVE WDC9A-STATUS-CODE      TO STATUS-WS                             
068300     PERFORM IMS-STATUSCHECK                                              
068400     .                                                                    
068500     EJECT                                                                
068600 IMS-GET-WDC901 SECTION.                                                  
068700                                                                          
068800     STRING 'WDC901  (WDC901KY =' W-WDC901KY-X ')'                        
068900             DELIMITED BY SIZE INTO SSA1                                  
069000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
069100     CALL CBLTDLI             USING GU                                    
069200                                    WDC9-PCB                              
069300                                    DLI-IO-WDC901                         
069400                                    SSA1                                  
069500     MOVE WDC9-STATUS-CODE       TO STATUS-WS                             
069600     PERFORM IMS-STATUSCHECK                                              
069700     .                                                                    
069800     EJECT                                                                
069900 IMS-GET-WDK601 SECTION.                                                  
070000                                                                          
070100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
070200             DELIMITED BY SIZE INTO SSA1                                  
070300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
070400     CALL CBLTDLI             USING GU                                    
070500                                    WDK6-PCB                              
070600                                    DLI-IO-WDK601                         
070700                                    SSA1                                  
070800     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
070900     PERFORM IMS-STATUSCHECK                                              
071000     .                                                                    
071100     EJECT                                                                
071200 IMS-GET-WDK611 SECTION.                                                  
071300                                                                          
071400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
071500             DELIMITED BY SIZE INTO SSA1                                  
071600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
071700             DELIMITED BY SIZE INTO SSA2                                  
071800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
071900     CALL CBLTDLI             USING GU                                    
072000                                    WDK6-PCB                              
072100                                    DLI-IO-WDK611                         
072200                                    SSA1 SSA2                             
072300     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
072400     PERFORM IMS-STATUSCHECK                                              
072500     .                                                                    
072600     EJECT                                                                
072700 IMS-GET-WDK712 SECTION.                                                  
072800                                                                          
072900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
073000             DELIMITED BY SIZE INTO SSA1                                  
073100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
073200             DELIMITED BY SIZE INTO SSA2                                  
073300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
073400     CALL CBLTDLI             USING GU                                    
073500                                    WDK7-PCB                              
073600                                    DLI-IO-WDK712                         
073700                                    SSA1 SSA2                             
073800     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
073900     PERFORM IMS-STATUSCHECK                                              
074000     .                                                                    
074100     EJECT                                                                
074200 IMS-GET-WDB601 SECTION.                                                  
074300                                                                          
074400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
074500             DELIMITED BY SIZE INTO SSA1                                  
074600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
074700     CALL CBLTDLI             USING GU                                    
074800                                    WDB6-PCB                              
074900                                    DLI-IO-WDB601                         
075000                                    SSA1                                  
075100     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
075200     PERFORM IMS-STATUSCHECK                                              
075300     .                                                                    
075400                                                                          
075493 IMS-GU-WDB601-MIN-MAX SECTION.                                           
075495                                                                          
075497     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-MIN-X                            
075498                    '&WDB6ASEQ<=' W-IDDC-MAX-X ')'                        
075499       DELIMITED BY SIZE  INTO SSA1                                       
075500     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
075501     CALL CBLTDLI USING GU WDB6A-PCB DLI-IO-WDB601 SSA1                   
075502     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
075503     PERFORM IMS-STATUSCHECK                                              
075504     .                                                                    
075505                                                                          
075506 IMS-GN-WDB601-MIN-MAX SECTION.                                           
075508                                                                          
075510     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-MIN-X                            
075511                    '&WDB6ASEQ<=' W-IDDC-MAX-X ')'                        
075512       DELIMITED BY SIZE  INTO SSA1                                       
075513     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
075514     CALL CBLTDLI USING GN WDB6A-PCB DLI-IO-WDB601 SSA1                   
075515     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
075516     PERFORM IMS-STATUSCHECK                                              
075517     .                                                                    
075518                                                                          
075520 IMS-GET-WDD201 SECTION.                                                  
075600                                                                          
075700     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
075800             DELIMITED BY SIZE INTO SSA1                                  
075900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
076000     CALL CBLTDLI             USING GU                                    
076100                                    WDD2-PCB                              
076200                                    DLI-IO-WDD201                         
076300                                    SSA1                                  
076400     MOVE WDD2-STATUS-CODE       TO STATUS-WS                             
076500     PERFORM IMS-STATUSCHECK                                              
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-STATUSCHECK SECTION.                                                 
076900                                                                          
077000     SET STATUS-IX               TO 1                                     
077100     SEARCH GOOD-STATUS                                                   
077200       AT END                                                             
077300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
077400             DELIMITED BY SIZE INTO ERROR-TEXT                            
077500         CALL FELLOG                                                      
077600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
077700         CONTINUE                                                         
077800     END-SEARCH                                                           
077900     .                                                                    
078000     EJECT                                                                
078100*    -COPY WY2000P2                                                       
078200     EJECT                                                                
