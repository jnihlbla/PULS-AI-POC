000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2044200.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   12/11/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN THAT SHOW REGISTERED PARTS FROM THE PROCURER QUEUE        
000900*        THAT ARE SENT TO SI+. NOW WAITING FOR AGREEMENT FROM SI+.        
001000*                                                                         
001100*        THE PROGRAM READS     WDC9                                       
001200*        THE PROGRAM READS     WDK6                                       
001300*        THE PROGRAM READS     WDK7                                       
001400*        THE PROGRAM READS     WDB6                                       
001500*        THE PROGRAM READS     WDD2                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W2T442                                              
001900*        MID:         W2I44201                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W2O442N1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W2044200'.            
003100                                                                          
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  WS-IDANSK-FOM               PIC X(3)    VALUE ZERO.                  
003900 77  WS-IDANSK-TOM               PIC X(3)    VALUE ZERO.                  
004000 77  WS-IDARTNR                  PIC X(9)    VALUE ZERO.                  
004100 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004200 77  WS-IDPROJ                   PIC X(4)    VALUE SPACE.                 
004300                                                                          
004400 01  WS-AAAAMMDD                 PIC 9(8).                                
004500 01  FILLER REDEFINES WS-AAAAMMDD.                                        
004600     03  FILLER                  PIC 9(2).                                
004700     03  WS-AAMMDD               PIC 9(6).                                
004800                                                                          
004900 01  WS-AAVVD                    PIC 9(5).                                
005000 01  FILLER REDEFINES WS-AAVVD.                                           
005200     03  WS-AAVV                 PIC 9(4).                                
005210     03  FILLER                  PIC 9(1).                                
005300                                                                          
005400*    --- INDEX FOR SCROLL LINES                                           
005500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
005700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005800                                                                          
005900                                                                          
006000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006100     88  KEYS-OK                             VALUE 'J'.                   
006200     88  KEYS-WRONG                          VALUE 'N'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  OWN-MID                             VALUE '2442'.                
006600     88  GOOD-MID                            VALUE '2442'.                
007100     88  HELP-MID                            VALUE '0551'.                
007200     EJECT                                                                
007210 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
007220*01 -COPY WWIDFTG                                                         
007230     EJECT                                                                
007300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008000     EJECT                                                                
008100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008200*01 -COPY WMEDAREA                                                        
008300     SKIP3                                                                
008400 01  MESSAGE-CODES.                                                       
008500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008800     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
008900     EJECT                                                                
009000*01  -COPY WDATAREA                                                       
009100     EJECT                                                                
009200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009300*                                                                         
009400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009500     SKIP3                                                                
009600*01 -COPY WMSGINIT                                                        
009700     EJECT                                                                
009800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009900*                                                                         
010000 01  SAVE-AREA.                                                           
010100     03  SAVE-IDTRANS            PIC X(4)    VALUE '2442'.                
010200     03  SAVE-IDANSK-ENTER       PIC S9(3)        COMP-3.                 
010300     03  SAVE-IDANSK-NEXT        PIC S9(3)        COMP-3.                 
010400     03  SAVE-IDARTNR-ENTER      PIC S9(9)        COMP-3.                 
010500     03  SAVE-IDARTNR-NEXT       PIC S9(9)        COMP-3.                 
010600     03  SAVE-IDDC-ENTER         PIC X(2).                                
010700     03  SAVE-IDDC-NEXT          PIC X(2).                                
010800     EJECT                                                                
010900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200     SKIP3                                                                
011300*01  MID -COPY W2I44201                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W2O44201                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300     SKIP3                                                                
012400*01  -COPY WMFSAREA                                                       
012500     EJECT                                                                
012600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  KEYS-FOR-DLI.                                                        
013100*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
013200     03  W-WDC9A1KY-GU-X.                                                 
013300         05  W-KDANSKQ-GU-X.                                              
013400             07  W-KDANSKQ-GU    PIC X       VALUE '2'.                   
013500         05  W-IDANSK-GU-X.                                               
013600             07  W-IDANSK-GU     PIC S9(3)   VALUE ZERO COMP-3.           
013610         05  W-IDARTNR-GU-X.                                              
013620             07  W-IDARTNR-GU    PIC S9(9)   VALUE ZERO COMP-3.           
013630         05  W-IDDC-GU-X.                                                 
013640             07  W-IDDC-GU       PIC X(2)    VALUE SPACE.                 
013800                                                                          
013810     03  W-WDC9A1KY-MIN-X.                                                
013820         05  W-KDANSKQ-MIN-X.                                             
013830             07  W-KDANSKQ-MIN   PIC X       VALUE '2'.                   
013840         05  W-IDANSK-MIN-X.                                              
013850             07  W-IDANSK-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
013860         05  FILLER              PIC X(7)    VALUE LOW-VALUES.            
013870                                                                          
013900     03  W-WDC9A1KY-MAX-X.                                                
014000         05  W-KDANSKQ-MAX-X.                                             
014100             07  W-KDANSKQ-MAX   PIC X       VALUE '2'.                   
014200         05  W-IDANSK-MAX-X.                                              
014300             07  W-IDANSK-MAX    PIC S9(3)   VALUE 999 COMP-3.            
014400         05  FILLER              PIC X(7)    VALUE HIGH-VALUES.           
014500                                                                          
014600     03  W-IDARTNR-MIN-X.                                                 
014700         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
014800     03  W-IDARTNR-MAX-X.                                                 
014900         05  W-IDARTNR-MAX       PIC S9(9)   VALUE 999999999              
015000                                             COMP-3.                      
015100     03  W-IDDC-MIN-X.                                                    
015200         05  FILLER              PIC X(1)    VALUE SPACE.                 
015210         05  W-IDDC2-MIN         PIC X(1)    VALUE SPACE.                 
015300     03  W-IDDC-MAX-X.                                                    
015400         05  FILLER              PIC X(1)    VALUE SPACE.                 
015410         05  W-IDDC2-MAX         PIC X(1)    VALUE SPACE.                 
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
018800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
018900 01  DLI-IO-WDC901.                                                       
019000*    03  -COPY WDC901                                                     
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
019200 01  DLI-IO-WDK601.                                                       
019300*    03  -COPY WDK601                                                     
019400     EJECT                                                                
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
019600 01  DLI-IO-WDK611.                                                       
019700*    03  -COPY WDK611                                                     
019800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
019900 01  DLI-IO-WDK701.                                                       
020000*    03  -COPY WDK701                                                     
020100     EJECT                                                                
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
020300 01  DLI-IO-WDK711.                                                       
020400*    03  -COPY WDK711                                                     
020500     EJECT                                                                
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
020700 01  DLI-IO-WDK712.                                                       
020800*    03  -COPY WDK712                                                     
020900     EJECT                                                                
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
021100 01  DLI-IO-WDK722.                                                       
021200*    03  -COPY WDK722                                                     
021300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
021400 01  DLI-IO-WDB601.                                                       
021500*    03  -COPY WDB601                                                     
021600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
021700 01  DLI-IO-WDD201.                                                       
021800*    03  -COPY WDD201                                                     
021900     EJECT                                                                
022000 LINKAGE SECTION.                                                         
022100*01  -COPY W0009   -PRE MSG-                                              
022200*01  -COPY W0008   -PRE WDP7-                                             
022300     05  FILLER                  PIC X.                                   
022400                                                                          
022500*01  -COPY W0008  -PRE WDC9A-                                             
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008  -PRE WDC9-                                              
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE WDK6-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400*01  -COPY W0008  -PRE WDK7-                                              
023500     05  FILLER                  PIC X.                                   
023600                                                                          
023700*01  -COPY W0008  -PRE WDB6-                                              
023800     05  FILLER                  PIC X.                                   
023900                                                                          
024000*01  -COPY W0008  -PRE WDD2-                                              
024100     05  FILLER                  PIC X.                                   
024200     EJECT                                                                
024300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDC9A-PCB WDC9-PCB            
024400     WDK6-PCB WDK7-PCB WDB6-PCB WDD2-PCB.                                 
024500 MAIN SECTION.                                                            
024600     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDC9A-PCB WDC9-PCB            
024700     WDK6-PCB WDK7-PCB WDB6-PCB WDD2-PCB.                                 
024800                                                                          
024900*------------------------                                                 
025000     PERFORM IMS-GET-MSG                                                  
025100     IF SEGMENT-FOUND                                                     
025200       PERFORM A-INIT                                                     
025300       PERFORM B-CHECK-KEYS                                               
025400       IF KEYS-OK                                                         
025500         IF MFS-FIRST                                                     
025600           PERFORM C-FIRST-PAGE                                           
025700         ELSE                                                             
025800           IF MFS-NEXT                                                    
025900             PERFORM D-NEXT-PAGE                                          
026000           ELSE                                                           
026100             PERFORM E-SAME-PAGE                                          
026200           END-IF                                                         
026300         END-IF                                                           
026400         PERFORM F-READ-SHOW-INFO                                         
026500       END-IF                                                             
026600       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O44201 + 4                      
026700       PERFORM IMS-INSERT-MSG                                             
026800     END-IF                                                               
026900                                                                          
027000     MOVE ZERO                   TO RETURN-CODE                           
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500                                                                          
027600     IF MSG-DOUBLE-TRANSACTIONS                                           
027700       MOVE MSG-INDATA-MINUS-2-TRANSACT                                   
027800                                 TO MID-W2I44201                          
027900       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
028000       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
028100     ELSE                                                                 
028200       MOVE MSG-INDATA-MINUS-1-TRANSACT                                   
028300                                 TO MID-W2I44201                          
028400       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
028500       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
028600     END-IF                                                               
028700                                                                          
028800     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
028900     MOVE MSG-IDPFK              TO MFS-IDPFK                             
029000     MOVE MFS-IDTRANS            TO W-IDTRANS                             
029100                                                                          
029200     MOVE LOW-VALUE              TO MSG-AREA                              
029300     MOVE 'W2O442N1'             TO MFS-IDMOD                             
029400     MOVE '2442'                 TO MOD-IDTRANS                           
029500     MOVE MFS-ERASE-FIELD        TO MOD-TEMFSFEL                          
029600                                    MOD-TEMFSINF                          
029700                                                                          
029800     IF OWN-MID OR HELP-MID                                               
029900       CONTINUE                                                           
030000     ELSE                                                                 
030100       MOVE SPACE                TO MFS-KDTRTYP                           
030200       MOVE '7'                  TO MFS-IDPFK                             
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 B-CHECK-KEYS SECTION.                                                    
033300                                                                          
033310     MOVE ALL '+'                TO MSGI-WMSGINIT                         
033320     MOVE '001'                  TO MSGI-KDCALL                           
033330     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
033340     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
033350     MOVE '2442'                 TO MSGI-IDTRANS                          
033360     CALL W005INIT            USING MSGI-WMSGINIT                         
033370                                    WDP7-PCB                              
033380     MOVE MSGI-SPAR-AREA         TO SAVE-AREA                             
033390                                                                          
033391*    - LANGUAGE TO BE USED BY MEDKONV                                     
033392     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
033393                                                                          
033394     MOVE YES                    TO KEYS-SW                               
033395                                                                          
033396                                                                          
033397*    -- CHECK OF IDARTNR                                                  
033398     MOVE MFS-ERASE-FIELD        TO MOD-IDARTNR-IN                        
033399                                                                          
033400     IF MID-IDARTNR-IN = ALL '+'                                          
033401       MOVE MID-IDARTNR-UT       TO WS-IDARTNR                            
033402     ELSE                                                                 
033403       MOVE MID-IDARTNR-IN       TO WS-IDARTNR                            
033404       MOVE '7'                  TO MFS-IDPFK                             
033405       MOVE SPACE                TO MFS-KDTRTYP                           
033406     END-IF                                                               
033407                                                                          
033410     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
033500     IF WS-IDARTNR NUMERIC                                                
033600       IF WS-IDARTNR > 0                                                  
033700         MOVE WS-IDARTNR         TO W-IDARTNR                             
033710         PERFORM IMS-GET-WDK601                                           
033720         IF SEGMENT-FOUND                                                 
033730           MOVE ART-REKSIFFR     TO MOD-REKSIFFR                          
033740           MOVE '-'              TO MOD-DASH                              
033750         END-IF                                                           
033800       ELSE                                                               
033900         MOVE ZERO               TO W-IDARTNR                             
034000       END-IF                                                             
034100     ELSE                                                                 
034200       MOVE NOO                  TO KEYS-SW                               
034300     END-IF                                                               
034400                                                                          
034500*    -- CHECK OF IDANSK-FOM                                               
034600     MOVE MFS-ERASE-FIELD        TO MOD-IDANSK-FOM-IN                     
034700                                                                          
034800     IF MID-IDANSK-FOM-IN = ALL '+'                                       
034900       MOVE MID-IDANSK-FOM-UT    TO WS-IDANSK-FOM                         
035000     ELSE                                                                 
035100       MOVE MID-IDANSK-FOM-IN    TO WS-IDANSK-FOM                         
035200       MOVE '7'                  TO MFS-IDPFK                             
035300       MOVE SPACE                TO MFS-KDTRTYP                           
035400     END-IF                                                               
035500                                                                          
035600     INSPECT WS-IDANSK-FOM REPLACING LEADING SPACE BY ZERO                
035700     IF WS-IDANSK-FOM NUMERIC                                             
035800       IF WS-IDANSK-FOM > ZERO                                            
035900         MOVE WS-IDANSK-FOM      TO W-IDANSK-MIN                          
035910       ELSE                                                               
035920         MOVE ZERO               TO W-IDANSK-MIN                          
035930                                    WS-IDANSK-FOM                         
036000       END-IF                                                             
036100     ELSE                                                                 
036200       MOVE NOO                  TO KEYS-SW                               
036300     END-IF                                                               
036400                                                                          
036500*    -- CHECK OF IDANSK-TOM                                               
036600     MOVE MFS-ERASE-FIELD        TO MOD-IDANSK-TOM-IN                     
036700                                                                          
036800     IF MID-IDANSK-TOM-IN = ALL '+'                                       
036900       MOVE MID-IDANSK-TOM-UT    TO WS-IDANSK-TOM                         
037000     ELSE                                                                 
037100       MOVE MID-IDANSK-TOM-IN    TO WS-IDANSK-TOM                         
037200       MOVE '7'                  TO MFS-IDPFK                             
037300       MOVE SPACE                TO MFS-KDTRTYP                           
037400     END-IF                                                               
037500                                                                          
037600     INSPECT WS-IDANSK-TOM REPLACING LEADING SPACE BY ZERO                
037700     IF WS-IDANSK-TOM NUMERIC                                             
037800       IF WS-IDANSK-TOM > ZERO                                            
037900         MOVE WS-IDANSK-TOM      TO W-IDANSK-MAX                          
037910       ELSE                                                               
037920         MOVE 999                TO W-IDANSK-MAX                          
037930                                    WS-IDANSK-TOM                         
038000       END-IF                                                             
038100     ELSE                                                                 
038200       MOVE NOO                  TO KEYS-SW                               
038300     END-IF                                                               
038400                                                                          
038500*    -- CHECK OF IDPROJ                                                   
038600     MOVE MFS-ERASE-FIELD        TO MOD-IDPROJ-IN                         
038700                                                                          
038800     IF MID-IDPROJ-IN = ALL '+'                                           
038900       MOVE MID-IDPROJ-UT        TO WS-IDPROJ                             
039000     ELSE                                                                 
039100       MOVE MID-IDPROJ-IN        TO WS-IDPROJ                             
039200       MOVE '7'                  TO MFS-IDPFK                             
039300       MOVE SPACE                TO MFS-KDTRTYP                           
039400     END-IF                                                               
039500                                                                          
039600*    -- CHECK OF IDDC                                                     
039700     MOVE MFS-ERASE-FIELD        TO MOD-IDDC-IN                           
039800                                                                          
039900     IF MID-IDDC-IN = ALL '+'                                             
040000       MOVE MID-IDDC-UT          TO WS-IDDC                               
040100     ELSE                                                                 
040200       MOVE MID-IDDC-IN          TO WS-IDDC                               
040300       MOVE '7'                  TO MFS-IDPFK                             
040400       MOVE SPACE                TO MFS-KDTRTYP                           
040500     END-IF                                                               
040600                                                                          
040610     IF WS-IDDC = SPACE                                                   
040620        MOVE MSGI-IDFTG          TO WS-IDFTG                              
040630        IF IDFTG-US                                                       
040640           MOVE '41'             TO W-IDDC-MIN-X                          
040650           MOVE '49'             TO W-IDDC-MAX-X                          
040651           MOVE '40'             TO WS-IDDC                               
040660        ELSE                                                              
040670           IF IDFTG-CN                                                    
040680              MOVE '71'          TO W-IDDC-MIN-X                          
040690              MOVE '79'          TO W-IDDC-MAX-X                          
040691              MOVE '70'          TO WS-IDDC                               
040692           ELSE                                                           
040693              MOVE NOO           TO KEYS-SW                               
040694           END-IF                                                         
040695        END-IF                                                            
040696     ELSE                                                                 
040697        MOVE WS-IDDC             TO W-IDDC-MIN-X                          
040698                                    W-IDDC-MAX-X                          
040699        IF WS-IDDC(2:1) = '0'                                             
040700           MOVE '1'              TO W-IDDC2-MIN                           
040701           MOVE '9'              TO W-IDDC2-MAX                           
040710        END-IF                                                            
040720     END-IF                                                               
040730                                                                          
040792     PERFORM IMS-GU-WDB601-MIN-MAX                                        
040793     IF SEGMENT-FOUND                                                     
040794        IF DCS-NDC-CN                                                     
040795        OR (DCS-NDC-NA AND DCS-USA)                                       
040796           MOVE DCS-IDLANDX2     TO W-IDLAND                              
040797        ELSE                                                              
040798           MOVE NOO              TO KEYS-SW                               
040800        END-IF                                                            
040801     ELSE                                                                 
040803        MOVE NOO                 TO KEYS-SW                               
040804     END-IF                                                               
041000                                                                          
041100     IF GOOD-MID OR KEYS-OK                                               
041200       MOVE WS-IDARTNR           TO MOD-IDARTNR-UT                        
041210       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
041300       MOVE WS-IDANSK-FOM        TO MOD-IDANSK-FOM-UT                     
041400       MOVE WS-IDANSK-TOM        TO MOD-IDANSK-TOM-UT                     
041500       MOVE WS-IDPROJ            TO MOD-IDPROJ-UT                         
041600       MOVE WS-IDDC              TO MOD-IDDC-UT                           
041700     ELSE                                                                 
041800       MOVE MFS-ERASE-FIELD      TO MOD-IDARTNR-UT                        
041900                                    MOD-DASH                              
042000                                    MOD-REKSIFFR                          
042100                                    MOD-IDANSK-FOM-UT                     
042200                                    MOD-IDANSK-TOM-UT                     
042300                                    MOD-IDPROJ-UT                         
042400                                    MOD-IDDC-UT                           
042500                                    MOD-IDLEVNR-DC-UT                     
042600     END-IF                                                               
042700                                                                          
042800     IF KEYS-WRONG                                                        
042900       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
043000       CALL WMEDKONV          USING MED-WMEDAREA                          
043100       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
043200       PERFORM MFS-ERASE-FIELD-OUT                                        
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600 C-FIRST-PAGE SECTION.                                                    
043700                                                                          
043800     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
043900     CALL WMEDKONV            USING MED-WMEDAREA                          
044000     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
044100                                                                          
044200     .                                                                    
044300     EJECT                                                                
044400 D-NEXT-PAGE SECTION.                                                     
044500                                                                          
044600     IF SAVE-IDTRANS = '2442'                                             
044700       MOVE SAVE-IDANSK-NEXT     TO W-IDANSK-GU                           
044800       MOVE SAVE-IDARTNR-NEXT    TO W-IDARTNR-GU                          
044900       MOVE SAVE-IDDC-NEXT       TO W-IDDC-GU                             
045000     END-IF                                                               
045010     IF SAVE-IDARTNR-NEXT = ZERO                                          
045020       MOVE '7'                  TO MFS-IDPFK                             
045030     END-IF                                                               
045100     .                                                                    
045200     EJECT                                                                
045300 E-SAME-PAGE SECTION.                                                     
045400                                                                          
045500     IF SAVE-IDTRANS = '2442' OR '0551'                                   
045600       MOVE SAVE-IDANSK-ENTER    TO W-IDANSK-GU                           
045700       MOVE SAVE-IDARTNR-ENTER   TO W-IDARTNR-GU                          
045800       MOVE SAVE-IDDC-ENTER      TO W-IDDC-GU                             
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 F-READ-SHOW-INFO SECTION.                                                
046300                                                                          
046400     IF WS-IDARTNR > ZERO                                                 
046500       MOVE WS-IDARTNR           TO W-IDARTNR-MIN                         
046600                                    W-IDARTNR-MAX                         
046700     END-IF                                                               
047213                                                                          
047220     IF MFS-IDPFK = '7'                                                   
047300       PERFORM IMS-GN-WDC9A1                                              
047301     ELSE                                                                 
047302       PERFORM IMS-GU-WDC9A1                                              
047303     END-IF                                                               
047400                                                                          
047500     IF SEGMENT-MISSING                                                   
047600       MOVE ERR-PART-MISSING     TO MED-IDMFSFEL                          
047700       CALL WMEDKONV          USING MED-WMEDAREA                          
047800       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
047900       PERFORM MFS-ERASE-FIELD-OUT                                        
048000     ELSE                                                                 
048010       MOVE SEQA-IDANSK          TO SAVE-IDANSK-ENTER                     
048020       MOVE SEQA-IDARTNR         TO SAVE-IDARTNR-ENTER                    
048030       MOVE SEQA-IDDC            TO SAVE-IDDC-ENTER                       
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
050700       MOVE '2442'               TO SAVE-IDTRANS                          
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
051900     PERFORM IMS-GET-WDK722                                               
051910     IF SEGMENT-FOUND                                                     
052000       PERFORM IMS-GET-WDK611                                             
052010       IF SEGMENT-FOUND                                                   
052100         IF (WS-IDPROJ = SPACE OR                                         
052200             WS-IDPROJ = CLAG-IDPROJ) AND                                 
052300            (XLAG-IDANSK >= W-IDANSK-MIN AND                              
052400             XLAG-IDANSK <= W-IDANSK-MAX)                                 
052500*WDC9A                                                                    
052600           MOVE SEQA-IDARTNR     TO MOD-IDARTNR (INDX)                    
052700*WDK722                                                                   
052800           MOVE XLAG-IDINK       TO MOD-IDINK   (INDX)                    
052900*WDK611                                                                   
053000           MOVE CLAG-IDPROJ      TO MOD-IDPROJ  (INDX)                    
053100           MOVE CLAG-IDPROJUP    TO MOD-IDPROJUP (INDX)                   
053200                                                                          
053300           PERFORM IMS-GET-WDC901                                         
053310           IF KART-TIMOTSI > 0                                            
053400             MOVE KART-TIMOTSI   TO MOD-TIMOTSI (INDX)                    
053401           ELSE                                                           
053402             MOVE MFS-ERASE-FIELD                                         
053403                                 TO MOD-TIMOTSI (INDX)                    
053404           END-IF                                                         
053410           IF KART-TIINKOP > ZERO                                         
053500             MOVE KART-TIINKOP   TO DAT-I-TIDATUM                         
053600             PERFORM S01-CALL-WDATKONV                                    
053700             MOVE DAT-TIAAVV-GRP TO MOD-TIINKOP (INDX)                    
053710           ELSE                                                           
053720             MOVE MFS-ERASE-FIELD                                         
053721                                 TO MOD-TIINKOP (INDX)                    
053730           END-IF                                                         
053800                                                                          
053900           PERFORM IMS-GET-WDK712                                         
054000           IF SEGMENT-MISSING OR                                          
054010             LART-DAPUBL = ZERO                                           
054100             PERFORM IMS-GET-WDK601                                       
054110             IF SEGMENT-FOUND AND                                         
054120                ART-TIFINLV > ZERO                                        
054200               MOVE ART-TIFINLV  TO WS-AAVVD                              
054300               MOVE WS-AAVV      TO MOD-DAPUBL  (INDX)                    
054310             ELSE                                                         
054320               MOVE MFS-ERASE-FIELD                                       
054321                                 TO MOD-DAPUBL  (INDX)                    
054330             END-IF                                                       
054400           ELSE                                                           
054500             MOVE LART-DAPUBL    TO WS-AAAAMMDD                           
054600             MOVE WS-AAMMDD      TO DAT-I-TIDATUM                         
054700             PERFORM S01-CALL-WDATKONV                                    
054800             MOVE DAT-TIAAVV-GRP TO MOD-DAPUBL  (INDX)                    
054900           END-IF                                                         
055000                                                                          
055100           PERFORM IMS-GET-WDD201                                         
055110           IF SEGMENT-FOUND                                               
055200             MOVE ART-IDPROJK    TO MOD-IDPROJK (INDX)                    
055300             MOVE ART-FLPISK     TO MOD-FLPISK  (INDX)                    
055301           ELSE                                                           
055302             MOVE MFS-ERASE-FIELD                                         
055303                                 TO MOD-IDPROJK (INDX)                    
055304                                    MOD-FLPISK  (INDX)                    
055310           END-IF                                                         
055400                                                                          
055500           PERFORM IMS-GET-WDB601                                         
055510           IF SEGMENT-FOUND                                               
055600             MOVE DCS-IDLEVNR-DC TO MOD-IDLEVNR-DC (INDX)                 
055610           ELSE                                                           
055611             MOVE MFS-ERASE-FIELD                                         
055620                                 TO MOD-IDLEVNR-DC (INDX)                 
055630           END-IF                                                         
055700                                                                          
055800                                                                          
055900           ADD +1 TO INDX                                                 
056000         END-IF                                                           
056001       END-IF                                                             
056010     END-IF                                                               
056100                                                                          
056200     .                                                                    
056300     EJECT                                                                
056400 S01-CALL-WDATKONV SECTION.                                               
056500                                                                          
056600     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
056700                                                                          
056800     CALL WDATKONV            USING DAT-KDDATFORM                         
056900                                    DAT-I-TIDATUM                         
057000                                    DAT-O-TIDATUM                         
057100                                    DAT-KDSVAR                            
057200                                                                          
057300     IF DAT-KDSVAR-OK                                                     
057400       CONTINUE                                                           
057500     ELSE                                                                 
057600       MOVE 'BAD RETURN CODE FROM WDATKONV' TO ERROR-TEXT                 
057700       CALL FELLOG                                                        
057800     END-IF                                                               
057900                                                                          
058000     .                                                                    
058100     EJECT                                                                
058200 MFS-ERASE-FIELD-OUT SECTION.                                             
058300                                                                          
058400     PERFORM                                                              
058500     VARYING INDX FROM +1 BY +1                                           
058600       UNTIL INDX > MAX-INDX                                              
058700       PERFORM MFS-ERASE-FIELD-LINE-OUT                                   
058800     END-PERFORM                                                          
058900     .                                                                    
059000     SKIP3                                                                
059100 MFS-ERASE-FIELD-LINE-OUT SECTION.                                        
059200                                                                          
059300     MOVE MFS-ERASE-FIELD        TO MOD-IDARTNR (INDX)                    
059400                                    MOD-IDPROJ (INDX)                     
059500                                    MOD-IDPROJK (INDX)                    
059600                                    MOD-IDPROJUP (INDX)                   
059700                                    MOD-IDLEVNR-DC (INDX)                 
059800                                    MOD-DAPUBL (INDX)                     
059900                                    MOD-FLPISK (INDX)                     
060000                                    MOD-TIINKOP (INDX)                    
060100                                    MOD-IDINK (INDX)                      
060200                                    MOD-TIMOTSI (INDX)                    
060300     .                                                                    
060400     SKIP3                                                                
060500 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
060600                                                                          
060700     PERFORM                                                              
060800     VARYING INDX FROM +1 BY +1                                           
060900       UNTIL INDX > MAX-INDX                                              
061000       PERFORM MFS-DONT-TOUCH-FIELD-LINE-OUT                              
061100     END-PERFORM                                                          
061200     .                                                                    
061300     SKIP3                                                                
061400 MFS-DONT-TOUCH-FIELD-LINE-OUT  SECTION.                                  
061500                                                                          
061600     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR (INDX)                    
061700                                    MOD-IDPROJ (INDX)                     
061800                                    MOD-IDPROJK (INDX)                    
061900                                    MOD-IDPROJUP (INDX)                   
062000                                    MOD-IDLEVNR-DC (INDX)                 
062100                                    MOD-DAPUBL (INDX)                     
062200                                    MOD-FLPISK (INDX)                     
062300                                    MOD-TIINKOP (INDX)                    
062400                                    MOD-IDINK (INDX)                      
062500                                    MOD-TIMOTSI (INDX)                    
062600     .                                                                    
062700     SKIP3                                                                
062800* --- IMS SECTIONS ---                                                    
062900     SKIP3                                                                
063000 IMS-GET-MSG SECTION.                                                     
063100                                                                          
063200     MOVE '  QC'                 TO GOOD-STATUSCODES                      
063300     CALL CBLTDLI             USING GU                                    
063400                                    MSG-PCB                               
063500                                    MSG-IO-AREA                           
063600     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
063700     PERFORM IMS-STATUSCHECK                                              
063800     .                                                                    
063900     SKIP3                                                                
064000 IMS-INSERT-MSG SECTION.                                                  
064100                                                                          
064200     MOVE LOW-VALUE              TO MSG-KDZ1 MSG-KDZ2                     
064300     MOVE SPACE                  TO GOOD-STATUSCODES                      
064400     CALL CBLTDLI             USING ISRT                                  
064500                                    MSG-PCB                               
064600                                    MSG-IO-AREA                           
064700                                    MFS-IDMOD                             
064800     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
064900     PERFORM IMS-STATUSCHECK                                              
065000     .                                                                    
065100     EJECT                                                                
065200 IMS-GU-WDC9A1 SECTION.                                                   
065300                                                                          
065400     STRING 'WDC9A1  (WDC9A1KY =' W-WDC9A1KY-GU-X ')'                     
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
066910 IMS-GN-WDC9A1 SECTION.                                                   
066920                                                                          
066930     STRING 'WDC9A1  (WDC9A1KY>=' W-WDC9A1KY-MIN-X                        
066940                    '&WDC9A1KY<=' W-WDC9A1KY-MAX-X                        
066950                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
066960                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
066970                    '&IDDC    >=' W-IDDC-MIN-X                            
066980                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
066990             DELIMITED BY SIZE INTO SSA1                                  
066991     MOVE '  GE'                 TO GOOD-STATUSCODES                      
066992     CALL CBLTDLI             USING GN                                    
066993                                    WDC9A-PCB                             
066994                                    DLI-IO-WDC9A1                         
066995                                    SSA1                                  
066996     MOVE WDC9A-STATUS-CODE      TO STATUS-WS                             
066997     PERFORM IMS-STATUSCHECK                                              
066998     .                                                                    
066999     EJECT                                                                
067000 IMS-GET-WDC901 SECTION.                                                  
067100                                                                          
067200     STRING 'WDC901  (WDC901KY =' W-WDC901KY-X ')'                        
067300             DELIMITED BY SIZE INTO SSA1                                  
067400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
067500     CALL CBLTDLI             USING GU                                    
067600                                    WDC9-PCB                              
067700                                    DLI-IO-WDC901                         
067800                                    SSA1                                  
067900     MOVE WDC9-STATUS-CODE       TO STATUS-WS                             
068000     PERFORM IMS-STATUSCHECK                                              
068100     .                                                                    
068200     EJECT                                                                
068300 IMS-GET-WDK601 SECTION.                                                  
068400                                                                          
068500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
068600             DELIMITED BY SIZE INTO SSA1                                  
068700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
068800     CALL CBLTDLI             USING GU                                    
068900                                    WDK6-PCB                              
069000                                    DLI-IO-WDK601                         
069100                                    SSA1                                  
069200     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
069300     PERFORM IMS-STATUSCHECK                                              
069400     .                                                                    
069500     EJECT                                                                
069600 IMS-GET-WDK611 SECTION.                                                  
069700                                                                          
069800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
069900             DELIMITED BY SIZE INTO SSA1                                  
070000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
070100             DELIMITED BY SIZE INTO SSA2                                  
070200     MOVE '  GE'                 TO GOOD-STATUSCODES                      
070300     CALL CBLTDLI             USING GU                                    
070400                                    WDK6-PCB                              
070500                                    DLI-IO-WDK611                         
070600                                    SSA1 SSA2                             
070700     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
070800     PERFORM IMS-STATUSCHECK                                              
070900     .                                                                    
071000     EJECT                                                                
071100 IMS-GET-WDK712 SECTION.                                                  
071200                                                                          
071300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
071400             DELIMITED BY SIZE INTO SSA1                                  
071500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
071600             DELIMITED BY SIZE INTO SSA2                                  
071710     MOVE '  GE'                 TO GOOD-STATUSCODES                      
071800     CALL CBLTDLI             USING GU                                    
071900                                    WDK7-PCB                              
072000                                    DLI-IO-WDK712                         
072100                                    SSA1 SSA2                             
072200     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
072300     PERFORM IMS-STATUSCHECK                                              
072400     .                                                                    
072500     EJECT                                                                
072600 IMS-GET-WDK722 SECTION.                                                  
072700                                                                          
072800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
072900             DELIMITED BY SIZE INTO SSA1                                  
073000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
073100             DELIMITED BY SIZE INTO SSA2                                  
073200     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
073300             DELIMITED BY SIZE INTO SSA3                                  
073400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
073500     CALL CBLTDLI             USING GU                                    
073600                                    WDK7-PCB                              
073700                                    DLI-IO-WDK722                         
073800                                    SSA1 SSA2 SSA3                        
073900     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
074000     PERFORM IMS-STATUSCHECK                                              
074100     .                                                                    
074200     EJECT                                                                
074300 IMS-GET-WDB601 SECTION.                                                  
074400                                                                          
074500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
074600             DELIMITED BY SIZE INTO SSA1                                  
074700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
074800     CALL CBLTDLI             USING GU                                    
074900                                    WDB6-PCB                              
075000                                    DLI-IO-WDB601                         
075100                                    SSA1                                  
075200     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
075300     PERFORM IMS-STATUSCHECK                                              
075400     .                                                                    
075500     EJECT                                                                
075510 IMS-GU-WDB601-MIN-MAX SECTION.                                           
075520                                                                          
075540     STRING 'WDB601  (IDDC    >=' W-IDDC-MIN-X                            
075550                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
075560             DELIMITED BY SIZE INTO SSA1                                  
075570     MOVE '  GE'                 TO GOOD-STATUSCODES                      
075580     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
075590     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
075591     PERFORM IMS-STATUSCHECK                                              
075592     .                                                                    
075593     EJECT                                                                
075600 IMS-GET-WDD201 SECTION.                                                  
075700                                                                          
075800     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
075900             DELIMITED BY SIZE INTO SSA1                                  
076000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
076100     CALL CBLTDLI             USING GU                                    
076200                                    WDD2-PCB                              
076300                                    DLI-IO-WDD201                         
076400                                    SSA1                                  
076500     MOVE WDD2-STATUS-CODE       TO STATUS-WS                             
076600     PERFORM IMS-STATUSCHECK                                              
076700     .                                                                    
076800     EJECT                                                                
076900 IMS-STATUSCHECK SECTION.                                                 
077000                                                                          
077100     SET STATUS-IX               TO 1                                     
077200     SEARCH GOOD-STATUS                                                   
077300       AT END                                                             
077400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
077500             DELIMITED BY SIZE INTO ERROR-TEXT                            
077600         CALL FELLOG                                                      
077700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
077800         CONTINUE                                                         
077900     END-SEARCH                                                           
078000     .                                                                    
