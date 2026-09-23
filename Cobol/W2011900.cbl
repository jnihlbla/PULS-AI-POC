000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2011900.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   16/08/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM IS USED TO HANDLE STEERING OF AUTOMATIC             
000900*        MAIL INFORMATION. THE SCREEN IS USED FOR BOTH DISPLAY            
001000*        AND UPDATE OF INFORMATION                                        
001100*                                                                         
001200*        THE PROGRAM UPDATES   WDF1                                       
001300*        THE PROGRAM READS     WDB6                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W2T119                                              
001700*        MID:         W2I11901                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W2O11901                                            
002010*                                                                         
002020***************************************************************           
002021* FIRST E'TRACKER 10217639, THAN MOVED TO E'TRACKER 10214419              
002022* 2016-09-28 ONLY SCREEN 2119 MOVED FROM 10217639.                        
002023***************************************************************           
002030*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W2011900'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  YES                         PIC X       VALUE 'J'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003410 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
003420 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
003460                                                                          
003500 77  WS-ERR-MSG-97               PIC X       VALUE SPACES.                
003600 77  WS-ERR-MSG-238              PIC X       VALUE SPACES.                
003700 77  WS-ERR-MSG-273              PIC X       VALUE SPACES.                
003900 77  WS-ERR-MSG-440              PIC X       VALUE SPACES.                
003910 77  WS-ERR-MSG-492              PIC X       VALUE SPACES.                
004000 77  WS-INPUT-ENTRD              PIC X       VALUE 'N'.                   
004100 77  WS-ROW-DEL                  PIC X       VALUE 'N'.                   
004200 77  WS-CMD-CNT                  PIC 9(2)    VALUE ZERO.                  
004201 77  WS-CMD-LINE                 PIC X       VALUE 'N'.                   
004210 77  WS-IDATTENT                 PIC X(2).                                
004220 01  WS-IDDC-KEY.                                                         
004230     03  FILLER                  PIC X(1)    VALUE SPACE.                 
004240     03  WS-IDDC-KEY-POS2        PIC X(1)    VALUE SPACE.                 
004300                                                                          
004310 01  WS-DC.                                                               
004320     03 WS-NDC-40                PIC 9(2)    VALUE 40.                    
004321     03 WS-NDC-41                PIC 9(2)    VALUE 41.                    
004330     03 WS-NDC-49                PIC 9(2)    VALUE 49.                    
004340     03 WS-NDC-70                PIC 9(2)    VALUE 70.                    
004341     03 WS-NDC-71                PIC 9(2)    VALUE 71.                    
004350     03 WS-NDC-79                PIC 9(2)    VALUE 79.                    
004351     03 WS-CDC-11                PIC 9(2)    VALUE 11.                    
004360                                                                          
004400*    --- INDEX FOR SCROLL LINES                                           
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  INDX1                       PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
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
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  OWN-MID                             VALUE '2119'.                
006000     88  GOOD-MID                            VALUE '2111' '2112'          
006100                                                   '2113' '2114'          
006200                                                   '2115' '2116'          
006300                                                   '2117' '2118'          
006400                                                   '2119'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006610 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
006620*01 -COPY WWIDFTG                                                         
006630     EJECT                                                                
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008300     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008600     03  ERR-MORE-THAN-ONE-FN    PIC X(3)    VALUE '097'.                 
008700     03  ERR-INVALID-COMBN       PIC X(3)    VALUE '238'.                 
008800     03  ERR-SUPPL-MISSING       PIC X(3)    VALUE '273'.                 
008900     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
009000     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
009010     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
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
010100     03  SAVE-IDTRANS            PIC X(4)  VALUE '2119'.                  
010200     03  SAVE-IDLEVNR-ENTER      PIC X(5)  VALUE SPACE.                   
010300     03  SAVE-IDLEVNR-NEXT       PIC X(5)  VALUE SPACE.                   
010400     03  SAVE-IDDC-KLEV-ENTER    PIC X(2)  VALUE SPACE.                   
010500     03  SAVE-IDDC-KLEV-NEXT     PIC X(2)  VALUE SPACE.                   
010600     03  SAVE-KDMAIL-ENTER       PIC X(4)  VALUE SPACE.                   
010700     03  SAVE-KDMAIL-NEXT        PIC X(4)  VALUE SPACE.                   
010800     03  SAVE-IDATTENT-ENTER     PIC S9(3) VALUE ZERO COMP-3.             
010900     03  SAVE-IDATTENT-NEXT      PIC S9(3) VALUE ZERO COMP-3.             
011000     EJECT                                                                
011100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011400     SKIP3                                                                
011500*01  MID -COPY W2I11901                                                   
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011800     SKIP3                                                                
011900*01  -COPY WMSGAREA                                                       
012000     EJECT                                                                
012100     03  MOD REDEFINES MSG-AREA.                                          
012200*      05  -COPY W2O11901                                                 
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012500     SKIP3                                                                
012600*01  -COPY WMFSAREA                                                       
012700     EJECT                                                                
012800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100     SKIP3                                                                
013200 01  KEYS-FOR-DLI.                                                        
013300     03  W-IDLEVNR-X.                                                     
013400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
013500                                                                          
013600     03  W-IDATTENT-X.                                                    
013700         05  W-IDATTENT          PIC S9(3)   VALUE ZERO COMP-3.           
013800                                                                          
013810     03  W-WDF1A1KY-MIN-X.                                                
013820         05  W-IDLEVNR-F1-MIN    PIC X(5)    VALUE SPACE.                 
013830         05  W-IDDC-F1-MIN.                                               
013831            07 FILLER            PIC  X(01).                              
013832            07 W-IDDC-F1-MIN-POS2 PIC  X(01).                             
013833         05  W-KDMAIL-F1-MIN     PIC X(4)    VALUE SPACE.                 
013834         05  W-IDATTENT-F1-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
013835                                                                          
013836     03  W-WDF1A1KY-MAX-X.                                                
013837         05  W-IDLEVNR-F1-MAX    PIC X(5)    VALUE SPACE.                 
013838         05  W-IDDC-F1-MAX.                                               
013839            07 FILLER            PIC  X(01).                              
013840            07 W-IDDC-F1-MAX-POS2 PIC  X(01).                             
013841         05  W-KDMAIL-F1-MAX     PIC X(4)    VALUE SPACE.                 
013842         05  W-IDATTENT-F1-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
013843                                                                          
013844     03  W-IDDC-KLEV-MIN-X.                                               
013845         05  FILLER              PIC X(1)    VALUE SPACE.                 
013846         05  W-IDDC-KLEV-MIN-POS2 PIC X(1)   VALUE SPACE.                 
013847                                                                          
013848     03  W-IDDC-KLEV-MAX-X.                                               
013849         05  FILLER              PIC X(1)    VALUE SPACE.                 
013850         05  W-IDDC-KLEV-MAX-POS2 PIC X(1)   VALUE SPACE.                 
013851                                                                          
013852     03  W-KDMAIL-MIN-X.                                                  
013853         05  W-KDMAIL-MIN        PIC X(4)    VALUE SPACE.                 
013860                                                                          
013861     03  W-KDMAIL-MAX-X.                                                  
013862         05  W-KDMAIL-MAX        PIC X(4)    VALUE SPACE.                 
013863                                                                          
014310     03  W-WDF122KY-X.                                                    
014320         05  W-IDDC-KLEV-F122    PIC X(2)    VALUE SPACE.                 
014340         05  W-KDMAIL-F122       PIC X(4)    VALUE SPACE.                 
014400     03  W-IDDC-B6-X.                                                     
014500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
014600     SKIP2                                                                
014700*    --- STATUS CODES FROM IMS                                            
014800 01  STATUS-WS                   PIC XX.                                  
014900     88  SEGMENT-FOUND                       VALUE '  '.                  
015000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015200     88  SEGMENT-END                         VALUE 'GB'.                  
015300     SKIP2                                                                
015400 01  GOOD-STATUSCODES.                                                    
015500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015600     SKIP3                                                                
015700 01  SSA1                        PIC X(128).                              
015800 01  SSA2                        PIC X(64).                               
015810 01  SSA3                        PIC X(64).                               
015900     EJECT                                                                
016000*    --- IMS FUNCTION CODES                                               
016100*01  -COPY W0003                                                          
016200     EJECT                                                                
016300*    ---  DLI INPUT-OUTPUT AREA                                           
016400                                                                          
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
016600 01  DLI-IO-WDF101.                                                       
016700*    03  -COPY WDF101                                                     
016800     EJECT                                                                
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF107'.                      
017000 01  DLI-IO-WDF107.                                                       
017100*    03  -COPY WDF107                                                     
017110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF122'.                      
017120 01  DLI-IO-WDF122.                                                       
017130*    03  -COPY WDF122                                                     
017200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF1A1'.                      
017300 01  DLI-IO-WDF1A1.                                                       
017400*    03  -COPY WDF1A1                                                     
017500     EJECT                                                                
017600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
017700 01  DLI-IO-WDB601.                                                       
017800*    03  -COPY WDB601                                                     
017900     EJECT                                                                
018000 LINKAGE SECTION.                                                         
018100*01  -COPY W0009  -PRE MSG-                                               
018200*01  -COPY W0008  -PRE WDP7-                                              
018300     05  FILLER                  PIC X.                                   
018400                                                                          
018500*01  -COPY W0008  -PRE WDF1-                                              
018600     05  FILLER                  PIC X.                                   
018700                                                                          
018800*01  -COPY W0008  -PRE WDF1A-                                             
018900     05  FILLER                  PIC X.                                   
019000                                                                          
019100*01  -COPY W0008  -PRE WDB6-                                              
019200     05  FILLER                  PIC X.                                   
019300     EJECT                                                                
019400 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDF1-PCB WDF1A-PCB            
019500                           WDB6-PCB.                                      
019600 MAIN SECTION.                                                            
019700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDF1-PCB WDF1A-PCB            
019800                           WDB6-PCB.                                      
019900                                                                          
020000     PERFORM IMS-GET-MSG                                                  
020100     IF SEGMENT-FOUND                                                     
020200       PERFORM A-INIT                                                     
020300       PERFORM B-CHECK-KEYS                                               
020400       IF KEYS-OK                                                         
020500         IF MFS-UPDATE                                                    
020600           PERFORM G-CHECK-INPUT                                          
020700           IF INDATA-OK                                                   
020800             PERFORM H-UPDATE                                             
020900           END-IF                                                         
021000         ELSE                                                             
021100           IF MFS-FIRST                                                   
021200             PERFORM C-FIRST-PAGE                                         
021300           ELSE                                                           
021400             IF MFS-NEXT                                                  
021500               PERFORM D-NEXT-PAGE                                        
021600             ELSE                                                         
021700               PERFORM E-SAME-PAGE                                        
021800             END-IF                                                       
021900           END-IF                                                         
022000         END-IF                                                           
022110         PERFORM F-READ-DC-INFO                                           
022200       END-IF                                                             
022300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O11901 + 4                      
022400       PERFORM IMS-INSERT-MSG                                             
022500     END-IF                                                               
022600                                                                          
022700     MOVE ZERO TO RETURN-CODE                                             
022800     GOBACK                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 A-INIT SECTION.                                                          
023200                                                                          
023300     IF MSG-DOUBLE-TRANSACTIONS                                           
023400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I11901                 
023500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
023600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023700     ELSE                                                                 
023800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I11901                  
023900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
024000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024100     END-IF                                                               
024200                                                                          
024300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
024500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024600                                                                          
024700     MOVE LOW-VALUE TO MSG-AREA                                           
024800     MOVE 'W2O119N1' TO MFS-IDMOD                                         
024900     MOVE '2119' TO MOD-IDTRANS                                           
025000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
025010     MOVE SPACE           TO MED-IDMFSFEL MED-IDMFSINF                    
025100                                                                          
025200     IF OWN-MID OR HELP-MID                                               
025300       CONTINUE                                                           
025400     ELSE                                                                 
025500       MOVE SPACE TO MFS-KDTRTYP                                          
025600       MOVE '7' TO MFS-IDPFK                                              
025700     END-IF                                                               
025710                                                                          
025720     MOVE LOW-VALUE         TO W-WDF1A1KY-MIN-X                           
025721                               W-IDDC-KLEV-MIN-X                          
025722                               W-KDMAIL-MIN-X                             
025723     MOVE HIGH-VALUE        TO W-WDF1A1KY-MAX-X                           
025724                               W-IDDC-KLEV-MAX-X                          
025725                               W-KDMAIL-MAX-X                             
025730                                                                          
025800     .                                                                    
025900     EJECT                                                                
026000 B-CHECK-KEYS SECTION.                                                    
026100                                                                          
026200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026300     MOVE '001'             TO MSGI-KDCALL                                
026400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026600     MOVE '2119'            TO MSGI-IDTRANS                               
026610     IF OWN-MID                                                           
026620       MOVE MID-IDLEVNR-START-IN   TO MSGI-IDLEVNR                        
026622       MOVE MID-IDDC-KLEV-START-IN TO MSGI-IDDC-KEY                       
026623       MOVE MID-KDMAIL-START-IN    TO MSGI-KDMAIL                         
026634     END-IF                                                               
026640                                                                          
026700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
026800     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
026900                                                                          
027000*    - LANGUAGE TO BE USED BY MEDKONV                                     
027100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
027200                                                                          
027300     MOVE YES TO KEYS-SW                                                  
027500                                                                          
027600*    -- CHECK OF IDLEVNR                                                  
027700     MOVE MFS-ERASE-FIELD TO MOD-IDLEVNR-START-IN                         
027800                                                                          
027900     IF MID-IDLEVNR-START-IN NOT = ALL '+'                                
027910       MOVE '7'                  TO MFS-IDPFK                             
027920       MOVE SPACE                TO MFS-KDTRTYP                           
027930     END-IF                                                               
028110     MOVE MSGI-IDLEVNR           TO W-IDLEVNR-F1-MIN                      
028120                                    MOD-IDLEVNR-START-UT                  
028700                                                                          
028800*    -- CHECK OF IDDC                                                     
028900     MOVE MFS-ERASE-FIELD TO MOD-IDDC-KLEV-START-IN                       
029000                                                                          
029010     IF MID-IDDC-KLEV-START-IN NOT = ALL '+'                              
029020       MOVE '7'                    TO MFS-IDPFK                           
029030       MOVE SPACE                  TO MFS-KDTRTYP                         
029040     END-IF                                                               
029060                                                                          
029073     IF MSGI-IDDC-KEY = SPACE                                             
029074        MOVE MSGI-IDFTG            TO WS-IDFTG                            
029075        IF IDFTG-US                                                       
029076           MOVE WS-NDC-41          TO W-IDDC-F1-MIN                       
029077           MOVE WS-NDC-49          TO W-IDDC-F1-MAX                       
029078           MOVE WS-NDC-40          TO MSGI-IDDC-KEY                       
029080        ELSE                                                              
029090           IF IDFTG-CN                                                    
029100              MOVE WS-NDC-71       TO W-IDDC-F1-MIN                       
029200              MOVE WS-NDC-79       TO W-IDDC-F1-MAX                       
029210              MOVE WS-NDC-70       TO MSGI-IDDC-KEY                       
029300           ELSE                                                           
029310              MOVE WS-CDC-11       TO W-IDDC-F1-MIN                       
029320                                      W-IDDC-F1-MAX                       
029330                                      MSGI-IDDC-KEY                       
029500           END-IF                                                         
029600        END-IF                                                            
029700     ELSE                                                                 
029800        MOVE MSGI-IDDC-KEY         TO W-IDDC-F1-MIN                       
029810                                      W-IDDC-F1-MAX                       
029820        IF MSGI-IDDC-KEY(2:1) = '0'                                       
029830           MOVE '1'                TO W-IDDC-F1-MIN-POS2                  
029840           MOVE '9'                TO W-IDDC-F1-MAX-POS2                  
029850        END-IF                                                            
029860     END-IF                                                               
029861     MOVE MSGI-IDDC-KEY            TO MOD-IDDC-KLEV-START-UT              
029862                                                                          
029865     MOVE MSGI-IDDC-KEY            TO W-IDDC-KLEV-MIN-X                   
029866                                      W-IDDC-KLEV-MAX-X                   
029867     IF MSGI-IDDC-KEY(2:1) = '0'                                          
029868        MOVE '1'                   TO W-IDDC-KLEV-MIN-POS2                
029869        MOVE '9'                   TO W-IDDC-KLEV-MAX-POS2                
029870     END-IF                                                               
029900                                                                          
030000*    -- CHECK OF KDMAIL                                                   
030010                                                                          
030011     MOVE MFS-ERASE-FIELD TO MOD-KDMAIL-START-IN                          
030020     IF MID-KDMAIL-START-IN NOT = ALL '+'                                 
030030       MOVE '7'                 TO MFS-IDPFK                              
030040       MOVE SPACE               TO MFS-KDTRTYP                            
030050     END-IF                                                               
030060                                                                          
030101     IF MSGI-KDMAIL NOT = SPACE                                           
030110        MOVE MSGI-KDMAIL        TO W-KDMAIL-F1-MIN                        
030111                                   W-KDMAIL-F1-MAX                        
030112                                   W-KDMAIL-MIN                           
030120                                   W-KDMAIL-MAX                           
030130     END-IF                                                               
030140     MOVE MSGI-KDMAIL           TO MOD-KDMAIL-START-UT                    
031200                                                                          
031314     IF OWN-MID OR KEYS-OK                                                
031315       CONTINUE                                                           
031400     ELSE                                                                 
031500       MOVE MFS-ERASE-FIELD TO MOD-IDLEVNR-START-UT                       
031600                               MOD-KDMAIL-START-UT                        
031700                               MOD-IDDC-KLEV-START-UT                     
031800     END-IF                                                               
031900                                                                          
032000     IF KEYS-WRONG                                                        
032010       IF MED-IDMFSFEL = SPACE                                            
032100         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
032110       END-IF                                                             
032200       CALL WMEDKONV USING MED-WMEDAREA                                   
032300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032400       PERFORM MFS-ERASE-FIELD-IN                                         
032500       PERFORM MFS-ERASE-FIELD-OUT                                        
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 C-FIRST-PAGE SECTION.                                                    
033000                                                                          
033100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
033200     CALL WMEDKONV USING MED-WMEDAREA                                     
033300     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
033400                                                                          
033500     PERFORM MFS-ERASE-FIELD-IN                                           
033600     .                                                                    
033700     EJECT                                                                
033800 D-NEXT-PAGE SECTION.                                                     
033810     MOVE 'D-NEXT-PAGE '  TO CURRENT-SECTION                              
033900                                                                          
034000     IF SAVE-IDTRANS = '2119'                                             
034100       MOVE SAVE-IDLEVNR-NEXT   TO W-IDLEVNR-F1-MIN                       
034200       MOVE SAVE-IDDC-KLEV-NEXT TO W-IDDC-F1-MIN                          
034300       MOVE SAVE-KDMAIL-NEXT    TO W-KDMAIL-F1-MIN                        
034400       MOVE SAVE-IDATTENT-NEXT  TO W-IDATTENT-F1-MIN                      
034401                                                                          
034410       MOVE SAVE-KDMAIL-NEXT    TO W-KDMAIL-MIN                           
034500     ELSE                                                                 
034600       PERFORM MFS-ERASE-FIELD-IN                                         
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 E-SAME-PAGE SECTION.                                                     
035010     MOVE 'E-SAME-PAGE '   TO CURRENT-SECTION                             
035100                                                                          
035200     IF SAVE-IDTRANS = '2119' OR '0551'                                   
035310       MOVE SAVE-IDLEVNR-ENTER   TO W-IDLEVNR-F1-MIN                      
035410       MOVE SAVE-IDDC-KLEV-ENTER TO W-IDDC-F1-MIN                         
035510       MOVE SAVE-KDMAIL-ENTER    TO W-KDMAIL-F1-MIN                       
035608       MOVE SAVE-IDATTENT-ENTER  TO W-IDATTENT-F1-MIN                     
035609                                                                          
035610       MOVE SAVE-KDMAIL-ENTER    TO W-KDMAIL-MIN                          
035620                                                                          
035622       MOVE +1                   TO INDX                                  
035623       PERFORM UNTIL INDX > MAX-INDX                                      
035624                  OR WS-CMD-LINE = YES                                    
035625         IF MID-KDCMD-IN(INDX) = ALL '+'                                  
035626            MOVE NOO             TO WS-CMD-LINE                           
035627         ELSE                                                             
035628          IF MID-IDLEVNR-INFO(INDX) = ALL '+'                             
035629          AND MID-KDCMD-IN(INDX) NOT = ALL '+'                            
035630            MOVE NOO             TO WS-CMD-LINE                           
035631          ELSE                                                            
035632            MOVE YES             TO WS-CMD-LINE                           
035633          END-IF                                                          
035634         END-IF                                                           
035635         ADD +1                  TO INDX                                  
035640       END-PERFORM                                                        
035700       IF WS-CMD-LINE = NOO AND MID-INPUT = ALL '+'                       
035800         PERFORM MFS-ERASE-FIELD-IN                                       
035900       ELSE                                                               
036000         MOVE INF-PRESS-PF11     TO MED-IDMFSINF                          
036100         CALL WMEDKONV        USING MED-WMEDAREA                          
036200         MOVE MED-MFSINF         TO MOD-TEMFSFEL                          
036300         PERFORM EA-MID-INDATA-FOR-MOD                                    
036400       END-IF                                                             
036500     ELSE                                                                 
036600       PERFORM MFS-ERASE-FIELD-IN                                         
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 EA-MID-INDATA-FOR-MOD SECTION.                                           
037010     MOVE 'EA-MID-INDATA-FOR-MOD '  TO CURRENT-SECTION                    
037100                                                                          
037200     MOVE +1                      TO INDX                                 
037300     PERFORM UNTIL INDX > MAX-INDX                                        
037400       IF MID-KDCMD-IN(INDX) NOT = ALL '+'                                
037500          MOVE MID-KDCMD-IN(INDX) TO MOD-KDCMD(INDX)                      
037600          MOVE MFS-ADD-READ-FIELD TO MOD-KDCMD-ATTR(INDX)                 
037700       ELSE                                                               
037800          MOVE MFS-ERASE-FIELD    TO MOD-KDCMD(INDX)                      
037900       END-IF                                                             
038000       ADD +1                     TO INDX                                 
038100     END-PERFORM                                                          
038200                                                                          
038300     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
038400        MOVE MID-IDLEVNR-IN      TO MOD-IDLEVNR-IN                        
038500        MOVE MFS-ADD-READ-FIELD  TO MOD-IDLEVNR-IN-ATTR                   
038600     ELSE                                                                 
038700        MOVE MFS-ERASE-FIELD     TO MOD-IDLEVNR-IN                        
038800     END-IF                                                               
038900                                                                          
039000     IF MID-IDATTENT-IN NOT = ALL '+'                                     
039100        MOVE MID-IDATTENT-IN     TO MOD-IDATTENT-IN                       
039200        MOVE MFS-ADD-READ-FIELD  TO MOD-IDATTENT-IN-ATTR                  
039300     ELSE                                                                 
039400        MOVE MFS-ERASE-FIELD     TO MOD-IDATTENT-IN                       
039500     END-IF                                                               
039600                                                                          
039700     IF MID-KDMAIL-IN NOT = ALL '+'                                       
039800        MOVE MID-KDMAIL-IN       TO MOD-KDMAIL-IN                         
039900        MOVE MFS-ADD-READ-FIELD  TO MOD-KDMAIL-IN-ATTR                    
040000     ELSE                                                                 
040100        MOVE MFS-ERASE-FIELD     TO MOD-KDMAIL-IN                         
040200     END-IF                                                               
040300                                                                          
040400     IF MID-IDDC-KLEV-IN NOT = ALL '+'                                    
040500        MOVE MID-IDDC-KLEV-IN    TO MOD-IDDC-KLEV-IN                      
040600        MOVE MFS-ADD-READ-FIELD  TO MOD-IDDC-KLEV-IN-ATTR                 
040700     ELSE                                                                 
040800        MOVE MFS-ERASE-FIELD     TO MOD-IDDC-KLEV-IN                      
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
058501 F-READ-DC-INFO  SECTION.                                                 
058504     MOVE ' F-READ-DC-INFO  '  TO CURRENT-SECTION                         
058515                                                                          
058516     PERFORM IMS-GU-WDF1A1                                                
058518     IF SEGMENT-MISSING                                                   
058519       MOVE ERR-SUPPL-MISSING    TO MED-IDMFSFEL                          
058520       CALL WMEDKONV          USING MED-WMEDAREA                          
058521       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
058522                                                                          
058523       PERFORM MFS-ERASE-FIELD-OUT                                        
058524       PERFORM MFS-STAENG-FAELT-IN                                        
058525     ELSE                                                                 
058526       MOVE +1                   TO INDX                                  
058527       MOVE SEQA-IDLEVNR         TO SAVE-IDLEVNR-ENTER                    
058530       MOVE SEQA-IDDC-KLEV       TO SAVE-IDDC-KLEV-ENTER                  
058531       MOVE SEQA-KDMAIL          TO SAVE-KDMAIL-ENTER                     
058532       MOVE SEQA-IDATTENT        TO SAVE-IDATTENT-ENTER                   
075597                                                                          
075598       PERFORM UNTIL SEGMENT-MISSING OR                                   
075599                     SEGMENT-END    OR                                    
075600                     INDX > MAX-INDX                                      
075601                                                                          
075604          PERFORM FA-MOVE-TO-MOD                                          
075606                                                                          
075607          PERFORM IMS-GN-WDF1A1                                           
075608          ADD +1  TO INDX                                                 
075609       END-PERFORM                                                        
075610                                                                          
075611       PERFORM UNTIL INDX > MAX-INDX                                      
075612         PERFORM MFS-ERASE-LINE-FIELD-OUT-ROW                             
075613         MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-KDCMD-ATTR(INDX)               
075614         ADD +1                     TO INDX                               
075615       END-PERFORM                                                        
075616                                                                          
075617                                                                          
075618       IF SEGMENT-FOUND                                                   
075619          PERFORM FB-SAVE-PF8-KEYS                                        
075620       ELSE                                                               
075621          IF MED-IDMFSINF = SPACE                                         
075622            MOVE INF-LAST-PAGE      TO MED-IDMFSINF                       
075623            CALL WMEDKONV        USING MED-WMEDAREA                       
075624            MOVE MED-MFSINF         TO MOD-TEMFSINF                       
075625          END-IF                                                          
075626          MOVE SAVE-IDLEVNR-ENTER   TO SAVE-IDLEVNR-NEXT                  
075629          MOVE SAVE-IDDC-KLEV-ENTER TO SAVE-IDDC-KLEV-NEXT                
075630          MOVE SAVE-KDMAIL-ENTER    TO SAVE-KDMAIL-NEXT                   
075631          MOVE SAVE-IDATTENT-ENTER  TO SAVE-IDATTENT-NEXT                 
075634       END-IF                                                             
075635                                                                          
075636       MOVE '002'      TO MSGI-KDCALL                                     
075637       MOVE '2119'     TO SAVE-IDTRANS                                    
075638       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
075639       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
075640     END-IF                                                               
075641                                                                          
075642     .                                                                    
075643     EJECT                                                                
075644 FA-MOVE-TO-MOD SECTION.                                                  
075645     MOVE 'FA-MOVE-TO-MOD '  TO CURRENT-SECTION                           
075646                                                                          
075647     MOVE SEQA-IDLEVNR         TO  W-IDLEVNR                              
075648     MOVE SEQA-IDATTENT        TO  W-IDATTENT                             
075649     PERFORM IMS-GU-WDF107                                                
075650     IF SEGMENT-FOUND                                                     
075651       MOVE ATT-IDATTENT       TO MOD-IDATTENT-UT(INDX)                   
075652       IF ATT-BELEV = SPACES                                              
075653          MOVE 'MISSING'       TO MOD-BELEV-UT(INDX)                      
075654       ELSE                                                               
075655          MOVE ATT-BELEV       TO MOD-BELEV-UT(INDX)                      
075656       END-IF                                                             
075657       MOVE SEQA-IDLEVNR       TO MOD-IDLEVNR-UT(INDX)                    
075658       MOVE SEQA-IDDC-KLEV     TO MOD-IDDC-KLEV-UT(INDX)                  
075659       MOVE SEQA-KDMAIL        TO MOD-KDMAIL-UT(INDX)                     
075660     END-IF                                                               
075661     .                                                                    
075662     EJECT                                                                
075663 FB-SAVE-PF8-KEYS SECTION.                                                
075664     MOVE 'FB-SAVE-PF8-KEYS '  TO CURRENT-SECTION                         
075665                                                                          
075666     MOVE SEQA-IDLEVNR         TO SAVE-IDLEVNR-NEXT                       
075667     MOVE SEQA-IDDC-KLEV       TO SAVE-IDDC-KLEV-NEXT                     
075668     MOVE SEQA-KDMAIL          TO SAVE-KDMAIL-NEXT                        
075669     MOVE SEQA-IDATTENT        TO SAVE-IDATTENT-NEXT                      
075670                                                                          
075671     IF MED-IDMFSINF = SPACE                                              
075672       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
075673       CALL WMEDKONV          USING MED-WMEDAREA                          
075674       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
075675     END-IF                                                               
075676     .                                                                    
075677     EJECT                                                                
075678 G-CHECK-INPUT SECTION.                                                   
075679     MOVE 'G-CHECK-INPUT '  TO CURRENT-SECTION                            
075680                                                                          
075681     MOVE YES  TO INDATA-SW                                               
075682     MOVE +1                     TO INDX                                  
075683     PERFORM UNTIL INDX > MAX-INDX                                        
075684                OR WS-CMD-LINE = YES                                      
075685       IF MID-KDCMD-IN(INDX) = ALL '+'                                    
075686          MOVE NOO               TO WS-CMD-LINE                           
075687       ELSE                                                               
075688          MOVE YES               TO WS-CMD-LINE                           
075689       END-IF                                                             
075690       ADD +1                    TO INDX                                  
075691     END-PERFORM                                                          
075692     IF WS-CMD-LINE = NOO AND MID-INPUT = ALL '+'                         
075693       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
075694       CALL WMEDKONV USING MED-WMEDAREA                                   
075695       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
075696       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
075697       MOVE NOO TO INDATA-SW                                              
075698     ELSE                                                                 
075699                                                                          
075700       PERFORM GA-CHECK-KDCMD                                             
075701       PERFORM GB-CHECK-INPUT                                             
075702                                                                          
075703       IF WS-CMD-CNT = 1 AND WS-INPUT-ENTRD = YES                         
075704          MOVE NOO         TO INDATA-SW                                   
075705          MOVE YES         TO WS-ERR-MSG-97                               
075706       END-IF                                                             
075707       IF INDATA-WRONG                                                    
075708         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
075709         PERFORM GC-MOVE-ERR-MSG                                          
075710         CALL WMEDKONV USING MED-WMEDAREA                                 
075711         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
075712         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
075713       END-IF                                                             
075714     END-IF                                                               
075715     .                                                                    
075716     EJECT                                                                
075717 GA-CHECK-KDCMD SECTION.                                                  
075718     MOVE 'GA-CHECK-KDCMD '  TO CURRENT-SECTION                           
075719                                                                          
075720     MOVE +1                            TO INDX                           
075721     PERFORM UNTIL INDX > MAX-INDX                                        
075722       IF  MID-KDCMD-IN(INDX) NOT = ALL '+'                               
075723       AND MID-KDCMD-IN(INDX) NOT = SPACES                                
075724          ADD +1                           TO WS-CMD-CNT                  
075725          IF MID-KDCMD-IN(INDX) = 'B' OR 'D'                              
075726            MOVE MID-IDDC-KLEV-INFO(INDX) TO W-IDDC-B6                    
075727            PERFORM IMS-GU-WDB601                                         
075728            IF SEGMENT-FOUND                                              
075729              IF (MSGI-IDDC  = DCS-IDDC AND                               
075730                  MSGI-IDFTG = DCS-IDFTG)                                 
075731              OR MSGI-IDFTG  = +57                                        
075732                MOVE MFS-ALPHA-FIELD-OK   TO MOD-KDCMD-ATTR(INDX)         
075733                MOVE YES                  TO INDATA-SW                    
075734              ELSE                                                        
075735                MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDCMD-ATTR(INDX)        
075736                MOVE NOO                   TO INDATA-SW                   
075737              END-IF                                                      
075738            ELSE                                                          
075739              MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDCMD-ATTR(INDX)          
075740              MOVE NOO                   TO INDATA-SW                     
075741            END-IF                                                        
075742          ELSE                                                            
075743            MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDCMD-ATTR(INDX)            
075744            MOVE NOO                   TO INDATA-SW                       
075745          END-IF                                                          
075746       END-IF                                                             
075747       ADD +1                             TO INDX                         
075748     END-PERFORM                                                          
075749                                                                          
075750     IF WS-CMD-CNT > 1                                                    
075751        MOVE NOO                        TO INDATA-SW                      
075752        MOVE YES                        TO WS-ERR-MSG-238                 
075753        MOVE +1                         TO INDX                           
075754        PERFORM UNTIL INDX > MAX-INDX                                     
075755          IF MID-KDCMD-IN(INDX) NOT = ALL '+'                             
075756             MOVE MFS-ALPHA-FIELD-WRONG TO                                
075757                                      MOD-KDCMD-ATTR(INDX)                
075758          END-IF                                                          
075759          ADD +1                        TO INDX                           
075760        END-PERFORM                                                       
075761     END-IF                                                               
075762     .                                                                    
075763     EJECT                                                                
075764 GB-CHECK-INPUT SECTION.                                                  
075765     MOVE 'GB-CHECK-INPUT '  TO CURRENT-SECTION                           
075766                                                                          
075767     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
075768     OR MID-KDMAIL-IN NOT = ALL '+'                                       
075769     OR MID-IDDC-KLEV-IN NOT = ALL '+'                                    
075770     OR MID-IDATTENT-IN NOT = ALL '+'                                     
075771        MOVE YES                      TO WS-INPUT-ENTRD                   
075772     END-IF                                                               
075773                                                                          
075774     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
075775       MOVE MID-IDLEVNR-IN            TO W-IDLEVNR                        
075776       PERFORM IMS-GU-WDF101                                              
075777       IF SEGMENT-MISSING                                                 
075778         MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-IDLEVNR-IN-ATTR              
075779         MOVE NOO                     TO INDATA-SW                        
075780         MOVE YES                     TO WS-ERR-MSG-273                   
075781       ELSE                                                               
075782         MOVE MFS-ALPHA-FIELD-OK      TO MOD-IDLEVNR-IN-ATTR              
075783       END-IF                                                             
075784     ELSE                                                                 
075785       IF WS-INPUT-ENTRD = YES                                            
075786         MOVE NOO                     TO INDATA-SW                        
075787         MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-IDLEVNR-IN-ATTR              
075788       END-IF                                                             
075789     END-IF                                                               
075790                                                                          
075791     IF MID-KDMAIL-IN NOT = ALL '+'                                       
075792       IF MID-KDMAIL-IN  = 'PADE'                                         
075793          MOVE MFS-ALPHA-FIELD-OK     TO MOD-KDMAIL-IN-ATTR               
075794       ELSE                                                               
075795         MOVE NOO                     TO INDATA-SW                        
075796         MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDMAIL-IN-ATTR               
075797         MOVE YES                     TO WS-ERR-MSG-492                   
075798       END-IF                                                             
075799     ELSE                                                                 
075800       IF WS-INPUT-ENTRD = YES                                            
075801          MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDMAIL-IN-ATTR               
075802          MOVE NOO                    TO INDATA-SW                        
075803       END-IF                                                             
075804     END-IF                                                               
075805                                                                          
075806     IF MID-IDDC-KLEV-IN NOT = ALL '+'                                    
075807        MOVE MID-IDDC-KLEV-IN         TO W-IDDC-B6                        
075808        PERFORM IMS-GU-WDB601                                             
075809        IF SEGMENT-FOUND                                                  
075810           IF (MSGI-IDDC(1:1) = DCS-IDDC(1:1) AND                         
075811               MSGI-IDFTG     = DCS-IDFTG)                                
075812           OR  MSGI-IDFTG     = +57                                       
075813             MOVE MFS-ALPHA-FIELD-OK  TO MOD-IDDC-KLEV-IN-ATTR            
075814           ELSE                                                           
075815             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-KLEV-IN-ATTR          
075816             MOVE NOO                 TO INDATA-SW                        
075817             MOVE YES                 TO WS-ERR-MSG-440                   
075818           END-IF                                                         
075819        ELSE                                                              
075820           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-KLEV-IN-ATTR            
075821           MOVE NOO                   TO INDATA-SW                        
075822           MOVE YES                   TO WS-ERR-MSG-440                   
075823        END-IF                                                            
075824     ELSE                                                                 
075825       IF WS-INPUT-ENTRD = YES                                            
075826         MOVE NOO                     TO INDATA-SW                        
075827         MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-IDDC-KLEV-IN-ATTR            
075828       END-IF                                                             
075829     END-IF                                                               
075830                                                                          
075831     IF MID-IDATTENT-IN NOT = ALL '+'                                     
075832        MOVE MID-IDATTENT-IN          TO WS-IDATTENT                      
075833        INSPECT WS-IDATTENT REPLACING LEADING SPACES BY ZERO              
075834        IF WS-IDATTENT NOT NUMERIC                                        
075835          MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDATTENT-IN-ATTR             
075836          MOVE NOO                    TO INDATA-SW                        
075837        ELSE                                                              
075838          MOVE MFS-NUM-FIELD-OK       TO MOD-IDATTENT-IN-ATTR             
075839          MOVE MID-IDATTENT-IN        TO W-IDATTENT                       
075840          PERFORM IMS-GU-WDF107                                           
075841          IF SEGMENT-FOUND                                                
075842             CONTINUE                                                     
075843          ELSE                                                            
075844             MOVE NOO                 TO INDATA-SW                        
075845             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDATTENT-IN-ATTR             
075846             MOVE YES                 TO WS-ERR-MSG-492                   
075847          END-IF                                                          
075848        END-IF                                                            
075849     ELSE                                                                 
075850        IF WS-INPUT-ENTRD = YES                                           
075851          MOVE NOO                    TO INDATA-SW                        
075852          MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDATTENT-IN-ATTR             
075853        END-IF                                                            
075854     END-IF                                                               
075855                                                                          
075856     .                                                                    
075857     EJECT                                                                
075858 GC-MOVE-ERR-MSG SECTION.                                                 
075859     MOVE 'GC-MOVE-ERR-MSG '  TO CURRENT-SECTION                          
075860                                                                          
075861     EVALUATE TRUE                                                        
075862       WHEN WS-ERR-MSG-238 = YES                                          
075863            MOVE ERR-INVALID-COMBN       TO MED-IDMFSFEL                  
075864       WHEN WS-ERR-MSG-97 = YES                                           
075865            MOVE ERR-MORE-THAN-ONE-FN    TO MED-IDMFSFEL                  
075866       WHEN WS-ERR-MSG-273 = YES                                          
075867            MOVE ERR-SUPPL-MISSING       TO MED-IDMFSFEL                  
075868       WHEN WS-ERR-MSG-492 = YES                                          
075869            MOVE ERR-INVALID-VALUE       TO MED-IDMFSFEL                  
075870       WHEN WS-ERR-MSG-440 = YES                                          
075871            MOVE ERR-WRONG-DC            TO MED-IDMFSFEL                  
075872     END-EVALUATE                                                         
075873     .                                                                    
075874     EJECT                                                                
075875 H-UPDATE SECTION.                                                        
075876     MOVE 'H-UPDATE '  TO CURRENT-SECTION                                 
075877                                                                          
075878     IF WS-CMD-CNT = 1                                                    
075879        PERFORM HA-DELETE-ROW                                             
075880     ELSE                                                                 
075881        PERFORM HB-ADD-ROW                                                
075882     END-IF                                                               
075883                                                                          
075884     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
075885     CALL WMEDKONV     USING MED-WMEDAREA                                 
075886     MOVE MED-MFSINF      TO MOD-TEMFSINF                                 
075887     PERFORM MFS-FORM-ATTR                                                
075888     PERFORM MFS-ERASE-FIELD-IN                                           
075889     .                                                                    
075890     EJECT                                                                
075891 HA-DELETE-ROW SECTION.                                                   
075892     MOVE 'HA-DELETE-ROW '  TO CURRENT-SECTION                            
075893                                                                          
075894     MOVE +1      TO INDX                                                 
075895     PERFORM UNTIL INDX > MAX-INDX OR WS-ROW-DEL = YES                    
075896       IF MID-KDCMD-IN(INDX) NOT = ALL '+'                                
075897          MOVE MID-IDLEVNR-INFO(INDX)   TO W-IDLEVNR                      
075898          MOVE MID-IDATTENT-INFO(INDX)  TO W-IDATTENT                     
075899          MOVE MID-IDDC-KLEV-INFO(INDX) TO W-IDDC-KLEV-F122               
075900          MOVE MID-KDMAIL-INFO(INDX)    TO W-KDMAIL-F122                  
075901          MOVE YES                      TO WS-ROW-DEL                     
075902       END-IF                                                             
075903       ADD +1                           TO INDX                           
075904     END-PERFORM                                                          
075905                                                                          
075906     PERFORM IMS-GHU-WDF107                                               
075907     IF SEGMENT-FOUND                                                     
075908        PERFORM IMS-GHNP-WDF122                                           
075909        IF SEGMENT-FOUND                                                  
075910          PERFORM IMS-DLET-WDF122                                         
075911        END-IF                                                            
075912     END-IF                                                               
075913     .                                                                    
075914     EJECT                                                                
075915 HB-ADD-ROW SECTION.                                                      
075916     MOVE 'HB-ADD-ROW '  TO CURRENT-SECTION                               
075917                                                                          
075918     MOVE MID-IDLEVNR-IN        TO W-IDLEVNR                              
075919     MOVE MID-IDATTENT-IN       TO W-IDATTENT                             
075920     PERFORM IMS-GHU-WDF107                                               
075921     IF SEGMENT-FOUND                                                     
075922       MOVE MID-IDDC-KLEV-IN    TO W-IDDC-KLEV-F122                       
075923       MOVE MID-KDMAIL-IN       TO W-KDMAIL-F122                          
075924       PERFORM IMS-GHNP-WDF122                                            
075925       IF SEGMENT-FOUND                                                   
075926          CONTINUE                                                        
075927       ELSE                                                               
075928          MOVE MID-IDLEVNR-IN   TO MAIL-IDLEVNR                           
075929          MOVE MID-IDDC-KLEV-IN TO MAIL-IDDC-KLEV                         
075930          MOVE MID-KDMAIL-IN    TO MAIL-KDMAIL                            
075931          PERFORM IMS-ISRT-WDF122                                         
075932       END-IF                                                             
075933                                                                          
075934       IF MID-IDDC-KLEV-IN(1:2) NOT = W-IDDC-F1-MIN(1:2)                  
075935          MOVE MID-IDDC-KLEV-IN  TO W-IDDC-KLEV-MIN-X                     
075936                                    W-IDDC-KLEV-MAX-X                     
075937          MOVE MID-IDDC-KLEV-IN  TO WS-IDDC-KEY                           
075938                                                                          
075939          MOVE ALL '+'           TO MSGI-WMSGINIT                         
075940          MOVE '001'             TO MSGI-KDCALL                           
075941          MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                     
075942          MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                           
075950          MOVE '2119'            TO MSGI-IDTRANS                          
075960          MOVE WS-IDDC-KEY       TO MSGI-IDDC-KEY                         
076600                                                                          
076700          CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                      
076800          MOVE MSGI-SPAR-AREA TO SAVE-AREA                                
076900                                                                          
077000          MOVE MSGI-IDDC-KEY     TO MOD-IDDC-KLEV-START-UT                
081853       END-IF                                                             
081854                                                                          
081857       MOVE MID-IDLEVNR-IN       TO W-IDLEVNR-F1-MIN                      
081858       MOVE MID-IDDC-KLEV-IN     TO W-IDDC-F1-MIN                         
081859       MOVE MID-KDMAIL-IN        TO W-KDMAIL-F1-MIN                       
081860       MOVE MID-IDATTENT-IN      TO W-IDATTENT-F1-MIN                     
081861       MOVE HIGH-VALUE           TO W-WDF1A1KY-MAX-X                      
081862                                                                          
081865       MOVE MID-KDMAIL-IN        TO W-KDMAIL-MIN                          
081866                                    W-KDMAIL-MAX                          
081867                                                                          
081868     END-IF                                                               
081869     .                                                                    
081870     EJECT                                                                
081871                                                                          
081872 MFS-ERASE-FIELD-OUT SECTION.                                             
081873                                                                          
081874*    --- ALLA UTDATA-FÄLT                                                 
081875*    --- INCL. SCROLL KEYS                                                
081876                                                                          
081877     PERFORM MFS-ERASE-LINE-FIELD-OUT                                     
081878     .                                                                    
081879     SKIP3                                                                
081880 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
081881                                                                          
081882*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
081883     MOVE +1                 TO INDX                                      
081884     PERFORM UNTIL INDX > MAX-INDX                                        
081885       MOVE MFS-ERASE-FIELD  TO MOD-IDLEVNR-UT(INDX)                      
081886                                MOD-IDATTENT-UT(INDX)                     
081887                                MOD-BELEV-UT(INDX)                        
081888                                MOD-KDMAIL-UT(INDX)                       
081889                                MOD-IDDC-KLEV-UT(INDX)                    
081890                                MOD-KDCMD(INDX)                           
081891       ADD +1                TO INDX                                      
081892     END-PERFORM                                                          
081893     .                                                                    
081894     SKIP3                                                                
081895 MFS-ERASE-LINE-FIELD-OUT-ROW SECTION.                                    
081896                                                                          
081897*    --- OUTDATA-FIELD ON OUTPUT ROW                                      
081898     MOVE MFS-ERASE-FIELD  TO MOD-IDLEVNR-UT(INDX)                        
081899                              MOD-IDATTENT-UT(INDX)                       
081900                              MOD-BELEV-UT(INDX)                          
081901                              MOD-KDMAIL-UT(INDX)                         
081902                              MOD-IDDC-KLEV-UT(INDX)                      
081903                              MOD-KDCMD(INDX)                             
081904     .                                                                    
081905     SKIP3                                                                
081906 MFS-ERASE-FIELD-IN SECTION.                                              
081907                                                                          
081908     MOVE +1                 TO INDX                                      
081909     PERFORM UNTIL INDX > MAX-INDX                                        
081910       MOVE MFS-ERASE-FIELD  TO MOD-KDCMD(INDX)                           
081911       ADD +1                TO INDX                                      
081912     END-PERFORM                                                          
081913                                                                          
081914     MOVE MFS-ERASE-FIELD    TO MOD-IDLEVNR-IN                            
081915                                MOD-IDATTENT-IN                           
081916                                MOD-KDMAIL-IN                             
081917                                MOD-IDDC-KLEV-IN                          
081918     .                                                                    
081919     EJECT                                                                
081920 MFS-STAENG-FAELT-IN SECTION.                                             
081921*    --- ALLA INDATA-FÄLT                                                 
081922                                                                          
081923     MOVE +1                       TO INDX                                
081924     PERFORM UNTIL INDX > MAX-INDX                                        
081925       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDCMD-ATTR(INDX)                
081926       ADD +1                      TO INDX                                
081927     END-PERFORM                                                          
081928     .                                                                    
081929     SKIP3                                                                
081930 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
081931                                                                          
081932     MOVE +1                       TO INDX                                
081933     PERFORM UNTIL INDX > MAX-INDX                                        
081934       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD(INDX)                     
081935       ADD +1                      TO INDX                                
081936     END-PERFORM                                                          
081937                                                                          
081938     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDLEVNR-IN                      
081939                                      MOD-IDATTENT-IN                     
081940                                      MOD-KDMAIL-IN                       
081941                                      MOD-IDDC-KLEV-IN                    
081942     .                                                                    
081943     EJECT                                                                
081944 MFS-FORM-ATTR SECTION.                                                   
081945                                                                          
081946     MOVE +1                        TO INDX                               
081947     PERFORM UNTIL INDX > MAX-INDX                                        
081948       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDCMD-ATTR(INDX)               
081949       ADD +1                       TO INDX                               
081950     END-PERFORM                                                          
081951                                                                          
081952     MOVE MFS-FORMAT-DEFAULT-ATTR   TO MOD-IDLEVNR-IN-ATTR                
081953                                       MOD-IDATTENT-IN-ATTR               
081954                                       MOD-KDMAIL-IN-ATTR                 
081955                                       MOD-IDDC-KLEV-IN-ATTR              
081956     .                                                                    
081957     SKIP2                                                                
081958 MFS-READ-IN-AGAIN SECTION.                                               
081959                                                                          
081960     MOVE +1                       TO INDX                                
081961     PERFORM UNTIL INDX > MAX-INDX                                        
081962       MOVE MFS-ADD-READ-FIELD     TO MOD-KDCMD-ATTR(INDX)                
081963       ADD +1                      TO INDX                                
081964     END-PERFORM                                                          
081965                                                                          
081966     MOVE MFS-ADD-READ-FIELD      TO MOD-IDLEVNR-IN-ATTR                  
081967                                     MOD-IDATTENT-IN-ATTR                 
081968                                     MOD-KDMAIL-IN-ATTR                   
081969                                     MOD-IDDC-KLEV-IN-ATTR                
081970                                                                          
081971     .                                                                    
081972     EJECT                                                                
081973* --- IMS SECTIONS ---                                                    
081974     SKIP3                                                                
081975 IMS-GET-MSG SECTION.                                                     
081976                                                                          
081977     MOVE '  QC' TO GOOD-STATUSCODES                                      
081978     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
081979     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081980     PERFORM IMS-STATUSCHECK                                              
081981     .                                                                    
081982     SKIP3                                                                
081983 IMS-INSERT-MSG SECTION.                                                  
081984                                                                          
081985     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
081986     MOVE SPACE TO GOOD-STATUSCODES                                       
081987     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
081988     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081989     PERFORM IMS-STATUSCHECK                                              
081990     .                                                                    
081991     EJECT                                                                
081992 IMS-GU-WDF107 SECTION.                                                   
081993     MOVE 'IMS-GU-WDF107      '  TO DBS-SECTION                           
081994                                                                          
081995     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
081996          DELIMITED BY SIZE INTO SSA1                                     
081997     STRING 'WDF107  (IDATTENT =' W-IDATTENT-X ')'                        
081998          DELIMITED BY SIZE INTO SSA2                                     
081999     MOVE '  GE' TO GOOD-STATUSCODES                                      
082000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF107 SSA1 SSA2               
082001     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
082002     PERFORM IMS-STATUSCHECK                                              
082003     .                                                                    
082010     EJECT                                                                
082030 IMS-GU-WDF1A1    SECTION.                                                
082031     MOVE 'IMS-GU-WDF1A1    '  TO DBS-SECTION                             
082050                                                                          
082051     STRING 'WDF1A1  (WDF1A1KY>=' W-WDF1A1KY-MIN-X                        
082052                    '&WDF1A1KY<=' W-WDF1A1KY-MAX-X                        
082053                    '&IDDCKLEV>=' W-IDDC-KLEV-MIN-X                       
082054                    '&IDDCKLEV<=' W-IDDC-KLEV-MAX-X                       
082055                    '&KDMAIL  >=' W-KDMAIL-MIN-X                          
082056                    '&KDMAIL  <=' W-KDMAIL-MAX-X ')'                      
082057          DELIMITED BY SIZE INTO SSA1                                     
082058     MOVE '  GE' TO GOOD-STATUSCODES                                      
082059     CALL CBLTDLI USING GU WDF1A-PCB DLI-IO-WDF1A1 SSA1                   
082060     MOVE WDF1A-STATUS-CODE TO STATUS-WS                                  
082061     PERFORM IMS-STATUSCHECK                                              
082062     .                                                                    
082063     SKIP3                                                                
082064 IMS-GN-WDF1A1    SECTION.                                                
082065     MOVE 'IMS-GN-WDF1A1    '  TO DBS-SECTION                             
082079                                                                          
082080     STRING 'WDF1A1  (WDF1A1KY>=' W-WDF1A1KY-MIN-X                        
082081                    '&WDF1A1KY<=' W-WDF1A1KY-MAX-X                        
082082                    '&IDDCKLEV>=' W-IDDC-KLEV-MIN-X                       
082083                    '&IDDCKLEV<=' W-IDDC-KLEV-MAX-X                       
082084                    '&KDMAIL  >=' W-KDMAIL-MIN-X                          
082085                    '&KDMAIL  <=' W-KDMAIL-MAX-X ')'                      
082086          DELIMITED BY SIZE INTO SSA1                                     
082087     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
082088     CALL CBLTDLI USING GN WDF1A-PCB DLI-IO-WDF1A1 SSA1                   
082089     MOVE WDF1A-STATUS-CODE TO STATUS-WS                                  
082090     PERFORM IMS-STATUSCHECK                                              
082091     .                                                                    
082100     SKIP3                                                                
082132 IMS-GU-WDF101 SECTION.                                                   
082133                                                                          
082140     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
082200          DELIMITED BY SIZE INTO SSA1                                     
082300     MOVE '  GE' TO GOOD-STATUSCODES                                      
082400     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
082500     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
082600     PERFORM IMS-STATUSCHECK                                              
082700     .                                                                    
082800     EJECT                                                                
083810 IMS-GHU-WDF107 SECTION.                                                  
083820                                                                          
083830     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
083840          DELIMITED BY SIZE INTO SSA1                                     
083841     STRING 'WDF107  (IDATTENT =' W-IDATTENT-X ')'                        
083842          DELIMITED BY SIZE INTO SSA2                                     
083850     MOVE '  GE' TO GOOD-STATUSCODES                                      
083860     CALL CBLTDLI USING GHU WDF1-PCB DLI-IO-WDF107 SSA1 SSA2              
083870     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
083880     PERFORM IMS-STATUSCHECK                                              
083890     .                                                                    
083891     EJECT                                                                
084810 IMS-GHNP-WDF122 SECTION.                                                 
084820                                                                          
084830     STRING 'WDF122  (WDF122KY =' W-WDF122KY-X ')'                        
084840          DELIMITED BY SIZE INTO SSA1                                     
084850     MOVE '  GE' TO GOOD-STATUSCODES                                      
084860     CALL CBLTDLI USING GHNP WDF1-PCB DLI-IO-WDF122 SSA1                  
084870     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
084880     PERFORM IMS-STATUSCHECK                                              
084890     .                                                                    
084891     SKIP3                                                                
084900 IMS-ISRT-WDF122 SECTION.                                                 
085000                                                                          
085001     MOVE SPACES           TO SSA1                                        
085002                              SSA2                                        
085003                              SSA3                                        
085004     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
085005          DELIMITED BY SIZE INTO SSA1                                     
085010     STRING 'WDF107  (IDATTENT =' W-IDATTENT-X ')'                        
085020          DELIMITED BY SIZE INTO SSA2                                     
085030     STRING 'WDF122  '                                                    
085040          DELIMITED BY SIZE INTO SSA3                                     
085100     MOVE '  II'           TO GOOD-STATUSCODES                            
085200     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF122 SSA1 SSA2 SSA3        
085300     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
085400     PERFORM IMS-STATUSCHECK                                              
085500     .                                                                    
085600     EJECT                                                                
085700 IMS-DLET-WDF122 SECTION.                                                 
085800                                                                          
085900     MOVE '  ' TO GOOD-STATUSCODES                                        
086000     CALL CBLTDLI USING DLET WDF1-PCB DLI-IO-WDF122                       
086100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
086200     PERFORM IMS-STATUSCHECK                                              
086300     .                                                                    
086400     EJECT                                                                
086500 IMS-GU-WDB601 SECTION.                                                   
086600                                                                          
086700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
086800          DELIMITED BY SIZE INTO SSA1                                     
086900     MOVE '  GE' TO GOOD-STATUSCODES                                      
087000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
087100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
087200     PERFORM IMS-STATUSCHECK                                              
087300     .                                                                    
087400     EJECT                                                                
087500 IMS-STATUSCHECK SECTION.                                                 
087600                                                                          
087700     SET STATUS-IX TO 1                                                   
087800     SEARCH GOOD-STATUS                                                   
087900       AT END                                                             
088000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
088100         DELIMITED BY SIZE INTO ERROR-TEXT                                
088200         CALL FELLOG                                                      
088300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
088400         CONTINUE                                                         
088500     END-SEARCH                                                           
088600     .                                                                    
