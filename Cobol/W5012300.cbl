000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5012300.                                                
000300 AUTHOR.         KUMAR LOVISH.                                            
000400 DATE-WRITTEN.   17/07/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PROG FOR SCREEN 5123 TO GET CURRENCY INFORMATION                 
000900*                                                                         
001000*        THE PROGRAM READS     WDB6                                       
001100*        THE PROGRAM READS     WDG2                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W5T123                                              
001500*        MID:         W5I12301                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W5O12301                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W5012300'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  YES                         PIC X       VALUE 'J'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300                                                                          
003400*    --- INDEX FOR SCROLL LINES                                           
003500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003600 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
003700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003800                                                                          
003900                                                                          
004000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004100     88  KEYS-OK                             VALUE 'J'.                   
004200     88  KEYS-WRONG                          VALUE 'N'.                   
004201                                                                          
004210 77  WS-FIRST-SW                 PIC X       VALUE 'J'.                   
004220     88  FIRST-PAGE                          VALUE 'J'.                   
004230     88  NOT-FIRST-PAGE                      VALUE 'N'.                   
004300                                                                          
004310 77  WS-F7-SW                    PIC X       VALUE 'J'.                   
004320     88  WS-F7                               VALUE 'J'.                   
004330     88  WS-NOT-F7                           VALUE 'N'.                   
004340                                                                          
004400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004500     88  OWN-MID                             VALUE '5123'.                
004600     88  GOOD-MID                            VALUE '5121' '5122'          
004700                                                   '5123' '5124'          
004800                                                   '5125' '5126'          
004900                                                   '5127' '5128'          
005000                                                   '5129'.                
005100     88  HELP-MID                            VALUE '0551'.                
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     EJECT                                                                
006000*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006100*01 -COPY WMEDAREA                                                        
006200     SKIP3                                                                
006300 01  MESSAGE-CODES.                                                       
006400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006700     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
006800     03  INF-DATA-MISSING        PIC X(3)    VALUE '010'.                 
006900     EJECT                                                                
007000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007300     SKIP3                                                                
007400*01 -COPY WMSGINIT                                                        
007500     EJECT                                                                
007600*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
007700*                                                                         
007800 01  SAVE-AREA.                                                           
007900     03  SAVE-IDTRANS           PIC X(4)    VALUE '5123'.                 
008000     03  SAVE-KDVALISO-FIRST       PIC X(3) VALUE SPACES.                 
008010     03  SAVE-KDVALISO-ENTER       PIC X(3) VALUE SPACES.                 
008100     03  SAVE-KDVALISO-NEXT        PIC X(3) VALUE SPACES.                 
008110     03  SAVE-KDVALTYP-FIRST       PIC X    VALUE SPACES.                 
008111     03  SAVE-KDVALTYP-ENTER       PIC X    VALUE SPACES.                 
008120     03  SAVE-KDVALTYP-NEXT        PIC X    VALUE SPACES.                 
008200     03  SAVE-TISTADA9-FIRST       PIC S9(7)   VALUE ZERO COMP-3.         
008210     03  SAVE-TISTADA9-ENTER       PIC S9(7)   VALUE ZERO COMP-3.         
008300     03  SAVE-TISTADA9-NEXT        PIC S9(7)   VALUE ZERO COMP-3.         
008310     03  SAVE-KDVALTYP-Y           PIC X    VALUE 'N'.                    
008400     EJECT                                                                
008500*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008800     SKIP3                                                                
008900*01  MID -COPY W5I12301                                                   
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009200     SKIP3                                                                
009300*01  -COPY WMSGAREA                                                       
009400     EJECT                                                                
009500     03  MOD REDEFINES MSG-AREA.                                          
009600*      05  -COPY W5O12301                                                 
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009900     SKIP3                                                                
010000*01  -COPY WMFSAREA                                                       
010100     EJECT                                                                
010200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010300*                                                                         
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  KEYS-FOR-DLI.                                                        
010700*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
010800*    03  W-KDVALISO-MIN-X.                                                
010900**       05  W-KDVALISO-MIN     PIC X(3).                                 
011000                                                                          
011100     03  W-TISTADAT-X.                                                    
011200         05  W-TISTADAT     PIC 9(6).                                     
011300                                                                          
011400     03  W-IDDC-X.                                                        
011500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011600     03  W-WDGXKEY-X.                                                     
011700         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
011800         05  W-KDVALISO-HUV      PIC X(3)    VALUE LOW-VALUE.             
011810         05  W-KDVALTYP          PIC X(1)    VALUE SPACE.                 
011900         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
012000     03  W-KDVALISO-X.                                                    
012100         05  W-KDVALISO          PIC X(3)    VALUE SPACE.                 
012200     03  W-TISTADA9-X.                                                    
012300         05  W-TISTADA9          PIC S9(7)   VALUE ZERO COMP-3.           
012400     SKIP2                                                                
012500*    --- STATUS CODES FROM IMS                                            
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FOUND                       VALUE '  '.                  
012800     88  SEGMENT-FOUND-EXISTS                VALUE 'GB'.                  
012900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013000     SKIP2                                                                
013100 01  GOOD-STATUSCODES.                                                    
013200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(64).                               
013500 01  SSA2                        PIC X(64).                               
013600     EJECT                                                                
013700*    --- IMS FUNCTION CODES                                               
013800*01  -COPY W0003                                                          
013900     EJECT                                                                
014000*    ---  DLI INPUT-OUTPUT AREA                                           
014100                                                                          
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014300 01  DLI-IO-WDB601.                                                       
014400*    03  -COPY WDB601                                                     
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9305'.                    
014600 01  DLI-IO-WDGX9305.                                                     
014700*    03  -COPY WDGX9305                                                   
014800     EJECT                                                                
014900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
015000 01  DLI-IO-WDGX9306.                                                     
015100*    03  -COPY WDGX9306                                                   
015200     EJECT                                                                
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
015400 01  DLI-IO-WDGX9308.                                                     
015500*    03  -COPY WDGX9308                                                   
015600     EJECT                                                                
015700 LINKAGE SECTION.                                                         
015800*01  -COPY W0009   -PRE MSG-                                              
015900*01  -COPY W0008   -PRE WDP7-                                             
016000     05  FILLER                  PIC X.                                   
016100                                                                          
016200*01  -COPY W0008  -PRE WDB6-                                              
016300     05  FILLER                  PIC X.                                   
016400                                                                          
016500*01  -COPY W0008  -PRE 9305-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB 9305-PCB.            
016900 MAIN SECTION.                                                            
017000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB 9305-PCB.            
017100                                                                          
017200     PERFORM IMS-GET-MSG                                                  
017300     IF SEGMENT-FOUND                                                     
017400       PERFORM A-INIT                                                     
017500       PERFORM B-CHECK-KEYS                                               
017600       IF KEYS-OK                                                         
017700          IF MFS-FIRST                                                    
017800             PERFORM C-FIRST-PAGE                                         
017900          ELSE                                                            
018000            IF MFS-NEXT                                                   
018100               PERFORM D-NEXT-PAGE                                        
018200            ELSE                                                          
018300               PERFORM E-SAME-PAGE                                        
018400            END-IF                                                        
018500          END-IF                                                          
018600          PERFORM F-READ-SHOW-INFO                                        
018610       ELSE                                                               
018611          MOVE MID-KDVALISO-IN TO SAVE-KDVALISO-ENTER                     
018615          IF MID-KDVALTYP-IN = ALL '+'                                    
018616           IF SAVE-KDVALTYP-Y = 'Y'                                       
018617             MOVE 'Y'                 TO MOD-KDVALTYP-UT                  
018618           ELSE                                                           
018619             MOVE SAVE-KDVALTYP-ENTER TO MOD-KDVALTYP-UT                  
018620           END-IF                                                         
018621          ELSE                                                            
018622           MOVE MID-KDVALTYP-IN TO SAVE-KDVALTYP-ENTER                    
018623                                   MOD-KDVALTYP-UT                        
018625          END-IF                                                          
018626          MOVE '002'      TO MSGI-KDCALL                                  
018630          MOVE '5123'     TO SAVE-IDTRANS                                 
018640          MOVE SAVE-AREA  TO MSGI-SPAR-AREA                               
018650          CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                      
018700       END-IF                                                             
019000       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O12301 + 4                      
019100       PERFORM IMS-INSERT-MSG                                             
019200     END-IF                                                               
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800                                                                          
019900     IF MSG-DOUBLE-TRANSACTIONS                                           
020000       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W5I12301                 
020100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020300     ELSE                                                                 
020400       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W5I12301                  
020500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020700     END-IF                                                               
020800                                                                          
020900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021200                                                                          
021300     MOVE LOW-VALUE TO MSG-AREA                                           
021400     MOVE 'W5O123N1' TO MFS-IDMOD                                         
021500     MOVE '5123' TO MOD-IDTRANS                                           
021600     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
021700                                                                          
021800     IF OWN-MID OR HELP-MID                                               
021900       CONTINUE                                                           
022000     ELSE                                                                 
022100       MOVE SPACE TO MFS-KDTRTYP                                          
022200       MOVE '7' TO MFS-IDPFK                                              
022300     END-IF                                                               
022310     MOVE ZERO TO INDX                                                    
022400     .                                                                    
022500     EJECT                                                                
022600 B-CHECK-KEYS SECTION.                                                    
022700                                                                          
022800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022900     MOVE '001'             TO MSGI-KDCALL                                
023000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023200     MOVE '5123'            TO MSGI-IDTRANS                               
023300     IF OWN-MID                                                           
023330         MOVE MID-KDVALISO-IN     TO MSGI-KDVALISO                        
023331         MOVE MID-KDVALTYP-IN     TO MSGI-KDVALTYP                        
023340     ELSE                                                                 
023350         MOVE MSGI-KDVALISO       TO MID-KDVALISO-IN                      
023360         MOVE MSGI-KDVALTYP       TO MID-KDVALTYP-IN                      
023500     END-IF                                                               
023600     CALL W005INIT        USING MSGI-WMSGINIT WDP7-PCB                    
023700     MOVE MSGI-SPAR-AREA  TO SAVE-AREA                                    
023900     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
024000     MOVE YES             TO KEYS-SW                                      
024100     MOVE MSGI-IDDC       TO W-IDDC                                       
024200                                                                          
024300*    -- CHECK OF KDVALISO                                                 
024400     MOVE MSGI-KDVALISO   TO W-KDVALISO                                   
024401     IF MSGI-KDVALTYP = 'Y'                                               
024402        MOVE 'A'             TO W-KDVALTYP                                
024403     ELSE                                                                 
024410        MOVE MSGI-KDVALTYP   TO W-KDVALTYP                                
024420     END-IF                                                               
024500     MOVE MFS-ERASE-FIELD TO MOD-KDVALISO-IN                              
024510     MOVE MFS-ERASE-FIELD TO MOD-KDVALTYP-IN                              
024600                                                                          
024610     IF (MID-KDVALISO-IN = ALL '+'                                        
024620     AND MID-KDVALTYP-IN = ALL '+')                                       
024630     AND MFS-IDPFK = '7'                                                  
024640        MOVE YES          TO WS-F7-SW                                     
024650     END-IF                                                               
024700     IF MID-KDVALISO-IN NOT = ALL '+'                                     
024710     OR MID-KDVALTYP-IN NOT = ALL '+'                                     
024720       MOVE NOO         TO WS-F7-SW                                       
024800       MOVE '7'         TO MFS-IDPFK                                      
024900       MOVE SPACE       TO MFS-KDTRTYP                                    
025000       INITIALIZE SAVE-AREA                                               
025100       IF (MID-KDVALISO-IN NOT = ALL '+'                                  
025110       AND (MID-KDVALTYP-IN = ALL '+' OR SPACES))                         
025120         MOVE 'M'       TO MOD-KDVALTYP-UT                                
025130                           W-KDVALTYP                                     
025132         MOVE YES       TO KEYS-SW                                        
025140       END-IF                                                             
025200     END-IF                                                               
025202     IF MID-KDVALTYP-IN = ALL '+'                                         
025203       PERFORM BA-MOVE-SAVE-KDVALTYP-DATA                                 
025224     END-IF                                                               
025225     IF MID-KDVALTYP-IN NOT = ALL '+'                                     
025227       IF MID-KDVALTYP-IN = 'D' OR 'M' OR 'A' OR 'Y'                      
025229          MOVE MID-KDVALTYP-IN TO MOD-KDVALTYP-UT                         
025230                                  W-KDVALTYP                              
025231          IF MID-KDVALTYP-IN = 'Y'                                        
025232             MOVE 'Y'          TO SAVE-KDVALTYP-Y                         
025233             MOVE 'A'          TO W-KDVALTYP                              
025234          END-IF                                                          
025240       ELSE                                                               
025266          MOVE NOO             TO KEYS-SW                                 
025268       END-IF                                                             
025269     END-IF                                                               
025300     IF GOOD-MID OR KEYS-OK                                               
025400       MOVE MSGI-IDDC        TO MOD-IDDC-UT                               
025500       MOVE MSGI-KDVALISO    TO MOD-KDVALISO-UT                           
025600     ELSE                                                                 
025700       MOVE MFS-ERASE-FIELD  TO MOD-IDDC-UT                               
025800       MOVE MFS-ERASE-FIELD  TO MOD-KDVALISO-UT                           
025810       MOVE MFS-ERASE-FIELD  TO MOD-KDVALTYP-UT                           
025900     END-IF                                                               
026000                                                                          
026100     IF KEYS-WRONG                                                        
026200       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
026300       CALL WMEDKONV         USING MED-WMEDAREA                           
026400       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
026500       PERFORM MFS-ERASE-FIELD-OUT                                        
026600     END-IF                                                               
026700     .                                                                    
026800     EJECT                                                                
026810 BA-MOVE-SAVE-KDVALTYP-DATA SECTION.                                      
026811                                                                          
026814     IF SAVE-KDVALTYP-ENTER NOT = SPACES                                  
026815       MOVE SAVE-KDVALTYP-ENTER TO MOD-KDVALTYP-UT                        
026816                                   W-KDVALTYP                             
026817       IF SAVE-KDVALTYP-ENTER = 'D' OR 'M' OR 'A' OR 'Y'                  
026818         CONTINUE                                                         
026819       ELSE                                                               
026821          MOVE NOO           TO KEYS-SW                                   
026822       END-IF                                                             
026823     ELSE                                                                 
026824       MOVE 'M'              TO MOD-KDVALTYP-UT                           
026825                                   W-KDVALTYP                             
026826       MOVE YES              TO KEYS-SW                                   
026827     END-IF                                                               
026828     .                                                                    
026830     EJECT                                                                
026900 C-FIRST-PAGE SECTION.                                                    
027000                                                                          
027011     IF WS-F7 AND SAVE-IDTRANS = '5123'                                   
027019       MOVE SAVE-KDVALTYP-FIRST TO W-KDVALTYP                             
027020       MOVE SAVE-KDVALISO-FIRST TO W-KDVALISO                             
027021       IF SAVE-TISTADA9-FIRST IS NUMERIC                                  
027030         MOVE SAVE-TISTADA9-FIRST TO W-TISTADA9                           
027031       ELSE                                                               
027032         MOVE ZEROS             TO W-TISTADA9                             
027033       END-IF                                                             
027040     END-IF                                                               
027100     MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                           
027200     CALL WMEDKONV USING MED-WMEDAREA                                     
027300     MOVE MED-MFSINF            TO MOD-TEMFSFEL                           
027400                                                                          
027500     .                                                                    
027600     EJECT                                                                
027700 D-NEXT-PAGE SECTION.                                                     
027800                                                                          
027900     IF SAVE-IDTRANS = '5123'                                             
028000       MOVE SAVE-KDVALISO-NEXT   TO W-KDVALISO                            
028001       IF SAVE-KDVALTYP-NEXT = 'Y'                                        
028002         MOVE 'A'                TO W-KDVALTYP                            
028003       ELSE                                                               
028004         MOVE SAVE-KDVALTYP-NEXT TO W-KDVALTYP                            
028005       END-IF                                                             
028006*      MOVE SAVE-KDVALTYP-NEXT   TO W-KDVALTYP                            
028010       IF SAVE-TISTADA9-NEXT IS NUMERIC                                   
028100         MOVE SAVE-TISTADA9-NEXT TO W-TISTADA9                            
028110       ELSE                                                               
028120         MOVE ZEROS              TO W-TISTADA9                            
028200       END-IF                                                             
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 E-SAME-PAGE SECTION.                                                     
028700                                                                          
028800     IF SAVE-IDTRANS = '5123' OR '0551'                                   
028900       MOVE SAVE-KDVALISO-ENTER     TO W-KDVALISO                         
028901       IF SAVE-KDVALTYP-ENTER = 'Y'                                       
028902         MOVE 'A'                   TO W-KDVALTYP                         
028903       ELSE                                                               
028904         MOVE SAVE-KDVALTYP-ENTER   TO W-KDVALTYP                         
028905       END-IF                                                             
028910       IF SAVE-TISTADA9-ENTER IS NUMERIC                                  
029000         MOVE SAVE-TISTADA9-ENTER   TO W-TISTADA9                         
029010       ELSE                                                               
029020         MOVE ZEROS                 TO W-TISTADA9                         
029100       END-IF                                                             
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
030300 F-READ-SHOW-INFO SECTION.                                                
030400     MOVE +1 TO INDX                                                      
030500     PERFORM IMS-GU-WDB601                                                
030600     IF SEGMENT-MISSING                                                   
030700        MOVE INF-DATA-MISSING   TO MED-IDMFSFEL                           
030800        CALL WMEDKONV           USING MED-WMEDAREA                        
030900        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
031000        PERFORM MFS-ERASE-FIELD-OUT                                       
031100     ELSE                                                                 
031200       MOVE DCS-KDVALISO        TO W-KDVALISO-HUV                         
031210       IF DCS-IDDC = '87'                                                 
031220         MOVE 'AED'             TO W-KDVALISO-HUV                         
031230       END-IF                                                             
031300       PERFORM IMS-GU-WDG201                                              
031400       IF SEGMENT-MISSING                                                 
031500          MOVE INF-DATA-MISSING TO MED-IDMFSFEL                           
031600          CALL WMEDKONV USING MED-WMEDAREA                                
031700          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
031800          PERFORM MFS-ERASE-FIELD-OUT                                     
031900       ELSE                                                               
032000         IF MSGI-KDVALISO = SPACES OR LOW-VALUES                          
032100            PERFORM FB-READ-NORMAL                                        
032200         ELSE                                                             
032300            PERFORM FA-READ-HISTORY                                       
032400         END-IF                                                           
032515       END-IF                                                             
032520     END-IF                                                               
032530     MOVE '002'      TO MSGI-KDCALL                                       
032540     MOVE '5123'     TO SAVE-IDTRANS                                      
032550     MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                    
032560     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
032800     .                                                                    
032900     EJECT                                                                
033000 FA-READ-HISTORY SECTION.                                                 
033100     PERFORM IMS-GU-WDGX9306                                              
033200     IF SEGMENT-MISSING                                                   
033201        IF SAVE-KDVALTYP-Y = 'Y'                                          
033203           MOVE 'Y'                TO SAVE-KDVALTYP-ENTER                 
033204        ELSE                                                              
033210          MOVE W-KDVALTYP          TO SAVE-KDVALTYP-ENTER                 
033211        END-IF                                                            
033212*       MOVE W-KDVALTYP           TO SAVE-KDVALTYP-ENTER                  
033220        MOVE W-KDVALISO           TO SAVE-KDVALISO-ENTER                  
033230        MOVE W-TISTADA9           TO SAVE-TISTADA9-ENTER                  
033300        MOVE INF-DATA-MISSING        TO MED-IDMFSFEL                      
033400        CALL WMEDKONV USING MED-WMEDAREA                                  
033500        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
033600        PERFORM MFS-ERASE-FIELD-OUT                                       
033700     ELSE                                                                 
033710        PERFORM IMS-GNP-WDGX9308                                          
033720        IF SEGMENT-FOUND                                                  
033721           IF SAVE-KDVALTYP-Y = 'Y'                                       
033723              MOVE 'Y'                TO SAVE-KDVALTYP-ENTER              
033724           ELSE                                                           
033725             MOVE 9305-KDVALTYP       TO SAVE-KDVALTYP-ENTER              
033726           END-IF                                                         
033730*          MOVE 9305-KDVALTYP        TO SAVE-KDVALTYP-ENTER               
033740           MOVE 9306-KDVALISO        TO SAVE-KDVALISO-ENTER               
033750           MOVE 9308-TISTADAT-9KOMPL TO SAVE-TISTADA9-ENTER               
033751           IF WS-NOT-F7                                                   
033761             MOVE 9305-KDVALTYP        TO SAVE-KDVALTYP-FIRST             
033762             MOVE 9306-KDVALISO        TO SAVE-KDVALISO-FIRST             
033763             MOVE 9308-TISTADAT-9KOMPL TO SAVE-TISTADA9-FIRST             
033764             MOVE YES                  TO WS-F7-SW                        
033765           END-IF                                                         
033766        ELSE                                                              
033767           IF SAVE-KDVALTYP-Y = 'Y'                                       
033769              MOVE 'Y'               TO SAVE-KDVALTYP-ENTER               
033770           ELSE                                                           
033771             MOVE W-KDVALTYP         TO SAVE-KDVALTYP-ENTER               
033772           END-IF                                                         
033773*          MOVE W-KDVALTYP           TO SAVE-KDVALTYP-ENTER               
033780           MOVE W-KDVALISO           TO SAVE-KDVALISO-ENTER               
033790           MOVE W-TISTADA9           TO SAVE-TISTADA9-ENTER               
033794           IF WS-NOT-F7                                                   
033801             MOVE W-KDVALTYP         TO SAVE-KDVALTYP-FIRST               
033802             MOVE W-KDVALISO         TO SAVE-KDVALISO-FIRST               
033803             MOVE W-TISTADA9         TO SAVE-TISTADA9-FIRST               
033804             MOVE YES                TO WS-F7-SW                          
033805           END-IF                                                         
033806           MOVE INF-DATA-MISSING     TO MED-IDMFSFEL                      
033807           CALL WMEDKONV USING MED-WMEDAREA                               
033808           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
033809           PERFORM MFS-ERASE-FIELD-OUT                                    
033810        END-IF                                                            
033811     END-IF                                                               
033812     MOVE +1 TO INDX                                                      
033813     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-MISSING                     
033814       IF SEGMENT-FOUND                                                   
033815         MOVE 9308-TIREGDAT      TO MOD-TIREGDAT(INDX)                    
033816         MOVE 9308-TISTADAT      TO MOD-TISTADAT(INDX)                    
033817         MOVE 9308-PRKURS        TO MOD-PRKURS-NEW(INDX)                  
033818         MOVE 9308-REVALUTA-TO   TO MOD-REVALUTA-TO(INDX)                 
033819         MOVE 9308-REVALUTA-FROM TO MOD-REVALUTA-FROM(INDX)               
033820         MOVE 9306-KDVALISO      TO MOD-KDVALISO(INDX)                    
033821         PERFORM IMS-GNP-WDGX9308                                         
033822       END-IF                                                             
033823       ADD +1 TO INDX                                                     
033824     END-PERFORM                                                          
033825     IF SEGMENT-FOUND AND INDX > MAX-INDX                                 
033826        MOVE 9305-KDVALTYP        TO SAVE-KDVALTYP-NEXT                   
033827        MOVE 9306-KDVALISO        TO SAVE-KDVALISO-NEXT                   
033828        MOVE 9308-TISTADAT-9KOMPL TO SAVE-TISTADA9-NEXT                   
033830        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
033831        CALL WMEDKONV USING MED-WMEDAREA                                  
033832        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
033833     ELSE                                                                 
033835       IF SEGMENT-MISSING AND MED-IDMFSFEL NOT = INF-DATA-MISSING         
033836         IF SAVE-KDVALTYP-ENTER = 'Y'                                     
033837           MOVE 'A'                 TO SAVE-KDVALTYP-NEXT                 
033838         ELSE                                                             
033839           MOVE SAVE-KDVALTYP-ENTER TO SAVE-KDVALTYP-NEXT                 
033840         END-IF                                                           
033841         MOVE SAVE-KDVALISO-ENTER TO SAVE-KDVALISO-NEXT                   
033842         MOVE SAVE-TISTADA9-ENTER TO SAVE-TISTADA9-NEXT                   
033843         MOVE INF-LAST-PAGE        TO MED-IDMFSINF                        
033844         CALL WMEDKONV USING MED-WMEDAREA                                 
033845         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
033846       END-IF                                                             
033847     END-IF                                                               
033849     IF INDX < MAX-INDX                                                   
033850       PERFORM UNTIL INDX > MAX-INDX                                      
033851         MOVE MFS-ERASE-FIELD   TO MOD-TIREGDAT(INDX)                     
033852                                   MOD-TISTADAT(INDX)                     
033853                                   MOD-PRKURS-NEW(INDX)                   
033854                                   MOD-REVALUTA-TO(INDX)                  
033855                                   MOD-REVALUTA-FROM(INDX)                
033856                                   MOD-KDVALISO(INDX)                     
033857         ADD +1 TO INDX                                                   
033858       END-PERFORM                                                        
033859     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 FB-READ-NORMAL SECTION.                                                  
037200                                                                          
037300     PERFORM IMS-GNP-WDGX9306                                             
037400     IF SEGMENT-MISSING                                                   
037401        IF SAVE-KDVALTYP-Y = 'Y'                                          
037402           MOVE 'Y'               TO SAVE-KDVALTYP-ENTER                  
037403        ELSE                                                              
037404          MOVE W-KDVALTYP         TO SAVE-KDVALTYP-ENTER                  
037405        END-IF                                                            
037410*       MOVE W-KDVALTYP           TO SAVE-KDVALTYP-ENTER                  
037420        MOVE W-KDVALISO           TO SAVE-KDVALISO-ENTER                  
037430        MOVE W-TISTADA9           TO SAVE-TISTADA9-ENTER                  
037500        MOVE INF-DATA-MISSING     TO MED-IDMFSFEL                         
037600        CALL WMEDKONV USING MED-WMEDAREA                                  
037700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
037800        PERFORM MFS-ERASE-FIELD-OUT                                       
037900     ELSE                                                                 
038000        PERFORM IMS-GNP-WDGX9308-UNQUAL                                   
038100        IF SEGMENT-FOUND                                                  
038110           IF SAVE-KDVALTYP-Y = 'Y'                                       
038120              MOVE 'Y'               TO SAVE-KDVALTYP-ENTER               
038130           ELSE                                                           
038140             MOVE 9305-KDVALTYP      TO SAVE-KDVALTYP-ENTER               
038150           END-IF                                                         
038200*          MOVE 9305-KDVALTYP        TO SAVE-KDVALTYP-ENTER               
038210           MOVE 9306-KDVALISO        TO SAVE-KDVALISO-ENTER               
038300           MOVE 9308-TISTADAT-9KOMPL TO SAVE-TISTADA9-ENTER               
038310           IF WS-NOT-F7                                                   
038320             MOVE 9305-KDVALTYP        TO SAVE-KDVALTYP-FIRST             
038330             MOVE 9306-KDVALISO        TO SAVE-KDVALISO-FIRST             
038340             MOVE 9308-TISTADAT-9KOMPL TO SAVE-TISTADA9-FIRST             
038341             MOVE YES                  TO WS-F7-SW                        
038350           END-IF                                                         
038400        ELSE                                                              
038410           IF SAVE-KDVALTYP-Y = 'Y'                                       
038420              MOVE 'Y'               TO SAVE-KDVALTYP-ENTER               
038430           ELSE                                                           
038440             MOVE W-KDVALTYP         TO SAVE-KDVALTYP-ENTER               
038450           END-IF                                                         
038500*          MOVE W-KDVALTYP         TO SAVE-KDVALTYP-ENTER                 
038510           MOVE W-KDVALISO         TO SAVE-KDVALISO-ENTER                 
038600           MOVE W-TISTADA9         TO SAVE-TISTADA9-ENTER                 
038610           IF WS-NOT-F7                                                   
038620             MOVE 9305-KDVALTYP        TO SAVE-KDVALTYP-FIRST             
038630             MOVE 9306-KDVALISO        TO SAVE-KDVALISO-FIRST             
038640             MOVE 9308-TISTADAT-9KOMPL TO SAVE-TISTADA9-FIRST             
038641             MOVE YES                  TO WS-F7-SW                        
038650           END-IF                                                         
038700           MOVE INF-DATA-MISSING   TO MED-IDMFSFEL                        
038800           CALL WMEDKONV USING MED-WMEDAREA                               
038900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
039000           PERFORM MFS-ERASE-FIELD-OUT                                    
039100        END-IF                                                            
039200     END-IF                                                               
039300     MOVE +1 TO INDX                                                      
039400     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-MISSING                     
039500       IF SEGMENT-FOUND                                                   
039600         MOVE 9308-TIREGDAT      TO MOD-TIREGDAT(INDX)                    
039700         MOVE 9308-TISTADAT      TO MOD-TISTADAT(INDX)                    
039800         MOVE 9308-PRKURS        TO MOD-PRKURS-NEW(INDX)                  
039900         MOVE 9308-REVALUTA-TO   TO MOD-REVALUTA-TO(INDX)                 
039910         MOVE 9308-REVALUTA-FROM TO MOD-REVALUTA-FROM(INDX)               
040000         MOVE 9306-KDVALISO TO MOD-KDVALISO(INDX)                         
040100         PERFORM IMS-GNP-WDGX9306                                         
040200         PERFORM IMS-GNP-WDGX9308-UNQUAL                                  
040210       END-IF                                                             
040211       ADD +1 TO INDX                                                     
040212     END-PERFORM                                                          
040213     IF SEGMENT-FOUND AND INDX > MAX-INDX                                 
040215        MOVE 9305-KDVALTYP        TO SAVE-KDVALTYP-NEXT                   
040216        MOVE 9306-KDVALISO        TO SAVE-KDVALISO-NEXT                   
040217        MOVE 9308-TISTADAT-9KOMPL TO SAVE-TISTADA9-NEXT                   
040221        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
040222        CALL WMEDKONV USING MED-WMEDAREA                                  
040223        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
040224     ELSE                                                                 
040226       IF SEGMENT-MISSING AND MED-IDMFSFEL NOT = INF-DATA-MISSING         
040227         IF SAVE-KDVALTYP-ENTER = 'Y'                                     
040228           MOVE 'A'                 TO SAVE-KDVALTYP-NEXT                 
040229         ELSE                                                             
040230           MOVE SAVE-KDVALTYP-ENTER TO SAVE-KDVALTYP-NEXT                 
040231         END-IF                                                           
040232         MOVE SAVE-KDVALISO-ENTER TO SAVE-KDVALISO-NEXT                   
040233         MOVE SAVE-TISTADA9-ENTER TO SAVE-TISTADA9-NEXT                   
040234         MOVE INF-LAST-PAGE        TO MED-IDMFSINF                        
040235         CALL WMEDKONV USING MED-WMEDAREA                                 
040236         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
040237       END-IF                                                             
040238     END-IF                                                               
040241     IF INDX < MAX-INDX                                                   
040242       PERFORM UNTIL INDX > MAX-INDX                                      
040244         MOVE MFS-ERASE-FIELD   TO MOD-TIREGDAT(INDX)                     
040245                                   MOD-TISTADAT(INDX)                     
040246                                   MOD-PRKURS-NEW(INDX)                   
040247                                   MOD-REVALUTA-TO(INDX)                  
040248                                   MOD-REVALUTA-FROM(INDX)                
040249                                   MOD-KDVALISO(INDX)                     
040250         ADD +1 TO INDX                                                   
040251       END-PERFORM                                                        
040260     END-IF                                                               
041200     .                                                                    
041300     SKIP3                                                                
044300 MFS-ERASE-FIELD-OUT SECTION.                                             
044310     MOVE +1 TO INDX                                                      
044320     PERFORM UNTIL INDX > MAX-INDX                                        
044400       MOVE MFS-ERASE-FIELD TO MOD-KDVALISO(INDX)                         
044500                               MOD-TISTADAT(INDX)                         
044600                               MOD-TIREGDAT(INDX)                         
044700                               MOD-PRKURS-NEW(INDX)                       
044800                               MOD-REVALUTA-TO(INDX)                      
044810                               MOD-REVALUTA-FROM(INDX)                    
044820       ADD +1 TO INDX                                                     
044830     END-PERFORM                                                          
044900     .                                                                    
045000     SKIP3                                                                
045100 IMS-GET-MSG SECTION.                                                     
045200                                                                          
045300     MOVE '  QC' TO GOOD-STATUSCODES                                      
045400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
045500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
045600     PERFORM IMS-STATUSCHECK                                              
045700     .                                                                    
045800     SKIP3                                                                
045900 IMS-INSERT-MSG SECTION.                                                  
046000                                                                          
046100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
046200     MOVE SPACE TO GOOD-STATUSCODES                                       
046300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
046400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046500     PERFORM IMS-STATUSCHECK                                              
046600     .                                                                    
046700     EJECT                                                                
046800 IMS-GU-WDB601 SECTION.                                                   
046900                                                                          
047000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
047100          DELIMITED BY SIZE INTO SSA1                                     
047200     MOVE '  GE' TO GOOD-STATUSCODES                                      
047300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
047400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
047500     PERFORM IMS-STATUSCHECK                                              
047600     .                                                                    
047700     EJECT                                                                
047800 IMS-GU-WDG201 SECTION.                                                   
047900                                                                          
048000     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
048100          DELIMITED BY SIZE INTO SSA1                                     
048200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
048300     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9305 SSA1                  
048400     MOVE 9305-STATUS-CODE TO STATUS-WS                                   
048500     PERFORM IMS-STATUSCHECK                                              
048600     .                                                                    
048700     EJECT                                                                
048800 IMS-GNP-WDGX9306 SECTION.                                                
048900                                                                          
049000     STRING 'WDGX9306(KDVALISO=>' W-KDVALISO-X ')'                        
049100          DELIMITED BY SIZE INTO SSA1                                     
049200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
049300     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9306 SSA1                 
049400     MOVE 9305-STATUS-CODE TO STATUS-WS                                   
049500     PERFORM IMS-STATUSCHECK                                              
049600     .                                                                    
049700     EJECT                                                                
049800 IMS-GNP-WDGX9308-UNQUAL SECTION.                                         
049900                                                                          
050000     MOVE 'WDGX9308  '        TO SSA1                                     
050100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
050200     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
050300     MOVE 9305-STATUS-CODE TO STATUS-WS                                   
050400     PERFORM IMS-STATUSCHECK                                              
050500     .                                                                    
050600     EJECT                                                                
050700 IMS-GU-WDGX9306 SECTION.                                                 
050800                                                                          
050900     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
051000          DELIMITED BY SIZE INTO SSA1                                     
051100     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
051200          DELIMITED BY SIZE INTO SSA2                                     
051300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
051400     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
051500     MOVE 9305-STATUS-CODE TO STATUS-WS                                   
051600     PERFORM IMS-STATUSCHECK                                              
051700     .                                                                    
051800     EJECT                                                                
051900 IMS-GNP-WDGX9308 SECTION.                                                
052000                                                                          
052100     STRING 'WDGX9308(TISTADA9>=' W-TISTADA9-X ')'                        
052200          DELIMITED BY SIZE INTO SSA1                                     
052300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
052400     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
052500     MOVE 9305-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUSCHECK                                              
052700     .                                                                    
052800     EJECT                                                                
052900 IMS-STATUSCHECK SECTION.                                                 
053000                                                                          
053100     SET STATUS-IX TO 1                                                   
053200     SEARCH GOOD-STATUS                                                   
053300       AT END                                                             
053400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
053500         DELIMITED BY SIZE INTO ERROR-TEXT                                
053600         CALL FELLOG                                                      
053700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
053800         CONTINUE                                                         
053900     END-SEARCH                                                           
054000     .                                                                    
