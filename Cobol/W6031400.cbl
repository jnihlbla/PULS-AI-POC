000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6031400.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/02/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        ADD LOCATIONS                                                    
000900*                                                                         
001000*     CHANGED 97-07-04 JOHAN LINDKVIST                                    
001100*     CHANGED 97-02-04 JOHAN LINDKVIST (IO-COUNT EXCEEDED)                
001200*                                                                         
001300*        THE PROGRAM READS     WL6313 (WDGX)                              
001400*        THE PROGRAM READS     WL6315 (WDGX)                              
001500*        THE PROGRAM ADD       WLLOCA (WDJ8)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W6T314                                              
001900*        MID:         W6I31401                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W6O31401                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6031400'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  YES                         PIC X       VALUE 'Y'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  IO-COUNT                   PIC S9(4)  VALUE +0    COMP SYNC.         
004000 77  MAX-IO-COUNT               PIC S9(4)  VALUE +200  COMP SYNC.         
004100 77  LNG-P-TO-P-PREFIX          PIC S9(4)  VALUE +17   COMP SYNC.         
004200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004300                                                                          
004400 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
004500 77  WS-ADGANG-FOM               PIC 9(2)   VALUE ZERO.                   
004600 77  WS-ADGANG-TOM               PIC 9(2)   VALUE ZERO.                   
004700 77  WS-ADGANG                   PIC 9(3)   VALUE ZERO.                   
004800 77  WS-ADSEC-FOM                PIC 9(2)   VALUE ZERO.                   
004900 77  WS-ADSEC-TOM                PIC 9(2)   VALUE ZERO.                   
005000 77  WS-ADSEC                    PIC 9(3)   VALUE ZERO.                   
005100 77  WS-ADLEVEL-FOM              PIC 9(2)   VALUE ZERO.                   
005200 77  WS-ADLEVEL-TOM              PIC 9(2)   VALUE ZERO.                   
005300 77  WS-ADLEVEL                  PIC 9(3)   VALUE ZERO.                   
005400 77  WS-ADSEQ-FOM                PIC 9(1)   VALUE ZERO.                   
005500 77  WS-ADSEQ-TOM                PIC 9(1)   VALUE ZERO.                   
005600 77  WS-ADSEQ                    PIC 9(2)   VALUE ZERO.                   
005700 77  WS-KDAOE                    PIC X(1)   VALUE SPACE.                  
005800                                                                          
005900 77  WS-SECTION-STEP             PIC 9      VALUE 1.                      
006000                                                                          
006100 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006200     88  INDATA-OK                           VALUE 'Y'.                   
006300     88  INDATA-WRONG                        VALUE 'N'.                   
006400                                                                          
006500 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
006600     88  KEYS-OK                             VALUE 'Y'.                   
006700     88  KEYS-WRONG                          VALUE 'N'.                   
006800                                                                          
006900 77  RESTART-SW                  PIC X       VALUE 'N'.                   
007000     88  RESTART                             VALUE 'Y'.                   
007100                                                                          
007200 77  PRIME                       PIC X        VALUE 'P'.                  
007300 77  BUFFER                      PIC X        VALUE 'R'.                  
007400 77  MIXED                       PIC X        VALUE 'M'.                  
007500 77  ALLA                        PIC X        VALUE 'A'.                  
007600 77  ODD                         PIC X        VALUE 'O'.                  
007700 77  EVEN                        PIC X        VALUE 'E'.                  
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  OWN-MID                             VALUE '6314'.                
008100     88  GOOD-MID                            VALUE '6314'.                
008200     88  HELP-MID                            VALUE '0551'.                
008300       EJECT                                                              
008400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008500 01  GENERAL-SUBPROGRAMS.                                                 
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     EJECT                                                                
009100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009200*01 -COPY WMEDAREA                                                        
009300     SKIP3                                                                
009400 01  MESSAGE-CODES.                                                       
009500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010000     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
010100     EJECT                                                                
010200   03    MED-1.                                                           
010300     05    FILLER                  PIC X(23)   VALUE                      
010400         'UPDATED UNTIL LOCATION '.                                       
010500     05    MED-IDDC                PIC X(02).                             
010600     05    FILLER                  PIC X(02)   VALUE SPACE.               
010700     05    MED-ADLAGOMR            PIC 9(02).                             
010800     05    FILLER                  PIC X(01)   VALUE SPACE.               
010900     05    MED-ADGANG              PIC 9(02).                             
011000     05    FILLER                  PIC X(01)   VALUE SPACE.               
011100     05    MED-ADSEC               PIC 9(02).                             
011200     05    MED-ADLEVEL             PIC 9(02).                             
011300     05    MED-ADSEQ               PIC 9(01).                             
011400     05    FILLER                  PIC X(01)   VALUE '.'.                 
011500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011600*                                                                         
011700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011800     SKIP3                                                                
011900*01 -COPY WMSGINIT                                                        
012000     EJECT                                                                
012100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012400     SKIP3                                                                
012500*01  MID -COPY W6I31401                                                   
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012800     SKIP3                                                                
012900*01  -COPY WMSGAREA                                                       
013000     EJECT                                                                
013100     03  MOD REDEFINES MSG-AREA.                                          
013200*      05  -COPY W6O31401                                                 
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013500     SKIP3                                                                
013600*01  -COPY WMFSAREA                                                       
013700     EJECT                                                                
013800 01      P-TO-P-SW.                                                       
013900                                                                          
014000   03     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.          
014100   03     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.              
014200   03     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.              
014300   03     P-TO-P-KDTRANS          PIC X(8).                               
014400   03     P-TO-P-IDTRANS          PIC X(4).                               
014500   03     P-TO-P-KDMFSFOR         PIC X(1).                               
014600   03     -COPY W6I31401 -PRE 6314-UT-                                    
014700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014800*                                                                         
014900     EJECT                                                                
015000 01  WORK-AREA.                                                           
015100     03 WORK-ADLAGOMR            PIC 9(2).                                
015200     03 WORK-ADGANG              PIC 9(2).                                
015300     03 WORK-ADPLATS.                                                     
015400        05 WORK-ADSEC            PIC 9(2).                                
015500        05 WORK-ADLEVEL          PIC 9(2).                                
015600        05 WORK-ADSEQ            PIC 9(1).                                
015700                                                                          
015800 01  SAVE-AREA.                                                           
015900     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
016000     03 SAVE-ADLAGOMR            PIC 9(2).                                
016100     03 SAVE-ADGANG              PIC 9(2).                                
016200     03 SAVE-ADPLATS.                                                     
016300        05 SAVE-ADSEC            PIC 9(2).                                
016400        05 SAVE-ADLEVEL          PIC 9(2).                                
016500        05 SAVE-ADSEQ            PIC 9(1).                                
016600                                                                          
016700                                                                          
016800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016900     SKIP3                                                                
017000 01  KEYS-TO-DLI.                                                         
017100     03  W-WDGXKEY-6313-X.                                                
017200          05 W-6313-IDHTYP       PIC X(4)    VALUE '6313'.                
017300          05 W-6313-IDDC         PIC X(2)    VALUE SPACE.                 
017400          05 W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
017500                                                                          
017600     03  W-WDGXKEY-6314-X.                                                
017700         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
017800                                                                          
017900     03  W-WDGXKEY-6315-X.                                                
018000          05 W-6315-IDHTYP       PIC X(4)    VALUE '6315'.                
018100          05 W-6315-IDDC         PIC X(2)    VALUE SPACE.                 
018200          05 W-6315-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
018300                                                                          
018400     03  W-WDGXKEY-6316-X.                                                
018500         05  W-KDSTOR            PIC X(3)    VALUE SPACE.                 
018600                                                                          
018700     03  W-WDJ8KEY-X.                                                     
018800         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
018900         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
019000         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
019100         05  W-LOC-ADPLATS.                                               
019200             07 W-LOC-ADSEC      PIC 9(2).                                
019300             07 W-LOC-ADLEVEL    PIC 9(2).                                
019400             07 W-LOC-ADSEQ      PIC 9(1).                                
019500                                                                          
019600     03  W-IDDC-B6-X.                                                     
019700         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
019800     SKIP2                                                                
019900*    --- STATUS-KOD FRÅN IMS                                              
020000 01  STATUS-WS                   PIC XX.                                  
020100     88  SEGMENT-FOUND                       VALUE '  '.                  
020200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
020300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
020400     SKIP2                                                                
020500 01  GOOD-STATUSCODES.                                                    
020600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020700     SKIP3                                                                
020800 01  SSA1                        PIC X(64).                               
020900 01  SSA2                        PIC X(64).                               
021000     EJECT                                                                
021100*    --- IMS FUNCTION CODES                                               
021200*01  -COPY W0003                                                          
021300     EJECT                                                                
021400*    ---  DLI INPUT-OUTPUT AREA                                           
021500 01  FILLER         PIC X(20) VALUE 'WL631301-AREA'.                      
021600 01  WL631301-AREA.                                                       
021700*    03  -COPY WDGX6313                                                   
021800     EJECT                                                                
021900 01  FILLER         PIC X(20) VALUE 'WL631311-AREA'.                      
022000 01  WL631311-AREA.                                                       
022100*    03  -COPY WDGX6314                                                   
022200                                                                          
022300 01  FILLER         PIC X(20) VALUE 'WL631501-AREA'.                      
022400 01  WL631501-AREA.                                                       
022500*    03  -COPY WDGX6315                                                   
022600     EJECT                                                                
022700 01  FILLER         PIC X(20) VALUE 'WL631511-AREA'.                      
022800 01  WL631511-AREA.                                                       
022900*    03  -COPY WDGX6316                                                   
023000                                                                          
023100 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
023200 01  DLI-IO-WLLOCA01.                                                     
023300*    03  -COPY WDJ801                                                     
023400     EJECT                                                                
023500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
023600 01   DLI-IO-AREA-B601.                                                   
023700*     03  -COPY WDB601                                                    
023800     EJECT                                                                
023900 LINKAGE SECTION.                                                         
024000*01  -COPY W0009  -PRE MSG-                                               
024100     EJECT                                                                
024200*01  -COPY W0009  -PRE ALT6314-                                           
024300     EJECT                                                                
024400*01  -COPY W0008  -PRE USEA-                                              
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE 6313-                                              
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000*01  -COPY W0008  -PRE 6315-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300*01  -COPY W0008  -PRE LOCA-                                              
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600*01  -COPY W0008      -PRE WDB6-                                          
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900 PROCEDURE DIVISION  USING MSG-PCB ALT6314-PCB                            
026000           USEA-PCB                                                       
026100           6313-PCB 6315-PCB LOCA-PCB                                     
026200           WDB6-PCB.                                                      
026300 MAIN SECTION.                                                            
026400     ENTRY 'DLITCBL' USING MSG-PCB ALT6314-PCB                            
026500            USEA-PCB                                                      
026600            6313-PCB 6315-PCB LOCA-PCB                                    
026700            WDB6-PCB.                                                     
026800                                                                          
026900     PERFORM IMS-GET-MSG                                                  
027000     IF SEGMENT-FOUND                                                     
027100       PERFORM A-INIT                                                     
027200       IF GOOD-MID OR HELP-MID                                            
027300          PERFORM B-CHECK-KEYS                                            
027400          IF KEYS-OK                                                      
027500             IF MFS-UPDATE                                                
027600                PERFORM G-CHECK-INPUT                                     
027700                IF INDATA-OK                                              
027800                   PERFORM H-UPDATE                                       
027900                END-IF                                                    
028000              ELSE                                                        
028100                IF MFS-FIRST                                              
028200                   PERFORM C-FIRST-PAGE                                   
028300                 ELSE                                                     
028400                   PERFORM E-SAME-PAGE                                    
028500                END-IF                                                    
028600             END-IF                                                       
028700          END-IF                                                          
028800       END-IF                                                             
028900       IF RESTART                                                         
029000          COMPUTE P-TO-P-KVLL =  LNG-P-TO-P-PREFIX +                      
029100                                 LENGTH OF MID-W6I31401                   
029200          MOVE 'W6T314U '     TO P-TO-P-KDTRANS                           
029300          MOVE '6314'         TO P-TO-P-IDTRANS                           
029400          MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                          
029500          MOVE MID-W6I31401   TO 6314-UT-MID-W6I31401                     
029600          PERFORM IMS-ISRT-ALT-MSG-6314                                   
029700        ELSE                                                              
029800          COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31401 + 4                   
029900          PERFORM IMS-INSERT-MSG                                          
030000       END-IF                                                             
030100     END-IF                                                               
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800                                                                          
030900     IF MSG-DOUBLE-TRANSACTIONS                                           
031000       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I31401                 
031100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031300     ELSE                                                                 
031400       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I31401                  
031500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031700     END-IF                                                               
031800                                                                          
031900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032200                                                                          
032300     MOVE LOW-VALUE TO MSG-AREA                                           
032400     MOVE 'W6O314N1' TO MFS-IDMOD                                         
032500     MOVE '6314' TO MOD-IDTRANS                                           
032600     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
032700                                                                          
032800     MOVE NOO                  TO RESTART-SW                              
032900                                                                          
033000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033100     MOVE '001'             TO MSGI-KDCALL                                
033200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033400     MOVE '6314'            TO MSGI-IDTRANS                               
033500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033600                                                                          
033700     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
033800     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
033900                                                                          
034000     IF GOOD-MID OR HELP-MID                                              
034100       CONTINUE                                                           
034200     ELSE                                                                 
034300       MOVE SPACE TO MFS-KDTRTYP                                          
034400       MOVE '7' TO MFS-IDPFK                                              
034500       PERFORM MFS-INIT-KEY-FIELD-IN                                      
034600       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
034700       PERFORM MFS-ERASE-FIELD-IN                                         
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 B-CHECK-KEYS SECTION.                                                    
035200                                                                          
035300                                                                          
035400     MOVE YES               TO KEYS-SW                                    
035500                                                                          
035600     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
035700                               MOD-ADLAGOMR-IN                            
035800                               MOD-ADGANG-FOM-IN                          
035900                               MOD-ADGANG-TOM-IN                          
036000                               MOD-ADSEC-FOM-IN                           
036100                               MOD-ADSEC-TOM-IN                           
036200                               MOD-ADLEVEL-FOM-IN                         
036300                               MOD-ADLEVEL-TOM-IN                         
036400                               MOD-ADSEQ-FOM-IN                           
036500                               MOD-ADSEQ-TOM-IN                           
036600                               MOD-KDAOE-IN                               
036700                                                                          
036800*                                                                         
036900*    -- CONTROL  ON WAREHOUSE                                             
037000*                                                                         
037100     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
037200     PERFORM IMS-GU-WDB601                                                
037300     IF SEGMENT-FOUND                                                     
037400     AND (DCS-SDC                                                         
037500     OR   DCS-NDC-NA                                                      
037600     OR   DCS-NDC-PF                                                      
037700     OR   DCS-NDC-OTHERS)                                                 
037800       CONTINUE                                                           
037900     ELSE                                                                 
038000       MOVE NOO                 TO KEYS-SW                                
038100     END-IF                                                               
038200*                                                                         
038300*    -- CONTROL  AREA                                                     
038400*                                                                         
038500     IF MID-ADLAGOMR-IN = ALL '+'                                         
038600       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
038700       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
038800     ELSE                                                                 
038900       IF MID-ADLAGOMR-IN NUMERIC                                         
039000          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
039100          MOVE '7' TO MFS-IDPFK                                           
039200          MOVE SPACE TO MFS-KDTRTYP                                       
039300       ELSE                                                               
039400          MOVE NOO             TO KEYS-SW                                 
039500       END-IF                                                             
039600     END-IF                                                               
039700*                                                                         
039800*    -- CONTROL  AISLE FROM                                               
039900*                                                                         
040000     IF MID-ADGANG-FOM-IN = ALL '+'                                       
040100       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
040200       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
040300     ELSE                                                                 
040400       IF MID-ADGANG-FOM-IN NUMERIC                                       
040500          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
040600          MOVE '7' TO MFS-IDPFK                                           
040700          MOVE SPACE TO MFS-KDTRTYP                                       
040800       ELSE                                                               
040900          MOVE NOO               TO KEYS-SW                               
041000       END-IF                                                             
041100     END-IF                                                               
041200*                                                                         
041300*    -- CONTROL  AISLE THRU                                               
041400*                                                                         
041500     IF MID-ADGANG-TOM-IN = ALL '+'                                       
041600       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
041700       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
041800     ELSE                                                                 
041900       IF MID-ADGANG-TOM-IN NUMERIC                                       
042000          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
042100          MOVE '7' TO MFS-IDPFK                                           
042200          MOVE SPACE TO MFS-KDTRTYP                                       
042300       ELSE                                                               
042400          MOVE NOO               TO KEYS-SW                               
042500       END-IF                                                             
042600     END-IF                                                               
042700*                                                                         
042800*    -- CONTROL  SECTION FROM                                             
042900*                                                                         
043000     IF MID-ADSEC-FOM-IN = ALL '+'                                        
043100       INSPECT MID-ADSEC-FOM-UT REPLACING LEADING SPACE BY ZERO           
043200       MOVE MID-ADSEC-FOM-UT    TO WS-ADSEC-FOM                           
043300     ELSE                                                                 
043400       IF MID-ADSEC-FOM-IN NUMERIC                                        
043500          MOVE MID-ADSEC-FOM-IN TO WS-ADSEC-FOM                           
043600          MOVE '7' TO MFS-IDPFK                                           
043700          MOVE SPACE TO MFS-KDTRTYP                                       
043800       ELSE                                                               
043900          MOVE NOO              TO KEYS-SW                                
044000       END-IF                                                             
044100     END-IF                                                               
044200                                                                          
044300*                                                                         
044400*    -- CONTROL  SECTION THRU                                             
044500*                                                                         
044600     IF MID-ADSEC-TOM-IN = ALL '+'                                        
044700       INSPECT MID-ADSEC-TOM-UT REPLACING LEADING SPACE BY ZERO           
044800       MOVE MID-ADSEC-TOM-UT    TO WS-ADSEC-TOM                           
044900     ELSE                                                                 
045000       IF MID-ADSEC-TOM-IN NUMERIC                                        
045100          MOVE MID-ADSEC-TOM-IN TO WS-ADSEC-TOM                           
045200          MOVE '7' TO MFS-IDPFK                                           
045300          MOVE SPACE TO MFS-KDTRTYP                                       
045400       ELSE                                                               
045500          MOVE NOO              TO KEYS-SW                                
045600       END-IF                                                             
045700     END-IF                                                               
045800                                                                          
045900*                                                                         
046000*    -- CONTROL  LEVEL FROM                                               
046100*                                                                         
046200     IF MID-ADLEVEL-FOM-IN = ALL '+'                                      
046300       INSPECT MID-ADLEVEL-FOM-UT REPLACING LEADING SPACE BY ZERO         
046400       MOVE MID-ADLEVEL-FOM-UT    TO WS-ADLEVEL-FOM                       
046500     ELSE                                                                 
046600       IF MID-ADLEVEL-FOM-IN NUMERIC                                      
046700          MOVE MID-ADLEVEL-FOM-IN TO WS-ADLEVEL-FOM                       
046800          MOVE '7' TO MFS-IDPFK                                           
046900          MOVE SPACE TO MFS-KDTRTYP                                       
047000       ELSE                                                               
047100          MOVE NOO                TO KEYS-SW                              
047200       END-IF                                                             
047300     END-IF                                                               
047400                                                                          
047500*                                                                         
047600*    -- CONTROL  LEVEL THRU                                               
047700*                                                                         
047800     IF MID-ADLEVEL-TOM-IN = ALL '+'                                      
047900       INSPECT MID-ADLEVEL-TOM-UT REPLACING LEADING SPACE BY ZERO         
048000       MOVE MID-ADLEVEL-TOM-UT    TO WS-ADLEVEL-TOM                       
048100     ELSE                                                                 
048200       IF MID-ADLEVEL-TOM-IN NUMERIC                                      
048300          MOVE MID-ADLEVEL-TOM-IN TO WS-ADLEVEL-TOM                       
048400          MOVE '7' TO MFS-IDPFK                                           
048500          MOVE SPACE TO MFS-KDTRTYP                                       
048600       ELSE                                                               
048700          MOVE NOO                TO KEYS-SW                              
048800       END-IF                                                             
048900     END-IF                                                               
049000                                                                          
049100*                                                                         
049200*    -- CONTROL  PLACEMENT FROM    (SEQUENCE)                             
049300*                                                                         
049400     IF MID-ADSEQ-FOM-IN = ALL '+'                                        
049500       INSPECT MID-ADSEQ-FOM-UT REPLACING LEADING SPACE BY ZERO           
049600       MOVE MID-ADSEQ-FOM-UT      TO WS-ADSEQ-FOM                         
049700     ELSE                                                                 
049800       IF MID-ADSEQ-FOM-IN NUMERIC                                        
049900          MOVE MID-ADSEQ-FOM-IN TO WS-ADSEQ-FOM                           
050000          MOVE '7' TO MFS-IDPFK                                           
050100          MOVE SPACE TO MFS-KDTRTYP                                       
050200       ELSE                                                               
050300          MOVE NOO                TO KEYS-SW                              
050400       END-IF                                                             
050500     END-IF                                                               
050600                                                                          
050700*                                                                         
050800*    -- CONTROL  PLACEMENT THRU    (SEQUENCE)                             
050900*                                                                         
051000     IF MID-ADSEQ-TOM-IN = ALL '+'                                        
051100       INSPECT MID-ADSEQ-TOM-UT REPLACING LEADING SPACE BY ZERO           
051200       MOVE MID-ADSEQ-TOM-UT      TO WS-ADSEQ-TOM                         
051300     ELSE                                                                 
051400       IF MID-ADSEQ-TOM-IN NUMERIC                                        
051500          MOVE MID-ADSEQ-TOM-IN   TO WS-ADSEQ-TOM                         
051600          MOVE '7' TO MFS-IDPFK                                           
051700          MOVE SPACE TO MFS-KDTRTYP                                       
051800       ELSE                                                               
051900          MOVE NOO                TO KEYS-SW                              
052000       END-IF                                                             
052100     END-IF                                                               
052200                                                                          
052300*                                                                         
052400*    -- CONTROL  ALL/ODD/EVEN                                             
052500*                                                                         
052600     IF MID-KDAOE-IN = ALL '+'                                            
052700       INSPECT MID-KDAOE-UT REPLACING LEADING SPACE BY 'A'                
052800       MOVE MID-KDAOE-UT          TO WS-KDAOE                             
052900     ELSE                                                                 
053000          INSPECT MID-KDAOE-IN REPLACING LEADING SPACE BY 'A'             
053100       IF MID-KDAOE-IN = ALLA OR ODD OR EVEN                              
053200          MOVE MID-KDAOE-IN       TO WS-KDAOE                             
053300          MOVE '7' TO MFS-IDPFK                                           
053400          MOVE SPACE TO MFS-KDTRTYP                                       
053500       ELSE                                                               
053600          MOVE NOO                TO KEYS-SW                              
053700       END-IF                                                             
053800     END-IF                                                               
053900                                                                          
054000*                                                                         
054100*    -- FILL MOD KEY-OUTPUT FIELDS                                        
054200*                                                                         
054300     MOVE W-IDDC-B6      TO  MOD-IDDC-UT                                  
054400     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
054500     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
054600     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
054700     MOVE WS-ADSEC-FOM   TO  MOD-ADSEC-FOM-UT                             
054800     MOVE WS-ADSEC-TOM   TO  MOD-ADSEC-TOM-UT                             
054900     MOVE WS-ADLEVEL-FOM TO  MOD-ADLEVEL-FOM-UT                           
055000     MOVE WS-ADLEVEL-TOM TO  MOD-ADLEVEL-TOM-UT                           
055100     MOVE WS-ADSEQ-FOM   TO  MOD-ADSEQ-FOM-UT                             
055200     MOVE WS-ADSEQ-TOM   TO  MOD-ADSEQ-TOM-UT                             
055300     MOVE WS-KDAOE       TO  MOD-KDAOE-UT                                 
055400                                                                          
055500* KONTROLL FÖR ATT UNDVIKA 'NOLLOR' (0) I INDATA                          
055600*    IF WS-ADGANG-FOM     = ZERO  OR                                      
055700*       WS-ADSEC-FOM      = ZERO  OR                                      
055800*       WS-ADLEVEL-FOM    = ZERO  OR                                      
055900*       WS-ADSEQ-FOM      = ZERO  OR                                      
056000*       WS-ADGANG-TOM     = ZERO  OR                                      
056100*       WS-ADSEC-TOM      = ZERO  OR                                      
056200*       WS-ADLEVEL-TOM    = ZERO  OR                                      
056300*       WS-ADSEQ-TOM      = ZERO                                          
056400*                                                                         
056500*          MOVE NOO               TO KEYS-SW                              
056600*    END-IF                                                               
056700                                                                          
056800     IF KEYS-WRONG                                                        
056900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
057000       CALL WMEDKONV USING MED-WMEDAREA                                   
057100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
057200      ELSE                                                                
057300* KONTROLL AV INTERVALL FÖR IN-DATA                                       
057400       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
057500          WS-ADSEC-FOM    >  WS-ADSEC-TOM   OR                            
057600          WS-ADLEVEL-FOM  >  WS-ADLEVEL-TOM OR                            
057700          WS-ADSEQ-FOM    >  WS-ADSEQ-TOM                                 
057800                                                                          
057900          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
058000          CALL WMEDKONV USING MED-WMEDAREA                                
058100          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
058200          MOVE NOO                 TO KEYS-SW                             
058300       END-IF                                                             
058400* KONTROLL FÖR ATT KOLLA A/O/E GENTEMOT ANGIVNA INTERVALLER               
058500       EVALUATE WS-KDAOE                                                  
058600       WHEN ALLA                                                          
058700         MOVE 1 TO WS-SECTION-STEP                                        
058800       WHEN EVEN                                                          
058900* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
059000         IF FUNCTION MOD (WS-ADSEC-FOM 2) = 0   AND                       
059100            FUNCTION MOD (WS-ADSEC-TOM 2) = 0                             
059200            MOVE 2 TO WS-SECTION-STEP                                     
059300         ELSE                                                             
059400            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
059500            CALL WMEDKONV USING MED-WMEDAREA                              
059600            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
059700            MOVE NOO               TO KEYS-SW                             
059800         END-IF                                                           
059900       WHEN ODD                                                           
060000* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
060100         IF FUNCTION MOD (WS-ADSEC-FOM 2) = 1   AND                       
060200            FUNCTION MOD (WS-ADSEC-TOM 2) = 1                             
060300            MOVE 2 TO WS-SECTION-STEP                                     
060400         ELSE                                                             
060500            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
060600            CALL WMEDKONV USING MED-WMEDAREA                              
060700            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
060800            MOVE NOO               TO KEYS-SW                             
060900         END-IF                                                           
061000       END-EVALUATE                                                       
061100     END-IF                                                               
061200                                                                          
061300     IF KEYS-WRONG                                                        
061400       PERFORM MFS-ERASE-FIELD-IN                                         
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 C-FIRST-PAGE SECTION.                                                    
061900                                                                          
062000     PERFORM MFS-ERASE-FIELD-IN                                           
062100*  SÄTTER DEFAULT VÄRDE PÅ "NUMBER OF PARTS"                              
062200          MOVE 1 TO MID-KVMPART                                           
062300          MOVE 1 TO MOD-KVMPART                                           
062400                                                                          
062500     MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOC-ATTR                         
062600*                                                                         
062700*    -- INIT SAVE KEYS                                                    
062800*                                                                         
062900     MOVE '6314'                TO SAVE-IDTRANS                           
063000     MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                          
063100     MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                            
063200     MOVE WS-ADSEC-FOM          TO SAVE-ADSEC                             
063300     MOVE WS-ADLEVEL-FOM        TO SAVE-ADLEVEL                           
063400     MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                             
063500                                                                          
063600     MOVE '002'                 TO MSGI-KDCALL                            
063700     MOVE SAVE-AREA             TO MSGI-SPAR-AREA                         
063800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
063900                                                                          
064000     MOVE INF-PRESS-PF11        TO MED-IDMFSINF                           
064100     CALL WMEDKONV USING MED-WMEDAREA                                     
064200     MOVE MED-MFSINF            TO MOD-TEMFSINF                           
064300     .                                                                    
064400     EJECT                                                                
064500 E-SAME-PAGE SECTION.                                                     
064600                                                                          
064700     IF OWN-MID OR HELP-MID                                               
064800       IF MID-KDLOC   = ALL '+' AND                                       
064900          MID-KDFREQ  = ALL '+' AND                                       
065000          MID-KDSTOR  = ALL '+' AND                                       
065100          MID-TELOC   = ALL '+' AND                                       
065200          MID-KVMPART = ALL '+'                                           
065300          PERFORM MFS-ERASE-FIELD-IN                                      
065400*  SÄTTER DEFAULT VÄRDE PÅ "NUMBER OF PARTS"                              
065500          MOVE 1 TO MID-KVMPART                                           
065600          MOVE 1 TO MOD-KVMPART                                           
065700                                                                          
065800          MOVE MFS-ADD-SET-CURSOR TO MOD-KDLOC-ATTR                       
065900        ELSE                                                              
066000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
066100         CALL WMEDKONV USING MED-WMEDAREA                                 
066200         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
066300         PERFORM EA-MID-INDATA-TO-MOD                                     
066400       END-IF                                                             
066500     ELSE                                                                 
066600       PERFORM MFS-ERASE-FIELD-IN                                         
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000 EA-MID-INDATA-TO-MOD SECTION.                                            
067100                                                                          
067200     IF MID-KDLOC NOT = ALL '+'                                           
067300        MOVE MID-KDLOC             TO MOD-KDLOC                           
067400        MOVE MFS-ADD-READ-FIELD    TO MOD-KDLOC-ATTR                      
067500      ELSE                                                                
067600        MOVE MFS-ERASE-FIELD       TO MOD-KDLOC                           
067700     END-IF                                                               
067800                                                                          
067900     IF MID-KDFREQ NOT = ALL '+'                                          
068000        MOVE MID-KDFREQ             TO MOD-KDFREQ                         
068100        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQ-ATTR                    
068200      ELSE                                                                
068300        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQ                         
068400     END-IF                                                               
068500                                                                          
068600     IF MID-KDSTOR NOT = ALL '+'                                          
068700        MOVE MID-KDSTOR             TO MOD-KDSTOR                         
068800        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTOR-ATTR                    
068900      ELSE                                                                
069000        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR                         
069100     END-IF                                                               
069200                                                                          
069300     IF MID-TELOC NOT = ALL '+'                                           
069400        MOVE MID-TELOC             TO MOD-TELOC                           
069500        MOVE MFS-ADD-READ-FIELD    TO MOD-TELOC-ATTR                      
069600      ELSE                                                                
069700        MOVE MFS-ERASE-FIELD       TO MOD-TELOC                           
069800     END-IF                                                               
069900                                                                          
070000     IF MID-KVMPART NOT = ALL '+'                                         
070100        MOVE MID-KVMPART           TO MOD-KVMPART                         
070200        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPART-ATTR                    
070300      ELSE                                                                
070400        MOVE MFS-ERASE-FIELD       TO MOD-KVMPART                         
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800 G-CHECK-INPUT SECTION.                                                   
070900                                                                          
071000     MOVE YES  TO INDATA-SW                                               
071100                                                                          
071200     IF MID-KDLOC      = ALL '+' AND                                      
071300        MID-KDFREQ     = ALL '+' AND                                      
071400        MID-KDSTOR     = ALL '+' AND                                      
071500        MID-TELOC      = ALL '+' AND                                      
071600        MID-KVMPART    = ALL '+'                                          
071700                                                                          
071800        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
071900        CALL WMEDKONV USING MED-WMEDAREA                                  
072000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
072100                                                                          
072200        PERFORM MFS-DONT-TOUCH-FIELD-OUT-IN                               
072300        MOVE NOO TO INDATA-SW                                             
072400      ELSE                                                                
072500        PERFORM GA-CHECK-INPUT-NEW                                        
072600        IF INDATA-WRONG                                                   
072700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
072800           CALL WMEDKONV USING MED-WMEDAREA                               
072900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
073000           PERFORM MFS-DONT-TOUCH-FIELD-OUT-IN                            
073100         END-IF                                                           
073200     END-IF                                                               
073300     .                                                                    
073400     EJECT                                                                
073500 GA-CHECK-INPUT-NEW SECTION.                                              
073600                                                                          
073700*                                                                         
073800*    -- CONTROL  ON MULTIPLE PART                                         
073900*                                                                         
074000      IF MID-KVMPART NOT = ALL '+'                                        
074100        IF MID-KVMPART  NOT NUMERIC                                       
074200          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPART-ATTR                    
074300          MOVE NOO                 TO INDATA-SW                           
074400         ELSE                                                             
074500          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPART-ATTR                       
074600        END-IF                                                            
074700       ELSE                                                               
074800        MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPART-ATTR                      
074900        MOVE NOO                 TO INDATA-SW                             
075000      END-IF                                                              
075100                                                                          
075200*                                                                         
075300*    -- CONTROL  ON STORAGE CODE                                          
075400*                                                                         
075500     IF MID-KDSTOR   = ALL '+'                                            
075600        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDSTOR-ATTR                       
075700        MOVE NOO                 TO INDATA-SW                             
075800      ELSE                                                                
075900        MOVE W-IDDC-B6           TO W-6315-IDDC                           
076000        MOVE MID-KDSTOR          TO W-KDSTOR                              
076100        PERFORM  IMS-GU-6316                                              
076200        IF SEGMENT-MISSING                                                
076300           MOVE NOO                     TO INDATA-SW                      
076400           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                
076500         ELSE                                                             
076600           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                
076700        END-IF                                                            
076800     END-IF                                                               
076900*                                                                         
077000*    -- CONTROL  ON FREQUENCY CODE                                        
077100*                                                                         
077200                                                                          
077300     IF MID-KDFREQ   = ALL '+'                                            
077400        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDFREQ-ATTR                       
077500        MOVE NOO                 TO INDATA-SW                             
077600      ELSE                                                                
077700        IF MID-KDFREQ   NUMERIC                                           
077800           MOVE W-IDDC-B6           TO W-6313-IDDC                        
077900           MOVE MID-KDFREQ          TO W-KDFREQ                           
078000           PERFORM  IMS-GU-6314                                           
078100           IF SEGMENT-MISSING                                             
078200              MOVE NOO                     TO INDATA-SW                   
078300              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR             
078400            ELSE                                                          
078500              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR             
078600           END-IF                                                         
078700         ELSE                                                             
078800           MOVE NOO                     TO INDATA-SW                      
078900           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                
079000        END-IF                                                            
079100     END-IF                                                               
079200                                                                          
079300*                                                                         
079400*    -- CONTROL  ON LOCATION TYPE                                         
079500*                                                                         
079600     IF MID-KDLOC    = ALL '+'                                            
079700        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOC-ATTR                      
079800        MOVE NOO                   TO INDATA-SW                           
079900      ELSE                                                                
080000        IF MID-KDLOC  =  PRIME OR BUFFER OR MIXED                         
080100           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOC-ATTR                   
080200         ELSE                                                             
080300           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOC-ATTR                   
080400           MOVE NOO                   TO INDATA-SW                        
080500        END-IF                                                            
080600     END-IF                                                               
080700*                                                                         
080800*    -- CONTROL  ON MULTIPLE PART                                         
080900*                                                                         
081000     MOVE MFS-ALPHA-FIELD-OK          TO MOD-TELOC-ATTR                   
081100                                                                          
081200     .                                                                    
081300     EJECT                                                                
081400 H-UPDATE SECTION.                                                        
081500                                                                          
081600     PERFORM HA-INIT                                                      
081700     PERFORM HB-UPDATE                                                    
081800     PERFORM HC-CLOSE                                                     
081900                                                                          
082000     .                                                                    
082100     EJECT                                                                
082200 HA-INIT SECTION.                                                         
082300                                                                          
082400     MOVE W-IDDC-B6          TO W-LOC-IDDC                                
082500                                LOC-IDDC                                  
082600     MOVE SAVE-ADLAGOMR        TO W-LOC-ADLAGOMR                          
082700                                LOC-ADLAGOMR                              
082800     MOVE SAVE-ADGANG          TO WS-ADGANG                               
082900     MOVE SAVE-ADSEC           TO WS-ADSEC                                
083000     MOVE SAVE-ADLEVEL         TO WS-ADLEVEL                              
083100     MOVE SAVE-ADSEQ           TO WS-ADSEQ                                
083200                                                                          
083300     MOVE MID-KDLOC          TO LOC-KDLOC   MOD-KDLOC                     
083400     MOVE MID-KDFREQ         TO LOC-KDFREQ  MOD-KDFREQ                    
083500     MOVE MID-KDSTOR         TO LOC-KDSTOR  MOD-KDSTOR                    
083600     MOVE MID-KVMPART        TO LOC-KVMPART MOD-KVMPART                   
083700     IF MID-TELOC  NOT = ALL '+'                                          
083800        MOVE MID-TELOC       TO LOC-TELOC   MOD-TELOC                     
083900     ELSE                                                                 
084000        MOVE SPACES          TO LOC-TELOC   MOD-TELOC                     
084100     END-IF                                                               
084200     MOVE ZEROES             TO IO-COUNT                                  
084300     .                                                                    
084400     EJECT                                                                
084500 HB-UPDATE SECTION.                                                       
084600                                                                          
084700     PERFORM UNTIL WS-ADGANG    > WS-ADGANG-TOM OR                        
084800                   IO-COUNT     > MAX-IO-COUNT  OR                        
084900                   SEGMENT-FOUND-EXISTS                                   
085000         MOVE    WS-ADGANG       TO LOC-ADGANG                            
085100                                    W-LOC-ADGANG                          
085200*                                                                         
085300         PERFORM UNTIL WS-ADSEC    > WS-ADSEC-TOM OR                      
085400                       IO-COUNT    > MAX-IO-COUNT OR                      
085500                       SEGMENT-FOUND-EXISTS                               
085600             MOVE    WS-ADSEC       TO W-LOC-ADSEC                        
085700*                                                                         
085800             PERFORM UNTIL WS-ADLEVEL > WS-ADLEVEL-TOM  OR                
085900                           IO-COUNT      > MAX-IO-COUNT OR                
086000                           SEGMENT-FOUND-EXISTS                           
086100                 MOVE    WS-ADLEVEL     TO W-LOC-ADLEVEL                  
086200*                                                                         
086300                 PERFORM UNTIL WS-ADSEQ    > WS-ADSEQ-TOM OR              
086400                               IO-COUNT    > MAX-IO-COUNT OR              
086500                               SEGMENT-FOUND-EXISTS                       
086600*                                                                         
086700                     MOVE  WS-ADSEQ         TO   W-LOC-ADSEQ              
086800                     MOVE  W-LOC-ADPLATS    TO   LOC-ADPLATS              
086900                     PERFORM IMS-ISRT-LOCA-LOC                            
087000                     IF SEGMENT-FOUND                                     
087100                        ADD +1     TO IO-COUNT                            
087200                        ADD +1     TO WS-ADSEQ                            
087300                     END-IF                                               
087400                 END-PERFORM                                              
087500                 IF IO-COUNT  <= MAX-IO-COUNT AND                         
087600                    SEGMENT-FOUND                                         
087700                    MOVE WS-ADSEQ-FOM TO WS-ADSEQ                         
087800                    ADD +1            TO WS-ADLEVEL                       
087900                 END-IF                                                   
088000*                                                                         
088100             END-PERFORM                                                  
088200             IF IO-COUNT  <= MAX-IO-COUNT AND                             
088300                SEGMENT-FOUND                                             
088400                MOVE WS-ADLEVEL-FOM   TO WS-ADLEVEL                       
088500* WS-SECTION-STEP INNEHÅLLER 1 OM ALLA SEKTIONER SKALL SKAPAS.            
088600* OM ENDAST DEN UDDA ELLER JÄMNA SIDAN AV GÅNGEN SKALL SKAPAS             
088700* INNEHÅLLER WS-SECTION-STEP 2.                                           
088800                ADD WS-SECTION-STEP   TO WS-ADSEC                         
088900             END-IF                                                       
089000*                                                                         
089100         END-PERFORM                                                      
089200         IF IO-COUNT  <= MAX-IO-COUNT AND                                 
089300            SEGMENT-FOUND                                                 
089400            MOVE WS-ADSEC-FOM         TO WS-ADSEC                         
089500            ADD +1                    TO WS-ADGANG                        
089600         END-IF                                                           
089700     END-PERFORM                                                          
089800                                                                          
089900                                                                          
090000     .                                                                    
090100     EJECT                                                                
090200 HC-CLOSE SECTION.                                                        
090300                                                                          
090400     IF SEGMENT-FOUND-EXISTS                                              
090500           MOVE W-LOC-IDDC            TO MED-IDDC                         
090600           MOVE W-LOC-ADLAGOMR        TO MED-ADLAGOMR                     
090700           MOVE W-LOC-ADGANG          TO MED-ADGANG                       
090800           MOVE W-LOC-ADSEC           TO MED-ADSEC                        
090900           MOVE W-LOC-ADLEVEL         TO MED-ADLEVEL                      
091000           MOVE W-LOC-ADSEQ           TO MED-ADSEQ                        
091100           MOVE MED-1                 TO MOD-TEMFSFEL                     
091200      ELSE                                                                
091300        IF WS-ADGANG > WS-ADGANG-TOM                                      
091400                                                                          
091500           MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                     
091600           CALL WMEDKONV USING MED-WMEDAREA                               
091700           MOVE MED-MFSINF            TO MOD-TEMFSINF                     
091800*                                                                         
091900*    -- INIT SAVE KEYS                                                    
092000*                                                                         
092100           MOVE '6314'                TO SAVE-IDTRANS                     
092200           MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                    
092300           MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                      
092400           MOVE WS-ADSEC-FOM          TO SAVE-ADSEC                       
092500           MOVE WS-ADLEVEL-FOM        TO SAVE-ADLEVEL                     
092600           MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                       
092700                                                                          
092800           MOVE '002'                 TO MSGI-KDCALL                      
092900           MOVE SAVE-AREA             TO MSGI-SPAR-AREA                   
093000                                                                          
093100           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
093200         ELSE                                                             
093300           MOVE YES                   TO RESTART-SW                       
093400           MOVE MID-W6I31401          TO 6314-UT-MID-W6I31401             
093500           MOVE W-LOC-ADLAGOMR        TO SAVE-ADLAGOMR                    
093600           MOVE WS-ADGANG             TO SAVE-ADGANG                      
093700           MOVE WS-ADSEC              TO SAVE-ADSEC                       
093800           MOVE WS-ADLEVEL            TO SAVE-ADLEVEL                     
093900           MOVE WS-ADSEQ              TO SAVE-ADSEQ                       
094000           MOVE '6314'                TO SAVE-IDTRANS                     
094100                                                                          
094200           MOVE '002'      TO MSGI-KDCALL                                 
094300           MOVE SAVE-AREA  TO MSGI-SPAR-AREA                              
094400           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
094500        END-IF                                                            
094600      END-IF                                                              
094700     .                                                                    
094800     EJECT                                                                
094900 MFS-INIT-KEY-FIELD-IN SECTION.                                           
095000                                                                          
095100*    --- ALL INPUT KEY FIELDS                                             
095200                                                                          
095300     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
095400                               MOD-ADLAGOMR-IN                            
095500                               MOD-ADGANG-FOM-IN                          
095600                               MOD-ADGANG-TOM-IN                          
095700                               MOD-ADSEC-FOM-IN                           
095800                               MOD-ADSEC-TOM-IN                           
095900                               MOD-ADLEVEL-FOM-IN                         
096000                               MOD-ADLEVEL-TOM-IN                         
096100                               MOD-ADSEQ-FOM-IN                           
096200                               MOD-ADSEQ-TOM-IN                           
096300                               MOD-KDAOE-IN                               
096400     .                                                                    
096500     EJECT                                                                
096600 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
096700                                                                          
096800*    --- ALL OUTPUT KEY FIELDS                                            
096900                                                                          
097000     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
097100     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
097200                               MOD-ADGANG-FOM-UT                          
097300                               MOD-ADGANG-TOM-UT                          
097400                               MOD-ADSEC-FOM-UT                           
097500                               MOD-ADSEC-TOM-UT                           
097600                               MOD-ADLEVEL-FOM-UT                         
097700                               MOD-ADLEVEL-TOM-UT                         
097800                               MOD-ADSEQ-FOM-UT                           
097900                               MOD-ADSEQ-TOM-UT                           
098000                               MOD-KDAOE-UT                               
098100     .                                                                    
098200     EJECT                                                                
098300 MFS-ERASE-FIELD-IN SECTION.                                              
098400                                                                          
098500*    --- ALL INPUT DATA FIELDS                                            
098600     MOVE MFS-ERASE-FIELD    TO MOD-KDLOC                                 
098700                                MOD-KDFREQ                                
098800                                MOD-KDSTOR                                
098900                                MOD-TELOC                                 
099000                                MOD-KVMPART                               
099100     .                                                                    
099200     EJECT                                                                
099300 MFS-DONT-TOUCH-FIELD-OUT-IN  SECTION.                                    
099400                                                                          
099500*    --- ALL OUTPUT DATA FIELDS                                           
099600     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
099700                                    MOD-KDFREQ                            
099800                                    MOD-KDSTOR                            
099900                                    MOD-TELOC                             
100000                                    MOD-KVMPART                           
100100     .                                                                    
100200     EJECT                                                                
100300* --- IMS SECTIONS ---                                                    
100400     SKIP3                                                                
100500 IMS-GET-MSG SECTION.                                                     
100600                                                                          
100700     MOVE '  QC' TO GOOD-STATUSCODES                                      
100800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
100900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101000     PERFORM IMS-STATUSCHECK                                              
101100     .                                                                    
101200     SKIP3                                                                
101300 IMS-INSERT-MSG SECTION.                                                  
101400                                                                          
101500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
101600     MOVE SPACE TO GOOD-STATUSCODES                                       
101700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
101800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101900     PERFORM IMS-STATUSCHECK                                              
102000     .                                                                    
102100     EJECT                                                                
102200 IMS-ISRT-ALT-MSG-6314  SECTION.                                          
102300     MOVE SPACE  TO GOOD-STATUSCODES                                      
102400     CALL  CBLTDLI  USING ISRT ALT6314-PCB P-TO-P-SW                      
102500     MOVE ALT6314-STATUS-CODE TO STATUS-WS                                
102600     PERFORM IMS-STATUSCHECK                                              
102700     .                                                                    
102800     SKIP3                                                                
102900 IMS-GU-6314 SECTION.                                                     
103000                                                                          
103100     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
103200          DELIMITED BY SIZE INTO SSA1                                     
103300     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
103400          DELIMITED BY SIZE INTO SSA2                                     
103500     MOVE '  GE' TO GOOD-STATUSCODES                                      
103600     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
103700     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
103800     PERFORM IMS-STATUSCHECK                                              
103900     .                                                                    
104000     SKIP3                                                                
104100 IMS-GU-6316 SECTION.                                                     
104200                                                                          
104300     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
104400          DELIMITED BY SIZE INTO SSA1                                     
104500     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
104600          DELIMITED BY SIZE INTO SSA2                                     
104700     MOVE '  GE' TO GOOD-STATUSCODES                                      
104800     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
104900     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
105000     PERFORM IMS-STATUSCHECK                                              
105100     .                                                                    
105200     SKIP3                                                                
105300 IMS-ISRT-LOCA-LOC SECTION.                                               
105400                                                                          
105500     MOVE 'WLLOCA01 ' TO SSA1                                             
105600     MOVE '  II' TO GOOD-STATUSCODES                                      
105700     CALL CBLTDLI USING ISRT LOCA-PCB LOC-WDJ801 SSA1                     
105800     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
105900     PERFORM IMS-STATUSCHECK                                              
106000     .                                                                    
106100     EJECT                                                                
106200                                                                          
106300 IMS-GU-WDB601    SECTION.                                                
106400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
106500          DELIMITED BY SIZE INTO SSA1                                     
106600     MOVE '  GE' TO GOOD-STATUSCODES                                      
106700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
106800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
106900     PERFORM IMS-STATUSCHECK                                              
107000     .                                                                    
107100     EJECT                                                                
107200 IMS-STATUSCHECK SECTION.                                                 
107300                                                                          
107400     SET STATUS-IX TO 1                                                   
107500     SEARCH GOOD-STATUS                                                   
107600       AT END                                                             
107700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
107800         DELIMITED BY SIZE INTO ERROR-TEXT                                
107900         CALL FELLOG                                                      
108000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
108100         CONTINUE                                                         
108200     END-SEARCH                                                           
108300     .                                                                    
