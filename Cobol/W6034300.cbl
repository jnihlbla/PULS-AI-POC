000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6034300.                                                
000400 AUTHOR.         MARTIEN HOMPES.                                          
000500 DATE-WRITTEN.   97/02/06.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        MAINTENANCE LOCATION                                             
001000*                                                                         
001100*        THE PROGRAM READS     WL6313 (WDGX)                              
001200*        THE PROGRAM READS     WL6315 (WDGX)                              
001300*        THE PROGRAM UPDATES   WLLOCA (WDJ8)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W6T343                                              
001700*        MID:         W6I34301                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W6O34301                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6034300'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003800 77  WS-ADLAGOMR                 PIC 9(3)   VALUE ZERO.                   
003900 77  WS-ADGANG                   PIC 9(3)   VALUE ZERO.                   
004000*    --- WORK FIELDS FOR LOCATION IN WORK AREA FOR IMS                    
004100 77  WS-KDANDR                   PIC X(1)   VALUE SPACE.                  
004200                                                                          
004300 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004400     88  INDATA-OK                           VALUE 'Y'.                   
004500     88  INDATA-WRONG                        VALUE 'N'.                   
004600                                                                          
004700 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004800     88  KEYS-OK                             VALUE 'Y'.                   
004900     88  KEYS-WRONG                          VALUE 'N'.                   
005000                                                                          
005100 77  NEW                         PIC X        VALUE 'N'.                  
005200 77  DEL                         PIC X        VALUE 'D'.                  
005300 77  CHG                         PIC X        VALUE 'C'.                  
005400 77  ENQ                         PIC X        VALUE 'E'.                  
005500                                                                          
005600 77  PRIME                       PIC X        VALUE 'P'.                  
005700 77  BUFFER                      PIC X        VALUE 'R'.                  
005800 77  MIXED                       PIC X        VALUE 'M'.                  
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  OWN-MID                             VALUE '6343'.                
006200     88  GOOD-MID                            VALUE '6343'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*      --- VALID IDDC CODES                                               
006600*                                                                         
006700*01    -COPY WWDC99                                                       
006800       EJECT                                                              
006900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007000 01  GENERAL-SUBPROGRAMS.                                                 
007100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     EJECT                                                                
007600*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007700*01 -COPY WMEDAREA                                                        
007800     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
008000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008400     03  IR-ALREADY-REG          PIC X(3)    VALUE '165'.                 
008500     03  LOCATION-ADDED          PIC X(3)    VALUE '260'.                 
008600     03  LOCATION-CHANGED        PIC X(3)    VALUE '261'.                 
008700     03  LOCATION-DELETED        PIC X(3)    VALUE '262'.                 
008800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008900     03  LOC-MISSING             PIC X(3)    VALUE '706'.                 
009000     EJECT                                                                
009100*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009200*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009400     SKIP3                                                                
009500*01 -COPY WMSGINIT                                                        
009600     EJECT                                                                
009700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010000     SKIP3                                                                
010100*01  MID -COPY W6I34301                                                   
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010400     SKIP3                                                                
010500*01  -COPY WMSGAREA                                                       
010600     EJECT                                                                
010700     03  MOD REDEFINES MSG-AREA.                                          
010800*      05  -COPY W6O34301                                                 
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011100     SKIP3                                                                
011200*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011500*                                                                         
011600     EJECT                                                                
011700 01  WORK-AREA.                                                           
011800     03 WS-ADPLATS.                                                       
011900        05  WS-ADSEC11            PIC 9(3)   VALUE ZERO.                  
012000        05  WS-ADLEVEL11          PIC 9(1)   VALUE ZERO.                  
012100        05  WS-ADSEQ              PIC 9(1)   VALUE ZERO.                  
012200     EJECT                                                                
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400     SKIP3                                                                
012500 01  KEYS-TO-DLI.                                                         
012600     03  W-WDGXKEY-6313-X.                                                
012700          05 W-6313-IDHTYP       PIC X(4)    VALUE '6313'.                
012800          05 W-6313-IDDC         PIC X(2)    VALUE SPACE.                 
012900          05 W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
013000                                                                          
013100     03  W-WDGXKEY-6314-X.                                                
013200         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
013300                                                                          
013400     03  W-WDGXKEY-6315-X.                                                
013500          05 W-6315-IDHTYP      PIC X(4)    VALUE '6315'.                 
013600          05 W-6315-IDDC        PIC X(2)    VALUE SPACE.                  
013700          05 W-6315-LOWVALUE    PIC X(24)   VALUE LOW-VALUE.              
013800                                                                          
013900     03  W-WDGXKEY-6316-X.                                                
014000         05  W-KDSTOR           PIC X(3)    VALUE SPACE.                  
014100                                                                          
014200     03  W-WDJ8KEY-X.                                                     
014300         05  W-LOC-IDDC          PIC X(2)   VALUE SPACE.                  
014400         05  W-LOC-ADLAGOMR      PIC 9(2)   VALUE ZERO.                   
014500         05  W-LOC-ADGANG        PIC 9(2)   VALUE ZERO.                   
014600         05  W-LOC-ADPLATS.                                               
014700             07 W-LOC-ADSEC11    PIC 9(3).                                
014800             07 W-LOC-ADLEVEL11  PIC 9(1).                                
014900             07 W-LOC-ADSEQ      PIC 9(1).                                
015000     SKIP2                                                                
015100*    --- STATUS-KOD FRÅN IMS                                              
015200 01  STATUS-WS                   PIC XX.                                  
015300     88  SEGMENT-FOUND                       VALUE '  '.                  
015400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015600     SKIP2                                                                
015700 01  GOOD-STATUSCODES.                                                    
015800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015900     SKIP3                                                                
016000 01  SSA1                        PIC X(64).                               
016100 01  SSA2                        PIC X(64).                               
016200     EJECT                                                                
016300*    --- IMS FUNCTION CODES                                               
016400*01  -COPY W0003                                                          
016500     EJECT                                                                
016600*    ---  DLI INPUT-OUTPUT AREA                                           
016700 01  FILLER         PIC X(20) VALUE 'WL631301-AREA'.                      
016800 01  WL631301-AREA.                                                       
016900*    03  -COPY WDGX6313                                                   
017000     EJECT                                                                
017100 01  FILLER         PIC X(20) VALUE 'WL631311-AREA'.                      
017200 01  WL631311-AREA.                                                       
017300*    03  -COPY WDGX6314                                                   
017400                                                                          
017500 01  FILLER         PIC X(20) VALUE 'WL631501-AREA'.                      
017600 01  WL631501-AREA.                                                       
017700*    03  -COPY WDGX6315                                                   
017800     EJECT                                                                
017900 01  FILLER         PIC X(20) VALUE 'WL631511-AREA'.                      
018000 01  WL631511-AREA.                                                       
018100*    03  -COPY WDGX6316                                                   
018200                                                                          
018300 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
018400 01  DLI-IO-WLLOCA01.                                                     
018500*    03  -COPY WDJ801                                                     
018600     EJECT                                                                
018700 LINKAGE SECTION.                                                         
018800*01  -COPY W0009   -PRE MSG-                                              
018900*01  -COPY W0008   -PRE USEA-                                             
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE 6313-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008  -PRE 6315-                                              
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800*01  -COPY W0008  -PRE LOCA-                                              
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 6313-PCB 6315-PCB             
020200     LOCA-PCB.                                                            
020300 MAIN SECTION.                                                            
020400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 6313-PCB 6315-PCB             
020500     LOCA-PCB.                                                            
020600                                                                          
020700     PERFORM IMS-GET-MSG                                                  
020800     IF SEGMENT-FOUND                                                     
020900       PERFORM A-INIT                                                     
021000       IF GOOD-MID OR HELP-MID                                            
021100          PERFORM B-CHECK-KEYS                                            
021200          IF KEYS-OK                                                      
021300             IF MFS-UPDATE                                                
021400                PERFORM G-CHECK-INPUT                                     
021500                IF INDATA-OK                                              
021600                   PERFORM H-UPDATE                                       
021700                END-IF                                                    
021800              ELSE                                                        
021900                IF MFS-FIRST                                              
022000                   PERFORM C-FIRST-PAGE                                   
022100                 ELSE                                                     
022200                   PERFORM E-SAME-PAGE                                    
022300                END-IF                                                    
022400                PERFORM F-READ-SHOW-INFO                                  
022500             END-IF                                                       
022600          END-IF                                                          
022700       END-IF                                                             
022800       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34301 + 4                      
022900       PERFORM IMS-INSERT-MSG                                             
023000     END-IF                                                               
023100                                                                          
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 A-INIT SECTION.                                                          
023700                                                                          
023800     IF MSG-DOUBLE-TRANSACTIONS                                           
023900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I34301                 
024000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024200     ELSE                                                                 
024300       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I34301                  
024400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
024500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024600     END-IF                                                               
024700                                                                          
024800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025100                                                                          
025200     MOVE LOW-VALUE TO MSG-AREA                                           
025300     MOVE 'W6O343N1' TO MFS-IDMOD                                         
025400     MOVE '6343'     TO MOD-IDTRANS                                       
025500     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
025600                                                                          
025700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025800     MOVE '001'             TO MSGI-KDCALL                                
025900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026100     MOVE '6343'            TO MSGI-IDTRANS                               
026200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026300                                                                          
026400     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
026500     MOVE MSGI-IDDC         TO WS-IDDC                                    
026600                                                                          
026700     IF GOOD-MID OR HELP-MID                                              
026800       CONTINUE                                                           
026900     ELSE                                                                 
027000       MOVE SPACE TO MFS-KDTRTYP                                          
027100       MOVE '7' TO MFS-IDPFK                                              
027200       PERFORM MFS-INIT-KEY-FIELD-IN                                      
027300       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
027400       PERFORM MFS-ERASE-FIELD-IN                                         
027500       PERFORM MFS-ERASE-FIELD-OUT                                        
027600     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 B-CHECK-KEYS SECTION.                                                    
028000                                                                          
028100                                                                          
028200     MOVE YES             TO KEYS-SW                                      
028300                                                                          
028400     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
028500                             MOD-ADLAGOMR-IN                              
028600                             MOD-ADGANG-IN                                
028700                             MOD-ADSEC11-IN                               
028800                             MOD-ADLEVEL11-IN                             
028900                             MOD-ADSEQ-IN                                 
029000                             MOD-KDANDR-IN                                
029100*                                                                         
029200*    -- CONTROL  ON WAREHOUSE                                             
029300*                                                                         
029400     IF CDC                                                               
029500       CONTINUE                                                           
029600     ELSE                                                                 
029700       MOVE NOO                 TO KEYS-SW                                
029800     END-IF                                                               
029900*                                                                         
030000*    -- CONTROL  ON AREA                                                  
030100*                                                                         
030200     IF MID-ADLAGOMR-IN = ALL '+'                                         
030300       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
030400       MOVE MID-ADLAGOMR-UT TO WS-ADLAGOMR                                
030500     ELSE                                                                 
030600       IF MID-ADLAGOMR-IN NUMERIC                                         
030700          MOVE    '7'          TO MFS-IDPFK                               
030800          MOVE    SPACE        TO MFS-KDTRTYP                             
030900          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
031000       ELSE                                                               
031100          MOVE NOO             TO KEYS-SW                                 
031200       END-IF                                                             
031300     END-IF                                                               
031400*                                                                         
031500*    -- CONTROL  ON AISLE                                                 
031600*                                                                         
031700     IF MID-ADGANG-IN = ALL '+'                                           
031800       INSPECT MID-ADGANG-UT REPLACING LEADING SPACE BY ZERO              
031900       MOVE MID-ADGANG-UT   TO WS-ADGANG                                  
032000     ELSE                                                                 
032100       IF MID-ADGANG-IN NUMERIC                                           
032200          MOVE    '7'          TO MFS-IDPFK                               
032300          MOVE    SPACE        TO MFS-KDTRTYP                             
032400          MOVE MID-ADGANG-IN   TO WS-ADGANG                               
032500       ELSE                                                               
032600          MOVE NOO             TO KEYS-SW                                 
032700       END-IF                                                             
032800     END-IF                                                               
032900*                                                                         
033000*    -- CONTROL  ON SECTION                                               
033100*                                                                         
033200     IF MID-ADSEC11-IN = ALL '+'                                          
033300       INSPECT MID-ADSEC11-UT REPLACING LEADING SPACE BY ZERO             
033400       MOVE MID-ADSEC11-UT  TO WS-ADSEC11                                 
033500     ELSE                                                                 
033600       IF MID-ADSEC11-IN NUMERIC                                          
033700          MOVE    '7'          TO MFS-IDPFK                               
033800          MOVE    SPACE        TO MFS-KDTRTYP                             
033900          MOVE MID-ADSEC11-IN  TO WS-ADSEC11                              
034000       ELSE                                                               
034100          MOVE NOO             TO KEYS-SW                                 
034200       END-IF                                                             
034300     END-IF                                                               
034400                                                                          
034500*                                                                         
034600*    -- CONTROL  ON LEVEL                                                 
034700*                                                                         
034800     IF MID-ADLEVEL11-IN = ALL '+'                                        
034900       INSPECT MID-ADLEVEL11-UT REPLACING LEADING SPACE BY ZERO           
035000       MOVE MID-ADLEVEL11-UT TO WS-ADLEVEL11                              
035100     ELSE                                                                 
035200       IF MID-ADLEVEL11-IN NUMERIC                                        
035300          MOVE    '7'          TO MFS-IDPFK                               
035400          MOVE    SPACE        TO MFS-KDTRTYP                             
035500          MOVE MID-ADLEVEL11-IN TO WS-ADLEVEL11                           
035600       ELSE                                                               
035700          MOVE NOO             TO KEYS-SW                                 
035800       END-IF                                                             
035900     END-IF                                                               
036000                                                                          
036100*                                                                         
036200*    -- CONTROL  ON PLACE                                                 
036300*                                                                         
036400     IF MID-ADSEQ-IN = ALL '+'                                            
036500       INSPECT MID-ADSEQ-UT REPLACING LEADING SPACE BY ZERO               
036600       MOVE MID-ADSEQ-UT    TO WS-ADSEQ                                   
036700     ELSE                                                                 
036800       IF MID-ADSEQ-IN NUMERIC                                            
036900          MOVE    '7'          TO MFS-IDPFK                               
037000          MOVE    SPACE        TO MFS-KDTRTYP                             
037100          MOVE MID-ADSEQ-IN    TO WS-ADSEQ                                
037200       ELSE                                                               
037300          MOVE NOO             TO KEYS-SW                                 
037400       END-IF                                                             
037500     END-IF                                                               
037600                                                                          
037700*                                                                         
037800*    -- CONTROL  ON ACTION                                                
037900*                                                                         
038000     IF MID-KDANDR-IN = ALL '+'                                           
038100       MOVE MID-KDANDR-UT      TO WS-KDANDR                               
038200     ELSE                                                                 
038300       MOVE MID-KDANDR-IN      TO WS-KDANDR                               
038400       IF MID-KDANDR-IN = NEW OR DEL OR CHG OR ENQ                        
038500          MOVE    '7'          TO MFS-IDPFK                               
038600          MOVE    SPACE        TO MFS-KDTRTYP                             
038700       ELSE                                                               
038800          MOVE NOO             TO KEYS-SW                                 
038900       END-IF                                                             
039000     END-IF                                                               
039100*                                                                         
039200*    -- FILL MOD KEY-OUTPUT FIELDS                                        
039300*                                                                         
039400     MOVE WS-IDDC      TO  MOD-IDDC-UT                                    
039500     MOVE WS-ADLAGOMR  TO  MOD-ADLAGOMR-UT                                
039600     MOVE WS-ADGANG    TO  MOD-ADGANG-UT                                  
039700     MOVE WS-ADSEC11   TO  MOD-ADSEC11-UT                                 
039800     MOVE WS-ADLEVEL11 TO  MOD-ADLEVEL11-UT                               
039900     MOVE WS-ADSEQ     TO  MOD-ADSEQ-UT                                   
040000     MOVE WS-KDANDR    TO  MOD-KDANDR-UT                                  
040100                                                                          
040200*                                                                         
040300*    -- CHECK  LOCATION ON DATABASE                                       
040400*                                                                         
040500     IF KEYS-WRONG                                                        
040600        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
040700        CALL WMEDKONV USING MED-WMEDAREA                                  
040800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
040900       ELSE                                                               
041000        MOVE WS-IDDC      TO  W-LOC-IDDC                                  
041100        MOVE WS-ADLAGOMR  TO  W-LOC-ADLAGOMR                              
041200        MOVE WS-ADGANG    TO  W-LOC-ADGANG                                
041300        MOVE WS-ADSEC11   TO  W-LOC-ADSEC11                               
041400        MOVE WS-ADLEVEL11 TO  W-LOC-ADLEVEL11                             
041500        MOVE WS-ADSEQ     TO  W-LOC-ADSEQ                                 
041600                                                                          
041700        PERFORM  IMS-GHU-LOCA                                             
041800                                                                          
041900        IF WS-KDANDR = NEW                                                
042000           IF SEGMENT-FOUND                                               
042100              MOVE IR-ALREADY-REG TO MED-IDMFSFEL                         
042200              CALL WMEDKONV USING MED-WMEDAREA                            
042300              MOVE MED-MFSFEL     TO MOD-TEMFSFEL                         
042400              MOVE NOO            TO KEYS-SW                              
042500           END-IF                                                         
042600        END-IF                                                            
042700                                                                          
042800        IF WS-KDANDR = ENQ OR DEL OR CHG                                  
042900           IF SEGMENT-MISSING                                             
043000              MOVE LOC-MISSING   TO MED-IDMFSFEL                          
043100              CALL WMEDKONV USING MED-WMEDAREA                            
043200              MOVE MED-MFSFEL    TO MOD-TEMFSFEL                          
043300              MOVE NOO           TO KEYS-SW                               
043400           END-IF                                                         
043500        END-IF                                                            
043600     END-IF                                                               
043700                                                                          
043800                                                                          
043900     IF KEYS-WRONG                                                        
044000       PERFORM MFS-ERASE-FIELD-IN                                         
044100       PERFORM MFS-ERASE-FIELD-OUT                                        
044200     END-IF                                                               
044300     .                                                                    
044400     EJECT                                                                
044500 C-FIRST-PAGE SECTION.                                                    
044600                                                                          
044700     PERFORM MFS-ERASE-FIELD-IN                                           
044800                                                                          
044900     IF WS-KDANDR = NEW                                                   
045000        MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOCU-ATTR                     
045100        MOVE '01'                  TO MOD-KVMPARTU                        
045200        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPARTU-ATTR                   
045300     END-IF                                                               
045400                                                                          
045500     IF WS-KDANDR = CHG                                                   
045600        MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOCU-ATTR                     
045700     END-IF                                                               
045800                                                                          
045900     IF WS-KDANDR = DEL                                                   
046000        MOVE 'N'                   TO MOD-FLSVARU                         
046100        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSVARU-ATTR                    
046200     END-IF                                                               
046300                                                                          
046400     IF WS-KDANDR = NEW OR CHG OR DEL                                     
046500        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                               
046600        CALL WMEDKONV USING MED-WMEDAREA                                  
046700        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
046800     END-IF                                                               
046900                                                                          
047000     .                                                                    
047100     EJECT                                                                
047200 E-SAME-PAGE SECTION.                                                     
047300                                                                          
047400     IF OWN-MID OR HELP-MID                                               
047500       IF MID-KDLOCU   = ALL '+' AND                                      
047600          MID-KDFREQU  = ALL '+' AND                                      
047700          MID-KDSTORU  = ALL '+' AND                                      
047800          MID-TELOCU   = ALL '+' AND                                      
047900          MID-KVMPARTU = ALL '+' AND                                      
048000          MID-FLSVARU  = ALL '+'                                          
048100         PERFORM MFS-ERASE-FIELD-IN                                       
048200       ELSE                                                               
048300         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
048400         CALL WMEDKONV USING MED-WMEDAREA                                 
048500         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
048600         PERFORM EA-MID-INDATA-TO-MOD                                     
048700       END-IF                                                             
048800     ELSE                                                                 
048900       PERFORM MFS-ERASE-FIELD-IN                                         
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 EA-MID-INDATA-TO-MOD SECTION.                                            
049400                                                                          
049500     IF MID-KDLOCU NOT = ALL '+'                                          
049600        MOVE MID-KDLOCU            TO MOD-KDLOCU                          
049700        MOVE MFS-ADD-READ-FIELD    TO MOD-KDLOCU-ATTR                     
049800      ELSE                                                                
049900        MOVE MFS-ERASE-FIELD       TO MOD-KDLOCU                          
050000     END-IF                                                               
050100                                                                          
050200     IF MID-KDFREQU NOT = ALL '+'                                         
050300        MOVE MID-KDFREQU            TO MOD-KDFREQU                        
050400        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQU-ATTR                   
050500      ELSE                                                                
050600        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQU                        
050700     END-IF                                                               
050800                                                                          
050900     IF MID-KDSTORU NOT = ALL '+'                                         
051000        MOVE MID-KDSTORU            TO MOD-KDSTORU                        
051100        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTORU-ATTR                   
051200      ELSE                                                                
051300        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR                         
051400     END-IF                                                               
051500                                                                          
051600     IF MID-TELOCU NOT = ALL '+'                                          
051700        MOVE MID-TELOCU            TO MOD-TELOCU                          
051800        MOVE MFS-ADD-READ-FIELD    TO MOD-TELOCU-ATTR                     
051900      ELSE                                                                
052000        MOVE MFS-ERASE-FIELD       TO MOD-TELOCU                          
052100     END-IF                                                               
052200                                                                          
052300     IF MID-KVMPARTU NOT = ALL '+'                                        
052400        MOVE MID-KVMPARTU          TO MOD-KVMPARTU                        
052500        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPARTU-ATTR                   
052600      ELSE                                                                
052700        MOVE MFS-ERASE-FIELD       TO MOD-KVMPARTU                        
052800     END-IF                                                               
052900                                                                          
053000     IF MID-FLSVARU NOT = ALL '+'                                         
053100        MOVE MID-FLSVARU           TO MOD-FLSVARU                         
053200        MOVE MFS-ADD-READ-FIELD    TO MOD-FLSVARU-ATTR                    
053300      ELSE                                                                
053400        MOVE MFS-ERASE-FIELD       TO MOD-FLSVARU                         
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 F-READ-SHOW-INFO SECTION.                                                
053900                                                                          
054000     IF WS-KDANDR NOT = NEW                                               
054100         MOVE LOC-KDLOC      TO    MOD-KDLOC                              
054200         MOVE LOC-KDFREQ     TO    MOD-KDFREQ                             
054300         MOVE LOC-KDSTOR     TO    MOD-KDSTOR                             
054400         MOVE LOC-TELOC      TO    MOD-TELOC                              
054500         MOVE LOC-KVMPART    TO    MOD-KVMPART                            
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 G-CHECK-INPUT SECTION.                                                   
055000                                                                          
055100     MOVE YES  TO INDATA-SW                                               
055200                                                                          
055300     IF MID-KDLOCU     = ALL '+' AND                                      
055400        MID-KDFREQU    = ALL '+' AND                                      
055500        MID-KDSTORU    = ALL '+' AND                                      
055600        MID-TELOCU     = ALL '+' AND                                      
055700        MID-KVMPARTU   = ALL '+' AND                                      
055800        MID-FLSVARU    = ALL '+'                                          
055900                                                                          
056000        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
056100        CALL WMEDKONV USING MED-WMEDAREA                                  
056200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
056300                                                                          
056400        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
056500        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
056600        MOVE NOO TO INDATA-SW                                             
056700      ELSE                                                                
056800        IF WS-KDANDR   = NEW                                              
056900           PERFORM GA-CHECK-INPUT-NEW                                     
057000        END-IF                                                            
057100        IF WS-KDANDR   = CHG                                              
057200           PERFORM GB-CHECK-INPUT-CHG                                     
057300        END-IF                                                            
057400        IF WS-KDANDR   = DEL                                              
057500           PERFORM GC-CHECK-INPUT-DEL                                     
057600        END-IF                                                            
057700        IF INDATA-WRONG                                                   
057800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
057900           CALL WMEDKONV USING MED-WMEDAREA                               
058000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
058100           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
058200           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
058300         END-IF                                                           
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 GA-CHECK-INPUT-NEW SECTION.                                              
058800                                                                          
058900*                                                                         
059000*    -- CONTROL  ON MULTIPLE PART                                         
059100*                                                                         
059200      IF MID-KVMPARTU NOT = ALL '+'                                       
059300        IF MID-KVMPARTU NOT NUMERIC                                       
059400          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPARTU-ATTR                   
059500          MOVE NOO                 TO INDATA-SW                           
059600         ELSE                                                             
059700          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPARTU-ATTR                      
059800        END-IF                                                            
059900       ELSE                                                               
060000        MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPARTU-ATTR                     
060100        MOVE NOO                 TO INDATA-SW                             
060200      END-IF                                                              
060300                                                                          
060400*                                                                         
060500*    -- CONTROL  ON SIZE REMARK                                           
060600*                                                                         
060700     IF MID-TELOCU NOT = ALL '+'                                          
060800        MOVE MFS-ALPHA-FIELD-OK TO MOD-TELOCU-ATTR                        
060900     END-IF                                                               
061000                                                                          
061100*                                                                         
061200*    -- CONTROL  ON STORAGE CODE                                          
061300*                                                                         
061400     IF MID-KDSTORU  = ALL '+'                                            
061500        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDSTORU-ATTR                      
061600        MOVE NOO                 TO INDATA-SW                             
061700      ELSE                                                                
061800        MOVE WS-IDDC             TO W-6315-IDDC                           
061900        MOVE MID-KDSTORU         TO W-KDSTOR                              
062000        PERFORM  IMS-GU-6316                                              
062100        IF SEGMENT-MISSING                                                
062200           MOVE NOO                     TO INDATA-SW                      
062300           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTORU-ATTR               
062400         ELSE                                                             
062500           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTORU-ATTR               
062600        END-IF                                                            
062700     END-IF                                                               
062800*                                                                         
062900*    -- CONTROL  ON FREQUENCY CODE                                        
063000*                                                                         
063100                                                                          
063200     IF MID-KDFREQU  = ALL '+'                                            
063300        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDFREQU-ATTR                      
063400        MOVE NOO                 TO INDATA-SW                             
063500      ELSE                                                                
063600        IF MID-KDFREQU  NUMERIC                                           
063700           MOVE WS-IDDC             TO W-6313-IDDC                        
063800           MOVE MID-KDFREQU         TO W-KDFREQ                           
063900           PERFORM  IMS-GU-6314                                           
064000           IF SEGMENT-MISSING                                             
064100              MOVE NOO                     TO INDATA-SW                   
064200              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR            
064300            ELSE                                                          
064400              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQU-ATTR            
064500           END-IF                                                         
064600         ELSE                                                             
064700           MOVE NOO                     TO INDATA-SW                      
064800           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR               
064900        END-IF                                                            
065000     END-IF                                                               
065100                                                                          
065200*                                                                         
065300*    -- CONTROL  ON LOCATION TYPE                                         
065400*                                                                         
065500     IF MID-KDLOCU   = ALL '+'                                            
065600        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOCU-ATTR                     
065700        MOVE NOO                   TO INDATA-SW                           
065800      ELSE                                                                
065900        IF MID-KDLOCU =  PRIME OR BUFFER OR MIXED                         
066000           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOCU-ATTR                  
066100         ELSE                                                             
066200           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOCU-ATTR                  
066300           MOVE NOO                   TO INDATA-SW                        
066400        END-IF                                                            
066500     END-IF                                                               
066600                                                                          
066700     .                                                                    
066800     EJECT                                                                
066900 GB-CHECK-INPUT-CHG SECTION.                                              
067000                                                                          
067100*                                                                         
067200*    -- CONTROL  ON MULTIPLE PART                                         
067300*                                                                         
067400      IF MID-KVMPARTU NOT = ALL '+'                                       
067500        IF MID-KVMPARTU NOT NUMERIC                                       
067600          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPARTU-ATTR                   
067700          MOVE NOO                 TO INDATA-SW                           
067800         ELSE                                                             
067900          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPARTU-ATTR                      
068000        END-IF                                                            
068100      END-IF                                                              
068200*                                                                         
068300*    -- CONTROL  ON SIZE REMARK                                           
068400*                                                                         
068500                                                                          
068600     IF MID-TELOCU NOT = ALL '+'                                          
068700        MOVE MFS-ALPHA-FIELD-OK TO MOD-TELOCU-ATTR                        
068800     END-IF                                                               
068900                                                                          
069000*                                                                         
069100*    -- CONTROL  ON STORAGE CODE                                          
069200*                                                                         
069300     IF MID-KDSTORU  NOT = ALL '+'                                        
069400        MOVE WS-IDDC             TO W-6315-IDDC                           
069500        MOVE MID-KDSTORU         TO W-KDSTOR                              
069600        PERFORM  IMS-GU-6316                                              
069700        IF SEGMENT-MISSING                                                
069800           MOVE NOO                     TO INDATA-SW                      
069900           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTORU-ATTR               
070000         ELSE                                                             
070100           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTORU-ATTR               
070200        END-IF                                                            
070300     END-IF                                                               
070400                                                                          
070500*                                                                         
070600*    -- CONTROL  ON FREQUENCY CODE                                        
070700*                                                                         
070800     IF MID-KDFREQU  NOT = ALL '+'                                        
070900        IF MID-KDFREQU  NUMERIC                                           
071000           MOVE WS-IDDC             TO W-6313-IDDC                        
071100           MOVE MID-KDFREQU         TO W-KDFREQ                           
071200           PERFORM  IMS-GU-6314                                           
071300           IF SEGMENT-MISSING                                             
071400              MOVE NOO                     TO INDATA-SW                   
071500              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR            
071600            ELSE                                                          
071700              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQU-ATTR            
071800           END-IF                                                         
071900         ELSE                                                             
072000           MOVE NOO                     TO INDATA-SW                      
072100           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR               
072200        END-IF                                                            
072300     END-IF                                                               
072400                                                                          
072500*                                                                         
072600*    -- CONTROL  ON LOCATION TYPE                                         
072700*                                                                         
072800     IF MID-KDLOCU   NOT = ALL '+'                                        
072900        IF MID-KDLOCU =  PRIME OR BUFFER OR MIXED                         
073000           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOCU-ATTR                  
073100         ELSE                                                             
073200           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOCU-ATTR                  
073300           MOVE NOO                   TO INDATA-SW                        
073400        END-IF                                                            
073500     END-IF                                                               
073600                                                                          
073700     .                                                                    
073800     EJECT                                                                
073900 GC-CHECK-INPUT-DEL SECTION.                                              
074000                                                                          
074100*                                                                         
074200*    -- CONTROL  ON CONFIRMATION Y/N                                      
074300*                                                                         
074400     IF MID-FLSVARU  NOT = ALL '+'                                        
074500        IF MID-FLSVARU = YES OR NOO                                       
074600           MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLSVARU-ATTR                 
074700         ELSE                                                             
074800           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSVARU-ATTR                 
074900           MOVE NOO                   TO INDATA-SW                        
075000        END-IF                                                            
075100      ELSE                                                                
075200        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSVARU-ATTR                    
075300        MOVE NOO                   TO INDATA-SW                           
075400     END-IF                                                               
075500     .                                                                    
075600     EJECT                                                                
075700 H-UPDATE SECTION.                                                        
075800                                                                          
075900     IF WS-KDANDR = NEW                                                   
076000        PERFORM HA-UPDATE-NEW                                             
076100     END-IF                                                               
076200                                                                          
076300     IF WS-KDANDR = CHG                                                   
076400        PERFORM HB-UPDATE-CHG                                             
076500     END-IF                                                               
076600                                                                          
076700     IF WS-KDANDR = DEL                                                   
076800        PERFORM HC-UPDATE-DEL                                             
076900     END-IF                                                               
077000                                                                          
077100                                                                          
077200     PERFORM MFS-FORM-ATTR                                                
077300     PERFORM MFS-ERASE-FIELD-IN                                           
077400     .                                                                    
077500     EJECT                                                                
077600 HA-UPDATE-NEW SECTION.                                                   
077700                                                                          
077800     MOVE WS-IDDC            TO LOC-IDDC                                  
077900     MOVE WS-ADLAGOMR        TO LOC-ADLAGOMR                              
078000     MOVE WS-ADGANG          TO LOC-ADGANG                                
078100     MOVE WS-ADPLATS         TO LOC-ADPLATS                               
078200     MOVE MID-KDLOCU         TO LOC-KDLOC  MOD-KDLOC                      
078300     MOVE MID-KDFREQU        TO LOC-KDFREQ  MOD-KDFREQ                    
078400     MOVE MID-KDSTORU        TO LOC-KDSTOR  MOD-KDSTOR                    
078500     MOVE MID-KVMPARTU       TO LOC-KVMPART MOD-KVMPART                   
078600     IF MID-TELOCU NOT = ALL '+'                                          
078700        MOVE MID-TELOCU      TO LOC-TELOC   MOD-TELOC                     
078800     ELSE                                                                 
078900        MOVE SPACES          TO LOC-TELOC   MOD-TELOC                     
079000     END-IF                                                               
079100     MOVE ZERO               TO LOC-KVPLATS                               
079200     PERFORM IMS-ISRT-LOCA-LOC                                            
079300                                                                          
079400     MOVE LOCATION-ADDED TO MED-IDMFSINF                                  
079500     CALL WMEDKONV USING MED-WMEDAREA                                     
079600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
079700                                                                          
079800     .                                                                    
079900     EJECT                                                                
080000 HB-UPDATE-CHG SECTION.                                                   
080100                                                                          
080200     IF MID-KDLOCU NOT = ALL '+'                                          
080300        MOVE MID-KDLOCU         TO LOC-KDLOC  MOD-KDLOC                   
080400      ELSE                                                                
080500        MOVE LOC-KDLOC          TO MOD-KDLOC                              
080600     END-IF                                                               
080700                                                                          
080800     IF MID-KDFREQU NOT = ALL '+'                                         
080900        MOVE MID-KDFREQU        TO LOC-KDFREQ  MOD-KDFREQ                 
081000      ELSE                                                                
081100        MOVE LOC-KDFREQ         TO MOD-KDFREQ                             
081200     END-IF                                                               
081300                                                                          
081400     IF MID-KDSTORU NOT = ALL '+'                                         
081500        MOVE MID-KDSTORU        TO LOC-KDSTOR  MOD-KDSTOR                 
081600      ELSE                                                                
081700        MOVE LOC-KDSTOR         TO MOD-KDSTOR                             
081800     END-IF                                                               
081900                                                                          
082000     IF MID-KVMPARTU NOT = ALL '+'                                        
082100        MOVE MID-KVMPARTU       TO LOC-KVMPART MOD-KVMPART                
082200      ELSE                                                                
082300        MOVE LOC-KVMPART        TO MOD-KVMPART                            
082400     END-IF                                                               
082500                                                                          
082600     IF MID-TELOCU NOT = ALL '+'                                          
082700        MOVE MID-TELOCU      TO LOC-TELOC  MOD-TELOC                      
082800      ELSE                                                                
082900        MOVE LOC-TELOC       TO MOD-TELOC                                 
083000     END-IF                                                               
083100                                                                          
083200     PERFORM IMS-REPL-LOCA-LOC                                            
083300                                                                          
083400     MOVE LOCATION-CHANGED  TO  MED-IDMFSINF                              
083500     CALL WMEDKONV USING MED-WMEDAREA                                     
083600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
083700                                                                          
083800     .                                                                    
083900     EJECT                                                                
084000 HC-UPDATE-DEL SECTION.                                                   
084100                                                                          
084200     IF MID-FLSVARU = YES                                                 
084300        PERFORM IMS-DLET-LOCA-LOC                                         
084400        PERFORM MFS-ERASE-FIELD-OUT                                       
084500        MOVE LOCATION-DELETED TO MED-IDMFSINF                             
084600        CALL WMEDKONV USING MED-WMEDAREA                                  
084700        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
084800     END-IF                                                               
084900                                                                          
085000     IF MID-FLSVARU = NOO                                                 
085100        MOVE LOC-KDLOC      TO    MOD-KDLOC                               
085200        MOVE LOC-KDFREQ     TO    MOD-KDFREQ                              
085300        MOVE LOC-KDSTOR     TO    MOD-KDSTOR                              
085400        MOVE LOC-TELOC      TO    MOD-TELOC                               
085500        MOVE LOC-KVMPART    TO    MOD-KVMPART                             
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
085900 MFS-INIT-KEY-FIELD-IN SECTION.                                           
086000                                                                          
086100*    --- ALL INPUT KEY FIELDS                                             
086200                                                                          
086300     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
086400                               MOD-ADLAGOMR-IN                            
086500                               MOD-ADGANG-IN                              
086600                               MOD-ADSEC11-IN                             
086700                               MOD-ADLEVEL11-IN                           
086800                               MOD-ADSEQ-IN                               
086900                               MOD-KDANDR-IN                              
087000     .                                                                    
087100     EJECT                                                                
087200 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
087300                                                                          
087400*    --- ALL OUTPUT KEY FIELDS                                            
087500                                                                          
087600     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
087700     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
087800                               MOD-ADGANG-UT                              
087900                               MOD-ADSEC11-UT                             
088000                               MOD-ADLEVEL11-UT                           
088100                               MOD-ADSEQ-UT                               
088200                               MOD-KDANDR-UT                              
088300     .                                                                    
088400     EJECT                                                                
088500 MFS-ERASE-FIELD-OUT SECTION.                                             
088600                                                                          
088700*    --- ALL OUTPUT DATA FIELDS                                           
088800     MOVE MFS-ERASE-FIELD    TO MOD-KDLOC                                 
088900                                MOD-KDFREQ                                
089000                                MOD-KDSTOR                                
089100                                MOD-TELOC                                 
089200                                MOD-KVMPART                               
089300     .                                                                    
089400     SKIP3                                                                
089500 MFS-ERASE-FIELD-IN SECTION.                                              
089600                                                                          
089700*    --- ALL INPUT DATA FIELDS                                            
089800     MOVE MFS-ERASE-FIELD    TO MOD-KDLOCU                                
089900                                MOD-KDFREQU                               
090000                                MOD-KDSTORU                               
090100                                MOD-TELOCU                                
090200                                MOD-KVMPARTU                              
090300                                MOD-FLSVARU                               
090400     .                                                                    
090500     EJECT                                                                
090600 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
090700                                                                          
090800*    --- ALL OUTPUT DATA FIELDS                                           
090900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
091000                                    MOD-KDFREQ                            
091100                                    MOD-KDSTOR                            
091200                                    MOD-TELOC                             
091300                                    MOD-KVMPART                           
091400     .                                                                    
091500     SKIP3                                                                
091600 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
091700                                                                          
091800*    --- ALL INPUT DATA FIELDS                                            
091900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOCU                            
092000                                    MOD-KDFREQU                           
092100                                    MOD-KDSTORU                           
092200                                    MOD-TELOCU                            
092300                                    MOD-KVMPARTU                          
092400                                    MOD-FLSVARU                           
092500     .                                                                    
092600     EJECT                                                                
092700 MFS-FORM-ATTR SECTION.                                                   
092800                                                                          
092900*    --- ALL INPUT DATA FIELDS                                            
093000     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDLOCU-ATTR                      
093100                                     MOD-KDFREQU-ATTR                     
093200                                     MOD-KDSTORU-ATTR                     
093300                                     MOD-TELOCU-ATTR                      
093400                                     MOD-KVMPARTU-ATTR                    
093500                                     MOD-FLSVARU-ATTR                     
093600     .                                                                    
093700     SKIP2                                                                
093800* --- IMS SECTIONS ---                                                    
093900     SKIP3                                                                
094000 IMS-GET-MSG SECTION.                                                     
094100                                                                          
094200     MOVE '  QC' TO GOOD-STATUSCODES                                      
094300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
094400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094500     PERFORM IMS-STATUSCHECK                                              
094600     .                                                                    
094700     SKIP3                                                                
094800 IMS-INSERT-MSG SECTION.                                                  
094900                                                                          
095000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095100     MOVE SPACE TO GOOD-STATUSCODES                                       
095200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
095300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095400     PERFORM IMS-STATUSCHECK                                              
095500     .                                                                    
095600     EJECT                                                                
095700 IMS-GU-6314 SECTION.                                                     
095800                                                                          
095900     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
096000          DELIMITED BY SIZE INTO SSA1                                     
096100     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
096200          DELIMITED BY SIZE INTO SSA2                                     
096300     MOVE '  GE' TO GOOD-STATUSCODES                                      
096400     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
096500     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
096600     PERFORM IMS-STATUSCHECK                                              
096700     .                                                                    
096800     SKIP3                                                                
096900 IMS-GU-6316 SECTION.                                                     
097000                                                                          
097100     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
097200          DELIMITED BY SIZE INTO SSA1                                     
097300     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
097400          DELIMITED BY SIZE INTO SSA2                                     
097500     MOVE '  GE' TO GOOD-STATUSCODES                                      
097600     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
097700     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
097800     PERFORM IMS-STATUSCHECK                                              
097900     .                                                                    
098000     SKIP3                                                                
098100 IMS-GHU-LOCA SECTION.                                                    
098200                                                                          
098300     STRING 'WLLOCA01(WDJ801KY =' W-WDJ8KEY-X ')'                         
098400          DELIMITED BY SIZE INTO SSA1                                     
098500     MOVE '  GE' TO GOOD-STATUSCODES                                      
098600     CALL CBLTDLI USING GHU LOCA-PCB LOC-WDJ801 SSA1                      
098700     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
098800     PERFORM IMS-STATUSCHECK                                              
098900     .                                                                    
099000     SKIP3                                                                
099100 IMS-ISRT-LOCA-LOC SECTION.                                               
099200                                                                          
099300     MOVE 'WLLOCA01 ' TO SSA1                                             
099400     MOVE '  II' TO GOOD-STATUSCODES                                      
099500     CALL CBLTDLI USING ISRT LOCA-PCB LOC-WDJ801 SSA1                     
099600     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
099700     PERFORM IMS-STATUSCHECK                                              
099800     .                                                                    
099900     SKIP3                                                                
100000 IMS-REPL-LOCA-LOC SECTION.                                               
100100                                                                          
100200     MOVE '  ' TO GOOD-STATUSCODES                                        
100300     CALL CBLTDLI USING REPL LOCA-PCB LOC-WDJ801                          
100400     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
100500     PERFORM IMS-STATUSCHECK                                              
100600     .                                                                    
100700     SKIP3                                                                
100800 IMS-DLET-LOCA-LOC SECTION.                                               
100900                                                                          
101000     MOVE '  ' TO GOOD-STATUSCODES                                        
101100     CALL CBLTDLI USING DLET LOCA-PCB LOC-WDJ801                          
101200     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUSCHECK                                              
101400     .                                                                    
101500     EJECT                                                                
101600 IMS-STATUSCHECK SECTION.                                                 
101700                                                                          
101800     SET STATUS-IX TO 1                                                   
101900     SEARCH GOOD-STATUS                                                   
102000       AT END                                                             
102100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
102200         DELIMITED BY SIZE INTO ERROR-TEXT                                
102300         CALL FELLOG                                                      
102400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
102500         CONTINUE                                                         
102600     END-SEARCH                                                           
102700     .                                                                    
