000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6031300.                                                
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
001600*        TRANSACTION: W6T313                                              
001700*        MID:         W6I31301                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W6O31301                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6031300'.            
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
006100     88  OWN-MID                             VALUE '6313'.                
006200     88  GOOD-MID                            VALUE '6313'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000     03  IR-ALREADY-REG          PIC X(3)    VALUE '165'.                 
008100     03  LOCATION-ADDED          PIC X(3)    VALUE '260'.                 
008200     03  LOCATION-CHANGED        PIC X(3)    VALUE '261'.                 
008300     03  LOCATION-DELETED        PIC X(3)    VALUE '262'.                 
008400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008500     03  LOC-MISSING             PIC X(3)    VALUE '706'.                 
008600     EJECT                                                                
008700*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009000     SKIP3                                                                
009100*01 -COPY WMSGINIT                                                        
009200     EJECT                                                                
009300*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009600     SKIP3                                                                
009700*01  MID -COPY W6I31301                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010000     SKIP3                                                                
010100*01  -COPY WMSGAREA                                                       
010200     EJECT                                                                
010300     03  MOD REDEFINES MSG-AREA.                                          
010400*      05  -COPY W6O31301                                                 
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010700     SKIP3                                                                
010800*01  -COPY WMFSAREA                                                       
010900     EJECT                                                                
011000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011100*                                                                         
011200     EJECT                                                                
011300 01  WORK-AREA.                                                           
011400     03 WS-ADPLATS.                                                       
011500        05  WS-ADSEC              PIC 9(2)   VALUE ZERO.                  
011600        05  WS-ADLEVEL            PIC 9(2)   VALUE ZERO.                  
011700        05  WS-ADSEQ              PIC 9(1)   VALUE ZERO.                  
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012000     SKIP3                                                                
012100 01  KEYS-TO-DLI.                                                         
012200     03  W-WDGXKEY-6313-X.                                                
012300          05 W-6313-IDHTYP       PIC X(4)    VALUE '6313'.                
012400          05 W-6313-IDDC         PIC X(2)    VALUE SPACE.                 
012500          05 W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
012600                                                                          
012700     03  W-WDGXKEY-6314-X.                                                
012800         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
012900                                                                          
013000     03  W-WDGXKEY-6315-X.                                                
013100          05 W-6315-IDHTYP      PIC X(4)    VALUE '6315'.                 
013200          05 W-6315-IDDC        PIC X(2)    VALUE SPACE.                  
013300          05 W-6315-LOWVALUE    PIC X(24)   VALUE LOW-VALUE.              
013400                                                                          
013500     03  W-WDGXKEY-6316-X.                                                
013600         05  W-KDSTOR           PIC X(3)    VALUE SPACE.                  
013700                                                                          
013800     03  W-WDJ8KEY-X.                                                     
013900         05  W-LOC-IDDC          PIC X(2)   VALUE SPACE.                  
014000         05  W-LOC-ADLAGOMR      PIC 9(2)   VALUE ZERO.                   
014100         05  W-LOC-ADGANG        PIC 9(2)   VALUE ZERO.                   
014200         05  W-LOC-ADPLATS.                                               
014300             07 W-LOC-ADSEC      PIC 9(2).                                
014400             07 W-LOC-ADLEVEL    PIC 9(2).                                
014500             07 W-LOC-ADSEQ      PIC 9(1).                                
014600                                                                          
014700     03  W-IDDC-B6-X.                                                     
014800         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
014900     SKIP2                                                                
015000*    --- STATUS-KOD FRÅN IMS                                              
015100 01  STATUS-WS                   PIC XX.                                  
015200     88  SEGMENT-FOUND                       VALUE '  '.                  
015300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015500     SKIP2                                                                
015600 01  GOOD-STATUSCODES.                                                    
015700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015800     SKIP3                                                                
015900 01  SSA1                        PIC X(64).                               
016000 01  SSA2                        PIC X(64).                               
016100     EJECT                                                                
016200*    --- IMS FUNCTION CODES                                               
016300*01  -COPY W0003                                                          
016400     EJECT                                                                
016500*    ---  DLI INPUT-OUTPUT AREA                                           
016600 01  FILLER         PIC X(20) VALUE 'WL631301-AREA'.                      
016700 01  WL631301-AREA.                                                       
016800*    03  -COPY WDGX6313                                                   
016900     EJECT                                                                
017000 01  FILLER         PIC X(20) VALUE 'WL631311-AREA'.                      
017100 01  WL631311-AREA.                                                       
017200*    03  -COPY WDGX6314                                                   
017300                                                                          
017400 01  FILLER         PIC X(20) VALUE 'WL631501-AREA'.                      
017500 01  WL631501-AREA.                                                       
017600*    03  -COPY WDGX6315                                                   
017700     EJECT                                                                
017800 01  FILLER         PIC X(20) VALUE 'WL631511-AREA'.                      
017900 01  WL631511-AREA.                                                       
018000*    03  -COPY WDGX6316                                                   
018100                                                                          
018200 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
018300 01  DLI-IO-WLLOCA01.                                                     
018400*    03  -COPY WDJ801                                                     
018500     EJECT                                                                
018600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018700 01   DLI-IO-AREA-B601.                                                   
018800*     03  -COPY WDB601                                                    
018900     EJECT                                                                
019000     EJECT                                                                
019100 LINKAGE SECTION.                                                         
019200*01  -COPY W0009   -PRE MSG-                                              
019300*01  -COPY W0008   -PRE USEA-                                             
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600*01  -COPY W0008  -PRE 6313-                                              
019700     05  FILLER                  PIC X.                                   
019800     EJECT                                                                
019900*01  -COPY W0008  -PRE 6315-                                              
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200*01  -COPY W0008  -PRE LOCA-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008      -PRE WDB6-                                          
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800     EJECT                                                                
020900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 6313-PCB 6315-PCB             
021000     LOCA-PCB WDB6-PCB.                                                   
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 6313-PCB 6315-PCB             
021300     LOCA-PCB WDB6-PCB.                                                   
021400                                                                          
021500     PERFORM IMS-GET-MSG                                                  
021600     IF SEGMENT-FOUND                                                     
021700       PERFORM A-INIT                                                     
021800       IF GOOD-MID OR HELP-MID                                            
021900          PERFORM B-CHECK-KEYS                                            
022000          IF KEYS-OK                                                      
022100             IF MFS-UPDATE                                                
022200                PERFORM G-CHECK-INPUT                                     
022300                IF INDATA-OK                                              
022400                   PERFORM H-UPDATE                                       
022500                END-IF                                                    
022600              ELSE                                                        
022700                IF MFS-FIRST                                              
022800                   PERFORM C-FIRST-PAGE                                   
022900                 ELSE                                                     
023000                   PERFORM E-SAME-PAGE                                    
023100                END-IF                                                    
023200                PERFORM F-READ-SHOW-INFO                                  
023300             END-IF                                                       
023400          END-IF                                                          
023500       END-IF                                                             
023600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31301 + 4                      
023700       PERFORM IMS-INSERT-MSG                                             
023800     END-IF                                                               
023900                                                                          
024000     MOVE ZERO TO RETURN-CODE                                             
024100     GOBACK                                                               
024200     .                                                                    
024300     EJECT                                                                
024400 A-INIT SECTION.                                                          
024500                                                                          
024600     IF MSG-DOUBLE-TRANSACTIONS                                           
024700       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I31301                 
024800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025000     ELSE                                                                 
025100       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I31301                  
025200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
025300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025400     END-IF                                                               
025500                                                                          
025600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
025700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025900                                                                          
026000     MOVE LOW-VALUE TO MSG-AREA                                           
026100     MOVE 'W6O313N1' TO MFS-IDMOD                                         
026200     MOVE '6313'     TO MOD-IDTRANS                                       
026300     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
026400                                                                          
026500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026600     MOVE '001'             TO MSGI-KDCALL                                
026700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026900     MOVE '6313'            TO MSGI-IDTRANS                               
027000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027100                                                                          
027200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
027300                                                                          
027400     IF GOOD-MID OR HELP-MID                                              
027500       CONTINUE                                                           
027600     ELSE                                                                 
027700       MOVE SPACE TO MFS-KDTRTYP                                          
027800       MOVE '7' TO MFS-IDPFK                                              
027900       PERFORM MFS-INIT-KEY-FIELD-IN                                      
028000       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
028100       PERFORM MFS-ERASE-FIELD-IN                                         
028200       PERFORM MFS-ERASE-FIELD-OUT                                        
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 B-CHECK-KEYS SECTION.                                                    
028700                                                                          
028800                                                                          
028900     MOVE YES             TO KEYS-SW                                      
029000                                                                          
029100     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
029200                             MOD-ADLAGOMR-IN                              
029300                             MOD-ADGANG-IN                                
029400                             MOD-ADSEC-IN                                 
029500                             MOD-ADLEVEL-IN                               
029600                             MOD-ADSEQ-IN                                 
029700                             MOD-KDANDR-IN                                
029800*                                                                         
029900*    -- CONTROL  ON WAREHOUSE                                             
030000*                                                                         
030100     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
030200     PERFORM IMS-GU-WDB601                                                
030300     IF SEGMENT-FOUND                                                     
030400     AND (DCS-SDC                                                         
030500     OR   DCS-NDC-NA                                                      
030600     OR   DCS-NDC-PF                                                      
030700     OR   DCS-NDC-OTHERS)                                                 
030800       CONTINUE                                                           
030900     ELSE                                                                 
031000       MOVE NOO                 TO KEYS-SW                                
031100     END-IF                                                               
031200*                                                                         
031300*    -- CONTROL  ON AREA                                                  
031400*                                                                         
031500     IF MID-ADLAGOMR-IN = ALL '+'                                         
031600       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
031700       MOVE MID-ADLAGOMR-UT TO WS-ADLAGOMR                                
031800     ELSE                                                                 
031900       IF MID-ADLAGOMR-IN NUMERIC                                         
032000          MOVE    '7'          TO MFS-IDPFK                               
032100          MOVE    SPACE        TO MFS-KDTRTYP                             
032200          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
032300       ELSE                                                               
032400          MOVE NOO             TO KEYS-SW                                 
032500       END-IF                                                             
032600     END-IF                                                               
032700*                                                                         
032800*    -- CONTROL  ON AISLE                                                 
032900*                                                                         
033000     IF MID-ADGANG-IN = ALL '+'                                           
033100       INSPECT MID-ADGANG-UT REPLACING LEADING SPACE BY ZERO              
033200       MOVE MID-ADGANG-UT   TO WS-ADGANG                                  
033300     ELSE                                                                 
033400       IF MID-ADGANG-IN NUMERIC                                           
033500          MOVE    '7'          TO MFS-IDPFK                               
033600          MOVE    SPACE        TO MFS-KDTRTYP                             
033700          MOVE MID-ADGANG-IN   TO WS-ADGANG                               
033800       ELSE                                                               
033900          MOVE NOO             TO KEYS-SW                                 
034000       END-IF                                                             
034100     END-IF                                                               
034200*                                                                         
034300*    -- CONTROL  ON SECTION                                               
034400*                                                                         
034500     IF MID-ADSEC-IN = ALL '+'                                            
034600       INSPECT MID-ADSEC-UT REPLACING LEADING SPACE BY ZERO               
034700       MOVE MID-ADSEC-UT    TO WS-ADSEC                                   
034800     ELSE                                                                 
034900       IF MID-ADSEC-IN NUMERIC                                            
035000          MOVE    '7'          TO MFS-IDPFK                               
035100          MOVE    SPACE        TO MFS-KDTRTYP                             
035200          MOVE MID-ADSEC-IN    TO WS-ADSEC                                
035300       ELSE                                                               
035400          MOVE NOO             TO KEYS-SW                                 
035500       END-IF                                                             
035600     END-IF                                                               
035700                                                                          
035800*                                                                         
035900*    -- CONTROL  ON LEVEL                                                 
036000*                                                                         
036100     IF MID-ADLEVEL-IN = ALL '+'                                          
036200       INSPECT MID-ADLEVEL-UT REPLACING LEADING SPACE BY ZERO             
036300       MOVE MID-ADLEVEL-UT  TO WS-ADLEVEL                                 
036400     ELSE                                                                 
036500       IF MID-ADLEVEL-IN NUMERIC                                          
036600          MOVE    '7'          TO MFS-IDPFK                               
036700          MOVE    SPACE        TO MFS-KDTRTYP                             
036800          MOVE MID-ADLEVEL-IN  TO WS-ADLEVEL                              
036900       ELSE                                                               
037000          MOVE NOO             TO KEYS-SW                                 
037100       END-IF                                                             
037200     END-IF                                                               
037300                                                                          
037400*                                                                         
037500*    -- CONTROL  ON PLACE                                                 
037600*                                                                         
037700     IF MID-ADSEQ-IN = ALL '+'                                            
037800       INSPECT MID-ADSEQ-UT REPLACING LEADING SPACE BY ZERO               
037900       MOVE MID-ADSEQ-UT    TO WS-ADSEQ                                   
038000     ELSE                                                                 
038100       IF MID-ADSEQ-IN NUMERIC                                            
038200          MOVE    '7'          TO MFS-IDPFK                               
038300          MOVE    SPACE        TO MFS-KDTRTYP                             
038400          MOVE MID-ADSEQ-IN    TO WS-ADSEQ                                
038500       ELSE                                                               
038600          MOVE NOO             TO KEYS-SW                                 
038700       END-IF                                                             
038800     END-IF                                                               
038900                                                                          
039000*                                                                         
039100*    -- CONTROL  ON ACTION                                                
039200*                                                                         
039300     IF MID-KDANDR-IN = ALL '+'                                           
039400       MOVE MID-KDANDR-UT      TO WS-KDANDR                               
039500     ELSE                                                                 
039600       MOVE MID-KDANDR-IN      TO WS-KDANDR                               
039700       IF MID-KDANDR-IN = NEW OR DEL OR CHG OR ENQ                        
039800          MOVE    '7'          TO MFS-IDPFK                               
039900          MOVE    SPACE        TO MFS-KDTRTYP                             
040000       ELSE                                                               
040100          MOVE NOO             TO KEYS-SW                                 
040200       END-IF                                                             
040300     END-IF                                                               
040400*                                                                         
040500*    -- FILL MOD KEY-OUTPUT FIELDS                                        
040600*                                                                         
040700     MOVE W-IDDC-B6    TO  MOD-IDDC-UT                                    
040800     MOVE WS-ADLAGOMR  TO  MOD-ADLAGOMR-UT                                
040900     MOVE WS-ADGANG    TO  MOD-ADGANG-UT                                  
041000     MOVE WS-ADSEC     TO  MOD-ADSEC-UT                                   
041100     MOVE WS-ADLEVEL   TO  MOD-ADLEVEL-UT                                 
041200     MOVE WS-ADSEQ     TO  MOD-ADSEQ-UT                                   
041300     MOVE WS-KDANDR    TO  MOD-KDANDR-UT                                  
041400                                                                          
041500*                                                                         
041600*    -- CHECK  LOCATION ON DATABASE                                       
041700*                                                                         
041800     IF KEYS-WRONG                                                        
041900        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
042000        CALL WMEDKONV USING MED-WMEDAREA                                  
042100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
042200       ELSE                                                               
042300        MOVE W-IDDC-B6    TO  W-LOC-IDDC                                  
042400        MOVE WS-ADLAGOMR  TO  W-LOC-ADLAGOMR                              
042500        MOVE WS-ADGANG    TO  W-LOC-ADGANG                                
042600        MOVE WS-ADSEC     TO  W-LOC-ADSEC                                 
042700        MOVE WS-ADLEVEL   TO  W-LOC-ADLEVEL                               
042800        MOVE WS-ADSEQ     TO  W-LOC-ADSEQ                                 
042900                                                                          
043000        PERFORM  IMS-GHU-LOCA                                             
043100                                                                          
043200        IF WS-KDANDR = NEW                                                
043300           IF SEGMENT-FOUND                                               
043400              MOVE IR-ALREADY-REG TO MED-IDMFSFEL                         
043500              CALL WMEDKONV USING MED-WMEDAREA                            
043600              MOVE MED-MFSFEL     TO MOD-TEMFSFEL                         
043700              MOVE NOO            TO KEYS-SW                              
043800           END-IF                                                         
043900        END-IF                                                            
044000                                                                          
044100        IF WS-KDANDR = ENQ OR DEL OR CHG                                  
044200           IF SEGMENT-MISSING                                             
044300              MOVE LOC-MISSING   TO MED-IDMFSFEL                          
044400              CALL WMEDKONV USING MED-WMEDAREA                            
044500              MOVE MED-MFSFEL    TO MOD-TEMFSFEL                          
044600              MOVE NOO           TO KEYS-SW                               
044700           END-IF                                                         
044800        END-IF                                                            
044900     END-IF                                                               
045000                                                                          
045100                                                                          
045200     IF KEYS-WRONG                                                        
045300       PERFORM MFS-ERASE-FIELD-IN                                         
045400       PERFORM MFS-ERASE-FIELD-OUT                                        
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 C-FIRST-PAGE SECTION.                                                    
045900                                                                          
046000     PERFORM MFS-ERASE-FIELD-IN                                           
046100                                                                          
046200     IF WS-KDANDR = NEW                                                   
046300        MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOCU-ATTR                     
046400        MOVE '01'                  TO MOD-KVMPARTU                        
046500        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPARTU-ATTR                   
046600     END-IF                                                               
046700                                                                          
046800     IF WS-KDANDR = CHG                                                   
046900        MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOCU-ATTR                     
047000     END-IF                                                               
047100                                                                          
047200     IF WS-KDANDR = DEL                                                   
047300        MOVE 'N'                   TO MOD-FLSVARU                         
047400        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSVARU-ATTR                    
047500     END-IF                                                               
047600                                                                          
047700     IF WS-KDANDR = NEW OR CHG OR DEL                                     
047800        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                               
047900        CALL WMEDKONV USING MED-WMEDAREA                                  
048000        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
048100     END-IF                                                               
048200                                                                          
048300     .                                                                    
048400     EJECT                                                                
048500 E-SAME-PAGE SECTION.                                                     
048600                                                                          
048700     IF OWN-MID OR HELP-MID                                               
048800       IF MID-KDLOCU   = ALL '+' AND                                      
048900          MID-KDFREQU  = ALL '+' AND                                      
049000          MID-KDSTORU  = ALL '+' AND                                      
049100          MID-TELOCU   = ALL '+' AND                                      
049200          MID-KVMPARTU = ALL '+' AND                                      
049300          MID-FLSVARU  = ALL '+'                                          
049400         PERFORM MFS-ERASE-FIELD-IN                                       
049500       ELSE                                                               
049600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
049700         CALL WMEDKONV USING MED-WMEDAREA                                 
049800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
049900         PERFORM EA-MID-INDATA-TO-MOD                                     
050000       END-IF                                                             
050100     ELSE                                                                 
050200       PERFORM MFS-ERASE-FIELD-IN                                         
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 EA-MID-INDATA-TO-MOD SECTION.                                            
050700                                                                          
050800     IF MID-KDLOCU NOT = ALL '+'                                          
050900        MOVE MID-KDLOCU            TO MOD-KDLOCU                          
051000        MOVE MFS-ADD-READ-FIELD    TO MOD-KDLOCU-ATTR                     
051100      ELSE                                                                
051200        MOVE MFS-ERASE-FIELD       TO MOD-KDLOCU                          
051300     END-IF                                                               
051400                                                                          
051500     IF MID-KDFREQU NOT = ALL '+'                                         
051600        MOVE MID-KDFREQU            TO MOD-KDFREQU                        
051700        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQU-ATTR                   
051800      ELSE                                                                
051900        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQU                        
052000     END-IF                                                               
052100                                                                          
052200     IF MID-KDSTORU NOT = ALL '+'                                         
052300        MOVE MID-KDSTORU            TO MOD-KDSTORU                        
052400        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTORU-ATTR                   
052500      ELSE                                                                
052600        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR                         
052700     END-IF                                                               
052800                                                                          
052900     IF MID-TELOCU NOT = ALL '+'                                          
053000        MOVE MID-TELOCU            TO MOD-TELOCU                          
053100        MOVE MFS-ADD-READ-FIELD    TO MOD-TELOCU-ATTR                     
053200      ELSE                                                                
053300        MOVE MFS-ERASE-FIELD       TO MOD-TELOCU                          
053400     END-IF                                                               
053500                                                                          
053600     IF MID-KVMPARTU NOT = ALL '+'                                        
053700        MOVE MID-KVMPARTU          TO MOD-KVMPARTU                        
053800        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPARTU-ATTR                   
053900      ELSE                                                                
054000        MOVE MFS-ERASE-FIELD       TO MOD-KVMPARTU                        
054100     END-IF                                                               
054200                                                                          
054300     IF MID-FLSVARU NOT = ALL '+'                                         
054400        MOVE MID-FLSVARU           TO MOD-FLSVARU                         
054500        MOVE MFS-ADD-READ-FIELD    TO MOD-FLSVARU-ATTR                    
054600      ELSE                                                                
054700        MOVE MFS-ERASE-FIELD       TO MOD-FLSVARU                         
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 F-READ-SHOW-INFO SECTION.                                                
055200                                                                          
055300     IF WS-KDANDR NOT = NEW                                               
055400         MOVE LOC-KDLOC      TO    MOD-KDLOC                              
055500         MOVE LOC-KDFREQ     TO    MOD-KDFREQ                             
055600         MOVE LOC-KDSTOR     TO    MOD-KDSTOR                             
055700         MOVE LOC-TELOC      TO    MOD-TELOC                              
055800         MOVE LOC-KVMPART    TO    MOD-KVMPART                            
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 G-CHECK-INPUT SECTION.                                                   
056300                                                                          
056400     MOVE YES  TO INDATA-SW                                               
056500                                                                          
056600     IF MID-KDLOCU     = ALL '+' AND                                      
056700        MID-KDFREQU    = ALL '+' AND                                      
056800        MID-KDSTORU    = ALL '+' AND                                      
056900        MID-TELOCU     = ALL '+' AND                                      
057000        MID-KVMPARTU   = ALL '+' AND                                      
057100        MID-FLSVARU    = ALL '+'                                          
057200                                                                          
057300        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
057400        CALL WMEDKONV USING MED-WMEDAREA                                  
057500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
057600                                                                          
057700        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
057800        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
057900        MOVE NOO TO INDATA-SW                                             
058000      ELSE                                                                
058100        IF WS-KDANDR   = NEW                                              
058200           PERFORM GA-CHECK-INPUT-NEW                                     
058300        END-IF                                                            
058400        IF WS-KDANDR   = CHG                                              
058500           PERFORM GB-CHECK-INPUT-CHG                                     
058600        END-IF                                                            
058700        IF WS-KDANDR   = DEL                                              
058800           PERFORM GC-CHECK-INPUT-DEL                                     
058900        END-IF                                                            
059000        IF INDATA-WRONG                                                   
059100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
059200           CALL WMEDKONV USING MED-WMEDAREA                               
059300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
059400           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
059500           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
059600         END-IF                                                           
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 GA-CHECK-INPUT-NEW SECTION.                                              
060100                                                                          
060200*                                                                         
060300*    -- CONTROL  ON MULTIPLE PART                                         
060400*                                                                         
060500      IF MID-KVMPARTU NOT = ALL '+'                                       
060600        IF MID-KVMPARTU NOT NUMERIC                                       
060700          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPARTU-ATTR                   
060800          MOVE NOO                 TO INDATA-SW                           
060900         ELSE                                                             
061000          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPARTU-ATTR                      
061100        END-IF                                                            
061200       ELSE                                                               
061300        MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPARTU-ATTR                     
061400        MOVE NOO                 TO INDATA-SW                             
061500      END-IF                                                              
061600                                                                          
061700*                                                                         
061800*    -- CONTROL  ON SIZE REMARK                                           
061900*                                                                         
062000     IF MID-TELOCU NOT = ALL '+'                                          
062100        MOVE MFS-ALPHA-FIELD-OK TO MOD-TELOCU-ATTR                        
062200     END-IF                                                               
062300                                                                          
062400*                                                                         
062500*    -- CONTROL  ON STORAGE CODE                                          
062600*                                                                         
062700     IF MID-KDSTORU  = ALL '+'                                            
062800        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDSTORU-ATTR                      
062900        MOVE NOO                 TO INDATA-SW                             
063000      ELSE                                                                
063100        MOVE W-IDDC-B6           TO W-6315-IDDC                           
063200        MOVE MID-KDSTORU         TO W-KDSTOR                              
063300        PERFORM  IMS-GU-6316                                              
063400        IF SEGMENT-MISSING                                                
063500           MOVE NOO                     TO INDATA-SW                      
063600           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTORU-ATTR               
063700         ELSE                                                             
063800           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTORU-ATTR               
063900        END-IF                                                            
064000     END-IF                                                               
064100*                                                                         
064200*    -- CONTROL  ON FREQUENCY CODE                                        
064300*                                                                         
064400                                                                          
064500     IF MID-KDFREQU  = ALL '+'                                            
064600        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDFREQU-ATTR                      
064700        MOVE NOO                 TO INDATA-SW                             
064800      ELSE                                                                
064900        IF MID-KDFREQU  NUMERIC                                           
065000           MOVE W-IDDC-B6           TO W-6313-IDDC                        
065100           MOVE MID-KDFREQU         TO W-KDFREQ                           
065200           PERFORM  IMS-GU-6314                                           
065300           IF SEGMENT-MISSING                                             
065400              MOVE NOO                     TO INDATA-SW                   
065500              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR            
065600            ELSE                                                          
065700              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQU-ATTR            
065800           END-IF                                                         
065900         ELSE                                                             
066000           MOVE NOO                     TO INDATA-SW                      
066100           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR               
066200        END-IF                                                            
066300     END-IF                                                               
066400                                                                          
066500*                                                                         
066600*    -- CONTROL  ON LOCATION TYPE                                         
066700*                                                                         
066800     IF MID-KDLOCU   = ALL '+'                                            
066900        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOCU-ATTR                     
067000        MOVE NOO                   TO INDATA-SW                           
067100      ELSE                                                                
067200        IF MID-KDLOCU =  PRIME OR BUFFER OR MIXED                         
067300           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOCU-ATTR                  
067400         ELSE                                                             
067500           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOCU-ATTR                  
067600           MOVE NOO                   TO INDATA-SW                        
067700        END-IF                                                            
067800     END-IF                                                               
067900                                                                          
068000     .                                                                    
068100     EJECT                                                                
068200 GB-CHECK-INPUT-CHG SECTION.                                              
068300                                                                          
068400*                                                                         
068500*    -- CONTROL  ON MULTIPLE PART                                         
068600*                                                                         
068700      IF MID-KVMPARTU NOT = ALL '+'                                       
068800        IF MID-KVMPARTU NOT NUMERIC                                       
068900          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPARTU-ATTR                   
069000          MOVE NOO                 TO INDATA-SW                           
069100         ELSE                                                             
069200          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPARTU-ATTR                      
069300        END-IF                                                            
069400      END-IF                                                              
069500*                                                                         
069600*    -- CONTROL  ON SIZE REMARK                                           
069700*                                                                         
069800                                                                          
069900     IF MID-TELOCU NOT = ALL '+'                                          
070000        MOVE MFS-ALPHA-FIELD-OK TO MOD-TELOCU-ATTR                        
070100     END-IF                                                               
070200                                                                          
070300*                                                                         
070400*    -- CONTROL  ON STORAGE CODE                                          
070500*                                                                         
070600     IF MID-KDSTORU  NOT = ALL '+'                                        
070700        MOVE W-IDDC-B6           TO W-6315-IDDC                           
070800        MOVE MID-KDSTORU         TO W-KDSTOR                              
070900        PERFORM  IMS-GU-6316                                              
071000        IF SEGMENT-MISSING                                                
071100           MOVE NOO                     TO INDATA-SW                      
071200           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTORU-ATTR               
071300         ELSE                                                             
071400           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTORU-ATTR               
071500        END-IF                                                            
071600     END-IF                                                               
071700                                                                          
071800*                                                                         
071900*    -- CONTROL  ON FREQUENCY CODE                                        
072000*                                                                         
072100     IF MID-KDFREQU  NOT = ALL '+'                                        
072200        IF MID-KDFREQU  NUMERIC                                           
072300           MOVE W-IDDC-B6           TO W-6313-IDDC                        
072400           MOVE MID-KDFREQU         TO W-KDFREQ                           
072500           PERFORM  IMS-GU-6314                                           
072600           IF SEGMENT-MISSING                                             
072700              MOVE NOO                     TO INDATA-SW                   
072800              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR            
072900            ELSE                                                          
073000              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQU-ATTR            
073100           END-IF                                                         
073200         ELSE                                                             
073300           MOVE NOO                     TO INDATA-SW                      
073400           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQU-ATTR               
073500        END-IF                                                            
073600     END-IF                                                               
073700                                                                          
073800*                                                                         
073900*    -- CONTROL  ON LOCATION TYPE                                         
074000*                                                                         
074100     IF MID-KDLOCU   NOT = ALL '+'                                        
074200        IF MID-KDLOCU =  PRIME OR BUFFER OR MIXED                         
074300           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOCU-ATTR                  
074400         ELSE                                                             
074500           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOCU-ATTR                  
074600           MOVE NOO                   TO INDATA-SW                        
074700        END-IF                                                            
074800     END-IF                                                               
074900                                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 GC-CHECK-INPUT-DEL SECTION.                                              
075300                                                                          
075400*                                                                         
075500*    -- CONTROL  ON CONFIRMATION Y/N                                      
075600*                                                                         
075700     IF MID-FLSVARU  NOT = ALL '+'                                        
075800        IF MID-FLSVARU = YES OR NOO                                       
075900           MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLSVARU-ATTR                 
076000         ELSE                                                             
076100           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSVARU-ATTR                 
076200           MOVE NOO                   TO INDATA-SW                        
076300        END-IF                                                            
076400      ELSE                                                                
076500        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSVARU-ATTR                    
076600        MOVE NOO                   TO INDATA-SW                           
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 H-UPDATE SECTION.                                                        
077100                                                                          
077200     IF WS-KDANDR = NEW                                                   
077300        PERFORM HA-UPDATE-NEW                                             
077400     END-IF                                                               
077500                                                                          
077600     IF WS-KDANDR = CHG                                                   
077700        PERFORM HB-UPDATE-CHG                                             
077800     END-IF                                                               
077900                                                                          
078000     IF WS-KDANDR = DEL                                                   
078100        PERFORM HC-UPDATE-DEL                                             
078200     END-IF                                                               
078300                                                                          
078400                                                                          
078500     PERFORM MFS-FORM-ATTR                                                
078600     PERFORM MFS-ERASE-FIELD-IN                                           
078700     .                                                                    
078800     EJECT                                                                
078900 HA-UPDATE-NEW SECTION.                                                   
079000                                                                          
079100     MOVE W-IDDC-B6          TO LOC-IDDC                                  
079200     MOVE WS-ADLAGOMR        TO LOC-ADLAGOMR                              
079300     MOVE WS-ADGANG          TO LOC-ADGANG                                
079400     MOVE WS-ADPLATS         TO LOC-ADPLATS                               
079500     MOVE MID-KDLOCU         TO LOC-KDLOC  MOD-KDLOC                      
079600     MOVE MID-KDFREQU        TO LOC-KDFREQ  MOD-KDFREQ                    
079700     MOVE MID-KDSTORU        TO LOC-KDSTOR  MOD-KDSTOR                    
079800     MOVE MID-KVMPARTU       TO LOC-KVMPART MOD-KVMPART                   
079900     IF MID-TELOCU NOT = ALL '+'                                          
080000        MOVE MID-TELOCU      TO LOC-TELOC   MOD-TELOC                     
080100     ELSE                                                                 
080200        MOVE SPACES          TO LOC-TELOC   MOD-TELOC                     
080300     END-IF                                                               
080400                                                                          
080500     PERFORM IMS-ISRT-LOCA-LOC                                            
080600                                                                          
080700     MOVE LOCATION-ADDED TO MED-IDMFSINF                                  
080800     CALL WMEDKONV USING MED-WMEDAREA                                     
080900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
081000                                                                          
081100     .                                                                    
081200     EJECT                                                                
081300 HB-UPDATE-CHG SECTION.                                                   
081400                                                                          
081500     IF MID-KDLOCU NOT = ALL '+'                                          
081600        MOVE MID-KDLOCU         TO LOC-KDLOC  MOD-KDLOC                   
081700      ELSE                                                                
081800        MOVE LOC-KDLOC          TO MOD-KDLOC                              
081900     END-IF                                                               
082000                                                                          
082100     IF MID-KDFREQU NOT = ALL '+'                                         
082200        MOVE MID-KDFREQU        TO LOC-KDFREQ  MOD-KDFREQ                 
082300      ELSE                                                                
082400        MOVE LOC-KDFREQ         TO MOD-KDFREQ                             
082500     END-IF                                                               
082600                                                                          
082700     IF MID-KDSTORU NOT = ALL '+'                                         
082800        MOVE MID-KDSTORU        TO LOC-KDSTOR  MOD-KDSTOR                 
082900      ELSE                                                                
083000        MOVE LOC-KDSTOR         TO MOD-KDSTOR                             
083100     END-IF                                                               
083200                                                                          
083300     IF MID-KVMPARTU NOT = ALL '+'                                        
083400        MOVE MID-KVMPARTU       TO LOC-KVMPART MOD-KVMPART                
083500      ELSE                                                                
083600        MOVE LOC-KVMPART        TO MOD-KVMPART                            
083700     END-IF                                                               
083800                                                                          
083900     IF MID-TELOCU NOT = ALL '+'                                          
084000        MOVE MID-TELOCU      TO LOC-TELOC  MOD-TELOC                      
084100      ELSE                                                                
084200        MOVE LOC-TELOC       TO MOD-TELOC                                 
084300     END-IF                                                               
084400                                                                          
084500     PERFORM IMS-REPL-LOCA-LOC                                            
084600                                                                          
084700     MOVE LOCATION-CHANGED  TO  MED-IDMFSINF                              
084800     CALL WMEDKONV USING MED-WMEDAREA                                     
084900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
085000                                                                          
085100     .                                                                    
085200     EJECT                                                                
085300 HC-UPDATE-DEL SECTION.                                                   
085400                                                                          
085500     IF MID-FLSVARU = YES                                                 
085600        PERFORM IMS-DLET-LOCA-LOC                                         
085700        PERFORM MFS-ERASE-FIELD-OUT                                       
085800        MOVE LOCATION-DELETED TO MED-IDMFSINF                             
085900        CALL WMEDKONV USING MED-WMEDAREA                                  
086000        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
086100     END-IF                                                               
086200                                                                          
086300     IF MID-FLSVARU = NOO                                                 
086400        MOVE LOC-KDLOC      TO    MOD-KDLOC                               
086500        MOVE LOC-KDFREQ     TO    MOD-KDFREQ                              
086600        MOVE LOC-KDSTOR     TO    MOD-KDSTOR                              
086700        MOVE LOC-TELOC      TO    MOD-TELOC                               
086800        MOVE LOC-KVMPART    TO    MOD-KVMPART                             
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 MFS-INIT-KEY-FIELD-IN SECTION.                                           
087300                                                                          
087400*    --- ALL INPUT KEY FIELDS                                             
087500                                                                          
087600     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
087700                               MOD-ADLAGOMR-IN                            
087800                               MOD-ADGANG-IN                              
087900                               MOD-ADSEC-IN                               
088000                               MOD-ADLEVEL-IN                             
088100                               MOD-ADSEQ-IN                               
088200                               MOD-KDANDR-IN                              
088300     .                                                                    
088400     EJECT                                                                
088500 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
088600                                                                          
088700*    --- ALL OUTPUT KEY FIELDS                                            
088800                                                                          
088900     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
089000     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
089100                               MOD-ADGANG-UT                              
089200                               MOD-ADSEC-UT                               
089300                               MOD-ADLEVEL-UT                             
089400                               MOD-ADSEQ-UT                               
089500                               MOD-KDANDR-UT                              
089600     .                                                                    
089700     EJECT                                                                
089800 MFS-ERASE-FIELD-OUT SECTION.                                             
089900                                                                          
090000*    --- ALL OUTPUT DATA FIELDS                                           
090100     MOVE MFS-ERASE-FIELD    TO MOD-KDLOC                                 
090200                                MOD-KDFREQ                                
090300                                MOD-KDSTOR                                
090400                                MOD-TELOC                                 
090500                                MOD-KVMPART                               
090600     .                                                                    
090700     SKIP3                                                                
090800 MFS-ERASE-FIELD-IN SECTION.                                              
090900                                                                          
091000*    --- ALL INPUT DATA FIELDS                                            
091100     MOVE MFS-ERASE-FIELD    TO MOD-KDLOCU                                
091200                                MOD-KDFREQU                               
091300                                MOD-KDSTORU                               
091400                                MOD-TELOCU                                
091500                                MOD-KVMPARTU                              
091600                                MOD-FLSVARU                               
091700     .                                                                    
091800     EJECT                                                                
091900 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
092000                                                                          
092100*    --- ALL OUTPUT DATA FIELDS                                           
092200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
092300                                    MOD-KDFREQ                            
092400                                    MOD-KDSTOR                            
092500                                    MOD-TELOC                             
092600                                    MOD-KVMPART                           
092700     .                                                                    
092800     SKIP3                                                                
092900 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
093000                                                                          
093100*    --- ALL INPUT DATA FIELDS                                            
093200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOCU                            
093300                                    MOD-KDFREQU                           
093400                                    MOD-KDSTORU                           
093500                                    MOD-TELOCU                            
093600                                    MOD-KVMPARTU                          
093700                                    MOD-FLSVARU                           
093800     .                                                                    
093900     EJECT                                                                
094000 MFS-FORM-ATTR SECTION.                                                   
094100                                                                          
094200*    --- ALL INPUT DATA FIELDS                                            
094300     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDLOCU-ATTR                      
094400                                     MOD-KDFREQU-ATTR                     
094500                                     MOD-KDSTORU-ATTR                     
094600                                     MOD-TELOCU-ATTR                      
094700                                     MOD-KVMPARTU-ATTR                    
094800                                     MOD-FLSVARU-ATTR                     
094900     .                                                                    
095000     SKIP2                                                                
095100* --- IMS SECTIONS ---                                                    
095200     SKIP3                                                                
095300 IMS-GET-MSG SECTION.                                                     
095400                                                                          
095500     MOVE '  QC' TO GOOD-STATUSCODES                                      
095600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
095700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095800     PERFORM IMS-STATUSCHECK                                              
095900     .                                                                    
096000     SKIP3                                                                
096100 IMS-INSERT-MSG SECTION.                                                  
096200                                                                          
096300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
096400     MOVE SPACE TO GOOD-STATUSCODES                                       
096500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
096600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096700     PERFORM IMS-STATUSCHECK                                              
096800     .                                                                    
096900     EJECT                                                                
097000 IMS-GU-6314 SECTION.                                                     
097100                                                                          
097200     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
097300          DELIMITED BY SIZE INTO SSA1                                     
097400     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
097500          DELIMITED BY SIZE INTO SSA2                                     
097600     MOVE '  GE' TO GOOD-STATUSCODES                                      
097700     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
097800     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSCHECK                                              
098000     .                                                                    
098100     SKIP3                                                                
098200 IMS-GU-6316 SECTION.                                                     
098300                                                                          
098400     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
098700          DELIMITED BY SIZE INTO SSA2                                     
098800     MOVE '  GE' TO GOOD-STATUSCODES                                      
098900     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
099000     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSCHECK                                              
099200     .                                                                    
099300     SKIP3                                                                
099400 IMS-GHU-LOCA SECTION.                                                    
099500                                                                          
099600     STRING 'WLLOCA01(WDJ801KY =' W-WDJ8KEY-X ')'                         
099700          DELIMITED BY SIZE INTO SSA1                                     
099800     MOVE '  GE' TO GOOD-STATUSCODES                                      
099900     CALL CBLTDLI USING GHU LOCA-PCB LOC-WDJ801 SSA1                      
100000     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
100100     PERFORM IMS-STATUSCHECK                                              
100200     .                                                                    
100300     SKIP3                                                                
100400 IMS-ISRT-LOCA-LOC SECTION.                                               
100500                                                                          
100600     MOVE 'WLLOCA01 ' TO SSA1                                             
100700     MOVE '  II' TO GOOD-STATUSCODES                                      
100800     CALL CBLTDLI USING ISRT LOCA-PCB LOC-WDJ801 SSA1                     
100900     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
101000     PERFORM IMS-STATUSCHECK                                              
101100     .                                                                    
101200     SKIP3                                                                
101300 IMS-REPL-LOCA-LOC SECTION.                                               
101400                                                                          
101500     MOVE '  ' TO GOOD-STATUSCODES                                        
101600     CALL CBLTDLI USING REPL LOCA-PCB LOC-WDJ801                          
101700     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
101800     PERFORM IMS-STATUSCHECK                                              
101900     .                                                                    
102000     SKIP3                                                                
102100 IMS-DLET-LOCA-LOC SECTION.                                               
102200                                                                          
102300     MOVE '  ' TO GOOD-STATUSCODES                                        
102400     CALL CBLTDLI USING DLET LOCA-PCB LOC-WDJ801                          
102500     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
102600     PERFORM IMS-STATUSCHECK                                              
102700     .                                                                    
102800     EJECT                                                                
102900                                                                          
103000 IMS-GU-WDB601    SECTION.                                                
103100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
103200          DELIMITED BY SIZE INTO SSA1                                     
103300     MOVE '  GE' TO GOOD-STATUSCODES                                      
103400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
103500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
103600     PERFORM IMS-STATUSCHECK                                              
103700     .                                                                    
103800     EJECT                                                                
103900 IMS-STATUSCHECK SECTION.                                                 
104000                                                                          
104100     SET STATUS-IX TO 1                                                   
104200     SEARCH GOOD-STATUS                                                   
104300       AT END                                                             
104400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
104500         DELIMITED BY SIZE INTO ERROR-TEXT                                
104600         CALL FELLOG                                                      
104700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
104800         CONTINUE                                                         
104900     END-SEARCH                                                           
105000     .                                                                    
