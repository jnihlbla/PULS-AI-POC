000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W200FCOI.                                                
000500 AUTHOR.         ARUP DATTA.                                              
000600 DATE-WRITTEN.   23/01/18.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SUBPROGRAM TO GET THE FOLLOWING :                                
001100*        -FC-REF AS IN 2342                                               
001200*        -OI DATA AS IN 2342                                              
001300*        -INCOMING ORDER QUANTITY 12 ROLL CDC AS IN 2104                  
001400*                                                                         
001500*        PROGRAMMET READS     WDK6                                        
001600*                             WDK7                                        
001700*                             WDL7                                        
001800*                             WDL8                                        
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400     SKIP2                                                                
003500*    -- CHECKED BY WY2000                                                 
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(8)    VALUE 'W200FCOI'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  FEL                         PIC X       VALUE 'F'.                   
004100 77  WS-CURRENT-SECTION          PIC X(80)   VALUE SPACES.                
004200     EJECT                                                                
004300                                                                          
004400 01  WORKING-FIELDS.                                                      
004500*                                                                         
004600*                                                                         
004700     03  INDX                    PIC  9(2)   VALUE ZERO.                  
004800     03  PER-IX                  PIC  9(2)   VALUE ZERO.                  
004900     03  FCOI-IX                 PIC  9(2)   VALUE ZERO.                  
005000     03  IX-VV                   PIC  9(2)   VALUE ZERO.                  
005100     03  VECKA-IX                PIC S9(9)   COMP SYNC.                   
005200*                                                                         
005300*PERIOD TABLES DEFINITIONS                                                
005400     03  WS-TABELL    OCCURS 12.                                          
005500         05 WS-TIAA              PIC  9(2)   VALUE ZERO.                  
005600         05 WS-PER               PIC  9(2)   VALUE ZERO.                  
005700         05 WS-FORSTA-V          PIC  9(2)   VALUE ZERO.                  
005800         05 WS-SISTA-V           PIC  9(2)   VALUE ZERO.                  
005900         05 WS-KVOI              PIC S9(7)   VALUE ZERO COMP-3.           
006000     03  WS-PER-TAB-AR-1 OCCURS 12.                                       
006100         05 WS-PER-AR-1          PIC  9(2)   VALUE ZERO.                  
006200         05 WS-PER-VV-1          PIC  9(2)   VALUE ZERO.                  
006300         05 WS-FORSTA-VV-1       PIC  9(2)   VALUE ZERO.                  
006400         05 WS-SISTA-VV-1        PIC  9(2)   VALUE ZERO.                  
006500     03  WS-PER-TAB-AR-0 OCCURS 12.                                       
006600         05 WS-PER-AR-0          PIC  9(2)   VALUE ZERO.                  
006700         05 WS-PER-VV-0          PIC  9(2)   VALUE ZERO.                  
006800         05 WS-FORSTA-VV-0       PIC  9(2)   VALUE ZERO.                  
006900         05 WS-SISTA-VV-0        PIC  9(2)   VALUE ZERO.                  
007000*                                                                         
007100*DATE/PERIOD  DECLARATIONS                                                
007200     03  ANT-VV                  PIC  9(2)   VALUE ZERO.                  
007300     03  WS-TIAAVV.                                                       
007400         05 WS-AAR               PIC 9(2)    VALUE ZERO.                  
007500         05 WS-VV                PIC 9(2)    VALUE ZERO.                  
007600     03  TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                             
007700     03  DAGENS-PER              PIC  9(4)   VALUE ZERO.                  
007800     03  DAG-PER REDEFINES DAGENS-PER.                                    
007900         05 DAGENS-AA            PIC  9(2).                               
008000         05 DAGENS-PP            PIC  9(2).                               
008100     03  WS-TISEKEL-AA.                                                   
008200         05  WS-DAGENS-SEKEL     PIC  99.                                 
008300         05  WS-DAGENS-AA        PIC  99.                                 
008400     03  FILLER REDEFINES WS-TISEKEL-AA.                                  
008500         05  WS-DAGENS-TIAAAA    PIC  9(4).                               
008600     03  WS-DAGENS-TIAAAA-1      PIC  9(4).                               
008700     03  WS-TIAAPER.                                                      
008800         05 TIAA                 PIC 9(2)    VALUE ZERO.                  
008900         05 PER                  PIC 9(2)    VALUE ZERO.                  
009000     03  TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                           
009100     03  WS-TIAAPER-1.                                                    
009200         05 TIAA-1               PIC 9(2)    VALUE ZERO.                  
009300         05 PER-1                PIC 9(2)    VALUE ZERO.                  
009400     03  TIAAPER-1 REDEFINES WS-TIAAPER-1 PIC 9(4).                       
009500     03  WS-FOM-TOM.                                                      
009600         05 WS-FOM               PIC  X(2)   VALUE ZERO.                  
009700         05 WS-STRECK            PIC  X(1)   VALUE '-'.                   
009800         05 WS-TOM               PIC  X(2)   VALUE ZERO.                  
009900*                                                                         
010000     03  WS-IDLANDX2             PIC  X(2)   VALUE SPACES.                
010100*                                                                         
010200*OI DECLATIONS                                                            
010300     03  WS-KVOI-SNITT-12-PROG   PIC S9(9)   VALUE ZERO.                  
010400     03  WS-KVOI-PROG-RULL-12    PIC S9(9)   VALUE ZERO.                  
010500     03  WS-KVOI-PROG            PIC S9(9)   VALUE ZERO.                  
010600     03  WS-KVOI-INNEV           PIC S9(9)   VALUE ZERO COMP-3.           
010700*                                                                         
010800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
010900     88  KEYS-OK                             VALUE 'J'.                   
011000     88  KEYS-ERR                            VALUE 'N'.                   
011100*                                                                         
011200 77  SW-FIRST-EXECUTE            PIC X(1)    VALUE 'J'.                   
011300     88 FIRST-RUN-JA                         VALUE 'J'.                   
011400     88 FIRST-RUN-NEJ                        VALUE 'N'.                   
011500*                                                                         
011600*                                                                         
011700*      --- VALID IDDC CODES                                               
011800*                                                                         
011900*01    -COPY WWDC99                                                       
012000       EJECT                                                              
012100*                                                                         
012200     EJECT                                                                
012300 01  DYNAMISKA-SUBPROGRAM.                                                
012400*                                                                         
012500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012900     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
013000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL POSTSUM                                          
013300*                                                                         
013400*01  -COPY W0005   -PRE  POSTSUM-                                         
013500     EJECT                                                                
013600 01  FILLER                  PIC X(16) VALUE 'WDATAREA        '.          
013700*    ---PARAMETRAR TILL DATKONV                                           
013800*01  -COPY WDATAREA                                                       
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL WZ20DAYS                                         
014100*01 -COPY WZ20DAYS                                                        
014200     EJECT                                                                
014300*    ---VARIABLES TO SUBPROGRAM W009VADD                                  
014400 01  DATUM-AAVV                  PIC S9(5)   COMP-3.                      
014500 01  ANTAL-VECKOR                PIC S9(3)   COMP-3.                      
014600     EJECT                                                                
014700 01  FELTEXT.                                                             
014800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015000     EJECT                                                                
015100 01  MESSAGE-CODES.                                                       
015200     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
015300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015400     03  ERR-DATE                PIC X(3)    VALUE '957'.                 
015500                                                                          
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800     EJECT                                                                
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200     03  W-IDARTNR-X.                                                     
016300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016400     03  W-KDSEGKEY-X.                                                    
016500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016600     03  W-IDDC-X.                                                        
016700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016800     03  W-IDDC-B6-X.                                                     
016900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
017000     03  W-TIAAAA-X.                                                      
017100         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
017200                                                                          
017300     SKIP2                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017900     SKIP2                                                                
018000 01  GODK-STATUSKODER.                                                    
018100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018200     SKIP3                                                                
018300 01  SSA1                        PIC X(64).                               
018400 01  SSA2                        PIC X(64).                               
018500 01  SSA3                        PIC X(64).                               
018600     EJECT                                                                
018700*    --- IMS FUNKTIONSKODER                                               
018800*01  -COPY W0003                                                          
018900     EJECT                                                                
019000*    ---  DLI INPUT-OUTPUT AREA                                           
019100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
019200 01  DLI-IO-AREA-WDK701.                                                  
019300*        05  -COPY WDK701                                                 
019400     EJECT                                                                
019500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
019600 01  DLI-IO-AREA-WDK711.                                                  
019700*        05  -COPY WDK711                                                 
019800     EJECT                                                                
019900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL801'.             
020000 01  DLI-IO-AREA-WDL801.                                                  
020100*        05  -COPY WDL801  -PRE WDL8-                                     
020200     EJECT                                                                
020300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
020400 01  DLI-IO-AREA-WDL811.                                                  
020500*        05  -COPY WDL811                                                 
020600     EJECT                                                                
020700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL711'.             
020800 01  DLI-IO-AREA-WDL711.                                                  
020900*        05  -COPY WDL711                                                 
021000     EJECT                                                                
021100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
021200 01  DLI-IO-AREA-WDB601.                                                  
021300*        05  -COPY WDB601                                                 
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600                                                                          
021700*    -COPY W200FCOI                                                       
021800     EJECT                                                                
021900*01  -COPY W0008      -PRE WDB6-                                          
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008      -PRE WDK7-                                          
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008      -PRE WDL7-                                          
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008      -PRE WDL8-                                          
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100 PROCEDURE DIVISION  USING FCOI-W200FCOI                                  
023200                           WDB6-PCB                                       
023300                           WDK7-PCB                                       
023400                           WDL7-PCB                                       
023500                           WDL8-PCB.                                      
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023900     PERFORM B-VALIDATE-INPUT                                             
024000     IF KEYS-OK                                                           
024100        IF CDC-SE                                                         
024200           PERFORM E-INCOMING-ORDER-CDC                                   
024300           PERFORM S03-INIT-OI-DC                                         
024400        ELSE                                                              
024500           PERFORM C-GET-PART-INFO-WDK7                                   
024600           IF KEYS-OK                                                     
024700              PERFORM D-GET-OI-DC                                         
024800              PERFORM E-INCOMING-ORDER-CDC                                
024900           END-IF                                                         
025000        END-IF                                                            
025100     END-IF                                                               
025200                                                                          
025300     SET FIRST-RUN-NEJ              TO TRUE                               
025400     MOVE ZERO TO RETURN-CODE                                             
025500     GOBACK                                                               
025600     .                                                                    
025700     EJECT                                                                
025800                                                                          
025900 A-INIT SECTION.                                                          
026000                                                                          
026100     MOVE SPACES                    TO FCOI-KDSVAR                        
026200                                       FCOI-FEL-TEXT                      
026300                                       WS-IDLANDX2                        
026400                                       WS-IDDC                            
026500                                                                          
026600***                                                                       
026700***  CALCULATE LAST 12 PERIODS AND CORRESPONDING WEEKS                    
026800***                                                                       
026900     IF FIRST-RUN-JA                                                      
027000        PERFORM S01-CALC-VV-I-PER                                         
027100     END-IF                                                               
027200     PERFORM S02-INIT-OI                                                  
027300     .                                                                    
027400     EJECT                                                                
027500                                                                          
027600 B-VALIDATE-INPUT SECTION.                                                
027700                                                                          
027800     IF FCOI-IDARTNR-IN NUMERIC AND                                       
027900        FCOI-IDARTNR-IN > ZERO                                            
028000        CONTINUE                                                          
028100     ELSE                                                                 
028200       MOVE NEJ                     TO KEYS-SW                            
028300       SET  FCOI-KDSVAR-FEL         TO TRUE                               
028400       MOVE '022'                   TO FCOI-IDMSG-ERROR                   
028500       MOVE 'IDARTNR'               TO FCOI-IDELMT-ERROR                  
028600       MOVE 'INVALID PART NUM '     TO FCOI-FEL-TEXT                      
028700     END-IF                                                               
028800*                                                                         
028900     IF FCOI-IDDC-IN > SPACES                                             
029000        MOVE FCOI-IDDC-IN           TO W-IDDC-B6                          
029100                                       WS-IDDC                            
029200                                                                          
029300        PERFORM IMS-GU-WDB601                                             
029400        IF SEGMENT-SAKNAS                                                 
029500           MOVE NEJ                 TO KEYS-SW                            
029600           SET FCOI-KDSVAR-FEL      TO TRUE                               
029700           MOVE '022'               TO FCOI-IDMSG-ERROR                   
029800           MOVE 'IDDC'              TO FCOI-IDELMT-ERROR                  
029900           MOVE 'INVALID DC VALUE ' TO FCOI-FEL-TEXT                      
030000        ELSE                                                              
030100           MOVE DCS-IDLANDX2        TO WS-IDLANDX2                        
030200                                       FCOI-IDLANDX2                      
030300        END-IF                                                            
030400     ELSE                                                                 
030500        MOVE NEJ                    TO KEYS-SW                            
030600        SET FCOI-KDSVAR-FEL         TO TRUE                               
030700        MOVE '020'                  TO FCOI-IDMSG-ERROR                   
030800        MOVE 'IDDC'                 TO FCOI-IDELMT-ERROR                  
030900        MOVE 'ENTER DC VALUE   '    TO FCOI-FEL-TEXT                      
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300                                                                          
031400 C-GET-PART-INFO-WDK7 SECTION.                                            
031500                                                                          
031600     IF  FCOI-IDARTNR-IN   IS NUMERIC                                     
031700     AND FCOI-IDARTNR-IN    > ZERO                                        
031800         MOVE FCOI-IDARTNR-IN       TO W-IDARTNR                          
031900         MOVE FCOI-IDDC-IN          TO W-IDDC                             
032000         PERFORM IMS-GU-WDK711                                            
032100         IF SEGMENT-FINNS                                                 
032200            MOVE SLAG-IDDC          TO FCOI-IDDC                          
032300            MOVE SLAG-KVPB-REF      TO FCOI-KVPB-REF                      
032400         ELSE                                                             
032500            SET FCOI-KDSVAR-FEL     TO TRUE                               
032600            MOVE '025'              TO FCOI-IDMSG-ERROR                   
032700*           NOT FOUND ***                                                 
032800            MOVE 'IDARTNR'          TO FCOI-IDELMT-ERROR                  
032900            MOVE 'MISSING IN WDK711'                                      
033000                                    TO FCOI-FEL-TEXT                      
033100         END-IF                                                           
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500                                                                          
033600 D-GET-OI-DC      SECTION.                                                
033700                                                                          
033800*    --- UPDATE TABLE WITH OI INFORMATION                                 
033900                                                                          
034000     MOVE FCOI-IDARTNR-IN           TO W-IDARTNR                          
034100     MOVE FCOI-IDDC-IN              TO W-IDDC                             
034200     PERFORM IMS-GU-WDL711                                                
034300     IF SEGMENT-FINNS                                                     
034400        MOVE +1                     TO INDX                               
034500        PERFORM UNTIL INDX           > 12                                 
034600          MOVE WS-FORSTA-V(INDX)    TO IX-VV                              
034700          PERFORM UNTIL IX-VV        > WS-SISTA-V(INDX)                   
034800            ADD DC-KVOI-RULL(IX-VV) TO WS-KVOI(INDX)                      
034900            ADD +1                  TO IX-VV                              
035000          END-PERFORM                                                     
035100          ADD +1                    TO INDX                               
035200        END-PERFORM                                                       
035300                                                                          
035400*       --- POPULATE OUTPUT FROM WORKING TABLE                            
035500*       --- SHOW ONLY THE LAST PERIODS                                    
035600                                                                          
035700        MOVE +7                   TO INDX                                 
035800        MOVE +1                   TO FCOI-IX                              
035900        PERFORM UNTIL INDX > +12                                          
036000          MOVE WS-PER(INDX)       TO FCOI-TIPP(FCOI-IX)                   
036100          MOVE WS-FORSTA-V(INDX)  TO WS-FOM                               
036200          MOVE WS-SISTA-V(INDX)   TO WS-TOM                               
036300          MOVE WS-FOM-TOM         TO FCOI-TIVV-FOM-TOM(FCOI-IX)           
036400          MOVE WS-KVOI(INDX)      TO FCOI-KVOI(FCOI-IX)                   
036500          ADD +1                  TO INDX                                 
036600                                     FCOI-IX                              
036700        END-PERFORM                                                       
036800                                                                          
036900*       --- INNEVARANDE PERIODS OI/OT                                     
037000                                                                          
037100        MOVE +1                   TO INDX                                 
037200        PERFORM UNTIL INDX         > +5                                   
037300          ADD DC-KVOI-INNEV(INDX) TO WS-KVOI-INNEV                        
037400          ADD +1                  TO INDX                                 
037500        END-PERFORM                                                       
037600        MOVE WS-KVOI-INNEV        TO FCOI-KVOI-INNEV                      
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 E-INCOMING-ORDER-CDC SECTION.                                            
038100                                                                          
038200     IF FIRST-RUN-JA                                                      
038300        PERFORM EA-CALC-OI-CDC                                            
038400     END-IF                                                               
038500     PERFORM EB-POPULATE-OI-CDC                                           
038600     .                                                                    
038700     EJECT                                                                
038800 EA-CALC-OI-CDC SECTION.                                                  
038900*    *****  CALCULATE DATE                                                
039000*                                                                         
039100     MOVE FCOI-IDARTNR-IN         TO W-IDARTNR                            
039200     MOVE ZERO                    TO WS-KVOI-PROG-RULL-12                 
039300                                     WS-KVOI-SNITT-12-PROG                
039400***                                                                       
039500***  PREVIOUS YEAR   ************                                         
039600***                                                                       
039700     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA-1                                
039800     PERFORM IMS-GET-WDL811                                               
039900                                                                          
040000     IF SEGMENT-FINNS                                                     
040100       MOVE 1                     TO INDX                                 
040200       MOVE ZERO                  TO WS-KVOI-PROG                         
040300       PERFORM UNTIL INDX     > 12                                        
040400         IF WS-PER-VV-1(INDX) > ZERO                                      
040500            MOVE WS-FORSTA-VV-1 (INDX)                                    
040600                                  TO VECKA-IX                             
040700            MOVE INDX             TO PER-IX                               
040800            MOVE 12               TO INDX                                 
040900         END-IF                                                           
041000         ADD 1                    TO INDX                                 
041100       END-PERFORM                                                        
041200                                                                          
041300       PERFORM UNTIL VECKA-IX > +52                                       
041400                                                                          
041500         PERFORM UNTIL VECKA-IX > WS-SISTA-VV-1 (PER-IX)                  
041600                                                                          
041700           ADD AAR-KVOI-PROG (VECKA-IX)                                   
041800                                  TO WS-KVOI-PROG                         
041900           ADD +1                 TO VECKA-IX                             
042000         END-PERFORM                                                      
042100                                                                          
042200         ADD +1                   TO PER-IX                               
042300       END-PERFORM                                                        
042400       ADD WS-KVOI-PROG           TO WS-KVOI-PROG-RULL-12                 
042500     END-IF                                                               
042600***                                                                       
042700***  CURRENT YEAR  ************                                           
042800***                                                                       
042900     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA                                  
043000     PERFORM IMS-GET-WDL811                                               
043100                                                                          
043200     IF SEGMENT-FINNS                                                     
043300                                                                          
043400       MOVE 1                     TO INDX                                 
043500       PERFORM UNTIL INDX  > 12                                           
043600         IF WS-PER-VV-0(INDX) > ZERO                                      
043700            MOVE WS-FORSTA-VV-0 (INDX)                                    
043800                                  TO VECKA-IX                             
043900            MOVE INDX             TO PER-IX                               
044000            MOVE 12               TO INDX                                 
044100         END-IF                                                           
044200         ADD 1                    TO INDX                                 
044300       END-PERFORM                                                        
044400                                                                          
044500       IF DAGENS-PP > 1                                                   
044600          MOVE ZERO               TO WS-KVOI-PROG                         
044700          PERFORM UNTIL VECKA-IX                                          
044800                              > WS-SISTA-VV-0 (DAGENS-PP - 1)             
044900            PERFORM UNTIL VECKA-IX                                        
045000                              > WS-SISTA-VV-0 (PER-IX)                    
045100                                                                          
045200              ADD AAR-KVOI-PROG (VECKA-IX)                                
045300                                  TO WS-KVOI-PROG                         
045400              ADD +1              TO VECKA-IX                             
045500            END-PERFORM                                                   
045600                                                                          
045700            ADD +1                TO PER-IX                               
045800          END-PERFORM                                                     
045900          ADD WS-KVOI-PROG        TO WS-KVOI-PROG-RULL-12                 
046000       END-IF                                                             
046100     END-IF                                                               
046200*    CALCUATE PB-TOT FOR THE WEEK.                                        
046300*    INCLUDE FUTURE FORECAST IN CALCULATION                               
046400                                                                          
046500     COMPUTE WS-KVOI-SNITT-12-PROG ROUNDED =                              
046600             WS-KVOI-PROG-RULL-12 / 12                                    
046700     .                                                                    
046800     EJECT                                                                
046900 EB-POPULATE-OI-CDC SECTION.                                              
047000                                                                          
047100     MOVE WS-KVOI-PROG-RULL-12    TO FCOI-KVOI-RULL-12-CDC                
047200     MOVE WS-KVOI-SNITT-12-PROG   TO FCOI-AARSFORB-CDC                    
047300     .                                                                    
047400     EJECT                                                                
047500                                                                          
047600 S01-CALC-VV-I-PER SECTION.                                               
047700                                                                          
047800     MOVE 'IDAG  '           TO DAT-KDDATFORM                             
047900                                                                          
048000     CALL WDATKONV  USING  DAT-KDDATFORM                                  
048100                           DAT-I-TIDATUM                                  
048200                           DAT-O-TIDATUM                                  
048300                           DAT-KDSVAR                                     
048400                                                                          
048500     MOVE DAT-TISEKEL        TO WS-DAGENS-SEKEL                           
048600     MOVE DAT-TIAA           TO WS-DAGENS-AA                              
048700     MOVE DAT-TIAARP         TO DAGENS-PER                                
048800*                                                                         
048900     COMPUTE WS-DAGENS-TIAAAA-1 = WS-DAGENS-TIAAAA - 1                    
049000*                                                                         
049100     MOVE +1                 TO INDX                                      
049200     MOVE DAGENS-PER         TO WS-TIAAPER                                
049300                                                                          
049400     IF TIAA = 00                                                         
049500        MOVE    99           TO TIAA                                      
049600     ELSE                                                                 
049700        SUBTRACT 1         FROM TIAA                                      
049800     END-IF                                                               
049900     MOVE WS-TIAAPER         TO WS-TIAAPER-1                              
050000                                                                          
050100*    --- CALCULATE HOW MANY WEEKS WAS LAST YEAR                           
050200     MOVE TIAA               TO WS-AAR                                    
050300     MOVE 53                 TO WS-VV                                     
050400     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
050500     MOVE TIAAVV             TO DAT-I-TIDATUM                             
050600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
050700                         DAT-O-TIDATUM DAT-KDSVAR                         
050800     IF DAT-KDSVAR-OK                                                     
050900       MOVE 53               TO ANT-VV                                    
051000     ELSE                                                                 
051100       MOVE 52               TO ANT-VV                                    
051200     END-IF                                                               
051300                                                                          
051400*    --- CALCULATE THE WEEK NUMBERS FOR THE PERIODS                       
051500                                                                          
051600     MOVE 'AARP  '            TO DAT-KDDATFORM                            
051700     MOVE TIAAPER             TO DAT-I-TIDATUM                            
051800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
051900                         DAT-O-TIDATUM DAT-KDSVAR                         
052000     IF DAT-KDSVAR-OK                                                     
052100       IF PER  = 1                                                        
052200         MOVE 1               TO WS-PER(INDX)                             
052300                                WS-FORSTA-V(INDX)                         
052400       ELSE                                                               
052500         MOVE PER             TO WS-PER(INDX)                             
052600         MOVE DAT-TIVV        TO WS-FORSTA-V(INDX)                        
052700       END-IF                                                             
052800       MOVE TIAA              TO WS-TIAA(INDX)                            
052900     ELSE                                                                 
053000       MOVE  FEL              TO FCOI-KDSVAR                              
053100       MOVE '023'             TO FCOI-IDMSG-ERROR                         
053200*      IS INVALID ***                                                     
053300       MOVE 'TIAAPER'          TO FCOI-IDELMT-ERROR                       
053400       MOVE 'DATE CONV ERR 1' TO FCOI-FEL-TEXT                            
053500     END-IF                                                               
053600*                                                                         
053700     PERFORM UNTIL INDX > 12                                              
053800       ADD +1                 TO PER                                      
053900       IF PER  >  12                                                      
054000         ADD +1               TO TIAA                                     
054100         MOVE +1              TO PER                                      
054200       END-IF                                                             
054300                                                                          
054400       MOVE TIAAPER           TO DAT-I-TIDATUM                            
054500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
054600                           DAT-O-TIDATUM DAT-KDSVAR                       
054700       IF DAT-KDSVAR-OK                                                   
054800         IF PER    = 1                                                    
054900           MOVE ANT-VV        TO WS-SISTA-V(INDX)                         
055000         ELSE                                                             
055100           COMPUTE WS-SISTA-V(INDX) = DAT-TIVV - 1                        
055200         END-IF                                                           
055300         ADD +1               TO INDX                                     
055400         IF INDX  <= 12                                                   
055500           MOVE TIAA          TO WS-TIAA(INDX)                            
055600           MOVE PER           TO WS-PER(INDX)                             
055700           IF PER  =  1                                                   
055800             MOVE +1          TO WS-FORSTA-V(INDX)                        
055900           ELSE                                                           
056000             MOVE DAT-TIVV    TO WS-FORSTA-V(INDX)                        
056100           END-IF                                                         
056200         END-IF                                                           
056300       ELSE                                                               
056400         MOVE FEL               TO FCOI-KDSVAR                            
056500         MOVE '023'             TO FCOI-IDMSG-ERROR                       
056600*        IS INVALID ***                                                   
056700         MOVE 'TIAAPER'         TO FCOI-IDELMT-ERROR                      
056800         MOVE 'DATE CONV ERR 2' TO FCOI-FEL-TEXT                          
056900       END-IF                                                             
057000     END-PERFORM                                                          
057100*    --- OM VECKOR I FÖRSTA OCH SISTA PERIODEN ÖVERLAPPAR,                
057200*    --- RÄTTA I FÖRSTA (DVS DEN ÄLDSTA) PERIODEN.                        
057300     IF WS-FORSTA-V(1)        = WS-SISTA-V(12)                            
057400     OR WS-FORSTA-V(1)        = WS-SISTA-V(12) - 1                        
057500       COMPUTE WS-FORSTA-V(1) = WS-SISTA-V(12) + 1                        
057600     END-IF                                                               
057700*                                                                         
057800     MOVE 1                         TO INDX                               
057900                                       PER-IX                             
058000     PERFORM UNTIL INDX > 12                                              
058100       IF WS-TIAA(INDX) = TIAA-1                                          
058200          MOVE WS-PER(INDX)         TO PER-IX                             
058300                                       WS-PER-AR-1    (PER-IX)            
058400          MOVE WS-PER(INDX)         TO WS-PER-VV-1    (PER-IX)            
058500          MOVE WS-FORSTA-V(INDX)    TO WS-FORSTA-VV-1 (PER-IX)            
058600          MOVE WS-SISTA-V (INDX)    TO WS-SISTA-VV-1  (PER-IX)            
058700       ELSE                                                               
058800          IF WS-TIAA(INDX) = TIAA                                         
058900             MOVE WS-PER(INDX)      TO PER-IX                             
059000                                       WS-PER-AR-0    (PER-IX)            
059100             MOVE WS-PER(INDX)      TO WS-PER-VV-0    (PER-IX)            
059200             MOVE WS-FORSTA-V(INDX) TO WS-FORSTA-VV-0 (PER-IX)            
059300             MOVE WS-SISTA-V (INDX) TO WS-SISTA-VV-0  (PER-IX)            
059400          END-IF                                                          
059500       END-IF                                                             
059600       ADD 1                        TO INDX                               
059700     END-PERFORM                                                          
059800     .                                                                    
059900     EJECT                                                                
060000 S02-INIT-OI SECTION.                                                     
060100                                                                          
060200     MOVE ZERO                      TO WS-KVOI-INNEV                      
060300*                                                                         
060400     MOVE 1                         TO INDX                               
060500     PERFORM UNTIL INDX > 12                                              
060600       MOVE ZERO                    TO WS-KVOI (INDX)                     
060700       MOVE ALL ZERO                TO WS-FOM                             
060800                                       WS-TOM                             
060900       ADD 1                        TO INDX                               
061000     END-PERFORM                                                          
061100     .                                                                    
061200     EJECT                                                                
061300                                                                          
061400 S03-INIT-OI-DC SECTION.                                                  
061500                                                                          
061600*    IF ONLY CDC OI REQUESTED, ZERO OUT THE REMAINING FILEDS              
061700*                                                                         
061800     MOVE ZERO                      TO FCOI-KVOI-INNEV                    
061900                                       FCOI-KVPB-REF                      
062000*                                                                         
062100     MOVE 1                         TO INDX                               
062200     PERFORM UNTIL INDX > 6                                               
062300       MOVE ZERO                    TO FCOI-TIPP (INDX)                   
062400                                       FCOI-KVOI (INDX)                   
062500       MOVE WS-FOM-TOM              TO FCOI-TIVV-FOM-TOM (INDX)           
062600       ADD 1                        TO INDX                               
062700     END-PERFORM                                                          
062800*                                                                         
062900     MOVE FCOI-IDDC-IN              TO FCOI-IDDC                          
063000     .                                                                    
063100     EJECT                                                                
063200                                                                          
063300                                                                          
063400* --- IMS SEKTIONER ---                                                   
063500     SKIP3                                                                
063600 IMS-GU-WDB601    SECTION.                                                
063700                                                                          
063800     MOVE 'IMS-GU-WDB601          ' TO WS-CURRENT-SECTION                 
063900                                                                          
064000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
064100          DELIMITED BY SIZE INTO SSA1                                     
064200     MOVE '  GE'            TO GODK-STATUSKODER                           
064300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
064400     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 IMS-GU-WDK711 SECTION.                                                   
065000                                                                          
065100     MOVE 'IMS-GU-WDK701          ' TO WS-CURRENT-SECTION                 
065200                                                                          
065300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
065400          DELIMITED BY SIZE INTO SSA1                                     
065500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
065600          DELIMITED BY SIZE INTO SSA2                                     
065700     MOVE '  GE'            TO GODK-STATUSKODER                           
065800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
065900     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     EJECT                                                                
066300 IMS-GET-WDL811  SECTION.                                                 
066400                                                                          
066500     MOVE 'IMS-GU-WDL811          ' TO WS-CURRENT-SECTION                 
066600                                                                          
066700     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
066800          DELIMITED BY SIZE INTO SSA1                                     
066900     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
067000          DELIMITED BY SIZE INTO SSA2                                     
067100     MOVE '  GE'            TO GODK-STATUSKODER                           
067200     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-AREA-WDL811 SSA1 SSA2          
067300     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
067400     PERFORM IMS-STATUSKONTROLL                                           
067500     .                                                                    
067600     EJECT                                                                
067700 IMS-GU-WDL711     SECTION.                                               
067800     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
067900          DELIMITED BY SIZE INTO SSA1                                     
068000     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
068100          DELIMITED BY SIZE INTO SSA2                                     
068200     MOVE '  GE' TO GODK-STATUSKODER                                      
068300     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-AREA-WDL711 SSA1 SSA2          
068400     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
068500     PERFORM IMS-STATUSKONTROLL                                           
068600     .                                                                    
068700     EJECT                                                                
068800                                                                          
068900 IMS-STATUSKONTROLL SECTION.                                              
069000                                                                          
069100     SET STATUS-IX TO 1                                                   
069200     SEARCH GODK-STATUS                                                   
069300       AT END                                                             
069400         STRING 'FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                 
069500         DELIMITED BY SIZE INTO FELTEXT                                   
069600         CALL FELLOG                                                      
069700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
069800         CONTINUE                                                         
069900     END-SEARCH                                                           
070000     .                                                                    
070100     EJECT                                                                
