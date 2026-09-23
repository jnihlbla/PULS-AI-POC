000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4739000.                                        
000300*              PROGRAM CONVERTED BY                                       
000400*              COBOL CONVERSION AID PO 5785-ABJ                           
000500*              CONVERSION DATE 05/25/91 16:11:59.                         
000600*AUTHOR.                 BS DAPRO.                                        
000700*DATE-WRITTEN.           MAJ 1987.                                        
000800                                                                          
000900*REMARKS.                                                                 
001000                                                                          
001100*    FUNKTION:                                                            
001200*        PROGRAMMET FÅR IDENTITER PÅ FÄRDIGPACKADE ORDER                  
001300*        SVERIGE KVANT (KLASS 4).                                         
001400*       KOLLIN TILL DESSA ORDER LÄSES UT FRÅN KOLLIREG.                   
001500*                                                                         
001600*        TVÅ FILER SKAPAS: - FIL TILL EMBALLAGEPROFORMA.                  
001700*                          - FIL TILL "PACKSEDEL"                         
001800*                            (LISTA PÅ ORDERNS KOLLIN)                    
001900*    ABENDKODER:                                                          
002000*        U0016 - OM RETURKOD FRÅN SORT (EX.VIS FÖR LITE SORTWK)           
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*    ---- INFILER:                                                        
002900*                            - FÄRDIGPACKADE ORDER                        
003000     SELECT  W092ZW-RY6    ASSIGN  W47390D1.                              
003100     SKIP2                                                                
003200*    ---- UTFILER:                                                        
003300     SELECT  W47390-EMB    ASSIGN  W47390D3.                              
003400     SKIP2                                                                
003500*    ---- SORTFIL:                                                        
003600*                                                                         
003700     SELECT  SORTFIL       ASSIGN  W47390DS.                              
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000                                                                          
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W092ZW-RY6                                                           
004400     RECORDING  F                                                         
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700*    -COPY WDGZRY6       -L.                                              
004800                                                                          
004900 FD  W47390-EMB                                                           
005000     RECORDING  F                                                         
005100     BLOCK CONTAINS 0.                                                    
005200                                                                          
005300*01  POST   -COPY W47390       -PRE EMB-   -L.                            
005400     EJECT                                                                
005500 SD  SORTFIL                                                              
005600                .                                                         
005700                                                                          
005800*01  POST   -COPY WDGZRY6      -PRE SORT-.                                
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4739000'.                
006200 77  JA                      PIC X       VALUE 'J'.                       
006300 77  NEJ                     PIC X       VALUE 'N'.                       
006400     SKIP2                                                                
006500*    ---- INDEXFÄLT                                                       
006600                                                                          
006700 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
006800     SKIP2                                                                
006900*    ---- END-OF-FILE SWITCHAR                                            
007000                                                                          
007100 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
007200     SKIP2                                                                
007300*    ---- ÖVRIGA SWITHCAR                                                 
007400                                                                          
007500 77  EMB-PROF                PIC X       VALUE 'N'.                       
007600     EJECT                                                                
007700*01  -COPY WWDC99                                                         
007800     EJECT                                                                
007900*    ---- DYNAMISKA SUBPROGRAM                                            
008000                                                                          
008100 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
008200 01  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.                
008300 01  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.                
008400 01  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.                
008500     SKIP3                                                                
008600 01  WS-KDKOLLI.                                                          
008700     03  WS-KDKOLLI-1        PIC X.                                       
008800     03  WS-KDKOLLI-2        PIC X.                                       
008900     03  WS-KDKOLLI-3        PIC X.                                       
009000     03  FILLER              PIC X(5).                                    
009100 01  FILLER   REDEFINES  WS-KDKOLLI.                                      
009200     03  FILLER              PIC X.                                       
009300     03  WS-KDEMB            PIC 9(3).                                    
009400     03  WS-KDEMB2           PIC 9(1).                                    
009500     03  FILLER              PIC 9(3).                                    
009600     SKIP1                                                                
009700 01  WS-KVPALL               PIC 9(3).                                    
009800 01  WS-KVLOCK               PIC 9(3).                                    
009900 01  WS-KVEMBSPA             PIC 9(3).                                    
010000 01  WS-KVRAM-X.                                                          
010100     03  WS-KVRAM            PIC 9.                                       
010200     EJECT                                                                
010300 01  FILLER          PIC X(16)   VALUE 'WWDIST04   '.                     
010400*01  FILLER -COPY WWDIST04                                                
010500     EJECT                                                                
010600*    ---- PARAMETRAR TILL ABEND                                           
010700                                                                          
010800 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
010900     EJECT                                                                
011000*    ----  PARAMETRAR TILL POSTSUM                                        
011100                                                                          
011200*01  -COPY W0005       -PRE POSTSUM-.                                     
011300     EJECT                                                                
011400*    ----  AREA FÖR SORTERADE TRANSAKTIONER                               
011500 01  FILLER          PIC X(24)   VALUE 'WSORT-AREA-START'.                
011600     SKIP2                                                                
011700*01  AREA  -COPY WDGZRY6     -PRE  WSORT-.                                
011800     EJECT                                                                
011900*    ----  AREA FÖR EMB-PROF                                              
012000 01  FILLER          PIC X(24)   VALUE 'EMB-AREA-START'.                  
012100     SKIP2                                                                
012200*01  AREA   -COPY W47390      -PRE EMB-.                                  
012300     EJECT                                                                
012400*    ----   ARBETSAREOR TILL IMS-SEKTIONERNA                              
012500 01  FILLER          PIC X(24)   VALUE 'IMS-WS-START'.                    
012600                                                                          
012700*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
012800 01  W-IDPRODNR-X.                                                        
012900     03  W-IDPRODNR          PIC S9(7)    COMP-3.                         
013000                                                                          
013100     SKIP3                                                                
013200*    --- STATUSKOD FRÅN IMS                                               
013300 01  STATUS-WS                   PIC XX.                                  
013400     88  SEGMENT-FINNS                       VALUE '  '.                  
013500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013700     SKIP3                                                                
013800 01  GODK-STATUSKODER.                                                    
013900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000     SKIP3                                                                
014100 01  SSA1                        PIC X(64).                               
014200     EJECT                                                                
014300*01  -COPY W0003                                                          
014400     EJECT                                                                
014500 01  FILLER              PIC X(16)   VALUE 'DLI-IO-E601'.                 
014600 01  DLI-IO-E601.                                                         
014700*    03  -COPY WDE601                                                     
014800     EJECT                                                                
014900 01  FILLER              PIC X(16)   VALUE 'DLI-IO-E611'.                 
015000 01  DLI-IO-E611.                                                         
015100*    03  -COPY WDE611                                                     
015200     EJECT                                                                
015300 LINKAGE SECTION.                                                         
015400     SKIP3                                                                
015500*01  -COPY W0008      -PRE  WDE6-                                         
015600       05  FILLER                PIC X.                                   
015700     EJECT                                                                
015800 PROCEDURE DIVISION  USING  WDE6-PCB.                                     
015900     ENTRY 'DLITCBL' USING  WDE6-PCB.                                     
016000     SKIP2                                                                
016100     PERFORM A-INIT                                                       
016200                                                                          
016300     SORT SORTFIL                                                         
016400       ASCENDING SORT-RY6-IDDISTR                                         
016500                 SORT-RY6-IDKUNDNR                                        
016600                 SORT-RY6-IDKUNDRF                                        
016700       USING W092ZW-RY6                                                   
016800       OUTPUT PROCEDURE B-LAS-BAS-SKAPA-FILER                             
016900                                                                          
017000     IF  SORT-RETURN > ZERO                                               
017100       DISPLAY '*** W4739000 - FEL VID SORTERING'                         
017200       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017300     ELSE                                                                 
017400       PERFORM Z-FINIT                                                    
017500       MOVE ZERO TO RETURN-CODE                                           
017600       GOBACK                                                             
017700     END-IF                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT       SECTION.                                                    
018100                                                                          
018200     OPEN OUTPUT W47390-EMB                                               
018300                                                                          
018400     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
018500     .                                                                    
018600     EJECT                                                                
018700 B-LAS-BAS-SKAPA-FILER SECTION.                                           
018800                                                                          
018900     PERFORM BD-LAS-SORTERAD-RY6                                          
019000                                                                          
019100     PERFORM UNTIL                                                        
019200      NOT ( SORTFIL-EOF = NEJ )                                           
019300       MOVE WSORT-RY6-IDDISTR    TO DIST04-IDDISTR-EXP                    
019400       MOVE WSORT-RY6-IDPRODNR   TO W-IDPRODNR                            
019500       PERFORM IMS-GU-WDE601                                              
019600*CO - OBS! OBS!                                                           
019700* HÄR KOLLAR JAG OM RÄTT URVAL...                                         
019800* DC. 11 OCH RÄTT DISTRIKT < 100 (INTERNA DISTR.)                         
019900* DESSA DISTRIKT ÄR INTE BESTÄMDA ÄN                                      
020000* NÄR DET BLIR DET, TAR MAN BARA BORT '*' FRÅN KODEN.                     
020100* OBS: KOLLA ÄVEN SECTION BC-SKRIV-EMBPROF.                               
020200*                                                                         
020300       MOVE WSORT-RY6-IDDC     TO WS-IDDC                                 
020400       IF CDC-SE                                                          
020500         CONTINUE                                                         
020600*        IF DIST04-EMB-INT                                                
020700*          PERFORM BA-BEHANDLA-HUV                                        
020800*                                                                         
020900*          PERFORM IMS-GNP-WDE611                                         
021000*          PERFORM UNTIL SEGMENT-SAKNAS                                   
021100*            PERFORM BB-BEHANDLA-KOLLI                                    
021200*                                                                         
021300*            PERFORM IMS-GNP-WDE611                                       
021400*          END-PERFORM                                                    
021500*          IF EMB-PROF = JA                                               
021600*            IF VORD-FLDIRLEV = NEJ                                       
021700*              PERFORM BF-BEHANDLA-DC                                     
021800*              PERFORM BC-SKRIV-EMBPROF                                   
021900*              MOVE NEJ TO EMB-PROF                                       
022000*            ELSE                                                         
022100*              MOVE NEJ TO EMB-PROF                                       
022200*            END-IF                                                       
022300*          END-IF                                                         
022400*        END-IF                                                           
022500       END-IF                                                             
022600       PERFORM BD-LAS-SORTERAD-RY6                                        
022700                                                                          
022800     END-PERFORM                                                          
022900     .                                                                    
023000     EJECT                                                                
023100 BA-BEHANDLA-HUV SECTION.                                                 
023200                                                                          
023300     MOVE SPACE              TO EMB-AREA                                  
023400     MOVE WSORT-RY6-IDDISTR  TO EMB-IDDISTR                               
023500     MOVE WSORT-RY6-IDKUNDNR TO EMB-IDKUNDNR                              
023600     MOVE WSORT-RY6-IDKUNDRF TO EMB-IDKUNDRF                              
023700     MOVE WSORT-RY6-IDPRODNR TO EMB-IDPRODNR                              
023800     MOVE WSORT-RY6-IDDC     TO EMB-IDDC-SEND                             
023900     MOVE VORD-KDORDKL       TO EMB-KDORDKL                               
024000     MOVE ZERO               TO EMB-IDFAKT                                
024100     MOVE SPACE              TO EMB-KDFAKTYP                              
024200                                                                          
024300     MOVE +1                 TO IX                                        
024400     PERFORM UNTIL                                                        
024500      NOT ( IX < 25)                                                      
024600       MOVE ZERO          TO EMB-KVPALL (IX)                              
024700                             EMB-KVRAM  (IX)                              
024800                             EMB-KVLOCK (IX)                              
024900                             EMB-KVEMBSPA-02(IX)                          
025000       ADD +1             TO IX                                           
025100     END-PERFORM                                                          
025200     .                                                                    
025300     EJECT                                                                
025400 BB-BEHANDLA-KOLLI SECTION.                                               
025500                                                                          
025600     MOVE KOLLI-KDKOLLI             TO WS-KDKOLLI                         
025700     MOVE ZERO                      TO WS-KVPALL                          
025800                                       WS-KVRAM                           
025900                                       WS-KVLOCK                          
026000                                       WS-KVEMBSPA                        
026100     IF   WS-KDKOLLI = 'FLEN    '                                         
026200       ADD +1                    TO EMB-KVPALL (1)                        
026300                                    WS-KVPALL                             
026400       ADD +2                    TO EMB-KVPALL (2)                        
026500                                    WS-KVPALL                             
026600       ADD +8                    TO EMB-KVRAM  (2)                        
026700                                    WS-KVRAM                              
026800       ADD +1                    TO EMB-KVLOCK (1)                        
026900                                    WS-KVLOCK                             
027000       ADD +1                    TO EMB-KVLOCK (2)                        
027100                                    WS-KVLOCK                             
027200       MOVE JA                   TO EMB-PROF                              
027300     ELSE                                                                 
027400       EVALUATE TRUE                                                      
027500           WHEN WS-KDKOLLI-1 = 'L' OR 'K' OR 'F' OR 'G'                   
027600          OR 'H' OR 'U' OR 'W' OR 'Y' OR 'C'                              
027700                                                                          
027800         IF     WS-KDKOLLI-1 = 'L'                                        
027900           ADD +1                TO WS-KVPALL                             
028602*                                                                         
028603*          NYTT EMBALLAGE FÅR PLATS 22 I TABELLEN                         
028604*                                                                         
028605           IF WS-KDKOLLI-3 NUMERIC                                        
028607             ADD +1              TO EMB-KVPALL (22)                       
028608             ADD +1              TO EMB-KVLOCK (22)                       
028609             MOVE WS-KDKOLLI-3 TO WS-KVRAM-X                              
028610             ADD WS-KVRAM        TO EMB-KVRAM  (22)                       
028611             IF WS-KDKOLLI-3 NOT = '0'                                    
028620               ADD +1            TO EMB-KVEMBSPA-02(22)                   
028630             END-IF                                                       
028701           ELSE                                                           
028801                                                                          
028901             IF WS-KDKOLLI-2 NUMERIC                                      
028902               ADD +1            TO EMB-KVPALL (1)                        
029001               IF WS-KDKOLLI-3 = 'D'                                      
029101                 ADD +2          TO EMB-KVLOCK (1)                        
029201                                      WS-KVLOCK                           
029301               ELSE                                                       
029401                 ADD +1          TO EMB-KVLOCK (1)                        
029501                                      WS-KVLOCK                           
029601               END-IF                                                     
029701               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
029801               ADD WS-KVRAM    TO EMB-KVRAM  (1)                          
029901               IF WS-KDKOLLI-2 NOT = '0'                                  
030001                 ADD +1          TO EMB-KVEMBSPA-02(1)                    
030101               END-IF                                                     
030201             END-IF                                                       
030301           END-IF                                                         
030401         ELSE                                                             
030501           EVALUATE TRUE                                                  
030601           WHEN WS-KDKOLLI-1 = 'K'                                        
030701             ADD +1                TO EMB-KVPALL (2)                      
030801                                      WS-KVPALL                           
030901             IF WS-KDKOLLI-2 NUMERIC                                      
031001               IF  WS-KDKOLLI-3 = 'D'                                     
031101                 ADD +2            TO EMB-KVLOCK (2)                      
031201                                      WS-KVLOCK                           
031301               ELSE                                                       
031401                 ADD +1            TO EMB-KVLOCK (2)                      
031501                                      WS-KVLOCK                           
031601               END-IF                                                     
031701               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
031801               ADD WS-KVRAM      TO EMB-KVRAM  (2)                        
031901              IF WS-KDKOLLI-2 NOT = '0'                                   
032001               ADD +1            TO EMB-KVEMBSPA-02(2)                    
032101              END-IF                                                      
032201             END-IF                                                       
032301           WHEN WS-KDKOLLI-1 = 'F'                                        
032501             ADD +1                TO WS-KVPALL                           
033202*                                                                         
033203*            NYTT EMBALLAGE FÅR PLATS 23 I TABELLEN                       
033204*                                                                         
033205             IF WS-KDKOLLI-3 NUMERIC                                      
033207               ADD +1              TO EMB-KVPALL (23)                     
033208               ADD +1              TO EMB-KVLOCK (23)                     
033209               MOVE WS-KDKOLLI-3   TO WS-KVRAM-X                          
033210               ADD WS-KVRAM        TO EMB-KVRAM  (23)                     
033211               IF WS-KDKOLLI-3 NOT = '0'                                  
033220                ADD +1             TO EMB-KVEMBSPA-02(23)                 
033230               END-IF                                                     
033301             ELSE                                                         
033401                                                                          
033501               IF WS-KDKOLLI-2 NUMERIC                                    
033502                 ADD +1            TO EMB-KVPALL (3)                      
033601                 IF WS-KDKOLLI-3 = 'D'                                    
033701                   ADD +2          TO EMB-KVLOCK (3)                      
033801                                        WS-KVLOCK                         
033901                 ELSE                                                     
034001                   ADD +1          TO EMB-KVLOCK (3)                      
034101                                        WS-KVLOCK                         
034201                 END-IF                                                   
034301                 MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                          
034401                 ADD WS-KVRAM    TO EMB-KVRAM  (3)                        
034501                IF WS-KDKOLLI-2 NOT = '0'                                 
034601                 ADD +1          TO EMB-KVEMBSPA-02(3)                    
034701                END-IF                                                    
034801               END-IF                                                     
034901             END-IF                                                       
035001           WHEN WS-KDKOLLI-1 = 'G'                                        
035101             ADD +1                TO EMB-KVPALL (4)                      
035201                                      WS-KVPALL                           
035301             IF WS-KDKOLLI-2 NUMERIC                                      
035401               IF  WS-KDKOLLI-3 = 'D'                                     
035501                 ADD +2            TO EMB-KVLOCK (4)                      
035601                                      WS-KVLOCK                           
035701               ELSE                                                       
035801                 ADD +1            TO EMB-KVLOCK (4)                      
035901                                      WS-KVLOCK                           
036001               END-IF                                                     
036101               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
036201               ADD WS-KVRAM      TO EMB-KVRAM  (4)                        
036301              IF WS-KDKOLLI-2 NOT = '0'                                   
036401               ADD +1            TO EMB-KVEMBSPA-02(4)                    
036501              END-IF                                                      
036601             END-IF                                                       
036701           WHEN WS-KDKOLLI-1 = 'H'                                        
036801             ADD +1                TO EMB-KVPALL (5)                      
036901                                      WS-KVPALL                           
037001             IF WS-KDKOLLI-2 NUMERIC                                      
037101               IF  WS-KDKOLLI-3 = 'D'                                     
037201                 ADD +2            TO EMB-KVLOCK (5)                      
037301                                      WS-KVLOCK                           
037401               ELSE                                                       
037501                 ADD +1            TO EMB-KVLOCK (5)                      
037601                                      WS-KVLOCK                           
037701               END-IF                                                     
037801               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
037901               ADD WS-KVRAM      TO EMB-KVRAM  (5)                        
038001              IF WS-KDKOLLI-2 NOT = '0'                                   
038101               ADD +1            TO EMB-KVEMBSPA-02(5)                    
038201              END-IF                                                      
038301             END-IF                                                       
038401           WHEN WS-KDKOLLI-1 = 'U'                                        
038501             ADD +1                TO EMB-KVPALL (6)                      
038601                                      WS-KVPALL                           
038701             IF WS-KDKOLLI-2 NUMERIC                                      
038801               IF  WS-KDKOLLI-3 = 'D'                                     
038901                 ADD +2            TO EMB-KVLOCK (6)                      
039001                                      WS-KVLOCK                           
039101               ELSE                                                       
039201                 ADD +1            TO EMB-KVLOCK (6)                      
039301                                      WS-KVLOCK                           
039401               END-IF                                                     
039501               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
039601               ADD WS-KVRAM      TO EMB-KVRAM  (6)                        
039701               ADD +1            TO EMB-KVEMBSPA-02(6)                    
039801             END-IF                                                       
039901           WHEN WS-KDKOLLI-1 = 'W'                                        
040001             ADD +1                TO EMB-KVPALL (7)                      
040101                                      WS-KVPALL                           
040201             IF WS-KDKOLLI-2 NUMERIC                                      
040301               IF  WS-KDKOLLI-3 = 'D'                                     
040401                 ADD +2            TO EMB-KVLOCK (7)                      
040501                                      WS-KVLOCK                           
040601               ELSE                                                       
040701                 ADD +1            TO EMB-KVLOCK (7)                      
040801                                      WS-KVLOCK                           
040901               END-IF                                                     
041001               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
041101               ADD WS-KVRAM      TO EMB-KVRAM  (7)                        
041201               ADD +1            TO EMB-KVEMBSPA-02(7)                    
041301             END-IF                                                       
041401           WHEN WS-KDKOLLI-1 = 'Y'                                        
041501            IF  WS-KDEMB = 790                                            
041601             ADD +1                TO EMB-KVPALL (11)                     
041701                                      WS-KVPALL                           
041801             IF WS-KDKOLLI-2 NUMERIC                                      
041901                 ADD +1            TO EMB-KVLOCK (11)                     
042001                                      WS-KVLOCK                           
042101               ADD +1            TO EMB-KVRAM  (11)                       
042201               ADD +1            TO EMB-KVEMBSPA-02(11)                   
042301             END-IF                                                       
042401            ELSE                                                          
042501             IF  WS-KDEMB = 750                                           
042601              ADD +1                TO EMB-KVPALL (12)                    
042701                                       WS-KVPALL                          
042801              IF WS-KDKOLLI-2 NUMERIC                                     
042901                  ADD +1            TO EMB-KVLOCK (12)                    
043001                                       WS-KVLOCK                          
043101                ADD +1            TO EMB-KVRAM  (12)                      
043201                ADD +1            TO EMB-KVEMBSPA-02(12)                  
043301              END-IF                                                      
043401             ELSE                                                         
043501*              KDEMB 780                                                  
043601              ADD +1                TO EMB-KVPALL (8)                     
043701                                       WS-KVPALL                          
043801              IF WS-KDKOLLI-2 NUMERIC                                     
043901               IF  WS-KDKOLLI-3 = 'D'                                     
044001                 ADD +2            TO EMB-KVLOCK (8)                      
044101                                      WS-KVLOCK                           
044201               ELSE                                                       
044301                 ADD +1            TO EMB-KVLOCK (8)                      
044401                                      WS-KVLOCK                           
044501               END-IF                                                     
044601               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
044701               ADD WS-KVRAM      TO EMB-KVRAM  (8)                        
044801               ADD +1            TO EMB-KVEMBSPA-02(8)                    
044901              END-IF                                                      
045001             END-IF                                                       
045101            END-IF                                                        
045201           WHEN WS-KDKOLLI-1 = 'C'                                        
045301            EVALUATE TRUE                                                 
045401            WHEN WS-KDEMB = 419                                           
045501             ADD +1                TO EMB-KVPALL (9)                      
045601                                      WS-KVPALL                           
045701             IF WS-KDKOLLI-2 NUMERIC                                      
045801                 ADD +1            TO EMB-KVLOCK (9)                      
045901                                      WS-KVLOCK                           
046001               ADD +1            TO EMB-KVRAM  (9)                        
046101               ADD +1            TO EMB-KVEMBSPA-02(9)                    
046201             END-IF                                                       
046301            WHEN WS-KDEMB = 422                                           
046401             ADD +1                TO EMB-KVPALL (10)                     
046501                                      WS-KVPALL                           
046601             IF WS-KDKOLLI-2 NUMERIC                                      
046701                 ADD +1            TO EMB-KVLOCK (10)                     
046801                                      WS-KVLOCK                           
046901               ADD +1            TO EMB-KVRAM  (10)                       
047001               ADD +1            TO EMB-KVEMBSPA-02(10)                   
047101             END-IF                                                       
047201           WHEN WS-KDEMB = 142 AND WS-KDEMB2 = 1                          
047301             ADD +1                TO EMB-KVPALL (13)                     
047401                                      WS-KVPALL                           
047501             IF WS-KDKOLLI-2 NUMERIC                                      
047601                 ADD +1            TO EMB-KVLOCK (13)                     
047701                                      WS-KVLOCK                           
047801               ADD +1            TO EMB-KVRAM  (13)                       
047901               ADD +1            TO EMB-KVEMBSPA-02(13)                   
048001             END-IF                                                       
048101            WHEN WS-KDEMB = 107                                           
048201             ADD +1             TO EMB-KVPALL (14)                        
048301             IF WS-KDKOLLI-2 NUMERIC                                      
048401              ADD +1            TO EMB-KVLOCK (14)                        
048501              ADD +1            TO EMB-KVRAM  (14)                        
048601              ADD +1            TO EMB-KVEMBSPA-02(14)                    
048701             END-IF                                                       
048801            WHEN WS-KDEMB = 460                                           
048901             ADD +1             TO EMB-KVPALL (15)                        
049001             IF WS-KDKOLLI-2 NUMERIC                                      
049101              ADD +1            TO EMB-KVLOCK (15)                        
049201              ADD +1            TO EMB-KVRAM  (15)                        
049301              ADD +1            TO EMB-KVEMBSPA-02(15)                    
049401             END-IF                                                       
049501            WHEN WS-KDEMB = 576                                           
049601             ADD +1             TO EMB-KVPALL (16)                        
049701             IF WS-KDKOLLI-2 NUMERIC                                      
049801              ADD +1            TO EMB-KVLOCK (16)                        
049901              ADD +1            TO EMB-KVRAM  (16)                        
050001              ADD +1            TO EMB-KVEMBSPA-02(16)                    
050101             END-IF                                                       
050201            WHEN WS-KDEMB = 595                                           
050301             ADD +1             TO EMB-KVPALL (17)                        
050401             IF WS-KDKOLLI-2 NUMERIC                                      
050501              ADD +1            TO EMB-KVLOCK (17)                        
050601              ADD +1            TO EMB-KVRAM  (17)                        
050701              ADD +1            TO EMB-KVEMBSPA-02(17)                    
050801             END-IF                                                       
050901            WHEN WS-KDEMB = 724                                           
051001             ADD +1             TO EMB-KVPALL (18)                        
051101             IF WS-KDKOLLI-2 NUMERIC                                      
051201              ADD +1            TO EMB-KVLOCK (18)                        
051301              ADD +1            TO EMB-KVRAM  (18)                        
051401              ADD +1            TO EMB-KVEMBSPA-02(18)                    
051501             END-IF                                                       
051601            WHEN WS-KDEMB = 742                                           
051701             ADD +1             TO EMB-KVPALL (19)                        
051801             IF WS-KDKOLLI-2 NUMERIC                                      
051901              ADD +1            TO EMB-KVLOCK (19)                        
052001              ADD +1            TO EMB-KVRAM  (19)                        
052101              ADD +1            TO EMB-KVEMBSPA-02(19)                    
052201             END-IF                                                       
052301            WHEN WS-KDEMB = 743                                           
052401             ADD +1             TO EMB-KVPALL (20)                        
052501             IF WS-KDKOLLI-2 NUMERIC                                      
052601              ADD +1            TO EMB-KVLOCK (20)                        
052701              ADD +1            TO EMB-KVRAM  (20)                        
052801              ADD +1            TO EMB-KVEMBSPA-02(20)                    
052901             END-IF                                                       
053001            WHEN WS-KDEMB = 840                                           
053101             ADD +1             TO EMB-KVPALL (21)                        
053201             IF WS-KDKOLLI-2 NUMERIC                                      
053301              ADD +1            TO EMB-KVLOCK (21)                        
053401              ADD +1            TO EMB-KVRAM  (21)                        
053501              ADD +1            TO EMB-KVEMBSPA-02(21)                    
053601             END-IF                                                       
053701            END-EVALUATE                                                  
053801           END-EVALUATE                                                   
053901         END-IF                                                           
054001         MOVE JA                   TO EMB-PROF                            
054101       END-EVALUATE                                                       
054201     END-IF                                                               
054301     .                                                                    
054401     EJECT                                                                
054501 BC-SKRIV-EMBPROF    SECTION.                                             
054601                                                                          
054701*    IF EMB-IDDC-REC > SPACE                                              
054801       WRITE EMB-POST FROM EMB-AREA                                       
054901*    END-IF                                                               
055001                                                                          
055101     MOVE 'W47390'               TO POSTSUM-FDNAMN                        
055201     MOVE 'W47390D3'             TO POSTSUM-DDNAMN2                       
055301     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
055401     CALL POSTSUM USING POSTSUM-PARM                                      
055501     .                                                                    
055601     EJECT                                                                
055701 BD-LAS-SORTERAD-RY6 SECTION.                                             
055801     SKIP3                                                                
055901     RETURN SORTFIL INTO WSORT-AREA                                       
056001     AT END                                                               
056101        MOVE JA TO SORTFIL-EOF                                            
056201     END-RETURN                                                           
056301                                                                          
056401     IF SORTFIL-EOF = NEJ                                                 
056501       MOVE 'W092ZW'         TO POSTSUM-FDNAMN                            
056601       MOVE 'W47390D1'       TO POSTSUM-DDNAMN2                           
056701       MOVE WSORT-RY6-IDPTYP TO POSTSUM-TRANSTYP                          
056801       CALL POSTSUM USING POSTSUM-PARM                                    
056901     END-IF                                                               
057001     .                                                                    
057101     EJECT                                                                
057201 BF-BEHANDLA-DC    SECTION.                                               
057301                                                                          
057401     SEARCH ALL DIST04-DIST-DC                                            
057501        AT END                                                            
057601           MOVE SPACE        TO EMB-IDDC-REC                              
057701        WHEN DIST04-SOK-IDDISTR(DIST04-IX1) = EMB-IDDISTR                 
057801           MOVE DIST04-IDDC-REC (DIST04-IX1)                              
057901                             TO EMB-IDDC-REC                              
058001     END-SEARCH                                                           
058101     .                                                                    
058201     EJECT                                                                
058301 Z-FINIT   SECTION.                                                       
058401     SKIP3                                                                
058501     CLOSE  W47390-EMB                                                    
058601                                                                          
058701*    ----  SKRIV UT ANTAL LÄSTA OCH SKRIVNA POSTER                        
058801     MOVE 'S' TO POSTSUM-OPKOD                                            
058901     CALL POSTSUM USING POSTSUM-PARM                                      
059001     .                                                                    
059101     EJECT                                                                
059201*    ---- IMS SUB-SEKTIONER ----                                          
059301                                                                          
059401 IMS-GU-WDE601 SECTION.                                                   
059501                                                                          
059601     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
059701            DELIMITED BY SIZE INTO SSA1                                   
059801     MOVE '  '                  TO GODK-STATUSKODER                       
059901     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-E601 SSA1                     
060001     MOVE WDE6-STATUS-CODE        TO STATUS-WS                            
060101     PERFORM IMS-STATUSKONTROLL                                           
060201     .                                                                    
060301     SKIP3                                                                
060401 IMS-GNP-WDE611 SECTION.                                                  
060501                                                                          
060601     MOVE 'WDE611'            TO SSA1                                     
060701     MOVE '  GE'                TO GODK-STATUSKODER                       
060801     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
060901     MOVE WDE6-STATUS-CODE        TO STATUS-WS                            
061001     PERFORM IMS-STATUSKONTROLL                                           
061101     .                                                                    
061201     SKIP3                                                                
061301 IMS-STATUSKONTROLL SECTION.                                              
061401                                                                          
061501     SET STATUS-IX TO 1                                                   
061601     SEARCH GODK-STATUS                                                   
061701       AT END CALL FELLOG                                                 
061801       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
062000     END-SEARCH                                                           
070000     .                                                                    
