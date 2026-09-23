000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4066700.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   95/09/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER HÄNDELSEBAS OCH KONTROLLERAR INMATNING. OM INDATA          
001000*        OK SÅ STARTAS ETT BAKGRUNDS-MPP SOM SKRIVER LISTOR FÖR           
001100*        ITALIEN.                                                         
001200*                                                                         
001300*        PROGRAMMET LÄSER      WL4491 (WDR4)                              
001310*                              WL4494 (WDR4)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T667                                              
001700*        MID:         W4I66701                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O66701                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W4066700'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003310 77  IX                          PIC S9(4)   VALUE +0 COMP SYNC.          
003311 77  INDX                        PIC S9(4)   VALUE +0 COMP SYNC.          
003320 77  MAX-INDX                    PIC S9(4)   VALUE +14 COMP SYNC.         
003330 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003340 77  WS-DATUM                    PIC X(6)    VALUE ZERO.                  
003341 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
003350 77  WS-IDTRPTNR                 PIC Z(2)9.                               
003400     EJECT                                                                
003500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003600                                                                          
003800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003900     88  NYCKLAR-OK                          VALUE 'J'.                   
004000     88  NYCKLAR-FEL                         VALUE 'N'.                   
004100                                                                          
004141 77  INPUT-SW                    PIC X       VALUE 'J'.                   
004142     88  INMATNING-OK                        VALUE 'J'.                   
004143     88  INMATNING-EJ-OK                     VALUE 'N'.                   
004144                                                                          
004145 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004146     88  INDATA-OK                           VALUE 'J'.                   
004147     88  INDATA-EJ-OK                        VALUE 'N'.                   
004148                                                                          
004150 77  SHOW-SW                     PIC X       VALUE 'N'.                   
004160     88  SHOWED                              VALUE 'J'.                   
004180                                                                          
004190 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004191     88  ALLT-OK                             VALUE 'J'.                   
004192                                                                          
004200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004300     88  EGEN-MID                            VALUE '4667'.                
004400     88  GODK-MID                            VALUE '4661' '4662'          
004500                                                   '4663' '4664'          
004600                                                   '4665' '4666'          
004700                                                   '4667' '4668'          
004800                                                   '4669'.                
004900     88  HELP-MID                            VALUE '0551'.                
005000     EJECT                                                                
005100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005200 01  GENERELLA-SUBPROGRAM.                                                
005300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005310     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     EJECT                                                                
005800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
005900*01 -COPY WMEDAREA                                                        
006000     SKIP3                                                                
006100 01  MESSAGE-CODES.                                                       
006200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006210     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006220     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006230     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '118'.                 
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006500*                                                                         
006600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
006700     SKIP3                                                                
006800*01 -COPY WMSGINIT                                                        
006900     SKIP3                                                                
006910*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
006920*01 -COPY WDATAREA                                                        
006990     EJECT                                                                
007000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007300     SKIP3                                                                
007400*01  MID -COPY W4I66701                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
007700     SKIP3                                                                
007800*01  -COPY WMSGAREA                                                       
007900     EJECT                                                                
008000     03  MOD REDEFINES MSG-AREA.                                          
008100*      05  -COPY W4O66701                                                 
008200     EJECT                                                                
008210 01  W-PROG-TO-PROG-SW.                                                   
008230     03  M-SW-LL-1               PIC S9(4)   VALUE +100 COMP SYNC.        
008240     03  M-SW-Z1-Z2-1            PIC X(2)    VALUE LOW-VALUE.             
008250     03  M-SW-KDTRANS-1          PIC X(8)    VALUE 'W4T697  '.            
008260     03  M-SW-IDTRANS-1          PIC X(4)    VALUE '4667'.                
008270     03  M-SW-KDMFSTYP-1         PIC X(1)    VALUE '1'.                   
008280                                                                          
008290     03 MID -COPY W4I69701 -PRE 4697-                                     
008291     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008400     SKIP3                                                                
008500*01  -COPY WMFSAREA                                                       
008600     EJECT                                                                
008700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*                                                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-WDGXKEY-X.                                                     
009600         05  W-IDHTYP            PIC X(4)    VALUE '4491'.                
009601         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009610         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
009700     SKIP2                                                                
009710     03  W-KY4494-MIN-X.                                                  
009720         05  W-DALASTN-MIN       PIC  9(8)   VALUE ZERO.                  
009730         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
009731                                                                          
009740     03  W-KY4494-MAX-X.                                                  
009750         05  W-DALASTN-MAX       PIC  9(8)   VALUE ZERO.                  
009760         05  FILLER              PIC X(26)   VALUE HIGH-VALUE.            
009780     EJECT                                                                
009800*    --- STATUS-KOD FRÅN IMS                                              
009900 01  STATUS-WS                   PIC XX.                                  
010000     88  SEGMENT-FINNS                       VALUE '  '.                  
010100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(128).                              
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011500     SKIP3                                                                
011600 01  DLI-IO-AREA.                                                         
011700     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
011800     SKIP3                                                                
011900     03  WL449112 REDEFINES IO-AREA.                                      
012000*        05  -COPY WDGX4494  -PRE BOLLA-                                  
012160     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0009   -PRE MSG-                                              
012401     EJECT                                                                
012410*01  -COPY W0009   -PRE ALT-                                              
012420     EJECT                                                                
012500*01  -COPY W0008   -PRE USEA-                                             
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800*01  -COPY W0008  -PRE 4494-                                              
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 4494-PCB.             
013200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 4494-PCB.             
013300                                                                          
013400     PERFORM IMS-GET-MSG                                                  
013500     IF SEGMENT-FINNS                                                     
013600       PERFORM A-INIT                                                     
013700       PERFORM B-KOLLA-NYCKLAR                                            
013800       IF NYCKLAR-OK                                                      
013900         IF MFS-UPDATE                                                    
014000           PERFORM G-KOLLA-INPUT                                          
014100           IF INDATA-OK                                                   
014200             PERFORM H-STARTA-PRINTPROGRAM                                
014300           END-IF                                                         
014400         ELSE                                                             
014500           IF MFS-FIRST                                                   
014600             PERFORM C-FOERSTA-SIDAN                                      
014700           ELSE                                                           
015100             PERFORM E-SAMMA-SIDA                                         
015300           END-IF                                                         
015400         END-IF                                                           
015500         PERFORM F-LAES-VISA-INFO                                         
015600       END-IF                                                             
015700*      IF NOT MFS-UPDATE                                                  
015900         PERFORM IMS-INSERT-MSG                                           
016000*      END-IF                                                             
016100     END-IF                                                               
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     IF MSG-DUBBLA-TRANSKODER                                             
017000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66701                 
017100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017300     ELSE                                                                 
017400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66701                  
017500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017700     END-IF                                                               
017800                                                                          
017900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018101                                                                          
018300     MOVE LOW-VALUE TO MSG-AREA                                           
018400     MOVE 'W4O667N1' TO MFS-IDMOD                                         
018500     MOVE '4667' TO MOD-IDTRANS                                           
018600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018700                                                                          
019000     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O66701 + 4                        
019100                                                                          
019200     IF EGEN-MID OR HELP-MID                                              
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
019701     IF ENGLISH-TEXT                                                      
019702       MOVE 'GB' TO MED-IDSKYLT                                           
019703     ELSE                                                                 
019704       MOVE 'S ' TO MED-IDSKYLT                                           
019705     END-IF                                                               
019710                                                                          
019720     ACCEPT DAGENS-DATUM FROM DATE                                        
019800     .                                                                    
019900     EJECT                                                                
020000 B-KOLLA-NYCKLAR SECTION.                                                 
020100                                                                          
020200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020300     MOVE '001'             TO MSGI-KDCALL                                
020400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020410     MOVE '4667'            TO MSGI-IDTRANS                               
020420     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020500     IF GODK-MID                                                          
020510       IF MID-TIDATUM-IN NOT = ALL '+'                                    
020600         MOVE MID-TIDATUM-IN TO MSGI-TIREGDAT                             
020601       ELSE                                                               
020603         IF MID-TIDATUM-UT = ZERO                                         
020604           MOVE DAGENS-DATUM TO MSGI-TIREGDAT                             
020605         END-IF                                                           
020606       END-IF                                                             
020610     END-IF                                                               
020700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020800                                                                          
020900     MOVE JA TO NYCKLAR-SW                                                
021000                                                                          
021010     MOVE MFS-RENSA-FAELT TO MOD-TIDATUM-IN                               
021020                                                                          
021021     IF EGEN-MID                                                          
021030       IF MID-TIDATUM-IN = ALL '+'                                        
021031         MOVE MID-TIDATUM-UT TO WS-DATUM                                  
021032         INSPECT WS-DATUM REPLACING LEADING SPACE BY ZERO                 
021035       ELSE                                                               
021036         MOVE MID-TIDATUM-IN TO WS-DATUM                                  
021037         MOVE '7'      TO MFS-IDPFK                                       
021038         MOVE SPACE    TO MFS-KDTRTYP                                     
021039       END-IF                                                             
021040     ELSE                                                                 
021041       MOVE DAGENS-DATUM TO WS-DATUM                                      
021042       MOVE '7'        TO MFS-IDPFK                                       
021043       MOVE SPACE      TO MFS-KDTRTYP                                     
021044     END-IF                                                               
021045                                                                          
021046     MOVE MSGI-IDDC    TO WS-IDDC                                         
021047                                                                          
021048     IF WS-DATUM NUMERIC                                                  
021050       IF WS-DATUM = ZERO                                                 
021051         MOVE DAGENS-DATUM TO WS-DATUM                                    
021052       END-IF                                                             
021054                                                                          
021055       MOVE 'AAMMDD'  TO DAT-KDDATFORM                                    
021056       MOVE WS-DATUM  TO DAT-I-TIDATUM                                    
021057       CALL WDATKONV USING DAT-KDDATFORM                                  
021058                           DAT-I-TIDATUM                                  
021059                           DAT-O-TIDATUM                                  
021060                           DAT-KDSVAR                                     
021061       IF DAT-KDSVAR-OK                                                   
021064         MOVE DAT-TIAAMMDD TO W-DALASTN-MIN                               
021065                              W-DALASTN-MAX                               
021066         MOVE DAT-TISEKEL  TO W-DALASTN-MIN (1:2)                         
021067                              W-DALASTN-MAX (1:2)                         
021068       ELSE                                                               
021069         MOVE NEJ TO NYCKLAR-SW                                           
021070       END-IF                                                             
021071     ELSE                                                                 
021072       MOVE NEJ TO NYCKLAR-SW                                             
021073     END-IF                                                               
021074                                                                          
021075*    IF MSGI-TIREGDAT  NUMERIC                                            
021076*      MOVE MSGI-TIREGDAT TO W-TILASTN-MIN                                
021077*                            W-TILASTN-MAX                                
021078*                            WS-DATUM                                     
021079*    ELSE                                                                 
021080*      MOVE NEJ TO NYCKLAR-SW                                             
021090*    END-IF                                                               
021100                                                                          
021110     IF EGEN-MID OR NYCKLAR-OK                                            
021111       MOVE WS-DATUM      TO MOD-TIDATUM-UT                               
021130       INSPECT MOD-TIDATUM-UT REPLACING LEADING ZERO BY SPACE             
021140     ELSE                                                                 
021150       MOVE MFS-RENSA-FAELT TO MOD-TIDATUM-UT                             
021160     END-IF                                                               
021170                                                                          
021200     IF NYCKLAR-FEL                                                       
021300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021400       CALL WMEDKONV USING MED-WMEDAREA                                   
021500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021600       PERFORM MFS-RENSA-FAELT-IN                                         
021700       PERFORM MFS-RENSA-FAELT-UT                                         
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 C-FOERSTA-SIDAN SECTION.                                                 
022200                                                                          
022400     PERFORM MFS-RENSA-FAELT-IN                                           
022500     .                                                                    
022600     EJECT                                                                
023300 E-SAMMA-SIDA SECTION.                                                    
023400                                                                          
023500     MOVE JA TO INPUT-SW                                                  
023600     MOVE +1 TO INDX                                                      
023700     PERFORM UNTIL INDX > MAX-INDX                                        
023800       IF MID-KDSVAR (INDX) = ALL '+' OR                                  
023801          MID-KDSVAR (INDX) = SPACE                                       
023810                                                                          
023900         MOVE JA TO INPUT-SW                                              
024000         ADD +1 TO INDX                                                   
024100       ELSE                                                               
024200         MOVE NEJ TO INPUT-SW                                             
024300         MOVE +9999 TO INDX                                               
024400       END-IF                                                             
024500     END-PERFORM                                                          
024600                                                                          
024700     IF INMATNING-OK                                                      
025200*      IF EGEN-MID OR HELP-MID                                            
025300         MOVE JA TO ALLT-SW                                               
025400         PERFORM MFS-RENSA-FAELT-IN                                       
026320*      END-IF                                                             
026330     ELSE                                                                 
026331       IF EGEN-MID OR HELP-MID                                            
026332         MOVE NEJ TO ALLT-SW                                              
026333         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
026334         CALL WMEDKONV USING MED-WMEDAREA                                 
026335         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
026336         PERFORM MFS-ROER-EJ-FAELT-IN                                     
026337         PERFORM MFS-ROER-EJ-FAELT-UT                                     
026338         PERFORM MFS-LAES-IN-IGEN                                         
026339         PERFORM EA-MID-INDATA-TILL-MOD                                   
026340       ELSE                                                               
026341         MOVE JA TO ALLT-SW                                               
026342         PERFORM MFS-LAES-IN-IGEN                                         
026343       END-IF                                                             
026344     END-IF                                                               
026345     .                                                                    
026346     EJECT                                                                
026400                                                                          
026410 EA-MID-INDATA-TILL-MOD SECTION.                                          
026420                                                                          
026430     MOVE +1 TO INDX                                                      
026440     PERFORM UNTIL INDX > MAX-INDX                                        
026450       IF MID-KDSVAR (INDX) NOT  = ALL '+'                                
026460         MOVE MID-KDSVAR (INDX)   TO MOD-KDSVAR (INDX)                    
026461         MOVE MID-IDTRPTNR (INDX) TO MOD-IDTRPTNR (INDX)                  
026462         MOVE MID-IDLBBET  (INDX) TO MOD-IDLBBET (INDX)                   
026470       END-IF                                                             
026480       ADD +1 TO INDX                                                     
026490     END-PERFORM                                                          
026491     .                                                                    
026492     EJECT                                                                
026500 F-LAES-VISA-INFO SECTION.                                                
026800                                                                          
026801     MOVE WS-IDDC    TO W-IDDC                                            
026810     PERFORM IMS-GU-WL4491                                                
026900     IF SEGMENT-SAKNAS                                                    
027000* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
027100        CALL WMEDKONV USING MED-WMEDAREA                                  
027200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
027300        PERFORM MFS-RENSA-FAELT-UT                                        
027400     ELSE                                                                 
027401       MOVE +1 TO INDX                                                    
027410       PERFORM IMS-GNP-WL4494                                             
027430       PERFORM UNTIL INDX > MAX-INDX                                      
027431         IF SEGMENT-FINNS                                                 
027432           MOVE NEJ TO SHOW-SW                                            
027433           MOVE +1  TO IX                                                 
027434           PERFORM UNTIL IX = INDX OR (SHOWED)                            
027435             MOVE BOLLA-4494-IDTRPTNR  TO WS-IDTRPTNR                     
027436             IF WS-IDTRPTNR        = MOD-IDTRPTNR (IX) AND                
027437                BOLLA-4494-IDLBBET = MOD-IDLBBET  (IX)                    
027438               MOVE JA               TO SHOW-SW                           
027439             END-IF                                                       
027440             ADD +1 TO IX                                                 
027441           END-PERFORM                                                    
027442           IF NOT SHOWED                                                  
027443             MOVE BOLLA-4494-IDTRPTNR  TO MOD-IDTRPTNR (INDX)             
027444             MOVE BOLLA-4494-IDLBBET   TO MOD-IDLBBET  (INDX)             
027445             ADD +1 TO INDX                                               
027446           END-IF                                                         
027447           PERFORM IMS-GNP-WL4494                                         
027448         ELSE                                                             
027449           MOVE MFS-RENSA-FAELT      TO MOD-IDTRPTNR (INDX)               
027450                                        MOD-IDLBBET  (INDX)               
027451           MOVE MFS-STAENG-FAELT     TO MOD-KDSVAR-ATTR (INDX)            
027452           ADD +1 TO INDX                                                 
027453         END-IF                                                           
027460       END-PERFORM                                                        
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 G-KOLLA-INPUT SECTION.                                                   
027900                                                                          
028100     MOVE +1 TO INDX                                                      
028200     PERFORM UNTIL INDX > MAX-INDX                                        
028300       IF MID-KDSVAR (INDX) = '+' OR SPACE                                
028500          MOVE MFS-RENSA-FAELT TO MOD-KDSVAR (INDX)                       
028510          MOVE NEJ TO INDATA-SW                                           
028600       ELSE                                                               
028700         IF MID-KDSVAR (INDX) = 'P' OR 'F'                                
028800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSVAR-ATTR (INDX)            
029000           PERFORM H-STARTA-PRINTPROGRAM                                  
029100         ELSE                                                             
029200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSVAR-ATTR (INDX)              
029300           MOVE MFS-ROER-EJ-FAELT  TO MOD-KDSVAR (INDX)                   
029310           MOVE NEJ TO INDATA-SW                                          
029400           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
029500           CALL WMEDKONV USING MED-WMEDAREA                               
029600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
029700         END-IF                                                           
029800       END-IF                                                             
029900       ADD +1 TO INDX                                                     
030000     END-PERFORM                                                          
030100     .                                                                    
030200     EJECT                                                                
030210 H-STARTA-PRINTPROGRAM SECTION.                                           
030220                                                                          
030221     MOVE LOW-VALUE TO 4697-MID-W4I69701                                  
030230     MOVE MID-KDSVAR (INDX) TO 4697-MID-KDSVAR                            
030231     MOVE WS-DATUM          TO 4697-MID-TIDATUM                           
030240     INSPECT MID-IDTRPTNR (INDX) REPLACING LEADING SPACE BY ZERO          
030260     MOVE WS-IDDC             TO 4697-MID-IDDC                            
030261     MOVE MID-IDTRPTNR (INDX) TO 4697-MID-IDTRPTNR                        
030270     MOVE MID-IDLBBET(INDX)   TO 4697-MID-IDLBBET                         
030272                                                                          
030273     PERFORM IMS-INSERT-ALT-MSG                                           
030274                                                                          
030275     MOVE INF-PRINT-REQUESTED TO MED-IDMFSINF                             
030276     CALL WMEDKONV USING MED-WMEDAREA                                     
030277     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
030280     .                                                                    
030290     EJECT                                                                
030300 MFS-RENSA-FAELT-UT SECTION.                                              
030400                                                                          
030500*    --- ALLA UTDATA-FÄLT                                                 
030510     MOVE +1 TO INDX                                                      
030520     PERFORM UNTIL INDX > MAX-INDX                                        
030530       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
030540       ADD +1 TO INDX                                                     
030550     END-PERFORM                                                          
030800     .                                                                    
030900     SKIP3                                                                
030910 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
030920                                                                          
030930*    --- ALLA UTDATA-FÄLT                                                 
030940     MOVE MFS-RENSA-FAELT TO MOD-KDSVAR    (INDX)                         
030950                             MOD-IDTRPTNR  (INDX)                         
030960                             MOD-IDLBBET   (INDX)                         
030970     .                                                                    
030980     EJECT                                                                
030990 MFS-RENSA-FAELT-IN SECTION.                                              
030991                                                                          
030992*    --- ALLA UTDATA-FÄLT                                                 
030993     MOVE +1 TO INDX                                                      
030994     PERFORM UNTIL INDX > MAX-INDX                                        
030995       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
030996       ADD +1 TO INDX                                                     
030997     END-PERFORM                                                          
030998     .                                                                    
030999     SKIP3                                                                
031000 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
031100                                                                          
031200*    --- ALLA UTDATA-FÄLT                                                 
031300     MOVE MFS-RENSA-FAELT TO MOD-KDSVAR    (INDX)                         
031600     .                                                                    
031610     EJECT                                                                
031700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
031800                                                                          
031900*    --- ALLA UTDATA-FÄLT                                                 
031910     MOVE +1 TO INDX                                                      
031920     PERFORM UNTIL INDX > MAX-INDX                                        
031930       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
031940       ADD +1 TO INDX                                                     
031950     END-PERFORM                                                          
031960     .                                                                    
031970     EJECT                                                                
031980 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
031990                                                                          
032000*    --- ALLA UTDATA-FÄLT                                                 
032100     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR    (INDX)                       
032200                               MOD-IDTRPTNR (INDX)                        
032300                               MOD-IDLBBET (INDX)                         
032310     .                                                                    
032320     EJECT                                                                
032400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
032500                                                                          
032530*    --- ALLA UTDATA-FÄLT                                                 
032540     MOVE +1 TO INDX                                                      
032550     PERFORM UNTIL INDX > MAX-INDX                                        
032560       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
032570       ADD +1 TO INDX                                                     
032580     END-PERFORM                                                          
032590     .                                                                    
032591     EJECT                                                                
032592 MFS-ROER-EJ-RAD-FAELT-IN SECTION.                                        
032593                                                                          
032594*    --- ALLA UTDATA-FÄLT                                                 
032595     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR    (INDX)                       
032598     .                                                                    
032599     EJECT                                                                
033800 MFS-LAES-IN-IGEN SECTION.                                                
033900                                                                          
034000*    --- ALLA INDATA-FÄLT                                                 
034010     MOVE +1 TO INDX                                                      
034020     PERFORM UNTIL INDX > MAX-INDX                                        
034030       IF MID-KDSVAR (INDX) NOT = ALL '+'                                 
034100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSVAR-ATTR (INDX)             
034110       END-IF                                                             
034111       ADD +1 TO INDX                                                     
034120     END-PERFORM                                                          
034300     .                                                                    
034400     EJECT                                                                
034500* --- IMS SEKTIONER ---                                                   
034600     SKIP3                                                                
034700 IMS-GET-MSG SECTION.                                                     
034800                                                                          
034900     MOVE '  QC' TO GODK-STATUSKODER                                      
035000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
035100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035200     PERFORM IMS-STATUSKONTROLL                                           
035300     .                                                                    
035400     SKIP3                                                                
035500 IMS-INSERT-MSG SECTION.                                                  
035600                                                                          
035700     IF NOT ENGLISH-TEXT                                                  
035800       MOVE '0' TO MFS-KDHUVOMR                                           
035900     END-IF                                                               
036000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
036100     MOVE SPACE TO GODK-STATUSKODER                                       
036200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
036300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036400     PERFORM IMS-STATUSKONTROLL                                           
036500     .                                                                    
036600     EJECT                                                                
036610 IMS-INSERT-ALT-MSG SECTION.                                              
036620                                                                          
036670     MOVE SPACE TO GODK-STATUSKODER                                       
036680     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
036690     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
036691     PERFORM IMS-STATUSKONTROLL                                           
036692     .                                                                    
036693     EJECT                                                                
036700 IMS-GU-WL4491  SECTION.                                                  
036800                                                                          
036900     STRING 'WL449101(WDGXKEY  =' W-WDGXKEY-X ')'                         
037000          DELIMITED BY SIZE INTO SSA1                                     
037100     MOVE '  GE' TO GODK-STATUSKODER                                      
037200     CALL CBLTDLI USING GU 4494-PCB DLI-IO-AREA SSA1                      
037300     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
037400     PERFORM IMS-STATUSKONTROLL                                           
037500     .                                                                    
037600     SKIP2                                                                
037610 IMS-GNP-WL4494 SECTION.                                                  
037620                                                                          
037630     STRING 'WL449112(KY4494  >=' W-KY4494-MIN-X                          
037631                    '&KY4494  <=' W-KY4494-MAX-X ')'                      
037640          DELIMITED BY SIZE INTO SSA1                                     
037650     MOVE '  GE' TO GODK-STATUSKODER                                      
037660     CALL CBLTDLI USING GNP 4494-PCB DLI-IO-AREA SSA1                     
037670     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
037680     PERFORM IMS-STATUSKONTROLL                                           
037690     .                                                                    
037691     EJECT                                                                
037700 IMS-STATUSKONTROLL SECTION.                                              
037800                                                                          
037900     SET STATUS-IX TO 1                                                   
038000     SEARCH GODK-STATUS                                                   
038100       AT END                                                             
038200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038300         DELIMITED BY SIZE INTO FELTEXT                                   
038400         CALL FELLOG                                                      
038500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038600         CONTINUE                                                         
038700     END-SEARCH                                                           
038800     .                                                                    
