000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3011100.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   98/02/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BILD SOM GER SKYDDSKOD NÄR MAN ANGER SERINR OCH                  
000900*        RADIOTYP. SAKNAS RADIOTYP KAN MAN GÅ TILL BILD                   
001000*        3112.                                                            
001100*                                                                         
001200*        PROGRAMMET LÄSER      TABELL FRADIO                              
001300*                              TABELL WRADIOT                             
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W3T111                                              
001700*        MID:         W3I11101                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W3O11101                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W3011100'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)  VALUE +8    COMP SYNC.        
003900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004000                                                                          
004100                                                                          
004200 77  ARTIKEL-SW                  PIC X       VALUE 'J'.                   
004300     88  ARTIKEL-FINNS                       VALUE 'J'.                   
004400     88  ARTIKEL-SAKNAS                      VALUE 'N'.                   
004500                                                                          
004600                                                                          
004700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004800     88  INDATA-OK                           VALUE 'J'.                   
004900     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '3111'.                
005700     88  GODK-MID                            VALUE '3111' '3112'          
005800                                                   '3113' '3114'          
005900                                                   '3115' '3116'          
006000                                                   '3117' '3118'          
006100                                                   '3119'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300                                                                          
006400 01  WS-APPARATTYP-RED       PIC X(11)       VALUE SPACE.                 
006500 01  WS-SKYDDSKOD            PIC X(06)       VALUE SPACE.                 
006600                                                                          
006700 77  W-IDARTNR-RADIO         PIC S9(10) COMP-3  VALUE ZERO.               
006800 77  W-IDARTNR-LABEL         PIC S9(10) COMP-3  VALUE ZERO.               
006900                                                                          
007000 01  WS-IDARTNR              PIC X(10)   VALUE SPACE.                     
007100                                                                          
007200 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
007300 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
007400 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
007500 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
007600 01  FILLER                  PIC X(16)   VALUE 'WS-DB2-SEKTION'.          
007700 01  WS-DB2-SEKTION              PIC X(30)   VALUE SPACE.                 
007800 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
007900 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
008000                                                                          
008100                                                                          
008200                                                                          
008300     EJECT                                                                
008400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008500 01  GENERELLA-SUBPROGRAM.                                                
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009400                                                                          
009500 01  FILLER                  PIC X(16)   VALUE 'RETURKODER '.             
009600 01  RETURKODER.                                                          
009700   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009900   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010000*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010100     SKIP3                                                                
010200 01  FILLER                  PIC X(16)   VALUE 'WMEDAREA   '.             
010300*01 -COPY WMEDAREA                                                        
010400     SKIP3                                                                
010500 01  FILLER                  PIC X(16)   VALUE 'MESSAGE-CODES'.           
010600 01  MESSAGE-CODES.                                                       
010700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010800     03  ERR-CODE-NOT-REGISTERED PIC X(3)    VALUE '132'.                 
010900     03  ART-MISSING             PIC X(3)    VALUE '017'.                 
011000     03  TAB-MISSING             PIC X(3)    VALUE '023'.                 
011100     03  SERIAL-MISSING          PIC X(3)    VALUE '334'.                 
011200     03  MODEL-MISSING           PIC X(3)    VALUE '335'.                 
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011700     SKIP3                                                                
011800*01 -COPY WMSGINIT                                                        
011900     EJECT                                                                
012000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012300     SKIP3                                                                
012400*01  MID -COPY W3I11101                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012700     SKIP3                                                                
012800*01  -COPY WMSGAREA                                                       
012900     EJECT                                                                
013000     03  MOD REDEFINES MSG-AREA.                                          
013100*      05  -COPY W3O11101                                                 
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013400     SKIP3                                                                
013500*01  -COPY WMFSAREA                                                       
013600     EJECT                                                                
013700*                                                                         
013800*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
013900*                                                                         
014000 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
014100*01  -COPY FRADIO -PRE FRADIO-                                            
014200     EJECT                                                                
014300*01  -COPY WRADIOT -PRE RADIO-                                            
014400     EJECT                                                                
014500 01  FILLER                  PIC X(16) VALUE 'FRADIO-AREA'.               
014600       EXEC SQL INCLUDE FRADIO END-EXEC.                                  
014700     SKIP3                                                                
014800 01  FILLER                  PIC X(16) VALUE 'WRADIOT-AREA'.              
014900       EXEC SQL INCLUDE WRADIOT END-EXEC.                                 
015000     SKIP3                                                                
015100 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
015200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
015300*                        **** STATUS-KOD FRÅN DB2                         
015400 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
015500 01  DB2-WS.                                                              
015600   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
015700     88  CURSOR-OK                           VALUE 000.                   
015800     88  RADER-FINNS                         VALUE 000.                   
015900     88  RADER-SAKNAS                        VALUE 100.                   
016000     88  904-KOD                             VALUE 904.                   
016100     88  811-KOD                             VALUE 811.                   
016200     SKIP1                                                                
016300   03  GODK-SQLCODESKODER.                                                
016400     05  GODK-SQLCODE OCCURS 5                                            
016500         INDEXED BY SQLCODE-IX PIC 999.                                   
016600     EJECT                                                                
016700 01  FILLER                     PIC X(16) VALUE 'TEST-SQLCODE'.           
016800 01  TEST-SQLCODE               PIC 9(3) VALUE ZERO.                      
016900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017000*                                                                         
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  ARBETSAREA-DB2.                                                      
017500     03  W-SERIENR               PIC X(10).                               
017600     03  W-SKYDDSKOD             PIC X(6).                                
017700     03  W-APPARATTYP            PIC X(11).                               
017800     03  W-APPARATTYP-SOK        PIC X(11).                               
017900     03  W-APPARATTYP-MIN        PIC X(11).                               
018000     03  W-APPARATTYP-MAX        PIC X(11).                               
018100     EJECT                                                                
018200     SKIP2                                                                
018300*    --- STATUS-KOD FRÅN IMS                                              
018400 01  STATUS-WS                   PIC XX.                                  
018500     88  SEGMENT-FINNS                       VALUE '  '.                  
018600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018800     SKIP2                                                                
018900 01  GODK-STATUSKODER.                                                    
019000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019100     SKIP3                                                                
019200 01  SSA1                        PIC X(64).                               
019300 01  SSA2                        PIC X(64).                               
019400     EJECT                                                                
019500*    --- IMS FUNKTIONSKODER                                               
019600*01  -COPY W0003                                                          
019700     EJECT                                                                
019800*    ---  DLI INPUT-OUTPUT AREA                                           
019900                                                                          
020000     EJECT                                                                
020100*01  FILLER                                                               
020200     EJECT                                                                
020300 LINKAGE SECTION.                                                         
020400*01  -COPY W0009   -PRE MSG-                                              
020500*01  -COPY W0008   -PRE USEA-                                             
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB.                              
020900 MAIN SECTION.                                                            
021000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB.                              
021100     PERFORM IMS-GET-MSG                                                  
021200     IF SEGMENT-FINNS                                                     
021300       PERFORM A-INIT                                                     
021400       PERFORM B-KOLLA-NYCKLAR                                            
021500       IF NYCKLAR-OK                                                      
021600         PERFORM F-LAES-VISA-INFO                                         
021700       END-IF                                                             
021800       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O11101 + 4                      
021900       PERFORM IMS-INSERT-MSG                                             
022000     END-IF                                                               
022100     MOVE ZERO TO RETURN-CODE                                             
022200     GOBACK                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600     MOVE 'A-INIT'         TO WS-SEKTION                                  
022700*    DISPLAY WS-SEKTION                                                   
022800                                                                          
022900     IF MSG-DUBBLA-TRANSKODER                                             
023000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I11101                 
023100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
023200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023300     ELSE                                                                 
023400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I11101                  
023500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
023600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023700     END-IF                                                               
023800                                                                          
023900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
024100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024200                                                                          
024300     MOVE LOW-VALUE TO MSG-AREA                                           
024400     MOVE 'W3O11101' TO MFS-IDMOD                                         
024500     MOVE '3111' TO MOD-IDTRANS                                           
024600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024700                                                                          
024800     IF EGEN-MID OR HELP-MID                                              
024900       CONTINUE                                                           
025000     ELSE                                                                 
025100       MOVE SPACE TO MFS-KDTRTYP                                          
025200       MOVE '7' TO MFS-IDPFK                                              
025300     END-IF                                                               
025400                                                                          
025500                                                                          
025600                                                                          
025700     INITIALIZE GODK-SQLCODESKODER                                        
025800     .                                                                    
025900     EJECT                                                                
026000 B-KOLLA-NYCKLAR SECTION.                                                 
026100     MOVE 'B-KOLLA-NYCKLAR'  TO WS-SEKTION                                
026200*    DISPLAY WS-SEKTION                                                   
026300                                                                          
026400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026500     MOVE '001'             TO MSGI-KDCALL                                
026600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026800     MOVE '3111'            TO MSGI-IDTRANS                               
026900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027000     IF  MSGI-IDLAND-SPR = 'GB'                                           
027100       MOVE 'GB ' TO MED-IDSKYLT                                          
027200     ELSE                                                                 
027300       MOVE 'S  ' TO MED-IDSKYLT                                          
027400     END-IF                                                               
027500     IF GODK-MID                                                          
027600        CONTINUE                                                          
027700     END-IF                                                               
027800     MOVE JA TO NYCKLAR-SW                                                
027900                                                                          
028000     IF NOT EGEN-MID                                                      
028100       MOVE ALL '+' TO MID-IDRADIONR-IN                                   
028200       MOVE ALL '+' TO MID-IDAPPTYP-IN                                    
028300       MOVE ALL '+' TO MID-IDARTNR-IN                                     
028400     END-IF                                                               
028500                                                                          
028600     IF MID-IDRADIONR-IN NOT = ALL '+'                                    
028700        MOVE MID-IDRADIONR-IN TO MOD-IDRADIONR-UT                         
028800                                 W-SERIENR                                
028900     ELSE                                                                 
029000        IF MID-IDRADIONR-UT NOT = ALL '+'                                 
029100           MOVE MID-IDRADIONR-UT TO MID-IDRADIONR-IN                      
029200                                    MOD-IDRADIONR-UT                      
029300                                    W-SERIENR                             
029400        END-IF                                                            
029500     END-IF                                                               
029600                                                                          
029700                                                                          
029800     IF MID-IDAPPTYP-IN NOT = ALL '+'                                     
029900        MOVE MID-IDAPPTYP-IN TO MOD-IDAPPTYP-UT                           
030000                                W-APPARATTYP                              
030100     END-IF                                                               
030200                                                                          
030300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
030400                                                                          
030500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
030600        MOVE MID-IDARTNR-IN TO WS-IDARTNR                                 
030700        INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                
030800        IF WS-IDARTNR IS NUMERIC                                          
030900        AND WS-IDARTNR > ZERO                                             
031000            MOVE WS-IDARTNR TO W-IDARTNR-RADIO                            
031100                               MOD-IDARTNR-UT                             
031200        ELSE                                                              
031300            MOVE SPACE           TO WS-IDARTNR                            
031400            MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                        
031500            MOVE NEJ TO NYCKLAR-SW                                        
031600        END-IF                                                            
031700     END-IF                                                               
031800                                                                          
031900     IF  MID-IDARTNR-IN = ALL '+'                                         
032000     AND MID-IDAPPTYP-IN = ALL '+'                                        
032100        IF MID-IDARTNR-UT NOT = ALL '+'                                   
032200        AND MID-IDARTNR-UT NOT = ALL ' '                                  
032300           MOVE MID-IDARTNR-UT TO WS-IDARTNR                              
032400           INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO             
032500           IF WS-IDARTNR IS NUMERIC                                       
032600           AND WS-IDARTNR > ZERO                                          
032700               MOVE WS-IDARTNR TO W-IDARTNR-RADIO                         
032800                                  MID-IDARTNR-IN                          
032900                                  MOD-IDARTNR-UT                          
033000           ELSE                                                           
033100               MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                     
033200               MOVE NEJ TO NYCKLAR-SW                                     
033300           END-IF                                                         
033400        ELSE                                                              
033500           IF MID-IDAPPTYP-UT NOT = ALL '+'                               
033600              MOVE MID-IDAPPTYP-UT TO MOD-IDAPPTYP-UT                     
033700                                      MID-IDAPPTYP-IN                     
033800                                      W-APPARATTYP                        
033900           ELSE                                                           
034000              MOVE NEJ TO NYCKLAR-SW                                      
034100              MOVE MFS-RENSA-FAELT TO MOD-IDAPPTYP-UT                     
034200              MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                      
034300           END-IF                                                         
034400        END-IF                                                            
034500     END-IF                                                               
034600                                                                          
034700                                                                          
034800     IF MID-IDRADIONR-IN = ALL '+'                                        
034900        MOVE NEJ TO NYCKLAR-SW                                            
035000     END-IF                                                               
035100                                                                          
035200     IF MID-IDAPPTYP-IN = ALL '+'                                         
035300     AND MID-IDARTNR-IN = ALL '+'                                         
035400        MOVE NEJ TO NYCKLAR-SW                                            
035500     END-IF                                                               
035600                                                                          
035700     IF NYCKLAR-FEL                                                       
035800*******IF EGEN-MID                                                        
035900          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
036000          CALL WMEDKONV USING MED-WMEDAREA                                
036100          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
036200*******END-IF                                                             
036300     END-IF                                                               
036400                                                                          
036500     .                                                                    
036600     EJECT                                                                
036700 F-LAES-VISA-INFO SECTION.                                                
036800     MOVE 'F-LAES-VISA-INFO'  TO WS-SEKTION                               
036900*    DISPLAY WS-SEKTION                                                   
037000                                                                          
037100     MOVE +1                  TO INDX                                     
037200     MOVE JA                  TO ARTIKEL-SW                               
037300     MOVE JA                  TO INDATA-SW                                
037400     MOVE NEJ                 TO INDATA-SW                                
037500                                                                          
037600     IF WS-IDARTNR IS NUMERIC                                             
037700     AND WS-IDARTNR > ZERO                                                
037800******************************************************************        
037900******* SERIENUMMER OCH ARTIKELNUMMER ÄR IFYLLT ******************        
038000******************************************************************        
038100        PERFORM DB2-SELECT-RADIOT-TAB                                     
038200        IF SQLCODE = ZERO                                                 
038300           IF RADIO-IDARTNR-LABEL > ZERO                                  
038400              MOVE RADIO-IDARTNR-LABEL                                    
038500                              TO W-IDARTNR-LABEL                          
038600             PERFORM DB2-DCL-OPN-CRS-WRADIOT                              
038700             IF SQLCODE = ZERO                                            
038800                PERFORM DB2-FETCH-WRADIOT                                 
038900                IF SQLCODE NOT = ZERO                                     
039000                   MOVE MODEL-MISSING   TO MED-IDMFSFEL                   
039100                   CALL WMEDKONV USING MED-WMEDAREA                       
039200                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
039300                END-IF                                                    
039400                PERFORM UNTIL SQLCODE NOT = ZERO                          
039500                   MOVE RADIO-IDARTNR-LABEL                               
039600                                      TO MOD-IDARTNRO (INDX)              
039700                   MOVE RADIO-APPARATTYP                                  
039800                                      TO W-APPARATTYP                     
039900                                         MOD-IDAPPTYPO (INDX)             
040000***                PERFORM DB2-SELECT-FRADIO-TAB                          
040100                   PERFORM DB2-OPEN-FRADIO-NY-CRS                         
040200                   PERFORM DB2-FETCH-FRADIO-NY-CRS                        
040300                   IF SQLCODE = ZERO                                      
040400                      MOVE FRADIO-SKYDDSKOD                               
040500                                        TO MOD-KDRADIO (INDX)             
040600                   ELSE                                                   
040700                      MOVE MFS-RENSA-FAELT                                
040800                                        TO MOD-KDRADIO (INDX)             
040900                      MOVE ZERO         TO SQLCODE                        
041000                      MOVE MFS-RENSA-FAELT TO                             
041100                                         MOD-IDAPPTYPO (INDX)             
041200                                         MOD-IDARTNRO (INDX)              
041300                      SUBTRACT 1 FROM INDX                                
041400                                                                          
041500                   END-IF                                                 
041600                   PERFORM DB2-CLOSE-FRADIO-NY-CRS                        
041700                   PERFORM DB2-FETCH-WRADIOT                              
041800                   ADD +1                      TO INDX                    
041900                END-PERFORM                                               
042000                PERFORM DB2-CLOSE-WRADIOT-CRS                             
042100             ELSE                                                         
042200                MOVE MODEL-MISSING   TO MED-IDMFSFEL                      
042300                CALL WMEDKONV USING MED-WMEDAREA                          
042400                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
042500             END-IF                                                       
042600           ELSE                                                           
042700              MOVE RADIO-IDARTNR-RADIO                                    
042800                                      TO MOD-IDARTNRO (INDX)              
042900              MOVE RADIO-APPARATTYP                                       
043000                                      TO W-APPARATTYP                     
043100                                         MOD-IDAPPTYPO (INDX)             
043200                                                                          
043300***           PERFORM DB2-SELECT-FRADIO-TAB                               
043400              PERFORM DB2-OPEN-FRADIO-NY-CRS                              
043500              IF SQLCODE = ZERO                                           
043600                  PERFORM DB2-FETCH-FRADIO-NY-CRS                         
043700                  PERFORM UNTIL SQLCODE NOT = ZERO                        
043800                    IF SQLCODE = ZERO                                     
043900                      MOVE FRADIO-SKYDDSKOD                               
044000                                        TO MOD-KDRADIO (INDX)             
044100                      MOVE RADIO-IDARTNR-RADIO                            
044200                                        TO MOD-IDARTNRO (INDX)            
044300                      MOVE RADIO-APPARATTYP                               
044400                                        TO W-APPARATTYP                   
044500                                           MOD-IDAPPTYPO (INDX)           
044600                    END-IF                                                
044700                    PERFORM DB2-FETCH-FRADIO-NY-CRS                       
044800                    ADD +1 TO INDX                                        
044900                  END-PERFORM                                             
045000                                                                          
045100              ELSE                                                        
045200                 MOVE MFS-RENSA-FAELT                                     
045300                                      TO MOD-KDRADIO (INDX)               
045400                 MOVE ZERO    TO SQLCODE                                  
045500              END-IF                                                      
045600              ADD +1                  TO INDX                             
045700              PERFORM DB2-CLOSE-FRADIO-NY-CRS                             
045800              IF SQLCODE = 100                                            
045900                MOVE ZERO TO SQLCODE                                      
046000              END-IF                                                      
046100           END-IF                                                         
046200        ELSE                                                              
046300           MOVE ART-MISSING TO MED-IDMFSINF                               
046400           CALL WMEDKONV USING MED-WMEDAREA                               
046500           MOVE MED-MFSINF TO MOD-TEMFSINF                                
046600           MOVE NEJ                 TO ARTIKEL-SW                         
046700        END-IF                                                            
046800     ELSE                                                                 
046900******************************************************************        
047000******* SERIENUMMER OCH RADIOTYP ÄR IFYLLT ***********************        
047100******************************************************************        
047200****    PERFORM DB2-SELECT-FRADIO-TAB                                     
047300                                                                          
047400        PERFORM DB2-OPEN-FRADIO-NY-CRS                                    
047500        PERFORM DB2-FETCH-FRADIO-NY-CRS                                   
047600        IF SQLCODE = ZERO                                                 
047700           MOVE FRADIO-APPARATTYP      TO WS-APPARATTYP-RED               
047800           MOVE FRADIO-SKYDDSKOD       TO WS-SKYDDSKOD                    
047900        ELSE                                                              
048000           MOVE SPACE                  TO WS-SKYDDSKOD                    
048100           MOVE W-APPARATTYP           TO WS-APPARATTYP-RED               
048200        END-IF                                                            
048300        PERFORM DB2-CLOSE-FRADIO-NY-CRS                                   
048400                                                                          
048500        IF WS-APPARATTYP-RED (1:3) = 'RTI'                                
048600        OR WS-APPARATTYP-RED (1:3) = 'TMC'                                
048700***************************************************************           
048800**** NAVIGATIONS ENHET ****************************************           
048900***************************************************************           
049000***************************************************************           
049100*TABELL FRADIO HAR ETT SERIENR FLERA APPARATTYPER MED TJUVKOD *           
049200*TABELL WRADIOT KAN EN APPARATTYPER HA FLERA ARTIKELNR *******            
049300*T GÖR DET VÄLDIGT SVÅRT ATT FÅ FRAM EN EXAKT TJUVKOD   *******           
049400***************************************************************           
049500           PERFORM FA-LAES-HAMTA-INFO                                     
049600        ELSE                                                              
049700******************************************************                    
049800**** FÖR EN RAD PÅ FRADIO KAN DET FINNAS TVÅ ELLER ***                    
049900**** FLERA RADER PÅ WRADIOT                     ******                    
050000******************************************************                    
050100          IF WS-APPARATTYP-RED (7:1) = 'G'                                
050200          OR WS-APPARATTYP-RED (8:1) = 'G'                                
050300******************************************************                    
050400*  OBS ! G BETYDER ATT RADIO HAR EN GRÅ FRONT     ****                    
050500*  EX PÅ RADIO CR-802G OCH CR-8020G               ****                    
050600******************************************************                    
050700             IF WS-APPARATTYP-RED (7:1) = 'G'                             
050800                PERFORM FA-LAES-HAMTA-INFO                                
050900                MOVE '0'         TO                                       
051000                                 WS-APPARATTYP-RED (7:1)                  
051100                MOVE 'G'         TO                                       
051200                                 WS-APPARATTYP-RED (8:1)                  
051300                MOVE WS-APPARATTYP-RED                                    
051400                                 TO W-APPARATTYP                          
051500                PERFORM DB2-SELECT-FRADIO-TAB                             
051600                IF SQLCODE = ZERO                                         
051700                   MOVE FRADIO-APPARATTYP                                 
051800                                 TO WS-APPARATTYP-RED                     
051900                   MOVE FRADIO-SKYDDSKOD                                  
052000                                 TO WS-SKYDDSKOD                          
052100                   PERFORM FA-LAES-HAMTA-INFO                             
052200                END-IF                                                    
052300             ELSE                                                         
052400                PERFORM FA-LAES-HAMTA-INFO                                
052500                MOVE 'G'     TO WS-APPARATTYP-RED (7:1)                   
052600                MOVE SPACE   TO WS-APPARATTYP-RED (8:1)                   
052700                MOVE WS-APPARATTYP-RED                                    
052800                                 TO W-APPARATTYP                          
052900                PERFORM DB2-SELECT-FRADIO-TAB                             
053000                IF SQLCODE = ZERO                                         
053100                   MOVE FRADIO-APPARATTYP                                 
053200                                 TO WS-APPARATTYP-RED                     
053300                   MOVE FRADIO-SKYDDSKOD                                  
053400                                 TO WS-SKYDDSKOD                          
053500                   PERFORM FA-LAES-HAMTA-INFO                             
053600                END-IF                                                    
053700             END-IF                                                       
053800          ELSE                                                            
053900***************************************************************           
054000** LÄS KURSOR MED RADIOTYP PÅ FRADIO LOW VALUE HIGH VALUE *****           
054100** LÄS SEDAN WRADIOT MED CURSOR FÖR VARJE RADIOTYP        *****           
054200** LÄS FÖRBI ALLA MED GRÅ FRONT                           *****           
054300***************************************************************           
054400             MOVE WS-APPARATTYP-RED                                       
054500                                 TO     W-APPARATTYP-MIN                  
054600                                        W-APPARATTYP-MAX                  
054700             MOVE HIGH-VALUES    TO W-APPARATTYP-MAX (7:2)                
054800             MOVE LOW-VALUES     TO W-APPARATTYP-MIN (7:2)                
054900             PERFORM DB2-DCL-OPN-CRS-FRADIO                               
055000             IF SQLCODE = ZERO                                            
055100                PERFORM DB2-FETCH-FRADIO                                  
055200                IF SQLCODE = ZERO                                         
055300                  PERFORM UNTIL SQLCODE NOT = ZERO                        
055400                    IF FRADIO-APPARATTYP (7:1) = 'G'                      
055500                    OR FRADIO-APPARATTYP (8:1) = 'G'                      
055600                       PERFORM DB2-FETCH-FRADIO                           
055700                    ELSE                                                  
055800                       MOVE FRADIO-APPARATTYP                             
055900                                 TO MOD-IDAPPTYPO (INDX)                  
056000                       MOVE FRADIO-SKYDDSKOD                              
056100                                 TO MOD-KDRADIO (INDX)                    
056200                       PERFORM FB-LAES-HAMTA-INFO                         
056300                       PERFORM DB2-FETCH-FRADIO                           
056400                       ADD +1        TO INDX                              
056500                    END-IF                                                
056600                  END-PERFORM                                             
056700                ELSE                                                      
056800                   MOVE SERIAL-MISSING       TO MED-IDMFSFEL              
056900                   CALL WMEDKONV USING MED-WMEDAREA                       
057000                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
057100                END-IF                                                    
057200                PERFORM DB2-CLOSE-FRADIO-CRS                              
057300             ELSE                                                         
057400                MOVE ERR-CODE-NOT-REGISTERED TO MED-IDMFSFEL              
057500                CALL WMEDKONV USING MED-WMEDAREA                          
057600                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
057700             END-IF                                                       
057800          END-IF                                                          
057900        END-IF                                                            
058000     END-IF                                                               
058100                                                                          
058200     IF INDX < +9                                                         
058300        PERFORM UNTIL INDX > +8                                           
058400           MOVE MFS-RENSA-FAELT                                           
058500                              TO MOD-IDARTNRO (INDX)                      
058600                                 MOD-KDRADIO (INDX)                       
058700                                 MOD-IDAPPTYPO (INDX)                     
058800           ADD +1             TO INDX                                     
058900        END-PERFORM                                                       
059000**   ELSE                                                                 
059100******************************************************************        
059200**** INDEX ÄR FÖR LITET SKÄRMEN HAR FÖR LITE FÄLT DUMPA       ****        
059300******************************************************************        
059400**     CALL ABEND USING                                                   
059500***                    RKOD-ABEND-UTAN-DUMP                               
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900 FA-LAES-HAMTA-INFO SECTION.                                              
060000     MOVE 'FA-LAES-HAMTA-INFO' TO WS-SEKTION                              
060100*    DISPLAY WS-SEKTION                                                   
060200                                                                          
060300     MOVE WS-APPARATTYP-RED    TO W-APPARATTYP-SOK                        
060400     PERFORM DB2-DCL-OPN-CRS-WMODEL                                       
060500     IF SQLCODE = ZERO                                                    
060600        PERFORM DB2-FETCH-WMODEL                                          
060700        PERFORM UNTIL SQLCODE NOT = ZERO                                  
060800           MOVE WS-APPARATTYP-RED                                         
060900                                    TO MOD-IDAPPTYPO (INDX)               
061000           MOVE WS-SKYDDSKOD                                              
061100                                    TO MOD-KDRADIO (INDX)                 
061200           MOVE RADIO-IDARTNR-LABEL                                       
061300                                    TO MOD-IDARTNRO (INDX)                
061400           PERFORM DB2-FETCH-WMODEL                                       
061500           ADD +1               TO INDX                                   
061600        END-PERFORM                                                       
061700        PERFORM DB2-CLOSE-WMODEL-CRS                                      
061800     ELSE                                                                 
061900        MOVE WS-APPARATTYP-RED                                            
062000                                    TO MOD-IDAPPTYPO (INDX)               
062100        MOVE WS-SKYDDSKOD                                                 
062200                                    TO MOD-KDRADIO (INDX)                 
062300        MOVE ZERO     TO MOD-IDARTNRO (INDX)                              
062400        ADD +1        TO INDX                                             
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 FB-LAES-HAMTA-INFO SECTION.                                              
062900     MOVE 'FB-LAES-HAMTA-INFO' TO WS-SEKTION                              
063000*    DISPLAY WS-SEKTION                                                   
063100                                                                          
063200     MOVE FRADIO-APPARATTYP    TO W-APPARATTYP-SOK                        
063300     PERFORM DB2-DCL-OPN-CRS-WMODEL                                       
063400     IF SQLCODE = ZERO                                                    
063500        PERFORM DB2-FETCH-WMODEL                                          
063600        PERFORM UNTIL SQLCODE NOT = ZERO OR INDX > MAX-INDX               
063700           MOVE FRADIO-APPARATTYP                                         
063800                                    TO MOD-IDAPPTYPO (INDX)               
063900           MOVE FRADIO-SKYDDSKOD                                          
064000                                    TO MOD-KDRADIO (INDX)                 
064100           IF RADIO-IDARTNR-LABEL > ZERO                                  
064200              MOVE RADIO-IDARTNR-LABEL                                    
064300                                    TO MOD-IDARTNRO (INDX)                
064400           ELSE                                                           
064500              MOVE RADIO-IDARTNR-RADIO                                    
064600                                    TO MOD-IDARTNRO (INDX)                
064700           END-IF                                                         
064800           PERFORM DB2-FETCH-WMODEL                                       
064900           IF SQLCODE  = ZERO                                             
065000              ADD +1               TO INDX                                
065100           END-IF                                                         
065200        END-PERFORM                                                       
065300        PERFORM DB2-CLOSE-WMODEL-CRS                                      
065400     ELSE                                                                 
065500        MOVE MODEL-MISSING TO MED-IDMFSFEL                                
065600        CALL WMEDKONV USING MED-WMEDAREA                                  
065700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
065800     END-IF                                                               
065900     .                                                                    
066000     EJECT                                                                
066100* --- IMS SEKTIONER ---                                                   
066200                                                                          
066300 IMS-GET-MSG SECTION.                                                     
066400     MOVE 'IMS-GET-MSG'  TO WS-IMS-SEKTION                                
066500*    DISPLAY WS-IMS-SEKTION                                               
066600                                                                          
066700     MOVE '  QC' TO GODK-STATUSKODER                                      
066800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
066900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067000     PERFORM IMS-STATUSKONTROLL                                           
067100     .                                                                    
067200     SKIP3                                                                
067300 IMS-INSERT-MSG SECTION.                                                  
067400     MOVE 'IMS-INSERT-MSG'  TO WS-IMS-SEKTION                             
067500*    DISPLAY WS-IMS-SEKTION                                               
067600                                                                          
067700     IF MSGI-IDLAND-SPR  = 'GB'                                           
067800       MOVE 'N' TO MFS-KDHUVOMR                                           
067900     END-IF                                                               
068000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
068100     MOVE SPACE TO GODK-STATUSKODER                                       
068200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
068300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600     EJECT                                                                
068700 IMS-STATUSKONTROLL SECTION.                                              
068800                                                                          
068900     SET STATUS-IX TO 1                                                   
069000     SEARCH GODK-STATUS                                                   
069100       AT END                                                             
069200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
069300         DELIMITED BY SIZE INTO FELTEXT                                   
069400         CALL FELLOG                                                      
069500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
069600         CONTINUE                                                         
069700     END-SEARCH                                                           
069800     .                                                                    
069900     EJECT                                                                
070000 DB2-DCL-OPN-CRS-WRADIOT SECTION.                                         
070100     MOVE 'DB2-DCL-OPN-CRS-WRADIOT' TO  WS-DB2-SEKTION                    
070200*    DISPLAY WS-DB2-SEKTION                                               
070300* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
070400     EXEC SQL DECLARE WRADIO-CRS CURSOR FOR                               
070500              SELECT IDARTNR_RADIO,                                       
070600                     APPARATTYP,                                          
070700                     BEAPTYP,                                             
070800                     IDARTNR_LABEL                                        
070900              FROM WRADIOT                                                
071000              WHERE IDARTNR_LABEL = :W-IDARTNR-LABEL                      
071100     END-EXEC                                                             
071200     MOVE 000               TO GODK-SQLCODESKODER                         
071300     EXEC SQL OPEN WRADIO-CRS END-EXEC                                    
071400     MOVE SQLCODE           TO SQLCODE-WS                                 
071500     PERFORM DB2-STATUSKONTROLL                                           
071600     .                                                                    
071700     EJECT                                                                
071800 DB2-FETCH-WRADIOT SECTION.                                               
071900     MOVE 'DB2-FETCH-WRADIOT' TO  WS-DB2-SEKTION                          
072000*    DISPLAY WS-DB2-SEKTION                                               
072100     MOVE 000100            TO GODK-SQLCODESKODER                         
072200     EXEC SQL FETCH WRADIO-CRS INTO                                       
072300            :RADIO-IDARTNR-RADIO                                          
072400           ,:RADIO-APPARATTYP                                             
072500           ,:RADIO-BEAPTYP                                                
072600           ,:RADIO-IDARTNR-LABEL                                          
072700     END-EXEC                                                             
072800     MOVE SQLCODE           TO SQLCODE-WS                                 
072900     PERFORM DB2-STATUSKONTROLL                                           
073000     .                                                                    
073100     EJECT                                                                
073200 DB2-CLOSE-WRADIOT-CRS SECTION.                                           
073300     MOVE 'DB2-CLOSE-WRADIOT-CRS' TO  WS-DB2-SEKTION                      
073400*    DISPLAY WS-DB2-SEKTION                                               
073500     SKIP2                                                                
073600     EXEC SQL CLOSE WRADIO-CRS END-EXEC                                   
073700     .                                                                    
073800     EJECT                                                                
073900 DB2-DCL-OPN-CRS-WMODEL SECTION.                                          
074000     MOVE 'DB2-DCL-OPN-CRS-WMODEL ' TO  WS-DB2-SEKTION                    
074100*    DISPLAY WS-DB2-SEKTION                                               
074200* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
074300     EXEC SQL DECLARE WMODEL-CRS CURSOR FOR                               
074400              SELECT IDARTNR_RADIO,                                       
074500                     APPARATTYP,                                          
074600                     BEAPTYP,                                             
074700                     IDARTNR_LABEL                                        
074800              FROM WRADIOT                                                
074900              WHERE APPARATTYP = :W-APPARATTYP-SOK                        
075000     END-EXEC                                                             
075100     MOVE 000               TO GODK-SQLCODESKODER                         
075200     EXEC SQL OPEN WMODEL-CRS END-EXEC                                    
075300     MOVE SQLCODE           TO SQLCODE-WS                                 
075400     PERFORM DB2-STATUSKONTROLL                                           
075500     .                                                                    
075600     EJECT                                                                
075700 DB2-FETCH-WMODEL SECTION.                                                
075800     MOVE 'DB2-FETCH-WMODEL ' TO  WS-DB2-SEKTION                          
075900*    DISPLAY WS-DB2-SEKTION                                               
076000     MOVE 000100            TO GODK-SQLCODESKODER                         
076100     EXEC SQL FETCH WMODEL-CRS INTO                                       
076200            :RADIO-IDARTNR-RADIO                                          
076300           ,:RADIO-APPARATTYP                                             
076400           ,:RADIO-BEAPTYP                                                
076500           ,:RADIO-IDARTNR-LABEL                                          
076600     END-EXEC                                                             
076700     MOVE SQLCODE           TO SQLCODE-WS                                 
076800     PERFORM DB2-STATUSKONTROLL                                           
076900     .                                                                    
077000     EJECT                                                                
077100 DB2-CLOSE-WMODEL-CRS SECTION.                                            
077200     MOVE 'DB2-CLOSE-WMODEL-CRS' TO  WS-DB2-SEKTION                       
077300*    DISPLAY WS-DB2-SEKTION                                               
077400     SKIP2                                                                
077500     EXEC SQL CLOSE WMODEL-CRS END-EXEC                                   
077600     .                                                                    
077700     EJECT                                                                
077800 DB2-DCL-OPN-CRS-FRADIO SECTION.                                          
077900     MOVE 'DB2-DCL-OPN-CRS-FRADIO' TO  WS-DB2-SEKTION                     
078000*    DISPLAY WS-DB2-SEKTION                                               
078100* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
078200     EXEC SQL DECLARE FRADIO-CRS CURSOR FOR                               
078300              SELECT SKYDDSKOD                                            
078400                    ,APPARATTYP                                           
078500              FROM FRADIO                                                 
078600              WHERE   SERIENR = :W-SERIENR                                
078700              AND    (APPARATTYP BETWEEN :W-APPARATTYP-MIN                
078800              AND   :W-APPARATTYP-MAX)                                    
078900     END-EXEC                                                             
079000     MOVE 000               TO GODK-SQLCODESKODER                         
079100     EXEC SQL OPEN FRADIO-CRS END-EXEC                                    
079200     MOVE SQLCODE           TO SQLCODE-WS                                 
079300     PERFORM DB2-STATUSKONTROLL                                           
079400     .                                                                    
079500     EJECT                                                                
079600 DB2-FETCH-FRADIO SECTION.                                                
079700     MOVE 'DB2-FETCH-FRADIO' TO  WS-DB2-SEKTION                           
079800*    DISPLAY WS-DB2-SEKTION                                               
079900     MOVE 000100            TO GODK-SQLCODESKODER                         
080000     EXEC SQL FETCH FRADIO-CRS INTO                                       
080100            :FRADIO-SKYDDSKOD                                             
080200           ,:FRADIO-APPARATTYP                                            
080300     END-EXEC                                                             
080400     MOVE SQLCODE           TO SQLCODE-WS                                 
080500     PERFORM DB2-STATUSKONTROLL                                           
080600     .                                                                    
080700     EJECT                                                                
080800 DB2-SELECT-FRADIO-TAB  SECTION.                                          
080900     MOVE 'DB2-SELECT-FRADIO-TAB' TO WS-DB2-SEKTION                       
081000*    DISPLAY WS-DB2-SEKTION                                               
081100     MOVE 000100  TO GODK-SQLCODESKODER                                   
081200                                                                          
081300     EXEC SQL                                                             
081400           SELECT  SKYDDSKOD,                                             
081500                   APPARATTYP                                             
081600           INTO                                                           
081700                  :FRADIO-SKYDDSKOD,                                      
081800                  :FRADIO-APPARATTYP                                      
081900           FROM    FRADIO                                                 
082000           WHERE   SERIENR = :W-SERIENR                                   
082100             AND   APPARATTYP = :W-APPARATTYP                             
082200     END-EXEC                                                             
082300                                                                          
082400     MOVE SQLCODE TO SQLCODE-WS                                           
082500                     TEST-SQLCODE                                         
082600     PERFORM DB2-STATUSKONTROLL                                           
082700     .                                                                    
082800     EJECT                                                                
082900 DB2-OPEN-FRADIO-NY-CRS SECTION.                                          
083000     MOVE 000100  TO GODK-SQLCODESKODER                                   
083100     EXEC SQL DECLARE FRADIO-NY-CRS CURSOR FOR                            
083200           SELECT  SKYDDSKOD,                                             
083300                   APPARATTYP                                             
083400           FROM    FRADIO                                                 
083500           WHERE   SERIENR = :W-SERIENR                                   
083600             AND   APPARATTYP = :W-APPARATTYP                             
083700     END-EXEC                                                             
083800     EXEC SQL OPEN FRADIO-NY-CRS END-EXEC                                 
083900     MOVE SQLCODE           TO SQLCODE-WS                                 
084000     PERFORM DB2-STATUSKONTROLL                                           
084100     .                                                                    
084200     EJECT                                                                
084300 DB2-FETCH-FRADIO-NY-CRS  SECTION.                                        
084400     MOVE 000100  TO GODK-SQLCODESKODER                                   
084500     EXEC SQL FETCH FRADIO-NY-CRS INTO                                    
084600       :FRADIO-SKYDDSKOD                                                  
084700       ,:FRADIO-APPARATTYP                                                
084800     END-EXEC                                                             
084900     MOVE SQLCODE TO SQLCODE-WS                                           
085000     PERFORM DB2-STATUSKONTROLL                                           
085100     .                                                                    
085200     EJECT                                                                
085300 DB2-CLOSE-FRADIO-NY-CRS SECTION.                                         
085400     EXEC SQL CLOSE FRADIO-NY-CRS END-EXEC                                
085500     .                                                                    
085600     EJECT                                                                
085700                                                                          
085800 DB2-SELECT-RADIOT-TAB  SECTION.                                          
085900     MOVE 'DB2-SELECT-RADIOT-TAB' TO WS-DB2-SEKTION                       
086000*    DISPLAY WS-DB2-SEKTION                                               
086100                                                                          
086200     MOVE 000100  TO GODK-SQLCODESKODER                                   
086300     EXEC SQL SELECT                                                      
086400                  IDARTNR_RADIO,                                          
086500                  APPARATTYP,                                             
086600                  BEAPTYP,                                                
086700                  IDARTNR_LABEL                                           
086800              INTO                                                        
086900                  :RADIO-IDARTNR-RADIO,                                   
087000                  :RADIO-APPARATTYP,                                      
087100                  :RADIO-BEAPTYP,                                         
087200                  :RADIO-IDARTNR-LABEL                                    
087300            FROM WRADIOT                                                  
087400            WHERE IDARTNR_RADIO = :W-IDARTNR-RADIO                        
087500     END-EXEC                                                             
087600                                                                          
087700     MOVE SQLCODE TO SQLCODE-WS                                           
087800                     TEST-SQLCODE                                         
087900     PERFORM DB2-STATUSKONTROLL                                           
088000     .                                                                    
088100     EJECT                                                                
088200 DB2-CLOSE-FRADIO-CRS SECTION.                                            
088300     MOVE 'DB2-CLOSE-FRADIO-CRS' TO  WS-DB2-SEKTION                       
088400*    DISPLAY WS-DB2-SEKTION                                               
088500     SKIP2                                                                
088600     EXEC SQL CLOSE FRADIO-CRS END-EXEC                                   
088700     .                                                                    
088800     EJECT                                                                
088900 DB2-STATUSKONTROLL  SECTION.                                             
089000                                                                          
089100     SET SQLCODE-IX TO 1                                                  
089200     SEARCH GODK-SQLCODE                                                  
089300       AT END CALL FELLOG                                                 
089400       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
089500     END-SEARCH                                                           
089600     .                                                                    
