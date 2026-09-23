000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9042400.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   99/11/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FÖRSÄLJNINGSSTATISTIK                                            
000900*        KOPIERAT STOMMEN FRÅN W2035600                                   
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLUSEA (WDP7)                              
001200*        PROGRAMMET LÄSER      WDL8                                       
001300*                              WDD3                                       
001400*                              WDK6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W90424T                                             
001800*        MID:         W90424I1                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W90424O1                                            
002200*                                                                         
002300*   ÄNDRINGAR:                                                            
002400*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002500*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002600*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002700*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W9042400'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  IX-AR                       PIC 9(9)    VALUE ZERO  COMP-3.          
004500 77  IX-PER                      PIC 9(9)    VALUE ZERO  COMP-3.          
004600 77  IX-FRAN-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
004700 77  IX-TILL-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
004800 77  IX-RULL                     PIC 9(9)    VALUE ZERO  COMP-3.          
004900 77  SPRAK-IX                    PIC 9(9)    VALUE ZERO  COMP-3.          
005000 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 77  DAGENS-PER                  PIC  9(4)   VALUE ZERO.                  
005200 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
005300 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
005400 01  FILLER                      PIC X(04)   VALUE 'WS'.                  
005500 01  WS.                                                                  
005600  05 WS-DAGENS-AAAAMMDD          PIC 9(8)    VALUE ZERO.                  
005700  05 FILLER                      PIC X(08)   VALUE 'WS-AAPP'.             
005800  05 WS-AAPP                     PIC 9(4)    VALUE ZERO.                  
005900  05 FILLER REDEFINES            WS-AAPP.                                 
006000   10 WS-AA                      PIC 9(2).                                
006100   10 WS-PP                      PIC 9(2).                                
006200  05 WS-VAL                      PIC X(1)    VALUE SPACE.                 
006300  05 WS-AARTAL                   PIC 9(4)    VALUE ZERO.                  
006400  05 WS-PER                      PIC 9(2)    VALUE ZERO.                  
006500  05 WS-KVOI-RED                 PIC 9(7)    VALUE ZERO.                  
006600  05 WS-TOTAL-KVOI               PIC 9(7)    VALUE ZERO.                  
006700  05 WS-KVOI-PER         OCCURS 6 TIMES.                                  
006800   10 WS-KVOI-TOT-TAB            PIC 9(7)        VALUE ZERO.              
006900   10 WS-KVOI-TAB        OCCURS 12 TIMES                                  
007000                                 PIC 9(7)        VALUE ZERO.              
007100  05 WS-VV                       PIC  9(2)   VALUE ZERO.                  
007200  05 FILLER                      PIC  X(16)  VALUE 'WS-TABELL'.           
007300  05  WS-TABELL    OCCURS 12.                                             
007400   10 WS-FORSTA-V                PIC  9(2)   VALUE ZERO.                  
007500   10 WS-SISTA-V                 PIC  9(2)   VALUE ZERO.                  
007600   10 WS-KVVIPER                 PIC 9       VALUE ZERO.                  
007700   10 WS-KVOI                    PIC S9(7)   VALUE ZERO COMP-3.           
007800  05 WS-TESTFAELT.                                                        
007900   10 WS-FORSTA-TF               PIC  9(2)   VALUE ZERO.                  
008000   10 FILLER                     PIC  X      VALUE SPACE.                 
008100   10 WS-SISTA-TF                PIC  9(2)   VALUE ZERO.                  
008200   10 FILLER                     PIC  X      VALUE SPACE.                 
008300   10 WS-KVOI-TF                 PIC  9(7)   VALUE ZERO.                  
008400                                                                          
008500                                                                          
008600                                                                          
008700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008800                                                                          
008900 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
009000                                                                          
009100                                                                          
009200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009300     88  INDATA-OK                           VALUE 'J'.                   
009400     88  INDATA-FEL                          VALUE 'N'.                   
009500                                                                          
009600 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
009700     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
009800     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
009900                                                                          
010000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010100     88  NYCKLAR-OK                          VALUE 'J'.                   
010200     88  NYCKLAR-FEL                         VALUE 'N'.                   
010300                                                                          
010400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010500     88  EGEN-MID                            VALUE '9424'.                
010600     88  GODK-MID                            VALUE '2351' '2352'          
010700                                                   '2353' '2354'          
010800                                                   '2355' '2356'          
010900                                                   '2357' '2358'          
011000                                                   '2359'.                
011100     88  HELP-MID                            VALUE '0551'.                
011200     EJECT                                                                
011300*                                                                         
011400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011500 01  GENERELLA-SUBPROGRAM.                                                
011600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012100     EJECT                                                                
012200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012300*01  -COPY WDATAREA                                                       
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012600*01 -COPY WMEDAREA                                                        
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'DISTRIKT-TAB'.        
012900     SKIP3                                                                
013000 01  MESSAGE-CODES.                                                       
013100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013400     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
013500     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
013600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013700     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
013800     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
013900     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
014000     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
014100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014200     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
014300                                                                          
014400 01  MEDDELANDE.                                                          
014500     03  MED-1                  PIC X(30)                                 
014600         VALUE 'TYPE : T,P,D,S,N,E,R OR L     '.                          
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014900*                                                                         
015000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015100     SKIP3                                                                
015200*01 -COPY WMSGINIT                                                        
015300     SKIP3                                                                
015400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015700     SKIP3                                                                
015800*01  MID -COPY W90424I1                                                   
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016100     SKIP3                                                                
016200*01  -COPY WMSGAREA                                                       
016300     EJECT                                                                
016400     03  MOD REDEFINES MSG-AREA.                                          
016500*      05  -COPY W90424O1                                                 
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016800     SKIP3                                                                
016900*01  -COPY WMFSAREA                                                       
017000     EJECT                                                                
017100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017200*                                                                         
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500     SKIP3                                                                
017600 01  NYCKLAR-TILL-DLI.                                                    
017700     03  W-IDARTNR-X.                                                     
017800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017900     03  W-IDUSER-X.                                                      
018000         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
018100     03  W-IDSKYLT-X.                                                     
018200         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
018300     03  W-TIAAAA-X.                                                      
018400         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
018500     SKIP2                                                                
018600*    --- STATUS-KOD FRÅN IMS                                              
018700 01  STATUS-WS                   PIC XX.                                  
018800     88  SEGMENT-FINNS                       VALUE '  '.                  
018900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019000     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
019100                                                   'GB'.                  
019200     SKIP2                                                                
019300 01  GODK-STATUSKODER.                                                    
019400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019500     SKIP3                                                                
019600 01  SSA1                        PIC X(64).                               
019700 01  SSA2                        PIC X(64).                               
019800     EJECT                                                                
019900*    --- IMS FUNKTIONSKODER                                               
020000*01  -COPY W0003                                                          
020100     EJECT                                                                
020200*    ---  DLI INPUT-OUTPUT AREA                                           
020300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL801'.             
020400     SKIP3                                                                
020500 01  DLI-IO-AREA-WDL801.                                                  
020600*        05  -COPY WDL801                                                 
020700     EJECT                                                                
020800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
020900     SKIP3                                                                
021000 01  DLI-IO-AREA-WDL811.                                                  
021100*        05  -COPY WDL811                                                 
021200     EJECT                                                                
021300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
021400     SKIP3                                                                
021500 01  DLI-IO-AREA-WDD301.                                                  
021600*        05  -COPY WDD301  -PRE BENA-                                     
021700     EJECT                                                                
021800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
021900     SKIP3                                                                
022000 01  DLI-IO-AREA-WDD311.                                                  
022100*        05  -COPY WDD311  -PRE BENA-                                     
022200     EJECT                                                                
022300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
022400     SKIP3                                                                
022500 01  DLI-IO-WDK601.                                                       
022600*        05  -COPY WDK601 .                                               
022700     EJECT                                                                
022800 LINKAGE SECTION.                                                         
022900                                                                          
023000*01  -COPY W0009   -PRE MSG-                                              
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE  USEA-                                             
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE  WDL8-                                             
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE  WDD3-                                             
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100*01  -COPY W0008  -PRE  WDK6-                                             
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDL8-PCB                      
024500     WDD3-PCB WDK6-PCB.                                                   
024600 MAIN SECTION.                                                            
024700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDL8-PCB                      
024800     WDD3-PCB WDK6-PCB.                                                   
024900                                                                          
025000     PERFORM IMS-GET-MSG                                                  
025100     IF SEGMENT-FINNS                                                     
025200       PERFORM A-INIT                                                     
025300       PERFORM B-KOLLA-NYCKLAR                                            
025400                                                                          
025500       PERFORM SEC-URITY                                                  
025600       IF MED-IDMFSFEL = ARTIKEL-SAKNAS                                   
025700         CONTINUE                                                         
025800       ELSE                                                               
025900         IF PASSED-SECURITY-CHECK                                         
026000           IF NYCKLAR-OK                                                  
026100             PERFORM F-LAES-VISA-INFO                                     
026200           END-IF                                                         
026300         END-IF                                                           
026400       END-IF                                                             
026500                                                                          
026600       COMPUTE MSG-KVLL = LENGTH OF MOD-W90424O1 + 4                      
026700       PERFORM IMS-INSERT-MSG                                             
026800     END-IF                                                               
026900                                                                          
027000     MOVE ZERO TO RETURN-CODE                                             
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500                                                                          
027600     IF MSG-DUBBLA-TRANSKODER                                             
027700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90424I1                 
027800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028000     ELSE                                                                 
028100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90424I1                  
028200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
028300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028400     END-IF                                                               
028500                                                                          
028600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028900                                                                          
029000     MOVE LOW-VALUE TO MSG-AREA                                           
029100     MOVE 'W90424O1' TO MFS-IDMOD                                         
029200     MOVE '9424' TO MOD-IDTRANS                                           
029300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029400                                                                          
029500     IF EGEN-MID OR HELP-MID                                              
029600       CONTINUE                                                           
029700     ELSE                                                                 
029800       MOVE SPACE TO MFS-KDTRTYP                                          
029900       MOVE '7' TO MFS-IDPFK                                              
030000     END-IF                                                               
030100                                                                          
030200     IF ENGLISH-TEXT                                                      
030300       MOVE +2    TO SPRAK-IX                                             
030400       MOVE 'GB ' TO MED-IDSKYLT                                          
030500     ELSE                                                                 
030600       MOVE +1    TO SPRAK-IX                                             
030700       MOVE 'S  ' TO MED-IDSKYLT                                          
030800     END-IF                                                               
030900                                                                          
031000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
031100     ACCEPT DAGENS-DATUM FROM DATE                                        
031200                                                                          
031300     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
031400     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
031500                                                                          
031600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
031700                     DAT-O-TIDATUM DAT-KDSVAR                             
031800                                                                          
031900     IF DAT-KDSVAR-OK                                                     
032000****             HÄMTA SEKELSIFFROR                                       
032100                                                                          
032200       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
032300       MOVE DAT-TIAARP       TO DAGENS-PER                                
032400       MOVE DAGENS-PER(3:2)  TO WS-PER                                    
032500                                                                          
032600     ELSE                                                                 
032700         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
032800         DELIMITED BY SIZE INTO FELTEXT                                   
032900         CALL FELLOG                                                      
033000     END-IF                                                               
033100                                                                          
033200     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
033300                                                                          
033400     IF EGEN-MID                                                          
033500       CONTINUE                                                           
033600     ELSE                                                                 
033700       MOVE 'T'              TO MID-VAL-IN                                
033800       MOVE SPACE TO MFS-KDTRTYP                                          
033900       MOVE '7' TO MFS-IDPFK                                              
034000     END-IF                                                               
034100                                                                          
034200     .                                                                    
034300     EJECT                                                                
034400 B-KOLLA-NYCKLAR SECTION.                                                 
034500                                                                          
034600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034700     MOVE '001'             TO MSGI-KDCALL                                
034900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034910                               MSGI-IDLTERM-USER                          
035000     MOVE '9424'            TO MSGI-IDTRANS                               
035100     IF EGEN-MID                                                          
035200       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
035300     ELSE                                                                 
035400       IF  MID-IDARTNR-IN NUMERIC                                         
035500       AND MID-IDARTNR-IN > ZERO                                          
035600         MOVE MID-IDARTNR-IN                                              
035700                            TO MSGI-IDARTNR                               
035800       END-IF                                                             
035900     END-IF                                                               
036000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036100                                                                          
036200     IF MSGI-IDLAND-SPR = 'SE'                                            
036300*       MOVE '0'             TO MFS-KDHUVOMR                              
036400        MOVE 'S  '          TO W-IDSKYLT                                  
036500     ELSE                                                                 
036600        MOVE 'GB '          TO W-IDSKYLT                                  
036700     END-IF                                                               
036800                                                                          
036900     MOVE JA TO NYCKLAR-SW                                                
037000                                                                          
037100*    -- KONTROLL AV IDARTNR                                               
037200*    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
037300                                                                          
037400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
037500       MOVE '7'         TO MFS-IDPFK                                      
037600       MOVE SPACE       TO MFS-KDTRTYP                                    
037700     END-IF                                                               
037800     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
037900     IF MSGI-IDARTNR NUMERIC                                              
038000       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
038100     ELSE                                                                 
038200       MOVE NEJ TO NYCKLAR-SW                                             
038300     END-IF                                                               
038400                                                                          
038500     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
038600                                                                          
038700*    -- KONTROLL AV VAL                                                   
038800*    MOVE MFS-RENSA-FAELT    TO MOD-VAL-IN                                
038900                                                                          
039000     IF MID-VAL-IN = ALL '+'                                              
039100       IF MID-VAL-UT = 'TOTALT'                                           
039200         MOVE 'T'            TO WS-VAL                                    
039300       END-IF                                                             
039400       IF MID-VAL-UT = 'PROGNOS'                                          
039500         MOVE 'P'            TO WS-VAL                                    
039600       END-IF                                                             
039700       IF MID-VAL-UT = 'DIVERSE'                                          
039800         MOVE 'D'            TO WS-VAL                                    
039900       END-IF                                                             
040000       IF MID-VAL-UT = 'NDC    '                                          
040100         MOVE 'N'            TO WS-VAL                                    
040200       END-IF                                                             
040300       IF MID-VAL-UT = 'SDC    '                                          
040400         MOVE 'E'            TO WS-VAL                                    
040500       END-IF                                                             
040600       IF MID-VAL-UT = 'SATS   '                                          
040700         MOVE 'S'            TO WS-VAL                                    
040800       END-IF                                                             
040900       IF MID-VAL-UT = 'LEDTID '                                          
041000         MOVE 'L'            TO WS-VAL                                    
041100       END-IF                                                             
041200       IF MID-VAL-UT = 'REFILL '                                          
041300         MOVE 'R'            TO WS-VAL                                    
041400       END-IF                                                             
041500     ELSE                                                                 
041600       MOVE MID-VAL-IN TO WS-VAL                                          
041700       MOVE '7'              TO MFS-IDPFK                                 
041800       MOVE SPACE            TO MFS-KDTRTYP                               
041900     END-IF                                                               
042000                                                                          
042100     IF WS-VAL = 'T'                                                      
042200     OR WS-VAL = 'P'                                                      
042300     OR WS-VAL = 'D'                                                      
042400     OR WS-VAL = 'N'                                                      
042500     OR WS-VAL = 'E'                                                      
042600     OR WS-VAL = 'S'                                                      
042700     OR WS-VAL = 'L'                                                      
042800     OR WS-VAL = 'R'                                                      
042810        CONTINUE                                                          
042900*       IF WS-VAL = 'T'                                                   
043000*          MOVE 'TOTALT'      TO MOD-VAL-UT                               
043100*       END-IF                                                            
043200*       IF WS-VAL = 'P'                                                   
043300*          MOVE 'PROGNOS'     TO MOD-VAL-UT                               
043400*       END-IF                                                            
043500*       IF WS-VAL = 'D'                                                   
043600*          MOVE 'DIVERSE'     TO MOD-VAL-UT                               
043700*       END-IF                                                            
043800*       IF WS-VAL = 'N'                                                   
043900*          MOVE 'NDC    '     TO MOD-VAL-UT                               
044000*       END-IF                                                            
044100*       IF WS-VAL = 'E'                                                   
044200*          MOVE 'SDC    '     TO MOD-VAL-UT                               
044300*       END-IF                                                            
044400*       IF WS-VAL = 'S'                                                   
044500*          MOVE 'SATS   '     TO MOD-VAL-UT                               
044600*       END-IF                                                            
044700*       IF WS-VAL = 'L'                                                   
044800*          MOVE 'LEDTID '     TO MOD-VAL-UT                               
044900*       END-IF                                                            
045000*       IF WS-VAL = 'R'                                                   
045100*          MOVE 'REFILL '     TO MOD-VAL-UT                               
045200*       END-IF                                                            
045300     ELSE                                                                 
045400       MOVE MED-1            TO MOD-TEMFSINF                              
045500       MOVE NEJ TO NYCKLAR-SW                                             
045600     END-IF                                                               
045700                                                                          
045800*    MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
045900     MOVE WS-IDARTNR       TO W-IDARTNR                                   
046000*    INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
046100                                                                          
046200     IF NYCKLAR-FEL                                                       
046300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
046400       CALL WMEDKONV USING MED-WMEDAREA                                   
046500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
046600       PERFORM MFS-RENSA-FAELT-UT                                         
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 F-LAES-VISA-INFO SECTION.                                                
047100                                                                          
047200     PERFORM FA-HAEMTA-BENAEMNING                                         
047300                                                                          
047400     PERFORM FB-HAMTA-VV-I-PER                                            
047500                                                                          
047600     MOVE DAGENS-AAR         TO MOD-AARTAL (6)                            
047700                                WS-AARTAL                                 
047800     SUBTRACT 1 FROM WS-AARTAL                                            
047900     MOVE WS-AARTAL          TO MOD-AARTAL (5)                            
048000     SUBTRACT 1 FROM WS-AARTAL                                            
048100     MOVE WS-AARTAL          TO MOD-AARTAL (4)                            
048200     SUBTRACT 1 FROM WS-AARTAL                                            
048300     MOVE WS-AARTAL          TO MOD-AARTAL (3)                            
048400     SUBTRACT 1 FROM WS-AARTAL                                            
048500     MOVE WS-AARTAL          TO MOD-AARTAL (2)                            
048600     SUBTRACT 1 FROM WS-AARTAL                                            
048700     MOVE WS-AARTAL          TO MOD-AARTAL (1)                            
048800                                                                          
048900     PERFORM FC-HAEMTA-HISTORIK                                           
049000                                                                          
049100     MOVE +1                 TO IX-AR                                     
049200                                IX-PER                                    
049300     PERFORM UNTIL IX-AR > +6                                             
049400*      PERFORM UNTIL IX-PER > +12                                         
049500*        MOVE WS-KVOI-TAB (IX-AR, IX-PER)                                 
049600*                            TO MOD-KVOI (IX-AR, IX-PER)                  
049700*        ADD +1              TO IX-PER                                    
049800*      END-PERFORM                                                        
049900                                                                          
050000       MOVE WS-KVOI-TOT-TAB (IX-AR)                                       
050100                             TO MOD-KVOI-TOT (IX-AR)                      
050200       ADD +1                TO IX-AR                                     
050300       MOVE +1               TO IX-PER                                    
050400     END-PERFORM                                                          
050500     .                                                                    
050600     EJECT                                                                
050700 FA-HAEMTA-BENAEMNING SECTION.                                            
050800                                                                          
050900     PERFORM IMS-GU-D301                                                  
051000     IF SEGMENT-FINNS                                                     
051100       PERFORM IMS-GNP-D311                                               
051200*      IF SEGMENT-FINNS                                                   
051300*        MOVE BENA-TEXT-BEART  TO MOD-BEART-ENG                           
051400*      ELSE                                                               
051500*        MOVE MFS-RENSA-FAELT  TO MOD-BEART-ENG                           
051600*      END-IF                                                             
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 FB-HAMTA-VV-I-PER SECTION.                                               
052100                                                                          
052200*    --- FYLL I VECKONR FÖR PERIODERNA                                    
052300     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
052400     MOVE 01                 TO WS-AAPP(3:2)                              
052500     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
052600     MOVE 'AARP'             TO DAT-KDDATFORM                             
052700     MOVE 1                  TO WS-PP                                     
052800                                                                          
052900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
053000                     DAT-O-TIDATUM DAT-KDSVAR                             
053100                                                                          
053200     IF DAT-KDSVAR-OK                                                     
053300                                                                          
053400       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
053500       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
053600                                                                          
053700     ELSE                                                                 
053800         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
053900         DELIMITED BY SIZE INTO FELTEXT                                   
054000         CALL FELLOG                                                      
054100     END-IF                                                               
054200                                                                          
054300     PERFORM UNTIL WS-PP      >  12                                       
054400       ADD 1                  TO WS-PP                                    
054500       IF WS-PP = 13                                                      
054600         MOVE 53             TO WS-SISTA-V(12)                            
054700       ELSE                                                               
054800                                                                          
054900         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
055000         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
055100                             DAT-O-TIDATUM DAT-KDSVAR                     
055200         IF DAT-KDSVAR-OK                                                 
055300             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
055400             IF WS-PP > 1                                                 
055500               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
055600               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
055700             END-IF                                                       
055800         ELSE                                                             
055900           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
056000           CALL FELLOG                                                    
056100         END-IF                                                           
056200       END-IF                                                             
056300     END-PERFORM                                                          
056400     .                                                                    
056500     EJECT                                                                
056600 FC-HAEMTA-HISTORIK SECTION.                                              
056700                                                                          
056800     MOVE WS-DAGENS-AAAAMMDD (1:4)                                        
056900                               TO W-TIAAAA                                
057000     PERFORM IMS-GU-L811                                                  
057100                                                                          
057200     IF SEGMENT-FINNS                                                     
057300                                                                          
057400       MOVE +6                 TO IX-AR                                   
057500       MOVE +1                 TO IX-PER                                  
057600       MOVE ZERO               TO WS-TOTAL-KVOI                           
057700                                                                          
057800       PERFORM UNTIL IX-PER    =  WS-PER                                  
057900         MOVE ZERO             TO WS-KVOI (IX-PER)                        
058000         MOVE WS-FORSTA-V(IX-PER)                                         
058100                               TO WS-VV                                   
058200         PERFORM UNTIL WS-VV   >  WS-SISTA-V(IX-PER)                      
058300           IF WS-VAL = 'T'                                                
058400             ADD AAR-KVOI-PROG(WS-VV)                                     
058500                               TO WS-KVOI(IX-PER)                         
058600             ADD AAR-KVOI-NDC(WS-VV)                                      
058700                               TO WS-KVOI(IX-PER)                         
058800             ADD AAR-KVOI-SDC(WS-VV)                                      
058900                               TO WS-KVOI(IX-PER)                         
059000             ADD AAR-KVOI-SATS(WS-VV)                                     
059100                               TO WS-KVOI(IX-PER)                         
059200             ADD AAR-KVOI-DIV(WS-VV)                                      
059300                               TO WS-KVOI(IX-PER)                         
059400           END-IF                                                         
059500           IF WS-VAL = 'P'                                                
059600             ADD AAR-KVOI-PROG(WS-VV)                                     
059700                               TO WS-KVOI(IX-PER)                         
059800           END-IF                                                         
059900           IF WS-VAL = 'D'                                                
060000             ADD AAR-KVOI-DIV(WS-VV)                                      
060100                               TO WS-KVOI(IX-PER)                         
060200           END-IF                                                         
060300           IF WS-VAL = 'N'                                                
060400             ADD AAR-KVOI-NDC(WS-VV)                                      
060500                               TO WS-KVOI(IX-PER)                         
060600           END-IF                                                         
060700           IF WS-VAL = 'E'                                                
060800             ADD AAR-KVOI-SDC(WS-VV)                                      
060900                               TO WS-KVOI(IX-PER)                         
061000           END-IF                                                         
061100           IF WS-VAL = 'R'                                                
061200             ADD AAR-KVOI-REFILL(WS-VV)                                   
061300                               TO WS-KVOI(IX-PER)                         
061400           END-IF                                                         
061500           IF WS-VAL = 'S'                                                
061600             ADD AAR-KVOI-SATS(WS-VV)                                     
061700                               TO WS-KVOI(IX-PER)                         
061800           END-IF                                                         
061900           IF WS-VAL = 'L'                                                
062000             ADD AAR-KVOI-LEDTID(WS-VV)                                   
062100                               TO WS-KVOI(IX-PER)                         
062200           END-IF                                                         
062300           ADD +1              TO WS-VV                                   
062400         END-PERFORM                                                      
062500         COMPUTE WS-KVOI-RED       ROUNDED =                              
062600                 WS-KVOI (IX-PER)                                         
062700                            / WS-KVVIPER (IX-PER) * +4.33                 
062800                                                                          
062900         ADD WS-KVOI-RED       TO WS-KVOI-TAB (IX-AR, IX-PER)             
063000                                  WS-TOTAL-KVOI                           
063100         ADD +1                TO IX-PER                                  
063200       END-PERFORM                                                        
063300                                                                          
063400       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-TAB (IX-AR)                 
063500     END-IF                                                               
063600                                                                          
063700     MOVE +5                   TO IX-AR                                   
063800     PERFORM UNTIL IX-AR < +1                                             
063900                                                                          
064000*  LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                        
064100                                                                          
064200       SUBTRACT 1              FROM W-TIAAAA                              
064300       PERFORM IMS-GU-L811                                                
064400                                                                          
064500       IF SEGMENT-FINNS                                                   
064600         MOVE 1                TO IX-PER                                  
064700         MOVE ZERO             TO WS-TOTAL-KVOI                           
064800         PERFORM UNTIL IX-PER > 12                                        
064900                                                                          
065000           MOVE ZERO           TO WS-KVOI (IX-PER)                        
065100           MOVE WS-FORSTA-V (IX-PER)                                      
065200                               TO WS-VV                                   
065300           PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                      
065400             IF WS-VAL = 'T'                                              
065500               ADD AAR-KVOI-PROG(WS-VV)                                   
065600                                 TO WS-KVOI(IX-PER)                       
065700               ADD AAR-KVOI-NDC(WS-VV)                                    
065800                                 TO WS-KVOI(IX-PER)                       
065900               ADD AAR-KVOI-SDC(WS-VV)                                    
066000                                 TO WS-KVOI(IX-PER)                       
066100               ADD AAR-KVOI-SATS(WS-VV)                                   
066200                                 TO WS-KVOI(IX-PER)                       
066300               ADD AAR-KVOI-DIV(WS-VV)                                    
066400                                 TO WS-KVOI(IX-PER)                       
066500             END-IF                                                       
066600             IF WS-VAL = 'P'                                              
066700               ADD AAR-KVOI-PROG(WS-VV)                                   
066800                                 TO WS-KVOI(IX-PER)                       
066900             END-IF                                                       
067000             IF WS-VAL = 'D'                                              
067100               ADD AAR-KVOI-DIV(WS-VV)                                    
067200                                 TO WS-KVOI(IX-PER)                       
067300             END-IF                                                       
067400             IF WS-VAL = 'N'                                              
067500               ADD AAR-KVOI-NDC(WS-VV)                                    
067600                                 TO WS-KVOI(IX-PER)                       
067700             END-IF                                                       
067800             IF WS-VAL = 'E'                                              
067900               ADD AAR-KVOI-SDC(WS-VV)                                    
068000                                 TO WS-KVOI(IX-PER)                       
068100             END-IF                                                       
068200             IF WS-VAL = 'R'                                              
068300               ADD AAR-KVOI-REFILL(WS-VV)                                 
068400                                 TO WS-KVOI(IX-PER)                       
068500             END-IF                                                       
068600             IF WS-VAL = 'S'                                              
068700               ADD AAR-KVOI-SATS(WS-VV)                                   
068800                                 TO WS-KVOI(IX-PER)                       
068900             END-IF                                                       
069000             IF WS-VAL = 'L'                                              
069100               ADD AAR-KVOI-LEDTID(WS-VV)                                 
069200                                 TO WS-KVOI(IX-PER)                       
069300             END-IF                                                       
069400             ADD 1             TO WS-VV                                   
069500           END-PERFORM                                                    
069600                                                                          
069700           COMPUTE WS-KVOI-RED ROUNDED =                                  
069800              WS-KVOI (IX-PER) * +4.33                                    
069900                 / WS-KVVIPER (IX-PER)                                    
070000                                                                          
070100           ADD WS-KVOI-RED     TO WS-KVOI-TAB (IX-AR, IX-PER)             
070200                                 WS-TOTAL-KVOI                            
070300           ADD +1              TO IX-PER                                  
070400         END-PERFORM                                                      
070500                                                                          
070600         ADD WS-TOTAL-KVOI     TO WS-KVOI-TOT-TAB (IX-AR)                 
070700       END-IF                                                             
070800                                                                          
070900       SUBTRACT +1             FROM IX-AR                                 
071000     END-PERFORM                                                          
071100     .                                                                    
071200     EJECT                                                                
071300                                                                          
071400 SEC-URITY SECTION.                                                       
071500     SKIP2                                                                
071600*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
071700     PERFORM IMS-GU-K601                                                  
071800     IF  SEGMENT-FINNS                                                    
071900        MOVE ART-IDLEVNR         TO WS-IDLEVNR-8                          
072000        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
072100        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
072200*         --- BEHÖRIG USER                                                
072300          SET PASSED-SECURITY-CHECK TO TRUE                               
072400        ELSE                                                              
072500*         --- OBEHÖRIG USER / USER NOT AUTHORIZED                         
072600            MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                       
072700            CALL WMEDKONV USING MED-WMEDAREA                              
072800            MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                             
072900        END-IF                                                            
073000     ELSE                                                                 
073100       MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                                
073200       CALL WMEDKONV USING MED-WMEDAREA                                   
073300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
073400       PERFORM MFS-RENSA-FAELT-UT                                         
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900 MFS-RENSA-FAELT-UT SECTION.                                              
074000                                                                          
074100*    --- ALLA UTDATA-FÄLT                                                 
074200*    MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                                
074300                                                                          
074400     MOVE +1                 TO IX-AR                                     
074500                                IX-PER                                    
074600     PERFORM UNTIL IX-AR > +6                                             
074700       MOVE MFS-RENSA-FAELT  TO MOD-AARTAL (IX-AR)                        
074800                                MOD-KVOI-TOT (IX-AR)                      
074900*      PERFORM UNTIL IX-PER > +12                                         
075000*        MOVE MFS-RENSA-FAELT                                             
075100*                            TO MOD-KVOI (IX-AR, IX-PER)                  
075200*        ADD +1              TO IX-PER                                    
075300*      END-PERFORM                                                        
075400       ADD +1                TO IX-AR                                     
075500       MOVE +1               TO IX-PER                                    
075600     END-PERFORM                                                          
075700     .                                                                    
075800     EJECT                                                                
075900* --- IMS SEKTIONER ---                                                   
076000     SKIP3                                                                
076100 IMS-GET-MSG SECTION.                                                     
076200                                                                          
076300     MOVE '  QC' TO GODK-STATUSKODER                                      
076400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
076500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     SKIP3                                                                
076900 IMS-INSERT-MSG SECTION.                                                  
077000                                                                          
077100     IF ENGLISH-TEXT                                                      
077200       MOVE 'N' TO MFS-KDHUVOMR                                           
077300     END-IF                                                               
077400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
077500     MOVE SPACE TO GODK-STATUSKODER                                       
077600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
077700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077800     PERFORM IMS-STATUSKONTROLL                                           
077900     .                                                                    
078000     EJECT                                                                
078100 IMS-GU-L811  SECTION.                                                    
078200                                                                          
078300     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
078400          DELIMITED BY SIZE INTO SSA1                                     
078500     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
078600          DELIMITED BY SIZE INTO SSA2                                     
078700     MOVE '  GE' TO GODK-STATUSKODER                                      
078800     CALL CBLTDLI USING GU WDL8-PCB                                       
078900                                 DLI-IO-AREA-WDL811 SSA1 SSA2             
079000     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
079100     PERFORM IMS-STATUSKONTROLL                                           
079200     .                                                                    
079300     EJECT                                                                
079400 IMS-GU-D301 SECTION.                                                     
079500                                                                          
079600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
079700          DELIMITED BY SIZE INTO SSA1                                     
079800     MOVE '  GE' TO GODK-STATUSKODER                                      
079900     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD301 SSA1               
080000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
080100     PERFORM IMS-STATUSKONTROLL                                           
080200     .                                                                    
080300     SKIP3                                                                
080400 IMS-GNP-D311 SECTION.                                                    
080500                                                                          
080600     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
080700          DELIMITED BY SIZE INTO SSA1                                     
080800     MOVE '  GE' TO GODK-STATUSKODER                                      
080900     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-WDD311 SSA1              
081000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
081100     PERFORM IMS-STATUSKONTROLL                                           
081200     .                                                                    
081300     EJECT                                                                
081400 IMS-GU-K601 SECTION.                                                     
081500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
081600            DELIMITED BY SIZE INTO SSA1                                   
081700     MOVE '  GE' TO GODK-STATUSKODER                                      
081800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
081900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
082000     PERFORM IMS-STATUSKONTROLL                                           
082100     .                                                                    
082200     EJECT                                                                
082300 IMS-STATUSKONTROLL SECTION.                                              
082400                                                                          
082500     SET STATUS-IX TO 1                                                   
082600     SEARCH GODK-STATUS                                                   
082700       AT END                                                             
082800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
082900         DELIMITED BY SIZE INTO FELTEXT                                   
083000         CALL FELLOG                                                      
083100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
083200         CONTINUE                                                         
083300     END-SEARCH                                                           
083400     .                                                                    
