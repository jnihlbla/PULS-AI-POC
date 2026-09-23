000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9031300.                                                
000300 AUTHOR.         OLGRENER LASSI.                                          
000400 DATE-WRITTEN.   08/04/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:       CARPARTS.PULS.EXTORDER                                   
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ONLINE TRATT TILL ORDER-ENTRY FÖR ORDER SOM KOMMER               
001100*        FRÅN EXTERNA SYSTEM VIA VCOM.                                    
001200*        (ERSÄTTER GAMLA W412X5/S1, MM )                                  
001300*                                                                         
001400*        PROGRAMMET SKICKAR ORDRAR/ANNULATIONER TILL W4T25*               
001500*        VIA DISPATCHERN                                                  
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W90313U                                             
001900*        REQUEST:     VCOM TACDIS - W412TS2                               
002000*                     VCOM ...... - W......                               
002100*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W9031300'.            
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700 77  WS-IDOUTREC                 PIC X(30)   VALUE SPACE.                 
003800 77  ORAD-IX                     PIC S9(4)   COMP VALUE +0.               
003900 77  MAX-ORAD-IX                 PIC S9(4)   COMP VALUE +5.               
004000 77  RFS-IX                      PIC S9(4)   COMP VALUE +0.               
004100 77  MAX-RFS-IX                  PIC S9(4)   COMP VALUE +4.               
004200 77  WS-KVDAGAR-RFS-DEF          PIC S9(3)   COMP-3.                      
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  TRE-SEKUNDER                PIC S9(9)   VALUE +300 COMP.             
004600 77  WS-MID-IDSYSTEM             PIC X(4)    VALUE SPACE.                 
004700 77  WS-TILOKDAT                 PIC 9(6).                                
004800                                                                          
004900 77  IN-IDPTYP                   PIC X(3)    VALUE SPACE.                 
005000     88  ANNULLATION             VALUE 'TS6' 'ETC'.                       
005100     88  ORDERRAD                VALUE 'TS2' THRU 'TS5' 'ETC'.            
005200     88  TILLAEGG                VALUE 'ETC'.                             
005300     88  TACDIS                  VALUE 'TS2' THRU 'TS6'.                  
005400     88  TACDIS-DLET             VALUE 'TS6'.                             
005500     88  TACDIS-RAD              VALUE 'TS2' THRU 'TS5'.                  
005600                                                                          
005700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005800     88  INDATA-OK                           VALUE 'J'.                   
005900     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 01  W-VIMSID.                                                            
006200   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
006300   03  FILLER                PIC X(4)    VALUE SPACE.                     
006400     EJECT                                                                
006500                                                                          
006600 01  HDR-AREA.                                                            
006700*    03  -COPY WZ01REQU                                                   
006800*    03  -COPY WZ04HDR                                                    
006900     EJECT                                                                
007000 01  MESSAGE-CODES.                                                       
007100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007200     03  ERR-ARTIKEL             PIC X(3)    VALUE '768'.                 
007300     EJECT                                                                
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
008000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008100     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
008200     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
008300     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
008400     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
008500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
008700     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
008800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'WORKAREA   '.         
009100*   -COPY WORKAREA                                                        
009200     EJECT                                                                
009300     SKIP3                                                                
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MEDKONV-AREA'.        
009600*   -COPY WMEDAREA                                                        
009700     EJECT                                                                
009800 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
009900*   -COPY WMSGINIT                                                        
010000     EJECT                                                                
010100 01  WZ20DAYS                    PIC X(8) VALUE 'WZ20DAYS'.               
010200     SKIP3                                                                
010300*    -COPY WZ20DAYS                                                       
010400     EJECT                                                                
010500                                                                          
010600 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
010700     SKIP3                                                                
010800*01  -COPY WZ01RECV                                                       
010900     EJECT                                                                
011000                                                                          
011100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
011200 01  SEND-AREA.                                                           
011300*    03  -COPY WZ01SEND                                                   
011400     EJECT                                                                
011500 01  SEND-RAD.                                                            
011600   03  STYRTECKEN-RAD          PIC X.                                     
011700   03  MAIL-RAD                PIC X(80)  VALUE SPACE.                    
011800*    --- CONTROL CHARACTERS                                               
011900 01  WS-SKIP1                    PIC X       VALUE ' '.                   
012000 01  WS-SKIP2                    PIC X       VALUE '0'.                   
012100 01  WS-SKIP3                    PIC X       VALUE '-'.                   
012200 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
012300     EJECT                                                                
012400 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
012500*01  -COPY WMSGKOM                                                        
012600     EJECT                                                                
012700 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
012800     SKIP3                                                                
012900*01  -COPY WMSGAREA                                                       
013000     EJECT                                                                
013100*    --- AREOR FÖR W006KOM SUBMODUL                                       
013200 01  FILLER                      PIC X(16)   VALUE 'KOM-AREA'.            
013300 01  KOM-IO-AREA.                                                         
013400   03  KOM-AREA                     PIC X(2400) VALUE SPACE.              
013500   03  FILLER REDEFINES KOM-AREA.                                         
013600*   05 -COPY W4I25101 -PRE 4251-                                          
013700    05  FILLER REDEFINES 4251-MID-W4I25101.                               
013800     07 FILLER                   PIC X(4).                                
013900     07 PREV-ORDER               PIC X(17).                               
014000     EJECT                                                                
014100*03  FILLER  -COPY W4I25201 -PRE 4252-  -RED KOM-AREA.                    
014200     EJECT                                                                
014300*03  FILLER  -COPY W4I25401 -PRE 4254-  -RED KOM-AREA.                    
014400     EJECT                                                                
014500                                                                          
014600 01  FILLER                      PIC X(16) VALUE 'KSIF-AREA'.             
014700 01  KONTROLL-SIFFRA.                                                     
014800   03  REK-IDARTNR               PIC 9(9)    VALUE 0.                     
014900   03  REK-LNGD                  PIC 9(1)    VALUE 9.                     
015000   03  REK-REKSIFFR              PIC 9(1)    VALUE 0.                     
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16) VALUE 'CIA-AREA'.              
015300     SKIP3                                                                
015400*   -COPY W009CIA                                                         
015500     EJECT                                                                
015600                                                                          
015700 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
015800     SKIP3                                                                
015900 01  RECV-AREA.                                                           
016000*    03  -COPY W412TS2 -PRE TACD-                                         
016100     03  FILLER REDEFINES TACD-W412TS2-CTX.                               
016200       05 FILLER                 PIC X(3).                                
016300       05 THIS-ORDER             PIC X(17).                               
016400     EJECT                                                                
016500 01  NYCKLAR-TILL-DLI.                                                    
016600                                                                          
016700     03  W-IDGMT-X.                                                       
016800       05 W-IDDISTR              PIC S9(5)    VALUE ZERO COMP-3.          
016900       05 W-IDKUNDNR             PIC S9(7)    VALUE ZERO COMP-3.          
017000*                                                                         
017100     EJECT                                                                
017200                                                                          
017300*------ STATUS-KOD FRÅN IMS                                               
017400 01  STATUS-WS                   PIC XX.                                  
017500     88  SEGMENT-FINNS                       VALUE '  '.                  
017600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017700     88  IMS-EJ-OK                           VALUE 'XD'.                  
017800     SKIP2                                                                
017900 01  GODK-STATUSKODER.                                                    
018000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100     SKIP3                                                                
018200 01  SSA1                        PIC X(64).                               
018300 01  SSA2                        PIC X(64).                               
018400     EJECT                                                                
018500*    IMS FUNKTIONSKODER                                                   
018600*    -COPY W0003                                                          
018700     EJECT                                                                
018800                                                                          
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
019000 01  DLI-IO-WDB201.                                                       
019100*    03  -COPY WDB201                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16)   VALUE 'WDI2-AREA'.                        
019400 01   DLI-IO-WDI201.                                                      
019500      03  -COPY WDI201.                                                   
019600     EJECT                                                                
019700 LINKAGE SECTION.                                                         
019800*01  -COPY W0009      -PRE MSG-                                           
019900     SKIP2                                                                
020000*01  -COPY W0009      -PRE DISTRDOC-                                      
020100     SKIP2                                                                
020200 01  DISP-PCB                    PIC X.                                   
020300     SKIP2                                                                
020400*01  -COPY W0008      -PRE WDB2-                                          
020500     05  FILLER                  PIC X.                                   
020600     SKIP2                                                                
020700 01  WDP8-PCB                    PIC X.                                   
020800     SKIP2                                                                
020900*01  -COPY W0008      -PRE WDI2-                                          
021000     05  FILLER                  PIC X.                                   
021100     SKIP2                                                                
021200 01  WDP7-PCB                    PIC X.                                   
021300     SKIP2                                                                
021400 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB                           
021500                           DISP-PCB WDB2-PCB WDP8-PCB WDI2-PCB            
021600                           WDP7-PCB.                                      
021700 MAIN SECTION.                                                            
021800     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB                           
021900                           DISP-PCB WDB2-PCB WDP8-PCB WDI2-PCB            
022000                           WDP7-PCB.                                      
022100                                                                          
022200     PERFORM S01-LAES-OPEN                                                
022300     PERFORM A-INIT                                                       
022400                                                                          
022500     PERFORM S01-LAES-MEDDELANDE                                          
022600     PERFORM UNTIL RECV-KDRC > ZERO                                       
022700       PERFORM B-KOLLA-INDATA                                             
022800       IF INDATA-OK                                                       
022900         IF ANNULLATION                                                   
023000           PERFORM C-SKRIV-ANNULL-RAD                                     
023100         END-IF                                                           
023200         IF ORDERRAD                                                      
023300           IF THIS-ORDER NOT = PREV-ORDER                                 
023400             IF PREV-ORDER NOT = SPACE                                    
023500               MOVE JA TO 4252-MID-FLSLUT                                 
023600               PERFORM S02-ORAD-DISP-TRANS                                
023700             END-IF                                                       
023800             PERFORM D-SKRIV-ORDERHUVUD                                   
023900             PERFORM F-INSERT-TACDIS-WDI201                               
024000           ELSE                                                           
024100             IF ORAD-IX = MAX-ORAD-IX                                     
024200               PERFORM S02-ORAD-DISP-TRANS                                
024300             END-IF                                                       
024400           END-IF                                                         
024500           PERFORM E-SKRIV-ORDERRAD                                       
024600         END-IF                                                           
024700       END-IF                                                             
024800       PERFORM S01-LAES-MEDDELANDE                                        
024900     END-PERFORM                                                          
025000     IF PREV-ORDER NOT = SPACE                                            
025100       MOVE JA TO 4252-MID-FLSLUT                                         
025200       PERFORM S02-ORAD-DISP-TRANS                                        
025300     END-IF                                                               
025400                                                                          
025500     PERFORM S01-LAES-CLOSE                                               
025600                                                                          
025700** FÖR ATT UNDVIKA ATT 2 TRANSAR SOM STARTAS INOM SAMMA SEKUND            
025800** ÖVERLAPPAR DET VARANDRAS TIDSSTÄMPELN SOM ANVÄNDS I DISP-DB            
025900** SOM NYCKEL (MSG-KOM-TIKLOCK), LÄGGS IN EN FÖRDRÖJNING HÄR.             
026000** 3 SEKUNDER KLARAR AV 300 ORDRAR MELLAN TVÅ 'TÄTA' TRANSAR              
026100     CALL W009WAIT USING  TRE-SEKUNDER                                    
026200                                                                          
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 A-INIT SECTION.                                                          
026800                                                                          
026900     MOVE LOW-VALUE              TO MSG-KDZ1                              
027000     MOVE LOW-VALUE              TO MSG-KDZ2                              
027100     MOVE '1'                    TO MSG-KDMFSFOR-1                        
027200                                                                          
027300     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
027400     MOVE SPACE                  TO WS-MID-IDSYSTEM                       
027500                                                                          
027600     MOVE +54                    TO MSG-KOM-KVLL                          
027700     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
027800     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
027900     MOVE 'W9031300'             TO MSG-KOM-IDSNDJOB                      
028000     ACCEPT MSG-KOM-TIREGDAT   FROM DATE                                  
028100     ACCEPT MSG-KOM-TIKLOCK    FROM TIME                                  
028200                                                                          
028300     MOVE 'GB ' TO MED-IDSKYLT                                            
028400                                                                          
028500     CALL VIMSID            USING W-IMSID                                 
028600     IF W-IMSID(1:3) = 'IMG'                                              
028700       MOVE 'QASE' TO W-IMSID                                             
028800     END-IF                                                               
028900     IF W-IMSID(1:3) = 'IMP'                                              
029000       MOVE 'DEVE' TO W-IMSID                                             
029100     END-IF                                                               
029200     IF W-IMSID(1:3) = 'IMY'                                              
029300       MOVE 'IGRT' TO W-IMSID                                             
029400     END-IF                                                               
029500     IF W-IMSID(1:3) = 'IMD'                                              
029600       MOVE 'XDEV' TO W-IMSID                                             
029700     END-IF                                                               
029800     IF W-IMSID(1:3) = 'IMB'                                              
029900       MOVE 'ACPT' TO W-IMSID                                             
030000     END-IF                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 B-KOLLA-INDATA SECTION.                                                  
030400                                                                          
030500     MOVE JA TO INDATA-SW                                                 
030600                                                                          
030700     MOVE TACD-IDPTYP TO IN-IDPTYP                                        
030800     IF NOT TACDIS                                                        
030900       MOVE NEJ      TO INDATA-SW                                         
031000     END-IF                                                               
031100                                                                          
031200     IF TACD-IDDISTR NUMERIC AND TACD-IDDISTR > ZERO                      
031300        AND                                                               
031400        TACD-IDKUNDNR NUMERIC                                             
031500        AND                                                               
031600        TACD-IDORDNR7 > ZERO AND TACD-IDORDNR7 < 95000                    
031700        CONTINUE                                                          
031800     ELSE                                                                 
031900       MOVE NEJ      TO INDATA-SW                                         
032000     END-IF                                                               
032100                                                                          
032200     IF INDATA-FEL                                                        
032300       PERFORM BA-FEL-TRANS-MAIL                                          
032400     ELSE                                                                 
032500       MOVE TACD-IDDISTR    TO W-IDDISTR                                  
032600       MOVE TACD-IDKUNDNR   TO W-IDKUNDNR                                 
032700       PERFORM IMS-GU-WDB201                                              
032800       IF SEGMENT-SAKNAS                                                  
032900          MOVE NEJ          TO  GMT-FLLDCKND                              
033000       END-IF                                                             
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 BA-FEL-TRANS-MAIL SECTION.                                               
033500                                                                          
033600     MOVE ERR-WRONG-KEY    TO MED-IDMFSINF                                
033700     CALL WMEDKONV      USING MED-WMEDAREA                                
033800                                                                          
033900     PERFORM S10-OPEN-DP                                                  
034000                                                                          
034100     EVALUATE RECV-ADDISPXTRA                                             
034200       WHEN 'W412TS2A'                                                    
034300         MOVE 'TACDIS'   TO WS-IDOUTREC                                   
034400       WHEN OTHER                                                         
034500         MOVE 'SLASK'    TO WS-IDOUTREC                                   
034600     END-EVALUATE                                                         
034700     PERFORM S10-PUT-HDR                                                  
034800                                                                          
034900     MOVE SPACE TO SEND-RAD                                               
035000     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
035100     MOVE RECV-AREA (1:80) TO MAIL-RAD                                    
035200     PERFORM S10-PUT-LINE                                                 
035300                                                                          
035400     MOVE SPACE TO SEND-RAD                                               
035500     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
035600     STRING 'ENVIRONMENT : ' W-IMSID                                      
035700     DELIMITED BY SIZE INTO MAIL-RAD                                      
035800     PERFORM S10-PUT-LINE                                                 
035900                                                                          
036000     PERFORM S10-CLOSE-DP                                                 
036100     .                                                                    
036200     EJECT                                                                
036300 C-SKRIV-ANNULL-RAD SECTION.                                              
036400                                                                          
036500     MOVE SPACE             TO KOM-AREA                                   
036600                                                                          
036700     IF TACDIS-DLET                                                       
036800       MOVE 'TACD'            TO 4254-MID-IDSYSTEM                        
036900       MOVE TACD-IDDISTR      TO 4254-MID-IDDISTR                         
037000       MOVE TACD-IDKUNDNR     TO 4254-MID-IDKUNDNR                        
037100       MOVE TACD-IDORDNR7     TO 4254-MID-IDORDNR                         
037200                                                                          
037300       MOVE TACD-IDARTPRE     TO CIA-IDARTPRE-IN                          
037400       MOVE TACD-IDARTBET     TO CIA-IDARTBET-IN                          
037500       CALL W009CIA USING CIA-W009CIA                                     
037600       IF CIA-KDSVAR = 'F'                                                
037700         MOVE ZERO            TO 4254-MID-IDARTNR                         
037800       ELSE                                                               
037900         MOVE CIA-IDARTNR     TO 4254-MID-IDARTNR                         
038000       END-IF                                                             
038100       MOVE TACD-KVBEART      TO 4254-MID-KVBEART                         
038200       MOVE JA                TO 4254-MID-FLSLUT                          
038300                                                                          
038400       COMPUTE MSG-KVLL = LENGTH OF 4254-MID-W4I25401-CTX + 17            
038500       MOVE KOM-AREA          TO MSG-INDATA-MINUS-1-TRANSKOD              
038600       MOVE 'W4T254X '        TO MSG-KDTRANS-1                            
038700       MOVE '4254'            TO MSG-IDTRANS-1                            
038800                                                                          
038900       MOVE 'W4I25401'        TO MSG-KOM-IDCPYTXT                         
039000       MOVE 'TACDDLET'        TO MSG-KOM-IDSNDNOD                         
039100       ADD +1                 TO MSG-KOM-TIKLOCK                          
039200                                                                          
039300       CALL W006KOM USING MSG-PCB                                         
039400                          DISP-PCB                                        
039500                          WDP8-PCB                                        
039600                          MSG-KOM-WMSGKOM                                 
039700                          MSG-IO-AREA                                     
039800       IF MSG-KOM-IDMFSMED NOT = SPACE                                    
039900*         FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                       
040000*         DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA               
040100          MOVE ' FELAKTIG DATUM, ELLER ANNAT I TS6-TRANSEN '              
040200                        TO MAIL-RAD                                       
040300          PERFORM S03-FEL-WZ01-RC                                         
040400       END-IF                                                             
040500     END-IF                                                               
040600                                                                          
040700     MOVE SPACE       TO KOM-AREA                                         
040800     .                                                                    
040900     EJECT                                                                
041000 D-SKRIV-ORDERHUVUD SECTION.                                              
041100                                                                          
041200     INITIALIZE                     4251-MID-W4I25101                     
041300                                                                          
041400     MOVE ZERO                   TO 4251-MID-TIREPDAT                     
041500                                                                          
041510     PERFORM DC-GET-LOCALDATE                                             
041520                                                                          
041600*    NEW RULE. FOR ORDERS WITHOUT ORDER CLASS AND WITH REP DATE,          
041700*    SET ORDER CLASS USING FOLLOWING CONDTIONS.                           
041800*    ORDER CLASS AS 1, IF REP DATE <= TODAY                               
041900*    ORDER CLASS AS 2, IF #DAYS BEFORE REP DATE < KVDAGAR-RFS-DEF         
042000*    ELSE ORDER CLASS 3.                                                  
042100*    FOR ORDERS WITHOUT ORDER CLASS AND WITHOUT REP DATE, SET             
042200*    ORDER CLASS AS 1.                                                    
042300                                                                          
042400     IF TACD-KDORDKL = ZERO AND                                           
042500        TACD-TIBEGPAC > ZERO                                              
042600                                                                          
042900       MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                        
043000       MOVE WS-TILOKDAT          TO DAYS-TIDATE1                          
043100       MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                        
043200       MOVE TACD-TIBEGPAC        TO DAYS-TIDATE2                          
043300       MOVE ZERO                 TO DAYS-KVDAYS                           
043400       MOVE SPACE                TO DAYS-IDCALEND                         
043500                                                                          
043600       CALL WZ20DAYS USING DAYS-WZ20DAYS                                  
043700                                                                          
043800       IF DAYS-KDRC = ZERO                                                
043900         IF DAYS-KVDAYS <= ZERO                                           
044000           MOVE 1                TO TACD-KDORDKL                          
044100         ELSE                                                             
044200           MOVE GMT-KVDAGAR-RFS-DEF TO WS-KVDAGAR-RFS-DEF                 
044300           PERFORM                                                        
044400           VARYING RFS-IX FROM 1 BY 1                                     
044500             UNTIL RFS-IX > MAX-RFS-IX                                    
044600             IF GMT-IDDC-RFS (RFS-IX) = GMT-IDDC-BULK(1)                  
044700               MOVE GMT-KVDAGAR-RFS (RFS-IX)                              
044800                                 TO WS-KVDAGAR-RFS-DEF                    
044900             END-IF                                                       
045000           END-PERFORM                                                    
045100                                                                          
045200           IF DAYS-KVDAYS < WS-KVDAGAR-RFS-DEF                            
045300             MOVE 2              TO TACD-KDORDKL                          
045400           ELSE                                                           
045500             MOVE 3              TO TACD-KDORDKL                          
045600           END-IF                                                         
045700         END-IF                                                           
045800       END-IF                                                             
045900     END-IF                                                               
046000                                                                          
046100                                                                          
046200     IF TACD-KDORDKL = ZERO AND                                           
046300        TACD-TIBEGPAC = ZERO                                              
046400       MOVE 1                    TO TACD-KDORDKL                          
046500     END-IF                                                               
046600                                                                          
046700     IF GMT-FLLDCKND = JA                                                 
046800       IF TACD-IDPTYP = 'TS2' OR 'TS3'                                    
046900         MOVE 'LDC '             TO 4251-MID-IDSYSTEM                     
047000         MOVE 'LDC '             TO WS-MID-IDSYSTEM                       
047100         IF TACD-KDORDKL = 2 OR TACD-IDPTYP = 'TS3'                       
047200            MOVE 'PC'            TO 4251-MID-KDORDTYP-LDC                 
047300         END-IF                                                           
047400         IF TACD-KDORDKL = 3                                              
047500         AND TACD-IDPTYP = 'TS2'                                          
047600            IF TACD-KDORDTYP-TACDIS = 'V '                                
047700               MOVE 'PW'         TO 4251-MID-KDORDTYP-LDC                 
047800            ELSE                                                          
047900               MOVE 'PC'         TO 4251-MID-KDORDTYP-LDC                 
048000            END-IF                                                        
048100         END-IF                                                           
048200         MOVE TACD-TIBEGPAC      TO 4251-MID-TIREPDAT                     
048300       ELSE                                                               
048400         IF TACD-KDORDKL = 1                                              
048500            IF TACD-KDORDTYP-TACDIS = 'VA'                                
048600               MOVE 'FW'         TO 4251-MID-KDORDTYP-LDC                 
048700            ELSE                                                          
048800               MOVE 'FC'         TO 4251-MID-KDORDTYP-LDC                 
048900            END-IF                                                        
049000         END-IF                                                           
049100         MOVE 'TACD'             TO 4251-MID-IDSYSTEM                     
049200         MOVE 'TACD'             TO WS-MID-IDSYSTEM                       
049300       END-IF                                                             
049400     ELSE                                                                 
049500       MOVE 'OVR '               TO 4251-MID-IDSYSTEM                     
049600       MOVE 'OVR '               TO WS-MID-IDSYSTEM                       
049700     END-IF                                                               
049800     MOVE TACD-IDDISTR           TO 4251-MID-IDDISTR                      
049900     MOVE TACD-IDKUNDNR          TO 4251-MID-IDKUNDNR                     
050000     MOVE TACD-IDORDNR7          TO 4251-MID-IDORDNR                      
050100     MOVE TACD-KDORDKL           TO 4251-MID-KDORDKL                      
050200     MOVE TACD-TIBEGPAC          TO 4251-MID-TIRFS                        
050300     IF (4251-MID-IDSYSTEM = 'LDC ' OR 'TACD')                            
050400          AND 4251-MID-TIRFS > ZERO                                       
050500       PERFORM DA-BERAKNA-RFS                                             
050600     END-IF                                                               
050700     MOVE 'R'                    TO 4251-MID-KDFAKTYP                     
050800     MOVE TACD-FLRESTN           TO 4251-MID-FLRESTN                      
050801                                                                          
050802     IF (WS-MID-IDSYSTEM = 'LDC ' OR 'TACD')                              
050803     AND GMT-FLLDCKND = JA AND TACD-IDGROSS = SPACE                       
050811*       *NO REAL ADDRESS FROM TACDIS. SHALL NOT UPDATE ORDER-             
050812*       *HEAD. W40251 WILL UPDATE ORDERHEAD FROM CUSTOMERFILE.            
050820        MOVE SPACE               TO 4251-MID-BEGMT-RAD1                   
050830        MOVE SPACE               TO 4251-MID-BEGMT-RAD2                   
050840        MOVE SPACE               TO 4251-MID-ADGMT-GATA                   
050850        MOVE SPACE               TO 4251-MID-ADGMT-PADR                   
050860     ELSE                                                                 
050900        MOVE TACD-BEGMT-RAD1     TO 4251-MID-BEGMT-RAD1                   
051000        MOVE TACD-BEGMT-RAD2     TO 4251-MID-BEGMT-RAD2                   
051300        MOVE TACD-ADGMT-GATA     TO 4251-MID-ADGMT-GATA                   
051400        MOVE TACD-ADGMT-PADR     TO 4251-MID-ADGMT-PADR                   
051401     END-IF                                                               
051402                                                                          
051410     MOVE TACD-IDGROSS           TO 4251-MID-IDGROSS                      
051500     MOVE TACD-IDDEPT            TO 4251-MID-IDDEPT                       
051600     MOVE TACD-IDBILREG          TO 4251-MID-IDBILREG                     
051700     MOVE TACD-IDVIN             TO 4251-MID-IDVIN                        
051800     MOVE TACD-IDCISNR           TO 4251-MID-IDCISNR                      
051900     MOVE NEJ                    TO 4251-MID-FLAUTPAC                     
052000                                    4251-MID-FLEMBORD                     
052100                                    4251-MID-FLOVRLEV                     
052200                                    4251-MID-FLFORBI                      
052300                                    4251-MID-FLORDTIL                     
052400                                                                          
052500     PERFORM DB-OHUV-DISP-TRANS                                           
052600     .                                                                    
052700     EJECT                                                                
052800                                                                          
052900 DA-BERAKNA-RFS SECTION.                                                  
053000                                                                          
053100*-- BERÄKNA RFS  UTIFRÅN REP.DATUM OM DET FINNS                           
053200     IF GMT-FLLDCKND = JA                                                 
053300                                                                          
053400       MOVE GMT-IDDC-BULK(1)     TO WORK-IDDC                             
053500       MOVE +002                 TO WORK-KDCALL                           
053600       MOVE +001                 TO WORK-KVWORKD                          
053700       IF TACD-TIBEGPAC = ZERO                                            
053800         MOVE WS-TILOKDAT        TO WORK-TIAAMMDD-FOM                     
053900       ELSE                                                               
054000         MOVE TACD-TIBEGPAC      TO WORK-TIAAMMDD-FOM                     
054100       END-IF                                                             
054200       CALL WORKDAY           USING WORK-KDCALL                           
054300                                    WORK-DATE-AREA                        
054400                                    WORK-KDSVAR                           
054500       IF WORK-KDSVAR-FEL                                                 
054600         MOVE 'SECT DA-1, DAT. SAKNAS I WORKDAY RFS NOLLAD'               
054700                                 TO MAIL-RAD                              
054800         PERFORM S03-FEL-WZ01-RC                                          
054900         MOVE ZERO               TO 4251-MID-TIRFS                        
055000       ELSE                                                               
055100         MOVE +003               TO WORK-KDCALL                           
055200         MOVE GMT-KVDAGAR-RFS-DEF TO WORK-KVWORKD                         
055300         PERFORM                                                          
055400         VARYING RFS-IX FROM 1 BY 1                                       
055500           UNTIL RFS-IX > MAX-RFS-IX                                      
055600           IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                           
055700             MOVE GMT-KVDAGAR-RFS (RFS-IX)                                
055800                                 TO WORK-KVWORKD                          
055900           END-IF                                                         
056000         END-PERFORM                                                      
056100         ADD +1                  TO WORK-KVWORKD                          
056200*--      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                       
056300*--      ANTAL DAGAR FÖRE RFS.                                            
056400*--      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...           
056500*--      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                 
056600*--      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                  
056700                                                                          
056800         CALL WORKDAY         USING WORK-KDCALL                           
056900                                    WORK-DATE-AREA                        
057000                                    WORK-KDSVAR                           
057100         IF WORK-KDSVAR-FEL                                               
057200           MOVE 'SECT DA-2, DAT. SAKNAS I WORKDAY RFS NOLLAD'             
057300                                 TO MAIL-RAD                              
057400           PERFORM S03-FEL-WZ01-RC                                        
057500           MOVE ZERO             TO 4251-MID-TIRFS                        
057600         ELSE                                                             
057700           IF WORK-TIAAMMDD-FOM < WS-TILOKDAT                             
057800             MOVE +002           TO WORK-KDCALL                           
057900             MOVE +001           TO WORK-KVWORKD                          
058000             MOVE WS-TILOKDAT    TO WORK-TIAAMMDD-FOM                     
058100             CALL WORKDAY     USING WORK-KDCALL                           
058200                                    WORK-DATE-AREA                        
058300                                    WORK-KDSVAR                           
058400             IF WORK-KDSVAR-FEL                                           
058500               MOVE 'SECT DA-3, DAT. SAKNAS I WORKDAY RFS NOLLAD'         
058600                                 TO MAIL-RAD                              
058700               PERFORM S03-FEL-WZ01-RC                                    
058800               MOVE ZERO         TO 4251-MID-TIRFS                        
058900             ELSE                                                         
059000               MOVE WORK-TIAAMMDD-TOM                                     
059100                                 TO 4251-MID-TIRFS                        
059200             END-IF                                                       
059300           ELSE                                                           
059400             MOVE WORK-TIAAMMDD-FOM                                       
059500                                 TO 4251-MID-TIRFS                        
059600           END-IF                                                         
059700         END-IF                                                           
059800       END-IF                                                             
059900     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
060200 DB-OHUV-DISP-TRANS SECTION.                                              
060300                                                                          
060400     COMPUTE MSG-KVLL = LENGTH OF 4251-MID-W4I25101 + 17                  
060500     MOVE KOM-AREA          TO MSG-INDATA-MINUS-1-TRANSKOD                
060600     MOVE 'W4T251X '        TO MSG-KDTRANS-1                              
060700     MOVE '4251'            TO MSG-IDTRANS-1                              
060800                                                                          
060900     MOVE 'W4I25101'        TO MSG-KOM-IDCPYTXT                           
061000     MOVE 'TACD ORD'        TO MSG-KOM-IDSNDNOD                           
061100     ADD +1                 TO MSG-KOM-TIKLOCK                            
061200                                                                          
061300     CALL W006KOM USING MSG-PCB                                           
061400                        DISP-PCB                                          
061500                        WDP8-PCB                                          
061600                        MSG-KOM-WMSGKOM                                   
061700                        MSG-IO-AREA                                       
061800     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
061900        MOVE ' FELAKTIG DATUM,TID, MM FRÅN TACDIS'                        
062000                      TO MAIL-RAD                                         
062100        PERFORM S03-FEL-WZ01-RC                                           
062200     END-IF                                                               
062300                                                                          
062400     MOVE SPACE       TO KOM-AREA                                         
062500     .                                                                    
062600     EJECT                                                                
062700 DC-GET-LOCALDATE SECTION.                                                
062800                                                                          
062900     MOVE ALL '+'              TO MSGI-WMSGINIT                           
063000     MOVE '013'                TO MSGI-KDCALL                             
063100     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
063200     MOVE GMT-IDDC-BULK(1)     TO MSGI-IDUSER(6:2)                        
063300     MOVE '9313'               TO MSGI-IDTRANS                            
063400                                                                          
063500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
063600                                                                          
063700     MOVE MSGI-TILOKDAT        TO WS-TILOKDAT                             
063800     .                                                                    
063900     EJECT                                                                
064000 E-SKRIV-ORDERRAD SECTION.                                                
064100                                                                          
064200     ADD +1         TO ORAD-IX                                            
064300                                                                          
064400     IF GMT-FLLDCKND = JA                                                 
064500       IF TACD-IDPTYP = 'TS2' OR 'TS3'                                    
064600         MOVE 'LDC '             TO 4252-MID-IDSYSTEM                     
064700         MOVE TACD-BERADREF      TO 4252-MID-IDKUNDRF-WIP(ORAD-IX)        
064800         MOVE TACD-BERADREF      TO 4252-MID-IDKUNDRF-WIP(ORAD-IX)        
064900       ELSE                                                               
065000         IF TACD-KDORDKL = 1                                              
065100           MOVE TACD-BERADREF    TO 4252-MID-IDKUNDRF-WIP(ORAD-IX)        
065200         END-IF                                                           
065300         MOVE 'TACD'             TO 4252-MID-IDSYSTEM                     
065400       END-IF                                                             
065500     ELSE                                                                 
065600       MOVE 'OVR '               TO 4252-MID-IDSYSTEM                     
065700     END-IF                                                               
065800                                                                          
065900     MOVE TACD-IDDISTR           TO 4252-MID-IDDISTR                      
066000     MOVE TACD-IDKUNDNR          TO 4252-MID-IDKUNDNR                     
066100     MOVE TACD-IDORDNR7          TO 4252-MID-IDORDNR                      
066200     MOVE TACD-BERADREF          TO 4252-MID-BERADREF (ORAD-IX)           
066300                                                                          
066400     MOVE 'N'                    TO 4252-MID-FLSLUT                       
066500                                                                          
066600     MOVE TACD-IDARTPRE          TO CIA-IDARTPRE-IN                       
066700     MOVE TACD-IDARTBET          TO CIA-IDARTBET-IN                       
066800     CALL W009CIA USING CIA-W009CIA                                       
066900     IF CIA-KDSVAR = 'F'                                                  
067000       MOVE ZERO                 TO 4252-MID-IDARTNR(ORAD-IX)             
067100     ELSE                                                                 
067200       MOVE CIA-IDARTNR          TO 4252-MID-IDARTNR(ORAD-IX)             
067300     END-IF                                                               
067400                                                                          
067500     IF 4252-MID-IDARTNR(ORAD-IX)  NUMERIC                                
067600       MOVE 4252-MID-IDARTNR(ORAD-IX) TO REK-IDARTNR                      
067700       MOVE 0                    TO REK-REKSIFFR                          
067800       MOVE 9                    TO REK-LNGD                              
067900       CALL W009KSIF USING REK-IDARTNR                                    
068000                           REK-LNGD                                       
068100                           REK-REKSIFFR                                   
068200       MOVE REK-REKSIFFR         TO 4252-MID-REKSIFFR(ORAD-IX)            
068300     ELSE                                                                 
068400       MOVE ZERO                 TO 4252-MID-REKSIFFR(ORAD-IX)            
068500     END-IF                                                               
068600                                                                          
068700     MOVE TACD-KVBEART           TO 4252-MID-KVBEART (ORAD-IX)            
068800     MOVE ZERO                   TO 4252-MID-IDKONTO (ORAD-IX)            
068900     MOVE SPACE                  TO 4252-MID-IDKST   (ORAD-IX)            
069000     MOVE TACD-BERADREF          TO 4252-MID-BERADREF(ORAD-IX)            
069100                                                                          
069200     .                                                                    
069300     EJECT                                                                
069400                                                                          
069500 F-INSERT-TACDIS-WDI201   SECTION.                                        
069600                                                                          
069700*TACDIS NYTT                                                              
069800     IF (WS-MID-IDSYSTEM = 'LDC ' OR 'TACD')                              
069900     AND GMT-FLLDCKND = JA                                                
070000                                                                          
070100       MOVE TACD-IDDISTR         TO TAKF-IDDISTR                          
070200       MOVE TACD-IDKUNDNR        TO TAKF-IDKUNDNR                         
070300       MOVE SPACE                TO TAKF-IDKUNDRF                         
070400       MOVE TACD-IDORDNR7        TO TAKF-IDORDNR7                         
070500       MOVE TACD-BEMEKAN         TO TAKF-BEMEKAN                          
070600       MOVE TACD-BETELNR-TACD    TO TAKF-BETELNR-TACD                     
070700       MOVE TACD-FLFPLOCK        TO TAKF-FLFPLOCK                         
070800       MOVE TACD-IDBILREG        TO TAKF-IDBILREG                         
070900       MOVE TACD-IDGROSS         TO TAKF-IDGROSS                          
071000       MOVE TACD-TETACDBO        TO TAKF-TETACDBO                         
071100       MOVE TACD-TIHHMM          TO TAKF-TIHHMM                           
071110       MOVE TACD-BEGMT-RAD1      TO TAKF-BEGMT-RAD1                       
071120       MOVE TACD-BEGMT-RAD2      TO TAKF-BEGMT-RAD2                       
071131       MOVE TACD-ADGMT-GATA      TO TAKF-ADGMT-GATA                       
071140       MOVE TACD-ADGMT-PADR      TO TAKF-ADGMT-PADR                       
071200       ACCEPT TAKF-TIREGDAT      FROM DATE                                
071300                                                                          
071400       PERFORM IMS-ISRT-WDI201                                            
071500     END-IF                                                               
071600     MOVE SPACE                  TO WS-MID-IDSYSTEM                       
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
072000 S02-ORAD-DISP-TRANS SECTION.                                             
072100                                                                          
072200     COMPUTE MSG-KVLL = LENGTH OF 4252-MID-W4I25201 + 17                  
072300                                                                          
072400     MOVE 'W4T252X '          TO MSG-KDTRANS-1                            
072500     MOVE '4252'              TO MSG-IDTRANS-1                            
072600     MOVE KOM-AREA            TO MSG-INDATA-MINUS-1-TRANSKOD              
072700                                                                          
072800     CALL W006KOM USING MSG-PCB                                           
072900                        DISP-PCB                                          
073000                        WDP8-PCB                                          
073100                        MSG-KOM-WMSGKOM                                   
073200                        MSG-IO-AREA                                       
073300     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
073400*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
073500*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
073600        MOVE ' FELAKTIG DATUM,TID, MM FRÅN TACDIS-S02'                    
073700                      TO MAIL-RAD                                         
073800        PERFORM S03-FEL-WZ01-RC                                           
073900     END-IF                                                               
074000                                                                          
074100     MOVE ZERO     TO ORAD-IX                                             
074200     MOVE SPACE    TO KOM-AREA                                            
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 S01-LAES-OPEN SECTION.                                                   
074700                                                                          
074800     MOVE 'OPEN'                 TO RECV-KDFUNC                           
074900     MOVE 'CARPARTS.PULS.EXTORDER'           TO RECV-ADDISPABS            
075000     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
075100                                                                          
075200     IF RECV-KDRC > 0 AND NOT = 20                                        
075300       MOVE SPACE TO SEND-RAD                                             
075400       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
075500       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
075600       DELIMITED BY SIZE INTO MAIL-RAD                                    
075700       PERFORM S03-FEL-WZ01-RC                                            
075800     END-IF                                                               
075900     .                                                                    
076000     SKIP3                                                                
076100 S01-LAES-MEDDELANDE SECTION.                                             
076200                                                                          
076300     MOVE 'GET'                      TO RECV-KDFUNC                       
076400     MOVE +2000                      TO RECV-KVDLEN                       
076500     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
076600                                                                          
076700     IF RECV-KDRC > 1                                                     
076800       MOVE SPACE TO SEND-RAD                                             
076900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
077000       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
077100       DELIMITED BY SIZE INTO MAIL-RAD                                    
077200       PERFORM S03-FEL-WZ01-RC                                            
077300     END-IF                                                               
077400     .                                                                    
077500     SKIP3                                                                
077600 S01-LAES-CLOSE SECTION.                                                  
077700                                                                          
077800     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
077900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
078000                                                                          
078100     IF RECV-KDRC > 0                                                     
078200       MOVE SPACE TO SEND-RAD                                             
078300       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
078400       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
078500       DELIMITED BY SIZE INTO MAIL-RAD                                    
078600       PERFORM S03-FEL-WZ01-RC                                            
078700     END-IF                                                               
078800     .                                                                    
078900     EJECT                                                                
079000 S03-FEL-WZ01-RC SECTION.                                                 
079100                                                                          
079200     PERFORM S10-OPEN-DP                                                  
079300                                                                          
079400     MOVE 'SLASK'    TO WS-IDOUTREC                                       
079500     PERFORM S10-PUT-HDR                                                  
079600                                                                          
079700     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
079800     PERFORM S10-PUT-LINE                                                 
079900                                                                          
080000     MOVE SPACE TO SEND-RAD                                               
080100     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
080200     MOVE RECV-AREA (1:80) TO MAIL-RAD                                    
080300     PERFORM S10-PUT-LINE                                                 
080400                                                                          
080500     MOVE SPACE TO SEND-RAD                                               
080600     MOVE WS-SKIP2 TO STYRTECKEN-RAD                                      
080700     STRING 'ENVIRONMENT : ' W-IMSID                                      
080800     DELIMITED BY SIZE INTO MAIL-RAD                                      
080900     PERFORM S10-PUT-LINE                                                 
081000                                                                          
081100     PERFORM S10-CLOSE-DP                                                 
081200     .                                                                    
081300     EJECT                                                                
081400 S10-OPEN-DP SECTION.                                                     
081500                                                                          
081600     MOVE 'OPEN'                        TO SEND-KDFUNC                    
081700     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
081800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
081900                         SEND-OPEN-AREA                                   
082000     IF SEND-KDRC > 0                                                     
082100       MOVE SPACE TO SEND-RAD                                             
082200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
082300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
082400       DELIMITED BY SIZE INTO MAIL-RAD                                    
082500       PERFORM S03-FEL-WZ01-RC                                            
082600     END-IF                                                               
082700     .                                                                    
082800     EJECT                                                                
082900 S10-PUT-HDR SECTION.                                                     
083000                                                                          
083100     MOVE 1                       TO REQU-IDMSGVER                        
083200     MOVE ' '                     TO REQU-KDPGMACT                        
083300     MOVE IDPGM                   TO REQU-IDUSER                          
083400     MOVE 'ORDER-EXT-9313'        TO HDR-IDOUTTYPE                        
083500     MOVE WS-IDOUTREC             TO HDR-IDOUTREC                         
083600     MOVE SPACE                   TO HDR-IDLIST                           
083700     MOVE 'PUT'                   TO SEND-KDFUNC                          
083800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
083900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
084000                         SEND-KVDLEN                                      
084100                         HDR-AREA                                         
084200     IF SEND-KDRC > ZERO                                                  
084300       MOVE SPACE TO SEND-RAD                                             
084400       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
084500       STRING 'WZ01SEND PUT-HDR ERROR RC=' KDRC-DISPLAY                   
084600       DELIMITED BY SIZE INTO MAIL-RAD                                    
084700       PERFORM S03-FEL-WZ01-RC                                            
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 S10-PUT-LINE SECTION.                                                    
085200                                                                          
085300     MOVE 'PUT'                           TO SEND-KDFUNC                  
085400     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
085500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
085600                         SEND-KVDLEN                                      
085700                         SEND-RAD                                         
085800     IF SEND-KDRC > ZERO                                                  
085900       MOVE SPACE TO SEND-RAD                                             
086000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
086100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
086200       DELIMITED BY SIZE INTO MAIL-RAD                                    
086300       PERFORM S03-FEL-WZ01-RC                                            
086400     END-IF                                                               
086500     .                                                                    
086600     EJECT                                                                
086700 S10-CLOSE-DP SECTION.                                                    
086800                                                                          
086900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
087000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
087100                                                                          
087200     IF SEND-KDRC > 0                                                     
087300       MOVE SPACE TO SEND-RAD                                             
087400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
087500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
087600       DELIMITED BY SIZE INTO MAIL-RAD                                    
087700       PERFORM S03-FEL-WZ01-RC                                            
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100 IMS-GU-WDB201 SECTION.                                                   
088200                                                                          
088300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
088400          DELIMITED BY SIZE INTO SSA1                                     
088500     MOVE '  GE' TO GODK-STATUSKODER                                      
088600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
088700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
088800     PERFORM IMS-STATUSKONTROLL                                           
088900     .                                                                    
089000     SKIP3                                                                
089100                                                                          
089200 IMS-ISRT-WDI201      SECTION.                                            
089300                                                                          
089400     MOVE   'WDI201'        TO SSA1                                       
089500     MOVE '  II' TO GODK-STATUSKODER                                      
089600     CALL CBLTDLI USING ISRT WDI2-PCB DLI-IO-WDI201 SSA1                  
089700     MOVE WDI2-STATUS-CODE TO STATUS-WS                                   
089800     PERFORM IMS-STATUSKONTROLL                                           
089900     SKIP3                                                                
090000     .                                                                    
090100                                                                          
090200 IMS-STATUSKONTROLL SECTION.                                              
090300                                                                          
090400     SET STATUS-IX TO 1                                                   
090500     SEARCH GODK-STATUS                                                   
090600       AT END                                                             
090700         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT                         
090800         CALL FELLOG                                                      
090900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
091000         CONTINUE                                                         
091100     END-SEARCH                                                           
091200     .                                                                    
