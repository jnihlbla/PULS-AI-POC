000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4070200.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   94/07/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        ADMINISTRATION AV ANALYSNUMMERREGISTRET                          
001000*                                                                         
001100*        PGM:ET SOM ÄR MPP GER MÖJLIGHET FÖRUTOM FRÅGA                    
001200*        PÅ VISS ARTIKEL OCKSÅ FÖR BORTTAG AV ANALYSNR,                   
001300*        SAMT NYUPPLÄGG MED ANGIVEN GILTIGHETS PERIOD.                    
001400*        HÅRDA KONTROLLER UTFÖRS VID NYANGIVEN TIDSPERIOD                 
001500*        SÅ ATT INGA ÖVERLAPPNINGAR SKER.                                 
001600*                                                                         
001700*                                                                         
001800*    OBSERVERA: -----------------------------------------------+          
001900*    ¦   VID FÖRÄNDRINGAR AV LOGIKEN, IFRÅGASÄTT OM KODEN ÄVEN ¦          
002000*    ¦   SKALL ÄNDRAS I SPIE-KOPIAN  W9043500                  ¦          
002100*    +---------------------------------------------------------+          
002200*                                                                         
002300*                                                                         
002400*        PROGRAMMET UPPDATERAR WL4109 (WDR1)                              
002500*                                                                         
002600*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W4T702                                              
003000*                     W4T702U                                             
003100*        MID:         W4I70201                                            
003200*                                                                         
003300*    UTDATA.                                                              
003400*        MOD:         W4O70201                                            
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 WORKING-STORAGE SECTION.                                                 
004100*    -COPY WY2000W1                                                       
004200     SKIP3                                                                
004300 77  IDPGM                       PIC X(08)   VALUE 'W4070200'.            
004400 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  FEL                         PIC X       VALUE 'N'.                   
004800 77  IX                          PIC S9(4)   VALUE +0   COMP SYNC.        
004900 77  MAX-IX                      PIC S9(4)   VALUE +20  COMP SYNC.        
005000 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
005100 77  MAX-INDX                    PIC S9(4)   VALUE +10  COMP SYNC.        
005200 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +0   COMP SYNC.        
005300 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005400 77  WS-IDFTG                    PIC 9(2)    VALUE ZERO.                  
005410 77  W-IDFTG-B6                  PIC 9(2)    VALUE ZERO.                  
005500 77  WS-MSGI-IDFTG               PIC 9(2)    VALUE ZERO.                  
005600 77  WS-IDKONTO                  PIC 9(10)   VALUE ZERO.                  
005700 77  DAGENS-DATUM                PIC S9(7)   VALUE +0   COMP-3.           
005800                                                                          
005900 77  SW-TIGILTIG-FINNS           PIC X       VALUE 'N'.                   
006000     88 TIGILTIG-FINNS                       VALUE 'J'.                   
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-FEL                          VALUE 'N'.                   
006500                                                                          
006600 77  IDFTG-UPD-SW                PIC X       VALUE 'J'.                   
006700     88  IDFTG-UPD-OK                        VALUE 'J'.                   
006800     88  IDFTG-UPD-FEL                       VALUE 'N'.                   
006900                                                                          
007000 77  PERIOD-SW                   PIC X       VALUE 'N'.                   
007100     88  PERIOD-OK                           VALUE 'J'.                   
007200                                                                          
007300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007400     88  NYCKLAR-OK                          VALUE 'J'.                   
007500     88  NYCKLAR-FEL                         VALUE 'N'.                   
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007800     88  EGEN-MID                            VALUE '4702'.                
007900     88  GODK-MID                            VALUE '4702'.                
008000*    88  GODK-MID                            VALUE '4701' '4702'          
008100*                                                  '4703' '4704'          
008200*                                                  '4705' '4706'          
008300*                                                  '4707' '4708'          
008400*                                                  '4709'.                
008500     88  HELP-MID                            VALUE '0551'.                
008600                                                                          
008700 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
008800 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
008900 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
009000 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
009100                                                                          
009200     EJECT                                                                
009300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009400 01  GENERELLA-SUBPROGRAM.                                                
009500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010000*                                                                         
010100     03  W411SAP                 PIC X(8)    VALUE 'W411SAP '.            
010200*            SAP KONTROLL                                                 
010300     EJECT                                                                
010400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010500*01 -COPY WMEDAREA                                                        
010600     SKIP3                                                                
010700 01  MESSAGE-CODES.                                                       
010800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010900     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
011000     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
011100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011200     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
011300     03  ERR-END-VALUE           PIC X(3)    VALUE '240'.                 
011400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011900     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012400     SKIP3                                                                
012500*01 -COPY WMSGINIT                                                        
012600     EJECT                                                                
012700*                                                                         
012800*01  -COPY WDATAREA                                                       
012900     EJECT                                                                
013000 01  FILLER                      PIC  X(12) VALUE                         
013100                                               'SAP KONTROLL'.            
013200*                                                                         
013300*01 -COPY W411SAP                                                         
013400     EJECT                                                                
013500                                                                          
013600*01 -COPY WWIDFTG    -PRE TEST-                                           
013700                                                                          
013800     EJECT                                                                
013900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014000*                                                                         
014100*                                                                         
014200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014300     SKIP3                                                                
014400*01  MID -COPY W4I70201                                                   
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014700     SKIP3                                                                
014800*01  -COPY WMSGAREA                                                       
014900     EJECT                                                                
015000     03  MOD REDEFINES MSG-AREA.                                          
015100*      05  -COPY W4O70201                                                 
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015400*01  -COPY WMFSAREA                                                       
015500     EJECT                                                                
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
016100     03  W-WDGXKEY-ROT-X.                                                 
016200         05  FILLER              PIC X(04)    VALUE '4109'.               
016300         05  W-IDFTG-4109        PIC 9(2)     VALUE ZERO.                 
016400         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
016500                                                                          
016600     03  W-KEY4110-X.                                                     
016700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016800         05  W-KDANMORS          PIC X(2)    VALUE SPACE.                 
016900         05  W-DAGILTIG-FOM      PIC 9(8)    VALUE ZERO.                  
017000                                                                          
017100     03  W-KEY4110-MAX-X.                                                 
017200         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
017300         05  W-KDANMORS-MAX      PIC X(2)    VALUE SPACE.                 
017400         05  FILLER              PIC X(8)    VALUE HIGH-VALUE.            
017500                                                                          
017600     03  W-WDH301KY-X.                                                    
017700         05  W-IDKONTO           PIC S9(11)  VALUE ZERO COMP-3.           
017800         05  W-IDKST             PIC X(10)   VALUE SPACE.                 
017900         05  W-IDPROFIT          PIC X(10)   VALUE SPACE.                 
018000         05  W-IDANALYS          PIC X(12)   VALUE SPACE.                 
018100                                                                          
018200     03  W-IDARTNR-D6-X.                                                  
018300         05  W-IDARTNR-D6        PIC S9(9)   VALUE ZERO COMP-3.           
018301                                                                          
018400     EJECT                                                                
018500*    --- LEVANM-TABELL                                                    
018600 01  ANALYS-POST-TABELL.                                                  
018700    03   ANALYS-POST OCCURS 20.                                           
018800      05  TAB-TIGILTIG-FOM      PIC S9(7)  COMP-3.                        
018900      05  TAB-TIGILTIG-TOM      PIC S9(7)  COMP-3.                        
019000      05  TAB-KVART             PIC S9(7)  COMP-3.                        
019100     EJECT                                                                
019200                                                                          
019300*    --- STATUS-KOD FRÅN IMS                                              
019400 01  STATUS-WS                   PIC XX.                                  
019500     88  SEGMENT-FINNS                       VALUE '  '.                  
019600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019700     SKIP2                                                                
019800 01  GODK-STATUSKODER.                                                    
019900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020000     SKIP3                                                                
020100 01  SSA1                        PIC X(64).                               
020200 01  SSA2                        PIC X(64).                               
020300     EJECT                                                                
020400*    --- IMS FUNKTIONSKODER                                               
020500*01  -COPY W0003                                                          
020600     EJECT                                                                
020700*    ---  DLI INPUT-OUTPUT AREA                                           
020800 01  FILLER                      PIC X(20) VALUE                          
020900                                          'DLI-IO-AREA-WL410901'.         
021000 01  DLI-IO-AREA-WL410901.                                                
021100     03  WL410901.                                                        
021200*        05  -COPY WDGX4109                                               
021300     EJECT                                                                
021400 01  FILLER                      PIC X(20)  VALUE                         
021500                                          'DLI-IO-AREA-WL410911'.         
021600 01  DLI-IO-AREA-WL410911.                                                
021700     03  WL410911.                                                        
021800*        05  -COPY WDGX4110                                               
021900     EJECT                                                                
022600 01  FILLER                      PIC X(20)  VALUE                         
022700                                          'DLI-IO-AREA-WDK601'.           
022800 01  DLI-IO-AREA-WDK601.                                                  
022900     03  WLARTC01.                                                        
023000*        05  -COPY WDK601                                                 
023100     EJECT                                                                
023110 01  FILLER                      PIC X(20)  VALUE                         
023120                                          'DLI-IO-AREA-WDB601'.           
023130 01  DLI-IO-AREA-WDB601.                                                  
023140     03  WDB601.                                                          
023150*        05  -COPY WDB601                                                 
023160     EJECT                                                                
023200 LINKAGE SECTION.                                                         
023300                                                                          
023400*01  -COPY W0009  -PRE MSG-                                               
023500*01  -COPY W0008  -PRE 4109-                                              
023600     05  FILLER                  PIC X.                                   
023700*01  -COPY W0008  -PRE USEA-                                              
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024000*01  -COPY W0008  -PRE SAPC-                                              
024100     05  FILLER                  PIC X.                                   
024200     EJECT                                                                
024300*01  -COPY W0008  -PRE ARTC-                                              
024400     05  FILLER                  PIC X.                                   
024410*01  -COPY W0008  -PRE WDB6-                                              
024420     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600 PROCEDURE DIVISION  USING MSG-PCB 4109-PCB USEA-PCB                      
024700*                                  GRSA-PCB ARTC-PCB.                     
024800                                   SAPC-PCB ARTC-PCB                      
024810                                   WDB6-PCB.                              
024900     ENTRY 'DLITCBL' USING MSG-PCB 4109-PCB USEA-PCB                      
025000*                                  GRSA-PCB ARTC-PCB.                     
025100                                   SAPC-PCB ARTC-PCB                      
025110                                   WDB6-PCB.                              
025300     PERFORM IMS-GET-MSG                                                  
025400     IF SEGMENT-FINNS                                                     
025500       PERFORM A-INIT                                                     
025600       PERFORM B-KOLLA-NYCKLAR                                            
025700       IF NYCKLAR-OK                                                      
025800         IF MFS-UPDATE OR MFS-UPD-V                                       
025900           PERFORM G-KOLLA-INPUT                                          
026000           IF INDATA-OK                                                   
026100             PERFORM H-UPPDATERA                                          
026200           END-IF                                                         
026300         ELSE                                                             
026400           IF MFS-FIRST                                                   
026500             PERFORM C-FOERSTA-SIDA                                       
026600           ELSE                                                           
026700             IF MFS-NEXT                                                  
026800               PERFORM D-NAESTA-SIDA                                      
026900             ELSE                                                         
027000               PERFORM E-SAMMA-SIDA                                       
027100             END-IF                                                       
027200           END-IF                                                         
027300         END-IF                                                           
027400         IF INDATA-OK                                                     
027500           PERFORM F-LAES-VISA-INFO                                       
027600         END-IF                                                           
027700       END-IF                                                             
027800       PERFORM Z-FINIT                                                    
027900     END-IF                                                               
028000                                                                          
028100     MOVE ZERO TO RETURN-CODE                                             
028200     GOBACK                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 A-INIT SECTION.                                                          
028600                                                                          
028700     MOVE 'A-INIT'                    TO WS-SEKTION                       
028800     IF MSG-DUBBLA-TRANSKODER                                             
028900       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
029000                                      TO MID-W4I70201                     
029100       MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                      
029200       MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                     
029300     ELSE                                                                 
029400       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
029500                                      TO MID-W4I70201                     
029600       MOVE MSG-IDTRANS-1             TO MFS-IDTRANS                      
029700       MOVE MSG-KDMFSFOR-1            TO MFS-KDMFSFOR                     
029800     END-IF                                                               
029900                                                                          
030000     MOVE MSG-KDTRTYP                 TO MFS-KDTRTYP                      
030100     MOVE MSG-IDPFK                   TO MFS-IDPFK                        
030200     MOVE MFS-IDTRANS                 TO W-IDTRANS                        
030300                                                                          
030400     MOVE LOW-VALUE                   TO MSG-AREA                         
030500     MOVE 'W4O70201'                  TO MFS-IDMOD                        
030600     MOVE '4702'                      TO MOD-IDTRANS                      
030700     MOVE MFS-RENSA-FAELT             TO MOD-TEMFSFEL MOD-TEMFSINF        
030800     MOVE SPACE                       TO MED-IDMFSFEL MED-IDMFSINF        
030900                                                                          
031000     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O70201 + 4                  
031100                                                                          
031200     ACCEPT DAGENS-DATUM               FROM DATE                          
031300     MOVE +1                           TO IX                              
031400     PERFORM UNTIL IX > MAX-IX                                            
031500        MOVE +0                        TO TAB-TIGILTIG-FOM  (IX)          
031600                                          TAB-TIGILTIG-TOM  (IX)          
031700                                          TAB-KVART         (IX)          
031800        ADD +1                         TO IX                              
031900     END-PERFORM                                                          
032000                                                                          
032100     IF EGEN-MID OR HELP-MID                                              
032200       CONTINUE                                                           
032300     ELSE                                                                 
032400       MOVE SPACE                     TO MFS-KDTRTYP                      
032500       MOVE '7'                       TO MFS-IDPFK                        
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 B-KOLLA-NYCKLAR SECTION.                                                 
033000     MOVE 'B-KOLLA-NYCKLAR'           TO WS-SEKTION                       
033100                                                                          
033200     PERFORM BB-HAEMTA-USERINFO                                           
033300                                                                          
033400     IF MSGI-IDLAND-SPR = 'GB'                                            
033500       MOVE 'GB '                     TO MED-IDSKYLT                      
033600     ELSE                                                                 
033700       MOVE 'S  '                     TO MED-IDSKYLT                      
033800     END-IF                                                               
033900                                                                          
034000     MOVE JA                          TO NYCKLAR-SW                       
034100                                                                          
034200*    -- KONTROLL AV IDARTNR                                               
034300     MOVE MFS-RENSA-FAELT             TO MOD-IDARTNR-IN                   
034400                                                                          
034500     IF MID-IDARTNR-IN = ALL '+'                                          
034600       MOVE MID-IDARTNR-UT            TO WS-IDARTNR                       
034700       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
034800     ELSE                                                                 
034900       MOVE MID-IDARTNR-IN            TO WS-IDARTNR                       
035000       MOVE '7'                       TO MFS-IDPFK                        
035100       MOVE SPACE                     TO MFS-KDTRTYP                      
035200     END-IF                                                               
035300     IF WS-IDARTNR NUMERIC                                                
035400       MOVE WS-IDARTNR                TO W-IDARTNR                        
035500     ELSE                                                                 
035600       MOVE NEJ                       TO NYCKLAR-SW                       
035700     END-IF                                                               
035800                                                                          
035900*    -- KONTROLL AV IDFTG                                                 
036000     IF MID-IDFTG-IN = ALL '+'                                            
036100       MOVE MID-IDFTG-UT              TO WS-IDFTG                         
036200       INSPECT WS-IDFTG REPLACING LEADING SPACE BY ZERO                   
036210       IF WS-IDFTG NOT NUMERIC                                            
036220          MOVE ZERO                  TO WS-IDFTG                          
036230       END-IF                                                             
036300       IF WS-IDFTG = ZERO                                                 
036400          MOVE MSGI-IDFTG             TO WS-IDFTG                         
036500          MOVE WS-IDFTG               TO W-IDFTG-4109                     
036600       ELSE                                                               
036700          MOVE WS-IDFTG               TO TEST-WS-IDFTG                    
036800          IF TEST-IDFTG-US OR TEST-IDFTG-CA OR TEST-IDFTG-PV              
036900             MOVE WS-IDFTG            TO W-IDFTG-4109                     
037000          ELSE                                                            
037100             MOVE MSGI-IDFTG          TO TEST-WS-IDFTG                    
037200             IF TEST-IDFTG-NON-VCC                                        
037300               MOVE WS-IDFTG         TO TEST-WS-IDFTG                     
037400               IF TEST-IDFTG-NON-VCC                                      
037500                 MOVE MSGI-IDFTG    TO WS-IDFTG                           
037600                 MOVE WS-IDFTG      TO W-IDFTG-4109                       
037700               ELSE                                                       
037800                 MOVE NEJ           TO NYCKLAR-SW                         
037900               END-IF                                                     
038000             ELSE                                                         
039000               MOVE MSGI-IDFTG     TO WS-IDFTG                            
039100               MOVE WS-IDFTG       TO W-IDFTG-4109                        
039300             END-IF                                                       
039400          END-IF                                                          
039500       END-IF                                                             
039600     ELSE                                                                 
039610       IF MID-IDFTG-IN NUMERIC                                            
039700         MOVE MID-IDFTG-IN            TO WS-IDFTG                         
039800                                                                          
039900         MOVE MSGI-IDFTG              TO TEST-WS-IDFTG                    
040000         IF TEST-IDFTG-NON-VCC                                            
040100           MOVE WS-IDFTG              TO TEST-WS-IDFTG                    
040200           IF TEST-IDFTG-NON-VCC                                          
040300             MOVE WS-IDFTG            TO W-IDFTG-4109                     
040400             MOVE '7'                 TO MFS-IDPFK                        
040500             MOVE SPACE               TO MFS-KDTRTYP                      
040600           ELSE                                                           
040700             MOVE NEJ                 TO NYCKLAR-SW                       
040800           END-IF                                                         
040900         ELSE                                                             
042000           MOVE WS-IDFTG              TO TEST-WS-IDFTG                    
042100           IF TEST-IDFTG-US OR TEST-IDFTG-CA OR TEST-IDFTG-PV             
042200             MOVE WS-IDFTG            TO W-IDFTG-4109                     
042300             MOVE '7'                 TO MFS-IDPFK                        
042400             MOVE SPACE               TO MFS-KDTRTYP                      
042500           ELSE                                                           
042600             MOVE NEJ                 TO NYCKLAR-SW                       
042800           END-IF                                                         
042900         END-IF                                                           
043000       ELSE                                                               
043001         MOVE NEJ                     TO NYCKLAR-SW                       
043010     END-IF                                                               
043100                                                                          
043200     IF GODK-MID OR NYCKLAR-OK                                            
043300       MOVE WS-IDARTNR                TO MOD-IDARTNR-UT                   
043400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
043500       MOVE WS-IDFTG                  TO MOD-IDFTG-UT                     
043600     ELSE                                                                 
043700       MOVE MFS-RENSA-FAELT           TO MOD-IDARTNR-UT                   
043800       MOVE MFS-RENSA-FAELT           TO MOD-IDFTG-UT                     
043900     END-IF                                                               
044000                                                                          
044100     IF NYCKLAR-FEL                                                       
044200       MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                     
044300       PERFORM MFS-RENSA-FAELT-IN                                         
044400       PERFORM MFS-RENSA-FAELT-UT                                         
044500     ELSE                                                                 
044600       PERFORM BA-KOLLA-ARTREG                                            
044700     END-IF                                                               
044800                                                                          
044900     .                                                                    
045000     EJECT                                                                
045100 BA-KOLLA-ARTREG SECTION.                                                 
045200     MOVE 'BA-KOLLA-ARTREG'            TO WS-SEKTION                      
045300                                                                          
045400     IF W-IDARTNR > ZERO                                                  
045500       MOVE W-IDARTNR                 TO W-IDARTNR-D6                     
045600       PERFORM IMS-GET-ARTC01                                             
045700       IF SEGMENT-SAKNAS                                                  
045800         MOVE NEJ                     TO NYCKLAR-SW                       
045900         MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                     
046000         PERFORM MFS-RENSA-FAELT-IN                                       
046100         PERFORM MFS-RENSA-FAELT-UT                                       
046200       END-IF                                                             
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 BB-HAEMTA-USERINFO SECTION.                                              
046700     MOVE 'BB-HAEMTA-USERINFO'         TO WS-SEKTION                      
046800                                                                          
046900     MOVE ALL '+'                     TO MSGI-WMSGINIT                    
047000     MOVE '001'                       TO MSGI-KDCALL                      
047100     MOVE MSG-SIGNON-USERID           TO MSGI-IDUSER                      
047200     MOVE '4702'                      TO MSGI-IDTRANS                     
047300     MOVE MSG-LTERM-NAME              TO MSGI-IDLTERM-USER                
047400                                                                          
047500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047600                                                                          
047700     .                                                                    
047800     EJECT                                                                
047900 C-FOERSTA-SIDA SECTION.                                                  
048000     MOVE 'C-FOERSTA-SIDA'            TO WS-SEKTION                       
048100                                                                          
048200     MOVE INF-FIRST-PAGE              TO MED-IDMFSINF                     
048300     MOVE LOW-VALUE                   TO W-KDANMORS                       
048400                                                                          
048500     MOVE ZERO                        TO W-DAGILTIG-FOM                   
048600     PERFORM MFS-RENSA-FAELT-IN                                           
048700     .                                                                    
048800     EJECT                                                                
048900 D-NAESTA-SIDA SECTION.                                                   
049000     MOVE 'C-NAESTA-SIDA'             TO WS-SEKTION                       
049100                                                                          
049200     IF MID-IDARTNR-NEXT  NUMERIC                                         
049300       MOVE MID-IDARTNR-NEXT          TO W-IDARTNR                        
049400     ELSE                                                                 
049500       MOVE ZERO                      TO W-IDARTNR                        
049600     END-IF                                                               
049700     IF MID-KDANMORS-NEXT  NUMERIC                                        
049800        MOVE MID-KDANMORS-NEXT        TO W-KDANMORS                       
049900     ELSE                                                                 
050000        MOVE LOW-VALUE                TO W-KDANMORS                       
050100     END-IF                                                               
050200     IF MID-TIGILTIG-FOM-NEXT NUMERIC                                     
050300       MOVE MID-TIGILTIG-FOM-NEXT     TO W-DAGILTIG-FOM                   
050400       IF MID-TIGILTIG-FOM-NEXT NOT = ZERO                                
050500         IF MID-TIGILTIG-FOM-NEXT < 500000                                
050600           MOVE 20                    TO W-DAGILTIG-FOM (1:2)             
050700         ELSE                                                             
050800           IF MID-TIGILTIG-FOM-NEXT < 999999                              
050900             MOVE 19                  TO W-DAGILTIG-FOM (1:2)             
051000           ELSE                                                           
051100             MOVE 99999999            TO W-DAGILTIG-FOM                   
051200           END-IF                                                         
051300         END-IF                                                           
051400       END-IF                                                             
051500     ELSE                                                                 
051600       MOVE ZERO                      TO W-DAGILTIG-FOM                   
051700     END-IF                                                               
051800     PERFORM MFS-RENSA-FAELT-IN                                           
051900     .                                                                    
052000     EJECT                                                                
052100 E-SAMMA-SIDA SECTION.                                                    
052200     MOVE 'E-SAMMA-SIDA'             TO WS-SEKTION                        
052300                                                                          
052400     IF EGEN-MID OR HELP-MID                                              
052500       IF MID-IDARTNR-ENTER NUMERIC                                       
052600         MOVE MID-IDARTNR-ENTER       TO W-IDARTNR                        
052700       ELSE                                                               
052800         MOVE ZERO                    TO W-IDARTNR                        
052900       END-IF                                                             
053000       IF MID-KDANMORS-ENTER NUMERIC                                      
053100          MOVE MID-KDANMORS-ENTER     TO W-KDANMORS                       
053200       ELSE                                                               
053300          MOVE LOW-VALUE              TO W-KDANMORS                       
053400       END-IF                                                             
053500                                                                          
053600       IF MID-TIGILTIG-FOM-ENTER NUMERIC                                  
053700         MOVE MID-TIGILTIG-FOM-ENTER  TO W-DAGILTIG-FOM                   
053800         IF MID-TIGILTIG-FOM-ENTER NOT = ZERO                             
053900           IF MID-TIGILTIG-FOM-ENTER < 500000                             
054000             MOVE 20                  TO W-DAGILTIG-FOM (1:2)             
054100           ELSE                                                           
054200             IF MID-TIGILTIG-FOM-ENTER < 999999                           
054300               MOVE 19                TO W-DAGILTIG-FOM (1:2)             
054400             ELSE                                                         
054500               MOVE 99999999          TO W-DAGILTIG-FOM                   
054600             END-IF                                                       
054700           END-IF                                                         
054800         END-IF                                                           
054900       ELSE                                                               
055000         MOVE ZERO                    TO W-DAGILTIG-FOM                   
055100       END-IF                                                             
055200       IF MID-INPUT = ALL '+'                                             
055300         PERFORM MFS-RENSA-FAELT-IN                                       
055400       ELSE                                                               
055500         MOVE INF-PRESS-PF11          TO MED-IDMFSINF                     
055600         PERFORM EA-MID-INDATA-TILL-MOD                                   
055700       END-IF                                                             
055800     ELSE                                                                 
055900       PERFORM MFS-RENSA-FAELT-IN                                         
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 EA-MID-INDATA-TILL-MOD SECTION.                                          
056400     MOVE 'EA-MID-INDATA-TILL-MOD'    TO WS-SEKTION                       
056500                                                                          
056600     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
056700       MOVE MID-IDARTNR-UPP           TO MOD-IDARTNR-UPP                  
056800       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDARTNR-UPP-ATTR             
056900     ELSE                                                                 
057000       MOVE MFS-RENSA-FAELT           TO MOD-IDARTNR-UPP                  
057100     END-IF                                                               
057200     IF MID-TIGILTIG-FOM-UPP NOT = ALL '+'                                
057300       MOVE MID-TIGILTIG-FOM-UPP      TO MOD-TIGILTIG-FOM-UPP             
057400       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-TIGILTIG-FOM-UPP-ATTR        
057500     ELSE                                                                 
057600       MOVE MFS-RENSA-FAELT           TO MOD-TIGILTIG-FOM-UPP             
057700     END-IF                                                               
057800     IF MID-TIGILTIG-TOM-UPP NOT = ALL '+'                                
057900       MOVE MID-TIGILTIG-TOM-UPP      TO MOD-TIGILTIG-TOM-UPP             
058000       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-TIGILTIG-TOM-UPP-ATTR        
058100     ELSE                                                                 
058200       MOVE MFS-RENSA-FAELT           TO MOD-TIGILTIG-TOM-UPP             
058300     END-IF                                                               
058400     IF MID-IDFTG-UPP NOT = ALL '+'                                       
058500       MOVE MID-IDFTG-UPP             TO MOD-IDFTG-UPP                    
058600       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDFTG-UPP-ATTR               
058700     ELSE                                                                 
058800       MOVE MFS-RENSA-FAELT           TO MOD-IDFTG-UPP                    
058900     END-IF                                                               
059000     IF MID-KDANMORS-UPP NOT = ALL '+'                                    
059100       MOVE MID-KDANMORS-UPP          TO MOD-KDANMORS-UPP                 
059200       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-KDANMORS-UPP-ATTR            
059300     ELSE                                                                 
059400       MOVE MFS-RENSA-FAELT           TO MOD-KDANMORS-UPP                 
059500     END-IF                                                               
059600     IF MID-IDANALYS-UPP NOT = ALL '+'                                    
059700       MOVE MID-IDANALYS-UPP        TO MOD-IDANALYS-UPP                   
059800       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDANALYS-UPP-ATTR            
059900     ELSE                                                                 
060000       MOVE MFS-RENSA-FAELT           TO MOD-IDANALYS-UPP                 
060100     END-IF                                                               
060200     IF MID-IDKONTO-UPP NOT = ALL '+'                                     
060300       MOVE MID-IDKONTO-UPP           TO MOD-IDKONTO-UPP                  
060400       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDKONTO-UPP-ATTR             
060500     ELSE                                                                 
060600       MOVE MFS-RENSA-FAELT           TO MOD-IDKONTO-UPP                  
060700     END-IF                                                               
060800     IF MID-IDKST-UPP NOT = ALL '+'                                       
060900       MOVE MID-IDKST-UPP             TO MOD-IDKST-UPP                    
061000       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDKST-UPP-ATTR               
061100     ELSE                                                                 
061200       MOVE MFS-RENSA-FAELT           TO MOD-IDKST-UPP                    
061300     END-IF                                                               
061400     IF MID-IDUSER-UPP NOT = ALL '+'                                      
061500       MOVE MID-IDUSER-UPP            TO MOD-IDUSER-UPP                   
061600       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDUSER-UPP-ATTR              
061700     ELSE                                                                 
061800       MOVE MFS-RENSA-FAELT           TO MOD-IDUSER-UPP                   
061900     END-IF                                                               
062000     IF MID-FLBORT-UPP NOT = '+'                                          
062100       MOVE MID-FLBORT-UPP            TO MOD-FLBORT-UPP                   
062200       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLBORT-UPP-ATTR              
062300     ELSE                                                                 
062400       MOVE MFS-RENSA-FAELT           TO MOD-FLBORT-UPP                   
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 F-LAES-VISA-INFO SECTION.                                                
062900     MOVE 'F-LAES-VISA-INFO'          TO WS-SEKTION                       
063000                                                                          
063100     PERFORM IMS-GET-410901                                               
063110     IF SEGMENT-SAKNAS                                                    
063120        MOVE ERR-KEY-MISSING          TO MED-IDMFSFEL                     
063130        PERFORM MFS-RENSA-FAELT-UT                                        
063200     ELSE                                                                 
063300       PERFORM IMS-GNP-410911                                             
063400       IF SEGMENT-SAKNAS                                                  
063500          MOVE ERR-KEY-MISSING          TO MED-IDMFSFEL                   
063600          PERFORM MFS-RENSA-FAELT-UT                                      
063700       ELSE                                                               
063800         PERFORM FA-REDIGERA-RAD-1                                        
063900         MOVE 4110-IDARTNR              TO MOD-IDARTNR-ENTER              
064000         MOVE 4110-DAGILTIG-FOM (3:6)   TO MOD-TIGILTIG-FOM-ENTER         
064100         MOVE 4110-KDANMORS             TO MOD-KDANMORS-ENTER             
064200                                                                          
064300         PERFORM IMS-GNP-410911                                           
064400         MOVE +1                        TO INDX                           
064500         PERFORM UNTIL INDX > MAX-INDX                                    
064600           IF SEGMENT-FINNS                                               
064700             PERFORM FB-REDIGERA-RAD-2-11                                 
064800             PERFORM IMS-GNP-410911                                       
064900           ELSE                                                           
065000             PERFORM MFS-RENSA-RAD                                        
065100           END-IF                                                         
065200           ADD 1 TO INDX                                                  
065300         END-PERFORM                                                      
065400                                                                          
065500         IF SEGMENT-FINNS                                                 
065600           MOVE 4110-IDARTNR            TO MOD-IDARTNR-NEXT               
065700           MOVE 4110-DAGILTIG-FOM (3:6) TO MOD-TIGILTIG-FOM-NEXT          
065800           MOVE 4110-KDANMORS           TO MOD-KDANMORS-NEXT              
065900           MOVE INF-MORE-INFO-EXISTS    TO MED-IDMFSINF                   
066000         ELSE                                                             
066100           MOVE ZERO                    TO MOD-IDARTNR-NEXT               
066200           MOVE ZERO                    TO MOD-TIGILTIG-FOM-NEXT          
066300           MOVE ZERO                    TO MOD-KDANMORS-NEXT              
066400           IF MED-IDMFSINF = SPACE                                        
066500             MOVE INF-LAST-PAGE         TO MED-IDMFSINF                   
066600           END-IF                                                         
066700         END-IF                                                           
066800       END-IF                                                             
066810     END-IF                                                               
066900     .                                                                    
067000     EJECT                                                                
067100 FA-REDIGERA-RAD-1 SECTION.                                               
067200     MOVE 'FA-REDIGERA-RAD-1'          TO WS-SEKTION                      
067300                                                                          
067400     MOVE 4110-IDARTNR                TO MOD-IDARTNR-RAD1                 
067500     MOVE 4110-DAGILTIG-FOM (3:6)     TO MOD-TIGILTIG-FOM-RAD1            
067600     MOVE 4110-DAGILTIG-TOM (3:6)     TO MOD-TIGILTIG-TOM-RAD1            
067700     MOVE WS-IDFTG                    TO MOD-IDFTG-RAD1                   
067800     MOVE 4110-IDANALYS               TO MOD-IDANALYS-RAD1                
067900     MOVE 4110-KDANMORS               TO MOD-KDANMORS-RAD1                
068000     MOVE 4110-IDKONTO                TO MOD-IDKONTO-RAD1                 
068100     MOVE 4110-IDKST                  TO MOD-IDKST-RAD1                   
068200     MOVE 4110-IDUSER                 TO MOD-IDUSER-RAD1                  
068300     MOVE 4110-KVART                  TO MOD-KVART-RAD1                   
068400                                                                          
068500     IF MFS-UPDATE AND MID-FLBORT-UPP = '+'                               
068600       MOVE MFS-ADD-LYS-UPP-FAELT     TO MOD-IDARTNR-ATTR                 
068700                                         MOD-TIGILTIG-FOM-ATTR            
068800                                         MOD-TIGILTIG-TOM-ATTR            
068900                                         MOD-KDANMORS-ATTR                
069000                                         MOD-IDFTG-ATTR                   
069100                                         MOD-IDANALYS-ATTR                
069200                                         MOD-IDKONTO-ATTR                 
069300                                         MOD-IDKST-ATTR                   
069400                                         MOD-IDUSER-ATTR                  
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 FB-REDIGERA-RAD-2-11 SECTION.                                            
069900     MOVE 'FB-REDIGERA-RAD-2-11'       TO WS-SEKTION                      
070000                                                                          
070100     MOVE 4110-IDARTNR                TO MOD-IDARTNR (INDX)               
070200     MOVE 4110-DAGILTIG-FOM (3:6)     TO MOD-TIGILTIG-FOM (INDX)          
070300     MOVE 4110-DAGILTIG-TOM (3:6)     TO MOD-TIGILTIG-TOM (INDX)          
070400     MOVE WS-IDFTG                    TO MOD-IDFTG (INDX)                 
070500     MOVE 4110-KDANMORS               TO MOD-KDANMORS   (INDX)            
070600     MOVE 4110-IDKONTO                TO MOD-IDKONTO    (INDX)            
070700     MOVE 4110-IDKST                  TO MOD-IDKST      (INDX)            
070800     MOVE 4110-IDANALYS               TO MOD-IDANALYS   (INDX)            
070900     MOVE 4110-IDUSER                 TO MOD-IDUSER (INDX)                
071000     MOVE 4110-KVART                  TO MOD-KVART (INDX)                 
071100     .                                                                    
071200     EJECT                                                                
071300 G-KOLLA-INPUT SECTION.                                                   
071400     MOVE 'G-KOLLA-INPUT'              TO WS-SEKTION                      
071500                                                                          
071600     MOVE JA                          TO INDATA-SW                        
071700     MOVE JA                          TO IDFTG-UPD-SW                     
071800     IF MID-INPUT = ALL '+'                                               
071900       MOVE ERR-PF11-AND-NO-DATA      TO MED-IDMFSFEL                     
072000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
072100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
072200       MOVE NEJ                       TO INDATA-SW                        
072300     ELSE                                                                 
072400       PERFORM GA-UPP-RAD-FORMELL-KOLL                                    
072500                                                                          
072600       IF INDATA-FEL                                                      
072700         IF MED-IDMFSFEL = SPACE                                          
072800           IF IDFTG-UPD-FEL                                               
072900             MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                  
073000           ELSE                                                           
073100             MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                  
073200           END-IF                                                         
073300         END-IF                                                           
073400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
073500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
073600       ELSE                                                               
073700         IF MID-FLBORT-UPP = 'J' OR 'Y'                                   
073800           PERFORM GB-KOLL-OM-BORTTAG-OK                                  
073900         ELSE                                                             
074000           PERFORM GC-UPP-RAD-LOGISK-KOLL                                 
074100         END-IF                                                           
074200         IF INDATA-FEL                                                    
074300           IF MED-IDMFSFEL = SPACE                                        
074400             IF IDFTG-UPD-FEL                                             
074500               MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                
074600             ELSE                                                         
074700               MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                
074800             END-IF                                                       
074900           END-IF                                                         
075000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
075100           PERFORM MFS-ROER-EJ-FAELT-IN                                   
075200         END-IF                                                           
075300       END-IF                                                             
075400     END-IF                                                               
075500     .                                                                    
075600     EJECT                                                                
075700 GA-UPP-RAD-FORMELL-KOLL SECTION.                                         
075800     MOVE 'GA-UPP-RAD-FORMELL-KOLL'     TO WS-SEKTION                     
075900                                                                          
076000     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
076100       IF MID-IDARTNR-UPP NUMERIC AND                                     
076200          MID-IDARTNR-UPP > ZERO                                          
076300         MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDARTNR-UPP-ATTR             
076400                                                                          
076500       ELSE                                                               
076600         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDARTNR-UPP-ATTR             
076700         MOVE NEJ                     TO INDATA-SW                        
076800       END-IF                                                             
076900     ELSE                                                                 
077000       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDARTNR-UPP-ATTR             
077100       MOVE NEJ                       TO INDATA-SW                        
077200     END-IF                                                               
077300                                                                          
077400     IF MID-TIGILTIG-FOM-UPP NOT = ALL '+'                                
077500       IF MID-TIGILTIG-FOM-UPP NUMERIC AND                                
077600          MID-TIGILTIG-FOM-UPP > ZERO                                     
077700         PERFORM GAA-KOLLA-FOM-TID                                        
077800       ELSE                                                               
077900         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIGILTIG-FOM-UPP-ATTR        
078000         MOVE NEJ                     TO INDATA-SW                        
078100       END-IF                                                             
078200     ELSE                                                                 
078300       MOVE ZERO                      TO MID-TIGILTIG-FOM-UPP             
078400       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-FOM-UPP-ATTR        
078500       MOVE NEJ                       TO INDATA-SW                        
078600     END-IF                                                               
078700                                                                          
078800     IF MID-TIGILTIG-TOM-UPP NOT = ALL '+'                                
078900       IF MID-TIGILTIG-TOM-UPP NUMERIC AND                                
079000          MID-TIGILTIG-TOM-UPP > ZERO                                     
079100         PERFORM GAB-KOLLA-TOM-TID                                        
079200       ELSE                                                               
079300         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIGILTIG-TOM-UPP-ATTR        
079400         MOVE NEJ                     TO INDATA-SW                        
079500       END-IF                                                             
079600     ELSE                                                                 
079700       MOVE ZERO                      TO MID-TIGILTIG-TOM-UPP             
079800       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-TOM-UPP-ATTR        
079900       MOVE NEJ                       TO INDATA-SW                        
080000     END-IF                                                               
080100                                                                          
080200                                                                          
080300     IF MID-IDFTG-UPP NOT = ALL '+'                                       
080400                                                                          
080500       IF MFS-UPD-V                                                       
080600                                                                          
080700         IF MID-IDFTG-UPP NUMERIC AND                                     
080800            MID-IDFTG-UPP > ZERO AND                                      
080900            MID-IDFTG-UPP = WS-IDFTG                                      
081000                                                                          
081100           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDFTG-UPP-ATTR               
081200         ELSE                                                             
081300           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDFTG-UPP-ATTR               
081400           MOVE NEJ                   TO INDATA-SW                        
081500           MOVE NEJ                   TO IDFTG-UPD-SW                     
081600         END-IF                                                           
081700       ELSE                                                               
081800                                                                          
081900         IF MID-IDFTG-UPP NUMERIC AND                                     
082000            MID-IDFTG-UPP > ZERO AND                                      
082100            MID-IDFTG-UPP = MSGI-IDFTG                                    
082200                                                                          
082300           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDFTG-UPP-ATTR               
082400         ELSE                                                             
082500           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDFTG-UPP-ATTR               
082600           MOVE NEJ                   TO INDATA-SW                        
082700           MOVE NEJ                   TO IDFTG-UPD-SW                     
082800         END-IF                                                           
082900       END-IF                                                             
083000     ELSE                                                                 
083100       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDFTG-UPP-ATTR               
083200       MOVE NEJ                       TO INDATA-SW                        
083300     END-IF                                                               
083400                                                                          
083500     IF MID-KDANMORS-UPP NOT = ALL '+'                                    
083600       IF MID-KDANMORS-UPP NUMERIC                                        
083700         MOVE MFS-NUM-FAELT-RAETT     TO MOD-KDANMORS-UPP-ATTR            
083800       ELSE                                                               
083900         MOVE MFS-NUM-FAELT-FEL       TO MOD-KDANMORS-UPP-ATTR            
084000         MOVE NEJ                     TO INDATA-SW                        
084100       END-IF                                                             
084200     ELSE                                                                 
084300       MOVE MFS-NUM-FAELT-FEL         TO MOD-KDANMORS-UPP-ATTR            
084400       MOVE NEJ                       TO INDATA-SW                        
084500     END-IF                                                               
084600                                                                          
084700     MOVE ZERO                      TO W-IDKONTO                          
084800     MOVE SPACE                     TO W-IDPROFIT                         
084900                                       W-IDANALYS                         
085000                                       W-IDKST                            
085100                                                                          
085200     IF MID-IDANALYS-UPP NOT = ALL '+'                                    
085300       IF MID-IDANALYS-UPP NUMERIC AND                                    
085400          MID-IDANALYS-UPP > ZERO                                         
085500          MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDANALYS-UPP-ATTR            
085600          MOVE MID-IDANALYS-UPP       TO W-IDANALYS                       
085700       ELSE                                                               
085800         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDANALYS-UPP-ATTR            
085900         MOVE NEJ                     TO INDATA-SW                        
086000         MOVE SPACE                   TO W-IDANALYS                       
086100       END-IF                                                             
086200     ELSE                                                                 
086300       MOVE SPACE                     TO W-IDANALYS                       
086400       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDANALYS-UPP-ATTR            
086500       MOVE NEJ                       TO INDATA-SW                        
086600     END-IF                                                               
086700                                                                          
086800     MOVE WS-IDFTG  TO TEST-WS-IDFTG                                      
086900                                                                          
087000     IF (MID-KDANMORS-UPP = '53' AND TEST-IDFTG-PV)                       
087100     OR (MID-KDANMORS-UPP = '55' AND TEST-IDFTG-PV)                       
087200     OR (MID-KDANMORS-UPP = '53' AND TEST-IDFTG-NON-VCC)                  
087470        IF MID-IDKONTO-UPP NOT = ALL '+'                                  
087500          IF MID-IDKONTO-UPP NUMERIC AND                                  
087600             MID-IDKONTO-UPP > ZERO                                       
087700             MOVE MID-IDKONTO-UPP     TO W-IDKONTO                        
087800             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-UPP-ATTR             
087900          ELSE                                                            
088000             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKONTO-UPP-ATTR            
088100             MOVE NEJ                  TO INDATA-SW                       
088200          END-IF                                                          
088300        ELSE                                                              
088400           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKONTO-UPP-ATTR            
088500           MOVE NEJ                    TO INDATA-SW                       
088600        END-IF                                                            
088700        IF MID-IDKST-UPP NOT = ALL '+'                                    
089000           MOVE MID-IDKST-UPP       TO W-IDKST                            
089100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKST-UPP-ATTR                 
089600        ELSE                                                              
089700           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKST-UPP-ATTR               
089800           MOVE NEJ                   TO INDATA-SW                        
089900        END-IF                                                            
090000******************************************                                
090100**** VALIDERAR SAP R3 MODULEN W411SAP ****                                
090200******************************************                                
090300        IF INDATA-OK                                                      
090500*          IF TEST-IDFTG-PV                                               
090600*            MOVE 'SEPV'           TO SAP-KDTRADP                         
090700*          ELSE                                                           
090800*            IF TEST-IDFTG-CN                                             
090900*               MOVE 'CN05'        TO SAP-KDTRADP                         
091000*            ELSE                                                         
091100*              IF TEST-IDFTG-IN                                           
091200*                 MOVE 'IN07'      TO SAP-KDTRADP                         
091300*              ELSE                                                       
091400*                 MOVE 'SEPV'      TO SAP-KDTRADP                         
091500*              END-IF                                                     
091600*            END-IF                                                       
091700*          END-IF                                                         
091701           MOVE MSGI-IDFTG TO TEST-WS-IDFTG                               
091702           IF TEST-IDFTG-NON-VCC                                          
091710             MOVE MSGI-IDFTG       TO W-IDFTG-B6                          
091720             PERFORM IMS-GU-WDB601-FTG                                    
091730             IF SEGMENT-FINNS                                             
091731               MOVE DCS-KDTRADP    TO SAP-KDTRADP                         
091740             END-IF                                                       
091750           ELSE                                                           
091751             MOVE 'SEPV'           TO SAP-KDTRADP                         
091760           END-IF                                                         
091800           MOVE W-IDKST            TO SAP-IDKST                           
091900           MOVE W-IDKONTO          TO SAP-IDKONTO                         
092000           MOVE W-IDANALYS         TO SAP-IDANALYS                        
092100           MOVE ZERO               TO SAP-IDDISTR                         
092200           MOVE MID-IDFTG-UPP      TO SAP-IDFTG                           
092300           MOVE SPACE              TO SAP-IDPROFIT                        
092400           MOVE SPACE              TO SAP-KDFAKTYP                        
092500           MOVE +2                 TO SAP-KDCALL                          
092600                                                                          
092700           CALL W411SAP USING SAP-W411SAP  SAPC-PCB                       
092800           IF SAP-BEFEL NOT = SPACE                                       
092900              IF SAP-IDFTG-OK = NEJ                                       
093000                 MOVE NEJ                TO INDATA-SW                     
093100              END-IF                                                      
093200                                                                          
093300              IF SAP-IDKST-OK = NEJ                                       
093400                IF SAP-BEFEL = 'COSTCENTER NOT ALLOWED'                   
093500* KST FÅR EJ ANGES OM KONTOTS FLAGGA FÖR KST ÄR AVSLAGEN.                 
093600* SE I PGM W411SAP. ÄNDRING BEGÄRD AV TUULA 030422. DET BETYDER           
093700* ATT VISSA KONTON + KOD 53 BARA HAR ANALYSNR. (T.EX. 481312 )            
093800                  MOVE SPACE             TO MID-IDKST-UPP                 
093900                  MOVE SPACE             TO SAP-BEFEL                     
094000                ELSE                                                      
094100                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDKST-UPP-ATTR            
094200                  MOVE NEJ               TO INDATA-SW                     
094300                END-IF                                                    
094400              END-IF                                                      
094500                                                                          
094600              IF SAP-IDKONTO-OK = NEJ                                     
094700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-UPP-ATTR           
094800                 MOVE NEJ               TO INDATA-SW                      
094900              END-IF                                                      
095000                                                                          
095100              IF SAP-IDANALYS-OK = NEJ                                    
095200                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDANALYS-UPP-ATTR          
095300                 MOVE NEJ               TO INDATA-SW                      
095400              END-IF                                                      
095500              MOVE SAP-BEFEL TO MOD-TEMFSINF                              
095600           END-IF                                                         
095700        END-IF                                                            
095800     ELSE                                                                 
095900        IF MID-KDANMORS-UPP = '52'                                        
096000        OR MID-KDANMORS-UPP = '53'                                        
096100        OR MID-KDANMORS-UPP = '54'                                        
096200        OR MID-KDANMORS-UPP = '55'                                        
096300           MOVE MFS-RENSA-FAELT         TO MOD-IDKONTO-UPP                
096400                                           MOD-IDKST-UPP                  
096500        ELSE                                                              
096600           MOVE MFS-NUM-FAELT-FEL       TO MOD-KDANMORS-UPP-ATTR          
096700           MOVE NEJ                     TO INDATA-SW                      
096800        END-IF                                                            
096900     END-IF                                                               
097000                                                                          
097100                                                                          
097200     IF MID-FLBORT-UPP = '+'                                              
097300       IF MID-IDUSER-UPP NOT = ALL '+'                                    
097400         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDUSER-UPP-ATTR              
097500       ELSE                                                               
097600         MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDUSER-UPP-ATTR              
097700         MOVE NEJ                     TO INDATA-SW                        
097800       END-IF                                                             
097900     END-IF                                                               
098000                                                                          
098100     IF MID-FLBORT-UPP = '+' OR 'J' OR 'Y'                                
098200       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLBORT-UPP-ATTR              
098300     ELSE                                                                 
098400       MOVE MFS-ALFA-FAELT-FEL        TO MOD-FLBORT-UPP-ATTR              
098500       MOVE NEJ                       TO INDATA-SW                        
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 GAA-KOLLA-FOM-TID SECTION.                                               
099000     MOVE 'GAA-KOLLA-FOM-TID'           TO WS-SEKTION                     
099100                                                                          
099200     MOVE 'AAMMDD'                    TO DAT-KDDATFORM                    
099300     MOVE MID-TIGILTIG-FOM-UPP        TO DAT-I-TIDATUM                    
099400                                                                          
099500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
099600                         DAT-O-TIDATUM DAT-KDSVAR                         
099700                                                                          
099800     IF DAT-KDSVAR-OK                                                     
099900       MOVE MFS-NUM-FAELT-RAETT       TO MOD-TIGILTIG-FOM-UPP-ATTR        
100000     ELSE                                                                 
100100       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-FOM-UPP-ATTR        
100200       MOVE NEJ                       TO INDATA-SW                        
100300     END-IF                                                               
100400     .                                                                    
100500     EJECT                                                                
100600 GAB-KOLLA-TOM-TID SECTION.                                               
100700     MOVE 'GAB-KOLLA-TOM-TID'           TO WS-SEKTION                     
100800                                                                          
100900     MOVE 'AAMMDD'                    TO DAT-KDDATFORM                    
101000     MOVE MID-TIGILTIG-TOM-UPP        TO DAT-I-TIDATUM                    
101100                                                                          
101200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
101300                         DAT-O-TIDATUM DAT-KDSVAR                         
101400                                                                          
101500     IF DAT-KDSVAR-OK                                                     
101600       MOVE MID-TIGILTIG-TOM-UPP      TO TMP1-YYMMDD                      
101700       MOVE MID-TIGILTIG-FOM-UPP      TO TMP2-YYMMDD                      
101800       PERFORM WY2000Q1                                                   
101900       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
102000         MOVE ERR-END-VALUE           TO MED-IDMFSFEL                     
102100         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIGILTIG-TOM-UPP-ATTR        
102200         MOVE NEJ                     TO INDATA-SW                        
102300       ELSE                                                               
102400         MOVE MFS-NUM-FAELT-RAETT     TO MOD-TIGILTIG-TOM-UPP-ATTR        
102500       END-IF                                                             
102600     ELSE                                                                 
102700       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-TOM-UPP-ATTR        
102800       MOVE NEJ                       TO INDATA-SW                        
102900     END-IF                                                               
103000     .                                                                    
103100     EJECT                                                                
103200 GB-KOLL-OM-BORTTAG-OK SECTION.                                           
103300     MOVE 'GB-KOLLA-OM-BORTTAG-OK'    TO WS-SEKTION                       
103400                                                                          
103500     PERFORM IMS-GET-410901                                               
103600     MOVE MID-IDARTNR-UPP             TO W-IDARTNR                        
103700     MOVE MID-KDANMORS-UPP            TO W-KDANMORS                       
103800     MOVE MID-TIGILTIG-FOM-UPP        TO W-DAGILTIG-FOM                   
103900     IF MID-TIGILTIG-FOM-UPP NOT = ZERO                                   
104000       IF MID-TIGILTIG-FOM-UPP < 500000                                   
104100         MOVE 20                     TO W-DAGILTIG-FOM (1:2)              
104200       ELSE                                                               
104300         IF MID-TIGILTIG-FOM-UPP < 999999                                 
104400           MOVE 19                   TO W-DAGILTIG-FOM (1:2)              
104500         ELSE                                                             
104600           MOVE 99999999             TO W-DAGILTIG-FOM                    
104700         END-IF                                                           
104800       END-IF                                                             
104900     END-IF                                                               
105000     PERFORM IMS-GHNP-410911                                              
105100     IF SEGMENT-FINNS                                                     
105200       IF 4110-KVART > +0                                                 
105300         MOVE MFS-NUM-FAELT-FEL       TO MOD-FLBORT-UPP-ATTR              
105400         MOVE NEJ                     TO INDATA-SW                        
105500       ELSE                                                               
105600         IF MID-IDANALYS-UPP NOT = 4110-IDANALYS                          
105700           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDANALYS-UPP-ATTR            
105800           MOVE NEJ                   TO INDATA-SW                        
105900         ELSE                                                             
106000           MOVE WS-IDFTG   TO TEST-WS-IDFTG                               
106100                                                                          
106200           IF (MID-KDANMORS-UPP = '53' AND TEST-IDFTG-PV)                 
106300           OR (MID-KDANMORS-UPP = '55' AND TEST-IDFTG-PV)                 
106400           OR (MID-KDANMORS-UPP = '53' AND TEST-IDFTG-NON-VCC)            
106600                                                                          
106700              IF MID-IDKONTO-UPP NUMERIC                                  
106800                 MOVE MID-IDKONTO-UPP TO WS-IDKONTO                       
106900                 IF WS-IDKONTO     NOT = 4110-IDKONTO                     
107000                   MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-UPP-ATTR        
107100                   MOVE NEJ                TO INDATA-SW                   
107200                 END-IF                                                   
107300              ELSE                                                        
107400                 MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-UPP-ATTR          
107500                 MOVE NEJ                TO INDATA-SW                     
107600              END-IF                                                      
107700                                                                          
107800              IF MID-IDKST-UPP NOT = 4110-IDKST                           
107900                 MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKST-UPP-ATTR            
108000                 MOVE NEJ                TO INDATA-SW                     
108100              END-IF                                                      
108200                                                                          
108300           END-IF                                                         
108400                                                                          
108500           IF NOT INDATA-FEL                                              
108600                                                                          
108700              IF MFS-UPD-V                                                
108800                IF MID-IDFTG-UPP NOT = WS-IDFTG                           
108900                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-UPP-ATTR            
109000                  MOVE NEJ               TO INDATA-SW                     
109100                  MOVE NEJ               TO IDFTG-UPD-SW                  
109200                END-IF                                                    
109300              ELSE                                                        
109400                IF MID-IDFTG-UPP NOT = MSGI-IDFTG                         
109500                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-UPP-ATTR            
109600                  MOVE NEJ               TO INDATA-SW                     
109700                  MOVE NEJ               TO IDFTG-UPD-SW                  
109800                END-IF                                                    
109900              END-IF                                                      
110000           END-IF                                                         
110100         END-IF                                                           
110200       END-IF                                                             
110300     ELSE                                                                 
110400                                                                          
110500       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDARTNR-UPP-ATTR             
110600       MOVE NEJ                       TO INDATA-SW                        
110700     END-IF                                                               
110800     .                                                                    
110900     EJECT                                                                
111000 GC-UPP-RAD-LOGISK-KOLL SECTION.                                          
111100     MOVE 'GC-UPP-RAD-LOGISKA-KOLL'   TO WS-SEKTION                       
111200                                                                          
111300     IF MID-IDARTNR-UPP NOT = W-IDARTNR                                   
111400       MOVE MID-IDARTNR-UPP           TO W-IDARTNR-D6                     
111500       PERFORM IMS-GET-ARTC01                                             
111600       IF SEGMENT-SAKNAS                                                  
111700         MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                     
111800         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDARTNR-UPP-ATTR             
111900         MOVE NEJ                     TO INDATA-SW                        
112000       END-IF                                                             
112100     END-IF                                                               
112200                                                                          
112300     IF INDATA-OK                                                         
112400       PERFORM GCA-KOLLA-INTERVALL                                        
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800 GCA-KOLLA-INTERVALL SECTION.                                             
112900     MOVE 'GCA-KOLLA-INTERVALL'        TO WS-SEKTION                      
113000                                                                          
113100     MOVE NEJ                           TO FEL                            
113200     PERFORM GCAA-LAES-IN-I-TABELL                                        
113300     MOVE +1                            TO IX                             
113400     PERFORM UNTIL IX > MAX-IX                                            
113500        MOVE MID-TIGILTIG-FOM-UPP       TO TMP1-YYMMDD                    
113600        MOVE MID-TIGILTIG-TOM-UPP       TO TMP2-YYMMDD                    
113700        MOVE DAGENS-DATUM               TO TMP3-YYMMDD                    
113800        MOVE TAB-TIGILTIG-TOM (IX)      TO TMP4-YYMMDD                    
113900        PERFORM WY2000Q1                                                  
114000                                                                          
114100        IF  TMP1-YYMMDD           >  TMP4-YYMMDD            AND           
114200            TMP1-YYMMDD           >= TMP3-YYMMDD            AND           
114300            TMP2-YYMMDD           >= TMP3-YYMMDD                          
114400           MOVE JA                      TO SW-TIGILTIG-FINNS              
114500        ELSE                                                              
114600           MOVE NEJ                     TO INDATA-SW                      
114700        END-IF                                                            
114800        ADD +1                          TO IX                             
114900     END-PERFORM                                                          
115000                                                                          
115100     IF INDATA-FEL                                                        
115200       MOVE +1                          TO IX                             
115300       PERFORM UNTIL IX > MAX-IX                                          
115400          MOVE MID-TIGILTIG-FOM-UPP     TO TMP1-YYMMDD                    
115500          MOVE MID-TIGILTIG-TOM-UPP     TO TMP2-YYMMDD                    
115600          MOVE DAGENS-DATUM             TO TMP3-YYMMDD                    
115700          MOVE TAB-TIGILTIG-FOM (IX)    TO TMP4-YYMMDD                    
115800          PERFORM WY2000Q1                                                
115900          IF TMP1-YYMMDD          =  TMP4-YYMMDD           AND            
116000             TMP2-YYMMDD          >= TMP3-YYMMDD           AND            
116100             TAB-KVART (IX) > +0                                          
116200             MOVE JA                    TO INDATA-SW                      
116300             MOVE JA                    TO SW-TIGILTIG-FINNS              
116400          ELSE                                                            
116500             MOVE MID-TIGILTIG-TOM-UPP  TO TMP1-YYMMDD                    
116600             MOVE TAB-TIGILTIG-FOM (IX) TO TMP2-YYMMDD                    
116700             MOVE TAB-TIGILTIG-TOM (IX) TO TMP3-YYMMDD                    
116800             PERFORM WY2000Q1                                             
116900             IF TMP1-YYMMDD          >= TMP2-YYMMDD           AND         
117000                TMP1-YYMMDD          <  TMP3-YYMMDD                       
117100                MOVE JA                 TO FEL                            
117200             END-IF                                                       
117300          END-IF                                                          
117400          ADD +1                        TO IX                             
117500       END-PERFORM                                                        
117600                                                                          
117700       IF TIGILTIG-FINNS                                                  
117800          MOVE JA                       TO INDATA-SW                      
117900       END-IF                                                             
118000                                                                          
118100       IF FEL = JA                                                        
118200          MOVE NEJ                      TO INDATA-SW                      
118300       END-IF                                                             
118400                                                                          
118500       IF INDATA-OK                                                       
118600          CONTINUE                                                        
118700       ELSE                                                               
118800          MOVE ERR-CORR-HILITE-FLDS     TO MED-IDMFSFEL                   
118900                                                                          
119000          MOVE MFS-NUM-FAELT-FEL     TO MOD-TIGILTIG-FOM-UPP-ATTR         
119100                                         MOD-TIGILTIG-TOM-UPP-ATTR        
119200          PERFORM MFS-ROER-EJ-FAELT-UT                                    
119300          PERFORM MFS-ROER-EJ-FAELT-IN                                    
119400       END-IF                                                             
119500     END-IF                                                               
119600     .                                                                    
119700     EJECT                                                                
119800 GCAA-LAES-IN-I-TABELL SECTION.                                           
119900     MOVE 'GCAA-LAES-IN-I-TABELL'       TO WS-SEKTION                     
120000                                                                          
120100     MOVE +1                         TO IX                                
120200     MOVE MID-IDARTNR-UPP            TO W-IDARTNR                         
120300                                        W-IDARTNR-MAX                     
120400     MOVE MID-KDANMORS-UPP           TO W-KDANMORS                        
120500                                        W-KDANMORS-MAX                    
120600     PERFORM IMS-GET-410901                                               
120700     PERFORM IMS-GNP-410911-KVAL                                          
120800     PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-IX                          
120900        MOVE 4110-DAGILTIG-FOM (3:6) TO TAB-TIGILTIG-FOM (IX)             
121000        MOVE 4110-DAGILTIG-TOM (3:6) TO TAB-TIGILTIG-TOM (IX)             
121100        MOVE 4110-KVART              TO TAB-KVART        (IX)             
121200        MOVE MID-TIGILTIG-TOM-UPP    TO TMP1-YYMMDD                       
121300        MOVE MID-TIGILTIG-FOM-UPP    TO TMP2-YYMMDD                       
121400        MOVE TAB-TIGILTIG-TOM (IX)   TO TMP3-YYMMDD                       
121500        MOVE TAB-TIGILTIG-FOM (IX)   TO TMP4-YYMMDD                       
121600        PERFORM WY2000Q1                                                  
121700        IF (TMP4-YYMMDD          > TMP2-YYMMDD  AND                       
121800            TMP4-YYMMDD          < TMP1-YYMMDD) OR                        
121900           (TMP3-YYMMDD         >= TMP2-YYMMDD  AND                       
122000            TMP3-YYMMDD         <= TMP1-YYMMDD)                           
122100           MOVE JA                   TO FEL                               
122200        END-IF                                                            
122300        PERFORM IMS-GNP-410911-KVAL                                       
122400        ADD +1                       TO IX                                
122500     END-PERFORM                                                          
122600     .                                                                    
122700     EJECT                                                                
122800 H-UPPDATERA SECTION.                                                     
122900     MOVE 'H-UPPDATERA'                 TO WS-SEKTION                     
123000                                                                          
123100     MOVE MID-IDARTNR-UPP            TO W-IDARTNR                         
123200                                        W-IDARTNR-MAX                     
123300     MOVE MID-KDANMORS-UPP           TO W-KDANMORS                        
123400     MOVE MID-TIGILTIG-FOM-UPP       TO W-DAGILTIG-FOM                    
123500     IF MID-TIGILTIG-FOM-UPP NOT = ZERO                                   
123600       IF MID-TIGILTIG-FOM-UPP < 500000                                   
123700         MOVE 20                     TO W-DAGILTIG-FOM (1:2)              
123800       ELSE                                                               
123900         IF MID-TIGILTIG-FOM-UPP < 999999                                 
124000           MOVE 19                   TO W-DAGILTIG-FOM (1:2)              
124100         ELSE                                                             
124200           MOVE 99999999             TO W-DAGILTIG-FOM                    
124300         END-IF                                                           
124400       END-IF                                                             
124500     END-IF                                                               
124600     PERFORM IMS-GHNP-410911-KVAL-FIRST                                   
124700     IF SEGMENT-FINNS                                                     
124800        IF MID-FLBORT-UPP = 'J' OR 'Y'                                    
124900          PERFORM IMS-DLET-4109                                           
125000        ELSE                                                              
125100*************** ÄNDRAR PÅ BEFINTLIGT DOKUMENT **********                  
125200          IF TIGILTIG-FINNS                                               
125300            MOVE MID-TIGILTIG-TOM-UPP TO 4110-DAGILTIG-TOM                
125400            IF MID-TIGILTIG-TOM-UPP NOT = ZERO                            
125500              IF MID-TIGILTIG-TOM-UPP < 500000                            
125600                MOVE 20               TO 4110-DAGILTIG-TOM (1:2)          
125700              ELSE                                                        
125800                IF MID-TIGILTIG-TOM-UPP < 999999                          
125900                  MOVE 19             TO 4110-DAGILTIG-TOM (1:2)          
126000                ELSE                                                      
126100                  MOVE 99999999       TO 4110-DAGILTIG-TOM                
126200                END-IF                                                    
126300              END-IF                                                      
126400            END-IF                                                        
126500                                                                          
126600            PERFORM IMS-REPL-410911                                       
126700          END-IF                                                          
126800        END-IF                                                            
126900     ELSE                                                                 
127000*************** FÖRSTA GÅNGEN RADEN LÄGGS UPP **********                  
127100       MOVE MID-IDARTNR-UPP          TO 4110-IDARTNR                      
127200       MOVE MID-TIGILTIG-FOM-UPP     TO 4110-DAGILTIG-FOM                 
127300                                        W-DAGILTIG-FOM                    
127400       IF MID-TIGILTIG-FOM-UPP NOT = ZERO                                 
127500         IF MID-TIGILTIG-FOM-UPP < 500000                                 
127600           MOVE 20                   TO 4110-DAGILTIG-FOM (1:2)           
127700                                        W-DAGILTIG-FOM (1:2)              
127800         ELSE                                                             
127900           IF MID-TIGILTIG-FOM-UPP < 999999                               
128000             MOVE 19                 TO 4110-DAGILTIG-FOM (1:2)           
128100                                        W-DAGILTIG-FOM (1:2)              
128200           ELSE                                                           
128300             MOVE 99999999           TO 4110-DAGILTIG-FOM                 
128400                                        W-DAGILTIG-FOM                    
128500           END-IF                                                         
128600         END-IF                                                           
128700       END-IF                                                             
128800                                                                          
128900       MOVE MID-KDANMORS-UPP         TO 4110-KDANMORS                     
129000                                                                          
129100       MOVE MID-IDANALYS-UPP         TO 4110-IDANALYS                     
129200                                                                          
129300       MOVE WS-IDFTG                 TO TEST-WS-IDFTG                     
129400                                                                          
129500       IF (MID-KDANMORS-UPP = '53' AND TEST-IDFTG-PV)                     
129600       OR (MID-KDANMORS-UPP = '55' AND TEST-IDFTG-PV)                     
129700       OR (MID-KDANMORS-UPP = '53' AND TEST-IDFTG-NON-VCC)                
129900          MOVE MID-IDKONTO-UPP       TO 4110-IDKONTO                      
130000          MOVE MID-IDKST-UPP         TO 4110-IDKST                        
130100       ELSE                                                               
130200          IF MID-KDANMORS-UPP = '52'                                      
130300             MOVE '0000000000'       TO 4110-IDKONTO                      
130400             MOVE SPACE              TO 4110-IDKST                        
130500          ELSE                                                            
130600             MOVE '0000000000'       TO 4110-IDKONTO                      
130700             MOVE SPACE              TO 4110-IDKST                        
130800          END-IF                                                          
130900       END-IF                                                             
131000       MOVE MID-IDUSER-UPP           TO 4110-IDUSER                       
131100       MOVE +0                       TO 4110-KVART                        
131200       MOVE MID-TIGILTIG-TOM-UPP     TO 4110-DAGILTIG-TOM                 
131300       IF MID-TIGILTIG-TOM-UPP NOT = ZERO                                 
131400         IF MID-TIGILTIG-TOM-UPP < 500000                                 
131500           MOVE 20                   TO 4110-DAGILTIG-TOM (1:2)           
131600         ELSE                                                             
131700           IF MID-TIGILTIG-TOM-UPP < 999999                               
131800             MOVE 19                 TO 4110-DAGILTIG-TOM (1:2)           
131900           ELSE                                                           
132000             MOVE 99999999           TO 4110-DAGILTIG-TOM                 
132100           END-IF                                                         
132200         END-IF                                                           
132300       END-IF                                                             
132400                                                                          
132500       PERFORM IMS-ISRT-410911                                            
132600     END-IF                                                               
132700     MOVE INF-UPDATE-DONE            TO MED-IDMFSINF                      
132800     PERFORM MFS-RENSA-FAELT-IN                                           
132900     .                                                                    
133000     EJECT                                                                
133100 Z-FINIT SECTION.                                                         
133200     MOVE 'Z-FINIT'                     TO WS-SEKTION                     
133300                                                                          
133400     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
133500       CALL WMEDKONV USING MED-WMEDAREA                                   
133600       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
133700       MOVE MED-MFSINF                TO MOD-TEMFSINF                     
133800     END-IF                                                               
133900     IF SAP-BEFEL NOT = SPACE                                             
134000       MOVE SAP-BEFEL TO MOD-TEMFSINF                                     
134100     END-IF                                                               
134200                                                                          
134300     MOVE MAX-MOD-LAENGD              TO MSG-KVLL                         
134400     PERFORM IMS-INSERT-MSG                                               
134500     .                                                                    
134600     EJECT                                                                
134700 MFS-RENSA-FAELT-UT SECTION.                                              
134800     MOVE 'MFS-RENSA-FAELT-UT'          TO WS-SEKTION                     
134900                                                                          
135000*    --- ALLA UTDATA-FÄLT                                                 
135100*    --- INKL. BLÄDDRINGSNYCKLAR                                          
135200     MOVE ZERO            TO MOD-IDARTNR-ENTER                            
135300                             MOD-IDARTNR-NEXT                             
135400                             MOD-TIGILTIG-FOM-ENTER                       
135500                             MOD-TIGILTIG-FOM-NEXT                        
135600                             MOD-KDANMORS-ENTER                           
135700                             MOD-KDANMORS-NEXT                            
135800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD1                             
135900                             MOD-TIGILTIG-FOM-RAD1                        
136000                             MOD-TIGILTIG-TOM-RAD1                        
136100                             MOD-IDFTG-RAD1                               
136200                             MOD-KDANMORS-RAD1                            
136300                             MOD-IDANALYS-RAD1                            
136400                             MOD-IDKONTO-RAD1                             
136500                             MOD-IDKST-RAD1                               
136600                             MOD-IDUSER-RAD1                              
136700                             MOD-KVART-RAD1                               
136800     MOVE +1 TO INDX                                                      
136900     PERFORM UNTIL INDX > MAX-INDX                                        
137000       PERFORM MFS-RENSA-RAD                                              
137100       ADD +1 TO INDX                                                     
137200     END-PERFORM                                                          
137300     .                                                                    
137400     SKIP2                                                                
137500 MFS-RENSA-RAD SECTION.                                                   
137600     MOVE 'MFS-RENSA-RAD'               TO WS-SEKTION                     
137700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(INDX)                            
137800                             MOD-TIGILTIG-FOM(INDX)                       
137900                             MOD-TIGILTIG-TOM(INDX)                       
138000                             MOD-KDANMORS(INDX)                           
138100                             MOD-IDFTG(INDX)                              
138200                             MOD-IDKONTO(INDX)                            
138300                             MOD-IDKST(INDX)                              
138400                             MOD-IDANALYS(INDX)                           
138500                             MOD-IDUSER(INDX)                             
138600                             MOD-KVART(INDX)                              
138700     .                                                                    
138800     EJECT                                                                
138900 MFS-RENSA-FAELT-IN SECTION.                                              
139000     MOVE 'MFS-RENSA-FAELT-IN'          TO WS-SEKTION                     
139100                                                                          
139200*    --- ALLA INDATA-FÄLT                                                 
139300     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UPP                            
139400                               MOD-TIGILTIG-FOM-UPP                       
139500                               MOD-TIGILTIG-TOM-UPP                       
139600                               MOD-KDANMORS-UPP                           
139700                               MOD-IDFTG-UPP                              
139800                               MOD-IDKONTO-UPP                            
139900                               MOD-IDKST-UPP                              
140000                               MOD-IDANALYS-UPP                           
140100                               MOD-IDUSER-UPP                             
140200                               MOD-FLBORT-UPP                             
140300     .                                                                    
140400     EJECT                                                                
140500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
140600     MOVE 'MFS-ROER-EJ-FAELT-UT'        TO WS-SEKTION                     
140700                                                                          
140800*    --- ALLA UTDATA-FÄLT                                                 
140900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD1                           
141000                               MOD-TIGILTIG-FOM-RAD1                      
141100                               MOD-TIGILTIG-TOM-RAD1                      
141200                               MOD-IDFTG-RAD1                             
141300                               MOD-KDANMORS-RAD1                          
141400                               MOD-IDANALYS-RAD1                          
141500                               MOD-IDKONTO-RAD1                           
141600                               MOD-IDKST-RAD1                             
141700                               MOD-IDUSER-RAD1                            
141800                               MOD-KVART-RAD1                             
141900     MOVE +1 TO INDX                                                      
142000     PERFORM UNTIL INDX > MAX-INDX                                        
142100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(INDX)                        
142200                                 MOD-TIGILTIG-FOM(INDX)                   
142300                                 MOD-TIGILTIG-TOM(INDX)                   
142400                                 MOD-IDFTG(INDX)                          
142500                                 MOD-KDANMORS(INDX)                       
142600                                 MOD-IDANALYS(INDX)                       
142700                                 MOD-IDKONTO(INDX)                        
142800                                 MOD-IDKST(INDX)                          
142900                                 MOD-IDUSER(INDX)                         
143000                                 MOD-KVART(INDX)                          
143100       ADD +1 TO INDX                                                     
143200     END-PERFORM                                                          
143300     .                                                                    
143400     SKIP3                                                                
143500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
143600     MOVE 'MFS-ROER-EJ-FAELT-IN'        TO WS-SEKTION                     
143700                                                                          
143800*    --- ALLA INDATA-FÄLT                                                 
143900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPP                            
144000                               MOD-TIGILTIG-FOM-UPP                       
144100                               MOD-TIGILTIG-TOM-UPP                       
144200                               MOD-IDFTG-UPP                              
144300                               MOD-KDANMORS-UPP                           
144400                               MOD-IDANALYS-UPP                           
144500                               MOD-IDKONTO-UPP                            
144600                               MOD-IDKST-UPP                              
144700                               MOD-IDUSER-UPP                             
144800                               MOD-FLBORT-UPP                             
144900     .                                                                    
145000     EJECT                                                                
145100* --- IMS SEKTIONER ---                                                   
145200     SKIP3                                                                
145300 IMS-GET-MSG SECTION.                                                     
145400                                                                          
145500     MOVE '  QC' TO GODK-STATUSKODER                                      
145600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
145700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145800     PERFORM IMS-STATUSKONTROLL                                           
145900     .                                                                    
146000     SKIP3                                                                
146100 IMS-INSERT-MSG SECTION.                                                  
146200                                                                          
146300     IF MSGI-IDLAND-SPR = 'GB'                                            
146400       MOVE 'N' TO MFS-KDHUVOMR                                           
146500     END-IF                                                               
146600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
146700     MOVE SPACE TO GODK-STATUSKODER                                       
146800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
146900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
147000     PERFORM IMS-STATUSKONTROLL                                           
147100     .                                                                    
147200     EJECT                                                                
147300 IMS-GET-410901 SECTION.                                                  
147400     MOVE 'IMS-GET-410901'      TO WS-IMS-SEKTION                         
147500                                                                          
147600     STRING 'WL410901(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
147700          DELIMITED BY SIZE INTO SSA1                                     
147800     MOVE '  GE' TO GODK-STATUSKODER                                      
147900     CALL CBLTDLI USING GU 4109-PCB DLI-IO-AREA-WL410901 SSA1             
148000     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
148100     PERFORM IMS-STATUSKONTROLL                                           
148200     .                                                                    
148300     SKIP3                                                                
148400 IMS-GHNP-410911 SECTION.                                                 
148500     MOVE 'IMS-GHNP-410911'      TO WS-IMS-SEKTION                        
148600                                                                          
148700     STRING 'WL410911(KEY4110  =' W-KEY4110-X ')'                         
148800          DELIMITED BY SIZE INTO SSA1                                     
148900     MOVE '  GE' TO GODK-STATUSKODER                                      
149000     CALL CBLTDLI USING GHNP 4109-PCB DLI-IO-AREA-WL410911 SSA1           
149100     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
149200     PERFORM IMS-STATUSKONTROLL                                           
149300     .                                                                    
149400     SKIP3                                                                
149500 IMS-GNP-410911 SECTION.                                                  
149600     MOVE 'IMS-GNP-410911'      TO WS-IMS-SEKTION                         
149700                                                                          
149800     STRING 'WL410911(KEY4110 =>' W-KEY4110-X ')'                         
149900          DELIMITED BY SIZE INTO SSA1                                     
150000     MOVE '  GE' TO GODK-STATUSKODER                                      
150100     CALL CBLTDLI USING GNP 4109-PCB DLI-IO-AREA-WL410911 SSA1            
150200     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
150300     PERFORM IMS-STATUSKONTROLL                                           
150400     .                                                                    
150500     EJECT                                                                
150600 IMS-GNP-410911-KVAL SECTION.                                             
150700     MOVE 'IMS-GNP-410911-KVAL' TO WS-IMS-SEKTION                         
150800                                                                          
150900     STRING 'WL410911(KEY4110 =>' W-KEY4110-X                             
151000                    '&KEY4110 =<' W-KEY4110-MAX-X ')'                     
151100          DELIMITED BY SIZE INTO SSA1                                     
151200     MOVE '  GE' TO GODK-STATUSKODER                                      
151300     CALL CBLTDLI USING GNP 4109-PCB DLI-IO-AREA-WL410911 SSA1            
151400     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
151500     PERFORM IMS-STATUSKONTROLL                                           
151600     .                                                                    
151700                                                                          
151800 IMS-GHNP-410911-KVAL-FIRST      SECTION.                                 
151900     MOVE 'IMS-GHNP-410911-KVAL-FIRST' TO WS-IMS-SEKTION                  
152000                                                                          
152100     STRING 'WL410901(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
152200          DELIMITED BY SIZE INTO SSA1                                     
152300     STRING 'WL410911(KEY4110 = ' W-KEY4110-X ')'                         
152400          DELIMITED BY SIZE INTO SSA2                                     
152500     MOVE '  GE' TO GODK-STATUSKODER                                      
152600     CALL CBLTDLI USING                                                   
152700                  GHU 4109-PCB DLI-IO-AREA-WL410911 SSA1 SSA2             
152800     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100     EJECT                                                                
153200 IMS-ISRT-410911 SECTION.                                                 
153300     MOVE 'IMS-ISRT-410911'  TO WS-IMS-SEKTION                            
153400                                                                          
153500     MOVE 'WL410911 ' TO SSA1                                             
153600     MOVE '    ' TO GODK-STATUSKODER                                      
153700     CALL CBLTDLI USING ISRT 4109-PCB DLI-IO-AREA-WL410911 SSA1           
153800     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
153900     PERFORM IMS-STATUSKONTROLL                                           
154000     .                                                                    
154100     SKIP3                                                                
154200 IMS-DLET-4109 SECTION.                                                   
154300     MOVE 'IMS-DLET-4109'    TO WS-IMS-SEKTION                            
154400                                                                          
154500     MOVE '  ' TO GODK-STATUSKODER                                        
154600     CALL CBLTDLI USING DLET 4109-PCB DLI-IO-AREA-WL410911                
154700     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
154800     PERFORM IMS-STATUSKONTROLL                                           
154900     .                                                                    
155000     SKIP3                                                                
155100 IMS-REPL-410911 SECTION.                                                 
155200     MOVE 'IMS-REPL-410911'  TO WS-IMS-SEKTION                            
155300                                                                          
155400     MOVE '  ' TO GODK-STATUSKODER                                        
155500     CALL CBLTDLI USING REPL 4109-PCB DLI-IO-AREA-WL410911                
155600     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
155700     PERFORM IMS-STATUSKONTROLL                                           
155800     .                                                                    
155900     EJECT                                                                
156000 IMS-GET-ARTC01 SECTION.                                                  
156100     MOVE 'IMS-GET-ARTC01'  TO WS-IMS-SEKTION                             
156200                                                                          
156300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-D6-X ')'                      
156400          DELIMITED BY SIZE INTO SSA1                                     
156500     MOVE '  GE' TO GODK-STATUSKODER                                      
156600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK601 SSA1               
156700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
156800     PERFORM IMS-STATUSKONTROLL                                           
156900     .                                                                    
157000     SKIP3                                                                
157010 IMS-GU-WDB601-FTG SECTION.                                               
157020     STRING 'WDB601  (IDFTG    =' W-IDFTG-B6 ')'                          
157030            DELIMITED BY SIZE INTO SSA1                                   
157040     MOVE '  GE'                 TO GODK-STATUSKODER                      
157050     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
157060     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
157070     PERFORM IMS-STATUSKONTROLL                                           
157080     .                                                                    
157090     SKIP3                                                                
157091                                                                          
157100 IMS-STATUSKONTROLL SECTION.                                              
157200                                                                          
157300     SET STATUS-IX TO 1                                                   
157400     SEARCH GODK-STATUS                                                   
157500       AT END                                                             
157600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
157700         DELIMITED BY SIZE INTO FELTEXT                                   
157800         CALL FELLOG                                                      
157900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
158000         CONTINUE                                                         
158100     END-SEARCH                                                           
158200     .                                                                    
158300     EJECT                                                                
158400*    -COPY WY2000Q1                                                       
