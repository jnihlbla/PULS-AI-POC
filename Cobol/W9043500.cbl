000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W9043500.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   2007/07/17.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        ADMINISTRATION AV ANALYSNUMMER-REGISTRET                         
001000*                                                                         
001100*        DETTA PGM ÄR EN KOPIA AV 4702, SPECIAL FÖR SPIE-2                
001200*        SKILLNADEN MOT 4702 ÄR ATT ENDAST SENASTE TOM-DATUM              
001300*        FÖR VARJE KOD GES I EN FRÅGETRANS, SAMT ATT SVARET               
001400*        ENDAST BERÖR DEN EFTERFRÅGADE ARTIKELN.                          
001500*                                                                         
001600*        PGM:ET SOM ÄR MPP GER MÖJLIGHET FÖRUTOM FRÅGA                    
001700*        PÅ VISS ARTIKEL OCKSÅ FÖR BORTTAG AV ANALYSNR,                   
001800*        SAMT NYUPPLÄGG MED ANGIVEN GILTIGHETS PERIOD.                    
001900*        HÅRDA KONTROLLER UTFÖRS VID NYANGIVEN TIDSPERIOD                 
002000*        SÅ ATT INGA ÖVERLAPPNINGAR SKER.                                 
002100*                                                                         
002200*    OBSERVERA: -----------------------------------------------+          
002300*    ¦   VID FÖRÄNDRINGAR AV LOGIKEN, IFRÅGASÄTT OM KODEN ÄVEN ¦          
002400*    ¦   SKALL ÄNDRAS I PULS-ORIGINALET w4070200.              ¦          
002500*    +---------------------------------------------------------+          
002600*                                                                         
002700*    SKAPAT:                                                              
002800*        Kopia av W4070200.                                               
002900*        Enligt eTracker 5643898 som är en del av 3408639.                
003000*                                                                         
003100*    SPECIELL FUNKTIONELL SKILLNAD MOT W4070200:                          
003200*        Vid läsning av info skall endast den rad med datum               
003300*        längst fram i tiden (DAGILTIG) visas för                         
003400*        varje KOD (KDANMORS), oavsett ANALYSNR. (IDANALYS)               
003500*                                                                         
003600*                                                                         
003700*                                                                         
003800*        PROGRAMMET UPPDATERAR WL4109 (WDR1)                              
003900*                                                                         
004000*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
004100*                                                                         
004200*    INDATA.                                                              
004300*        TRANSAKTION: W90435T                                             
004400*                     W90435U                                             
004500*        MID:         W90435I1                                            
004600*                                                                         
004700*    UTDATA.                                                              
004800*        MOD:         W90435O1                                            
004900                                                                          
005000     SKIP3                                                                
005100 ENVIRONMENT DIVISION.                                                    
005200     EJECT                                                                
005300 DATA DIVISION.                                                           
005400 WORKING-STORAGE SECTION.                                                 
005500*    -COPY WY2000W1                                                       
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(08)   VALUE 'W9043500'.            
005800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005900 77  ftg-fel-text                PIC X(11)   VALUE SPACE.                 
006000 77  ftg-fel                     PIC X       VALUE 'N'.                   
006100 77  art-fel-text                PIC X(21)   VALUE SPACE.                 
006200 77  art-fel                     PIC X       VALUE 'N'.                   
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 77  FEL                         PIC X       VALUE 'N'.                   
006600 77  IX                          PIC S9(4)   VALUE +0   COMP SYNC.        
006700 77  MAX-IX                      PIC S9(4)   VALUE +20  COMP SYNC.        
006800 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
006900 77  MAX-INDX                    PIC S9(4)   VALUE +20  COMP SYNC.        
007000 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +0   COMP SYNC.        
007100 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
007200 77  WS-IDKONTO                  PIC 9(10)   VALUE ZERO.                  
007300 77  WS-KDANMORS-NUM             PIC 9(2)    VALUE ZERO.                  
007310 77  WS-IDFTG-B6                 PIC 9(2)    VALUE ZERO.                  
007400 77  DAGENS-DATUM                PIC S9(7)   VALUE +0   COMP-3.           
007500                                                                          
007600 77  SW-TIGILTIG-FINNS           PIC X       VALUE 'N'.                   
007700     88 TIGILTIG-FINNS                       VALUE 'J'.                   
007800                                                                          
007900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008000     88  INDATA-OK                           VALUE 'J'.                   
008100     88  INDATA-FEL                          VALUE 'N'.                   
008200                                                                          
008300 77  IDFTG-UPD-SW                PIC X       VALUE 'J'.                   
008400     88  IDFTG-UPD-OK                        VALUE 'J'.                   
008500     88  IDFTG-UPD-FEL                       VALUE 'N'.                   
008600                                                                          
008700 77  PERIOD-SW                   PIC X       VALUE 'N'.                   
008800     88  PERIOD-OK                           VALUE 'J'.                   
008900                                                                          
009000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009100     88  NYCKLAR-OK                          VALUE 'J'.                   
009200     88  NYCKLAR-FEL                         VALUE 'N'.                   
009300                                                                          
009400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009500     88  EGEN-MID                            VALUE '9435'.                
009600     88  GODK-MID                            VALUE '9435'.                
009700                                                                          
009800 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
009900 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
010000 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
010100 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
010200                                                                          
010300     EJECT                                                                
010400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010500 01  GENERELLA-SUBPROGRAM.                                                
010600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011100*                                                                         
011200     03  W411SAP                 PIC X(8)    VALUE 'W411SAP '.            
011300*            SAP KONTROLL                                                 
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011600*01 -COPY WMEDAREA                                                        
011700     SKIP3                                                                
011800 01  MESSAGE-CODES.                                                       
011900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012000     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
012100     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
012200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012300     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
012400     03  ERR-END-VALUE           PIC X(3)    VALUE '240'.                 
012500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013300*                                                                         
013400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013500     SKIP3                                                                
013600*01 -COPY WMSGINIT                                                        
013700     EJECT                                                                
013800*                                                                         
013900*01  -COPY WDATAREA                                                       
014000     EJECT                                                                
014100 01  FILLER                      PIC  X(12) VALUE                         
014200                                               'SAP KONTROLL'.            
014300*                                                                         
014400*01 -COPY W411SAP                                                         
014500     EJECT                                                                
014600*01  -COPY WWIDFTG                                                        
014700     EJECT                                                                
014800                                                                          
014900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015000*                                                                         
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015300     SKIP3                                                                
015400*01  MID -COPY W90435I1                                                   
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015700     SKIP3                                                                
015800*01  -COPY WMSGAREA                                                       
015900     EJECT                                                                
016000*    03 MOD -COPY W90435O1 -RED MSG-AREA.                                 
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016300*01  -COPY WMFSAREA                                                       
016400     EJECT                                                                
016500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800     SKIP3                                                                
016900 01  NYCKLAR-TILL-DLI.                                                    
017000     03  W-WDGXKEY-ROT-X.                                                 
017100         05  FILLER              PIC X(04)    VALUE '4109'.               
017200         05  W-IDFTG-4109        PIC 9(2)     VALUE ZERO.                 
017300         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
017400                                                                          
017500     03  W-KEY4110-X.                                                     
017600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017700         05  W-KDANMORS          PIC X(2)    VALUE SPACE.                 
017800         05  W-DAGILTIG-FOM      PIC 9(8)    VALUE ZERO.                  
017900                                                                          
018000     03  W-KEY4110-MAX-X.                                                 
018100         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
018200         05  W-KDANMORS-MAX      PIC X(2)    VALUE SPACE.                 
018300         05  FILLER              PIC X(8)    VALUE HIGH-VALUE.            
018400*                                                                         
018500     03  W-WDH301KY-X.                                                    
018600         05  W-IDKONTO           PIC S9(11)  VALUE ZERO COMP-3.           
018700         05  W-IDKST             PIC X(10)   VALUE SPACE.                 
018800         05  W-IDPROFIT          PIC X(10)   VALUE SPACE.                 
018900         05  W-IDANALYS          PIC X(12)   VALUE SPACE.                 
019000                                                                          
019100     03  W-IDARTNR-D6-X.                                                  
019200         05  W-IDARTNR-D6        PIC S9(9)   VALUE ZERO COMP-3.           
019201                                                                          
019300     EJECT                                                                
019400*    --- LEVANM-TABELL                                                    
019500 01  ANALYS-POST-TABELL.                                                  
019600    03   ANALYS-POST OCCURS 20.                                           
019700      05  TAB-TIGILTIG-FOM      PIC S9(7)  COMP-3.                        
019800      05  TAB-TIGILTIG-TOM      PIC S9(7)  COMP-3.                        
019900      05  TAB-KVART             PIC S9(7)  COMP-3.                        
020000     EJECT                                                                
020100                                                                          
020200*    --- STATUS-KOD FRÅN IMS                                              
020300 01  STATUS-WS                   PIC XX.                                  
020400     88  SEGMENT-FINNS                       VALUE '  '.                  
020500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020600     SKIP2                                                                
020700 01  GODK-STATUSKODER.                                                    
020800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020900     SKIP3                                                                
021000 01  SSA1                        PIC X(64).                               
021100 01  SSA2                        PIC X(64).                               
021200     EJECT                                                                
021300*    --- IMS FUNKTIONSKODER                                               
021400*01  -COPY W0003                                                          
021500     EJECT                                                                
021600*    ---  DLI INPUT-OUTPUT AREA                                           
021700 01  FILLER                      PIC X(20) VALUE                          
021800                                          'DLI-IO-AREA-WL410901'.         
021900 01  DLI-IO-AREA-WL410901.                                                
022000     03  WL410901.                                                        
022100*        05  -COPY WDGX4109                                               
022200     EJECT                                                                
022300 01  FILLER                      PIC X(20)  VALUE                         
022400                                          'DLI-IO-AREA-WL410911'.         
022500 01  DLI-IO-AREA-WL410911.                                                
022600     03  WL410911.                                                        
022700*        05  -COPY WDGX4110                                               
022800     EJECT                                                                
022900 01  SPAR-IO-AREA-WL410911.                                               
023000*    03  -COPY WDGX4110 -PRE SPAR-                                        
023100     EJECT                                                                
023200 01  FILLER                      PIC X(20)  VALUE                         
023300                                          'DLI-IO-AREA-WDK601'.           
023400 01  DLI-IO-AREA-WDK601.                                                  
023500     03  WLARTC01.                                                        
023600*        05  -COPY WDK601                                                 
023700     EJECT                                                                
023710 01  FILLER                      PIC X(20)  VALUE                         
023720                                          'DLI-IO-AREA-WDB601'.           
023730 01  DLI-IO-AREA-WDB601.                                                  
023731*     03  -COPY WDB601                                                    
023760     EJECT                                                                
023800 LINKAGE SECTION.                                                         
023900                                                                          
024000*01  -COPY W0009  -PRE MSG-                                               
024100*01  -COPY W0008  -PRE 4109-                                              
024200     05  FILLER                  PIC X.                                   
024300*01  -COPY W0008  -PRE USEA-                                              
024400     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600*01  -COPY W0008  -PRE SAPC-                                              
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008  -PRE ARTC-                                              
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025110*01  -COPY W0008  -PRE WDB6-                                              
025120     05  FILLER                  PIC X.                                   
025130     EJECT                                                                
025200 PROCEDURE DIVISION  USING MSG-PCB 4109-PCB USEA-PCB                      
025300                                   SAPC-PCB ARTC-PCB WDB6-PCB.            
025400     ENTRY 'DLITCBL' USING MSG-PCB 4109-PCB USEA-PCB                      
025500                                   SAPC-PCB ARTC-PCB WDB6-PCB.            
025600                                                                          
025700     PERFORM IMS-GET-MSG                                                  
025800     IF SEGMENT-FINNS                                                     
025900       PERFORM A-INIT                                                     
026000       PERFORM B-KOLLA-NYCKLAR                                            
026100       IF NYCKLAR-OK                                                      
026200         IF MFS-UPDATE OR MFS-UPD-V                                       
026300           PERFORM G-KOLLA-INPUT                                          
026400           IF INDATA-OK                                                   
026500             PERFORM H-UPPDATERA                                          
026600           END-IF                                                         
026700         ELSE                                                             
026800           IF MFS-FIRST                                                   
026900             PERFORM C-FOERSTA-SIDA                                       
027000           ELSE                                                           
027100             IF MFS-NEXT                                                  
027200               PERFORM D-NAESTA-SIDA                                      
027300             ELSE                                                         
027400               PERFORM E-SAMMA-SIDA                                       
027500             END-IF                                                       
027600           END-IF                                                         
027700         END-IF                                                           
027800                                                                          
027900         IF INDATA-OK                                                     
028000           PERFORM F-LAES-VISA-INFO                                       
028100         END-IF                                                           
028200       END-IF                                                             
028300       PERFORM Z-FINIT                                                    
028400     END-IF                                                               
028500                                                                          
028600     MOVE ZERO TO RETURN-CODE                                             
028700     GOBACK                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 A-INIT SECTION.                                                          
029100                                                                          
029200     MOVE 'A-INIT'                    TO WS-SEKTION                       
029300     IF MSG-DUBBLA-TRANSKODER                                             
029400       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
029500                                      TO MID-W90435I1                     
029600       MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                      
029700       MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                     
029800     ELSE                                                                 
029900       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
030000                                      TO MID-W90435I1                     
030100       MOVE MSG-IDTRANS-1             TO MFS-IDTRANS                      
030200       MOVE MSG-KDMFSFOR-1            TO MFS-KDMFSFOR                     
030300     END-IF                                                               
030400                                                                          
030500     MOVE MSG-KDTRTYP                 TO MFS-KDTRTYP                      
030600     MOVE MSG-IDPFK                   TO MFS-IDPFK                        
030700     MOVE MFS-IDTRANS                 TO W-IDTRANS                        
030800                                                                          
030900     MOVE LOW-VALUE                   TO MSG-AREA                         
031000     MOVE 'W90435O1'                  TO MFS-IDMOD                        
031100     MOVE '9435'                      TO MOD-IDTRANS                      
031200                                                                          
031300     MOVE SPACE                       TO MOD-TEMFSFEL MOD-TEMFSINF        
031400                                         MED-IDMFSFEL MED-IDMFSINF        
031500                                                                          
031600     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W90435O1 + 4                  
031700                                                                          
031800     ACCEPT DAGENS-DATUM               FROM DATE                          
031900     MOVE +1                           TO IX                              
032000     PERFORM UNTIL IX > MAX-IX                                            
032100        MOVE +0                        TO TAB-TIGILTIG-FOM  (IX)          
032200                                          TAB-TIGILTIG-TOM  (IX)          
032300                                          TAB-KVART         (IX)          
032400        ADD +1                         TO IX                              
032500     END-PERFORM                                                          
032600     .                                                                    
032700     EJECT                                                                
032800 B-KOLLA-NYCKLAR SECTION.                                                 
032900     MOVE 'B-KOLLA-NYCKLAR'           TO WS-SEKTION                       
033000                                                                          
033100     PERFORM BB-HAEMTA-USERINFO                                           
033200     MOVE 'S  '                       TO MED-IDSKYLT                      
033300                                                                          
033400     MOVE JA                          TO NYCKLAR-SW                       
033500     MOVE nej                         TO art-fel  ftg-fel                 
033600*    -- KONTROLL AV IDARTNR                                               
033700     IF MID-IDARTNR-IN = ALL '+'                                          
033800       MOVE MID-IDARTNR-UT            TO WS-IDARTNR                       
033900       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
034000     ELSE                                                                 
034100       MOVE MID-IDARTNR-IN            TO WS-IDARTNR                       
034200       MOVE '7'                       TO MFS-IDPFK                        
034300       MOVE SPACE                     TO MFS-KDTRTYP                      
034400     END-IF                                                               
034500                                                                          
034600     IF WS-IDARTNR NUMERIC                                                
034700       IF WS-IDARTNR > ZERO                                               
034800         MOVE WS-IDARTNR              TO W-IDARTNR                        
034900       ELSE                                                               
035000         string 'IDART = ' ws-idartnr delimited by size                   
035100         into art-fel-text                                                
035200         move ja                      TO art-fel                          
035300         MOVE NEJ                     TO NYCKLAR-SW                       
035400       END-IF                                                             
035500     ELSE                                                                 
035600         string 'IDART = ' ws-idartnr delimited by size                   
035700         into art-fel-text                                                
035800         move ja                      TO art-fel                          
035900       MOVE NEJ                       TO NYCKLAR-SW                       
036000     END-IF                                                               
036100                                                                          
036200*    -- KONTROLL AV IDFTG                                                 
036300     IF MID-IDFTG-IN = ALL '+'                                            
036400       MOVE MID-IDFTG-UT              TO WS-IDFTG                         
036500       INSPECT WS-IDFTG REPLACING LEADING SPACE BY ZERO                   
036600       IF WS-IDFTG = ZERO                                                 
036700         MOVE MSGI-IDFTG              TO WS-IDFTG                         
036800         MOVE WS-IDFTG                TO W-IDFTG-4109                     
036900       ELSE                                                               
037000         IF WS-IDFTG = 53 OR 54 OR 57                                     
037100           MOVE WS-IDFTG              TO W-IDFTG-4109                     
037200         ELSE                                                             
037300           MOVE MSGI-IDFTG            TO WS-IDFTG                         
037400           MOVE WS-IDFTG              TO W-IDFTG-4109                     
037500         END-IF                                                           
037600       END-IF                                                             
037700     ELSE                                                                 
037800       MOVE MID-IDFTG-IN              TO WS-IDFTG                         
037900       IF WS-IDFTG = 53 OR 54 OR 57                                       
038000         MOVE WS-IDFTG                TO W-IDFTG-4109                     
038100         MOVE '7'                     TO MFS-IDPFK                        
038200         MOVE SPACE                   TO MFS-KDTRTYP                      
038300       ELSE                                                               
038400         move ja                      TO ftg-fel                          
038500         MOVE NEJ                     TO NYCKLAR-SW                       
038600         string 'IDFTG = ' ws-idftg delimited by size                     
038700         into ftg-fel-text                                                
038800       END-IF                                                             
038900     END-IF                                                               
039000                                                                          
039100     IF GODK-MID OR NYCKLAR-OK                                            
039200       MOVE WS-IDARTNR                TO MOD-IDARTNR-UT                   
039300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
039400       MOVE WS-IDFTG                  TO MOD-IDFTG-UT                     
039500     ELSE                                                                 
039600       MOVE SPACE                     TO MOD-IDARTNR-UT                   
039700       MOVE SPACE                     TO MOD-IDFTG-UT                     
039800     END-IF                                                               
039900                                                                          
040000     IF NYCKLAR-FEL                                                       
040100*      MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                     
040200       if art-fel = ja                                                    
040300       and ftg-fel = ja                                                   
040400         string '401 fel nyckel. ' art-fel-text ftg-fel-text              
040500         delimited by size                                                
040600         into MOD-TEMFSFEL                                                
040700       else                                                               
040800         if art-fel = ja                                                  
040900           string '401 fel nyckel. ' art-fel-text                         
041000           delimited by size                                              
041100           into MOD-TEMFSFEL                                              
041200         else                                                             
041300           string '401 fel nyckel. ' ftg-fel-text                         
041400           delimited by size                                              
041500           into MOD-TEMFSFEL                                              
041600         end-if                                                           
041700       end-if                                                             
041800       PERFORM MFS-RENSA-FAELT-IN                                         
041900       PERFORM MFS-RENSA-FAELT-UT                                         
042000     ELSE                                                                 
042100       PERFORM BA-KOLLA-ARTREG                                            
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 BA-KOLLA-ARTREG SECTION.                                                 
042600     MOVE 'BA-KOLLA-ARTREG'            TO WS-SEKTION                      
042700                                                                          
042800     IF W-IDARTNR > ZERO                                                  
042900       MOVE W-IDARTNR                 TO W-IDARTNR-D6                     
043000       PERFORM IMS-GET-ARTC01                                             
043100       IF SEGMENT-SAKNAS                                                  
043200         MOVE NEJ                     TO NYCKLAR-SW                       
043300         MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                     
043400         PERFORM MFS-RENSA-FAELT-IN                                       
043500         PERFORM MFS-RENSA-FAELT-UT                                       
043600       END-IF                                                             
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 BB-HAEMTA-USERINFO SECTION.                                              
044100     MOVE 'BB-HAEMTA-USERINFO'         TO WS-SEKTION                      
044200                                                                          
044300     MOVE ALL '+'                     TO MSGI-WMSGINIT                    
044400     MOVE '001'                       TO MSGI-KDCALL                      
044500     MOVE MSG-SIGNON-USERID           TO MSGI-IDUSER                      
044600     MOVE '9435'                      TO MSGI-IDTRANS                     
044700     MOVE MSG-LTERM-NAME              TO MSGI-IDLTERM-USER                
044800                                                                          
044900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045000                                                                          
045100     .                                                                    
045200     EJECT                                                                
045300 C-FOERSTA-SIDA SECTION.                                                  
045400     MOVE 'C-FOERSTA-SIDA'            TO WS-SEKTION                       
045500                                                                          
045600     MOVE INF-FIRST-PAGE              TO MED-IDMFSINF                     
045700     MOVE LOW-VALUE                   TO W-KDANMORS                       
045800                                                                          
045900     MOVE ZERO                        TO W-DAGILTIG-FOM                   
046000     PERFORM MFS-RENSA-FAELT-IN                                           
046100     .                                                                    
046200     EJECT                                                                
046300 D-NAESTA-SIDA SECTION.                                                   
046400     MOVE 'C-NAESTA-SIDA'             TO WS-SEKTION                       
046500                                                                          
046600     IF MID-IDARTNR-NEXT  NUMERIC                                         
046700       MOVE MID-IDARTNR-NEXT          TO W-IDARTNR                        
046800     ELSE                                                                 
046900       MOVE ZERO                      TO W-IDARTNR                        
047000     END-IF                                                               
047100     IF MID-KDANMORS-NEXT  NUMERIC                                        
047200        MOVE MID-KDANMORS-NEXT        TO W-KDANMORS                       
047300     ELSE                                                                 
047400        MOVE LOW-VALUE                TO W-KDANMORS                       
047500     END-IF                                                               
047600     IF MID-TIGILTIG-FOM-NEXT NUMERIC                                     
047700       MOVE MID-TIGILTIG-FOM-NEXT     TO W-DAGILTIG-FOM                   
047800       IF MID-TIGILTIG-FOM-NEXT NOT = ZERO                                
047900         IF MID-TIGILTIG-FOM-NEXT < 500000                                
048000           MOVE 20                    TO W-DAGILTIG-FOM (1:2)             
048100         ELSE                                                             
048200           IF MID-TIGILTIG-FOM-NEXT < 999999                              
048300             MOVE 19                  TO W-DAGILTIG-FOM (1:2)             
048400           ELSE                                                           
048500             MOVE 99999999            TO W-DAGILTIG-FOM                   
048600           END-IF                                                         
048700         END-IF                                                           
048800       END-IF                                                             
048900     ELSE                                                                 
049000       MOVE ZERO                      TO W-DAGILTIG-FOM                   
049100     END-IF                                                               
049200     PERFORM MFS-RENSA-FAELT-IN                                           
049300     .                                                                    
049400     EJECT                                                                
049500 E-SAMMA-SIDA SECTION.                                                    
049600     MOVE 'E-SAMMA-SIDA'             TO WS-SEKTION                        
049700                                                                          
049800     IF EGEN-MID                                                          
049900       IF MID-IDARTNR-ENTER NUMERIC                                       
050000         MOVE MID-IDARTNR-ENTER       TO W-IDARTNR                        
050100       ELSE                                                               
050200         MOVE ZERO                    TO W-IDARTNR                        
050300       END-IF                                                             
050400       IF MID-KDANMORS-ENTER NUMERIC                                      
050500          MOVE MID-KDANMORS-ENTER     TO W-KDANMORS                       
050600       ELSE                                                               
050700          MOVE LOW-VALUE              TO W-KDANMORS                       
050800       END-IF                                                             
050900                                                                          
051000       IF MID-TIGILTIG-FOM-ENTER NUMERIC                                  
051100         MOVE MID-TIGILTIG-FOM-ENTER  TO W-DAGILTIG-FOM                   
051200         IF MID-TIGILTIG-FOM-ENTER NOT = ZERO                             
051300           IF MID-TIGILTIG-FOM-ENTER < 500000                             
051400             MOVE 20                  TO W-DAGILTIG-FOM (1:2)             
051500           ELSE                                                           
051600             IF MID-TIGILTIG-FOM-ENTER < 999999                           
051700               MOVE 19                TO W-DAGILTIG-FOM (1:2)             
051800             ELSE                                                         
051900               MOVE 99999999          TO W-DAGILTIG-FOM                   
052000             END-IF                                                       
052100           END-IF                                                         
052200         END-IF                                                           
052300       ELSE                                                               
052400         MOVE ZERO                    TO W-DAGILTIG-FOM                   
052500       END-IF                                                             
052600       IF MID-INPUT = ALL '+'                                             
052700         PERFORM MFS-RENSA-FAELT-IN                                       
052800       ELSE                                                               
052900         MOVE INF-PRESS-PF11          TO MED-IDMFSINF                     
053000         PERFORM EA-MID-INDATA-TILL-MOD                                   
053100       END-IF                                                             
053200     ELSE                                                                 
053300       PERFORM MFS-RENSA-FAELT-IN                                         
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 EA-MID-INDATA-TILL-MOD SECTION.                                          
053800     MOVE 'EA-MID-INDATA-TILL-MOD'    TO WS-SEKTION                       
053900***                                                                       
054000     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
054100       MOVE MID-IDARTNR-UPP           TO MOD-IDARTNR-UPP                  
054200       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDARTNR-UPP-ATTR             
054300     ELSE                                                                 
054400       MOVE MFS-RENSA-FAELT           TO MOD-IDARTNR-UPP                  
054500     END-IF                                                               
054600     IF MID-TIGILTIG-FOM-UPP NOT = ALL '+'                                
054700       MOVE MID-TIGILTIG-FOM-UPP      TO MOD-TIGILTIG-FOM-UPP             
054800       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-TIGILTIG-FOM-UPP-ATTR        
054900     ELSE                                                                 
055000       MOVE MFS-RENSA-FAELT           TO MOD-TIGILTIG-FOM-UPP             
055100     END-IF                                                               
055200     IF MID-TIGILTIG-TOM-UPP NOT = ALL '+'                                
055300       MOVE MID-TIGILTIG-TOM-UPP      TO MOD-TIGILTIG-TOM-UPP             
055400       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-TIGILTIG-TOM-UPP-ATTR        
055500     ELSE                                                                 
055600       MOVE MFS-RENSA-FAELT           TO MOD-TIGILTIG-TOM-UPP             
055700     END-IF                                                               
055800     IF MID-IDFTG-UPP NOT = ALL '+'                                       
055900       MOVE MID-IDFTG-UPP             TO MOD-IDFTG-UPP                    
056000       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDFTG-UPP-ATTR               
056100     ELSE                                                                 
056200       MOVE MFS-RENSA-FAELT           TO MOD-IDFTG-UPP                    
056300     END-IF                                                               
056400     IF MID-KDANMORS-UPP NOT = ALL '+'                                    
056500       MOVE MID-KDANMORS-UPP          TO MOD-KDANMORS-UPP                 
056600       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-KDANMORS-UPP-ATTR            
056700     ELSE                                                                 
056800       MOVE MFS-RENSA-FAELT           TO MOD-KDANMORS-UPP                 
056900     END-IF                                                               
057000     IF MID-IDANALYS-UPP NOT = ALL '+'                                    
057100       MOVE MID-IDANALYS-UPP        TO MOD-IDANALYS-UPP                   
057200       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDANALYS-UPP-ATTR            
057300     ELSE                                                                 
057400       MOVE MFS-RENSA-FAELT           TO MOD-IDANALYS-UPP                 
057500     END-IF                                                               
057600     IF MID-IDKONTO-UPP NOT = ALL '+'                                     
057700       MOVE MID-IDKONTO-UPP           TO MOD-IDKONTO-UPP                  
057800       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDKONTO-UPP-ATTR             
057900     ELSE                                                                 
058000       MOVE MFS-RENSA-FAELT           TO MOD-IDKONTO-UPP                  
058100     END-IF                                                               
058200     IF MID-IDKST-UPP NOT = ALL '+'                                       
058300       MOVE MID-IDKST-UPP             TO MOD-IDKST-UPP                    
058400       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDKST-UPP-ATTR               
058500     ELSE                                                                 
058600       MOVE MFS-RENSA-FAELT           TO MOD-IDKST-UPP                    
058700     END-IF                                                               
058800     IF MID-IDUSER-UPP NOT = ALL '+'                                      
058900       MOVE MID-IDUSER-UPP            TO MOD-IDUSER-UPP                   
059000       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDUSER-UPP-ATTR              
059100     ELSE                                                                 
059200       MOVE MFS-RENSA-FAELT           TO MOD-IDUSER-UPP                   
059300     END-IF                                                               
059400     IF MID-FLBORT-UPP NOT = '+'                                          
059500       MOVE MID-FLBORT-UPP            TO MOD-FLBORT-UPP                   
059600       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-FLBORT-UPP-ATTR              
059700     ELSE                                                                 
059800       MOVE MFS-RENSA-FAELT           TO MOD-FLBORT-UPP                   
059900     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 F-LAES-VISA-INFO SECTION.                                                
060400     MOVE 'F-LAES-VISA-INFO'          TO WS-SEKTION                       
060500                                                                          
060600     PERFORM IMS-GU-410901                                                
060700                                                                          
060800     PERFORM IMS-GNP-410911                                               
060900     IF SEGMENT-SAKNAS                                                    
061000        MOVE ERR-KEY-MISSING          TO MED-IDMFSFEL                     
061100        PERFORM MFS-RENSA-FAELT-UT                                        
061200     ELSE                                                                 
061300*      -- Visar den senaste artikelraden för varje KDANMORS               
061400* RAD 1                                                                   
061500       PERFORM FA-SOEK-OCH-REDIGERA-RAD-1                                 
061600*      -- I spar- finns nu den första förekomsten                         
061700*      -- av artikeln med sista datum för första KDANMORS                 
061800       MOVE SPAR-4110-IDARTNR         TO MOD-IDARTNR-ENTER                
061900       MOVE SPAR-4110-DAGILTIG-FOM (3:6) TO MOD-TIGILTIG-FOM-ENTER        
062000       MOVE SPAR-4110-KDANMORS        TO MOD-KDANMORS-ENTER               
062100                                                                          
062200*      -- Läs nästa kod inom artikeln.                                    
062300                                                                          
062400       Perform IMS-GNP-410911                                             
062500* RAD 2-21   (rad 2 har INDX=1 )  SPIE kan ta emot 21 rader               
062600       MOVE +1 TO INDX                                                    
062700       PERFORM UNTIL INDX > MAX-INDX                                      
062800       OR (4110-IDARTNR NOT = W-IDARTNR)                                  
062900         If SEGMENT-FINNS                                                 
063000           Move DLI-IO-AREA-WL410911 To SPAR-IO-AREA-WL410911             
063100           Move 4110-KDANMORS        To W-KDANMORS                        
063200                                        W-KDANMORS-MAX                    
063300*          -- Pröva om den är den sista i koden.                          
063400           Perform IMS-GNP-410911-SIST-PER-KOD                            
063500           If SEGMENT-FINNS                                               
063600*            -- Fanns fler! Spara sista!                                  
063700             Move DLI-IO-AREA-WL410911 To SPAR-IO-AREA-WL410911           
063800           Else                                                           
063900*            -- Sista ligger redan i SPAR-                                
064000             Continue                                                     
064100           End-If                                                         
064200           PERFORM FB-REDIGERA-RAD-2-21                                   
064300           Perform IMS-GNP-410911                                         
064400         Else                                                             
064500           PERFORM MFS-RENSA-RAD                                          
064600         End-If                                                           
064700         ADD 1 TO INDX                                                    
064800       END-PERFORM                                                        
064900                                                                          
065000       IF SEGMENT-FINNS                                                   
065100       AND 4110-IDARTNR = W-IDARTNR                                       
065200         MOVE 4110-IDARTNR            TO MOD-IDARTNR-NEXT                 
065300         MOVE 4110-DAGILTIG-FOM (3:6) TO MOD-TIGILTIG-FOM-NEXT            
065400         MOVE 4110-KDANMORS           TO MOD-KDANMORS-NEXT                
065500         MOVE INF-MORE-INFO-EXISTS    TO MED-IDMFSINF                     
065600       ELSE                                                               
065700         MOVE ZERO                    TO MOD-IDARTNR-NEXT                 
065800         MOVE ZERO                    TO MOD-TIGILTIG-FOM-NEXT            
065900         MOVE ZERO                    TO MOD-KDANMORS-NEXT                
066000         IF MED-IDMFSINF = SPACE                                          
066100           MOVE INF-LAST-PAGE         TO MED-IDMFSINF                     
066200         END-IF                                                           
066300       END-IF                                                             
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800 FA-SOEK-OCH-REDIGERA-RAD-1 SECTION.                                      
066900     MOVE 'FA-SOEK-OCH-REDIGERA-RAD-1' TO WS-SEKTION                      
067000     SKIP2                                                                
067100     Move DLI-IO-AREA-WL410911 TO SPAR-IO-AREA-WL410911                   
067200*    -- Första träffen sparad, se om det finns fler på samma kod          
067300     Move W-IDARTNR     To W-IDARTNR-MAX                                  
067400     Move 4110-KDANMORS To W-KDANMORS                                     
067500                           W-KDANMORS-MAX                                 
067600                                                                          
067700     Perform IMS-GNP-410911-SIST-PER-KOD                                  
067800                                                                          
067900     If SEGMENT-FINNS                                                     
068000*      -- Det fanns fler på samma kod!  Spara för RAD1                    
068100       Move DLI-IO-AREA-WL410911 TO SPAR-IO-AREA-WL410911                 
068200     Else                                                                 
068300*      -- Sista ligger redan i SPAR-                                      
068400       Continue                                                           
068500     End-If                                                               
068600                                                                          
068700     MOVE SPAR-4110-IDARTNR           TO MOD-IDARTNR-RAD1                 
068800     MOVE SPAR-4110-DAGILTIG-FOM (3:6) TO MOD-TIGILTIG-FOM-RAD1           
068900     MOVE SPAR-4110-DAGILTIG-TOM (3:6) TO MOD-TIGILTIG-TOM-RAD1           
069000     MOVE WS-IDFTG                    TO MOD-IDFTG-RAD1                   
069100     MOVE SPAR-4110-IDANALYS          TO MOD-IDANALYS-RAD1                
069200     MOVE SPAR-4110-KDANMORS          TO MOD-KDANMORS-RAD1                
069300     MOVE SPAR-4110-IDKONTO           TO MOD-IDKONTO-RAD1                 
069400     MOVE SPAR-4110-IDKST             TO MOD-IDKST-RAD1                   
069500     MOVE SPAR-4110-IDUSER            TO MOD-IDUSER-RAD1                  
069600     MOVE SPAR-4110-KVART             TO MOD-KVART-RAD1                   
069700                                                                          
069800***  IF MFS-UPDATE AND MID-FLBORT-UPP = '+'                               
069900***    MOVE MFS-ADD-LYS-UPP-FAELT     TO MOD-IDARTNR-ATTR                 
070000***                                      MOD-TIGILTIG-FOM-ATTR            
070100***                                      MOD-TIGILTIG-TOM-ATTR            
070200***                                      MOD-KDANMORS-ATTR                
070300***                                      MOD-IDFTG-ATTR                   
070400***                                      MOD-IDANALYS-ATTR                
070500***                                      MOD-IDKONTO-ATTR                 
070600***                                      MOD-IDKST-ATTR                   
070700***                                      MOD-IDUSER-ATTR                  
070800***  END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100                                                                          
071200 FB-REDIGERA-RAD-2-21 SECTION.                                            
071300     MOVE 'FB-REDIGERA-RAD-2-21'       TO WS-SEKTION                      
071400                                                                          
071500     MOVE SPAR-4110-IDARTNR           TO MOD-IDARTNR (INDX)               
071600     MOVE SPAR-4110-DAGILTIG-FOM (3:6) TO MOD-TIGILTIG-FOM (INDX)         
071700     MOVE SPAR-4110-DAGILTIG-TOM (3:6) TO MOD-TIGILTIG-TOM (INDX)         
071800     MOVE WS-IDFTG                    TO MOD-IDFTG (INDX)                 
071900     MOVE SPAR-4110-KDANMORS          TO MOD-KDANMORS   (INDX)            
072000     MOVE SPAR-4110-IDKONTO           TO MOD-IDKONTO    (INDX)            
072100     MOVE SPAR-4110-IDKST             TO MOD-IDKST      (INDX)            
072200     MOVE SPAR-4110-IDANALYS          TO MOD-IDANALYS   (INDX)            
072300     MOVE SPAR-4110-IDUSER            TO MOD-IDUSER (INDX)                
072400     MOVE SPAR-4110-KVART             TO MOD-KVART (INDX)                 
072500     .                                                                    
072600     EJECT                                                                
072700                                                                          
072800 G-KOLLA-INPUT SECTION.                                                   
072900     MOVE 'G-KOLLA-INPUT'              TO WS-SEKTION                      
073000                                                                          
073100     MOVE JA                          TO INDATA-SW                        
073200     MOVE JA                          TO IDFTG-UPD-SW                     
073300     IF MID-INPUT = ALL '+'                                               
073400       MOVE ERR-PF11-AND-NO-DATA      TO MED-IDMFSFEL                     
073500***    PERFORM MFS-ROER-EJ-FAELT-IN                                       
073600***    PERFORM MFS-ROER-EJ-FAELT-UT                                       
073700       MOVE NEJ                       TO INDATA-SW                        
073800     ELSE                                                                 
073900       PERFORM GA-UPP-RAD-FORMELL-KOLL                                    
074000                                                                          
074100       IF INDATA-FEL                                                      
074200         IF MED-IDMFSFEL = SPACE                                          
074300           IF IDFTG-UPD-FEL                                               
074400             MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                  
074500           ELSE                                                           
074600             MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                  
074700           END-IF                                                         
074800         END-IF                                                           
074900***      PERFORM MFS-ROER-EJ-FAELT-UT                                     
075000***      PERFORM MFS-ROER-EJ-FAELT-IN                                     
075100       ELSE                                                               
075200         IF MID-FLBORT-UPP = 'J' OR 'Y'                                   
075300           PERFORM GB-KOLL-OM-BORTTAG-OK                                  
075400         ELSE                                                             
075500           PERFORM GC-UPP-RAD-LOGISK-KOLL                                 
075600         END-IF                                                           
075700         IF INDATA-FEL                                                    
075800           IF MED-IDMFSFEL = SPACE                                        
075900             IF IDFTG-UPD-FEL                                             
076000               MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                
076100             ELSE                                                         
076200               MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                
076300             END-IF                                                       
076400           END-IF                                                         
076500***        PERFORM MFS-ROER-EJ-FAELT-UT                                   
076600***        PERFORM MFS-ROER-EJ-FAELT-IN                                   
076700         END-IF                                                           
076800       END-IF                                                             
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 GA-UPP-RAD-FORMELL-KOLL SECTION.                                         
077300     MOVE 'GA-UPP-RAD-FORMELL-KOLL'     TO WS-SEKTION                     
077400                                                                          
077500     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
077600       IF MID-IDARTNR-UPP NUMERIC AND                                     
077700          MID-IDARTNR-UPP > ZERO                                          
077800         MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDARTNR-UPP-ATTR             
077900       ELSE                                                               
078000         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDARTNR-UPP-ATTR             
078100         MOVE NEJ                     TO INDATA-SW                        
078200       END-IF                                                             
078300     ELSE                                                                 
078400       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDARTNR-UPP-ATTR             
078500       MOVE NEJ                       TO INDATA-SW                        
078600     END-IF                                                               
078700                                                                          
078800     IF MID-TIGILTIG-FOM-UPP NOT = ALL '+'                                
078900       IF MID-TIGILTIG-FOM-UPP NUMERIC AND                                
079000          MID-TIGILTIG-FOM-UPP > ZERO                                     
079100         PERFORM GAA-KOLLA-FOM-TID                                        
079200       ELSE                                                               
079300         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIGILTIG-FOM-UPP-ATTR        
079400         MOVE NEJ                     TO INDATA-SW                        
079500       END-IF                                                             
079600     ELSE                                                                 
079700       MOVE ZERO                      TO MID-TIGILTIG-FOM-UPP             
079800       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-FOM-UPP-ATTR        
079900       MOVE NEJ                       TO INDATA-SW                        
080000     END-IF                                                               
080100                                                                          
080200     IF MID-TIGILTIG-TOM-UPP NOT = ALL '+'                                
080300       IF MID-TIGILTIG-TOM-UPP NUMERIC AND                                
080400          MID-TIGILTIG-TOM-UPP > ZERO                                     
080500         PERFORM GAB-KOLLA-TOM-TID                                        
080600       ELSE                                                               
080700         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIGILTIG-TOM-UPP-ATTR        
080800         MOVE NEJ                     TO INDATA-SW                        
080900       END-IF                                                             
081000     ELSE                                                                 
081100       MOVE ZERO                      TO MID-TIGILTIG-TOM-UPP             
081200       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-TOM-UPP-ATTR        
081300       MOVE NEJ                       TO INDATA-SW                        
081400     END-IF                                                               
081500                                                                          
081600                                                                          
081700     IF MID-IDFTG-UPP NOT = ALL '+'                                       
081800                                                                          
081900       IF MFS-UPD-V                                                       
082000         IF MID-IDFTG-UPP NUMERIC AND                                     
082100            MID-IDFTG-UPP > ZERO AND                                      
082200            MID-IDFTG-UPP = WS-IDFTG                                      
082300           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDFTG-UPP-ATTR               
082400         ELSE                                                             
082500           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDFTG-UPP-ATTR               
082600           MOVE NEJ                   TO INDATA-SW                        
082700           MOVE NEJ                   TO IDFTG-UPD-SW                     
082800         END-IF                                                           
082900       ELSE                                                               
083000         IF MID-IDFTG-UPP NUMERIC                                         
083100         AND MID-IDFTG-UPP > ZERO                                         
083200         AND MID-IDFTG-UPP = WS-IDFTG                                     
083300           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDFTG-UPP-ATTR               
083400         ELSE                                                             
083500           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDFTG-UPP-ATTR               
083600           MOVE NEJ                   TO INDATA-SW                        
083700           MOVE NEJ                   TO IDFTG-UPD-SW                     
083800         END-IF                                                           
083900       END-IF                                                             
084000     ELSE                                                                 
084100       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDFTG-UPP-ATTR               
084200       MOVE NEJ                       TO INDATA-SW                        
084300     END-IF                                                               
084400                                                                          
084500     IF MID-KDANMORS-UPP NOT = ALL '+'                                    
084600       IF MID-KDANMORS-UPP NUMERIC                                        
084700         MOVE MFS-NUM-FAELT-RAETT     TO MOD-KDANMORS-UPP-ATTR            
084800       ELSE                                                               
084900         MOVE MFS-NUM-FAELT-FEL       TO MOD-KDANMORS-UPP-ATTR            
085000         MOVE NEJ                     TO INDATA-SW                        
085100       END-IF                                                             
085200     ELSE                                                                 
085300       MOVE MFS-NUM-FAELT-FEL         TO MOD-KDANMORS-UPP-ATTR            
085400       MOVE NEJ                       TO INDATA-SW                        
085500     END-IF                                                               
085600                                                                          
085700     MOVE ZERO                      TO W-IDKONTO                          
085800     MOVE SPACE                     TO W-IDPROFIT                         
085900                                       W-IDANALYS                         
086000                                       W-IDKST                            
086100                                                                          
086200     IF MID-IDANALYS-UPP NOT = ALL '+'                                    
086300       IF MID-IDANALYS-UPP NUMERIC AND                                    
086400          MID-IDANALYS-UPP > ZERO                                         
086500          MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDANALYS-UPP-ATTR            
086600          MOVE MID-IDANALYS-UPP       TO W-IDANALYS                       
086700       ELSE                                                               
086800         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDANALYS-UPP-ATTR            
086900         MOVE NEJ                     TO INDATA-SW                        
087000         MOVE SPACE                   TO W-IDANALYS                       
087100       END-IF                                                             
087200     ELSE                                                                 
087300       MOVE SPACE                     TO W-IDANALYS                       
087400       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDANALYS-UPP-ATTR            
087500       MOVE NEJ                       TO INDATA-SW                        
087600     END-IF                                                               
087700                                                                          
087800     IF (MID-KDANMORS-UPP = '53' AND WS-IDFTG = 57)                       
087900     OR (MID-KDANMORS-UPP = '55' AND WS-IDFTG = 57)                       
088000        IF MID-IDKONTO-UPP NOT = ALL '+'                                  
088100          IF MID-IDKONTO-UPP NUMERIC AND                                  
088200             MID-IDKONTO-UPP > ZERO                                       
088300             MOVE MID-IDKONTO-UPP     TO W-IDKONTO                        
088400             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-UPP-ATTR             
088500          ELSE                                                            
088600             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKONTO-UPP-ATTR            
088700             MOVE NEJ                  TO INDATA-SW                       
088800          END-IF                                                          
088900        ELSE                                                              
089000           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKONTO-UPP-ATTR            
089100           MOVE NEJ                    TO INDATA-SW                       
089200        END-IF                                                            
089300        IF MID-IDKST-UPP NOT = ALL '+'                                    
089400           IF MID-IDKST-UPP > space                                       
089500             MOVE MID-IDKST-UPP       TO W-IDKST                          
089600             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKST-UPP-ATTR               
089700           ELSE                                                           
089800             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-UPP-ATTR               
089900             MOVE NEJ                 TO INDATA-SW                        
090000           END-IF                                                         
090100        ELSE                                                              
090200           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKST-UPP-ATTR               
090300           MOVE NEJ                   TO INDATA-SW                        
090400        END-IF                                                            
090500******************************************                                
090600**** VALIDERAR SAP R3 MODULEN W411SAP ****                                
090700******************************************                                
090800        IF INDATA-OK                                                      
090900           MOVE MSGI-IDFTG   TO WS-IDFTG                                  
091000                                WS-IDFTG-B6                               
091810           IF IDFTG-NON-VCC                                               
091820             PERFORM IMS-GU-WDB601-FTG                                    
091830             IF SEGMENT-FINNS                                             
091840               MOVE DCS-KDTRADP    TO SAP-KDTRADP                         
091841             ELSE                                                         
091842               MOVE SPACES         TO SAP-KDTRADP                         
091850             END-IF                                                       
091860           ELSE                                                           
091870             MOVE 'SEPV'           TO SAP-KDTRADP                         
091880           END-IF                                                         
091900           MOVE W-IDKST            TO SAP-IDKST                           
092000           MOVE W-IDKONTO          TO SAP-IDKONTO                         
092100           MOVE W-IDANALYS         TO SAP-IDANALYS                        
092200           MOVE ZERO               TO SAP-IDDISTR                         
092300           MOVE MID-IDFTG-UPP      TO SAP-IDFTG                           
092400           MOVE SPACE              TO SAP-IDPROFIT                        
092500           MOVE SPACE              TO SAP-KDFAKTYP                        
092600           MOVE +2                 TO SAP-KDCALL                          
092700                                                                          
092800           CALL W411SAP USING SAP-W411SAP  SAPC-PCB                       
092900           IF SAP-BEFEL NOT = SPACE                                       
093000              IF SAP-IDFTG-OK = NEJ                                       
093100                 MOVE NEJ                TO INDATA-SW                     
093200              END-IF                                                      
093300                                                                          
093400              IF SAP-IDKST-OK = NEJ                                       
093500                IF SAP-BEFEL = 'COSTCENTER NOT ALLOWED'                   
093600* KST FÅR EJ ANGES OM KONTOTS FLAGGA FÖR KST ÄR AVSLAGEN.                 
093700* SE I PGM W411SAP. ÄNDRING BEGÄRD AV TUULA 030422. DET BETYDER           
093800* ATT VISSA KONTON + KOD 53 BARA HAR ANALYSNR. (T.EX. 481312 )            
093900                  MOVE space             TO MID-IDKST-UPP                 
094000                  MOVE SPACE             TO SAP-BEFEL                     
094100                ELSE                                                      
094200                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDKST-UPP-ATTR            
094300                  MOVE NEJ               TO INDATA-SW                     
094400                END-IF                                                    
094500              END-IF                                                      
094600                                                                          
094700              IF SAP-IDKONTO-OK = NEJ                                     
094800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-UPP-ATTR           
094900                 MOVE NEJ               TO INDATA-SW                      
095000              END-IF                                                      
095100                                                                          
095200              IF SAP-IDANALYS-OK = NEJ                                    
095300                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDANALYS-UPP-ATTR          
095400                 MOVE NEJ               TO INDATA-SW                      
095500              END-IF                                                      
095600              MOVE SAP-BEFEL TO MOD-TEMFSINF                              
095700           END-IF                                                         
095800        END-IF                                                            
095900     ELSE                                                                 
096000        IF MID-KDANMORS-UPP = '52'                                        
096100        OR MID-KDANMORS-UPP = '53'                                        
096200        OR MID-KDANMORS-UPP = '54'                                        
096300        OR MID-KDANMORS-UPP = '55'                                        
096400           MOVE MFS-RENSA-FAELT         TO MOD-IDKONTO-UPP                
096500                                           MOD-IDKST-UPP                  
096600        ELSE                                                              
096700           MOVE MFS-NUM-FAELT-FEL       TO MOD-KDANMORS-UPP-ATTR          
096800           MOVE NEJ                     TO INDATA-SW                      
096900        END-IF                                                            
097000     END-IF                                                               
097100                                                                          
097200                                                                          
097300     IF MID-FLBORT-UPP = '+'                                              
097400       IF MID-IDUSER-UPP NOT = ALL '+'                                    
097500         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDUSER-UPP-ATTR              
097600       ELSE                                                               
097700         MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDUSER-UPP-ATTR              
097800         MOVE NEJ                     TO INDATA-SW                        
097900       END-IF                                                             
098000     END-IF                                                               
098100                                                                          
098200     IF MID-FLBORT-UPP = '+' OR 'J' OR 'Y'                                
098300       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLBORT-UPP-ATTR              
098400     ELSE                                                                 
098500       MOVE MFS-ALFA-FAELT-FEL        TO MOD-FLBORT-UPP-ATTR              
098600       MOVE NEJ                       TO INDATA-SW                        
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000 GAA-KOLLA-FOM-TID SECTION.                                               
099100     MOVE 'GAA-KOLLA-FOM-TID'           TO WS-SEKTION                     
099200                                                                          
099300     MOVE 'AAMMDD'                    TO DAT-KDDATFORM                    
099400     MOVE MID-TIGILTIG-FOM-UPP        TO DAT-I-TIDATUM                    
099500                                                                          
099600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
099700                         DAT-O-TIDATUM DAT-KDSVAR                         
099800                                                                          
099900     IF DAT-KDSVAR-OK                                                     
100000       MOVE MFS-NUM-FAELT-RAETT       TO MOD-TIGILTIG-FOM-UPP-ATTR        
100100     ELSE                                                                 
100200       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-FOM-UPP-ATTR        
100300       MOVE NEJ                       TO INDATA-SW                        
100400     END-IF                                                               
100500     .                                                                    
100600     EJECT                                                                
100700 GAB-KOLLA-TOM-TID SECTION.                                               
100800     MOVE 'GAB-KOLLA-TOM-TID'           TO WS-SEKTION                     
100900                                                                          
101000     MOVE 'AAMMDD'                    TO DAT-KDDATFORM                    
101100     MOVE MID-TIGILTIG-TOM-UPP        TO DAT-I-TIDATUM                    
101200                                                                          
101300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
101400                         DAT-O-TIDATUM DAT-KDSVAR                         
101500                                                                          
101600     IF DAT-KDSVAR-OK                                                     
101700       MOVE MID-TIGILTIG-TOM-UPP      TO TMP1-YYMMDD                      
101800       MOVE MID-TIGILTIG-FOM-UPP      TO TMP2-YYMMDD                      
101900       PERFORM WY2000Q1                                                   
102000       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
102100         MOVE ERR-END-VALUE           TO MED-IDMFSFEL                     
102200         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIGILTIG-TOM-UPP-ATTR        
102300         MOVE NEJ                     TO INDATA-SW                        
102400       ELSE                                                               
102500         MOVE MFS-NUM-FAELT-RAETT     TO MOD-TIGILTIG-TOM-UPP-ATTR        
102600       END-IF                                                             
102700     ELSE                                                                 
102800       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIGILTIG-TOM-UPP-ATTR        
102900       MOVE NEJ                       TO INDATA-SW                        
103000     END-IF                                                               
103100     .                                                                    
103200     EJECT                                                                
103300 GB-KOLL-OM-BORTTAG-OK SECTION.                                           
103400     MOVE 'GB-KOLLA-OM-BORTTAG-OK'    TO WS-SEKTION                       
103500                                                                          
103600     PERFORM IMS-GU-410901                                                
103700     MOVE MID-IDARTNR-UPP             TO W-IDARTNR                        
103800     MOVE MID-KDANMORS-UPP            TO W-KDANMORS                       
103900     MOVE MID-TIGILTIG-FOM-UPP        TO W-DAGILTIG-FOM                   
104000     IF MID-TIGILTIG-FOM-UPP NOT = ZERO                                   
104100       IF MID-TIGILTIG-FOM-UPP < 500000                                   
104200         MOVE 20                     TO W-DAGILTIG-FOM (1:2)              
104300       ELSE                                                               
104400         IF MID-TIGILTIG-FOM-UPP < 999999                                 
104500           MOVE 19                   TO W-DAGILTIG-FOM (1:2)              
104600         ELSE                                                             
104700           MOVE 99999999             TO W-DAGILTIG-FOM                    
104800         END-IF                                                           
104900       END-IF                                                             
105000     END-IF                                                               
105100     PERFORM IMS-GHNP-410911                                              
105200     IF SEGMENT-FINNS                                                     
105300       IF 4110-KVART > +0                                                 
105400         MOVE MFS-NUM-FAELT-FEL       TO MOD-FLBORT-UPP-ATTR              
105500         MOVE NEJ                     TO INDATA-SW                        
105600       ELSE                                                               
105700         IF MID-IDANALYS-UPP NOT = 4110-IDANALYS                          
105800           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDANALYS-UPP-ATTR            
105900           MOVE NEJ                   TO INDATA-SW                        
106000         ELSE                                                             
106100           IF (MID-KDANMORS-UPP = '53' AND WS-IDFTG = 57)                 
106200           OR (MID-KDANMORS-UPP = '55' AND WS-IDFTG = 57)                 
106300                                                                          
106400              IF MID-IDKONTO-UPP NUMERIC                                  
106500                 MOVE MID-IDKONTO-UPP TO WS-IDKONTO                       
106600                 IF WS-IDKONTO     NOT = 4110-IDKONTO                     
106700                   MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-UPP-ATTR        
106800                   MOVE NEJ                TO INDATA-SW                   
106900                 END-IF                                                   
107000              ELSE                                                        
107100                 MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-UPP-ATTR          
107200                 MOVE NEJ                TO INDATA-SW                     
107300              END-IF                                                      
107400                                                                          
107500              IF MID-IDKST-UPP NOT = 4110-IDKST                           
107600                 MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKST-UPP-ATTR            
107700                 MOVE NEJ                TO INDATA-SW                     
107800              END-IF                                                      
107900                                                                          
108000           END-IF                                                         
108100                                                                          
108200           IF NOT INDATA-FEL                                              
108300                                                                          
108400              IF MFS-UPD-V                                                
108500                IF MID-IDFTG-UPP NOT = WS-IDFTG                           
108600                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-UPP-ATTR            
108700                  MOVE NEJ               TO INDATA-SW                     
108800                  MOVE NEJ               TO IDFTG-UPD-SW                  
108900                END-IF                                                    
109000              ELSE                                                        
109100                IF MID-IDFTG-UPP NOT = MSGI-IDFTG                         
109200                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-UPP-ATTR            
109300                  MOVE NEJ               TO INDATA-SW                     
109400                  MOVE NEJ               TO IDFTG-UPD-SW                  
109500                END-IF                                                    
109600              END-IF                                                      
109700           END-IF                                                         
109800         END-IF                                                           
109900       END-IF                                                             
110000     ELSE                                                                 
110100                                                                          
110200       MOVE MFS-NUM-FAELT-FEL         TO MOD-IDARTNR-UPP-ATTR             
110300       MOVE NEJ                       TO INDATA-SW                        
110400     END-IF                                                               
110500     .                                                                    
110600     EJECT                                                                
110700 GC-UPP-RAD-LOGISK-KOLL SECTION.                                          
110800     MOVE 'GC-UPP-RAD-LOGISKA-KOLL'   TO WS-SEKTION                       
110900                                                                          
111000     IF MID-IDARTNR-UPP NOT = W-IDARTNR                                   
111100       MOVE MID-IDARTNR-UPP           TO W-IDARTNR-D6                     
111200       PERFORM IMS-GET-ARTC01                                             
111300       IF SEGMENT-SAKNAS                                                  
111400         MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                     
111500         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDARTNR-UPP-ATTR             
111600         MOVE NEJ                     TO INDATA-SW                        
111700       END-IF                                                             
111800     END-IF                                                               
111900                                                                          
112000     IF INDATA-OK                                                         
112100       PERFORM GCA-KOLLA-INTERVALL                                        
112200     END-IF                                                               
112300     .                                                                    
112400     EJECT                                                                
112500 GCA-KOLLA-INTERVALL SECTION.                                             
112600     MOVE 'GCA-KOLLA-INTERVALL'        TO WS-SEKTION                      
112700                                                                          
112800     MOVE NEJ                           TO FEL                            
112900     PERFORM GCAA-LAES-IN-I-TABELL                                        
113000     MOVE +1                            TO IX                             
113100     PERFORM UNTIL IX > MAX-IX                                            
113200        MOVE MID-TIGILTIG-FOM-UPP       TO TMP1-YYMMDD                    
113300        MOVE MID-TIGILTIG-TOM-UPP       TO TMP2-YYMMDD                    
113400        MOVE DAGENS-DATUM               TO TMP3-YYMMDD                    
113500        MOVE TAB-TIGILTIG-TOM (IX)      TO TMP4-YYMMDD                    
113600        PERFORM WY2000Q1                                                  
113700        IF  TMP1-YYMMDD           >  TMP4-YYMMDD            AND           
113800            TMP1-YYMMDD           >= TMP3-YYMMDD            AND           
113900            TMP2-YYMMDD           >= TMP3-YYMMDD                          
114000*       IF  MID-FOM  >  BAS-TOM                                           
114100*       AND MID-FOM  >= IDAG                                              
114200*       AND MID-TOM  >= IDAG                                              
114300           MOVE JA                      TO SW-TIGILTIG-FINNS              
114400        ELSE                                                              
114500           MOVE NEJ                     TO INDATA-SW                      
114600        END-IF                                                            
114700        ADD +1                          TO IX                             
114800     END-PERFORM                                                          
114900                                                                          
115000     IF INDATA-FEL                                                        
115100       MOVE +1                          TO IX                             
115200       PERFORM UNTIL IX > MAX-IX                                          
115300          MOVE MID-TIGILTIG-FOM-UPP     TO TMP1-YYMMDD                    
115400          MOVE MID-TIGILTIG-TOM-UPP     TO TMP2-YYMMDD                    
115500          MOVE DAGENS-DATUM             TO TMP3-YYMMDD                    
115600          MOVE TAB-TIGILTIG-FOM (IX)    TO TMP4-YYMMDD                    
115700          PERFORM WY2000Q1                                                
115800          IF TMP1-YYMMDD          =  TMP4-YYMMDD           AND            
115900             TMP2-YYMMDD          >= TMP3-YYMMDD           AND            
116000             TAB-KVART (IX) > +0                                          
116100*         IF  MID-FOM  = BAS-FOM                                          
116200*         AND MID-TOM >= IDAG                                             
116300*         AND BAS-KVART > 0                                               
116400             MOVE JA                    TO INDATA-SW                      
116500             MOVE JA                    TO SW-TIGILTIG-FINNS              
116600          ELSE                                                            
116700             MOVE MID-TIGILTIG-TOM-UPP  TO TMP1-YYMMDD                    
116800             MOVE TAB-TIGILTIG-FOM (IX) TO TMP2-YYMMDD                    
116900             MOVE TAB-TIGILTIG-TOM (IX) TO TMP3-YYMMDD                    
117000             PERFORM WY2000Q1                                             
117100             IF TMP1-YYMMDD          >= TMP2-YYMMDD           AND         
117200                TMP1-YYMMDD          <  TMP3-YYMMDD                       
117300*            IF  MID-TOM  >= BAS-FOM                                      
117400*            AND MID-TOM  <  BAS-TOM                                      
117500                MOVE JA                 TO FEL                            
117600             END-IF                                                       
117700          END-IF                                                          
117800          ADD +1                        TO IX                             
117900       END-PERFORM                                                        
118000                                                                          
118100       IF TIGILTIG-FINNS                                                  
118200          MOVE JA                       TO INDATA-SW                      
118300       END-IF                                                             
118400                                                                          
118500       IF FEL = JA                                                        
118600          MOVE NEJ                      TO INDATA-SW                      
118700       END-IF                                                             
118800                                                                          
118900       IF INDATA-OK                                                       
119000          CONTINUE                                                        
119100       ELSE                                                               
119200          MOVE ERR-CORR-HILITE-FLDS     TO MED-IDMFSFEL                   
119300                                                                          
119400          MOVE MFS-NUM-FAELT-FEL     TO MOD-TIGILTIG-FOM-UPP-ATTR         
119500                                         MOD-TIGILTIG-TOM-UPP-ATTR        
119600*         PERFORM MFS-ROER-EJ-FAELT-UT                                    
119700*         PERFORM MFS-ROER-EJ-FAELT-IN                                    
119800       END-IF                                                             
119900     END-IF                                                               
120000     .                                                                    
120100     EJECT                                                                
120200 GCAA-LAES-IN-I-TABELL SECTION.                                           
120300     MOVE 'GCAA-LAES-IN-I-TABELL'       TO WS-SEKTION                     
120400                                                                          
120500     MOVE +1                         TO IX                                
120600     MOVE MID-IDARTNR-UPP            TO W-IDARTNR                         
120700                                        W-IDARTNR-MAX                     
120800     MOVE MID-KDANMORS-UPP           TO W-KDANMORS                        
120900                                        W-KDANMORS-MAX                    
121000     PERFORM IMS-GU-410901                                                
121100     PERFORM IMS-GNP-410911-KVAL                                          
121200     PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-IX                          
121300        MOVE 4110-DAGILTIG-FOM (3:6) TO TAB-TIGILTIG-FOM (IX)             
121400        MOVE 4110-DAGILTIG-TOM (3:6) TO TAB-TIGILTIG-TOM (IX)             
121500        MOVE 4110-KVART              TO TAB-KVART        (IX)             
121600        MOVE MID-TIGILTIG-TOM-UPP    TO TMP1-YYMMDD                       
121700        MOVE MID-TIGILTIG-FOM-UPP    TO TMP2-YYMMDD                       
121800        MOVE TAB-TIGILTIG-TOM (IX)   TO TMP3-YYMMDD                       
121900        MOVE TAB-TIGILTIG-FOM (IX)   TO TMP4-YYMMDD                       
122000        PERFORM WY2000Q1                                                  
122100                                                                          
122200        IF (TMP4-YYMMDD          > TMP2-YYMMDD  AND                       
122300            TMP4-YYMMDD          < TMP1-YYMMDD) OR                        
122400           (TMP3-YYMMDD         >= TMP2-YYMMDD  AND                       
122500            TMP3-YYMMDD         <= TMP1-YYMMDD)                           
122600*       IF    (BAS-FOM  >  MID-FOM                                        
122700*          AND BAS-FOM  <  MID-TOM )                                      
122800*       OR    (BAS-TOM >=  MID-FOM                                        
122900*          AND BAS-TOM <=  MID-TOM )                                      
123000           MOVE JA                   TO FEL                               
123100        END-IF                                                            
123200        PERFORM IMS-GNP-410911-KVAL                                       
123300        ADD +1                       TO IX                                
123400     END-PERFORM                                                          
123500     .                                                                    
123600     EJECT                                                                
123700 H-UPPDATERA SECTION.                                                     
123800     MOVE 'H-UPPDATERA'                 TO WS-SEKTION                     
123900                                                                          
124000     MOVE MID-IDARTNR-UPP            TO W-IDARTNR                         
124100                                        W-IDARTNR-MAX                     
124200     MOVE MID-KDANMORS-UPP           TO W-KDANMORS                        
124300     MOVE MID-TIGILTIG-FOM-UPP       TO W-DAGILTIG-FOM                    
124400     IF MID-TIGILTIG-FOM-UPP NOT = ZERO                                   
124500       IF MID-TIGILTIG-FOM-UPP < 500000                                   
124600         MOVE 20                     TO W-DAGILTIG-FOM (1:2)              
124700       ELSE                                                               
124800         IF MID-TIGILTIG-FOM-UPP < 999999                                 
124900           MOVE 19                   TO W-DAGILTIG-FOM (1:2)              
125000         ELSE                                                             
125100           MOVE 99999999             TO W-DAGILTIG-FOM                    
125200         END-IF                                                           
125300       END-IF                                                             
125400     END-IF                                                               
125500     PERFORM IMS-GHNP-410911-KVAL-FIRST                                   
125600     IF SEGMENT-FINNS                                                     
125700        IF MID-FLBORT-UPP = 'J' OR 'Y'                                    
125800          PERFORM IMS-DLET-4109                                           
125900        ELSE                                                              
126000*************** ÄNDRAR PÅ BEFINTLIGT DOKUMENT **********                  
126100          IF TIGILTIG-FINNS                                               
126200            MOVE MID-TIGILTIG-TOM-UPP TO 4110-DAGILTIG-TOM                
126300            IF MID-TIGILTIG-TOM-UPP NOT = ZERO                            
126400              IF MID-TIGILTIG-TOM-UPP < 500000                            
126500                MOVE 20               TO 4110-DAGILTIG-TOM (1:2)          
126600              ELSE                                                        
126700                IF MID-TIGILTIG-TOM-UPP < 999999                          
126800                  MOVE 19             TO 4110-DAGILTIG-TOM (1:2)          
126900                ELSE                                                      
127000                  MOVE 99999999       TO 4110-DAGILTIG-TOM                
127100                END-IF                                                    
127200              END-IF                                                      
127300            END-IF                                                        
127400                                                                          
127500            PERFORM IMS-REPL-410911                                       
127600          END-IF                                                          
127700        END-IF                                                            
127800     ELSE                                                                 
127900*************** FÖRSTA GÅNGEN RADEN LÄGGS UPP **********                  
128000       MOVE MID-IDARTNR-UPP          TO 4110-IDARTNR                      
128100       MOVE MID-TIGILTIG-FOM-UPP     TO 4110-DAGILTIG-FOM                 
128200                                        W-DAGILTIG-FOM                    
128300       IF MID-TIGILTIG-FOM-UPP NOT = ZERO                                 
128400         IF MID-TIGILTIG-FOM-UPP < 500000                                 
128500           MOVE 20                   TO 4110-DAGILTIG-FOM (1:2)           
128600                                        W-DAGILTIG-FOM (1:2)              
128700         ELSE                                                             
128800           IF MID-TIGILTIG-FOM-UPP < 999999                               
128900             MOVE 19                 TO 4110-DAGILTIG-FOM (1:2)           
129000                                        W-DAGILTIG-FOM (1:2)              
129100           ELSE                                                           
129200             MOVE 99999999           TO 4110-DAGILTIG-FOM                 
129300                                        W-DAGILTIG-FOM                    
129400           END-IF                                                         
129500         END-IF                                                           
129600       END-IF                                                             
129700                                                                          
129800       MOVE MID-KDANMORS-UPP         TO 4110-KDANMORS                     
129900                                                                          
130000       MOVE MID-IDANALYS-UPP         TO 4110-IDANALYS                     
130100                                                                          
130200       IF (MID-KDANMORS-UPP = '53' AND WS-IDFTG = 57)                     
130300       OR (MID-KDANMORS-UPP = '55' AND WS-IDFTG = 57)                     
130400          MOVE MID-IDKONTO-UPP       TO 4110-IDKONTO                      
130500          MOVE MID-IDKST-UPP         TO 4110-IDKST                        
130600       ELSE                                                               
130700          IF MID-KDANMORS-UPP = '52'                                      
130800             MOVE '0000000000'       TO 4110-IDKONTO                      
130900             MOVE space              TO 4110-IDKST                        
131000          ELSE                                                            
131100             MOVE '0000000000'       TO 4110-IDKONTO                      
131200             MOVE space              TO 4110-IDKST                        
131300          END-IF                                                          
131400       END-IF                                                             
131500       MOVE MID-IDUSER-UPP           TO 4110-IDUSER                       
131600       MOVE +0                       TO 4110-KVART                        
131700       MOVE MID-TIGILTIG-TOM-UPP     TO 4110-DAGILTIG-TOM                 
131800       IF MID-TIGILTIG-TOM-UPP NOT = ZERO                                 
131900         IF MID-TIGILTIG-TOM-UPP < 500000                                 
132000           MOVE 20                   TO 4110-DAGILTIG-TOM (1:2)           
132100         ELSE                                                             
132200           IF MID-TIGILTIG-TOM-UPP < 999999                               
132300             MOVE 19                 TO 4110-DAGILTIG-TOM (1:2)           
132400           ELSE                                                           
132500             MOVE 99999999           TO 4110-DAGILTIG-TOM                 
132600           END-IF                                                         
132700         END-IF                                                           
132800       END-IF                                                             
132900                                                                          
133000       PERFORM IMS-ISRT-410911                                            
133100     END-IF                                                               
133200     MOVE INF-UPDATE-DONE            TO MED-IDMFSINF                      
133300     PERFORM MFS-RENSA-FAELT-IN                                           
133400     .                                                                    
133500     EJECT                                                                
133600 Z-FINIT SECTION.                                                         
133700     MOVE 'Z-FINIT'                     TO WS-SEKTION                     
133800                                                                          
133900     IF MED-IDMFSINF NOT = SPACE                                          
134000*    OR MED-IDMFSFEL NOT = SPACE                                          
134100       CALL WMEDKONV USING MED-WMEDAREA                                   
134200*      MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
134300       MOVE MED-MFSINF                TO MOD-TEMFSINF                     
134400     END-IF                                                               
134500                                                                          
134600     if art-fel = nej                                                     
134700     and ftg-fel = nej                                                    
134800       IF MED-IDMFSFEL NOT = SPACE                                        
134900         CALL WMEDKONV USING MED-WMEDAREA                                 
135000         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
135100       END-IF                                                             
135200     end-if                                                               
135300                                                                          
135400     IF SAP-BEFEL NOT = SPACE                                             
135500       MOVE SAP-BEFEL TO MOD-TEMFSINF                                     
135600     END-IF                                                               
135700                                                                          
135800     MOVE MAX-MOD-LAENGD              TO MSG-KVLL                         
135900     PERFORM IMS-INSERT-MSG                                               
136000     .                                                                    
136100     EJECT                                                                
136200 MFS-RENSA-FAELT-UT SECTION.                                              
136300     MOVE 'MFS-RENSA-FAELT-UT'          TO WS-SEKTION                     
136400                                                                          
136500*    --- ALLA UTDATA-FÄLT                                                 
136600*    --- INKL. BLÄDDRINGSNYCKLAR                                          
136700     MOVE ZERO            TO MOD-IDARTNR-ENTER                            
136800                             MOD-IDARTNR-NEXT                             
136900                             MOD-TIGILTIG-FOM-ENTER                       
137000                             MOD-TIGILTIG-FOM-NEXT                        
137100                             MOD-KDANMORS-ENTER                           
137200                             MOD-KDANMORS-NEXT                            
137300                             MOD-IDARTNR-RAD1                             
137400                             MOD-TIGILTIG-FOM-RAD1                        
137500                             MOD-TIGILTIG-TOM-RAD1                        
137600                             MOD-IDFTG-RAD1                               
137700                             MOD-IDANALYS-RAD1                            
137800                             MOD-IDKONTO-RAD1                             
137900                             MOD-KVART-RAD1                               
138000     MOVE SPACE           TO MOD-KDANMORS-RAD1                            
138100                             MOD-IDKST-RAD1                               
138200                             MOD-IDUSER-RAD1                              
138300     MOVE +1 TO INDX                                                      
138400     PERFORM UNTIL INDX > MAX-INDX                                        
138500       PERFORM MFS-RENSA-RAD                                              
138600       ADD +1 TO INDX                                                     
138700     END-PERFORM                                                          
138800     .                                                                    
138900     SKIP2                                                                
139000 MFS-RENSA-RAD SECTION.                                                   
139100     MOVE 'MFS-RENSA-RAD'               TO WS-SEKTION                     
139200     MOVE ZERO            TO MOD-IDARTNR(INDX)                            
139300                             MOD-TIGILTIG-FOM(INDX)                       
139400                             MOD-TIGILTIG-TOM(INDX)                       
139500                             MOD-IDFTG(INDX)                              
139600                             MOD-IDKONTO(INDX)                            
139700                             MOD-IDANALYS(INDX)                           
139800                             MOD-KVART(INDX)                              
139900     MOVE SPACE         to   MOD-KDANMORS(INDX)                           
140000                             MOD-IDUSER(INDX)                             
140100                             MOD-IDKST(INDX)                              
140200     .                                                                    
140300     EJECT                                                                
140400 MFS-RENSA-FAELT-IN SECTION.                                              
140500     MOVE 'MFS-RENSA-FAELT-IN'          TO WS-SEKTION                     
140600                                                                          
140700*    --- ALLA INDATA-FÄLT                                                 
140800     MOVE ZERO              TO MOD-IDARTNR-UPP                            
140900                               MOD-TIGILTIG-FOM-UPP                       
141000                               MOD-TIGILTIG-TOM-UPP                       
141100                               MOD-IDFTG-UPP                              
141200                               MOD-IDKONTO-UPP                            
141300                               MOD-IDANALYS-UPP                           
141400     MOVE SPACE             TO MOD-KDANMORS-UPP                           
141500                               MOD-IDUSER-UPP                             
141600                               MOD-IDKST-UPP                              
141700                               MOD-FLBORT-UPP                             
141800     .                                                                    
141900     EJECT                                                                
142000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
142100     MOVE 'MFS-ROER-EJ-FAELT-UT'        TO WS-SEKTION                     
142200*                                                                         
142300*    --- ALLA UTDATA-FÄLT                                                 
142400*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD1                           
142500*                              MOD-TIGILTIG-FOM-RAD1                      
142600*                              MOD-TIGILTIG-TOM-RAD1                      
142700*                              MOD-IDFTG-RAD1                             
142800*                              MOD-KDANMORS-RAD1                          
142900*                              MOD-IDANALYS-RAD1                          
143000*                              MOD-IDKONTO-RAD1                           
143100*                              MOD-IDKST-RAD1                             
143200*                              MOD-IDUSER-RAD1                            
143300*                              MOD-KVART-RAD1                             
143400*    MOVE +1 TO INDX                                                      
143500*    PERFORM UNTIL INDX > MAX-INDX                                        
143600*      MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(INDX)                        
143700*                                MOD-TIGILTIG-FOM(INDX)                   
143800*                                MOD-TIGILTIG-TOM(INDX)                   
143900*                                MOD-IDFTG(INDX)                          
144000*                                MOD-KDANMORS(INDX)                       
144100*                                MOD-IDANALYS(INDX)                       
144200*                                MOD-IDKONTO(INDX)                        
144300*                                MOD-IDKST(INDX)                          
144400*                                MOD-IDUSER(INDX)                         
144500*                                MOD-KVART(INDX)                          
144600*      ADD +1 TO INDX                                                     
144700*    END-PERFORM                                                          
144800     .                                                                    
144900     SKIP3                                                                
145000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
145100     MOVE 'MFS-ROER-EJ-FAELT-IN'        TO WS-SEKTION                     
145200*                                                                         
145300*    --- ALLA INDATA-FÄLT                                                 
145400*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPP                            
145500*                              MOD-TIGILTIG-FOM-UPP                       
145600*                              MOD-TIGILTIG-TOM-UPP                       
145700*                              MOD-IDFTG-UPP                              
145800*                              MOD-KDANMORS-UPP                           
145900*                              MOD-IDANALYS-UPP                           
146000*                              MOD-IDKONTO-UPP                            
146100*                              MOD-IDKST-UPP                              
146200*                              MOD-IDUSER-UPP                             
146300*                              MOD-FLBORT-UPP                             
146400     .                                                                    
146500     EJECT                                                                
146600* --- IMS SEKTIONER ---                                                   
146700     SKIP3                                                                
146800 IMS-GET-MSG SECTION.                                                     
146900                                                                          
147000     MOVE '  QC' TO GODK-STATUSKODER                                      
147100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
147200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
147300     PERFORM IMS-STATUSKONTROLL                                           
147400     .                                                                    
147500     SKIP3                                                                
147600 IMS-INSERT-MSG SECTION.                                                  
147700                                                                          
147800*    IF MSGI-IDLAND-SPR = 'GB'                                            
147900*      MOVE 'N' TO MFS-KDHUVOMR                                           
148000*    END-IF                                                               
148100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
148200     MOVE SPACE TO GODK-STATUSKODER                                       
148300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
148400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700     EJECT                                                                
148800 IMS-GU-410901 SECTION.                                                   
148900     MOVE 'IMS-GU-410901'      TO WS-IMS-SEKTION                          
149000                                                                          
149100     STRING 'WL410901(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
149200          DELIMITED BY SIZE INTO SSA1                                     
149300     MOVE '    ' TO GODK-STATUSKODER                                      
149400     CALL CBLTDLI USING GU 4109-PCB DLI-IO-AREA-WL410901 SSA1             
149500     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
149600     PERFORM IMS-STATUSKONTROLL                                           
149700     .                                                                    
149800     SKIP3                                                                
149900 IMS-GHNP-410911 SECTION.                                                 
150000     MOVE 'IMS-GHNP-410911'      TO WS-IMS-SEKTION                        
150100                                                                          
150200     STRING 'WL410911(KEY4110  =' W-KEY4110-X ')'                         
150300          DELIMITED BY SIZE INTO SSA1                                     
150400     MOVE '  GE' TO GODK-STATUSKODER                                      
150500     CALL CBLTDLI USING GHNP 4109-PCB DLI-IO-AREA-WL410911 SSA1           
150600     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900     SKIP3                                                                
151000 IMS-GNP-410911 SECTION.                                                  
151100     MOVE 'IMS-GNP-410911'      TO WS-IMS-SEKTION                         
151200                                                                          
151300     STRING 'WL410911(KEY4110 =>' W-KEY4110-X ')'                         
151400          DELIMITED BY SIZE INTO SSA1                                     
151500     MOVE '  GE' TO GODK-STATUSKODER                                      
151600     CALL CBLTDLI USING GNP 4109-PCB DLI-IO-AREA-WL410911 SSA1            
151700     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     .                                                                    
152000     EJECT                                                                
152100 IMS-GNP-410911-SIST-PER-KOD SECTION.                                     
152200     MOVE 'IMS-GNP-410911-SIST-PER-KOD' TO WS-IMS-SEKTION                 
152300                                                                          
152400     STRING 'WL410911*L(KEY4110 =>' W-KEY4110-X                           
152500                    '&KEY4110 =<' W-KEY4110-MAX-X ')'                     
152600          DELIMITED BY SIZE INTO SSA1                                     
152700     MOVE '  GE' TO GODK-STATUSKODER                                      
152800     CALL CBLTDLI USING GNP 4109-PCB DLI-IO-AREA-WL410911 SSA1            
152900     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
153000     PERFORM IMS-STATUSKONTROLL                                           
153100     .                                                                    
153200 IMS-GNP-410911-KVAL SECTION.                                             
153300     MOVE 'IMS-GNP-410911-KVAL' TO WS-IMS-SEKTION                         
153400                                                                          
153500     STRING 'WL410911(KEY4110 =>' W-KEY4110-X                             
153600                    '&KEY4110 =<' W-KEY4110-MAX-X ')'                     
153700          DELIMITED BY SIZE INTO SSA1                                     
153800     MOVE '  GE' TO GODK-STATUSKODER                                      
153900     CALL CBLTDLI USING GNP 4109-PCB DLI-IO-AREA-WL410911 SSA1            
154000     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300                                                                          
154400 IMS-GHNP-410911-KVAL-FIRST      SECTION.                                 
154500     MOVE 'IMS-GHNP-410911-KVAL-FIRST' TO WS-IMS-SEKTION                  
154600                                                                          
154700     STRING 'WL410901(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
154800          DELIMITED BY SIZE INTO SSA1                                     
154900     STRING 'WL410911(KEY4110 = ' W-KEY4110-X ')'                         
155000          DELIMITED BY SIZE INTO SSA2                                     
155100     MOVE '  GE' TO GODK-STATUSKODER                                      
155200     CALL CBLTDLI USING                                                   
155300                  GHU 4109-PCB DLI-IO-AREA-WL410911 SSA1 SSA2             
155400     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
155500     PERFORM IMS-STATUSKONTROLL                                           
155600     .                                                                    
155700     EJECT                                                                
155800 IMS-ISRT-410911 SECTION.                                                 
155900     MOVE 'IMS-ISRT-410911'  TO WS-IMS-SEKTION                            
156000                                                                          
156100     MOVE 'WL410911 ' TO SSA1                                             
156200     MOVE '    ' TO GODK-STATUSKODER                                      
156300     CALL CBLTDLI USING ISRT 4109-PCB DLI-IO-AREA-WL410911 SSA1           
156400     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
156500     PERFORM IMS-STATUSKONTROLL                                           
156600     .                                                                    
156700     SKIP3                                                                
156800 IMS-DLET-4109 SECTION.                                                   
156900     MOVE 'IMS-DLET-4109'    TO WS-IMS-SEKTION                            
157000                                                                          
157100     MOVE '  ' TO GODK-STATUSKODER                                        
157200     CALL CBLTDLI USING DLET 4109-PCB DLI-IO-AREA-WL410911                
157300     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
157400     PERFORM IMS-STATUSKONTROLL                                           
157500     .                                                                    
157600     SKIP3                                                                
157700 IMS-REPL-410911 SECTION.                                                 
157800     MOVE 'IMS-REPL-410911'  TO WS-IMS-SEKTION                            
157900                                                                          
158000     MOVE '  ' TO GODK-STATUSKODER                                        
158100     CALL CBLTDLI USING REPL 4109-PCB DLI-IO-AREA-WL410911                
158200     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
158300     PERFORM IMS-STATUSKONTROLL                                           
158400     .                                                                    
158500     EJECT                                                                
158600 IMS-GET-ARTC01 SECTION.                                                  
158700     MOVE 'IMS-GET-ARTC01'  TO WS-IMS-SEKTION                             
158800                                                                          
158900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-D6-X ')'                      
159000          DELIMITED BY SIZE INTO SSA1                                     
159100     MOVE '  GE' TO GODK-STATUSKODER                                      
159200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK601 SSA1               
159300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
159400     PERFORM IMS-STATUSKONTROLL                                           
159500     .                                                                    
159600     SKIP3                                                                
159610 IMS-GU-WDB601-FTG SECTION.                                               
159620     STRING 'WDB601  (IDFTG    =' WS-IDFTG-B6 ')'                         
159630            DELIMITED BY SIZE INTO SSA1                                   
159640     MOVE '  GE'                 TO GODK-STATUSKODER                      
159650     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
159660     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
159670     PERFORM IMS-STATUSKONTROLL                                           
159680     .                                                                    
159690     SKIP3                                                                
159691                                                                          
159700 IMS-STATUSKONTROLL SECTION.                                              
159800                                                                          
159900     SET STATUS-IX TO 1                                                   
160000     SEARCH GODK-STATUS                                                   
160100       AT END                                                             
160200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
160300         DELIMITED BY SIZE INTO FELTEXT                                   
160400         CALL FELLOG                                                      
160500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
160600         CONTINUE                                                         
160700     END-SEARCH                                                           
160800     .                                                                    
160900     EJECT                                                                
161000*    -COPY WY2000Q1                                                       
