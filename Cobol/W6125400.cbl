000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6125400.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   97/04/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOMMENTAR                                                        
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001100*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001200*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001300*        PROGRAMMET LÄSER      WDB6   (WDB6)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- INLEVERANSHISTORIK                                         
002800     SELECT W61253                     ASSIGN TO W61254D1.                
002900     SKIP2                                                                
003000*          --- FÖRVÄNTADE FLYGLEVERANSER                                  
003100     SELECT W61254                     ASSIGN TO W61254D2.                
003200     SKIP2                                                                
003300*          --- FKATURERAT UNDER DYGNET                                    
003400     SELECT W61255                     ASSIGN TO W61254D3.                
003500     SKIP2                                                                
003600*          --- R32OR OCH 310OR                                            
003700     SELECT W61256                     ASSIGN TO W61254D4.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W61253                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W61253      -L.                                                
004800     SKIP3                                                                
004900 FD  W61254                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W61254 -PRE  FLYG-  -L.                                   
005400     SKIP3                                                                
005500 FD  W61255                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  POST -COPY W61255 -PRE  FAKT-  -L.                                   
006000     SKIP3                                                                
006100 FD  W61256                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  POST -COPY W61256 -PRE R3X- -L.                                      
006600     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900 01  WS-FLNYART                  PIC X       VALUE 'N'.                   
007000                                                                          
007100*    -- CHECKED BY WY2000                                                 
007200 77  IDPGM                       PIC X(8)    VALUE 'W6125400'.            
007300 77  JA                          PIC X       VALUE 'J'.                   
007400 77  NEJ                         PIC X       VALUE 'N'.                   
007500 77  WS-ADCITY                   PIC X(25)   VALUE SPACES.                
007510 77  OLD-IDDC                    PIC X(2)    VALUE SPACES.                
007600                                                                          
007700 77  WS-IDSKYLT-CHINESE          PIC X(3)    VALUE 'RCN'.                 
007800 77  WS-IDSKYLT-USA              PIC X(3)    VALUE 'USA'.                 
007900                                                                          
008000 77  W61253-EOF-SW               PIC X       VALUE 'N'.                   
008100     88  END-OF-W61253                       VALUE 'J'.                   
008200     EJECT                                                                
008300 01  TESTFLAG                    PIC X       VALUE 'J'.                   
008400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES DAGENS-DATUM.                                       
008600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009700     SKIP2                                                                
009800*    --- PARAMETRAR TILL ABEND                                            
009900                                                                          
010000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010300     SKIP2                                                                
010400 01  FELTEXT.                                                             
010500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL POSTSUM                                          
010900*                                                                         
011000*01  -COPY W0005   -PRE  POSTSUM-                                         
011100     EJECT                                                                
011200                                                                          
011300 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
011400*01  -COPY WTRAUTF8                                                       
011500                                                                          
011600 01  IN-AREA-START               PIC X(24)   VALUE                        
011700                                 'IN-AREA-START  '.                       
011800     SKIP2                                                                
011900                                                                          
012000*01  AREA -COPY W61253     -PRE IN-                                       
012100     EJECT                                                                
012200 01  FLYG-AREA-START             PIC X(24)   VALUE                        
012300                                 'FLYG-AREA-START  '.                     
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W61254     -PRE FLYG-                                     
012700     EJECT                                                                
012800 01  FAKT-AREA-START             PIC X(24)   VALUE                        
012900                                 'FAKT-AREA-START  '.                     
013000     SKIP2                                                                
013100                                                                          
013200*01  AREA -COPY W61255     -PRE FAKT-                                     
013300     EJECT                                                                
013400 01  R3X-AREA-START              PIC X(24)   VALUE                        
013500                                 'R3X-AREA-START  '.                      
013600     SKIP2                                                                
013700                                                                          
013800*01  AREA -COPY W61256     -PRE R3X-                                      
013900     EJECT                                                                
014000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014100*                                                                         
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014400     SKIP3                                                                
014500 01  NYCKLAR-TILL-DLI.                                                    
014600     03  W-IDSKYLT-X.                                                     
014700         05  W-IDSKYLT           PIC X(3)    VALUE 'USA'.                 
014800     03  W-IDARTNR-X.                                                     
014900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015000     03  W-IDDC-X.                                                        
015100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015200     SKIP2                                                                
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FINNS                       VALUE '  '.                  
015600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015800     SKIP2                                                                
015900 01  GODK-STATUSKODER.                                                    
016000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016100     SKIP3                                                                
016200 01  SSA1                        PIC X(64).                               
016300 01  SSA2                        PIC X(64).                               
016400     EJECT                                                                
016500*    --- IMS FUNKTIONSKODER                                               
016600*01  -COPY W0003                                                          
016700     EJECT                                                                
016800*    ---  DLI INPUT-OUTPUT AREA                                           
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
017000 01  DLI-IO-WLBENA11.                                                     
017100*    03  -COPY WDD311  -PRE BENA-                                         
017200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
017300 01  DLI-IO-WLARTC11.                                                     
017400*    03  -COPY WDK611  -PRE ARTC-                                         
017500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
017600 01  DLI-IO-WLARTS11.                                                     
017700*    03  -COPY WDK711  -PRE ARTS-                                         
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
017900 01  DLI-IO-WDB601.                                                       
018000*    03  -COPY WDB601  -PRE WDB6-                                         
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE BENA-                                              
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800*01  -COPY W0008  -PRE ARTC-                                              
018900     05  FILLER                  PIC X.                                   
019000     EJECT                                                                
019100*01  -COPY W0008  -PRE ARTS-                                              
019200     05  FILLER                  PIC X.                                   
019300     EJECT                                                                
019400*01  -COPY W0008  -PRE WDB6-                                              
019500     05  FILLER                  PIC X.                                   
019600     EJECT                                                                
019700 PROCEDURE DIVISION  USING BENA-PCB ARTC-PCB                              
019800     ARTS-PCB WDB6-PCB.                                                   
019900 MAIN SECTION.                                                            
020000     ENTRY 'DLITCBL' USING BENA-PCB ARTC-PCB                              
020100     ARTS-PCB WDB6-PCB.                                                   
020200                                                                          
020300                                                                          
020400     PERFORM A-INIT                                                       
020500                                                                          
020600     PERFORM S01-LAES-W61253                                              
020700     PERFORM UNTIL END-OF-W61253                                          
020800       MOVE IN-SHIST-IDARTNR TO W-IDARTNR                                 
020900       MOVE IN-SHIST-IDDC    TO W-IDDC                                    
021010       IF W-IDDC NOT = OLD-IDDC                                           
021011         MOVE W-IDDC TO OLD-IDDC                                          
021020         PERFORM IMS-GU-WDB601                                            
021022         IF SEGMENT-FINNS                                                 
021023           MOVE WDB6-DCS-ADCITY IN WDB6-DCS-ADPOST-PNRORT                 
021024                       TO WS-ADCITY                                       
021025         ELSE                                                             
021026           MOVE SPACES TO WS-ADCITY                                       
021027         END-IF                                                           
021030       END-IF                                                             
021100                                                                          
021110       IF WDB6-DCS-CDC                                                    
021111         PERFORM IMS-GET-ARTC-CLAG                                        
021112         EVALUATE IN-SHIST-IDPTYP                                         
021113           WHEN 'R30'                                                     
021114             IF IN-SHIST-KDFRAKT = 17 OR 19                               
021115               PERFORM S21-TILLDELNING-FLYG                               
021116               PERFORM S11-SKRIV-W61254                                   
021117             END-IF                                                       
021123           WHEN OTHER                                                     
021124*          (DVS. 310:OR OCH R32:OR FRÅN INFILEN)                          
021125             PERFORM S23-TILLDELNING-R3X                                  
021126             PERFORM S13-SKRIV-W61256                                     
021127         END-EVALUATE                                                     
021130       ELSE                                                               
021200         PERFORM IMS-GET-ARTS-SLAG                                        
021300         IF TESTFLAG = 'J'                                                
021400            PERFORM S30-NYART-KONTROLLERING                               
021500                                                                          
021600            EVALUATE IN-SHIST-IDPTYP                                      
021700              WHEN 'R30'                                                  
021800                IF IN-SHIST-KDFRAKT = 17 OR 19                            
022400                  PERFORM S21-TILLDELNING-FLYG                            
022500                  PERFORM S11-SKRIV-W61254                                
022600                END-IF                                                    
022700                IF WS-FLNYART = JA                                        
022800                  PERFORM IMS-GET-ARTC-CLAG                               
022900                  PERFORM S22-TILLDELNING-FAKT                            
023000                  PERFORM S12-SKRIV-W61255                                
023100                END-IF                                                    
023200              WHEN OTHER                                                  
023300*             (DVS. 310:OR OCH R32:OR FRÅN INFILEN)                       
023400                PERFORM S23-TILLDELNING-R3X                               
023500                PERFORM S13-SKRIV-W61256                                  
023600            END-EVALUATE                                                  
023610            MOVE NEJ             TO WS-FLNYART                            
023700         END-IF                                                           
023710       END-IF                                                             
023800       PERFORM S01-LAES-W61253                                            
023900     END-PERFORM                                                          
024000                                                                          
024100     PERFORM Z-FINIT                                                      
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 A-INIT SECTION.                                                          
024800                                                                          
024900     OPEN INPUT  W61253                                                   
025000                                                                          
025100     OPEN OUTPUT W61254                                                   
025200                 W61255                                                   
025300                 W61256                                                   
025400                                                                          
025500     ACCEPT DAGENS-DATUM  FROM DATE                                       
025600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025700     .                                                                    
025800     EJECT                                                                
025900 Z-FINIT SECTION.                                                         
026000     CLOSE W61253                                                         
026100           W61254                                                         
026200           W61255                                                         
026300           W61256                                                         
026400     SKIP2                                                                
026500     MOVE 'S' TO POSTSUM-OPKOD                                            
026600     CALL POSTSUM USING POSTSUM-PARM                                      
026700     .                                                                    
026800     EJECT                                                                
026900 S01-LAES-W61253 SECTION.                                                 
027000     READ W61253 INTO IN-AREA                                             
027100     AT END                                                               
027200        MOVE HIGH-VALUE TO IN-AREA                                        
027300        SET END-OF-W61253 TO TRUE                                         
027400                                                                          
027500     NOT AT END                                                           
027600        MOVE 'W61253' TO POSTSUM-FDNAMN                                   
027700        MOVE 'W61254D1' TO POSTSUM-DDNAMN2                                
027800        MOVE IN-SHIST-IDPTYP TO POSTSUM-TRANSTYP                          
027900        CALL POSTSUM USING POSTSUM-PARM                                   
028000     END-READ                                                             
028100     .                                                                    
028200     EJECT                                                                
028300 S11-SKRIV-W61254 SECTION.                                                
028400                                                                          
028500     WRITE FLYG-POST FROM FLYG-AREA                                       
028600                                                                          
028700     MOVE IN-SHIST-IDPTYP TO POSTSUM-TRANSTYP                             
028800     MOVE 'W61254' TO POSTSUM-FDNAMN                                      
028900     MOVE 'W61254D2' TO POSTSUM-DDNAMN2                                   
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     .                                                                    
029200     EJECT                                                                
029300 S12-SKRIV-W61255 SECTION.                                                
029400                                                                          
029500     WRITE FAKT-POST FROM FAKT-AREA                                       
029600                                                                          
029700     MOVE IN-SHIST-IDPTYP TO POSTSUM-TRANSTYP                             
029800     MOVE 'W61255' TO POSTSUM-FDNAMN                                      
029900     MOVE 'W61254D3' TO POSTSUM-DDNAMN2                                   
030000     CALL POSTSUM USING POSTSUM-PARM                                      
030100     .                                                                    
030200     EJECT                                                                
030300 S13-SKRIV-W61256 SECTION.                                                
030400                                                                          
030500     WRITE R3X-POST FROM R3X-AREA                                         
030600                                                                          
030700     MOVE IN-SHIST-IDPTYP TO POSTSUM-TRANSTYP                             
030800     MOVE 'W61256' TO POSTSUM-FDNAMN                                      
030900     MOVE 'W61254D4' TO POSTSUM-DDNAMN2                                   
031000     CALL POSTSUM USING POSTSUM-PARM                                      
031100     .                                                                    
031200     EJECT                                                                
031300 S21-TILLDELNING-FLYG SECTION.                                            
031400                                                                          
031500     SKIP2                                                                
031600     MOVE IN-SHIST-IDDC     TO FLYG-SHIST-IDDC                            
031700     MOVE IN-SHIST-IDFAKT   TO FLYG-SHIST-IDFAKT                          
031800     MOVE IN-SHIST-IDKOLLI  TO FLYG-SHIST-IDKOLLI                         
031900     MOVE IN-SHIST-IDKUNDNR TO FLYG-SHIST-IDKUNDNR                        
032000     MOVE IN-SHIST-IDORDNR5 TO FLYG-SHIST-IDORDNR5                        
032100     MOVE IN-SHIST-IDARTNR  TO FLYG-SHIST-IDARTNR                         
032200     MOVE IN-SHIST-PRARTNTO TO FLYG-SHIST-PRARTNTO                        
032300     MOVE IN-SHIST-PRKURS   TO FLYG-SHIST-PRKURS                          
032400     MOVE IN-SHIST-KDVALISO TO FLYG-SHIST-KDVALISO                        
032500     MOVE IN-SHIST-KVAVIS   TO FLYG-SHIST-KVAVIS                          
032600     MOVE IN-SHIST-TIBERANK TO FLYG-SHIST-TIBERANK                        
032700     MOVE WS-FLNYART        TO FLYG-SHIST-FLNYART                         
032800     MOVE WS-ADCITY         TO FLYG-SHIST-ADCITY                          
032900     .                                                                    
033000     EJECT                                                                
033100 S22-TILLDELNING-FAKT SECTION.                                            
033200                                                                          
033300     SKIP2                                                                
033400     MOVE IN-SHIST-IDDC         TO FAKT-SHIST-IDDC                        
033500     MOVE IN-SHIST-TIBERANK     TO FAKT-SHIST-TIBERANK                    
033600     MOVE IN-SHIST-KDFRAKT      TO FAKT-SHIST-KDFRAKT                     
033700     MOVE IN-SHIST-IDARTNR      TO FAKT-SHIST-IDARTNR                     
           MOVE WDB6-DCS-IDSKYLT-DB   TO W-IDSKYLT                              
           IF WDB6-DCS-UNICODE-IDSKYLT                                          
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
033800     IF WDB6-DCS-CHINA                                                    
034100        PERFORM IMS-GET-BENA-TEXT                                         
034200        IF SEGMENT-FINNS                                                  
034300           MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM                    
034400        ELSE                                                              
034500           MOVE SPACE           TO TRAUTF8-TECONV-FROM                    
034600           MOVE '278 '          TO TRAUTF8-KDCP                           
034700        END-IF                                                            
034800        MOVE 25                 TO TRAUTF8-KVMAXTL                        
034900        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
035000        MOVE TRAUTF8-TECONV-TO  TO FAKT-SHIST-BEART                       
035100     ELSE                                                                 
035200        MOVE WS-IDSKYLT-USA     TO W-IDSKYLT-X                            
035300        PERFORM IMS-GET-BENA-TEXT                                         
035400        MOVE BENA-TEXT-BEART    TO FAKT-SHIST-BEART                       
035500     END-IF                                                               
035600     MOVE ARTS-SLAG-KVPB-REF    TO FAKT-SHIST-KVPB-REF                    
035700     MOVE IN-SHIST-KVAVIS       TO FAKT-SHIST-KVAVIS                      
035800     MOVE ARTC-CLAG-KDVSOP      TO FAKT-SHIST-KDVSOP                      
035900     MOVE ARTC-CLAG-VKART       TO FAKT-SHIST-VKART                       
036000     MOVE ARTC-CLAG-VLARTNTO    TO FAKT-SHIST-VLARTNTO                    
036100     MOVE ARTC-CLAG-ADLAGOMR    TO FAKT-SHIST-ADLAGOMR                    
036200     MOVE IN-SHIST-IDORDNR5     TO FAKT-SHIST-IDORDNR5                    
036300     MOVE IN-SHIST-IDFAKT       TO FAKT-SHIST-IDFAKT                      
036400     MOVE IN-SHIST-IDKOLLI      TO FAKT-SHIST-IDKOLLI                     
036500     .                                                                    
036600     EJECT                                                                
036700 S23-TILLDELNING-R3X  SECTION.                                            
036800                                                                          
036900     SKIP2                                                                
037000     MOVE IN-SHIST-IDDC     TO R3X-SHIST-IDDC                             
037100     MOVE IN-SHIST-IDDISTR  TO R3X-SHIST-IDDISTR                          
037200     MOVE IN-SHIST-KDFRAKT  TO R3X-SHIST-KDFRAKT                          
037300     MOVE IN-SHIST-IDARTNR  TO R3X-SHIST-IDARTNR                          
037400     MOVE IN-SHIST-PRARTNTO TO R3X-SHIST-PRARTNTO                         
037500     MOVE IN-SHIST-PRKURS   TO R3X-SHIST-PRKURS                           
037600     MOVE IN-SHIST-KDVALISO TO R3X-SHIST-KDVALISO                         
037700     MOVE IN-SHIST-KVANTMOT TO R3X-SHIST-KVANTMOT                         
037800     MOVE IN-SHIST-FLPRIO   TO R3X-SHIST-FLPRIO                           
037810     IF WDB6-DCS-CDC                                                      
037900       MOVE ARTC-CLAG-KVROS TO R3X-SHIST-KVROS                            
038200       MOVE ARTC-CLAG-PRARTSTD                                            
038210                            TO R3X-SHIST-PRAVCOST                         
038211     ELSE                                                                 
038230       ADD ARTS-SLAG-KVROS-BULK ARTS-SLAG-KVROS-DAG                       
038240                        GIVING R3X-SHIST-KVROS                            
038250       MOVE ARTS-SLAG-PRAVCOST                                            
038260                            TO R3X-SHIST-PRAVCOST                         
038270     END-IF                                                               
038300     MOVE IN-SHIST-KVAVIS   TO R3X-SHIST-KVAVIS                           
038400     MOVE IN-SHIST-IDPTYP   TO R3X-SHIST-IDPTYP                           
038500     MOVE IN-SHIST-TIINLMOT TO R3X-SHIST-TIINLMOT                         
038600     MOVE IN-SHIST-TIINLMTI TO R3X-SHIST-TIINLMTI                         
038700     MOVE IN-SHIST-TIINLINL TO R3X-SHIST-TIINLINL                         
038800     MOVE IN-SHIST-TIINLITI TO R3X-SHIST-TIINLITI                         
038900     .                                                                    
039000     EJECT                                                                
039100 S30-NYART-KONTROLLERING SECTION.                                         
039200                                                                          
039300     SKIP2                                                                
039400*    KONTROLL OM ARTIKELN ÄR PLATSSATT I NDC-LAGERET                      
039500     IF   ARTS-SLAG-ADLAGOMR  = 0     AND                                 
039600          ARTS-SLAG-ADGANG    = 0     AND                                 
039700          ARTS-SLAG-ADPLATS   = 0                                         
039800       MOVE JA   TO  WS-FLNYART                                           
039900     ELSE                                                                 
040000       MOVE NEJ  TO  WS-FLNYART                                           
040100     END-IF                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 S99-ABEND SECTION.                                                       
040500                                                                          
040600     SKIP2                                                                
040700     MOVE 'S' TO POSTSUM-OPKOD                                            
040800     CALL POSTSUM USING POSTSUM-PARM                                      
040900     CALL ABEND USING RKOD-ABEND                                          
041000     .                                                                    
041100     EJECT                                                                
041200* --- IMS SEKTIONER ---                                                   
041300 IMS-GET-BENA-TEXT SECTION.                                               
041400                                                                          
041500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
041600          DELIMITED BY SIZE INTO SSA1                                     
041700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
041800            DELIMITED BY SIZE INTO SSA2                                   
041900     MOVE SPACE TO GODK-STATUSKODER                                       
042000     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2            
042100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     EJECT                                                                
042500 IMS-GET-ARTC-CLAG SECTION.                                               
042600                                                                          
042700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
042800          DELIMITED BY SIZE INTO SSA1                                     
042900     MOVE 'WLARTC11 '         TO SSA2                                     
043000     MOVE SPACE TO GODK-STATUSKODER                                       
043100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
043200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     .                                                                    
043500     EJECT                                                                
043600 IMS-GET-ARTS-SLAG SECTION.                                               
043700                                                                          
043800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
043900          DELIMITED BY SIZE INTO SSA1                                     
044000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
044100          DELIMITED BY SIZE INTO SSA2                                     
044200*    MOVE SPACE TO GODK-STATUSKODER                                       
044300     MOVE '  GE' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
044500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     IF ARTS-STATUS-CODE = 'GE'                                           
044800        MOVE 'N' TO TESTFLAG                                              
044900        DISPLAY W-IDARTNR ' ' W-IDDC                                      
045000     ELSE                                                                 
045100        MOVE 'J' TO TESTFLAG                                              
045200     END-IF                                                               
045300                                                                          
045400     .                                                                    
045500     EJECT                                                                
045600 IMS-GU-WDB601    SECTION.                                                
045700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
045800          DELIMITED BY SIZE INTO SSA1                                     
045900     MOVE '  GE' TO GODK-STATUSKODER                                      
046000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
046100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-STATUSKONTROLL SECTION.                                              
046600                                                                          
046700     SET STATUS-IX TO 1                                                   
046800     SEARCH GODK-STATUS                                                   
046900       AT END                                                             
047000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047100           DELIMITED BY SIZE INTO FELTEXT                                 
047200         DISPLAY FELTEXT                                                  
047300         CALL FELLOG                                                      
047400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047500         CONTINUE                                                         
047600     END-SEARCH                                                           
047700     .                                                                    
