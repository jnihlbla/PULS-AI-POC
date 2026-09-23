000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL011900.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   JUNI 2005.                                               
000600                                                                          
000700     REMARKS.                                                             
000800* WL011900 PROGRAM IS A REPLICA OF W4032400 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*    NAMN:       CARPARTS.LDC.ORDERSSTARTEDNOTCOMPLETED                   
001200*                                                                         
001300                                                                          
001400     REMARKS.                                                             
001500*                                                                         
001600*    FUNKTION.                                                            
001700*        PÅBÖRJADE EJ AVSLUTADE ORDRAR.                                   
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0119                                              
002200*        REQU:        WL0119I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        RESP:        WL0119O1                                            
002600*                                                                         
002700*    CHANGE LOG                                                           
002800*                                                                         
002900*    DIGAMBAR/021011                                                      
003000*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
003100*    THE RESPONSE TIME OF THE SCREEN 4312.                                
003200*                                                                         
003300*    SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP3                                                                
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77   PROGRAM-NAMN                           VALUE 'WL011900'             
004200                                 PIC X(8).                                
004300 77    JA                        PIC X       VALUE 'J'.                   
004400 77    NEJ                       PIC X       VALUE 'N'.                   
004500 77    BORTTAG                   PIC X       VALUE 'B'.                   
004600 77    RAD-FINNS                 PIC X       VALUE 'N'.                   
004700 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
004800 77    PGM-POS                   PIC X(32)   VALUE SPACE.                 
004900 77    FELTEXT                   PIC X(64)   VALUE SPACE.                 
005000 77    KDRC-DISPLAY              PIC Z(5)    VALUE ZERO.                  
005100 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005200 77    SPRAK-INDX                PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77    RESP-RAD-IND              PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77    MAX-LINE                  PIC S9(9)  VALUE +1000 COMP SYNC.        
005500 77    WS-KVORDRAD               PIC S9(5)              COMP-3.           
005600 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
005700 01    WS-IDPRODNR                           PIC 9(7).                    
005800*01    IDPRODNR-9  REDEFINES WS-IDPRODNR     PIC 9(7).                    
005900*                                                                         
006000 77    WS-SAVE-DARFS             PIC 9(12).                               
006100 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
006200     SKIP2                                                                
006300 77    TIBEGPAC-WS               PIC S9(7)              COMP-3.           
006400 77    KDORDKL-WS                PIC S9(1)              COMP-3.           
006500 77    IDDISTR-WS                PIC S9(5)              COMP-3.           
006600 77    IDKUNDNR-WS               PIC S9(7)              COMP-3.           
006700 77    KDFRAKT-WS                PIC S9(3)              COMP-3.           
006800 77    IDPRODNR-WS               PIC S9(7)              COMP-3.           
006900 77    KVORDRAD-WS               PIC S9(7)              COMP-3.           
007000 77    KVORDRAD-PACK-WS          PIC S9(7)              COMP-3.           
007100 77    KVKOLPAC-WS               PIC S9(5)              COMP-3.           
007200 77    TIUTSKR-WS                PIC S9(7)              COMP-3.           
007300 77    WS-ARBDAT-NUM7            PIC S9(7).                               
007400 77    WS-IDTIDZON               PIC  X(2).                               
007500 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
007600*      - - - - - - - - - - - - - *****                                    
007700 77    WS-KDFEL                  PIC 9(2)    VALUE ZERO COMP-3.           
007800     SKIP2                                                                
007900 01    WS-KDORDKL                          PIC X.                         
008000 01    WS-KDORDKL-NUM REDEFINES WS-KDORDKL PIC 9.                         
008100     SKIP2                                                                
008200 01    WS-KDPRCGRP                       PIC X(5).                        
008300 01    KDPRCGRP-WS REDEFINES WS-KDPRCGRP PIC 9(5).                        
008400     SKIP2                                                                
008500 01    WS-IDKUNDRF.                                                       
008600   03  WS-IDORDNR                PIC X(5).                                
008700   03  FILLER                    PIC X(5)    VALUE SPACE.                 
008800     SKIP2                                                                
008900 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
009000 77    WS-DATUM                  PIC 9(6)    VALUE ZERO.                  
009100 77    WS-DATUM-TIRFS            PIC 9(6)    VALUE ZERO.                  
009200     EJECT                                                                
009300 01    INGAANG-SW                PIC X.                                   
009400   88  PRODNR-INGANG             VALUE 'J'.                               
009500   88  PRCGRP-INGANG             VALUE 'N'.                               
009600                                                                          
009700 01    NYCKEL-TYP                PIC S9(2)   COMP-3.                      
009800   88  GAMMAL-NYCKEL             VALUE +1.                                
009900   88  NY-NYCKEL                 VALUE +2.                                
010000     SKIP3                                                                
010100 01    FRAN-BILD                 PIC 9(4).                                
010200   88  FRAN-BILD-OK              VALUE 4312                               
010300                                       4321 THRU 4324.                    
010400   88  FRAN-BILD-EGEN            VALUE 4324.                              
010500     SKIP3                                                                
010600 77    FILLER                    PIC X(8)    VALUE 'FFFFFFFF'.            
010700 01    WS-NYCKEL.                                                         
010800   03  WS-DARFS-NYCKEL           PIC 9(12).                               
010900   03  WS-IDPRODNR-NYCKEL        PIC 9(7).                                
011000     EJECT                                                                
011100 77    FILLER                    PIC X(8)    VALUE 'DLIKEYS:'.            
011200 01    NYCKLAR-TILL-DLI.                                                  
011300   03    W-WDE601-IDPRODNR-X.                                             
011400     05    W-601-IDPRODNR        PIC S9(7)    COMP-3.                     
011500     SKIP1                                                                
011600   03    W-WDE401-KORD-X.                                                 
011700     05    W-401-IDDISTR         PIC S9(5)    COMP-3.                     
011800     05    W-401-IDKUNDNR        PIC S9(7)    COMP-3.                     
011900     05    W-401-IDKUNDRF.                                                
012000       07    W-401-IDORDNR       PIC 9(5).                                
012100       07    FILLER              PIC X(5)     VALUE SPACE.                
012200     05    W-401-IDPRODNR        PIC S9(7)    COMP-3.                     
012300     05    W-401-IDPLKLST        PIC S9(3)    COMP-3.                     
012400     SKIP1                                                                
012500   03    W-WDE411-KEYSEQ-MIN-X.                                           
012600     05    W-411-IDPRODNR-MIN    PIC S9(7)    COMP-3.                     
012700     05    W-411-IDPURAD-MIN     PIC S9(5)    COMP-3.                     
012800     SKIP1                                                                
012900   03    W-WDE411-KEYSEQ-MAX-X.                                           
013000     05    W-411-IDPRODNR-MAX    PIC S9(7)    COMP-3.                     
013100     05    W-411-IDPURAD-MAX     PIC S9(5)    COMP-3.                     
013200     SKIP1                                                                
013300   03    W-4487-KEY-X.                                                    
013400     05    W-4487-IDHTYP         PIC X(4)  VALUE '4487'.                  
013500     05    W-4487-IDDC           PIC X(2).                                
013600     05    W-4487-LOW-VALUE      PIC X(24) VALUE LOW-VALUE.               
013700     SKIP1                                                                
013800   03    W-4488-KEY-X.                                                    
013900     05    W-4488-KDPRCGRP       PIC X(5).                                
014000     SKIP1                                                                
014100   03    W-4490-KEY-X.                                                    
014200     05    W-4490-DARFS          PIC 9(12).                               
014300     05    W-4490-IDPRODNR       PIC S9(7)    COMP-3.                     
014400     05    W-4490-IDPLKLST       PIC S9(3)    COMP-3.                     
014500     EJECT                                                                
014600 01    MEDDELANDE.                                                        
014700   03    FEL1.                                                            
014800     05    FILLER                PIC X(40)   VALUE                        
014900             '784  BLÄDDRING EJ TILLÅTEN             '.                   
015000     05    FILLER                PIC X(40)   VALUE                        
015100             '784 SCROLLING NOT ALLOWED              '.                   
015200   03    FILLER REDEFINES FEL1.                                           
015300     05    FEL-1 OCCURS 2        PIC X(40).                               
015400     SKIP2                                                                
015500   03    FEL2.                                                            
015600     05    FILLER                PIC X(40)   VALUE                        
015700             '802  ORDER SAKNAS PÅ ANG FÖRMANSOMRÅDE '.                   
015800     05    FILLER                PIC X(40)   VALUE                        
015900             '802  WRONG AREA FOR ORDER              '.                   
016000   03    FILLER REDEFINES FEL2.                                           
016100     05    FEL-2 OCCURS 2        PIC X(40).                               
016200     SKIP2                                                                
016300   03    FEL3.                                                            
016400     05    FILLER                PIC X(40)   VALUE                        
016500             '749  FEL NYCKEL                        '.                   
016600     05    FILLER                PIC X(40)   VALUE                        
016700             '749  WRONG KEY                         '.                   
016800   03    FILLER REDEFINES FEL3.                                           
016900     05    FEL-3 OCCURS 2        PIC X(40).                               
017000     SKIP2                                                                
017100   03    FEL4.                                                            
017200     05    FILLER                PIC X(40)   VALUE                        
017300             '054  ORDER SAKNAS                      '.                   
017400     05    FILLER                PIC X(40)   VALUE                        
017500             '054  ORDER MISSING                     '.                   
017600   03    FILLER REDEFINES FEL4.                                           
017700     05    FEL-4 OCCURS 2        PIC X(40).                               
017800     SKIP2                                                                
017900   03    MED1.                                                            
018000     05    FILLER                PIC X(40)   VALUE                        
018100             '778  FLER RADER FINNS                  '.                   
018200     05    FILLER                PIC X(40)   VALUE                        
018300             '778  MORE LINES                        '.                   
018400   03    FILLER REDEFINES MED1.                                           
018500     05    MED-1 OCCURS 2        PIC X(40).                               
018600     EJECT                                                                
018700******************************************************************        
018800*                                                                         
018900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
019000*                                                                         
019100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
019200     SKIP3                                                                
019300*01  -COPY WZ01SUB                                                        
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
019600     SKIP3                                                                
019700 01  REQU-AREA.                                                           
019800*    03  -COPY WZ01REQU                                                   
019900*    03  -COPY WL0119I1                                                   
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
020200     SKIP3                                                                
020300 01  RESP-AREA.                                                           
020400*    03  -COPY WZ01RESP                                                   
020500*    03  -COPY WL0119O1                                                   
020600     SKIP3                                                                
020700******************************************************************        
020800*                                                                         
020900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021000*                                                                         
021100 01    IMS-WS.                                                            
021200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
021300     SKIP3                                                                
021400*                        **** STATUS-KOD FRÅN IMS                         
021500   03    STATUS-WS               PIC XX.                                  
021600     88    SEGMENT-FINNS                    VALUE '  '.                   
021700     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
021800     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
021900     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
022000     SKIP3                                                                
022100   03    GODK-STATUSKODER.                                                
022200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
022300     SKIP3                                                                
022400 01    SSA1                      PIC X(64).                               
022500 01    SSA2                      PIC X(64).                               
022600 01    SSA3                      PIC X(64).                               
022700     SKIP3                                                                
022800 01    GENERELLA-SUBPROGRAM.                                              
022900   03  WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
023000   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
023100   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
023200   03  ABEND                     PIC X(8)   VALUE 'ABEND   '.             
023300   03  WZ01SEND                  PIC X(8)   VALUE 'WZ01SEND'.             
023400   03  WZ01SUB                   PIC X(8)   VALUE 'WZ01SUB '.             
023500     SKIP3                                                                
023600*                                                                         
023700*    --- PARAMETERS TO ABEND                                              
023800*                                                                         
023900 77  FILLER                      PIC X(08)   VALUE 'ABENDARE'.            
024000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
024100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
024200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
024300     SKIP2                                                                
024400 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
024500 01  MESSAGE-CODES.                                                       
024600     03  ERR-MISSING              PIC X(3)    VALUE '041'.                
024700     03  ERR-WRONG-KEY            PIC X(3)    VALUE '022'.                
024800     03  ERR-CORR-FIELDS          PIC X(3)    VALUE '023'.                
024900     03  ERR-ARTIKEL-SAKNAS       PIC X(3)    VALUE '017'.                
025000     03  ERR-WRONG-AREA-FOR-ORDER PIC X(3)    VALUE '317'.                
025100     03  ERR-LINES-NOT-FIND       PIC X(3)    VALUE '027'.                
025500     EJECT                                                                
025600*01    -COPY WDATAREA                                                     
025700     EJECT                                                                
025800*                            IMS FUNKTIONSKODER                           
025900*01    -COPY W0003                                                        
026000     EJECT                                                                
026100*                            DLI INPUT-OUTPUT AREA                        
026200 01    DLI-IO-AREA.                                                       
026300   03    IO-AREA                 PIC X(510)  VALUE SPACE.                 
026400     SKIP3                                                                
026500*  03    WDE601 -COPY WDE601              -RED IO-AREA.                   
026600     EJECT                                                                
026700 01    DLI-IO-AREA2.                                                      
026800   03    IO-AREA2                PIC X(145)  VALUE SPACE.                 
026900     SKIP3                                                                
027000*  03    WDE401 -COPY WDE401              -RED IO-AREA2.                  
027100     EJECT                                                                
027200 01    DLI-IO-AREA3.                                                      
027300   03    IO-AREA3                PIC X(30)   VALUE SPACE.                 
027400     SKIP3                                                                
027500*  03    4487-AREA  -COPY WDGX4487            -RED IO-AREA3.              
027600     EJECT                                                                
027700*  03    WDGX4488 -COPY WDGX4488            -RED IO-AREA3.                
027800     EJECT                                                                
027900*  03    WDGX4490 -COPY WDGX4490            -RED IO-AREA3.                
028000     EJECT                                                                
028100 LINKAGE SECTION.                                                         
028200*01    -COPY W0009     -PRE MSG-                                          
028300     EJECT                                                                
028400*01    -COPY W0008     -PRE WDE4-                                         
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700*01    -COPY W0008     -PRE WDE6-                                         
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000*01    -COPY W0008     -PRE 4487-                                         
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01    -COPY W0008     -PRE WDE42-                                        
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600 PROCEDURE DIVISION USING MSG-PCB                                         
029700                          WDE4-PCB WDE6-PCB 4487-PCB WDE42-PCB.           
029800     ENTRY 'DLITCBL' USING MSG-PCB                                        
029900                           WDE4-PCB WDE6-PCB 4487-PCB WDE42-PCB.          
030000     SKIP2                                                                
030100 MAIN SECTION.                                                            
030200                                                                          
030300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
030400     IF SUB-KDRC = 0                                                      
030500       PERFORM A-INIT-SPARA-INPUT                                         
030600       PERFORM B-KOLLA-INPUT                                              
030700                                                                          
030800       IF PRCGRP-INGANG                                                   
030900        IF WS-KDFEL = ZERO                                                
031000         PERFORM C-HAEMTA-NYCKEL                                          
031100         MOVE KDPRCGRP-WS         TO W-4488-KDPRCGRP                      
031200         MOVE REQU-IDDC-KEY       TO W-4487-IDDC                          
031300         PERFORM IMS-GU-WDGX4488                                          
031400         IF SEGMENT-FINNS AND                                             
031500            WS-KDFEL NOT > 0                                              
031600            PERFORM IMS-GNP-WDGX4490                                      
031700            IF SEGMENT-FINNS                                              
031800               MOVE ZERO TO RESP-RAD-IND, RESP-KVRADER                    
031900                                                                          
032000               PERFORM UNTIL SEGMENT-SAKNAS OR                            
032100                             SLUT-PA-BASEN  OR                            
032200                             RESP-RAD-IND >= MAX-LINE                     
032300                 IF WS-KDORDKL-NUM = 4490-KDORDKL    OR                   
032400                        WS-KDORDKL = SPACE                                
032500                   MOVE 4490-IDPRODNR       TO W-601-IDPRODNR             
032600                   MOVE 4490-DARFS          TO WS-SAVE-DARFS              
032700                                                                          
032800                   IF 4490-IDPRODNR          = IDPRODNR-WS                
032900                     CONTINUE                                             
033000                   ELSE                                                   
033100                     IF WS-KDORDKL-NUM = KDORDKL-WS OR                    
033200                            WS-KDORDKL = SPACE                            
033300                        IF RAD-FINNS = JA                                 
033400                           ADD 1 TO RESP-RAD-IND                          
033500                           MOVE RESP-RAD-IND TO RESP-KVRADER              
033600                           PERFORM M-FLYTTA-TILL-RESP                     
033700                           MOVE NEJ        TO RAD-FINNS                   
033800                        END-IF                                            
033900                     END-IF                                               
034000                                                                          
034100                     PERFORM IMS-GET-WDE601                               
034200                     IF SEGMENT-FINNS                                     
034300                       MOVE JA               TO RAD-FINNS                 
034400                       PERFORM F-FLYTTA-VORD-TILL-RAD                     
034500                       MOVE 4490-IDPRODNR    TO W-411-IDPRODNR-MIN        
034600                                                W-411-IDPRODNR-MAX        
034700                       MOVE 1                TO W-411-IDPURAD-MIN         
034800                       MOVE 99999            TO W-411-IDPURAD-MAX         
034900                       PERFORM IMS-GU-KUNDORDER-SEK-INV                   
035000                       MOVE KORD-IDDISTR     TO W-401-IDDISTR             
035100                       MOVE KORD-IDKUNDNR    TO W-401-IDKUNDNR            
035200                       MOVE KORD-IDORDNR5    TO W-401-IDORDNR             
035300                       MOVE 4490-IDPRODNR    TO W-401-IDPRODNR            
035400                       MOVE 4490-IDPLKLST    TO W-401-IDPLKLST            
035500                       PERFORM IMS-GU-KUNDORDER                           
035600                       PERFORM S01-FLYTTA-PLOC-TILL-RAD                   
035700                     END-IF                                               
035800                   END-IF                                                 
035900                 END-IF                                                   
036000                 PERFORM IMS-GNP-WDGX4490                                 
036100               END-PERFORM                                                
036200                                                                          
036300               IF RESP-RAD-IND < MAX-LINE                                 
036400                  IF WS-KDORDKL-NUM = KDORDKL-WS  OR                      
036500                         WS-KDORDKL = SPACE                               
036600                     IF RAD-FINNS = JA                                    
036700                        ADD 1 TO RESP-RAD-IND                             
036800                        MOVE RESP-RAD-IND TO RESP-KVRADER                 
036900                        PERFORM M-FLYTTA-TILL-RESP                        
037000                     END-IF                                               
037100                  END-IF                                                  
037200               END-IF                                                     
037300            ELSE                                                          
037400               MOVE 2 TO WS-KDFEL                                         
037500            END-IF                                                        
037600         ELSE                                                             
037700            MOVE 1 TO WS-KDFEL                                            
037800         END-IF                                                           
037900        END-IF                                                            
038000       ELSE                                                               
038100         PERFORM N-VISA-VOLVOORDER                                        
038200       END-IF                                                             
038300       IF WS-KDFEL > ZERO                                                 
038400         PERFORM J-HAMTA-MEDDELANDE                                       
038500       END-IF                                                             
038600       PERFORM S02-RETURN-RESPONSE                                        
038700     END-IF                                                               
038800     MOVE ZERO TO RETURN-CODE                                             
038900     GOBACK                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 A-INIT-SPARA-INPUT SECTION.                                              
039300     MOVE 'STA A-INIT        ' TO PGM-POS                                 
039400                                                                          
039500     MOVE ALL '+'              TO RESP-WL0119O1                           
039600     MOVE 001                  TO RESP-IDMSGVER                           
039700     MOVE SPACE                TO RESP-IDMSG-ERROR                        
039800                                  RESP-IDMSG-INFO                         
039900                                  RESP-IDELMT-ERROR                       
040000     MOVE ZERO                 TO RESP-KVRADER                            
040100*                                                                         
040200     MOVE NEJ                  TO INGAANG-SW                              
040300     MOVE ZERO                 TO WS-KDFEL                                
040400                                  TIBEGPAC-WS                             
040500                                  KDORDKL-WS                              
040600                                  IDDISTR-WS                              
040700                                  IDKUNDNR-WS                             
040800                                  KDFRAKT-WS                              
040900                                  IDPRODNR-WS                             
041000                                  KVORDRAD-WS                             
041100                                  KVORDRAD-PACK-WS                        
041200                                  KVKOLPAC-WS                             
041300                                  TIUTSKR-WS                              
041400                                                                          
041500                                                                          
041600     MOVE +2 TO SPRAK-INDX                                                
041700                                                                          
041800        IF REQU-KDPRCGRP-KEY NOT = ALL '+'                                
041900           INSPECT REQU-KDPRCGRP-KEY REPLACING LEADING                    
042000           SPACE BY ZEROES                                                
042100           MOVE REQU-KDPRCGRP-KEY     TO WS-KDPRCGRP                      
042200           MOVE +2 TO NYCKEL-TYP                                          
042300        ELSE                                                              
042400           IF REQU-IDPRODNR-KEY NOT = ALL '+'                             
042500              MOVE REQU-IDPRODNR-KEY  TO WS-IDPRODNR                      
042600              MOVE '00000'            TO WS-KDPRCGRP                      
042700           ELSE                                                           
042800              MOVE ZERO           TO WS-IDPRODNR                          
042900              MOVE '00000'        TO WS-KDPRCGRP                          
043000           END-IF                                                         
043100        END-IF                                                            
043200                                                                          
043300        IF REQU-KDORDKL-KEY = ALL '+'                                     
043400           MOVE SPACE                 TO WS-KDORDKL                       
043500        ELSE                                                              
043600           MOVE REQU-KDORDKL-KEY      TO WS-KDORDKL                       
043700        END-IF                                                            
043800                                                                          
043900        IF REQU-IDDC-KEY = ALL '+'                                        
044000           MOVE ZERO                  TO RESP-IDDC-KEY                    
044100        ELSE                                                              
044200           MOVE REQU-IDDC-KEY         TO RESP-IDDC-KEY                    
044300        END-IF                                                            
044400                                                                          
044500     .                                                                    
044600     EJECT                                                                
044700 B-KOLLA-INPUT SECTION.                                                   
044800     IF WS-KDORDKL   NUMERIC OR                                           
044900        WS-KDORDKL = ALL SPACE                                            
045000       IF WS-KDPRCGRP NUMERIC AND                                         
045100          WS-KDPRCGRP > ZERO                                              
045200          MOVE WS-KDPRCGRP  TO RESP-KDPRCGRP-KEY                          
045300          MOVE WS-KDORDKL   TO RESP-KDORDKL-KEY                           
045400          INSPECT RESP-KDORDKL-KEY  REPLACING                             
045500                                 LEADING ZEROES BY SPACE                  
045600          INSPECT RESP-KDPRCGRP-KEY REPLACING                             
045700                                 LEADING ZEROES BY SPACE                  
045800          MOVE NEJ TO INGAANG-SW                                          
045900       ELSE                                                               
046000          IF WS-IDPRODNR     NUMERIC AND                                  
046100             WS-IDPRODNR     > ZERO                                       
046200             MOVE JA TO INGAANG-SW                                        
046300             MOVE REQU-IDPRODNR-KEY  TO RESP-IDPRODNR-KEY                 
046400          ELSE                                                            
046500             MOVE 3 TO WS-KDFEL                                           
046600          END-IF                                                          
046700       END-IF                                                             
046800     ELSE                                                                 
046900        MOVE 4 TO WS-KDFEL                                                
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 C-HAEMTA-NYCKEL SECTION.                                                 
047400     MOVE WS-DARFS-NYCKEL          TO W-4490-DARFS                        
047500     MOVE WS-IDPRODNR-NYCKEL       TO W-4490-IDPRODNR                     
047600     MOVE ZERO                     TO W-4490-IDPLKLST                     
047700     .                                                                    
047800     EJECT                                                                
047900 F-FLYTTA-VORD-TILL-RAD SECTION.                                          
048000                                                                          
048100     MOVE VORD-KVKOLPAC      TO KVKOLPAC-WS                               
048200     MOVE VORD-KVORDRAD      TO KVORDRAD-WS                               
048300     MOVE VORD-KVORDRAD-PACK TO KVORDRAD-PACK-WS                          
048400     MOVE VORD-TIUTSKR       TO WS-DATUM                                  
048500     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
048600     MOVE WS-DATUM           TO DAT-I-TIDATUM                             
048700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
048800                         DAT-O-TIDATUM DAT-KDSVAR                         
048900     IF DAT-KDSVAR-OK                                                     
049000       MOVE DAT-TIAAVVD      TO TIUTSKR-WS                                
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 J-HAMTA-MEDDELANDE SECTION.                                              
049500     EVALUATE WS-KDFEL                                                    
049600       WHEN  1 MOVE ERR-WRONG-AREA-FOR-ORDER TO RESP-IDMSG-ERROR          
049700               MOVE 'IDORDNR'          TO RESP-IDELMT-ERROR               
049800       WHEN  2 MOVE ERR-LINES-NOT-FIND TO RESP-IDMSG-ERROR                
049900               MOVE 'IDORDNR'          TO RESP-IDELMT-ERROR               
050000       WHEN  3 MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                
050100               MOVE 'KEY'              TO RESP-IDELMT-ERROR               
050200       WHEN  4 MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                
050300               MOVE 'KEY'              TO RESP-IDELMT-ERROR               
050400       WHEN  5 MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                
050500               MOVE 'KEY'              TO RESP-IDELMT-ERROR               
050600       WHEN  6 MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                
050700               MOVE 'KEY'              TO RESP-IDELMT-ERROR               
050800       WHEN  7 MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                
050900               MOVE 'KEY'              TO RESP-IDELMT-ERROR               
051000       WHEN  8 MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                
051100               MOVE 'KEY'              TO RESP-IDELMT-ERROR               
051200       WHEN  9 MOVE ERR-MISSING        TO RESP-IDMSG-ERROR                
051300               MOVE 'KEY'              TO RESP-IDELMT-ERROR               
051400     END-EVALUATE                                                         
051500     .                                                                    
051600     EJECT                                                                
051700 M-FLYTTA-TILL-RESP SECTION.                                              
051800                                                                          
051900     MOVE TIBEGPAC-WS          TO RESP-TIBEGPAC     (RESP-RAD-IND)        
052000     MOVE KDORDKL-WS           TO RESP-KDORDKL      (RESP-RAD-IND)        
052100     MOVE IDDISTR-WS           TO RESP-IDDISTR      (RESP-RAD-IND)        
052200     MOVE IDKUNDNR-WS          TO RESP-IDKUNDNR     (RESP-RAD-IND)        
052300     MOVE KDFRAKT-WS           TO RESP-KDFRAKT      (RESP-RAD-IND)        
052400     MOVE IDPRODNR-WS          TO RESP-IDPRODNR     (RESP-RAD-IND)        
052500     MOVE KVORDRAD-WS          TO RESP-KVORDRAD     (RESP-RAD-IND)        
052600     MOVE KVORDRAD-PACK-WS   TO RESP-KVORDRAD-PACK (RESP-RAD-IND)         
052700     MOVE KVKOLPAC-WS          TO RESP-KVKOLPAC     (RESP-RAD-IND)        
052800                                                                          
052900     MOVE TIUTSKR-WS           TO RESP-TIUTSKR      (RESP-RAD-IND)        
053000     MOVE WS-IDORDNR           TO RESP-IDORDNR      (RESP-RAD-IND)        
053100     MOVE KORD-IDUSER          TO RESP-IDANSTNR     (RESP-RAD-IND)        
053200                                                                          
053300     MOVE ZERO                 TO  TIBEGPAC-WS                            
053400                                   KDORDKL-WS                             
053500                                   IDDISTR-WS                             
053600                                   IDKUNDNR-WS                            
053700                                   KDFRAKT-WS                             
053800                                   IDPRODNR-WS                            
053900                                   KVORDRAD-WS                            
054000                                   KVORDRAD-PACK-WS                       
054100                                   KVKOLPAC-WS                            
054200                                   TIUTSKR-WS                             
054300     .                                                                    
054400     SKIP2                                                                
054500 N-VISA-VOLVOORDER SECTION.                                               
054600                                                                          
054700     MOVE WS-IDPRODNR         TO W-601-IDPRODNR                           
054800                                 W-411-IDPRODNR-MIN                       
054900                                 W-411-IDPRODNR-MAX                       
055000     PERFORM IMS-GET-WDE601                                               
055100     IF SEGMENT-FINNS                                                     
055200        IF VORD-IDDC = REQU-IDDC-KEY                                      
055300           MOVE VORD-KVKOLPAC   TO KVKOLPAC-WS                            
055400           MOVE VORD-KVORDRAD-PACK TO KVORDRAD-PACK-WS                    
055500           MOVE VORD-KDORDKL    TO KDORDKL-WS                             
055600           MOVE VORD-IDDISTR    TO IDDISTR-WS                             
055700           MOVE VORD-IDKUNDNR   TO IDKUNDNR-WS                            
055800           MOVE VORD-KDFRAKT    TO KDFRAKT-WS                             
055900           MOVE VORD-IDPRODNR   TO IDPRODNR-WS                            
056000                                   WS-IDPRODNR-NYCKEL                     
056100           MOVE VORD-KVORDRAD   TO KVORDRAD-WS                            
056200                                                                          
056300           MOVE VORD-TIUTSKR    TO WS-DATUM                               
056400           MOVE 'AAMMDD'        TO DAT-KDDATFORM                          
056500           MOVE WS-DATUM        TO DAT-I-TIDATUM                          
056600           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
056700                               DAT-O-TIDATUM DAT-KDSVAR                   
056800           IF DAT-KDSVAR-OK                                               
056900             MOVE DAT-TIAAVVD   TO TIUTSKR-WS                             
057000           END-IF                                                         
057100           MOVE 1               TO W-411-IDPURAD-MIN                      
057200           MOVE 99999           TO W-411-IDPURAD-MAX                      
057300           PERFORM IMS-GU-KUNDORDER-SEK-INV                               
057400                                                                          
057500           MOVE KORD-TIBEGPAC   TO WS-DATUM                               
057600           MOVE 'AAMMDD'        TO DAT-KDDATFORM                          
057700           MOVE WS-DATUM        TO DAT-I-TIDATUM                          
057800           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
057900                               DAT-O-TIDATUM DAT-KDSVAR                   
058000           IF DAT-KDSVAR-OK                                               
058100             MOVE DAT-TIAAVVD      TO TIBEGPAC-WS                         
058200           END-IF                                                         
058300           MOVE KORD-IDKUNDRF   TO WS-IDKUNDRF                            
058400           MOVE +1              TO RESP-RAD-IND                           
058500           MOVE +1              TO RESP-KVRADER                           
058600                                                                          
058700           MOVE TIBEGPAC-WS     TO RESP-TIBEGPAC   (RESP-RAD-IND)         
058800           MOVE KDORDKL-WS      TO RESP-KDORDKL    (RESP-RAD-IND)         
058900           MOVE IDDISTR-WS      TO RESP-IDDISTR    (RESP-RAD-IND)         
059000           MOVE IDKUNDNR-WS     TO RESP-IDKUNDNR   (RESP-RAD-IND)         
059100           MOVE KDFRAKT-WS      TO RESP-KDFRAKT    (RESP-RAD-IND)         
059200           MOVE IDPRODNR-WS     TO RESP-IDPRODNR   (RESP-RAD-IND)         
059300           MOVE KVORDRAD-WS     TO RESP-KVORDRAD   (RESP-RAD-IND)         
059400         MOVE KVORDRAD-PACK-WS TO RESP-KVORDRAD-PACK(RESP-RAD-IND)        
059500           MOVE KVKOLPAC-WS     TO RESP-KVKOLPAC   (RESP-RAD-IND)         
059600                                                                          
059700           MOVE TIUTSKR-WS      TO RESP-TIUTSKR    (RESP-RAD-IND)         
059800           MOVE WS-IDORDNR      TO RESP-IDORDNR    (RESP-RAD-IND)         
059900                                                                          
060000           MOVE ZERO            TO  TIBEGPAC-WS                           
060100                                    KDORDKL-WS                            
060200                                    IDDISTR-WS                            
060300                                    IDKUNDNR-WS                           
060400                                    KDFRAKT-WS                            
060500                                    IDPRODNR-WS                           
060600                                    KVORDRAD-WS                           
060700                                    KVORDRAD-PACK-WS                      
060800                                    KVKOLPAC-WS                           
060900                                    TIUTSKR-WS                            
061000                                                                          
061100        ELSE                                                              
061200           MOVE 5                TO WS-KDFEL                              
061300        END-IF                                                            
061400     ELSE                                                                 
061500        MOVE 6                   TO WS-KDFEL                              
061600     END-IF                                                               
061700     SKIP3                                                                
061800     .                                                                    
061900 S01-FLYTTA-PLOC-TILL-RAD SECTION.                                        
062000     MOVE KORD-KDORDKL      TO KDORDKL-WS                                 
062100     MOVE KORD-IDDISTR      TO IDDISTR-WS                                 
062200     MOVE KORD-IDKUNDNR     TO IDKUNDNR-WS                                
062300     MOVE KORD-IDKUNDRF     TO WS-IDKUNDRF                                
062400     MOVE KORD-KDFRAKT      TO KDFRAKT-WS                                 
062500     MOVE KORD-IDPRODNR     TO IDPRODNR-WS                                
062600                               WS-IDPRODNR-NYCKEL                         
062700                                                                          
062800     MOVE WS-SAVE-DARFS (3:6) TO WS-DATUM-TIRFS                           
062900                                                                          
063000     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
063100     MOVE WS-DATUM-TIRFS    TO DAT-I-TIDATUM                              
063200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
063300                         DAT-O-TIDATUM DAT-KDSVAR                         
063400     IF DAT-KDSVAR-OK                                                     
063500       MOVE DAT-TIAAVVD     TO TIBEGPAC-WS                                
063600     END-IF                                                               
063700     .                                                                    
063800     SKIP3                                                                
063900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
064000     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
064100                                                                          
064200     MOVE 'GETARG'               TO SUB-KDFUNC                            
064300     MOVE 'CARPARTS.LDC.ORDERSSTARTEDNOTCOMPLETED'                        
064400       TO SUB-ADDISPABS                                                   
064500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
064600                                                                          
064700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
064800                                                                          
064900     IF SUB-KDRC > 0                                                      
065000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
065100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
065200       DELIMITED BY SIZE INTO FELTEXT                                     
065300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
065400     END-IF                                                               
065500     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
065600     .                                                                    
065700     SKIP3                                                                
065800 S02-RETURN-RESPONSE SECTION.                                             
065900     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
066000                                                                          
066100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
066200     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
066300            (MAX-LINE - RESP-RAD-IND) * LENGTH OF RESP-RAD                
066400                                                                          
066500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
066600                                                                          
066700     IF SUB-KDRC > 0                                                      
066800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
066900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
067000       DELIMITED BY SIZE INTO FELTEXT                                     
067100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
067200     END-IF                                                               
067300     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
067400     .                                                                    
067500     SKIP3                                                                
067600* IMS SEKTIONER                                                           
067700     SKIP3                                                                
067800                                                                          
067900 IMS-GU-KUNDORDER-SEK-INV      SECTION.                                   
068000                                                                          
068100     STRING 'WDE411  (WDE4BSEQ>=' W-WDE411-KEYSEQ-MIN-X                   
068200                    '&WDE4BSEQ<=' W-WDE411-KEYSEQ-MAX-X ')'               
068300            DELIMITED BY SIZE INTO SSA1                                   
068400     MOVE 'WDE401   ' TO SSA2                                             
068500     MOVE '  '     TO GODK-STATUSKODER                                    
068600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA2 SSA1 SSA2                
068700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
068800     PERFORM IMS-STATUSKONTROLL                                           
068900     .                                                                    
069000     EJECT                                                                
069100 IMS-GET-WDE601   SECTION.                                                
069200                                                                          
069300     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
069400            DELIMITED BY SIZE INTO SSA1                                   
069500     MOVE '  GE' TO GODK-STATUSKODER                                      
069600     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
069700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
069800     PERFORM IMS-STATUSKONTROLL                                           
069900     .                                                                    
070000     EJECT                                                                
070100 IMS-GU-WDGX4488  SECTION.                                                
070200                                                                          
070300     STRING 'WDR401  (WDGXKEY  =' W-4487-KEY-X ')'                        
070400            DELIMITED BY SIZE INTO SSA1                                   
070500     STRING 'WDGX4488(KDPRCGRP =' W-4488-KEY-X ')'                        
070600            DELIMITED BY SIZE INTO SSA2                                   
070700     MOVE '  GE' TO GODK-STATUSKODER                                      
070800     CALL CBLTDLI USING GU 4487-PCB DLI-IO-AREA3 SSA1 SSA2                
070900     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     SKIP3                                                                
071200     .                                                                    
071300 IMS-GNP-WDGX4490       SECTION.                                          
071400                                                                          
071500     STRING 'WDR401  (WDGXKEY  =' W-4487-KEY-X ')'                        
071600            DELIMITED BY SIZE INTO SSA1                                   
071700     STRING 'WDGX4488(KDPRCGRP =' W-4488-KEY-X ')'                        
071800            DELIMITED BY SIZE INTO SSA2                                   
071900     MOVE 'WDGX4490 ' TO SSA3                                             
072000     MOVE '  GE' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING GNP 4487-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3          
072200     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     EJECT                                                                
072600 IMS-GU-KUNDORDER SECTION.                                                
072700                                                                          
072800     STRING 'WDE401  (WDE401KY =' W-WDE401-KORD-X ')'                     
072900            DELIMITED BY SIZE INTO SSA1                                   
073000     MOVE '  GE' TO GODK-STATUSKODER                                      
073100     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA2 SSA1                    
073200     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
073300     PERFORM IMS-STATUSKONTROLL                                           
073400     .                                                                    
073500     EJECT                                                                
073600 IMS-STATUSKONTROLL SECTION.                                              
073700                                                                          
073800     SET STATUS-IX TO 1                                                   
073900     SEARCH GODK-STATUS AT END CALL FELLOG                                
074000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
074100     END-SEARCH                                                           
074200     CONTINUE                                                             
074300     .                                                                    
