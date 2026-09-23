000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4070500.                                                
000400 AUTHOR.         EVA LUNDELL.                                             
000500 DATE-WRITTEN.   95/09/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000710                                                                          
000800*    FUNKTION:                                                            
000900*        REGISTRERING AV INTERNUPPACKNING ON-LINE                         
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDA2                                       
001200*        PROGRAMMET LÄSER      WDE4                                       
001300*        PROGRAMMET LÄSER      WDE6                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T705                                              
001700*        MID:         W4I70501                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O70501                                            
002100*                                                                         
002200*    E'TRACKER: 5935191 2007-11-20 STOPPA DISTR 2640 OCH 2699             
002300*                                                                         
002400                                                                          
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000*    -- CHECKED BY WY2000                                                 
003100     SKIP3                                                                
003200 77  IDPGM                       PIC X(08)   VALUE 'W4070500'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  MAX-MOD-LAENGD              PIC S9(3)  COMP-3 VALUE +200.            
004000 77  WS-IDKOLLI-MIN              PIC S9(5)  COMP-3.                       
004100 77  WS-IDKOLLI-MAX              PIC S9(5)  COMP-3.                       
004200 77  RAD-IX                      PIC S9(2)  COMP-3 VALUE ZERO.            
004300 77  MAX-IX                      PIC S9(2)  COMP-3 VALUE +13.             
004400 77  WS-IDDISTR                  PIC X(4)          VALUE ZERO.            
004500 77  WS-IDFAKT                   PIC 9(7)          VALUE ZERO.            
004600 77  WS-IDKOLLI                  PIC S9(5)  COMP-3 VALUE ZERO.            
004700 77  WS-IDKUNDNR                 PIC X(6)          VALUE ZERO.            
004800 77  WS-IDRAPPNR                 PIC X(7)          VALUE ZERO.            
004900 77  TEST-KOLLI-IDFAKT           PIC 9(7)          VALUE ZERO.            
005000                                                                          
005100     EJECT                                                                
005200                                                                          
005300 01  WS-PRFOERS.                                                          
005400    03 WS-PRFOERS-ALFA           PIC X(10).                               
005500 01  FILLER REDEFINES WS-PRFOERS.                                         
005600   03 WS-PRFOERS-NUM             PIC  9(7).9(2).                          
005700                                                                          
005800 01  WS-PRFRAKT.                                                          
005900    03 WS-PRFRAKT-ALFA           PIC X(10).                               
006000 01  FILLER REDEFINES WS-PRFRAKT.                                         
006100   03 WS-PRFRAKT-NUM             PIC  9(7).9(2).                          
006200                                                                          
006300 01  WS-PRLEGKST.                                                         
006400    03 WS-PRLEGKST-ALFA          PIC X(10).                               
006500 01  FILLER REDEFINES WS-PRLEGKST.                                        
006600   03 WS-PRLEGKST-NUM            PIC  9(7).9(2).                          
006700                                                                          
006800 01  WS-RELANDCO.                                                         
006900    03 WS-RELANDCO-ALFA          PIC X(6).                                
007000 01  FILLER REDEFINES WS-RELANDCO.                                        
007100   03 WS-RELANDCO-NUM            PIC  9(3).9(2).                          
007200     EJECT                                                                
007300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007400                                                                          
007500                                                                          
007600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007700     88  NYCKLAR-OK                          VALUE 'J'.                   
007800     88  NYCKLAR-FEL                         VALUE 'N'.                   
007900                                                                          
008000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008100     88  INDATA-OK                           VALUE 'J'.                   
008200     88  INDATA-FEL                          VALUE 'N'.                   
008300                                                                          
008400 77  KOLLA-ALLA-KOLLI-SW         PIC X       VALUE 'N'.                   
008500     88  KOLLA-ALLA-KOLLI                    VALUE 'J'.                   
008600     88  KOLLA-UTVALDA-KOLLI                 VALUE 'N'.                   
008700                                                                          
008800 77  KOLLA-LEVANM-SW            PIC X        VALUE 'N'.                   
008900     88  KOLLA-LEVANM                        VALUE 'J'.                   
009000                                                                          
009100 77  DISTRIKT-IFYLLT-SW         PIC X        VALUE 'N'.                   
009200     88  DISTRIKT-IFYLLT                     VALUE 'J'.                   
009300                                                                          
009400 77  KUNDNR-IFYLLT-SW           PIC X        VALUE 'N'.                   
009500     88  KUNDNR-IFYLLT                       VALUE 'J'.                   
009600                                                                          
009700 77  RAPPNR-IFYLLT-SW           PIC X        VALUE 'N'.                   
009800     88  RAPPNR-IFYLLT                       VALUE 'J'.                   
009900                                                                          
010000 77  ORDERNR-IFYLLT-SW          PIC X        VALUE 'N'.                   
010100     88  ORDERNR-IFYLLT                      VALUE 'J'.                   
010200                                                                          
010300 77  FAKTNR-IFYLLT-SW           PIC X        VALUE 'N'.                   
010400     88  FAKTNR-IFYLLT                       VALUE 'J'.                   
010500                                                                          
010600 77  PRODNR-IFYLLT-SW           PIC X        VALUE 'N'.                   
010700     88  PRODNR-IFYLLT                       VALUE 'J'.                   
010800                                                                          
010900 77  KOLLI-FOM-IFYLLT-SW        PIC X        VALUE 'N'.                   
011000     88  KOLLI-FOM-IFYLLT                    VALUE 'J'.                   
011100                                                                          
011200 77  KOLLI-TOM-IFYLLT-SW        PIC X        VALUE 'N'.                   
011300     88  KOLLI-TOM-IFYLLT                    VALUE 'J'.                   
011400                                                                          
011500 77  RELANDCO-IFYLLD-SW         PIC X        VALUE 'J'.                   
011600     88  RELANDCO-IFYLLD                     VALUE 'J'.                   
011700                                                                          
011800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011900     88  EGEN-MID                            VALUE '4705'.                
012000     88  GODK-MID                            VALUE '4701' '4702'          
012100                                                   '4703' '4704'          
012200                                                   '4705' '4706'          
012300                                                   '4707' '4708'          
012400                                                   '4709'.                
012500     88  HELP-MID                            VALUE '0551'.                
012600     EJECT                                                                
012700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012800 01  GENERELLA-SUBPROGRAM.                                                
012900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
013000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013400     EJECT                                                                
013500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013600*01 -COPY WMEDAREA                                                        
013700                                                                          
013800 01  MESSAGE-CODES.                                                       
013900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014000     03  ERR-FEL-KOLLI           PIC X(3)    VALUE '732'.                 
014100     03  ERR-FAKTURA-SAKNAS      PIC X(3)    VALUE '320'.                 
014200     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
014300     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '054'.                 
014400     03  ERR-PRODNR-SAKNAS       PIC X(3)    VALUE '280'.                 
014500     03  ERR-FINNS-REDAN         PIC X(3)    VALUE '245'.                 
014600     03  ERR-FEL-IFYLLD-RAD      PIC X(3)    VALUE '184'.                 
014700     03  ERR-INFO-SAKNAS         PIC X(3)    VALUE '413'.                 
014710     03  ERR-UPPDAT-EJ-TILLATEN  PIC X(3)    VALUE '777'.                 
014800     03  INF-UPPDAT-GJORD        PIC X(3)    VALUE '101'.                 
014900     EJECT                                                                
015000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
015700*                                                                         
015800 01  DECAREA.                                                             
015900*    03  WDECAREA   -COPY WDECAREA                                        
016000     EJECT                                                                
016100**  PARAMETRAR FÖR START AV PGM W4796                                     
016200 01  PROGRAMHOPP-AREOR.                                                   
016300     03  FILLER                  PIC X(16)   VALUE 'P-TO-P-AREA'.         
016400     03  P-TO-P-SW.                                                       
016500         05  P-TO-P-KVLL         PIC S9(4) COMP SYNC.                     
016600         05  P-TO-P-KDZ1         PIC X(1)  VALUE LOW-VALUE.               
016700         05  P-TO-P-KDZ2         PIC X(1)  VALUE LOW-VALUE.               
016800         05  P-TO-P-KDTRANS      PIC X(8)  VALUE 'W4T796X '.              
016900         05  P-TO-P-IDTRANS      PIC X(4)  VALUE '4796'.                  
017000         05  P-TO-P-KDMFSFOR     PIC X(1).                                
017100         03  MID -COPY W4I79601  -PRE 4796-                               
017200     EJECT                                                                
017300*01 -COPY WMSGINIT                                                        
017400                                                                          
017500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017600*                                                                         
017700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017800                                                                          
017900*01  MID -COPY W4I70501                                                   
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018200                                                                          
018300*01  -COPY WMSGAREA                                                       
018400     EJECT                                                                
018500     03  MOD REDEFINES MSG-AREA.                                          
018600*      05  -COPY W4O70501                                                 
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018900                                                                          
019000*01  -COPY WMFSAREA                                                       
019100     EJECT                                                                
019200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019300*                                                                         
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019600                                                                          
019700 01  NYCKLAR-TILL-DLI.                                                    
019800     03  W-IDLEVANM-X.                                                    
019900         05  W-IDLEVANM.                                                  
020000             07  W-IDDISTR       PIC S9(5) COMP-3 VALUE ZERO.             
020100             07  W-IDKUNDNR      PIC S9(7) COMP-3 VALUE ZERO.             
020200             07  W-IDRAPPNR      PIC 9(7)         VALUE ZERO.             
020300                                                                          
020400     03  W-WDE4A1KY-MIN-X.                                                
020500         05  W-SEQA-IDGMTREF-MIN.                                         
020600           07  W-SEQA-IDDISTR-MIN    PIC S9(5) COMP-3 VALUE ZERO.         
020700           07  W-SEQA-IDKUNDNR-MIN   PIC S9(7) COMP-3 VALUE ZERO.         
020800           07  W-SEQA-IDKUNDRF-MIN   PIC X(10).                           
020900           07  W-SEQA-IDORDNR5-FILLER-MIN                                 
021000                               REDEFINES W-SEQA-IDKUNDRF-MIN.             
021100               09  W-SEQA-IDORDNR5-MIN  PIC 9(5).                         
021200               09  FILLER        PIC X(5).                                
021300         05  W-SEQA-IDPRODNR-MIN     PIC S9(7) COMP-3 VALUE ZERO.         
021400         05  W-SEQA-IDPLKLST-MIN     PIC S9(3) COMP-3 VALUE ZERO.         
021500     03  W-WDE4A1KY-MAX-X.                                                
021600         05  W-SEQA-IDGMTREF-MAX.                                         
021700           07  W-SEQA-IDDISTR-MAX    PIC S9(5) COMP-3 VALUE ZERO.         
021800           07  W-SEQA-IDKUNDNR-MAX   PIC S9(7) COMP-3 VALUE ZERO.         
021900           07  W-SEQA-IDKUNDRF-MAX   PIC X(10).                           
022000           07  W-SEQA-IDORDNR5-FILLER-MAX                                 
022100                           REDEFINES W-SEQA-IDKUNDRF-MAX.                 
022200               09  W-SEQA-IDORDNR5-MAX  PIC 9(5).                         
022300               09  FILLER        PIC X(5).                                
022400         05  W-SEQA-IDPRODNR-MAX     PIC S9(7) COMP-3 VALUE ZERO.         
022500         05  W-SEQA-IDPLKLST-MAX     PIC S9(3) COMP-3 VALUE ZERO.         
022600                                                                          
022700     03  W-IDPRODNR-X.                                                    
022800         05  W-IDPRODNR          PIC S9(7) COMP-3 VALUE ZERO.             
022900     03  W-IDKOLLI-X.                                                     
023000         05  W-IDKOLLI          PIC S9(5) COMP-3 VALUE ZERO.              
023100                                                                          
023200*    --- STATUS-KOD FRÅN IMS                                              
023300 01  STATUS-WS                   PIC XX.                                  
023400     88  SEGMENT-FINNS                       VALUE '  '.                  
023500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023700                                                                          
023800 01  GODK-STATUSKODER.                                                    
023900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024000                                                                          
024100 01  SSA1                        PIC X(148).                              
024200 01  SSA2                        PIC X(64).                               
024300     EJECT                                                                
024400*    --- IMS FUNKTIONSKODER                                               
024500*01  -COPY W0003                                                          
024600     EJECT                                                                
024700*    ---  DLI INPUT-OUTPUT AREA                                           
024800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A201'.         
024900 01  DLI-IO-A201.                                                         
025000*    03  -COPY WDA201                                                     
025100     EJECT                                                                
025200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E4A1'.         
025300 01  DLI-IO-E4A1.                                                         
025400*    03  -COPY WDE4A1                                                     
025500     EJECT                                                                
025600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
025700 01  DLI-IO-E601.                                                         
025800*    03  -COPY WDE601                                                     
025900     EJECT                                                                
026000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E611'.         
026100 01  DLI-IO-E611.                                                         
026200*    03  -COPY WDE611                                                     
026300     EJECT                                                                
026400 LINKAGE SECTION.                                                         
026500                                                                          
026600*01  -COPY W0009   -PRE MSG-                                              
026700*01  -COPY W0009   -PRE 4796-                                             
026800*01  -COPY W0008   -PRE WDP7-                                             
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100*01  -COPY W0008  -PRE WDA2-                                              
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE WDE4A-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008  -PRE WDE6-                                              
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000 PROCEDURE DIVISION  USING MSG-PCB 4796-PCB WDP7-PCB WDA2-PCB             
028100                         WDE4A-PCB WDE6-PCB.                              
028200     ENTRY 'DLITCBL' USING MSG-PCB 4796-PCB WDP7-PCB WDA2-PCB             
028300                         WDE4A-PCB WDE6-PCB.                              
028400                                                                          
028500     PERFORM IMS-GET-MSG                                                  
028600                                                                          
028700     IF SEGMENT-FINNS                                                     
028800       PERFORM A-INIT                                                     
028900       PERFORM B-KOLLA-NYCKLAR                                            
029000       IF NYCKLAR-OK AND EGEN-MID                                         
029100         PERFORM G-FORMELLA-KONTROLLER                                    
029200         IF INDATA-OK                                                     
029300           PERFORM H-SKAPA-MID-SKICKA-TRANS                               
029400           PERFORM F-LAES-VISA-INFO                                       
029500         END-IF                                                           
029600       END-IF                                                             
029700       PERFORM IMS-INSERT-MSG                                             
029800     END-IF                                                               
029900                                                                          
030000     MOVE ZERO TO RETURN-CODE                                             
030100     GOBACK                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 A-INIT SECTION.                                                          
030500                                                                          
030600     IF MSG-DUBBLA-TRANSKODER                                             
030700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I70501                 
030800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
030900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
031000     ELSE                                                                 
031100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I70501                 
031200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
031300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
031400     END-IF                                                               
031500                                                                          
031600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031900                                                                          
032000     MOVE LOW-VALUE TO MSG-AREA                                           
032100     MOVE 'W4O70501' TO MFS-IDMOD                                         
032200     MOVE '4705' TO MOD-IDTRANS                                           
032300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
032400                             MOD-TEMFSINF                                 
032500                             MOD-IDDISTR-IN                               
032600                             MOD-IDKUNDNR-IN                              
032700                             MOD-IDRAPPNR-IN                              
032800                                                                          
032900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O70501 + 4                        
033000                                                                          
033100                                                                          
033200     MOVE SPACE                       TO MED-IDMFSINF                     
033300     MOVE SPACE                       TO MED-IDMFSFEL                     
033400                                                                          
033500     IF EGEN-MID OR HELP-MID                                              
033600       CONTINUE                                                           
033700     ELSE                                                                 
033800       MOVE SPACE TO MFS-KDTRTYP                                          
033900       MOVE '7' TO MFS-IDPFK                                              
034000     END-IF                                                               
034100                                                                          
034200     IF ENGLISH-TEXT                                                      
034300       MOVE 'GB '                         TO MED-IDSKYLT                  
034400     ELSE                                                                 
034500       MOVE 'S  '                         TO MED-IDSKYLT                  
034600     END-IF                                                               
034700                                                                          
034800     .                                                                    
034900     EJECT                                                                
035000 B-KOLLA-NYCKLAR SECTION.                                                 
035100                                                                          
035200     MOVE JA TO NYCKLAR-SW                                                
035300                                                                          
035400     MOVE LOW-VALUE TO W-WDE4A1KY-MIN-X                                   
035500     MOVE HIGH-VALUE TO W-WDE4A1KY-MAX-X                                  
035600                                                                          
035700     PERFORM BA-KOLLA-IDDISTR                                             
035800     PERFORM BB-KOLLA-IDKUNDNR                                            
035900     PERFORM BC-KOLLA-IDRAPPNR                                            
036000     IF EGEN-MID                                                          
036100       PERFORM BD-KOLLA-IDORDERNR                                         
036200       PERFORM BE-KOLLA-IDFAKTNR                                          
036300       PERFORM BF-KOLLA-IDPRODNR                                          
036400       PERFORM BG-KOLLA-KOLLI                                             
036500       PERFORM BH-KOLLA-RELANDCO                                          
036600       PERFORM BI-KOLLA-PRFRAKT                                           
036700       PERFORM BJ-KOLLA-PRFOERS                                           
036800       PERFORM BK-KOLLA-PRLEGKST                                          
036900     ELSE                                                                 
037000       PERFORM MFS-RENSA-FAELT-UT                                         
037100     END-IF                                                               
037200                                                                          
037300     IF EGEN-MID                                                          
037400     AND DISTRIKT-IFYLLT                                                  
037500     AND KUNDNR-IFYLLT                                                    
037600     AND RAPPNR-IFYLLT                                                    
037700     AND MID-IDORDNR5 =   ALL '+'                                         
037800     AND MID-IDFAKT =     ALL '+'                                         
037900     AND MID-IDPRODNR =   ALL '+'                                         
038000         MOVE JA TO NYCKLAR-SW                                            
038100                    KOLLA-LEVANM-SW                                       
038200         PERFORM MFS-RENSA-SAMTLIGA-FAELT-UT                              
038300     END-IF                                                               
038400                                                                          
038500     IF EGEN-MID                                                          
038600       MOVE WS-IDDISTR         TO MOD-IDDISTR-UT                          
038700       MOVE WS-IDKUNDNR        TO MOD-IDKUNDNR-UT                         
038800       MOVE WS-IDRAPPNR        TO MOD-IDRAPPNR-UT                         
038900     ELSE                                                                 
039000       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
039100       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-UT                            
039200       MOVE MFS-RENSA-FAELT TO MOD-IDRAPPNR-UT                            
039300     END-IF                                                               
039400                                                                          
039410*-- EJ OK ATT GÖRA INTERNUPPAKNING FÖR RYSSLAND ENLIGT PETER WIIK.        
039500     IF WS-IDDISTR = '2640' OR '2699' OR '2602' OR '2697'                 
039501       MOVE ERR-UPPDAT-EJ-TILLATEN TO MED-IDMFSFEL                        
039502       CALL WMEDKONV USING MED-WMEDAREA                                   
039503       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
039504       MOVE NEJ TO NYCKLAR-SW                                             
039505     END-IF                                                               
039510                                                                          
039600     IF NYCKLAR-FEL                                                       
039700       IF EGEN-MID                                                        
039800          PERFORM MFS-ROER-EJ-FAELT-UT                                    
039900       ELSE                                                               
040000           PERFORM MFS-RENSA-FAELT-IN                                     
040100           PERFORM MFS-RENSA-SAMTLIGA-FAELT-UT                            
040200       END-IF                                                             
040300       IF RELANDCO-IFYLLD                                                 
040400         IF MED-IDMFSFEL = SPACE                                          
040500           MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                           
040600           CALL WMEDKONV USING MED-WMEDAREA                               
040700           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
040800         END-IF                                                           
040900       ELSE                                                               
040910         IF MED-IDMFSFEL = SPACE                                          
041000           MOVE ERR-INFO-SAKNAS TO MED-IDMFSFEL                           
041100           CALL WMEDKONV USING MED-WMEDAREA                               
041200           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
041300         END-IF                                                           
041310       END-IF                                                             
041400     ELSE                                                                 
041500                                                                          
041600       MOVE WS-IDDISTR         TO MOD-IDDISTR-UT                          
041700       MOVE WS-IDKUNDNR        TO MOD-IDKUNDNR-UT                         
041800       MOVE WS-IDRAPPNR        TO MOD-IDRAPPNR-UT                         
041900     END-IF                                                               
042000     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
042100     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
042200     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
042300     .                                                                    
042400     EJECT                                                                
042500                                                                          
042600 BA-KOLLA-IDDISTR SECTION.                                                
042700                                                                          
042800     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
042900       MOVE MID-IDDISTR-IN      TO WS-IDDISTR                             
043000       MOVE SPACE               TO MFS-KDTRTYP                            
043100       MOVE '7'                 TO MFS-IDPFK                              
043200       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
043300       IF WS-IDDISTR NUMERIC  AND                                         
043400          WS-IDDISTR > ZERO                                               
043500         MOVE WS-IDDISTR        TO W-SEQA-IDDISTR-MIN                     
043600                                   W-SEQA-IDDISTR-MAX                     
043700                                   W-IDDISTR                              
043800                                   MOD-IDDISTR-UT                         
043900         MOVE JA                TO DISTRIKT-IFYLLT-SW                     
044000       ELSE                                                               
044100         MOVE NEJ TO NYCKLAR-SW                                           
044200       END-IF                                                             
044300     ELSE                                                                 
044400       MOVE MID-IDDISTR-UT      TO WS-IDDISTR                             
044500       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
044600       IF WS-IDDISTR NUMERIC AND                                          
044700          WS-IDDISTR > ZERO                                               
044800           MOVE WS-IDDISTR    TO W-SEQA-IDDISTR-MIN                       
044900                                 W-SEQA-IDDISTR-MAX                       
045000                                 W-IDDISTR                                
045100                                 MOD-IDDISTR-UT                           
045200           MOVE JA            TO DISTRIKT-IFYLLT-SW                       
045300       ELSE                                                               
045400           MOVE NEJ           TO NYCKLAR-SW                               
045500       END-IF                                                             
045600     END-IF                                                               
045700                                                                          
045800     .                                                                    
045900     EJECT                                                                
046000 BB-KOLLA-IDKUNDNR SECTION.                                               
046100                                                                          
046200*    -- KONTROLL AV IDKUNDNR                                              
046300                                                                          
046400     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
046500       MOVE MID-IDKUNDNR-IN     TO WS-IDKUNDNR                            
046600       MOVE SPACE               TO MFS-KDTRTYP                            
046700       MOVE '7'                 TO MFS-IDPFK                              
046800       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
046900       IF WS-IDKUNDNR NUMERIC                                             
047000         MOVE WS-IDKUNDNR       TO W-SEQA-IDKUNDNR-MIN                    
047100                                   W-SEQA-IDKUNDNR-MAX                    
047200                                   W-IDKUNDNR                             
047300                                   MOD-IDKUNDNR-UT                        
047400         MOVE JA                TO KUNDNR-IFYLLT-SW                       
047500       ELSE                                                               
047600         MOVE NEJ TO NYCKLAR-SW                                           
047700       END-IF                                                             
047800     ELSE                                                                 
047900       IF MID-IDKUNDNR-UT NOT = ALL '+'                                   
048000         MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                            
048100         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
048200         IF WS-IDKUNDNR NUMERIC                                           
048300             MOVE WS-IDKUNDNR   TO W-SEQA-IDKUNDNR-MIN                    
048400                                   W-SEQA-IDKUNDNR-MAX                    
048500                                   W-IDKUNDNR                             
048600                                   MOD-IDKUNDNR-UT                        
048700             MOVE JA            TO KUNDNR-IFYLLT-SW                       
048800         ELSE                                                             
048900             MOVE NEJ           TO NYCKLAR-SW                             
049000         END-IF                                                           
049100       ELSE                                                               
049200           MOVE NEJ             TO NYCKLAR-SW                             
049300       END-IF                                                             
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 BC-KOLLA-IDRAPPNR SECTION.                                               
049800                                                                          
049900                                                                          
050000*    -- KONTROLL AV IDRAPPNR                                              
050100                                                                          
050200     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
050300       MOVE MID-IDRAPPNR-IN     TO WS-IDRAPPNR                            
050400       MOVE SPACE               TO MFS-KDTRTYP                            
050500       MOVE '7'                 TO MFS-IDPFK                              
050600       INSPECT WS-IDRAPPNR REPLACING LEADING SPACE BY ZERO                
050700       IF WS-IDRAPPNR NUMERIC  AND                                        
050800          WS-IDRAPPNR > ZERO                                              
050900         MOVE WS-IDRAPPNR       TO W-IDRAPPNR                             
051000                                   MOD-IDRAPPNR-UT                        
051100         MOVE JA                TO RAPPNR-IFYLLT-SW                       
051200       ELSE                                                               
051300         MOVE NEJ               TO NYCKLAR-SW                             
051400       END-IF                                                             
051500     ELSE                                                                 
051600       IF MID-IDRAPPNR-UT NOT = ALL '+'                                   
051700         MOVE MID-IDRAPPNR-UT   TO WS-IDRAPPNR                            
051800         INSPECT WS-IDRAPPNR REPLACING LEADING SPACE BY ZERO              
051900         IF WS-IDRAPPNR NUMERIC AND                                       
052000            WS-IDRAPPNR > ZERO                                            
052100             MOVE WS-IDRAPPNR   TO W-IDRAPPNR                             
052200                                   MOD-IDRAPPNR-UT                        
052300             MOVE JA            TO RAPPNR-IFYLLT-SW                       
052400         ELSE                                                             
052500             MOVE NEJ           TO NYCKLAR-SW                             
052600         END-IF                                                           
052700       ELSE                                                               
052800           MOVE NEJ             TO NYCKLAR-SW                             
052900       END-IF                                                             
053000     END-IF                                                               
053100     .                                                                    
053200     EJECT                                                                
053300 BD-KOLLA-IDORDERNR SECTION.                                              
053400                                                                          
053500                                                                          
053600*    -- KONTROLL AV IDORDERNR                                             
053700                                                                          
053800     IF MID-IDORDNR5    NOT = ALL '+'                                     
053900       MOVE '7'         TO MFS-IDPFK                                      
054000       MOVE SPACE       TO MFS-KDTRTYP                                    
054100       INSPECT MID-IDORDNR5    REPLACING LEADING SPACE BY ZERO            
054200       IF MID-IDORDNR5    NUMERIC  AND                                    
054300          MID-IDORDNR5    > ZERO                                          
054400         MOVE MID-IDORDNR5    TO W-SEQA-IDKUNDRF-MIN                      
054500                                 W-SEQA-IDKUNDRF-MAX                      
054600         MOVE JA TO ORDERNR-IFYLLT-SW                                     
054700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDORDNR5-ATTR                   
054800       ELSE                                                               
054900         MOVE NEJ TO NYCKLAR-SW                                           
055000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDORDNR5-ATTR                   
055100       END-IF                                                             
055200     ELSE                                                                 
055300       MOVE NEJ TO NYCKLAR-SW                                             
055400       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDORDNR5-ATTR                     
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800                                                                          
055900 BE-KOLLA-IDFAKTNR SECTION.                                               
056000                                                                          
056100                                                                          
056200*    -- KONTROLL AV IDFAKTNR                                              
056300                                                                          
056400     IF MID-IDFAKT      NOT = ALL '+'                                     
056500       MOVE '7'         TO MFS-IDPFK                                      
056600       MOVE SPACE       TO MFS-KDTRTYP                                    
056700       INSPECT MID-IDFAKT      REPLACING LEADING SPACE BY ZERO            
056800       IF MID-IDFAKT      NUMERIC  AND                                    
056900          MID-IDFAKT      > ZERO                                          
057000         MOVE MID-IDFAKT      TO WS-IDFAKT                                
057100         MOVE JA TO FAKTNR-IFYLLT-SW                                      
057200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDFAKT-ATTR                     
057300       ELSE                                                               
057400         MOVE NEJ TO NYCKLAR-SW                                           
057500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDFAKT-ATTR                     
057600       END-IF                                                             
057700     ELSE                                                                 
057800       MOVE NEJ TO NYCKLAR-SW                                             
057900       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDFAKT-ATTR                       
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300 BF-KOLLA-IDPRODNR SECTION.                                               
058400                                                                          
058500                                                                          
058600*    -- KONTROLL AV IDPRODNR                                              
058700                                                                          
058800     IF MID-IDPRODNR    NOT = ALL '+'                                     
058900       MOVE '7'         TO MFS-IDPFK                                      
059000       MOVE SPACE       TO MFS-KDTRTYP                                    
059100       INSPECT MID-IDPRODNR    REPLACING LEADING SPACE BY ZERO            
059200       IF MID-IDPRODNR    NUMERIC  AND                                    
059300          MID-IDPRODNR    > ZERO                                          
059400         MOVE MID-IDPRODNR    TO W-SEQA-IDPRODNR-MIN                      
059500                                 W-SEQA-IDPRODNR-MAX                      
059600                                 W-IDPRODNR                               
059700         MOVE JA TO PRODNR-IFYLLT-SW                                      
059800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRODNR-ATTR                   
059900       ELSE                                                               
060000         MOVE NEJ TO NYCKLAR-SW                                           
060100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRODNR-ATTR                   
060200       END-IF                                                             
060300     ELSE                                                                 
060400       MOVE NEJ TO NYCKLAR-SW                                             
060500       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRODNR-ATTR                     
060600     END-IF                                                               
060700                                                                          
060800     .                                                                    
060900     EJECT                                                                
061000 BG-KOLLA-KOLLI SECTION.                                                  
061100                                                                          
061200     MOVE +1 TO RAD-IX                                                    
061300     PERFORM UNTIL RAD-IX > MAX-IX                                        
061400         INSPECT MID-IDKOLLI-FOM (RAD-IX)                                 
061500             REPLACING ALL '+' BY SPACE                                   
061600         INSPECT MID-IDKOLLI-TOM (RAD-IX)                                 
061700             REPLACING ALL '+' BY SPACE                                   
061800         ADD +1 TO RAD-IX                                                 
061900     END-PERFORM                                                          
062000                                                                          
062100     PERFORM BGA-KOLLA-KOLLI-RAD-ETT                                      
062200                                                                          
062300     PERFORM BGB-KOLLA-KOLLI-RESTEN-RADER                                 
062400                                                                          
062500     .                                                                    
062600     EJECT                                                                
062700 BGA-KOLLA-KOLLI-RAD-ETT SECTION.                                         
062800                                                                          
062900***                                                                       
063000**   KONTOLL AV ATT MID-IDKOLLI-FOM ÄR < MID-IDKOLLLI-TOM                 
063100**   DESSUTOM ATT KOLLI-FOM ÄR IFYLLT OM KOLLI-TOM ÄR                     
063200**   IFYLLT. DETTA GÄLLER ENDAST FÖR DEN FÖRSTA RADEN AV                  
063300**   KOLLI-INTERVALL.                                                     
063400***                                                                       
063500                                                                          
063600     MOVE +1 TO RAD-IX                                                    
063700     IF MID-IDKOLLI-FOM   (RAD-IX) NOT = SPACE                            
063800       IF MID-IDKOLLI-TOM (RAD-IX) NOT = SPACE                            
063900           IF MID-IDKOLLI-TOM (RAD-IX) <= MID-IDKOLLI-FOM (RAD-IX)        
064000               MOVE NEJ                TO NYCKLAR-SW                      
064100               MOVE MFS-ALFA-FAELT-FEL TO                                 
064200                    MOD-IDKOLLI-FOM-ATTR (RAD-IX)                         
064300                    MOD-IDKOLLI-TOM-ATTR (RAD-IX)                         
064400               MOVE JA                 TO KOLLI-FOM-IFYLLT-SW             
064500               MOVE ERR-FEL-KOLLI      TO MED-IDMFSFEL                    
064600               CALL WMEDKONV USING MED-WMEDAREA                           
064700               MOVE MED-MFSFEL       TO MOD-TEMFSFEL                      
064800           ELSE                                                           
064900             MOVE MFS-ALFA-FAELT-RAETT TO                                 
065000                                   MOD-IDKOLLI-FOM-ATTR (RAD-IX)          
065100                                   MOD-IDKOLLI-TOM-ATTR (RAD-IX)          
065200           END-IF                                                         
065300       ELSE                                                               
065400         MOVE MFS-ALFA-FAELT-RAETT      TO                                
065500                               MOD-IDKOLLI-FOM-ATTR (RAD-IX)              
065600                               MOD-IDKOLLI-TOM-ATTR (RAD-IX)              
065700       END-IF                                                             
065800     ELSE                                                                 
065900       IF MID-IDKOLLI-TOM (RAD-IX) NOT = SPACE                            
066000           MOVE NEJ                     TO INDATA-SW                      
066100           MOVE ERR-FEL-KOLLI           TO MED-IDMFSFEL                   
066200           CALL WMEDKONV USING MED-WMEDAREA                               
066300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
066400               MOVE MFS-ALFA-FAELT-FEL  TO                                
066500                    MOD-IDKOLLI-FOM-ATTR (RAD-IX)                         
066600                    MOD-IDKOLLI-TOM-ATTR (RAD-IX)                         
066700               MOVE NEJ                 TO NYCKLAR-SW                     
066800       ELSE                                                               
066900           MOVE JA                      TO KOLLA-ALLA-KOLLI-SW            
067000           MOVE MFS-ALFA-FAELT-RAETT    TO                                
067100                                   MOD-IDKOLLI-FOM-ATTR (RAD-IX)          
067200                                   MOD-IDKOLLI-TOM-ATTR (RAD-IX)          
067300                                                                          
067400       END-IF                                                             
067500     END-IF                                                               
067600                                                                          
067700     .                                                                    
067800     EJECT                                                                
067900 BGB-KOLLA-KOLLI-RESTEN-RADER SECTION.                                    
068000                                                                          
068100                                                                          
068200     MOVE +2 TO RAD-IX                                                    
068300*** KONTROLL AV ATT KOLLI-FOM < KOLLI-TOM                                 
068400***                                                                       
068500                                                                          
068600     PERFORM UNTIL RAD-IX > MAX-IX                                        
068700        IF MID-IDKOLLI-FOM (RAD-IX) NOT = SPACE                           
068800            IF MID-IDKOLLI-TOM (RAD-IX) NOT = SPACE                       
068900                IF MID-IDKOLLI-FOM (RAD-IX) >=                            
069000                   MID-IDKOLLI-TOM (RAD-IX)                               
069100                     MOVE ERR-FEL-KOLLI TO MED-IDMFSFEL                   
069200                     CALL WMEDKONV USING  MED-WMEDAREA                    
069300                     MOVE MED-MFSFEL   TO MOD-TEMFSFEL                    
069400                     MOVE MFS-ALFA-FAELT-FEL TO                           
069500                          MOD-IDKOLLI-FOM-ATTR (RAD-IX)                   
069600                          MOD-IDKOLLI-TOM-ATTR (RAD-IX)                   
069700                     MOVE NEJ TO NYCKLAR-SW                               
069800                ELSE                                                      
069900                  MOVE MFS-ALFA-FAELT-RAETT TO                            
070000                                   MOD-IDKOLLI-FOM-ATTR (RAD-IX)          
070100                                   MOD-IDKOLLI-TOM-ATTR (RAD-IX)          
070200                END-IF                                                    
070300            ELSE                                                          
070400              MOVE MFS-ALFA-FAELT-RAETT TO                                
070500                               MOD-IDKOLLI-FOM-ATTR (RAD-IX)              
070600                               MOD-IDKOLLI-TOM-ATTR (RAD-IX)              
070700            END-IF                                                        
070800        ELSE                                                              
070900            IF MID-IDKOLLI-TOM (RAD-IX) NOT = SPACE                       
071000                MOVE NEJ TO INDATA-SW                                     
071100                MOVE ERR-FEL-KOLLI TO MED-IDMFSFEL                        
071200                CALL WMEDKONV USING  MED-WMEDAREA                         
071300                MOVE MED-MFSFEL   TO MOD-TEMFSFEL                         
071400                MOVE MFS-ALFA-FAELT-FEL TO                                
071500                     MOD-IDKOLLI-FOM-ATTR (RAD-IX)                        
071600                     MOD-IDKOLLI-TOM-ATTR (RAD-IX)                        
071700            ELSE                                                          
071800                MOVE MFS-ALFA-FAELT-RAETT TO                              
071900                               MOD-IDKOLLI-FOM-ATTR (RAD-IX)              
072000                               MOD-IDKOLLI-TOM-ATTR (RAD-IX)              
072100            END-IF                                                        
072200        END-IF                                                            
072300                                                                          
072400****  KONTROLL AV ATT DET SOM ÄR SKRIVET PÅ RADEN                         
072500***   ÄR STÖRRE ÄN DET SOM STÅR PÅ FÖREGÅENDE RAD                         
072600                                                                          
072700        IF MID-IDKOLLI-FOM (RAD-IX) NOT = SPACE                           
072800            IF MID-IDKOLLI-TOM (RAD-IX - 1) NOT = SPACE                   
072900                IF MID-IDKOLLI-FOM (RAD-IX) <=                            
073000                    MID-IDKOLLI-TOM (RAD-IX - 1)                          
073100                        MOVE ERR-FEL-KOLLI TO MED-IDMFSFEL                
073200                        CALL WMEDKONV USING  MED-WMEDAREA                 
073300                        MOVE MED-MFSFEL   TO MOD-TEMFSFEL                 
073400                        MOVE MFS-ALFA-FAELT-FEL TO                        
073500                             MOD-IDKOLLI-FOM-ATTR (RAD-IX)                
073600                             MOD-IDKOLLI-TOM-ATTR (RAD-IX)                
073700                        MOVE NEJ TO NYCKLAR-SW                            
073800                ELSE                                                      
073900                    MOVE MFS-ALFA-FAELT-RAETT TO                          
074000                               MOD-IDKOLLI-FOM-ATTR (RAD-IX)              
074100                               MOD-IDKOLLI-TOM-ATTR (RAD-IX)              
074200                END-IF                                                    
074300            ELSE                                                          
074400                IF MID-IDKOLLI-FOM (RAD-IX - 1) = SPACE                   
074500                    MOVE ERR-FEL-KOLLI TO MED-IDMFSFEL                    
074600                    CALL WMEDKONV USING   MED-WMEDAREA                    
074700                    MOVE MED-MFSFEL   TO MOD-TEMFSFEL                     
074800                    MOVE MFS-ALFA-FAELT-FEL TO                            
074900                         MOD-IDKOLLI-FOM-ATTR (RAD-IX)                    
075000                         MOD-IDKOLLI-TOM-ATTR (RAD-IX)                    
075100                    MOVE NEJ TO NYCKLAR-SW                                
075200                ELSE                                                      
075300                    IF MID-IDKOLLI-FOM (RAD-IX) <=                        
075400                       MID-IDKOLLI-FOM (RAD-IX - 1)                       
075500                         MOVE ERR-FEL-KOLLI TO MED-IDMFSFEL               
075600                         CALL WMEDKONV USING  MED-WMEDAREA                
075700                         MOVE MED-MFSFEL   TO MOD-TEMFSFEL                
075800                         MOVE NEJ TO INDATA-SW                            
075900                         MOVE MFS-ALFA-FAELT-FEL TO                       
076000                              MOD-IDKOLLI-FOM-ATTR (RAD-IX)               
076100                              MOD-IDKOLLI-TOM-ATTR (RAD-IX)               
076200                         MOVE NEJ TO NYCKLAR-SW                           
076300                    ELSE                                                  
076400                        MOVE MFS-ALFA-FAELT-RAETT TO                      
076500                               MOD-IDKOLLI-FOM-ATTR (RAD-IX)              
076600                               MOD-IDKOLLI-TOM-ATTR (RAD-IX)              
076700                    END-IF                                                
076800                END-IF                                                    
076900            END-IF                                                        
077000        END-IF                                                            
077100        ADD +1 TO RAD-IX                                                  
077200     END-PERFORM                                                          
077300                                                                          
077400     .                                                                    
077500     EJECT                                                                
077600 BH-KOLLA-RELANDCO              SECTION.                                  
077700                                                                          
077800*    -- KONTROLL AV RELANDCO                                              
077900                                                                          
078000     MOVE JA                              TO RELANDCO-IFYLLD-SW           
078100     IF MID-RELANDCO NOT = ALL '+'                                        
078200        MOVE MID-RELANDCO                 TO DEC-IDFRIDATA                
078300        MOVE 3                            TO DEC-KVHELTAL                 
078400        MOVE 2                            TO DEC-KVDECIMAL                
078500        CALL WDECEDIT USING WDECAREA                                      
078600        IF DEC-KDSVAR-OK                                                  
078700           MOVE DEC-IDEDITDATA            TO WS-RELANDCO-NUM              
078800           MOVE MFS-NUM-FAELT-RAETT       TO MOD-RELANDCO-ATTR            
078900        ELSE                                                              
079000           MOVE NEJ                        TO NYCKLAR-SW                  
079100           MOVE MFS-NUM-FAELT-FEL          TO MOD-RELANDCO-ATTR           
079200        END-IF                                                            
079300     ELSE                                                                 
079400        MOVE NEJ                           TO NYCKLAR-SW                  
079500        MOVE MFS-NUM-FAELT-FEL             TO MOD-RELANDCO-ATTR           
079600        MOVE NEJ                           TO RELANDCO-IFYLLD-SW          
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000 BI-KOLLA-PRFRAKT               SECTION.                                  
080100                                                                          
080200*    -- KONTROLL AV PRFRAKT                                               
080300                                                                          
080400     IF MID-PRFRAKT NOT = ALL '+'                                         
080500        MOVE MID-PRFRAKT                  TO DEC-IDFRIDATA                
080600        MOVE 7                            TO DEC-KVHELTAL                 
080700        MOVE 2                            TO DEC-KVDECIMAL                
080800        CALL WDECEDIT USING WDECAREA                                      
080900        IF DEC-KDSVAR-OK                                                  
081000           IF DEC-IDEDITDATA       > ZERO                                 
081100              MOVE MFS-NUM-FAELT-RAETT    TO MOD-PRFRAKT-ATTR             
081200              MOVE DEC-IDEDITDATA         TO WS-PRFRAKT-NUM               
081300           ELSE                                                           
081400              MOVE NEJ                    TO NYCKLAR-SW                   
081500              MOVE MFS-NUM-FAELT-FEL      TO MOD-PRFRAKT-ATTR             
081600           END-IF                                                         
081700        ELSE                                                              
081800           MOVE NEJ                       TO NYCKLAR-SW                   
081900           MOVE MFS-NUM-FAELT-FEL         TO MOD-PRFRAKT-ATTR             
082000        END-IF                                                            
082100     END-IF                                                               
082200     .                                                                    
082300     EJECT                                                                
082400 BJ-KOLLA-PRFOERS               SECTION.                                  
082500                                                                          
082600*    -- KONTROLL AV PRFOERS                                               
082700                                                                          
082800     IF MID-PRFOERS NOT = ALL '+'                                         
082900        MOVE MID-PRFOERS                  TO DEC-IDFRIDATA                
083000        MOVE 7                            TO DEC-KVHELTAL                 
083100        MOVE 2                            TO DEC-KVDECIMAL                
083200        CALL WDECEDIT USING WDECAREA                                      
083300        IF DEC-KDSVAR-OK                                                  
083400           IF DEC-IDEDITDATA       > ZERO                                 
083500              MOVE MFS-NUM-FAELT-RAETT    TO MOD-PRFOERS-ATTR             
083600              MOVE DEC-IDEDITDATA         TO WS-PRFOERS-NUM               
083700           ELSE                                                           
083800              MOVE NEJ                    TO NYCKLAR-SW                   
083900              MOVE MFS-NUM-FAELT-FEL      TO MOD-PRFOERS-ATTR             
084000           END-IF                                                         
084100        ELSE                                                              
084200           MOVE NEJ                       TO NYCKLAR-SW                   
084300           MOVE MFS-NUM-FAELT-FEL         TO MOD-PRFOERS-ATTR             
084400        END-IF                                                            
084500     END-IF                                                               
084600     .                                                                    
084700     EJECT                                                                
084800 BK-KOLLA-PRLEGKST              SECTION.                                  
084900                                                                          
085000*    -- KONTROLL AV PRLEGKST                                              
085100                                                                          
085200     IF MID-PRLEGKST NOT = ALL '+'                                        
085300        MOVE MID-PRLEGKST                 TO DEC-IDFRIDATA                
085400        MOVE 7                            TO DEC-KVHELTAL                 
085500        MOVE 2                            TO DEC-KVDECIMAL                
085600        CALL WDECEDIT USING WDECAREA                                      
085700        IF DEC-KDSVAR-OK                                                  
085800           IF DEC-IDEDITDATA       > ZERO                                 
085900              MOVE MFS-NUM-FAELT-RAETT    TO MOD-PRLEGKST-ATTR            
086000              MOVE DEC-IDEDITDATA         TO WS-PRLEGKST-NUM              
086100           ELSE                                                           
086200              MOVE NEJ                    TO NYCKLAR-SW                   
086300              MOVE MFS-NUM-FAELT-FEL      TO MOD-PRLEGKST-ATTR            
086400           END-IF                                                         
086500        ELSE                                                              
086600           MOVE NEJ                       TO NYCKLAR-SW                   
086700           MOVE MFS-NUM-FAELT-FEL         TO MOD-PRLEGKST-ATTR            
086800        END-IF                                                            
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 F-LAES-VISA-INFO SECTION.                                                
087300                                                                          
087400                                                                          
087500     MOVE INF-UPPDAT-GJORD TO MED-IDMFSINF                                
087600     CALL WMEDKONV USING MED-WMEDAREA                                     
087700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
087800     PERFORM MFS-RENSA-FAELT-UT                                           
087900     .                                                                    
088000     EJECT                                                                
088100 G-FORMELLA-KONTROLLER SECTION.                                           
088200                                                                          
088300     MOVE JA TO INDATA-SW                                                 
088400     PERFORM GA-KOLLA-LEVANM                                              
088500     IF ORDERNR-IFYLLT                                                    
088600     AND FAKTNR-IFYLLT                                                    
088700     AND PRODNR-IFYLLT                                                    
088800         IF INDATA-OK                                                     
088900           PERFORM GC-KOLLA-ORDER-PRODNR-FINNS                            
089000         END-IF                                                           
089100         IF INDATA-OK                                                     
089200           PERFORM GD-KOLLA-FAKTNR-PAA-KOLLI                              
089300         END-IF                                                           
089400     END-IF                                                               
089500                                                                          
089600     IF INDATA-FEL                                                        
089700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
089800     END-IF                                                               
089900     .                                                                    
090000     EJECT                                                                
090100 GA-KOLLA-LEVANM SECTION.                                                 
090200                                                                          
090300     PERFORM IMS-GU-WDA201                                                
090400     IF SEGMENT-FINNS                                                     
090500         MOVE NEJ TO INDATA-SW                                            
090600         MOVE ERR-FINNS-REDAN  TO MED-IDMFSFEL                            
090700         CALL WMEDKONV USING  MED-WMEDAREA                                
090800         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
090900     ELSE                                                                 
091000         IF ORDERNR-IFYLLT                                                
091100         AND FAKTNR-IFYLLT                                                
091200         AND PRODNR-IFYLLT                                                
091300             CONTINUE                                                     
091400         ELSE                                                             
091500             MOVE NEJ TO INDATA-SW                                        
091600             PERFORM MFS-RENSA-FAELT-UT                                   
091700             MOVE ERR-FEL-IFYLLD-RAD TO MED-IDMFSFEL                      
091800             CALL WMEDKONV USING MED-WMEDAREA                             
091900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
092000         END-IF                                                           
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400 GC-KOLLA-ORDER-PRODNR-FINNS SECTION.                                     
092500                                                                          
092600     MOVE JA  TO INDATA-SW                                                
092700     PERFORM IMS-GU-WDE4A                                                 
092800     IF SEGMENT-SAKNAS                                                    
092900         MOVE NEJ TO INDATA-SW                                            
093000         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDORDNR5-ATTR                    
093100         MOVE ERR-ORDER-SAKNAS TO MED-IDMFSFEL                            
093200         CALL WMEDKONV USING  MED-WMEDAREA                                
093300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
093400     END-IF                                                               
093500     .                                                                    
093600     EJECT                                                                
093700 GD-KOLLA-FAKTNR-PAA-KOLLI SECTION.                                       
093800                                                                          
093900     PERFORM IMS-GU-WDE601                                                
094000                                                                          
094100     IF KOLLA-ALLA-KOLLI                                                  
094200         PERFORM GDA-KOLLA-ALLA-KOLLI                                     
094300     ELSE                                                                 
094400         PERFORM GDB-KOLLA-UTVALDA-KOLLI                                  
094500     END-IF                                                               
094600                                                                          
094700     CALL WMEDKONV USING  MED-WMEDAREA                                    
094800     MOVE MED-MFSFEL       TO MOD-TEMFSFEL                                
094900                                                                          
095000     .                                                                    
095100     EJECT                                                                
095200 GDA-KOLLA-ALLA-KOLLI SECTION.                                            
095300                                                                          
095400     PERFORM IMS-GNP-WDE611                                               
095500     IF SEGMENT-FINNS                                                     
095600         PERFORM UNTIL SEGMENT-SAKNAS                                     
095700         OR INDATA-FEL                                                    
095800             IF MID-IDFAKT NUMERIC                                        
095900                 MOVE MID-IDFAKT TO WS-IDFAKT                             
096000                 MOVE KOLLI-IDFAKT TO TEST-KOLLI-IDFAKT                   
096100                 IF WS-IDFAKT NOT = KOLLI-IDFAKT                          
096200                     MOVE NEJ TO INDATA-SW                                
096300                     MOVE MFS-ALFA-FAELT-FEL                              
096400                          TO MOD-IDFAKT-ATTR                              
096500                     MOVE ERR-FAKTURA-SAKNAS TO MED-IDMFSFEL              
096600                 END-IF                                                   
096700             END-IF                                                       
096800             PERFORM IMS-GNP-WDE611                                       
096900         END-PERFORM                                                      
097000     ELSE                                                                 
097100         MOVE NEJ TO INDATA-SW                                            
097200         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFAKT-ATTR                       
097300         MOVE ERR-KOLLI-SAKNAS TO MED-IDMFSFEL                            
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700 GDB-KOLLA-UTVALDA-KOLLI SECTION.                                         
097800                                                                          
097900     MOVE +1 TO RAD-IX                                                    
098000     PERFORM UNTIL RAD-IX > MAX-IX                                        
098100         IF MID-IDKOLLI-FOM (RAD-IX) NOT = SPACE                          
098200             IF MID-IDKOLLI-TOM (RAD-IX) NOT = SPACE                      
098300                 MOVE MID-IDKOLLI-FOM (RAD-IX) TO WS-IDKOLLI-MIN          
098400                 MOVE MID-IDKOLLI-TOM (RAD-IX) TO WS-IDKOLLI-MAX          
098500             ELSE                                                         
098600                 MOVE MID-IDKOLLI-FOM (RAD-IX) TO WS-IDKOLLI-MIN          
098700                                                  WS-IDKOLLI-MAX          
098800             END-IF                                                       
098900             PERFORM UNTIL WS-IDKOLLI-MIN > WS-IDKOLLI-MAX                
099000             OR INDATA-FEL                                                
099100                 MOVE WS-IDKOLLI-MIN TO WS-IDKOLLI                        
099200                                        W-IDKOLLI                         
099300                 PERFORM IMS-GNP-WDE611-KVAL                              
099400                 IF SEGMENT-FINNS                                         
099500                     IF MID-IDKOLLI-FOM (RAD-IX) NUMERIC                  
099600                         MOVE MID-IDFAKT TO WS-IDFAKT                     
099700                         IF KOLLI-IDFAKT NOT = WS-IDFAKT                  
099800                             MOVE NEJ TO INDATA-SW                        
099900                             MOVE MFS-ALFA-FAELT-FEL TO                   
100000                                  MOD-IDFAKT-ATTR                         
100100                                  MOD-IDKOLLI-FOM-ATTR (RAD-IX)           
100200                                  MOD-IDKOLLI-TOM-ATTR (RAD-IX)           
100300                             MOVE ERR-FAKTURA-SAKNAS                      
100400                                                 TO MED-IDMFSFEL          
100500                         END-IF                                           
100600                     END-IF                                               
100700                     ADD +1 TO WS-IDKOLLI-MIN                             
100800                 ELSE                                                     
100900                     MOVE NEJ TO INDATA-SW                                
101000                             MOVE MFS-ALFA-FAELT-FEL TO                   
101100                                  MOD-IDKOLLI-FOM-ATTR (RAD-IX)           
101200                                  MOD-IDKOLLI-TOM-ATTR (RAD-IX)           
101300                     MOVE ERR-KOLLI-SAKNAS TO MED-IDMFSFEL                
101400                 END-IF                                                   
101500             END-PERFORM                                                  
101600             ADD +1 TO RAD-IX                                             
101700         ELSE                                                             
101800             MOVE +99 TO RAD-IX                                           
101900         END-IF                                                           
102000     END-PERFORM                                                          
102100     .                                                                    
102200     EJECT                                                                
102300 H-SKAPA-MID-SKICKA-TRANS SECTION.                                        
102400                                                                          
102500     PERFORM HA-REDIGERA-4796-MID                                         
102600     PERFORM HB-SKICKA-TRANS                                              
102700     .                                                                    
102800     EJECT                                                                
102900 HA-REDIGERA-4796-MID SECTION.                                            
103000                                                                          
103100     MOVE WS-IDDISTR            TO 4796-MID-IDDISTR                       
103200     MOVE WS-IDKUNDNR           TO 4796-MID-IDKUNDNR                      
103300     MOVE WS-IDRAPPNR           TO 4796-MID-IDRAPPNR                      
103400                                                                          
103500     MOVE MID-IDORDNR5          TO 4796-MID-IDORDNR5                      
103600     MOVE MID-IDFAKT            TO 4796-MID-IDFAKT                        
103700     MOVE MID-IDPRODNR          TO 4796-MID-IDPRODNR                      
103800                                                                          
103900     IF MID-RELANDCO NOT = ALL '+'                                        
104000         MOVE WS-RELANDCO       TO 4796-MID-RELANDCO                      
104100     ELSE                                                                 
104200         MOVE ALL ZERO          TO 4796-MID-RELANDCO                      
104300     END-IF                                                               
104400                                                                          
104500     IF MID-PRFRAKT NOT = ALL '+'                                         
104600         MOVE WS-PRFRAKT        TO 4796-MID-PRFRAKT                       
104700     ELSE                                                                 
104800         MOVE ALL ZERO          TO 4796-MID-PRFRAKT                       
104900     END-IF                                                               
105000                                                                          
105100     IF MID-PRFOERS NOT = ALL '+'                                         
105200         MOVE WS-PRFOERS        TO 4796-MID-PRFOERS                       
105300     ELSE                                                                 
105400         MOVE ALL ZERO          TO 4796-MID-PRFOERS                       
105500     END-IF                                                               
105600                                                                          
105700     IF MID-PRLEGKST NOT = ALL '+'                                        
105800         MOVE WS-PRLEGKST       TO 4796-MID-PRLEGKST                      
105900     ELSE                                                                 
106000         MOVE ALL ZERO          TO 4796-MID-PRLEGKST                      
106100     END-IF                                                               
106200                                                                          
106300     MOVE +1 TO RAD-IX                                                    
106400     PERFORM UNTIL RAD-IX > MAX-IX                                        
106500         MOVE MID-IDKOLLI-FOM (RAD-IX)                                    
106600                                TO 4796-MID-IDKOLLI-FOM (RAD-IX)          
106700         MOVE MID-IDKOLLI-TOM (RAD-IX)                                    
106800                                TO 4796-MID-IDKOLLI-TOM (RAD-IX)          
106900         ADD +1 TO RAD-IX                                                 
107000     END-PERFORM                                                          
107100                                                                          
107200     MOVE SPACE                 TO 4796-MID-IDPLKLST                      
107300                                   4796-MID-IDPURAD                       
107400                                   4796-MID-IDKOLLI                       
107500                                   4796-MID-RAD-IX                        
107600                                                                          
107700                                                                          
107800                                                                          
107900     .                                                                    
108000     EJECT                                                                
108100 HB-SKICKA-TRANS SECTION.                                                 
108200                                                                          
108300     MOVE MFS-KDMFSFOR          TO P-TO-P-KDMFSFOR                        
108400     COMPUTE P-TO-P-KVLL = LENGTH OF 4796-MID + 17                        
108500     PERFORM IMS-ISRT-4796-MSG-4796                                       
108600     .                                                                    
108700     EJECT                                                                
108800 MFS-RENSA-FAELT-UT SECTION.                                              
108900                                                                          
109000*    --- ALLA UTDATA-FÄLT FÖRUTOM NYCKLAR                                 
109100                                                                          
109200     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR5                                 
109300                             MOD-IDFAKT                                   
109400                             MOD-IDPRODNR                                 
109500                             MOD-RELANDCO                                 
109600                             MOD-PRFRAKT                                  
109700                             MOD-PRFOERS                                  
109800                             MOD-PRLEGKST                                 
109900     MOVE +1 TO RAD-IX                                                    
110000     PERFORM UNTIL RAD-IX > MAX-IX                                        
110100         MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-FOM (RAD-IX)                 
110200                                 MOD-IDKOLLI-TOM (RAD-IX)                 
110300         ADD +1 TO RAD-IX                                                 
110400     END-PERFORM                                                          
110500                                                                          
110600     .                                                                    
110700     EJECT                                                                
110800 MFS-RENSA-SAMTLIGA-FAELT-UT SECTION.                                     
110900                                                                          
111000*    --- ALLA UTDATA-FÄLT                                                 
111100     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                               
111200                             MOD-IDKUNDNR-UT                              
111300                             MOD-IDRAPPNR-UT                              
111400                             MOD-RELANDCO                                 
111500                             MOD-PRFRAKT                                  
111600                             MOD-PRFOERS                                  
111700                             MOD-PRLEGKST                                 
111800                             MOD-IDORDNR5                                 
111900                             MOD-IDFAKT                                   
112000                             MOD-IDPRODNR                                 
112100     MOVE +1 TO RAD-IX                                                    
112200     PERFORM UNTIL RAD-IX > MAX-IX                                        
112300         MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-FOM (RAD-IX)                 
112400                                 MOD-IDKOLLI-TOM (RAD-IX)                 
112500         ADD +1 TO RAD-IX                                                 
112600     END-PERFORM                                                          
112700                                                                          
112800     .                                                                    
112900                                                                          
113000 MFS-RENSA-FAELT-IN SECTION.                                              
113100                                                                          
113200     MOVE MFS-RENSA-FAELT TO MOD-RELANDCO                                 
113300                             MOD-PRFRAKT                                  
113400                             MOD-PRFOERS                                  
113500                             MOD-PRLEGKST                                 
113600                             MOD-IDORDNR5                                 
113700                             MOD-IDFAKT                                   
113800                             MOD-IDPRODNR                                 
113900     MOVE +1 TO RAD-IX                                                    
114000     PERFORM UNTIL RAD-IX > MAX-IX                                        
114100         MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-FOM (RAD-IX)                 
114200                                 MOD-IDKOLLI-TOM (RAD-IX)                 
114300         ADD +1 TO RAD-IX                                                 
114400     END-PERFORM                                                          
114500     .                                                                    
114600     EJECT                                                                
114700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
114800                                                                          
114900*    --- ALLA UTDATA-FÄLT                                                 
115000     MOVE MFS-ROER-EJ-FAELT TO  MOD-RELANDCO                              
115100                                MOD-PRFRAKT                               
115200                                MOD-PRFOERS                               
115300                                MOD-PRLEGKST                              
115400                                MOD-IDORDNR5                              
115500                                MOD-IDFAKT                                
115600                                MOD-IDPRODNR                              
115700     MOVE +1 TO RAD-IX                                                    
115800     PERFORM UNTIL RAD-IX > MAX-IX                                        
115900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-FOM (RAD-IX)                 
116000                                 MOD-IDKOLLI-TOM (RAD-IX)                 
116100       ADD +1 TO RAD-IX                                                   
116200     END-PERFORM                                                          
116300     .                                                                    
116400                                                                          
116500                                                                          
116600* --- IMS SEKTIONER ---                                                   
116700                                                                          
116800 IMS-GET-MSG SECTION.                                                     
116900                                                                          
117000     MOVE '  QC' TO GODK-STATUSKODER                                      
117100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
117200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
117300     PERFORM IMS-STATUSKONTROLL                                           
117400     .                                                                    
117500                                                                          
117600 IMS-INSERT-MSG SECTION.                                                  
117700                                                                          
117800     IF ENGLISH-TEXT                                                      
117900       MOVE 'N' TO MFS-KDHUVOMR                                           
118000     END-IF                                                               
118100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
118200     MOVE SPACE TO GODK-STATUSKODER                                       
118300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
118400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
118500     PERFORM IMS-STATUSKONTROLL                                           
118600     .                                                                    
118700     EJECT                                                                
118800 IMS-ISRT-4796-MSG-4796 SECTION.                                          
118900                                                                          
119000     MOVE LOW-VALUE TO P-TO-P-KDZ1 P-TO-P-KDZ2                            
119100     MOVE SPACE TO GODK-STATUSKODER                                       
119200     CALL CBLTDLI USING ISRT 4796-PCB P-TO-P-SW                           
119300     MOVE 4796-STATUS-CODE TO STATUS-WS                                   
119400     PERFORM IMS-STATUSKONTROLL                                           
119500     .                                                                    
119600     EJECT                                                                
119700 IMS-GU-WDA201 SECTION.                                                   
119800                                                                          
119900     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
120000          DELIMITED BY SIZE INTO SSA1                                     
120100     MOVE '  GE' TO GODK-STATUSKODER                                      
120200     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-A201 SSA1                      
120300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
120400     PERFORM IMS-STATUSKONTROLL                                           
120500     .                                                                    
120600     EJECT                                                                
120700 IMS-GU-WDE4A SECTION.                                                    
120800                                                                          
120900     STRING 'WDE4A1  (WDE4A1KY>=' W-WDE4A1KY-MIN-X                        
121000                    '&WDE4A1KY<=' W-WDE4A1KY-MAX-X ')'                    
121100          DELIMITED BY SIZE INTO SSA1                                     
121200     MOVE '  GE' TO GODK-STATUSKODER                                      
121300     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-E4A1 SSA1                     
121400     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
121500     PERFORM IMS-STATUSKONTROLL                                           
121600     .                                                                    
121700     EJECT                                                                
121800 IMS-GU-WDE601 SECTION.                                                   
121900                                                                          
122000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
122100          DELIMITED BY SIZE INTO SSA1                                     
122200     MOVE '    ' TO GODK-STATUSKODER                                      
122300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
122400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
122500     PERFORM IMS-STATUSKONTROLL                                           
122600     .                                                                    
122700     EJECT                                                                
122800 IMS-GNP-WDE611 SECTION.                                                  
122900                                                                          
123000     MOVE 'WDE611' TO SSA1                                                
123100     MOVE '  GE' TO GODK-STATUSKODER                                      
123200     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
123300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
123400     PERFORM IMS-STATUSKONTROLL                                           
123500     .                                                                    
123600     EJECT                                                                
123700 IMS-GNP-WDE611-KVAL SECTION.                                             
123800                                                                          
123900     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
124000          DELIMITED BY SIZE INTO SSA1                                     
124100     MOVE '  GE' TO GODK-STATUSKODER                                      
124200     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
124300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
124400     PERFORM IMS-STATUSKONTROLL                                           
124500     .                                                                    
124600     EJECT                                                                
124700 IMS-STATUSKONTROLL SECTION.                                              
124800                                                                          
124900     SET STATUS-IX TO 1                                                   
125000     SEARCH GODK-STATUS                                                   
125100       AT END                                                             
125200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
125300         DELIMITED BY SIZE INTO FELTEXT                                   
125400         CALL FELLOG                                                      
125500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
125600         CONTINUE                                                         
125700     END-SEARCH                                                           
125800     .                                                                    
