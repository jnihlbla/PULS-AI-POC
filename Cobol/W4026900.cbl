000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4026900.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   91/04/24.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*     PROGRAMMET ÄR ETT BAKGRUNDS-MPP SOM MED PROFORMA-REGISTER           
001100*     (WLPROC/WLPROD) SOM INDATA SKAPAR BUNTHUVUD OCH TRANSAKTIN          
001200*     (WLKOMA) VILKA SEDAN BEARBETAS VIDARE OCH SLUTLIGEN HAMNAR          
001300*     PÅ ORDERKÖN.                                                        
001400*     UPPLÄGGNING AV BUNTHUVUD OCH TRANSAKTIONER GÖRS MED HJÄLP           
001500*     PROGRAM W006KOM.                                                    
001600*                                                                         
001700*        PROGRAMMET LÄSER     WLPROC (WDE8)                               
001800*        PROGRAMMET UPPDAT    WLPROC (WDE8)                               
001900*        PROGRAMMET LÄSER     WLPROD (WDE9)                               
002000*        PROGRAMMET LÄSER     WLARTM (WDK9)                               
002100*        PROGRAMMET UPPDAT    WLARTM (WDK9)                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T269X                                             
002500*        MID:         TRANS FRÅN W4026600                                 
002600*                     TRANS FRÅN W4802100                                 
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O26602                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600*    -- CHECKED BY WY2000                                                 
003700     SKIP3                                                                
003800 77  IDPGM                       PIC X(08)   VALUE 'W4026900'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  RAD-IX                      PIC S9(3)  VALUE +0    COMP SYNC.        
004700 77  MAX-RAD-IX                  PIC S9(3)  VALUE +5    COMP SYNC.        
004800 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
004900 77  MAX-INDX                    PIC S9(3)  VALUE +9    COMP SYNC.        
005000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +33   COMP SYNC.        
005200                                                                          
005300 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
005400 77  DATUM-AR-MAN-DAG            PIC 9(6)   VALUE ZERO.                   
005500 77  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
005600 77  WS-KDMFSFOR                 PIC X      VALUE SPACE.                  
005700 77  MFS-IDMOD                   PIC X(8)   VALUE 'W4O26602'.             
005800                                                                          
005900*    --- ARBETSFÄLT FÖR FLYTTNINGAR FRÅN ALFA TILL NUM FÄLT               
006000 77  WS-IDDISTR-NUM              PIC 9(4)   VALUE ZERO.                   
006100 77  WS-IDKUNDNR-NUM             PIC 9(6)   VALUE ZERO.                   
006200 77  WS-IDORDNR7-NUM             PIC 9(7)   VALUE ZERO.                   
006300 77  WS-KDORDKL-NUM              PIC 9(1)   VALUE ZERO.                   
006400 77  WS-KDFRAKT-NUM              PIC 9(2)   VALUE ZERO.                   
006500 77  WS-REOMRTAL-NUM             PIC 9(1).9(3) VALUE ZERO.                
006600 77  WS-IDKONTO-NUM              PIC 9(10)  VALUE ZERO.                   
006700 77  WS-IDKST                    PIC X(10)  VALUE SPACE.                  
006800                                                                          
006900 77  WS-IDARTNR-NUM              PIC 9(9)   VALUE ZERO.                   
007000 77  WS-REKSIFFR-NUM             PIC 9(1)   VALUE ZERO.                   
007100 77  WS-KVBEART-NUM              PIC 9(6)   VALUE ZERO.                   
007200 77  WS-PRARTNTO-NUM             PIC 9(7).9(2) VALUE ZERO.                
007300 77  WS-PRARTNTO-NUM-LOC         PIC 9(7).9(2) VALUE ZERO.                
007400 77  WS-PRARTBTO-NUM-LOC         PIC 9(7).9(2) VALUE ZERO.                
007500 77  WS-KDKVBRYT-NUM             PIC 9(1)   VALUE ZERO.                   
007600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007700                                                                          
007800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007900     88  ALLT-OK                             VALUE 'J'.                   
008000                                                                          
008100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008200     88  EGEN-MID                            VALUE '4269'.                
008300     88  GODK-MID                            VALUE '4266' '4269'.         
008400     EJECT                                                                
008500                                                                          
008600 01  DATUM-AR-DAGNR.                                                      
008700    03 DATUM-AR                  PIC 9(2)   VALUE ZERO.                   
008800    03 DATUM-DAGNR               PIC 9(3)   VALUE ZERO.                   
008900                                                                          
009000 01  DATUM-AR-DAGNR-RAD.                                                  
009100    03 DATUM-AR-RAD              PIC 9(2)   VALUE ZERO.                   
009200    03 DATUM-DAGNR-RAD           PIC 9(3)   VALUE ZERO.                   
009300 01  DATUM-AR-DAGNR-RAD-NUM REDEFINES DATUM-AR-DAGNR-RAD                  
009400                                 PIC 9(5).                                
009500                                                                          
009600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009700 01  GENERELLA-SUBPROGRAM.                                                
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
010300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010400     EJECT                                                                
010500                                                                          
010600 01  FILLER              PIC X(16)  VALUE 'WDATKONV-IO-AREA'.             
010700*01 -COPY WDATAREA                                                        
010800     EJECT                                                                
010900 01  FILLER              PIC X(16)  VALUE 'WMEDKONV-IO-AREA'.             
011000*   -COPY WMEDAREA                                                        
011100                                                                          
011200 01  MESSAGE-CODES.                                                       
011300     03 INF-RELEASE-OK   PIC X(3)   VALUE '096'.                          
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
011600 01  MID-IO-AREA.                                                         
011700     03  MID-AREA.                                                        
011800        05  MID-IDDISTR          PIC X(4).                                
011900        05  MID-IDKUNDNR         PIC X(6).                                
012000        05  MID-IDKUNDRF         PIC X(10).                               
012100        05  MID-IDPRT            PIC X(3).                                
012200     EJECT                                                                
012300 01  WS-IDKUNDRF-X.                                                       
012400     03  WS-IDKUNDRF             PIC  X(10) VALUE SPACE.                  
012500     03  WS-IDKUNDRF-FILLER REDEFINES WS-IDKUNDRF.                        
012600         05  WS-IDORDNR7         PIC  9(7).                               
012700         05  FILLER              PIC  X(3).                               
012800                                                                          
012900                                                                          
013000 01  FILLER              PIC X(16)  VALUE 'MSG-IO-AREA'.                  
013100*01 -COPY WMSGAREA                                                        
013200                                                                          
013300 01  FILLER              PIC X(16)  VALUE 'DHUV-MID-AREA'.                
013400*01  -COPY W4I25101     -PRE DHUV-                                        
013500     EJECT                                                                
013600                                                                          
013700 01  FILLER              PIC X(16)  VALUE 'DRAD-MID-AREA'.                
013800*01  -COPY W4I25201     -PRE DRAD-                                        
013900     EJECT                                                                
014000                                                                          
014100     EJECT                                                                
014200 01  TEST-IDDISTR                PIC  S9(5)   COMP-3.                     
014300*    ----DISTR-DEALER-PRICE-----                                          
014400*01  FILLER  -COPY WWDIST79     -RED TEST-IDDISTR.                        
014500*                                                                         
014600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014700*                                                                         
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015000     SKIP3                                                                
015100 01  NYCKLAR-TILL-DLI.                                                    
015200     03  W-IDGMTREF-X.                                                    
015300         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
015400         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
015500         05  W-IDKUNDRF          PIC  X(10)   VALUE SPACE.                
015600         05  W-IDORDNR7-FILLER REDEFINES W-IDKUNDRF.                      
015700            07  W-IDORDNR7       PIC  9(7).                               
015800            07  FILLER           PIC  X(3).                               
015900                                                                          
016000     03  W-WDE901KY-MIN-X.                                                
016100         05  W-IDORDER-MIN       PIC S9(7)    VALUE ZERO COMP-3.          
016200         05  W-IDARTNR-MIN       PIC S9(9)    VALUE ZERO COMP-3.          
016300         05  W-IDLOPNR-MIN       PIC S9(3)    VALUE ZERO COMP-3.          
016400                                                                          
016500     03  W-WDE901KY-MAX-X.                                                
016600         05  W-IDORDER-MAX       PIC S9(7)    VALUE ZERO COMP-3.          
016700         05  W-IDARTNR-MAX       PIC S9(9)    VALUE ZERO COMP-3.          
016800         05  W-IDLOPNR-MAX       PIC S9(3)    VALUE ZERO COMP-3.          
016900                                                                          
017000     03  W-IDARTNR-X.                                                     
017100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017200                                                                          
017300     SKIP2                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500                                                                          
017600 01  STATUS-WS                   PIC XX.                                  
017700     88  SEGMENT-FINNS                       VALUE '  '.                  
017800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018000     88  BASEN-SLUT                          VALUE 'GB'.                  
018100     SKIP2                                                                
018200                                                                          
018300 01  GODK-STATUSKODER.                                                    
018400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018500     SKIP3                                                                
018600                                                                          
018700 01  SSA1                        PIC X(64).                               
018800 01  SSA2                        PIC X(64).                               
018900     EJECT                                                                
019000                                                                          
019100*    --- IMS FUNKTIONSKODER                                               
019200*01  -COPY W0003                                                          
019300     EJECT                                                                
019400                                                                          
019500*    ---  DLI INPUT-OUTPUT AREA                                           
019600 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLPROC01'.        
019700 01  DLI-IO-WLPROC01.                                                     
019800*    03  -COPY WDE801                                                     
019900     SKIP3                                                                
020000 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLPROD01'.        
020100 01  DLI-IO-WLPROD01.                                                     
020200*    03  -COPY WDE901                                                     
020300     SKIP3                                                                
020400 01  FILLER                     PIC X(16) VALUE 'ARTM-IO-AREA'.           
020500 01  ARTM-IO-AREA.                                                        
020600*    03  -COPY WDK901                                                     
020700     EJECT                                                                
020800                                                                          
020900 01  FILLER                     PIC X(16) VALUE 'MSG-KOM-AREA'.           
021000*01  -COPY WMSGKOM                                                        
021100     EJECT                                                                
021200                                                                          
021300 01  FILLER                      PIC X(16) VALUE '4266-IO-AREA'.          
021400                                                                          
021500 01  4266-IO-AREA.                                                        
021600     03  4266-LL                 PIC S9(4) VALUE +109 COMP SYNC.          
021700     03  4266-Z1                 PIC X.                                   
021800     03  4266-Z2                 PIC X.                                   
021900*03  -COPY W4O26901                                                       
022000                                                                          
022100     EJECT                                                                
022200 01  FILLER                      PIC X(16) VALUE '4295-IO-AREA'.          
022300                                                                          
022400 01  4295-MSG-IO-AREA.                                                    
022500     03  4295-LL                 PIC S9(4) VALUE +92 COMP SYNC.           
022600     03  4295-Z1                 PIC X.                                   
022700     03  4295-Z2                 PIC X.                                   
022800     03  4295-TRANSKOD           PIC X(8)  VALUE 'W4T295X '.              
022900     03  4295-IDTRANS            PIC X(4)  VALUE '4269'.                  
023000     03  4295-SPRAK              PIC X.                                   
023100*03  -COPY W4I29501      -PRE 4295-                                       
023200                                                                          
023300     EJECT                                                                
023400                                                                          
023500 LINKAGE SECTION.                                                         
023600                                                                          
023700*01  -COPY W0009      -PRE MSG-                                           
023800     EJECT                                                                
023900*01  -COPY W0009      -PRE 0693-                                          
024000     EJECT                                                                
024100*01  -COPY W0009      -PRE 4295-                                          
024200     EJECT                                                                
024300*01  -COPY W0008      -PRE PROC-                                          
024400     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600*01  -COPY W0008      -PRE PROD-                                          
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008      -PRE ARTM-                                          
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200*01  -COPY W0008      -PRE KOMA-                                          
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500 PROCEDURE DIVISION  USING MSG-PCB 0693-PCB 4295-PCB PROC-PCB             
025600                                   PROD-PCB ARTM-PCB KOMA-PCB.            
025700                                                                          
025800     ENTRY 'DLITCBL' USING MSG-PCB 0693-PCB 4295-PCB PROC-PCB             
025900                                   PROD-PCB ARTM-PCB KOMA-PCB.            
026000                                                                          
026100     PERFORM IMS-GET-MSG                                                  
026200                                                                          
026300     IF SEGMENT-FINNS                                                     
026400       PERFORM A-INIT                                                     
026500       PERFORM B-INITIERA-NYCKLAR                                         
026600       PERFORM F-KOLLA-OM-PHUV-FINNS                                      
026700       PERFORM G-INITIERA-4295-TRANSEN                                    
026800       PERFORM H-SKAPA-TRANSAR                                            
026900                                                                          
027000       IF W-IDTRANS = '4266'                                              
027100         CONTINUE                                                         
027200       ELSE                                                               
027300         PERFORM I-STARTA-4295                                            
027400       END-IF                                                             
027500                                                                          
027600       PERFORM J-SKICKA-SVAR-TILL-4266                                    
027700       PERFORM K-UPPDATERA-PROFHUVUD                                      
027800     END-IF                                                               
027900                                                                          
028000     MOVE ZERO TO RETURN-CODE                                             
028100*    CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
028200     GOBACK                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 A-INIT SECTION.                                                          
028600                                                                          
028700     IF MSG-DUBBLA-TRANSKODER                                             
028800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-AREA                     
028900       MOVE MSG-KDMFSFOR-2 TO WS-KDMFSFOR                                 
029000       MOVE MSG-IDTRANS-2  TO W-IDTRANS                                   
029100     ELSE                                                                 
029200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-AREA                      
029300       MOVE MSG-KDMFSFOR-1 TO WS-KDMFSFOR                                 
029400       MOVE MSG-IDTRANS-1  TO W-IDTRANS                                   
029500     END-IF                                                               
029600                                                                          
029700     MOVE SPACE                      TO DHUV-MID-W4I25101                 
029800                                                                          
029900     ACCEPT DAGENS-DATUM             FROM DATE                            
030000     ACCEPT DAGENS-TID               FROM TIME                            
030100                                                                          
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500 B-INITIERA-NYCKLAR SECTION.                                              
030600                                                                          
030700     MOVE LOW-VALUE                  TO W-IDGMTREF-X                      
030800                                        W-IDARTNR-X                       
030900                                        W-WDE901KY-MIN-X                  
031000     MOVE HIGH-VALUE                 TO W-WDE901KY-MAX-X                  
031100                                                                          
031200     MOVE MID-IDDISTR                TO WS-IDDISTR-NUM                    
031300     MOVE WS-IDDISTR-NUM             TO W-IDDISTR                         
031400                                        TEST-IDDISTR                      
031500     MOVE MID-IDKUNDNR               TO WS-IDKUNDNR-NUM                   
031600     MOVE WS-IDKUNDNR-NUM            TO W-IDKUNDNR                        
031700     MOVE SPACE                      TO W-IDKUNDRF                        
031800     MOVE MID-IDKUNDRF               TO WS-IDKUNDRF                       
031900     MOVE WS-IDORDNR7                TO W-IDORDNR7                        
032000     .                                                                    
032100     EJECT                                                                
032200                                                                          
032300 F-KOLLA-OM-PHUV-FINNS SECTION.                                           
032400                                                                          
032500     PERFORM FA-LAES-GRUNDDATA                                            
032600                                                                          
032700     MOVE PHUV-IDORDER               TO W-IDORDER-MIN                     
032800                                        W-IDORDER-MAX                     
032900                                                                          
033000     PERFORM FB-SKAPA-BUNTHUVUD                                           
033100                                                                          
033200     .                                                                    
033300     EJECT                                                                
033400 FA-LAES-GRUNDDATA SECTION.                                               
033500                                                                          
033600     PERFORM IMS-GET-PHUV-WDE8                                            
033700                                                                          
033800     IF SEGMENT-FINNS                                                     
033900        CONTINUE                                                          
034000     ELSE                                                                 
034100*       PROFORMAHUVUD SAKNAS SKALL EJ KUNNA SKE                           
034200        MOVE 'PROFORMAHUVUD SAKNAS'  TO FELTEXT                           
034300        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
034400     END-IF                                                               
034500                                                                          
034600     .                                                                    
034700     EJECT                                                                
034800                                                                          
034900 FB-SKAPA-BUNTHUVUD SECTION.                                              
035000                                                                          
035100     MOVE +54                        TO MSG-KOM-KVLL                      
035200     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
035300     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
035400     MOVE SPACE                      TO MSG-KOM-KDTRANS                   
035500     MOVE 'W4I25101'                 TO MSG-KOM-IDCPYTXT                  
035600     MOVE 'PROFORMA'                 TO MSG-KOM-IDSNDNOD                  
035700     MOVE 'W4026900'                 TO MSG-KOM-IDSNDJOB                  
035800     MOVE DAGENS-DATUM               TO MSG-KOM-TIREGDAT                  
035900     MOVE DAGENS-TID                 TO MSG-KOM-TIKLOCK                   
036000     MOVE SPACE                      TO MSG-KOM-IDMFSMED                  
036100                                        MSG-KOM-KDSVAR                    
036200                                                                          
036300     .                                                                    
036400     EJECT                                                                
036500                                                                          
036600 G-INITIERA-4295-TRANSEN SECTION.                                         
036700                                                                          
036800     MOVE LOW-VALUE                  TO 4295-Z1                           
036900     MOVE LOW-VALUE                  TO 4295-Z2                           
037000     MOVE WS-KDMFSFOR                TO 4295-SPRAK                        
037100     MOVE PHUV-IDDISTR               TO 4295-MID-IDDISTR                  
037200     MOVE PHUV-IDKUNDNR              TO 4295-MID-IDKUNDNR                 
037300     MOVE PHUV-IDKUNDRF              TO 4295-MID-IDKUNDRF                 
037400     MOVE ZERO                       TO 4295-MID-IDSID                    
037500     MOVE 'PROF'                     TO 4295-MID-IDSYSTEM                 
037600     MOVE MID-IDPRT                  TO 4295-MID-IDPRT                    
037700     MOVE ZERO                       TO 4295-MID-SUORDV                   
037800     MOVE DAGENS-DATUM               TO 4295-MID-TIUPPDAT                 
037900     MOVE DAGENS-TID                 TO 4295-MID-TIUPPTID                 
038000     MOVE LOW-VALUE                  TO 4295-MID-NYCKEL-GRP               
038100                                                                          
038200     .                                                                    
038300     EJECT                                                                
038400                                                                          
038500 H-SKAPA-TRANSAR SECTION.                                                 
038600                                                                          
038700     PERFORM HA-SKAPA-SKICKA-PHUV-TRANS                                   
038800                                                                          
038900     PERFORM HB-SKAPA-SKICKA-PRAD-TRANS                                   
039000                                                                          
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 HA-SKAPA-SKICKA-PHUV-TRANS  SECTION.                                     
039500                                                                          
039600     PERFORM HAA-INITIERA-PHUV-TRANS                                      
039700     MOVE DAGENS-DATUM       TO DATUM-AR-MAN-DAG                          
039800                                DAT-I-TIDATUM                             
039900     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
040000                                                                          
040100     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
040200                         DAT-O-TIDATUM, DAT-KDSVAR                        
040300     IF DAT-KDSVAR-OK                                                     
040400        MOVE DAT-TIAADDD        TO DATUM-AR-DAGNR                         
040500     ELSE                                                                 
040600*       DATUMKONVERTERING HAR GÅTT FEL                                    
040700        MOVE                                                              
040800        'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL'                    
040900                                     TO FELTEXT                           
041000        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
041100     END-IF                                                               
041200     MOVE DHUV-MID-W4I25101  TO MSG-INDATA-MINUS-1-TRANSKOD               
041300                                                                          
041400     CALL W006KOM USING MSG-PCB                                           
041500                        0693-PCB                                          
041600                        KOMA-PCB                                          
041700                        MSG-KOM-WMSGKOM                                   
041800                        MSG-IO-AREA                                       
041900                                                                          
042000     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
042100*       FELAKTIG UPPDATERING AV PROFORMAHUVUDET PÅ                        
042200*       KOMMUNIKATIONS DB                                                 
042300        MOVE                                                              
042400        'FELAKTIG UPPDATERING AV PHUV PÅ KOMMUNIKATIONS DB'               
042500                                     TO FELTEXT                           
042600        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000                                                                          
043100                                                                          
043200 HAA-INITIERA-PHUV-TRANS SECTION.                                         
043300                                                                          
043400     COMPUTE MSG-KVLL = LENGTH OF DHUV-MID-W4I25101 + 17                  
043500     MOVE LOW-VALUE                  TO MSG-KDZ1                          
043600     MOVE LOW-VALUE                  TO MSG-KDZ2                          
043700     MOVE SPACE                      TO MSG-AREA                          
043800     MOVE 'W4T251X'                  TO MSG-KDTRANS-1                     
043900     MOVE '4251'                     TO MSG-IDTRANS-1                     
044000     MOVE WS-KDMFSFOR                TO MSG-KDMFSFOR-1                    
044100     MOVE 'OREL'                     TO DHUV-MID-IDSYSTEM                 
044200     MOVE PHUV-IDDISTR               TO WS-IDDISTR-NUM                    
044300     MOVE WS-IDDISTR-NUM             TO DHUV-MID-IDDISTR                  
044400     MOVE PHUV-IDKUNDNR              TO WS-IDKUNDNR-NUM                   
044500     MOVE WS-IDKUNDNR-NUM            TO DHUV-MID-IDKUNDNR                 
044600     MOVE PHUV-IDORDNR7              TO WS-IDORDNR7-NUM                   
044700     MOVE WS-IDORDNR7-NUM            TO DHUV-MID-IDORDNR                  
044800     MOVE PHUV-KDORDKL               TO WS-KDORDKL-NUM                    
044900     MOVE WS-KDORDKL-NUM             TO DHUV-MID-KDORDKL                  
045000     MOVE PHUV-KDFRAKT               TO WS-KDFRAKT-NUM                    
045100     MOVE WS-KDFRAKT-NUM             TO DHUV-MID-KDFRAKT                  
045200     MOVE SPACE                      TO DHUV-MID-TIRFS                    
045300     MOVE PHUV-BEKUNDRF              TO DHUV-MID-BEKUNDRF                 
045400     MOVE PHUV-KDFAKTYP              TO DHUV-MID-KDFAKTYP                 
045500     MOVE PHUV-FLRESTN               TO DHUV-MID-FLRESTN                  
045600     MOVE SPACE                      TO DHUV-MID-KDTPOTYP                 
045700     MOVE SPACE                      TO DHUV-MID-TITPO                    
045800     MOVE SPACE                      TO DHUV-MID-BELAGINS                 
045900     MOVE PHUV-BEGMT                 TO DHUV-MID-BEGMT                    
046000     MOVE PHUV-ADGMT-GATA            TO DHUV-MID-ADGMT-GATA               
046100     MOVE PHUV-ADGMT-PADR            TO DHUV-MID-ADGMT-PADR               
046200     IF PHUV-FLRESTN = JA                                                 
046300        MOVE 'L'                     TO DHUV-MID-KDROPACK                 
046400     ELSE                                                                 
046500        MOVE SPACE                   TO DHUV-MID-KDROPACK                 
046600     END-IF                                                               
046700     MOVE PHUV-IDKONTO               TO WS-IDKONTO-NUM                    
046800     MOVE WS-IDKONTO-NUM             TO DHUV-MID-IDKONTO                  
046900     MOVE PHUV-IDKST                 TO WS-IDKST                          
047000     MOVE WS-IDKST                   TO DHUV-MID-IDKST                    
047100     MOVE PHUV-IDANALYS              TO DHUV-MID-IDANALYS                 
047200     MOVE PHUV-BEVARREF              TO DHUV-MID-BEVARREF                 
047300     MOVE SPACE                      TO DHUV-MID-KDTULLVE                 
047400     MOVE SPACE                      TO DHUV-MID-KDNOTES                  
047500     MOVE NEJ                        TO DHUV-MID-FLAUTFAK                 
047600     MOVE NEJ                        TO DHUV-MID-FLAUTPAC                 
047700     MOVE NEJ                        TO DHUV-MID-FLEMBORD                 
047800     MOVE NEJ                        TO DHUV-MID-FLOVRLEV                 
047900     MOVE SPACE                      TO DHUV-MID-IDKAMPRF                 
048000                                        DHUV-MID-IDDC                     
048100                                        DHUV-MID-FLLSBOK                  
048300                                        DHUV-MID-IDBILREG                 
048310                                        DHUV-MID-IDVIN                    
048320                                        DHUV-MID-IDCISNR                  
048400     MOVE PHUV-IDFTG                 TO DHUV-MID-IDFTG                    
048500     MOVE PHUV-ADBET                 TO DHUV-MID-ADBET                    
048600     MOVE PHUV-BEBET                 TO DHUV-MID-BEBET                    
048700     MOVE PHUV-IDSKYLT               TO DHUV-MID-IDSKYLT                  
048800     MOVE SPACE                      TO DHUV-MID-KDORDTYP-LDC             
048900     MOVE ZERO                       TO DHUV-MID-TIREPDAT                 
049101                                        DHUV-MID-IDGROSS                  
049200     MOVE NEJ                        TO DHUV-MID-FLFORBI                  
049300                                        DHUV-MID-FLORDTIL                 
049400                                                                          
049500     .                                                                    
049600     EJECT                                                                
049700                                                                          
049800 HB-SKAPA-SKICKA-PRAD-TRANS SECTION.                                      
049900                                                                          
050000     PERFORM IMS-GHU-PRAD-WDE9                                            
050100     IF SEGMENT-FINNS                                                     
050200        MOVE 'W4I25201'              TO MSG-KOM-IDCPYTXT                  
050300        COMPUTE MSG-KVLL = LENGTH OF DRAD-MID-W4I25201 + 17               
050400        MOVE LOW-VALUE               TO MSG-KDZ1                          
050500        MOVE LOW-VALUE               TO MSG-KDZ2                          
050600        MOVE SPACE                   TO MSG-AREA                          
050700        MOVE 'W4T252X'               TO MSG-KDTRANS-1                     
050800        MOVE '4252'                  TO MSG-IDTRANS-1                     
050900        MOVE WS-KDMFSFOR             TO MSG-KDMFSFOR-1                    
051000                                                                          
051100        PERFORM HBA-INITIERA-PRAD-TRANS                                   
051200     ELSE                                                                 
051300*       PROFORMARADER SAKNAS SKALL EJ KUNNA SKE                           
051400        MOVE 'PROFORMARADER SAKNAS'  TO FELTEXT                           
051500        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000 HBA-INITIERA-PRAD-TRANS SECTION.                                         
052100                                                                          
052200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
052300                                                                          
052400        PERFORM HBAA-INITIERA-ORDERRADER                                  
052500        MOVE DRAD-MID-W4I25201 TO MSG-INDATA-MINUS-1-TRANSKOD             
052600                                                                          
052700                                                                          
052800        CALL W006KOM USING MSG-PCB                                        
052900                           0693-PCB                                       
053000                           KOMA-PCB                                       
053100                           MSG-KOM-WMSGKOM                                
053200                           MSG-IO-AREA                                    
053300                                                                          
053400        IF MSG-KOM-IDMFSMED NOT = SPACE                                   
053500*          FELAKTIG UPPDATERING AV PROFORMARADER PÅ                       
053600*          KOMMUNIKATIONS DB                                              
053700           MOVE                                                           
053800           'FELAKTIG UPPDATERING AV PRAD PÅ KOMMUNIKATIONS DB'            
053900                                        TO FELTEXT                        
054000           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
054100        END-IF                                                            
054200     END-PERFORM                                                          
054300     .                                                                    
054400                                                                          
054500     EJECT                                                                
054600                                                                          
054700 HBAA-INITIERA-ORDERRADER SECTION.                                        
054800                                                                          
054900     MOVE +1                    TO RAD-IX                                 
055000     MOVE SPACE                 TO DRAD-MID-W4I25201                      
055100     MOVE 'OREL'                TO DRAD-MID-IDSYSTEM                      
055200     MOVE WS-IDDISTR-NUM        TO DRAD-MID-IDDISTR                       
055300     MOVE WS-IDKUNDNR-NUM       TO DRAD-MID-IDKUNDNR                      
055400     MOVE WS-IDORDNR7-NUM       TO DRAD-MID-IDORDNR                       
055500     MOVE SPACE                 TO DRAD-MID-BEVOLREF                      
055600                                                                          
055700     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                        
055800                   RAD-IX > MAX-RAD-IX                                    
055900        MOVE PRAD-IDARTNR       TO WS-IDARTNR-NUM                         
056000                                   W-IDARTNR                              
056100        PERFORM HBAAA-UPPDATERA-OFFERTSALDO                               
056200                                                                          
056300        MOVE WS-IDARTNR-NUM     TO DRAD-MID-IDARTNR(RAD-IX)               
056400        MOVE PRAD-REKSIFFR      TO WS-REKSIFFR-NUM                        
056500        MOVE WS-REKSIFFR-NUM    TO DRAD-MID-REKSIFFR(RAD-IX)              
056600        MOVE PRAD-KVBEART-Q     TO WS-KVBEART-NUM                         
056700        MOVE WS-KVBEART-NUM     TO DRAD-MID-KVBEART(RAD-IX)               
056800        IF DIST79-DEALER-PRICE                                            
056900                                                                          
057000          IF PRAD-PRARTNTO-LOC > 0                                        
057100            MOVE PRAD-PRARTNTO-LOC TO WS-PRARTNTO-NUM-LOC                 
057200            MOVE WS-PRARTNTO-NUM-LOC                                      
057300                              TO DRAD-MID-PRARTNTO-LOC(RAD-IX)            
057400          ELSE                                                            
057500            IF PRAD-PRARTNTO-LOCPREL  > 0                                 
057600              MOVE PRAD-PRARTNTO-LOCPREL                                  
057700                                   TO WS-PRARTNTO-NUM-LOC                 
057800              MOVE WS-PRARTNTO-NUM-LOC                                    
057900                              TO DRAD-MID-PRARTNTO-LOC(RAD-IX)            
058000              MOVE PRAD-PRARTNTO-LOCPREL TO PRAD-PRARTNTO-LOC             
058100              MOVE ZERO                  TO PRAD-PRARTNTO-LOCPREL         
058200              PERFORM IMS-REPL-PRAD-WDE9                                  
058300            END-IF                                                        
058400          END-IF                                                          
058500          MOVE PRAD-PRARTBTO-LOC TO WS-PRARTBTO-NUM-LOC                   
058600          MOVE WS-PRARTBTO-NUM-LOC                                        
058700                            TO DRAD-MID-PRARTBTO-LOC(RAD-IX)              
058800                                                                          
058900          MOVE PRAD-KDVALISO     TO DRAD-MID-KDVALISO(RAD-IX)             
059000          MOVE PRAD-KDVAT        TO DRAD-MID-KDVAT(RAD-IX)                
059100          MOVE PRAD-RERAB        TO DRAD-MID-RERAB(RAD-IX)                
059200          MOVE PRAD-KDRAB        TO DRAD-MID-KDRAB(RAD-IX)                
059300          MOVE PRAD-BEART-VIPS   TO DRAD-MID-BEART-VIPS(RAD-IX)           
059400                                                                          
059500        ELSE                                                              
059600          MOVE PRAD-PRARTNTO    TO WS-PRARTNTO-NUM                        
059700          MOVE WS-PRARTNTO-NUM TO DRAD-MID-PRARTNTO(RAD-IX)               
059800        END-IF                                                            
059900        IF PRAD-KVVECKOR-TPO5 = +0                                        
060000           MOVE SPACE           TO DRAD-MID-TITPO(RAD-IX)                 
060100        ELSE                                                              
060200           COMPUTE DATUM-DAGNR-RAD = PRAD-KVVECKOR-TPO5 * 7               
060300           ADD  DATUM-DAGNR     TO DATUM-DAGNR-RAD                        
060400           MOVE DATUM-AR        TO DATUM-AR-RAD                           
060500                                                                          
060600           IF DATUM-DAGNR-RAD > 365                                       
060700              SUBTRACT 365      FROM DATUM-DAGNR-RAD                      
060800              ADD 1             TO   DATUM-AR-RAD                         
060900           END-IF                                                         
061000                                                                          
061100           IF DATUM-DAGNR-RAD > 365                                       
061200              SUBTRACT 365      FROM DATUM-DAGNR-RAD                      
061300              ADD 1             TO   DATUM-AR-RAD                         
061400           END-IF                                                         
061500                                                                          
061600           MOVE DATUM-AR-DAGNR-RAD-NUM TO DAT-I-TIDATUM                   
061700           MOVE 'AADDD '           TO DAT-KDDATFORM                       
061800           CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,              
061900                               DAT-O-TIDATUM, DAT-KDSVAR                  
062000           IF DAT-KDSVAR-OK                                               
062100              MOVE DAT-TIAAMMDD                                           
062200                                TO DRAD-MID-TITPO(RAD-IX)                 
062300           ELSE                                                           
062400*             DATUMKONVERTERING HAR GÅTT FEL DRAD                         
062500              MOVE                                                        
062600          'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL I DRAD'           
062700                                              TO FELTEXT                  
062800              CALL ABEND USING RKOD-ABEND-MED-DUMP                        
062900           END-IF                                                         
063000        END-IF                                                            
063100        MOVE SPACE              TO DRAD-MID-FLRESTN(RAD-IX)               
063200        MOVE PRAD-KDKVBRYT      TO WS-KDKVBRYT-NUM                        
063300        MOVE WS-KDKVBRYT-NUM    TO DRAD-MID-KDKVBRYT(RAD-IX)              
063400        MOVE PRAD-FLINVEST      TO DRAD-MID-FLINVEST(RAD-IX)              
063500        MOVE ZERO               TO DRAD-MID-KDVRINFO(RAD-IX)              
063600        MOVE PHUV-IDKONTO       TO WS-IDKONTO-NUM                         
063700        MOVE WS-IDKONTO-NUM     TO DRAD-MID-IDKONTO(RAD-IX)               
063800        MOVE PHUV-IDKST         TO WS-IDKST                               
063900        MOVE WS-IDKST           TO DRAD-MID-IDKST(RAD-IX)                 
064000        MOVE PRAD-BERADREF      TO DRAD-MID-BERADREF(RAD-IX)              
064100        MOVE '1'                TO DRAD-MID-KDDSP(RAD-IX)                 
064200        MOVE NEJ                TO DRAD-MID-FLSLATT(RAD-IX)               
064300        MOVE SPACE              TO DRAD-MID-IDKUNDRF-WIP(RAD-IX)          
064400        ADD +1                  TO RAD-IX                                 
064500        PERFORM IMS-GHN-PRAD-WDE9                                         
064600     END-PERFORM                                                          
064700                                                                          
064800     IF SEGMENT-FINNS AND RAD-IX > MAX-RAD-IX                             
064900        MOVE NEJ                TO DRAD-MID-FLSLUT                        
065000     ELSE                                                                 
065100        MOVE JA                 TO DRAD-MID-FLSLUT                        
065200     END-IF                                                               
065300     .                                                                    
065400                                                                          
065500     EJECT                                                                
065600                                                                          
065700 HBAAA-UPPDATERA-OFFERTSALDO SECTION.                                     
065800                                                                          
065900     PERFORM IMS-GHU-ARTM-WDK9                                            
066000     IF SEGMENT-FINNS                                                     
066100        COMPUTE ART-KVOFFERT =                                            
066200                ART-KVOFFERT - PRAD-KVBEART-Q                             
066300        PERFORM IMS-REPL-ARTM-WDK9                                        
066400     ELSE                                                                 
066500*       ARTIKELN SAKNAS PÅ ARTIKELREGISTER WDK9                           
066600        MOVE 'ARTIKELN SAKNAS PÅ ARTIKELREGISTRET WDK9'                   
066700                                  TO FELTEXT                              
066800        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
066900     END-IF                                                               
067000     .                                                                    
067100                                                                          
067200     EJECT                                                                
067300                                                                          
067400 I-STARTA-4295 SECTION.                                                   
067500                                                                          
067600     PERFORM IMS-INSERT-4295-MSG                                          
067700     .                                                                    
067800     EJECT                                                                
067900 J-SKICKA-SVAR-TILL-4266 SECTION.                                         
068000                                                                          
068100     IF W-IDTRANS NOT = 'FMV '                                            
068200       MOVE LOW-VALUE                  TO 4266-Z1                         
068300       MOVE LOW-VALUE                  TO 4266-Z2                         
068400       MOVE '4266'                     TO MOD-IDTRANS                     
068500       MOVE INF-RELEASE-OK             TO MED-IDMFSINF                    
068600       CALL WMEDKONV USING MED-WMEDAREA                                   
068700       MOVE MED-MFSINF                 TO MOD-TEMFSINF                    
068800                                                                          
068900       PERFORM IMS-INSERT-MSG                                             
069000     END-IF                                                               
069100     .                                                                    
069200                                                                          
069300     EJECT                                                                
069400                                                                          
069500 K-UPPDATERA-PROFHUVUD SECTION.                                           
069600                                                                          
069700      PERFORM IMS-GHU-PHUV-WDE8                                           
069800                                                                          
069900      MOVE DAGENS-DATUM              TO PHUV-TIORDDAT                     
070000                                        PHUV-TIUPPDAT                     
070100      MOVE DAGENS-TID                TO PHUV-TIUPPTID                     
070200                                                                          
070300      IF DIST79-DEALER-PRICE                                              
070400        COMPUTE PHUV-SUORDV-LOC = PHUV-SUORDV-LOC +                       
070500                                    PHUV-SUORDV-LOCPREL                   
070600                                                                          
070700        MOVE ZERO                      TO PHUV-SUORDV-LOCPREL             
070800      END-IF                                                              
070900                                                                          
071000      PERFORM IMS-REPL-PHUV-WDE8                                          
071100                                                                          
071200     .                                                                    
071300                                                                          
071400     EJECT                                                                
071500                                                                          
071600* --- IMS SEKTIONER ---                                                   
071700     SKIP3                                                                
071800 IMS-GET-MSG SECTION.                                                     
071900                                                                          
072000     MOVE '  QC' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     SKIP3                                                                
072600 IMS-INSERT-MSG SECTION.                                                  
072700                                                                          
072800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
072900     MOVE SPACE TO GODK-STATUSKODER                                       
073000     CALL CBLTDLI USING ISRT MSG-PCB 4266-IO-AREA MFS-IDMOD               
073100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073200     PERFORM IMS-STATUSKONTROLL                                           
073300     .                                                                    
073400     EJECT                                                                
073500 IMS-INSERT-4295-MSG SECTION.                                             
073600                                                                          
073700     MOVE SPACE TO GODK-STATUSKODER                                       
073800     CALL CBLTDLI USING ISRT 4295-PCB 4295-MSG-IO-AREA                    
073900     MOVE 4295-STATUS-CODE TO STATUS-WS                                   
074000     PERFORM IMS-STATUSKONTROLL                                           
074100     .                                                                    
074200     EJECT                                                                
074300 IMS-GET-PHUV-WDE8 SECTION.                                               
074400                                                                          
074500     STRING 'WLPROC01(WDE801KY =' W-IDGMTREF-X ')'                        
074600          DELIMITED BY SIZE INTO SSA1                                     
074700     MOVE '  GE' TO GODK-STATUSKODER                                      
074800     CALL CBLTDLI USING GU PROC-PCB DLI-IO-WLPROC01 SSA1                  
074900     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
075000     PERFORM IMS-STATUSKONTROLL                                           
075100     .                                                                    
075200     EJECT                                                                
075300 IMS-GHU-PHUV-WDE8 SECTION.                                               
075400                                                                          
075500     STRING 'WLPROC01(WDE801KY =' W-IDGMTREF-X ')'                        
075600          DELIMITED BY SIZE INTO SSA1                                     
075700     MOVE '  GE' TO GODK-STATUSKODER                                      
075800     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-WLPROC01 SSA1                 
075900     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
076000     PERFORM IMS-STATUSKONTROLL                                           
076100     .                                                                    
076200     EJECT                                                                
076300 IMS-REPL-PHUV-WDE8 SECTION.                                              
076400                                                                          
076500     MOVE '  '   TO GODK-STATUSKODER                                      
076600     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-WLPROC01                     
076700     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
076800     PERFORM IMS-STATUSKONTROLL                                           
076900     .                                                                    
077000     EJECT                                                                
077100 IMS-GHU-PRAD-WDE9 SECTION.                                               
077200                                                                          
077300     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
077400                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
077500          DELIMITED BY SIZE INTO SSA1                                     
077600     MOVE '  GE' TO GODK-STATUSKODER                                      
077700     CALL CBLTDLI USING GHU PROD-PCB DLI-IO-WLPROD01 SSA1                 
077800     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100     EJECT                                                                
078200 IMS-GHN-PRAD-WDE9 SECTION.                                               
078300                                                                          
078400     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
078500                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
078600          DELIMITED BY SIZE INTO SSA1                                     
078700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
078800     CALL CBLTDLI USING GHN PROD-PCB DLI-IO-WLPROD01 SSA1                 
078900     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
079000     PERFORM IMS-STATUSKONTROLL                                           
079100     .                                                                    
079200     EJECT                                                                
079300 IMS-REPL-PRAD-WDE9 SECTION.                                              
079400                                                                          
079500     MOVE '  '   TO GODK-STATUSKODER                                      
079600     CALL CBLTDLI USING REPL PROD-PCB DLI-IO-WLPROD01                     
079700     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
079800     PERFORM IMS-STATUSKONTROLL                                           
079900     .                                                                    
080000     EJECT                                                                
080100 IMS-GHU-ARTM-WDK9 SECTION.                                               
080200                                                                          
080300     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
080400          DELIMITED BY SIZE INTO SSA1                                     
080500     MOVE '  GE' TO GODK-STATUSKODER                                      
080600     CALL CBLTDLI USING GHU ARTM-PCB ARTM-IO-AREA SSA1                    
080700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
080800     PERFORM IMS-STATUSKONTROLL                                           
080900     .                                                                    
081000     EJECT                                                                
081100 IMS-REPL-ARTM-WDK9 SECTION.                                              
081200                                                                          
081300     MOVE '  '   TO GODK-STATUSKODER                                      
081400     CALL CBLTDLI USING REPL ARTM-PCB ARTM-IO-AREA                        
081500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     EJECT                                                                
081900 IMS-STATUSKONTROLL SECTION.                                              
082000                                                                          
082100     SET STATUS-IX TO 1                                                   
082200     SEARCH GODK-STATUS                                                   
082300       AT END                                                             
082400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
082500         DELIMITED BY SIZE INTO FELTEXT                                   
082600         CALL FELLOG                                                      
082700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
082800     END-SEARCH                                                           
082900     .                                                                    
083000     EJECT                                                                
