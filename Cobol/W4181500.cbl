000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4181500.                                                
000400*AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500*DATE-WRITTEN.   95/06/26.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR BEKRÄFTAT ANTAL FRÅN VIPS PÅ WDA2                     
001100*        OM KVLEVANM-BEKR ÄNDRAS TILL +0 SÅ BLIR RADEN ANN OCH OM         
001200*        INGA FLER OBEHANDLADE RADER FINNS, SÅ SÄTTS STATUS TILL 7        
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
001500*        PROGRAMMET LÄSER/UPPDATERAR WDR5 (WDGX4103)                      
001600*        PROGRAMMET LÄSER            WDB6                                 
001700*        PROGRAMMET LÄSER            WDA3F                                
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300*    E-TRACKER 1572353 DATUM 20050425                                     
002400*    E-TRACKER 1658417 DATUM 20060302                                     
002500*    E-TRACKER 5787718 DATUM 20071101                                     
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*          --- INFIL                                                      
003600     SELECT W41811                     ASSIGN TO W41815D1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W41811                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W41811      -L.                                                
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W4181500'.            
005300 77  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO COMP-3.           
005400 77  SPAR-IDKUNDNR               PIC S9(7)   VALUE ZERO COMP-3.           
005500 77  SPAR-IDRAPPNR               PIC  9(7)   VALUE ZERO.                  
005600 01  CHKP-VAR.                                                            
005700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
006000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
006100 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006200 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 77  W-W41811-KVPOST-IN          PIC S9(3)   VALUE +0 COMP-3.             
006600 77  WS-KVRADER-ANN              PIC S9(7)  VALUE +0    COMP-3.           
006700 77  WS-RADPRIS                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006800 77  WS-NYTT-RADPRIS             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006900 77  SPAR-KVLEVANM-BEKR          PIC S9(7)  VALUE ZERO  COMP-3.           
007000                                                                          
007100 77  GODK-RADER-FINNS-SW         PIC X       VALUE 'N'.                   
007200     88  GODK-RADER-FINNS                    VALUE 'J'.                   
007300     88  GODK-RADER-SAKNAS                   VALUE 'N'.                   
007400                                                                          
007500 77  ANNULLERAD-RAD-SW           PIC X       VALUE 'N'.                   
007600     88  ANNULLERAD-RAD                      VALUE 'J'.                   
007700                                                                          
007710 77  RETUR-FINNS-PA-RETTERM-SW   PIC X       VALUE 'N'.                   
007720     88  RETUR-FINNS-PA-RETTERM              VALUE 'J'.                   
007730                                                                          
007800 77  WDR501-SW                   PIC X       VALUE 'N'.                   
007900     88  WDR501-FINNS                        VALUE 'J'.                   
008000                                                                          
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400                                                                          
008500 77  W41811-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W41811                       VALUE 'J'.                   
008700     EJECT                                                                
008800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009300     EJECT                                                                
009400                                                                          
009500 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
009600*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
009700*                                                                         
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010400     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL POSTSUM                                          
010700*                                                                         
010800*01  -COPY W0005   -PRE  POSTSUM-                                         
010900     EJECT                                                                
011000*    ---  LÄNKAREA TILL W418OKOD                                          
011100 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
011200                                                                          
011300*01 -COPY W418OKOD           -PRE OKOD-.                                  
011400     EJECT                                                                
011410*01 -COPY WWIDFTG                                                         
011420     EJECT                                                                
011500 01  IN-AREA-START               PIC X(24)   VALUE                        
011600                                             'IN-AREA-START'.             
011700     SKIP2                                                                
011800                                                                          
011900*01  AREA -COPY W41811     -PRE IN-                                       
012000*                                                                         
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012300     SKIP3                                                                
012400 01  NYCKLAR-TILL-DLI.                                                    
012500     03  W-IDLEVANM-X.                                                    
012600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
012700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
012800         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
012900                                                                          
013000     03  W-WDA211KY-X.                                                    
013100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013200         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
013300                                                                          
013400* TILL WDR5 ATTEST AV KREDITNOTOR                                         
013500   03  W-WDGXKEY-4103-X.                                                  
013600       05  W-IDHTYP-4103       PIC X(4)  VALUE '4103'.                    
013700       05  W-IDDISTR-4103      PIC S9(5) VALUE ZERO COMP-3.               
013800       05  W-IDKUNDNR-4103     PIC S9(7) VALUE ZERO COMP-3.               
013900       05  W-IDRAPPNR-4103     PIC 9(7)  VALUE ZERO.                      
014000       05  FILLER              PIC X(12)   VALUE LOW-VALUE.               
014100                                                                          
014200   03  W-KEY4104-X.                                                       
014300       05  W-IDDC-4104         PIC X(2)  VALUE SPACE.                     
014400       05  W-KDKRENOT-4104     PIC X(2)  VALUE SPACE.                     
014500                                                                          
014600   03  W-IDDC-B6-X.                                                       
014700       05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                   
014800                                                                          
014900   03  W-WDA3F1KY-MIN-X.                                                  
015000       05  W-WDA3-IDDC-MIN     PIC X(2)         VALUE SPACE.              
015100       05  W-WDA3-IDDISTR-MIN  PIC S9(5) COMP-3 VALUE ZERO.               
015200       05  W-WDA3-IDKUNDNR-MIN PIC S9(7) COMP-3 VALUE ZERO.               
015300       05  W-WDA3-IDRAPPNR-MIN PIC 9(7)         VALUE ZERO.               
015400       05  W-WDA3-IDRT-MIN     PIC X(3)         VALUE SPACE.              
015500       05  W-WDA3-IDRTLOP-MIN  PIC 9(3)         VALUE ZERO.               
015600       05  W-WDA3-IDKOLLI-MIN  PIC S9(5) COMP-3 VALUE ZERO.               
015700       05  W-WDA3-DAREGDAT-MIN PIC S9(8)        VALUE ZERO.               
015800       05  W-WDA3-TIKLOCK-MIN  PIC S9(9) COMP-3 VALUE ZERO.               
015900                                                                          
016000   03  W-WDA3F1KY-MAX-X.                                                  
016100       05  W-WDA3-IDDC-MAX     PIC X(2)         VALUE SPACE.              
016200       05  W-WDA3-IDDISTR-MAX  PIC S9(5) COMP-3 VALUE ZERO.               
016300       05  W-WDA3-IDKUNDNR-MAX PIC S9(7) COMP-3 VALUE ZERO.               
016400       05  W-WDA3-IDRAPPNR-MAX PIC 9(7)         VALUE ZERO.               
016500       05  W-WDA3-IDRT-MAX     PIC X(3)         VALUE SPACE.              
016600       05  W-WDA3-IDRTLOP-MAX  PIC 9(3)         VALUE ZERO.               
016700       05  W-WDA3-IDKOLLI-MAX  PIC S9(5) COMP-3 VALUE ZERO.               
016900       05  W-WDA3-DAREGDAT-MAX PIC S9(8)        VALUE ZERO.               
017100       05  W-WDA3-TIKLOCK-MAX  PIC S9(9) COMP-3 VALUE ZERO.               
017300     SKIP2                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018000     88  IMS-EJ-OK                           VALUE 'XD'.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(128).                              
018600 01  SSA2                        PIC X(64).                               
018700     EJECT                                                                
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200 01  FILLER                     PIC X(16) VALUE 'DLI-IO-AREA-ANM'.        
019300     SKIP3                                                                
019400 01  DLI-IO-AREA-ANM.                                                     
019500     03  IO-AREA-ANM             PIC X(150)  VALUE SPACE.                 
019600     SKIP3                                                                
019700     03  WLKREE01 REDEFINES IO-AREA-ANM.                                  
019800*        05  -COPY WDA201                                                 
019900*    ---  DLI INPUT-OUTPUT AREA                                           
020000     EJECT                                                                
020100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-AREA-LEV'.        
020200     SKIP3                                                                
020300 01  DLI-IO-AREA-LEV.                                                     
020400     03  IO-AREA-LEV             PIC X(300)  VALUE SPACE.                 
020500     SKIP3                                                                
020600     03  WLKREE11 REDEFINES IO-AREA-LEV.                                  
020700*        05  -COPY WDA211                                                 
020800     EJECT                                                                
020900                                                                          
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
021100 01  DLI-IO-WDGX4103.                                                     
021200*    03  -COPY WDGX4103                                                   
021300     EJECT                                                                
021400                                                                          
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4104'.                    
021600 01  DLI-IO-WDGX4104.                                                     
021700*    03  -COPY WDGX4104                                                   
021800     EJECT                                                                
021900                                                                          
022000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
022100 01  DLI-IO-WDB601.                                                       
022200*    03  -COPY WDB601                                                     
022300     EJECT                                                                
022310                                                                          
022320 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA3F1'.                      
022330 01  DLI-IO-WDA3F1.                                                       
022340*    03  -COPY WDA3F1                                                     
022400     EJECT                                                                
022500 LINKAGE SECTION.                                                         
022600                                                                          
022700*01  -COPY W0009   -PRE MSG-                                              
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE KREE-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE 4103-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE WDB6-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023710*01  -COPY W0008  -PRE WDA3-                                              
023720     05  FILLER                  PIC X.                                   
023730     EJECT                                                                
023800 PROCEDURE DIVISION  USING MSG-PCB KREE-PCB 4103-PCB WDB6-PCB             
023810                           WDA3-PCB.                                      
023900     ENTRY 'DLITCBL' USING MSG-PCB KREE-PCB 4103-PCB WDB6-PCB             
023910                           WDA3-PCB.                                      
024000                                                                          
024100     SKIP2                                                                
024200     PERFORM A-INIT                                                       
024300     PERFORM S01-LAES-W41811                                              
024400     PERFORM B-BEHANDLA                                                   
024500                                                                          
024600     PERFORM Z-FINIT                                                      
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300     SKIP2                                                                
025400                                                                          
025500     PERFORM IMS-RESTART                                                  
025600                                                                          
025700     OPEN INPUT W41811                                                    
025800                                                                          
025900                                                                          
026000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026001                                                                          
026010     MOVE LOW-VALUE  TO W-WDA3F1KY-MIN-X                                  
026030     MOVE HIGH-VALUE TO W-WDA3F1KY-MAX-X                                  
026100     .                                                                    
026200     EJECT                                                                
026300 B-BEHANDLA SECTION.                                                      
026400                                                                          
026500     PERFORM UNTIL END-OF-W41811                                          
026600       IF CHKP-ANT > CHKP-MAX                                             
026700         PERFORM X-TAG-CHECKPOINT                                         
026800       END-IF                                                             
026900       MOVE IN-IDDISTR                   TO W-IDDISTR                     
027000                                            SPAR-IDDISTR                  
027100                                            W-IDDISTR-4103                
027200                                            TEST-IDDISTR                  
027300       MOVE IN-IDKUNDNR                  TO W-IDKUNDNR                    
027400                                            SPAR-IDKUNDNR                 
027500                                            W-IDKUNDNR-4103               
027600       MOVE IN-IDRAPPNR                  TO W-IDRAPPNR                    
027700                                            SPAR-IDRAPPNR                 
027800                                            W-IDRAPPNR-4103               
027900                                                                          
028000       MOVE +0                           TO WS-KVRADER-ANN                
028100       MOVE NEJ                          TO ANNULLERAD-RAD-SW             
028200       MOVE NEJ                          TO WDR501-SW                     
028300                                                                          
028400       PERFORM IMS-GU-WDR501-4103                                         
028500       IF SEGMENT-FINNS                                                   
028600         MOVE JA                         TO WDR501-SW                     
028700       END-IF                                                             
028800                                                                          
028900                                                                          
029000       PERFORM IMS-GU-KREE-ANM                                            
029010       MOVE ANM-IDFTG                    TO WS-IDFTG                      
029020                                                                          
029100       IF ANM-KDLEVANM = '1' OR '2' OR '3' OR '4'                         
029200         PERFORM UNTIL END-OF-W41811 OR                                   
029300                       IN-IDDISTR  NOT = SPAR-IDDISTR  OR                 
029400                       IN-IDKUNDNR NOT = SPAR-IDKUNDNR OR                 
029500                       IN-IDRAPPNR NOT = SPAR-IDRAPPNR                    
029600            MOVE IN-IDARTNR             TO W-IDARTNR                      
029700            MOVE IN-IDRADNR             TO W-IDRADNR                      
029800            PERFORM IMS-GHU-KREE-LEV                                      
029900                                                                          
030000*- KOD 12 OCH 22 BOKAR NER SALDOT DIREKT OCH TF TAR BEKR ANTAL.           
030100*- DÄRFÖR BLIR DET PROBLEM OM MAN ÄNDRAR ANTALET I EFTERHAND.             
030200            IF LEV-KDANMORS = '12' OR '22' OR '27'                        
030300              DISPLAY '** DISTRIKT    = ' SPAR-IDDISTR                    
030400              DISPLAY '** KUND        = ' SPAR-IDKUNDNR                   
030500              DISPLAY '** RAPPNR      = ' SPAR-IDRAPPNR                   
030600              DISPLAY '** ARTNR       = ' W-IDARTNR                       
030700              DISPLAY '** KOD         = ' LEV-KDANMORS                    
030800                                                                          
030900              PERFORM S01-LAES-W41811                                     
031000            ELSE                                                          
031100                                                                          
031200              IF IN-KVLEVANM-BEKR > LEV-KVLEVANM-BEKR                     
031300                 DISPLAY '** DISTRIKT    = ' SPAR-IDDISTR                 
031400                 DISPLAY '** KUND        = ' SPAR-IDKUNDNR                
031500                 DISPLAY '** RAPPNR      = ' SPAR-IDRAPPNR                
031600                 DISPLAY '** ARTNR       = ' W-IDARTNR                    
031700                 DISPLAY '** RADNR       = ' W-IDRADNR                    
031800                 DISPLAY '** ANTAL       = ' IN-KVLEVANM-BEKR             
031900                                                                          
032000                 PERFORM S01-LAES-W41811                                  
032100              ELSE                                                        
032101                MOVE NEJ  TO RETUR-FINNS-PA-RETTERM-SW                    
032102                PERFORM BE-KOLLA-RETURTERMINAL                            
032103                                                                          
032110                IF RETUR-FINNS-PA-RETTERM                                 
032120                   DISPLAY '** DISTRIKT    = ' SPAR-IDDISTR               
032130                   DISPLAY '** KUND        = ' SPAR-IDKUNDNR              
032140                   DISPLAY '** RAPPNR      = ' SPAR-IDRAPPNR              
032150                   DISPLAY '** ARTNR       = ' W-IDARTNR                  
032160                   DISPLAY '** RADNR       = ' W-IDRADNR                  
032170                   DISPLAY '** ANTAL       = ' IN-KVLEVANM-BEKR           
032171                                                                          
032172                   PERFORM S01-LAES-W41811                                
032191                ELSE                                                      
032200                   MOVE LEV-KVLEVANM-BEKR TO SPAR-KVLEVANM-BEKR           
032300                                                                          
032400**-- OM RAD ÄNDRAS SKALL SUMMAN FÖR KNOTAN RÄKNAS OM PÅ WDR5              
032500**-- WDGX4104 - SOX-ÄNDRING 20050425.UNDANTAG USA/CAN.                    
032600**-- STATUS 4 HAR REDAN FÅTT OK ATTEST OCH BERÖRS EJ ENL. SUSSI.          
032700                   IF ANM-KDLEVANM = '1' OR '2' OR '3'                    
032800                     IF IN-KVLEVANM-BEKR = LEV-KVLEVANM-BEKR OR           
032900                        IN-KVLEVANM-BEKR = +0                             
033000                       CONTINUE                                           
033100                     ELSE                                                 
033200                       IF IDFTG-PV OR IDFTG-CN                            
033300                         IF LEV-IDDC NOT = W-IDDC-B6                      
033400                           MOVE LEV-IDDC  TO W-IDDC-B6                    
033500                           PERFORM IMS-GU-WDB601                          
033600                         END-IF                                           
033700                         IF DCS-NDC-PF                                    
033800                           CONTINUE                                       
033900                         ELSE                                             
034000                           PERFORM BB-UPPDAT-WDGX4103                     
034100                         END-IF                                           
034200                       END-IF                                             
034300                     END-IF                                               
034400                   END-IF                                                 
034500                                                                          
034600                   MOVE IN-KVLEVANM-BEKR TO LEV-KVLEVANM-BEKR             
034700                   IF LEV-KVLEVANM-BEKR = +0                              
034800                      MOVE JA               TO LEV-FLANNULL               
034900                      MOVE 'ANN'            TO LEV-KDKREBEH               
035000                      ADD +1                TO WS-KVRADER-ANN             
035100                                                                          
035200                      IF ANM-KDLEVANM = '1' OR '2' OR '3'                 
035300                        MOVE JA             TO ANNULLERAD-RAD-SW          
035400                        IF IDFTG-PV OR IDFTG-CN                           
035500                          IF LEV-IDDC NOT = W-IDDC-B6                     
035600                            MOVE LEV-IDDC  TO W-IDDC-B6                   
035700                            PERFORM IMS-GU-WDB601                         
035800                          END-IF                                          
035900                          IF DCS-NDC-PF                                   
036000                            CONTINUE                                      
036100                          ELSE                                            
036200                            PERFORM BD-UPPDAT-WDGX4103-ANN                
036300                          END-IF                                          
036400                        END-IF                                            
036500                      END-IF                                              
036600                   END-IF                                                 
036700                                                                          
036800                   PERFORM IMS-REPL-KREE-LEV                              
036900                   ADD +1                   TO CHKP-ANT                   
037000                   PERFORM S01-LAES-W41811                                
037100                END-IF                                                    
037110              END-IF                                                      
037200            END-IF                                                        
037300         END-PERFORM                                                      
037400                                                                          
037500         IF ANNULLERAD-RAD                                                
037600           PERFORM BC-UPPDAT-WDGX4104                                     
037700         END-IF                                                           
037800                                                                          
037900         MOVE NEJ                       TO GODK-RADER-FINNS-SW            
038000         PERFORM BA-UPD-STATUS                                            
038100       ELSE                                                               
038200         PERFORM S01-LAES-W41811                                          
038300       END-IF                                                             
038400     END-PERFORM                                                          
038500                                                                          
038600     .                                                                    
038700     EJECT                                                                
038800 BA-UPD-STATUS SECTION.                                                   
038900                                                                          
039000     PERFORM IMS-GU-KREE-ANM                                              
039010     MOVE ANM-IDFTG                    TO WS-IDFTG                        
039100     PERFORM IMS-GNP-KREE-LEV                                             
039200     PERFORM UNTIL SEGMENT-SAKNAS OR GODK-RADER-FINNS-SW = JA             
039300        IF LEV-KVLEVANM-BEKR > +0 AND                                     
039400           LEV-FLANNULL = NEJ                                             
039500           MOVE JA                        TO GODK-RADER-FINNS-SW          
039600        END-IF                                                            
039700        PERFORM IMS-GNP-KREE-LEV                                          
039800     END-PERFORM                                                          
039900                                                                          
040000     IF ANM-KDLEVANM = '4'                                                
040100       PERFORM IMS-GHU-KREE-ANM                                           
040200       COMPUTE ANM-KVRADER-RT   =                                         
040300               ANM-KVRADER-RT   - WS-KVRADER-ANN                          
040400       COMPUTE ANM-KVRADER-OBEH =                                         
040500               ANM-KVRADER-OBEH - WS-KVRADER-ANN                          
040600       IF ANM-KVRADER-RT      = +0                                        
040700         MOVE '7'             TO ANM-KDLEVANM                             
040800       END-IF                                                             
040900       PERFORM IMS-REPL-KREE-ANM                                          
041000       ADD +1                            TO CHKP-ANT                      
041100     ELSE                                                                 
041200       IF GODK-RADER-FINNS                                                
041300          CONTINUE                                                        
041400       ELSE                                                               
041500                                                                          
041600* - NÄR HELA LA ANNULLERAS SKALL MAN ÄVEN TA BORT DEN FR ATTESTKÖN        
041700* - OBS! SKALL EJ GÄLLA USA/CAN,JAPAN DC61 OCH AUSTRALIEN DC62.           
041800          IF IDFTG-PV OR IDFTG-CN                                         
041900            PERFORM IMS-GHU-WDR501-4103                                   
042000            IF SEGMENT-FINNS                                              
042100              PERFORM IMS-DLET-WDR501-4103                                
042200            END-IF                                                        
042300          END-IF                                                          
042400                                                                          
042500          PERFORM IMS-GHU-KREE-ANM                                        
042600          MOVE '7'                          TO ANM-KDLEVANM               
042700          MOVE '0'                          TO ANM-KDLEVATT               
042800          PERFORM IMS-REPL-KREE-ANM                                       
042900          ADD +1                            TO CHKP-ANT                   
043000       END-IF                                                             
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 BB-UPPDAT-WDGX4103  SECTION.                                             
043500                                                                          
043600     MOVE ZERO TO WS-RADPRIS                                              
043700     MOVE ZERO TO WS-NYTT-RADPRIS                                         
043800                                                                          
043900     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
044000                       '84' OR '27' OR '28' OR '74'                       
044100       CONTINUE                                                           
044200     ELSE                                                                 
044300       MOVE LEV-KDANMORS TO OKOD-KDANMORS                                 
044400       CALL W418OKOD USING OKOD-W418OKOD                                  
044500                                                                          
044600       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
044700          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
044800          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
044900          LEV-KDANMORS = '97'                                             
045000                                                                          
045100         MOVE LEV-IDDC            TO W-IDDC-4104                          
045200                                                                          
045300         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
045400              LEV-KDANMORS = '97'                                         
045500           MOVE 'RP'  TO W-KDKRENOT-4104                                  
045600         ELSE                                                             
045700           MOVE 'CN'  TO W-KDKRENOT-4104                                  
045800         END-IF                                                           
045900                                                                          
046000         IF WDR501-FINNS                                                  
046100           PERFORM IMS-GHNP-WDGX4104                                      
046200           IF SEGMENT-FINNS                                               
046300             IF DIST79-DEALER-PRICE OR                                    
046320                DIST79-ECOM-PRICE                                         
046400               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
046500                      IN-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                 
046600             ELSE                                                         
046610*CHINA-PRICE1                                                             
046630               MOVE LEV-IDFTG          TO WS-IDFTG                        
046640               IF IDFTG-CN                                                
046650                 MOVE 'CNY'            TO 4104-KDVALISO                   
046660                 COMPUTE WS-NYTT-RADPRIS ROUNDED =                        
046670                         IN-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV           
046680               ELSE                                                       
046690                 COMPUTE WS-NYTT-RADPRIS ROUNDED =                        
046691                         IN-KVLEVANM-BEKR * LEV-PRARTBTO                  
046692               END-IF                                                     
046900             END-IF                                                       
047000                                                                          
047100*- TAG BORT DET GAMLA PRISET OCH BYT TILL NYA.WDGX4104 = KN-SUMMA.        
047200             IF DIST79-DEALER-PRICE OR                                    
047220                DIST79-ECOM-PRICE                                         
047300               COMPUTE WS-RADPRIS ROUNDED =                               
047400                       LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC               
047500             ELSE                                                         
047510*CHINA-PRICE2                                                             
047520               IF IDFTG-CN                                                
047530                 MOVE 'CNY'            TO 4104-KDVALISO                   
047540                 COMPUTE WS-RADPRIS ROUNDED =                             
047550                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
047560               ELSE                                                       
047570                 COMPUTE WS-RADPRIS ROUNDED =                             
047580                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO                 
047590               END-IF                                                     
047800             END-IF                                                       
047900                                                                          
048000             SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                       
048100             ADD WS-NYTT-RADPRIS   TO 4104-SUKRENOT                       
048200                                                                          
048300             PERFORM IMS-REPL-WDGX4104                                    
048400           END-IF                                                         
048500         END-IF                                                           
048600       END-IF                                                             
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000 BC-UPPDAT-WDGX4104     SECTION.                                          
049100                                                                          
049200     IF WDR501-FINNS                                                      
049300       PERFORM IMS-GHNP-WDGX4104-FIRST                                    
049400       PERFORM UNTIL SEGMENT-SAKNAS                                       
049500         IF 4104-SUKRENOT = ZERO                                          
049600           PERFORM IMS-DLET-WDGX4104                                      
049700         END-IF                                                           
049800         PERFORM IMS-GHNP-WDGX4104-OKVAL                                  
049900       END-PERFORM                                                        
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 BD-UPPDAT-WDGX4103-ANN  SECTION.                                         
050400                                                                          
050500     MOVE ZERO TO WS-RADPRIS                                              
050600                                                                          
050700     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
050800                       '84' OR '27' OR '28' OR '74'                       
050900       CONTINUE                                                           
051000     ELSE                                                                 
051100       MOVE LEV-KDANMORS TO OKOD-KDANMORS                                 
051200       CALL W418OKOD USING OKOD-W418OKOD                                  
051300                                                                          
051400       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
051500          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
051600          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
051700          LEV-KDANMORS = '97'                                             
051800                                                                          
051900         MOVE LEV-IDDC            TO W-IDDC-4104                          
052000                                                                          
052100         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
052200              LEV-KDANMORS = '97'                                         
052300           MOVE 'RP'  TO W-KDKRENOT-4104                                  
052400         ELSE                                                             
052500           MOVE 'CN'  TO W-KDKRENOT-4104                                  
052600         END-IF                                                           
052700                                                                          
052800         IF WDR501-FINNS                                                  
052900           PERFORM IMS-GHNP-WDGX4104                                      
053000           IF SEGMENT-FINNS                                               
053100             IF DIST79-DEALER-PRICE OR                                    
053120                DIST79-ECOM-PRICE                                         
053200               COMPUTE WS-RADPRIS ROUNDED =                               
053300                    SPAR-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                 
053400             ELSE                                                         
053410*CHINA-PRICE3                                                             
053430               MOVE LEV-IDFTG          TO WS-IDFTG                        
053440               IF IDFTG-CN                                                
053450                 MOVE 'CNY'            TO 4104-KDVALISO                   
053460                 COMPUTE WS-RADPRIS ROUNDED =                             
053470                        SPAR-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
053480               ELSE                                                       
053490                 COMPUTE WS-RADPRIS ROUNDED =                             
053491                        SPAR-KVLEVANM-BEKR * LEV-PRARTBTO                 
053492               END-IF                                                     
053700             END-IF                                                       
053800                                                                          
053900             SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                       
054000                                                                          
054100             PERFORM IMS-REPL-WDGX4104                                    
054200           END-IF                                                         
054300         END-IF                                                           
054400       END-IF                                                             
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054702 BE-KOLLA-RETURTERMINAL  SECTION.                                         
054703*  --- OM SEGMENT SAKNAS PÅ WDA3F SKALL POSTEN BEHANDLAS.                 
054704*  --- ANN ÄR INTE OK NÄR RETUREN ÄR MOTTAGEN PÅ RETURTERMINALEN.         
054705                                                                          
054706     IF IN-KVLEVANM-BEKR = +0 AND ANM-KDLEVANM = '4'                      
054707        MOVE ANM-IDDC-RET  TO W-WDA3-IDDC-MIN                             
054708                              W-WDA3-IDDC-MAX                             
054709        MOVE ANM-IDDISTR   TO W-WDA3-IDDISTR-MIN                          
054710                              W-WDA3-IDDISTR-MAX                          
054711        MOVE ANM-IDKUNDNR  TO W-WDA3-IDKUNDNR-MIN                         
054712                              W-WDA3-IDKUNDNR-MAX                         
054713        MOVE ANM-IDRAPPNR  TO W-WDA3-IDRAPPNR-MIN                         
054714                              W-WDA3-IDRAPPNR-MAX                         
054715        PERFORM IMS-GU-WDA3F1                                             
054716                                                                          
054717        IF SEGMENT-FINNS                                                  
054718          MOVE JA          TO RETUR-FINNS-PA-RETTERM-SW                   
054719        END-IF                                                            
054720     END-IF                                                               
054721     .                                                                    
054730     EJECT                                                                
054800 Z-FINIT SECTION.                                                         
054900                                                                          
055000                                                                          
055100     CLOSE W41811                                                         
055200     SKIP2                                                                
055300     MOVE 'S' TO POSTSUM-OPKOD                                            
055400     CALL POSTSUM USING POSTSUM-PARM                                      
055500     .                                                                    
055600     EJECT                                                                
055700 S01-LAES-W41811  SECTION.                                                
055800     SKIP2                                                                
055900     READ W41811 INTO IN-AREA                                             
056000     AT END                                                               
056100        SET END-OF-W41811               TO TRUE                           
056200                                                                          
056300     NOT AT END                                                           
056400        MOVE 'W41811'                   TO POSTSUM-FDNAMN                 
056500        MOVE 'W41815D1'                 TO POSTSUM-DDNAMN2                
056600        MOVE IN-IDPTYP                  TO POSTSUM-TRANSTYP               
056700        CALL POSTSUM USING POSTSUM-PARM                                   
056800                                                                          
056900        ADD 1                           TO W-W41811-KVPOST-IN             
057000     END-READ                                                             
057100     .                                                                    
057200     EJECT                                                                
057300 X-TAG-CHECKPOINT   SECTION.                                              
057400                                                                          
057500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
057600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
057700     PERFORM IMS-CHECKPOINT                                               
057800     MOVE ZERO                          TO CHKP-ANT                       
057900* --- LÄS OM DATABAS OM DET BEHÖVS                                        
058000     .                                                                    
058100     EJECT                                                                
058200* --- IMS SEKTIONER ---                                                   
058300     SKIP3                                                                
058400     EJECT                                                                
058500 IMS-GHU-KREE-LEV SECTION.                                                
058600                                                                          
058700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
058800          DELIMITED BY SIZE INTO SSA1                                     
058900     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
059000          DELIMITED BY SIZE INTO SSA2                                     
059100     MOVE '  ' TO GODK-STATUSKODER                                        
059200     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA-LEV SSA1 SSA2            
059300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     .                                                                    
059600     SKIP3                                                                
059700 IMS-GHU-KREE-ANM SECTION.                                                
059800                                                                          
059900     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
060000          DELIMITED BY SIZE INTO SSA1                                     
060100     MOVE '  ' TO GODK-STATUSKODER                                        
060200     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA-ANM SSA1                 
060300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
060400     PERFORM IMS-STATUSKONTROLL                                           
060500     .                                                                    
060600     SKIP3                                                                
060700 IMS-GU-KREE-ANM SECTION.                                                 
060800                                                                          
060900     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
061000          DELIMITED BY SIZE INTO SSA1                                     
061100     MOVE '  ' TO GODK-STATUSKODER                                        
061200     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-ANM SSA1                  
061300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
061400     PERFORM IMS-STATUSKONTROLL                                           
061500     .                                                                    
061600     SKIP3                                                                
061700 IMS-GNP-KREE-LEV SECTION.                                                
061800                                                                          
061900     MOVE 'WLKREE11 ' TO SSA1                                             
062000     MOVE '  GE' TO GODK-STATUSKODER                                      
062100     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-LEV SSA1                 
062200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062500     EJECT                                                                
062600 IMS-REPL-KREE-LEV SECTION.                                               
062700                                                                          
062800     MOVE '  ' TO GODK-STATUSKODER                                        
062900     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA-LEV                     
063000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     .                                                                    
063300     EJECT                                                                
063400 IMS-REPL-KREE-ANM SECTION.                                               
063500                                                                          
063600     MOVE '  ' TO GODK-STATUSKODER                                        
063700     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA-ANM                     
063800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     EJECT                                                                
064200 IMS-RESTART SECTION.                                                     
064300     SKIP2                                                                
064400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
064500     MOVE '  ' TO GODK-STATUSKODER                                        
064600     CALL CBLTDLI USING XRST MSG-PCB                                      
064700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
064800                        CHKP-AREA-LENGTH CHKP-AREA                        
064900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065000     PERFORM IMS-STATUSKONTROLL                                           
065100     .                                                                    
065200     EJECT                                                                
065300 IMS-CHECKPOINT SECTION.                                                  
065400     SKIP2                                                                
065500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
065600     MOVE '  XD' TO GODK-STATUSKODER                                      
065700     CALL CBLTDLI USING CHKP MSG-PCB                                      
065800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
065900                        CHKP-AREA-LENGTH CHKP-AREA                        
066000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066100     PERFORM IMS-STATUSKONTROLL                                           
066200                                                                          
066300     IF IMS-EJ-OK                                                         
066400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
066500       DISPLAY FELTEXT                                                    
066600       CALL FELLOG                                                        
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000 IMS-GHU-WDR501-4103 SECTION.                                             
067100                                                                          
067200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
067300          DELIMITED BY SIZE INTO SSA1                                     
067400     MOVE '  GE' TO GODK-STATUSKODER                                      
067500     CALL CBLTDLI USING GHU 4103-PCB DLI-IO-WDGX4103 SSA1                 
067600     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
067700     PERFORM IMS-STATUSKONTROLL                                           
067800     .                                                                    
067900     SKIP3                                                                
068000 IMS-DLET-WDR501-4103 SECTION.                                            
068100                                                                          
068200     MOVE '  ' TO GODK-STATUSKODER                                        
068300     CALL CBLTDLI USING DLET 4103-PCB DLI-IO-WDGX4103                     
068400     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
068500     PERFORM IMS-STATUSKONTROLL                                           
068600     .                                                                    
068700     EJECT                                                                
068800 IMS-GU-WDR501-4103 SECTION.                                              
068900                                                                          
069000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
069100          DELIMITED BY SIZE INTO SSA1                                     
069200     MOVE '  GE' TO GODK-STATUSKODER                                      
069300     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
069400     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
069500     PERFORM IMS-STATUSKONTROLL                                           
069600     .                                                                    
069700     SKIP3                                                                
069800 IMS-GHNP-WDGX4104-FIRST SECTION.                                         
069900                                                                          
070000     MOVE 'WDGX4104*F' TO SSA1                                            
070100     MOVE '  GE' TO GODK-STATUSKODER                                      
070200     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
070300     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
070400     PERFORM IMS-STATUSKONTROLL                                           
070500     .                                                                    
070600     EJECT                                                                
070700 IMS-GHNP-WDGX4104-OKVAL SECTION.                                         
070800                                                                          
070900     MOVE 'WDGX4104' TO SSA1                                              
071000     MOVE '  GE' TO GODK-STATUSKODER                                      
071100     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
071200     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
071300     PERFORM IMS-STATUSKONTROLL                                           
071400     .                                                                    
071500     EJECT                                                                
071600 IMS-GHNP-WDGX4104 SECTION.                                               
071700                                                                          
071800     STRING 'WDGX4104*F(KEY4104  =' W-KEY4104-X ')'                       
071900          DELIMITED BY SIZE INTO SSA1                                     
072000     MOVE '  GE' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
072200     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     SKIP3                                                                
072600 IMS-REPL-WDGX4104 SECTION.                                               
072700                                                                          
072800     MOVE 'WDGX4104' TO SSA1                                              
072900     MOVE '    ' TO GODK-STATUSKODER                                      
073000     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4104 SSA1                
073100     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
073200     PERFORM IMS-STATUSKONTROLL                                           
073300     .                                                                    
073400     EJECT                                                                
073500 IMS-DLET-WDGX4104 SECTION.                                               
073600                                                                          
073700     MOVE '    ' TO GODK-STATUSKODER                                      
073800     CALL CBLTDLI USING DLET 4103-PCB DLI-IO-WDGX4104 SSA1                
073900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
074000     PERFORM IMS-STATUSKONTROLL                                           
074100     .                                                                    
074200     EJECT                                                                
074300 IMS-GU-WDB601    SECTION.                                                
074400                                                                          
074500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
074600          DELIMITED BY SIZE INTO SSA1                                     
074700     MOVE '  ' TO GODK-STATUSKODER                                        
074800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
074900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
075000     PERFORM IMS-STATUSKONTROLL                                           
075100     .                                                                    
075200     EJECT                                                                
075210 IMS-GU-WDA3F1 SECTION.                                                   
075220                                                                          
075230     STRING 'WDA3F1  (WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
075240                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
075250          DELIMITED BY SIZE INTO SSA1                                     
075260     MOVE '  GE'           TO GODK-STATUSKODER                            
075270     CALL CBLTDLI USING GU WDA3-PCB DLI-IO-WDA3F1 SSA1                    
075280     MOVE WDA3-STATUS-CODE TO STATUS-WS                                   
075290     PERFORM IMS-STATUSKONTROLL                                           
075291     .                                                                    
075292     EJECT                                                                
075300 IMS-STATUSKONTROLL SECTION.                                              
075400     SKIP2                                                                
075500     SET STATUS-IX TO 1                                                   
075600     SEARCH GODK-STATUS                                                   
075700       AT END                                                             
075800         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
075900         DISPLAY FELTEXT                                                  
076000         CALL FELLOG                                                      
076100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
076200         CONTINUE                                                         
076300     END-SEARCH                                                           
076400     .                                                                    
