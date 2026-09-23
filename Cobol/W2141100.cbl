000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2141100.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   94/11/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    LARMRAPPORT                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        W2141100 LÄSER WDK7 MED SB.                                      
001200*                                                                         
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*        PROGRAMMET LÄSER      WLARTC (WDK7)                              
001600*        PROGRAMMET LÄSER      WLARTC (WDB6)                              
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- LARMPOSTER                                                 
002700     SELECT W21412                     ASSIGN TO W21411D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W21412                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST -COPY W21410 -PRE  W21412-  -L.                                 
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -COPY WY2000W2                                                       
004200     SKIP3                                                                
004300 77  IDPGM                       PIC X(8)    VALUE 'W2141100'.            
004400 77  PROGRAM-NAMN                PIC X(6)    VALUE 'W21411'.              
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  IX1-CN                      PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  IX1-US                      PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  IX2                         PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  WS-FLLARM-BUF               PIC X       VALUE 'N'.                   
005100 01  SW-ARTIKEL-OK               PIC X       VALUE SPACE.                 
005200     88  ARTIKEL-OK                          VALUE 'J'.                   
005300                                                                          
005400 01  W-TAB-CN-W21412.                                                     
005500     03  FILLER OCCURS 99.                                                
005600         05 W-TAB-CN-IDARTNR     PIC S9(09) COMP-3.                       
005700         05 W-TAB-CN-IDDC        PIC X(02).                               
005800         05 W-TAB-CN-IDLEVNR     PIC X(05).                               
005900         05 W-TAB-CN-FLLARM-BUF  PIC X.                                   
006000                                                                          
006100 01  W-TAB-US-W21412.                                                     
006200     03  FILLER OCCURS 99.                                                
006300         05 W-TAB-US-IDARTNR     PIC S9(09) COMP-3.                       
006400         05 W-TAB-US-IDDC        PIC X(02).                               
006500         05 W-TAB-US-IDLEVNR     PIC X(05).                               
006600         05 W-TAB-US-FLLARM-BUF  PIC X.                                   
006700                                                                          
006800 01  -COPY WWDCKONS.                                                      
006900                                                                          
007000******************************************************************        
007100*    ALLMÄNNA ARBETSAREOR                                                 
007200******************************************************************        
007300                                                                          
007400 01  FILLER                  PIC X(16)  VALUE 'WS-FALT        '.          
007500                                                                          
007600 01  WS-FALT.                                                             
007700     05  WS-LAGERTILLG       PIC S9(9)               COMP-3.              
007800     05  WS-DDATUM           PIC 9(5).                                    
007900     05  FILLER              REDEFINES WS-DDATUM.                         
008000         10  WS-DDATUMAAVV-X.                                             
008100             15  WS-DDATUMAA PIC 9(2).                                    
008200             15  WS-DDATUMVV PIC 9(2).                                    
008300         10  WS-DDATUMAAVV   REDEFINES WS-DDATUMAAVV-X                    
008400                             PIC 9(4).                                    
008500         10  WS-DDATUMD      PIC 9.                                       
008600     SKIP3                                                                
008700 01  WS-AARDEL               PIC 99.                                      
008800 01  FILLER  REDEFINES WS-AARDEL.                                         
008900     03  AARDEL1             PIC 9.                                       
009000     03  FILLER              PIC 9.                                       
009100     SKIP3                                                                
009200                                                                          
009300******************************************************************        
009400*    SWITCHAR.                                                            
009500******************************************************************        
009600                                                                          
009700 01  FILLER                  PIC X(16)  VALUE 'SWITCHAR       '.          
009800                                                                          
009900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010000 01  FILLER REDEFINES DAGENS-DATUM.                                       
010100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010400     EJECT                                                                
010500 01  DYNAMISKA-SUBPROGRAM.                                                
010600*                                                                         
010700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011100     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011400     SKIP2                                                                
011500 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
011600*   -COPY WZ20DAYS                                                        
011700     EJECT                                                                
011800                                                                          
011900*    --- PARAMETRAR TILL ABEND                                            
012000                                                                          
012100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012300     SKIP2                                                                
012400 01  FELTEXT.                                                             
012500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012700     EJECT                                                                
012800*    ----PARAMETRAR TILL DATKORT                                          
012900                                                                          
013000 01  FILLER                  PIC X(16)  VALUE 'DATUMKORT      '.          
013100 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
013200*01  -COPY WDATKORT                                                       
013300     EJECT                                                                
013400*01  -COPY WDATAREA                                                       
013500     EJECT                                                                
013600                                                                          
013700*    --- PARAMETRAR TILL POSTSUM                                          
013800*01  -COPY W0005   -PRE  POSTSUM-                                         
013900     EJECT                                                                
014000 01  W21412-AREA-START           PIC X(24)   VALUE                        
014100                                 'W21412-AREA-START  '.                   
014200     SKIP2                                                                
014300                                                                          
014400*01  AREA -COPY W21410     -PRE W21412-                                   
014500     EJECT                                                                
014600                                                                          
014700 01  FILLER                    PIC X(16) VALUE 'W-SLAG-WDK711 '.          
014800*01  -COPY WDK711 -PRE W-                                                 
014900     EJECT                                                                
015000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015100*                                                                         
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015400     SKIP3                                                                
015500 01  NYCKLAR-TILL-DLI.                                                    
015600     03  W-IDARTNR-X.                                                     
015700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015800     03  W-KDSEGKEY-X.                                                    
015900         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
016000     03  W-IDSKYLT-X.                                                     
016100        05 W-IDSKYLT            PIC X(3)     VALUE 'S  '.                 
016200     03  W-IDDC-X.                                                        
016300        05 W-IDDC               PIC X(2)     VALUE SPACE.                 
016400     EJECT                                                                
016500     SKIP2                                                                
016600*    --- STATUS-KOD FRÅN IMS                                              
016700 01  STATUS-WS                   PIC XX.                                  
016800     88  SEGMENT-FINNS                       VALUE '  '.                  
016900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(64).                               
017700 01  SSA2                        PIC X(64).                               
017800     EJECT                                                                
017900*    --- IMS FUNKTIONSKODER                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7'.                        
018400 01  DLI-IO-WDK7.                                                         
018500     03  IO-AREA    PIC X(500).                                           
018600     03  AREA-WDK701     REDEFINES IO-AREA.                               
018700*      05  -COPY WDK701                                                   
018800     SKIP3                                                                
018900     03  AREA-WDK711     REDEFINES IO-AREA.                               
019000*      05  -COPY WDK711                                                   
019100     SKIP3                                                                
019200     03  AREA-WDK722     REDEFINES IO-AREA.                               
019300*      05  -COPY WDK722                                                   
019400     SKIP3                                                                
019500     03  AREA-WDK712     REDEFINES IO-AREA.                               
019600*      05  -COPY WDK712                                                   
019700     EJECT                                                                
019800     SKIP2                                                                
019900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601 '.          
020000     SKIP3                                                                
020100 01  DLI-IO-WDK601.                                                       
020200*    03  -COPY WDK601                                                     
020300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611 '.          
020400     SKIP3                                                                
020500 01  DLI-IO-WDK611.                                                       
020600*    03  -COPY WDK611                                                     
020700     SKIP3                                                                
020800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB601 '.          
020900     SKIP3                                                                
021000 01  DLI-IO-WDB601.                                                       
021100*    03  -COPY WDB601                                                     
021200     SKIP3                                                                
021300                                                                          
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600                                                                          
021700     EJECT                                                                
021800*01  -COPY W0008  -PRE WDK7-                                              
021900     05  FILLER                  PIC X.                                   
022000*01  -COPY W0008  -PRE WDK6-                                              
022100     05  FILLER                  PIC X.                                   
022200*01  -COPY W0008  -PRE WDB6-                                              
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500 PROCEDURE DIVISION  USING WDK7-PCB WDK6-PCB WDB6-PCB.                    
022600 MAIN SECTION.                                                            
022700     ENTRY 'DLITCBL' USING WDK7-PCB WDK6-PCB WDB6-PCB.                    
022800                                                                          
022900                                                                          
023000     PERFORM A-INIT                                                       
023100                                                                          
023200     PERFORM IMS-GET-WDK7                                                 
023300                                                                          
023400     PERFORM UNTIL SEGMENT-SLUT                                           
023500       EVALUATE WDK7-SEG-NAME-FB                                          
023600         WHEN 'WDK701'                                                    
023700               MOVE SART-IDARTNR TO W-IDARTNR                             
023800               MOVE +0           TO IX1-CN IX1-US                         
023900         WHEN 'WDK711'                                                    
024000              PERFORM B-KOLL-K611-K711                                    
024100         WHEN 'WDK722'                                                    
024200              IF ARTIKEL-OK                                               
024300                 PERFORM C-KOLL-K722                                      
024400              END-IF                                                      
024500         WHEN 'WDK712'                                                    
024600              PERFORM D-KOLLA-DAPUBL-SKRIV-W21412                         
024700       END-EVALUATE                                                       
024800       PERFORM IMS-GET-WDK7                                               
024900     END-PERFORM                                                          
025000     PERFORM Z-FINIT                                                      
025100                                                                          
025200     MOVE ZERO TO RETURN-CODE                                             
025300     GOBACK                                                               
025400     .                                                                    
025500     EJECT                                                                
025600                                                                          
025700                                                                          
025800 A-INIT SECTION.                                                          
025900                                                                          
026000     OPEN OUTPUT W21412                                                   
026100                                                                          
026200     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
026300                                                                          
026400     MOVE D-AAR TO WS-DDATUMAA                                            
026500     MOVE D-VECKA TO WS-DDATUMVV                                          
026600     MOVE D-DAGNR TO WS-DDATUMD                                           
026700                                                                          
026800     MOVE D-AAR TO WS-AARDEL                                              
026900     .                                                                    
027000     EJECT                                                                
027100                                                                          
027200                                                                          
027300 B-KOLL-K611-K711   SECTION.                                              
027400                                                                          
027500     MOVE SLAG-WDK711 TO W-SLAG-WDK711                                    
027600                                                                          
027700     IF SLAG-IDDC NOT = DCS-IDDC                                          
027800        MOVE SLAG-IDDC TO W-IDDC                                          
027900        PERFORM IMS-GU-WDB601                                             
028000        IF SEGMENT-SAKNAS                                                 
028100           MOVE SPACE  TO DCS-IDDC                                        
028200                          DCS-KDDC                                        
028300        END-IF                                                            
028400     END-IF                                                               
028500                                                                          
028600     IF (DCS-NDC-CN OR (DCS-NDC-NA AND DCS-USA))                          
028700     AND SLAG-IDDC-REF = SPACE                                            
028800          MOVE JA TO SW-ARTIKEL-OK                                        
028900          PERFORM IMS-GU-WDK601                                           
029000          IF SEGMENT-FINNS AND                                            
029100             ART-KDERS-UTG = ZERO                                         
029200                                                                          
029300             PERFORM IMS-GNP-WDK611                                       
029400             IF  SEGMENT-FINNS                                            
029500             AND  CLAG-KDERS    = ZERO                                    
029600             AND (SLAG-KVPB-REF + SLAG-KVPBREOI) > ZERO                   
029700             AND  SLAG-KVUTRS   = ZERO                                    
029800                 CONTINUE                                                 
029900             ELSE                                                         
030000                MOVE NEJ TO SW-ARTIKEL-OK                                 
030100             END-IF                                                       
030200          ELSE                                                            
030300             MOVE NEJ    TO SW-ARTIKEL-OK                                 
030400          END-IF                                                          
030500     ELSE                                                                 
030600       MOVE NEJ          TO SW-ARTIKEL-OK                                 
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 C-KOLL-K722         SECTION.                                             
031100                                                                          
031200     MOVE NEJ                   TO WS-FLLARM-BUF                          
031300                                                                          
031400     IF (W-SLAG-KVPB-REF + W-SLAG-KVPBREOI) > ZERO                        
031500                                                                          
031600        COMPUTE WS-LAGERTILLG = W-SLAG-KVLS       +                       
031700                                W-SLAG-KVAKS-SDC  +                       
031800                                W-SLAG-KVAKS-PAV  +                       
031900                                W-SLAG-KVRESS     -                       
032000                                W-SLAG-KVROS-BULK -                       
032100                                W-SLAG-KVROS-DAG                          
032200        IF W-SLAG-KVUTRS = ZERO                                           
032300           IF XLAG-KVSLAGER = 0                                           
032400              MOVE NEJ          TO SW-ARTIKEL-OK                          
032500           ELSE                                                           
032600              IF  XLAG-KVSLAGER  > WS-LAGERTILLG                          
032700                  IF XLAG-FLLARM-BUF =  JA                                
032800                     MOVE NEJ   TO SW-ARTIKEL-OK                          
032900                  ELSE                                                    
033000                     MOVE JA    TO WS-FLLARM-BUF                          
033100                  END-IF                                                  
033200              ELSE                                                        
033300                 MOVE NEJ       TO WS-FLLARM-BUF                          
033400              END-IF                                                      
033500           END-IF                                                         
033600        END-IF                                                            
033700     ELSE                                                                 
033800        MOVE NEJ   TO SW-ARTIKEL-OK                                       
033900     END-IF                                                               
034000                                                                          
034100     IF ARTIKEL-OK                                                        
034200        IF DCS-NDC-CN                                                     
034300           ADD +1                TO IX1-CN                                
034400           MOVE W-IDARTNR        TO W-TAB-CN-IDARTNR    (IX1-CN)          
034500           MOVE W-SLAG-IDDC      TO W-TAB-CN-IDDC       (IX1-CN)          
034600           MOVE W-SLAG-IDLEVNR   TO W-TAB-CN-IDLEVNR    (IX1-CN)          
034700           MOVE WS-FLLARM-BUF    TO W-TAB-CN-FLLARM-BUF (IX1-CN)          
034800        ELSE                                                              
034900           IF DCS-NDC-NA AND DCS-USA                                      
035000             ADD +1              TO IX1-US                                
035100             MOVE W-IDARTNR      TO W-TAB-US-IDARTNR    (IX1-US)          
035200             MOVE W-SLAG-IDDC    TO W-TAB-US-IDDC       (IX1-US)          
035300             MOVE W-SLAG-IDLEVNR TO W-TAB-US-IDLEVNR    (IX1-US)          
035400             MOVE WS-FLLARM-BUF  TO W-TAB-US-FLLARM-BUF (IX1-US)          
035500           END-IF                                                         
035600        END-IF                                                            
035700     END-IF                                                               
035800                                                                          
035900     .                                                                    
036000     EJECT                                                                
036100 D-KOLLA-DAPUBL-SKRIV-W21412 SECTION.                                     
036200                                                                          
036300     IF  IX1-CN        > +0                                               
036400     AND LART-IDLANDX2 = 'CN'                                             
036500       IF LART-DAPUBL  > +0                                               
036600         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
036700         MOVE LART-DAPUBL(3:6)        TO DAT-I-TIDATUM                    
036800         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
036900                             DAT-O-TIDATUM DAT-KDSVAR                     
037000         IF DAT-KDSVAR-OK                                                 
037100           MOVE DAT-TIAAVVD           TO TMP1-YYWWD                       
037200         END-IF                                                           
037300       ELSE                                                               
037400         MOVE ART-TIFINLV             TO TMP1-YYWWD                       
037500       END-IF                                                             
037600       MOVE WS-DDATUM                 TO TMP2-YYWWD                       
037700       PERFORM WY2000P2                                                   
037800       PERFORM DA-CALL-DAPUBL-SKRIV-W21412                                
037900       IF TMP1-YYWWD < TMP2-YYWWD                                         
038000         MOVE +1                         TO IX2                           
038100         PERFORM UNTIL IX2 > IX1-CN                                       
038200           MOVE W-TAB-CN-IDARTNR   (IX2) TO W21412-LARM-IDARTNR           
038300           MOVE W-TAB-CN-IDDC      (IX2) TO W21412-LARM-IDDC              
038400           MOVE W-TAB-CN-IDLEVNR   (IX2) TO W21412-LARM-IDLEVNR           
038500           MOVE W-TAB-CN-FLLARM-BUF(IX2) TO W21412-LARM-FLLARM-BUF        
038600           PERFORM S12-SKRIV-W21412                                       
038700           ADD +1                        TO IX2                           
038800         END-PERFORM                                                      
038900       END-IF                                                             
039000     ELSE                                                                 
039100       IF  IX1-US        > +0                                             
039200       AND LART-IDLANDX2 = 'US'                                           
039300         IF LART-DAPUBL  > +0                                             
039400           MOVE 'AAMMDD'                TO DAT-KDDATFORM                  
039500           MOVE LART-DAPUBL(3:6)        TO DAT-I-TIDATUM                  
039600           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
039700                               DAT-O-TIDATUM DAT-KDSVAR                   
039800           IF DAT-KDSVAR-OK                                               
039900             MOVE DAT-TIAAVVD           TO TMP1-YYWWD                     
040000           END-IF                                                         
040100         ELSE                                                             
040200           MOVE ART-TIFINLV             TO TMP1-YYWWD                     
040300         END-IF                                                           
040400         MOVE WS-DDATUM                 TO TMP2-YYWWD                     
040500         PERFORM WY2000P2                                                 
040600         PERFORM DA-CALL-DAPUBL-SKRIV-W21412                              
040700         IF TMP1-YYWWD < TMP2-YYWWD                                       
040800           MOVE +1                      TO IX2                            
040900           PERFORM UNTIL IX2 > IX1-US                                     
041000             MOVE JA                    TO W21412-LARM-FLLARM-BUF         
041100             MOVE W-TAB-US-IDARTNR(IX2) TO W21412-LARM-IDARTNR            
041200             MOVE W-TAB-US-IDDC   (IX2) TO W21412-LARM-IDDC               
041300             MOVE W-TAB-US-IDLEVNR(IX2) TO W21412-LARM-IDLEVNR            
041400             MOVE W-TAB-US-FLLARM-BUF(IX2)                                
041500                                        TO W21412-LARM-FLLARM-BUF         
041600             PERFORM S12-SKRIV-W21412                                     
041700             ADD +1                     TO IX2                            
041800           END-PERFORM                                                    
041900         END-IF                                                           
042000       END-IF                                                             
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400                                                                          
042500 DA-CALL-DAPUBL-SKRIV-W21412 SECTION.                                     
042600                                                                          
042700     MOVE TMP1-YYWWD          TO DAYS-TIDATE1                             
042800                                                                          
042900     MOVE 'YYWWD'             TO DAYS-KDDATFMT1                           
043000     MOVE 'YYWWD'             TO DAYS-KDDATFMT2                           
043100     MOVE 7                   TO DAYS-KVDAYS                              
043200     MOVE SPACE               TO DAYS-TIDATE2                             
043300                                 DAYS-IDCALEND                            
043400     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
043500                                                                          
043600*                                                                         
043700     IF DAYS-KDRC = 8                                                     
043800       MOVE 'FEL VID ANROP TILL WZ20DAYS 5'                               
043900                                TO FELTEXT                                
044000       CALL FELLOG                                                        
044100     ELSE                                                                 
044200       MOVE DAYS-TIDATE2(1:5)     TO TMP1-YYWWD                           
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600                                                                          
044700******************************************************************        
044800*                                                                *        
044900*    AVSLUTNING                                                  *        
045000*    STÄNG FIL, SKRIV UT POSTSUMS RÄKNEVERK                      *        
045100*                                                                *        
045200******************************************************************        
045300                                                                          
045400 Z-FINIT SECTION.                                                         
045500     CLOSE W21412                                                         
045600                                                                          
045700     MOVE 'S' TO POSTSUM-OPKOD                                            
045800     CALL POSTSUM USING POSTSUM-PARM                                      
045900     .                                                                    
046000     EJECT                                                                
046100                                                                          
046200                                                                          
046300 S12-SKRIV-W21412 SECTION.                                                
046400                                                                          
046500     WRITE W21412-POST FROM W21412-AREA                                   
046600                                                                          
046700     MOVE 'W21411' TO POSTSUM-FDNAMN                                      
046800     MOVE 'W21411D2' TO POSTSUM-DDNAMN2                                   
046900     CALL POSTSUM USING POSTSUM-PARM                                      
047000     .                                                                    
047100     EJECT                                                                
047200                                                                          
047300                                                                          
047400******************************************************************        
047500*                                                                *        
047600*    BER LAGERTILLG                                              *        
047700*    BERÄKNA LAGERTILLGÅNG PER C-LAGER                           *        
047800*                                                                *        
047900******************************************************************        
048000                                                                          
048100                                                                          
048200* --- IMS SEKTIONER ---                                                   
048300                                                                          
048400                                                                          
048500 IMS-GET-WDK7   SECTION.                                                  
048600                                                                          
048700     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-WDK7                           
048800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
048900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
049000     PERFORM IMS-STATUSKONTROLL                                           
049100     .                                                                    
049200     EJECT                                                                
049300                                                                          
049400                                                                          
049500 IMS-GU-WDK601 SECTION.                                                   
049600                                                                          
049700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X  ')'                        
049800          DELIMITED BY SIZE INTO SSA1                                     
049900     MOVE '  GE' TO GODK-STATUSKODER                                      
050000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
050100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400                                                                          
050500                                                                          
050600 IMS-GNP-WDK611 SECTION.                                                  
050700                                                                          
050800     MOVE 'WDK611 '           TO SSA1                                     
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
051100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400                                                                          
051500                                                                          
051600 IMS-GU-WDB601 SECTION.                                                   
051700                                                                          
051800     STRING 'WDB601  (IDDC     =' W-IDDC-X  ')'                           
051900          DELIMITED BY SIZE INTO SSA1                                     
052000     MOVE '  GE' TO GODK-STATUSKODER                                      
052100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
052200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
052300     PERFORM IMS-STATUSKONTROLL                                           
052400     .                                                                    
052500     EJECT                                                                
052600                                                                          
052700                                                                          
052800 IMS-STATUSKONTROLL SECTION.                                              
052900                                                                          
053000     SET STATUS-IX TO 1                                                   
053100     SEARCH GODK-STATUS                                                   
053200       AT END                                                             
053300         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
053400         DISPLAY FELTEXT                                                  
053500         CALL FELLOG                                                      
053600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053700         CONTINUE                                                         
053800     END-SEARCH                                                           
053900     .                                                                    
054000     EJECT                                                                
054100*    -COPY WY2000P2                                                       
