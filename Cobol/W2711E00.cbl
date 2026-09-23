000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2711E00.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   210510.                                                  
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET BERÄKNAR NY PRE-PLANNED FAKTOR PÅ ALLA                
001100*        ARTIKLAR PÅ ALLA SDC                                             
001200*                                                                         
001300*        PROGRAMMET GÅR VID VECKOSLUT                                     
002800*                                                                         
002900*        PROGRAMMET LÄSER      WDK7                                       
003000*                              WDL7                                       
003100*                                                                         
003110*                                                                         
003200*    ABENDKODER:                                                          
003300*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
003400*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
003500*        U1000 -  . . . .                                                 
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP2                                                                
005000*                                                                         
005100     SELECT W2711E                     ASSIGN TO W2711ED1.                
005200*          --- UT-FILE WITH NEW PP-FACTOR                                 
005300                                                                          
006000 DATA DIVISION.                                                           
006100     SKIP3                                                                
006200 FILE SECTION.                                                            
006300     SKIP3                                                                
007900                                                                          
008000 FD  W2711E                                                               
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300                                                                          
008400*01  POST -COPY W2711E  -PRE UT-    -L.                                   
008500                                                                          
009900 WORKING-STORAGE SECTION.                                                 
010000     SKIP2                                                                
010900 77  IDPGM                       PIC X(8)    VALUE 'W2711E00'.            
011000 77  JA                          PIC X       VALUE 'J'.                   
011100 77  NEJ                         PIC X       VALUE 'N'.                   
011200 77  AKTIV                       PIC X       VALUE 'A'.                   
011300 77  DCS-TRAEFF                  PIC X       VALUE 'N'.                   
011400                                                                          
011500*    --- INDEX SAMT MAX-INDEX                                             
011600 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
011700 77  INDX                        PIC 9(2)    VALUE ZERO.                  
011800 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
011900 77  IX2                         PIC 9       VALUE ZERO.                  
011910 77  IX-TAB                      PIC 9(2)    VALUE ZERO.                  
012000 77  IX-DC                       PIC 9(3)    VALUE ZERO.                  
012100 77  DC-MAX                      PIC 9(3)    VALUE 200.                   
012200 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
012300 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
012400 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 12.                    
012500                                                                          
012600 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
012700                                                                          
012800 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
012900 77  MAX-TAB-RADIX               PIC 9(2)    VALUE ZERO.                  
013000                                                                          
013100 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
013200 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
013300                                                                          
013400*    --- SWITCHAR                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
013600                                                                          
016100 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
016200     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
016300                                                                          
016400 01  BEHANDLA-SW                 PIC X       VALUE 'N'.                   
016500     88  BEHANDLA                            VALUE 'J'.                   
016600                                                                          
017500*    --- ARBETSFÄLT                                                       
017600 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
017700 01  ARBETSFAELT.                                                         
018600                                                                          
019700     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
019800     03  FILLER REDEFINES DAGENS-TIAARP.                                  
019900         05 DAGENS-TIAA          PIC  9(2).                               
020000         05 DAGENS-TIRP          PIC  9(2).                               
020100                                                                          
021200     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
021300     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
021400         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
021500         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
021600         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
021700                                                                          
021800     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
021900                                                                          
021930     03  WS-FACTOR-1             PIC  S9V9(3) VALUE ZERO COMP-3.          
021931     03  WS-FACTOR-2             PIC  S9V9(3) VALUE ZERO COMP-3.          
021932     03  WS-QUOTA-PP             PIC  S9V9(2) VALUE ZERO COMP-3.          
021933     03  ALPHA                   PIC  S9V9(2) VALUE ZERO COMP-3.          
021940                                                                          
022000                                                                          
022100     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
022200     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
022300         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
022400         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
022500         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
022600                                                                          
022700     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
022800                                                                          
023100     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
023200     03  WS-KVVIPER              PIC  9(1)      VALUE ZERO.               
023300     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
023600     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
023700     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
023800     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
023900     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
024000     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
024100     03  WS-KVOI-TOT             PIC S9(11)V9(2)                          
024200                                                VALUE ZERO COMP-3.        
025200     03  WS-VECKA-I-AKT-PERIOD   PIC  9(2)      VALUE ZERO.               
025300     03  WS-FLREFILL             PIC  X         VALUE SPACE.              
025400     03  WS-KDREFSTA             PIC  X         VALUE SPACE.              
025500     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
025900*                                                                         
026500     EJECT                                                                
026600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
026700 01  FILLER REDEFINES DAGENS-DATUM.                                       
026800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
026900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
027000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
027100     EJECT                                                                
027200* ARBETSFÄLT FÖR ATT KONTROLLERA PB-LSÅNING, ESC-LÅS MM.                  
027300 01  DA-DAGENS-DATUM.                                                     
027400     03  DAGENS-DATUM-20         PIC 9(2) VALUE 20.                       
027500     03  DAGENS-DATUM-6LONG      PIC 9(6).                                
027600 01  WS-DAPUBL-US.                                                        
027700     03  WS-DAPUBL-US-YEAR       PIC 9(4).                                
027800     03  FILLER                  PIC 9(4).                                
027900 01  WS2-TIFINLV                 PIC 9(5).                                
028000 01  FILLER REDEFINES WS2-TIFINLV.                                        
028100     03  WS2-TIFINLV-AAVV        PIC 9(4).                                
028200     03  FILLER                  PIC 9(1).                                
028300 01  WS3-TIFINLV                 PIC 9(6).                                
028400     EJECT                                                                
028500                                                                          
029600 01  DYNAMISKA-SUBPROGRAM.                                                
029700*                                                                         
029800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
029900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
030000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
030100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
030200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
030300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
030500     SKIP2                                                                
030600*    --- PARAMETRAR TILL ABEND                                            
030700                                                                          
030800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
030900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
031000     SKIP2                                                                
031100 01  FELTEXT.                                                             
031200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
031300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
031400     EJECT                                                                
031500*    --- PARAMETRAR TILL DATKORT                                          
031600*                                                                         
031700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2711E'.              
031800     SKIP2                                                                
031900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
032000     SKIP2                                                                
032100*01  -COPY WDATKORT                                                       
032110                                                                          
032300*    --- PARAMETRAR TILL POSTSUM                                          
032400*                                                                         
032500*01  -COPY W0005   -PRE  POSTSUM-                                         
032600     EJECT                                                                
032700*    --- PARAMETRAR TILL WDATKONV                                         
032800*                                                                         
032900*01  -COPY WDATAREA                                                       
033000     EJECT                                                                
033100*01    -COPY WWDC99                                                       
033200     EJECT                                                                
043300 01  UT-AREA-START              PIC X(24)   VALUE                         
043400                                 'UT-AREA-START  '.                       
043500     SKIP2                                                                
043600                                                                          
043700*01  AREA -COPY W2711E     -PRE UT-                                       
043800     EJECT                                                                
045500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
045600                                                                          
045700     SKIP3                                                                
045800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
045900     SKIP3                                                                
046000 01  NYCKLAR-TILL-DLI.                                                    
046100     03  W-IDARTNR-X.                                                     
046200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
046300                                                                          
046400     03  W-IDDC-X.                                                        
046500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
046600                                                                          
047600     03  W-KDSEGKEY-X.                                                    
047700         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
047800                                                                          
048200     SKIP2                                                                
048300*    --- STATUS-KOD FRÅN IMS                                              
048400 01  STATUS-WS                   PIC XX.                                  
048500     88  SEGMENT-FINNS                       VALUE '  '.                  
048600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
048700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
048800     SKIP2                                                                
048900 01  GODK-STATUSKODER.                                                    
049000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
049100     SKIP3                                                                
049200 01  SSA1                        PIC X(64).                               
049300 01  SSA2                        PIC X(64).                               
049500     EJECT                                                                
049600*    --- IMS FUNKTIONSKODER                                               
049700*01  -COPY W0003                                                          
049800     EJECT                                                                
049900*    ---  DLI INPUT-OUTPUT AREA                                           
049910 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K7'.        
049920     SKIP3                                                                
049930 01  DLI-IO-AREA-K7.                                                      
049940     03  IO-AREA-K7              PIC X(300)  VALUE SPACE.                 
049950     SKIP3                                                                
049960     03  WLARTS01 REDEFINES IO-AREA-K7.                                   
049970*        05  -COPY WDK701                                                 
049980     SKIP3                                                                
049990     03  WLARTS11 REDEFINES IO-AREA-K7.                                   
049991*        05  -COPY WDK711                                                 
049992     EJECT                                                                
049993                                                                          
050000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
050100 01  DLI-IO-WDK601.                                                       
050200*    03  -COPY WDK601     -PRE WDK6-                                      
050300     EJECT                                                                
050400                                                                          
050500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
050600 01  DLI-IO-WDK611.                                                       
050700*    03  -COPY WDK611                                                     
050800     EJECT                                                                
050900                                                                          
053300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL711'.                      
053400 01  DLI-IO-WDL711.                                                       
053500*    03  -COPY WDL711                                                     
053600     EJECT                                                                
054800 LINKAGE SECTION.                                                         
054900                                                                          
055000     EJECT                                                                
055100*01  -COPY W0008  -PRE WDK7-                                              
055200     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
055300     EJECT                                                                
055700*01  -COPY W0008  -PRE WDK6-                                              
055800     05  FILLER                  PIC X.                                   
055900     EJECT                                                                
056000*01  -COPY W0008  -PRE WDL7-                                              
056100     05  FILLER                  PIC X.                                   
056200     EJECT                                                                
056600 PROCEDURE DIVISION  USING WDK7-PCB WDK6-PCB WDL7-PCB.                    
056700                                                                          
056800     ENTRY 'DLITCBL' USING WDK7-PCB WDK6-PCB WDL7-PCB.                    
056900                                                                          
057000     PERFORM A-INIT                                                       
057100     PERFORM IMS-GN-WDK7                                                  
057200     PERFORM UNTIL SEGMENT-SLUT                                           
057300       EVALUATE WDK7-SEG-NAME-FB                                          
057400         WHEN 'WDK701  '                                                  
057500           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
057600                                                                          
057700         WHEN 'WDK711  '                                                  
057800                                                                          
057900            MOVE SLAG-IDDC             TO W-IDDC                          
058100                                          WS-IDDC                         
058300            IF SDC OR LDC                                                 
058410                MOVE 0.15 TO ALPHA                                        
058420                IF  SLAG-KDREFSTA = 'A'                                   
058430                AND SLAG-RESEASON (DAGENS-TIRP) > 0.5                     
058500                  PERFORM IMS-GU-WDK611                                   
058510                  IF CLAG-PRARTSTD > 120                                  
058600                    PERFORM C-BEHANDLA-ARTIKEL                            
058601                  ELSE                                                    
058602                   IF SLAG-REPPFAKT > ZERO                                
058603                     COMPUTE WS-FACTOR-1 = ((1 - ALPHA) *                 
058604                                             SLAG-REPPFAKT)               
058605                     MOVE WS-FACTOR-1 TO UT-REPPFAKT                      
058606                     MOVE W-IDDC TO UT-IDDC                               
058607                     MOVE W-IDARTNR TO UT-IDARTNR                         
058608                     PERFORM S12-SKRIV-W2711E                             
058609                   END-IF                                                 
058610                  END-IF                                                  
058620                END-IF                                                    
063400            END-IF                                                        
063500                                                                          
063600       END-EVALUATE                                                       
063700       PERFORM IMS-GN-WDK7                                                
063800     END-PERFORM                                                          
063900                                                                          
064000     PERFORM Z-FINIT                                                      
064100                                                                          
064200     MOVE ZERO TO RETURN-CODE                                             
064300     GOBACK                                                               
064400     .                                                                    
064500     EJECT                                                                
064600                                                                          
076700 A-INIT SECTION.                                                          
076800                                                                          
077100     OPEN OUTPUT W2711E                                                   
077400                                                                          
078000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
078100     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
078200     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
078300     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
078400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
078500                                                                          
078600     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
078700     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
078800     MOVE DAGENS-DATUM TO DAGENS-DATUM-6LONG                              
078900                                                                          
079000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
079100                         DAT-O-TIDATUM DAT-KDSVAR                         
079200                                                                          
079210     MOVE ZERO TO IX2                                                     
079300     IF DAT-KDSVAR-OK                                                     
079400       MOVE DAT-TIAARP     TO DAGENS-TIAARP                               
079700       MOVE DAT-TIVV       TO DAGENS-TIVV                                 
079800       MOVE DAT-TIAAVV-GRP TO DAGENS-TIAAVV-GRP                           
079900       MOVE DAT-TIAAVVD    TO DAGENS-TIAAVVD                              
080000     ELSE                                                                 
080100       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
080200                         TO FELTEXT-STR                                   
080300       DISPLAY FELTEXT                                                    
080400       PERFORM S99-ABEND                                                  
080500     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
110200                                                                          
110300 C-BEHANDLA-ARTIKEL SECTION.                                              
110400                                                                          
110500     MOVE NEJ TO ARTIKEL-SW                                               
110600                                                                          
110700     PERFORM IMS-GU-WDL711                                                
110800     IF SEGMENT-FINNS                                                     
110900        IF SLAG-FLREFILL = JA                                             
110901          IF IX2 = ZERO                                                   
110910            PERFORM CA-CHECK-WEEK                                         
110920          END-IF                                                          
111000          IF  DC-KVOI-INNEV (IX2) > ZERO                                  
111010          OR DC-KVOI-CDC-INNEV (IX2) > ZERO                               
111020          OR DC-KVOI-PP-INNEV (IX2) > ZERO                                
111100            IF DC-KVOI-PP-INNEV (IX2) > ZERO                              
111101              IF DC-KVOI-PP-INNEV (IX2)  NEGATIVE                         
111102              OR DC-KVOI-CDC-INNEV (IX2) NEGATIVE                         
111103              OR DC-KVOI-INNEV (IX2)     NEGATIVE                         
111104                MOVE ZERO TO WS-FACTOR-2                                  
111105              ELSE                                                        
111110                COMPUTE WS-QUOTA-PP =                                     
111111                        DC-KVOI-PP-INNEV (IX2) /                          
111120                        (DC-KVOI-INNEV (IX2)                              
111130                       + DC-KVOI-CDC-INNEV (IX2)                          
111140                       + DC-KVOI-PP-INNEV (IX2))                          
111200                COMPUTE WS-FACTOR-2 = ALPHA * WS-QUOTA-PP                 
111201              END-IF                                                      
111202            ELSE                                                          
111203              MOVE ZERO TO WS-FACTOR-2                                    
111204            END-IF                                                        
111210            COMPUTE WS-FACTOR-1 = ((1 - ALPHA) * SLAG-REPPFAKT)           
111300            COMPUTE UT-REPPFAKT = WS-FACTOR-1 + WS-FACTOR-2               
111400            MOVE W-IDDC    TO UT-IDDC                                     
111500            MOVE W-IDARTNR TO UT-IDARTNR                                  
111510            PERFORM S12-SKRIV-W2711E                                      
111600          END-IF                                                          
119900        END-IF                                                            
119910     ELSE                                                                 
119911       COMPUTE WS-FACTOR-1 = ((1 - ALPHA) * SLAG-REPPFAKT)                
119912       MOVE WS-FACTOR-1    TO UT-REPPFAKT                                 
119913       MOVE W-IDDC         TO UT-IDDC                                     
119914       MOVE W-IDARTNR      TO UT-IDARTNR                                  
119915       PERFORM S12-SKRIV-W2711E                                           
120000     END-IF                                                               
139100     .                                                                    
139200     EJECT                                                                
139300                                                                          
139310 CA-CHECK-WEEK SECTION.                                                   
139400                                                                          
139410     MOVE +0 TO IX2                                                       
139420                                                                          
139421     IF DAGENS-TIVV = DC-TIVV (1)                                         
139422       MOVE +1 TO IX2                                                     
139423     ELSE                                                                 
139424       IF DAGENS-TIVV = DC-TIVV (2)                                       
139425         MOVE +2 TO IX2                                                   
139426       ELSE                                                               
139427         IF DAGENS-TIVV = DC-TIVV (3)                                     
139428           MOVE +3 TO IX2                                                 
139429         ELSE                                                             
139430           IF DAGENS-TIVV = DC-TIVV (4)                                   
139431             MOVE +4 TO IX2                                               
139432           ELSE                                                           
139433             IF DAGENS-TIVV = DC-TIVV (5)                                 
139434               MOVE +5 TO IX2                                             
139435             END-IF                                                       
139436           END-IF                                                         
139437         END-IF                                                           
139438       END-IF                                                             
139439     END-IF                                                               
139440     .                                                                    
139450     EJECT                                                                
139500 Z-FINIT SECTION.                                                         
139600                                                                          
139700     CLOSE W2711E                                                         
140200                                                                          
140300     MOVE 'S' TO POSTSUM-OPKOD                                            
140400     CALL POSTSUM USING POSTSUM-PARM                                      
140500     .                                                                    
140600     EJECT                                                                
207700 S12-SKRIV-W2711E SECTION.                                                
207800                                                                          
207900     WRITE UT-POST FROM UT-AREA                                           
208000                                                                          
208100     MOVE 'W2711E'   TO POSTSUM-FDNAMN                                    
208200     MOVE 'W2711ED3' TO POSTSUM-DDNAMN2                                   
208300     CALL POSTSUM USING POSTSUM-PARM                                      
208400     .                                                                    
208500     SKIP3                                                                
208600                                                                          
211200 S99-ABEND SECTION.                                                       
211300                                                                          
211400     MOVE 'S' TO POSTSUM-OPKOD                                            
211500     CALL POSTSUM USING POSTSUM-PARM                                      
211600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
211700     .                                                                    
211800     EJECT                                                                
211900* --- IMS SEKTIONER ---                                                   
212000     SKIP3                                                                
212100     EJECT                                                                
212200 IMS-GU-WDK611 SECTION.                                                   
212300                                                                          
212400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
212500          DELIMITED BY SIZE INTO SSA1                                     
212510     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
212520          DELIMITED BY SIZE INTO SSA2                                     
212600     MOVE '  ' TO GODK-STATUSKODER                                        
212700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
212800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
212900     PERFORM IMS-STATUSKONTROLL                                           
213000     .                                                                    
213100     EJECT                                                                
214200 IMS-GN-WDK7 SECTION.                                                     
214300                                                                          
214400     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
214500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
214600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
214700     PERFORM IMS-STATUSKONTROLL                                           
214800     .                                                                    
214900     EJECT                                                                
217600 IMS-GU-WDL711 SECTION.                                                   
217700                                                                          
217800     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
217900          DELIMITED BY SIZE INTO SSA1                                     
218000     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
218100          DELIMITED BY SIZE INTO SSA2                                     
218200     MOVE '  GE' TO GODK-STATUSKODER                                      
218300     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
218400     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
218500     PERFORM IMS-STATUSKONTROLL                                           
218600     .                                                                    
218700     EJECT                                                                
218800                                                                          
221800 IMS-STATUSKONTROLL SECTION.                                              
221900                                                                          
222000     SET STATUS-IX TO 1                                                   
222100     SEARCH GODK-STATUS                                                   
222200       AT END                                                             
222300         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
222400           DELIMITED BY SIZE INTO FELTEXT-STR                             
222500         DISPLAY FELTEXT                                                  
222600         CALL FELLOG                                                      
222700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
222800         CONTINUE                                                         
222900     END-SEARCH                                                           
223000     .                                                                    
