000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2210400.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   95/10/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSAR GAMLA LARM 200/210/221/222                                
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLXXBU (WDR5)                              
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
001500*                                                                         
001600*    ÄNDRINGAR:                                                           
001700*        2005-02-18 UTÖKAT MED KONTROLL AV LEVERANSBESKED INNAN           
001800*                   LARM 221 TAS BORT /L.A.                               
001900*        2001-11-20 UTÖKAT MED BEHANDLING AV LARM 222 /C.E.               
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200*          --- LOG FILE FOR ALARMS DELETED FROM WDR5                      
003300     SELECT W22104               ASSIGN W22104D1.                         
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800                                                                          
003900 FD  W22104                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200*01  POST -COPY W214ALOG -PRE  UT1-     -L.                               
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004600*    -COPY WY2000W1                                                       
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W2210400'.            
004900 01  CHKP-VAR.                                                            
005000 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005500 03  CHKP-MAX                    PIC S9(3)   VALUE +5.                    
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 01  RKOD                        PIC S9(4)        COMP SYNC.              
005900 01  WS-IDANSK                   PIC S9(3)   COMP-3 VALUE ZERO.           
006000 01  WS-TISENBEK                 PIC X(7)    VALUE SPACE.                 
006100 01  WS-KDLARM                   PIC 9(3)    VALUE ZERO.                  
006200 01  WS-LAGERTILLG               PIC S9(9)   COMP-3.                      
006300 01  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
006400                                                                          
006500     SKIP2                                                                
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006900     SKIP3                                                                
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500 01  DAGENS-DATUM-MINUS-2VV      PIC 9(6)    VALUE ZERO.                  
007600 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
007700 01  WS-ALARM-AAVV               PIC 9(4)    VALUE ZERO.                  
007800                                                                          
007900*    -COPY WWDCKONS                                                       
008000                                                                          
008100     SKIP3                                                                
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
008900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009000*                                                                         
009100     EJECT                                                                
009200*--------------------------------------- PARAMETRAR TILL POSTSUM          
009300                                                                          
009400*01  -COPY W0005      -PRE POSTSUM-                                       
009500     EJECT                                                                
009600*01  -COPY WORKAREA                                                       
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL WDATKONV                                         
009900*01 -COPY WDATAREA                                                        
010000     EJECT                                                                
010100 01  UT-AREA-START               PIC X(16)   VALUE                        
010200                                             'UT-AREA-START'.             
010300*01  AREA -COPY W214ALOG    -PRE UT1-                                     
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700***************************************************************           
010800*            N Y C K L A R   T I L L   D L I                              
010900***************************************************************           
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-IDARTNR-X.                                                     
011200         05  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.              
011300     03  W-WDD901KY-X.                                                    
011400         05  W-IDARTNR-D9    PIC S9(9)   VALUE ZERO  COMP-3.              
011500         05  W-IDDC-D9       PIC X(2)    VALUE SPACE.                     
011600     03  W-IDLEVNR-X.                                                     
011700         05  W-IDLEVNR       PIC X(5)    VALUE SPACE.                     
011800     03  W-DALEVBSK-X.                                                    
011900         05  W-DALEVBSK      PIC  9(8)   VALUE ZERO.                      
012000     03  W-WDGXKEY-ROT-X.                                                 
012100         05  W-WDGXKEY-IDHTYP      PIC X(04)    VALUE '2223'.             
012200         05  W-WDGXKEY-IDANSK      PIC S9(3)    COMP-3 VALUE ZERO.        
012300         05  W-WDGXKEY-LW          PIC X(24)    VALUE LOW-VALUE.          
012400     03  W-WDGXKEY-X.                                                     
012500         05  W-WDGXKEY-TISENBEK  PIC X(7)    VALUE SPACE.                 
012600         05  W-WDGXKEY-KDLARM    PIC S9(3)   COMP-3 VALUE ZERO.           
012700     03  W-KDLARM-MAX.                                                    
012800         05  W-WDGXKEY-KDLARM-MAX  PIC S9(3)   COMP-3 VALUE 222.          
012900     03  W-KDLARM-MIN.                                                    
013000         05  W-WDGXKEY-KDLARM-MIN  PIC S9(3)   COMP-3 VALUE 110.          
013100     03  W-KDSEGKEY-X.                                                    
013200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013300     03  W-IDHTYP-X.                                                      
013400         05  W-IDHTYP            PIC X(4)    VALUE '2223'.                
013500     03  W-IDDC-X.                                                        
013600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013700     03  W-IDDC-CDC-SE-X.                                                 
013800         05  W-IDDC-CDC-SE       PIC X(2)    VALUE SPACE.                 
013900     SKIP2                                                                
014000*    --- STATUS-KOD FRÅN IMS                                              
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FINNS                       VALUE '  '.                  
014300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014600     88  IMS-EJ-OK                           VALUE 'XD'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(128).                              
015200 01  SSA2                        PIC X(64).                               
015300 01  SSA3                        PIC X(64).                               
015400     EJECT                                                                
015500*    --- IMS FUNKTIONSKODER                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016000     SKIP3                                                                
016100 01  DLI-IO-AREA.                                                         
016200     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
016300     SKIP3                                                                
016400     03  WLXXBU01 REDEFINES IO-AREA.                                      
016500*        05  -COPY WDGX2223  -PRE XXBU-                                   
016600     EJECT                                                                
016700     03  WLXXBU11 REDEFINES IO-AREA.                                      
016800*        05  -COPY WDGX2224  -PRE XXBU-                                   
016900     EJECT                                                                
017000*    ---  DLI INPUT-OUTPUT AREA                                           
017100 01  FILLER              PIC X(16)   VALUE 'DLI-IO-AREA-wdd9'.            
017200     SKIP3                                                                
017300 01      DLI-IO-AREA-WDD9.                                                
017400     03  IO-AREA-WDD9    PIC X(200)  VALUE SPACE.                         
017500     SKIP2                                                                
017600*    03  WLINLB01 -COPY WDD901                -RED IO-AREA-WDD9.          
017700     EJECT                                                                
017800*    03  WLINLB11 -COPY WDD902                -RED IO-AREA-WDD9.          
017900     EJECT                                                                
018000*    03  WLINLB24 -COPY WDD924                -RED IO-AREA-WDD9.          
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)                                
018300                             VALUE 'DLI-IO-AREA-2'.                       
018400     SKIP3                                                                
018500 01  DLI-IO-AREA-2.                                                       
018600     03  IO-AREA-2               PIC X(900)  VALUE SPACE.                 
018700                                                                          
018800     03  WLARTC01 REDEFINES IO-AREA-2.                                    
018900*        05  -COPY WDK601                                                 
019000     EJECT                                                                
019100     03  WLARTC11 REDEFINES IO-AREA-2.                                    
019200*        05  -COPY WDK611                                                 
019300     EJECT                                                                
019400 01  FILLER              PIC X(16)   VALUE 'DLI-IO-AREA-WDB6'.            
019500 01  DLI-IO-AREA-WDB601.                                                  
019600*    03  -COPY WDB601                                                     
019700     SKIP2                                                                
019800 LINKAGE SECTION.                                                         
019900                                                                          
020000*01  -COPY W0008  -PRE WDD9-                                              
020100     05  KFBA-IDARTNR            PIC S9(9) COMP-3.                        
020200     05  KFBA-IDLEVNR            PIC X(5).                                
020300     EJECT                                                                
020400*01  -COPY W0009  -PRE MSG-                                               
020500     EJECT                                                                
020600*01  -COPY W0008  -PRE XXBU-                                              
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900*01  -COPY W0008  -PRE ARTC-                                              
021000     05  FILLER                  PIC X.                                   
021100     EJECT                                                                
021200*01  -COPY W0008  -PRE WDB6-                                              
021300     05  FILLER                  PIC X.                                   
021400     EJECT                                                                
021500 PROCEDURE DIVISION  USING MSG-PCB XXBU-PCB ARTC-PCB WDD9-PCB             
021600                                   WDB6-PCB.                              
021700 MAIN SECTION.                                                            
021800     ENTRY 'DLITCBL' USING MSG-PCB XXBU-PCB ARTC-PCB WDD9-PCB             
021900                                   WDB6-PCB.                              
022000                                                                          
022100     SKIP2                                                                
022200     PERFORM A-INIT                                                       
022300                                                                          
022400     PERFORM IMS-GET-XXBU-ROT                                             
022500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
022600       IF CHKP-ANT > CHKP-MAX                                             
022700         PERFORM X-TAG-CHECKPOINT                                         
022800       END-IF                                                             
022900                                                                          
023000        MOVE WC-CDC-SE            TO W-IDDC-CDC-SE                        
023100        PERFORM IMS-GET-XXBU-LARM                                         
023200        PERFORM UNTIL SEGMENT-SAKNAS                                      
023300           MOVE XXBU-2224-IDARTNR TO W-IDARTNR                            
023400           IF XXBU-2224-IDDC NOT = DCS-IDDC                               
023500              MOVE XXBU-2224-IDDC TO W-IDDC                               
023600              PERFORM IMS-GU-WDB601                                       
023700              IF SEGMENT-SAKNAS                                           
023800                 MOVE SPACE       TO DCS-IDDC                             
023900                                     DCS-KDDC                             
024000              END-IF                                                      
024100           END-IF                                                         
024200                                                                          
024300           IF DCS-CDC                                                     
024400              PERFORM IMS-GET-ARTC-CLAG                                   
024500              IF SEGMENT-FINNS                                            
024600                 IF XXBU-2224-KDLARM = 200                                
024700                    IF CLAG-KVUTRS = ZERO                                 
024800                       PERFORM IMS-DLET-XXBU                              
024900                       PERFORM G-PREPARE-ALARM-LOG                        
025000                    END-IF                                                
025100                 END-IF                                                   
025200                 IF XXBU-2224-KDLARM = 210                                
025300                    IF CLAG-KVROS  = ZERO                                 
025400                       PERFORM IMS-DLET-XXBU                              
025500                       PERFORM G-PREPARE-ALARM-LOG                        
025600                    END-IF                                                
025700                 END-IF                                                   
025800                 IF XXBU-2224-KDLARM = 222                                
025900*                -- LARM = "SÄK.LAGER UNDERSKRIDET"                       
026000*                -- BERÄKNA LAGERTILLGÅNG                                 
026100                    COMPUTE WS-LAGERTILLG = CLAG-KVLS                     
026200                        + CLAG-KVAKS-CDC + CLAG-KVAKS-PAV                 
026300                        + CLAG-KVAKS-T   - CLAG-KVRESS                    
026400                        - CLAG-KVROS                                      
026500                    IF CLAG-KVSLAGER <= WS-LAGERTILLG                     
026600                       PERFORM IMS-DLET-XXBU                              
026700                       PERFORM G-PREPARE-ALARM-LOG                        
026800                    END-IF                                                
026900                 END-IF                                                   
027000                 IF XXBU-2224-KDLARM = 110                                
027100                    MOVE XXBU-2224-TISENBEK-DAG TO TMP1-YYMMDD            
027200                    MOVE DAGENS-DATUM           TO TMP2-YYMMDD            
027300                    PERFORM WY2000P1                                      
027400                    IF TMP1-YYMMDD < TMP2-YYMMDD AND                      
027500                       TMP1-YYMMDD < 999999                               
027600                       PERFORM IMS-DLET-XXBU                              
027700                       PERFORM G-PREPARE-ALARM-LOG                        
027800                    END-IF                                                
027900                 END-IF                                                   
028000                 IF XXBU-2224-KDLARM = 221                                
028100                    MOVE XXBU-2224-IDARTNR TO W-IDARTNR                   
028200                    MOVE W-IDARTNR         TO W-IDARTNR-D9                
028300                    MOVE WC-CDC-SE         TO W-IDDC-D9                   
028400                    MOVE XXBU-2224-IDLEVNR TO W-IDLEVNR                   
028500*                -- KOLLA LEVERANSBESKED                                  
028600                    PERFORM IMS-GU-WDD924                                 
028700                    IF SEGMENT-SAKNAS                                     
028800                       PERFORM IMS-DLET-XXBU                              
028900                       PERFORM G-PREPARE-ALARM-LOG                        
029000                    ELSE                                                  
029100                       PERFORM F-KOLL-WDD924                              
029200                       IF (WORK-TIAAMMDD-TOM > DAGENS-DATUM) OR           
029300                          (XXBU-2224-TIREGDAT <                           
029400                                DAGENS-DATUM-MINUS-2VV)                   
029500                          PERFORM IMS-DLET-XXBU                           
029600                          PERFORM G-PREPARE-ALARM-LOG                     
029700                       END-IF                                             
029800                    END-IF                                                
029900                 END-IF                                                   
030000              ELSE                                                        
030100                  PERFORM IMS-DLET-XXBU                                   
030200              END-IF                                                      
030300           END-IF                                                         
030400           PERFORM IMS-GET-XXBU-LARM                                      
030500        END-PERFORM                                                       
030600        PERFORM IMS-GET-XXBU-ROT                                          
030700     END-PERFORM                                                          
030800                                                                          
030900                                                                          
031000     PERFORM Z-FINIT                                                      
031100                                                                          
031200     MOVE ZERO TO RETURN-CODE                                             
031300     GOBACK                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 A-INIT SECTION.                                                          
031700     SKIP2                                                                
031800     OPEN OUTPUT W22104                                                   
031900*                                                                         
032000     MOVE IDPGM          TO POSTSUM-PROGNAMN                              
032100*                                                                         
032200     ACCEPT DAGENS-DATUM FROM DATE                                        
032300                                                                          
032400     MOVE DAGENS-DATUM   TO WORK-TIAAMMDD-TOM                             
032500     MOVE 3              TO WORK-KDCALL                                   
032600     MOVE 11             TO WORK-KVWORKD                                  
032700     MOVE WC-CDC-SE      TO WORK-IDDC                                     
032800                                                                          
032900     CALL WORKDAY USING                                                   
033000          WORK-KDCALL                                                     
033100          WORK-DATE-AREA                                                  
033200          WORK-KDSVAR                                                     
033300                                                                          
033400     IF WORK-KDSVAR-OK                                                    
033500        MOVE WORK-TIAAMMDD-FOM TO DAGENS-DATUM-MINUS-2VV                  
033600        DISPLAY ' DAGENS DATUM - 2VV ' DAGENS-DATUM-MINUS-2VV             
033700     ELSE                                                                 
033800       DISPLAY '*** W22104, FEL I WORKDAY '                               
033900       MOVE +25 TO RKOD                                                   
034000       CALL ABEND USING RKOD                                              
034100     END-IF                                                               
034200*                                                                         
034300     MOVE   DAGENS-DATUM       TO DAT-I-TIDATUM                           
034400     PERFORM S10-CONVERT-DATE-FMT                                         
034500     MOVE DAT-TIAAVV-GRP       TO DAGENS-AAVV                             
034600                                                                          
034700     PERFORM IMS-RESTART                                                  
034800                                                                          
034900     MOVE 'W22104'           TO POSTSUM-PROGNAMN                          
035000                                                                          
035100     .                                                                    
035200     EJECT                                                                
035300 F-KOLL-WDD924 SECTION.                                                   
035400                                                                          
035500     IF LEV-KVAVIS-BSKKVAR > 0                                            
035600       MOVE 2                     TO WORK-KDCALL                          
035700       MOVE WC-CDC-SE             TO WORK-IDDC                            
035800       MOVE LEV-TILEVBSK-INL      TO WORK-TIAAMMDD-FOM                    
035900       MOVE +3                    TO WORK-KVWORKD                         
036000       CALL WORKDAY USING WORK-KDCALL                                     
036100                 WORK-DATE-AREA WORK-KDSVAR                               
036200       IF WORK-KDSVAR-OK                                                  
036300          CONTINUE                                                        
036400       ELSE                                                               
036500          MOVE +24 TO RKOD                                                
036600          CALL ABEND USING RKOD                                           
036700       END-IF                                                             
036800                                                                          
036900     END-IF                                                               
037000     .                                                                    
037100     EJECT                                                                
037200 G-PREPARE-ALARM-LOG SECTION.                                             
037300                                                                          
037400     MOVE XXBU-2224-TIREGDAT                                              
037500                            TO DAT-I-TIDATUM                              
037600     PERFORM S10-CONVERT-DATE-FMT                                         
037700     IF DAT-KDSVAR = ' '                                                  
037800        MOVE DAT-TIAAVV-GRP TO WS-ALARM-AAVV                              
037900        IF WS-ALARM-AAVV  = DAGENS-AAVV                                   
038000           PERFORM GA-LOG-ALARM                                           
038100        END-IF                                                            
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500 GA-LOG-ALARM SECTION.                                                    
038600*                                                                         
038700     MOVE XXBU-2224-TISENBEK-DAG                                          
038800                            TO DAT-I-TIDATUM                              
038900     PERFORM S10-CONVERT-DATE-FMT                                         
039000     IF DAT-KDSVAR = ' '                                                  
039100        MOVE DAT-TIAAVVD    TO UT1-ALOG-TIAAVVD                           
039200     ELSE                                                                 
039300        MOVE ZERO           TO UT1-ALOG-TIAAVVD                           
039400     END-IF                                                               
039500*                                                                         
039600     MOVE 'WDR5'            TO UT1-ALOG-IDSYSTEM                          
039700     MOVE XXBU-2224-KDLARM  TO UT1-ALOG-KDLARM                            
039800     MOVE XXBU-2224-IDARTNR TO UT1-ALOG-IDARTNR                           
039900     MOVE WS-IDANSK         TO UT1-ALOG-IDANSK                            
040000     MOVE XXBU-2224-IDLEVNR TO UT1-ALOG-IDLEVNR                           
040100     MOVE XXBU-2224-IDDISTR TO UT1-ALOG-IDDISTR                           
040200     MOVE XXBU-2224-TIREGDAT                                              
040300                            TO UT1-ALOG-TIREGDAT                          
040400     MOVE ZERO              TO UT1-ALOG-TIPLANDAT                         
040500                               UT1-ALOG-KVAVIS                            
040600                               UT1-ALOG-KVAVROP                           
040700*                                                                         
040800     PERFORM S01-SKRIV-W22104                                             
040900     .                                                                    
041000     EJECT                                                                
041100     .                                                                    
041200     EJECT                                                                
041300 Z-FINIT SECTION.                                                         
041400                                                                          
041500     CLOSE W22104                                                         
041600                                                                          
041700     MOVE 'S'                TO POSTSUM-OPKOD                             
041800     CALL POSTSUM USING POSTSUM-PARM                                      
041900     .                                                                    
042000     EJECT                                                                
042100 S01-SKRIV-W22104 SECTION.                                                
042200                                                                          
042300     WRITE UT1-POST FROM UT1-AREA                                         
042400                                                                          
042500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
042600     MOVE 'W22104'   TO POSTSUM-FDNAMN                                    
042700     MOVE 'W22104D1' TO POSTSUM-DDNAMN2                                   
042800     CALL POSTSUM USING POSTSUM-PARM                                      
042900     .                                                                    
043000     SKIP3                                                                
043100 S10-CONVERT-DATE-FMT SECTION.                                            
043200                                                                          
043300     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
043400     CALL WDATKONV       USING DAT-KDDATFORM                              
043500                               DAT-I-TIDATUM                              
043600                               DAT-O-TIDATUM                              
043700                               DAT-KDSVAR                                 
043800     .                                                                    
043900     EJECT                                                                
044000                                                                          
044100 X-TAG-CHECKPOINT   SECTION.                                              
044200                                                                          
044300* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
044400                                                                          
044500     PERFORM IMS-CHECKPOINT                                               
044600     MOVE ZERO TO CHKP-ANT                                                
044700* --- LÄS OM DATABAS                                                      
044800     MOVE WS-IDANSK TO W-WDGXKEY-IDANSK                                   
044900     PERFORM IMS-GU-XXBU-ROT                                              
045000     .                                                                    
045100     EJECT                                                                
045200* --- IMS SEKTIONER ---                                                   
045300     SKIP3                                                                
045400 IMS-GET-XXBU-ROT SECTION.                                                
045500                                                                          
045600     STRING 'WLXXBU01(IDHTYP   =' W-IDHTYP-X ')  '                        
045700          DELIMITED BY SIZE INTO SSA1                                     
045800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
045900     CALL CBLTDLI USING GN XXBU-PCB DLI-IO-AREA SSA1                      
046000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
046100     PERFORM IMS-STATUSKONTROLL                                           
046200                                                                          
046300     IF SEGMENT-FINNS                                                     
046400        MOVE XXBU-2223-IDANSK TO WS-IDANSK                                
046500        MOVE 'WLXXBU01'     TO POSTSUM-FDNAMN                             
046600        MOVE 'GN  '         TO POSTSUM-DDNAMN2                            
046700        MOVE ' '            TO POSTSUM-TRANSTYP                           
046800        CALL POSTSUM USING POSTSUM-PARM                                   
046900     END-IF                                                               
047000     .                                                                    
047100     SKIP3                                                                
047200 IMS-GU-XXBU-ROT SECTION.                                                 
047300                                                                          
047400     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
047500          DELIMITED BY SIZE INTO SSA1                                     
047600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
047700     CALL CBLTDLI USING GU XXBU-PCB DLI-IO-AREA SSA1                      
047800     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
047900     PERFORM IMS-STATUSKONTROLL                                           
048000                                                                          
048100     IF SEGMENT-FINNS                                                     
048200        MOVE 'WLXXBU01'     TO POSTSUM-FDNAMN                             
048300        MOVE 'GU  '         TO POSTSUM-DDNAMN2                            
048400        MOVE ' '            TO POSTSUM-TRANSTYP                           
048500        CALL POSTSUM USING POSTSUM-PARM                                   
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 IMS-GET-XXBU-LARM SECTION.                                               
049000                                                                          
049100     STRING 'WLXXBU11(KDLARM  >=' W-KDLARM-MIN                            
049200                    '&KDLARM  <=' W-KDLARM-MAX                            
049300                    '&IDDC     =' W-IDDC-CDC-SE-X ')'                     
049400          DELIMITED BY SIZE INTO SSA1                                     
049500     MOVE '  GE' TO GODK-STATUSKODER                                      
049600     CALL CBLTDLI USING GHNP XXBU-PCB DLI-IO-AREA SSA1                    
049700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
049800     PERFORM IMS-STATUSKONTROLL                                           
049900                                                                          
050000     IF SEGMENT-FINNS                                                     
050100        MOVE XXBU-2224-KDLARM   TO WS-KDLARM                              
050200        MOVE XXBU-2224-TISENBEK TO WS-TISENBEK                            
050300        MOVE 'WLXXBU11'     TO POSTSUM-FDNAMN                             
050400        MOVE 'GHNP'         TO POSTSUM-DDNAMN2                            
050500        MOVE ' '            TO POSTSUM-TRANSTYP                           
050600        CALL POSTSUM USING POSTSUM-PARM                                   
050700     END-IF                                                               
050800     .                                                                    
050900     SKIP3                                                                
051000 IMS-DLET-XXBU SECTION.                                                   
051100                                                                          
051200     DISPLAY ' BORTTAG LARM ' W-IDARTNR ' *** '                           
051300                              WS-IDANSK ' *** '                           
051400                              WS-KDLARM ' *** '                           
051500                                                                          
051600     MOVE '  ' TO GODK-STATUSKODER                                        
051700     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-AREA                         
051800     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
051900     PERFORM IMS-STATUSKONTROLL                                           
052000     ADD +1    TO CHKP-ANT                                                
052100                                                                          
052200     MOVE 'WLXXBU11'     TO POSTSUM-FDNAMN                                
052300     MOVE 'DLET'         TO POSTSUM-DDNAMN2                               
052400     MOVE WS-KDLARM      TO POSTSUM-TRANSTYP                              
052500     CALL POSTSUM USING POSTSUM-PARM                                      
052600     .                                                                    
052700     EJECT                                                                
052800 IMS-GET-ARTC-CLAG SECTION.                                               
052900                                                                          
053000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
053100          DELIMITED BY SIZE INTO SSA1                                     
053200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
053300          DELIMITED BY SIZE INTO SSA2                                     
053400     MOVE '  GE' TO GODK-STATUSKODER                                      
053500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1 SSA2               
053600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053700     PERFORM IMS-STATUSKONTROLL                                           
053800     .                                                                    
053900                                                                          
054000                                                                          
054100 IMS-GU-WDB601     SECTION.                                               
054200                                                                          
054300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
054400          DELIMITED BY SIZE INTO SSA1                                     
054500     MOVE '  GE' TO GODK-STATUSKODER                                      
054600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
054700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     EJECT                                                                
055100******************************************************'                   
055200 IMS-GU-WDD924 SECTION.                                                   
055300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
055400          DELIMITED BY SIZE INTO SSA1                                     
055500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
055600          DELIMITED BY SIZE INTO SSA2                                     
055700     MOVE 'WDD924   ' TO SSA3                                             
055800     MOVE '  GE' TO GODK-STATUSKODER                                      
055900     CALL CBLTDLI USING GU WDD9-PCB IO-AREA-WDD9 SSA1 SSA2 SSA3           
056000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
056100     PERFORM IMS-STATUSKONTROLL                                           
056200     .                                                                    
056300     SKIP3                                                                
056400******************************************************'                   
056500 IMS-RESTART SECTION.                                                     
056600     SKIP2                                                                
056700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
056800     MOVE '  ' TO GODK-STATUSKODER                                        
056900     CALL CBLTDLI USING XRST MSG-PCB                                      
057000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
057100                        CHKP-AREA-LENGTH CHKP-AREA                        
057200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057300     PERFORM IMS-STATUSKONTROLL                                           
057400     .                                                                    
057500     EJECT                                                                
057600 IMS-CHECKPOINT SECTION.                                                  
057700     SKIP2                                                                
057800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
057900     MOVE '  XD' TO GODK-STATUSKODER                                      
058000     CALL CBLTDLI USING CHKP MSG-PCB                                      
058100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
058200                        CHKP-AREA-LENGTH CHKP-AREA                        
058300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058400     PERFORM IMS-STATUSKONTROLL                                           
058500                                                                          
058600     IF IMS-EJ-OK                                                         
058700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
058800       DISPLAY FELTEXT                                                    
058900       CALL FELLOG                                                        
059000     END-IF                                                               
059100     .                                                                    
059200     EJECT                                                                
059300 IMS-STATUSKONTROLL SECTION.                                              
059400     SKIP2                                                                
059500     SET STATUS-IX TO 1                                                   
059600     SEARCH GODK-STATUS                                                   
059700       AT END                                                             
059800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059900           DELIMITED BY SIZE INTO FELTEXT                                 
060000         DISPLAY FELTEXT                                                  
060100         CALL FELLOG                                                      
060200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060300         CONTINUE                                                         
060400     END-SEARCH                                                           
060500     .                                                                    
060600     EJECT                                                                
060700*    -COPY WY2000P1                                                       
