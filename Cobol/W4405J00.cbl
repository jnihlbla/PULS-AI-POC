000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4405J00.                                                
000300 AUTHOR.         GÖRAN KJELLSON    GUIDE                                  
000400 DATE-WRITTEN.   JUNI 2006                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER VORKÖ  (WDA6J)                                             
000900*        SKAPAR FIL TILL VOR-UPPF ARTIKEL, W4405J                         
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200                                                                          
002300*          --- FIL TILL VOR-UPPF ARTIKEL                                  
002400     SELECT W4405J                     ASSIGN TO W4405JD1.                
002500                                                                          
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800                                                                          
002900 FD  W4405J                                                               
003000     RECORDING       V                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  POST -COPY W4405J  -PRE  ARTIKEL- -L.                                
003400 01  ARTIKEL-HEAD-POST PIC X(150).                                        
003500                                                                          
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W4405J00'.            
004000 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
004100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  HORIZTAB                    PIC X       VALUE X'05'.                 
004500                                                                          
004600 01  DAGENS-DATUM                PIC 9(6) VALUE ZERO.                     
004700 01  HELP-YYMMDD                 PIC 9(6) VALUE ZERO.                     
004800                                                                          
004900 01  SW-LEVBESK                  PIC X.                                   
005000     88 LEVBESK-FINNS                        VALUE 'J'.                   
005100 01  SW-INFO                     PIC X.                                   
005200     88 INFO-FINNS                           VALUE 'J'.                   
005300                                                                          
005400                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006200                                                                          
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600                                                                          
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000                                                                          
007100*    --- PARAMETRAR TILL POSTSUM                                          
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300                                                                          
007400 01  FILLER             PIC X(16) VALUE 'WDATAREA     '.                  
007500*01   -COPY WDATAREA.                                                     
007600                                                                          
007700 01  ARTIKEL-AREA-START          PIC X(24)   VALUE                        
007800                                 'ARTIKEL-AREA-START '.                   
007900                                                                          
008000 01  ARTIKEL-HEAD-AREA           PIC X(150).                              
008100*01  AREA -COPY W4405J     -PRE ARTIKEL-                                  
008200                                                                          
008300                                                                          
008400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008500                                                                          
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700                                                                          
008800 01      W-IDARTNR-X.                                                     
008900     03  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
008910                                                                          
008920 01      W-IDDC-X.                                                        
008930     03  W-IDDC          PIC X(2)    VALUE SPACE.                         
009000                                                                          
009010 01      W-WDD901KY-X.                                                    
009020     03  W-IDARTNR-D9    PIC S9(9)   VALUE ZERO  COMP-3.                  
009021     03  W-IDDC-D9       PIC X(2)    VALUE SPACE.                         
009030                                                                          
009100 01    W-IDLEVNR-X.                                                       
009200     03 W-IDLEVNR        PIC X(5)    VALUE SPACE.                         
009300                                                                          
009400 01  W-IDSKYLT-X.                                                         
009500     03  W-IDSKYLT       PIC X(3)    VALUE 'GB'.                          
009600                                                                          
009700 01  W-IDLEVBSK-X.                                                        
009800     03  W-IDLEVBSK      PIC S9(1)   VALUE +2    COMP-3.                  
009900                                                                          
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FINNS                       VALUE '  '.                  
010300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010500                                                                          
010600 01  GODK-STATUSKODER.                                                    
010700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     SKIP3                                                                
010900 01  SSA1                        PIC X(64).                               
011000 01  SSA2                        PIC X(64).                               
011010 01  SSA3                        PIC X(64).                               
011100                                                                          
011200*    --- IMS FUNKTIONSKODER                                               
011300*01  -COPY W0003                                                          
011400                                                                          
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
011700 01  DLI-IO-WDA601.                                                       
011800*    03  -COPY WDA601                                                     
011900                                                                          
012000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012100 01  DLI-IO-WDK601.                                                       
012200*    03  -COPY WDK601                                                     
012300                                                                          
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012500 01  DLI-IO-WDK611.                                                       
012600*    03  -COPY WDK611                                                     
012610                                                                          
012620 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
012630 01  DLI-IO-WDK722.                                                       
012640*    03  -COPY WDK722                                                     
012700                                                                          
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
012900 01  DLI-IO-WDD311.                                                       
013000*    03  -COPY WDD311                                                     
013100                                                                          
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
013300 01  DLI-IO-WDD902.                                                       
013400*    03  -COPY WDD902                                                     
013500                                                                          
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
013700 01  DLI-IO-WDD924.                                                       
013800*    03  -COPY WDD924                                                     
013900                                                                          
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
014100 01  DLI-IO-WDD925.                                                       
014200*    03  -COPY WDD925                                                     
014300                                                                          
014400 LINKAGE SECTION.                                                         
014500                                                                          
014600*01  -COPY W0008  -PRE WDA6J-                                             
014700     05  FILLER                  PIC X.                                   
014800                                                                          
014900*01  -COPY W0008  -PRE WDK6-                                              
015000     05  FILLER                  PIC X.                                   
015100                                                                          
015110*01  -COPY W0008  -PRE WDK7-                                              
015120     05  FILLER                  PIC X.                                   
015130                                                                          
015200*01  -COPY W0008  -PRE WDD3-                                              
015300     05  FILLER                  PIC X.                                   
015400                                                                          
015500*01  -COPY W0008  -PRE WDD9-                                              
015600     05  FILLER                  PIC X.                                   
015700                                                                          
015800*01  -COPY W0008  -PRE WDD9-INFO-                                         
015900     05  FILLER                  PIC X.                                   
016000                                                                          
016100                                                                          
016200 PROCEDURE DIVISION  USING WDA6J-PCB WDK6-PCB WDK7-PCB WDD3-PCB           
016300                           WDD9-PCB WDD9-INFO-PCB.                        
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING WDA6J-PCB WDK6-PCB WDK7-PCB WDD3-PCB           
016600                           WDD9-PCB WDD9-INFO-PCB.                        
016700                                                                          
016800     PERFORM A-INIT                                                       
016900                                                                          
017000     PERFORM IMS-01-GN-WDA601-JSEQ                                        
017100     IF SEGMENT-FINNS                                                     
017200        PERFORM B-TILL-ARTIKEL                                            
017300                                                                          
017400        PERFORM IMS-01-GN-WDA601-JSEQ                                     
017500        PERFORM UNTIL SEGMENT-SLUT                                        
017600                                                                          
017700           IF VOR-IDARTNR NOT = ARTIKEL-IDARTNR                           
017800              PERFORM S10-SKRIV-W4405J                                    
017900              PERFORM B-TILL-ARTIKEL                                      
018000           ELSE                                                           
018100              IF VOR-TIREGDAT-URSP < ARTIKEL-TIREGDAT                     
018200                 MOVE VOR-TIREGDAT-URSP   TO ARTIKEL-TIREGDAT             
018300              END-IF                                                      
018400           END-IF                                                         
018500           PERFORM IMS-01-GN-WDA601-JSEQ                                  
018600                                                                          
018700        END-PERFORM                                                       
018800        PERFORM S10-SKRIV-W4405J                                          
018900     END-IF                                                               
019000                                                                          
019100                                                                          
019200     PERFORM Z-FINIT                                                      
019300                                                                          
019400     MOVE ZERO TO RETURN-CODE                                             
019500     GOBACK                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 A-INIT SECTION.                                                          
019900     MOVE 'A-INIT           ' TO CURRENT-SECTION                          
020000                                                                          
020100     OPEN OUTPUT W4405J                                                   
020200                                                                          
020300     MOVE ZERO TO ARTIKEL-IDARTNR                                         
020400     PERFORM S01-TILL-VORNU-HEAD                                          
020500                                                                          
020600     ACCEPT DAGENS-DATUM FROM DATE                                        
020700     .                                                                    
020800                                                                          
020900 B-TILL-ARTIKEL SECTION.                                                  
021000     MOVE 'B-TILL-ARTIKEL   ' TO CURRENT-SECTION                          
021100                                                                          
021200     MOVE VOR-IDARTNR TO W-IDARTNR                                        
021210     MOVE VOR-IDDC    TO W-IDDC                                           
021300     MOVE VOR-IDLEVNR TO W-IDLEVNR                                        
021400     PERFORM IMS-02-GU-WDK601                                             
021500     PERFORM IMS-03-GNP-WDK611                                            
021600                                                                          
021700*--- OBS ' ' SKALL INNEHÅLLA TAB (HEX-05) -------                         
021800     MOVE ALL '	'             TO ARTIKEL-AREA                             
021900     MOVE ART-IDARTNR         TO ARTIKEL-IDARTNR                          
022000     MOVE CLAG-IDANSK         TO ARTIKEL-IDANSK                           
022001     MOVE CLAG-IDINK          TO ARTIKEL-IDINK                            
022010     PERFORM IMS-04-GU-WDK722                                             
022020     IF SEGMENT-FINNS                                                     
022021       IF XLAG-IDANSK > 0                                                 
022030         MOVE XLAG-IDANSK     TO ARTIKEL-IDANSK                           
022031       END-IF                                                             
022032       IF XLAG-IDINK > 0                                                  
022033        MOVE XLAG-IDINK       TO ARTIKEL-IDINK                            
022034       END-IF                                                             
022050     END-IF                                                               
022200     MOVE CLAG-IDBERED        TO ARTIKEL-IDBERED                          
022300     MOVE CLAG-KVVORKO        TO ARTIKEL-KVVORKO                          
022400     MOVE CLAG-KVROS          TO ARTIKEL-KVROS                            
022500     MOVE VOR-TIREGDAT-URSP   TO ARTIKEL-TIREGDAT                         
022600     MOVE CLAG-IDPROJ         TO ARTIKEL-IDPROJ                           
022700                                                                          
022800     PERFORM IMS-05-GU-WDD311                                             
022900     MOVE TEXT-BEART          TO ARTIKEL-BEART                            
023000                                                                          
023100     PERFORM BA-INIT-LEVBESKED-DEL                                        
023101                                                                          
023102     MOVE VOR-IDARTNR TO W-IDARTNR-D9                                     
023110     MOVE VOR-IDDC    TO W-IDDC-D9                                        
023200     PERFORM IMS-06-GU-WDD902                                             
023300*    TILL SKILLNAD MOT 2109-BILDEN VISAR VI HÄR BARA                      
023400*    INFORMATION PER ARTIKEL-LEVERANTÖR                                   
023500                                                                          
023600     IF SEGMENT-FINNS                                                     
023700        PERFORM IMS-08-GU-WDD902-INFO                                     
023800*       VI LÄSER SAMMA SEGMENT EN GÅNG TILL                               
023900*       FÖR ATT HÅLLA TVÅ POSITIONER I BASEN                              
024000                                                                          
024100        MOVE JA  TO SW-LEVBESK                                            
024200                    SW-INFO                                               
024300        PERFORM IMS-07-GNP-WDD924                                         
024400        PERFORM IMS-09-GNP-WDD925-2                                       
024500                                                                          
024600        IF LEVBESK-FINNS                                                  
024700           PERFORM BB-REDIGERA-LEVERANSBESKED                             
024800        END-IF                                                            
024900                                                                          
025000        IF INFO-FINNS                                                     
025100           PERFORM BC-REDIGERA-LEVERANSINFO                               
025200        END-IF                                                            
025300     END-IF                                                               
025400                                                                          
025500     .                                                                    
025600                                                                          
025700 BA-INIT-LEVBESKED-DEL     SECTION.                                       
025800     MOVE 'BA-INIT-LEVBESKED-DEL ' TO CURRENT-SECTION                     
025900                                                                          
026000     MOVE ZERO  TO ARTIKEL-KVAVIS                                         
026100     MOVE SPACE TO ARTIKEL-TILEVBSK-INL                                   
026200                   ARTIKEL-TILEVBSK-DISP                                  
026300                   ARTIKEL-IDLEVNR                                        
026400                   ARTIKEL-TELEVBSK-EXT                                   
026500                                                                          
026600     MOVE DAGENS-DATUM TO HELP-YYMMDD                                     
026700     .                                                                    
026800                                                                          
026900 BB-REDIGERA-LEVERANSBESKED SECTION.                                      
027000     MOVE 'BB-REDIGERA-LEVERANSBESKED' TO CURRENT-SECTION                 
027100                                                                          
027200     MOVE VOR-IDLEVNR          TO ARTIKEL-IDLEVNR                         
027300     MOVE LEV-KVAVIS-BSKKVAR   TO ARTIKEL-KVAVIS                          
027400                                                                          
027500     IF LEV-TILEVBSK-INL > 0                                              
027600        MOVE LEV-TILEVBSK-INL  TO DAT-I-TIDATUM                           
027700        PERFORM S02-DATUMKONV-TILL-AAVVD                                  
027800        IF DAT-KDSVAR-OK                                                  
027900           MOVE DAT-TIAAVVD    TO ARTIKEL-TILEVBSK-INL                    
028000        END-IF                                                            
028100     END-IF                                                               
028200                                                                          
028300     IF LEV-TILEVBSK-DISP > 0                                             
028400        MOVE LEV-TILEVBSK-DISP TO DAT-I-TIDATUM                           
028500        PERFORM S02-DATUMKONV-TILL-AAVVD                                  
028600        IF DAT-KDSVAR-OK                                                  
028700           MOVE DAT-TIAAVVD    TO ARTIKEL-TILEVBSK-DISP                   
028800        END-IF                                                            
028900     END-IF                                                               
029000                                                                          
029100     .                                                                    
029200                                                                          
029300 BC-REDIGERA-LEVERANSINFO   SECTION.                                      
029400     MOVE 'BC-REDIGERA-LEVERANSINFO  ' TO CURRENT-SECTION                 
029500                                                                          
029600     MOVE INFO-TELEVBSK          TO ARTIKEL-TELEVBSK-EXT                  
029700     .                                                                    
029800                                                                          
029900 Z-FINIT SECTION.                                                         
030000     MOVE 'Z-FINIT ' TO CURRENT-SECTION                                   
030100                                                                          
030200     CLOSE W4405J                                                         
030300                                                                          
030400     MOVE 'S' TO POSTSUM-OPKOD                                            
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     .                                                                    
030700**** RUBRIK TILL EXCEL-FIL ****                                           
030800 S01-TILL-VORNU-HEAD  SECTION.                                            
030900     MOVE 'S01-TILL-VORNU-HEAD  ' TO CURRENT-SECTION                      
031000                                                                          
031100     STRING 'PU PLANNER',    HORIZTAB,                                    
031200            'PURCH',         HORIZTAB,                                    
031300            'PARTS PLANNER', HORIZTAB,                                    
031400            'PART NO',       HORIZTAB,                                    
031500            'DESC',          HORIZTAB,                                    
031600            'NO VOR',        HORIZTAB,                                    
031700            'NO BO',         HORIZTAB,                                    
031800            'VOR',           HORIZTAB,                                    
031900            'PROJ',          HORIZTAB,                                    
032000            'INFO',          HORIZTAB,                                    
032100            'QUANT',         HORIZTAB,                                    
032200            'DATE WH',       HORIZTAB,                                    
032300            'DATE STOCK',    HORIZTAB,                                    
032400            'SUPPL',         HORIZTAB,                                    
032500            'REASON',        HORIZTAB,                                    
032600            'ACTIVITY',      HORIZTAB,                                    
032700            'CLEAR',         HORIZTAB,                                    
032800            'RAG',           HORIZTAB                                     
032900     DELIMITED BY SIZE INTO ARTIKEL-HEAD-AREA                             
033000                                                                          
033100     WRITE ARTIKEL-HEAD-POST FROM ARTIKEL-HEAD-AREA                       
033200     .                                                                    
033300                                                                          
033400 S02-DATUMKONV-TILL-AAVVD  SECTION.                                       
033500                                                                          
033600     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
033700     CALL WDATKONV USING DAT-KDDATFORM,                                   
033800                         DAT-I-TIDATUM,                                   
033900                         DAT-O-TIDATUM,                                   
034000                         DAT-KDSVAR                                       
034100     .                                                                    
034200                                                                          
034300 S10-SKRIV-W4405J SECTION.                                                
034400     MOVE 'S10-SKRIV-W4405J     ' TO CURRENT-SECTION                      
034500                                                                          
034600     WRITE ARTIKEL-POST FROM ARTIKEL-AREA                                 
034700                                                                          
034800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
034900     MOVE 'W4405J'   TO POSTSUM-FDNAMN                                    
035000     MOVE 'W4405JD1' TO POSTSUM-DDNAMN2                                   
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     .                                                                    
035300                                                                          
035400* --- IMS SEKTIONER ---                                                   
035500                                                                          
035600                                                                          
035700 IMS-01-GN-WDA601-JSEQ SECTION.                                           
035800     MOVE 'IMS-01' TO CURRENT-IMS-SECTION                                 
035900                                                                          
036000     MOVE 'WDA601 ' TO SSA1                                               
036100     MOVE '  GB'    TO GODK-STATUSKODER                                   
036200     CALL CBLTDLI USING GN WDA6J-PCB DLI-IO-WDA601 SSA1                   
036300     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
036400     PERFORM IMS-STATUSKONTROLL                                           
036500     .                                                                    
036600                                                                          
036700 IMS-02-GU-WDK601 SECTION.                                                
036800     MOVE 'IMS-02' TO CURRENT-IMS-SECTION                                 
036900                                                                          
037000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
037100            DELIMITED BY SIZE INTO SSA1                                   
037200     MOVE '  '                  TO GODK-STATUSKODER                       
037300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
037400     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
037500     PERFORM IMS-STATUSKONTROLL                                           
037600     .                                                                    
037700                                                                          
037800 IMS-03-GNP-WDK611 SECTION.                                               
037900     MOVE 'IMS-03' TO CURRENT-IMS-SECTION                                 
038000                                                                          
038100     MOVE 'WDK611  '       TO SSA1                                        
038200     MOVE '  '             TO GODK-STATUSKODER                            
038300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
038400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
038500     PERFORM IMS-STATUSKONTROLL                                           
038600     .                                                                    
038700                                                                          
038710 IMS-04-GU-WDK722 SECTION.                                                
038720     MOVE 'IMS-04' TO CURRENT-IMS-SECTION                                 
038730                                                                          
038731     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
038732            DELIMITED BY SIZE INTO SSA1                                   
038733     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
038734            DELIMITED BY SIZE INTO SSA2                                   
038740     MOVE 'WDK722  '       TO SSA3                                        
038750     MOVE '  GE'           TO GODK-STATUSKODER                            
038760     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
038770     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
038780     PERFORM IMS-STATUSKONTROLL                                           
038790     .                                                                    
038791                                                                          
038800 IMS-05-GU-WDD311 SECTION.                                                
038900     MOVE 'IMS-05' TO CURRENT-IMS-SECTION                                 
039000                                                                          
039100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
039200          DELIMITED BY SIZE INTO SSA1                                     
039300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
039400          DELIMITED BY SIZE INTO SSA2                                     
039500     MOVE '  GE'              TO GODK-STATUSKODER                         
039600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
039700     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     IF SEGMENT-SAKNAS                                                    
040000        MOVE SPACE TO TEXT-BEART                                          
040100     END-IF                                                               
040200     .                                                                    
040300                                                                          
040400 IMS-06-GU-WDD902   SECTION.                                              
040500     MOVE 'IMS-06' TO CURRENT-IMS-SECTION                                 
040600                                                                          
040700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
040800            DELIMITED BY SIZE INTO SSA1                                   
040900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
041000            DELIMITED BY SIZE INTO SSA2                                   
041100     MOVE '  GE'                TO GODK-STATUSKODER                       
041200     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
041300     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     .                                                                    
041600                                                                          
041700 IMS-07-GNP-WDD924   SECTION.                                             
041800     MOVE 'IMS-07' TO CURRENT-IMS-SECTION                                 
041900                                                                          
042000     MOVE 'WDD924 '          TO SSA1                                      
042100     MOVE '  GE'             TO GODK-STATUSKODER                          
042200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
042300     MOVE WDD9-STATUS-CODE   TO STATUS-WS                                 
042400     PERFORM IMS-STATUSKONTROLL                                           
042500                                                                          
042600     IF SEGMENT-SAKNAS                                                    
042700        MOVE NEJ TO SW-LEVBESK                                            
042800     END-IF                                                               
042900     .                                                                    
043000                                                                          
043100 IMS-08-GU-WDD902-INFO   SECTION.                                         
043200     MOVE 'IMS-08' TO CURRENT-IMS-SECTION                                 
043300                                                                          
043400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
043500            DELIMITED BY SIZE INTO SSA1                                   
043600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
043700            DELIMITED BY SIZE INTO SSA2                                   
043800     MOVE '  '                  TO GODK-STATUSKODER                       
043900     CALL CBLTDLI USING GU WDD9-INFO-PCB DLI-IO-WDD902 SSA1 SSA2          
044000     MOVE WDD9-INFO-STATUS-CODE TO STATUS-WS                              
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300                                                                          
044400 IMS-09-GNP-WDD925-2      SECTION.                                        
044500     MOVE 'IMS-09' TO CURRENT-IMS-SECTION                                 
044600                                                                          
044700     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
044800            DELIMITED BY SIZE INTO SSA1                                   
044900     MOVE '  GE'                TO GODK-STATUSKODER                       
045000     CALL CBLTDLI USING GNP WDD9-INFO-PCB DLI-IO-WDD925 SSA1              
045100     MOVE WDD9-INFO-STATUS-CODE TO STATUS-WS                              
045200     PERFORM IMS-STATUSKONTROLL                                           
045300     IF SEGMENT-SAKNAS                                                    
045400        MOVE NEJ TO SW-INFO                                               
045500     END-IF                                                               
045600     .                                                                    
045700                                                                          
045800 IMS-STATUSKONTROLL SECTION.                                              
045900                                                                          
046000     SET STATUS-IX TO 1                                                   
046100     SEARCH GODK-STATUS                                                   
046200       AT END                                                             
046300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046400           DELIMITED BY SIZE INTO FELTEXT-STR                             
046500         DISPLAY FELTEXT                                                  
046600         CALL FELLOG                                                      
046700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046800         CONTINUE                                                         
046900     END-SEARCH                                                           
047000     .                                                                    
