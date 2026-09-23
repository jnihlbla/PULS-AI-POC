000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W200CDCI.                                                
000400 AUTHOR.         SATHISH THIRUVENGADAM.                                   
000500 DATE-WRITTEN.   MAR 2023.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SUBPROGRAM TO GET THE FOLLOWING FOR CDC:                         
001100*        -PACKAGING TYPE , PACKING-0,1,2,3 ,Q3 ,WEIGHT ,VOLUME,           
001200*         LOCATION ,ORIGIN  AS IN 4101                                    
001300*        -PSN AS IN 1118                                                  
001400*        -FIRST OCCURANCE OF PURCH., MFG, SHP AS IN 2103                  
001500*        -GOODS REC DAY AS IN 2101                                        
001600*        -PLANNED INCOME WEEK AS IN 2106                                  
001700*                                                                         
001800*        PROGRAMMET READS     WDK6                                        
001900*                             WDD9                                        
002000*                                                                         
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W200CDCI'.            
003800 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003900 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200     EJECT                                                                
004300                                                                          
004400                                                                          
004500 01  WORKING-FIELDS.                                                      
004600*                                                                         
004700     03  INDX                    PIC  9(3)   VALUE ZERO.                  
004800                                                                          
004900     03  WS-DATUM-CCYYWW.                                                 
005000         05 WS-DATUM-CC          PIC 9(2).                                
005100         05 WS-DATUM-YYWW        PIC 9(4).                                
005200                                                                          
005300     03  WS-DAAVROP-AVS          PIC 9(6).                                
005400     03  FILLER  REDEFINES WS-DAAVROP-AVS.                                
005500         05  FILLER              PIC 9(2).                                
005600         05  WS-TIAVROP-AVS      PIC 9(4).                                
005700                                                                          
005800 01  TODAYS-DATE                 PIC 9(8)    VALUE ZERO.                  
005900*                                                                         
006000*                                                                         
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200*                                                                         
006300     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
006400     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
006500     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
006600     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM'.              
006700     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
006800     SKIP2                                                                
006900***************************************************************           
007000*       C O P Y T E X T E R    (DYNAMISKA ANROP)                          
007100***************************************************************           
007200 01  FILLER                      PIC X(16) VALUE 'WDATAREA     '.         
007300*01   -COPY WDATAREA.                                                     
007400                                                                          
007500                                                                          
007600*    --- PARAMETERS TO ABEND                                              
007700                                                                          
007800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008100*                                                                         
008200*SWITCHES                                                                 
008300                                                                          
008400 77  INPUT-DATA-SW               PIC X(01)   VALUE 'J'.                   
008500     88  INPUT-DATA-OK                       VALUE 'J'.                   
008600     88  INPUT-DATA-FEL                      VALUE 'N'.                   
008700*                                                                         
008800                                                                          
008900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200 01  KEYS-TO-DLI.                                                         
009300     03  W-IDARTNR-X.                                                     
009400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009500                                                                          
009600     03  W-WDD901KY-X.                                                    
009700         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
009800         05  W-IDDC-D9           PIC X(2)    VALUE '11'.                  
009900                                                                          
010000     03  W-WDD905KY-X.                                                    
010100         05  W-DAAVROP-X.                                                 
010200             07  W-DAAVROP       PIC 9(6)    VALUE ZERO.                  
010300         05  W-TILEVDAG-X.                                                
010400             07  W-TILEVDAG      PIC S9      VALUE ZERO COMP-3.           
010500                                                                          
010600     03  W-KDAVROP-X.                                                     
010700         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
010800                                                                          
010900     03  W-IDLEVNR-X.                                                     
011000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
011100                                                                          
011200*    --- IMS FUNCTION CODES                                               
011300*01  -COPY W0003                                                          
011400                                                                          
011500                                                                          
011600*    ---  DLI INPUT-OUTPUT AREA                                           
011700                                                                          
011800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
011900 01  DLI-IO-WDK601.                                                       
012000*    03  -COPY WDK601                                                     
012100                                                                          
012200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012300 01  DLI-IO-WDK611.                                                       
012400*    03  -COPY WDK611                                                     
012500                                                                          
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
012700 01  DLI-IO-WDD902.                                                       
012800*    03  -COPY WDD902                                                     
012900                                                                          
013000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
013100 01  DLI-IO-WDD905.                                                       
013200*    03  -COPY WDD905                                                     
013300                                                                          
013400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
013500 01  DLI-IO-WDD924.                                                       
013600*    03  -COPY WDD924                                                     
013700                                                                          
013800                                                                          
013900*    --- STATUS-CODE FROM IMS                                             
014000 01  STATUS-WS                   PIC XX.                                  
014100     88  SEGMENT-FOUND                       VALUE '  '.                  
014200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014400     88  END-OF-BASE                         VALUE 'GB'.                  
014500     SKIP2                                                                
014600 01  GOOD-STATUSCODES.                                                    
014700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800                                                                          
014900 01  ALL-SSA.                                                             
015000    03 SSA1                     PIC X(128).                               
015100    03 SSA2                     PIC X(64).                                
015200    03 SSA3                     PIC X(64).                                
015300                                                                          
015400                                                                          
015500 LINKAGE SECTION.                                                         
015600*    -COPY W200CDCI                                                       
015700                                                                          
015800     EJECT                                                                
015900*01  -COPY W0008      -PRE WDK6-                                          
016000     05  FILLER                  PIC X.                                   
016100*01  -COPY W0008      -PRE WDD9-                                          
016200     05  FILLER                  PIC X.                                   
016300                                                                          
016400                                                                          
016500 PROCEDURE DIVISION  USING CDCI-W200CDCI WDK6-PCB WDD9-PCB.               
016600                                                                          
016700     PERFORM A-INIT                                                       
016800     PERFORM B-VALIDATE-INPUT                                             
016900     IF INPUT-DATA-OK                                                     
017000        PERFORM C-GET-WDK6                                                
017100        IF CDCI-IDLEVNR-MFG > SPACES                                      
017200           MOVE CDCI-IDLEVNR-MFG     TO W-IDLEVNR                         
017300           PERFORM D-GET-WDD9                                             
017400        END-IF                                                            
017500     END-IF                                                               
017600                                                                          
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100     MOVE 'A-INIT         ' TO CURRENT-SECTION                            
018200                                                                          
018300     MOVE FUNCTION CURRENT-DATE(1:8) TO TODAYS-DATE                       
018400                                                                          
018500     INITIALIZE CDCI-UTDATA                                               
018600     MOVE JA                         TO INPUT-DATA-SW                     
018700                                                                          
018800     MOVE SPACES                     TO CDCI-KDSVAR                       
018900                                        CDCI-FEL-TEXT                     
019000                                        CDCI-IDMSG-ERROR                  
019100                                        CDCI-IDELMT-ERROR                 
019200                                                                          
019300*TO GET CURRENT WEEK -CCYYWW FORMAT                                       
019400     MOVE TODAYS-DATE                TO DAT-I-TIDATUM                     
019500     MOVE 'AAMMDD'                   TO DAT-KDDATFORM                     
019600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
019700                         DAT-O-TIDATUM DAT-KDSVAR                         
019800     IF DAT-KDSVAR-OK                                                     
019900       MOVE DAT-TISEKEL              TO WS-DATUM-CC                       
020000       MOVE DAT-TIAAVV-GRP           TO WS-DATUM-YYWW                     
020100     ELSE                                                                 
020200       SET  CDCI-KDSVAR-FEL          TO TRUE                              
020300       MOVE '023'                    TO CDCI-IDMSG-ERROR                  
020400       MOVE 'CURRENT DATE'           TO CDCI-IDELMT-ERROR                 
020500       MOVE 'DATE CONV ERR  '        TO CDCI-FEL-TEXT                     
020600     END-IF                                                               
020700*                                                                         
020800     .                                                                    
020900     EJECT                                                                
021000                                                                          
021100 B-VALIDATE-INPUT SECTION.                                                
021200     MOVE 'B-VALIDATE-    ' TO CURRENT-SECTION                            
021300                                                                          
021400     IF  CDCI-IDARTNR-IN  IS NUMERIC                                      
021500     AND CDCI-IDARTNR-IN    > ZERO                                        
021600         MOVE CDCI-IDARTNR-IN    TO W-IDARTNR                             
021700                                    W-IDARTNR-D9                          
021800     ELSE                                                                 
021900         MOVE NEJ                TO INPUT-DATA-SW                         
022000*                                                                         
022100         SET  CDCI-KDSVAR-FEL     TO TRUE                                 
022200         MOVE '022'               TO CDCI-IDMSG-ERROR                     
022300         MOVE 'IDARTNR'           TO CDCI-IDELMT-ERROR                    
022400         MOVE 'INVALID PART     ' TO CDCI-FEL-TEXT                        
022500*                                                                         
022600     END-IF                                                               
022700     .                                                                    
022800 C-GET-WDK6              SECTION.                                         
022900     MOVE 'C-GET-WDK6    ' TO CURRENT-SECTION                             
023000                                                                          
023100     PERFORM IMS-GU-WDK601                                                
023200     IF SEGMENT-FOUND                                                     
023300       MOVE ART-IDLEVNR    TO CDCI-IDLEVNR-MFG                            
023400       PERFORM IMS-GNP-WDK611                                             
023500       IF  SEGMENT-FOUND                                                  
023600         MOVE CLAG-BEFT          TO CDCI-BEFT                             
023700         MOVE CLAG-IDARTNR-EMBQ0 TO CDCI-IDARTNR-EMBQ0                    
023800         MOVE CLAG-IDARTNR-EMBQ1 TO CDCI-IDARTNR-EMBQ1                    
023900         MOVE CLAG-IDARTNR-EMBQ2 TO CDCI-IDARTNR-EMBQ2                    
024000         MOVE CLAG-IDARTNR-EMBQ3 TO CDCI-IDARTNR-EMBQ3                    
024100         MOVE CLAG-IDPSN         TO CDCI-IDPSN                            
024200         MOVE CLAG-KVQPACK-3     TO CDCI-KVQPACK-3                        
024300         MOVE CLAG-VKART         TO CDCI-VKART                            
024400         MOVE CLAG-VLARTNTO      TO CDCI-VLARTNTO                         
024500         MOVE CLAG-ADLAGOMR      TO CDCI-ADLAGOMR                         
024600         MOVE CLAG-ADGANG        TO CDCI-ADGANG                           
024700         MOVE CLAG-ADPLATS       TO CDCI-ADPLATS                          
024800         MOVE CLAG-KDARTURS      TO CDCI-KDARTURS                         
024900         MOVE CLAG-IDLEVNR-SHIP  TO CDCI-IDLEVNR-SHIP                     
025000         MOVE CLAG-KVVECKOR-LT   TO CDCI-KVVECKOR-LT                      
025100         MOVE CLAG-KVDAGAR-TT    TO CDCI-KVDAGAR-TT                       
025200         MOVE CLAG-KVDAGAR-INLEV TO CDCI-KVDAGAR-INLEV                    
025300         MOVE CLAG-KDFARLIG      TO CDCI-KDFARLIG                         
025400       END-IF                                                             
025500     ELSE                                                                 
025600        SET  CDCI-KDSVAR-FEL     TO TRUE                                  
025700        MOVE '025'               TO CDCI-IDMSG-ERROR                      
025800        MOVE 'IDARTNR'           TO CDCI-IDELMT-ERROR                     
025900        MOVE 'MISSING IN WDK601' TO CDCI-FEL-TEXT                         
026000     END-IF                                                               
026100     .                                                                    
026200 D-GET-WDD9   SECTION.                                                    
026300                                                                          
026400     MOVE 'D-GET-WDD9    ' TO CURRENT-SECTION                             
026500                                                                          
026600     PERFORM IMS-GU-WDD902                                                
026700     IF SEGMENT-FOUND                                                     
026800        PERFORM E-GET-WDD905                                              
026900        PERFORM F-GET-WDD924                                              
027000     END-IF                                                               
027100     .                                                                    
027200                                                                          
027300 E-GET-WDD905 SECTION.                                                    
027400*TO GET CALL OFF                                                          
027500     MOVE 'E-GET-WDD905  ' TO CURRENT-SECTION                             
027600                                                                          
027700     MOVE WS-DATUM-CCYYWW         TO W-DAAVROP                            
027800     MOVE +2                      TO W-KDAVROP                            
027900*                                                                         
028000     PERFORM IMS-GNP-WDD905                                               
028100     IF SEGMENT-FOUND                                                     
028200        MOVE KVAVROP              TO CDCI-KVAVROP                         
028300        MOVE DAAVROP-AVS          TO WS-DAAVROP-AVS                       
028400        MOVE WS-TIAVROP-AVS       TO CDCI-TIAVROP-AVS                     
028500     END-IF                                                               
028600     .                                                                    
028700                                                                          
028800 F-GET-WDD924 SECTION.                                                    
028900*TO GET ETA                                                               
029000     MOVE 'F-GET-WDD924  ' TO CURRENT-SECTION                             
029100                                                                          
029200     PERFORM IMS-GNP-WDD924                                               
029300     IF SEGMENT-FOUND                                                     
029400        MOVE LEV-TILEVBSK-DISP         TO DAT-I-TIDATUM                   
029500        PERFORM S01-DATE-CONVER-TO-AAVVD                                  
029600        IF DAT-KDSVAR-OK                                                  
029700           MOVE DAT-TIAAVVD       TO CDCI-TIETA                           
029800        ELSE                                                              
029900           MOVE ALL ZERO          TO CDCI-TIETA                           
030000        END-IF                                                            
030100     END-IF                                                               
030200     .                                                                    
030300                                                                          
030400 S01-DATE-CONVER-TO-AAVVD  SECTION.                                       
030500     MOVE 'S01-DATE-CONVER-TO-AAVVD' TO CURRENT-SECTION                   
030600                                                                          
030700     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
030800     CALL WDATKONV USING DAT-KDDATFORM,                                   
030900                         DAT-I-TIDATUM,                                   
031000                         DAT-O-TIDATUM,                                   
031100                         DAT-KDSVAR                                       
031200     .                                                                    
031300     EJECT                                                                
031400* ---                                                                     
031500* --- IMS SECTIONS  ---                                                   
031600* ---                                                                     
031700                                                                          
031800                                                                          
031900 IMS-GU-WDK601  SECTION.                                                  
032000     MOVE 'IMS-GU-WDK601 ' TO CURR-IMS-SECTION                            
032100                                                                          
032200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032300          DELIMITED BY SIZE INTO SSA1                                     
032400     MOVE '  GE' TO GOOD-STATUSCODES                                      
032500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
032600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032700     PERFORM IMS-STATUSCHECK                                              
032800     .                                                                    
032900     SKIP3                                                                
033000 IMS-GNP-WDK611 SECTION.                                                  
033100     MOVE 'IMS-GNP-WDK611' TO CURR-IMS-SECTION                            
033200                                                                          
033300     MOVE 'WDK611    '      TO SSA1                                       
033400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
033500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
033600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033700     PERFORM IMS-STATUSCHECK                                              
033800     .                                                                    
033900     EJECT                                                                
034000 IMS-GU-WDD902 SECTION.                                                   
034100     MOVE 'IMS-GU-WDD902         ' TO CURR-IMS-SECTION                    
034200                                                                          
034300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
034400          DELIMITED BY SIZE INTO SSA1                                     
034500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
034600          DELIMITED BY SIZE INTO SSA2                                     
034700     MOVE '  GE' TO GOOD-STATUSCODES                                      
034800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
034900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
035000     PERFORM IMS-STATUSCHECK                                              
035100     .                                                                    
035200     EJECT                                                                
035300 IMS-GNP-WDD905       SECTION.                                            
035400     MOVE 'IMS-GNP-WDD905        ' TO CURR-IMS-SECTION                    
035500                                                                          
035600     STRING 'WDD905  (WDD905KY>=' W-WDD905KY-X     '&'                    
035700                     'KDAVROP  =' W-KDAVROP-X ')'                         
035800          DELIMITED BY SIZE INTO SSA1                                     
035900     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
036000     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
036100     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
036200     PERFORM IMS-STATUSCHECK                                              
036300     .                                                                    
036400     SKIP2                                                                
036500 IMS-GNP-WDD924 SECTION.                                                  
036600     MOVE 'IMS-GNP-WDD924        ' TO CURR-IMS-SECTION                    
036700                                                                          
036800     MOVE 'WDD924   ' TO SSA1                                             
036900     MOVE '  GEGB'    TO GOOD-STATUSCODES                                 
037000     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
037100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
037200     PERFORM IMS-STATUSCHECK                                              
037300     .                                                                    
037400     SKIP3                                                                
037500 IMS-STATUSCHECK SECTION.                                                 
037600                                                                          
037700     SET STATUS-IX TO 1                                                   
037800     SEARCH GOOD-STATUS                                                   
037900       AT END                                                             
038000         CALL FELLOG                                                      
038100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
038200         CONTINUE                                                         
038300     END-SEARCH                                                           
038400     .                                                                    
