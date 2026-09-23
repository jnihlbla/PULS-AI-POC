000100*COMPOPT VPOSIX=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W6039100.                                                
000500 AUTHOR.         HÅKAN BOHLIN.                                            
000600 DATE-WRITTEN.   21/09/24.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        RECEIVE PUSHEVENT TRANSACTIONS FROM PROJECT44.                   
001100*                                                                         
001200*        PROGRAM UPDATES  WDR5 (6301/WDGX6302)                            
001210*                READS    WDB6                                            
001300*    INDATA.                                                              
001400*        TRANSAKTION: W6T391X                                             
001500*        MID:         W6I39101                                            
001600*                                                                         
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400 77  IDPGM                       PIC X(08)     VALUE 'W6039100'.          
002500                                                                          
002600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002800 77  KDRC-DISPLAY                PIC Z(5).                                
002900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
003000                                                                          
003100 77  WZ11OUTM-SW                 PIC X       VALUE ' '.                   
003200     88 SW-OUTM-ERR                          VALUE 'E'.                   
003300     88 SW-OUTM-OPEN                         VALUE 'O'.                   
003400                                                                          
003500 01  WS-OUTDATA-L                PIC S9(9) COMP.                          
003600 01  WS-OUTDATA                  PIC X(3000).                             
003700 01  WS-TEXT                     PIC X(10000).                            
003800 01  WS-NUM-DISPLAY              PIC 9(9).                                
003900 01  WS-BAQCTERM-RC              PIC X(03)   VALUE SPACES.                
004000 01  WS-BAQCSTUB-RC              PIC X(03)   VALUE SPACES.                
004100                                                                          
004200 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004300 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004400                                                                          
004500 01  CURRENT-TIME                PIC 9(8)    VALUE ZERO.                  
004600 01  CURRENT-DATE                PIC 9(6)    VALUE ZERO.                  
004700 01  FILLER REDEFINES CURRENT-DATE.                                       
004800     03  CURRENT-DATE-YEAR       PIC 9(2).                                
004900     03  CURRENT-DATE-MONTH      PIC 9(2).                                
005000     03  CURRENT-DATE-DAY        PIC 9(2).                                
005100     EJECT                                                                
005200 01  P44-CER-DATE.                                                        
005300     03  FILLER                  PIC X(2)    VALUE '20'.                  
005400     03  P44-CER-YEAR            PIC 9(2).                                
005500     03  FILLER                  PIC X(1)    VALUE '-'.                   
005600     03  P44-CER-MONTH           PIC 9(2).                                
005700     03  FILLER                  PIC X(1)    VALUE '-'.                   
005800     03  P44-CER-DAY             PIC 9(2).                                
005900                                                                          
006000 01  WS-LIFDEPPL.                                                         
006100     03  WS-LIFDEPPL-YEAR        PIC X(2).                                
006200     03  WS-LIFDEPPL-MONTH       PIC X(2).                                
006300     03  WS-LIFDEPPL-DAY         PIC X(2).                                
006400 01  FILLER REDEFINES WS-LIFDEPPL.                                        
006500     03  WS-LIFDEPPL-NUM         PIC 9(6).                                
006600                                                                          
006700 01  WS-LIFDEPAC.                                                         
006800     03  WS-LIFDEPAC-YEAR        PIC X(2).                                
006900     03  WS-LIFDEPAC-MONTH       PIC X(2).                                
007000     03  WS-LIFDEPAC-DAY         PIC X(2).                                
007100 01  FILLER REDEFINES WS-LIFDEPAC.                                        
007200     03  WS-LIFDEPAC-NUM         PIC 9(6).                                
007300                                                                          
007400 01  WS-PODDEPPL.                                                         
007500     03  WS-PODDEPPL-YEAR        PIC X(2).                                
007600     03  WS-PODDEPPL-MONTH       PIC X(2).                                
007700     03  WS-PODDEPPL-DAY         PIC X(2).                                
007800 01  FILLER REDEFINES WS-PODDEPPL.                                        
007900     03  WS-PODDEPPL-NUM         PIC 9(6).                                
008000                                                                          
008100 01  WS-PODDEPAC.                                                         
008200     03  WS-PODDEPAC-YEAR        PIC X(2).                                
008300     03  WS-PODDEPAC-MONTH       PIC X(2).                                
008400     03  WS-PODDEPAC-DAY         PIC X(2).                                
008500 01  FILLER REDEFINES WS-PODDEPAC.                                        
008600     03  WS-PODDEPAC-NUM         PIC 9(6).                                
008700                                                                          
008800 01  WS-DLVDELPL.                                                         
008900     03  WS-DLVDELPL-YEAR        PIC X(2).                                
009000     03  WS-DLVDELPL-MONTH       PIC X(2).                                
009100     03  WS-DLVDELPL-DAY         PIC X(2).                                
009200 01  FILLER REDEFINES WS-DLVDELPL.                                        
009300     03  WS-DLVDELPL-NUM         PIC 9(6).                                
009400                                                                          
009500 01  WS-DLVDELAC.                                                         
009600     03  WS-DLVDELAC-YEAR        PIC X(2).                                
009700     03  WS-DLVDELAC-MONTH       PIC X(2).                                
009800     03  WS-DLVDELAC-DAY         PIC X(2).                                
009900 01  FILLER REDEFINES WS-DLVDELAC.                                        
010000     03  WS-DLVDELAC-NUM         PIC 9(6).                                
010100                                                                          
010200 01  WS-PODDISPL.                                                         
010300     03  WS-PODDISPL-YEAR        PIC X(2).                                
010400     03  WS-PODDISPL-MONTH       PIC X(2).                                
010500     03  WS-PODDISPL-DAY         PIC X(2).                                
010600 01  FILLER REDEFINES WS-PODDISPL.                                        
010700     03  WS-PODDISPL-NUM         PIC 9(6).                                
010800                                                                          
010900 01  WS-PODDISAC.                                                         
011000     03  WS-PODDISAC-YEAR        PIC X(2).                                
011100     03  WS-PODDISAC-MONTH       PIC X(2).                                
011200     03  WS-PODDISAC-DAY         PIC X(2).                                
011300 01  FILLER REDEFINES WS-PODDISAC.                                        
011400     03  WS-PODDISAC-NUM         PIC 9(6).                                
011500                                                                          
011600 01  WS-PODARRPL.                                                         
011700     03  WS-PODARRPL-YEAR        PIC X(2).                                
011800     03  WS-PODARRPL-MONTH       PIC X(2).                                
011900     03  WS-PODARRPL-DAY         PIC X(2).                                
012000 01  FILLER REDEFINES WS-PODARRPL.                                        
012100     03  WS-PODARRPL-NUM         PIC 9(6).                                
012200                                                                          
012300 01  WS-LIFARRAC.                                                         
012400     03  WS-LIFARRAC-YEAR        PIC X(2).                                
012500     03  WS-LIFARRAC-MONTH       PIC X(2).                                
012600     03  WS-LIFARRAC-DAY         PIC X(2).                                
012700 01  FILLER REDEFINES WS-LIFARRAC.                                        
012800     03  WS-LIFARRAC-NUM         PIC 9(6).                                
012900                                                                          
013000 01  WS-DISCH-ETA                PIC 9(6)    VALUE ZERO.                  
013100 01  WS-IDSUBSCR                 PIC 9(11)   VALUE ZERO.                  
013200 01  WS-IDCONTNR                 PIC 9(11)   VALUE ZERO.                  
013210 01  WS-LEADTIMES-BOAT           PIC 9(5)    VALUE ZERO.                  
013300                                                                          
013400 77  YES                         PIC X       VALUE 'Y'.                   
013500 77  NOO                         PIC X       VALUE 'N'.                   
013600                                                                          
013700 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
013800     88  INDATA-OK                           VALUE 'Y'.                   
013900     88  INDATA-WRONG                        VALUE 'N'.                   
014000                                                                          
014100 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
014200     88  KEYS-OK                             VALUE 'Y'.                   
014300     88  KEYS-WRONG                          VALUE 'N'.                   
014400                                                                          
014500 77  CER-SW                      PIC X       VALUE 'N'.                   
014600     88  CER-TYPE                            VALUE 'Y'.                   
014700                                                                          
014800 77  SW-MAIL-OPEN                PIC X       VALUE 'N'.                   
014900     88 MAIL-OPEN-YES                        VALUE 'Y'.                   
015000     88 MAIL-OPEN-NO                         VALUE 'N'.                   
015100                                                                          
015200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015300     88  GOOD-MID                            VALUE '6391'.                
015400     88  HELP-MID                            VALUE '0551'.                
015500                                                                          
015600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
015700 01  GENERAL-SUBPROGRAMS.                                                 
015800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016000     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
016010     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
016300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016400     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
016500     03  BAQCSTUB                PIC X(8)    VALUE 'BAQCSTUB'.            
016600     03  BAQCTERM                PIC X(8)    VALUE 'BAQCTERM'.            
016700     03  WZ11OUTM                PIC X(8)    VALUE 'WZ11OUTM'.            
016800     EJECT                                                                
016900*01  -COPY WZ11OUTM                                                       
017000     EJECT                                                                
017100*    --- PARAMETERS TO VIMSID                                             
017200 01 WS-VIMSID                PIC X(8)  VALUE SPACE.                       
017300 01 FILLER REDEFINES WS-VIMSID.                                           
017400    03 IMS-REGION            PIC X(3).                                    
017500       88 TEST-REGION                  VALUE 'IMD' 'IMP' 'IMY'            
017600                                             'IMB'.                       
017700       88 ENV-DEVE                     VALUE 'IMP'.                       
017800       88 ENV-IGRT                     VALUE 'IMY'.                       
017900       88 ENV-XDEV                     VALUE 'IMD'.                       
018000       88 ENV-ACPT                     VALUE 'IMB'.                       
018100       88 ENV-PROD                     VALUE 'IMG' 'IMR'.                 
018200    03 FILLER                PIC X(5).                                    
018300                                                                          
018310     EJECT                                                                
018320 01  -COPY WWDC99                                                         
018330     EJECT                                                                
018340 01  -COPY WWDCKONS                                                       
018350                                                                          
018400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
018500*01  -COPY WMEDAREA                                                       
018600                                                                          
018700*    --- PARAMETERS FOR SUBPROGRAM W005INIT                               
018801 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018803*01  -COPY WMSGINIT                                                       
018804                                                                          
018810     EJECT                                                                
018820*    --- PARAMETERS FOR SUBPROGRAM WDAGKONV                               
018821 01  FILLER                      PIC X(16)   VALUE 'WDAGKONV'.            
018830*01  -COPY WDAGAREA                                                       
018840                                                                          
019300*                                                                         
019400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
019500     SKIP3                                                                
019600*01  -COPY WZ01SUB                                                        
019700*                                                                         
019800     SKIP3                                                                
019900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
020000 01  REQU-AREA.                                                           
020100*    03  -COPY WZ01REQ2                                                   
020200*    03  -COPY W6I39101                                                   
020300     EJECT                                                                
020400 01  RESP-AREA.                                                           
020500*    03  -COPY WZ01RESP                                                   
020600                                                                          
020700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020800                                                                          
020900*01  -COPY WMSGAREA                                                       
021000     EJECT                                                                
021100**************************************************************            
021200 01  P44-API-START               PIC X(24)   VALUE                        
021300                                             'P44-API-START'.             
021400                                                                          
021500 01 BAQ-REQUEST-PTR                          USAGE POINTER.               
021600 01 BAQ-REQUEST-LEN              PIC S9(9)   COMP-5 SYNC.                 
021700 01 BAQ-RESPONSE-PTR                         USAGE POINTER.               
021800 01 BAQ-RESPONSE-LEN             PIC S9(9)   COMP-5 SYNC.                 
021900                                                                          
022000*                                                                         
022100*   -COPY BAQRINFO                                                        
022200*                                                                         
022300*** API INFO                                                              
022400 01 P44-APINAME                  PIC X(36) VALUE                          
022500                           'PULS-Project44-ContainerTracking_1.0'.        
022600 01 P44-APINAME-LEN              PIC 9(4)  VALUE 36.                      
022700*** API INFO for archive subscription                                     
022800 01 P44-APIMETHOD-ARCH           PIC X(4)  VALUE 'POST'.                  
022900 01 P44-APIMETHOD-LEN-ARCH       PIC 9(4)  VALUE 4.                       
023000 01 P44-APIPATH-ARCH             PIC X(255)  VALUE                        
023100              '%2Fb2b%2Fpuls%2Fproject44%2Fcontainertracking%2Fv2%        
023200-     '2Fsubscriptions%2F%7Bsubscription_id%7D%2Farchive%2F'.             
023300 01 P44-APIPATH-LEN-ARCH         PIC 9(4)   VALUE 103.                    
023400*** API INFO TO STOP PUSHEVENTS (CER)                                     
023500 01 P44-APIMETHOD-CER            PIC X(4)  VALUE 'PUT'.                   
023600 01 P44-APIMETHOD-LEN-CER        PIC 9(4)  VALUE 3.                       
023700 01 P44-APIPATH-CER              PIC X(100) VALUE                         
023800              '%2Fb2b%2Fpuls%2Fproject44%2Fcontainertracking%2Fv2%        
023900-             '2Fshipments%2F%7Bshipment_id%7D%2Fempty_return%2F'.        
024000 01 P44-APIPATH-LEN-CER          PIC 9(4)   VALUE 100.                    
024100                                                                          
024200*** PROXY KEYS FOR DIFFERENT ENVIROMENTS                                  
024300 01 P44-PROXYKEY-PROD            PIC X(32) VALUE                          
024400          '********************************'.                             
024500 01 P44-PROXYKEY-PROD-LEN       PIC 9(04) VALUE 32.                       
024600 01 P44-PROXYKEY-QA              PIC X(32) VALUE                          
024700          '********************************'.                             
024800 01 P44-PROXYKEY-QA-LEN         PIC 9(04) VALUE 32.                       
024900 01 P44-PROXYKEY-TEST            PIC X(32) VALUE                          
025000          '********************************'.                             
025100 01 P44-PROXYKEY-TEST-LEN       PIC 9(04) VALUE 32.                       
025200                                                                          
025300 01 API-INFO.                                                             
025400    03 BAQ-APINAME               PIC X(255).                              
025500    03 BAQ-APINAME-LEN           PIC S9(9) COMP-5 SYNC.                   
025600    03 BAQ-APIPATH               PIC X(255).                              
025700    03 BAQ-APIPATH-LEN           PIC S9(9) COMP-5 SYNC.                   
025800    03 BAQ-APIMETHOD             PIC X(255).                              
025900    03 BAQ-APIMETHOD-LEN         PIC S9(9) COMP-5 SYNC.                   
026000                                                                          
026100 01  API-REQUEST                 PIC X(10000).                            
026200                                                                          
026300 01  API-RESPONSE                PIC X(100000000).                        
026400                                                                          
026500 01  P44-ARCH-REQU.                                                       
026600*    03 -COPY W6122703 -PRE ARCH-                                         
026700                                                                          
026800 01  P44-CER-REQU.                                                        
026900*    03 -COPY W6122702 -PRE CER-                                          
027000                                                                          
027100 01  RESPONSE-AREA-ARCH.                                                  
027200     03  idsubscr-arch           PIC 9(11).                               
027300     03  detail-err-arch         PIC X(255).                              
027400     03  non_field_errors        PIC X(255).                              
027500                                                                          
027600 01  RESPONSE-AREA-CER.                                                   
027700     03  empty_return_customer   PIC X(10).                               
027800     03  detail-err-cer          PIC X(255).                              
027900**************************************************************            
028000                                                                          
028100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
028200*                                                                         
028300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028400 01  KEYS-TILL-DLI.                                                       
028500     03  W-6301-IDHTYP           PIC X(4)    VALUE '6301'.                
028600     03  W-6302-IDLBBET-X        PIC X(12)   VALUE SPACE.                 
028700     03  W-6302-IDSUBSCR-X.                                               
028800         05 W-6302-IDSUBSCR      PIC S9(11)  COMP-3 VALUE ZERO.           
028810     03  W-IDDC-REC-X.                                                    
028820         05  W-IDDC-REC          PIC X(2)    VALUE SPACE.                 
028830     03  W-IDDC-SEND-X.                                                   
028840         05  W-IDDC-SEND         PIC X(2)    VALUE SPACE.                 
028900                                                                          
029000*    --- STATUS CODES FROM IMS                                            
029100 01  STATUS-WS                   PIC XX.                                  
029200     88  SEGMENT-FOUND                       VALUE '  '.                  
029300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
029400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
029500     88  SEGMENT-END                         VALUE 'GB'.                  
029600                                                                          
029700 01  GOOD-STATUSCODES.                                                    
029800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029900                                                                          
030000 01  ALL-SSA.                                                             
030100     03 SSA1                     PIC X(128).                              
030200     03 SSA2                     PIC X(128).                              
030300                                                                          
030400*    --- IMS FUNCTION CODES                                               
030500*01  -COPY W0003                                                          
030600                                                                          
030700*    ---  DLI INPUT-OUTPUT AREA                                           
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6302'.                    
031200 01  DLI-IO-WDGX6302.                                                     
031300*    03  -COPY WDGX6302                                                   
031310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
031320 01  DLI-IO-WDB601.                                                       
031330*    03  -COPY WDB601                                                     
031340     EJECT                                                                
031350 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
031360 01  DLI-IO-WDB616.                                                       
031370*    03  -COPY WDB616                                                     
031392 01  FILLER         PIC X(17) VALUE 'DLI-IO-WDGX6302-2'.                  
031393 01  DLI-IO-WDGX6302-2.                                                   
031394*    03  -COPY WDGX6302 -PRE 6301-2-                                      
031400                                                                          
031500                                                                          
031600 LINKAGE SECTION.                                                         
031700*01  -COPY W0009   -PRE MSG-                                              
031800                                                                          
031900*01  -COPY W0008   -PRE 6301-                                             
031910     05  6301-KFB-IDHTYP         PIC X(4).                                
031920     05  6301-KFB-IDDC           PIC X(2).                                
032100                                                                          
032101*01  -COPY W0008  -PRE WDB6-                                              
032102     05  FILLER                  PIC X.                                   
032103                                                                          
032104*01  -COPY W0008   -PRE 6301-2-                                           
032105     05  FILLER                  PIC X.                                   
032110     EJECT                                                                
032200 PROCEDURE DIVISION  USING MSG-PCB  6301-PCB WDB6-PCB 6301-2-PCB.         
032300 MAIN SECTION.                                                            
032400     ENTRY 'DLITCBL' USING MSG-PCB  6301-PCB WDB6-PCB 6301-2-PCB.         
032500                                                                          
032600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
032700     IF SUB-KDRC = 0                                                      
032800         PERFORM A-INIT                                                   
032900         PERFORM B-REC-PUSHEVNT                                           
033000     END-IF                                                               
033100                                                                          
033200     IF MAIL-OPEN-YES                                                     
033300       PERFORM S91-SEND-MAIL-CLOSE                                        
033400     END-IF                                                               
033500                                                                          
033600     PERFORM S02-RETURN-RESPONSE                                          
033700                                                                          
033800     MOVE ZERO TO RETURN-CODE                                             
033900     GOBACK                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 A-INIT SECTION.                                                          
034300                                                                          
034400     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
034500                                                                          
034600     ACCEPT CURRENT-DATE   FROM DATE                                      
034700     ACCEPT CURRENT-TIME   FROM TIME                                      
034800                                                                          
034900     MOVE CURRENT-DATE-YEAR  TO P44-CER-YEAR                              
035000     MOVE CURRENT-DATE-MONTH TO P44-CER-MONTH                             
035100     MOVE CURRENT-DATE-DAY   TO P44-CER-DAY                               
035200                                                                          
035300     CALL VIMSID USING WS-VIMSID                                          
035400     .                                                                    
035500     EJECT                                                                
035600 B-REC-PUSHEVNT SECTION.                                                  
035700                                                                          
035800     IF MID-IDEVENT = 'SUBSCRIPTIONEVENT'                                 
035900       PERFORM BA-SUBSCR-EVENT                                            
036000                                                                          
036100       MOVE MID-IDSUBSCR-ARCH TO W-6302-IDSUBSCR                          
036400       PERFORM IMS-GHN-WDGX6302-SUB                                       
036500       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                       
036600           MOVE CURRENT-DATE TO 6302-TILST-PUSHEVNT                       
036700           PERFORM IMS-REPL-WDGX6302                                      
036800           PERFORM IMS-GHN-WDGX6302-SUB                                   
036900       END-PERFORM                                                        
037600     END-IF                                                               
037700                                                                          
037800     IF MID-IDEVENT = 'CONTAINERSHIPMENTEVENT'                            
037900       PERFORM BB-CONTAINER-EVENT                                         
038000                                                                          
038010       IF CER-TYPE                                                        
038011         PERFORM S04-CALL-BAQCTERM                                        
038012       ELSE                                                               
038013         PERFORM IMS-GHN-WDGX6302-CON                                     
038300         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                     
038301          IF 6302-KDTRPSTA = SPACE                                        
038303           MOVE 6301-KFB-IDDC      TO W-IDDC-REC                          
038310           IF 6302-IDDC-LEV > SPACE                                       
038320             MOVE 6302-IDDC-LEV    TO W-IDDC-SEND                         
038330           ELSE                                                           
038331             MOVE 6302-IDDC-SEND   TO WS-IDDC                             
038332             IF DDC-SE                                                    
038333                MOVE WC-CDC-SE     TO W-IDDC-SEND                         
038334             ELSE                                                         
038340               MOVE 6302-IDDC-SEND TO W-IDDC-SEND                         
038351             END-IF                                                       
038352           END-IF                                                         
038360           PERFORM IMS-GU-WDB616                                          
038370           IF SEGMENT-FOUND AND                                           
038371              WS-DISCH-ETA >= 6302-TIFAKT                                 
038372             MOVE 001                  TO DAG-KDCALL                      
038373             MOVE 6302-TIFAKT          TO DAG-TIAAMMDD-FOM                
038374             MOVE WS-DISCH-ETA         TO DAG-TIAAMMDD-TOM                
038378                                                                          
038379             CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                
038380                                 DAG-KDSVAR                               
038381                                                                          
038382             IF DAG-KDSVAR = 'F'                                          
038383               MOVE 'WRONG ANSWER FROM WDAGKONV' TO ERROR-TEXT            
038384               CALL ABEND USING RKOD-ABEND-WITH-DUMP                      
038600             END-IF                                                       
038601                                                                          
038602             COMPUTE WS-LEADTIMES-BOAT ROUNDED =                          
038603             ((REF-KVDLTID-BOATPAC * 7 / 5)                               
038604               + REF-KVDLTID-BOATTRP) * 0.5                               
038605                                                                          
038606             IF DAG-KVKALDAG < WS-LEADTIMES-BOAT                          
038607               CONTINUE                                                   
038608             ELSE                                                         
038609               MOVE CURRENT-DATE      TO 6302-TILST-PUSHEVNT              
038610               MOVE MID-IDCONTNR      TO 6302-IDCONTNR                    
038611               MOVE WS-LIFDEPPL-NUM   TO 6302-DABERANK-LIFDEPPL           
038612               MOVE WS-LIFDEPAC-NUM   TO 6302-DABERANK-LIFDEPAC           
038613               MOVE WS-PODDEPPL-NUM   TO 6302-DABERANK-PODDEPPL           
038614               MOVE WS-PODDEPAC-NUM   TO 6302-DABERANK-PODDEPAC           
038615               MOVE WS-DLVDELPL-NUM   TO 6302-DABERANK-DLVDELPL           
038616               MOVE WS-DLVDELAC-NUM   TO 6302-DABERANK-DLVDELAC           
038617               MOVE WS-PODDISPL-NUM   TO 6302-DABERANK-PODDISPL           
038618               MOVE WS-PODDISAC-NUM   TO 6302-DABERANK-PODDISAC           
038619               MOVE WS-PODARRPL-NUM   TO 6302-DABERANK-PODARRPL           
038620               MOVE WS-LIFARRAC-NUM   TO 6302-DABERANK-LIFARRAC           
038621               MOVE WS-DISCH-ETA      TO 6302-DABERANK-DISCH              
038622               PERFORM IMS-REPL-WDGX6302                                  
038624             END-IF                                                       
038625           END-IF                                                         
038700          END-IF                                                          
040700          PERFORM IMS-GHN-WDGX6302-CON                                    
040800         END-PERFORM                                                      
041100       END-IF                                                             
041400     END-IF                                                               
041500     .                                                                    
041600     EJECT                                                                
041700 BA-SUBSCR-EVENT SECTION.                                                 
041800                                                                          
041900     IF MID-KDSTASUB = 2                                                  
042000**** Archive transaction to Project44 ***                                 
042100        PERFORM C-PREP-P44-CALL                                           
042110        PERFORM S04-CALL-BAQCTERM                                         
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 BB-CONTAINER-EVENT SECTION.                                              
042600                                                                          
042700     MOVE NOO TO CER-SW                                                   
042800     MOVE MID-IDLBBET TO W-6302-IDLBBET-X                                 
042900     PERFORM IMS-GN-WDGX6302-CON                                          
043000     IF SEGMENT-FOUND                                                     
043100        PERFORM BBA-MOVE-ETADATES                                         
043200        PERFORM BBB-CHECK-ETADATES                                        
043300     ELSE                                                                 
043400       MOVE YES TO CER-SW                                                 
043500       PERFORM C-PREP-P44-CALL                                            
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900 BBA-MOVE-ETADATES SECTION.                                               
044000                                                                          
044100     MOVE MID-DABERANK-LIFDEPPL(3:2) TO WS-LIFDEPPL-YEAR                  
044200     MOVE MID-DABERANK-LIFDEPPL(6:2) TO WS-LIFDEPPL-MONTH                 
044300     MOVE MID-DABERANK-LIFDEPPL(9:2) TO WS-LIFDEPPL-DAY                   
044400     IF WS-LIFDEPPL NOT NUMERIC                                           
044500       MOVE ZERO TO WS-LIFDEPPL                                           
044600     END-IF                                                               
044700                                                                          
044800     MOVE MID-DABERANK-LIFDEPAC(3:2) TO WS-LIFDEPAC-YEAR                  
044900     MOVE MID-DABERANK-LIFDEPAC(6:2) TO WS-LIFDEPAC-MONTH                 
045000     MOVE MID-DABERANK-LIFDEPAC(9:2) TO WS-LIFDEPAC-DAY                   
045100     IF WS-LIFDEPAC NOT NUMERIC                                           
045200       MOVE ZERO TO WS-LIFDEPAC                                           
045300     END-IF                                                               
045400                                                                          
045500     MOVE MID-DABERANK-PODDEPPL(3:2) TO WS-PODDEPPL-YEAR                  
045600     MOVE MID-DABERANK-PODDEPPL(6:2) TO WS-PODDEPPL-MONTH                 
045700     MOVE MID-DABERANK-PODDEPPL(9:2) TO WS-PODDEPPL-DAY                   
045800     IF WS-PODDEPPL NOT NUMERIC                                           
045900       MOVE ZERO TO WS-PODDEPPL                                           
046000     END-IF                                                               
046100                                                                          
046200     MOVE MID-DABERANK-PODDEPAC(3:2) TO WS-PODDEPAC-YEAR                  
046300     MOVE MID-DABERANK-PODDEPAC(6:2) TO WS-PODDEPAC-MONTH                 
046400     MOVE MID-DABERANK-PODDEPAC(9:2) TO WS-PODDEPAC-DAY                   
046500     IF WS-PODDEPAC NOT NUMERIC                                           
046600       MOVE ZERO TO WS-PODDEPAC                                           
046700     END-IF                                                               
046800                                                                          
046900     MOVE MID-DABERANK-DLVDELPL(3:2) TO WS-DLVDELPL-YEAR                  
047000     MOVE MID-DABERANK-DLVDELPL(6:2) TO WS-DLVDELPL-MONTH                 
047100     MOVE MID-DABERANK-DLVDELPL(9:2) TO WS-DLVDELPL-DAY                   
047200     IF WS-DLVDELPL NOT NUMERIC                                           
047300       MOVE ZERO TO WS-DLVDELPL                                           
047400     END-IF                                                               
047500                                                                          
047600     MOVE MID-DABERANK-DLVDELAC(3:2) TO WS-DLVDELAC-YEAR                  
047700     MOVE MID-DABERANK-DLVDELAC(6:2) TO WS-DLVDELAC-MONTH                 
047800     MOVE MID-DABERANK-DLVDELAC(9:2) TO WS-DLVDELAC-DAY                   
047900     IF WS-DLVDELAC NOT NUMERIC                                           
048000       MOVE ZERO TO WS-DLVDELAC                                           
048100     END-IF                                                               
048200                                                                          
048300     MOVE MID-DABERANK-PODDISPL(3:2) TO WS-PODDISPL-YEAR                  
048400     MOVE MID-DABERANK-PODDISPL(6:2) TO WS-PODDISPL-MONTH                 
048500     MOVE MID-DABERANK-PODDISPL(9:2) TO WS-PODDISPL-DAY                   
048600     IF WS-PODDISPL NOT NUMERIC                                           
048700       MOVE ZERO TO WS-PODDISPL                                           
048800     END-IF                                                               
048900                                                                          
049000     MOVE MID-DABERANK-PODDISAC(3:2) TO WS-PODDISAC-YEAR                  
049100     MOVE MID-DABERANK-PODDISAC(6:2) TO WS-PODDISAC-MONTH                 
049200     MOVE MID-DABERANK-PODDISAC(9:2) TO WS-PODDISAC-DAY                   
049300     IF WS-PODDISAC NOT NUMERIC                                           
049400       MOVE ZERO TO WS-PODDISAC                                           
049500     END-IF                                                               
049600                                                                          
049700     MOVE MID-DABERANK-PODARRPL(3:2) TO WS-PODARRPL-YEAR                  
049800     MOVE MID-DABERANK-PODARRPL(6:2) TO WS-PODARRPL-MONTH                 
049900     MOVE MID-DABERANK-PODARRPL(9:2) TO WS-PODARRPL-DAY                   
050000     IF WS-PODARRPL NOT NUMERIC                                           
050100       MOVE ZERO TO WS-PODARRPL                                           
050200     END-IF                                                               
050300                                                                          
050400     MOVE MID-DABERANK-LIFARRAC(3:2) TO WS-LIFARRAC-YEAR                  
050500     MOVE MID-DABERANK-LIFARRAC(6:2) TO WS-LIFARRAC-MONTH                 
050600     MOVE MID-DABERANK-LIFARRAC(9:2) TO WS-LIFARRAC-DAY                   
050700     IF WS-LIFARRAC NOT NUMERIC                                           
050800       MOVE ZERO TO WS-LIFARRAC                                           
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 BBB-CHECK-ETADATES SECTION.                                              
051300                                                                          
051400     MOVE ZERO TO WS-DISCH-ETA                                            
051500     IF WS-LIFDEPPL > WS-DISCH-ETA                                        
051600       MOVE WS-LIFDEPPL TO WS-DISCH-ETA                                   
051700     END-IF                                                               
051800     IF WS-LIFDEPAC > WS-DISCH-ETA                                        
051900       MOVE WS-LIFDEPAC TO WS-DISCH-ETA                                   
052000     END-IF                                                               
052100     IF WS-PODDEPPL > WS-DISCH-ETA                                        
052200       MOVE WS-PODDEPPL TO WS-DISCH-ETA                                   
052300     END-IF                                                               
052400     IF WS-PODDEPAC > WS-DISCH-ETA                                        
052500       MOVE WS-PODDEPAC TO WS-DISCH-ETA                                   
052600     END-IF                                                               
052700     IF WS-DLVDELPL > WS-DISCH-ETA                                        
052800       MOVE WS-DLVDELPL TO WS-DISCH-ETA                                   
052900     END-IF                                                               
053000     IF WS-DLVDELAC > WS-DISCH-ETA                                        
053100       MOVE WS-DLVDELAC TO WS-DISCH-ETA                                   
053200     END-IF                                                               
053300     IF WS-PODDISPL > WS-DISCH-ETA                                        
053400       MOVE WS-PODDISPL TO WS-DISCH-ETA                                   
053500     END-IF                                                               
053600     IF WS-PODDISAC > WS-DISCH-ETA                                        
053700       MOVE WS-PODDISAC TO WS-DISCH-ETA                                   
053800     END-IF                                                               
053900     IF WS-PODARRPL > WS-DISCH-ETA                                        
054000       MOVE WS-PODARRPL TO WS-DISCH-ETA                                   
054100     END-IF                                                               
054200     IF WS-LIFARRAC > WS-DISCH-ETA                                        
054300       MOVE WS-LIFARRAC TO WS-DISCH-ETA                                   
054400     END-IF                                                               
054500                                                                          
054600     .                                                                    
054700     EJECT                                                                
054800 C-PREP-P44-CALL SECTION.                                                 
054900                                                                          
055000     MOVE SPACE TO API-REQUEST                                            
055100                   API-RESPONSE                                           
055200                                                                          
055300                                                                          
055400     PERFORM CA-SET-PROXYKEY                                              
055500                                                                          
055600     MOVE P44-APINAME       TO BAQ-APINAME                                
055700     MOVE P44-APINAME-LEN   TO BAQ-APINAME-LEN                            
055800                                                                          
055900     IF MID-IDEVENT = 'SUBSCRIPTIONEVENT'                                 
056000        MOVE P44-APIMETHOD-ARCH      TO BAQ-APIMETHOD                     
056100        MOVE P44-APIMETHOD-LEN-ARCH  TO BAQ-APIMETHOD-LEN                 
056200        MOVE P44-APIPATH-ARCH        TO BAQ-APIPATH                       
056300        MOVE P44-APIPATH-LEN-ARCH    TO BAQ-APIPATH-LEN                   
056400                                                                          
056500        MOVE MID-IDSUBSCR-ARCH TO ARCH-SUBSCRIPTION-ID                    
056600                                                                          
056700        MOVE P44-ARCH-REQU TO API-REQUEST                                 
056800     END-IF                                                               
056900                                                                          
057000     IF MID-IDEVENT = 'CONTAINERSHIPMENTEVENT' AND CER-TYPE               
057100        MOVE P44-APIMETHOD-CER       TO BAQ-APIMETHOD                     
057200        MOVE P44-APIMETHOD-LEN-CER   TO BAQ-APIMETHOD-LEN                 
057300        MOVE P44-APIPATH-CER         TO BAQ-APIPATH                       
057400        MOVE P44-APIPATH-LEN-CER     TO BAQ-APIPATH-LEN                   
057500                                                                          
057600        MOVE MID-IDCONTNR  TO CER-SHIPMENT-ID                             
057700                                                                          
057800        MOVE 1             TO CER-EMPTY-RETURN-CUSTOMER-NUM               
057900        MOVE 10            TO CER-EMPTY-RETURN-CUSTOMER-LEN               
058000        MOVE P44-CER-DATE  TO CER-EMPTY-RETURN-CUSTOMER2                  
058100                                                                          
058200        MOVE P44-CER-REQU  TO API-REQUEST                                 
058300     END-IF                                                               
058400                                                                          
058500     SET BAQ-REQUEST-PTR TO ADDRESS OF API-REQUEST                        
058600     MOVE LENGTH OF API-REQUEST TO BAQ-REQUEST-LEN                        
058700     SET BAQ-RESPONSE-PTR TO ADDRESS OF API-RESPONSE                      
058800     MOVE LENGTH OF API-RESPONSE TO BAQ-RESPONSE-LEN                      
058900                                                                          
059000     PERFORM S03-CALL-BAQCSTUB                                            
059100     IF BAQ-SUCCESS                                                       
059200       IF BAQ-RESPONSE-LEN > ZERO                                         
059300         IF MID-IDEVENT = 'SUBSCRIPTIONEVENT'                             
059400           PERFORM CB-PARSE-JSON-ARCH                                     
059500         ELSE                                                             
059600           PERFORM CC-PARSE-JSON-CER                                      
059700         END-IF                                                           
059800       END-IF                                                             
059900     ELSE                                                                 
060000       IF BAQ-RESPONSE-LEN > ZERO                                         
060100         IF MID-IDEVENT = 'SUBSCRIPTIONEVENT'                             
060200           PERFORM CB-PARSE-JSON-ARCH                                     
060300         ELSE                                                             
060400           PERFORM CC-PARSE-JSON-CER                                      
060500         END-IF                                                           
060600       END-IF                                                             
060700     END-IF                                                               
060800     .                                                                    
060900                                                                          
061000     EJECT                                                                
061100 CA-SET-PROXYKEY SECTION.                                                 
061200     IF MID-IDEVENT = 'SUBSCRIPTIONEVENT'                                 
061300       EVALUATE TRUE                                                      
061400         WHEN ENV-DEVE OR ENV-IGRT OR ENV-XDEV                            
061500          MOVE P44-PROXYKEY-TEST     TO ARCH-PROXY-KEY                    
061600          MOVE P44-PROXYKEY-TEST-LEN TO ARCH-PROXY-KEY-LENGTH             
061700         WHEN ENV-ACPT                                                    
061800          MOVE P44-PROXYKEY-QA       TO ARCH-PROXY-KEY                    
061900          MOVE P44-PROXYKEY-QA-LEN   TO ARCH-PROXY-KEY-LENGTH             
062000         WHEN ENV-PROD                                                    
062100          MOVE P44-PROXYKEY-PROD     TO ARCH-PROXY-KEY                    
062200          MOVE P44-PROXYKEY-PROD-LEN TO ARCH-PROXY-KEY-LENGTH             
062300       END-EVALUATE                                                       
062400     ELSE                                                                 
062500       EVALUATE TRUE                                                      
062600         WHEN ENV-DEVE OR ENV-IGRT OR ENV-XDEV                            
062700          MOVE P44-PROXYKEY-TEST     TO CER-PROXY-KEY                     
062800          MOVE P44-PROXYKEY-TEST-LEN TO CER-PROXY-KEY-LENGTH              
062900         WHEN ENV-ACPT                                                    
063000          MOVE P44-PROXYKEY-QA       TO CER-PROXY-KEY                     
063100          MOVE P44-PROXYKEY-QA-LEN   TO CER-PROXY-KEY-LENGTH              
063200         WHEN ENV-PROD                                                    
063300          MOVE P44-PROXYKEY-PROD     TO CER-PROXY-KEY                     
063400          MOVE P44-PROXYKEY-PROD-LEN TO CER-PROXY-KEY-LENGTH              
063500       END-EVALUATE                                                       
063600     END-IF                                                               
063700     .                                                                    
063800                                                                          
063900     EJECT                                                                
064000 CB-PARSE-JSON-ARCH SECTION.                                              
064100     MOVE SPACE TO RESPONSE-AREA-ARCH                                     
064200                                                                          
064300     JSON PARSE API-RESPONSE(1:BAQ-RESPONSE-LEN)                          
064400       INTO RESPONSE-AREA-ARCH                                            
064500       NAME OF idsubscr-arch    "subscription_id"                         
064600               detail-err-arch  "detail"                                  
064700       RESPONSE-AREA-ARCH OMITTED                                         
064800     PERFORM S99-JSON-CODE-VALIDATION                                     
064900     .                                                                    
065000                                                                          
065100     EJECT                                                                
065200 CC-PARSE-JSON-CER SECTION.                                               
065300     MOVE SPACE TO RESPONSE-AREA-CER                                      
065400                                                                          
065500     JSON PARSE API-RESPONSE(1:BAQ-RESPONSE-LEN)                          
065600       INTO RESPONSE-AREA-CER                                             
065700       NAME OF detail-err-cer "detail"                                    
065800       RESPONSE-AREA-CER OMITTED                                          
065900     PERFORM S99-JSON-CODE-VALIDATION                                     
066000     .                                                                    
066100                                                                          
066200     EJECT                                                                
066300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
066400                                                                          
066500     MOVE 'GETARG'               TO SUB-KDFUNC                            
066600     MOVE 'CARPARTS.PULS.PROJECT44EVENT'                                  
066700                                 TO SUB-ADDISPABS                         
066800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
066900                                                                          
067000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
067100                                                                          
067200     IF SUB-KDRC > 0                                                      
067300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
067400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
067500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
067600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
067700     END-IF                                                               
067800     .                                                                    
067900                                                                          
068000     EJECT                                                                
068100 S02-RETURN-RESPONSE SECTION.                                             
068200                                                                          
068300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
068400     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA                             
068500                                                                          
068600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
068700                                                                          
068800     IF SUB-KDRC > 0                                                      
068900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
069000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
069100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
069200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
069300     END-IF                                                               
069400     .                                                                    
069500 S03-CALL-BAQCSTUB SECTION.                                               
069600     SKIP2                                                                
069700*CALL API VIA z/OS CONNECT                                                
069800     CALL BAQCSTUB USING API-INFO                                         
069900                         BAQ-REQUEST-INFO                                 
070000                         BAQ-REQUEST-PTR                                  
070100                         BAQ-REQUEST-LEN                                  
070200                         BAQ-RESPONSE-INFO                                
070300                         BAQ-RESPONSE-PTR                                 
070400                         BAQ-RESPONSE-LEN                                 
070500                                                                          
070600     IF BAQ-SUCCESS                                                       
070700       CONTINUE                                                           
070800     ELSE                                                                 
070900       EVALUATE TRUE                                                      
071000         WHEN BAQ-ERROR-IN-API                                            
071100           MOVE 'E01'            TO WS-BAQCSTUB-RC                        
071200         WHEN BAQ-ERROR-IN-ZCEE                                           
071300           MOVE 'E02'            TO WS-BAQCSTUB-RC                        
071400         WHEN BAQ-ERROR-IN-STUB                                           
071500           MOVE 'E03'            TO WS-BAQCSTUB-RC                        
071600       END-EVALUATE                                                       
071700       PERFORM S81-TRACE1                                                 
071800     END-IF                                                               
071900     .                                                                    
072000                                                                          
072100     EJECT                                                                
072200 S04-CALL-BAQCTERM SECTION.                                               
072300*CLOSE AND CLEAR CACHED CONNECTION                                        
072400     INITIALIZE BAQ-RESPONSE-INFO                                         
072500     CALL BAQCTERM USING BAQ-RESPONSE-INFO                                
072600                                                                          
072700     IF BAQ-SUCCESS                                                       
072800       CONTINUE                                                           
072900     ELSE                                                                 
073000       EVALUATE TRUE                                                      
073100         WHEN BAQ-ERROR-IN-API                                            
073200           MOVE 'T01'            TO WS-BAQCTERM-RC                        
073300         WHEN BAQ-ERROR-IN-ZCEE                                           
073400           MOVE 'T02'            TO WS-BAQCTERM-RC                        
073500         WHEN BAQ-ERROR-IN-STUB                                           
073600           MOVE 'T03'            TO WS-BAQCTERM-RC                        
073700       END-EVALUATE                                                       
073800       PERFORM S81-TRACE2                                                 
073900     END-IF                                                               
074000     .                                                                    
074100                                                                          
074200 S81-TRACE1 SECTION.                                                      
074201                                                                          
074210     IF MAIL-OPEN-NO                                                      
074220       PERFORM S91-SEND-MAIL-OPEN                                         
074230       PERFORM S91-SEND-MAIL-LINE                                         
074240       SET MAIL-OPEN-YES         TO TRUE                                  
074250     END-IF                                                               
074260                                                                          
074300     STRING 'PGM ERROR CODE   = ' WS-BAQCSTUB-RC                          
074400                       DELIMITED BY SIZE                                  
074500                               INTO OUTM-TEOUTDATA                        
074600     MOVE 80                     TO OUTM-TEOUTDATA-L                      
074700     PERFORM S91-SEND-MAIL-LINE2                                          
074800     MOVE BAQ-STATUS-CODE        TO WS-NUM-DISPLAY                        
074900     STRING 'BAQ STATUS CODE  = ' WS-NUM-DISPLAY                          
075000                       DELIMITED BY SIZE                                  
075100                               INTO OUTM-TEOUTDATA                        
075200     MOVE 80                     TO OUTM-TEOUTDATA-L                      
075300     PERFORM S91-SEND-MAIL-LINE2                                          
075400     STRING 'BAQ STATUS MSG   = ' BAQ-STATUS-MESSAGE                      
075500                       DELIMITED BY SIZE                                  
075600                               INTO OUTM-TEOUTDATA                        
075700     MOVE 200                    TO OUTM-TEOUTDATA-L                      
075800     PERFORM S91-SEND-MAIL-LINE2                                          
075900     IF MID-IDEVENT = 'SUBSCRIPTIONEVENT'                                 
076000       STRING 'API RESPONSE     = ' RESPONSE-AREA-ARCH                    
076100                         DELIMITED BY SIZE                                
076200                                 INTO OUTM-TEOUTDATA                      
076300       MOVE   541                    TO OUTM-TEOUTDATA-L                  
076400     ELSE                                                                 
076500       STRING 'API RESPONSE     = ' RESPONSE-AREA-CER                     
076600                         DELIMITED BY SIZE                                
076700                                 INTO OUTM-TEOUTDATA                      
076800       MOVE   285                    TO OUTM-TEOUTDATA-L                  
076900     END-IF                                                               
077000     PERFORM S91-SEND-MAIL-LINE2                                          
077100     .                                                                    
077200                                                                          
077300 S81-TRACE2 SECTION.                                                      
077301                                                                          
077310     IF MAIL-OPEN-NO                                                      
077320       PERFORM S91-SEND-MAIL-OPEN                                         
077330       PERFORM S91-SEND-MAIL-LINE                                         
077340       SET MAIL-OPEN-YES         TO TRUE                                  
077350     END-IF                                                               
077360                                                                          
077400     MOVE 'TERM CONNECTION : '   TO OUTM-TEOUTDATA                        
077500     MOVE 80                     TO OUTM-TEOUTDATA-L                      
077600     PERFORM S91-SEND-MAIL-LINE2                                          
077700     STRING 'PGM ERROR CODE   = ' WS-BAQCTERM-RC                          
077800                       DELIMITED BY SIZE                                  
077900                               INTO OUTM-TEOUTDATA                        
078000     MOVE 80                     TO OUTM-TEOUTDATA-L                      
078100     PERFORM S91-SEND-MAIL-LINE2                                          
078200     MOVE BAQ-STATUS-CODE        TO WS-NUM-DISPLAY                        
078300     STRING 'BAQ STATUS CODE  = ' WS-NUM-DISPLAY                          
078400                       DELIMITED BY SIZE                                  
078500                               INTO OUTM-TEOUTDATA                        
078600     MOVE 80                     TO OUTM-TEOUTDATA-L                      
078700     PERFORM S91-SEND-MAIL-LINE2                                          
078800     STRING 'BAQ STATUS MSG   = ' BAQ-STATUS-MESSAGE                      
078900                       DELIMITED BY SIZE                                  
079000                               INTO OUTM-TEOUTDATA                        
079100     MOVE 200                    TO OUTM-TEOUTDATA-L                      
079200     PERFORM S91-SEND-MAIL-LINE2                                          
079300     .                                                                    
079400                                                                          
081300 S91-SEND-MAIL-OPEN SECTION.                                              
081400                                                                          
081500     MOVE SPACE                  TO OUTM-WZ11OUT                          
081600                                                                          
081700     MOVE 1                      TO OUTM-IDCALL                           
081800     MOVE 'OPEN'                 TO OUTM-KDFUNC                           
081900     MOVE ZERO                   TO OUTM-KDRC                             
082000                                                                          
082100     MOVE 'HAKAN.BOHLIN@VOLVOCARS.COM'                                    
082200                                 TO OUTM-IDOUTDEST                        
082300     MOVE '.TXT'                 TO OUTM-IDPFDEF                          
082400     MOVE SPACES                 TO OUTM-FLCARRCNTL                       
082500     STRING 'NO.REPLY.' IMS-REGION '@VOLVOCARS.COM'                       
082600                       DELIMITED BY SIZE                                  
082700                               INTO OUTM-IDMAIL-SENDER                    
082800     MOVE 'PUSHEVENT PROJECT44'  TO OUTM-IDMAILTTL                        
082900                                                                          
083000*    -- INITIALIZE ADDL INFO FIELDS. THESE ARE NOT USED                   
083100*    -- AS RESTART IS NOT AVAILABLE HERE.                                 
083200     MOVE SPACES                 TO OUTM-IDOUTTYPE                        
083300     MOVE SPACES                 TO OUTM-IDOUTREC                         
083400     MOVE SPACES                 TO OUTM-IDLIST                           
083500     MOVE ZEROES                 TO OUTM-TIREGDAT                         
083600     MOVE ZEROES                 TO OUTM-TIKLOCK                          
083700                                                                          
083800     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
083900                                                                          
084000     IF OUTM-KDRC > 0                                                     
084100       SET SW-OUTM-ERR           TO TRUE                                  
084200     ELSE                                                                 
084300       SET SW-OUTM-OPEN          TO TRUE                                  
084400     END-IF                                                               
084500     .                                                                    
084600                                                                          
084700 S91-SEND-MAIL-LINE SECTION.                                              
084800                                                                          
084900     MOVE 'PUT'                  TO OUTM-KDFUNC                           
085000     MOVE LENGTH OF MID-W6I39101                                          
085100                                 TO OUTM-TEOUTDATA-L                      
085200     MOVE MID-W6I39101           TO OUTM-TEOUTDATA                        
085300                                                                          
085400     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
085500                                                                          
085600     MOVE SPACES                 TO OUTM-TEOUTDATA                        
085700     .                                                                    
085800                                                                          
085900 S91-SEND-MAIL-LINE2 SECTION.                                             
086000                                                                          
086700     MOVE 'PUT'                  TO OUTM-KDFUNC                           
086800                                                                          
086900     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
087000                                                                          
087100     MOVE SPACES                 TO OUTM-TEOUTDATA                        
087200     .                                                                    
087300                                                                          
087400 S91-SEND-MAIL-CLOSE SECTION.                                             
087500                                                                          
087600     MOVE 'CLOSE'                TO OUTM-KDFUNC                           
087700     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
087800                                                                          
087900     .                                                                    
088000     EJECT                                                                
088100 S99-JSON-CODE-VALIDATION SECTION.                                        
088200                                                                          
088300     MOVE JSON-CODE TO WS-NUM-DISPLAY                                     
088400*    STRING 'JSON CODE = ' WS-NUM-DISPLAY                                 
088500*                          DELIMITED BY SIZE                              
088600*                                  INTO OUTM-TEOUTDATA                    
088700*    MOVE 21                         TO OUTM-TEOUTDATA-L                  
088800*    PERFORM S91-SEND-MAIL-LINE2                                          
088900*                                                                         
089000*    IF FUNCTION MOD(JSON-STATUS 2 * 1) / 1 = 1                           
089100*      STRING 'JSON STATUS = 1'                                           
089200*                            DELIMITED BY SIZE                            
089300*                                    INTO OUTM-TEOUTDATA                  
089400*      MOVE 15                         TO OUTM-TEOUTDATA-L                
089500*      PERFORM S91-SEND-MAIL-LINE2                                        
089600*    END-IF                                                               
089700*                                                                         
089800*    IF FUNCTION MOD(JSON-STATUS 2 * 2) / 2 = 1                           
089900*      STRING 'JSON STATUS = 2'                                           
090000*                            DELIMITED BY SIZE                            
090100*                                    INTO OUTM-TEOUTDATA                  
090200*      MOVE 15                         TO OUTM-TEOUTDATA-L                
090300*      PERFORM S91-SEND-MAIL-LINE2                                        
090400*    END-IF                                                               
090500*                                                                         
090600*    IF FUNCTION MOD(JSON-STATUS 2 * 4) / 4 = 1                           
090700*      STRING 'JSON STATUS = 4'                                           
090800*                            DELIMITED BY SIZE                            
090900*                                    INTO OUTM-TEOUTDATA                  
091000*      MOVE 15                         TO OUTM-TEOUTDATA-L                
091100*      PERFORM S91-SEND-MAIL-LINE2                                        
091200*    END-IF                                                               
091300*                                                                         
091400*    IF FUNCTION MOD(JSON-STATUS 2 * 8) / 8 = 1                           
091500*      STRING 'JSON STATUS = 8'                                           
091600*                            DELIMITED BY SIZE                            
091700*                                    INTO OUTM-TEOUTDATA                  
091800*      MOVE 15                         TO OUTM-TEOUTDATA-L                
091900*      PERFORM S91-SEND-MAIL-LINE2                                        
092000*    END-IF                                                               
092100*                                                                         
092200*    IF FUNCTION MOD(JSON-STATUS 2 * 16) / 16 = 1                         
092300*      STRING 'JSON STATUS = 16'                                          
092400*                            DELIMITED BY SIZE                            
092500*                                    INTO OUTM-TEOUTDATA                  
092600*      MOVE 16                         TO OUTM-TEOUTDATA-L                
092700*      PERFORM S91-SEND-MAIL-LINE2                                        
092800*    END-IF                                                               
092900*                                                                         
093000*    IF FUNCTION MOD(JSON-STATUS 2 * 32) / 32 = 1                         
093100*      STRING 'JSON STATUS = 32'                                          
093200*                            DELIMITED BY SIZE                            
093300*                                    INTO OUTM-TEOUTDATA                  
093400*      MOVE 16                         TO OUTM-TEOUTDATA-L                
093500*      PERFORM S91-SEND-MAIL-LINE2                                        
093600*    END-IF                                                               
093700*                                                                         
093800*    IF FUNCTION MOD(JSON-STATUS 2 * 64) / 64 = 1                         
093900*      STRING 'JSON STATUS = 64'                                          
094000*                            DELIMITED BY SIZE                            
094100*                                    INTO OUTM-TEOUTDATA                  
094200*      MOVE 16                         TO OUTM-TEOUTDATA-L                
094300*      PERFORM S91-SEND-MAIL-LINE2                                        
094400*    END-IF                                                               
094500*                                                                         
094600     IF FUNCTION MOD(JSON-STATUS 2 * 128) / 128 = 1                       
094610       IF MAIL-OPEN-NO                                                    
094620          PERFORM S91-SEND-MAIL-OPEN                                      
094630          PERFORM S91-SEND-MAIL-LINE                                      
094640          SET MAIL-OPEN-YES         TO TRUE                               
094650       END-IF                                                             
094700       STRING 'JSON STATUS = 128'                                         
094800                             DELIMITED BY SIZE                            
094900                                     INTO OUTM-TEOUTDATA                  
095000       MOVE 17                         TO OUTM-TEOUTDATA-L                
095100       PERFORM S91-SEND-MAIL-LINE2                                        
095200     END-IF                                                               
095300                                                                          
095400     IF FUNCTION MOD(JSON-STATUS 2 * 256) / 256 = 1                       
095410       IF MAIL-OPEN-NO                                                    
095420          PERFORM S91-SEND-MAIL-OPEN                                      
095430          PERFORM S91-SEND-MAIL-LINE                                      
095440          SET MAIL-OPEN-YES         TO TRUE                               
095450       END-IF                                                             
095500       STRING 'JSON STATUS = 256'                                         
095600                             DELIMITED BY SIZE                            
095700                                     INTO OUTM-TEOUTDATA                  
095800       MOVE 17                         TO OUTM-TEOUTDATA-L                
095900       PERFORM S91-SEND-MAIL-LINE2                                        
096000     END-IF                                                               
096100                                                                          
096200*    IF FUNCTION MOD(JSON-STATUS 2 * 512) / 512 = 1                       
096300*      STRING 'JSON STATUS = 512'                                         
096400*                            DELIMITED BY SIZE                            
096500*                                    INTO OUTM-TEOUTDATA                  
096600*      MOVE 17                         TO OUTM-TEOUTDATA-L                
096700*      PERFORM S91-SEND-MAIL-LINE2                                        
096800*    END-IF                                                               
096900     .                                                                    
097000     SKIP3                                                                
097100* --- IMS SECTIONS ---                                                    
099200     EJECT                                                                
099300 IMS-GHN-WDGX6302-SUB SECTION.                                            
099400                                                                          
099500     STRING 'WDGX6302(IDSUBSCR= ' W-6302-IDSUBSCR-X ')'                   
099600          DELIMITED BY SIZE INTO SSA1                                     
099700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
099800     CALL CBLTDLI USING GHN 6301-PCB DLI-IO-WDGX6302 SSA1                 
099900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
100000     PERFORM IMS-STATUSCHECK                                              
100100     .                                                                    
100200     EJECT                                                                
100300 IMS-GN-WDGX6302-CON SECTION.                                             
100400                                                                          
100500     STRING 'WDGX6302(IDLBBET = ' W-6302-IDLBBET-X ')'                    
100600          DELIMITED BY SIZE INTO SSA1                                     
100700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
100800     CALL CBLTDLI USING GN 6301-2-PCB DLI-IO-WDGX6302-2 SSA1              
100900     MOVE 6301-2-STATUS-CODE TO STATUS-WS                                 
101000     PERFORM IMS-STATUSCHECK                                              
101100     .                                                                    
101200     EJECT                                                                
101300 IMS-GHN-WDGX6302-CON SECTION.                                            
101400                                                                          
101500     STRING 'WDGX6302(IDLBBET = ' W-6302-IDLBBET-X ')'                    
101600          DELIMITED BY SIZE INTO SSA1                                     
101700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
101800     CALL CBLTDLI USING GHN 6301-PCB DLI-IO-WDGX6302 SSA1                 
101900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
102000     PERFORM IMS-STATUSCHECK                                              
102100     .                                                                    
102400     EJECT                                                                
102500 IMS-REPL-WDGX6302 SECTION.                                               
102600     MOVE '  '             TO GOOD-STATUSCODES                            
102700     CALL CBLTDLI USING REPL 6301-PCB DLI-IO-WDGX6302                     
102800     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
102900     PERFORM IMS-STATUSCHECK                                              
103000     .                                                                    
103010     EJECT                                                                
103020 IMS-GU-WDB616 SECTION.                                                   
103030                                                                          
103040     STRING 'WDB601  (IDDC     =' W-IDDC-REC-X ')'                        
103050          DELIMITED BY SIZE INTO SSA1                                     
103060     STRING 'WDB616  (IDDCREF  =' W-IDDC-SEND-X ')'                       
103070          DELIMITED BY SIZE INTO SSA2                                     
103080     MOVE '  ' TO GOOD-STATUSCODES                                        
103090     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-WDB616 SSA1 SSA2              
103091     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
103092     PERFORM IMS-STATUSCHECK                                              
103093     .                                                                    
103100                                                                          
103200 IMS-STATUSCHECK SECTION.                                                 
103300                                                                          
103400     SET STATUS-IX TO 1                                                   
103500     SEARCH GOOD-STATUS                                                   
103600       AT END                                                             
103700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
103800         DELIMITED BY SIZE INTO ERROR-TEXT                                
103900         CALL FELLOG                                                      
104000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
104100         CONTINUE                                                         
104200     END-SEARCH                                                           
104300     .                                                                    
