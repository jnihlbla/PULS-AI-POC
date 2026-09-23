000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4184100.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   FEBRUARI 2009.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        LÄSER FIL W41411 IDENTIFIERADE ARTNR 100 POSTER                  
001000*        OCH FIL W61271 MED R34 POSTER                                    
001100*        OCH SKAPAR LISTA TILL D&P.                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U1000 - D&P ERROR                                                
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INFIL                                                      
002500     SELECT W41411                     ASSIGN TO W41841D1.                
002600     SKIP2                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W41411                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W414100A  -L.                                                  
003600     SKIP3                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W4184100'.            
004000 77  FILLER                      PIC X(8)    VALUE 'PGM-POS:'.            
004100 77  PGM-POS                     PIC X(64)   VALUE SPACE.                 
004200 77  JA                          PIC X       VALUE 'J'.                   
004210 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
004500 77  SPAR-IDFAKT                 PIC 9(7)    VALUE ZERO.                  
004600 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004700 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
004910 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
004920 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
004930                                                                          
005000     SKIP2                                                                
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500                                                                          
005600 77  W41411-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W41411                       VALUE 'Y'.                   
005800     EJECT                                                                
005900*                                                                         
006000*---------------------------------------                                  
006100                                                                          
006200 01  W-IDDC-B6-X.                                                         
006300     03 W-IDDC-B6        PIC X(2).                                        
006310                                                                          
006320 01  W-IDARTNR-X.                                                         
006330     03  W-IDARTNR       PIC S9(9)   COMP-3.                              
006340 01  W-IDSKYLT-X.                                                         
006350     03  W-IDSKYLT       PIC X(3)    VALUE SPACE.                         
006400                                                                          
006500     EJECT                                                                
006600*---------------------------------------                                  
006700 01  HDR-AREA.                                                            
006800*    03  -COPY WZ01REQU                                                   
006900*    03  -COPY WZ04HDR                                                    
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
007200 01  SEND-AREA.                                                           
007300*    03  -COPY WZ01SEND                                                   
007400     EJECT                                                                
007500 01  SEND-RADER.                                                          
007600     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
007700* SKIP2                                                                   
007800 01  MAIL-DATA REDEFINES SEND-RADER.                                      
007900     03 -COPY W414101   -PRE WEB-                                         
008600     EJECT                                                                
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
008900                                                                          
009000     EJECT                                                                
009100*    --- LISTLAYOUT                                                       
009200 01  LISTA.                                                               
009300     03  RUBRIK-1.                                                        
009400         05  FILLER     PIC X      VALUE SPACE.                           
009500         05  FILLER     PIC X(15)  VALUE 'VCCS W41841-001'.               
009600         05  FILLER     PIC X(3)   VALUE SPACE.                           
009700         05  FILLER     PIC X(2)   VALUE 'DC'.                            
009800         05  FILLER     PIC X      VALUE SPACE.                           
009900         05  RUB1-IDDC  PIC X(2)   VALUE SPACE.                           
010000         05  FILLER     PIC X(3)   VALUE SPACE.                           
010100         05  FILLER     PIC X(6)   VALUE 'DATUM'.                         
010200         05  FILLER     PIC X(2)   VALUE SPACE.                           
010300         05  RUB1-DAT   PIC X(6)   VALUE SPACE.                           
010400         05  FILLER     PIC X(3)   VALUE SPACE.                           
010500         05  FILLER     PIC X(22)  VALUE 'FOLLOW UP R34         '.        
010600                                                                          
010700     03  RUBRIK-2.                                                        
010800         05  FILLER            PIC X(4)   VALUE SPACE.                    
010900         05  RUB2-IDARTNR      PIC X(10)  VALUE '  PARTNO. '.             
011000         05  FILLER            PIC X(3)   VALUE SPACE.                    
011100         05  RUB2-KVANTAL      PIC X(10)  VALUE '     QTY. '.             
011200         05  FILLER            PIC X(3)   VALUE SPACE.                    
011300         05  RUB2-BEART       PIC X(16)  VALUE 'DESCRIPTION     '.        
011400         05  FILLER            PIC X(12)  VALUE SPACE.                    
011500         05  RUB2-ADART        PIC X(12)  VALUE 'LOCATION    '.           
011600         05  FILLER            PIC X(3)   VALUE SPACE.                    
011700         05  RUB2-IDDC-SEND    PIC X(7)   VALUE 'DEV. DC'.                
011800         05  FILLER            PIC X(3)   VALUE SPACE.                    
011900         05  RUB2-AVVIKELSETYP PIC X(9)   VALUE 'DEV. TYPE'.              
011910         05  FILLER            PIC X(28)   VALUE SPACE.                   
012000                                                                          
012100     03  RAD.                                                             
012200         05  FILLER           PIC X(4)    VALUE SPACE.                    
012300         05  RAD-IDARTNR      PIC ZZZZZZZZ9.                              
012400         05  FILLER           PIC X(4)    VALUE SPACE.                    
012500         05  RAD-KVANTAL      PIC --------9.                              
012600         05  FILLER           PIC X(4)    VALUE SPACE.                    
012700         05  RAD-BEART        PIC X(25)   VALUE SPACE.                    
012800         05  FILLER           PIC X(3)    VALUE SPACE.                    
012900         05  RAD-ADLAGOMR     PIC Z9.                                     
013000         05  FILLER           PIC X(1)    VALUE SPACE.                    
013100         05  RAD-ADGANG       PIC Z9.                                     
013200         05  FILLER           PIC X(1)    VALUE SPACE.                    
013300         05  RAD-ADPLATS      PIC ZZZZ9.                                  
013400         05  FILLER           PIC X(9)    VALUE SPACE.                    
013500         05  RAD-IDDC-SEND    PIC X(2)    VALUE SPACE.                    
013600         05  FILLER           PIC X(3)    VALUE SPACE.                    
013700         05  RAD-AVVIKELSETYP PIC X(10)   VALUE SPACE.                    
013710         05  FILLER           PIC X(27)   VALUE SPACE.                    
013800                                                                          
013900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014000 01  FILLER REDEFINES DAGENS-DATUM.                                       
014100     03  DAGENS-AA               PIC 9(2).                                
014200     03  DAGENS-MM               PIC 9(2).                                
014300     03  DAGENS-DD               PIC 9(2).                                
014400     EJECT                                                                
014500 01  GENERAL-SUBPROGRAMS.                                                 
014600*                                                                         
014700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015100     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
015200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015210     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
015300     SKIP2                                                                
015400*    --- PARAMETERS TO ABEND                                              
015500                                                                          
015600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015900     SKIP2                                                                
016000 01  ERRTEXT.                                                             
016100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
016200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL POSTSUM                                          
016500*                                                                         
016600*01  -COPY W0005   -PRE  POSTSUM-                                         
016610                                                                          
016620 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
016630*01  -COPY WTRAUTF8                                                       
016640                                                                          
016700     EJECT                                                                
016800 01  IN-AREA-START           PIC X(24)   VALUE                            
016900                                 'IN-AREA-START  '.                       
017000     SKIP2                                                                
017100*01  AREA -COPY W414100A    -PRE IN-                                      
017200     EJECT                                                                
017300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017400 01   DLI-IO-AREA-B601.                                                   
017500*     03  -COPY WDB601                                                    
017510                                                                          
017520 01  FILLER               PIC X(16)   VALUE 'WDD311 AREA'.                
017530 01  DLI-IO-WDD311.                                                       
017540*    03  -COPY WDD311                                                     
017550                                                                          
017600                                                                          
017700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017800*    --- STATUS-KOD FRÅN IMS                                              
017900 01  STATUS-WS                   PIC XX.                                  
018000     88  SEGMENT-FINNS                       VALUE '  '.                  
018100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018400     88  IMS-EJ-OK                           VALUE 'XD'.                  
018500     SKIP2                                                                
018600 01  GODK-STATUSKODER.                                                    
018700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018800     SKIP3                                                                
018900 01  SSA1                        PIC X(64).                               
019000 01  SSA2                        PIC X(64).                               
019100     EJECT                                                                
019200*    --- IMS FUNKTIONSKODER                                               
019300*01  -COPY W0003                                                          
019400     EJECT                                                                
019500                                                                          
019600 LINKAGE SECTION.                                                         
019700                                                                          
019800*01  -COPY W0009            -PRE MSG-                                     
019900                                                                          
020000*01  -COPY W0009            -PRE DISTRDOC-                                
020100                                                                          
020200*01  -COPY W0008 -PRE WDB6-                                               
020300     05  FILLER           PIC X.                                          
020310                                                                          
020320*01  -COPY W0008 -PRE WDD3-                                               
020330     05  FILLER           PIC X.                                          
020400                                                                          
020500 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB WDB6-PCB WDD3-PCB.        
020600 MAIN SECTION.                                                            
020700     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDB6-PCB WDD3-PCB.        
020800                                                                          
020900     PERFORM A-INIT                                                       
021000                                                                          
021100     PERFORM S01-LAS-W41411                                               
021200     PERFORM UNTIL END-OF-W41411                                          
021300       MOVE IN-100-IDDC-REC TO SPAR-IDDC                                  
021400                               WS-IDDC                                    
021410       MOVE IN-100-IDDC-REC TO W-IDDC-B6                                  
021411     DISPLAY 'WS-IDDC=' WS-IDDC 'IN-100-IDDC-REC=' IN-100-IDDC-REC        
021420       PERFORM IMS-GU-WDB601                                              
021430                                                                          
021500       PERFORM S90-SEND-OPEN                                              
021600       PERFORM S90-PUT-DAP-START                                          
021700                                                                          
021800       PERFORM B-INIT-DC                                                  
021830                                                                          
021840       IF DCS-FLWEBDC = NEJ                                               
021850     DISPLAY 'DCS-FLWEBDC = NEJ'                                          
021900         PERFORM C-PRINT-HEAD                                             
021910       END-IF                                                             
021920                                                                          
022000       PERFORM UNTIL END-OF-W41411 OR                                     
022100         (SPAR-IDDC NOT = IN-100-IDDC-REC)                                
022110                                                                          
022400           IF DCS-FLWEBDC = NEJ                                           
022500     DISPLAY 'D-SEND-DATA-MAIL-LISTA'                                     
022700             PERFORM D-SEND-DATA-MAIL-LISTA                               
022701           ELSE                                                           
022702     DISPLAY 'E-SEND-DATA-WEB-LISTA'                                      
022710             PERFORM E-SEND-DATA-WEB-LISTA                                
022800           END-IF                                                         
022802*    DISPLAY 'SEND-RAD=' SEND-RAD                                         
022830*    DISPLAY 'WEB-W414101=' HDR-WZ04HDR REQU-WZ01REQU                     
022840                                                                          
022900         PERFORM S01-LAS-W41411                                           
023000       END-PERFORM                                                        
023010                                                                          
023100       PERFORM S90-SEND-CLOSE                                             
023200     END-PERFORM                                                          
023300                                                                          
023400     PERFORM Z-FINIT                                                      
023500                                                                          
023600     MOVE ZERO TO RETURN-CODE                                             
023700     GOBACK                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 A-INIT SECTION.                                                          
024100                                                                          
024200     OPEN INPUT  W41411                                                   
024300     ACCEPT DAGENS-DATUM FROM DATE                                        
024400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024500     .                                                                    
024600     EJECT                                                                
024700 B-INIT-DC SECTION.                                                       
024800                                                                          
024900     MOVE IN-100-IDDC-REC            TO RUB1-IDDC                         
025000     .                                                                    
025100     EJECT                                                                
025200 C-PRINT-HEAD SECTION.                                                    
025300                                                                          
025400     MOVE SPACE                      TO SEND-RADER                        
025600     MOVE SPACE                      TO SEND-RAD                          
025610     MOVE LENGTH OF SEND-RAD  TO SEND-KVDLEN                              
025700     PERFORM S90-PUT-DOC-LINE                                             
025800     MOVE IN-100-IDDC-REC            TO RUB1-IDDC                         
025900     MOVE DAGENS-DATUM               TO RUB1-DAT                          
026000     MOVE RUBRIK-1                   TO SEND-RAD                          
026020     MOVE LENGTH OF SEND-RAD  TO SEND-KVDLEN                              
026100     PERFORM S90-PUT-DOC-LINE                                             
026200                                                                          
026400     MOVE SPACE                      TO SEND-RAD                          
026410     MOVE LENGTH OF SEND-RAD  TO SEND-KVDLEN                              
026500     PERFORM S90-PUT-DOC-LINE                                             
026501                                                                          
026510     MOVE SPACE                      TO SEND-RAD                          
026600     MOVE RUBRIK-2                   TO SEND-RAD                          
026620     MOVE LENGTH OF SEND-RAD  TO SEND-KVDLEN                              
026700     PERFORM S90-PUT-DOC-LINE                                             
026800                                                                          
027000     MOVE SPACE                      TO SEND-RAD                          
027010     MOVE LENGTH OF SEND-RAD  TO SEND-KVDLEN                              
027100     PERFORM S90-PUT-DOC-LINE                                             
027200     .                                                                    
027300     EJECT                                                                
027400 D-SEND-DATA-MAIL-LISTA   SECTION.                                        
027401                                                                          
027410     MOVE SPACE            TO SEND-RAD                                    
027500                                                                          
027510     MOVE IN-100-IDARTNR   TO RAD-IDARTNR                                 
027600     MOVE IN-100-KVANTAL   TO RAD-KVANTAL                                 
027800     MOVE IN-100-BEART     TO RAD-BEART                                   
028000     MOVE IN-100-ADLAGOMR  TO RAD-ADLAGOMR                                
028100     MOVE IN-100-ADGANG    TO RAD-ADGANG                                  
028200     MOVE IN-100-ADPLATS   TO RAD-ADPLATS                                 
028310     MOVE IN-100-IDDC-SEND TO RAD-IDDC-SEND                               
028320     MOVE IN-100-AVVIKELSETYP TO RAD-AVVIKELSETYP                         
028400                                                                          
028500     MOVE RAD TO SEND-RAD                                                 
028600     MOVE LENGTH OF SEND-RAD  TO SEND-KVDLEN                              
028700     PERFORM S90-PUT-DOC-LINE                                             
028800     .                                                                    
028900     EJECT                                                                
029000 E-SEND-DATA-WEB-LISTA     SECTION.                                       
029001                                                                          
029010     MOVE SPACE            TO SEND-RAD                                    
029100                                                                          
029101     MOVE '1'                 TO WEB-IDAFPRCD                             
029102     MOVE SPAR-IDDC           TO WEB-IDDC                                 
029110     MOVE IN-100-IDARTNR      TO WEB-IDARTNR                              
029200     MOVE IN-100-KVANTAL      TO WEB-KVANTAL                              
029210     MOVE IN-100-BEART        TO WEB-BEART                                
029220                                                                          
029230*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
029240*    BEFORE DISPLAY OF WEB-BEART                                          
029250                                                                          
029260     MOVE IN-100-IDARTNR        TO W-IDARTNR                              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
029313     PERFORM IMS-GU-WDD311                                                
029314     IF SEGMENT-FINNS                                                     
029315        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
029316     ELSE                                                                 
029317        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
029318        MOVE '278 '             TO TRAUTF8-KDCP                           
029319     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
029320     MOVE 25                    TO TRAUTF8-KVMAXTL                        
029321     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
029322     MOVE TRAUTF8-TECONV-TO     TO WEB-BEART                              
029323                                                                          
029324     MOVE IN-100-ADLAGOMR     TO WEB-ADLAGOMR                             
029330     MOVE IN-100-ADGANG       TO WEB-ADGANG                               
029340     MOVE IN-100-ADPLATS      TO WEB-ADPLATS                              
029500     MOVE IN-100-IDDC-SEND    TO WEB-IDDC-SEND                            
029600     MOVE IN-100-AVVIKELSETYP TO WEB-AVVIKELSETYP                         
029700     MOVE DAGENS-DATUM        TO WEB-DATUM                                
029900                                                                          
030010     MOVE WEB-W414101       TO SEND-RAD                                   
030100     MOVE LENGTH OF SEND-RAD  TO SEND-KVDLEN                              
030200     PERFORM S90-PUT-DOC-LINE                                             
030300     .                                                                    
030400     EJECT                                                                
030500 Z-FINIT SECTION.                                                         
030600                                                                          
030700     CLOSE W41411                                                         
030800     MOVE 'S' TO POSTSUM-OPKOD                                            
030900     CALL POSTSUM USING POSTSUM-PARM                                      
031000     .                                                                    
031100     EJECT                                                                
031200 S01-LAS-W41411 SECTION.                                                  
031300     READ W41411 INTO IN-AREA                                             
031400     AT END                                                               
031500        MOVE HIGH-VALUE TO IN-AREA                                        
031600        SET END-OF-W41411 TO TRUE                                         
031700     NOT AT END                                                           
031800        MOVE 'W41411'   TO POSTSUM-FDNAMN                                 
031900        MOVE 'W41841D1' TO POSTSUM-DDNAMN2                                
032000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
032100        CALL POSTSUM USING POSTSUM-PARM                                   
032200     END-READ                                                             
032300     .                                                                    
032400     EJECT                                                                
032500 S90-SEND-OPEN SECTION.                                                   
032510     DISPLAY 'S90-SEND-OPEN        '                                      
032600                                                                          
032700     MOVE 'OPEN'                        TO SEND-KDFUNC                    
032800     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
032900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033000                         SEND-OPEN-AREA                                   
033100     IF SEND-KDRC > 0                                                     
033200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
033300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
033400       DELIMITED BY SIZE INTO ERRTEXT                                     
033500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033600     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 S90-PUT-DAP-START SECTION.                                               
034000                                                                          
034010     DISPLAY 'S90-SEND-OPEN        '                                      
034100     MOVE 1                       TO REQU-IDMSGVER                        
034200     MOVE ' '                     TO REQU-KDPGMACT                        
034300     MOVE IDPGM                   TO REQU-IDUSER                          
034610     IF DCS-FLWEBDC = JA                                                  
034620       MOVE 'R34-LIST-DAY'        TO HDR-IDOUTTYPE                        
034640     ELSE                                                                 
034641       MOVE 'W41841'              TO HDR-IDOUTTYPE                        
034650     END-IF                                                               
034651     DISPLAY 'HDR-AREA=' HDR-AREA                                         
034653     MOVE WS-IDDC               TO HDR-IDOUTREC                           
034654                                   HDR-IDLIST                             
034900     MOVE 'PUT'                   TO SEND-KDFUNC                          
035000     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
035100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035200                         SEND-KVDLEN                                      
035300                         HDR-AREA                                         
035400     IF SEND-KDRC > ZERO                                                  
035500       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
035600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035700       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
035800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 S90-PUT-DOC-LINE SECTION.                                                
036300                                                                          
036310     DISPLAY 'S90-PUT-DOC-LINE     '                                      
036400     MOVE 'PUT'                           TO SEND-KDFUNC                  
036500*                     -- UTAN STYRTECKEN:                                 
036600*    MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
036700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036800                         SEND-KVDLEN                                      
036900*                     -- UTAN STYRTECKEN:                                 
037000                         SEND-RAD                                         
037100     IF SEND-KDRC > ZERO                                                  
037200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
037300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
037400       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
037500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 S90-SEND-CLOSE SECTION.                                                  
038000                                                                          
038010     DISPLAY 'S90-SEND-CLOSE       '                                      
038100     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
038200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
038300                                                                          
038400     IF SEND-KDRC > 0                                                     
038500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
038600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
038700       DELIMITED BY SIZE INTO ERRTEXT                                     
038800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200                                                                          
039300 IMS-GU-WDB601    SECTION.                                                
039400                                                                          
039500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
039600          DELIMITED BY SIZE INTO SSA1                                     
039700     MOVE '  ' TO GODK-STATUSKODER                                        
039800     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
039900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
040000     PERFORM IMS-STATUSKONTROLL                                           
040100     .                                                                    
040200                                                                          
040210 IMS-GU-WDD311 SECTION.                                                   
040220                                                                          
040230     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
040240             DELIMITED BY SIZE INTO SSA1                                  
040250     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
040260             DELIMITED BY SIZE INTO SSA2                                  
040270     MOVE '  GE'                 TO GODK-STATUSKODER                      
040280     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
040290     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
040291     PERFORM IMS-STATUSKONTROLL                                           
040292     .                                                                    
040293                                                                          
040300 IMS-STATUSKONTROLL SECTION.                                              
040400                                                                          
040500     SET STATUS-IX TO 1                                                   
040600     SEARCH GODK-STATUS AT END CALL FELLOG                                
040700        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
040800        CONTINUE                                                          
040900     END-SEARCH                                                           
041000     .                                                                    
