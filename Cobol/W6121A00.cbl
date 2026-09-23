000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6121A00.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   FEBRUARI 2007.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER FIL W6121C OCH SKAPAR LISTOR SOM LÄGGS PÅ                  
001000*        D&P (DISTRIBUTION & PRINT). LISTA KAN SES PÅ WEBBEN              
001100*        PÅ FOLLOW UP SIDAN, SUP-NO-STOCK.                                
001200*        ERSATTA ARTIKLAR UTAN LAGERSALDO.                                
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INFILER W612IC                                             
002300     SELECT W6121C                     ASSIGN TO W6121AD1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W6121C                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W6121C      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 01  CHKP-VAR.                                                            
003800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004300     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004400 77  IDPGM                       PIC X(8)    VALUE 'W6121A00'.            
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
004900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100                                                                          
005200 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
005300 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
005400                                                                          
005500 01  CURR-SECTION                PIC X(30)   VALUE SPACE.                 
005600 01  WS-ANTAL-POSTER             PIC 9(9)    VALUE ZERO.                  
005700                                                                          
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100                                                                          
006200 77  W6121C-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W6121C                       VALUE 'J'.                   
006400                                                                          
006500 77  DAP-OPEN-SW                 PIC X       VALUE 'N'.                   
006600     88  DAP-OPEN                            VALUE 'J'.                   
006700     88  DAP-CLOSED                          VALUE 'N'.                   
006800                                                                          
006900 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
007000     88  POST-FINNS                          VALUE 'J'.                   
007100     88  POST-SAKNAS                         VALUE 'N'.                   
007200                                                                          
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800                                                                          
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
008800     EJECT                                                                
008900*---- PARAMETRAR TILL ABEND                                               
009000 01  RETURKODER.                                                          
009100     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
009200     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
009300     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
009400     03  RKOD-ABEND-WITH-DUMP    PIC S9(4) COMP SYNC VALUE +1000.         
009500                                                                          
009600     EJECT                                                                
009700*01  -COPY WDATAREA                                                       
009800     EJECT                                                                
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010200*01  -COPY WZ01SUB                                                        
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
010500*01  -COPY WZ01SEND                                                       
010600     EJECT                                                                
010700                                                                          
010800 01  FILLER                      PIC X(16) VALUE 'WWDC99-AREA'.           
010900*01  -COPY WWDC99                                                         
011000                                                                          
011100 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
011200*01  -COPY WTRAUTF8                                                       
011300                                                                          
011400 01  IN-AREA-START               PIC X(24)   VALUE 'IN-AREA'.             
011500*01  AREA -COPY W6121C     -PRE IN-                                       
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
011800 01  HDR-AREA.                                                            
011900*    03  -COPY WZ01REQU  -PRE HDR-                                        
012000*    03  -COPY WZ04HDR                                                    
012100 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
012200 01  DOC-AREA.                                                            
012300*    03  -COPY W6121D                                                     
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600     SKIP3                                                                
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FINNS                       VALUE '  '.                  
013000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013300     88  IMS-EJ-OK                           VALUE 'XD'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014100 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
014200 01  NYCKLAR-TILL-DLI.                                                    
014300   03   W-IDARTNR-X.                                                      
014400     05 W-IDARTNR            PIC S9(9)  COMP-3  VALUE ZERO.               
014500   03   W-IDDC-X.                                                         
014600     05 W-IDDC               PIC X(2) VALUE SPACE.                        
014700   03   W-IDSKYLT-X.                                                      
014800     05 W-IDSKYLT            PIC X(3) VALUE 'GB'.                         
         03  W-IDDC-B6-X.                                                       
             05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                   
014900     EJECT                                                                
015000*    --- IMS FUNKTIONSKODER                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300*    ---  DLI INPUT-OUTPUT AREA                                           
015400     EJECT                                                                
015500 01  DLI-IO-WDD311.                                                       
015600*    03  -COPY WDD311                                                     
015700     EJECT                                                                
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
           EJECT                                                                
015800 LINKAGE SECTION.                                                         
015900*01  -COPY W0009   -PRE MSG-                                              
016000 01  DISTRDOC-PCB                PIC X.                                   
016100     EJECT                                                                
016200*01  -COPY W0008  -PRE BENA-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
016500 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB BENA-PCB                  
                                 WDB6-PCB.                                      
016600 MAIN SECTION.                                                            
016700     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB BENA-PCB                  
                                 WDB6-PCB.                                      
016800                                                                          
016900     PERFORM A-INIT                                                       
017000     PERFORM S01-LAS-W6121C                                               
017100     PERFORM UNTIL END-OF-W6121C                                          
017200        MOVE IN-DOC-IDDC TO SPAR-IDDC                                     
017300        PERFORM S90-OPEN-DAP-SEND                                         
017400        MOVE JA TO DAP-OPEN-SW                                            
017500        PERFORM C-SKAPA-HEADER                                            
017600                                                                          
017700        PERFORM UNTIL END-OF-W6121C OR                                    
017800          (SPAR-IDDC NOT = IN-DOC-IDDC)                                   
017900            PERFORM D-RAD-DATA                                            
018000            PERFORM S01-LAS-W6121C                                        
018100        END-PERFORM                                                       
018200                                                                          
018300        PERFORM S90-CLOSE-DAP-SEND                                        
018400        MOVE NEJ TO DAP-OPEN-SW                                           
018500     END-PERFORM                                                          
018600                                                                          
018700     IF DAP-OPEN                                                          
018800       PERFORM S90-CLOSE-DAP-SEND                                         
018900     END-IF                                                               
019000                                                                          
019100     PERFORM Z-FINIT                                                      
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800                                                                          
019900     OPEN INPUT W6121C                                                    
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100                                                                          
020200     ACCEPT DAGENS-DATUM FROM DATE                                        
020300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020400                                                                          
020500     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
020600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
020700                     DAT-O-TIDATUM DAT-KDSVAR                             
020800     IF DAT-KDSVAR-OK                                                     
020900       MOVE DAT-TIAAVV-GRP TO DOC-TIAAVV                                  
021000     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 C-SKAPA-HEADER SECTION.                                                  
021400                                                                          
021500     MOVE 001             TO HDR-REQU-IDMSGVER                            
021600     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
021700     MOVE 'W6121A'        TO HDR-REQU-IDUSER                              
021800                                                                          
021900     MOVE 'SUP-NO-STOCK'  TO HDR-IDOUTTYPE                                
022000     MOVE SPACE           TO HDR-IDOUTREC                                 
022100     MOVE IN-DOC-IDDC     TO HDR-IDOUTREC(1:2)                            
022200     MOVE 'W6121A'        TO HDR-IDOUTREC(3:8)                            
022300                                                                          
022400     MOVE FUNCTION CURRENT-DATE(3:10)                                     
022500                                 TO HDR-IDLIST                            
022600     PERFORM S90-PUT-DAP-HEADER                                           
022700     .                                                                    
022800     EJECT                                                                
022900 D-RAD-DATA SECTION.                                                      
023000                                                                          
023100     IF IN-DOC-ADLAGOMR = ZERO AND IN-DOC-ADGANG = ZERO                   
023200     AND IN-DOC-ADPLATS = ZERO AND                                        
023300     IN-DOC-ADBUFFOMR = ZERO AND IN-DOC-ADBUFFGANG = ZERO                 
023400     AND IN-DOC-ADBUFFPL = ZERO                                           
023500        CONTINUE                                                          
023600     ELSE                                                                 
023700        MOVE IN-DOC-IDARTNR    TO W-IDARTNR                               
023800                                                                          
023810        MOVE IN-DOC-IDDC       TO WS-IDDC                                 
                                        W-IDDC-B6                               
              PERFORM IMS-GU-WDB601                                             
              MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                           
              IF DCS-UNICODE-IDSKYLT                                            
                 MOVE 'UTF8'             TO TRAUTF8-KDCP                        
              ELSE                                                              
                 MOVE '278 '             TO TRAUTF8-KDCP                        
              END-IF                                                            
023890        PERFORM IMS-GET-BENA-TEXT                                         
023891        IF SEGMENT-FINNS                                                  
023892           MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                 
023893        ELSE                                                              
023894           MOVE SPACE              TO TRAUTF8-TECONV-FROM                 
023895           MOVE '278'              TO TRAUTF8-KDCP                        
023896        END-IF                                                            
              IF TRAUTF8-TECONV-FROM = SPACES                                   
               MOVE 'GB'  TO W-IDSKYLT                                          
               MOVE '278' TO TRAUTF8-KDCP                                       
               PERFORM IMS-GET-BENA-TEXT                                        
               MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                        
              END-IF                                                            
023897        MOVE 25                    TO TRAUTF8-KVMAXTL                     
023898        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
023899        MOVE TRAUTF8-TECONV-TO     TO DOC-BEART                           
023900                                                                          
024500        MOVE 'LINE'            TO DOC-IDAFPRCD                            
024600        MOVE IN-DOC-IDDC       TO DOC-IDDC                                
024700        MOVE IN-DOC-IDARTNR    TO DOC-IDARTNR                             
024800        MOVE IN-DOC-ADLAGOMR   TO DOC-ADLAGOMR                            
024900        MOVE IN-DOC-ADGANG     TO DOC-ADGANG                              
025000        MOVE IN-DOC-ADPLATS    TO DOC-ADPLATS                             
025100        MOVE IN-DOC-ADBUFFOMR  TO DOC-ADBUFFOMR                           
025200        MOVE IN-DOC-ADBUFFGANG TO DOC-ADBUFFGANG                          
025300        MOVE IN-DOC-ADBUFFPL   TO DOC-ADBUFFPL                            
025400                                                                          
025500        PERFORM S90-PUT-DOC                                               
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 Z-FINIT SECTION.                                                         
026000                                                                          
026100     CLOSE W6121C                                                         
026200     MOVE 'S' TO POSTSUM-OPKOD                                            
026300     CALL POSTSUM USING POSTSUM-PARM                                      
026400     .                                                                    
026500     EJECT                                                                
026600 X-TAG-CHECKPOINT SECTION.                                                
026700                                                                          
026800     PERFORM IMS-CHECKPOINT                                               
026900     MOVE ZERO TO CHKP-ANT                                                
027000     .                                                                    
027100     EJECT                                                                
027200 S01-LAS-W6121C SECTION.                                                  
027300                                                                          
027400     READ W6121C INTO IN-AREA                                             
027500     AT END                                                               
027600        SET END-OF-W6121C TO TRUE                                         
027700                                                                          
027800     NOT AT END                                                           
027900        MOVE 'W6121C'   TO POSTSUM-FDNAMN                                 
028000        MOVE 'W6121AD1' TO POSTSUM-DDNAMN2                                
028100        MOVE  W-IDDC   TO POSTSUM-TRANSTYP                                
028200        CALL POSTSUM USING POSTSUM-PARM                                   
028300        ADD +1 TO WS-ANTAL-POSTER                                         
028400     END-READ                                                             
028500     .                                                                    
028600     EJECT                                                                
028700* --- IMS SEKTIONER ---                                                   
028800 S90-OPEN-DAP-SEND SECTION.                                               
028900*    DISPLAY 'S90-OPEN-DAP-S'                                             
029000     MOVE 'S90-OPEN-DAP-S' TO CURR-SECTION                                
029100                                                                          
029200     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
029300     MOVE 'OPEN'                     TO SEND-KDFUNC                       
029400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029500                         SEND-OPEN-AREA                                   
029600     IF SEND-KDRC > ZERO                                                  
029700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
029800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
029900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030100     END-IF                                                               
030200     .                                                                    
030300 S90-CLOSE-DAP-SEND SECTION.                                              
030400*    DISPLAY 'S90-CLOSE-DAP-'                                             
030500     MOVE 'S90-CLOSE-DAP-' TO CURR-SECTION                                
030600                                                                          
030700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
030800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030900                                                                          
031000     IF SEND-KDRC > 0                                                     
031100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
031200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
031300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031500     END-IF                                                               
031600     .                                                                    
031700 S90-PUT-DAP-HEADER SECTION.                                              
031800*    DISPLAY'90-PUT-DAP-HE'                                               
031900     MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
032000                                                                          
032100     MOVE 'PUT'                           TO SEND-KDFUNC                  
032200     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
032300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032400                         SEND-KVDLEN                                      
032500                         HDR-AREA                                         
032600     IF SEND-KDRC > ZERO                                                  
032700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
032800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
032900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033100     END-IF                                                               
033200     .                                                                    
033300 S90-PUT-DOC SECTION.                                                     
033400*    DISPLAY   'S90-PUT-DOC '                                             
033500     MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
033600                                                                          
033700     MOVE 'PUT'                           TO SEND-KDFUNC                  
033800     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
033900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034000                         SEND-KVDLEN                                      
034100                         DOC-AREA                                         
034200     IF SEND-KDRC > ZERO                                                  
034300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
034400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
034500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034700     END-IF                                                               
034800     MOVE 'W612C1'   TO POSTSUM-FDNAMN                                    
034900     MOVE 'PUTLINE'  TO POSTSUM-DDNAMN2                                   
035000     MOVE  W-IDDC   TO POSTSUM-TRANSTYP                                   
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     .                                                                    
035300 IMS-GET-BENA-TEXT SECTION.                                               
035400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
035500            DELIMITED BY SIZE INTO SSA1                                   
035600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
035700            DELIMITED BY SIZE INTO SSA2                                   
035800     MOVE '  GE' TO GODK-STATUSKODER                                      
035900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
036000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
036100     PERFORM IMS-STATUS-KONTROLL                                          
036200     .                                                                    
036300     SKIP3                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUS-KONTROLL                                          
           .                                                                    
           SKIP3                                                                
036400 IMS-RESTART SECTION.                                                     
036500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036600     MOVE '  ' TO GODK-STATUSKODER                                        
036700     CALL CBLTDLI USING XRST MSG-PCB                                      
036800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036900                        CHKP-AREA-LENGTH CHKP-AREA                        
037000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037100     PERFORM IMS-STATUS-KONTROLL                                          
037200     .                                                                    
037300     SKIP3                                                                
037400 IMS-CHECKPOINT SECTION.                                                  
037500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037600     MOVE '  XD' TO GODK-STATUSKODER                                      
037700     CALL CBLTDLI USING CHKP MSG-PCB                                      
037800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037900                        CHKP-AREA-LENGTH CHKP-AREA                        
038000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038100     PERFORM IMS-STATUS-KONTROLL                                          
038200                                                                          
038300     IF IMS-EJ-OK                                                         
038400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
038500       DISPLAY FELTEXT                                                    
038600       CALL FELLOG                                                        
038700     END-IF                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 IMS-STATUS-KONTROLL SECTION.                                             
039100     SKIP2                                                                
039200     SET STATUS-IX TO 1                                                   
039300     SEARCH GODK-STATUS                                                   
039400       AT END                                                             
039500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039600           DELIMITED BY SIZE INTO FELTEXT                                 
039700         DISPLAY FELTEXT                                                  
039800         CALL FELLOG                                                      
039900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040000         CONTINUE                                                         
040100     END-SEARCH                                                           
040200     .                                                                    
