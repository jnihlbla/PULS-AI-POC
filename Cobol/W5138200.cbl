000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5138200.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   07/01/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER W01184-LDC  FILER OCH SKAPAR LISTOR SOM LÄGGS PÅ           
001000*        D&P (DISTRIBUTION & PRINT) . LISTA KAN SES PÅ WEBBEN             
001100*        PÅ FOLLOW UP SIDAN, INVEST-BALANCE                               
001200*        ARTIKLAR MED UTREDNINGSSALDO                                     
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INFILER W01184 LDC                                         
002300     SELECT W01184                     ASSIGN TO W51382D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W01184                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W01184      -L.                                                
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
004400 77  IDPGM                       PIC X(8)    VALUE 'W5138200'.            
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5).                                
004910                                                                          
004920 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
004930 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
004940                                                                          
005000 01  CURR-SECTION                PIC X(30)   VALUE SPACE.                 
005100 01  WS-ANTAL-POSTER             PIC 9(9)    VALUE ZERO.                  
005200     SKIP2                                                                
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W01184                       VALUE 'J'.                   
005900     EJECT                                                                
006000 77  DAP-OPEN-SW                 PIC X       VALUE 'N'.                   
006100     88  DAP-OPEN                            VALUE 'J'.                   
006200     88  DAP-CLOSED                          VALUE 'N'.                   
006300                                                                          
006400 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
006500     88  POST-FINNS                          VALUE 'J'.                   
006600     88  POST-SAKNAS                         VALUE 'N'.                   
006700                                                                          
006800     EJECT                                                                
006900 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W51382'.             
007000                                                                          
007100 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
007200                                                                          
007300*01  -COPY WDATKORT                                                       
007400     EJECT                                                                
007500                                                                          
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008100     EJECT                                                                
008200 01  DAGENS-DAAAVV               PIC 9(6)    VALUE ZERO.                  
008300 01  FILLER REDEFINES DAGENS-DAAAVV.                                      
008400     03  DAGENS-DAAAVV-SS         PIC 9(2).                               
008500     03  DAGENS-DAAAVV-AA         PIC 9(2).                               
008600     03  DAGENS-DAAAVV-VV         PIC 9(2).                               
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009610     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009700     EJECT                                                                
009800*---- PARAMETRAR TILL ABEND                                               
009900 01  RETURKODER.                                                          
010000     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
010100     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
010200     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
010300     03  RKOD-ABEND-WITH-DUMP    PIC S9(4) COMP SYNC VALUE +1000.         
010400                                                                          
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL POSTSUM                                          
010700*                                                                         
010800*01  -COPY W0005   -PRE  POSTSUM-                                         
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011100*01  -COPY WZ01SUB                                                        
011200     EJECT                                                                
011300                                                                          
011400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
011500*01  -COPY WZ01SEND                                                       
011510                                                                          
011520 01  FILLER                      PIC X(16) VALUE 'WWDC99-AREA'.           
011530*01  -COPY WWDC99                                                         
011600                                                                          
011700     EJECT                                                                
011710 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
011720*01  -COPY WTRAUTF8                                                       
011730                                                                          
011740     EJECT                                                                
011800                                                                          
011900 01  IN-AREA-START               PIC X(24)   VALUE                        
012000                                             'IN-AREA-START'.             
012100     SKIP2                                                                
012200                                                                          
012300*01  AREA -COPY W01184     -PRE IN-                                       
012400*                                                                         
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
012700 01  HDR-AREA.                                                            
012800*    03  -COPY WZ01REQU  -PRE HDR-                                        
012900*    03  -COPY WZ04HDR                                                    
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
013200 01  DOC-AREA.                                                            
013300*    03  -COPY W5138201                                                   
013400*                                                                         
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP3                                                                
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FINNS                       VALUE '  '.                  
014100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014400     88  IMS-EJ-OK                           VALUE 'XD'.                  
014500     SKIP2                                                                
014600 01  GODK-STATUSKODER.                                                    
014700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800     SKIP3                                                                
014900 01  SSA1                        PIC X(64).                               
015000 01  SSA2                        PIC X(64).                               
015100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015200*                                                                         
015300 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
015400 01  NYCKLAR-TILL-DLI.                                                    
015500   03   W-IDARTNR-X.                                                      
015600     05 W-IDARTNR            PIC S9(9)  COMP-3  VALUE ZERO.               
015700                                                                          
015800   03   W-IDDC-X.                                                         
015900     05 W-IDDC               PIC X(2) VALUE SPACE.                        
016000                                                                          
016100   03   W-IDSKYLT-X.                                                      
016200     05 W-IDSKYLT            PIC X(3) VALUE 'GB'.                         
         03  W-IDDC-B6-X.                                                       
             05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                   
016300                                                                          
016400     EJECT                                                                
016500*    --- IMS FUNKTIONSKODER                                               
016600*01  -COPY W0003                                                          
016700     EJECT                                                                
016800*    ---  DLI INPUT-OUTPUT AREA                                           
016900     EJECT                                                                
017000 01  DLI-IO-WDD311.                                                       
017100*    03  -COPY WDD311                                                     
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
017200                                                                          
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500                                                                          
017600*01  -COPY W0009   -PRE MSG-                                              
017700 01  DISTRDOC-PCB                PIC X.                                   
017800     EJECT                                                                
017900*01  -COPY W0008  -PRE BENA-                                              
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
018200 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB BENA-PCB                  
                                 WDB6-PCB.                                      
018300 MAIN SECTION.                                                            
018400     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB BENA-PCB                  
                                 WDB6-PCB.                                      
018500                                                                          
018600     SKIP2                                                                
018700     PERFORM A-INIT                                                       
018800     PERFORM S01-LAES-W01184                                              
018900     IF NOT END-OF-W01184                                                 
019000       PERFORM UNTIL POST-FINNS OR END-OF-W01184                          
019100                                                                          
019200         IF IN-SLAG-KVUTRS NOT = 0                                        
019300           PERFORM S90-OPEN-DAP-SEND                                      
019400           MOVE JA      TO DAP-OPEN-SW                                    
019500           MOVE IN-SLAG-IDDC TO W-IDDC                                    
019600           PERFORM C-SKAPA-HEADER                                         
019700           MOVE JA TO POST-FINNS-SW                                       
019800         ELSE                                                             
019900           PERFORM S01-LAES-W01184                                        
020000         END-IF                                                           
020100       END-PERFORM                                                        
020200     END-IF                                                               
020300     PERFORM UNTIL END-OF-W01184                                          
020400       IF IN-SLAG-KVUTRS NOT = 0                                          
020500         IF W-IDDC = IN-SLAG-IDDC                                         
020600           PERFORM D-SKAPA-LINE                                           
020700         ELSE                                                             
020800           PERFORM S90-CLOSE-DAP-SEND                                     
020900           MOVE IN-SLAG-IDDC TO W-IDDC                                    
021000           PERFORM S90-OPEN-DAP-SEND                                      
021100           PERFORM C-SKAPA-HEADER                                         
021200           PERFORM D-SKAPA-LINE                                           
021300         END-IF                                                           
021400       END-IF                                                             
021500       PERFORM S01-LAES-W01184                                            
021600     END-PERFORM                                                          
021700     IF DAP-OPEN                                                          
021800       PERFORM S90-CLOSE-DAP-SEND                                         
021900     END-IF                                                               
022000     PERFORM Z-FINIT                                                      
022100                                                                          
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT SECTION.                                                          
022700     SKIP2                                                                
022800     OPEN INPUT W01184                                                    
022900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023000     ACCEPT DAGENS-DATUM FROM DATE                                        
023100                                                                          
023200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
023300     MOVE 20               TO DAGENS-DAAAVV-SS                            
023400     MOVE D-AAR            TO DAGENS-DAAAVV-AA                            
023500     MOVE D-VECKA          TO DAGENS-DAAAVV-VV                            
023600     .                                                                    
023700     EJECT                                                                
023800*                                                                         
023900 C-SKAPA-HEADER SECTION.                                                  
024000*    DISPLAY 'C-SKAPA-HEADER  '                                           
024100     MOVE 'C-SKAPA-HEADER  ' TO CURR-SECTION                              
024200     MOVE 001             TO HDR-REQU-IDMSGVER                            
024300     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
024400     MOVE 'W51382'        TO HDR-REQU-IDUSER                              
024500                                                                          
024600     MOVE 'INVEST-BALANCE'    TO HDR-IDOUTTYPE                            
024700     MOVE SPACE               TO HDR-IDOUTREC                             
024800     MOVE IN-SLAG-IDDC        TO HDR-IDOUTREC(1:2)                        
024900     MOVE 'W51382'            TO HDR-IDOUTREC(3:8)                        
025000                                                                          
025100     MOVE FUNCTION CURRENT-DATE(3:10)                                     
025200                                 TO HDR-IDLIST                            
025300     PERFORM S90-PUT-DAP-HEADER                                           
025400                                                                          
025500     .                                                                    
025600     EJECT                                                                
025700 D-SKAPA-LINE   SECTION.                                                  
025800     MOVE IN-SLAG-IDARTNR   TO W-IDARTNR                                  
025900                                                                          
025910     MOVE IN-SLAG-IDDC      TO WS-IDDC                                    
                                     W-IDDC-B6                                  
           PERFORM IMS-GU-WDB601                                                
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
026580     PERFORM IMS-GET-BENA-TEXT                                            
026590     IF SEGMENT-FINNS                                                     
026591        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
026592     ELSE                                                                 
026593        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
026594        MOVE '278'              TO TRAUTF8-KDCP                           
026595     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GET-BENA-TEXT                                           
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
026596     MOVE 25                    TO TRAUTF8-KVMAXTL                        
026597     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
026598     MOVE TRAUTF8-TECONV-TO     TO DOC-BEART                              
026600                                                                          
026700     MOVE 'LINE'            TO DOC-IDAFPRCD                               
026800     MOVE IN-SLAG-IDDC      TO DOC-IDDC                                   
026900     MOVE IN-SLAG-IDARTNR   TO DOC-IDARTNR                                
027000     MOVE IN-SLAG-ADLAGOMR  TO DOC-ADLAGOMR                               
027100     MOVE IN-SLAG-ADGANG    TO DOC-ADGANG                                 
027200     MOVE IN-SLAG-ADPLATS   TO DOC-ADPLATS                                
027300     MOVE IN-SLAG-KVLS      TO DOC-KVLS                                   
027400     MOVE IN-SLAG-KVUTRS    TO DOC-KVUTRS                                 
027500     MOVE DAGENS-DAAAVV     TO DOC-DAAAVV                                 
027600                                                                          
027700     PERFORM S90-PUT-DOC                                                  
027800     .                                                                    
027900     EJECT                                                                
028000 Z-FINIT SECTION.                                                         
028100                                                                          
028200                                                                          
028300     CLOSE W01184                                                         
028400     SKIP2                                                                
028500     MOVE 'S' TO POSTSUM-OPKOD                                            
028600     CALL POSTSUM USING POSTSUM-PARM                                      
028700     .                                                                    
028800     EJECT                                                                
028900 S01-LAES-W01184  SECTION.                                                
029000*    DISPLAY   'S01-LAES -W01184'                                         
029100     MOVE 'S01-LAES-W01' TO CURR-SECTION                                  
029200     SKIP2                                                                
029300     READ W01184 INTO IN-AREA                                             
029400     AT END                                                               
029500*       MOVE HIGH-VALUE TO IN-ID                                          
029600        SET END-OF-W01184 TO TRUE                                         
029700                                                                          
029800     NOT AT END                                                           
029900        MOVE 'W01184'   TO POSTSUM-FDNAMN                                 
030000        MOVE 'W51382D1' TO POSTSUM-DDNAMN2                                
030100        MOVE  IN-SLAG-IDDC   TO POSTSUM-TRANSTYP                          
030200        CALL POSTSUM USING POSTSUM-PARM                                   
030300        ADD +1 TO WS-ANTAL-POSTER                                         
030400     END-READ                                                             
030500     .                                                                    
030600     EJECT                                                                
030700* --- IMS SEKTIONER ---                                                   
030800 S90-OPEN-DAP-SEND SECTION.                                               
030900     DISPLAY 'S90-OPEN-DAP-S'                                             
031000     MOVE 'S90-OPEN-DAP-S' TO CURR-SECTION                                
031100                                                                          
031200     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
031300     MOVE 'OPEN'                     TO SEND-KDFUNC                       
031400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031500                         SEND-OPEN-AREA                                   
031600     IF SEND-KDRC > ZERO                                                  
031700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
031900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032100     END-IF                                                               
032200     .                                                                    
032300                                                                          
032400 S90-CLOSE-DAP-SEND SECTION.                                              
032500     DISPLAY 'S90-CLOSE-DAP-'                                             
032600     MOVE 'S90-CLOSE-DAP-' TO CURR-SECTION                                
032700                                                                          
032800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
032900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033000                                                                          
033100     IF SEND-KDRC > 0                                                     
033200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
033300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
033400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033600     END-IF                                                               
033700     .                                                                    
033800                                                                          
033900 S90-PUT-DAP-HEADER SECTION.                                              
034000     DISPLAY '90-PUT-DAP-HE'                                              
034100     DISPLAY 'HEAD AREA    ' HDR-AREA                                     
034200     MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
034300                                                                          
034400     MOVE 'PUT'                           TO SEND-KDFUNC                  
034500     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
034600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034700                         SEND-KVDLEN                                      
034800                         HDR-AREA                                         
034900     IF SEND-KDRC > ZERO                                                  
035000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035400     END-IF                                                               
035500     .                                                                    
035600 S90-PUT-DOC      SECTION.                                                
035700     DISPLAY   'S90-PUT-DOC '                                             
035800     DISPLAY   'LINE AREA ' DOC-AREA                                      
035900     MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
036000                                                                          
036100     MOVE 'PUT'                           TO SEND-KDFUNC                  
036200     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
036300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036400                         SEND-KVDLEN                                      
036500                         DOC-AREA                                         
036600     IF SEND-KDRC > ZERO                                                  
036700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
036800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
036900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
037000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037100     END-IF                                                               
037200     MOVE 'W51382'   TO POSTSUM-FDNAMN                                    
037300     MOVE 'PUTLINE'  TO POSTSUM-DDNAMN2                                   
037400     MOVE  W-IDDC   TO POSTSUM-TRANSTYP                                   
037500     CALL POSTSUM USING POSTSUM-PARM                                      
037600     .                                                                    
037700                                                                          
037800 IMS-GET-BENA-TEXT SECTION.                                               
037900*    DISPLAY   'IMS-GET-BENA'                                             
038000     MOVE 'IMS-GET-BENA' TO CURR-SECTION                                  
038100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
038200            DELIMITED BY SIZE INTO SSA1                                   
038300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
038400            DELIMITED BY SIZE INTO SSA2                                   
038500     MOVE '  GE' TO GODK-STATUSKODER                                      
038600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
038700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
038800     PERFORM IMS-STATUS-KONTROLL                                          
038900     .                                                                    
039000     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUS-KONTROLL                                          
           .                                                                    
           SKIP3                                                                
039100 IMS-STATUS-KONTROLL SECTION.                                             
039200     SKIP2                                                                
039300     SET STATUS-IX TO 1                                                   
039400     SEARCH GODK-STATUS                                                   
039500       AT END                                                             
039600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039700           DELIMITED BY SIZE INTO FELTEXT                                 
039800         DISPLAY FELTEXT                                                  
039900         CALL FELLOG                                                      
040000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040100         CONTINUE                                                         
040200     END-SEARCH                                                           
040300     .                                                                    
