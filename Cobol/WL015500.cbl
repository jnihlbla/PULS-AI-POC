000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL015500.                                                
000400*AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500*DATE-WRITTEN.   2004/09/07.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*        WL015500 PROGRAM IS A REPLICA OF W4079500 PROGRAM                
001000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001100*                                                                         
001200*                                                                         
001300*    FUNKTION:                                                            
001400*        BAKGRUNDS MPP SOM SKRIVER UT INLÄGGNINGSLISTA.                   
001500*        STARTAS AV WL0153/WL0164                                         
001600*                                                                         
001700*        PROGRAMMET ÄR KOPIERAT FRÅN W60199                               
001800*                                                                         
001900*        PROGRAMMET          LÄSER      WLKREJ (WDA3E)                    
002000*                                       WLKREE (WDA3)                     
002100*                                       WLARTC (WDK6)                     
002200*                                                                         
002300* ADDRESS : 'CARPARTS.LDC.PRBINNINGLISTBG'                                
002400*                                                                         
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: WL0155U                                             
002800*        REQUEST:     WZ01REQU                                            
002900*                     WL0155I1                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        RESPONSE:    WZ01RESP                                            
003300*                     WZ04HRD                                             
003400*                     WL01551                                             
003500*                     WL01552                                             
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'WL015500'.            
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004800 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005100                                                                          
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  WS-IDSKYLT-CN               PIC X(3)    VALUE 'RCN'.                 
005500 77  WS-IDSKYLT-GB               PIC X(3)    VALUE 'GB '.                 
005600 77  WS-CP-UTF8                  PIC X(4)    VALUE 'UTF8'.                
005700 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
005800                                                                          
005900 77  MAX-TAB-IX                  PIC S9(3)   VALUE +500 COMP-3.           
006000 77  WS-IX                       PIC 9(3).                                
006100 77  ANTAL-RADER                 PIC 9(5).                                
006200                                                                          
006300*    --- PARAMETERS TO ABEND                                              
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800                                                                          
006900*01  -COPY WWDC99                                                         
007000                                                                          
007100     EJECT                                                                
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007900     03  WZ04HDR                 PIC X(8)    VALUE 'WZ04HDR '.            
008000     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
008100     SKIP3                                                                
008200* VARIABLER TILL SUBPROGRAM W006PRR1                                      
008300*01  -COPY W006PRAR                                                       
008400     SKIP2                                                                
008500     EJECT                                                                
008600 01  WS-RAPP-AREA.                                                        
008700     03  WS-RAPP-LISTID.                                                  
008800         05  FILLER              PIC X(5)    VALUE 'ILIST'.               
008900         05  WS-RAPP-IDILIST     PIC 9(5)    VALUE ZERO.                  
009000     03  WS-RAPP-LISTRAD.                                                 
009100         05  FILLER              PIC X(2)    VALUE SPACE.                 
009200         05  WS-RAPP-RAD         PIC X(130).                              
009300     03  WS-DUMMY                PIC X(1).                                
009400     03  WS-RAPP-PRINTER         PIC X(8).                                
009500     EJECT                                                                
009600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009700*                                                                         
009800 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
009900*   -COPY WZ01SEND                                                        
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'WZ01SUB '.             
010200*   -COPY WZ01SUB                                                         
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
010500                                                                          
010600 01  REQU-AREA.                                                           
010700*    03 -COPY WZ01REQU                                                    
010800*    03 -COPY WL0155I1                                                    
010900                                                                          
011000 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
011100                                                                          
011200 01  RESP-AREA.                                                           
011300*    03 -COPY WZ01RESP                                                    
011400                                                                          
011500 01  FILLER                      PIC X(16)  VALUE 'HDR-AREA'.             
011600                                                                          
011700 01  HDR-AREA.                                                            
011800*    03 -COPY WZ01REQU -PRE HDR-                                          
011900*    03 -COPY WZ04HDR                                                     
012000                                                                          
012100 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-HEAD'.            
012200 01  RESP-AREA-HEAD.                                                      
012300*    03 -COPY WL01551                                                     
012400                                                                          
012500 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-LINE'.            
012600 01  RESP-AREA-LINE.                                                      
012700*    03 -COPY WL01552                                                     
012800                                                                          
012900     EJECT                                                                
013000 01  FILLER                 PIC X(16)  VALUE 'WTRAUTF8-AREA'.             
013100*01  -COPY WTRAUTF8                                                       
013200                                                                          
013300     EJECT                                                                
013400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*                                                                         
013600 01  FILLER                 PIC X(16)   VALUE 'IMS-WS'.                   
013700     SKIP3                                                                
013800 01  NYCKLAR-TILL-DLI.                                                    
013900     EJECT                                                                
014000     03  W-WDA2E1KY-MIN-X.                                                
014100        05  W-A2E1KY-IDDC-MIN        PIC  X(2) VALUE SPACE.               
014200        05  W-A2E1KY-IDILIST-MIN     PIC  9(5) VALUE ZERO.                
014300        05  W-A2E1KY-ADLAGOMR-MIN    PIC S9(3) COMP-3 VALUE ZERO.         
014400        05  W-A2E1KY-ADGANG-MIN      PIC S9(3) COMP-3 VALUE ZERO.         
014500        05  W-A2E1KY-ADPLATS-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
014600        05  W-A2E1KY-IDDISTR-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
014700        05  W-A2E1KY-IDKUNDNR-MIN    PIC S9(7) COMP-3 VALUE ZERO.         
014800        05  W-A2E1KY-IDRAPPNR-MIN    PIC  X(7) VALUE SPACE.               
014900        05  W-A2E1KY-IDARTNR-MIN     PIC S9(9) COMP-3 VALUE ZERO.         
015000        05  W-A2E1KY-IDRADNR-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
015100                                                                          
015200     03  W-WDA2E1KY-MAX-X.                                                
015300        05  W-A2E1KY-IDDC-MAX        PIC  X(2) VALUE SPACE.               
015400        05  W-A2E1KY-IDILIST-MAX     PIC  9(5) VALUE ZERO.                
015500        05  W-A2E1KY-ADLAGOMR-MAX    PIC S9(3) COMP-3 VALUE ZERO.         
015600        05  W-A2E1KY-ADGANG-MAX      PIC S9(3) COMP-3 VALUE ZERO.         
015700        05  W-A2E1KY-ADPLATS-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
015800        05  W-A2E1KY-IDDISTR-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
015900        05  W-A2E1KY-IDKUNDNR-MAX    PIC S9(7) COMP-3 VALUE ZERO.         
016000        05  W-A2E1KY-IDRAPPNR-MAX    PIC  X(7) VALUE SPACE.               
016100        05  W-A2E1KY-IDARTNR-MAX     PIC S9(9) COMP-3 VALUE ZERO.         
016200        05  W-A2E1KY-IDRADNR-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
016300                                                                          
016400     03  W-IDLEVANM-X.                                                    
016500         05  W-IDDISTR           PIC S9(5)    COMP-3 VALUE ZERO.          
016600         05  W-IDKUNDNR          PIC S9(7)    COMP-3 VALUE ZERO.          
016700         05  W-IDRAPPNR          PIC X(7)     VALUE SPACE.                
016800                                                                          
016900     03  W-WDA211KY-X.                                                    
017000       04   W-IDARTNR-X.                                                  
017100        05  W-IDARTNR                PIC S9(9) COMP-3 VALUE ZERO.         
017200       04   W-IDRADNR                PIC S9(5) COMP-3 VALUE ZERO.         
017300                                                                          
017400     03  W-KDSEGKEY-X.                                                    
017500        05  W-KDSEGKEY               PIC 9(1)        VALUE 1.             
017600                                                                          
017700     03  W-IDDC-X.                                                        
017800        05  W-IDDC                   PIC  X(2)        VALUE '11'.         
017900                                                                          
018000     03  W-IDSKYLT-X.                                                     
018100        05  W-IDSKYLT                PIC  X(3).                           
018200                                                                          
018300     03  W-IDARTNR-B.                                                     
018400        05  W-IDARTNR-BEART         PIC S9(9)   COMP-3 VALUE ZERO.        
           03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
018500                                                                          
018600     SKIP2                                                                
018700*    --- STATUS-KOD FRÅN IMS                                              
018800 01  STATUS-WS                   PIC XX.                                  
018900     88  SEGMENT-FINNS                       VALUE '  '.                  
019000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019300     SKIP2                                                                
019400 01  GODK-STATUSKODER.                                                    
019500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019600     SKIP3                                                                
019700 01  SSA1                        PIC X(112).                              
019800 01  SSA2                        PIC X(64).                               
019900     EJECT                                                                
020000*    --- IMS FUNKTIONSKODER                                               
020100*01  -COPY W0003                                                          
020200     EJECT                                                                
020300*    ---  DLI INPUT-OUTPUT AREA                                           
020400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020500     SKIP3                                                                
020600 01  DLI-IO-AREA1.                                                        
020700     03  IO-AREA1                PIC X(1200)  VALUE SPACE.                
020800     SKIP3                                                                
020900     03  WLKREE01 REDEFINES IO-AREA1.                                     
021000*        05  -COPY WDA201                                                 
021100     EJECT                                                                
021200     03  WLKREE11 REDEFINES IO-AREA1.                                     
021300*        05  -COPY WDA211                                                 
021400     EJECT                                                                
021500     03  WLKREE21 REDEFINES IO-AREA1.                                     
021600*        05  -COPY WDA221                                                 
021700     EJECT                                                                
021800     03  WLKREJ01 REDEFINES IO-AREA1.                                     
021900*        05  -COPY WDA2E1                                                 
022000     EJECT                                                                
022100     03  WLARTC11 REDEFINES IO-AREA1.                                     
022200*        05  -COPY WDK611   -PRE ARTC-                                    
022300     EJECT                                                                
022400     03  WLARTS11 REDEFINES IO-AREA1.                                     
022500*        05  -COPY WDK711   -PRE ARTS-                                    
022600     EJECT                                                                
022700     03  WLBENA11 REDEFINES IO-AREA1.                                     
022800*        05  -COPY WDD311                                                 
022900     EJECT                                                                
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
           EJECT                                                                
023000                                                                          
023100 LINKAGE SECTION.                                                         
023200                                                                          
023300 01  MSG-PCB                     PIC X.                                   
023400                                                                          
023500 01  DISTRDOC-PCB                PIC X.                                   
023600*01  -COPY W0008  -PRE KREJ-                                              
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023900*01  -COPY W0008  -PRE KREE-                                              
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200*01  -COPY W0008  -PRE ARTC-                                              
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500*01  -COPY W0008  -PRE BENA-                                              
024600     05  FILLER                  PIC X.                                   
024700     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
024800 PROCEDURE DIVISION  USING  MSG-PCB DISTRDOC-PCB                          
024900                            KREJ-PCB KREE-PCB ARTC-PCB BENA-PCB           
                                  WDB6-PCB.                                     
025000                                                                          
025100     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB                           
025200                            KREJ-PCB KREE-PCB ARTC-PCB BENA-PCB           
                                  WDB6-PCB.                                     
025300                                                                          
025400     PERFORM S04-FETCH-REQUEST-ARGUMENT                                   
025500     IF SUB-KDRC = 0                                                      
025600          PERFORM A-INIT                                                  
025700          PERFORM B-SKAPA-ILISTA                                          
025800     END-IF                                                               
025900                                                                          
026000     MOVE ZERO TO RETURN-CODE                                             
026100                                                                          
026200     GOBACK                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 A-INIT SECTION.                                                          
026600                                                                          
026700                                                                          
026800     MOVE SPACE                TO RESP-AREA                               
026900                                  HDR-AREA                                
027000                                  RESP-AREA-HEAD                          
027100                                  RESP-AREA-LINE                          
027200                                                                          
027300*    -- BEART SKA VARA SPACE I UNICODE                                    
027310     MOVE ALL X'20'            TO L155-BEART                              
027320                                                                          
027330                                                                          
027340                                                                          
027400     .                                                                    
027500     EJECT                                                                
027600 B-SKAPA-ILISTA    SECTION.                                               
027700                                                                          
027800**OPEN PRINTER                                                            
027900     IF WZ04-SEND-IDCOM = ZERO                                            
028000       PERFORM S05-SEND-OPEN                                              
028100       MOVE SEND-IDCOM                  TO WZ04-SEND-IDCOM                
028200     END-IF                                                               
028300                                                                          
           IF REQU-L155-KVRADER IS NUMERIC                                      
028400      MOVE REQU-L155-KVRADER TO ANTAL-RADER                               
           ELSE                                                                 
            MOVE 0 TO ANTAL-RADER                                               
           END-IF                                                               
028900                                                                          
029000     MOVE 1                             TO WS-IX                          
029100**** PERFORM UNTIL WS-IX > MAX-TAB-IX OR                                  
029200     PERFORM UNTIL WS-IX > ANTAL-RADER                                    
029300     MOVE 001                           TO HDR-REQU-IDMSGVER              
029400     MOVE 'BINNING-LIST-D'              TO HDR-IDOUTTYPE                  
029500     MOVE REQU-L155-IDDC                TO HDR-IDOUTREC(1:2)              
029600     MOVE REQU-IDUSER IN REQU-AREA      TO HDR-IDOUTREC(3:8)              
029700     MOVE REQU-L155-IDILIST (WS-IX)     TO HDR-IDLIST                     
029800*HDR                                                                      
029900     PERFORM S05-PUT-HEADER                                               
030000                                                                          
030100     MOVE LOW-VALUE                     TO W-WDA2E1KY-MIN-X               
030200     MOVE HIGH-VALUE                    TO W-WDA2E1KY-MAX-X               
030300     MOVE REQU-L155-IDDC                TO W-A2E1KY-IDDC-MIN              
030400                                           W-A2E1KY-IDDC-MAX              
030500****** PERFORM UNTIL WS-IX > MAX-TAB-IX OR                                
030600******               WS-IX > REQU-L155-KVRADER                            
030700         MOVE REQU-L155-IDILIST (WS-IX)   TO W-A2E1KY-IDILIST-MIN         
030800                                        W-A2E1KY-IDILIST-MAX              
030900                                                                          
031000         MOVE '1'                         TO L155-IDAFPRCD-1              
031100         MOVE REQU-L155-IDDC              TO L155-IDDC                    
031200         MOVE REQU-L155-IDILIST (WS-IX)   TO L155-IDILIST                 
031300                                                                          
031400***DOCUMENT HEADER                                                        
031500         PERFORM S05-PUT-REPORT-HEAD                                      
031600                                                                          
031700         PERFORM IMS-GU-KREJ-KREJ01                                       
031800         IF SEGMENT-FINNS                                                 
031900           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
032000             MOVE SEQE-IDDISTR            TO W-IDDISTR                    
032100             MOVE SEQE-IDKUNDNR           TO W-IDKUNDNR                   
032200             MOVE SEQE-IDRAPPNR           TO W-IDRAPPNR                   
032300             MOVE SEQE-IDARTNR            TO W-IDARTNR                    
032400             MOVE SEQE-IDRADNR            TO W-IDRADNR                    
032500                                                                          
032600             PERFORM IMS-GU-KREE-KREE11                                   
032700             PERFORM BA-SKAPA-RAD                                         
032800             PERFORM IMS-GN-KREJ-KREJ01                                   
032900           END-PERFORM                                                    
033000         END-IF                                                           
033100         ADD 1                            TO WS-IX                        
033200****** END-PERFORM                                                        
033300     END-PERFORM                                                          
033400                                                                          
033500**CLOSE PRINTER                                                           
033600     IF WZ04-SEND-IDCOM > ZERO                                            
033700       PERFORM S05-SEND-CLOSE                                             
033800       MOVE ZERO TO WZ04-SEND-IDCOM                                       
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
034300 BA-SKAPA-RAD       SECTION.                                              
034400     MOVE '2'                  TO L155-IDAFPRCD-2                         
034500     MOVE LEV-ADLAGOMR         TO L155-ADLAGOMR                           
034600     MOVE LEV-ADGANG           TO L155-ADGANG                             
034700     MOVE LEV-ADPLATS          TO L155-ADPLATS                            
034800                                                                          
034900     MOVE LEV-IDARTNR          TO L155-IDARTNR                            
035000                                  W-IDARTNR-BEART                         
035100     MOVE W-IDDISTR            TO L155-IDDISTR                            
035200     MOVE W-IDKUNDNR           TO L155-IDKUNDNR                           
035300     MOVE W-IDRAPPNR           TO L155-IDRAPPNR                           
035400     MOVE LEV-KVANTAL-ILI      TO L155-KVLEVANM-KVAR                      
035500                                                                          
035600     PERFORM BAB-HAMTA-BENAMNING                                          
035700                                                                          
035800     PERFORM BAD-HAMTA-PB                                                 
035900                                                                          
036000* DOCUMENT DETAIL-LINE                                                    
036100     PERFORM S05-PUT-REPORT-LINE                                          
036200     .                                                                    
036300     EJECT                                                                
036400                                                                          
036500 BAB-HAMTA-BENAMNING       SECTION.                                       
036600                                                                          
036700     MOVE REQU-L155-IDDC        TO WS-IDDC                                
                                         W-IDDC-B6                              
           PERFORM IMS-GU-WDB601                                                
                                                                                
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
037500                                                                          
037600     PERFORM IMS-GU-WLBENA11                                              
037700     IF SEGMENT-FINNS                                                     
037800       MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                     
037900     ELSE                                                                 
038000       MOVE '?'                TO L155-BEART                              
038100       MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                            
             MOVE SPACE              TO TRAUTF8-TECONV-FROM                     
038200     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WLBENA11                                             
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
038300                                                                          
038400*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
038500     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
038600                                                                          
038700*    -- MOVE CONVERTED DESCRIPTION TO OUTPUT WORK FIELD                   
038800     MOVE TRAUTF8-TECONV-TO    TO L155-BEART                              
038900     .                                                                    
039000     EJECT                                                                
039100 BAD-HAMTA-PB              SECTION.                                       
039200                                                                          
039300     PERFORM IMS-GU-WLARTC11                                              
039400     MOVE ZERO                 TO L155-KVPB-SEP                           
039500                                  L155-BEFT                               
039600     .                                                                    
039700                                                                          
039800* DISPATCHER-SEKTIONER                                                    
039900     SKIP3                                                                
040000 S04-FETCH-REQUEST-ARGUMENT SECTION.                                      
040100                                                                          
040200     MOVE 'GETARG'                        TO SUB-KDFUNC                   
040300     MOVE 'CARPARTS.LDC.PRBINNINGLISTBG'  TO SUB-ADDISPABS                
040400     MOVE LENGTH OF REQU-AREA             TO SUB-KVDLEN                   
040500                                                                          
040600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
040700                                                                          
040800     IF SUB-KDRC > 0                                                      
040900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
041000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
041100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
041200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041300     END-IF                                                               
041400     .                                                                    
041500     SKIP3                                                                
041600 S05-SEND-OPEN SECTION.                                                   
041700                                                                          
041800     MOVE 'CARPARTS.DAP.DISTRDOCWEB'      TO SEND-ADDISPABS               
041900     MOVE 'OPEN'                          TO SEND-KDFUNC                  
042000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
042100                         SEND-OPEN-AREA                                   
042200     IF SEND-KDRC > ZERO                                                  
042300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
042400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
042500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
042600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042700     END-IF                                                               
042800     .                                                                    
042900     SKIP3                                                                
043000 S05-PUT-HEADER SECTION.                                                  
043100                                                                          
043200     MOVE 'PUT'                           TO SEND-KDFUNC                  
043300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
043400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
043500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
043600                         SEND-KVDLEN                                      
043700                         HDR-AREA                                         
043800     IF SEND-KDRC > ZERO                                                  
043900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
044000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
044100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
044200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 S05-PUT-REPORT-HEAD    SECTION.                                          
044700                                                                          
044800     MOVE 'PUT'                           TO SEND-KDFUNC                  
044900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
045000     MOVE LENGTH OF RESP-AREA-HEAD        TO SEND-KVDLEN                  
045100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
045200                         SEND-KVDLEN                                      
045300                         RESP-AREA-HEAD                                   
045400     IF SEND-KDRC > ZERO                                                  
045500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
045600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
045700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
045800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
045900     END-IF                                                               
046000     .                                                                    
046100     SKIP3                                                                
046200 S05-PUT-REPORT-LINE    SECTION.                                          
046300                                                                          
046400     MOVE 'PUT'                           TO SEND-KDFUNC                  
046500     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
046600     MOVE LENGTH OF RESP-AREA-LINE        TO SEND-KVDLEN                  
046700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
046800                         SEND-KVDLEN                                      
046900                         RESP-AREA-LINE                                   
047000     IF SEND-KDRC > ZERO                                                  
047100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
047200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
047300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
047400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
047500     END-IF                                                               
047600     .                                                                    
047700     SKIP3                                                                
047800 S05-SEND-CLOSE SECTION.                                                  
047900                                                                          
048000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
048100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
048200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
048300     .                                                                    
048400     EJECT                                                                
048500* --- IMS SEKTIONER ---                                                   
048600     SKIP3                                                                
048700 IMS-GU-KREJ-KREJ01 SECTION.                                              
048800     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
048900                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
049000          DELIMITED BY SIZE INTO SSA1                                     
049100     MOVE '  GE' TO GODK-STATUSKODER                                      
049200     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA1 SSA1                     
049300     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
049400     PERFORM IMS-STATUSKONTROLL                                           
049500     .                                                                    
049600     SKIP2                                                                
049700 IMS-GN-KREJ-KREJ01 SECTION.                                              
049800     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
049900                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
050000          DELIMITED BY SIZE INTO SSA1                                     
050100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
050200     CALL CBLTDLI USING GN KREJ-PCB DLI-IO-AREA1 SSA1                     
050300     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     .                                                                    
050600     EJECT                                                                
050700 IMS-GU-KREE-KREE11 SECTION.                                              
050800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
050900          DELIMITED BY SIZE INTO SSA1                                     
051000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
051100          DELIMITED BY SIZE INTO SSA2                                     
051200     MOVE '  ' TO GODK-STATUSKODER                                        
051300     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA1 SSA1 SSA2                
051400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
051500     PERFORM IMS-STATUSKONTROLL                                           
051600     .                                                                    
051700     EJECT                                                                
051800 IMS-GU-WLARTC11    SECTION.                                              
051900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
052000          DELIMITED BY SIZE INTO SSA1                                     
052100     MOVE 'WLARTC11 ' TO SSA2                                             
052200     MOVE '  GE' TO GODK-STATUSKODER                                      
052300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1 SSA2                
052400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     .                                                                    
052700     SKIP3                                                                
052800 IMS-GU-WLBENA11    SECTION.                                              
052900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-B ')'                         
053000          DELIMITED BY SIZE INTO SSA1                                     
053100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
053200          DELIMITED BY SIZE INTO SSA2                                     
053300     MOVE '  GE' TO GODK-STATUSKODER                                      
053400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA1 SSA1 SSA2                
053500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
053600     PERFORM IMS-STATUSKONTROLL                                           
053700     .                                                                    
053800     SKIP3                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
053900 IMS-STATUSKONTROLL SECTION.                                              
054000                                                                          
054100     SET STATUS-IX TO 1                                                   
054200     SEARCH GODK-STATUS                                                   
054300       AT END                                                             
054400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
054500         DELIMITED BY SIZE INTO FELTEXT                                   
054600         CALL FELLOG                                                      
054700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054800         CONTINUE                                                         
054900     END-SEARCH                                                           
055000     .                                                                    
