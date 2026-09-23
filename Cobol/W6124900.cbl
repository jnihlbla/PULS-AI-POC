000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6124900.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   JANUARI 2007.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        LÄSER FIL W6124A AVVIKELSE VID REFILL INLEVERANS                 
001000*        OCH SKAPAR LISTA.                                                
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U1000 - D&P ERROR                                                
001400*                                                                         
001500                                                                          
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100                                                                          
002200*          --- INFIL                                                      
002300     SELECT W6124A                     ASSIGN TO W61249D1.                
002400                                                                          
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700                                                                          
002800 FD  W6124A                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W6124A    -L.                                                  
003300                                                                          
003400                                                                          
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W6124900'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
004100 77  SPAR-IDFAKT                 PIC 9(7)    VALUE ZERO.                  
004200 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004300 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600 77  W6124A-EOF-SW               PIC X       VALUE 'N'.                   
004700     88  END-OF-W6124A                       VALUE 'Y'.                   
004800     EJECT                                                                
004900*                                                                         
005000 01  HDR-AREA.                                                            
005100*    03  -COPY WZ01REQU                                                   
005200*    03  -COPY WZ04HDR                                                    
005300     EJECT                                                                
005400 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
005500 01  SEND-AREA.                                                           
005600*    03  -COPY WZ01SEND                                                   
005700     EJECT                                                                
005800 01  SEND-RAD-STYRTECKEN.                                                 
005900     03  STYRTECKEN-RAD          PIC X.                                   
006000     03  SEND-RAD                PIC X(160)  VALUE SPACE.                 
006100*    --- CONTROL CHARACTERS                                               
006200 01  WS-SKIP1                    PIC X       VALUE ' '.                   
006300 01  WS-SKIP2                    PIC X       VALUE '0'.                   
006400 01  WS-SKIP3                    PIC X       VALUE '-'.                   
006500 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
006600     EJECT                                                                
006700                                                                          
006800 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
006900                                                                          
007000     EJECT                                                                
007100*    --- LISTLAYOUT                                                       
007200 01  LISTA.                                                               
007300     03  RUBRIK-1.                                                        
007400         05  FILLER     PIC X       VALUE SPACE.                          
007500         05  FILLER     PIC X(15)   VALUE 'VCCS W61249-001'.              
007600         05  FILLER     PIC X(3)    VALUE SPACE.                          
007700         05  FILLER     PIC X(2)    VALUE 'DC'.                           
007800         05  FILLER     PIC X       VALUE SPACE.                          
007900         05  RUB1-IDDC  PIC X(2)    VALUE SPACE.                          
008000         05  FILLER     PIC X(3)    VALUE SPACE.                          
008100         05  FILLER     PIC X(6)    VALUE 'DATUM'.                        
008200         05  FILLER     PIC X(2)    VALUE SPACE.                          
008300         05  RUB1-DAT   PIC X(6)    VALUE SPACE.                          
008400                                                                          
008500     03  RUBRIK-2.                                                        
008600         05  FILLER           PIC X(4)    VALUE SPACE.                    
008700         05  RUB2-IDFAKT      PIC X(10)   VALUE 'FAKT.NR   '.             
008710         05  FILLER           PIC X(3)    VALUE SPACE.                    
008800         05  RUB2-IDDISTR     PIC X(10)   VALUE 'DISTRIKT  '.             
008900         05  FILLER           PIC X(3)    VALUE SPACE.                    
009000         05  RUB2-IDKUNDNR    PIC X(7)    VALUE 'KUND NR'.                
009100         05  FILLER           PIC X(3)    VALUE SPACE.                    
009200         05  RUB2-IDORDNR     PIC X(10)   VALUE 'ORDER NR  '.             
009300         05  FILLER           PIC X(3)    VALUE SPACE.                    
009400         05  RUB2-IDKOLLI     PIC X(8)    VALUE 'KOLLI NR'.               
009500         05  FILLER           PIC X(6)    VALUE SPACE.                    
009600         05  RUB2-IDARTNR     PIC X(6)    VALUE 'ART.NR'.                 
009700         05  FILLER           PIC X(3)    VALUE SPACE.                    
009800         05  RUB2-KVAVIS      PIC X(10)   VALUE 'AVIS.ANTAL'.             
009900         05  FILLER           PIC X(3)    VALUE SPACE.                    
010000         05  RUB2-KVANTAL     PIC X(10)   VALUE 'AVIK.ANTAL'.             
010100         05  FILLER           PIC X(6)    VALUE SPACE.                    
010200         05  RUB2-SUMMA       PIC X(12)   VALUE 'VÄRDE AVVIK.'.           
010300         05  FILLER           PIC X(3)    VALUE SPACE.                    
010400         05  RUB2-TYP         PIC X(9)    VALUE 'AVVIK.TYP'.              
010500         05  FILLER           PIC X(17)   VALUE SPACE.                    
010600         05  RUB2-PICKER      PIC X(9)    VALUE 'PICKER   '.              
010700                                                                          
010800     03  RAD.                                                             
010900         05  FILLER           PIC X(4)    VALUE SPACE.                    
011000         05  RAD-IDFAKT       PIC ZZZZZZ9.                                
011100         05  FILLER           PIC X(5)    VALUE SPACE.                    
011200         05  RAD-IDDISTR      PIC ZZZZ9.                                  
011300         05  FILLER           PIC X(8)    VALUE SPACE.                    
011400         05  RAD-IDKUNDNR     PIC ZZZZZZ9.                                
011500         05  FILLER           PIC X(3)    VALUE SPACE.                    
011600         05  RAD-IDKUNDRF     PIC X(10)   VALUE SPACE.                    
011700         05  FILLER           PIC X(3)    VALUE SPACE.                    
011800         05  RAD-IDKOLLI      PIC ZZZZ9.                                  
011900         05  FILLER           PIC X(5)    VALUE SPACE.                    
012000         05  RAD-IDARTNR      PIC ZZZZZZZZ9.                              
012100         05  FILLER           PIC X(6)    VALUE SPACE.                    
012200         05  RAD-KVAVIS       PIC ZZZZZZ9.                                
012300         05  FILLER           PIC X(6)    VALUE SPACE.                    
012400         05  RAD-KVANTAL      PIC ZZZZZZ9.                                
012500         05  FILLER           PIC X(3)    VALUE SPACE.                    
012600         05  RAD-SUMMA        PIC ZZZZZZZZZZZ9V,99.                       
012700         05  FILLER           PIC X(3)    VALUE SPACE.                    
012800         05  RAD-TYP          PIC X(25)   VALUE SPACE.                    
012900         05  FILLER           PIC X(1)    VALUE SPACE.                    
013000         05  RAD-PICKER       PIC X(8)    VALUE SPACE.                    
013100                                                                          
013200                                                                          
013300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013400 01  FILLER REDEFINES DAGENS-DATUM.                                       
013500     03  DAGENS-AA               PIC 9(2).                                
013600     03  DAGENS-MM               PIC 9(2).                                
013700     03  DAGENS-DD               PIC 9(2).                                
013800     EJECT                                                                
013900 01  GENERAL-SUBPROGRAMS.                                                 
014000*                                                                         
014100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
014400     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
014500     SKIP2                                                                
014600*    --- PARAMETERS TO ABEND                                              
014700                                                                          
014800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015100     SKIP2                                                                
015200 01  ERRTEXT.                                                             
015300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
015400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL POSTSUM                                          
015700*                                                                         
015800*01  -COPY W0005   -PRE  POSTSUM-                                         
015900     EJECT                                                                
016000 01  IN-AREA-START               PIC X(24)   VALUE                        
016100                                 'IN-AREA-START  '.                       
016200     SKIP2                                                                
016300*01  AREA -COPY W6124A      -PRE IN-                                      
016400     EJECT                                                                
016500                                                                          
016600 LINKAGE SECTION.                                                         
016700                                                                          
016800*01  -COPY W0009            -PRE MSG-                                     
016900                                                                          
017000*01  -COPY W0009            -PRE DISTRDOC-                                
017100     EJECT                                                                
017200 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
017300 MAIN SECTION.                                                            
017400     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
017500                                                                          
017600     PERFORM A-INIT                                                       
017700                                                                          
017800     PERFORM S01-LAS-W6124A                                               
017900     PERFORM UNTIL END-OF-W6124A                                          
018000       MOVE IN-IDDC-REC TO SPAR-IDDC                                      
018100                           WS-IDDC                                        
018200       MOVE IN-IDFAKT TO SPAR-IDFAKT                                      
018300       PERFORM S90-SEND-OPEN                                              
018400       PERFORM S90-PUT-DAP-START                                          
018500                                                                          
018600       PERFORM B-INIT-DC                                                  
018700       PERFORM C-PRINT-HEAD                                               
018800       PERFORM UNTIL END-OF-W6124A OR                                     
018900         (SPAR-IDDC NOT = IN-IDDC-REC) OR                                 
019000         (SPAR-IDFAKT NOT = IN-IDFAKT)                                    
019100           PERFORM D-RAD-DATA                                             
019200         PERFORM S01-LAS-W6124A                                           
019300       END-PERFORM                                                        
019400       PERFORM S90-SEND-CLOSE                                             
019500     END-PERFORM                                                          
019600                                                                          
019700     PERFORM Z-FINIT                                                      
019800                                                                          
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT SECTION.                                                          
020400                                                                          
020500     OPEN INPUT  W6124A                                                   
020600     ACCEPT DAGENS-DATUM FROM DATE                                        
020700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020800     .                                                                    
020900     EJECT                                                                
021000 B-INIT-DC SECTION.                                                       
021100                                                                          
021200     MOVE IN-IDDC-REC                TO RUB1-IDDC                         
021300     .                                                                    
021400     EJECT                                                                
021500 C-PRINT-HEAD SECTION.                                                    
021600                                                                          
021700     MOVE SPACE                      TO SEND-RAD-STYRTECKEN               
021800     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
021900     MOVE SPACE                      TO SEND-RAD                          
022000     PERFORM S90-PUT-DOC-LINE                                             
022100     MOVE IN-IDDC-REC                TO RUB1-IDDC                         
022200     MOVE DAGENS-DATUM               TO RUB1-DAT                          
022300     MOVE RUBRIK-1                   TO SEND-RAD                          
022400     PERFORM S90-PUT-DOC-LINE                                             
022500                                                                          
022600     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
022700     MOVE SPACE                      TO SEND-RAD                          
022800     PERFORM S90-PUT-DOC-LINE                                             
022900     MOVE RUBRIK-2                   TO SEND-RAD                          
023000     PERFORM S90-PUT-DOC-LINE                                             
023100                                                                          
023200     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
023300     MOVE SPACE                      TO SEND-RAD                          
023400     PERFORM S90-PUT-DOC-LINE                                             
023500     .                                                                    
023600     EJECT                                                                
023700 D-RAD-DATA SECTION.                                                      
023800                                                                          
023900     MOVE IN-IDFAKT        TO RAD-IDFAKT                                  
024000     MOVE IN-IDDISTR       TO RAD-IDDISTR                                 
024100     MOVE IN-IDKUNDRF      TO RAD-IDKUNDRF                                
024200     MOVE IN-IDKUNDNR      TO RAD-IDKUNDNR                                
024300     MOVE IN-IDKOLLI       TO RAD-IDKOLLI                                 
024400     MOVE IN-IDARTNR       TO RAD-IDARTNR                                 
024500     MOVE IN-KVAVIS        TO RAD-KVAVIS                                  
024600     MOVE IN-KVANTAL       TO RAD-KVANTAL                                 
024610     MOVE IN-IDUSER-PIC    TO RAD-PICKER                                  
024700     IF IN-AVVIKELSETYP = 'ÖVERLEV.'                                      
024800       MOVE 'ÖVERLEVERANS' TO RAD-TYP                                     
024900     ELSE                                                                 
025000       IF IN-AVVIKELSETYP = 'UNDERLEV.'                                   
025100          MOVE 'UNDERLEVERANS' TO RAD-TYP                                 
025200       ELSE                                                               
025300         IF IN-AVVIKELSETYP = 'DAM'                                       
025400            MOVE 'SKROT' TO RAD-TYP                                       
025500         ELSE                                                             
025600           IF IN-AVVIKELSETYP = 'LOST'                                    
025700             MOVE 'FÖRLORAT KOLLI' TO RAD-TYP                             
025800           ELSE                                                           
025900             IF IN-AVVIKELSETYP = 'FOUND'                                 
026000               MOVE 'ÅTERFUNNET KOLLI' TO RAD-TYP                         
026100             ELSE                                                         
026200               IF IN-AVVIKELSETYP = 'NY'                                  
026300                 MOVE 'EJ BESTÄLLD ARTIKEL' TO RAD-TYP                    
026400               END-IF                                                     
026500             END-IF                                                       
026600           END-IF                                                         
026700         END-IF                                                           
026800       END-IF                                                             
026900     END-IF                                                               
027000     COMPUTE RAD-SUMMA ROUNDED = IN-KVANTAL * IN-PRARTSTD                 
027100                                                                          
027200     MOVE RAD TO SEND-RAD                                                 
027300     PERFORM S90-PUT-DOC-LINE                                             
027400     .                                                                    
027500     EJECT                                                                
027600 Z-FINIT SECTION.                                                         
027700                                                                          
027800     CLOSE W6124A                                                         
027900     MOVE 'S' TO POSTSUM-OPKOD                                            
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     EJECT                                                                
028300 S01-LAS-W6124A SECTION.                                                  
028400     READ W6124A INTO IN-AREA                                             
028500     AT END                                                               
028600        MOVE HIGH-VALUE TO IN-AREA                                        
028700        SET END-OF-W6124A TO TRUE                                         
028800     NOT AT END                                                           
028900        MOVE 'W6124A'   TO POSTSUM-FDNAMN                                 
029000        MOVE 'W61249D1' TO POSTSUM-DDNAMN2                                
029100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029200        CALL POSTSUM USING POSTSUM-PARM                                   
029300     END-READ                                                             
029400     .                                                                    
029500     EJECT                                                                
029600 S90-SEND-OPEN SECTION.                                                   
029700                                                                          
029800     MOVE 'OPEN'                        TO SEND-KDFUNC                    
029900     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
030000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030100                         SEND-OPEN-AREA                                   
030200     IF SEND-KDRC > 0                                                     
030300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
030400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
030500       DELIMITED BY SIZE INTO ERRTEXT                                     
030600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 S90-PUT-DAP-START SECTION.                                               
031100                                                                          
031200     MOVE 1                       TO REQU-IDMSGVER                        
031300     MOVE 'R'                     TO REQU-KDPGMACT                        
031400     MOVE IDPGM                   TO REQU-IDUSER                          
031500     MOVE 'W61249'                TO HDR-IDOUTTYPE                        
031600     MOVE SPACE                   TO HDR-IDOUTREC                         
031700                                     HDR-IDLIST                           
031800     MOVE WS-IDDC                 TO HDR-IDOUTREC (1:2)                   
031900                                     HDR-IDLIST                           
032000     MOVE 'PUT'                   TO SEND-KDFUNC                          
032100     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
032200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032300                         SEND-KVDLEN                                      
032400                         HDR-AREA                                         
032500     IF SEND-KDRC > ZERO                                                  
032600       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
032700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
032800       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
032900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 S90-PUT-DOC-LINE SECTION.                                                
033400                                                                          
033500     MOVE 'PUT'                           TO SEND-KDFUNC                  
033600*                     -- UTAN STYRTECKEN:                                 
033700     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
033800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033900                         SEND-KVDLEN                                      
034000*                     -- UTAN STYRTECKEN:                                 
034100                         SEND-RAD                                         
034200     IF SEND-KDRC > ZERO                                                  
034300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
034400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
034500       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
034600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 S90-SEND-CLOSE SECTION.                                                  
035100                                                                          
035200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
035300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035400                                                                          
035500     IF SEND-KDRC > 0                                                     
035600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
035700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
035800       DELIMITED BY SIZE INTO ERRTEXT                                     
035900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
