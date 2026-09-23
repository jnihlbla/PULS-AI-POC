000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4281200.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   08/05/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SUMMERAR ANTAL INLAGDA, SKROTADE OCH AVVIKELSE        
001000*        RAPPORTERADE RADER FÖR LDC RETURER KOD 72 PER RETUR-DC.          
001100*        PROGRAMMET INGÅR I VECKORUTIN W428V1.                            
001200*        LÄSER FIL W42811 FRÅN EPLUS PGM W42811 OCH SKICKAR LIST-         
001300*        RADER TILL DISTR. OCH PRINT VIA WZ01.                            
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDB6                                       
001600*                                                                         
001700*        E'TRACKER. 6785206  DATED 2008-05-16                             
001800*                                                                         
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- VECKANS INLAGDA RADER KOD 72 HOS LDC                       
002900     SELECT W42811                     ASSIGN TO W42812D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W42811                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W42811      -L.                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4281200'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  SPAR-IDDISTR                PIC 9(4)    VALUE ZERO.                  
004700 77  SPAR-IDKUNDNR               PIC 9(6)    VALUE ZERO.                  
004800 77  SPAR-IDRAPPNR               PIC 9(7)    VALUE ZERO.                  
004900 77  SPAR-IDDC-RET               PIC X(2)    VALUE SPACE.                 
004910 77  SPAR-DOC-TIAAVV             PIC 9(4)    VALUE ZERO.                  
005000 77  W-KVRETINL-TOT              PIC 9(7)    VALUE ZERO.                  
005100 77  W-KVRETINL-SKR-TOT          PIC 9(7)    VALUE ZERO.                  
005200 77  W-KVAVV-KVANT-TOT           PIC 9(7)    VALUE ZERO.                  
005300 77  W-KVDAGAR-RET-TOT           PIC 9(7)    VALUE ZERO.                  
005400 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
005410 77  W-KVRETINL-SUM              PIC 9(7)    VALUE ZERO.                  
005500                                                                          
005600 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005700 77  KDRC-DISPLAY                PIC Z(5).                                
005800     SKIP2                                                                
005900 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 77  W42811-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W42811                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400*    --- PARAMETERS TO ABEND                                              
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900                                                                          
009000     EJECT                                                                
009100                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
009300*01  -COPY WZ01SEND                                                       
009400                                                                          
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
009700 01  HDR-AREA.                                                            
009800*    03  -COPY WZ01REQU  -PRE HDR-                                        
009900*    03  -COPY WZ04HDR                                                    
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
010200 01  DOC-AREA.                                                            
010300*    03  -COPY W428121                                                    
010400*                                                                         
010500     EJECT                                                                
010600     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                             'IN-AREA-START'.             
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY W42811     -PRE IN-                                       
011200*                                                                         
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700     03  W-IDDC-X.                                                        
011800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012600     88  IMS-EJ-OK                           VALUE 'XD'.                  
012700     SKIP2                                                                
012800 01  GODK-STATUSKODER.                                                    
012900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     SKIP3                                                                
013100 01  SSA1                        PIC X(64).                               
013200 01  SSA2                        PIC X(64).                               
013300     EJECT                                                                
013400*    --- IMS FUNKTIONSKODER                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014000 01  DLI-IO-WDB601.                                                       
014100*    03  -COPY WDB601                                                     
014200     EJECT                                                                
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500*01  -COPY W0009   -PRE MSG-                                              
014600     EJECT                                                                
014700 01  DISTRDOC-PCB                PIC X.                                   
014800     EJECT                                                                
014900                                                                          
015000*01  -COPY W0008  -PRE WDB6-                                              
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB WDB6-PCB.                 
015400 MAIN SECTION.                                                            
015500     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDB6-PCB.                 
015600                                                                          
015700     SKIP2                                                                
015800     PERFORM A-INIT                                                       
015900     PERFORM S01-LAES-W42811                                              
016000                                                                          
016100     PERFORM UNTIL END-OF-W42811                                          
016200       MOVE IN-IDDC-RET   TO SPAR-IDDC-RET                                
016300                            W-IDDC                                        
016310       MOVE IN-TISAAVV-INLINL-TIAAVV TO SPAR-DOC-TIAAVV                   
016400       MOVE ZERO          TO W-KVRETINL-TOT                               
016500                             W-KVRETINL-SKR-TOT                           
016600                             W-KVAVV-KVANT-TOT                            
016700                             W-KVDAGAR-RET-TOT                            
016710                             W-KVRETINL-SUM                               
016800                                                                          
016900       PERFORM S05-OPEN-DAP-SEND                                          
017000       PERFORM S02-FLYTTA-HEADER-DATA                                     
017100       PERFORM S06-PUT-DAP-HEADER                                         
017200                                                                          
017300       PERFORM UNTIL END-OF-W42811 OR                                     
017400            IN-IDDC-RET NOT = SPAR-IDDC-RET                               
017500                                                                          
017600         PERFORM B-BEHANDLA                                               
017740                                                                          
017800       END-PERFORM                                                        
017900                                                                          
018000       PERFORM C-FLYTTA-DATA                                              
018100       PERFORM S08-CLOSE-DAP-SEND                                         
018200     END-PERFORM                                                          
018400                                                                          
018500     PERFORM Z-FINIT                                                      
018600                                                                          
018700     MOVE ZERO TO RETURN-CODE                                             
018800     GOBACK                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 A-INIT SECTION.                                                          
019200     SKIP2                                                                
019300                                                                          
019400     OPEN INPUT W42811                                                    
019500                                                                          
019600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019700     .                                                                    
019800     EJECT                                                                
019900 B-BEHANDLA  SECTION.                                                     
020000                                                                          
020100     MOVE IN-IDDISTR    TO SPAR-IDDISTR                                   
020200     MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR                                  
020300     MOVE IN-IDRAPPNR   TO SPAR-IDRAPPNR                                  
020310                                                                          
020400     COMPUTE W-KVDAGAR-RET-TOT = W-KVDAGAR-RET-TOT +                      
020500                                 IN-KVDAGAR-RET                           
020510     END-COMPUTE                                                          
020600                                                                          
020700     PERFORM UNTIL END-OF-W42811 OR                                       
020800           IN-IDDISTR  NOT = SPAR-IDDISTR  OR                             
020900           IN-IDKUNDNR NOT = SPAR-IDKUNDNR OR                             
021000           IN-IDRAPPNR NOT = SPAR-IDRAPPNR                                
021100                                                                          
021110       IF IN-KVRETINL > 0                                                 
021200         ADD +1 TO W-KVRETINL-TOT                                         
021211       END-IF                                                             
021220                                                                          
021230       IF IN-KVRETINL-SKR > 0                                             
021240         ADD +1  TO W-KVRETINL-SKR-TOT                                    
021250       END-IF                                                             
021320                                                                          
021330       IF IN-KVAVV-KVANT > 0                                              
021340         ADD +1  TO W-KVAVV-KVANT-TOT                                     
021350       END-IF                                                             
021500                                                                          
021600       PERFORM S01-LAES-W42811                                            
021700     END-PERFORM                                                          
021800     .                                                                    
021900     EJECT                                                                
022000 C-FLYTTA-DATA  SECTION.                                                  
022100                                                                          
022300     MOVE 'LINE'              TO DOC-IDAFPRCD                             
022310     MOVE SPAR-IDDC-RET       TO DOC-IDDC                                 
022320     MOVE SPAR-DOC-TIAAVV     TO DOC-TIAAVV                               
022400                                                                          
022410     IF SPAR-IDDC-RET NOT = DCS-IDDC                                      
022411       MOVE SPAR-IDDC-RET TO W-IDDC                                       
022420     END-IF                                                               
022500     PERFORM IMS-GET-WDB601                                               
022510                                                                          
022600     IF SEGMENT-FINNS                                                     
022700       MOVE DCS-ADGMT-PADR(11:20)  TO DOC-ADCITY                          
022800     ELSE                                                                 
022900       MOVE SPACE                  TO DOC-ADCITY                          
023000     END-IF                                                               
023100                                                                          
023200     MOVE W-KVRETINL-TOT      TO DOC-KVRETINL                             
023300     MOVE W-KVRETINL-SKR-TOT  TO DOC-KVRETINL-SKR                         
023400     MOVE W-KVAVV-KVANT-TOT   TO DOC-KVAVV-KVANT                          
023500                                                                          
024201     COMPUTE W-KVRETINL-SUM = W-KVRETINL-TOT + W-KVRETINL-SKR-TOT         
024202     END-COMPUTE                                                          
024203                                                                          
024204     IF W-KVRETINL-SUM > ZERO                                             
024210       COMPUTE DOC-KVDAGDEC ROUNDED =                                     
024220               W-KVDAGAR-RET-TOT / W-KVRETINL-SUM                         
024230       END-COMPUTE                                                        
024231     ELSE                                                                 
024232       MOVE W-KVDAGAR-RET-TOT  TO DOC-KVDAGDEC                            
024233     END-IF                                                               
024240                                                                          
024300     PERFORM S07-PUT-DOC                                                  
024400     .                                                                    
024500     EJECT                                                                
024600 Z-FINIT SECTION.                                                         
024700                                                                          
024800                                                                          
024900     CLOSE W42811                                                         
025000     SKIP2                                                                
025100     MOVE 'S' TO POSTSUM-OPKOD                                            
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400     EJECT                                                                
025500 S01-LAES-W42811  SECTION.                                                
025600     SKIP2                                                                
025700     READ W42811 INTO IN-AREA                                             
025800     AT END                                                               
025900        MOVE HIGH-VALUE TO IN-W42811                                      
026000        SET END-OF-W42811 TO TRUE                                         
026100                                                                          
026200     NOT AT END                                                           
026300        MOVE 'W42811'   TO POSTSUM-FDNAMN                                 
026400        MOVE 'W42812D1' TO POSTSUM-DDNAMN2                                
026500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
026600        CALL POSTSUM USING POSTSUM-PARM                                   
026700     END-READ                                                             
026800     .                                                                    
026900     EJECT                                                                
027000 S02-FLYTTA-HEADER-DATA  SECTION.                                         
027100                                                                          
027200     MOVE 001             TO HDR-REQU-IDMSGVER                            
027300     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
027400     MOVE 'W42812'        TO HDR-REQU-IDUSER                              
027500                                                                          
027600     MOVE 'DISC-72-WEEK   '   TO HDR-IDOUTTYPE                            
027700     MOVE SPACE               TO HDR-IDOUTREC                             
027800     MOVE IN-IDDC-RET         TO HDR-IDOUTREC(1:2)                        
027900     MOVE 'W42812'            TO HDR-IDOUTREC(3:8)                        
028000                                                                          
028100     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
028200     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
028300     .                                                                    
028400     EJECT                                                                
028500 S05-OPEN-DAP-SEND SECTION.                                               
028600*    MOVE 'S05-OPEN-DAP-S' TO CURR-SECTION                                
028700                                                                          
028800     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
028900     MOVE 'OPEN'                     TO SEND-KDFUNC                       
029000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029100                         SEND-OPEN-AREA                                   
029200     IF SEND-KDRC > ZERO                                                  
029300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
029400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
029500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
029600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000                                                                          
030100 S06-PUT-DAP-HEADER SECTION.                                              
030200*    MOVE 'S06-PUT-DAP-HE' TO CURR-SECTION                                
030300                                                                          
030400     MOVE 'PUT'                           TO SEND-KDFUNC                  
030500     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
030600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030700                         SEND-KVDLEN                                      
030800                         HDR-AREA                                         
030900     IF SEND-KDRC > ZERO                                                  
031000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
031200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031400     END-IF                                                               
031500     .                                                                    
031600 S07-PUT-DOC      SECTION.                                                
031700*    MOVE 'S07-PUT-DOC ' TO CURR-SECTION                                  
031800                                                                          
031900     MOVE 'PUT'                           TO SEND-KDFUNC                  
032000     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
032100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032200                         SEND-KVDLEN                                      
032300                         DOC-AREA                                         
032400     IF SEND-KDRC > ZERO                                                  
032500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
032600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
032700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 S08-CLOSE-DAP-SEND SECTION.                                              
033300*    MOVE 'S08-CLOSE-DAP-' TO CURR-SECTION                                
033400                                                                          
033500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
033600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033700                                                                          
033800     IF SEND-KDRC > 0                                                     
033900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
034000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
034100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034300     END-IF                                                               
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700* --- IMS SEKTIONER ---                                                   
034800                                                                          
034900     EJECT                                                                
035000 IMS-GET-WDB601 SECTION.                                                  
035100                                                                          
035200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
035300          DELIMITED BY SIZE INTO SSA1                                     
035400     MOVE '  GE' TO GODK-STATUSKODER                                      
035500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
035600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900     EJECT                                                                
036000 IMS-STATUSKONTROLL SECTION.                                              
036100     SKIP2                                                                
036200     SET STATUS-IX TO 1                                                   
036300     SEARCH GODK-STATUS                                                   
036400       AT END                                                             
036500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036600           DELIMITED BY SIZE INTO FELTEXT                                 
036700         DISPLAY FELTEXT                                                  
036800         CALL FELLOG                                                      
036900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037000         CONTINUE                                                         
037100     END-SEARCH                                                           
037200     .                                                                    
