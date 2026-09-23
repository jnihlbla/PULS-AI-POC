000101 ID DIVISION.                                                             
000202 PROGRAM-ID.     W4281300.                                                
000302 AUTHOR.         OLSSON SUSANNE.                                          
000402 DATE-WRITTEN.   08/05/16.                                                
000502 DATE-COMPILED.                                                           
000602                                                                          
000702                                                                          
000802*    FUNKTION:                                                            
000902*        PROGRAMMET SUMMERAR ANTAL INLAGDA, SKROTADE OCH AVVIKELSE        
001002*        RAPPORTERADE RADER FÖR LDC RETURER KOD 72 PER RETUR-DC.          
001102*        PROGRAMMET INGÅR I RUTIN W428R1.PERIODENS RADER.                 
001202*        LÄSER IN ALLA W42811-FILER FRÅN VECKORUTIN W428V1 OCH            
001302*        SKICKAR RADER TILL DISTR. OCH PRINT VIA WZ01.                    
001402*                                                                         
001502*        PROGRAMMET LÄSER      WDB6                                       
001602*                                                                         
001702*        E'TRACKER. 6785206  DATED 2008-06-18                             
001802*                                                                         
001902*                                                                         
002002                                                                          
002102     SKIP3                                                                
002202 ENVIRONMENT DIVISION.                                                    
002302     SKIP2                                                                
002402 INPUT-OUTPUT SECTION.                                                    
002502                                                                          
002602 FILE-CONTROL.                                                            
002702     SKIP2                                                                
002802*          --- PERIODENS INLAGDA RADER KOD 72 HOS LDC                     
002902     SELECT W42811                     ASSIGN TO W42813D1.                
003002     EJECT                                                                
003102 DATA DIVISION.                                                           
003202     SKIP3                                                                
003302 FILE SECTION.                                                            
003402     SKIP3                                                                
003502 FD  W42811                                                               
003602     RECORDING       F                                                    
003702     BLOCK CONTAINS  0.                                                   
003802                                                                          
003902*01  -COPY W42811      -L.                                                
004002     EJECT                                                                
004102 WORKING-STORAGE SECTION.                                                 
004202                                                                          
004302 77  IDPGM                       PIC X(8)    VALUE 'W4281300'.            
004402 77  JA                          PIC X       VALUE 'J'.                   
004502 77  NEJ                         PIC X       VALUE 'N'.                   
004602 77  SPAR-IDDISTR                PIC 9(4)    VALUE ZERO.                  
004702 77  SPAR-IDKUNDNR               PIC 9(6)    VALUE ZERO.                  
004802 77  SPAR-IDRAPPNR               PIC 9(7)    VALUE ZERO.                  
004902 77  SPAR-IDDC-RET               PIC X(2)    VALUE SPACE.                 
005002 77  SPAR-DOC-TIAAPP             PIC 9(4)    VALUE ZERO.                  
005100 77  W-KVRETINL-TOT              PIC 9(7)    VALUE ZERO.                  
005200 77  W-KVRETINL-SKR-TOT          PIC 9(7)    VALUE ZERO.                  
005300 77  W-KVAVV-KVANT-TOT           PIC 9(7)    VALUE ZERO.                  
005400 77  W-KVDAGAR-RET-TOT           PIC 9(7)    VALUE ZERO.                  
005500 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
005602 77  W-KVRETINL-SUM              PIC 9(7)    VALUE ZERO.                  
005700                                                                          
005800 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005900 77  KDRC-DISPLAY                PIC Z(5).                                
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400                                                                          
006500 77  W42811-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W42811                       VALUE 'J'.                   
006700     EJECT                                                                
006800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300     EJECT                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600*    --- PARAMETERS TO ABEND                                              
008700                                                                          
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009100                                                                          
009200     EJECT                                                                
009300                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
009500*01  -COPY WZ01SEND                                                       
009600                                                                          
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
009900 01  HDR-AREA.                                                            
010000*    03  -COPY WZ01REQU  -PRE HDR-                                        
010100*    03  -COPY WZ04HDR                                                    
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
010400 01  DOC-AREA.                                                            
010502*    03  -COPY W428131                                                    
010600*                                                                         
010700     EJECT                                                                
010800     EJECT                                                                
010900 01  IN-AREA-START               PIC X(24)   VALUE                        
011000                                             'IN-AREA-START'.             
011100     SKIP2                                                                
011200                                                                          
011300*01  AREA -COPY W42811     -PRE IN-                                       
011400*                                                                         
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-IDDC-X.                                                        
012000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012100     SKIP2                                                                
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012800     88  IMS-EJ-OK                           VALUE 'XD'.                  
012900     SKIP2                                                                
013000 01  GODK-STATUSKODER.                                                    
013100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     SKIP3                                                                
013300 01  SSA1                        PIC X(64).                               
013400 01  SSA2                        PIC X(64).                               
013500     EJECT                                                                
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000                                                                          
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014200 01  DLI-IO-WDB601.                                                       
014300*    03  -COPY WDB601                                                     
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014800     EJECT                                                                
014900 01  DISTRDOC-PCB                PIC X.                                   
015000     EJECT                                                                
015100                                                                          
015200*01  -COPY W0008  -PRE WDB6-                                              
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB WDB6-PCB.                 
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDB6-PCB.                 
015800                                                                          
015900     SKIP2                                                                
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-LAES-W42811                                              
016200                                                                          
016300     PERFORM UNTIL END-OF-W42811                                          
016400       MOVE IN-IDDC-RET   TO SPAR-IDDC-RET                                
016500                             W-IDDC                                       
016602       MOVE IN-TISAAPP-INLINL-TIAAPP TO SPAR-DOC-TIAAPP                   
016700       MOVE ZERO          TO W-KVRETINL-TOT                               
016800                             W-KVRETINL-SKR-TOT                           
016900                             W-KVAVV-KVANT-TOT                            
017000                             W-KVDAGAR-RET-TOT                            
017102                             W-KVRETINL-SUM                               
017200                                                                          
017300       PERFORM S05-OPEN-DAP-SEND                                          
017400       PERFORM S02-FLYTTA-HEADER-DATA                                     
017500       PERFORM S06-PUT-DAP-HEADER                                         
017600                                                                          
017700       PERFORM UNTIL END-OF-W42811 OR                                     
017800            IN-IDDC-RET NOT = SPAR-IDDC-RET                               
017930                                                                          
018000         PERFORM B-BEHANDLA                                               
018100                                                                          
018200       END-PERFORM                                                        
018300                                                                          
018400       PERFORM C-FLYTTA-DATA                                              
018410                                                                          
018500       PERFORM S08-CLOSE-DAP-SEND                                         
018600     END-PERFORM                                                          
018700                                                                          
018800     PERFORM Z-FINIT                                                      
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500     SKIP2                                                                
019600                                                                          
019700     OPEN INPUT W42811                                                    
019800                                                                          
019900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020000     .                                                                    
020100     EJECT                                                                
020200 B-BEHANDLA  SECTION.                                                     
020300                                                                          
020400     MOVE IN-IDDISTR    TO SPAR-IDDISTR                                   
020500     MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR                                  
020600     MOVE IN-IDRAPPNR   TO SPAR-IDRAPPNR                                  
020702                                                                          
020800     COMPUTE W-KVDAGAR-RET-TOT = W-KVDAGAR-RET-TOT +                      
020900                                 IN-KVDAGAR-RET                           
021002     END-COMPUTE                                                          
021100                                                                          
021200     PERFORM UNTIL END-OF-W42811 OR                                       
021300           IN-IDDISTR  NOT = SPAR-IDDISTR  OR                             
021400           IN-IDKUNDNR NOT = SPAR-IDKUNDNR OR                             
021500           IN-IDRAPPNR NOT = SPAR-IDRAPPNR                                
021600                                                                          
021610       IF IN-KVRETINL > 0                                                 
021620         ADD +1 TO W-KVRETINL-TOT                                         
021630       END-IF                                                             
021640                                                                          
021650       IF IN-KVRETINL-SKR > 0                                             
021660         ADD +1  TO W-KVRETINL-SKR-TOT                                    
021670       END-IF                                                             
021680                                                                          
021690       IF IN-KVAVV-KVANT > 0                                              
021691         ADD +1  TO W-KVAVV-KVANT-TOT                                     
021692       END-IF                                                             
021902                                                                          
022800       PERFORM S01-LAES-W42811                                            
022900     END-PERFORM                                                          
023000     .                                                                    
023100     EJECT                                                                
023200 C-FLYTTA-DATA  SECTION.                                                  
023300                                                                          
023400     MOVE 'LINE'              TO DOC-IDAFPRCD                             
023502     MOVE SPAR-IDDC-RET       TO DOC-IDDC                                 
023602     MOVE SPAR-DOC-TIAAPP     TO DOC-TIAAPP                               
023700                                                                          
023802     IF SPAR-IDDC-RET NOT = DCS-IDDC                                      
023902       MOVE SPAR-IDDC-RET TO W-IDDC                                       
024002     END-IF                                                               
024100     PERFORM IMS-GET-WDB601                                               
024202                                                                          
024300     IF SEGMENT-FINNS                                                     
024400       MOVE DCS-ADGMT-PADR(11:20)  TO DOC-ADCITY                          
024500     ELSE                                                                 
024600       MOVE SPACE                  TO DOC-ADCITY                          
024700     END-IF                                                               
024800                                                                          
024900     MOVE W-KVRETINL-TOT      TO DOC-KVRETINL                             
025000     MOVE W-KVRETINL-SKR-TOT  TO DOC-KVRETINL-SKR                         
025100     MOVE W-KVAVV-KVANT-TOT   TO DOC-KVAVV-KVANT                          
025200                                                                          
026001     COMPUTE W-KVRETINL-SUM = W-KVRETINL-TOT + W-KVRETINL-SKR-TOT         
026102     END-COMPUTE                                                          
026202                                                                          
026302     COMPUTE DOC-KVDAGDEC ROUNDED =                                       
026402             W-KVDAGAR-RET-TOT / W-KVRETINL-SUM                           
026502     END-COMPUTE                                                          
026602                                                                          
026700     PERFORM S07-PUT-DOC                                                  
026800     .                                                                    
026900     EJECT                                                                
027000 Z-FINIT SECTION.                                                         
027100                                                                          
027200                                                                          
027300     CLOSE W42811                                                         
027400     SKIP2                                                                
027500     MOVE 'S' TO POSTSUM-OPKOD                                            
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     .                                                                    
027800     EJECT                                                                
027900 S01-LAES-W42811  SECTION.                                                
028000     SKIP2                                                                
028100     READ W42811 INTO IN-AREA                                             
028200     AT END                                                               
028300        MOVE HIGH-VALUE TO IN-W42811                                      
028400        SET END-OF-W42811 TO TRUE                                         
028500                                                                          
028600     NOT AT END                                                           
028700        MOVE 'W42811'   TO POSTSUM-FDNAMN                                 
028802        MOVE 'W42813D1' TO POSTSUM-DDNAMN2                                
028900        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029000        CALL POSTSUM USING POSTSUM-PARM                                   
029100     END-READ                                                             
029200     .                                                                    
029300     EJECT                                                                
029400 S02-FLYTTA-HEADER-DATA  SECTION.                                         
029500                                                                          
029600     MOVE 001             TO HDR-REQU-IDMSGVER                            
029700     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
029802     MOVE 'W42813'        TO HDR-REQU-IDUSER                              
029900                                                                          
030010     MOVE 'DISC-72-PERIOD '   TO HDR-IDOUTTYPE                            
030100     MOVE SPACE               TO HDR-IDOUTREC                             
030200     MOVE IN-IDDC-RET         TO HDR-IDOUTREC(1:2)                        
030302     MOVE 'W42813'            TO HDR-IDOUTREC(3:8)                        
030400                                                                          
030500     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
030600     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
030700     .                                                                    
030800     EJECT                                                                
030900 S05-OPEN-DAP-SEND SECTION.                                               
031000*    MOVE 'S05-OPEN-DAP-S' TO CURR-SECTION                                
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
032300     EJECT                                                                
032400                                                                          
032500 S06-PUT-DAP-HEADER SECTION.                                              
032600*    MOVE 'S06-PUT-DAP-HE' TO CURR-SECTION                                
032700                                                                          
032800     MOVE 'PUT'                           TO SEND-KDFUNC                  
032900     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
033000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033100                         SEND-KVDLEN                                      
033200                         HDR-AREA                                         
033300     IF SEND-KDRC > ZERO                                                  
033400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
033500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033800     END-IF                                                               
033900     .                                                                    
034000 S07-PUT-DOC      SECTION.                                                
034100*    MOVE 'S07-PUT-DOC ' TO CURR-SECTION                                  
034200                                                                          
034300     MOVE 'PUT'                           TO SEND-KDFUNC                  
034400     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
034500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034600                         SEND-KVDLEN                                      
034700                         DOC-AREA                                         
034800     IF SEND-KDRC > ZERO                                                  
034900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035300     END-IF                                                               
035400     .                                                                    
035500     EJECT                                                                
035600 S08-CLOSE-DAP-SEND SECTION.                                              
035700*    MOVE 'S08-CLOSE-DAP-' TO CURR-SECTION                                
035800                                                                          
035900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
036000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036100                                                                          
036200     IF SEND-KDRC > 0                                                     
036300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
036400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
036500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000                                                                          
037100* --- IMS SEKTIONER ---                                                   
037200                                                                          
037300     EJECT                                                                
037400 IMS-GET-WDB601 SECTION.                                                  
037500                                                                          
037600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
037700          DELIMITED BY SIZE INTO SSA1                                     
037800     MOVE '  GE' TO GODK-STATUSKODER                                      
037900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
038000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
038100     PERFORM IMS-STATUSKONTROLL                                           
038200     .                                                                    
038300     EJECT                                                                
038400 IMS-STATUSKONTROLL SECTION.                                              
038500     SKIP2                                                                
038600     SET STATUS-IX TO 1                                                   
038700     SEARCH GODK-STATUS                                                   
038800       AT END                                                             
038900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039000           DELIMITED BY SIZE INTO FELTEXT                                 
039100         DISPLAY FELTEXT                                                  
039200         CALL FELLOG                                                      
039300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039400         CONTINUE                                                         
039500     END-SEARCH                                                           
039600     .                                                                    
