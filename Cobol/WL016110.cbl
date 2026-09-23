000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.             WL016110.                                        
000500 AUTHOR.                 TAPAS KUMAR GHSOH.                               
000600     DATE-WRITTEN.       2004/10/14.                                      
000700*                                                                         
000800     REMARKS.                                                             
000900*    FUNKTION:                                                            
001000*        UTSKRIFT AV DETALJINFORMATION                                    
001100*                                                                         
001200*        WL016110 PROGRAM IS A REPLICA OF W418UDET PROGRAM                
001300*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001400*                                                                         
001500*                                                                         
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 DATA DIVISION.                                                           
002000                                                                          
002100 WORKING-STORAGE SECTION.                                                 
002200     SKIP2                                                                
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  PROGRAM-NAMN            PIC X(8) VALUE 'WL016110'.                   
002600     SKIP2                                                                
002700*    ---- KONSTANTER                                                      
002800                                                                          
002900 77  JA                      PIC X       VALUE 'J'.                       
003000 77  NEJ                     PIC X       VALUE 'N'.                       
003100 77  TEXT-INDX               PIC S9(4)   VALUE ZERO COMP SYNC.            
003200 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
003300 77  KDRC-DISPLAY                PIC Z(5).                                
003400     EJECT                                                                
003500*    --- PARAMETERS TO ABEND                                              
003600                                                                          
003700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
003800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
003900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004000                                                                          
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
004300   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
004400   03  ABEND                   PIC X(8)  VALUE 'ABEND   '.                
004500   03  WZ01SEND                PIC X(8)  VALUE 'WZ01SEND'.                
004600                                                                          
004700     EJECT                                                                
004800 01  WS-RAPP-AREA.                                                        
004900     03  WS-RAPP-PRINTER         PIC X(8).                                
005000     03  WS-RAPP-LISTRAD.                                                 
005100         05  FILLER              PIC X(1)    VALUE SPACE.                 
005200         05  WS-RAPP-RAD         PIC X(120).                              
005300     03  WS-DUMMY                PIC X(1).                                
005400     EJECT                                                                
005500*    --- STATUS-KOD FRÅN IMS                                              
005600 01  STATUS-WS                   PIC XX.                                  
005700     88  STATUS-OK                           VALUE '  '.                  
005800*    --- IMS FUNKTIONSKODER                                               
005900*01  -COPY W0003                                                          
006000     EJECT                                                                
006100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
006200*                                                                         
006300 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
006400*   -COPY WZ01SEND                                                        
006500     EJECT                                                                
006600 01  FILLER                      PIC X(16)  VALUE 'HDR-AREA'.             
006700 01  HDR-AREA.                                                            
006800*    03 -COPY WZ01REQU -PRE HDR-                                          
006900*    03 -COPY WZ04HDR                                                     
007000                                                                          
007100 01  FILLER                 PIC X(16)  VALUE 'DAP-AREA-HEAD'.             
007200 01  DAP-AREA-HEAD.                                                       
007300*    03 -COPY WL01611                                                     
007400                                                                          
007500 01  FILLER                 PIC X(16)  VALUE 'DAP-AREA-LINE'.             
007600 01  DAP-AREA-LINE.                                                       
007700*    03 -COPY WL01612                                                     
007800     EJECT                                                                
007900*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
008000                                                                          
008100 01  GODK-STATUSKODER.                                                    
008200   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
008300     SKIP2                                                                
008400 01  SSA1                    PIC X(64).                                   
008500 01  SSA2                    PIC X(64).                                   
008600     SKIP2                                                                
008700 LINKAGE SECTION.                                                         
008800     SKIP2                                                                
008900*01  -COPY WL016110                                                       
009000     EJECT                                                                
009100 PROCEDURE DIVISION  USING  L16110-WL016110.                              
009200                                                                          
009300 STYR SECTION.                                                            
009400                                                                          
009500     PERFORM A-INIT                                                       
009600                                                                          
009700     PERFORM B-SKAPA-LISTA                                                
009800                                                                          
009900     PERFORM Z-FINIT                                                      
010000                                                                          
010100     GOBACK                                                               
010200     .                                                                    
010300     EJECT                                                                
010400                                                                          
010500 A-INIT        SECTION.                                                   
010600                                                                          
010700                                                                          
010800     PERFORM S02-SEND-OPEN                                                
010900                                                                          
011000     MOVE SPACE TO HDR-REQU-WZ01REQU                                      
011100     MOVE '001' TO HDR-REQU-IDMSGVER                                      
011200                                                                          
011300     MOVE 'DISCREPANCIES'            TO HDR-IDOUTTYPE                     
011400     MOVE SPACE                      TO HDR-IDOUTREC                      
011500     MOVE L16110-IDDC                TO HDR-IDOUTREC(1:2)                 
011600     MOVE L16110-IDUSER              TO HDR-IDOUTREC(3:8)                 
011700     MOVE FUNCTION CURRENT-DATE(5:10) TO HDR-IDLIST                       
011800*D&P HEADER                                                               
011900     PERFORM S02-PUT-HEADER                                               
012000                                                                          
012100     .                                                                    
012200     EJECT                                                                
012300 B-SKAPA-LISTA  SECTION.                                                  
012400                                                                          
012500*DOCUMENT HEADER                                                          
012600     PERFORM BA-REDIGERA-HUVUD                                            
012700     PERFORM S02-PUT-REPORT-HEAD                                          
012800                                                                          
012900*DOCUMENT DETAILS                                                         
013000     PERFORM BB-REDIGERA-RADER                                            
013100     PERFORM S02-PUT-REPORT-LINE                                          
013200                                                                          
013300     .                                                                    
013400     EJECT                                                                
013500                                                                          
013600 BA-REDIGERA-HUVUD  SECTION.                                              
013700                                                                          
013800                                                                          
013900     MOVE '1'                   TO IDAFPRCD IN HEAD-WL01611               
014000     MOVE L16110-IDDC           TO HEAD-IDDC                              
014100     MOVE L16110-IDDISTR        TO HEAD-IDDISTR                           
014200     MOVE L16110-IDKUNDNR       TO HEAD-IDKUNDNR                          
014300     MOVE L16110-IDRAPPNR       TO HEAD-IDRAPPNR                          
014400                                                                          
014500     MOVE L16110-IDARTNR        TO HEAD-IDARTNR                           
014600     MOVE L16110-IDRADNR        TO HEAD-IDRADNR                           
014700     MOVE L16110-IDUSER         TO HEAD-IDUSER                            
014800     .                                                                    
014900     EJECT                                                                
015000                                                                          
015100 BB-REDIGERA-RADER  SECTION.                                              
015200                                                                          
015300     MOVE '2'                   TO IDAFPRCD IN DET-WL01612                
015400     MOVE L16110-KDANMORS       TO DET-KDANMORS                           
015500     MOVE L16110-KVBEART-Q      TO DET-KVBEART-Q                          
015600     MOVE L16110-ADLAGOMR       TO DET-ADLAGOMR                           
015700     MOVE L16110-ADGANG         TO DET-ADGANG                             
015800     MOVE L16110-ADPLATS        TO DET-ADPLATS                            
015900                                                                          
016000     MOVE L16110-IDORDNR5       TO DET-IDORDNR7                           
016100     MOVE L16110-KVLEVART       TO DET-KVLEVART                           
016200     MOVE L16110-KVLS           TO DET-KVLS                               
016300                                                                          
016400     MOVE L16110-KVLEVANM-BEKR  TO DET-KVLEVANM-BEKR                      
016500     MOVE L16110-VKORDBTO-KOLLI TO DET-VKORDBTO-KOLLI                     
016600     MOVE L16110-KVPB-TOT       TO DET-KVPB-TOT                           
016700                                                                          
016800     MOVE L16110-PRARTBTO       TO DET-PRARTBTO                           
016900     MOVE L16110-VKORDNTO-KOLLI TO DET-VKORDNTO-KOLLI                     
017000     MOVE L16110-KDERS          TO DET-KDERS                              
017100                                                                          
017200     MOVE L16110-IDKOLLI        TO DET-IDKOLLI                            
017300     MOVE L16110-VKTARA         TO DET-VKTARA                             
017400     MOVE L16110-VKART          TO DET-VKART                              
017500                                                                          
017600     MOVE L16110-KDFAKTYP       TO DET-KDFAKTYP                           
017700     MOVE L16110-IDFAKT         TO DET-IDFAKT                             
017800     MOVE L16110-VKORDNTO-TOT   TO DET-VKORDNTO-TOT                       
017900     MOVE L16110-PRARTBTO-EXP   TO DET-PRARTBTO-EXP                       
018000                                                                          
018100     MOVE L16110-TIFAKT         TO DET-TIFAKT                             
018200     MOVE L16110-KDORDKL        TO DET-KDORDKL                            
018300     MOVE L16110-PRINK          TO DET-PRINK                              
018400                                                                          
018500     MOVE L16110-FLDIRLEV       TO DET-FLDIRLEV                           
018600     MOVE L16110-KDPRODSL       TO DET-KDPRODSL                           
018700                                                                          
018800     MOVE L16110-IDUSER-PACK    TO DET-IDUSER-PACK                        
018900     MOVE L16110-IDFKNGRP       TO DET-IDFKNGRP                           
019000                                                                          
019100     MOVE L16110-IDPRODNR       TO DET-IDPRODNR                           
019200     MOVE L16110-TIINVDAT       TO DET-TIINVDAT                           
019300                                                                          
019400     MOVE L16110-KVORDRAD       TO DET-KVORDRAD                           
019500     MOVE L16110-KVINVS         TO DET-KVINVS                             
019600                                                                          
019700     MOVE L16110-IDUSER-OREG    TO DET-IDUSER-OREG                        
019800     MOVE L16110-IDANSK         TO DET-IDANSK                             
019900                                                                          
020000     MOVE L16110-TIREGDAT       TO DET-TIREGDAT                           
020100                                                                          
020200     MOVE L16110-BEART          TO DET-BEART                              
020300                                                                          
020400                                                                          
020500     PERFORM BBA-FIXA-TEXT-INFO                                           
020600                                                                          
020700     .                                                                    
020800     EJECT                                                                
020900                                                                          
021000 BBA-FIXA-TEXT-INFO  SECTION.                                             
021100                                                                          
021200     PERFORM BBAA-TEANMNOT-REG                                            
021300     PERFORM BBAB-TEANMNOT-ADM                                            
021400     PERFORM BBAC-TEANMNOT-REM                                            
021500     PERFORM BBAD-TEANMNOT-RET                                            
021600                                                                          
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
022000 BBAA-TEANMNOT-REG SECTION.                                               
022100                                                                          
022200                                                                          
022300     MOVE +1                    TO TEXT-INDX                              
022400     PERFORM UNTIL TEXT-INDX    >  3                                      
022500        MOVE L16110-TEANMNOT-REG (TEXT-INDX)                              
022600                                TO DET-TEANMNOT-REG (TEXT-INDX)           
022700        ADD +1                  TO TEXT-INDX                              
022800     END-PERFORM                                                          
022900                                                                          
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300 BBAB-TEANMNOT-ADM SECTION.                                               
023400                                                                          
023500                                                                          
023600     MOVE +1                    TO TEXT-INDX                              
023700     PERFORM UNTIL TEXT-INDX    >  3                                      
023800        MOVE L16110-TEANMNOT-ADM (TEXT-INDX)                              
023900                                TO DET-TEANMNOT-ADM (TEXT-INDX)           
024000        ADD +1                  TO TEXT-INDX                              
024100     END-PERFORM                                                          
024200     .                                                                    
024300     EJECT                                                                
024400                                                                          
024500 BBAC-TEANMNOT-REM SECTION.                                               
024600                                                                          
024700                                                                          
024800     MOVE +1                    TO TEXT-INDX                              
024900     PERFORM UNTIL TEXT-INDX    >  3                                      
025000        MOVE L16110-TEANMNOT-REM (TEXT-INDX)                              
025100                                TO DET-TEANMNOT-REM (TEXT-INDX)           
025200        ADD +1                  TO TEXT-INDX                              
025300     END-PERFORM                                                          
025400                                                                          
025500     .                                                                    
025600     EJECT                                                                
025700                                                                          
025800 BBAD-TEANMNOT-RET SECTION.                                               
025900                                                                          
026000                                                                          
026100     MOVE +1                    TO TEXT-INDX                              
026200     PERFORM UNTIL TEXT-INDX    >  3                                      
026300        MOVE L16110-TEANMNOT-RET (TEXT-INDX)                              
026400                                TO DET-TEANMNOT-RET (TEXT-INDX)           
026500        ADD +1                  TO TEXT-INDX                              
026600     END-PERFORM                                                          
026700                                                                          
026800     .                                                                    
026900     EJECT                                                                
027000                                                                          
027100 Z-FINIT                   SECTION.                                       
027200                                                                          
027300     PERFORM S05-SEND-CLOSE                                               
027400                                                                          
027500     .                                                                    
027600     EJECT                                                                
027700* DISPATCHER-SEKTIONER                                                    
027800 S02-SEND-OPEN SECTION.                                                   
027900                                                                          
028000     MOVE 'CARPARTS.DAP.DISTRDOCWEB'      TO SEND-ADDISPABS               
028100     MOVE 'OPEN'                          TO SEND-KDFUNC                  
028200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
028300                         SEND-OPEN-AREA                                   
028400     IF SEND-KDRC > ZERO                                                  
028500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
028600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
028700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
028800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
028900     END-IF                                                               
029000     .                                                                    
029100     SKIP3                                                                
029200 S02-PUT-HEADER SECTION.                                                  
029300                                                                          
029400     MOVE 'PUT'                           TO SEND-KDFUNC                  
029500     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
029600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029700                         SEND-KVDLEN                                      
029800                         HDR-AREA                                         
029900     IF SEND-KDRC > ZERO                                                  
030000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
030100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
030200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 S02-PUT-REPORT-HEAD    SECTION.                                          
030800                                                                          
030900     MOVE 'PUT'                           TO SEND-KDFUNC                  
031000     MOVE LENGTH OF DAP-AREA-HEAD         TO SEND-KVDLEN                  
031100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031200                         SEND-KVDLEN                                      
031300                         DAP-AREA-HEAD                                    
031400     IF SEND-KDRC > ZERO                                                  
031500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
031700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031900     END-IF                                                               
032000     .                                                                    
032100     SKIP3                                                                
032200 S02-PUT-REPORT-LINE    SECTION.                                          
032300                                                                          
032400     MOVE 'PUT'                           TO SEND-KDFUNC                  
032500     MOVE LENGTH OF DAP-AREA-LINE         TO SEND-KVDLEN                  
032600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032700                         SEND-KVDLEN                                      
032800                         DAP-AREA-LINE                                    
032900     IF SEND-KDRC > ZERO                                                  
033000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
033100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033400     END-IF                                                               
033500     .                                                                    
033600     SKIP3                                                                
033700 S05-SEND-CLOSE SECTION.                                                  
033800                                                                          
033900     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
034000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034100     .                                                                    
034200     EJECT                                                                
