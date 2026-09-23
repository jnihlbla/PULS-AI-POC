000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF201100.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/04/24.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        CREATES DOCUMENT DATA FROM MISCELLANEOUS DB2-TABLES              
001000*                                                                         
001100*        PGM READS                                                        
001200*        - DB2-TABLE T01PROC                                              
001300*        - DB2-TABLE T01DHEA                                              
001400*        - DB2-TABLE T01DLIN                                              
001500*        - DB2-TABLE T01DAPP                                              
001600*        - DB2-TABLE T01FCUS                                              
001700*        - DB2-TABLE T01BURE                                              
001800*        - DB2-TABLE T01COCO                                              
001900*        - DB2-TABLE T01CURR                                              
002000*        - DB2-TABLE T01RECO                                              
002100*        - DB2-TABLE T01SECO                                              
002200*        - DB2-TABLE T01PAIN                                              
002300*        - DB2-TABLE T01INRE                                              
002400*        - DB2-TABLE T01LSEL                                              
002500*                                                                         
002600*                                                                         
002700*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
002800*                                                                         
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500                                                                          
003600*          --- DOCUMENT DATA OUTPUT FILE                                  
003700     SELECT WF2011                     ASSIGN TO WF2011D1.                
003800                                                                          
003900     SELECT WF2031                     ASSIGN TO WF2011D2.                
004000                                                                          
004100 DATA DIVISION.                                                           
004200                                                                          
004300 FILE SECTION.                                                            
004400 FD  WF2011                                                               
004500     LABEL RECORD STANDARD                                                
004600     RECORDING  V                                                         
004700     BLOCK CONTAINS 0.                                                    
004800                                                                          
004900 01  REC-WF2011                  PIC X(5695).                             
005000                                                                          
005100 FD  WF2031                                                               
005200     LABEL RECORD STANDARD                                                
005300     RECORDING  V                                                         
005400     BLOCK CONTAINS 0.                                                    
005500                                                                          
005600 01  REC-WF2031                  PIC X(5695).                             
005700                                                                          
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
006100 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006200 77  WS-DAGENS-DATUM             PIC X(8)    VALUE '00000000'.            
006300 77  WS-DAGENS-DATUM-NUM         PIC 9(8).                                
006400                                                                          
006500* CONSTANTS.                                                              
006600 77  IDPGM                       PIC X(8)    VALUE 'WF201100'.            
006700 77  WS-CURRENT-VERSION          PIC S9(3)   VALUE +001 COMP-3.           
006800 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
006900 77  WS-IDSYSTEM                 PIC X(4)    VALUE 'WF02'.                
007000 77  WS-IDLEGSEL-CRS             PIC X(4)           VALUE SPACE.          
007100                                                                          
007200*  WORKING-FIELDS.                                                        
007300 01  WS-MISCELLANEOUS.                                                    
007400     03  WS-DAFINDOC             PIC X(8)           VALUE SPACE.          
007500     03  WS-DAREFDAT             PIC X(8)           VALUE SPACE.          
007600     03  WS-DAFAKREF             PIC X(8)           VALUE SPACE.          
007700     03  WS-KDVALISO             PIC X(3)           VALUE SPACE.          
007800     03  WS-KDVALISO-HEAD        PIC X(3)           VALUE SPACE.          
007900     03  WS-KDVALISO-SND         PIC X(3)           VALUE SPACE.          
008000     03  WS-KDFINDOC             PIC X(4)           VALUE SPACE.          
008100     03  WS-IDLEGSEL             PIC X(4)           VALUE SPACE.          
008200     03  WS-KDPARTTY             PIC X(3)           VALUE SPACE.          
008300     03  WS-KDPARTGR             PIC X(15)          VALUE SPACE.          
008400     03  WS-BETEXT-1             PIC X(50)          VALUE SPACE.          
008500     03  WS-BETEXT-2             PIC X(50)          VALUE SPACE.          
008600     03  WS-BETEXT-3             PIC X(50)          VALUE SPACE.          
008700     03  WS-BETEXT-4             PIC X(50)          VALUE SPACE.          
008800     03  WS-BETEXT-5             PIC X(100)         VALUE SPACE.          
008900     03  WS-BETEXT-6             PIC X(100)         VALUE SPACE.          
009000     03  WS-BETEXT-7             PIC X(100)         VALUE SPACE.          
009100     03  WS-BETEXT-8             PIC X(100)         VALUE SPACE.          
009200     03  WS-BETEXT-9             PIC X(100)         VALUE SPACE.          
009300     03  WS-BETEXT-10            PIC X(100)         VALUE SPACE.          
009400     03  WS-BETEXT-11            PIC X(100)         VALUE SPACE.          
009500     03  WS-BETEXT-12            PIC X(100)         VALUE SPACE.          
009600     03  WS-BETEXT-13            PIC X(100)         VALUE SPACE.          
009700     03  WS-BETEXT-14            PIC X(100)         VALUE SPACE.          
009800     03  WS-BETEXT-15            PIC X(100)         VALUE SPACE.          
009900     03  WS-BETEXT-16            PIC X(100)         VALUE SPACE.          
010000     03  WS-BETEXT-17            PIC X(100)         VALUE SPACE.          
010100     03  WS-BETEXT-18            PIC X(100)         VALUE SPACE.          
010200     03  WS-BETEXT-19            PIC X(100)         VALUE SPACE.          
010300     03  WS-BETEXT-20            PIC X(100)         VALUE SPACE.          
010400     03  WS-BETEXT-21            PIC X(100)         VALUE SPACE.          
010500     03  WS-BETEXT-22            PIC X(100)         VALUE SPACE.          
010600     03  WS-BETEXT-23            PIC X(100)         VALUE SPACE.          
010700     03  WS-BETEXT-24            PIC X(100)         VALUE SPACE.          
010800     03  WS-BETEXT-25            PIC X(100)         VALUE SPACE.          
010900     03  WS-BETEXT-26            PIC X(100)         VALUE SPACE.          
011000     03  WS-BETEXT-27            PIC X(100)         VALUE SPACE.          
011100     03  WS-BETEXT-28            PIC X(100)         VALUE SPACE.          
011200     03  WS-BETEXT-29            PIC X(100)         VALUE SPACE.          
011300     03  WS-BETEXT-30            PIC X(100)         VALUE SPACE.          
011400     03  WS-BETEXT-31            PIC X(100)         VALUE SPACE.          
011500     03  WS-BETEXT-32            PIC X(100)         VALUE SPACE.          
011600     03  WS-BETEXT-33            PIC X(100)         VALUE SPACE.          
011700     03  WS-BETEXT-34            PIC X(100)         VALUE SPACE.          
011800     03  WS-BETEXT-35            PIC X(100)         VALUE SPACE.          
011900     03  WS-BETEXT-36            PIC X(100)         VALUE SPACE.          
012000     03  WS-BETEXT-37            PIC X(100)         VALUE SPACE.          
012100     03  WS-BETEXT-38            PIC X(100)         VALUE SPACE.          
012200     03  WS-BETEXT-39            PIC X(100)         VALUE SPACE.          
012300     03  WS-BETEXT-40            PIC X(100)         VALUE SPACE.          
012400     03  WS-BETEXT-41            PIC X(100)         VALUE SPACE.          
012500     03  WS-BETEXT-42            PIC X(100)         VALUE SPACE.          
012600     03  WS-BETEXT-43            PIC X(100)         VALUE SPACE.          
012700     03  WS-BETEXT-44            PIC X(100)         VALUE SPACE.          
012800     03  WS-IDEXCUST-1           PIC X(15)          VALUE SPACE.          
012900     03  WS-IDEXCUST-2           PIC X(15)          VALUE SPACE.          
013000     03  WS-IDEXCUST-3           PIC X(15)          VALUE SPACE.          
013100     03  WS-IDOPTION-1           PIC X(15)          VALUE SPACE.          
013200     03  WS-IDOPTION-2           PIC X(15)          VALUE SPACE.          
013300     03  WS-IDOPTION-3           PIC X(15)          VALUE SPACE.          
013400     03  WS-IDOPTION-4           PIC X(15)          VALUE SPACE.          
013500     03  WS-IDOPTION-5           PIC X(15)          VALUE SPACE.          
013600     03  WS-IDACCNT-1            PIC X(15)          VALUE SPACE.          
013700     03  WS-IDACCNT-2            PIC X(15)          VALUE SPACE.          
013800     03  WS-IDACCNT-3            PIC X(15)          VALUE SPACE.          
013900     03  WS-IDACCNT-4            PIC X(15)          VALUE SPACE.          
014000     03  WS-DASTADAT-KEY         PIC X(8)           VALUE SPACE.          
014100     03  WS-DASTADAT-SND         PIC X(8)           VALUE SPACE.          
014200     03  WS-IDLOPNR              PIC S9(5)      COMP-3 VALUE ZERO.        
014300     03  WS-REVALUTA-LOCCUR      PIC S9(5)      COMP-3 VALUE ZERO.        
014400     03  WS-PRKURS-LOCCUR        PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
014500     03  WS-REVALUTA-SND         PIC S9(5)      COMP-3 VALUE ZERO.        
014600     03  WS-PRKURS-SND           PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
014700     03  WS-KDTRADP              PIC X(4)           VALUE SPACE.          
014800     03  WS-DASTADAT-CREDIT      PIC X(8).                                
014900     03  WS2-DASTADAT-CREDIT     PIC X(8).                                
015000     03  WS2-DASTADAT-CREDIT-SND PIC X(8).                                
015100     03  WS-FLPRIV               PIC X.                                   
015200     03  WS-RECO-IDVAT           PIC X(17)      VALUE SPACE.              
015300     03  WS-FLDIRVAT             PIC X(1)       VALUE SPACE.              
015400                                                                          
015500*                                                                         
015600*01  -COPY WWLANDX2                                                       
015700*                                                                         
015800 01  GENERAL-SUBPROGRAMS.                                                 
015900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016000                                                                          
016100*    --- PARAMETERS TO ABEND                                              
016200 01  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016300 01  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016400 01  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016500 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
016600                                                                          
016700*    --- WORK-AREAS FOR OUTPUT-FILE                                       
016800 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
016900                                 'OUTPUT-AREA     '.                      
017000                                                                          
017100*01  -COPY WF201101                                                       
017200                                                                          
017300*01  -COPY WF201102                                                       
017400                                                                          
017500*01  -COPY WF201103                                                       
017600                                                                          
017700*01  -COPY WF201104                                                       
017800                                                                          
017900*    --- WORK-AREAS FOR DB2-SECTIONS                                      
018000 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
018100*01  -COPY T01PROC    -PRE PROC-                                          
018200                                                                          
018300 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
018400*01  -COPY T01DHEA    -PRE DHEA-                                          
018500                                                                          
018600 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
018700*01  -COPY T01DLIN    -PRE DLIN-                                          
018800                                                                          
018900 01  FILLER                       PIC X(16)  VALUE 'DAPP-TAB   '.         
019000*01  -COPY T01DAPP    -PRE DAPP-                                          
019100                                                                          
019200 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
019300*01  -COPY T01FCUS    -PRE CUST-                                          
019400                                                                          
019500 01  FILLER                       PIC X(16)  VALUE 'BURE-TAB   '.         
019600*01  -COPY T01BURE    -PRE BURE-                                          
019700                                                                          
019800 01  FILLER                       PIC X(16)  VALUE 'COCO-TAB   '.         
019900*01  -COPY T01COCO    -PRE COCO-                                          
020000                                                                          
020100 01  FILLER                       PIC X(16)  VALUE 'CURR-TAB   '.         
020200*01  -COPY T01CURR    -PRE CURR-                                          
020300                                                                          
020400 01  FILLER                       PIC X(16)  VALUE 'RECO-TAB   '.         
020500*01  -COPY T01RECO    -PRE RECO-                                          
020600                                                                          
020700 01  FILLER                       PIC X(16)  VALUE 'SECO-TAB   '.         
020800*01  -COPY T01SECO    -PRE SECO-                                          
020900                                                                          
021000 01  FILLER                       PIC X(16)  VALUE 'PAIN-TAB   '.         
021100*01  -COPY T01PAIN    -PRE PAIN-                                          
021200                                                                          
021300 01  FILLER                       PIC X(16)  VALUE 'INRE-TAB   '.         
021400*01  -COPY T01INRE    -PRE INRE-                                          
021500                                                                          
021600 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
021700*01  -COPY T01LSEL    -PRE LSEL-                                          
021800                                                                          
021900 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
022000       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
022100                                                                          
022200 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
022300       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
022400                                                                          
022500 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
022600       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
022700                                                                          
022800 01  FILLER                       PIC X(16)  VALUE 'DAPP-AREA'.           
022900       EXEC SQL INCLUDE T01DAPP  END-EXEC.                                
023000                                                                          
023100 01  FILLER                       PIC X(16)  VALUE 'CUST-AREA'.           
023200       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
023300                                                                          
023400 01  FILLER                       PIC X(16)  VALUE 'BURE-AREA'.           
023500       EXEC SQL INCLUDE T01BURE  END-EXEC.                                
023600                                                                          
023700 01  FILLER                       PIC X(16)  VALUE 'COCO-AREA'.           
023800       EXEC SQL INCLUDE T01COCO  END-EXEC.                                
023900                                                                          
024000 01  FILLER                       PIC X(16)  VALUE 'CURR-AREA'.           
024100       EXEC SQL INCLUDE T01CURR  END-EXEC.                                
024200                                                                          
024300 01  FILLER                       PIC X(16)  VALUE 'RECO-AREA'.           
024400       EXEC SQL INCLUDE T01RECO  END-EXEC.                                
024500                                                                          
024600 01  FILLER                       PIC X(16)  VALUE 'SECO-AREA'.           
024700       EXEC SQL INCLUDE T01SECO  END-EXEC.                                
024800                                                                          
024900 01  FILLER                       PIC X(16)  VALUE 'PAIN-AREA'.           
025000       EXEC SQL INCLUDE T01PAIN  END-EXEC.                                
025100                                                                          
025200 01  FILLER                       PIC X(16)  VALUE 'INRE-AREA'.           
025300       EXEC SQL INCLUDE T01INRE  END-EXEC.                                
025400                                                                          
025500 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
025600       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
025700                                                                          
025800 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
025900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
026000*                        **** STATUS-CODE FROM DB2                        
026100                                                                          
026200 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
026300 01  DB2-WS.                                                              
026400   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
026500     88  LINES-FOUND                         VALUE +000.                  
026600     88  LINES-MISSING                       VALUE +100.                  
026700     88  TABLE-MISSING                       VALUE 305.                   
026800     88  RESOURCE-WRONG                      VALUE 904.                   
026900   03  GOOD-SQLCODES.                                                     
027000     05  GOOD-SQLCODE OCCURS 5                                            
027100         INDEXED BY SQLCODE-IX    PIC 999.                                
027200                                                                          
027300 PROCEDURE DIVISION.                                                      
027400 MAIN SECTION.                                                            
027500     PERFORM A-INIT                                                       
027600     PERFORM DB2-OPEN-CRS-LSEL                                            
027700     PERFORM DB2-FETCH-CRS-LSEL                                           
027800     PERFORM UNTIL LINES-MISSING                                          
027900       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                
028000       MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM            
028100       PERFORM DB2-SELECT-T01PROC                                         
028200       IF PROC-KDBEH = 'P'                                                
028300         SUBTRACT 1 FROM WS-DAGENS-DATUM-NUM                              
028400         MOVE WS-DAGENS-DATUM-NUM       TO WS-DAGENS-DATUM                
028500       END-IF                                                             
028600                                                                          
028700       PERFORM DB2-DCL-OPN-CRS1                                           
028800       PERFORM DB2-FETCH-CRS1                                             
028900       PERFORM UNTIL LINES-MISSING                                        
029000         PERFORM DB2-SELECT-T01DLIN-KDFRAKT                               
029100         PERFORM DB2-SELECT-T01COCO-LSEL                                  
029200         PERFORM DB2-SELECT-T01COCO-RESP                                  
029300         PERFORM DB2-SELECT-T01COCO-BET                                   
029400         PERFORM S11-WRITE-WF20X1-HEAD                                    
029500         PERFORM DB2-SELECT-T01PAIN                                       
029600         PERFORM S13-WRITE-WF20X1-FOOT                                    
029700         PERFORM DB2-FETCH-CRS1                                           
029800       END-PERFORM                                                        
029900       PERFORM DB2-CLOSE-CRS1                                             
030000                                                                          
030100       PERFORM DB2-DCL-OPN-CRS2                                           
030200       PERFORM DB2-FETCH-CRS2                                             
030300       PERFORM UNTIL LINES-MISSING                                        
030400         PERFORM S12-WRITE-WF20X1-LINE                                    
030500         PERFORM DB2-FETCH-CRS2                                           
030600       END-PERFORM                                                        
030700       PERFORM DB2-CLOSE-CRS2                                             
030800                                                                          
030900       PERFORM DB2-DCL-OPN-CRS3                                           
031000       PERFORM DB2-FETCH-CRS3                                             
031100       PERFORM UNTIL LINES-MISSING                                        
031200         PERFORM S14-WRITE-WF20X1-APPX                                    
031300         PERFORM DB2-FETCH-CRS3                                           
031400       END-PERFORM                                                        
031500       PERFORM DB2-CLOSE-CRS3                                             
031600                                                                          
031700       PERFORM DB2-FETCH-CRS-LSEL                                         
031800     END-PERFORM                                                          
031900     PERFORM DB2-CLOSE-CRS-LSEL                                           
032000                                                                          
032100     PERFORM Z-FINIT                                                      
032200     MOVE ZERO TO RETURN-CODE                                             
032300     GOBACK                                                               
032400     .                                                                    
032500                                                                          
032600 A-INIT SECTION.                                                          
032700     OPEN OUTPUT WF2011                                                   
032800                 WF2031                                                   
032900                                                                          
033000     INITIALIZE HEAD-WF201101                                             
033100     INITIALIZE LINE-WF201102                                             
033200     INITIALIZE FOOT-WF201103                                             
033300     INITIALIZE APPX-WF201104                                             
033400                                                                          
033500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
033600     MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM              
033700     .                                                                    
033800                                                                          
033900 Z-FINIT SECTION.                                                         
034000     CLOSE WF2011                                                         
034100     CLOSE WF2031                                                         
034200     .                                                                    
034300                                                                          
034400 S11-WRITE-WF20X1-HEAD SECTION.                                           
034500     MOVE WS-DAFINDOC   TO HEAD-DAFINDOC                                  
034600     IF CUST-FLRATE = 'N'                                                 
034700       IF HEAD-KDFINDOC = 'CR'                                            
034800         IF HEAD-IDLEVNR(1:4) = '0000'                                    
034900         OR HEAD-IDLEVNR(1:4) = '    '                                    
035000           PERFORM DB2-SELECT-MAX-T01CURR                                 
035100           PERFORM DB2-SELECT-T01CURR                                     
035200         ELSE                                                             
035300           MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                           
035400           MOVE HEAD-IDLEVNR(1:4) TO WS-DASTADAT-CREDIT(3:4)              
035500           MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                           
035600           PERFORM DB2-SELECT-MAX-CURR-CRE                                
035700           PERFORM DB2-SELECT-CURR-CRE                                    
035800           MOVE HEAD-PRKURS-LOC TO HEAD-PRKURS                            
035900         END-IF                                                           
036000       ELSE                                                               
036100         PERFORM DB2-SELECT-MAX-T01CURR                                   
036200         PERFORM DB2-SELECT-T01CURR                                       
036300       END-IF                                                             
036400     ELSE                                                                 
036500       MOVE HEAD-PRKURS TO HEAD-PRKURS-LOC                                
036600       MOVE 1           TO HEAD-REVALUTA                                  
036700     END-IF                                                               
036800     MOVE HEAD-REVALUTA     TO WS-REVALUTA-LOCCUR                         
036900     MOVE HEAD-PRKURS-LOC   TO WS-PRKURS-LOCCUR                           
037000     COMPUTE HEAD-SUNTO-TOT-LOC ROUNDED =                                 
037100                              (HEAD-SUNTO-TOT * HEAD-REVALUTA) /          
037200                              HEAD-PRKURS-LOC                             
037300     END-COMPUTE                                                          
037400     COMPUTE HEAD-SUVAT-BILLIT-TOT-LOC ROUNDED =                          
037500                       (HEAD-SUVAT-BILLIT-TOT * HEAD-REVALUTA) /          
037600                       HEAD-PRKURS-LOC                                    
037700     END-COMPUTE                                                          
037800     COMPUTE HEAD-SUBTO-TOT-LOC =                                         
037900                                HEAD-SUNTO-TOT-LOC +                      
038000                                HEAD-SUVAT-BILLIT-TOT-LOC                 
038100     END-COMPUTE                                                          
038200                                                                          
038300     IF HEAD-SUDOCLIM > ZERO                                              
038400       COMPUTE HEAD-SUDOCLIM ROUNDED =                                    
038500                             (HEAD-SUDOCLIM * HEAD-REVALUTA) /            
038600                             HEAD-PRKURS                                  
038700       END-COMPUTE                                                        
038800     END-IF                                                               
038900                                                                          
039000     IF HEAD-IDLEGSEL = WS-IDLEGSEL                                       
039100       CONTINUE                                                           
039200     ELSE                                                                 
039300       MOVE HEAD-IDLEGSEL TO WS-IDLEGSEL                                  
039400     END-IF                                                               
039500     MOVE HEAD-KDVALISO TO WS-KDVALISO-HEAD                               
039600     MOVE WS-KDTRADP    TO HEAD-KDTRADP                                   
039700     IF CUST-FLLOCCUR = 'J' AND WS-KDVALISO-HEAD = 'SEK'                  
039800       IF HEAD-KDVALISO-LOC = WS-KDVALISO-HEAD                            
039900         CONTINUE                                                         
040000       ELSE                                                               
040100         COMPUTE HEAD-SUNTO-TOT =                                         
040200                 HEAD-SUNTO-TOT-LOC * 1                                   
040300         COMPUTE HEAD-SUVAT-BILLIT-TOT =                                  
040400                 HEAD-SUVAT-BILLIT-TOT-LOC * 1                            
040500         COMPUTE HEAD-SUBTO-TOT =                                         
040600                 HEAD-SUBTO-TOT-LOC * 1                                   
040700         COMPUTE HEAD-SUNTO-SERV ROUNDED =                                
040800                         (HEAD-SUNTO-SERV * WS-REVALUTA-LOCCUR) /         
040900                          WS-PRKURS-LOCCUR                                
041000         COMPUTE HEAD-SUBTO-SERV ROUNDED =                                
041100                         (HEAD-SUBTO-SERV * WS-REVALUTA-LOCCUR) /         
041200                          WS-PRKURS-LOCCUR                                
041300         COMPUTE HEAD-SUNTO-PART ROUNDED =                                
041400                         (HEAD-SUNTO-PART * WS-REVALUTA-LOCCUR) /         
041500                          WS-PRKURS-LOCCUR                                
041600         COMPUTE HEAD-SUBTO-PART ROUNDED =                                
041700                         (HEAD-SUBTO-PART * WS-REVALUTA-LOCCUR) /         
041800                          WS-PRKURS-LOCCUR                                
041900         MOVE HEAD-KDVALISO-LOC TO HEAD-KDVALISO                          
042000         MOVE HEAD-KDVALISO-LOC TO WS-KDVALISO                            
042100       END-IF                                                             
042200     END-IF                                                               
042300     PERFORM DB2-SELECT-T01LSEL                                           
042400     IF LINES-FOUND                                                       
042500       PERFORM DB2-SELECT-T01INRE                                         
042600       IF LINES-FOUND                                                     
042700*        IF  HEAD-IDBREAK-1 = 'SERVEXT'                                   
042800*        AND HEAD-IDPARTNR = '25728'                                      
042900*          MOVE 'EXEMPT'        TO HEAD-BETEXT-5                          
043000*        ELSE                                                             
043100           MOVE WS-BETEXT-5     TO HEAD-BETEXT-5                          
043200*        END-IF                                                           
043300         MOVE WS-BETEXT-6       TO HEAD-BETEXT-6                          
043400         MOVE WS-BETEXT-7       TO HEAD-BETEXT-7                          
043500         MOVE WS-BETEXT-8       TO HEAD-BETEXT-8                          
043600         MOVE WS-BETEXT-9       TO HEAD-BETEXT-9                          
043700         MOVE WS-BETEXT-10      TO HEAD-BETEXT-10                         
043800         MOVE WS-BETEXT-11      TO HEAD-BETEXT-11                         
043900         MOVE WS-BETEXT-12      TO HEAD-BETEXT-12                         
044000         MOVE WS-BETEXT-13      TO HEAD-BETEXT-13                         
044100         MOVE WS-BETEXT-14      TO HEAD-BETEXT-14                         
044200         MOVE WS-BETEXT-15      TO HEAD-BETEXT-15                         
044300         MOVE WS-BETEXT-16      TO HEAD-BETEXT-16                         
044400         MOVE WS-BETEXT-17      TO HEAD-BETEXT-17                         
044500         MOVE WS-BETEXT-18      TO HEAD-BETEXT-18                         
044600         MOVE WS-BETEXT-19      TO HEAD-BETEXT-19                         
044700         MOVE WS-BETEXT-20      TO HEAD-BETEXT-20                         
044800         MOVE WS-BETEXT-21      TO HEAD-BETEXT-21                         
044900         MOVE WS-BETEXT-22      TO HEAD-BETEXT-22                         
045000         MOVE WS-BETEXT-23      TO HEAD-BETEXT-23                         
045100         MOVE WS-BETEXT-24      TO HEAD-BETEXT-24                         
045200         MOVE WS-BETEXT-25      TO HEAD-BETEXT-25                         
045300         MOVE WS-BETEXT-26      TO HEAD-BETEXT-26                         
045400         MOVE WS-BETEXT-27      TO HEAD-BETEXT-27                         
045500         MOVE WS-BETEXT-28      TO HEAD-BETEXT-28                         
045600         MOVE WS-BETEXT-29      TO HEAD-BETEXT-29                         
045700         MOVE WS-BETEXT-30      TO HEAD-BETEXT-30                         
045800         MOVE WS-BETEXT-31      TO HEAD-BETEXT-31                         
045900         MOVE WS-BETEXT-32      TO HEAD-BETEXT-32                         
046000         MOVE WS-BETEXT-33      TO HEAD-BETEXT-33                         
046100         MOVE WS-BETEXT-34      TO HEAD-BETEXT-34                         
046200         MOVE WS-BETEXT-35      TO HEAD-BETEXT-35                         
046300         MOVE WS-BETEXT-36      TO HEAD-BETEXT-36                         
046400         MOVE WS-BETEXT-37      TO HEAD-BETEXT-37                         
046500         MOVE WS-BETEXT-38      TO HEAD-BETEXT-38                         
046600         MOVE WS-BETEXT-39      TO HEAD-BETEXT-39                         
046700         MOVE WS-BETEXT-40      TO HEAD-BETEXT-40                         
046800         MOVE WS-BETEXT-41      TO HEAD-BETEXT-41                         
046900         MOVE WS-BETEXT-42      TO HEAD-BETEXT-42                         
047000         MOVE WS-BETEXT-43      TO HEAD-BETEXT-43                         
047100         MOVE WS-BETEXT-44      TO HEAD-BETEXT-44                         
047200       ELSE                                                               
047300         MOVE SPACE             TO HEAD-BETEXT-5                          
047400         MOVE SPACE             TO HEAD-BETEXT-6                          
047500         MOVE SPACE             TO HEAD-BETEXT-7                          
047600         MOVE SPACE             TO HEAD-BETEXT-8                          
047700         MOVE SPACE             TO HEAD-BETEXT-9                          
047800         MOVE SPACE             TO HEAD-BETEXT-10                         
047900         MOVE SPACE             TO HEAD-BETEXT-11                         
048000         MOVE SPACE             TO HEAD-BETEXT-12                         
048100         MOVE SPACE             TO HEAD-BETEXT-13                         
048200         MOVE SPACE             TO HEAD-BETEXT-14                         
048300         MOVE SPACE             TO HEAD-BETEXT-15                         
048400         MOVE SPACE             TO HEAD-BETEXT-16                         
048500         MOVE SPACE             TO HEAD-BETEXT-17                         
048600         MOVE SPACE             TO HEAD-BETEXT-18                         
048700         MOVE SPACE             TO HEAD-BETEXT-19                         
048800         MOVE SPACE             TO HEAD-BETEXT-20                         
048900         MOVE SPACE             TO HEAD-BETEXT-21                         
049000         MOVE SPACE             TO HEAD-BETEXT-22                         
049100         MOVE SPACE             TO HEAD-BETEXT-23                         
049200         MOVE SPACE             TO HEAD-BETEXT-24                         
049300         MOVE SPACE             TO HEAD-BETEXT-25                         
049400         MOVE SPACE             TO HEAD-BETEXT-26                         
049500         MOVE SPACE             TO HEAD-BETEXT-27                         
049600         MOVE SPACE             TO HEAD-BETEXT-28                         
049700         MOVE SPACE             TO HEAD-BETEXT-29                         
049800         MOVE SPACE             TO HEAD-BETEXT-30                         
049900         MOVE SPACE             TO HEAD-BETEXT-31                         
050000         MOVE SPACE             TO HEAD-BETEXT-32                         
050100         MOVE SPACE             TO HEAD-BETEXT-33                         
050200         MOVE SPACE             TO HEAD-BETEXT-34                         
050300         MOVE SPACE             TO HEAD-BETEXT-35                         
050400         MOVE SPACE             TO HEAD-BETEXT-36                         
050500         MOVE SPACE             TO HEAD-BETEXT-37                         
050600         MOVE SPACE             TO HEAD-BETEXT-38                         
050700         MOVE SPACE             TO HEAD-BETEXT-39                         
050800         MOVE SPACE             TO HEAD-BETEXT-40                         
050900         MOVE SPACE             TO HEAD-BETEXT-41                         
051000         MOVE SPACE             TO HEAD-BETEXT-42                         
051100         MOVE SPACE             TO HEAD-BETEXT-43                         
051200         MOVE SPACE             TO HEAD-BETEXT-44                         
051300       END-IF                                                             
051400     END-IF                                                               
051500                                                                          
051600**** SHOULD ONLY BE FOR DDGS                                              
051700     MOVE WS-FLDIRVAT          TO HEAD-FLDIRVAT                           
051800     MOVE HEAD-IDLANDX3-RESP   TO LANDX2-IDLANDX2                         
051900     IF LANDX2-EU-IDLANDX2                                                
052000       MOVE HEAD-IDLANDX3-SEND TO LANDX2-IDLANDX2                         
052100       IF LANDX2-EU-IDLANDX2                                              
052200         IF WS-FLDIRVAT = 'J'                                             
052300           IF  HEAD-IDLEVNR > ' '                                         
052400           AND HEAD-KDFINDOC = 'INV'                                      
052500             MOVE HEAD-IDBREAK-2(6:2) TO LANDX2-IDLANDX2                  
052600             IF LANDX2-EU-IDLANDX2                                        
052700**** DDGS IF VCC VAT REGISTRATED IN COUNTRY ITALY A EXEPTION              
052800               IF HEAD-IDPARTNR = '17754'                                 
052900               OR HEAD-IDPARTNR = '7620'                                  
053000                 CONTINUE                                                 
053100               ELSE                                                       
053200                 MOVE WS-RECO-IDVAT TO HEAD-IDVAT-RESP                    
053300                 IF HEAD-IDPARTNR = '119612'                              
053400                   MOVE 'SE556074308901' TO HEAD-IDVAT-RESP               
053500                 END-IF                                                   
053600               END-IF                                                     
053700               PERFORM DB2-SELECT-T01INRE-DDGS                            
053800               IF LINES-FOUND                                             
053900*                IF HEAD-IDBREAK-1 = 'SERVEXT'                            
054000*                AND HEAD-IDPARTNR = '25728'                              
054100*                  MOVE 'EXEMPT' TO HEAD-BETEXT-5                         
054200*                ELSE                                                     
054300                   MOVE WS-BETEXT-5 TO HEAD-BETEXT-5                      
054400*                END-IF                                                   
054500                 MOVE WS-BETEXT-6 TO HEAD-BETEXT-6                        
054600                 MOVE WS-BETEXT-7 TO HEAD-BETEXT-7                        
054700                 MOVE WS-BETEXT-8 TO HEAD-BETEXT-8                        
054800                 MOVE WS-BETEXT-9 TO HEAD-BETEXT-9                        
054900                 MOVE WS-BETEXT-10 TO HEAD-BETEXT-10                      
055000                 MOVE WS-BETEXT-11 TO HEAD-BETEXT-11                      
055100                 MOVE WS-BETEXT-12 TO HEAD-BETEXT-12                      
055200                 MOVE WS-BETEXT-13 TO HEAD-BETEXT-13                      
055300                 MOVE WS-BETEXT-14 TO HEAD-BETEXT-14                      
055400                 MOVE WS-BETEXT-15 TO HEAD-BETEXT-15                      
055500                 MOVE WS-BETEXT-16 TO HEAD-BETEXT-16                      
055600                 MOVE WS-BETEXT-17 TO HEAD-BETEXT-17                      
055700                 MOVE WS-BETEXT-18 TO HEAD-BETEXT-18                      
055800                 MOVE WS-BETEXT-19 TO HEAD-BETEXT-19                      
055900                 MOVE WS-BETEXT-20 TO HEAD-BETEXT-20                      
056000                 MOVE WS-BETEXT-21 TO HEAD-BETEXT-21                      
056100                 MOVE WS-BETEXT-22 TO HEAD-BETEXT-22                      
056200                 MOVE WS-BETEXT-23 TO HEAD-BETEXT-23                      
056300                 MOVE WS-BETEXT-24 TO HEAD-BETEXT-24                      
056400                 MOVE WS-BETEXT-25 TO HEAD-BETEXT-25                      
056500                 MOVE WS-BETEXT-26 TO HEAD-BETEXT-26                      
056600                 MOVE WS-BETEXT-27 TO HEAD-BETEXT-27                      
056700                 MOVE WS-BETEXT-28 TO HEAD-BETEXT-28                      
056800                 MOVE WS-BETEXT-29 TO HEAD-BETEXT-29                      
056900                 MOVE WS-BETEXT-30 TO HEAD-BETEXT-30                      
057000                 MOVE WS-BETEXT-31 TO HEAD-BETEXT-31                      
057100                 MOVE WS-BETEXT-32 TO HEAD-BETEXT-32                      
057200                 MOVE WS-BETEXT-33 TO HEAD-BETEXT-33                      
057300                 MOVE WS-BETEXT-34 TO HEAD-BETEXT-34                      
057400                 MOVE WS-BETEXT-35 TO HEAD-BETEXT-35                      
057500                 MOVE WS-BETEXT-36 TO HEAD-BETEXT-36                      
057600                 MOVE WS-BETEXT-37 TO HEAD-BETEXT-37                      
057700                 MOVE WS-BETEXT-38 TO HEAD-BETEXT-38                      
057800                 MOVE WS-BETEXT-39 TO HEAD-BETEXT-39                      
057900                 MOVE WS-BETEXT-40 TO HEAD-BETEXT-40                      
058000                 MOVE WS-BETEXT-41 TO HEAD-BETEXT-41                      
058100                 MOVE WS-BETEXT-42 TO HEAD-BETEXT-42                      
058200                 MOVE WS-BETEXT-43 TO HEAD-BETEXT-43                      
058300                 MOVE WS-BETEXT-44 TO HEAD-BETEXT-44                      
058400               END-IF                                                     
058500             ELSE                                                         
058600               CONTINUE                                                   
058700             END-IF                                                       
058800           ELSE                                                           
058900             IF HEAD-KDFINDOC = 'CR'                                      
059000               MOVE HEAD-IDBREAK-2(1:2) TO LANDX2-IDLANDX2                
059100               IF LANDX2-EU-IDLANDX2                                      
059200                 MOVE WS-RECO-IDVAT TO HEAD-IDVAT-RESP                    
059300                 IF HEAD-IDPARTNR = '119612'                              
059400                   MOVE 'SE556074308901' TO HEAD-IDVAT-RESP               
059500                 END-IF                                                   
059600                 PERFORM DB2-SELECT-T01INRE-DDGS                          
059700                 IF LINES-FOUND                                           
059800*                  IF HEAD-IDBREAK-1 = 'SERVEXT'                          
059900*                  AND HEAD-IDPARTNR = '25728'                            
060000*                    MOVE 'EXEMPT' TO HEAD-BETEXT-5                       
060100*                  ELSE                                                   
060200                     MOVE WS-BETEXT-5 TO HEAD-BETEXT-5                    
060300*                  END-IF                                                 
060400                   MOVE WS-BETEXT-6 TO HEAD-BETEXT-6                      
060500                   MOVE WS-BETEXT-7 TO HEAD-BETEXT-7                      
060600                   MOVE WS-BETEXT-8 TO HEAD-BETEXT-8                      
060700                   MOVE WS-BETEXT-9 TO HEAD-BETEXT-9                      
060800                   MOVE WS-BETEXT-10 TO HEAD-BETEXT-10                    
060900                   MOVE WS-BETEXT-11 TO HEAD-BETEXT-11                    
061000                   MOVE WS-BETEXT-12 TO HEAD-BETEXT-12                    
061100                   MOVE WS-BETEXT-13 TO HEAD-BETEXT-13                    
061200                   MOVE WS-BETEXT-14 TO HEAD-BETEXT-14                    
061300                   MOVE WS-BETEXT-15 TO HEAD-BETEXT-15                    
061400                   MOVE WS-BETEXT-16 TO HEAD-BETEXT-16                    
061500                   MOVE WS-BETEXT-17 TO HEAD-BETEXT-17                    
061600                   MOVE WS-BETEXT-18 TO HEAD-BETEXT-18                    
061700                   MOVE WS-BETEXT-19 TO HEAD-BETEXT-19                    
061800                   MOVE WS-BETEXT-20 TO HEAD-BETEXT-20                    
061900                   MOVE WS-BETEXT-21 TO HEAD-BETEXT-21                    
062000                   MOVE WS-BETEXT-22 TO HEAD-BETEXT-22                    
062100                   MOVE WS-BETEXT-23 TO HEAD-BETEXT-23                    
062200                   MOVE WS-BETEXT-24 TO HEAD-BETEXT-24                    
062300                   MOVE WS-BETEXT-25 TO HEAD-BETEXT-25                    
062400                   MOVE WS-BETEXT-26 TO HEAD-BETEXT-26                    
062500                   MOVE WS-BETEXT-27 TO HEAD-BETEXT-27                    
062600                   MOVE WS-BETEXT-28 TO HEAD-BETEXT-28                    
062700                   MOVE WS-BETEXT-29 TO HEAD-BETEXT-29                    
062800                   MOVE WS-BETEXT-30 TO HEAD-BETEXT-30                    
062900                   MOVE WS-BETEXT-31 TO HEAD-BETEXT-31                    
063000                   MOVE WS-BETEXT-32 TO HEAD-BETEXT-32                    
063100                   MOVE WS-BETEXT-33 TO HEAD-BETEXT-33                    
063200                   MOVE WS-BETEXT-34 TO HEAD-BETEXT-34                    
063300                   MOVE WS-BETEXT-35 TO HEAD-BETEXT-35                    
063400                   MOVE WS-BETEXT-36 TO HEAD-BETEXT-36                    
063500                   MOVE WS-BETEXT-37 TO HEAD-BETEXT-37                    
063600                   MOVE WS-BETEXT-38 TO HEAD-BETEXT-38                    
063700                   MOVE WS-BETEXT-39 TO HEAD-BETEXT-39                    
063800                   MOVE WS-BETEXT-40 TO HEAD-BETEXT-40                    
063900                   MOVE WS-BETEXT-41 TO HEAD-BETEXT-41                    
064000                   MOVE WS-BETEXT-42 TO HEAD-BETEXT-42                    
064100                   MOVE WS-BETEXT-43 TO HEAD-BETEXT-43                    
064200                   MOVE WS-BETEXT-44 TO HEAD-BETEXT-44                    
064300                 END-IF                                                   
064400               ELSE                                                       
064500                 CONTINUE                                                 
064600               END-IF                                                     
064700             END-IF                                                       
064800           END-IF                                                         
064900         ELSE                                                             
065000           CONTINUE                                                       
065100         END-IF                                                           
065200       ELSE                                                               
065300         CONTINUE                                                         
065400       END-IF                                                             
065500     ELSE                                                                 
065600       CONTINUE                                                           
065700     END-IF                                                               
065800                                                                          
065900     IF HEAD-IDLEGSEL          = 'VCCS'                                   
066000       WRITE REC-WF2011     FROM HEAD-WF201101                            
066100     ELSE                                                                 
066200       WRITE REC-WF2031     FROM HEAD-WF201101                            
066300     END-IF                                                               
066400     .                                                                    
066500                                                                          
066600 S12-WRITE-WF20X1-LINE SECTION.                                           
066700     MOVE WS-DAFINDOC   TO LINE-DAFINDOC                                  
066800     MOVE WS-DAREFDAT   TO LINE-DAREFDAT                                  
066900     MOVE WS-DAFAKREF   TO LINE-DAFAKREF                                  
067000     MOVE WS-IDEXCUST-1 TO LINE-IDEXCUST-1                                
067100     MOVE WS-IDEXCUST-2 TO LINE-IDEXCUST-2                                
067200     MOVE WS-IDEXCUST-3 TO LINE-IDEXCUST-3                                
067300     MOVE WS-IDOPTION-1 TO LINE-IDOPTION-1                                
067400     MOVE WS-IDOPTION-2 TO LINE-IDOPTION-2                                
067500     MOVE WS-IDOPTION-3 TO LINE-IDOPTION-3                                
067600     MOVE WS-IDOPTION-4 TO LINE-IDOPTION-4                                
067700     MOVE WS-IDOPTION-5 TO LINE-IDOPTION-5                                
067800     MOVE WS-IDACCNT-1  TO LINE-IDACCNT-1                                 
067900     MOVE WS-IDACCNT-2  TO LINE-IDACCNT-2                                 
068000     MOVE WS-IDACCNT-3  TO LINE-IDACCNT-3                                 
068100     MOVE WS-IDACCNT-4  TO LINE-IDACCNT-4                                 
068200     MOVE CUST-FLCURRND TO LINE-FLCURRND                                  
068300                                                                          
068400     IF LINE-IDLEGSEL = WS-IDLEGSEL                                       
068500       CONTINUE                                                           
068600     ELSE                                                                 
068700       MOVE LINE-IDLEGSEL TO WS-IDLEGSEL                                  
068800     END-IF                                                               
068900     MOVE LINE-KDVALISO TO WS-KDVALISO-HEAD                               
069000     IF CUST-FLLOCCUR = 'J' AND WS-KDVALISO-HEAD = 'SEK'                  
069100     AND CUST-FLRATE = 'N'                                                
069200       IF LINE-KDFINDOC = 'CR'                                            
069300         IF LINE-IDLEVNR(1:4) = '0000'                                    
069400         OR LINE-IDLEVNR(1:4) = '    '                                    
069500           PERFORM DB2-SELECT-MAX-T01CURR-LINE                            
069600           PERFORM DB2-SELECT-T01CURR-LINE                                
069700         ELSE                                                             
069800           MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                           
069900     MOVE LINE-IDLEVNR(1:4) TO WS-DASTADAT-CREDIT(3:4)                    
070000           MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                           
070100           PERFORM DB2-SELECT-MAX-CURR-LINE-CRE                           
070200           PERFORM DB2-SELECT-CURR-LINE-CRE                               
070300         END-IF                                                           
070400       ELSE                                                               
070500         PERFORM DB2-SELECT-MAX-T01CURR-LINE                              
070600         PERFORM DB2-SELECT-T01CURR-LINE                                  
070700       END-IF                                                             
070800       MOVE HEAD-REVALUTA   TO WS-REVALUTA-LOCCUR                         
070900       MOVE HEAD-PRKURS-LOC TO WS-PRKURS-LOCCUR                           
071000                                                                          
071100       IF CUST-KDVALISO = WS-KDVALISO-HEAD                                
071200         CONTINUE                                                         
071300       ELSE                                                               
071400         IF LINE-KDFINDOC = 'CR'                                          
071500           IF LINE-IDLEVNR(1:4) = '0000'                                  
071600           OR LINE-IDLEVNR(1:4) = '    '                                  
071700             PERFORM DB2-SELECT-MAX-T01CURR-LOC                           
071800             PERFORM DB2-SELECT-T01CURR-LOC                               
071900           ELSE                                                           
072000             MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                         
072100     MOVE LINE-IDLEVNR(1:4) TO WS-DASTADAT-CREDIT(3:4)                    
072200             MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                         
072300             PERFORM DB2-SELECT-MAX-CURR-LOC-CRE                          
072400             PERFORM DB2-SELECT-CURR-LOC-CRE                              
072500           END-IF                                                         
072600         ELSE                                                             
072700           PERFORM DB2-SELECT-MAX-T01CURR-LOC                             
072800           PERFORM DB2-SELECT-T01CURR-LOC                                 
072900         END-IF                                                           
073000         MOVE HEAD-REVALUTA   TO WS-REVALUTA-LOCCUR                       
073100         MOVE HEAD-PRKURS-LOC TO WS-PRKURS-LOCCUR                         
073200         COMPUTE LINE-PRARTBTO ROUNDED =                                  
073300                          (LINE-PRARTBTO * WS-REVALUTA-LOCCUR) /          
073400                           WS-PRKURS-LOCCUR                               
073500         COMPUTE LINE-PRARTNTO ROUNDED =                                  
073600                          (LINE-PRARTNTO * WS-REVALUTA-LOCCUR) /          
073700                           WS-PRKURS-LOCCUR                               
073800         COMPUTE LINE-SUNTO    ROUNDED =                                  
073900                          (LINE-SUNTO    * WS-REVALUTA-LOCCUR) /          
074000                           WS-PRKURS-LOCCUR                               
074100         COMPUTE LINE-SUBTO    ROUNDED =                                  
074200                          (LINE-SUBTO    * WS-REVALUTA-LOCCUR) /          
074300                           WS-PRKURS-LOCCUR                               
074400         COMPUTE LINE-SUVAT-BILLIT ROUNDED =                              
074500                        (LINE-SUVAT-BILLIT * WS-REVALUTA-LOCCUR) /        
074600                           WS-PRKURS-LOCCUR                               
074700         MOVE CUST-KDVALISO     TO LINE-KDVALISO                          
074800       END-IF                                                             
074900     END-IF                                                               
075000                                                                          
075100     IF LINE-IDLEGSEL          = 'VCCS'                                   
075200       WRITE REC-WF2011     FROM LINE-WF201102                            
075300     ELSE                                                                 
075400       WRITE REC-WF2031     FROM LINE-WF201102                            
075500     END-IF                                                               
075600     .                                                                    
075700                                                                          
075800 S13-WRITE-WF20X1-FOOT SECTION.                                           
075900     MOVE '3  '                 TO FOOT-IDPTYP                            
076000     MOVE HEAD-DAFINDOC         TO FOOT-DAFINDOC                          
076100     MOVE HEAD-IDFINDOC         TO FOOT-IDFINDOC                          
076200     MOVE HEAD-IDLOPNR          TO FOOT-IDLOPNR                           
076300     MOVE HEAD-IDLEGSEL         TO FOOT-IDLEGSEL                          
076400     MOVE HEAD-BEFORMS          TO FOOT-BEFORMS                           
076500     MOVE HEAD-DAEXDAT          TO FOOT-DAEXDAT                           
076600     MOVE HEAD-TIEXTID          TO FOOT-TIEXTID                           
076700     MOVE HEAD-KDVALISO         TO FOOT-KDVALISO                          
076800     MOVE HEAD-IDLANDX3-SEND    TO FOOT-IDLANDX3-SEND                     
076900     MOVE HEAD-IDLEVNR          TO FOOT-IDLEVNR                           
077000     MOVE HEAD-IDPARTNR         TO FOOT-IDPARTNR                          
077100     MOVE HEAD-KDFINDOC         TO FOOT-KDFINDOC                          
077200     MOVE HEAD-FLSOFT           TO FOOT-FLSOFT                            
077300     MOVE HEAD-FLFREE           TO FOOT-FLFREE                            
077400     MOVE HEAD-IDBREAK-1        TO FOOT-IDBREAK-1                         
077500     MOVE HEAD-IDBREAK-2        TO FOOT-IDBREAK-2                         
077600     MOVE HEAD-SUNTO-TOT        TO FOOT-SUNTO-TOT                         
077700     MOVE HEAD-SUBTO-TOT        TO FOOT-SUBTO-TOT                         
077800     MOVE HEAD-SUVAT-BILLIT-TOT TO FOOT-SUVAT-BILLIT-TOT                  
077900     MOVE HEAD-SUNTO-TOT-LOC    TO FOOT-SUNTO-TOT-LOC                     
078000     MOVE HEAD-SUBTO-TOT-LOC    TO FOOT-SUBTO-TOT-LOC                     
078100     MOVE HEAD-SUVAT-BILLIT-TOT-LOC                                       
078200                                TO FOOT-SUVAT-BILLIT-TOT-LOC              
078300     MOVE HEAD-KDVALISO-LOC     TO FOOT-KDVALISO-LOC                      
078400     MOVE CUST-FLDECIMAL        TO FOOT-FLDECIMAL                         
078500                                                                          
078600     IF LINES-FOUND                                                       
078700       MOVE WS-BETEXT-1         TO FOOT-BETEXT-1                          
078800       MOVE WS-BETEXT-2         TO FOOT-BETEXT-2                          
078900       MOVE WS-BETEXT-3         TO FOOT-BETEXT-3                          
079000       MOVE WS-BETEXT-4         TO FOOT-BETEXT-4                          
079100     ELSE                                                                 
079200       MOVE SPACE               TO FOOT-BETEXT-1                          
079300       MOVE SPACE               TO FOOT-BETEXT-2                          
079400       MOVE SPACE               TO FOOT-BETEXT-3                          
079500       MOVE SPACE               TO FOOT-BETEXT-4                          
079600     END-IF                                                               
079700* FOR VOR SENDING CURRENCY MUST BE SENT IF DIFFERENT FROM                 
079800* INVOICE CURRENCY AND LOCAL CURRENCY                                     
079900     PERFORM DB2-SELECT-T01SECO-SND                                       
080000     MOVE WS-KDVALISO-SND         TO FOOT-KDVALISO-SND                    
080100     MOVE ZERO                    TO FOOT-PRKURS-SND                      
080200     MOVE ZERO                    TO FOOT-REVALUTA-SND                    
080300     MOVE SPACE                   TO FOOT-FLCURINF                        
080400* FOR DUBAI INVOICE THAT IS IN USD, AED EQUIVALENT SHOULD BE SHOWN        
080500     IF FOOT-IDLANDX3-SEND = 'AE'                                         
080600     AND WS-KDVALISO-SND   = 'USD'                                        
080700       MOVE 'AED'                 TO FOOT-KDVALISO-SND                    
080800     END-IF                                                               
080900**** END FIX                                                              
081000     IF FOOT-KDVALISO-SND = FOOT-KDVALISO                                 
081100       CONTINUE                                                           
081200     ELSE                                                                 
081300       IF FOOT-KDVALISO-SND = FOOT-KDVALISO-LOC                           
081400         CONTINUE                                                         
081500       ELSE                                                               
081600         IF FOOT-KDFINDOC = 'CR'                                          
081700           IF FOOT-IDLEVNR(1:4) = '0000'                                  
081800           OR FOOT-IDLEVNR(1:4) = '    '                                  
081900             PERFORM DB2-SELECT-MAX-T01CURR-SND                           
082000             PERFORM DB2-SELECT-T01CURR-SND                               
082100             IF TABLE-MISSING                                             
082200               MOVE 1 TO WS-PRKURS-SND                                    
082300               MOVE 1 TO WS-REVALUTA-SND                                  
082400             END-IF                                                       
082500           ELSE                                                           
082600             MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                         
082700             MOVE FOOT-IDLEVNR(1:4) TO WS-DASTADAT-CREDIT(3:4)            
082800             MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                         
082900             PERFORM DB2-SELECT-MAX-CURR-SND-CRE                          
083000             PERFORM DB2-SELECT-CURR-SND-CRE                              
083100           END-IF                                                         
083200         ELSE                                                             
083300           PERFORM DB2-SELECT-MAX-T01CURR-SND                             
083400           PERFORM DB2-SELECT-T01CURR-SND                                 
083500           IF TABLE-MISSING                                               
083600             MOVE 1 TO WS-PRKURS-SND                                      
083700             MOVE 1 TO WS-REVALUTA-SND                                    
083800           END-IF                                                         
083900         END-IF                                                           
084000         IF CUST-FLCURINF = 'J' OR SPACES                                 
084100           MOVE 'J'               TO FOOT-FLCURINF                        
084200           COMPUTE FOOT-SUNTO-TOT-SND ROUNDED =                           
084300                   HEAD-SUNTO-TOT *                                       
084400                   (HEAD-PRKURS / HEAD-REVALUTA) /                        
084500                   (WS-PRKURS-SND * WS-REVALUTA-SND)                      
084600           END-COMPUTE                                                    
084700           COMPUTE FOOT-SUVAT-BILLIT-TOT-SND ROUNDED =                    
084800                   HEAD-SUVAT-BILLIT-TOT *                                
084900                   (HEAD-PRKURS / HEAD-REVALUTA) /                        
085000                   (WS-PRKURS-SND * WS-REVALUTA-SND)                      
085100           END-COMPUTE                                                    
085200           COMPUTE FOOT-SUBTO-TOT-SND =                                   
085300                                  FOOT-SUNTO-TOT-SND +                    
085400                                  FOOT-SUVAT-BILLIT-TOT-SND               
085500           END-COMPUTE                                                    
085600         ELSE                                                             
085700           MOVE 'N'               TO FOOT-FLCURINF                        
085800         END-IF                                                           
085900*** SENDING CURRENCY SHOULD BE AED FOR DUBAI TO SHOW ON INVOICE           
086000*** IN WF2326                                                             
086100         IF FOOT-IDLANDX3-SEND = 'AE'                                     
086200         AND FOOT-KDVALISO-SND = 'AED'                                    
086300           MOVE HEAD-PRKURS           TO FOOT-PRKURS-SND                  
086400         ELSE                                                             
086500           MOVE WS-PRKURS-SND         TO FOOT-PRKURS-SND                  
086600         END-IF                                                           
086700         MOVE WS-REVALUTA-SND         TO FOOT-REVALUTA-SND                
086800         IF CUST-FLLOCCUR = 'J' AND WS-KDVALISO-HEAD =                    
086900                                    'SEK'                                 
087000           IF HEAD-KDVALISO-LOC = WS-KDVALISO-HEAD                        
087100             CONTINUE                                                     
087200           ELSE                                                           
087300             COMPUTE FOOT-SUNTO-TOT-SND ROUNDED =                         
087400                 HEAD-SUNTO-TOT *                                         
087500                 (WS-PRKURS-LOCCUR / WS-REVALUTA-LOCCUR) /                
087600                 (WS-PRKURS-SND * WS-REVALUTA-SND)                        
087700             END-COMPUTE                                                  
087800             COMPUTE FOOT-SUVAT-BILLIT-TOT-SND ROUNDED =                  
087900                     HEAD-SUVAT-BILLIT-TOT *                              
088000                 (WS-PRKURS-LOCCUR / WS-REVALUTA-LOCCUR) /                
088100                 (WS-PRKURS-SND * WS-REVALUTA-SND)                        
088200             END-COMPUTE                                                  
088300             COMPUTE FOOT-SUBTO-TOT-SND =                                 
088400                                    FOOT-SUNTO-TOT-SND +                  
088500                                    FOOT-SUVAT-BILLIT-TOT-SND             
088600             END-COMPUTE                                                  
088700           END-IF                                                         
088800         END-IF                                                           
088900       END-IF                                                             
089000     END-IF                                                               
089100     IF FOOT-IDLEGSEL          = 'VCCS'                                   
089200       WRITE REC-WF2011     FROM FOOT-WF201103                            
089300     ELSE                                                                 
089400       WRITE REC-WF2031     FROM FOOT-WF201103                            
089500     END-IF                                                               
089600     .                                                                    
089700                                                                          
089800 S14-WRITE-WF20X1-APPX SECTION.                                           
089900     MOVE WS-DAFINDOC   TO APPX-DAFINDOC                                  
090000     COMPUTE WS-IDLOPNR = WS-IDLOPNR + 1                                  
090100                                                                          
090200     IF APPX-IDLEGSEL = WS-IDLEGSEL                                       
090300       CONTINUE                                                           
090400     ELSE                                                                 
090500       MOVE APPX-IDLEGSEL TO WS-IDLEGSEL                                  
090600     END-IF                                                               
090700     MOVE APPX-KDVALISO TO WS-KDVALISO-HEAD                               
090800     IF CUST-FLLOCCUR = 'J' AND WS-KDVALISO-HEAD = 'SEK'                  
090900     AND CUST-FLRATE = 'N'                                                
091000       IF APPX-KDFINDOC = 'CR'                                            
091100         IF APPX-IDLEVNR(1:4) = '0000'                                    
091200         OR APPX-IDLEVNR(1:4) = '    '                                    
091300           PERFORM DB2-SELECT-MAX-T01CURR-APPX                            
091400           PERFORM DB2-SELECT-T01CURR-APPX                                
091500         ELSE                                                             
091600           MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                           
091700     MOVE APPX-IDLEVNR(1:4) TO WS-DASTADAT-CREDIT(3:4)                    
091800           MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                           
091900           PERFORM DB2-SELECT-MAX-CURR-APPX-CRE                           
092000           PERFORM DB2-SELECT-CURR-APPX-CRE                               
092100         END-IF                                                           
092200       ELSE                                                               
092300         PERFORM DB2-SELECT-MAX-T01CURR-APPX                              
092400         PERFORM DB2-SELECT-T01CURR-APPX                                  
092500       END-IF                                                             
092600       MOVE HEAD-REVALUTA   TO WS-REVALUTA-LOCCUR                         
092700       MOVE HEAD-PRKURS-LOC TO WS-PRKURS-LOCCUR                           
092800                                                                          
092900       IF CUST-KDVALISO = WS-KDVALISO-HEAD                                
093000         CONTINUE                                                         
093100       ELSE                                                               
093200         IF APPX-KDFINDOC = 'CR'                                          
093300           IF APPX-IDLEVNR(1:4) = '0000'                                  
093400           OR APPX-IDLEVNR(1:4) = '    '                                  
093500             PERFORM DB2-SELECT-MAX-T01CURR-LOC                           
093600             PERFORM DB2-SELECT-T01CURR-LOC                               
093700           ELSE                                                           
093800             MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                         
093900     MOVE APPX-IDLEVNR(1:4) TO WS-DASTADAT-CREDIT(3:4)                    
094000             MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                         
094100             PERFORM DB2-SELECT-MAX-CURR-LOC-CRE                          
094200             PERFORM DB2-SELECT-CURR-LOC-CRE                              
094300           END-IF                                                         
094400         ELSE                                                             
094500           PERFORM DB2-SELECT-MAX-T01CURR-LOC                             
094600           PERFORM DB2-SELECT-T01CURR-LOC                                 
094700         END-IF                                                           
094800         MOVE HEAD-REVALUTA   TO WS-REVALUTA-LOCCUR                       
094900         MOVE HEAD-PRKURS-LOC TO WS-PRKURS-LOCCUR                         
095000         COMPUTE APPX-SUNTO-APP ROUNDED =                                 
095100                      (APPX-SUNTO-APP * WS-REVALUTA-LOCCUR) /             
095200                       WS-PRKURS-LOCCUR                                   
095300         COMPUTE APPX-SUBTO-APP ROUNDED =                                 
095400                      (APPX-SUBTO-APP * WS-REVALUTA-LOCCUR) /             
095500                       WS-PRKURS-LOCCUR                                   
095600         COMPUTE APPX-SUVAT-BILLIT-APP ROUNDED =                          
095700                    (APPX-SUVAT-BILLIT-APP * WS-REVALUTA-LOCCUR) /        
095800                       WS-PRKURS-LOCCUR                                   
095900         MOVE CUST-KDVALISO       TO APPX-KDVALISO                        
096000       END-IF                                                             
096100     END-IF                                                               
096200*    IF  HEAD-IDBREAK-1 = 'SERVEXT'                                       
096300*    AND HEAD-IDPARTNR = '25728'                                          
096400*      IF APPX-KDAPPEND = 'PGRP'                                          
096500*        CONTINUE                                                         
096600*      ELSE                                                               
096700*        MOVE 'IPT'               TO APPX-KDAPPEND                        
096800*      END-IF                                                             
096900*    END-IF                                                               
097000                                                                          
097100     IF APPX-IDLEGSEL          = 'VCCS'                                   
097200       WRITE REC-WF2011       FROM APPX-WF201104                          
097300     ELSE                                                                 
097400       WRITE REC-WF2031       FROM APPX-WF201104                          
097500     END-IF                                                               
097600     .                                                                    
097700                                                                          
097800* --- DB2 SECTIONS  ---                                                   
097900*                                                                         
098000 DB2-SELECT-T01PROC SECTION.                                              
098100     MOVE 000     TO GOOD-SQLCODES                                        
098200                                                                          
098300     EXEC SQL                                                             
098400           SELECT  IDLEGSEL                                               
098500                ,  DAEXDAT                                                
098600                ,  TIEXTID                                                
098700                ,  KDBEH                                                  
098800                                                                          
098900           INTO   :PROC-IDLEGSEL                                          
099000                , :PROC-DAEXDAT                                           
099100                , :PROC-TIEXTID                                           
099200                , :PROC-KDBEH                                             
099300                                                                          
099400           FROM    T01PROC                                                
099500                                                                          
099600           WHERE   IDSYSTEM = :WS-IDSYSTEM     AND                        
099700                   IDLEGSEL = :WS-IDLEGSEL-CRS                            
099800     END-EXEC                                                             
099900                                                                          
100000     MOVE SQLCODE TO SQLCODE-WS                                           
100100     PERFORM DB2-STATUS-CHECK                                             
100200     .                                                                    
100300                                                                          
100400 DB2-SELECT-T01COCO-LSEL  SECTION.                                        
100500     MOVE 000     TO GOOD-SQLCODES                                        
100600                                                                          
100700     EXEC SQL                                                             
100800           SELECT  BELAND                                                 
100900                                                                          
101000           INTO   :HEAD-BELAND-LEG                                        
101100                                                                          
101200           FROM    T01COCO                                                
101300                                                                          
101400           WHERE   IDLANDX3 = :HEAD-IDLANDX3-LEG                          
101500     END-EXEC                                                             
101600                                                                          
101700     MOVE SQLCODE TO SQLCODE-WS                                           
101800     PERFORM DB2-STATUS-CHECK                                             
101900     .                                                                    
102000                                                                          
102100 DB2-SELECT-T01COCO-RESP SECTION.                                         
102200     MOVE 000     TO GOOD-SQLCODES                                        
102300                                                                          
102400     EXEC SQL                                                             
102500           SELECT  BELAND                                                 
102600                                                                          
102700           INTO   :HEAD-BELAND-RESP                                       
102800                                                                          
102900           FROM    T01COCO                                                
103000                                                                          
103100           WHERE   IDLANDX3 = :HEAD-IDLANDX3-RESP                         
103200     END-EXEC                                                             
103300                                                                          
103400     MOVE SQLCODE TO SQLCODE-WS                                           
103500     PERFORM DB2-STATUS-CHECK                                             
103600     .                                                                    
103700                                                                          
103800 DB2-SELECT-T01COCO-BET SECTION.                                          
103900     MOVE 000     TO GOOD-SQLCODES                                        
104000                                                                          
104100     EXEC SQL                                                             
104200           SELECT  BELAND                                                 
104300                                                                          
104400           INTO   :HEAD-BELAND-BET                                        
104500                                                                          
104600           FROM    T01COCO                                                
104700                                                                          
104800           WHERE   IDLANDX3 = :HEAD-IDLANDX3-BET                          
104900     END-EXEC                                                             
105000                                                                          
105100     MOVE SQLCODE TO SQLCODE-WS                                           
105200     PERFORM DB2-STATUS-CHECK                                             
105300     .                                                                    
105400                                                                          
105500 DB2-SELECT-T01SECO-SND SECTION.                                          
105600     MOVE 000  TO GOOD-SQLCODES                                           
105700                                                                          
105800     EXEC SQL                                                             
105900           SELECT  KDVALISO                                               
106000                                                                          
106100           INTO   :WS-KDVALISO-SND                                        
106200                                                                          
106300           FROM    T01SECO                                                
106400                                                                          
106500           WHERE   IDLANDX3 = :FOOT-IDLANDX3-SEND                         
106600           AND     IDLEGSEL = :FOOT-IDLEGSEL                              
106700           AND     KDSTATUS = 001                                         
106800     END-EXEC                                                             
106900                                                                          
107000     MOVE SQLCODE TO SQLCODE-WS                                           
107100     PERFORM DB2-STATUS-CHECK                                             
107200     .                                                                    
107300                                                                          
107400 DB2-SELECT-MAX-T01CURR SECTION.                                          
107500     MOVE 000            TO GOOD-SQLCODES                                 
107600                                                                          
107700     EXEC SQL                                                             
107800     SELECT   MAX(T01CURR.DASTADAT)                                       
107900                                                                          
108000     INTO     :WS-DASTADAT-KEY                                            
108100                                                                          
108200     FROM     T01CURR                                                     
108300                                                                          
108400     WHERE    T01CURR.IDLEGSEL = :HEAD-IDLEGSEL                           
108500     AND     (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
108600     OR       T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
108700     END-EXEC                                                             
108800                                                                          
108900     MOVE SQLCODE        TO SQLCODE-WS                                    
109000     PERFORM DB2-STATUS-CHECK                                             
109100     .                                                                    
109200                                                                          
109300 DB2-SELECT-MAX-T01CURR-LINE SECTION.                                     
109400     MOVE 000            TO GOOD-SQLCODES                                 
109500                                                                          
109600     EXEC SQL                                                             
109700     SELECT   MAX(T01CURR.DASTADAT)                                       
109800                                                                          
109900     INTO     :WS-DASTADAT-KEY                                            
110000                                                                          
110100     FROM     T01CURR                                                     
110200                                                                          
110300     WHERE    T01CURR.IDLEGSEL = :LINE-IDLEGSEL                           
110400     AND     (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
110500     OR       T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
110600     END-EXEC                                                             
110700                                                                          
110800     MOVE SQLCODE        TO SQLCODE-WS                                    
110900     PERFORM DB2-STATUS-CHECK                                             
111000     .                                                                    
111100                                                                          
111200 DB2-SELECT-MAX-T01CURR-APPX SECTION.                                     
111300     MOVE 000            TO GOOD-SQLCODES                                 
111400                                                                          
111500     EXEC SQL                                                             
111600     SELECT   MAX(T01CURR.DASTADAT)                                       
111700                                                                          
111800     INTO     :WS-DASTADAT-KEY                                            
111900                                                                          
112000     FROM     T01CURR                                                     
112100                                                                          
112200     WHERE    T01CURR.IDLEGSEL = :APPX-IDLEGSEL                           
112300     AND     (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
112400     OR       T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
112500     END-EXEC                                                             
112600                                                                          
112700     MOVE SQLCODE        TO SQLCODE-WS                                    
112800     PERFORM DB2-STATUS-CHECK                                             
112900     .                                                                    
113000                                                                          
113100 DB2-SELECT-T01CURR SECTION.                                              
113200     MOVE 000            TO GOOD-SQLCODES                                 
113300                                                                          
113400     EXEC SQL                                                             
113500     SELECT   PRKURS                                                      
113600             ,REVALUTA                                                    
113700                                                                          
113800     INTO     :HEAD-PRKURS-LOC                                            
113900             ,:HEAD-REVALUTA                                              
114000                                                                          
114100     FROM     T01CURR                                                     
114200                                                                          
114300     WHERE    IDLEGSEL = :HEAD-IDLEGSEL                                   
114400     AND      KDVALISO = :HEAD-KDVALISO-LOC                               
114500     AND      DASTADAT = :WS-DASTADAT-KEY                                 
114600     END-EXEC                                                             
114700                                                                          
114800     MOVE SQLCODE        TO SQLCODE-WS                                    
114900     PERFORM DB2-STATUS-CHECK                                             
115000     .                                                                    
115100                                                                          
115200 DB2-SELECT-T01CURR-LINE SECTION.                                         
115300     MOVE 000            TO GOOD-SQLCODES                                 
115400                                                                          
115500     EXEC SQL                                                             
115600     SELECT   PRKURS                                                      
115700             ,REVALUTA                                                    
115800                                                                          
115900     INTO     :HEAD-PRKURS-LOC                                            
116000             ,:HEAD-REVALUTA                                              
116100                                                                          
116200     FROM     T01CURR                                                     
116300                                                                          
116400     WHERE    IDLEGSEL = :LINE-IDLEGSEL                                   
116500     AND      KDVALISO = :CUST-KDVALISO                                   
116600     AND      DASTADAT = :WS-DASTADAT-KEY                                 
116700     END-EXEC                                                             
116800                                                                          
116900     MOVE SQLCODE        TO SQLCODE-WS                                    
117000     PERFORM DB2-STATUS-CHECK                                             
117100     .                                                                    
117200                                                                          
117300 DB2-SELECT-T01CURR-APPX SECTION.                                         
117400     MOVE 000            TO GOOD-SQLCODES                                 
117500                                                                          
117600     EXEC SQL                                                             
117700     SELECT   PRKURS                                                      
117800             ,REVALUTA                                                    
117900                                                                          
118000     INTO     :HEAD-PRKURS-LOC                                            
118100             ,:HEAD-REVALUTA                                              
118200                                                                          
118300     FROM     T01CURR                                                     
118400                                                                          
118500     WHERE    IDLEGSEL = :APPX-IDLEGSEL                                   
118600     AND      KDVALISO = :CUST-KDVALISO                                   
118700     AND      DASTADAT = :WS-DASTADAT-KEY                                 
118800     END-EXEC                                                             
118900                                                                          
119000     MOVE SQLCODE        TO SQLCODE-WS                                    
119100     PERFORM DB2-STATUS-CHECK                                             
119200     .                                                                    
119300                                                                          
119400 DB2-SELECT-MAX-T01CURR-SND SECTION.                                      
119500     MOVE 000            TO GOOD-SQLCODES                                 
119600                                                                          
119700     EXEC SQL                                                             
119800     SELECT   MAX(T01CURR.DASTADAT)                                       
119900                                                                          
120000     INTO     :WS-DASTADAT-SND                                            
120100                                                                          
120200     FROM     T01CURR                                                     
120300                                                                          
120400     WHERE    T01CURR.IDLEGSEL = :HEAD-IDLEGSEL                           
120500     AND     (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
120600     OR       T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
120700     END-EXEC                                                             
120800                                                                          
120900     MOVE SQLCODE        TO SQLCODE-WS                                    
121000     PERFORM DB2-STATUS-CHECK                                             
121100     .                                                                    
121200                                                                          
121300 DB2-SELECT-T01CURR-SND SECTION.                                          
121400     MOVE 000305         TO GOOD-SQLCODES                                 
121500                                                                          
121600     EXEC SQL                                                             
121700     SELECT   PRKURS                                                      
121800             ,REVALUTA                                                    
121900                                                                          
122000     INTO     :WS-PRKURS-SND                                              
122100             ,:WS-REVALUTA-SND                                            
122200                                                                          
122300     FROM     T01CURR                                                     
122400                                                                          
122500     WHERE    IDLEGSEL = :FOOT-IDLEGSEL                                   
122600     AND      KDVALISO = :FOOT-KDVALISO-SND                               
122700     AND      DASTADAT = :WS-DASTADAT-SND                                 
122800     END-EXEC                                                             
122900                                                                          
123000     MOVE SQLCODE        TO SQLCODE-WS                                    
123100     PERFORM DB2-STATUS-CHECK                                             
123200     .                                                                    
123300                                                                          
123400 DB2-SELECT-MAX-T01CURR-LOC SECTION.                                      
123500     MOVE 000            TO GOOD-SQLCODES                                 
123600                                                                          
123700     EXEC SQL                                                             
123800     SELECT   MAX(T01CURR.DASTADAT)                                       
123900                                                                          
124000     INTO     :WS-DASTADAT-KEY                                            
124100                                                                          
124200     FROM     T01CURR                                                     
124300                                                                          
124400     WHERE    T01CURR.IDLEGSEL = :CUST-IDLEGSEL                           
124500     AND     (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
124600     OR       T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
124700     END-EXEC                                                             
124800                                                                          
124900     MOVE SQLCODE        TO SQLCODE-WS                                    
125000     PERFORM DB2-STATUS-CHECK                                             
125100     .                                                                    
125200                                                                          
125300 DB2-SELECT-T01CURR-LOC SECTION.                                          
125400     MOVE 000            TO GOOD-SQLCODES                                 
125500                                                                          
125600     EXEC SQL                                                             
125700     SELECT   PRKURS                                                      
125800             ,REVALUTA                                                    
125900                                                                          
126000     INTO     :HEAD-PRKURS-LOC                                            
126100             ,:HEAD-REVALUTA                                              
126200                                                                          
126300     FROM     T01CURR                                                     
126400                                                                          
126500     WHERE    IDLEGSEL = :CUST-IDLEGSEL                                   
126600     AND      KDVALISO = :CUST-KDVALISO                                   
126700     AND      DASTADAT = :WS-DASTADAT-KEY                                 
126800     END-EXEC                                                             
126900                                                                          
127000     MOVE SQLCODE        TO SQLCODE-WS                                    
127100     PERFORM DB2-STATUS-CHECK                                             
127200     .                                                                    
127300                                                                          
127400 DB2-SELECT-MAX-CURR-CRE SECTION.                                         
127500     MOVE 000            TO GOOD-SQLCODES                                 
127600                                                                          
127700     EXEC SQL                                                             
127800     SELECT   MAX(T01CURR.DASTADAT)                                       
127900                                                                          
128000     INTO     :WS2-DASTADAT-CREDIT                                        
128100                                                                          
128200     FROM     T01CURR                                                     
128300                                                                          
128400     WHERE    T01CURR.IDLEGSEL = :HEAD-IDLEGSEL                           
128500     AND     (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
128600     OR       T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
128700     END-EXEC                                                             
128800                                                                          
128900     MOVE SQLCODE        TO SQLCODE-WS                                    
129000     PERFORM DB2-STATUS-CHECK                                             
129100     .                                                                    
129200                                                                          
129300 DB2-SELECT-MAX-CURR-LINE-CRE SECTION.                                    
129400     MOVE 000            TO GOOD-SQLCODES                                 
129500                                                                          
129600     EXEC SQL                                                             
129700     SELECT   MAX(T01CURR.DASTADAT)                                       
129800                                                                          
129900     INTO     :WS2-DASTADAT-CREDIT                                        
130000                                                                          
130100     FROM     T01CURR                                                     
130200                                                                          
130300     WHERE    T01CURR.IDLEGSEL = :LINE-IDLEGSEL                           
130400     AND     (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
130500     OR       T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
130600     END-EXEC                                                             
130700                                                                          
130800     MOVE SQLCODE        TO SQLCODE-WS                                    
130900     PERFORM DB2-STATUS-CHECK                                             
131000     .                                                                    
131100                                                                          
131200 DB2-SELECT-MAX-CURR-APPX-CRE SECTION.                                    
131300     MOVE 000            TO GOOD-SQLCODES                                 
131400                                                                          
131500     EXEC SQL                                                             
131600     SELECT   MAX(T01CURR.DASTADAT)                                       
131700                                                                          
131800     INTO     :WS2-DASTADAT-CREDIT                                        
131900                                                                          
132000     FROM     T01CURR                                                     
132100                                                                          
132200     WHERE    T01CURR.IDLEGSEL = :APPX-IDLEGSEL                           
132300     AND     (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
132400     OR       T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
132500     END-EXEC                                                             
132600                                                                          
132700     MOVE SQLCODE        TO SQLCODE-WS                                    
132800     PERFORM DB2-STATUS-CHECK                                             
132900     .                                                                    
133000                                                                          
133100 DB2-SELECT-CURR-CRE SECTION.                                             
133200     MOVE 000            TO GOOD-SQLCODES                                 
133300                                                                          
133400     EXEC SQL                                                             
133500     SELECT   PRKURS                                                      
133600             ,REVALUTA                                                    
133700                                                                          
133800     INTO     :HEAD-PRKURS-LOC                                            
133900             ,:HEAD-REVALUTA                                              
134000                                                                          
134100     FROM     T01CURR                                                     
134200                                                                          
134300     WHERE    IDLEGSEL = :HEAD-IDLEGSEL                                   
134400     AND      KDVALISO = :HEAD-KDVALISO-LOC                               
134500     AND      DASTADAT = :WS2-DASTADAT-CREDIT                             
134600     END-EXEC                                                             
134700                                                                          
134800     MOVE SQLCODE        TO SQLCODE-WS                                    
134900     PERFORM DB2-STATUS-CHECK                                             
135000     .                                                                    
135100                                                                          
135200 DB2-SELECT-CURR-LINE-CRE SECTION.                                        
135300     MOVE 000            TO GOOD-SQLCODES                                 
135400                                                                          
135500     EXEC SQL                                                             
135600     SELECT   PRKURS                                                      
135700             ,REVALUTA                                                    
135800                                                                          
135900     INTO     :HEAD-PRKURS-LOC                                            
136000             ,:HEAD-REVALUTA                                              
136100                                                                          
136200     FROM     T01CURR                                                     
136300                                                                          
136400     WHERE    IDLEGSEL = :LINE-IDLEGSEL                                   
136500     AND      KDVALISO = :CUST-KDVALISO                                   
136600     AND      DASTADAT = :WS2-DASTADAT-CREDIT                             
136700     END-EXEC                                                             
136800                                                                          
136900     MOVE SQLCODE        TO SQLCODE-WS                                    
137000     PERFORM DB2-STATUS-CHECK                                             
137100     .                                                                    
137200                                                                          
137300 DB2-SELECT-CURR-APPX-CRE SECTION.                                        
137400     MOVE 000            TO GOOD-SQLCODES                                 
137500                                                                          
137600     EXEC SQL                                                             
137700     SELECT   PRKURS                                                      
137800             ,REVALUTA                                                    
137900                                                                          
138000     INTO     :HEAD-PRKURS-LOC                                            
138100             ,:HEAD-REVALUTA                                              
138200                                                                          
138300     FROM     T01CURR                                                     
138400                                                                          
138500     WHERE    IDLEGSEL = :APPX-IDLEGSEL                                   
138600     AND      KDVALISO = :CUST-KDVALISO                                   
138700     AND      DASTADAT = :WS2-DASTADAT-CREDIT                             
138800     END-EXEC                                                             
138900                                                                          
139000     MOVE SQLCODE        TO SQLCODE-WS                                    
139100     PERFORM DB2-STATUS-CHECK                                             
139200     .                                                                    
139300                                                                          
139400 DB2-SELECT-MAX-CURR-SND-CRE SECTION.                                     
139500     MOVE 000            TO GOOD-SQLCODES                                 
139600                                                                          
139700     EXEC SQL                                                             
139800     SELECT   MAX(T01CURR.DASTADAT)                                       
139900                                                                          
140000     INTO     :WS2-DASTADAT-CREDIT-SND                                    
140100                                                                          
140200     FROM     T01CURR                                                     
140300                                                                          
140400     WHERE    T01CURR.IDLEGSEL = :HEAD-IDLEGSEL                           
140500     AND     (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
140600     OR       T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
140700     END-EXEC                                                             
140800                                                                          
140900     MOVE SQLCODE        TO SQLCODE-WS                                    
141000     PERFORM DB2-STATUS-CHECK                                             
141100     .                                                                    
141200                                                                          
141300 DB2-SELECT-CURR-SND-CRE SECTION.                                         
141400     MOVE 000            TO GOOD-SQLCODES                                 
141500                                                                          
141600     EXEC SQL                                                             
141700     SELECT   PRKURS                                                      
141800             ,REVALUTA                                                    
141900                                                                          
142000     INTO     :WS-PRKURS-SND                                              
142100             ,:WS-REVALUTA-SND                                            
142200                                                                          
142300     FROM     T01CURR                                                     
142400                                                                          
142500     WHERE    IDLEGSEL = :FOOT-IDLEGSEL                                   
142600     AND      KDVALISO = :FOOT-KDVALISO-SND                               
142700     AND      DASTADAT = :WS2-DASTADAT-CREDIT-SND                         
142800     END-EXEC                                                             
142900                                                                          
143000     MOVE SQLCODE        TO SQLCODE-WS                                    
143100     PERFORM DB2-STATUS-CHECK                                             
143200     .                                                                    
143300                                                                          
143400 DB2-SELECT-MAX-CURR-LOC-CRE SECTION.                                     
143500     MOVE 000            TO GOOD-SQLCODES                                 
143600                                                                          
143700     EXEC SQL                                                             
143800     SELECT   MAX(T01CURR.DASTADAT)                                       
143900                                                                          
144000     INTO     :WS2-DASTADAT-CREDIT                                        
144100                                                                          
144200     FROM     T01CURR                                                     
144300                                                                          
144400     WHERE    T01CURR.IDLEGSEL = :CUST-IDLEGSEL                           
144500     AND     (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
144600     OR       T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
144700     END-EXEC                                                             
144800                                                                          
144900     MOVE SQLCODE        TO SQLCODE-WS                                    
145000     PERFORM DB2-STATUS-CHECK                                             
145100     .                                                                    
145200                                                                          
145300 DB2-SELECT-CURR-LOC-CRE SECTION.                                         
145400     MOVE 000            TO GOOD-SQLCODES                                 
145500                                                                          
145600     EXEC SQL                                                             
145700     SELECT   PRKURS                                                      
145800             ,REVALUTA                                                    
145900                                                                          
146000     INTO     :HEAD-PRKURS-LOC                                            
146100             ,:HEAD-REVALUTA                                              
146200                                                                          
146300     FROM     T01CURR                                                     
146400                                                                          
146500     WHERE    IDLEGSEL = :CUST-IDLEGSEL                                   
146600     AND      KDVALISO = :CUST-KDVALISO                                   
146700     AND      DASTADAT = :WS2-DASTADAT-CREDIT                             
146800     END-EXEC                                                             
146900                                                                          
147000     MOVE SQLCODE        TO SQLCODE-WS                                    
147100     PERFORM DB2-STATUS-CHECK                                             
147200     .                                                                    
147300                                                                          
147400 DB2-SELECT-T01PAIN SECTION.                                              
147500     MOVE 000100         TO GOOD-SQLCODES                                 
147600                                                                          
147700     EXEC SQL                                                             
147800     SELECT   BETEXT_1                                                    
147900             ,BETEXT_2                                                    
148000             ,BETEXT_3                                                    
148100             ,BETEXT_4                                                    
148200                                                                          
148300     INTO     :WS-BETEXT-1                                                
148400             ,:WS-BETEXT-2                                                
148500             ,:WS-BETEXT-3                                                
148600             ,:WS-BETEXT-4                                                
148700                                                                          
148800     FROM     T01PAIN                                                     
148900                                                                          
149000     WHERE    IDLEGSEL = :HEAD-IDLEGSEL                                   
149100     AND      KDVALISO = :WS-KDVALISO                                     
149200     AND      KDFINDOC = :WS-KDFINDOC                                     
149300     AND      KDPARTTY = :WS-KDPARTTY                                     
149400     AND      KDPARTGR = :HEAD-KDPARTGR                                   
149500     AND      KDSTATUS = 001                                              
149600     END-EXEC                                                             
149700                                                                          
149800     MOVE SQLCODE        TO SQLCODE-WS                                    
149900     PERFORM DB2-STATUS-CHECK                                             
150000     .                                                                    
150100                                                                          
150200 DB2-SELECT-T01INRE SECTION.                                              
150300     MOVE 000100         TO GOOD-SQLCODES                                 
150400                                                                          
150500     EXEC SQL                                                             
150600     SELECT   BETEXT_1                                                    
150700             ,BETEXT_2                                                    
150800             ,BETEXT_3                                                    
150900             ,BETEXT_4                                                    
151000             ,BETEXT_5                                                    
151100             ,BETEXT_6                                                    
151200             ,BETEXT_7                                                    
151300             ,BETEXT_8                                                    
151400             ,BETEXT_9                                                    
151500             ,BETEXT_10                                                   
151600             ,BETEXT_11                                                   
151700             ,BETEXT_12                                                   
151800             ,BETEXT_13                                                   
151900             ,BETEXT_14                                                   
152000             ,BETEXT_15                                                   
152100             ,BETEXT_16                                                   
152200             ,BETEXT_17                                                   
152300             ,BETEXT_18                                                   
152400             ,BETEXT_19                                                   
152500             ,BETEXT_20                                                   
152600             ,BETEXT_21                                                   
152700             ,BETEXT_22                                                   
152800             ,BETEXT_23                                                   
152900             ,BETEXT_24                                                   
153000             ,BETEXT_25                                                   
153100             ,BETEXT_26                                                   
153200             ,BETEXT_27                                                   
153300             ,BETEXT_28                                                   
153400             ,BETEXT_29                                                   
153500             ,BETEXT_30                                                   
153600             ,BETEXT_31                                                   
153700             ,BETEXT_32                                                   
153800             ,BETEXT_33                                                   
153900             ,BETEXT_34                                                   
154000             ,BETEXT_35                                                   
154100             ,BETEXT_36                                                   
154200             ,BETEXT_37                                                   
154300             ,BETEXT_38                                                   
154400             ,BETEXT_39                                                   
154500             ,BETEXT_40                                                   
154600                                                                          
154700     INTO     :WS-BETEXT-5                                                
154800             ,:WS-BETEXT-6                                                
154900             ,:WS-BETEXT-7                                                
155000             ,:WS-BETEXT-8                                                
155100             ,:WS-BETEXT-9                                                
155200             ,:WS-BETEXT-10                                               
155300             ,:WS-BETEXT-11                                               
155400             ,:WS-BETEXT-12                                               
155500             ,:WS-BETEXT-13                                               
155600             ,:WS-BETEXT-14                                               
155700             ,:WS-BETEXT-15                                               
155800             ,:WS-BETEXT-16                                               
155900             ,:WS-BETEXT-17                                               
156000             ,:WS-BETEXT-18                                               
156100             ,:WS-BETEXT-19                                               
156200             ,:WS-BETEXT-20                                               
156300             ,:WS-BETEXT-21                                               
156400             ,:WS-BETEXT-22                                               
156500             ,:WS-BETEXT-23                                               
156600             ,:WS-BETEXT-24                                               
156700             ,:WS-BETEXT-25                                               
156800             ,:WS-BETEXT-26                                               
156900             ,:WS-BETEXT-27                                               
157000             ,:WS-BETEXT-28                                               
157100             ,:WS-BETEXT-29                                               
157200             ,:WS-BETEXT-30                                               
157300             ,:WS-BETEXT-31                                               
157400             ,:WS-BETEXT-32                                               
157500             ,:WS-BETEXT-33                                               
157600             ,:WS-BETEXT-34                                               
157700             ,:WS-BETEXT-35                                               
157800             ,:WS-BETEXT-36                                               
157900             ,:WS-BETEXT-37                                               
158000             ,:WS-BETEXT-38                                               
158100             ,:WS-BETEXT-39                                               
158200             ,:WS-BETEXT-40                                               
158300             ,:WS-BETEXT-41                                               
158400             ,:WS-BETEXT-42                                               
158500             ,:WS-BETEXT-43                                               
158600             ,:WS-BETEXT-44                                               
158700                                                                          
158800     FROM     T01INRE A                                                   
158900             ,T01FCUS B                                                   
159000                                                                          
159100     WHERE    A.IDLEGSEL      = :HEAD-IDLEGSEL                            
159200     AND      B.IDLEGSEL      = :HEAD-IDLEGSEL                            
159300     AND      A.IDLANDX3_SEND = :HEAD-IDLANDX3-SEND                       
159400     AND      A.IDLANDX3_REC  = B.IDLANDX3                                
159500     AND      B.IDPARTNR      = :HEAD-IDPARTNR                            
159600     AND      A.KDSTATUS      = 001                                       
159700     AND      B.KDSTATUS      = 001                                       
159800     END-EXEC                                                             
159900                                                                          
160000     MOVE SQLCODE        TO SQLCODE-WS                                    
160100     PERFORM DB2-STATUS-CHECK                                             
160200     .                                                                    
160300                                                                          
160400 DB2-SELECT-T01INRE-DDGS SECTION.                                         
160500     MOVE 000100         TO GOOD-SQLCODES                                 
160600                                                                          
160700     EXEC SQL                                                             
160800     SELECT   BETEXT_1                                                    
160900             ,BETEXT_2                                                    
161000             ,BETEXT_3                                                    
161100             ,BETEXT_4                                                    
161200             ,BETEXT_5                                                    
161300             ,BETEXT_6                                                    
161400             ,BETEXT_7                                                    
161500             ,BETEXT_8                                                    
161600             ,BETEXT_9                                                    
161700             ,BETEXT_10                                                   
161800             ,BETEXT_11                                                   
161900             ,BETEXT_12                                                   
162000             ,BETEXT_13                                                   
162100             ,BETEXT_14                                                   
162200             ,BETEXT_15                                                   
162300             ,BETEXT_16                                                   
162400             ,BETEXT_17                                                   
162500             ,BETEXT_18                                                   
162600             ,BETEXT_19                                                   
162700             ,BETEXT_20                                                   
162800             ,BETEXT_21                                                   
162900             ,BETEXT_22                                                   
163000             ,BETEXT_23                                                   
163100             ,BETEXT_24                                                   
163200             ,BETEXT_25                                                   
163300             ,BETEXT_26                                                   
163400             ,BETEXT_27                                                   
163500             ,BETEXT_28                                                   
163600             ,BETEXT_29                                                   
163700             ,BETEXT_30                                                   
163800             ,BETEXT_31                                                   
163900             ,BETEXT_32                                                   
164000             ,BETEXT_33                                                   
164100             ,BETEXT_34                                                   
164200             ,BETEXT_35                                                   
164300             ,BETEXT_36                                                   
164400             ,BETEXT_37                                                   
164500             ,BETEXT_38                                                   
164600             ,BETEXT_39                                                   
164700             ,BETEXT_40                                                   
164800                                                                          
164900     INTO     :WS-BETEXT-5                                                
165000             ,:WS-BETEXT-6                                                
165100             ,:WS-BETEXT-7                                                
165200             ,:WS-BETEXT-8                                                
165300             ,:WS-BETEXT-9                                                
165400             ,:WS-BETEXT-10                                               
165500             ,:WS-BETEXT-11                                               
165600             ,:WS-BETEXT-12                                               
165700             ,:WS-BETEXT-13                                               
165800             ,:WS-BETEXT-14                                               
165900             ,:WS-BETEXT-15                                               
166000             ,:WS-BETEXT-16                                               
166100             ,:WS-BETEXT-17                                               
166200             ,:WS-BETEXT-18                                               
166300             ,:WS-BETEXT-19                                               
166400             ,:WS-BETEXT-20                                               
166500             ,:WS-BETEXT-21                                               
166600             ,:WS-BETEXT-22                                               
166700             ,:WS-BETEXT-23                                               
166800             ,:WS-BETEXT-24                                               
166900             ,:WS-BETEXT-25                                               
167000             ,:WS-BETEXT-26                                               
167100             ,:WS-BETEXT-27                                               
167200             ,:WS-BETEXT-28                                               
167300             ,:WS-BETEXT-29                                               
167400             ,:WS-BETEXT-30                                               
167500             ,:WS-BETEXT-31                                               
167600             ,:WS-BETEXT-32                                               
167700             ,:WS-BETEXT-33                                               
167800             ,:WS-BETEXT-34                                               
167900             ,:WS-BETEXT-35                                               
168000             ,:WS-BETEXT-36                                               
168100             ,:WS-BETEXT-37                                               
168200             ,:WS-BETEXT-38                                               
168300             ,:WS-BETEXT-39                                               
168400             ,:WS-BETEXT-40                                               
168500             ,:WS-BETEXT-41                                               
168600             ,:WS-BETEXT-42                                               
168700             ,:WS-BETEXT-43                                               
168800             ,:WS-BETEXT-44                                               
168900                                                                          
169000     FROM     T01INRE A                                                   
169100             ,T01FCUS B                                                   
169200                                                                          
169300     WHERE    A.IDLEGSEL      = :HEAD-IDLEGSEL                            
169400     AND      B.IDLEGSEL      = :HEAD-IDLEGSEL                            
169500     AND      A.IDLANDX3_SEND = :HEAD-IDLANDX3-BET                        
169600     AND      A.IDLANDX3_REC  = B.IDLANDX3                                
169700     AND      B.IDPARTNR      = :HEAD-IDPARTNR                            
169800     AND      A.KDSTATUS      = 001                                       
169900     AND      B.KDSTATUS      = 001                                       
170000     END-EXEC                                                             
170100                                                                          
170200     MOVE SQLCODE        TO SQLCODE-WS                                    
170300     PERFORM DB2-STATUS-CHECK                                             
170400     .                                                                    
170500                                                                          
170600 DB2-SELECT-T01DLIN-KDFRAKT SECTION.                                      
170700     MOVE 000100  TO GOOD-SQLCODES                                        
170800                                                                          
170900*    DEFAULT KDFRAKT IS SET TO 00. IF THERE EXISTS NO LINE RECORDS        
171000*    FOR SOME REASON THEN 0 IS USED.                                      
171100                                                                          
171200     MOVE ZERO           TO HEAD-KDFRAKT                                  
171300                                                                          
171400************************ NOTE **********************************          
171500*    FETCH ***FIRST*** KDFRAKT AND ADD TO HEAD-RECORD.                    
171600*    TO BE USED IN LATER PROGRAMS FOR SETTING THE DAP RULE.               
171700************************ NOTE  END *****************************          
171800                                                                          
171900     EXEC SQL                                                             
172000           SELECT  KDFRAKT                                                
172100                                                                          
172200           INTO   :HEAD-KDFRAKT                                           
172300                                                                          
172400           FROM    T01DLIN                                                
172500                                                                          
172600           WHERE IDLEGSEL      = :PROC-IDLEGSEL                           
172700             AND DAEXDAT       = :PROC-DAEXDAT                            
172800             AND TIEXTID       = :PROC-TIEXTID                            
172900             AND KDVALISO      = :HEAD-KDVALISO                           
173000             AND IDLANDX3_SEND = :HEAD-IDLANDX3-SEND                      
173100             AND IDLEVNR       = :HEAD-IDLEVNR                            
173200             AND IDPARTNR      = :HEAD-IDPARTNR                           
173300             AND KDFINDOC      = :HEAD-KDFINDOC                           
173400             AND FLSOFT        = :HEAD-FLSOFT                             
173500             AND FLFREE        = :HEAD-FLFREE                             
173600             AND FLPRIV        = :WS-FLPRIV                               
173700             AND IDBREAK_1     = :HEAD-IDBREAK-1                          
173800             AND IDBREAK_2     = :HEAD-IDBREAK-2                          
173900             AND IDLOPNR IN                                               
174000                 (SELECT MIN(IDLOPNR)                                     
174100                  FROM   T01DLIN                                          
174200                  WHERE IDLEGSEL      = :PROC-IDLEGSEL                    
174300                    AND DAEXDAT       = :PROC-DAEXDAT                     
174400                    AND TIEXTID       = :PROC-TIEXTID                     
174500                    AND KDVALISO      = :HEAD-KDVALISO                    
174600                    AND IDLANDX3_SEND = :HEAD-IDLANDX3-SEND               
174700                    AND IDLEVNR       = :HEAD-IDLEVNR                     
174800                    AND IDPARTNR      = :HEAD-IDPARTNR                    
174900                    AND KDFINDOC      = :HEAD-KDFINDOC                    
175000                    AND FLSOFT        = :HEAD-FLSOFT                      
175100                    AND FLFREE        = :HEAD-FLFREE                      
175200                    AND FLPRIV        = :WS-FLPRIV                        
175300                    AND IDBREAK_1     = :HEAD-IDBREAK-1                   
175400                    AND IDBREAK_2     = :HEAD-IDBREAK-2                   
175500                 )                                                        
175600     END-EXEC                                                             
175700                                                                          
175800     MOVE SQLCODE TO SQLCODE-WS                                           
175900     PERFORM DB2-STATUS-CHECK                                             
176000     .                                                                    
176100                                                                          
176200 DB2-DCL-OPN-CRS1 SECTION.                                                
176300     MOVE 000100 TO GOOD-SQLCODES                                         
176400                                                                          
176500     EXEC SQL                                                             
176600        DECLARE CRS1 CURSOR WITH HOLD FOR                                 
176700        SELECT '1  '                                                      
176800             , A.IDLEGSEL                                                 
176900             , D.BEFORMS                                                  
177000             , A.KDVALISO                                                 
177100             , A.IDLANDX3_SEND                                            
177200             , A.IDLEVNR                                                  
177300             , A.IDPARTNR                                                 
177400             , A.KDFINDOC                                                 
177500             , A.FLSOFT                                                   
177600             , A.FLFREE                                                   
177700             , A.FLPRIV                                                   
177800             , A.IDBREAK_1                                                
177900             , A.IDBREAK_2                                                
178000             , A.DAFINDOC                                                 
178100             , A.IDFINDOC                                                 
178200             , 0                                                          
178300             , A.IDSPRAK                                                  
178400             , A.BEBETVIL                                                 
178500             , A.BELEGRAD_1                                               
178600             , A.BELEGRAD_2                                               
178700             , A.ADLEG_STREET                                             
178800             , A.ADLEG_BOX                                                
178900             , A.ADLEG_CITY                                               
179000             , A.ADLEG_PCODE                                              
179100             , A.IDLANDX3_LEG                                             
179200             , A.IDTFN_LEG                                                
179300             , A.IDTFX_LEG                                                
179400             , A.IDMAIL_LEG                                               
179500             , A.BECONT_LEG                                               
179600             , A.IDVAT_LEG                                                
179700             , A.IDBG_LEG                                                 
179800             , A.IDPG_LEG                                                 
179900             , A.BERESPRA_1                                               
180000             , A.BERESPRA_2                                               
180100             , A.ADRESP_STREET                                            
180200             , A.ADRESP_BOX                                               
180300             , A.ADRESP_CITY                                              
180400             , A.ADRESP_PCODE                                             
180500             , A.IDLANDX3_RESP                                            
180600             , A.IDTFN_RESP                                               
180700             , A.IDTFX_RESP                                               
180800             , A.IDMAIL_RESP                                              
180900             , A.BECONT_RESP                                              
181000             , A.IDVAT_RESP                                               
181100             , A.IDBG_RESP                                                
181200             , A.IDPG_RESP                                                
181300             , A.BEBET_NAME1                                              
181400             , A.BEBET_NAME2                                              
181500             , A.ADBET_STREET                                             
181600             , A.ADBET_BOX                                                
181700             , A.ADBET_CITY                                               
181800             , A.ADBET_PCODE                                              
181900             , A.IDLANDX3_BET                                             
182000             , A.IDVAT_BET                                                
182100             , A.SUNTO_SERV                                               
182200             , A.SUBTO_SERV                                               
182300             , A.SUNTO_PART                                               
182400             , A.SUBTO_PART                                               
182500             , A.SUNTO_TOT                                                
182600             , A.SUBTO_TOT                                                
182700             , A.SUVAT_BILLIT_TOT                                         
182800             , A.PRKURS                                                   
182900             , A.BEANST                                                   
183000             , A.IDUSER                                                   
183100             , A.BETEXT_1                                                 
183200             , A.BETEXT_2                                                 
183300             , A.BETEXT_3                                                 
183400             , A.BETEXT_4                                                 
183500             , A.BETEXT                                                   
183600             , A.BETEXT_CRE                                               
183700             , C.KDVALISO                                                 
183800             , C.FLRATE                                                   
183900             , C.FLLOCCUR                                                 
184000             , E.SUDOCLIM                                                 
184100             , A.IDVAT_AGENT                                              
184200             , C.KDVALISO                                                 
184300             , D.KDFINDOC                                                 
184400             , D.KDPARTTY                                                 
184500             , D.KDPARTGR                                                 
184600             , C.FLFINFIL                                                 
184700             , C.FLCURINF                                                 
184800             , C.FLCURRND                                                 
184900             , C.FLDIRVAT                                                 
185000             , E.IDVAT                                                    
185100             , A.IDSYSTEM_SEND                                            
185200             , C.FLDECIMAL                                                
185300                                                                          
185400        FROM   T01DHEA A                                                  
185500             , T01FCUS C                                                  
185600             , T01BURE D                                                  
185700             , T01RECO E                                                  
185800                                                                          
185900        WHERE  A.IDLEGSEL = :PROC-IDLEGSEL                                
186000        AND    A.DAEXDAT  = :PROC-DAEXDAT                                 
186100        AND    A.TIEXTID  = :PROC-TIEXTID                                 
186200        AND    C.IDLEGSEL = A.IDLEGSEL                                    
186300        AND    C.IDPARTNR = A.IDPARTNR                                    
186400        AND    C.KDSTATUS = :WS-CURRENT-VERSION                           
186500        AND    C.DADELDAT = :WS-ACTIVE                                    
186600        AND    D.IDLEGSEL = A.IDLEGSEL                                    
186700        AND    D.KDFINDOC = A.KDFINDOC                                    
186800        AND    D.KDPARTTY = C.KDPARTTY                                    
186900        AND    D.KDPARTGR = C.KDPARTGR                                    
187000        AND    D.KDSTATUS = :WS-CURRENT-VERSION                           
187100        AND    D.DADELDAT = :WS-ACTIVE                                    
187200        AND    E.IDLEGSEL = A.IDLEGSEL                                    
187300        AND    E.IDLANDX3 = A.IDLANDX3_BET                                
187400        AND    E.KDSTATUS = :WS-CURRENT-VERSION                           
187500        AND    E.DADELDAT = :WS-ACTIVE                                    
187600                                                                          
187700        ORDER BY A.IDLEGSEL                                               
187800               , D.BEFORMS                                                
187900               , A.DAEXDAT                                                
188000               , A.TIEXTID                                                
188100               , A.KDVALISO                                               
188200               , A.IDLANDX3_SEND                                          
188300               , A.IDLEVNR                                                
188400               , A.IDPARTNR                                               
188500               , A.KDFINDOC                                               
188600               , A.FLSOFT                                                 
188700               , A.FLFREE                                                 
188800               , A.FLPRIV                                                 
188900               , A.IDBREAK_1                                              
189000               , A.IDBREAK_2                                              
189100               , A.DAFINDOC                                               
189200               , A.IDFINDOC                                               
189300     END-EXEC                                                             
189400                                                                          
189500     EXEC SQL                                                             
189600        OPEN CRS1                                                         
189700     END-EXEC                                                             
189800                                                                          
189900     MOVE SQLCODE TO SQLCODE-WS                                           
190000     PERFORM DB2-STATUS-CHECK                                             
190100     .                                                                    
190200                                                                          
190300 DB2-DCL-OPN-CRS2 SECTION.                                                
190400     MOVE 000100 TO GOOD-SQLCODES                                         
190500                                                                          
190600     EXEC SQL                                                             
190700        DECLARE CRS2 CURSOR WITH HOLD FOR                                 
190800        SELECT '2  '                                                      
190900             , B.IDLEGSEL                                                 
191000             , D.BEFORMS                                                  
191100             , B.KDVALISO                                                 
191200             , B.IDLANDX3_SEND                                            
191300             , B.IDLEVNR                                                  
191400             , B.IDPARTNR                                                 
191500             , B.KDFINDOC                                                 
191600             , B.FLSOFT                                                   
191700             , B.FLFREE                                                   
191800             , B.IDBREAK_1                                                
191900             , B.IDBREAK_2                                                
192000             , A.DAFINDOC                                                 
192100             , A.IDFINDOC                                                 
192200             , B.IDLOPNR                                                  
192300             , B.IDLANDX3_REC                                             
192400             , B.IDEXCUST_1                                               
192500             , B.IDEXCUST_2                                               
192600             , B.IDEXCUST_3                                               
192700             , B.IDBUNDLE                                                 
192800             , B.BEVOLREF                                                 
192900             , B.IDREF                                                    
193000             , B.DAREFDAT                                                 
193100             , B.IDOPTION_1                                               
193200             , B.IDOPTION_2                                               
193300             , B.IDOPTION_3                                               
193400             , B.IDOPTION_4                                               
193500             , B.IDOPTION_5                                               
193600             , B.IDACCNT_1                                                
193700             , B.IDACCNT_2                                                
193800             , B.IDACCNT_3                                                
193900             , B.IDACCNT_4                                                
194000             , B.IDARTNR_FINANCE                                          
194100             , B.BEART                                                    
194200             , B.IDSTATNR                                                 
194300             , B.VKORDBTO_KOLLI                                           
194400             , B.VKARTNTO                                                 
194500             , B.KDARTURS                                                 
194600             , B.KVBEART                                                  
194700             , B.KVLEVART                                                 
194800             , B.PRARTBTO                                                 
194900             , B.PRARTNTO                                                 
195000             , B.REARTRAB                                                 
195100             , B.FLSPECPR                                                 
195200             , B.KDANMORS                                                 
195300             , B.IDFAKREF                                                 
195400             , B.DAFAKREF                                                 
195500             , B.IDDC                                                     
195600             , B.KDFRAKT                                                  
195700             , B.BELEVVIL                                                 
195800             , B.REVAT                                                    
195900             , B.SUNTO                                                    
196000             , B.SUVAT_BILLIT                                             
196100             , B.SUBTO                                                    
196200             , B.KDVAT                                                    
196300             , B.BEVAT                                                    
196400             , B.IDAPPEND                                                 
196500             , B.IDARTNR_CNTRL                                            
196600             , B.FLPCOO                                                   
196700             , B.IDLEVNR_ART                                              
196800             , B.IDTRACK_1                                                
196900             , B.KVANT_TRACK_1                                            
197000             , B.IDTRACK_2                                                
197100             , B.KVANT_TRACK_2                                            
197200             , B.IDTRACK_3                                                
197300             , B.KVANT_TRACK_3                                            
197400             , B.IDTRACK_4                                                
197500             , B.KVANT_TRACK_4                                            
197600             , B.IDTRACK_5                                                
197700             , B.KVANT_TRACK_5                                            
197800             , C.FLLOCCUR                                                 
197900             , C.FLRATE                                                   
198000             , C.IDLEGSEL                                                 
198100             , C.KDVALISO                                                 
198200             , C.FLFINFIL                                                 
198300             , C.FLCURRND                                                 
198400                                                                          
198500        FROM   T01DHEA A                                                  
198600             , T01DLIN B                                                  
198700             , T01FCUS C                                                  
198800             , T01BURE D                                                  
198900                                                                          
199000        WHERE  B.IDLEGSEL      = :PROC-IDLEGSEL                           
199100        AND    B.DAEXDAT       = :PROC-DAEXDAT                            
199200        AND    B.TIEXTID       = :PROC-TIEXTID                            
199300        AND    B.IDLEGSEL      = A.IDLEGSEL                               
199400        AND    B.DAEXDAT       = A.DAEXDAT                                
199500        AND    B.TIEXTID       = A.TIEXTID                                
199600        AND    B.KDVALISO      = A.KDVALISO                               
199700        AND    B.IDLANDX3_SEND = A.IDLANDX3_SEND                          
199800        AND    B.IDLEVNR       = A.IDLEVNR                                
199900        AND    B.IDPARTNR      = A.IDPARTNR                               
200000        AND    B.KDFINDOC      = A.KDFINDOC                               
200100        AND    B.FLSOFT        = A.FLSOFT                                 
200200        AND    B.FLFREE        = A.FLFREE                                 
200300        AND    B.FLPRIV        = A.FLPRIV                                 
200400        AND    B.IDBREAK_1     = A.IDBREAK_1                              
200500        AND    B.IDBREAK_2     = A.IDBREAK_2                              
200600        AND    C.IDLEGSEL      = A.IDLEGSEL                               
200700        AND    C.IDPARTNR      = A.IDPARTNR                               
200800        AND    C.KDSTATUS      = :WS-CURRENT-VERSION                      
200900        AND    C.DADELDAT      = :WS-ACTIVE                               
201000        AND    D.IDLEGSEL      = A.IDLEGSEL                               
201100        AND    D.KDFINDOC      = A.KDFINDOC                               
201200        AND    D.KDPARTTY      = C.KDPARTTY                               
201300        AND    D.KDPARTGR      = C.KDPARTGR                               
201400        AND    D.KDSTATUS      = :WS-CURRENT-VERSION                      
201500        AND    D.DADELDAT      = :WS-ACTIVE                               
201600                                                                          
201700        ORDER BY B.IDLEGSEL                                               
201800               , D.BEFORMS                                                
201900               , B.DAEXDAT                                                
202000               , B.TIEXTID                                                
202100               , B.KDVALISO                                               
202200               , B.IDLANDX3_SEND                                          
202300               , B.IDLEVNR                                                
202400               , B.IDPARTNR                                               
202500               , B.KDFINDOC                                               
202600               , B.FLSOFT                                                 
202700               , B.FLFREE                                                 
202800               , B.FLPRIV                                                 
202900               , B.IDBREAK_1                                              
203000               , B.IDBREAK_2                                              
203100               , A.DAFINDOC                                               
203200               , A.IDFINDOC                                               
203300               , B.IDLOPNR                                                
203400     END-EXEC                                                             
203500                                                                          
203600     EXEC SQL                                                             
203700        OPEN CRS2                                                         
203800     END-EXEC                                                             
203900                                                                          
204000     MOVE SQLCODE TO SQLCODE-WS                                           
204100     PERFORM DB2-STATUS-CHECK                                             
204200     .                                                                    
204300                                                                          
204400 DB2-DCL-OPN-CRS3 SECTION.                                                
204500     MOVE 000100 TO GOOD-SQLCODES                                         
204600                                                                          
204700     EXEC SQL                                                             
204800        DECLARE CRS3 CURSOR WITH HOLD FOR                                 
204900        SELECT '4  '                                                      
205000             , B.IDLEGSEL                                                 
205100             , D.BEFORMS                                                  
205200             , B.KDVALISO                                                 
205300             , B.IDLANDX3_SEND                                            
205400             , B.IDLEVNR                                                  
205500             , B.IDPARTNR                                                 
205600             , B.KDFINDOC                                                 
205700             , B.FLSOFT                                                   
205800             , B.FLFREE                                                   
205900             , B.IDBREAK_1                                                
206000             , B.IDBREAK_2                                                
206100             , A.DAFINDOC                                                 
206200             , A.IDFINDOC                                                 
206300             , :WS-IDLOPNR                                                
206400             , B.KDAPPEND                                                 
206500             , B.IDAPPEND                                                 
206600             , B.SUNTO_APP                                                
206700             , B.SUVAT_BILLIT_APP                                         
206800             , B.SUBTO_APP                                                
206900             , C.FLLOCCUR                                                 
207000             , C.FLRATE                                                   
207100             , C.IDLEGSEL                                                 
207200             , C.KDVALISO                                                 
207300                                                                          
207400        FROM   T01DHEA A                                                  
207500             , T01DAPP B                                                  
207600             , T01FCUS C                                                  
207700             , T01BURE D                                                  
207800                                                                          
207900        WHERE  B.IDLEGSEL      = :PROC-IDLEGSEL                           
208000        AND    B.DAEXDAT       = :PROC-DAEXDAT                            
208100        AND    B.TIEXTID       = :PROC-TIEXTID                            
208200        AND    B.IDLEGSEL      = A.IDLEGSEL                               
208300        AND    B.DAEXDAT       = A.DAEXDAT                                
208400        AND    B.TIEXTID       = A.TIEXTID                                
208500        AND    B.KDVALISO      = A.KDVALISO                               
208600        AND    B.IDLANDX3_SEND = A.IDLANDX3_SEND                          
208700        AND    B.IDLEVNR       = A.IDLEVNR                                
208800        AND    B.IDPARTNR      = A.IDPARTNR                               
208900        AND    B.KDFINDOC      = A.KDFINDOC                               
209000        AND    B.FLSOFT        = A.FLSOFT                                 
209100        AND    B.FLFREE        = A.FLFREE                                 
209200        AND    B.FLPRIV        = A.FLPRIV                                 
209300        AND    B.IDBREAK_1     = A.IDBREAK_1                              
209400        AND    B.IDBREAK_2     = A.IDBREAK_2                              
209500        AND    C.IDLEGSEL      = A.IDLEGSEL                               
209600        AND    C.IDPARTNR      = A.IDPARTNR                               
209700        AND    C.KDSTATUS      = :WS-CURRENT-VERSION                      
209800        AND    C.DADELDAT      = :WS-ACTIVE                               
209900        AND    D.IDLEGSEL      = A.IDLEGSEL                               
210000        AND    D.KDFINDOC      = A.KDFINDOC                               
210100        AND    D.KDPARTTY      = C.KDPARTTY                               
210200        AND    D.KDPARTGR      = C.KDPARTGR                               
210300        AND    D.KDSTATUS      = :WS-CURRENT-VERSION                      
210400        AND    D.DADELDAT      = :WS-ACTIVE                               
210500                                                                          
210600        ORDER BY B.IDLEGSEL                                               
210700               , D.BEFORMS                                                
210800               , B.DAEXDAT                                                
210900               , B.TIEXTID                                                
211000               , B.KDVALISO                                               
211100               , B.IDLANDX3_SEND                                          
211200               , B.IDLEVNR                                                
211300               , B.IDPARTNR                                               
211400               , B.KDFINDOC                                               
211500               , B.FLSOFT                                                 
211600               , B.FLFREE                                                 
211700               , B.FLPRIV                                                 
211800               , B.IDBREAK_1                                              
211900               , B.IDBREAK_2                                              
212000               , A.DAFINDOC                                               
212100               , A.IDFINDOC                                               
212200               , B.IDAPPEND                                               
212300     END-EXEC                                                             
212400                                                                          
212500     EXEC SQL                                                             
212600        OPEN CRS3                                                         
212700     END-EXEC                                                             
212800                                                                          
212900     MOVE SQLCODE TO SQLCODE-WS                                           
213000     PERFORM DB2-STATUS-CHECK                                             
213100     .                                                                    
213200                                                                          
213300 DB2-FETCH-CRS1 SECTION.                                                  
213400     MOVE 000100         TO GOOD-SQLCODES                                 
213500                                                                          
213600     EXEC SQL                                                             
213700       FETCH CRS1                                                         
213800       INTO  :HEAD-IDPTYP                                                 
213900           , :HEAD-IDLEGSEL                                               
214000           , :HEAD-BEFORMS                                                
214100           , :HEAD-KDVALISO                                               
214200           , :HEAD-IDLANDX3-SEND                                          
214300           , :HEAD-IDLEVNR                                                
214400           , :HEAD-IDPARTNR                                               
214500           , :HEAD-KDFINDOC                                               
214600           , :HEAD-FLSOFT                                                 
214700           , :HEAD-FLFREE                                                 
214800           , :WS-FLPRIV                                                   
214900           , :HEAD-IDBREAK-1                                              
215000           , :HEAD-IDBREAK-2                                              
215100           , :WS-DAFINDOC                                                 
215200           , :HEAD-IDFINDOC                                               
215300           , :HEAD-IDLOPNR                                                
215400           , :HEAD-IDSPRAK                                                
215500           , :HEAD-BEBETVIL                                               
215600           , :HEAD-BELEGRAD-1                                             
215700           , :HEAD-BELEGRAD-2                                             
215800           , :HEAD-ADLEG-STREET                                           
215900           , :HEAD-ADLEG-BOX                                              
216000           , :HEAD-ADLEG-CITY                                             
216100           , :HEAD-ADLEG-PCODE                                            
216200           , :HEAD-IDLANDX3-LEG                                           
216300           , :HEAD-IDTFN-LEG                                              
216400           , :HEAD-IDTFX-LEG                                              
216500           , :HEAD-IDMAIL-LEG                                             
216600           , :HEAD-BECONT-LEG                                             
216700           , :HEAD-IDVAT-LEG                                              
216800           , :HEAD-IDBG-LEG                                               
216900           , :HEAD-IDPG-LEG                                               
217000           , :HEAD-BERESPRA-1                                             
217100           , :HEAD-BERESPRA-2                                             
217200           , :HEAD-ADRESP-STREET                                          
217300           , :HEAD-ADRESP-BOX                                             
217400           , :HEAD-ADRESP-CITY                                            
217500           , :HEAD-ADRESP-PCODE                                           
217600           , :HEAD-IDLANDX3-RESP                                          
217700           , :HEAD-IDTFN-RESP                                             
217800           , :HEAD-IDTFX-RESP                                             
217900           , :HEAD-IDMAIL-RESP                                            
218000           , :HEAD-BECONT-RESP                                            
218100           , :HEAD-IDVAT-RESP                                             
218200           , :HEAD-IDBG-RESP                                              
218300           , :HEAD-IDPG-RESP                                              
218400           , :HEAD-BEBET-NAME1                                            
218500           , :HEAD-BEBET-NAME2                                            
218600           , :HEAD-ADBET-STREET                                           
218700           , :HEAD-ADBET-BOX                                              
218800           , :HEAD-ADBET-CITY                                             
218900           , :HEAD-ADBET-PCODE                                            
219000           , :HEAD-IDLANDX3-BET                                           
219100           , :HEAD-IDVAT-BET                                              
219200           , :HEAD-SUNTO-SERV                                             
219300           , :HEAD-SUBTO-SERV                                             
219400           , :HEAD-SUNTO-PART                                             
219500           , :HEAD-SUBTO-PART                                             
219600           , :HEAD-SUNTO-TOT                                              
219700           , :HEAD-SUBTO-TOT                                              
219800           , :HEAD-SUVAT-BILLIT-TOT                                       
219900           , :HEAD-PRKURS                                                 
220000           , :HEAD-BEANST                                                 
220100           , :HEAD-IDUSER                                                 
220200           , :HEAD-BETEXT-1                                               
220300           , :HEAD-BETEXT-2                                               
220400           , :HEAD-BETEXT-3                                               
220500           , :HEAD-BETEXT-4                                               
220600           , :HEAD-BETEXT                                                 
220700           , :HEAD-BETEXT-CRE                                             
220800           , :HEAD-KDVALISO-LOC                                           
220900           , :CUST-FLRATE                                                 
221000           , :CUST-FLLOCCUR                                               
221100           , :HEAD-SUDOCLIM                                               
221200           , :HEAD-IDVAT-AGENT                                            
221300           , :WS-KDVALISO                                                 
221400           , :WS-KDFINDOC                                                 
221500           , :WS-KDPARTTY                                                 
221600           , :HEAD-KDPARTGR                                               
221700           , :CUST-FLFINFIL                                               
221800           , :CUST-FLCURINF                                               
221900           , :CUST-FLCURRND                                               
222000           , :WS-FLDIRVAT                                                 
222100           , :WS-RECO-IDVAT                                               
222200           , :HEAD-IDSYSTEM-SEND                                          
222300           , :CUST-FLDECIMAL                                              
222400     END-EXEC                                                             
222500                                                                          
222600     MOVE SQLCODE TO SQLCODE-WS                                           
222700     PERFORM DB2-STATUS-CHECK                                             
222800     .                                                                    
222900                                                                          
223000 DB2-FETCH-CRS2 SECTION.                                                  
223100     MOVE 000100         TO GOOD-SQLCODES                                 
223200                                                                          
223300     EXEC SQL                                                             
223400       FETCH CRS2                                                         
223500       INTO  :LINE-IDPTYP                                                 
223600           , :LINE-IDLEGSEL                                               
223700           , :LINE-BEFORMS                                                
223800           , :LINE-KDVALISO                                               
223900           , :LINE-IDLANDX3-SEND                                          
224000           , :LINE-IDLEVNR                                                
224100           , :LINE-IDPARTNR                                               
224200           , :LINE-KDFINDOC                                               
224300           , :LINE-FLSOFT                                                 
224400           , :LINE-FLFREE                                                 
224500           , :LINE-IDBREAK-1                                              
224600           , :LINE-IDBREAK-2                                              
224700           , :WS-DAFINDOC                                                 
224800           , :LINE-IDFINDOC                                               
224900           , :LINE-IDLOPNR                                                
225000           , :LINE-IDLANDX3-REC                                           
225100           , :WS-IDEXCUST-1                                               
225200           , :WS-IDEXCUST-2                                               
225300           , :WS-IDEXCUST-3                                               
225400           , :LINE-IDBUNDLE                                               
225500           , :LINE-BEVOLREF                                               
225600           , :LINE-IDREF                                                  
225700           , :WS-DAREFDAT                                                 
225800           , :WS-IDOPTION-1                                               
225900           , :WS-IDOPTION-2                                               
226000           , :WS-IDOPTION-3                                               
226100           , :WS-IDOPTION-4                                               
226200           , :WS-IDOPTION-5                                               
226300           , :WS-IDACCNT-1                                                
226400           , :WS-IDACCNT-2                                                
226500           , :WS-IDACCNT-3                                                
226600           , :WS-IDACCNT-4                                                
226700           , :LINE-IDARTNR-FINANCE                                        
226800           , :LINE-BEART                                                  
226900           , :LINE-IDSTATNR                                               
227000           , :LINE-VKORDBTO-KOLLI                                         
227100           , :LINE-VKARTNTO                                               
227200           , :LINE-KDARTURS                                               
227300           , :LINE-KVBEART                                                
227400           , :LINE-KVLEVART                                               
227500           , :LINE-PRARTBTO                                               
227600           , :LINE-PRARTNTO                                               
227700           , :LINE-REARTRAB                                               
227800           , :LINE-FLSPECPR                                               
227900           , :LINE-KDANMORS                                               
228000           , :LINE-IDFAKREF                                               
228100           , :WS-DAFAKREF                                                 
228200           , :LINE-IDDC                                                   
228300           , :LINE-KDFRAKT                                                
228400           , :LINE-BELEVVIL                                               
228500           , :LINE-REVAT                                                  
228600           , :LINE-SUNTO                                                  
228700           , :LINE-SUVAT-BILLIT                                           
228800           , :LINE-SUBTO                                                  
228900           , :LINE-KDVAT                                                  
229000           , :LINE-BEVAT                                                  
229100           , :LINE-IDAPPEND                                               
229200           , :LINE-IDARTNR-CNTRL                                          
229300           , :LINE-FLPCOO                                                 
229400           , :LINE-IDLEVNR-ART                                            
229500           , :LINE-IDTRACK-1                                              
229600           , :LINE-KVANT-TRACK-1                                          
229700           , :LINE-IDTRACK-2                                              
229800           , :LINE-KVANT-TRACK-2                                          
229900           , :LINE-IDTRACK-3                                              
230000           , :LINE-KVANT-TRACK-3                                          
230100           , :LINE-IDTRACK-4                                              
230200           , :LINE-KVANT-TRACK-4                                          
230300           , :LINE-IDTRACK-5                                              
230400           , :LINE-KVANT-TRACK-5                                          
230500           , :CUST-FLLOCCUR                                               
230600           , :CUST-FLRATE                                                 
230700           , :CUST-IDLEGSEL                                               
230800           , :CUST-KDVALISO                                               
230900           , :CUST-FLFINFIL                                               
231000           , :CUST-FLCURRND                                               
231100     END-EXEC                                                             
231200                                                                          
231300     MOVE SQLCODE TO SQLCODE-WS                                           
231400     PERFORM DB2-STATUS-CHECK                                             
231500     .                                                                    
231600                                                                          
231700 DB2-FETCH-CRS3 SECTION.                                                  
231800     MOVE 000100         TO GOOD-SQLCODES                                 
231900                                                                          
232000     EXEC SQL                                                             
232100       FETCH CRS3                                                         
232200       INTO  :APPX-IDPTYP                                                 
232300           , :APPX-IDLEGSEL                                               
232400           , :APPX-BEFORMS                                                
232500           , :APPX-KDVALISO                                               
232600           , :APPX-IDLANDX3-SEND                                          
232700           , :APPX-IDLEVNR                                                
232800           , :APPX-IDPARTNR                                               
232900           , :APPX-KDFINDOC                                               
233000           , :APPX-FLSOFT                                                 
233100           , :APPX-FLFREE                                                 
233200           , :APPX-IDBREAK-1                                              
233300           , :APPX-IDBREAK-2                                              
233400           , :WS-DAFINDOC                                                 
233500           , :APPX-IDFINDOC                                               
233600           , :APPX-IDLOPNR                                                
233700           , :APPX-KDAPPEND                                               
233800           , :APPX-IDAPPEND                                               
233900           , :APPX-SUNTO-APP                                              
234000           , :APPX-SUVAT-BILLIT-APP                                       
234100           , :APPX-SUBTO-APP                                              
234200           , :CUST-FLLOCCUR                                               
234300           , :CUST-FLRATE                                                 
234400           , :CUST-IDLEGSEL                                               
234500           , :CUST-KDVALISO                                               
234600     END-EXEC                                                             
234700                                                                          
234800     MOVE SQLCODE TO SQLCODE-WS                                           
234900     PERFORM DB2-STATUS-CHECK                                             
235000     .                                                                    
235100                                                                          
235200 DB2-CLOSE-CRS1 SECTION.                                                  
235300     EXEC SQL                                                             
235400        CLOSE CRS1                                                        
235500     END-EXEC                                                             
235600     .                                                                    
235700                                                                          
235800 DB2-CLOSE-CRS2 SECTION.                                                  
235900     EXEC SQL                                                             
236000        CLOSE CRS2                                                        
236100     END-EXEC                                                             
236200     .                                                                    
236300                                                                          
236400 DB2-CLOSE-CRS3 SECTION.                                                  
236500     EXEC SQL                                                             
236600        CLOSE CRS3                                                        
236700     END-EXEC                                                             
236800     .                                                                    
236900                                                                          
237000 DB2-OPEN-CRS-LSEL SECTION.                                               
237100     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
237200     SELECT   T01LSEL.IDLEGSEL                                            
237300             ,T01LSEL.KDTRADP                                             
237400                                                                          
237500     FROM     T01LSEL                                                     
237600                                                                          
237700     WHERE    KDSTATUS = 1                                                
237800     END-EXEC                                                             
237900                                                                          
238000     EXEC SQL OPEN T01LSEL-CRS                                            
238100     END-EXEC                                                             
238200                                                                          
238300     MOVE 000            TO GOOD-SQLCODES                                 
238400     MOVE SQLCODE        TO SQLCODE-WS                                    
238500     PERFORM DB2-STATUS-CHECK                                             
238600     .                                                                    
238700     EJECT                                                                
238800                                                                          
238900 DB2-FETCH-CRS-LSEL SECTION.                                              
239000     EXEC SQL FETCH T01LSEL-CRS INTO                                      
239100            :WS-IDLEGSEL-CRS                                              
239200           ,:WS-KDTRADP                                                   
239300     END-EXEC                                                             
239400                                                                          
239500     MOVE 000100         TO GOOD-SQLCODES                                 
239600     MOVE SQLCODE        TO SQLCODE-WS                                    
239700     PERFORM DB2-STATUS-CHECK                                             
239800     .                                                                    
239900     EJECT                                                                
240000                                                                          
240100 DB2-CLOSE-CRS-LSEL SECTION.                                              
240200     EXEC SQL CLOSE T01LSEL-CRS                                           
240300     END-EXEC                                                             
240400     .                                                                    
240500     EJECT                                                                
240600                                                                          
240700 DB2-SELECT-T01LSEL SECTION.                                              
240800     MOVE 000100    TO GOOD-SQLCODES                                      
240900     EXEC SQL                                                             
241000       SELECT IDLEGSEL                                                    
241100                                                                          
241200       INTO :WS-IDLEGSEL                                                  
241300                                                                          
241400       FROM T01LSEL                                                       
241500                                                                          
241600       WHERE IDLEGSEL = :WS-IDLEGSEL                                      
241700       AND     KDSTATUS = 001                                             
241800     END-EXEC                                                             
241900     MOVE SQLCODE        TO SQLCODE-WS                                    
242000     PERFORM DB2-STATUS-CHECK                                             
242100     .                                                                    
242200     EJECT                                                                
242300 DB2-STATUS-CHECK  SECTION.                                               
242400     SET SQLCODE-IX TO 1                                                  
242500     SEARCH GOOD-SQLCODE                                                  
242600       AT END                                                             
242700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
242800          DELIMITED BY SIZE INTO ERROR-TEXT                               
242900          CALL ABEND USING RKOD-ABEND-DB2                                 
243000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
243100          CONTINUE                                                        
243200     END-SEARCH                                                           
243300     .                                                                    
