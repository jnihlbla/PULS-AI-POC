000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4281600.                                                
000400 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000500 DATE-WRITTEN.   08/08/19.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SUMMERAR ANTAL INLAGDA, SKROTADE OCH AVVIKELSE        
001100*        RAPPORTERADE RADER FÖR LDC RETURER KOD 72 PER RETUR-DC.          
001200*        PROGRAMMET INGÅR I RUTIN W428V1.VECKANS RADER.                   
001300*        LÄSER IN W42811-FIL FRÅN VECKORUTIN W428V1 OCH                   
001400*        SKICKAR RADER TILL DISTR. OCH PRINT VIA WZ01.                    
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDB6                                       
001700*                                                                         
001710*                                                                         
001800* 2008-06-18  E'TRACKER. 6785206  DATED 2008-06-18                        
001900* 2011-12-07  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                
001910*                                                                         
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- VECKANS INLAGDA RADER KOD 72 HOS LDC                       
003000     SELECT W42811                     ASSIGN TO W42816D1.                
003100     EJECT                                                                
003200*          --- UTFIL SUMMERADE RADER MED LANDSPREFIX                      
003300     SELECT W42816                     ASSIGN TO W42816D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W42811                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W42811      -L.                                                
004400     SKIP2                                                                
004500 FD  W42816                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  POST -COPY W42816 -PRE  UT-   -L.                                    
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W4281600'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  SPAR-IDDISTR                PIC 9(4)    VALUE ZERO.                  
005700 77  SPAR-IDKUNDNR               PIC 9(6)    VALUE ZERO.                  
005800 77  SPAR-IDRAPPNR               PIC 9(7)    VALUE ZERO.                  
005900 77  SPAR-IDDC-RET               PIC X(2)    VALUE SPACE.                 
005910 77  SPAR-IDFTG                  PIC 9(2)    VALUE ZERO.                  
006000 77  SPAR-DOC-TIAAVV             PIC 9(4)    VALUE ZERO.                  
006100 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
006200 77  W-KVRETINL-TOT              PIC 9(7)    VALUE ZERO.                  
006300 77  W-KVRETINL-SKR-TOT          PIC 9(7)    VALUE ZERO.                  
006400 77  W-KVAVV-KVANT-TOT           PIC 9(7)    VALUE ZERO.                  
006500 77  W-KVDAGAR-RET-TOT           PIC 9(7)    VALUE ZERO.                  
006600 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
006700 77  W-KVRETINL-SUM              PIC 9(7)    VALUE ZERO.                  
006800                                                                          
006900 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
007000 77  KDRC-DISPLAY                PIC Z(5).                                
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500                                                                          
007600 77  W42811-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W42811                       VALUE 'J'.                   
007800     EJECT                                                                
007900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008000 01  FILLER REDEFINES DAGENS-DATUM.                                       
008100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008400     EJECT                                                                
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008600*                                                                         
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600*    --- PARAMETERS TO ABEND                                              
009700                                                                          
009800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010100                                                                          
010200     EJECT                                                                
010300*                                                                         
010400 01  IN-AREA-START               PIC X(24)   VALUE                        
010500                                             'IN-AREA-START'.             
010600     SKIP2                                                                
010700                                                                          
010800*01  AREA -COPY W42811     -PRE IN-                                       
010900*                                                                         
011000 01  UT-AREA-START               PIC X(24)   VALUE                        
011100                                             'UT-AREA-START'.             
011200     SKIP2                                                                
011300                                                                          
011400*01  AREA -COPY W42816     -PRE UT-                                       
011500*                                                                         
011600     EJECT                                                                
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  NYCKLAR-TILL-DLI.                                                    
012100     03  W-IDDC-X.                                                        
012200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012300     SKIP2                                                                
012400*    --- STATUS-KOD FRÅN IMS                                              
012500 01  STATUS-WS                   PIC XX.                                  
012600     88  SEGMENT-FINNS                       VALUE '  '.                  
012700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013000     88  IMS-EJ-OK                           VALUE 'XD'.                  
013100     SKIP2                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200                                                                          
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014400 01  DLI-IO-WDB601.                                                       
014500*    03  -COPY WDB601                                                     
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800                                                                          
014900*01  -COPY W0009   -PRE MSG-                                              
015000     EJECT                                                                
015100 01  DISTRDOC-PCB                PIC X.                                   
015200     EJECT                                                                
015300                                                                          
015400*01  -COPY W0008  -PRE WDB6-                                              
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700 PROCEDURE DIVISION  USING MSG-PCB WDB6-PCB.                              
015800 MAIN SECTION.                                                            
015900     ENTRY 'DLITCBL' USING MSG-PCB WDB6-PCB.                              
016000                                                                          
016100     SKIP2                                                                
016200     PERFORM A-INIT                                                       
016300                                                                          
016400     PERFORM S01-LAES-W42811                                              
016500                                                                          
016600     PERFORM UNTIL END-OF-W42811                                          
016700       MOVE IN-IDDC-RET   TO SPAR-IDDC-RET                                
016800                             W-IDDC                                       
016810       MOVE IN-IDFTG      TO SPAR-IDFTG                                   
016820                                                                          
016900       MOVE IN-TISAAVV-INLINL-TIAAVV TO SPAR-DOC-TIAAVV                   
017000       MOVE ZERO          TO W-KVRETINL-TOT                               
017100                             W-KVRETINL-SKR-TOT                           
017200                             W-KVAVV-KVANT-TOT                            
017300                             W-KVDAGAR-RET-TOT                            
017400                             W-KVRETINL-SUM                               
017500                                                                          
017600       PERFORM UNTIL END-OF-W42811 OR                                     
017700            IN-IDDC-RET NOT = SPAR-IDDC-RET                               
017800                                                                          
017900         PERFORM B-BEHANDLA                                               
018000                                                                          
018100       END-PERFORM                                                        
018200                                                                          
018300       PERFORM C-FLYTTA-DATA                                              
018400                                                                          
018500     END-PERFORM                                                          
018600                                                                          
018700     PERFORM Z-FINIT                                                      
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT SECTION.                                                          
019400     SKIP2                                                                
019500                                                                          
019600     OPEN INPUT  W42811                                                   
019700                                                                          
019800     OPEN OUTPUT W42816                                                   
019900                                                                          
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100     .                                                                    
020200     EJECT                                                                
020300 B-BEHANDLA  SECTION.                                                     
020400                                                                          
020500     MOVE IN-IDDISTR    TO SPAR-IDDISTR                                   
020600     MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR                                  
020700     MOVE IN-IDRAPPNR   TO SPAR-IDRAPPNR                                  
020800                                                                          
020900     COMPUTE W-KVDAGAR-RET-TOT = W-KVDAGAR-RET-TOT +                      
021000                                 IN-KVDAGAR-RET                           
021100     END-COMPUTE                                                          
021200                                                                          
021300     PERFORM UNTIL END-OF-W42811 OR                                       
021400           IN-IDDISTR  NOT = SPAR-IDDISTR  OR                             
021500           IN-IDKUNDNR NOT = SPAR-IDKUNDNR OR                             
021600           IN-IDRAPPNR NOT = SPAR-IDRAPPNR                                
021700                                                                          
021800       IF IN-KVRETINL > 0                                                 
021900         ADD +1 TO W-KVRETINL-TOT                                         
022000       END-IF                                                             
022100                                                                          
022200       IF IN-KVRETINL-SKR > 0                                             
022300         ADD +1  TO W-KVRETINL-SKR-TOT                                    
022400       END-IF                                                             
022500                                                                          
022600       IF IN-KVAVV-KVANT > 0                                              
022700         ADD +1  TO W-KVAVV-KVANT-TOT                                     
022800       END-IF                                                             
022900                                                                          
023000       PERFORM S01-LAES-W42811                                            
023100     END-PERFORM                                                          
023200     .                                                                    
023300     EJECT                                                                
023400 C-FLYTTA-DATA  SECTION.                                                  
023500                                                                          
023600     MOVE SPAR-IDDC-RET       TO UT-IDDC-RET                              
023610     MOVE SPAR-IDFTG          TO UT-IDFTG                                 
023700     MOVE SPAR-DOC-TIAAVV     TO UT-TIAAVV                                
023800                                                                          
023900     IF SPAR-IDDC-RET NOT = DCS-IDDC                                      
024000       MOVE SPAR-IDDC-RET TO W-IDDC                                       
024100     END-IF                                                               
024200     PERFORM IMS-GET-WDB601                                               
024300                                                                          
024400     IF SEGMENT-FINNS                                                     
024500       MOVE DCS-ADGMT-PADR(11:20)  TO UT-ADCITY                           
024700       MOVE DCS-IDLANDX2           TO UT-IDLANDX2                         
024800                                      SPAR-IDLANDX2                       
024900     ELSE                                                                 
025000       MOVE SPACE                  TO UT-ADCITY                           
025100       MOVE SPACE                  TO UT-IDLANDX2                         
025200     END-IF                                                               
025300                                                                          
025400     MOVE W-KVRETINL-TOT      TO UT-KVRETINL                              
025500     MOVE W-KVRETINL-SKR-TOT  TO UT-KVRETINL-SKR                          
025600     MOVE W-KVAVV-KVANT-TOT   TO UT-KVAVV-KVANT                           
025700                                                                          
025800     COMPUTE W-KVRETINL-SUM = W-KVRETINL-TOT + W-KVRETINL-SKR-TOT         
025900     END-COMPUTE                                                          
026000                                                                          
026010     IF W-KVRETINL-SUM > ZERO                                             
026100       COMPUTE UT-KVDAGDEC ROUNDED =                                      
026200               W-KVDAGAR-RET-TOT / W-KVRETINL-SUM                         
026300       END-COMPUTE                                                        
026310     ELSE                                                                 
026320       MOVE W-KVDAGAR-RET-TOT TO UT-KVDAGDEC                              
026330     END-IF                                                               
026400                                                                          
026500     IF DCS-IDLANDX2 =  SPAR-IDLANDX2                                     
026600       PERFORM S10-SKRIV-UTFIL                                            
026700     ELSE                                                                 
026800       MOVE SPACE TO UT-AREA                                              
026900     END-IF                                                               
027000                                                                          
027100     .                                                                    
027200     EJECT                                                                
027300 Z-FINIT SECTION.                                                         
027400                                                                          
027500                                                                          
027600     CLOSE W42811                                                         
027700           W42816                                                         
027800     SKIP2                                                                
027900     MOVE 'S' TO POSTSUM-OPKOD                                            
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     EJECT                                                                
028300 S01-LAES-W42811  SECTION.                                                
028400     SKIP2                                                                
028500     READ W42811 INTO IN-AREA                                             
028600     AT END                                                               
028700        MOVE HIGH-VALUE TO IN-W42811                                      
028800        SET END-OF-W42811 TO TRUE                                         
028900                                                                          
029000     NOT AT END                                                           
029100        MOVE 'W42811'   TO POSTSUM-FDNAMN                                 
029200        MOVE 'W42816D1' TO POSTSUM-DDNAMN2                                
029300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029400        CALL POSTSUM USING POSTSUM-PARM                                   
029500     END-READ                                                             
029600     .                                                                    
029700     EJECT                                                                
029800 S10-SKRIV-UTFIL SECTION.                                                 
029900     SKIP2                                                                
030000     WRITE UT-POST FROM UT-AREA                                           
030100     MOVE 'W42816'      TO POSTSUM-FDNAMN                                 
030200     MOVE 'W42816D2'    TO POSTSUM-DDNAMN2                                
030300     MOVE 'UTPOST'      TO POSTSUM-TRANSTYP                               
030400     CALL POSTSUM      USING POSTSUM-PARM                                 
030500     .                                                                    
030600     EJECT                                                                
030700* --- IMS SEKTIONER ---                                                   
030800                                                                          
030900     EJECT                                                                
031000 IMS-GET-WDB601 SECTION.                                                  
031100                                                                          
031200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
031300          DELIMITED BY SIZE INTO SSA1                                     
031400     MOVE '  GE' TO GODK-STATUSKODER                                      
031500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
031600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-STATUSKONTROLL SECTION.                                              
032100     SKIP2                                                                
032200     SET STATUS-IX TO 1                                                   
032300     SEARCH GODK-STATUS                                                   
032400       AT END                                                             
032500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032600           DELIMITED BY SIZE INTO FELTEXT                                 
032700         DISPLAY FELTEXT                                                  
032800         CALL FELLOG                                                      
032900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033000         CONTINUE                                                         
033100     END-SEARCH                                                           
033200     .                                                                    
