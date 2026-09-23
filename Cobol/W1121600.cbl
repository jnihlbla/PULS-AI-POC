001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W1121600.                                                
001400*AUTHOR.         BODIL LINDAHL.                                           
001500*DATE-WRITTEN.   92/05/06.                                                
001600                                                                          
001700*    REMARKS.                                                             
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        STÄDNING AV 1002-SATSER.                                         
002100*                                                                         
002201*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
002202*                              WLARTC (WDK6)                              
002210*                              WLXXBY (WDG3)                              
002300*                                                                         
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401*    -COPY WY2000W1                                                       
004410     SKIP3                                                                
004500 77  IDPGM                       PIC X(8)    VALUE 'W1121600'.            
004600 01  CHKP-VAR.                                                            
004700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005200 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005401 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005402 77  WS-TISTADAT                 PIC 9(6)    VALUE ZERO.                  
005403 77  WS-TISTODAT                 PIC 9(6)    VALUE ZERO.                  
005410 77  WS-IDARTNR-SATS             PIC S9(9)   VALUE ZERO COMP-3.           
005420 77  WS-FLIART                   PIC X       VALUE SPACE.                 
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300     EJECT                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900*                                                                         
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  NYCKLAR-TILL-DLI.                                                    
008401     03  W-IDARTNR-X.                                                     
008402         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
008403     03  W-IDLEVNR-X.                                                     
008404         05  W-IDLEVNR           PIC X(5)    VALUE '1002'.                
008405     03  W-WDJ1CSEQ-X.                                                    
008406         05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                 
008407         05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                 
008408         05  W-IDARTNR-S         PIC S9(9)   VALUE ZERO  COMP-3.          
008409     03  W-WDG3KEY-X.                                                     
008410         05  W-IDHTYP            PIC X(4)    VALUE '2233'.                
008420         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  BASEN-SLUT                          VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(96).                               
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010400*    ---  DLI INPUT-OUTPUT AREA                                           
010500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010600                                                                          
010700 01  DLI-IO-AREA.                                                         
010800     03  IO-AREA                 PIC X(400)  VALUE SPACE.                 
010901                                                                          
010902     03  WLARTC01 REDEFINES IO-AREA.                                      
010903*        05  -COPY WDK601                                                 
010904     EJECT                                                                
010905     03  WLSATB01 REDEFINES IO-AREA.                                      
010906*        05  -COPY WDJ101     -PRE SATB-                                  
010907     EJECT                                                                
010908     03  WLSATB11 REDEFINES IO-AREA.                                      
010909*        05  -COPY WDJ111     -PRE SATB-                                  
010910     EJECT                                                                
010911     03  WDJ1CSEQ REDEFINES IO-AREA.                                      
010912*        05  -COPY WDJ111     -PRE SATE-                                  
010913*        05  -COPY WDJ101     -PRE SATE-                                  
010914     EJECT                                                                
010916 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
010917                                                                          
010918 01  DLI-IO-AREA-2.                                                       
010919     03  IO-AREA-2               PIC X(400)  VALUE SPACE.                 
010920     03  WLXXBY11 REDEFINES IO-AREA-2.                                    
010930*        05  -COPY WDGX2234   -PRE XXBY-                                  
011800     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012000                                                                          
012100*01  -COPY W0009   -PRE MSG-                                              
012201     EJECT                                                                
012202*01  -COPY W0008  -PRE ARTC-                                              
012203     05  FILLER                  PIC X.                                   
012204     EJECT                                                                
012205*01  -COPY W0008  -PRE SATB-                                              
012206     05  FILLER                  PIC X.                                   
012207     EJECT                                                                
012208*01  -COPY W0008  -PRE SATB2-                                             
012209     05  FILLER                  PIC X.                                   
012210     EJECT                                                                
012211*01  -COPY W0008  -PRE SATE-                                              
012212     05  FILLER                  PIC X.                                   
012213     EJECT                                                                
012214*01  -COPY W0008  -PRE XXBY-                                              
012220     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012601 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB SATB-PCB SATB2-PCB            
012602                           SATE-PCB XXBY-PCB.                             
012610     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB SATB-PCB SATB2-PCB            
012700                           SATE-PCB XXBY-PCB.                             
012900     SKIP2                                                                
013000     PERFORM A-INIT                                                       
013100                                                                          
013110     PERFORM IMS-GET-SATB01                                               
013120                                                                          
013200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
013210        IF SATB-STR-IDARTNR < 100000000                                   
013220           IF SATB-STR-TIBORT = ZERO                                      
013221              MOVE SATB-STR-IDARTNR TO WS-IDARTNR-SATS                    
013230              PERFORM B-STAEDA-SATS-STRUKTUR                              
013240           END-IF                                                         
013250        END-IF                                                            
013260                                                                          
013300        IF CHKP-ANT > CHKP-MAX                                            
013400           PERFORM X-TAG-CHECKPOINT                                       
013500        END-IF                                                            
013510                                                                          
013520        PERFORM IMS-GET-SATB01                                            
013530     END-PERFORM                                                          
013600                                                                          
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 A-INIT SECTION.                                                          
015400                                                                          
015500     PERFORM IMS-RESTART                                                  
015600     MOVE ZERO TO CHKP-ANT                                                
016000                                                                          
016100     ACCEPT DAGENS-DATUM FROM DATE                                        
016800     .                                                                    
017000     EJECT                                                                
017100 B-STAEDA-SATS-STRUKTUR SECTION.                                          
017200*****************************************************************         
017300* ALLA ARTIKLAR I SATSEN LÄSES. DATUM KONTROLLERAS PÅ ARTIKLAR  *         
017400* MED KDISATS N (NYTILLKOMNA) OCH U (UTGÅENDE).                 *         
017500* VID STÄDNING  SKICKAS 2234-TRANS FÖR PB-BERÄKNING             *         
017600*               FLIART (ARTC01) KONTROLLERAS                    *         
017700*               TIUPPDAT (WDJ101) UPPDATERAS                    *         
017800*****************************************************************         
017900                                                                          
018000     PERFORM IMS-GET-SATB11                                               
018010                                                                          
018100     PERFORM UNTIL SEGMENT-SAKNAS                                         
018101        IF SATB-RAD-KDISATS = 'U'                                         
018110           MOVE SATB-RAD-TISTODAT TO WS-TISTODAT                          
018111           MOVE DAGENS-DATUM   TO TMP1-YYMMDD                             
018112           MOVE WS-TISTODAT    TO TMP2-YYMMDD                             
018120           PERFORM WY2000P1                                               
018200           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
018300              MOVE SPACE TO SATB-RAD-KDISATS                              
018400              MOVE SATB-RAD-IDARTNR TO W-IDARTNR                          
018410              PERFORM IMS-REPL-SATB                                       
018411              ADD +1 TO CHKP-ANT                                          
018420                                                                          
018430              MOVE WS-IDARTNR-SATS  TO XXBY-2234-IDARTNR-SATS             
018440              MOVE SATB-RAD-IDARTNR TO XXBY-2234-IDARTNR-ING              
018450              MOVE 'U'              TO XXBY-2234-KDISATS                  
018460              MOVE ZERO             TO XXBY-2234-KVPB-SEP-TOT             
018470                                       XXBY-2234-REANTPSA-GAMMAL          
018471                                       XXBY-2234-REANTPSA-NY              
018480                                       XXBY-2234-TIBEHDAT                 
018490              PERFORM IMS-ISRT-XXBY11                                     
018491              ADD +1 TO CHKP-ANT                                          
018492              PERFORM BA-KONTROLLERA-FLIART                               
018493              PERFORM BB-UPPDATERA-TIUPPDAT                               
018494           END-IF                                                         
018495        ELSE                                                              
018496           IF SATB-RAD-KDISATS = 'N'                                      
018497              MOVE SATB-RAD-TISTADAT TO WS-TISTADAT                       
018498              MOVE DAGENS-DATUM   TO TMP1-YYMMDD                          
018499              MOVE WS-TISTADAT    TO TMP2-YYMMDD                          
018500              PERFORM WY2000P1                                            
018501              IF TMP1-YYMMDD >= TMP2-YYMMDD                               
018502                 MOVE SPACE TO SATB-RAD-KDISATS                           
018503                 MOVE SATB-RAD-IDARTNR TO W-IDARTNR                       
018504                 PERFORM IMS-REPL-SATB                                    
018505                 ADD +1 TO CHKP-ANT                                       
018506                                                                          
018507                 MOVE WS-IDARTNR-SATS  TO XXBY-2234-IDARTNR-SATS          
018508                 MOVE SATB-RAD-IDARTNR TO XXBY-2234-IDARTNR-ING           
018509                 MOVE 'N'              TO XXBY-2234-KDISATS               
018510                 MOVE ZERO             TO XXBY-2234-KVPB-SEP-TOT          
018511                                          XXBY-2234-REANTPSA-NY           
018512                                          XXBY-2234-TIBEHDAT              
018513                                      XXBY-2234-REANTPSA-GAMMAL           
018514                 PERFORM IMS-ISRT-XXBY11                                  
018515                 ADD +1 TO CHKP-ANT                                       
018516                 PERFORM BB-UPPDATERA-TIUPPDAT                            
018517              END-IF                                                      
018518           END-IF                                                         
018519        END-IF                                                            
018520        PERFORM IMS-GET-SATB11                                            
018521     END-PERFORM                                                          
018522     .                                                                    
018523     EJECT                                                                
018524 BA-KONTROLLERA-FLIART SECTION.                                           
018525                                                                          
018526     MOVE W-IDARTNR TO W-IDARTNR-S                                        
018527     MOVE SPACE     TO W-BELEVART-S                                       
018528                       W-IDLEVNR-S                                        
018529     MOVE NEJ       TO WS-FLIART                                          
018530                                                                          
018531     PERFORM IMS-GET-SATE-CSEQ-FIRST                                      
018532     PERFORM UNTIL SEGMENT-SAKNAS OR WS-FLIART = JA                       
018533        IF SATE-STR-TIBORT = ZERO                                         
018534         AND SATE-STR-IDARTNR < 100000000                                 
018535           MOVE SATE-RAD-TISTODAT TO WS-TISTODAT                          
018536           MOVE DAGENS-DATUM   TO TMP1-YYMMDD                             
018537           MOVE WS-TISTODAT    TO TMP2-YYMMDD                             
018538           PERFORM WY2000P1                                               
018539           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
018540              MOVE JA TO WS-FLIART                                        
018541           END-IF                                                         
018542        END-IF                                                            
018543        PERFORM IMS-GET-SATE-CSEQ-NEXT                                    
018544     END-PERFORM                                                          
018545                                                                          
018546     PERFORM IMS-GET-ARTC01                                               
018547     IF SEGMENT-FINNS                                                     
018548        IF WS-FLIART = ART-FLIART                                         
018549           CONTINUE                                                       
018550        ELSE                                                              
018551           MOVE WS-FLIART TO ART-FLIART                                   
018552           PERFORM IMS-REPL-ARTC                                          
018553           ADD +1 TO CHKP-ANT                                             
018554        END-IF                                                            
018555     END-IF                                                               
018556     .                                                                    
018557     EJECT                                                                
018558 BB-UPPDATERA-TIUPPDAT SECTION.                                           
018559                                                                          
018560     MOVE WS-IDARTNR-SATS TO W-IDARTNR                                    
018561     PERFORM IMS-GET-SATB01-SATB2-PCB                                     
018562     MOVE DAGENS-DATUM TO SATB-STR-TIUPPDAT                               
018563     PERFORM IMS-REPL-SATB2-PCB                                           
018564     ADD +1 TO CHKP-ANT                                                   
018565     .                                                                    
018566     EJECT                                                                
018570 X-TAG-CHECKPOINT SECTION.                                                
018600                                                                          
018800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
018810                                                                          
018900     MOVE WS-IDARTNR-SATS TO W-IDARTNR                                    
019000                                                                          
019200     PERFORM IMS-CHECKPOINT                                               
019210                                                                          
019300* --- LÄS OM DATABAS OM DET BEHÖVS                                        
019310                                                                          
019320     PERFORM IMS-GU-SATB01                                                
019330     MOVE ZERO TO CHKP-ANT                                                
019400     .                                                                    
019500     EJECT                                                                
019600* --- IMS SEKTIONER ---                                                   
019700     SKIP3                                                                
019802 IMS-GET-ARTC01 SECTION.                                                  
019803                                                                          
019804     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
019805          DELIMITED BY SIZE INTO SSA1                                     
019806     MOVE '  GE' TO GODK-STATUSKODER                                      
019807     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
019808     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
019809     PERFORM IMS-STATUSKONTROLL                                           
019810     .                                                                    
019811     SKIP3                                                                
019812 IMS-REPL-ARTC SECTION.                                                   
019813                                                                          
019814     MOVE '  ' TO GODK-STATUSKODER                                        
019815     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
019816     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
019817     PERFORM IMS-STATUSKONTROLL                                           
019818     .                                                                    
019819     EJECT                                                                
019820 IMS-GET-SATB01 SECTION.                                                  
019821                                                                          
019822     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
019823          DELIMITED BY SIZE INTO SSA1                                     
019824     MOVE '  GBGE' TO GODK-STATUSKODER                                    
019825     CALL CBLTDLI USING GHN SATB-PCB DLI-IO-AREA SSA1                     
019826     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
019827     PERFORM IMS-STATUSKONTROLL                                           
019828     .                                                                    
019829     SKIP3                                                                
019830 IMS-GU-SATB01 SECTION.                                                   
019831                                                                          
019832     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
019833          DELIMITED BY SIZE INTO SSA1                                     
019834     MOVE '  ' TO GODK-STATUSKODER                                        
019835     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
019836     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
019837     PERFORM IMS-STATUSKONTROLL                                           
019838     .                                                                    
019839     SKIP3                                                                
019840 IMS-GET-SATB01-SATB2-PCB SECTION.                                        
019841                                                                          
019842     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
019843          DELIMITED BY SIZE INTO SSA1                                     
019844     MOVE '  ' TO GODK-STATUSKODER                                        
019845     CALL CBLTDLI USING GHU SATB2-PCB DLI-IO-AREA SSA1                    
019846     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
019847     PERFORM IMS-STATUSKONTROLL                                           
019848     .                                                                    
019849     EJECT                                                                
019850 IMS-GET-SATB11 SECTION.                                                  
019851                                                                          
019852     MOVE 'WLSATB11 ' TO SSA1                                             
019853     MOVE '  GE' TO GODK-STATUSKODER                                      
019854     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
019855     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
019856     PERFORM IMS-STATUSKONTROLL                                           
019857     .                                                                    
019858     SKIP3                                                                
019859 IMS-REPL-SATB SECTION.                                                   
019860                                                                          
019861     MOVE '  ' TO GODK-STATUSKODER                                        
019862     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
019863     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
019864     PERFORM IMS-STATUSKONTROLL                                           
019865     .                                                                    
019866     SKIP3                                                                
019867 IMS-REPL-SATB2-PCB SECTION.                                              
019868                                                                          
019869     MOVE '  ' TO GODK-STATUSKODER                                        
019870     CALL CBLTDLI USING REPL SATB2-PCB DLI-IO-AREA                        
019871     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
019872     PERFORM IMS-STATUSKONTROLL                                           
019873     .                                                                    
019874     EJECT                                                                
019875 IMS-GET-SATE-CSEQ-FIRST SECTION.                                         
019876                                                                          
019877     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
019878          DELIMITED BY SIZE INTO SSA1                                     
019879     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
019880          DELIMITED BY SIZE INTO SSA2                                     
019881     MOVE '  GE' TO GODK-STATUSKODER                                      
019882     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA SSA1 SSA2                 
019883     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
019884     PERFORM IMS-STATUSKONTROLL                                           
019885     .                                                                    
019886     SKIP3                                                                
019887 IMS-GET-SATE-CSEQ-NEXT SECTION.                                          
019888                                                                          
019889     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
019890          DELIMITED BY SIZE INTO SSA1                                     
019891     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
019892          DELIMITED BY SIZE INTO SSA2                                     
019893     MOVE '  GE' TO GODK-STATUSKODER                                      
019894     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA SSA1 SSA2                 
019895     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
019896     PERFORM IMS-STATUSKONTROLL                                           
019897     .                                                                    
019898     EJECT                                                                
019899 IMS-ISRT-XXBY11 SECTION.                                                 
019900                                                                          
019901     STRING 'WLXXBY01(WDG3KEY  =' W-WDG3KEY-X ')'                         
019902          DELIMITED BY SIZE INTO SSA1                                     
019903     MOVE 'WLXXBY11 ' TO SSA2                                             
019904     MOVE '  ' TO GODK-STATUSKODER                                        
019905     CALL CBLTDLI USING ISRT XXBY-PCB DLI-IO-AREA-2 SSA1 SSA2             
019906     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
019907     PERFORM IMS-STATUSKONTROLL                                           
019908     .                                                                    
019910     SKIP3                                                                
020000 IMS-RESTART SECTION.                                                     
020100     SKIP2                                                                
020200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300     MOVE '  ' TO GODK-STATUSKODER                                        
020400     CALL CBLTDLI USING XRST MSG-PCB                                      
020500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600                        CHKP-AREA-LENGTH CHKP-AREA                        
020700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSKONTROLL                                           
020900     .                                                                    
021000     EJECT                                                                
021100 IMS-CHECKPOINT SECTION.                                                  
021200     SKIP2                                                                
021300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021400     MOVE '  XD' TO GODK-STATUSKODER                                      
021500     CALL CBLTDLI USING CHKP MSG-PCB                                      
021600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021700                        CHKP-AREA-LENGTH CHKP-AREA                        
021800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021900     PERFORM IMS-STATUSKONTROLL                                           
022000                                                                          
022100     IF IMS-EJ-OK                                                         
022200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022300       DISPLAY FELTEXT                                                    
022400       CALL FELLOG                                                        
022500     END-IF                                                               
022600     .                                                                    
022700     SKIP3                                                                
022800 IMS-STATUSKONTROLL SECTION.                                              
022900     SKIP2                                                                
023000     SET STATUS-IX TO 1                                                   
023100     SEARCH GODK-STATUS                                                   
023200       AT END                                                             
023300         STRING 'FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                 
023310         DELIMITED BY SIZE INTO FELTEXT-STR                               
023400         DISPLAY FELTEXT                                                  
023500         CALL FELLOG                                                      
023600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023700         CONTINUE                                                         
023800     END-SEARCH                                                           
023900     .                                                                    
023910     EJECT                                                                
024000*    -COPY WY2000P1                                                       
