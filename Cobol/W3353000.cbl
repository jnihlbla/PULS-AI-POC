000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3353000.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   93/10/14.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        PRISGRUPPSINFORMATION                                            
001100*        UPPDATERAR KUNDREGISTRET MED EN FIL FRÅN RESP                    
001200*        MARKNAD.                                                         
001300*        FILEN INNEHÅLLER ARTIKELRABATTER(SPECIALRABATTER)                
001400*                                                                         
001410*                                                                         
001500*        PROGRAMMET UPPDATERAR WLPRIB (WDC2)                              
001600*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- NYA RABATTSTRUKTURER                                       
003000     SELECT W33507                     ASSIGN TO W33530D1.                
003100     SKIP2                                                                
003200*          --- RABATTSTRUKTURER SOM SAKNAR PRIS-OMR                       
003300     SELECT W33531                     ASSIGN TO W33530D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W33507                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  -COPY W335301B  -L.                                                  
004400     SKIP2                                                                
004500 FD  W33531                                                               
004600     RECORDING       V                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900*01  POST   -COPY W335301B  -PRE FEL-   -L.                               
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005101*    -COPY WY2000W1                                                       
005102     SKIP3                                                                
005103 01  CHKP-VAR.                                                            
005104 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005105 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005106 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005107 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005108 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005110 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005200 77  IDPGM                       PIC X(8)    VALUE 'W3353000'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  UPPDATERA                   PIC X       VALUE 'J'.                   
005600 77  IX                          PIC 9(2)    COMP SYNC.                   
005900 77  SPAR-IDPROMR                PIC X(3)    VALUE SPACE.                 
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400                                                                          
006500 77  W33507-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W33507                       VALUE 'J'.                   
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800                                                                          
006900 77  NY-TABELL-SW                PIC X       VALUE 'N'.                   
007000     88  NY-TABELL                           VALUE 'J'.                   
007100     EJECT                                                                
007630                                                                          
007631 01  WS-AMC-TISTADAT             PIC 9(6)    VALUE ZERO.                  
007632                                                                          
007640 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007650 01  FILLER REDEFINES DAGENS-DATUM.                                       
007660     03  DAGENS-AA               PIC 9(2).                                
007670     03  DAGENS-MM               PIC 9(2).                                
007680     03  DAGENS-DD               PIC 9(2).                                
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
008000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900 01  AMC-AREA-START              PIC X(24)   VALUE                        
009000                                             'AMC-AREA-START'.            
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W335301B   -PRE AMC-                                      
009400*                                                                         
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-IDPROMR-X.                                                     
010000         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
010010     03  W-WDC211KY-X.                                                    
010020         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010030         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
010100     03  W-WDC211KY-MIN-X.                                                
010200         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
010300         05  W-DASTADAT-MIN      PIC 9(8)    VALUE ZERO.                  
010310     03  W-WDC211KY-MAX-X.                                                
010320         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
010330         05  W-DASTADAT-MAX      PIC 9(8)    VALUE 99999999.              
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011100     88  IMS-EJ-OK                           VALUE 'XD'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012400     SKIP3                                                                
012500 01  DLI-IO-AREA.                                                         
012600     03  IO-AREA                 PIC X(800)  VALUE SPACE.                 
012700     SKIP3                                                                
012800     03  WLPRIB01 REDEFINES IO-AREA.                                      
012900*        05  -COPY WDC201  -PRE PRIB-                                     
013000     SKIP3                                                                
013100     03  WLPRIB11 REDEFINES IO-AREA.                                      
013200*        05  -COPY WDC211  -PRE SPEC-                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013700     EJECT                                                                
013800*01  -COPY W0008  -PRE PRIB-                                              
013900     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100 PROCEDURE DIVISION  USING MSG-PCB  PRIB-PCB.                             
014110 MAIN SECTION.                                                            
014200     ENTRY 'DLITCBL' USING MSG-PCB  PRIB-PCB.                             
014300                                                                          
014500     PERFORM A-INIT                                                       
014600     PERFORM S01-LAES-W33507                                              
014700     PERFORM UNTIL END-OF-W33507                                          
014701       IF CHKP-ANT > CHKP-MAX                                             
014702         PERFORM X-TAG-CHECKPOINT                                         
014710       END-IF                                                             
014800       MOVE AMC-IDPROMR TO W-IDPROMR-X                                    
014900       MOVE AMC-IDPROMR TO SPAR-IDPROMR                                   
015000       PERFORM B-UPPDATERA-PRIB-SPEC                                      
015100     END-PERFORM                                                          
015200                                                                          
015300                                                                          
015400     PERFORM Z-FINIT                                                      
015500                                                                          
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK                                                               
015800     .                                                                    
015900     EJECT                                                                
016000 A-INIT SECTION.                                                          
016200                                                                          
016210     PERFORM IMS-RESTART                                                  
016300     ACCEPT DAGENS-DATUM FROM DATE                                        
016400     OPEN INPUT W33507                                                    
016500     OPEN OUTPUT W33531                                                   
016600                                                                          
016700                                                                          
016800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     .                                                                    
017000     EJECT                                                                
020500 B-UPPDATERA-PRIB-SPEC SECTION.                                           
020700                                                                          
020800     PERFORM IMS-GET-PRIB-PRO                                             
021000     IF SEGMENT-FINNS                                                     
021100       PERFORM UNTIL END-OF-W33507 OR                                     
021200         AMC-IDPROMR NOT = SPAR-IDPROMR                                   
021300           MOVE AMC-IDARTNR TO W-IDARTNR-MIN                              
021400                               W-IDARTNR-MAX                              
021500           PERFORM IMS-GHNP-SPEC-FIRST                                    
021600           IF SEGMENT-FINNS                                               
021700             PERFORM BA-KOLLA-KOD-DATUM                                   
021900             PERFORM BB-UPPDATERA                                         
022800           ELSE                                                           
022900             MOVE AMC-TISTADAT    TO SPEC-ART-DASTADAT                    
022901                                     WS-AMC-TISTADAT                      
022910*---Y2K-FIX*******                                                        
022920             IF AMC-TISTADAT NOT = ZERO                                   
022930               IF AMC-TISTADAT < 500000                                   
022940                 MOVE 20          TO SPEC-ART-DASTADAT(1:2)               
022950               ELSE                                                       
022960                 IF AMC-TISTADAT < 999999                                 
022970                   MOVE 19        TO SPEC-ART-DASTADAT(1:2)               
022980                 ELSE                                                     
022990                   MOVE 99999999  TO SPEC-ART-DASTADAT                    
022991                 END-IF                                                   
022992               END-IF                                                     
022993             END-IF                                                       
023000             MOVE AMC-TISTODAT      TO SPEC-ART-TISTODAT                  
023100             MOVE AMC-IDARTNR       TO SPEC-ART-IDARTNR                   
023200             MOVE AMC-REARTRAB-DO   TO SPEC-ART-REARTRAB-DO               
023300             MOVE AMC-REARTRAB-BULK TO SPEC-ART-REARTRAB-BULK             
023400             PERFORM IMS-ISRT-PRIB-SPEC                                   
023410             ADD +1 TO CHKP-ANT                                           
023500           END-IF                                                         
024100         PERFORM S01-LAES-W33507                                          
024200       END-PERFORM                                                        
024300     ELSE                                                                 
024400       PERFORM BC-SKRIV-FELFIL                                            
024610       PERFORM S01-LAES-W33507                                            
024700     END-IF                                                               
024800     .                                                                    
024900     EJECT                                                                
025000 BA-KOLLA-KOD-DATUM SECTION.                                              
025100                                                                          
025300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
025400       IF AMC-IDARTNR = SPEC-ART-IDARTNR                                  
025401         MOVE AMC-TISTADAT           TO TMP1-YYMMDD                       
025403         MOVE SPEC-ART-DASTADAT(3:6) TO TMP2-YYMMDD                       
025404         MOVE AMC-TISTODAT           TO TMP3-YYMMDD                       
025410         PERFORM WY2000Q1                                                 
025500         IF  TMP1-YYMMDD < TMP2-YYMMDD                                    
025700         AND TMP3-YYMMDD > TMP2-YYMMDD                                    
025800           PERFORM IMS-DLET-SPEC                                          
025810           ADD +1 TO CHKP-ANT                                             
025900         ELSE                                                             
026000                                                                          
026001           MOVE AMC-TISTADAT           TO TMP1-YYMMDD                     
026002           MOVE SPEC-ART-DASTADAT(3:6) TO TMP2-YYMMDD                     
026004           MOVE AMC-TISTODAT           TO TMP3-YYMMDD                     
026005           MOVE SPEC-ART-TISTODAT      TO TMP4-YYMMDD                     
026010           PERFORM WY2000Q1                                               
026100           IF  TMP1-YYMMDD >  TMP2-YYMMDD                                 
026300           AND TMP3-YYMMDD <= TMP4-YYMMDD                                 
026306             PERFORM IMS-DLET-SPEC                                        
026307             ADD +1 TO CHKP-ANT                                           
026500           ELSE                                                           
026600                                                                          
026601             MOVE AMC-TISTADAT           TO TMP1-YYMMDD                   
026602             MOVE SPEC-ART-TISTODAT      TO TMP2-YYMMDD                   
026603             MOVE AMC-TISTODAT           TO TMP3-YYMMDD                   
026610             PERFORM WY2000Q1                                             
026700             IF  TMP1-YYMMDD <= TMP2-YYMMDD                               
026900             AND TMP3-YYMMDD >  TMP2-YYMMDD                               
026906               PERFORM IMS-DLET-SPEC                                      
026907               ADD +1 TO CHKP-ANT                                         
027200             ELSE                                                         
027202               IF WS-AMC-TISTADAT =  SPEC-ART-DASTADAT(3:6)               
027204                  AND AMC-TISTODAT = SPEC-ART-TISTODAT                    
027209                 PERFORM IMS-DLET-SPEC                                    
027210                 ADD +1 TO CHKP-ANT                                       
027211               END-IF                                                     
027212             END-IF                                                       
027213           END-IF                                                         
027220         END-IF                                                           
027230       END-IF                                                             
027240       IF CHKP-ANT > CHKP-MAX                                             
027250         PERFORM X-TAG-CHECKPOINT                                         
027270*--------HÄMTA ROTEN IGEN                                                 
027280         PERFORM IMS-GET-PRIB-PRO                                         
027290       END-IF                                                             
027300       PERFORM IMS-GHNP-SPEC-SEG                                          
027400     END-PERFORM                                                          
027500     .                                                                    
027600     EJECT                                                                
027700 BB-UPPDATERA SECTION.                                                    
027800                                                                          
027900     MOVE AMC-TISTADAT    TO SPEC-ART-DASTADAT                            
027910*---Y2K-FIX*******                                                        
027920     IF AMC-TISTADAT NOT = ZERO                                           
027930       IF AMC-TISTADAT < 500000                                           
027940         MOVE 20          TO SPEC-ART-DASTADAT(1:2)                       
027950       ELSE                                                               
027960         IF AMC-TISTADAT < 999999                                         
027970           MOVE 19        TO SPEC-ART-DASTADAT(1:2)                       
027980         ELSE                                                             
027990           MOVE 99999999  TO SPEC-ART-DASTADAT                            
027991         END-IF                                                           
027992       END-IF                                                             
027993     END-IF                                                               
028000     MOVE AMC-TISTODAT      TO SPEC-ART-TISTODAT                          
028100     MOVE AMC-IDARTNR       TO SPEC-ART-IDARTNR                           
028200     MOVE AMC-REARTRAB-DO   TO SPEC-ART-REARTRAB-DO                       
028300     MOVE AMC-REARTRAB-BULK TO SPEC-ART-REARTRAB-BULK                     
028400     PERFORM IMS-ISRT-PRIB-SPEC                                           
028500     IF SEGMENT-FINNS-REDAN                                               
028600        PERFORM BBA-BYT-TILL-NYTT                                         
028610     ELSE                                                                 
028620        ADD +1 TO CHKP-ANT                                                
028700     END-IF                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 BBA-BYT-TILL-NYTT SECTION.                                               
029010*--- BYT GAMMALT SEGMENT MOT NYTT.                                        
029100                                                                          
029300     MOVE AMC-TISTADAT    TO W-DASTADAT                                   
029301*---Y2K-FIX*******                                                        
029302     IF AMC-TISTADAT NOT = ZERO                                           
029303       IF AMC-TISTADAT < 500000                                           
029304         MOVE 20          TO W-DASTADAT(1:2)                              
029305       ELSE                                                               
029306         IF AMC-TISTADAT < 999999                                         
029307           MOVE 19        TO W-DASTADAT(1:2)                              
029308         ELSE                                                             
029309           MOVE 99999999  TO W-DASTADAT                                   
029310         END-IF                                                           
029311       END-IF                                                             
029312     END-IF                                                               
029400     MOVE AMC-IDARTNR       TO W-IDARTNR                                  
029500     PERFORM IMS-GHU-SPEC-SEG                                             
029600     IF SEGMENT-FINNS                                                     
029700       MOVE AMC-TISTODAT      TO SPEC-ART-TISTODAT                        
029800       MOVE AMC-REARTRAB-DO   TO SPEC-ART-REARTRAB-DO                     
029900       MOVE AMC-REARTRAB-BULK TO SPEC-ART-REARTRAB-BULK                   
030000       PERFORM IMS-REPL-SPEC-SEG                                          
030010       ADD +1 TO CHKP-ANT                                                 
030500     END-IF                                                               
030501     IF CHKP-ANT > CHKP-MAX                                               
030502       PERFORM X-TAG-CHECKPOINT                                           
030503     END-IF                                                               
030510*----HÄMTA ROTEN IGEN                                                     
030520     PERFORM IMS-GET-PRIB-PRO                                             
030600     .                                                                    
030700     EJECT                                                                
030800 BC-SKRIV-FELFIL SECTION.                                                 
031000                                                                          
031100     WRITE FEL-POST FROM AMC-AREA                                         
031200     MOVE SPACE TO POSTSUM-TRANSTYP                                       
031300     MOVE 'W33531' TO POSTSUM-FDNAMN                                      
031400     MOVE 'W33020D2' TO POSTSUM-DDNAMN2                                   
031500     CALL POSTSUM USING POSTSUM-PARM                                      
031600     .                                                                    
031700     EJECT                                                                
031800 Z-FINIT SECTION.                                                         
031900                                                                          
032100     CLOSE W33507                                                         
032200           W33531                                                         
032300                                                                          
032400     MOVE 'S' TO POSTSUM-OPKOD                                            
032500     CALL POSTSUM USING POSTSUM-PARM                                      
032600     .                                                                    
032700     EJECT                                                                
032800 S01-LAES-W33507  SECTION.                                                
032900                                                                          
033000     READ W33507 INTO AMC-AREA                                            
033100     AT END                                                               
033200     MOVE JA TO W33507-EOF-SW                                             
033300                                                                          
033400     NOT AT END                                                           
033500        MOVE 'W33507' TO POSTSUM-FDNAMN                                   
033600        MOVE 'W33530D1' TO POSTSUM-DDNAMN2                                
033700        MOVE 'AMC'      TO POSTSUM-TRANSTYP                               
033800        CALL POSTSUM USING POSTSUM-PARM                                   
033900     END-READ                                                             
034000     .                                                                    
034100     EJECT                                                                
034101 X-TAG-CHECKPOINT   SECTION.                                              
034102                                                                          
034105     PERFORM IMS-CHECKPOINT                                               
034106     MOVE ZERO TO CHKP-ANT                                                
034108     .                                                                    
034110     EJECT                                                                
034200* --- IMS SEKTIONER ---                                                   
034300                                                                          
034500 IMS-GET-PRIB-PRO SECTION.                                                
034600     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
034700          DELIMITED BY SIZE INTO SSA1                                     
034800     MOVE '  GE' TO GODK-STATUSKODER                                      
034900     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-AREA SSA1                     
035000     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
035100     PERFORM IMS-STATUSKONTROLL                                           
035200     .                                                                    
035300     SKIP3                                                                
035400 IMS-GHNP-SPEC-FIRST SECTION.                                             
035500                                                                          
035600     MOVE 'WLPRIB11*F '  TO SSA1                                          
035700     MOVE '  GE' TO GODK-STATUSKODER                                      
035800     CALL CBLTDLI USING GHNP PRIB-PCB DLI-IO-AREA SSA1                    
035900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
036000     PERFORM IMS-STATUSKONTROLL                                           
036200     .                                                                    
036210     SKIP3                                                                
036300 IMS-GHNP-SPEC-SEG SECTION.                                               
036400                                                                          
036410     STRING 'WLPRIB11(WDC211KY>=' W-WDC211KY-MIN-X                        
036430                    '&WDC211KY<=' W-WDC211KY-MAX-X ')'                    
036440          DELIMITED BY SIZE INTO SSA1                                     
036600     MOVE '  GE' TO GODK-STATUSKODER                                      
036700     CALL CBLTDLI USING GHNP PRIB-PCB DLI-IO-AREA SSA1                    
036800     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
036900     PERFORM IMS-STATUSKONTROLL                                           
037100     .                                                                    
037110     EJECT                                                                
037200 IMS-ISRT-PRIB-SPEC SECTION.                                              
037300                                                                          
037400     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
037500          DELIMITED BY SIZE INTO SSA1                                     
037600     MOVE 'WLPRIB11 ' TO SSA2                                             
037700     MOVE '  II' TO GODK-STATUSKODER                                      
037800     CALL CBLTDLI USING ISRT PRIB-PCB DLI-IO-AREA SSA1 SSA2               
037900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
038000     PERFORM IMS-STATUSKONTROLL                                           
038200     .                                                                    
038210     SKIP2                                                                
038300 IMS-GHU-SPEC-SEG   SECTION.                                              
038400     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
038500          DELIMITED BY SIZE INTO SSA1                                     
038600     STRING 'WLPRIB11(WDC211KY =' W-WDC211KY-X ')'                        
038700          DELIMITED BY SIZE INTO SSA2                                     
038800     MOVE '  ' TO GODK-STATUSKODER                                        
038900     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-AREA SSA1 SSA2                
039000     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
039100     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039310     EJECT                                                                
039400 IMS-DLET-SPEC  SECTION.                                                  
039500     MOVE '  ' TO GODK-STATUSKODER                                        
039600     CALL CBLTDLI USING DLET PRIB-PCB DLI-IO-AREA                         
039700     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
039800     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040001     SKIP2                                                                
040010 IMS-REPL-SPEC-SEG  SECTION.                                              
040020     MOVE '  ' TO GODK-STATUSKODER                                        
040030     CALL CBLTDLI USING REPL PRIB-PCB DLI-IO-AREA                         
040040     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
040050     PERFORM IMS-STATUSKONTROLL                                           
040070     .                                                                    
040080     EJECT                                                                
040101 IMS-RESTART SECTION.                                                     
040102                                                                          
040103     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
040104     MOVE '  ' TO GODK-STATUSKODER                                        
040105     CALL CBLTDLI USING XRST MSG-PCB                                      
040106                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
040107                        CHKP-AREA-LENGTH CHKP-AREA                        
040108     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040109     PERFORM IMS-STATUSKONTROLL                                           
040110     .                                                                    
040111     SKIP3                                                                
040112 IMS-CHECKPOINT SECTION.                                                  
040113                                                                          
040114     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
040115     MOVE '  XD' TO GODK-STATUSKODER                                      
040116     CALL CBLTDLI USING CHKP MSG-PCB                                      
040117                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
040118                        CHKP-AREA-LENGTH CHKP-AREA                        
040119     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040120     PERFORM IMS-STATUSKONTROLL                                           
040121                                                                          
040122     IF IMS-EJ-OK                                                         
040123       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
040124       DISPLAY FELTEXT                                                    
040125       CALL FELLOG                                                        
040126     END-IF                                                               
040127     .                                                                    
040130     EJECT                                                                
040200 IMS-STATUSKONTROLL SECTION.                                              
040300                                                                          
040400     SET STATUS-IX TO 1                                                   
040500     SEARCH GODK-STATUS                                                   
040600       AT END                                                             
040700         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
040800         DISPLAY FELTEXT                                                  
040900         CALL FELLOG                                                      
041000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041100         CONTINUE                                                         
041200     END-SEARCH                                                           
041300     .                                                                    
041310     EJECT                                                                
041400*    -COPY WY2000Q1                                                       
