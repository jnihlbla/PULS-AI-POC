000100 PROCESS DYNAM                                                            
001220*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
001230*                                                                         
001240 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5223100.                                                
001400 AUTHOR.         HENRIKSSON ANDERS.                                       
001500 DATE-WRITTEN.   02/05/23.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        SELECTS DATA FROM DB2-TABLE T01IVW                               
002200*                                                                         
002310*        THE PROGRAM READS AND DELETES DB2-TABLE T01IVW                   
002400*                                                                         
002500*    INDATA.                                                              
002600*    UTDATA.                                                              
002800*        FILES FOR THE DELETED DATA                                       
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003500 INPUT-OUTPUT SECTION.                                                    
003700 FILE-CONTROL.                                                            
003801                                                                          
003802*          --- SEQUENCEFILE INTRASTAT                                     
003803     SELECT W52231                     ASSIGN TO W52231D1.                
003804                                                                          
003805*          --- SEQUENCEFILE VAT                                           
003810     SELECT W52232                     ASSIGN TO W52231D2.                
003820                                                                          
003830*          --- SEQUENCEFILE CUSTOMS                                       
003840     SELECT W52233                     ASSIGN TO W52231D3.                
004000     EJECT                                                                
004010                                                                          
004100 DATA DIVISION.                                                           
004300 FILE SECTION.                                                            
004401     SKIP3                                                                
004402 FD  W52231                                                               
004403     RECORDING       F                                                    
004404     BLOCK CONTAINS  0.                                                   
004405                                                                          
004406*01  POST    -COPY W522INT -PRE  INT-  -L.                                
004407     EJECT                                                                
004408                                                                          
004409 FD  W52232                                                               
004410     RECORDING       F                                                    
004411     BLOCK CONTAINS  0.                                                   
004412                                                                          
004420*01  POST    -COPY W522VAT -PRE  VAT-  -L.                                
004500     EJECT                                                                
004501                                                                          
004510 FD  W52233                                                               
004520     RECORDING       F                                                    
004530     BLOCK CONTAINS  0.                                                   
004540                                                                          
004550*01  POST    -COPY W522CUS -PRE  CUS-  -L.                                
004560     EJECT                                                                
004570                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W5223100'.            
004800                                                                          
004900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND                
005000 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200                                                                          
005300 77  YES                         PIC X       VALUE 'J'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500                                                                          
006200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006300     88  KEYS-OK                             VALUE 'J'.                   
006400     88  KEYS-WRONG                          VALUE 'N'.                   
006410     EJECT                                                                
006411                                                                          
006412 01  WS-COMMIT.                                                           
006413     03  WS-COMMIT-FREQUENCY     PIC S9(9)V9(2)  COMP-3.                  
006414     03  WS-COMMIT-COUNT         PIC S9(9)V9(2)  COMP-3.                  
006416     EJECT                                                                
006417                                                                          
006418 01  WS-MISC-DATES.                                                       
006419     03 WS-CURRENT-DATE          PIC X(8)  VALUE SPACE.                   
006420     03 WS-DELDATUM              PIC X(8)  VALUE SPACE.                   
006421     EJECT                                                                
006422                                                                          
006430*    --- WS-AREA FOR SEQUENCEFILES                                        
006450 01  WS-DAREGDAT-INT             PIC X(8)    VALUE SPACE.                 
006460 01  WS-TIREGTID-INT             PIC S9(7)   VALUE ZERO COMP-3.           
006470 01  WS-IDLOPNR-INT              PIC S9(5)   VALUE ZERO COMP-3.           
006480 01  WS-IDPTYP-INT               PIC X(3)    VALUE 'INT'.                 
006490 01  WS-TIRP-INT                 PIC S9(2)   VALUE ZERO COMP-3.           
006500 01  WS-FLKLAR-INT               PIC X       VALUE SPACE.                 
006510 01  WS-IV-DATA-INT              PIC X(200)  VALUE SPACE.                 
006520 01  WS-IV-DATA-INT2             PIC X(200)  VALUE SPACE.                 
006560*                                                                         
006570 01  WS-DAREGDAT-VAT             PIC X(8)    VALUE SPACE.                 
006580 01  WS-TIREGTID-VAT             PIC S9(7)   VALUE ZERO COMP-3.           
006590 01  WS-IDLOPNR-VAT              PIC S9(5)   VALUE ZERO COMP-3.           
006591 01  WS-IDPTYP-VAT               PIC X(3)    VALUE 'VAT'.                 
006592 01  WS-TIRP-VAT                 PIC S9(2)   VALUE ZERO COMP-3.           
006593 01  WS-FLKLAR-VAT               PIC X       VALUE SPACE.                 
006594 01  WS-IV-DATA-VAT              PIC X(200)  VALUE SPACE.                 
006595*                                                                         
006596 01  WS-DAREGDAT-CUS             PIC X(8)    VALUE SPACE.                 
006597 01  WS-TIREGTID-CUS             PIC S9(7)   VALUE ZERO COMP-3.           
006598 01  WS-IDLOPNR-CUS              PIC S9(5)   VALUE ZERO COMP-3.           
006599 01  WS-IDPTYP-CUS               PIC X(3)    VALUE 'CUS'.                 
006600 01  WS-TIRP-CUS                 PIC S9(2)   VALUE ZERO COMP-3.           
006601 01  WS-FLKLAR-CUS               PIC X       VALUE SPACE.                 
006602 01  WS-IV-DATA-CUS              PIC X(200)  VALUE SPACE.                 
006603*                                                                         
006610*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
007200     SKIP3                                                                
007300*    --- PARAMETERS TO ABEND                                              
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007810 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007900     EJECT                                                                
007910                                                                          
008000 01  MESSAGE-CODES.                                                       
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008301     EJECT                                                                
008302                                                                          
008305*    -COPY WZ20DAYS                                                       
008306     EJECT                                                                
008504                                                                          
008602 01  INT-AREA.                                                            
008606*03  -COPY W522INT   -PRE INT-                                            
008607     EJECT                                                                
008608                                                                          
008609 01  VAT-AREA.                                                            
008613*03  -COPY W522VAT   -PRE VAT-                                            
008614     EJECT                                                                
008615                                                                          
008616 01  CUS-AREA.                                                            
008617*03  -COPY W522CUS   -PRE CUS-                                            
008618     EJECT                                                                
008619                                                                          
009902 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009903       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009904                                                                          
009905 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009906 01  DB2-WS.                                                              
009907     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009908         88  CURSOR-OK                       VALUE 000.                   
009909         88  LINES-FOUND                     VALUE 000.                   
009910         88  LINES-MISSING                   VALUE 100.                   
009911         88  DOUBLE-LINES                    VALUE 811.                   
009912         88  RESOURCE-WRONG                  VALUE 904.                   
009913     03  GOOD-SQLCODECODES.                                               
009914         05  GOOD-SQLCODE OCCURS 5                                        
009920             INDEXED BY SQLCODE-IX PIC 9(3).                              
010000     EJECT                                                                
010100                                                                          
010302 01  FILLER                      PIC X(16)  VALUE 'T01IVW-AREA'.          
010310*01  -COPY T01IVW   -PRE T01IVW-                                          
010320                                                                          
010401     EJECT                                                                
010410     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
010500     EJECT                                                                
010510                                                                          
010600 LINKAGE SECTION.                                                         
011001 PROCEDURE DIVISION.                                                      
011002 MAIN SECTION.                                                            
011100                                                                          
011600     PERFORM A-INIT                                                       
011700     PERFORM B-BEHANDLA-RADER                                             
012900     PERFORM Z-FINIT                                                      
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013310                                                                          
013400 A-INIT SECTION.                                                          
013500     MOVE +5000                       TO WS-COMMIT-FREQUENCY              
013600     MOVE +0                          TO WS-COMMIT-COUNT                  
013700                                                                          
013702     OPEN OUTPUT W52231                                                   
013703                 W52232                                                   
013704                 W52233                                                   
013711                                                                          
013920     INITIALIZE GOOD-SQLCODECODES                                         
013921                                                                          
013922     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
013930     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
013940     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
013950     MOVE 50                          TO DAYS-KVDAYS                      
013960     MOVE 'WEEKDAYS'                  TO DAYS-IDCALEND                    
013970     MOVE SPACE                       TO DAYS-TIDATE1                     
013980     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
013990     CALL WZ20DAYS USING                                                  
014000          DAYS-WZ20DAYS                                                   
014100     IF DAYS-KDRC = ZERO                                                  
014110       MOVE DAYS-TIDATE1              TO WS-DELDATUM                      
014120     ELSE                                                                 
014121       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014140     END-IF                                                               
014150     DISPLAY 'REG.DATUM < ' WS-DELDATUM                                   
014200     .                                                                    
014300     EJECT                                                                
014400                                                                          
017900 B-BEHANDLA-RADER SECTION.                                                
018005     PERFORM BA-BEHANDLA-RADER-INT                                        
018009     PERFORM BB-BEHANDLA-RADER-VAT                                        
018010     PERFORM BC-BEHANDLA-RADER-CUS                                        
018106     .                                                                    
018107     EJECT                                                                
018108                                                                          
018109 BA-BEHANDLA-RADER-INT SECTION.                                           
018111     PERFORM DB2-DCL-OPEN-CRS-INT                                         
018112     PERFORM DB2-FETCH-CRS-INT                                            
018113     PERFORM UNTIL LINES-MISSING                                          
018114       MOVE WS-IV-DATA-INT  TO INT-W522INT-001                            
018115       MOVE WS-IV-DATA-INT2 TO INT-W522INT-002                            
018116       PERFORM S11-WRITE-W52231                                           
018117       PERFORM DB2-DELETE-T01IVW-INT                                      
018118       IF WS-COMMIT-COUNT = WS-COMMIT-FREQUENCY                           
018119         PERFORM DB2-COMMIT-WORK                                          
018120         MOVE ZERO TO WS-COMMIT-COUNT                                     
018121         PERFORM DB2-CLOSE-CRS-INT                                        
018122         PERFORM DB2-DCL-OPEN-CRS-INT                                     
018123       END-IF                                                             
018124       PERFORM DB2-FETCH-CRS-INT                                          
018125     END-PERFORM                                                          
018126     PERFORM DB2-CLOSE-CRS-INT                                            
018127     .                                                                    
018128     EJECT                                                                
018129                                                                          
018130 BB-BEHANDLA-RADER-VAT SECTION.                                           
018131     PERFORM DB2-DCL-OPEN-CRS-VAT                                         
018132     PERFORM DB2-FETCH-CRS-VAT                                            
018133     PERFORM UNTIL LINES-MISSING                                          
018134       MOVE WS-IV-DATA-VAT  TO VAT-W522VAT                                
018135       PERFORM S12-WRITE-W52232                                           
018136       PERFORM DB2-DELETE-T01IVW-VAT                                      
018137       IF WS-COMMIT-COUNT = WS-COMMIT-FREQUENCY                           
018138         PERFORM DB2-COMMIT-WORK                                          
018139         MOVE ZERO TO WS-COMMIT-COUNT                                     
018140         PERFORM DB2-CLOSE-CRS-VAT                                        
018141         PERFORM DB2-DCL-OPEN-CRS-VAT                                     
018142       END-IF                                                             
018143       PERFORM DB2-FETCH-CRS-VAT                                          
018144     END-PERFORM                                                          
018150     PERFORM DB2-CLOSE-CRS-VAT                                            
018500     .                                                                    
018600     EJECT                                                                
018610                                                                          
018620 BC-BEHANDLA-RADER-CUS SECTION.                                           
018630     PERFORM DB2-DCL-OPEN-CRS-CUS                                         
018640     PERFORM DB2-FETCH-CRS-CUS                                            
018650     PERFORM UNTIL LINES-MISSING                                          
018660       MOVE WS-IV-DATA-CUS  TO CUS-W522CUS                                
018670       PERFORM S13-WRITE-W52233                                           
018680       PERFORM DB2-DELETE-T01IVW-CUS                                      
018690       IF WS-COMMIT-COUNT = WS-COMMIT-FREQUENCY                           
018691         PERFORM DB2-COMMIT-WORK                                          
018692         MOVE ZERO TO WS-COMMIT-COUNT                                     
018693         PERFORM DB2-CLOSE-CRS-CUS                                        
018694         PERFORM DB2-DCL-OPEN-CRS-CUS                                     
018695       END-IF                                                             
018696       PERFORM DB2-FETCH-CRS-CUS                                          
018697     END-PERFORM                                                          
018698     PERFORM DB2-CLOSE-CRS-CUS                                            
018699     .                                                                    
018700     EJECT                                                                
018710                                                                          
018800 Z-FINIT SECTION.                                                         
018900     CLOSE W52231                                                         
019000           W52232                                                         
019010           W52233                                                         
019100     SKIP2                                                                
019400     .                                                                    
019500     EJECT                                                                
019600                                                                          
023002 S11-WRITE-W52231 SECTION.                                                
023004     WRITE INT-POST FROM INT-AREA                                         
023010     .                                                                    
023011     EJECT                                                                
023012                                                                          
023013 S12-WRITE-W52232 SECTION.                                                
023015     WRITE VAT-POST FROM VAT-AREA                                         
023020     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 S13-WRITE-W52233 SECTION.                                                
023510     WRITE CUS-POST FROM CUS-AREA                                         
023520     .                                                                    
023530     EJECT                                                                
023540                                                                          
023549 DB2-DELETE-T01IVW-INT SECTION.                                           
023550     MOVE 000100 TO GOOD-SQLCODECODES                                     
023551     EXEC SQL                                                             
023552        DELETE FROM T01IVW                                                
023553                                                                          
023554        WHERE DAREGDAT = :WS-DAREGDAT-INT AND                             
023555              TIREGTID = :WS-TIREGTID-INT AND                             
023556              IDLOPNR  = :WS-IDLOPNR-INT  AND                             
023557              IDPTYP   = :WS-IDPTYP-INT   AND                             
023558              TIRP     = :WS-TIRP-INT                                     
023560     END-EXEC                                                             
023561     MOVE SQLCODE TO SQLCODE-WS                                           
023562     ADD +1       TO WS-COMMIT-COUNT                                      
023563     PERFORM DB2-STATUS-CHECK                                             
023564     .                                                                    
023565     EJECT                                                                
023566                                                                          
023567 DB2-DELETE-T01IVW-VAT SECTION.                                           
023568     MOVE 000100 TO GOOD-SQLCODECODES                                     
023569     EXEC SQL                                                             
023570        DELETE FROM T01IVW                                                
023571                                                                          
023572        WHERE DAREGDAT = :WS-DAREGDAT-VAT AND                             
023573              TIREGTID = :WS-TIREGTID-VAT AND                             
023574              IDLOPNR  = :WS-IDLOPNR-VAT  AND                             
023575              IDPTYP   = :WS-IDPTYP-VAT   AND                             
023576              TIRP     = :WS-TIRP-VAT                                     
023577     END-EXEC                                                             
023578     MOVE SQLCODE TO SQLCODE-WS                                           
023579     ADD +1       TO WS-COMMIT-COUNT                                      
023580     PERFORM DB2-STATUS-CHECK                                             
023581     .                                                                    
023582     EJECT                                                                
023583                                                                          
023584 DB2-DELETE-T01IVW-CUS SECTION.                                           
023585     MOVE 000100 TO GOOD-SQLCODECODES                                     
023586     EXEC SQL                                                             
023587        DELETE FROM T01IVW                                                
023588                                                                          
023589        WHERE DAREGDAT = :WS-DAREGDAT-CUS AND                             
023590              TIREGTID = :WS-TIREGTID-CUS AND                             
023591              IDLOPNR  = :WS-IDLOPNR-CUS  AND                             
023592              IDPTYP   = :WS-IDPTYP-CUS   AND                             
023593              TIRP     = :WS-TIRP-CUS                                     
023594     END-EXEC                                                             
023595     MOVE SQLCODE TO SQLCODE-WS                                           
023596     ADD +1       TO WS-COMMIT-COUNT                                      
023597     PERFORM DB2-STATUS-CHECK                                             
023598     .                                                                    
023599     EJECT                                                                
023600                                                                          
023601 DB2-DCL-OPEN-CRS-INT SECTION.                                            
023602     MOVE 000100 TO GOOD-SQLCODECODES                                     
023603                                                                          
023604     EXEC SQL DECLARE T01IVW-CRS-INT CURSOR WITH HOLD FOR                 
023605         SELECT DAREGDAT                                                  
023606              , TIREGTID                                                  
023607              , IDLOPNR                                                   
023608              , IDPTYP                                                    
023609              , TIRP                                                      
023610              , FLKLAR                                                    
023611              , IV_DATA                                                   
023612              , IV_DATA2                                                  
023613                                                                          
023614         FROM T01IVW                                                      
023615                                                                          
023616         WHERE IDPTYP   = :WS-IDPTYP-INT                                  
023617*          AND FLKLAR   = 'J'                                             
023618           AND DAREGDAT < :WS-DELDATUM                                    
023619     END-EXEC                                                             
023620                                                                          
023621     EXEC SQL                                                             
023622        OPEN T01IVW-CRS-INT                                               
023623     END-EXEC                                                             
023624                                                                          
023625     MOVE SQLCODE TO SQLCODE-WS                                           
023626     PERFORM DB2-STATUS-CHECK                                             
023627     .                                                                    
023628     EJECT                                                                
023629                                                                          
023630 DB2-FETCH-CRS-INT SECTION.                                               
023631     MOVE 000100  TO GOOD-SQLCODECODES                                    
023632     EXEC SQL                                                             
023633         FETCH T01IVW-CRS-INT                                             
023634         INTO   :WS-DAREGDAT-INT                                          
023635              , :WS-TIREGTID-INT                                          
023636              , :WS-IDLOPNR-INT                                           
023637              , :WS-IDPTYP-INT                                            
023638              , :WS-TIRP-INT                                              
023639              , :WS-FLKLAR-INT                                            
023640              , :WS-IV-DATA-INT                                           
023641              , :WS-IV-DATA-INT2                                          
023642     END-EXEC                                                             
023643                                                                          
023644     MOVE SQLCODE TO SQLCODE-WS                                           
023645     PERFORM DB2-STATUS-CHECK                                             
023646     .                                                                    
023647     EJECT                                                                
023650                                                                          
023659 DB2-CLOSE-CRS-INT  SECTION.                                              
023661     EXEC SQL                                                             
023662         CLOSE T01IVW-CRS-INT                                             
023663     END-EXEC                                                             
023664     .                                                                    
023665     EJECT                                                                
023666                                                                          
023667 DB2-DCL-OPEN-CRS-VAT SECTION.                                            
023669     MOVE 000100 TO GOOD-SQLCODECODES                                     
023670                                                                          
023671     EXEC SQL DECLARE T01IVW-CRS-VAT CURSOR WITH HOLD FOR                 
023672         SELECT DAREGDAT                                                  
023673              , TIREGTID                                                  
023674              , IDLOPNR                                                   
023675              , IDPTYP                                                    
023676              , TIRP                                                      
023677              , FLKLAR                                                    
023678              , IV_DATA                                                   
023680                                                                          
023681         FROM T01IVW                                                      
023682                                                                          
023683         WHERE IDPTYP   = :WS-IDPTYP-VAT                                  
023684*          AND FLKLAR   = 'J'                                             
023685           AND DAREGDAT < :WS-DELDATUM                                    
023686                                                                          
023688     END-EXEC                                                             
023689                                                                          
023690     EXEC SQL                                                             
023691        OPEN T01IVW-CRS-VAT                                               
023692     END-EXEC                                                             
023693                                                                          
023694     MOVE SQLCODE TO SQLCODE-WS                                           
023695     PERFORM DB2-STATUS-CHECK                                             
023696     .                                                                    
023700     EJECT                                                                
023710                                                                          
023737 DB2-FETCH-CRS-VAT SECTION.                                               
023739     MOVE 000100  TO GOOD-SQLCODECODES                                    
023740     EXEC SQL                                                             
023741         FETCH T01IVW-CRS-VAT                                             
023742         INTO   :WS-DAREGDAT-VAT                                          
023743              , :WS-TIREGTID-VAT                                          
023744              , :WS-IDLOPNR-VAT                                           
023745              , :WS-IDPTYP-VAT                                            
023746              , :WS-TIRP-VAT                                              
023747              , :WS-FLKLAR-VAT                                            
023748              , :WS-IV-DATA-VAT                                           
023749     END-EXEC                                                             
023750                                                                          
023751     MOVE SQLCODE TO SQLCODE-WS                                           
023752     PERFORM DB2-STATUS-CHECK                                             
023753     .                                                                    
023754     EJECT                                                                
023755                                                                          
023775 DB2-CLOSE-CRS-VAT SECTION.                                               
023777     EXEC SQL                                                             
023778         CLOSE T01IVW-CRS-VAT                                             
023779     END-EXEC                                                             
023780     .                                                                    
023781     EJECT                                                                
023782                                                                          
023783 DB2-DCL-OPEN-CRS-CUS SECTION.                                            
023784     MOVE 000100 TO GOOD-SQLCODECODES                                     
023785                                                                          
023786     EXEC SQL DECLARE T01IVW-CRS-CUS CURSOR WITH HOLD FOR                 
023787         SELECT DAREGDAT                                                  
023788              , TIREGTID                                                  
023789              , IDLOPNR                                                   
023790              , IDPTYP                                                    
023791              , TIRP                                                      
023792              , FLKLAR                                                    
023793              , IV_DATA                                                   
023794                                                                          
023795         FROM T01IVW                                                      
023796                                                                          
023797         WHERE IDPTYP   = :WS-IDPTYP-CUS                                  
023798*          AND FLKLAR   = 'J'                                             
023799           AND DAREGDAT < :WS-DELDATUM                                    
023800                                                                          
023801     END-EXEC                                                             
023802                                                                          
023803     EXEC SQL                                                             
023804        OPEN T01IVW-CRS-CUS                                               
023805     END-EXEC                                                             
023806                                                                          
023807     MOVE SQLCODE TO SQLCODE-WS                                           
023808     PERFORM DB2-STATUS-CHECK                                             
023809     .                                                                    
023810     EJECT                                                                
023811                                                                          
023812 DB2-FETCH-CRS-CUS SECTION.                                               
023813     MOVE 000100  TO GOOD-SQLCODECODES                                    
023814     EXEC SQL                                                             
023815         FETCH T01IVW-CRS-CUS                                             
023816         INTO   :WS-DAREGDAT-CUS                                          
023817              , :WS-TIREGTID-CUS                                          
023818              , :WS-IDLOPNR-CUS                                           
023819              , :WS-IDPTYP-CUS                                            
023820              , :WS-TIRP-CUS                                              
023821              , :WS-FLKLAR-CUS                                            
023822              , :WS-IV-DATA-CUS                                           
023823     END-EXEC                                                             
023824                                                                          
023825     MOVE SQLCODE TO SQLCODE-WS                                           
023826     PERFORM DB2-STATUS-CHECK                                             
023827     .                                                                    
023828     EJECT                                                                
023829                                                                          
023830 DB2-CLOSE-CRS-CUS SECTION.                                               
023831     EXEC SQL                                                             
023832         CLOSE T01IVW-CRS-CUS                                             
023833     END-EXEC                                                             
023834     .                                                                    
023835     EJECT                                                                
023836                                                                          
023837 DB2-COMMIT-WORK SECTION.                                                 
023838     EXEC SQL COMMIT WORK                                                 
023839     END-EXEC                                                             
023840     .                                                                    
023841     EJECT                                                                
023842                                                                          
023843 DB2-STATUS-CHECK SECTION.                                                
023844     SET SQLCODE-IX TO 1                                                  
023845     SEARCH GOOD-SQLCODE                                                  
023846       AT END                                                             
023847          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
023848          DELIMITED BY SIZE INTO ERROR-TEXT                               
023849          CALL ABEND USING RKOD-ABEND-DB2                                 
023850       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
023860     END-SEARCH                                                           
023900     .                                                                    
