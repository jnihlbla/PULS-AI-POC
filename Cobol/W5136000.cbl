000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W5136000.                                                 
000300 AUTHOR.        CHRISTINA BRUHN.                                          
000400 DATE-WRITTEN.  FEBRUARI 1989.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        PROGRAMMET LÄSER WDH7 MED SB.                                    
001000*        SKAPAR EN FIL MED SAMTLIGA JUSTERINGAR UNDER VECKAN.             
001010*        SKAPAR EN FIL MED JUSTERINGAR UNDER VECKAN FÖR DC 11             
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*--- RAPPORTFIL:                                                          
001900                                                                          
002000     SELECT W51361                       ASSIGN TO UT-S-W51360D1.         
002010     SELECT W51360                       ASSIGN TO UT-S-W51360D2.         
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP2                                                                
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD  W51361                                                               
002700     RECORDING       F                                                    
002800     BLOCK CONTAINS 0.                                                    
002900                                                                          
003000*01  UTPOST -COPY W51358     -L.                                          
003100     EJECT                                                                
003110 FD  W51360                                                               
003120     RECORDING       F                                                    
003130     BLOCK CONTAINS 0.                                                    
003140                                                                          
003150*01  UTPOST1 -COPY W51360     -L.                                         
003160     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)  VALUE 'W5136000'.              
003500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3) VALUE +16.                     
003600                                                                          
003700 01  DYNAMISKA-SUBPROGRAM.                                                
003800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
003900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
004300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004320*    -- VALID IDDC CODES                                                  
004330*01  -COPY WWDC99                                                         
004340                                                                          
004400                                                                          
004500 01  DAGENS-TIAAVV.                                                       
004600     03  DAGENS-AA               PIC 9(2).                                
004700     03  DAGENS-VV               PIC 9(2).                                
004800     EJECT                                                                
004900*01  -COPY W0005      -PRE POSTSUM-                                       
005000 01  UT-TRANSID.                                                          
005100     03  FILLER                  PIC X(6)  VALUE 'W51360'.                
005200     03  FILLER                  PIC X(8)  VALUE 'W51360D1'.              
005300     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
005400     EJECT                                                                
005410 01  UT-TRANSID1.                                                         
005420     03  FILLER                  PIC X(6)  VALUE 'W51360'.                
005430     03  FILLER                  PIC X(8)  VALUE 'W51360D2'.              
005440     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
005450     EJECT                                                                
005500 01  DATUMKORT-ID                PIC X(6)  VALUE 'WDATUM'.                
005600*01  -COPY WDATKORT                                                       
005700     EJECT                                                                
005800*01  -COPY WDATAREA                                                       
005900     EJECT                                                                
006000 01  FILLER                      PIC X(8)    VALUE 'UT-AREOR'.            
006100                                                                          
006200*01  POST -COPY W51358   -PRE UT-                                         
006300     EJECT                                                                
006310 01  FILLER                      PIC X(8)    VALUE 'UT-AREA1'.            
006320                                                                          
006330*01  POST -COPY W51360   -PRE UT1-                                        
006340     EJECT                                                                
006341                                                                          
006350 01  DIVERSE.                                                             
006360     03  W-KDSEGKEY-X.                                                    
006370       05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                         
006380     03  W-IDARTNR-X.                                                     
006381       05  W-IDARTNR       PIC S9(9)   COMP-3.                            
006382                                                                          
006383                                                                          
006390                                                                          
006400 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
006500                                                                          
006600 01  IMS-WS.                                                              
006700                                                                          
006800     03  STATUS-WS               PIC X(2).                                
006900        88  SEGMENT-SLUT                     VALUE 'GB'.                  
007000        88  SEGMENT-FINNS                    VALUE '  ' 'GA' 'GK'.        
007100        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
007200                                                                          
007300     03  GODK-STATUSKODER.                                                
007400         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
007430     SKIP3                                                                
007440 01      SSA1            PIC X(64).                                       
007450 01      SSA2            PIC X(64).                                       
007500                                                                          
007600*01  -COPY W0003                                                          
007700     EJECT                                                                
007800 01  DLI-IO-AREA.                                                         
007900     03  IO-AREA              PIC X(130).                                 
008000     SKIP3                                                                
008100*    03  -COPY WDH701             -RED IO-AREA                            
008200     EJECT                                                                
008300*    03  -COPY WDH711             -RED IO-AREA                            
008400     EJECT                                                                
008411 01  DLI-IO-WDK601.                                                       
008412*    03  -COPY WDK601                                                     
008413     EJECT                                                                
008414 01  DLI-IO-WDK611.                                                       
008415*    03  -COPY WDK611                                                     
008470     EJECT                                                                
008500 LINKAGE SECTION.                                                         
008600     SKIP3                                                                
008700*01  -COPY W0008   -PRE WDH7-                                             
008800         05  FILLER           PIC X(1).                                   
008810*01  -COPY W0008   -PRE WDK6-                                             
008820         05  FILLER           PIC X(1).                                   
008900     EJECT                                                                
009000 PROCEDURE DIVISION USING  WDH7-PCB WDK6-PCB.                             
009100     ENTRY 'DLITCBL' USING WDH7-PCB WDK6-PCB.                             
009200                                                                          
009300     PERFORM A-INIT                                                       
009400                                                                          
009500     PERFORM IMS-GET-WDH7                                                 
009600                                                                          
009700     PERFORM UNTIL SEGMENT-SLUT                                           
009800                                                                          
009900        EVALUATE WDH7-SEG-NAME-FB                                         
010000           WHEN 'WDH701  ' MOVE INVA-IDARTNR     TO UT-IDARTNR            
010100                                                    W-IDARTNR             
010200           WHEN 'WDH711  ' PERFORM B-SKRIV-POST                           
010350                                                                          
010400        END-EVALUATE                                                      
010500                                                                          
010600        PERFORM IMS-GET-WDH7                                              
010700                                                                          
010800     END-PERFORM                                                          
010900                                                                          
011000     PERFORM Z-FINIT                                                      
011100     MOVE ZERO TO RETURN-CODE                                             
011200     GOBACK                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 A-INIT SECTION.                                                          
011600                                                                          
011700     OPEN OUTPUT W51361                                                   
011710     OPEN OUTPUT W51360                                                   
011800                                                                          
011900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012000                                                                          
012100     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
012200                                                                          
012300     MOVE D-AAR             TO DAGENS-AA                                  
012400     MOVE D-VECKA           TO DAGENS-VV                                  
012500                                                                          
012600     DISPLAY 'DAGENS ÅR            : ' DAGENS-AA                          
012700     DISPLAY 'DAGENS VECKA         : ' DAGENS-VV                          
012800     .                                                                    
012900     EJECT                                                                
013000 B-SKRIV-POST SECTION.                                                    
013100                                                                          
013110                                                                          
013111     MOVE INVH-IDDC          TO WS-IDDC                                   
013120     IF CDC OR SDC OR XDC-NON-VCC-OWNED OR NDC-JP OR NDC-AU               
013200       MOVE 'AAMMDD'         TO DAT-KDDATFORM                             
013300       MOVE INVH-DAREGDAT-CLO(3:6) TO DAT-I-TIDATUM                       
013400                                                                          
013500       CALL WDATKONV USING DAT-KDDATFORM                                  
013600                           DAT-I-TIDATUM                                  
013700                           DAT-O-TIDATUM                                  
013800                           DAT-KDSVAR                                     
013900                                                                          
014000       IF (DAT-TIAA = DAGENS-AA) AND                                      
014100          (DAT-TIVV = DAGENS-VV)                                          
014200                                                                          
014300         MOVE INVH-PRARTSTD  TO UT-PRARTSTD                               
014400         MOVE INVH-IDDC      TO UT-IDDC                                   
014500         MOVE INVH-KVJUSTKV  TO UT-KVJUSTKV                               
014600         MOVE INVH-KDJUSTYP  TO UT-KDJUSTYP                               
014700                                                                          
014800         PERFORM S01-SKRIV-W51361                                         
014801                                                                          
014820         IF CDC-SE                                                        
014830           IF INVH-KDJUSTYP NOT = +6                                      
014840             PERFORM IMS-GU-WDK601                                        
014850             IF SEGMENT-FINNS                                             
014860               PERFORM IMS-GET-WDK611                                     
014870               IF SEGMENT-FINNS                                           
014880                 PERFORM BA-SKRIV-W51360-POST                             
014890               END-IF                                                     
014891             END-IF                                                       
014892           END-IF                                                         
014893         END-IF                                                           
014900       END-IF                                                             
014910     END-IF                                                               
015000     .                                                                    
015010 BA-SKRIV-W51360-POST SECTION.                                            
015020                                                                          
015030                                                                          
015040     MOVE W-IDARTNR          TO UT1-IDARTNR                               
015050     MOVE CLAG-ADLAGOMR      TO UT1-ADLAGOMR                              
015060     MOVE INVH-KVJUSTKV      TO UT1-KVJUSTKV                              
015070     MOVE CLAG-PRARTSTD      TO UT1-PRARTSTD                              
015080     MOVE CLAG-KVLS          TO UT1-KVLS                                  
015102                                                                          
015103     PERFORM S02-SKRIV-W51360                                             
015104                                                                          
015105                                                                          
015106     .                                                                    
015110 Z-FINIT  SECTION.                                                        
015200                                                                          
015300     CLOSE W51361                                                         
015310     CLOSE W51360                                                         
015400                                                                          
015500     MOVE 'S' TO POSTSUM-OPKOD                                            
015600     CALL POSTSUM USING POSTSUM-PARM                                      
015700     .                                                                    
015800     SKIP2                                                                
015900 S01-SKRIV-W51361     SECTION.                                            
016000                                                                          
016100     WRITE UTPOST FROM UT-POST                                            
016200                                                                          
016300     MOVE UT-TRANSID      TO POSTSUM-TRANSID                              
016400     CALL POSTSUM USING POSTSUM-PARM                                      
016500     .                                                                    
016600     EJECT                                                                
016610 S02-SKRIV-W51360     SECTION.                                            
016620                                                                          
016630     WRITE UTPOST1 FROM UT1-POST                                          
016640                                                                          
016650     MOVE UT-TRANSID1     TO POSTSUM-TRANSID                              
016660     CALL POSTSUM USING POSTSUM-PARM                                      
016670     .                                                                    
016680     EJECT                                                                
016700*         * I M S  S E C T I O N                                          
016800                                                                          
016900 IMS-GET-WDH7         SECTION.                                            
017000                                                                          
017100     MOVE '  GAGKGBGAGK' TO GODK-STATUSKODER                              
017200     CALL CBLTDLI USING GN WDH7-PCB IO-AREA                               
017300     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
017400     PERFORM IMS-STATUSKONTROLL                                           
017500     .                                                                    
017600     SKIP3                                                                
017610 IMS-GU-WDK601 SECTION.                                                   
017620                                                                          
017630     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
017640     DELIMITED BY SIZE INTO SSA1                                          
017650     MOVE '  GE' TO GODK-STATUSKODER                                      
017660     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601   SSA1                  
017670     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
017680     PERFORM IMS-STATUSKONTROLL                                           
017690     .                                                                    
017691     SKIP3                                                                
017692 IMS-GET-WDK611 SECTION.                                                  
017693     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
017694          DELIMITED BY SIZE INTO SSA1                                     
017695     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
017696          DELIMITED BY SIZE INTO SSA2                                     
017697     MOVE '  GE'           TO GODK-STATUSKODER                            
017698     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
017699     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
017700     PERFORM IMS-STATUSKONTROLL                                           
017701     .                                                                    
017702     EJECT                                                                
017720 IMS-STATUSKONTROLL   SECTION.                                            
017800                                                                          
017900     SET STATUS-IX TO 1                                                   
018000     SEARCH GODK-STATUS                                                   
018100       AT END CALL FELLOG                                                 
018200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
018300     END-SEARCH                                                           
018400     .                                                                    
