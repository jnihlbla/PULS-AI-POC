000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6118C00.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   09/12/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        TO FIND OUT NEW GATE (ADINPORT) FOR ALL THE CHANGED              
000900*        IN WDT2                                                          
001000*                                                                         
001100*        THE PROGRAM READS     WDT2                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- UNLOAD OF WDK6 DATABASE                                    
002600     SELECT W01160                     ASSIGN TO W6118CD1.                
002700*          --- PART NUMBERS WITH NEW GATE                                 
002800     SELECT W6118C                     ASSIGN TO W6118CD2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W01160                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900*01  -COPY W01160      -L.                                                
004000                                                                          
004100 FD  W6118C                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400*01  RECORD -COPY W6118C01 -PRE  W6118C-  -L.                             
004500                                                                          
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W6118C00'.            
005400 77  YES                         PIC X       VALUE 'J'.                   
005500 77  NOO                         PIC X       VALUE 'N'.                   
005600 77  W-ADINPORT-201              PIC X(08)   VALUE SPACES.                
005700 77  W-ADINPORT                  PIC X(08)   VALUE SPACES.                
005800 77  W-PREV-BEFT                 PIC 9(02)   VALUE ZERO.                  
005900                                                                          
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES TODAYS-DATE.                                        
006200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  TODAYS-DATE-DAY         PIC 9(2).                                
006500     EJECT                                                                
006510 77  W-TIAAVVD               PIC 9(5)               VALUE ZERO.           
006520 01  W-TIAAVVD-ALPHA.                                                     
006530     03 W-AAVV               PIC 9(4).                                    
006540     03 W-WEEKDAY            PIC 9(1).                                    
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007110     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007200*                                                                         
007300 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W01160                       VALUE 'Y'.                   
007500                                                                          
007600*                                                                         
007700 77  WDT2-CHECK                  PIC X       VALUE 'N'.                   
007800     88  NO-CHILD-SEG                        VALUE 'Y'.                   
007900     88  CHILD-SEG                           VALUE 'N'.                   
008000                                                                          
008010*    ---PARAMETRAR TILL DATKONV                                           
008020*01  -COPY WDATAREA                                                       
008030     EJECT                                                                
008100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600     SKIP2                                                                
008700 01  ERROR-TEXT.                                                          
008800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
008900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL POSTSUM                                          
009200*                                                                         
009300*01  -COPY W0005   -PRE  POSTSUM-                                         
009400     EJECT                                                                
009500 01  W01160-AREA-START           PIC X(24)   VALUE                        
009600                                             'W01160-AREA-START'.         
009700     SKIP2                                                                
009800                                                                          
009900*01  AREA -COPY W01160 -PRE W01160-                                       
010000     EJECT                                                                
010100 01  W6118C-AREA-START           PIC X(24)   VALUE                        
010200                                 'W6118C-AREA-START  '.                   
010300     SKIP2                                                                
010400                                                                          
010500*01  AREA -COPY W6118C01     -PRE W6118C-                                 
010600     EJECT                                                                
010900*    --- AREAS FOR IMS-SECTIONS                                           
011000*                                                                         
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 01  KEYS-FOR-DLI.                                                        
011501     03  W-IDDC-X.                                                        
011601         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
012210     03  W-WDT211KY-MIN-X.                                                
012300         05  W-ADLAGOMR-MIN        PIC S9(3)    VALUE ZERO COMP-3.        
012301         05  W-TIAAVV1-FOM-MIN     PIC S9(5)    VALUE ZERO COMP-3.        
012302     03  W-WDT211KY-MAX-X.                                                
012303         05  W-ADLAGOMR-MAX        PIC S9(3)    VALUE ZERO COMP-3.        
012310         05  W-TIAAVV1-FOM-MAX     PIC S9(5)    VALUE 9999 COMP-3.        
012320     03  W-WDT212KY-MIN-X.                                                
012330         05  W-BEFT-MIN            PIC 9(02)    VALUE ZERO.               
012340         05  W-TIAAVV2-FOM-MIN     PIC S9(5)    VALUE ZERO COMP-3.        
012350     03  W-WDT212KY-MAX-X.                                                
012360         05  W-BEFT-MAX            PIC 9(02)    VALUE ZERO.               
012370         05  W-TIAAVV2-FOM-MAX     PIC S9(5)    VALUE 9999 COMP-3.        
012380     03  W-WDT213KY-MIN-X.                                                
012390         05  W-IDARTNR-MIN         PIC S9(9)    VALUE ZERO COMP-3.        
012400         05  W-TIAAVV3-FOM-MIN     PIC S9(5)    VALUE ZERO COMP-3.        
012500     03  W-WDT213KY-MAX-X.                                                
012600         05  W-IDARTNR-MAX         PIC S9(9)    VALUE ZERO COMP-3.        
012700         05  W-TIAAVV3-FOM-MAX     PIC S9(5)    VALUE 9999 COMP-3.        
012900     SKIP2                                                                
013000*    --- STATUS-KOD FRÅN IMS                                              
013100 01  STATUS-WS                   PIC XX.                                  
013200     88  SEGMENT-FOUND                       VALUE '  '.                  
013300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013500     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
013600     SKIP2                                                                
013700 01  GOOD-STATUSCODES.                                                    
013800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200     EJECT                                                                
014300*    --- IMS FUNCTION CODES                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT201'.                      
014800 01  DLI-IO-WDT201.                                                       
014900*    03  -COPY WDT201                                                     
015000     EJECT                                                                
015100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT211'.                      
015200 01  DLI-IO-WDT211.                                                       
015300*    03  -COPY WDT211                                                     
015400     EJECT                                                                
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT212'.                      
015600 01  DLI-IO-WDT212.                                                       
015700*    03  -COPY WDT212                                                     
015801     EJECT                                                                
015901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT213'.                      
016001 01  DLI-IO-WDT213.                                                       
016101*    03  -COPY WDT213                                                     
016500     SKIP3                                                                
016600 LINKAGE SECTION.                                                         
016700                                                                          
016800*01  -COPY W0008  -PRE WDT2-                                              
016900     05  FILLER                  PIC X.                                   
017000                                                                          
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING WDT2-PCB.                                      
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING WDT2-PCB.                                      
017600                                                                          
017700     PERFORM A-INIT                                                       
017800                                                                          
017901     MOVE '11'        TO W-IDDC                                           
018000     PERFORM IMS-GU-WDT201                                                
018100     PERFORM S01-READ-W01160                                              
018404                                                                          
018502     MOVE W01160-CLAG-ADLAGOMR TO W-ADLAGOMR-MIN                          
018503                                  W-ADLAGOMR-MAX                          
018803     PERFORM IMS-GNP-WDT211                                               
018804     IF SEGMENT-MISSING OR SEGMENT-NOMORE                                 
018805        MOVE SPACES TO GLO-ADINPORT-LO                                    
019800     END-IF                                                               
019900                                                                          
019902     MOVE W01160-CLAG-BEFT TO W-BEFT-MIN                                  
019903                              W-BEFT-MAX                                  
020203     PERFORM IMS-GNP-WDT212                                               
020204     IF SEGMENT-MISSING OR SEGMENT-NOMORE                                 
020205        MOVE SPACES TO GFT-ADINPORT-FT                                    
020206     END-IF                                                               
020901                                                                          
021002     MOVE W01160-CLAG-IDARTNR TO W-IDARTNR-MIN                            
021003                                 W-IDARTNR-MAX                            
021301     PERFORM IMS-GNP-WDT213                                               
021302     IF SEGMENT-MISSING OR SEGMENT-NOMORE                                 
021303        MOVE SPACES TO GART-ADINPORT-ART                                  
021304     END-IF                                                               
021701                                                                          
021801     PERFORM B-GATE-CHANGE                                                
021901     PERFORM S01-READ-W01160                                              
022001     PERFORM UNTIL END-OF-W01160                                          
022102         IF W-ADLAGOMR-MIN NOT = W01160-CLAG-ADLAGOMR                     
022602            MOVE W01160-CLAG-ADLAGOMR TO W-ADLAGOMR-MIN                   
022603                                         W-ADLAGOMR-MAX                   
022901            PERFORM IMS-GNP-WDT211                                        
022902            IF SEGMENT-MISSING OR SEGMENT-NOMORE                          
022903               MOVE SPACES TO GLO-ADINPORT-LO                             
022904            END-IF                                                        
023301         END-IF                                                           
023401                                                                          
024002         IF W-BEFT-MIN NOT = W01160-CLAG-BEFT                             
025202            MOVE W01160-CLAG-BEFT TO W-BEFT-MIN                           
025203                                     W-BEFT-MAX                           
025501            PERFORM IMS-GNP-WDT212                                        
025502            IF SEGMENT-MISSING OR SEGMENT-NOMORE                          
025503               MOVE SPACES TO GFT-ADINPORT-FT                             
025504            END-IF                                                        
025901          END-IF                                                          
026001                                                                          
026202         MOVE W01160-CLAG-IDARTNR TO W-IDARTNR-MIN                        
026203                                     W-IDARTNR-MAX                        
026501         PERFORM IMS-GNP-WDT213                                           
026502         IF SEGMENT-MISSING OR SEGMENT-NOMORE                             
026503            MOVE SPACES TO GART-ADINPORT-ART                              
026504         END-IF                                                           
026901                                                                          
027001         PERFORM B-GATE-CHANGE                                            
027101         PERFORM S01-READ-W01160                                          
027201                                                                          
027300     END-PERFORM                                                          
027400                                                                          
027500     PERFORM Z-FINIT                                                      
027600                                                                          
027700     MOVE ZERO TO RETURN-CODE                                             
027800     GOBACK                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 A-INIT SECTION.                                                          
028200     OPEN INPUT  W01160                                                   
028300     OPEN OUTPUT W6118C                                                   
028500                                                                          
028600     ACCEPT TODAYS-DATE  FROM DATE                                        
028610     MOVE TODAYS-DATE TO DAT-I-TIDATUM                                    
028620     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
028630                                                                          
028640     CALL WDATKONV USING DAT-KDDATFORM                                    
028650                         DAT-I-TIDATUM                                    
028660                         DAT-O-TIDATUM                                    
028670                         DAT-KDSVAR                                       
028680                                                                          
028690     IF DAT-KDSVAR = SPACE                                                
028691       MOVE DAT-TIAAVVD    TO W-TIAAVVD                                   
028692       MOVE W-TIAAVVD      TO W-TIAAVVD-ALPHA                             
028696       IF W-WEEKDAY = 7                                                   
028697          ADD 1 TO W-AAVV                                                 
028698       ELSE                                                               
028699          CONTINUE                                                        
028700       END-IF                                                             
028701       MOVE W-AAVV TO W-TIAAVV1-FOM-MAX                                   
028702                      W-TIAAVV2-FOM-MAX                                   
028703                      W-TIAAVV3-FOM-MAX                                   
028704       MOVE ZEROS  TO W-TIAAVV1-FOM-MIN                                   
028705                      W-TIAAVV2-FOM-MIN                                   
028706                      W-TIAAVV3-FOM-MIN                                   
028707     ELSE                                                                 
028708       MOVE 'ERROR FROM WDATKONV' TO ERROR-TEXT-STR                       
028709       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
028710     END-IF                                                               
028720     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028800     .                                                                    
028900     EJECT                                                                
029001                                                                          
029101 B-GATE-CHANGE SECTION.                                                   
029201     MOVE SPACE     TO W-ADINPORT                                         
029301     IF GART-ADINPORT-ART NOT = SPACE                                     
029401        MOVE GART-ADINPORT-ART TO W-ADINPORT                              
029501     ELSE                                                                 
029502        IF GFT-ADINPORT-FT NOT = SPACE                                    
029503           MOVE GFT-ADINPORT-FT TO W-ADINPORT                             
029504        ELSE                                                              
029601          IF GLO-ADINPORT-LO NOT = SPACE                                  
029701             MOVE GLO-ADINPORT-LO TO W-ADINPORT                           
029801          ELSE                                                            
029901             MOVE GDC-ADINPORT TO W-ADINPORT                              
030001          END-IF                                                          
030101        END-IF                                                            
030201     END-IF                                                               
030302     IF W01160-CLAG-ADINPORT NOT = W-ADINPORT                             
030402        MOVE W01160-CLAG-IDARTNR TO W6118C-IDARTNR                        
030501        MOVE W-ADINPORT          TO W6118C-ADINPORT                       
030601        PERFORM S11-WRITE-W6118C                                          
030701     END-IF                                                               
030801                                                                          
030901     .                                                                    
031001     EJECT                                                                
031100 Z-FINIT SECTION.                                                         
031200     CLOSE W01160                                                         
031300           W6118C                                                         
031500     MOVE 'S' TO POSTSUM-OPKOD                                            
031600     CALL POSTSUM USING POSTSUM-PARM                                      
031700     .                                                                    
031800     EJECT                                                                
031900** READ INPUT FILE W01160                                                 
032000 S01-READ-W01160  SECTION.                                                
032100     SKIP2                                                                
032200     READ W01160 INTO W01160-AREA                                         
032300     AT END                                                               
032400        SET END-OF-W01160 TO TRUE                                         
032500                                                                          
032600     NOT AT END                                                           
032700        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
032800        MOVE 'W6118CD1' TO POSTSUM-DDNAMN2                                
032900        MOVE SPACE      TO POSTSUM-TRANSTYP                               
033000        CALL POSTSUM USING POSTSUM-PARM                                   
033100     END-READ                                                             
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033500 S11-WRITE-W6118C SECTION.                                                
033600     WRITE W6118C-RECORD FROM W6118C-AREA                                 
033700                                                                          
033800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
033900     MOVE 'W6118C'   TO POSTSUM-FDNAMN                                    
034000     MOVE 'W6118CD2' TO POSTSUM-DDNAMN2                                   
034100     CALL POSTSUM USING POSTSUM-PARM                                      
034200     .                                                                    
034300     EJECT                                                                
035300* --- IMS SECTIONS  ---                                                   
035400     EJECT                                                                
035500 IMS-GU-WDT201 SECTION.                                                   
035601     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
035701          DELIMITED BY SIZE INTO SSA1                                     
035801     MOVE '  ' TO GOOD-STATUSCODES                                        
035900     CALL CBLTDLI USING GU WDT2-PCB DLI-IO-WDT201 SSA1                    
036000     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
036100     PERFORM IMS-STATUSCHECK                                              
036200     .                                                                    
036300     EJECT                                                                
036401 IMS-GNP-WDT211  SECTION.                                                 
036402     STRING 'WDT211  *F(WDT211KY>=' W-WDT211KY-MIN-X                      
036403                      '&WDT211KY<=' W-WDT211KY-MAX-X ')'                  
036801          DELIMITED BY SIZE INTO SSA1                                     
036901     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
037001     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT211 SSA1                   
037101                                                                          
037201     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
037301     PERFORM IMS-STATUSCHECK                                              
037401     .                                                                    
037501     SKIP3                                                                
037601 IMS-GNP-WDT212  SECTION.                                                 
037602     STRING 'WDT212  *F(WDT212KY>=' W-WDT212KY-MIN-X                      
037603                      '&WDT212KY<=' W-WDT212KY-MAX-X ')'                  
038001          DELIMITED BY SIZE INTO SSA1                                     
038101     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
038201     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT212 SSA1                   
038301                                                                          
038401     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
038501     PERFORM IMS-STATUSCHECK                                              
038601     .                                                                    
038701     SKIP3                                                                
038801 IMS-GNP-WDT213  SECTION.                                                 
038901     STRING 'WDT213  *F(WDT213KY>=' W-WDT213KY-MIN-X                      
039001                      '&WDT213KY<=' W-WDT213KY-MAX-X ')'                  
039201          DELIMITED BY SIZE INTO SSA1                                     
039301     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
039401     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
039501                                                                          
039601     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
039701     PERFORM IMS-STATUSCHECK                                              
039801     .                                                                    
039901     SKIP3                                                                
043500 IMS-STATUSCHECK SECTION.                                                 
043600     SET STATUS-IX TO 1                                                   
043700     SEARCH GOOD-STATUS                                                   
043800       AT END                                                             
043900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044000           DELIMITED BY SIZE INTO ERROR-TEXT                              
044100         DISPLAY ERROR-TEXT                                               
044200         CALL FELLOG                                                      
044300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
044400         CONTINUE                                                         
045000     END-SEARCH                                                           
050000     .                                                                    
