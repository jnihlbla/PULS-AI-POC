000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W1143800.                                    
000300 AUTHOR.                     P. DAHLÖF.                                   
000400 DATE-WRITTEN.               JULETID 1988.                                
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700                                                                          
000800     FUNKTION.                                                            
000900                                                                          
001000     LÄSER WLARTG MED SB                                                  
001100     TESTAR OM TIFINLEV = 999999                                          
001200     SAMT EV SKRIVER EN UTFIL                                             
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SELECT W11438 ASSIGN TO W11438D1.                                    
001800 DATA DIVISION.                                                           
001900 FILE SECTION.                                                            
002000 FD     W11438                                                            
002100        RECORDING F                                                       
002200        LABEL RECORD STANDARD                                             
002300        BLOCK CONTAINS 0.                                                 
002400*01  POST  -COPY W11438  -L -PRE UT-                                      
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800     SKIP2                                                                
002801*    -COPY WY2000W2                                                       
002810     SKIP3                                                                
002900*- - - - - - - - - - - - - - - - - KONSTANTER.                            
003000 77  JA                      PIC X       VALUE 'J'.                       
003100 77  NEJ                     PIC X       VALUE 'N'.                       
003200 77  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                      
003300 77  SKRIV-INDX              PIC S9(9)   VALUE ZERO COMP SYNC.            
003400                                                                          
003500 01  WS-IDPROENH-X           PIC X(8)    VALUE ZERO.                      
003600 01  WS-IDPROENH REDEFINES  WS-IDPROENH-X PIC 9(8).                       
003700*- - - - - - - - - - - - - - - - - NYCKLAR TILL DLI                       
003800                                                                          
003900 01  W-IDARTNR-X.                                                         
004000    03  W-IDARTNR            PIC S9(9)       VALUE ZERO COMP-3.           
004100                                                                          
004200 01  W-KDSEGKEY-X            PIC  X(1)       VALUE '1'.                   
004300*- - - - - - - - - - - - - - - - - DATUM-FAELT                            
004400*01  -COPY WDATAREAC0                                                     
004600 77  WS-TIFINLV-AAVVD        PIC 9(5)    VALUE ZERO.                      
004700 77  WS-TIFINLV-AAVV         PIC 9(4)    VALUE ZERO.                      
004800 77  DAGENS-AAVVD            PIC 9(5) VALUE ZERO.                         
004900 01  WS-TIFINLV-AAVVD-X.                                                  
005000   03  WS-TIFINLV-AAVV-X     PIC X(4) VALUE SPACE.                        
005100   03  WS-TIFINLV-D-X        PIC X(1) VALUE SPACE.                        
005200 01  W009VADD-DATUM          PIC 9(5) VALUE ZERO  COMP-3.                 
005300 01  W009VADD-ANTAL          PIC 9(3) VALUE ZERO  COMP-3.                 
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
005700     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
005800     03  FELLOG              PIC X(8)    VALUE 'FELLOG '.                 
006000     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
006100     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
006200     SKIP2                                                                
006300*- - - - - - - - - - - - - - - - - UTFIL.                                 
006400*                                                                         
006500*01  AREA  -COPY W11438  -PRE UT-                                         
006700     EJECT                                                                
006800*                                                                         
006900*- - - - - - - - - - - - - - - - - ARBETSAREOR  IMS-SEKTIONEN.            
007000*                                                                         
007100 01    IMS-WS.                                                            
007200   03  FILLER           PIC X(8)   VALUE 'IMS-WS  '.                      
007300*                                                                         
007400*- - - - - - - - - - - - - - - - - STATUSKOD FRÅN IMS                     
007500   03    STATUS-WS      PIC XX.                                           
007600     88  SEGMENT-FINNS             VALUE '  '.                            
007700     88  SEGMENT-SAKNAS            VALUE 'GE'.                            
007800     88  SEGMENT-SLUT              VALUE 'GB'.                            
007900*                                                                         
008000   03    SSA1           PIC X(64).                                        
008100   03    SSA2           PIC X(64).                                        
008200   03    SSA3           PIC X(64).                                        
008300*                                                                         
008400   03    GODK-STATUSKODER.                                                
008500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008600     EJECT                                                                
008700*- - - - - - - - - - - - - - - - - PARAMETERAR TILL POSTSUM               
008800*01      -COPY W0005 -PRE POSTSUM-.                                       
009000     EJECT                                                                
009100*- - - - - - - - - - - - - - - - - IMS-CALL FUNKTIONER                    
009200*01      -COPY W0003.                                                     
009400     EJECT                                                                
009500*- - - - - - - - - - - - - - - - - IMS - COMM-AREA                        
009600 01  DLI-IO-AREA.                                                         
009700     03 IO-AREA             PIC X(900).                                   
009800*                                                                         
009900*    03 AREA  -COPY WDD201  -PRE ARTG01-  -RED IO-AREA.                   
010100     EJECT                                                                
010200*    03       -COPY WDK601                -RED IO-AREA.                   
010400     EJECT                                                                
010500*    03       -COPY WDK611                -RED IO-AREA.                   
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900     SKIP2                                                                
011000*01  -COPY W0008 -PRE WDD2-                                               
011200          05  FILLER         PIC X.                                       
011300*01  -COPY W0008 -PRE ARTC-                                               
011500          05  FILLER         PIC X.                                       
011600     EJECT                                                                
011700 PROCEDURE DIVISION  USING WDD2-PCB ARTC-PCB.                             
011800     ENTRY 'DLITCBL' USING WDD2-PCB ARTC-PCB.                             
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012100     PERFORM IMS-GN-WDD2                                                  
012200     PERFORM UNTIL SEGMENT-SLUT                                           
012300     OR SKRIV-INDX > +200                                                 
012400        IF WDD2-SEG-NAME-FB = 'WDD201  '                                  
012500           IF ARTG01-ART-DAFINLEV = 99999999                              
012600              PERFORM B-TEST-OM-INGAENDE-ART-EV-SK-F                      
012700           END-IF                                                         
012800        END-IF                                                            
012900        PERFORM IMS-GN-WDD2                                               
013000     END-PERFORM                                                          
013100     PERFORM Z-FINIT                                                      
013200     MOVE ZERO               TO RETURN-CODE                               
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700     SKIP2                                                                
013800     OPEN OUTPUT W11438                                                   
013900     MOVE +1                 TO SKRIV-INDX                                
014000     MOVE 'W11438'           TO POSTSUM-PROGNAMN                          
014100     MOVE 'IDAG  '           TO DAT-KDDATFORM                             
014200     PERFORM S97-CALL-WDATKONV                                            
014300                                                                          
014400     IF DAT-KDSVAR-OK                                                     
014500        MOVE DAT-TIAAVVD     TO DAGENS-AAVVD                              
014600     END-IF                                                               
014700     .                                                                    
014800     EJECT                                                                
014900 B-TEST-OM-INGAENDE-ART-EV-SK-F   SECTION.                                
015000     SKIP1                                                                
015100     MOVE ARTG01-ART-IDARTNR         TO WS-IDARTNR                        
015200                                         W-IDARTNR                        
015300     PERFORM IMS-GU-ARTC11                                                
015400     IF SEGMENT-FINNS                                                     
015500        IF CLAG-IDPROENH(1) =  SPACE OR ZERO                              
015600           IF CLAG-IDPROENH(2) = SPACE OR ZERO                            
015610              IF CLAG-IDPROENH(3) = SPACE OR ZERO                         
015620                 CONTINUE                                                 
015700              ELSE                                                        
015800                 MOVE CLAG-IDPROENH(3) TO WS-IDPROENH-X                   
015900                 INSPECT WS-IDPROENH-X REPLACING LEADING SPACE BY         
015901                    ZERO                                                  
015902                 IF WS-IDPROENH-X NUMERIC                                 
015903                    MOVE WS-IDPROENH          TO W-IDARTNR                
015906                        PERFORM IMS-GU-ARTC01                             
015907                                                                          
015908                        IF SEGMENT-FINNS                                  
015909                           IF ART-TIFINLV = 99999 OR ZERO                 
015910                              CONTINUE                                    
015911                           ELSE                                           
015912                            PERFORM BA-FIXA-TIFINLV-OCH-SKRIV-FIL         
015913                           END-IF                                         
015914                        END-IF                                            
015915                                                                          
015916                  END-IF                                                  
015917              END-IF                                                      
015918           ELSE                                                           
015919              MOVE CLAG-IDPROENH(2) TO WS-IDPROENH-X                      
015920              INSPECT WS-IDPROENH-X REPLACING LEADING SPACE BY            
015921                 ZERO                                                     
015922              IF WS-IDPROENH-X NUMERIC                                    
015923                 MOVE WS-IDPROENH TO W-IDARTNR                            
015924                     PERFORM IMS-GU-ARTC01                                
015927                                                                          
015928                     IF SEGMENT-FINNS                                     
015929                        IF ART-TIFINLV = 99999 OR ZERO                    
015930                           CONTINUE                                       
015931                        ELSE                                              
015932                           PERFORM BA-FIXA-TIFINLV-OCH-SKRIV-FIL          
015933                        END-IF                                            
015934                     END-IF                                               
015935              END-IF                                                      
015936           END-IF                                                         
015937        ELSE                                                              
015938           MOVE CLAG-IDPROENH(1) TO WS-IDPROENH-X                         
015939           INSPECT WS-IDPROENH-X REPLACING LEADING SPACE BY ZERO          
015940           IF WS-IDPROENH-X NUMERIC                                       
015941              MOVE WS-IDPROENH TO W-IDARTNR                               
015942              PERFORM IMS-GU-ARTC01                                       
015945                                                                          
015946              IF SEGMENT-FINNS                                            
015947                 IF ART-TIFINLV = 99999 OR ZERO                           
015948                    CONTINUE                                              
015949                 ELSE                                                     
015950                    PERFORM BA-FIXA-TIFINLV-OCH-SKRIV-FIL                 
015951                 END-IF                                                   
015952              END-IF                                                      
015953          END-IF                                                          
015960       END-IF                                                             
015970     END-IF                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 BA-FIXA-TIFINLV-OCH-SKRIV-FIL  SECTION.                                  
018000     SKIP1                                                                
018100     MOVE ART-TIFINLV             TO WS-TIFINLV-AAVVD                     
018101     MOVE WS-TIFINLV-AAVVD   TO TMP1-YYWWD                                
018102     MOVE DAGENS-AAVVD       TO TMP2-YYWWD                                
018110     PERFORM WY2000P2                                                     
018200     IF TMP1-YYWWD > TMP2-YYWWD                                           
018300        CONTINUE                                                          
018400     ELSE                                                                 
018500        MOVE DAGENS-AAVVD            TO WS-TIFINLV-AAVVD-X                
018600        MOVE WS-TIFINLV-AAVV-X       TO W009VADD-DATUM                    
018700        MOVE +5                      TO W009VADD-ANTAL                    
018800        CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                 
018900        MOVE W009VADD-DATUM          TO WS-TIFINLV-AAVV                   
019000        MOVE WS-TIFINLV-AAVV         TO WS-TIFINLV-AAVV-X                 
019100        MOVE 1                       TO WS-TIFINLV-D-X                    
019200        MOVE WS-TIFINLV-AAVVD-X      TO WS-TIFINLV-AAVVD                  
019300     END-IF                                                               
019400     MOVE WS-IDARTNR                 TO UT-IDARTNR                        
019500     MOVE WS-TIFINLV-AAVVD           TO UT-TIFINLEV-AAVVD                 
019600                                        DAT-I-TIDATUM                     
019700     MOVE 'AAVVD '                   TO DAT-KDDATFORM                     
019800     PERFORM S97-CALL-WDATKONV                                            
019900     IF DAT-KDSVAR-OK                                                     
020000        MOVE DAT-TIAAMMDD            TO UT-TIFINLEV-AAMMDD                
020100        PERFORM S11-SKRIV-W11438                                          
020200        ADD +1                       TO SKRIV-INDX                        
020300     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 S11-SKRIV-W11438    SECTION.                                             
020700     SKIP1                                                                
020800     WRITE UT-POST FROM UT-AREA                                           
020900     MOVE 'W11438D1'          TO POSTSUM-DDNAMN2                          
021000     MOVE 'W11438'            TO POSTSUM-FDNAMN                           
021100     MOVE 'UT'                TO POSTSUM-TRANSTYP                         
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     .                                                                    
021400     EJECT                                                                
021500 S97-CALL-WDATKONV     SECTION.                                           
021600     SKIP1                                                                
021700     CALL WDATKONV USING DAT-KDDATFORM                                    
021800                         DAT-I-TIDATUM                                    
021900                         DAT-O-TIDATUM                                    
022000                         DAT-KDSVAR                                       
022100     .                                                                    
022200     EJECT                                                                
022300 Z-FINIT SECTION.                                                         
022400     SKIP2                                                                
022500     CLOSE W11438                                                         
022600     MOVE 'S'                 TO POSTSUM-OPKOD                            
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     .                                                                    
022900     EJECT                                                                
023000*- - - - - - - - - - - - - - - - - IMS SEKTION                            
023100 IMS-GN-WDD2 SECTION.                                                     
023200     SKIP1                                                                
023300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
023400     CALL CBLTDLI USING GN WDD2-PCB DLI-IO-AREA                           
023500     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
023600     PERFORM IMS-STATUS-KONTROLL                                          
023700     .                                                                    
023800     SKIP3                                                                
023900 IMS-GU-ARTC01 SECTION.                                                   
024000     SKIP1                                                                
024100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
024200             DELIMITED BY SIZE INTO SSA1                                  
024300     MOVE '  GE' TO GODK-STATUSKODER                                      
024400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
024500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUS-KONTROLL                                          
024700     .                                                                    
024800     SKIP3                                                                
024900 IMS-GU-ARTC11 SECTION.                                                   
025000     SKIP1                                                                
025100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
025200             DELIMITED BY SIZE INTO SSA1                                  
025300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
025400             DELIMITED BY SIZE INTO SSA2                                  
025600     MOVE '  GE' TO GODK-STATUSKODER                                      
025700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
025800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
025900     PERFORM IMS-STATUS-KONTROLL                                          
026000     .                                                                    
026100     SKIP3                                                                
026200 IMS-STATUS-KONTROLL SECTION.                                             
026300     SET STATUS-IX TO 1                                                   
026400     SEARCH GODK-STATUS AT END CALL FELLOG                                
026500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
026600     END-SEARCH                                                           
026700     .                                                                    
026710     EJECT                                                                
026800*    -COPY WY2000P2                                                       
