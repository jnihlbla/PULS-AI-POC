000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1140500.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   JANUARI 1989.                                            
000500     REMARKS.                                                             
000600******************************************************************        
000700*                                                                *        
000800*                   N Y P O N - B A T C H                        *        
000900*                                                                *        
001000* PROGRAMMET ÄR ETT SB-PGM SOM LÄSER ALLA ARTIKLAR PÅ NYPON-     *        
001100* BASEN. OM ARTIKEL UPPFYLLER BEVAKNINGSVILLKOREN SKRIVS         *        
001200* ARTIKELN PÅ UTFIL.                                             *        
001300*                                                                *        
001400******************************************************************        
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200     SELECT  W11406-UTFIL             ASSIGN TO    W11405D1.              
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W11406-UTFIL                                                         
002900     LABEL RECORD STANDARD                                                
003000     RECORDING      F                                                     
003100     BLOCK CONTAINS 0.                                                    
003200     SKIP2                                                                
003300 01  W11406-UTPOST.                                                       
003400*03  -COPY  W11406 -L.                                                    
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003701*    -COPY WY2000W3                                                       
003702     SKIP3                                                                
003703*    -COPY WY2000W1                                                       
003710     SKIP3                                                                
003800*                                                                         
003900******************************************************************        
004000*    W O R K I N G  S T O R A G E  S E C T I O N                 *        
004100******************************************************************        
004200*                                                                         
004300 77  PROGRAM-NAMN                 PIC X(8)     VALUE 'W11405'.            
004400 77  JA                           PIC X        VALUE 'J'.                 
004500 77  NEJ                          PIC X        VALUE 'N'.                 
004600 77  RAKNARE                      PIC 9(4)     VALUE ZERO.                
004700 77  WS-RITNINGSDATUM             PIC 9(6)     VALUE ZERO.                
004800     EJECT                                                                
004900*                                                                         
005000******************************************************************        
005100*    S W I T C H A R                                             *        
005200******************************************************************        
005300*                                                                         
005400 01  DATUM-SW                     PIC X.                                  
005500     88  DAGENS-DATUM-OK                     VALUE 'J'.                   
005600     88  DAGENS-DATUM-FEL                    VALUE 'N'.                   
005700 01  SWITCHAR.                                                            
005800     05  SW-SKRIV-ARTIKELN        PIC X(01)  VALUE 'N'.                   
005900*                                                                         
006000******************************************************************        
006100*    D I V E R S E  S P A R F Ä L T                              *        
006200******************************************************************        
006300*                                                                         
006400 01  SPAR-FALT.                                                           
006500     05  SPAR-DAGENS-DATUM1   PIC 9(06)    VALUE ZERO.                    
006600                                                                          
006700     05  SPAR-DAGENS-DATUM2.                                              
006800         10 SPAR-DAGENS-AA    PIC 9(02)    VALUE ZERO.                    
006900         10 SPAR-DAGENS-VV    PIC 9(02)    VALUE ZERO.                    
007000     05  SPAR-DAGENS-DATUM-R  REDEFINES  SPAR-DAGENS-DATUM2.              
007100         10 SPAR-DAGENS-AAVV  PIC 9(04).                                  
007200                                                                          
007300     05  SPAR-ARTIKEL-DATUM.                                              
007400         10 SPAR-ARTIKEL-AA   PIC 9(02)    VALUE ZERO.                    
007500         10 SPAR-ARTIKEL-VV   PIC 9(02)    VALUE ZERO.                    
007600     05  SPAR-ARTIKEL-DATUM-R  REDEFINES  SPAR-ARTIKEL-DATUM.             
007700         10 SPAR-ARTIKEL-AAVV PIC 9(04).                                  
007800                                                                          
007900     05  SPAR-W009VADD-DATUM  PIC S9(5)    VALUE ZERO COMP-3.             
008000                                                                          
008100     05  SPAR-W009VADD-ANTAL  PIC S9(3)    VALUE ZERO COMP-3.             
008200                                                                          
008300     EJECT                                                                
008400*                                                                         
008500******************************************************************        
008600*    D Y N A M I S K A  S U B P R O G R A M                      *        
008700******************************************************************        
008800*                                                                         
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000  03 CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.                 
009100  03 FELLOG                  PIC X(8)    VALUE 'FELLOG '.                 
009200  03 WDATKONV                PIC X(8)    VALUE 'WDATKONV'.                
009400  03 W009VADD                PIC X(8)    VALUE 'W009VADD'.                
009500  03 POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.                
009600     EJECT                                                                
009700*                                                                         
009800******************************************************************        
009900*    P O S T S U M - C O P Y T E X T                             *        
010000******************************************************************        
010100*                                                                         
010200*01          -COPY W0005 -PRE POSTSUM-                                    
010400     EJECT                                                                
010500*                                                                         
010600******************************************************************        
010700*    U T P O S T - A  R E A                                      *        
010800******************************************************************        
010900*                                                                         
011000*01  AREA  -COPY W11406       -PRE UT-                                    
011200     EJECT                                                                
011300******************************************************************        
011400*    D A T K O N V - C O P Y T E X T                             *        
011500******************************************************************        
011600*                                                                         
011700*01          -COPY WDATAREA                                               
011900     EJECT                                                                
012000*                                                                         
012100******************************************************************        
012200*    N Y C K L A R  T I L L  D L I                               *        
012300******************************************************************        
012400*                                                                         
012500 01  FILLER                  PIC X(11) VALUE 'DLI-NYCKLAR'.               
012600                                                                          
012700 01  NYCKLAR-TILL-DLI.                                                    
012800     05  W-IDARTNR-X.                                                     
012900         10  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
013000                                                                          
013400     05  W-1131KEY-X.                                                     
013500         10  FILLER          PIC X(04)  VALUE '1131'.                     
013600         10  W-KDPRODSL      PIC S9(3)  VALUE ZERO COMP-3.                
013700         10  FILLER          PIC X(24)  VALUE LOW-VALUE.                  
013800                                                                          
013820     05  W-1132KEY-X.                                                     
013821         10  W-IDPROJK       PIC X(04)  VALUE SPACE.                      
013822         10  W-IDPROJOBJ     PIC X(04)  VALUE SPACE.                      
013823         10  W-IDPROJ        PIC X(04)  VALUE SPACE.                      
013850         10  FILLER          PIC X(03)  VALUE LOW-VALUE.                  
013860                                                                          
013900     EJECT                                                                
014000*                                                                         
014100******************************************************************        
014200*    A R B E T S A R E O R  T I L L  I M S                       *        
014300******************************************************************        
014400*                                                                         
014500 01  SSA1                    PIC X(64).                                   
014600 01  SSA2                    PIC X(64).                                   
014700                                                                          
014800 01  STATUS-WS               PIC X(2).                                    
014900     88 SEGMENT-FINNS                  VALUE '  '.                        
015000     88 SEGMENT-SAKNAS                 VALUE 'GE'.                        
015100     88 SEGMENT-SLUT                   VALUE 'GB'.                        
015200                                                                          
015300 01  GODK-STATUSKODER.                                                    
015400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
015500                                                                          
015600     EJECT                                                                
015700*                                                                         
015800******************************************************************        
015900*    I M S  F U N K T I O N S K O D E R                          *        
016000******************************************************************        
016100*                                                                         
016200*01  -COPY W0003                                                          
016400     EJECT                                                                
016500*                                                                         
016600******************************************************************        
016700*    D L I  I N P U T - O U T P U T A R E A      *1*             *        
016800******************************************************************        
016900*                                                                         
017000 01  FILLER                  PIC X(8)  VALUE 'DLI-IO-1'.                  
017100                                                                          
017200 01  DLI-IO-AREA1            PIC X(550).                                  
017300     EJECT                                                                
017400*01  WDD201      -COPY WDD201     -PRE WDD201- -RED DLI-IO-AREA1          
017600     EJECT                                                                
017700*                                                                         
017800******************************************************************        
017900*    D L I  I N P U T - O U T P U T A R E A      *2*             *        
018000******************************************************************        
018100*                                                                         
018200 01  FILLER                  PIC X(8)  VALUE 'DLI-IO-2'.                  
018300                                                                          
018400 01  DLI-IO-AREA2            PIC X(110).                                  
018500     EJECT                                                                
018600*01  ARTC        -COPY WDK601                  -RED DLI-IO-AREA2          
018800     EJECT                                                                
018900*01  WDGX1132    -COPY WDGX1132 -PRE WLXXAQ- -RED DLI-IO-AREA2            
019100     EJECT                                                                
019200*                                                                         
019300******************************************************************        
019400*    L I N K A G E  S E C T I O N                                *        
019500******************************************************************        
019600*                                                                         
019700 LINKAGE SECTION.                                                         
019800                                                                          
019900*01  -COPY W0008  -PRE WDD2-                                              
020100   05  FILLER             PIC X(2).                                       
020200     EJECT                                                                
020300*01  -COPY W0008  -PRE WLARTC-                                            
020500   05  FILLER             PIC X(2).                                       
020600     EJECT                                                                
020700*01  -COPY W0008  -PRE WLXXAQ-                                            
020900   05  FILLER             PIC X(2).                                       
021000     EJECT                                                                
021100 PROCEDURE DIVISION  USING WDD2-PCB WLARTC-PCB WLXXAQ-PCB.                
021200     ENTRY 'DLITCBL' USING WDD2-PCB WLARTC-PCB WLXXAQ-PCB.                
021300     PERFORM A-INIT                                                       
021400                                                                          
021500     PERFORM IMS-GN-WDD2                                                  
021600     PERFORM UNTIL SEGMENT-SLUT                                           
021700        EVALUATE WDD2-SEG-NAME-FB                                         
021800           WHEN 'WDD201  '                                                
021900              IF WDD201-ART-IDARTNR > 99999999                            
022000                 CONTINUE                                                 
022100              ELSE                                                        
022200                 IF WDD201-ART-TIREGDAT > ZERO                            
022300                    IF (WDD201-ART-FLBERQ = JA)                           
022400                    OR ((WDD201-ART-TIRITC = ZERO OR 999999) AND          
022500                        (WDD201-ART-TIRITP = ZERO OR 999999))             
022600*                       *****************************************         
022700*                       * TIRIT = 999999 'OMHÄNDERTAS' I W11402 *         
022800*                       *****************************************         
022900                    OR (WDD201-ART-KDRESBED = 'R' OR 'E'                  
023000                                                  OR '-')                 
023100                    OR (WDD201-ART-KDARTUTG = 'U' OR 'B')                 
023200                       CONTINUE                                           
023300                    ELSE                                                  
023400                       IF WDD201-ART-IDPROENH = ZERO OR SPACE             
023500                          MOVE NEJ TO SW-SKRIV-ARTIKELN                   
023600                                                                          
023700                          IF WDD201-ART-KDRESBED = 'U'                    
023800                            MOVE WDD201-ART-IDARTNR TO                    
023900                                                  W-IDARTNR               
024000                            PERFORM IMS-GU-WLARTC01                       
024100                            IF SEGMENT-FINNS                              
024101                              MOVE ART-TIREGDAT                           
024102                                              TO TMP1-YYMMDD              
024103                              MOVE WDD201-ART-TIREGDAT                    
024104                                              TO TMP2-YYMMDD              
024110                              PERFORM WY2000P1                            
024200                              IF TMP1-YYMMDD > TMP2-YYMMDD                
024400                                CONTINUE                                  
024500                              ELSE                                        
024600                                PERFORM B-TEST-DATUM-IDPROJK              
024700                              END-IF                                      
024800                            ELSE                                          
024900                              IF WDD201-ART-TIRITC = ZERO                 
025000                                IF WDD201-ART-TIRITP = ZERO               
025100                                  MOVE ZERO TO SPAR-ARTIKEL-AAVV          
025200                                ELSE                                      
025300                                  MOVE WDD201-ART-TIRITP                  
025400                                  TO   WS-RITNINGSDATUM                   
025500                                  PERFORM D-OMV-RITN-DATUM                
025600                                END-IF                                    
025700                              ELSE                                        
025800                                MOVE WDD201-ART-TIRITC                    
025900                                TO   WS-RITNINGSDATUM                     
026000                                PERFORM D-OMV-RITN-DATUM                  
026100                              END-IF                                      
026101                              MOVE SPAR-ARTIKEL-AAVV TO TMP1-YYWW         
026102                              MOVE SPAR-DAGENS-AAVV  TO TMP2-YYWW         
026110                              PERFORM WY2000P3                            
026200                              IF TMP1-YYWW < TMP2-YYWW                    
026400                                 CONTINUE                                 
026500                              ELSE                                        
026600                                PERFORM B-TEST-DATUM-IDPROJK              
026700                              END-IF                                      
026800                            END-IF                                        
026900                          ELSE                                            
027000                             PERFORM B-TEST-DATUM-IDPROJK                 
027100                          END-IF                                          
027200                          IF SW-SKRIV-ARTIKELN = JA                       
027300                             PERFORM C-SKRIV-ARTIKEL                      
027400                          END-IF                                          
027500                       END-IF                                             
027600                    END-IF                                                
027700                 END-IF                                                   
027800              END-IF                                                      
027900        END-EVALUATE                                                      
028000        PERFORM IMS-GN-WDD2                                               
028100     END-PERFORM                                                          
028200                                                                          
028300     PERFORM Z-FINIT                                                      
028400     MOVE ZERO TO RETURN-CODE                                             
028500     GOBACK.                                                              
028600     EJECT                                                                
028700 A-INIT SECTION.                                                          
028800                                                                          
028900     OPEN OUTPUT W11406-UTFIL                                             
029000                                                                          
029100     MOVE PROGRAM-NAMN        TO POSTSUM-PROGNAMN                         
029200     MOVE 'W11405D1'          TO POSTSUM-DDNAMN2                          
029300     MOVE 'W11406'            TO POSTSUM-FDNAMN                           
029400     MOVE 'UT-'               TO POSTSUM-TRANSTYP                         
029500                                                                          
029600     ACCEPT SPAR-DAGENS-DATUM1 FROM DATE                                  
029700     MOVE   SPAR-DAGENS-DATUM1 TO DAT-I-TIDATUM                           
029800     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
029900     PERFORM S01-WDATKONV                                                 
030000     IF DAT-KDSVAR-OK                                                     
030100        MOVE DAT-TIAA-VECKA TO SPAR-DAGENS-AA                             
030200        MOVE DAT-TIVV       TO SPAR-DAGENS-VV                             
030300        MOVE JA TO DATUM-SW                                               
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 B-TEST-DATUM-IDPROJK SECTION.                                            
030800     SKIP2                                                                
030900     IF DAGENS-DATUM-OK                                                   
031000        IF WDD201-ART-TIRITC = ZERO                                       
031100           IF WDD201-ART-TIRITP = ZERO                                    
031200              MOVE 9997 TO SPAR-ARTIKEL-AAVV                              
031300*             *** SKIPPAR BA-TESTA-LOPANDE-PROJK NEDAN ****               
031400           ELSE                                                           
031500              MOVE WDD201-ART-TIRITP TO WS-RITNINGSDATUM                  
031600              PERFORM BB-RAEKNA-DATUM-MINUS4-VECKOR                       
031700           END-IF                                                         
031800        ELSE                                                              
031900           MOVE WDD201-ART-TIRITC TO WS-RITNINGSDATUM                     
032000           PERFORM BB-RAEKNA-DATUM-MINUS4-VECKOR                          
032100        END-IF                                                            
032200                                                                          
032201        MOVE SPAR-DAGENS-AAVV    TO TMP1-YYWW                             
032202        MOVE SPAR-ARTIKEL-AAVV   TO TMP2-YYWW                             
032210        PERFORM WY2000P3                                                  
032300        IF TMP1-YYWW < TMP2-YYWW                                          
032400           CONTINUE                                                       
032500        ELSE                                                              
032600           PERFORM BA-TESTA-LOPANDE-PROJK                                 
032700        END-IF                                                            
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 BB-RAEKNA-DATUM-MINUS4-VECKOR SECTION.                                   
033200     SKIP2                                                                
033300     MOVE WS-RITNINGSDATUM  TO DAT-I-TIDATUM                              
033400     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
033500                                                                          
033600     PERFORM S01-WDATKONV                                                 
033700     IF DAT-KDSVAR-OK                                                     
033800        MOVE DAT-TIAA-VECKA TO SPAR-ARTIKEL-AA                            
033900        MOVE DAT-TIVV       TO SPAR-ARTIKEL-VV                            
034000        MOVE SPAR-ARTIKEL-AAVV                                            
034100                            TO SPAR-W009VADD-DATUM                        
034200        MOVE -4             TO SPAR-W009VADD-ANTAL                        
034300        PERFORM S02-W009VADD                                              
034400                                                                          
034500        MOVE SPAR-W009VADD-DATUM TO SPAR-ARTIKEL-AAVV                     
034600     ELSE                                                                 
034700        MOVE 9997                TO SPAR-ARTIKEL-AAVV                     
034800*       ********************************************************          
034900*       * DATUM GICK EJ ATT KONVERTERA, SKIP KOLL I B- SECTION *          
035000*       ********************************************************          
035100     END-IF                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 BA-TESTA-LOPANDE-PROJK SECTION.                                          
035500     SKIP2                                                                
035600     MOVE WDD201-ART-KDPRODSL TO  W-KDPRODSL                              
035700     MOVE WDD201-ART-IDPROJK  TO  W-IDPROJK                               
035710     MOVE WDD201-ART-IDPROJOBJ  TO  W-IDPROJOBJ                           
035720     MOVE WDD201-ART-IDPROJ     TO  W-IDPROJ                              
035800                                                                          
035900     PERFORM IMS-GU-WLXXAQ11                                              
036000     IF SEGMENT-FINNS                                                     
036100        IF WLXXAQ-1132-TIPRODSTA = +111111                                
036200           CONTINUE                                                       
036300        ELSE                                                              
036400           MOVE JA                TO SW-SKRIV-ARTIKELN                    
036500        END-IF                                                            
036600     ELSE                                                                 
036700        MOVE JA                   TO SW-SKRIV-ARTIKELN                    
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 C-SKRIV-ARTIKEL SECTION.                                                 
037200     SKIP2                                                                
037300     MOVE WDD201-ART-IDARTNR  TO UT-IDARTNR                               
037400     WRITE W11406-UTPOST FROM UT-AREA                                     
037500                                                                          
037600     PERFORM S03-POSTSUM                                                  
037700     .                                                                    
037800     EJECT                                                                
037900 D-OMV-RITN-DATUM  SECTION.                                               
038000     SKIP2                                                                
038100     MOVE WS-RITNINGSDATUM  TO DAT-I-TIDATUM                              
038200     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
038300                                                                          
038400     PERFORM S01-WDATKONV                                                 
038500     IF DAT-KDSVAR-OK                                                     
038600        MOVE DAT-TIAA-VECKA TO SPAR-ARTIKEL-AA                            
038700        MOVE DAT-TIVV       TO SPAR-ARTIKEL-VV                            
038800     ELSE                                                                 
038900        MOVE 9997           TO SPAR-ARTIKEL-AAVV                          
039000*       ********************************************************          
039100*       * DATUM GICK EJ ATT KONV, SKIP KOLL I HUVUD-SLINGAN    *          
039200*       ********************************************************          
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 Z-FINIT         SECTION.                                                 
039700     SKIP2                                                                
039800     CLOSE W11406-UTFIL                                                   
039900                                                                          
040000     MOVE 'S'                 TO POSTSUM-OPKOD                            
040100     PERFORM S03-POSTSUM.                                                 
040200     EJECT                                                                
040300 S01-WDATKONV SECTION.                                                    
040400     SKIP2                                                                
040500     CALL WDATKONV USING DAT-KDDATFORM                                    
040600                         DAT-I-TIDATUM                                    
040700                         DAT-O-TIDATUM                                    
040800                         DAT-KDSVAR.                                      
040900     SKIP3                                                                
041000 S02-W009VADD SECTION.                                                    
041100     SKIP2                                                                
041200     CALL W009VADD USING SPAR-W009VADD-DATUM                              
041300                         SPAR-W009VADD-ANTAL.                             
041400     SKIP3                                                                
041500 S03-POSTSUM SECTION.                                                     
041600     SKIP2                                                                
041700     CALL POSTSUM USING  POSTSUM-PARM.                                    
041800     EJECT                                                                
041900 IMS-GN-WDD2 SECTION.                                                     
042000     SKIP2                                                                
042100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
042200     CALL CBLTDLI USING GN WDD2-PCB DLI-IO-AREA1                          
042300     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
042400     PERFORM IMS-STATUSKONTROLL.                                          
042500     SKIP3                                                                
042600 IMS-GU-WLARTC01 SECTION.                                                 
042700     SKIP2                                                                
042800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
042900             DELIMITED BY SIZE INTO SSA1                                  
043000     MOVE '  GE'    TO GODK-STATUSKODER                                   
043100     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA2 SSA1                   
043200     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
043300     PERFORM IMS-STATUSKONTROLL.                                          
043400     SKIP3                                                                
043500 IMS-GU-WLXXAQ11 SECTION.                                                 
043600     SKIP2                                                                
043700     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
043800             DELIMITED BY SIZE INTO SSA1                                  
043920     STRING 'WLXXAQ11(WDGXKEY  =' W-1132KEY-X ')'                         
044000             DELIMITED BY SIZE INTO SSA2                                  
044100     MOVE '  GE'    TO GODK-STATUSKODER                                   
044200     CALL CBLTDLI USING GU WLXXAQ-PCB DLI-IO-AREA2 SSA1 SSA2              
044300     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
044400     PERFORM IMS-STATUSKONTROLL.                                          
044500     SKIP3                                                                
044600 IMS-STATUSKONTROLL SECTION.                                              
044700     SKIP2                                                                
044800     SET STATUS-IX TO 1                                                   
044900     SEARCH GODK-STATUS AT END CALL FELLOG                                
045000        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
045100        CONTINUE                                                          
045200     END-SEARCH.                                                          
045210     EJECT                                                                
045300*    -COPY WY2000P1                                                       
045310     EJECT                                                                
045400*    -COPY WY2000P3                                                       
