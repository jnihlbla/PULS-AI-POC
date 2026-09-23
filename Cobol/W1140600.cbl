000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.             W1140600.                                        
000400 AUTHOR.                 KENT HELLQVIST.                                  
000500     DATE-WRITTEN.       JANUARI 1988.                                    
000600     REMARKS.                                                             
000700******************************************************************        
000800*                                                                *        
000900*            N Y P O N - B M P                                   *        
001000*                                                                *        
001100*            LÄSER FIL MED ARTIKLAR SOM UPPFYLLER BEVAKNINGS-    *        
001200*            VILLKOREN OCH SÄTTER DESS FLBERQ TILL JA SÅ ATT     *        
001300*            ARTIKELN HAMNAR PÅ BEREDARENS KÖ.                   *        
001400******************************************************************        
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*                                                                         
002300     SELECT  W11406-INFIL             ASSIGN TO    W11406D1.              
002400*                                                                         
002500     SELECT  W11406-NY-GEN            ASSIGN TO    W11406D2.              
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP2                                                                
003100 FD  W11406-INFIL                                                         
003200     LABEL RECORD STANDARD                                                
003300     RECORDING      F                                                     
003400     BLOCK CONTAINS 0.                                                    
003500     SKIP2                                                                
003600 01  W11406-INPOST.                                                       
003700*03  -COPY  W11406   -L.                                                  
003800 ++INCLUDE   W11406                                                       
003900     EJECT                                                                
004000 FD  W11406-NY-GEN                                                        
004100     LABEL RECORD STANDARD                                                
004200     RECORDING      F                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004500 01  W11406-UTPOST.                                                       
004600*03  -COPY  W11406      -L.                                               
004700 ++INCLUDE   W11406                                                       
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005001                                                                          
005010*    -- CHECKED BY WY2000                                                 
005100 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1140600'.               
005200     SKIP2                                                                
005300*    ---- FLAGGOR / SWITCHAR -----------------------------------          
005400 77  W11406-EOF                  PIC X       VALUE 'N'.                   
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  WS-ANT-REPL                 PIC S9(3)   VALUE +0    COMP-3.          
005800*                                                                         
005900     EJECT                                                                
006000****************************************************************          
006100*    DIVERSE SPARFÄLT                                          *          
006200****************************************************************          
006300     SKIP2                                                                
006400 01  SPAR-FALT.                                                           
006500     05  SPAR-DAGENS-DATUM1      PIC 9(06)   VALUE ZERO.                  
006600                                                                          
006700     05  SPAR-DAGENS-DATUM2.                                              
006800         10  SPAR-DAGENS-AA      PIC 9(02)   VALUE ZERO.                  
006900         10  SPAR-DAGENS-VV      PIC 9(02)   VALUE ZERO.                  
007000     05  SPAR-DAGENS-DATUM-R  REDEFINES  SPAR-DAGENS-DATUM2.              
007100         10  SPAR-DAGENS-AAVV    PIC 9(04).                               
007200                                                                          
007300     05  SPAR-W009VADD-DATUM     PIC S9(5)   VALUE ZERO COMP-3.           
007400     05  SPAR-W009VADD-ANTAL     PIC S9(3)   VALUE ZERO COMP-3.           
007500                                                                          
007600     EJECT                                                                
007700****************************************************************          
007800*    SUBPROGRAM OCH PARAMETERAREOR                             *          
007900****************************************************************          
008000     SKIP2                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
008300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008500   03  W980SOP                   PIC X(8)    VALUE 'W980SOP '.            
008600   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
008700   03  W009VADD                  PIC X(8)    VALUE 'W009VADD'.            
008800     EJECT                                                                
008900*    ---- PARAMETRAR TILL W980SOP                                         
009000 01  FILLER               PIC X(16)   VALUE  'W980SOP-AREA'.              
009100*01  -COPY WSOPAREA.                                                      
009200 ++INCLUDE WSOPAREAC0                                                     
009300     EJECT                                                                
009400*    ---- PARAMETRAR TILL WDATKONV                                        
009500 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
009600*01  -COPY WDATAREA                                                       
009700*++INCLUDE WDATAREAC0                                                     
009800     EJECT                                                                
009900*    ---- PARAMETRAR TILL POSTSUM                                         
010000 01  FILLER               PIC X(16)   VALUE  'POSTSUM-AREA'.              
010100*01  -COPY W0005 -PRE POSTSUM-.                                           
010200*++INCLUDE W0005CCCC0                                                     
010300     EJECT                                                                
010400****************************************************************          
010500*    IN-AREA                                                   *          
010600****************************************************************          
010700 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
010800*01  AREA   -COPY W11406       -PRE IN-.                                  
010900*++INCLUDE W11406                                                         
011000     EJECT                                                                
011100****************************************************************          
011200*    UT-AREA                                                   *          
011300****************************************************************          
011400 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
011500*01  AREA   -COPY W11406       -PRE UT-.                                  
011600*++INCLUDE W11406                                                         
011700     EJECT                                                                
011800****************************************************************          
011900*    ARBETS-AREOR FÖR IMS-SEKTIONERNA                                     
012000****************************************************************          
012100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012200     SKIP2                                                                
012300*    ---- STATUSKOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FINNS                       VALUE '  '.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     SKIP2                                                                
012800 01  GODK-STATUSKODER.                                                    
012900   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
013000     SKIP2                                                                
013100 01  SSA1                        PIC X(64).                               
013200     EJECT                                                                
013300****************************************************************          
013400*    NYCKLAR OCH SÖKFÄLT TILL DLI                              *          
013500****************************************************************          
013600 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-DLI'.               
013700 01  NYCKLAR-TILL-DLI.                                                    
013800   03  W-IDARTNR-X.                                                       
013900     05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.                
014000*                                                                         
014100   03  W-1135KEY-X.                                                       
014200     05  FILLER              PIC X(04)  VALUE '1135'.                     
014300     05  W-KDPRODSL          PIC S9(3)  VALUE ZERO COMP-3.                
014400     05  FILLER              PIC X(24)  VALUE LOW-VALUE.                  
014500                                                                          
014600   03  W-1136KEY-X.                                                       
014700     05  W-IDFKNGRP          PIC S9(5)  VALUE ZERO COMP-3.                
014800     05  FILLER              PIC X(02)  VALUE LOW-VALUE.                  
014900                                                                          
015000*SÖKBEGREPP                                                               
015100   03  W-IDFKNGRP-X.                                                      
015200     05  W-IDFKNGRP-SBGP     PIC S9(5)  VALUE ZERO COMP-3.                
015300                                                                          
015400     EJECT                                                                
015500*01  -COPY W0003                                                          
015600 ++INCLUDE W0003CCCC0                                                     
015700     EJECT                                                                
015800****************************************************************          
015900*    DLI-IO-AREA / IO-AREOR                                    *          
016000****************************************************************          
016100 01  DLI-IO-AREA1.                                                        
016200   03 IO-AREA1                   PIC X(600)  VALUE SPACE.                 
016300     SKIP2                                                                
016400                                                                          
016500*  03  WLARTG01 -COPY WDD201 -PRE ARTG01- -RED IO-AREA1.                  
016600 ++INCLUDE WDD201CCC0                                                     
016700     EJECT                                                                
016800 01  DLI-IO-AREA2.                                                        
016900   03 IO-AREA2                   PIC X(50)   VALUE SPACE.                 
017000     SKIP2                                                                
017100                                                                          
017200*  03  WDGX1135 -COPY WDGX1135C0 -PRE XXAS- -RED IO-AREA2.                
017300 ++INCLUDE WDGX1135C0                                                     
017400     EJECT                                                                
017500*  03  WDGX1136 -COPY WDGX1136C0 -PRE XXAS- -RED IO-AREA2.                
017600 ++INCLUDE WDGX1136C0                                                     
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900* - - - - - - - - -LOGISK TERMINAL PCB-COPYTEXT FÖR BMP                   
018000*01      -COPY W0009     -PRE MSG-                                        
018100 ++INCLUDE W0009CCCC0                                                     
018200     EJECT                                                                
018300*01      -COPY W0008     -PRE ARTG-                                       
018400 ++INCLUDE W0008CCCC0                                                     
018500      05 FILLER          PIC X(2).                                        
018600     EJECT                                                                
018700*01      -COPY W0008     -PRE XXAS-                                       
018800 ++INCLUDE W0008CCCC0                                                     
018900      05 FILLER          PIC X(2).                                        
019000     EJECT                                                                
019100 PROCEDURE DIVISION USING MSG-PCB  ARTG-PCB  XXAS-PCB.                    
019200     SKIP2                                                                
019300     PERFORM A-INIT                                                       
019400     PERFORM S01-LAES-INFIL                                               
019500                                                                          
019600     PERFORM UNTIL W11406-EOF = JA OR WS-ANT-REPL > +199                  
019700                                                                          
019800        MOVE IN-IDARTNR  TO W-IDARTNR                                     
019900        PERFORM IMS-GHU-ARTG01                                            
020000        PERFORM B-BESTAM-START-SLUT-BEREDNING                             
020100        PERFORM C-HAMTA-IDBERED                                           
020200                                                                          
020300        MOVE JA          TO ARTG01-ART-FLBERQ                             
020400        PERFORM IMS-REPL-ARTG01                                           
020500                                                                          
020600        ADD +1           TO WS-ANT-REPL                                   
020700        MOVE 'W11406D3'  TO  POSTSUM-DDNAMN2                              
020800        MOVE 'W11406'    TO  POSTSUM-FDNAMN                               
020900        MOVE 'REPL'      TO  POSTSUM-TRANSTYP                             
021000        PERFORM S02-POSTSUM                                               
021100                                                                          
021200        PERFORM S01-LAES-INFIL                                            
021300     END-PERFORM                                                          
021400                                                                          
021500     IF W11406-EOF = JA                                                   
021600        CONTINUE                                                          
021700     ELSE                                                                 
021800        PERFORM D-SKRIV-NY-INFIL                                          
021900        PERFORM E-STARTA-NYTT-JOBB-VIA-SOP                                
022000     END-IF                                                               
022100                                                                          
022200     PERFORM Z-FINIT                                                      
022300     MOVE ZERO TO RETURN-CODE                                             
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT SECTION.                                                          
022800     SKIP2                                                                
022900     OPEN INPUT  W11406-INFIL                                             
023000          OUTPUT W11406-NY-GEN                                            
023100                                                                          
023200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
023300     MOVE +0           TO WS-ANT-REPL                                     
023400                                                                          
023500     ACCEPT SPAR-DAGENS-DATUM1 FROM DATE                                  
023600                                                                          
023700     MOVE SPAR-DAGENS-DATUM1 TO DAT-I-TIDATUM                             
023800     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
023900     PERFORM S03-WDATKONV                                                 
024000                                                                          
024100     IF DAT-KDSVAR-OK                                                     
024200        MOVE DAT-TIAA-VECKA TO SPAR-DAGENS-AA                             
024300        MOVE DAT-TIVV       TO SPAR-DAGENS-VV                             
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 B-BESTAM-START-SLUT-BEREDNING SECTION.                                   
024800     SKIP2                                                                
024900     MOVE SPAR-DAGENS-DATUM1              TO ARTG01-ART-TISTABER          
025000                                                                          
025100     MOVE SPAR-DAGENS-AAVV                TO SPAR-W009VADD-DATUM          
025200     MOVE +3                              TO SPAR-W009VADD-ANTAL          
025300     PERFORM S04-W009VADD                                                 
025400                                                                          
025500     MOVE 'AAVV'                          TO DAT-KDDATFORM                
025600     MOVE SPAR-W009VADD-DATUM             TO DAT-I-TIDATUM                
025700     PERFORM S03-WDATKONV                                                 
025800                                                                          
025900     IF DAT-KDSVAR-OK                                                     
026000        MOVE DAT-TIAAMMDD                 TO ARTG01-ART-TISLUBER          
026100     END-IF.                                                              
026200     EJECT                                                                
026300 C-HAMTA-IDBERED SECTION.                                                 
026400     SKIP2                                                                
026500     MOVE ARTG01-ART-KDPRODSL             TO W-KDPRODSL                   
026600     MOVE ARTG01-ART-IDFKNGRP             TO W-IDFKNGRP                   
026700                                             W-IDFKNGRP-SBGP              
026800                                                                          
026900     PERFORM IMS-GU-XXAS01                                                
027000     PERFORM IMS-GNP-XXAS11                                               
027100                                                                          
027200     IF SEGMENT-FINNS                                                     
027300        MOVE XXAS-1136-IDBERED            TO ARTG01-ART-IDBERED           
027400     ELSE                                                                 
027500        MOVE ZERO                         TO ARTG01-ART-IDBERED           
027600     END-IF.                                                              
027700                                                                          
027800     EJECT                                                                
027900 D-SKRIV-NY-INFIL SECTION.                                                
028000     SKIP2                                                                
028100     PERFORM UNTIL W11406-EOF = JA                                        
028200        WRITE W11406-UTPOST FROM IN-AREA                                  
028300        END-WRITE                                                         
028400*                                                                         
028500        MOVE 'W11406D2'  TO  POSTSUM-DDNAMN2                              
028600        MOVE 'W11406'    TO  POSTSUM-FDNAMN                               
028700        MOVE 'UT-'       TO  POSTSUM-TRANSTYP                             
028800        PERFORM S02-POSTSUM                                               
028900        PERFORM S01-LAES-INFIL                                            
029000     END-PERFORM                                                          
029100     .                                                                    
029200     EJECT                                                                
029300 E-STARTA-NYTT-JOBB-VIA-SOP   SECTION.                                    
029400     SKIP2                                                                
029500     MOVE SPACE          TO SOP-DDPREFIX                                  
029600     MOVE 'O'            TO SOP-SOPFUNC                                   
029700     MOVE 'W114J006'     TO SOP-PROC-NAME                                 
029800     MOVE ZERO           TO SOP-ACTPASS-DATE                              
029900                                                                          
030000     CALL W980SOP USING SOP-PARM-AREA                                     
030100                                                                          
030200     IF SOP-RETCODE > +8                                                  
030300        CALL FELLOG                                                       
030400     END-IF                                                               
030500                                                                          
030600     .                                                                    
030700     EJECT                                                                
030800 S01-LAES-INFIL SECTION.                                                  
030900     SKIP2                                                                
031000     READ W11406-INFIL INTO IN-AREA                                       
031100        AT END MOVE JA TO W11406-EOF                                      
031200     END-READ                                                             
031300*                                                                         
031400     IF W11406-EOF = NEJ                                                  
031500        MOVE 'W11406D1'  TO  POSTSUM-DDNAMN2                              
031600        MOVE 'W11406'    TO  POSTSUM-FDNAMN                               
031700        MOVE 'IN-'       TO  POSTSUM-TRANSTYP                             
031800        PERFORM S02-POSTSUM                                               
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 S02-POSTSUM SECTION.                                                     
032300     SKIP2                                                                
032400     CALL POSTSUM USING POSTSUM-PARM.                                     
032500     SKIP3                                                                
032600 S03-WDATKONV SECTION.                                                    
032700     SKIP2                                                                
032800     CALL WDATKONV USING DAT-KDDATFORM                                    
032900                         DAT-I-TIDATUM                                    
033000                         DAT-O-TIDATUM                                    
033100                         DAT-KDSVAR.                                      
033200     SKIP3                                                                
033300 S04-W009VADD SECTION.                                                    
033400     SKIP2                                                                
033500     CALL W009VADD USING SPAR-W009VADD-DATUM                              
033600                         SPAR-W009VADD-ANTAL.                             
033700     EJECT                                                                
033800 Z-FINIT   SECTION.                                                       
033900     SKIP2                                                                
034000     CLOSE   W11406-INFIL                                                 
034100             W11406-NY-GEN                                                
034200                                                                          
034300     MOVE 'S' TO POSTSUM-OPKOD                                            
034400     PERFORM S02-POSTSUM                                                  
034500                                                                          
034600     .                                                                    
034700     EJECT                                                                
034800* IMS SECTIONER                                                           
034900     SKIP2                                                                
035000 IMS-GHU-ARTG01 SECTION.                                                  
035100     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
035200            DELIMITED BY SIZE INTO SSA1                                   
035300     MOVE '  ' TO GODK-STATUSKODER                                        
035400     CALL CBLTDLI USING GHU ARTG-PCB IO-AREA1 SSA1                        
035500     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
035600     PERFORM IMS-STATUSKONTROLL                                           
035700     .                                                                    
035800     SKIP2                                                                
035900 IMS-REPL-ARTG01 SECTION.                                                 
036000     MOVE '  ' TO GODK-STATUSKODER                                        
036100     CALL CBLTDLI USING REPL ARTG-PCB IO-AREA1                            
036200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
036300     PERFORM IMS-STATUSKONTROLL                                           
036400     .                                                                    
036500     SKIP2                                                                
036600 IMS-GU-XXAS01 SECTION.                                                   
036700     SKIP2                                                                
036800     STRING 'WLXXAS01(WDGXKEY  =' W-1135KEY-X ')'                         
036900            DELIMITED BY SIZE INTO SSA1                                   
037000     MOVE '  '    TO GODK-STATUSKODER                                     
037100     CALL CBLTDLI USING GU XXAS-PCB DLI-IO-AREA2 SSA1                     
037200     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
037300     PERFORM IMS-STATUSKONTROLL.                                          
037400     EJECT                                                                
037500 IMS-GNP-XXAS11 SECTION.                                                  
037600     SKIP2                                                                
037700     STRING 'WLXXAS11(WDGXKEY <=' W-1136KEY-X                             
037800                    '&IDFKNGRP>=' W-IDFKNGRP-X ')'                        
037900            DELIMITED BY SIZE INTO SSA1                                   
038000     MOVE '  GE'  TO GODK-STATUSKODER                                     
038100     CALL CBLTDLI USING GNP XXAS-PCB DLI-IO-AREA2 SSA1                    
038200     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
038300     PERFORM IMS-STATUSKONTROLL.                                          
038400     EJECT                                                                
038500 IMS-STATUSKONTROLL SECTION.                                              
038600     SET STATUS-IX TO 1                                                   
038700     SEARCH GODK-STATUS                                                   
038800       AT END CALL FELLOG                                                 
038900       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
039000     END-SEARCH.                                                          
