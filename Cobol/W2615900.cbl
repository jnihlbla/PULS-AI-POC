000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2615900.                                                
000300 AUTHOR.         P-A HELGEGREN.                                           
000400 DATE-WRITTEN.   SEP     2007.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        LÄSER FIL W26159 ARTIKLAR MED TISKROT-AUTO > DAGENS-DAT          
001000*        OCH SKAPAR LISTA.                                                
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U1000 - D&P ERROR                                                
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INFIL                                                      
002400     SELECT W26159                     ASSIGN TO W26159D1.                
002500     SKIP2                                                                
002600*          --- SORTERINGSFIL                                              
002700     SELECT SORTFIL                    ASSIGN TO W26159DS.                
002800     SKIP2                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W26159                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W26159    -L.                                                  
003800     SKIP3                                                                
003900 SD  SORTFIL.                                                             
004000                                                                          
004100*01  POST -COPY W26159    -PRE SORT-                                      
004200     03  SORT-IDANSK-GRP    PIC 9(3).                                     
004300     SKIP3                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W2615900'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  SPAR-IDANSK-GRP             PIC 9(3)    VALUE ZERO.                  
005000 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200                                                                          
005300 77  W26159-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W26159                       VALUE 'Y'.                   
005500                                                                          
005600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005700     88  END-OF-SORTFIL                      VALUE 'Y'.                   
005800     EJECT                                                                
005900*                                                                         
006000 01  HDR-AREA.                                                            
006100*    03  -COPY WZ01REQU                                                   
006200*    03  -COPY WZ04HDR                                                    
006300     EJECT                                                                
006400 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
006500 01  SEND-AREA.                                                           
006600*    03  -COPY WZ01SEND                                                   
006700     EJECT                                                                
006800 01  SEND-RAD-STYRTECKEN.                                                 
006900     03  STYRTECKEN-RAD          PIC X.                                   
007000     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
007100*    --- CONTROL CHARACTERS                                               
007200 01  WS-SKIP1                    PIC X       VALUE ' '.                   
007300 01  WS-SKIP2                    PIC X       VALUE '0'.                   
007400 01  WS-SKIP3                    PIC X       VALUE '-'.                   
007500 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
007600     EJECT                                                                
007700                                                                          
007800 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
007900                                                                          
008000     EJECT                                                                
008100*    --- LISTLAYOUT                                                       
008200 01  LISTA.                                                               
008300     03  RUBRIK-1.                                                        
008400         05  FILLER     PIC X       VALUE SPACE.                          
008500         05  FILLER     PIC X(15)   VALUE 'VCCS W26159-001'.              
008600         05  FILLER     PIC X(3)    VALUE SPACE.                          
008700         05  FILLER     PIC X(23) VALUE 'Artiklar med stoppdatum'.        
008710         05  FILLER     PIC X(23) VALUE ' för skrot i framtiden '.        
009000         05  FILLER     PIC X(3)    VALUE SPACE.                          
009100         05  FILLER     PIC X(6)    VALUE 'DATUM'.                        
009200         05  FILLER     PIC X(1)    VALUE SPACE.                          
009300         05  RUB1-DAT   PIC X(6)    VALUE SPACE.                          
009400                                                                          
009500     03  RUBRIK-2.                                                        
009600         05  FILLER           PIC X(4)    VALUE SPACE.                    
009700         05  RUB2-IDANSK      PIC X(06)   VALUE 'Ansk  '.                 
009710         05  RUB2-TISKROT     PIC X(12)   VALUE 'Ej Aut tom  '.           
009720         05  RUB2-KDPRODSL    PIC X(13)   VALUE 'Produktslag  '.          
009800         05  RUB2-IDARTNR     PIC X(15)   VALUE 'Artikelnummer  '.        
010000         05  RUB2-IDFKNGRP    PIC X(14)   VALUE 'Funktionsgrupp'.         
010500         05  FILLER           PIC X(3)    VALUE SPACE.                    
010600                                                                          
010700     03  RAD.                                                             
010800         05  FILLER           PIC X(4)    VALUE SPACE.                    
010900         05  RAD-IDANSK       PIC ZZ9.                                    
011000         05  FILLER           PIC X(3)    VALUE SPACE.                    
011010         05  RAD-TISKROT      PIC 999999.                                 
011011         05  FILLER           PIC X(6)    VALUE SPACE.                    
011020         05  RAD-KDPRODSL     PIC 99.                                     
011021         05  FILLER           PIC X(15)   VALUE SPACE.                    
011100         05  RAD-IDARTNR      PIC ZZZZZZZZ9.                              
011200         05  FILLER           PIC X(2)    VALUE SPACE.                    
011300         05  RAD-IDFKNGRP     PIC 9999.                                   
011800         05  FILLER           PIC X(13)   VALUE SPACE.                    
011900                                                                          
012000                                                                          
012100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012200 01  FILLER REDEFINES DAGENS-DATUM.                                       
012300     03  DAGENS-AA               PIC 9(2).                                
012400     03  DAGENS-MM               PIC 9(2).                                
012500     03  DAGENS-DD               PIC 9(2).                                
012600     EJECT                                                                
012700 01  GENERAL-SUBPROGRAMS.                                                 
012800*                                                                         
012900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
013200     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
013300     SKIP2                                                                
013400*    --- PARAMETERS TO ABEND                                              
013500                                                                          
013600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013900     SKIP2                                                                
014000 01  ERRTEXT.                                                             
014100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
014200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL POSTSUM                                          
014500*                                                                         
014600*01  -COPY W0005   -PRE  POSTSUM-                                         
014700     EJECT                                                                
014800 01  IN-AREA-START               PIC X(24)   VALUE                        
014900                                 'IN-AREA-START  '.                       
015000     SKIP2                                                                
015100*01  AREA -COPY W26159      -PRE IN-                                      
015200     SKIP2                                                                
015300                                                                          
015400     EJECT                                                                
015500 01  SORTWS-AREA-START               PIC X(24)   VALUE                    
015600                                 'SORTWS-AREA-START  '.                   
015700     SKIP2                                                                
015800*01  AREA -COPY W26159      -PRE SORTWS-                                  
015900     03  SORTWS-IDANSK-GRP       PIC 9(3).                                
016000                                                                          
016100 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
016200     EJECT                                                                
016300                                                                          
016400 LINKAGE SECTION.                                                         
016500                                                                          
016600*01  -COPY W0009            -PRE MSG-                                     
016700                                                                          
016800*01  -COPY W0009            -PRE DISTRDOC-                                
016900     EJECT                                                                
017000 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
017100 MAIN SECTION.                                                            
017200     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
017300                                                                          
017400     PERFORM A-INIT                                                       
017500                                                                          
017600     SORT SORTFIL ASCENDING  KEY SORT-IDANSK-GRP                          
017700                  DESCENDING     SORT-TISKROT-AUTO                        
017710                  ASCENDING      SORT-KDPRODSL                            
017720                  ASCENDING      SORT-IDARTNR                             
017800                  INPUT PROCEDURE B-SORT-INPUT                            
017900                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
018000                                                                          
018100     IF SORT-RETURN NOT = 0                                               
018200       MOVE SORT-RETURN TO SORT-RETURN-X                                  
018300       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
018400       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
018500       DISPLAY ERRTEXT                                                    
018600       MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                              
018700       PERFORM S99-ABEND                                                  
018800     ELSE                                                                 
018900       PERFORM Z-FINIT                                                    
019000                                                                          
019100       MOVE ZERO TO RETURN-CODE                                           
019200       GOBACK                                                             
019300     END-IF                                                               
019400                                                                          
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800                                                                          
019900     OPEN INPUT  W26159                                                   
020000     ACCEPT DAGENS-DATUM FROM DATE                                        
020100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020200     .                                                                    
020300     EJECT                                                                
020400 B-SORT-INPUT  SECTION.                                                   
020500                                                                          
020600     PERFORM S01-LAS-W26159                                               
020700     PERFORM UNTIL END-OF-W26159                                          
020800       MOVE IN-W26159        TO SORTWS-W26159                             
020900       MOVE IN-IDANSK        TO SORTWS-IDANSK-GRP                         
021000       MOVE '0'              TO SORTWS-IDANSK-GRP (3:1)                   
021100       PERFORM S31-SORT-RELEASE                                           
021200       PERFORM S01-LAS-W26159                                             
021300     END-PERFORM                                                          
021400     .                                                                    
021500     EJECT                                                                
021600 C-SORT-OUTPUT SECTION.                                                   
021700                                                                          
021800     PERFORM S32-SORT-RETURN                                              
021810     PERFORM S90-SEND-OPEN                                                
021820     PERFORM S90-PUT-DAP-START                                            
021830     PERFORM CC-PRINT-HEAD                                                
021840                                                                          
021900     PERFORM UNTIL END-OF-SORTFIL                                         
022000       MOVE SORTWS-IDANSK-GRP TO SPAR-IDANSK-GRP                          
022600       PERFORM UNTIL END-OF-SORTFIL OR                                    
022700         (SPAR-IDANSK-GRP NOT = SORTWS-IDANSK-GRP)                        
022800           PERFORM CD-RAD-DATA                                            
022900         PERFORM S32-SORT-RETURN                                          
023000       END-PERFORM                                                        
023200     END-PERFORM                                                          
023210                                                                          
023300     PERFORM S90-SEND-CLOSE                                               
023400     .                                                                    
023500     EJECT                                                                
024100 CC-PRINT-HEAD SECTION.                                                   
024200                                                                          
024300     MOVE SPACE                      TO SEND-RAD-STYRTECKEN               
024400     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
024500     MOVE SPACE                      TO SEND-RAD                          
024600     PERFORM S90-PUT-DOC-LINE                                             
024800     MOVE DAGENS-DATUM               TO RUB1-DAT                          
024900     MOVE RUBRIK-1                   TO SEND-RAD                          
025000     PERFORM S90-PUT-DOC-LINE                                             
025100                                                                          
025200     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
025300     MOVE SPACE                      TO SEND-RAD                          
025400     PERFORM S90-PUT-DOC-LINE                                             
025500     MOVE RUBRIK-2                   TO SEND-RAD                          
025600     PERFORM S90-PUT-DOC-LINE                                             
025700                                                                          
025800     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
025900     MOVE SPACE                      TO SEND-RAD                          
026000     PERFORM S90-PUT-DOC-LINE                                             
026100     .                                                                    
026200     EJECT                                                                
026300 CD-RAD-DATA SECTION.                                                     
026400                                                                          
026500     MOVE SORTWS-IDANSK       TO RAD-IDANSK                               
026600     MOVE SORTWS-IDARTNR      TO RAD-IDARTNR                              
026700     MOVE SORTWS-IDFKNGRP     TO RAD-IDFKNGRP                             
026800     MOVE SORTWS-KDPRODSL     TO RAD-KDPRODSL                             
026900     MOVE SORTWS-TISKROT-AUTO TO RAD-TISKROT                              
027000                                                                          
027100     MOVE RAD                 TO SEND-RAD                                 
027200     PERFORM S90-PUT-DOC-LINE                                             
027300     .                                                                    
027400     EJECT                                                                
027500 Z-FINIT SECTION.                                                         
027600                                                                          
027700     CLOSE W26159                                                         
027800     MOVE 'S' TO POSTSUM-OPKOD                                            
027900     CALL POSTSUM USING POSTSUM-PARM                                      
028000     .                                                                    
028100     EJECT                                                                
028200 S01-LAS-W26159 SECTION.                                                  
028300     READ W26159 INTO IN-AREA                                             
028400     AT END                                                               
028500        MOVE HIGH-VALUE TO IN-AREA                                        
028600        SET END-OF-W26159 TO TRUE                                         
028700     NOT AT END                                                           
028800        MOVE 'W26159'   TO POSTSUM-FDNAMN                                 
028900        MOVE 'W26159D1' TO POSTSUM-DDNAMN2                                
029000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029100        CALL POSTSUM USING POSTSUM-PARM                                   
029200     END-READ                                                             
029300     .                                                                    
029400     EJECT                                                                
029500 S31-SORT-RELEASE  SECTION.                                               
029600                                                                          
029700     RELEASE SORT-POST FROM SORTWS-AREA                                   
029800     .                                                                    
029900     EJECT                                                                
030000 S32-SORT-RETURN  SECTION.                                                
030100                                                                          
030200     RETURN SORTFIL INTO SORTWS-AREA                                      
030300     AT END                                                               
030400         SET END-OF-SORTFIL TO TRUE                                       
030500     .                                                                    
030600     EJECT                                                                
030700 S90-SEND-OPEN SECTION.                                                   
030800                                                                          
030900     MOVE 'OPEN'                        TO SEND-KDFUNC                    
031000     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
031100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031200                         SEND-OPEN-AREA                                   
031300     IF SEND-KDRC > 0                                                     
031400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
031500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
031600       DELIMITED BY SIZE INTO ERRTEXT                                     
031700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031800     END-IF                                                               
031900     .                                                                    
032000     EJECT                                                                
032100 S90-PUT-DAP-START SECTION.                                               
032200                                                                          
032300     MOVE 1                       TO REQU-IDMSGVER                        
032400     MOVE 'R'                     TO REQU-KDPGMACT                        
032500     MOVE IDPGM                   TO REQU-IDUSER                          
032600     MOVE 'W26159-001'            TO HDR-IDOUTTYPE                        
032700     MOVE SPACE                   TO HDR-IDOUTREC                         
032800                                     HDR-IDLIST                           
032900     MOVE 'W26159'                TO HDR-IDOUTREC                         
033000     MOVE DAGENS-DATUM            TO HDR-IDLIST                           
033100     MOVE 'PUT'                   TO SEND-KDFUNC                          
033200     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
033300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033400                         SEND-KVDLEN                                      
033500                         HDR-AREA                                         
033600     IF SEND-KDRC > ZERO                                                  
033700       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
033800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
034000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 S90-PUT-DOC-LINE SECTION.                                                
034500                                                                          
034600     MOVE 'PUT'                           TO SEND-KDFUNC                  
034700*                     -- UTAN STYRTECKEN:                                 
034800     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
034900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035000                         SEND-KVDLEN                                      
035100*                     -- UTAN STYRTECKEN:                                 
035200                         SEND-RAD                                         
035300     IF SEND-KDRC > ZERO                                                  
035400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035600       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
035700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 S90-SEND-CLOSE SECTION.                                                  
036200                                                                          
036300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
036400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036500                                                                          
036600     IF SEND-KDRC > 0                                                     
036700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
036800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
036900       DELIMITED BY SIZE INTO ERRTEXT                                     
037000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 S99-ABEND SECTION.                                                       
037500                                                                          
037600     SKIP2                                                                
037700     MOVE 'S' TO POSTSUM-OPKOD                                            
037800     CALL POSTSUM USING POSTSUM-PARM                                      
037900     CALL ABEND USING RKOD-ABEND                                          
038000     .                                                                    
