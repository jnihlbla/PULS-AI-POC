000100 ID  DIVISION.                                                            
000200     SKIP3                                                                
000300 PROGRAM-ID.    W3313700.                                                 
000400*                                                                         
000500*AUTHOR.        ELEONOR ÖSTRÖM                                            
000600*DATE-WRITTEN.  07/11/07.                                                 
000700                                                                          
000800*                                                                         
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*   FUNKTION: MATCHAR REGISTER MED 91042-FIL SKAPAR                       
001300*             ERSÄTTNINGSINFO TILL S&T MED DE FÖRÄNDRADE                  
001400*             ARTIKLARNA.                                                 
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*                                                                         
001800*        U0016    - OM RETURKOD FRÅN SORT                                 
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*- - - - - - - - - - - - INFILER:                                         
002700     SELECT W91042                       ASSIGN TO W33137D1.              
002800     SELECT REG-IN                       ASSIGN TO W33137D2.              
002900     SKIP2                                                                
003000*- - - - - - - - - - - - UTFILER:                                         
003100     SELECT REG-UT                       ASSIGN TO W33137D3.              
003200     SELECT W33138                       ASSIGN TO W33137D4.              
003300     SKIP2                                                                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W91042                                                               
004000     RECORDING      F                                                     
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300*01  FILLER -COPY W91042     -L.                                          
004400     SKIP2                                                                
004500 FD  REG-IN                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS 0.                                                    
004800     SKIP2                                                                
004900*01  FILLER -COPY W33137     -L.                                          
005000     SKIP2                                                                
005100 FD  REG-UT                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS 0.                                                    
005400     SKIP2                                                                
005500*01  REG-UT-POST -COPY W33137     -L.                                     
005600     SKIP2                                                                
005700 FD  W33138                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS 0.                                                    
006000     SKIP2                                                                
006100*01  ERS-POST -COPY W33138    -L.                                         
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400*    -COPY WY2000W2                                                       
006500     SKIP3                                                                
006600*    -COPY WY2000W3                                                       
006700     SKIP3                                                                
006800*    -COPY WWPRODSL                                                       
006900     SKIP3                                                                
007000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3313700'.               
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W91042-EOF                  PIC X(1)    VALUE 'N'.                   
008000 77  REG-IN-EOF                  PIC X(1)    VALUE 'N'.                   
008100     SKIP2                                                                
008200*- - - - - - - - - - - - - -  DIVERSE VARIABLER                           
008300 77  BEARBETNING                 PIC X(1).                                
008400 77  ART-ANDRAD                  PIC X(1).                                
008500 77  SKRIV-ERS                   PIC X(1).                                
008600 77  WS-PRHANTK                  PIC S9(5)V9(2)      COMP-3.              
008700 77  VECKOR-TILL-TIFINLV         PIC S9(3) COMP-3 VALUE ZERO.             
008800 77  AAR-SEDAN-TIERSDAT          PIC S9(3) COMP-3 VALUE ZERO.             
008900 77  IX                          PIC S9(2) COMP-3 VALUE ZERO.             
009000 77  IX-RAD                      PIC S9(5) COMP-3 VALUE ZERO.             
009100 77  IX-KOLUMN                   PIC S9(5) COMP-3 VALUE ZERO.             
009200 77  IX-STATNR                   PIC S9(3) COMP-3 VALUE ZERO.             
009300 77  KDHBLKRV                    PIC S9(5) COMP-3.                        
009400     EJECT                                                                
009500*- - - - - - - - - - - - - -  TILLÄGG 971210                              
009600 01  WS-PLUS-VECKOR              PIC S9(3) COMP-3 VALUE ZERO.             
009700 01  WS-TIFINLV                  PIC 9(5).                                
009800 01  FILLER REDEFINES WS-TIFINLV.                                         
009900     03  WS-TIFINLV-AAVV         PIC 9(4).                                
010000     03  FILLER                  PIC 9(1).                                
010100 01  WS-AKT-AAVV                 PIC 9(4).                                
010200 01  FILLER REDEFINES WS-AKT-AAVV.                                        
010300     03  WS-AKT-AA               PIC 9(2).                                
010400     03  WS-AKT-VV               PIC 9(2).                                
010500 01  WS-AKT-AAVV-NUM             PIC S9(5) COMP-3.                        
010600*- - - - - - - - - - - - - -  ÅR-VECKA                                    
010700 01  AAR-VECKA                   PIC 9(5).                                
010800 01  FILLER REDEFINES AAR-VECKA.                                          
010900     03  AAR                     PIC 99.                                  
011000     03  VECKA                   PIC 99.                                  
011100     03  FILLER                  PIC  9.                                  
011200     SKIP3                                                                
011300 01  AAVVD-WEEK                  PIC 9(5) VALUE ZERO.                     
011400 01  FILLER REDEFINES AAVVD-WEEK.                                         
011500     03  AA-WEEK                 PIC 99.                                  
011600     03  VV-WEEK                 PIC 99.                                  
011700     03  FILLER                  PIC  9.                                  
011800     SKIP3                                                                
011900     EJECT                                                                
012000 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
012100 01  FILLER REDEFINES DAGENS-DATUM.                                       
012200     03  DAGENS-DATUM-YYYY       PIC 9(4).                                
012300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012500     EJECT                                                                
012600 01  AA53D.                                                               
012700     03  AA                      PIC 99.                                  
012800     03  FILLER                  PIC 999 VALUE 531.                       
012900     SKIP3                                                                
013000 01  DYNAMISKA-SUBPROGRAM.                                                
013100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
013300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013500     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
013600     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
013700     SKIP3                                                                
013800*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
013900                                                                          
014000 01  RETURKODER.                                                          
014100     03  RKOD                    PIC S9(4)  COMP SYNC VALUE ZERO.         
014200     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)  COMP SYNC VALUE +16.          
014300     03  RKOD-ABEND-MED-DUMP     PIC S9(4)  COMP SYNC VALUE +1000.        
014400     EJECT                                                                
014500*- - - - - - - - - - - - - - - - PARAMETRAR TILL DATKORT                  
014600*                                                                         
014700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014800     SKIP2                                                                
014900*01  -COPY WDATKORT                                                       
015000     EJECT                                                                
015100*- - - - - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                 
015200*                                                                         
015300*01  -COPY WDATAREA                                                       
015400     EJECT                                                                
015500                                                                          
015600* VARIABLER TILL SUBPROGRAM W400ARTU                                      
015700*01  -COPY W400ARTU                                                       
015800     SKIP2                                                                
015900*--- - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
016000                                                                          
016100*01  -COPY W0005       -PRE  POSTSUM-.                                    
016200     EJECT                                                                
016300******************************************************************        
016400*         W91042-AREA                                            *        
016500******************************************************************        
016600 01  W91042-AREA.                                                         
016700*    03  FILLER -COPY W91042   -PRE W91042-.                              
016800     EJECT                                                                
016900******************************************************************        
017000*         REG-IN-AREA                                            *        
017100******************************************************************        
017200 01  REG-IN-AREA.                                                         
017300*    03  FILLER -COPY W33137     -PRE REG-IN-.                            
017400     EJECT                                                                
017500******************************************************************        
017600*         REG-UT-AREA                                            *        
017700******************************************************************        
017800 01  REG-UT-AREA.                                                         
017900*    03  FILLER -COPY W33137     -PRE REG-UT-.                            
018000     EJECT                                                                
018100******************************************************************        
018200*         ERS-UT-AREA                                            *        
018300******************************************************************        
018400 01  ERS-UT-AREA.                                                         
018500*    03  FILLER -COPY W33138     -PRE ERS-.                               
018600     EJECT                                                                
018700                                                                          
018800 PROCEDURE DIVISION.                                                      
018900                                                                          
019000     PERFORM A-INIT                                                       
019100     PERFORM S01-LAS-W91042                                               
019200     PERFORM S02-LAS-REG-IN                                               
019300     PERFORM UNTIL W91042-EOF = JA                                        
019400       PERFORM B-SOLLA-POSTER                                             
019500       IF BEARBETNING = JA                                                
019600         IF W91042-IDARTNR = REG-IN-ART-IDARTNR                           
019700           MOVE NEJ TO ART-ANDRAD                                         
019800           PERFORM D-JAMFOR-FILER                                         
019900           IF ART-ANDRAD = JA                                             
020000             IF W91042-KDERS NOT = +52                                    
020100               PERFORM C-FLYTTA-FAELT                                     
020200               PERFORM S10-SKRIV-REG-UT                                   
020300             END-IF                                                       
020400             IF SKRIV-ERS = JA                                            
020500               PERFORM S14-SKRIV-ERSAETTNING                              
020600               MOVE NEJ TO SKRIV-ERS                                      
020700             END-IF                                                       
020800             PERFORM S01-LAS-W91042                                       
020900             PERFORM S02-LAS-REG-IN                                       
021000           ELSE                                                           
021100             IF W91042-KDERS NOT = +52                                    
021200               PERFORM C-FLYTTA-FAELT                                     
021300               PERFORM S10-SKRIV-REG-UT                                   
021400             END-IF                                                       
021500             PERFORM S01-LAS-W91042                                       
021600             PERFORM S02-LAS-REG-IN                                       
021700           END-IF                                                         
021800         ELSE                                                             
021900           EVALUATE TRUE                                                  
022000           WHEN W91042-IDARTNR < REG-IN-ART-IDARTNR                       
022100                                                                          
022200             IF  W91042-PRARTSJK  > ZERO                                  
022300                 AND W91042-PRARTSTD  > ZERO                              
022400                 AND W91042-KDERS NOT = +52                               
022500                                                                          
022600               PERFORM C-FLYTTA-FAELT                                     
022700               PERFORM S10-SKRIV-REG-UT                                   
022800             END-IF                                                       
022900             PERFORM S01-LAS-W91042                                       
023000           WHEN W91042-IDARTNR > REG-IN-ART-IDARTNR                       
023100             PERFORM S02-LAS-REG-IN                                       
023200           END-EVALUATE                                                   
023300         END-IF                                                           
023400       ELSE                                                               
023500         PERFORM S01-LAS-W91042                                           
023600       END-IF                                                             
023700     END-PERFORM                                                          
023800     PERFORM Z-FINIT                                                      
023900     MOVE ZERO TO RETURN-CODE                                             
024000     GOBACK                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 A-INIT SECTION.                                                          
024400     SKIP2                                                                
024500     OPEN INPUT  REG-IN                                                   
024600                 W91042                                                   
024700     OPEN OUTPUT REG-UT                                                   
024800                 W33138                                                   
024900     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
025000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025100     .                                                                    
025200     EJECT                                                                
025300 B-SOLLA-POSTER SECTION.                                                  
025400     SKIP2                                                                
025500******************************************************************        
025600*                                                                         
025700*    SELEKTERAR BORT FÖLJANDE POSTER:                                     
025800*    -OM MER ÄN 10 VECKOR TILL FÖRSTA INLEVERANS                          
025900*    -OM KDUART =  'M' ELLER 'A'                                          
026000*    -OM KDPRODSL = 0, 19, 28, ELLER 29                                   
026100*    -OM IDFKNGRP = 0                                                     
026200*    -OM TIERSDAT ÄLDRE ÄN 4 ÅR                                           
026300*                                                                         
026400******************************************************************        
026500     SKIP2                                                                
026600     MOVE D-AAR             TO WS-AKT-AA                                  
026700     MOVE D-VECKA           TO WS-AKT-VV                                  
026800     MOVE WS-AKT-AAVV       TO WS-AKT-AAVV-NUM                            
026900     MOVE +20               TO WS-PLUS-VECKOR                             
027000     CALL W009VADD USING WS-AKT-AAVV-NUM WS-PLUS-VECKOR                   
027100     MOVE W91042-TIFINLV    TO WS-TIFINLV                                 
027200     MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                                  
027300     MOVE WS-AKT-AAVV-NUM   TO TMP2-YYWW                                  
027400     PERFORM WY2000P3                                                     
027500     IF TMP1-YYWW > TMP2-YYWW                                             
027600       MOVE NEJ             TO BEARBETNING                                
027700     ELSE                                                                 
027800       MOVE JA              TO BEARBETNING                                
027900     END-IF                                                               
028000*                                                                         
028100     IF W91042-TIERSDAT = 0                                               
028200       MOVE 0 TO AAR-SEDAN-TIERSDAT                                       
028300     ELSE                                                                 
028400       MOVE W91042-TIERSDAT TO TMP1-YYWWD                                 
028500       MOVE D-AAR           TO  AA-WEEK                                   
028600       MOVE D-VECKA         TO  VV-WEEK                                   
028700       MOVE AAVVD-WEEK      TO TMP2-YYWWD                                 
028800       PERFORM WY2000P2                                                   
028900       COMPUTE AAR-VECKA = TMP2-YYWWD - TMP1-YYWWD                        
029000       MOVE AAR TO AAR-SEDAN-TIERSDAT                                     
029100       IF VECKA < VV-WEEK                                                 
029200         ADD +1 TO AAR-SEDAN-TIERSDAT                                     
029300       END-IF                                                             
029400     END-IF                                                               
029500*                                                                         
029600     IF BEARBETNING = JA                                                  
029700       MOVE W91042-KDPRODSL      TO TEST-KDPRODSL                         
029800       IF W91042-KDUART =  'M' OR 'A'                                     
029900       OR W91042-IDFKNGRP = 0                                             
030000       OR TEST-KDPRODSL = 0                                               
030100       OR KDPRODSL-EMB                                                    
030200       OR KDPRODSL-VCBV-TOOLS                                             
030300       OR KDPRODSL-VCBV-EMB                                               
030400*  ÖKAT FRÅN 4 TILL 8 ÅR**************''                                  
030500       OR AAR-SEDAN-TIERSDAT > 8                                          
030600         MOVE NEJ TO BEARBETNING                                          
030700       END-IF                                                             
030800     END-IF                                                               
030900*                                                                         
031000     .                                                                    
031100     EJECT                                                                
031200 C-FLYTTA-FAELT SECTION.                                                  
031300     SKIP2                                                                
031400* POSTTYP OCH VERTYP UPPDATERAS I A-INIT.                                 
031500     MOVE W91042-IDARTNR         TO REG-UT-ART-IDARTNR                    
031600     MOVE W91042-IDFKNGRP        TO REG-UT-ART-IDFKNGRP                   
031700     MOVE W91042-KDSRA           TO REG-UT-ART-KDSRA                      
031800     MOVE W91042-KVQPACK-1       TO REG-UT-ART-KVQPACK-1                  
031900     MOVE W91042-KDARTURS        TO REG-UT-ART-KDARTURS                   
032000     MOVE W91042-KDPRODSL        TO REG-UT-ART-KDPRODSL                   
032100     MOVE W91042-VLARTNTO        TO REG-UT-ART-VLARTNTO                   
032200     MOVE W91042-VKART           TO REG-UT-ART-VKART                      
032300     MOVE W91042-IDSTATNR(3)     TO REG-UT-ART-IDSTATNR                   
032400     MOVE W91042-KDVSOP          TO REG-UT-ART-KDVSOP                     
032500     MOVE W91042-KDSORT          TO REG-UT-ART-KDSORT                     
032600     MOVE W91042-KDERS           TO REG-UT-ART-KDERS                      
032700     MOVE W91042-TIERSDAT        TO REG-UT-ART-TIERSDAT                   
032800     MOVE W91042-KDBPSR          TO REG-UT-ART-KDBPSR                     
032900     MOVE ZERO                   TO REG-UT-ART-KDBBCL                     
033000     MOVE W91042-IDLEVNR         TO REG-UT-ART-IDLEVNR                    
033100     MOVE W91042-PRARTSJK        TO REG-UT-ART-PRARTSJK                   
033200     MOVE W91042-PRARTSTD        TO REG-UT-ART-PRARTSTD                   
033300     MOVE W91042-FLIART          TO REG-UT-ART-FLIART                     
033400     MOVE W91042-IDPROJ          TO REG-UT-ART-IDPROJ                     
033500     MOVE W91042-IDAO(1)         TO REG-UT-ART-IDAO(1)                    
033600     MOVE W91042-IDAO(2)         TO REG-UT-ART-IDAO(2)                    
033700     MOVE W91042-KDPSLLOC        TO REG-UT-ART-KDPSLLOC                   
033800     MOVE W91042-TIFINLV         TO REG-UT-ART-TIFINLV                    
033900     MOVE W91042-IDANSK          TO REG-UT-ART-IDANSK                     
034000     MOVE W91042-KDVVKL          TO REG-UT-ART-KDVVKL                     
034100     MOVE W91042-KDTIPPR         TO REG-UT-ART-KDTIPPR                    
034200     MOVE W91042-IDINK           TO REG-UT-ART-IDINK                      
034300     MOVE W91042-KDUART          TO REG-UT-ART-KDUART                     
034400     MOVE W91042-IDARTNR-MOTSV   TO REG-UT-ART-IDARTNR-MOTSV              
034500     MOVE W91042-PRINK           TO REG-UT-ART-PRINK                      
034600     MOVE W91042-IDRITN          TO REG-UT-ART-IDRITN                     
034700     MOVE ZERO                   TO WS-PRHANTK                            
034800     COMPUTE WS-PRHANTK = W91042-PRDIRLON +                               
034900                          W91042-PRDMTRL  + W91042-PROVRPAL               
035000     MOVE WS-PRHANTK             TO REG-UT-ART-PRHANTK                    
035100     MOVE W91042-KDAGE           TO REG-UT-ART-KDAGE                      
035200     MOVE W91042-IDKAT(1)        TO REG-UT-ART-IDKAT(1)                   
035300     MOVE W91042-IDKAT(2)        TO REG-UT-ART-IDKAT(2)                   
035400     MOVE W91042-IDKAT(3)        TO REG-UT-ART-IDKAT(3)                   
035500     MOVE W91042-FLLSRDEL        TO REG-UT-ART-FLLSRDEL                   
035600     MOVE W91042-IDPROJUP        TO REG-UT-ART-IDPROJUP                   
035700     MOVE W91042-FLGEMFMC        TO REG-UT-ART-FLGEMFMC                   
035800     MOVE W91042-TEORSAK         TO REG-UT-ART-TEORSAK                    
035900     MOVE W91042-TEARTNOT        TO REG-UT-ART-TEARTNOT                   
036000     MOVE W91042-BEART(04)       TO REG-UT-ART-BEART(1)                   
036100     MOVE W91042-BEART(08)       TO REG-UT-ART-BEART(2)                   
036200     MOVE W91042-BEART(10)       TO REG-UT-ART-BEART(3)                   
036300                                                                          
036400     .                                                                    
036500                                                                          
036600     EJECT                                                                
036700 D-JAMFOR-FILER SECTION.                                                  
036800     SKIP2                                                                
036900******************************************************************        
037000*                                                                         
037100*    JÄMFÖR REG-IN OCH W91042-POSTER MED SAMMA ARTIKELNUMMER              
037200*    OM POSTERNA ÄR OLIKA SKRIVS POSTEN PÅ REG-UT                         
037300*                                                                         
037400******************************************************************        
037500*                                                                         
037600     MOVE ZERO TO WS-PRHANTK                                              
037700     COMPUTE WS-PRHANTK   =   W91042-PRDIRLON +                           
037800               W91042-PRDMTRL + W91042-PROVRPAL                           
037900                                                                          
038000     IF W91042-KDERS             NOT = REG-IN-ART-KDERS                   
038100       IF  W91042-PRARTSJK  > ZERO AND                                    
038200           W91042-PRARTSTD  > ZERO                                        
038300         MOVE JA TO ART-ANDRAD                                            
038400       END-IF                                                             
038500     END-IF                                                               
038600     IF W91042-KDERS            = REG-IN-ART-KDERS                        
038700      AND W91042-TIERSDAT       = REG-IN-ART-TIERSDAT                     
038800       CONTINUE                                                           
038900     ELSE                                                                 
039000       MOVE JA TO SKRIV-ERS                                               
039100       MOVE W91042-IDARTNR   TO ERS-IDARTNR                               
039200       MOVE W91042-KDERS     TO ERS-KDERS-NEW                             
039300       MOVE REG-IN-ART-KDERS TO ERS-KDERS-OLD                             
039400       MOVE W91042-TIERSDAT   TO ERS-TIERSDAT                             
039500     END-IF                                                               
039600                                                                          
039700     .                                                                    
039800     EJECT                                                                
039900 S01-LAS-W91042 SECTION.                                                  
040000     SKIP2                                                                
040100     READ W91042 INTO W91042-AREA                                         
040200                      AT END MOVE JA TO W91042-EOF                        
040300                      MOVE 999999999 TO W91042-IDARTNR                    
040400     END-READ                                                             
040500     IF W91042-EOF = NEJ                                                  
040600       MOVE 'W91042'             TO POSTSUM-FDNAMN                        
040700       MOVE 'W33137D1'           TO POSTSUM-DDNAMN2                       
040800       MOVE ZERO                 TO POSTSUM-TRANSTYP                      
040900       CALL POSTSUM   USING POSTSUM-PARM                                  
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 S02-LAS-REG-IN SECTION.                                                  
041400     SKIP2                                                                
041500     READ REG-IN INTO REG-IN-AREA                                         
041600                      AT END MOVE JA TO REG-IN-EOF                        
041700                      MOVE 999999999 TO REG-IN-ART-IDARTNR                
041800     END-READ                                                             
041900     IF REG-IN-EOF = NEJ                                                  
042000       MOVE 'REG-IN'            TO POSTSUM-FDNAMN                         
042100       MOVE 'W33137D2'          TO POSTSUM-DDNAMN2                        
042200       MOVE 'REGIN'             TO POSTSUM-TRANSTYP                       
042300       CALL POSTSUM   USING POSTSUM-PARM                                  
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 S10-SKRIV-REG-UT SECTION.                                                
042800     SKIP2                                                                
042900     WRITE REG-UT-POST FROM REG-UT-AREA                                   
043000     MOVE 'REG-UT'               TO POSTSUM-FDNAMN                        
043100     MOVE 'W33537D3'             TO POSTSUM-DDNAMN2                       
043200     MOVE 'REGUT'                TO POSTSUM-TRANSTYP                      
043300     CALL POSTSUM      USING POSTSUM-PARM                                 
043400     .                                                                    
043500     EJECT                                                                
043600 S14-SKRIV-ERSAETTNING SECTION.                                           
043700     SKIP2                                                                
043800*                                                                         
043900     WRITE ERS-POST FROM ERS-UT-AREA                                      
044000     MOVE 'W33137'               TO POSTSUM-FDNAMN                        
044100     MOVE 'W33137D5'             TO POSTSUM-DDNAMN2                       
044200     MOVE 'ERS'                  TO POSTSUM-TRANSTYP                      
044300     CALL POSTSUM      USING POSTSUM-PARM                                 
044400     .                                                                    
044500     EJECT                                                                
044600 Z-FINIT SECTION.                                                         
044700     SKIP2                                                                
044800     CLOSE REG-IN                                                         
044900           W91042                                                         
045000           REG-UT                                                         
045100           W33138                                                         
045200     SKIP2                                                                
045300     MOVE 'S' TO POSTSUM-OPKOD                                            
045400     CALL POSTSUM USING POSTSUM-PARM                                      
045500     .                                                                    
045600     EJECT                                                                
045700*    -COPY WY2000P2                                                       
045800     EJECT                                                                
045900*    -COPY WY2000P3                                                       
