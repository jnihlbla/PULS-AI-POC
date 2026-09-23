000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300     SKIP3                                                                
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.    W2217600.                                                 
000700*              PROGRAM CONVERTED BY                                       
000800*              COBOL CONVERSION AID PO 5785-ABJ                           
000900*              CONVERSION DATE 05/25/91 19:30:18.                         
001000*AUTHOR.        MATS VINNEFORS.                                           
001100*DATE-WRITTEN.  NOVEMBER 1980.                                            
001200                                                                          
001300*REMARKS*********************************************************         
001400*                                                               *         
001500*    FUNKTION:                                                  *         
001600*        PROGRAMMET SKRIVER LISTA W22176-001                    *         
001700*        'BRISTLISTA BYTESOBJEKT'.                              *         
001800*                                                               *         
001900*    INFIL:                                                     *         
002000*        W22144 + WDD3                                          *         
002100*                                                               *         
002200*    LISTFIL:                                                   *         
002300*        W22176-001           SORTERAD STIGANDE  IDANSK         *         
002400*                                                IDLEVNR        *         
002500*                                                IDARTNR        *         
002600*        SIDBRYTNING VID NY ANSKAFFARE.                         *         
002700*                                                               *         
002800*    SUBPROGRAM:                                                *         
002900*        ABEND                                                  *         
003000*        DATKORT                                                *         
003100*        W2217610                                               *         
003200*                                                               *         
003300*                                                               *         
003400*    ABENDKODER:                                                *         
003500*        16    - OM RETURKOD FRÅN SORT                          *         
003600*                                                               *         
003700*****************************************************************         
003800     EJECT                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP2                                                                
004100 INPUT-OUTPUT SECTION.                                                    
004200                                                                          
004300 FILE-CONTROL.                                                            
004400     SKIP2                                                                
004500*****************************************************************         
004600*    INFIL:                                                     *         
004700*                                                               *         
004800*                                                               *         
004900*****************************************************************         
005000                                                                          
005100     SELECT W22144               ASSIGN TO UT-S-W22176D1.                 
005200     SKIP2                                                                
005300*****************************************************************         
005400*    LISTFIL:                                                   *         
005500*           LARMLISTA BYTES                                     *         
005600*****************************************************************         
005700                                                                          
005800     SELECT LISTA                ASSIGN TO UT-S-W22176D2.                 
005900     SKIP2                                                                
006000*****************************************************************         
006100*    SORTFIL                                                    *         
006200*****************************************************************         
006300                                                                          
006400     SELECT SORTFIL              ASSIGN TO UT-S-W22176DS.                 
006500     EJECT                                                                
006600 DATA DIVISION.                                                           
006700     SKIP2                                                                
006800 FILE SECTION.                                                            
006900     SKIP3                                                                
007000 FD  W22144                                                               
007100     RECORDING      F                                                     
007200     BLOCK CONTAINS 0.                                                    
007300     SKIP2                                                                
007400*    POST -COPY W221LI76  -L -PRE IN-.                                    
007600     SKIP3                                                                
007700 FD  LISTA                                                                
007800     RECORDING      F                                                     
007900     BLOCK CONTAINS 0.                                                    
008000 01  LISTPOST            PIC X(121).                                      
008100     EJECT                                                                
008200 SD  SORTFIL                                                              
008300                .                                                         
008400*01  POST  -COPY W221LI76    -PRE SORT-.                                  
008600     EJECT                                                                
008700 WORKING-STORAGE SECTION.                                                 
008710                                                                          
008800*    -- CHECKED BY WY2000                                                 
008900 77  INDENT-I PIC X(40) VALUE                                             
009000     'W2217600 91/05/25 TIME 12.50 VILMAII'.                              
009100***  STATEMENT ABOVE GENERATED BY VILMAII CONVERTER                       
009200*                                                                         
009300     SKIP2                                                                
009400*****************************************************************         
009500*    GENERERAT PROGRAM-NAMN                                     *         
009600*****************************************************************         
009700                                                                          
009800 77  PROGRAM-NAMN                REDEFINES INDENT-I                       
009900                                 PIC X(8).                                
010000     SKIP2                                                                
010100*****************************************************************         
010200*    GENERELLA KONSTANTER                                       *         
010300*****************************************************************         
010400                                                                          
010500 77  JA                          PIC X(1)    VALUE 'J'.                   
010600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010700     SKIP2                                                                
010800*****************************************************************         
010900*    END-OFF-FILE-SWITCHAR                                      *         
011000*****************************************************************         
011100                                                                          
011200 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
011300     SKIP2                                                                
011400 01  HJAELP-AREOR.                                                        
011500     03  SIDRAK                  PIC 9(3)  VALUE ZERO.                    
011600     03  RADRAK                  PIC S9(3) COMP-3 VALUE ZERO.             
011700     03  IDANSKJAMFOR            PIC S9(3) COMP-3 VALUE ZERO.             
011800     EJECT                                                                
011900 01  DYNAMISKA-SUBPROGRAM.                                                
012000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
012200     03  W2217610                PIC X(8)    VALUE 'W2217610'.            
012300     SKIP3                                                                
012400*****************************************************************         
012500*    PARAMETRAR TILL ABEND                                      *         
012600*****************************************************************         
012700                                                                          
012800 01  RETURKODER.                                                          
012900     03  RKOD                    PIC S9(4)  COMP SYNC VALUE ZERO.         
013000     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)  COMP SYNC VALUE +16.          
013100     SKIP2                                                                
013200*****************************************************************         
013300*    PARAMETER TILL SUBPROGRAM W2217610                         *         
013400*****************************************************************         
013500                                                                          
013600 01  LAES-BENAEMNING             PIC S9(3)   VALUE +761.                  
013700     EJECT                                                                
013800*****************************************************************         
013900*    PARAMETRAR TILL DATKORT                                    *         
014000*****************************************************************         
014100                                                                          
014200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014300     SKIP2                                                                
014400*    -COPY WDATKORT                                                       
014600     EJECT                                                                
014700     SKIP2                                                                
014800***********ARBETS-AREA FÖR INFIL***********************                   
014900*    -COPY W221LI76                                                       
015200     SKIP2                                                                
015300 01  KVANTAL                     PIC S9(7)  COMP-3.                       
015400     EJECT                                                                
015500*01  AREA  -COPY W221L761 -PRE LINK-                                      
015700     EJECT                                                                
015800******LISTA**************                                                 
015900*    PAGE 42 LINES *                                                      
016000*    HEADING 1 *                                                          
016100*    FIRST DETAIL 5 *                                                     
016200* *  CONTROL IS IDANSK. *                                                 
016300     SKIP2                                                                
016400 01    TOMRAD                            PIC X(121) VALUE SPACE.          
016500 01    RUBRIK1.                                                           
016600         05  FILLER                      PIC X(3) VALUE SPACE.            
016700         05  FILLER                      PIC X(18)                        
016800                                         VALUE 'VOLVO PARTS'.             
016900         05  FILLER                      PIC X(14)                        
017000                                         VALUE 'W22176-001'.              
017100         05  FILLER                      PIC X(26) VALUE                  
017200                                        'BRISTLISTA BYTESOBJEKT'.         
017300         05  FILLER                      PIC X(6)   VALUE 'VECKA'.        
017400         05  UT-D-VECKA               PIC Z(1)9(1) VALUE ZERO.            
017500         05  FILLER                     PIC X(28) VALUE SPACE.            
017600         05  UT-D-AAR                    PIC 9(2) VALUE ZERO.             
017700         05  FILLER                      PIC X(1)   VALUE '.'.            
017800         05  UT-D-MAANAD                 PIC 9(2) VALUE ZERO.             
017900         05  FILLER                      PIC X(1)   VALUE '.'.            
018000         05  UT-D-DAG                    PIC 9(2) VALUE ZERO.             
018100         05  FILLER                      PIC X(6) VALUE SPACE.            
018200         05  FILLER                      PIC X(4)   VALUE 'SIDA'.         
018300         05  SIDMARKERING                PIC Z(2)9(1)                     
018400                                         VALUE ZERO.                      
018500                                                                          
018600     EJECT                                                                
018700 01   RUBRIK2.                                                            
018800         05  FILLER                      PIC X(3) VALUE SPACE.            
018900         05  FILLER                      PIC X(13)                        
019000                                         VALUE 'ANSK  LEVNR'.             
019100         05  FILLER                      PIC X(11)                        
019200                                         VALUE 'ARTIKELNR'.               
019300         05  FILLER                      PIC X(25)                        
019400                                         VALUE 'SVENSK BENÄMNING'.        
019500         05  FILLER                      PIC X(13)                        
019600                                         VALUE 'BEST.REST'.               
019700         05  FILLER                      PIC X(14)                        
019800                                         VALUE 'BEHOV    ANTAL'.          
019900     EJECT                                                                
020000 01  DETALJ.                                                              
020100     03  FILLER                        PIC X(3) VALUE SPACE.              
020200     03  UT-IDANSK                    PIC Z(2)9(1) VALUE ZERO.            
020300     03  FILLER                        PIC X(2)  VALUE SPACE.             
020400     03  UT-IDLEVNR                    PIC X(5) VALUE SPACE.              
020500     03  FILLER                        PIC X(3)  VALUE SPACE.             
020600     03  UT-IDARTNR                    PIC Z(8)9(1) VALUE ZERO.           
020700     03  FILLER                        PIC X(2)   VALUE SPACE.            
020800     03  UT-BEART-SVE                  PIC X(27) VALUE ZERO.              
020900     03  UT-KVBR                       PIC Z(6)9(1) VALUE ZERO.           
021000     03  FILLER                        PIC X(2) VALUE SPACE.              
021100     03  UT-KVBEHOV                    PIC Z(6)9(1) VALUE ZERO.           
021200     03  FILLER                        PIC X(2)    VALUE SPACE.           
021300     03  UT-KVANTAL                    PIC Z(6)9(1) VALUE ZERO.           
021400                                                                          
021500                                                                          
021600     EJECT                                                                
021700 LINKAGE SECTION.                                                         
021800     SKIP2                                                                
021900*01  -COPY W0008  -PRE BENA-                                              
022100         05  FILLER                      PIC X(1).                        
022200     EJECT                                                                
022300 PROCEDURE DIVISION USING  BENA-PCB.                                      
022400     SKIP2                                                                
022500     ENTRY 'DLITCBL' USING  BENA-PCB.                                     
022600     PERFORM A-INIT                                                       
022700                                                                          
022800     SORT SORTFIL ON ASCENDING                                            
022900                  SORT-IDANSK                                             
023000                  SORT-IDLEVNR                                            
023100                  SORT-IDARTNR                                            
023200          USING   W22144                                                  
023300          OUTPUT PROCEDURE B-BEARBETNING                                  
023400     SKIP2                                                                
023500     IF SORT-RETURN > ZERO                                                
023600       DISPLAY '***  W2217600  - FEL VID SORTERING'                       
023700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
023800                                                                          
023900     ELSE                                                                 
024000       CLOSE LISTA                                                        
024100                                                                          
024200       MOVE ZERO TO RETURN-CODE                                           
024300       GOBACK                                                             
024400     END-IF                                                               
024500     CONTINUE.                                                            
024600     EJECT                                                                
024700 A-INIT SECTION.                                                          
024800     SKIP2                                                                
024900     OPEN OUTPUT LISTA                                                    
025000     SKIP2                                                                
025100***  DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET  ***                       
025200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025300     EJECT                                                                
025400     CONTINUE.                                                            
025500 B-BEARBETNING SECTION.                                                   
025600     SKIP2                                                                
025700     PERFORM S01-LAS-SORTERAD-W22144                                      
025900     PERFORM UNTIL                                                        
026000      NOT ( SORTFIL-EOF = NEJ AND RADRAK = ZERO )                         
026010       MOVE IDANSK TO IDANSKJAMFOR                                        
026100       PERFORM BA-SKRIV-RUBRIKER                                          
026200*                                                                         
026300       PERFORM UNTIL                                                      
026400        NOT ( RADRAK < 44 AND IDANSK = IDANSKJAMFOR )                     
026500         COMPUTE KVANTAL = KVBEHOV - KVBR                                 
026600         PERFORM BB-SKRIV-BLIST-RAD                                       
026700         PERFORM S01-LAS-SORTERAD-W22144                                  
026800       END-PERFORM                                                        
026900       MOVE ZERO TO RADRAK                                                
027100                                                                          
027200     END-PERFORM                                                          
027300     CONTINUE.                                                            
027400     EJECT                                                                
027500 BA-SKRIV-RUBRIKER SECTION.                                               
027600     ADD 1 TO SIDRAK                                                      
027700     MOVE SIDRAK TO SIDMARKERING                                          
027800     MOVE D-VECKA TO UT-D-VECKA                                           
027900     MOVE D-AAR TO UT-D-AAR                                               
028000     MOVE D-MAANAD TO UT-D-MAANAD                                         
028100     MOVE D-DAG TO UT-D-DAG                                               
028200*                                                                         
028300     WRITE LISTPOST FROM RUBRIK1 AFTER PAGE                               
028400     ADD 4 TO RADRAK                                                      
028500*                                                                         
028600     WRITE LISTPOST FROM RUBRIK2 AFTER 2                                  
028700     ADD 2 TO RADRAK                                                      
028800*                                                                         
028900     WRITE LISTPOST FROM TOMRAD AFTER 1                                   
029000     ADD 1 TO RADRAK                                                      
029100     CONTINUE.                                                            
029200     EJECT                                                                
029300 BB-SKRIV-BLIST-RAD SECTION.                                              
029400     SKIP2                                                                
029500     MOVE IDARTNR TO LINK-IDARTNR                                         
029600     MOVE LAES-BENAEMNING TO LINK-KDCALL                                  
029700     CALL W2217610 USING LINK-AREA  BENA-PCB                              
029800                                                                          
029900     SKIP2                                                                
030000     MOVE IDANSK TO UT-IDANSK                                             
030100     MOVE IDLEVNR TO UT-IDLEVNR                                           
030200     MOVE IDARTNR TO UT-IDARTNR                                           
030300     MOVE LINK-BEART-SVE TO UT-BEART-SVE                                  
030400     MOVE KVBR TO UT-KVBR                                                 
030500     MOVE KVBEHOV TO UT-KVBEHOV                                           
030600     MOVE KVANTAL TO UT-KVANTAL                                           
030700*                                                                         
030800     WRITE LISTPOST FROM DETALJ AFTER 1                                   
030900     ADD 1 TO RADRAK                                                      
031000     CONTINUE.                                                            
031100     EJECT                                                                
031200                                                                          
031300 S01-LAS-SORTERAD-W22144 SECTION.                                         
031400     SKIP2                                                                
031500     RETURN SORTFIL INTO W221LI76                                         
031600                           AT END                                         
031700                           MOVE 44 TO RADRAK                              
031800                           MOVE JA TO SORTFIL-EOF                         
031900       END-RETURN                                                         
032000     .                                                                    
