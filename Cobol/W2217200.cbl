000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W2217200.                                                 
000700*              PROGRAM CONVERTED BY                                       
000800*              COBOL CONVERSION AID PO 5785-ABJ                           
000900*              CONVERSION DATE 05/25/91 19:28:48.                         
001000*AUTHOR.        MATS VINNEFORS.                                           
001100*DATE-WRITTEN.  OKTOBER 1980.                                             
001200                                                                          
001300*REMARKS*********************************************************         
001400*                                                               *         
001500*    FUNKTION:                                                  *         
001600*                                                               *         
001700*        PROGRAMMET LÄSER FRÅN W01172, SAMTLIGA POSTER MED      *         
001800*        ART.NR => 5004000 OCH <= 5009999 OCH                   *         
001900*        ART.NR => 8114000 OCH <= 8119999 OCH                   *         
001910*        ART.NR => 8604000 OCH <= 8609999 OCH GÖR OM DESSA      *         
002000*        SÅ ATT 5004XXX OCH 5007XXX = 5001XXX                   *         
002100*               5005XXX OCH 5008XXX = 5002XXX                   *         
002200*               5006XXX OCH 5009XXX = 5003XXX                   *         
002300*               8114XXX OCH 8117XXX = 8111XXX                   *         
002400*               8115XXX OCH 8118XXX = 8112XXX                   *         
002500*               8116XXX OCH 8119XXX = 8113XXX                   *         
002510*               8604XXX OCH 8607XXX = 8601XXX                   *         
002520*               8605XXX OCH 8608XXX = 8602XXX                   *         
002530*               8606XXX OCH 8609XXX = 8603XXX                   *         
002600*        DÄREFTER SORTERING M.A.P. IDARTNR-SORT. I OUTPUT       *         
002700*        PROCEDURE SUMMERAS RESP IDARTNR-SORT:S LAGERSALDO      *         
002800*        TILL RESP BESTÄLLNINGSREST, VAREFTER RESULTATET        *         
002900*        SKRIVS UT PÅ W22173                                    *         
003000*                                                               *         
003100*    ABENDKODER:                                                *         
003200*                                                               *         
003300*        0016    - OM RETURKOD FRÅN SORT                        *         
003400*                                                               *         
003500*****************************************************************         
003600     EJECT                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP2                                                                
004300*****************************************************************         
004400*    INFIL:                                                     *         
004500*         DAGLIGT LAGERBAND                                     *         
004600*****************************************************************         
004700                                                                          
004800     SELECT W01172-IN       ASSIGN TO UT-S-W22172D1.                      
004900     SKIP2                                                                
005000*****************************************************************         
005100*    UTFIL:                                                     *         
005200*         BEST.RESTER FÖR BYTESOBJEKT                           *         
005300*****************************************************************         
005400                                                                          
005500     SELECT W22173-UT       ASSIGN TO UT-S-W22172D2.                      
005600     SKIP2                                                                
005700*****************************************************************         
005800*    SORTFIL:                                                   *         
005900*****************************************************************         
006000                                                                          
006100     SELECT SORTFIL         ASSIGN TO UT-S-W22172DS.                      
006200     EJECT                                                                
006300 DATA DIVISION.                                                           
006400     SKIP2                                                                
006500 FILE SECTION.                                                            
006600     SKIP3                                                                
006700 FD  W01172-IN                                                            
006800     RECORDING      F                                                     
006900     BLOCK CONTAINS 0.                                                    
007000     SKIP2                                                                
007100*    -COPY W011100     -L.                                                
007300     EJECT                                                                
007400 FD  W22173-UT                                                            
007500     RECORDING      F                                                     
007600     BLOCK CONTAINS 0.                                                    
007700*01  POST   -COPY W221721    -PRE UT-.                                    
007900     EJECT                                                                
008000 SD  SORTFIL                                                              
008100                     .                                                    
008200*01  POST   -COPY W221W72    -PRE SORT-.                                  
008400     EJECT                                                                
008500 WORKING-STORAGE SECTION.                                                 
008510                                                                          
008600*    -- CHECKED BY WY2000                                                 
008700 77  INDENT-I PIC X(40) VALUE                                             
008800     'W2217200 91/05/25 TIME 12.49 VILMAII'.                              
008900***  STATEMENT ABOVE GENERATED BY VILMAII CONVERTER                       
009000*                                                                         
009100     SKIP2                                                                
009200*****************************************************************         
009300*    GENERERAT PROGRAM-NAMN                                     *         
009400*****************************************************************         
009500                                                                          
009600 77  PROGRAM-NAMN                REDEFINES INDENT-I                       
009700                                 PIC X(8).                                
009800     SKIP2                                                                
009900*****************************************************************         
010000*    GENERELLA KONSTANTER                                       *         
010100*****************************************************************         
010200                                                                          
010300 77  JA                          PIC X(1)    VALUE 'J'.                   
010400 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010500     SKIP2                                                                
010600*****************************************************************         
010700*    END-OF-FILE-SWITCHAR                                       *         
010800*****************************************************************         
010900                                                                          
011000 77  W01172-EOF                  PIC X(1)    VALUE 'N'.                   
011100 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
011200     EJECT                                                                
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011500     SKIP3                                                                
011600*****************************************************************         
011700*    PARAMETRAR TILL ABEND                                      *         
011800*****************************************************************         
011900                                                                          
012000 01  RETURKODER.                                                          
012100     03  RKOD                    PIC S9(4)  COMP SYNC VALUE ZERO.         
012200     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)  COMP SYNC VALUE +16.          
012300     SKIP3                                                                
012400*****************************************************************         
012500*    INTERNT TESTVÄRDE                                          *         
012600*****************************************************************         
012700                                                                          
012800 01  SUMMA                       PIC 9(9)   COMP-3.                       
012900     EJECT                                                                
013000*****************************************************************         
013100*    BYTES-ARTIKELNR TEST                                       *         
013200*****************************************************************         
013300                                                                          
013400 01  FILLER                      PIC X(16) VALUE 'BYTES-ARTIKEL'.         
013500 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
013600*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
013800     EJECT                                                                
013810*01  FILLER  -COPY WWBYT09     -RED TEST-IDARTNR.                         
013830     EJECT                                                                
013840*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
013850     EJECT                                                                
013900*    -COPY W011100     -PRE ARB-.                                         
014100     EJECT                                                                
014200*    -COPY W221W72   .                                                    
014400     EJECT                                                                
014500*    -COPY W221721   .                                                    
014700     EJECT                                                                
014800 PROCEDURE DIVISION.                                                      
014900     SKIP2                                                                
015000     PERFORM A-INIT                                                       
015100                                                                          
015200     SORT SORTFIL ASCENDING SORT-ARB-IDARTNR-SORT                         
015300          INPUT PROCEDURE B-BEARBETNING                                   
015400          OUTPUT PROCEDURE C-SUMMERING                                    
015500     SKIP2                                                                
015600     IF SORT-RETURN > ZERO                                                
015700       DISPLAY '***  W22172  - FEL VID SORTERING'                         
015800       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
015900                                                                          
016000     ELSE                                                                 
016100       CLOSE W01172-IN W22173-UT                                          
016200       MOVE ZERO TO RETURN-CODE                                           
016300       GOBACK                                                             
016400                                                                          
016500     END-IF                                                               
016600     CONTINUE.                                                            
016700     EJECT                                                                
016800 A-INIT SECTION.                                                          
016900     SKIP2                                                                
017000     OPEN INPUT W01172-IN                                                 
017100     OPEN OUTPUT W22173-UT                                                
017200     MOVE 721 TO 721-IDPTYP                                               
017300     MOVE '8261 ' TO 721-IDLEVNR                                          
017400     CONTINUE.                                                            
017500     EJECT                                                                
017600 B-BEARBETNING SECTION.                                                   
017700     SKIP2                                                                
017800     PERFORM D-LAS-W01172                                                 
017900                                                                          
018000     PERFORM UNTIL                                                        
018100      NOT ( W01172-EOF = NEJ )                                            
018200       MOVE ARB-IDARTNR TO TEST-IDARTNR                                   
018300       IF BYT03-OBJEKT                                                    
018400         MOVE ARB-IDARTNR TO ARB-IDARTNR-OBJ                              
018500                                                                          
018510         IF BYT16-RADIO                                                   
018512            SUBTRACT 1000 FROM ARB-IDARTNR                                
018513         ELSE                                                             
018514            IF BYT09-OBJEKT                                               
018516              SUBTRACT 3000 FROM ARB-IDARTNR                              
018517            ELSE                                                          
018518              SUBTRACT 6000 FROM ARB-IDARTNR                              
018519            END-IF                                                        
018520         END-IF                                                           
019400         MOVE ARB-IDARTNR TO ARB-IDARTNR-SORT                             
019500         MOVE 1            TO ARB-KDCLAGER-OBJ                            
019600         MOVE ARB-KVLS TO ARB-KVLS-OBJ                                    
019700         ADD ARB-KVAKS-CDC TO ARB-KVLS-OBJ                                
019710         ADD ARB-KVAKS-PAV TO ARB-KVLS-OBJ                                
019720         ADD ARB-KVAKS-T   TO ARB-KVLS-OBJ                                
019800         RELEASE SORT-POST FROM ARB-W221W72                               
019900                                                                          
020000       END-IF                                                             
020100       PERFORM D-LAS-W01172                                               
020200                                                                          
020300     END-PERFORM                                                          
020400     CONTINUE.                                                            
020500     EJECT                                                                
020600 C-SUMMERING SECTION.                                                     
020700     SKIP2                                                                
020800     PERFORM E-LAS-SORTERAD-W221W72                                       
020900                                                                          
021000     MOVE 5001000 TO SUMMA                                                
021100     PERFORM UNTIL                                                        
021200      NOT ( SORTFIL-EOF = NEJ )                                           
021300       MOVE ZERO TO 721-KVBR                                              
021400                                                                          
021500       IF ARB-IDARTNR-SORT > SUMMA                                        
021600         MOVE ARB-IDARTNR-SORT TO SUMMA                                   
021700                                                                          
021800       END-IF                                                             
021900       PERFORM UNTIL                                                      
022000        NOT ( ARB-IDARTNR-SORT = SUMMA AND SORTFIL-EOF = NEJ )            
022100         ADD ARB-KVLS-OBJ TO 721-KVBR                                     
022200         MOVE ARB-IDARTNR-SORT TO 721-IDARTNR                             
022300         PERFORM E-LAS-SORTERAD-W221W72                                   
022400                                                                          
022500       END-PERFORM                                                        
022600       IF 721-KVBR NOT = +0                                               
022700         WRITE UT-POST FROM 721-W221721                                   
022800       END-IF                                                             
022900     END-PERFORM                                                          
023000     CONTINUE.                                                            
023100     EJECT                                                                
023200 D-LAS-W01172 SECTION.                                                    
023300     SKIP2                                                                
023400     READ W01172-IN INTO ARB-W011100                                      
023500          AT END                                                          
023600          MOVE JA TO W01172-EOF                                           
023700       END-READ                                                           
023800     CONTINUE.                                                            
023900     EJECT                                                                
024000 E-LAS-SORTERAD-W221W72 SECTION.                                          
024100     SKIP2                                                                
024200     RETURN SORTFIL INTO ARB-W221W72                                      
024300          AT END                                                          
024400          MOVE JA TO SORTFIL-EOF                                          
024500       END-RETURN                                                         
024600     .                                                                    
