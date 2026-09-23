000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4610200.                                                 
001000*AUTHOR.        N. N.                                                     
001100*DATE-WRITTEN.  NOV 1984.                                                 
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        FAKTURAPOSTER PÅ FIL W46102 SORTERAS I RÄTT                      
001800*        ORDNING FÖR VIPS .                                               
001900*        EFTER SORTERING NUMRERAS POSTERNA OCH                            
002000*        SKRIVS PÅ FIL W46104.                                            
002100*                                                                         
002200*        RESTORDERNR I SORTDELEN PÅ W46104 FIL FYLLS                      
002300*        I FÖR BIPACKADE RADER (RONR NOT = NOLL I RADPOSTEN)              
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*                                                                         
002700*        U0016    - OM RETURKOD FRÅN SORT                                 
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*- - - - - - - - - - - - INFIL:                                           
003600*                        - -  FIL FRÅN FAKTURERINGEN                      
003700     SELECT W46102-FAKT                  ASSIGN TO UT-S-W46102D1.         
003800     SKIP2                                                                
003900*- - - - - - - - - - - - UTFIL :                                          
004000*                        - -  W46104 KOMPLETTERAD FIL                     
004100     SELECT W46104-UT                    ASSIGN TO UT-S-W46102D2.         
004200     SKIP2                                                                
004300*- - - - - - - - - - - - SORTFIL:                                         
004400     SELECT SORTFIL                      ASSIGN TO UT-S-W46102DS.         
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP2                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W46102-FAKT                                                          
005100     RECORDING      V                                                     
005200     BLOCK CONTAINS 0.                                                    
005300     SKIP2                                                                
005400*01  POST  -COPY W461010    -PRE IN-.                                     
005500*01  POST1 -COPY W461011    -PRE IN-.                                     
005600*01  POST2 -COPY W461012    -PRE IN-.                                     
005700*01  POST3 -COPY W461013    -PRE IN-.                                     
005800     SKIP3                                                                
005900 FD  W46104-UT                                                            
006000     RECORDING      V                                                     
006100     BLOCK CONTAINS 0.                                                    
006200     SKIP2                                                                
006300 01  UTPOST-FHUV.                                                         
006400*03  POST  -COPY W461S010   -PRE UT-.                                     
006500     SKIP2                                                                
006600 01  UTPOST-FREF.                                                         
006700*03   POST1 -COPY W461S011   -PRE UT-.                                    
006800     SKIP2                                                                
006900 01  UTPOST-FKOLLI.                                                       
007000*03   POST2 -COPY W461S012   -PRE UT-.                                    
007100     SKIP2                                                                
007200 01  UTPOST-FRAD.                                                         
007300*03   POST3 -COPY W461S013   -PRE UT-.                                    
007400     EJECT                                                                
007500 SD  SORTFIL                                                              
007600                .                                                         
007700*01  POST  -COPY W461010    -PRE SORT-.                                   
007800*01  POST1 -COPY W461011    -PRE SORT-.                                   
007900*01  POST2 -COPY W461012    -PRE SORT-.                                   
008000*01  POST3 -COPY W461013    -PRE SORT-.                                   
008100     EJECT                                                                
008200 WORKING-STORAGE SECTION.                                                 
008210                                                                          
008300*    -- CHECKED BY WY2000                                                 
008800*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
008900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4610200'.            
009100     SKIP2                                                                
009200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
009300                                                                          
009400 77  JA                          PIC X(1)    VALUE 'J'.                   
009500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
009600     SKIP2                                                                
009700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
009800                                                                          
009900 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
010000*                                                                         
010100     SKIP2                                                                
010200*- - - - - - - - - - - - - -  SPAR AREAOR                                 
010300*                                                                         
010400*- - - - - - - - - - - - - -                                              
010500*                                                                         
010600                                                                          
010700 01  W-SUMMOR.                                                            
010800     03  W-IDLOPNR               PIC S9(7)   COMP-3  VALUE ZERO.          
010900*                                                                         
011000*- - - - - - - - - - - - - -                                              
011100                                                                          
011200     EJECT                                                                
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
011500   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
011600     SKIP3                                                                
011700*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
011800                                                                          
011900 01  RETURKODER.                                                          
012000   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
012100   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
012200   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
012300     EJECT                                                                
012400*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
012500                                                                          
012600*01  -COPY W0005       -PRE  POSTSUM-.                                    
012700     EJECT                                                                
012800 01  WSORT-AREA-TOT.                                                      
012900*03  AREA1 -COPY W461SOR0    -PRE  WSORT-.                                
013000*                                                                         
013100*03  AREA  -COPY W461013     -PRE  WSORT-.                                
013200     04   FILLER               PIC X(100).                                
013300     EJECT                                                                
013400 PROCEDURE DIVISION.                                                      
013500     SKIP2                                                                
013600     PERFORM A-INIT                                                       
013700                                                                          
013800     SORT SORTFIL ASCENDING                                               
013900                  SORT-FRAD-IDDISTR                                       
014000                  SORT-FRAD-KDFAKTYP                                      
014100                  SORT-FRAD-IDFAKT                                        
014200                  SORT-FRAD-KDSORT2                                       
014300                  SORT-FRAD-IDKUNDNR                                      
014400                  SORT-FRAD-IDORDNR                                       
014500                  SORT-FRAD-IDKOLLI                                       
014600                  SORT-FRAD-IDARTNR                                       
014700          USING   W46102-FAKT                                             
014800          OUTPUT PROCEDURE C-OUTPUT-PROCEDURE                             
014900     SKIP2                                                                
015000     IF SORT-RETURN > ZERO                                                
015100       DISPLAY '***  W4610200  - FEL VID SORTERING'                       
015200       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
015300     ELSE                                                                 
015400       PERFORM Z-FINIT                                                    
015500       MOVE ZERO TO RETURN-CODE                                           
015600       GOBACK                                                             
015700                                                                          
015800     END-IF                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200     SKIP2                                                                
016300     OPEN OUTPUT W46104-UT                                                
016400     SKIP2                                                                
016500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
016600     .                                                                    
016700     SKIP2                                                                
016800     EJECT                                                                
016900 C-OUTPUT-PROCEDURE SECTION.                                              
017000     SKIP2                                                                
017100     PERFORM S01-SORT-RETURN                                              
017200     PERFORM UNTIL SORTFIL-EOF = JA                                       
017400       MOVE  WSORT-FRAD-IDDISTR  TO WSORT-SOR0-IDDISTR                    
017500       MOVE  WSORT-FRAD-IDKUNDNR TO WSORT-SOR0-IDKUNDNR                   
017600       MOVE  '003'               TO WSORT-SOR0-IDPTYP                     
017700       ADD   +1                  TO W-IDLOPNR                             
017800       MOVE  W-IDLOPNR           TO WSORT-SOR0-IDLOPNR                    
017900       IF    WSORT-FRAD-IDPTYP = '013' AND                                
018000       (WSORT-FRAD-IDRONR NOT = ZERO)                                     
018100         MOVE    WSORT-FRAD-IDRONR  TO WSORT-SOR0-IDRONR                  
018200         MOVE    WSORT-FRAD-TIRODAT TO WSORT-SOR0-TIRODAT                 
018300       ELSE                                                               
018400         MOVE    ZERO               TO WSORT-SOR0-IDRONR                  
018500         MOVE    ZERO               TO WSORT-SOR0-TIRODAT                 
018600       END-IF                                                             
018700       PERFORM S02-SKRIV-W46102                                           
018800       PERFORM S01-SORT-RETURN                                            
018900                                                                          
019000     END-PERFORM                                                          
019100     .                                                                    
019200 S01-SORT-RETURN    SECTION.                                              
019300     SKIP2                                                                
019400     RETURN SORTFIL           INTO WSORT-AREA                             
019500                      AT END MOVE JA TO SORTFIL-EOF                       
019600     END-RETURN                                                           
019700     .                                                                    
019800                                                                          
019900     SKIP3                                                                
020000 S02-SKRIV-W46102  SECTION.                                               
020100     SKIP2                                                                
020200     EVALUATE WSORT-FRAD-IDPTYP                                           
020300     WHEN '010'                                                           
020400       WRITE UTPOST-FHUV FROM WSORT-AREA-TOT                              
020500     WHEN '011'                                                           
020600       WRITE UTPOST-FREF FROM WSORT-AREA-TOT                              
020700     WHEN '012'                                                           
020800       WRITE UTPOST-FKOLLI FROM WSORT-AREA-TOT                            
020900     WHEN '013'                                                           
021000       WRITE UTPOST-FRAD   FROM WSORT-AREA-TOT                            
021100     END-EVALUATE                                                         
021200     MOVE 'W46104'            TO POSTSUM-FDNAMN                           
021300     MOVE 'W46102D2'          TO POSTSUM-DDNAMN2                          
021400     MOVE WSORT-FRAD-IDPTYP   TO POSTSUM-TRANSTYP                         
021500     CALL POSTSUM   USING POSTSUM-PARM                                    
021600     .                                                                    
021700                                                                          
021800     SKIP3                                                                
021900     EJECT                                                                
022000 Z-FINIT SECTION.                                                         
022100     SKIP2                                                                
022200     CLOSE   W46104-UT                                                    
022300     SKIP2                                                                
022400*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
022500*                                    SKRIVNA POSTER                       
022600                                                                          
022700     MOVE 'S' TO POSTSUM-OPKOD                                            
022800     CALL POSTSUM USING POSTSUM-PARM                                      
022900     .                                                                    
