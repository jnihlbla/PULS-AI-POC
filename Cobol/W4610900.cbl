000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4610900.                                                 
000400*AUTHOR.        MARGARETA GABRIELSSON                                     
000500*DATE-WRITTEN.  SEP 1995.                                                 
000600                                                                          
000700*    REMARKS.                                                             
001000*                                                                         
001100*        FAKTURAPOSTER PÅ FIL W46109 SORTERAS I RÄTT                      
001200*        ORDNING FÖR ATT KUNNA FLYTTA FÖRSTA KOLLIPOSTENS                 
001300*        BOLLANUMMER TILL RÄTT ORDERPOST.                                 
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*                                                                         
002100*        U0016    - OM RETURKOD FRÅN SORT                                 
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*- - - - - - - - - - - - INFIL:                                           
003000*                        - -  FIL MED FAKTURATRANSAR                      
003100     SELECT W46102-FAKT                  ASSIGN TO UT-S-W46109D1.         
003200     SKIP2                                                                
003300*- - - - - - - - - - - - UTFIL :                                          
003400*                        - -  W46103 KOMPLETTERAD FIL                     
003500     SELECT W46103-UT                    ASSIGN TO UT-S-W46109D2.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - SORTFIL:                                         
003800     SELECT SORTFIL                      ASSIGN TO UT-S-W46109DS.         
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W46102-FAKT                                                          
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800*01  POST  -COPY W461010    -PRE IN-.                                     
004900*01  POST1 -COPY W461011    -PRE IN-.                                     
005000*01  POST2 -COPY W461012    -PRE IN-.                                     
005100*01  POST3 -COPY W461013    -PRE IN-.                                     
005200     SKIP3                                                                
005300 FD  W46103-UT                                                            
005400     RECORDING      V                                                     
005500     BLOCK CONTAINS 0.                                                    
005600     SKIP2                                                                
005700 01  UTPOST-FHUV.                                                         
005800*03  POST  -COPY W461010   -PRE UT-.                                      
005900     SKIP2                                                                
006000 01  UTPOST-FREF.                                                         
006100*03   POST1 -COPY W461011   -PRE UT-.                                     
006200     SKIP2                                                                
006300 01  UTPOST-FKOLLI.                                                       
006400*03   POST2 -COPY W461012   -PRE UT-.                                     
006500     SKIP2                                                                
006600 01  UTPOST-FRAD.                                                         
006700*03   POST3 -COPY W461013   -PRE UT-.                                     
006800     EJECT                                                                
006900 SD  SORTFIL                                                              
007000                .                                                         
007100*01  POST  -COPY W461010    -PRE SORT-.                                   
007200*01  POST1 -COPY W461011    -PRE SORT-.                                   
007300*01  POST2 -COPY W461012    -PRE SORT-.                                   
007400*01  POST3 -COPY W461013    -PRE SORT-.                                   
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007601                                                                          
007610*    -- CHECKED BY WY2000                                                 
007700*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
007800 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4610900'.            
007900     SKIP2                                                                
008000*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
008100                                                                          
008200 77  JA                          PIC X(1)    VALUE 'J'.                   
008300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
008400     SKIP2                                                                
008500*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
008600                                                                          
008700 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008800*                                                                         
008900     SKIP2                                                                
008910*- - - - - - - - - - - - - -  THE FLAG                                    
008920                                                                          
009000 77  FL-FREF-SKRIVEN             PIC X       VALUE 'J'.                   
009100*                                                                         
010000     EJECT                                                                
010100 01  DYNAMISKA-SUBPROGRAM.                                                
010200   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010400     SKIP3                                                                
010500*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010600                                                                          
010700 01  RETURKODER.                                                          
010800   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010900   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
011000   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011100     EJECT                                                                
011200*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
011300                                                                          
011400*01  -COPY W0005       -PRE  POSTSUM-.                                    
011500     EJECT                                                                
011800*                                                                         
011810 01  SPAR-AREOR.                                                          
011900*    03  AREA11  -COPY W461011     -PRE  SPAR-                            
012100     EJECT                                                                
012102*    03  AREA12  -COPY W461012     -PRE  SPAR-                            
012103     EJECT                                                                
012110 01  WSORT-AREAN.                                                         
012120*    03  AREA  -COPY W461013     -PRE  WSORT-.                            
012130     EJECT                                                                
012200 PROCEDURE DIVISION.                                                      
012300     SKIP2                                                                
012400     PERFORM A-INIT                                                       
012500                                                                          
012600     SORT SORTFIL ASCENDING                                               
012700                  SORT-FRAD-IDFAKT                                        
012800                  SORT-FRAD-IDDISTR                                       
012900                  SORT-FRAD-IDKUNDNR                                      
013000                  SORT-FRAD-IDORDNR                                       
013100                  SORT-FRAD-IDPTYP                                        
013500          USING   W46102-FAKT                                             
013600          OUTPUT PROCEDURE C-OUTPUT-PROCEDURE                             
013700     SKIP2                                                                
013800     IF SORT-RETURN > ZERO                                                
013900       DISPLAY '***  W4610900  - FEL VID SORTERING'                       
014000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
014100     ELSE                                                                 
014200       PERFORM Z-FINIT                                                    
014300       MOVE ZERO TO RETURN-CODE                                           
014400       GOBACK                                                             
014500                                                                          
014600     END-IF                                                               
014700     .                                                                    
014800     EJECT                                                                
014900 A-INIT SECTION.                                                          
015000     SKIP2                                                                
015100     OPEN OUTPUT W46103-UT                                                
015200     SKIP2                                                                
015300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015400     .                                                                    
015500     SKIP2                                                                
015600     EJECT                                                                
015700 C-OUTPUT-PROCEDURE SECTION.                                              
015800     SKIP2                                                                
015900     PERFORM S01-SORT-RETURN                                              
016000     PERFORM UNTIL SORTFIL-EOF = JA                                       
016100       IF WSORT-FRAD-IDPTYP = '010'                                       
016200         PERFORM S02-SKRIV-W46103                                         
016300       ELSE                                                               
016400         IF WSORT-FRAD-IDPTYP = '011'                                     
016500           MOVE WSORT-AREA TO SPAR-AREA11                                 
016510           MOVE NEJ TO FL-FREF-SKRIVEN                                    
016600         ELSE                                                             
016700           IF WSORT-FRAD-IDPTYP = '012'                                   
016701               MOVE WSORT-AREA TO SPAR-AREA12                             
016710               IF FL-FREF-SKRIVEN = NEJ                                   
016900                 MOVE SPAR-FKOLLI-IDTRPBOT TO SPAR-FREF-IDTRPBOT          
016910                 MOVE SPAR-FKOLLI-IDTRPBON TO SPAR-FREF-IDTRPBON          
016911                 MOVE SPAR-AREA11 TO WSORT-AREA                           
016920                 PERFORM S02-SKRIV-W46103                                 
016921                 MOVE JA TO FL-FREF-SKRIVEN                               
016922               END-IF                                                     
016930               MOVE SPAR-AREA12 TO WSORT-AREA                             
016940               PERFORM S02-SKRIV-W46103                                   
017000           ELSE                                                           
017100             IF WSORT-FRAD-IDPTYP = '013'                                 
017200               PERFORM S02-SKRIV-W46103                                   
017300             END-IF                                                       
017301           END-IF                                                         
017302         END-IF                                                           
017310       END-IF                                                             
017500       PERFORM S01-SORT-RETURN                                            
017600                                                                          
017700     END-PERFORM                                                          
017800     .                                                                    
017900 S01-SORT-RETURN    SECTION.                                              
018000     SKIP2                                                                
018100     RETURN SORTFIL           INTO WSORT-AREA                             
018200                      AT END MOVE JA TO SORTFIL-EOF                       
018300     END-RETURN                                                           
018400     .                                                                    
018500                                                                          
018600     SKIP3                                                                
018700 S02-SKRIV-W46103  SECTION.                                               
018800     SKIP2                                                                
018900     EVALUATE WSORT-FRAD-IDPTYP                                           
019000     WHEN '010'                                                           
019100       WRITE UTPOST-FHUV FROM WSORT-AREA                                  
019200     WHEN '011'                                                           
019300       WRITE UTPOST-FREF FROM WSORT-AREA                                  
019400     WHEN '012'                                                           
019500       WRITE UTPOST-FKOLLI FROM WSORT-AREA                                
019600     WHEN '013'                                                           
019700       WRITE UTPOST-FRAD   FROM WSORT-AREA                                
019800     END-EVALUATE                                                         
019900     MOVE 'W46103'            TO POSTSUM-FDNAMN                           
020000     MOVE 'W46109D2'          TO POSTSUM-DDNAMN2                          
020100     MOVE WSORT-FRAD-IDPTYP   TO POSTSUM-TRANSTYP                         
020200     CALL POSTSUM   USING POSTSUM-PARM                                    
020300     .                                                                    
020400                                                                          
020500     SKIP3                                                                
020600     EJECT                                                                
020700 Z-FINIT SECTION.                                                         
020800     SKIP2                                                                
020900     CLOSE   W46103-UT                                                    
021000     SKIP2                                                                
021100*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
021200*                                    SKRIVNA POSTER                       
021300                                                                          
021400     MOVE 'S' TO POSTSUM-OPKOD                                            
021500     CALL POSTSUM USING POSTSUM-PARM                                      
021600     .                                                                    
