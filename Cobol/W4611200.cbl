000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4611200.                                                 
000400*AUTHOR.        MARGARETA GABRIELSSON                                     
000500*DATE-WRITTEN.  OKT 1996.                                                 
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*        FAKTURAPOSTER PÅ FIL W46103 SORTERAS I FALLANDE                  
001000*        ORDNING FÖR ATT KUNNA ADDERA SAMTLIGA KOLLIPOSTERS               
001100*        VIKT INOM EN ORDER TILL DESS ORDERPOST.                          
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*                                                                         
001500*        U0016    - OM RETURKOD FRÅN SORT                                 
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*- - - - - - - - - - - - INFIL:                                           
002400*                        - -  FIL MED FAKTURATRANSAR                      
002500     SELECT W46103-FAKT                  ASSIGN TO W46112D1.              
002600     SKIP2                                                                
002700*- - - - - - - - - - - - UTFIL :                                          
002800*                        - -  W46118 KOMPLETTERAD FIL                     
002900     SELECT W46118-UT                    ASSIGN TO W46112D2.              
003000     SKIP2                                                                
003100*- - - - - - - - - - - - SORTFIL:                                         
003200     SELECT SORTFIL                      ASSIGN TO W46112DS.              
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W46103-FAKT                                                          
003900     RECORDING      V                                                     
004000     BLOCK CONTAINS 0.                                                    
004100     SKIP2                                                                
004200*01  POST  -COPY W461010    -PRE IN-.                                     
004300*01  POST1 -COPY W461011    -PRE IN-.                                     
004400*01  POST2 -COPY W461012    -PRE IN-.                                     
004500*01  POST3 -COPY W461013    -PRE IN-.                                     
004600     SKIP3                                                                
004700 FD  W46118-UT                                                            
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005100 01  UTPOST-FHUV.                                                         
005200*03  POST  -COPY W461010    -PRE UT-.                                     
005300     SKIP2                                                                
005400 01  UTPOST-FREF.                                                         
005500*03   POST1 -COPY W461011   -PRE UT-.                                     
005600     SKIP2                                                                
005700 01  UTPOST-FKOLLI.                                                       
005800*03   POST2 -COPY W461012   -PRE UT-.                                     
005900     SKIP2                                                                
006000 01  UTPOST-FRAD.                                                         
006100*03   POST3 -COPY W461013   -PRE UT-.                                     
006200     EJECT                                                                
006300 SD  SORTFIL                                                              
006400                .                                                         
006500*01  POST  -COPY W461010    -PRE SORT-.                                   
006600*01  POST1 -COPY W461011    -PRE SORT-.                                   
006700*01  POST2 -COPY W461012    -PRE SORT-.                                   
006800*01  POST3 -COPY W461013    -PRE SORT-.                                   
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007001                                                                          
007010*    -- CHECKED BY WY2000                                                 
007100*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
007200 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4611200'.            
007300     SKIP2                                                                
007400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007500                                                                          
007600 77  JA                          PIC X(1)    VALUE 'J'.                   
007700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007800     SKIP2                                                                
007900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
008000                                                                          
008100 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008200*                                                                         
008300     SKIP2                                                                
008400*- - - - - - - - - - - - - -  SPAR-FÄLT                                   
008810 01  SPAR-FALT.                                                           
008820     03  SPAR-VKORDBTO-ORDER     PIC S9(6)V9(1) COMP-3.                   
008830     EJECT                                                                
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
009100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009200     SKIP3                                                                
009300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009400                                                                          
009500 01  RETURKODER.                                                          
009600   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009800   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
009900     EJECT                                                                
010000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
010100                                                                          
010200*01  -COPY W0005       -PRE  POSTSUM-.                                    
010300     EJECT                                                                
010400*                                                                         
010500 01  FILLER            PIC X(16)  VALUE 'WSORTAREOR'.                     
011000 01  WSORT-AREAN.                                                         
011100*    03  WSORT-AREAN-X  -COPY W461013   -L.                               
011200     EJECT                                                                
011210     03  FILLER  REDEFINES WSORT-AREAN-X.                                 
011211*      05  -COPY W461012  -PRE  WSORT-                                    
011212     03  FILLER  REDEFINES WSORT-AREAN-X.                                 
011213*      05  -COPY W461011  -PRE  WSORT-                                    
011250     EJECT                                                                
011300 PROCEDURE DIVISION.                                                      
011400     SKIP2                                                                
011500     PERFORM A-INIT                                                       
011600                                                                          
011700     SORT SORTFIL DESCENDING                                              
011800                  SORT-FKOLLI-IDFAKT                                      
011900                  SORT-FKOLLI-IDDISTR                                     
012000                  SORT-FKOLLI-IDKUNDNR                                    
012100                  SORT-FKOLLI-IDORDNR                                     
012200                  SORT-FKOLLI-IDPTYP                                      
012300          USING   W46103-FAKT                                             
012400          OUTPUT PROCEDURE C-OUTPUT-PROCEDURE                             
012500     SKIP2                                                                
012600     IF SORT-RETURN > ZERO                                                
012700       DISPLAY '***  W4611200  - FEL VID SORTERING'                       
012800       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
012900     ELSE                                                                 
013000       PERFORM Z-FINIT                                                    
013100       MOVE ZERO TO RETURN-CODE                                           
013200       GOBACK                                                             
013300                                                                          
013400     END-IF                                                               
013500     .                                                                    
013600     EJECT                                                                
013700 A-INIT SECTION.                                                          
013800     SKIP2                                                                
013900     OPEN OUTPUT W46118-UT                                                
014000     SKIP2                                                                
014100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014110     MOVE ZERO TO SPAR-VKORDBTO-ORDER                                     
014200     .                                                                    
014400     EJECT                                                                
014500 C-OUTPUT-PROCEDURE SECTION.                                              
014600     SKIP2                                                                
014700     PERFORM S01-SORT-RETURN                                              
014800     PERFORM UNTIL SORTFIL-EOF = JA                                       
015200       IF WSORT-FREF-IDPTYP = '012'                                       
015450         ADD WSORT-FKOLLI-VKORDBTO-KOLLI                                  
015460         TO SPAR-VKORDBTO-ORDER                                           
015471       ELSE                                                               
015472         IF WSORT-FREF-IDPTYP = '011'                                     
015474           MOVE SPAR-VKORDBTO-ORDER                                       
015475           TO WSORT-FREF-VKORDBTO-ORDER                                   
015477           MOVE ZERO TO SPAR-VKORDBTO-ORDER                               
015478         ELSE                                                             
015480           CONTINUE                                                       
015482         END-IF                                                           
015500       END-IF                                                             
015510       PERFORM S02-SKRIV-UTFIL                                            
015600       PERFORM S01-SORT-RETURN                                            
017600     END-PERFORM                                                          
017700     .                                                                    
017800 S01-SORT-RETURN    SECTION.                                              
017900     SKIP2                                                                
018000     RETURN SORTFIL           INTO WSORT-AREAN                            
018100                      AT END MOVE JA TO SORTFIL-EOF                       
018200     END-RETURN                                                           
018300     .                                                                    
018400                                                                          
018500     SKIP3                                                                
018600 S02-SKRIV-UTFIL  SECTION.                                                
018700     SKIP2                                                                
018800     EVALUATE WSORT-FKOLLI-IDPTYP                                         
018900     WHEN '010'                                                           
019000       WRITE UTPOST-FHUV   FROM WSORT-AREAN                               
019100     WHEN '011'                                                           
019200       WRITE UTPOST-FREF   FROM WSORT-AREAN                               
019300     WHEN '012'                                                           
019400       WRITE UTPOST-FKOLLI FROM WSORT-AREAN                               
019500     WHEN '013'                                                           
019600       WRITE UTPOST-FRAD   FROM WSORT-AREAN                               
019700     END-EVALUATE                                                         
019800     MOVE 'W46118'              TO POSTSUM-FDNAMN                         
019900     MOVE 'W46112D2'            TO POSTSUM-DDNAMN2                        
020000     MOVE WSORT-FKOLLI-IDPTYP   TO POSTSUM-TRANSTYP                       
020100     CALL POSTSUM   USING POSTSUM-PARM                                    
020200     .                                                                    
020300                                                                          
020500     EJECT                                                                
020600 Z-FINIT SECTION.                                                         
020700     SKIP2                                                                
020800     CLOSE   W46118-UT                                                    
020900     SKIP2                                                                
021000*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
021100*                                    SKRIVNA POSTER                       
021200                                                                          
021300     MOVE 'S' TO POSTSUM-OPKOD                                            
021400     CALL POSTSUM USING POSTSUM-PARM                                      
021500     .                                                                    
