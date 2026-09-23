000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4610300.                                                 
001000*AUTHOR.        STIG MULLER.                                              
001100*DATE-WRITTEN.  NOV  1984.                                                
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700                                                                          
001800*    ABENDKODER:                                                          
001900                                                                          
002000*        U0016    - OM RETURKOD FRÅN SORT                                 
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - INFIL:                                           
002900*                        - -  FIL MED SELEKTERADE POSTER UR               
003000*                             KOLLIREGISTRET                              
003100     SELECT EMB-IN                       ASSIGN TO UT-S-W46103D1.         
003200     SKIP2                                                                
003300*- - - - - - - - - - - - LISTFIL:                                         
003400*                        - -  DAGLIG UPPFÖLJNINGSLISTA                    
003500     SELECT W46105                       ASSIGN TO UT-S-W46103D2.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - SORTFIL:                                         
003800     SELECT SORTFIL                      ASSIGN TO UT-S-W46103DS.         
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  EMB-IN                                                               
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800*01  FILLER -COPY W461014    -L.                                          
005000     SKIP2                                                                
005100 FD  W46105                                                               
005200     RECORDING       V                                                    
005300     BLOCK CONTAINS 0.                                                    
005400     SKIP2                                                                
005500*01  UT-POST -COPY W461S014   -L.                                         
005700     EJECT                                                                
005800 SD  SORTFIL                                                              
005900                .                                                         
006000*01  POST   -COPY W461014    -PRE SORT-                                   
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006310                                                                          
006400*    -- CHECKED BY WY2000                                                 
006900*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007000 77  IDPGM                       PIC X(8)    VALUE 'W4610300'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008100     EJECT                                                                
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
008400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
008500     SKIP3                                                                
008600*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
008700                                                                          
008800 01  RETURKODER.                                                          
008900   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009000   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009100   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
009200     EJECT                                                                
009300*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
009400                                                                          
009500*01  -COPY W0005       -PRE  POSTSUM-.                                    
009700     EJECT                                                                
009800******************************************************************        
009900*         SORTERADE POSTER                                       *        
010000******************************************************************        
010100*01  AREA  -PRE WSORT-  -COPY W461014.                                    
010300     EJECT                                                                
010400******************************************************************        
010500*         UTAREA                                                 *        
010600******************************************************************        
010700*01  AREA -COPY W461S014   -PRE UT-.                                      
010900     EJECT                                                                
011000 PROCEDURE DIVISION.                                                      
011100     SKIP2                                                                
011200     PERFORM A-INIT                                                       
011300                                                                          
011400     SORT SORTFIL ASCENDING                                               
011500                  SORT-EMB-IDDISTR                                        
011600                  SORT-EMB-IDKUNDNR                                       
011700                  SORT-EMB-IDORDNR                                        
011800                  SORT-EMB-IDDC                                           
011900                  SORT-EMB-KDFAKTYP                                       
012000                  SORT-EMB-IDFAKT                                         
012100                  SORT-EMB-KDPALL                                         
012200          USING   EMB-IN                                                  
012300          OUTPUT PROCEDURE B-BEARBETNING                                  
012400     SKIP2                                                                
012500     IF SORT-RETURN > ZERO                                                
012600       DISPLAY '***  W4610300  - FEL VID SORTERING'                       
012700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
012800     ELSE                                                                 
012900       PERFORM Z-FINIT                                                    
013000       MOVE ZERO TO RETURN-CODE                                           
013100       GOBACK                                                             
013200                                                                          
013300     END-IF                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700     SKIP2                                                                
013800     OPEN OUTPUT W46105                                                   
013900     .                                                                    
014000     EJECT                                                                
014100 B-BEARBETNING SECTION.                                                   
014200     SKIP2                                                                
014300     PERFORM S01-LAS-SORTERAD-EMB-IN                                      
014400     MOVE +0 TO UT-EMB-SOR0-IDLOPNR                                       
014500     PERFORM UNTIL SORTFIL-EOF = JA                                       
014700       MOVE WSORT-EMB-IDDISTR TO UT-EMB-SOR0-IDDISTR                      
014800       MOVE WSORT-EMB-IDKUNDNR TO UT-EMB-SOR0-IDKUNDNR                    
014900       MOVE ZERO TO UT-EMB-SOR0-IDRONR                                    
015000       MOVE ZERO TO UT-EMB-SOR0-TIRODAT                                   
015100       MOVE '004' TO UT-EMB-SOR0-IDPTYP                                   
015200       MOVE WSORT-EMB-IDPTYP TO UT-EMB-IDPTYP                             
015300       MOVE WSORT-EMB-IDDISTR TO UT-EMB-IDDISTR                           
015400       MOVE WSORT-EMB-IDKUNDNR TO UT-EMB-IDKUNDNR                         
015500       MOVE WSORT-EMB-IDORDNR TO UT-EMB-IDORDNR                           
015600       MOVE WSORT-EMB-IDDC    TO UT-EMB-IDDC                              
015700       MOVE WSORT-EMB-KDFAKTYP TO UT-EMB-KDFAKTYP                         
015800       MOVE WSORT-EMB-IDFAKT TO UT-EMB-IDFAKT                             
015900       MOVE WSORT-EMB-TIFAKT TO UT-EMB-TIFAKT                             
016000       MOVE WSORT-EMB-KDPALL TO UT-EMB-KDPALL                             
016100       MOVE ZERO TO UT-EMB-KVPALL                                         
016200       MOVE ZERO TO UT-EMB-KVKRAG                                         
016300       MOVE ZERO TO UT-EMB-KVLOCK                                         
016400       PERFORM UNTIL                                                      
016500        NOT ( SORTFIL-EOF NOT = JA AND WSORT-EMB-IDDISTR =                
016600          UT-EMB-IDDISTR AND WSORT-EMB-IDKUNDNR =                         
016700          UT-EMB-IDKUNDNR AND WSORT-EMB-IDORDNR =                         
016800          UT-EMB-IDORDNR AND WSORT-EMB-KDFAKTYP =                         
016900          UT-EMB-KDFAKTYP AND WSORT-EMB-IDFAKT =                          
017000          UT-EMB-IDFAKT AND WSORT-EMB-KDPALL = UT-EMB-KDPALL )            
017100         ADD WSORT-EMB-KVPALL TO UT-EMB-KVPALL                            
017200         ADD WSORT-EMB-KVKRAG TO UT-EMB-KVKRAG                            
017300         ADD WSORT-EMB-KVLOCK TO UT-EMB-KVLOCK                            
017400         PERFORM S01-LAS-SORTERAD-EMB-IN                                  
017500       END-PERFORM                                                        
017600       ADD +1 TO UT-EMB-SOR0-IDLOPNR                                      
017700       PERFORM S10-SKRIV-POST                                             
017800     END-PERFORM                                                          
017900     .                                                                    
018000     EJECT                                                                
018100 S01-LAS-SORTERAD-EMB-IN SECTION.                                         
018200     SKIP2                                                                
018300     RETURN SORTFIL   INTO WSORT-AREA                                     
018400                      AT END MOVE JA TO SORTFIL-EOF                       
018500     END-RETURN                                                           
018600                                                                          
018700     IF SORTFIL-EOF = NEJ                                                 
018800                                                                          
018900       MOVE 'EMB-IN'            TO POSTSUM-FDNAMN                         
019000       MOVE 'W46103D1'          TO POSTSUM-DDNAMN2                        
019100       MOVE WSORT-EMB-IDPTYP    TO POSTSUM-TRANSTYP                       
019200       CALL POSTSUM   USING POSTSUM-PARM                                  
019300                                                                          
019400     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 S10-SKRIV-POST SECTION.                                                  
019800     SKIP2                                                                
019900     WRITE UT-POST FROM UT-AREA                                           
020000     MOVE 'W46105'               TO POSTSUM-FDNAMN                        
020100     MOVE 'W46103D2'             TO POSTSUM-DDNAMN2                       
020200     MOVE UT-EMB-SOR0-IDPTYP     TO POSTSUM-TRANSTYP                      
020300     CALL POSTSUM      USING POSTSUM-PARM                                 
020400     .                                                                    
020500     EJECT                                                                
020600 Z-FINIT SECTION.                                                         
020700     SKIP2                                                                
020800     CLOSE W46105                                                         
020900     SKIP2                                                                
021000                                                                          
021100     MOVE 'S' TO POSTSUM-OPKOD                                            
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     .                                                                    
