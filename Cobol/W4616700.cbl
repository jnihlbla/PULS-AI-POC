000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4616700.                                                 
001000*AUTHOR.        MARGARETA GABRIELSSON.                                    
001100*DATE-WRITTEN.  JANUARI 1986.                                             
001200                                                                          
001400                                                                          
001700*    FUNKTION:                                                            
001800                                                                          
001900                                                                          
002000*    ABENDKODER:                                                          
002100                                                                          
002200*        U0016    - OM RETURKOD FRÅN SORT                                 
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*- - - - - - - - - - - - INFIL:                                           
003100*                        - -  FIL MED BYPASS-POSTER TILL DSP              
003200     SELECT W092X3                       ASSIGN TO UT-S-W46167D1.         
003300     SKIP2                                                                
003400*- - - - - - - - - - - - UTFIL:                                           
003500*                        - -  FIL MED BYPASS-POSTER TILL DSP              
003600     SELECT W46167                       ASSIGN TO UT-S-W46167D2.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - SORTFIL:                                         
003900     SELECT SORTFIL                      ASSIGN TO UT-S-W46167DS.         
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W092X3                                                               
004600     RECORDING      F                                                     
004700     BLOCK CONTAINS 0.                                                    
004800     SKIP2                                                                
004900*01  FILLER -COPY W461015    -L.                                          
005100     SKIP2                                                                
005200 FD  W46167                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*01  UT-POST -COPY W461S015   -L.                                         
005800     EJECT                                                                
005900 SD  SORTFIL                                                              
006000                .                                                         
006100*01  POST   -COPY W461015    -PRE SORT-                                   
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006401                                                                          
006410*    -- CHECKED BY WY2000                                                 
006500 77  IDPGM                       PIC X(8)    VALUE 'W4616700'.            
007300     SKIP2                                                                
007400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007500                                                                          
007600 77  JA                          PIC X(1)    VALUE 'J'.                   
007700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007800     SKIP2                                                                
007900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
008000                                                                          
008100 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008200     EJECT                                                                
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
008500   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
008600     SKIP3                                                                
008700*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
008800                                                                          
008900 01  RETURKODER.                                                          
009000   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009100   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009200   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
009300     EJECT                                                                
009400*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
009500                                                                          
009600*01  -COPY W0005       -PRE  POSTSUM-.                                    
009800     EJECT                                                                
009900******************************************************************        
010000*         SORTERADE POSTER                                       *        
010100******************************************************************        
010200*01  AREA  -PRE WSORT-  -COPY W461015                                     
010400     EJECT                                                                
010500******************************************************************        
010600*         UTAREA                                                 *        
010700******************************************************************        
010800*01  AREA -COPY W461S015   -PRE UT-.                                      
011000     EJECT                                                                
011100 PROCEDURE DIVISION.                                                      
011200     SKIP2                                                                
011300     PERFORM A-INIT                                                       
011400                                                                          
011500     SORT SORTFIL ASCENDING                                               
011600                  SORT-BYPASS-IDDISTR                                     
011700                  SORT-BYPASS-IDKUNDNR                                    
011800                  SORT-BYPASS-IDORDNR                                     
011900                  SORT-BYPASS-IDARTNR                                     
012000          USING   W092X3                                                  
012100          OUTPUT PROCEDURE B-BEARBETNING                                  
012200     SKIP2                                                                
012300     IF SORT-RETURN > ZERO                                                
012400       DISPLAY '***  W4616700  - FEL VID SORTERING'                       
012500       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
012600     ELSE                                                                 
012700       PERFORM Z-FINIT                                                    
012800       MOVE ZERO TO RETURN-CODE                                           
012900       GOBACK                                                             
013000                                                                          
013100     END-IF                                                               
013200     CONTINUE.                                                            
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500     SKIP2                                                                
013600     OPEN OUTPUT W46167                                                   
013700     CONTINUE.                                                            
013800     EJECT                                                                
013900 B-BEARBETNING SECTION.                                                   
014000     SKIP2                                                                
014100     PERFORM S01-LAS-SORTERAD-BYPASS                                      
014200     MOVE +0 TO UT-BYPASS-SOR0-IDLOPNR                                    
014300     PERFORM UNTIL                                                        
014400      NOT ( SORTFIL-EOF = NEJ )                                           
014500       MOVE WSORT-BYPASS-IDDISTR TO UT-BYPASS-SOR0-IDDISTR                
014600       MOVE WSORT-BYPASS-IDKUNDNR TO UT-BYPASS-SOR0-IDKUNDNR              
014700       MOVE ZERO TO UT-BYPASS-SOR0-IDRONR                                 
014800       MOVE ZERO TO UT-BYPASS-SOR0-TIRODAT                                
014900       MOVE '000' TO UT-BYPASS-SOR0-IDPTYP                                
015000       MOVE WSORT-BYPASS-IDPTYP TO UT-BYPASS-IDPTYP                       
015100       MOVE WSORT-BYPASS-IDDISTR TO UT-BYPASS-IDDISTR                     
015200       MOVE WSORT-BYPASS-IDKUNDNR TO UT-BYPASS-IDKUNDNR                   
015300       MOVE WSORT-BYPASS-IDORDNR TO UT-BYPASS-IDORDNR                     
015400       MOVE WSORT-BYPASS-KDORDKL TO UT-BYPASS-KDORDKL                     
015500       MOVE WSORT-BYPASS-IDARTNR TO UT-BYPASS-IDARTNR                     
015600       MOVE WSORT-BYPASS-REKSIFFR TO UT-BYPASS-REKSIFFR                   
015700       MOVE WSORT-BYPASS-BERADREF TO UT-BYPASS-BERADREF                   
015800       MOVE WSORT-BYPASS-BEVOLREF TO UT-BYPASS-BEVOLREF                   
015900       MOVE WSORT-BYPASS-KVBEART TO UT-BYPASS-KVBEART                     
016000       MOVE WSORT-BYPASS-KDFAKTYP TO UT-BYPASS-KDFAKTYP                   
016100       MOVE WSORT-BYPASS-KDDSP TO UT-BYPASS-KDDSP                         
016200       MOVE WSORT-BYPASS-FLABON TO UT-BYPASS-FLABON                       
016210       MOVE WSORT-BYPASS-KDTPOTYP TO UT-BYPASS-KDTPOTYP                   
016300       ADD +1 TO UT-BYPASS-SOR0-IDLOPNR                                   
016400       PERFORM S02-SKRIV-POST                                             
016500       PERFORM S01-LAS-SORTERAD-BYPASS                                    
016600     END-PERFORM                                                          
016700     CONTINUE.                                                            
016800     EJECT                                                                
016900 S01-LAS-SORTERAD-BYPASS SECTION.                                         
017000     SKIP2                                                                
017100     RETURN SORTFIL   INTO WSORT-AREA                                     
017200                      AT END MOVE JA TO SORTFIL-EOF                       
017300     END-RETURN                                                           
017400                                                                          
017500     IF SORTFIL-EOF = NEJ                                                 
017600                                                                          
017700       MOVE 'W092X3'            TO POSTSUM-FDNAMN                         
017800       MOVE 'W46167D1'          TO POSTSUM-DDNAMN2                        
017900       MOVE WSORT-BYPASS-IDPTYP TO POSTSUM-TRANSTYP                       
018000       CALL POSTSUM   USING POSTSUM-PARM                                  
018100                                                                          
018200     END-IF                                                               
018300     CONTINUE.                                                            
018400     EJECT                                                                
018500 S02-SKRIV-POST SECTION.                                                  
018600     SKIP2                                                                
018700     WRITE UT-POST FROM UT-AREA                                           
018800     MOVE 'W46167'               TO POSTSUM-FDNAMN                        
018900     MOVE 'W46167D2'             TO POSTSUM-DDNAMN2                       
019000     MOVE UT-BYPASS-SOR0-IDPTYP  TO POSTSUM-TRANSTYP                      
019100     CALL POSTSUM      USING POSTSUM-PARM                                 
019200     CONTINUE.                                                            
019300     EJECT                                                                
019400 Z-FINIT SECTION.                                                         
019500     SKIP2                                                                
019600     CLOSE W46167                                                         
019700     SKIP2                                                                
019800                                                                          
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     CONTINUE.                                                            
