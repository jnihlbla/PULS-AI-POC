000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4610600.                                                 
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
002900     SELECT SERV-IN                      ASSIGN TO UT-S-W46106D1.         
003000     SKIP2                                                                
003100*- - - - - - - - - - - - UTFILER:                                         
003200     SELECT W46109                       ASSIGN TO UT-S-W46106D2.         
003300     SELECT W46110                       ASSIGN TO UT-S-W46106D3.         
003400     SKIP2                                                                
003500*- - - - - - - - - - - - SORTFIL:                                         
003600     SELECT SORTFIL                      ASSIGN TO UT-S-W46106DS.         
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  SERV-IN                                                              
004300     RECORDING      F                                                     
004400     BLOCK CONTAINS 0.                                                    
004500     SKIP2                                                                
004600*01  FILLER -COPY W461009 -L.                                             
004800     EJECT                                                                
004900 FD  W46109                                                               
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS 0.                                                    
005200     SKIP2                                                                
005300*01  W46109-POST -COPY W461S009 -L.                                       
005500     EJECT                                                                
005600 FD  W46110                                                               
005700     RECORDING       V                                                    
005800     BLOCK CONTAINS 0.                                                    
005900     SKIP2                                                                
006000*01  W46110-POST   -COPY W461019  -L.                                     
006200     EJECT                                                                
006300 SD  SORTFIL                                                              
006400                .                                                         
006500*01  POST   -COPY W461009 -PRE SORT-                                      
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006810                                                                          
006900*    -- CHECKED BY WY2000                                                 
007400*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007500 77  IDPGM                       PIC X(8)    VALUE 'W4610600'.            
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007900                                                                          
008000 77  JA                          PIC X(1)    VALUE 'J'.                   
008100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
008200     SKIP2                                                                
008300*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
008400                                                                          
008500 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008600     EJECT                                                                
008700 01  DYNAMISKA-SUBPROGRAM.                                                
008800   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
008900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009000     SKIP3                                                                
009100*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009200                                                                          
009300 01  RETURKODER.                                                          
009400   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009500   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009600   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
009700     EJECT                                                                
009800*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
009900                                                                          
010000*01  -COPY W0005       -PRE  POSTSUM-.                                    
010200     EJECT                                                                
010300******************************************************************        
010400*         SORTERADE POSTER                                       *        
010500******************************************************************        
010600*01  AREA  -PRE WSORT-  -COPY W461009.                                    
010800     EJECT                                                                
010900******************************************************************        
011000*         W46109-AREA                                            *        
011100******************************************************************        
011200*01  AREA -COPY W461S009   -PRE W46109-.                                  
011400     EJECT                                                                
011500******************************************************************        
011600*         W46110-AREA                                            *        
011700******************************************************************        
011800*01  AREA  -COPY W461019    -PRE W46110-.                                 
012000     EJECT                                                                
012100 PROCEDURE DIVISION.                                                      
012200     SKIP2                                                                
012300     PERFORM A-INIT                                                       
012400                                                                          
012500     SORT SORTFIL ASCENDING                                               
012600                  SORT-SERV-IDDISTR                                       
012700                  SORT-SERV-IDKUNDNR                                      
012800                  SORT-SERV-IDORDNR                                       
012900                  SORT-SERV-IDARTNR                                       
013000          USING   SERV-IN                                                 
013100          OUTPUT PROCEDURE B-BEARBETNING                                  
013200     SKIP2                                                                
013300     IF SORT-RETURN > ZERO                                                
013400       DISPLAY '***  W4610600  - FEL VID SORTERING'                       
013500       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
013600     ELSE                                                                 
013700       PERFORM Z-FINIT                                                    
013800       MOVE ZERO TO RETURN-CODE                                           
013900       GOBACK                                                             
014000                                                                          
014100     END-IF                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500     SKIP2                                                                
014600     OPEN OUTPUT W46109 W46110                                            
014700     .                                                                    
014800     EJECT                                                                
014900 B-BEARBETNING SECTION.                                                   
015000     SKIP2                                                                
015100     PERFORM S01-LAS-SORTERAD-SERV-IN                                     
015200     MOVE +1 TO W46109-SERV-SOR0-IDLOPNR                                  
015300     PERFORM UNTIL SORTFIL-EOF = JA                                       
015500       MOVE WSORT-SERV-IDDISTR TO W46109-SERV-SOR0-IDDISTR                
015600       MOVE WSORT-SERV-IDKUNDNR TO W46109-SERV-SOR0-IDKUNDNR              
015700       MOVE ZERO TO W46109-SERV-SOR0-IDRONR                               
015800       MOVE ZERO TO W46109-SERV-SOR0-TIRODAT                              
015900       MOVE '005' TO W46109-SERV-SOR0-IDPTYP                              
016000       ADD +1 TO W46109-SERV-SOR0-IDLOPNR                                 
016100       MOVE WSORT-SERV-IDPTYP TO W46109-SERV-IDPTYP                       
016200                                 W46110-SERVGR-IDPTYP                     
016300       MOVE WSORT-SERV-IDDC     TO W46109-SERV-IDDC                       
016400       MOVE WSORT-SERV-IDDISTR TO W46109-SERV-IDDISTR                     
016500                                 W46110-SERVGR-IDDISTR                    
016600       MOVE WSORT-SERV-IDKUNDNR TO W46109-SERV-IDKUNDNR                   
016700                                 W46110-SERVGR-IDKUNDNR                   
016800       MOVE WSORT-SERV-IDORDNR TO W46109-SERV-IDORDNR                     
016900                                 W46110-SERVGR-IDORDNR                    
017000       MOVE WSORT-SERV-KDORDKL TO W46109-SERV-KDORDKL                     
017100       MOVE WSORT-SERV-IDARTNR TO W46109-SERV-IDARTNR                     
017200       MOVE WSORT-SERV-REKSIFFR TO W46109-SERV-REKSIFFR                   
017300       MOVE WSORT-SERV-KDPRODSL TO W46109-SERV-KDPRODSL                   
017400                                 W46110-SERVGR-KDPRODSL                   
017500       MOVE WSORT-SERV-KVBEART TO W46109-SERV-KVBEART                     
017600                                 W46110-SERVGR-KVBEART                    
017700       MOVE WSORT-SERV-TIORDREG TO W46109-SERV-TIORDREG                   
017800                                 W46110-SERVGR-TIORDREG                   
017900       MOVE ZERO                TO W46110-SERVGR-KVLEVART                 
018000       MOVE WSORT-SERV-KDFAKTYP TO W46109-SERV-KDFAKTYP                   
018100       PERFORM S10-SKRIV-POSTER                                           
018200       PERFORM S01-LAS-SORTERAD-SERV-IN                                   
018300     END-PERFORM                                                          
018400     .                                                                    
018500     EJECT                                                                
018600 S01-LAS-SORTERAD-SERV-IN SECTION.                                        
018700     SKIP2                                                                
018800     RETURN SORTFIL   INTO WSORT-AREA                                     
018900                      AT END MOVE JA TO SORTFIL-EOF                       
019000     END-RETURN                                                           
019100                                                                          
019200     IF SORTFIL-EOF = NEJ                                                 
019300                                                                          
019400       MOVE 'SERV-IN'           TO POSTSUM-FDNAMN                         
019500       MOVE 'W46106D1'          TO POSTSUM-DDNAMN2                        
019600       MOVE WSORT-SERV-IDPTYP   TO POSTSUM-TRANSTYP                       
019700       CALL POSTSUM   USING POSTSUM-PARM                                  
019800                                                                          
019900     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 S10-SKRIV-POSTER SECTION.                                                
020300     SKIP2                                                                
020400     WRITE W46109-POST FROM W46109-AREA                                   
020500     MOVE 'W46109'               TO POSTSUM-FDNAMN                        
020600     MOVE 'W46106D2'             TO POSTSUM-DDNAMN2                       
020700     MOVE W46109-SERV-SOR0-IDPTYP TO POSTSUM-TRANSTYP                     
020800     CALL POSTSUM      USING POSTSUM-PARM                                 
020900     SKIP2                                                                
021000     WRITE W46110-POST FROM W46110-AREA                                   
021100     MOVE 'W46110'               TO POSTSUM-FDNAMN                        
021200     MOVE 'W46106D3'             TO POSTSUM-DDNAMN2                       
021300     MOVE W46109-SERV-SOR0-IDPTYP TO POSTSUM-TRANSTYP                     
021400     CALL POSTSUM      USING POSTSUM-PARM                                 
021500     .                                                                    
021600     EJECT                                                                
021700 Z-FINIT SECTION.                                                         
021800     SKIP2                                                                
021900     CLOSE W46109 W46110                                                  
022000     SKIP2                                                                
022100     MOVE 'S' TO POSTSUM-OPKOD                                            
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300     .                                                                    
