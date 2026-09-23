000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4616300.                                                 
001000*AUTHOR.        BOSSE B                                                   
001100*DATE-WRITTEN.  NOV 1985.                                                 
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700*        ON ORDERPOSTER TILL IMPORTÖR SORTERAS                            
001800*         OCH NUMRERAS OCH SORTDEL HÄNGS PÅ I                             
001900*         SORTDELEN LÄGGS IDENTITETER SOM GÖR ATT                         
002000*         RDA2 LÄSES PÅ BÄSTA SÄTT I W46110 .                             
002100*        DE RO SOM HÄNDELSEVIS FÖREKOMMER BETRAKTAS                       
002200*         SOM EGEN IDENTITET, SKILD FRÅN LEVERANSORDERN                   
002300*         OCH SORTERAS PÅ RO-NUMMRET.                                     
002400                                                                          
002500*    ABENDKODER:                                                          
002600                                                                          
002700*        U0016    - OM RETURKOD FRÅN SORT                                 
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*- - - - - - - - - - - - INFIL:                                           
003600*                    --  KONKATERNERAD INFIL FRÅN DAGO. & KVANTO.         
003700     SELECT W46163-IN                    ASSIGN TO UT-S-W46163D1.         
003800     SKIP2                                                                
003900*- - - - - - - - - - - - UTFIL :                                          
004000*                                                                         
004100     SELECT W46164-UT                    ASSIGN TO UT-S-W46163D2.         
004200     SKIP2                                                                
004300*- - - - - - - - - - - - SORTFIL:                                         
004400     SELECT SORTFIL                      ASSIGN TO UT-S-W46163DS.         
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP2                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W46163-IN                                                            
005100     RECORDING      F                                                     
005200     BLOCK CONTAINS 0.                                                    
005300     SKIP2                                                                
005400*01  POST  -COPY W461020    -PRE IN-  -L.                                 
005600     SKIP3                                                                
005700 FD  W46164-UT                                                            
005800     RECORDING      V                                                     
005900     BLOCK CONTAINS 0.                                                    
006000     SKIP2                                                                
006100*01  POST  -COPY W461S020   -PRE UT- -L.                                  
006300 SD  SORTFIL                                                              
006400                     .                                                    
006500     SKIP2                                                                
006600*01  POST   -COPY W461S020   -PRE SORT-                                   
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
006910                                                                          
007000*    -- CHECKED BY WY2000                                                 
007500*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007600 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4616300'.            
007800     SKIP2                                                                
007900*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
008000                                                                          
008100 77  JA                          PIC X(1)    VALUE 'J'.                   
008200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
008300     SKIP2                                                                
008400*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
008500                                                                          
008600 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008700 77  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
008800*                                                                         
008900 77  W-IDLOPNR                   PIC S9(5) COMP-3 VALUE ZERO.             
009000     SKIP2                                                                
009100*- - - - - - - - - - - - - -                                              
009200 01  W-ID.                                                                
009300   03  W-ID-IDDISTR              PIC 9(4).                                
009400   03  W-ID-IDKUNDNR             PIC 9(6).                                
009500   03  W-ID-IDORDNR              PIC 9(7).                                
009600 01  SPAR-ID                     PIC X(17).                               
009700     EJECT                                                                
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010100     SKIP3                                                                
010200*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010300                                                                          
010400 01  RETURKODER.                                                          
010500   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010600   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010700   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010800     EJECT                                                                
010900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
011000                                                                          
011100*01  -COPY W0005       -PRE  POSTSUM-.                                    
011300     EJECT                                                                
011400 01  FILLER               PIC X(16) VALUE 'W-AREA'.                       
011500 01  W-AREA.                                                              
011600*03  -COPY W461S020    -PRE  W-.                                          
011800     EJECT                                                                
011900 PROCEDURE DIVISION.                                                      
012000     SKIP2                                                                
012100     PERFORM A-INIT                                                       
012200                                                                          
012300     SORT SORTFIL ASCENDING                                               
012400                  SORT-ONORD-SOR0-IDDISTR                                 
012500                  SORT-ONORD-SOR0-IDKUNDNR                                
012600                  SORT-ONORD-SOR0-IDRONR                                  
012700                  SORT-ONORD-IDARTNR                                      
012800          INPUT PROCEDURE B-FYLL-I                                        
012900          OUTPUT PROCEDURE C-BEARBETA                                     
013000     SKIP2                                                                
013100     IF SORT-RETURN > ZERO                                                
013200       DISPLAY '***  W4616300  - FEL VID SORTERING'                       
013300       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
013400     ELSE                                                                 
013500       PERFORM Z-FINIT                                                    
013600       MOVE ZERO TO RETURN-CODE                                           
013700       GOBACK                                                             
013800                                                                          
013900     END-IF                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400     OPEN INPUT  W46163-IN                                                
014500     OUTPUT W46164-UT                                                     
014600     SKIP2                                                                
014700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014800     .                                                                    
014900     SKIP2                                                                
015000     EJECT                                                                
015100 B-FYLL-I SECTION.                                                        
015200     PERFORM S03-LES-W46163-IN                                            
015300     PERFORM UNTIL                                                        
015400      ( INFIL-EOF = JA )                                                  
015500       MOVE W-ONORD-IDDISTR  TO W-ONORD-SOR0-IDDISTR                      
015600       MOVE W-ONORD-IDKUNDNR TO W-ONORD-SOR0-IDKUNDNR                     
015700       MOVE W-ONORD-IDORDNR  TO W-ONORD-SOR0-IDRONR                       
015800       MOVE W-ONORD-TIORDREG TO W-ONORD-SOR0-TIRODAT                      
015900       MOVE '006'            TO W-ONORD-SOR0-IDPTYP                       
016000       MOVE +0               TO W-ONORD-SOR0-IDLOPNR                      
016100                                                                          
016200       IF W-ONORD-IDRONR > +0                                             
016300         MOVE W-ONORD-IDRONR  TO W-ONORD-SOR0-IDRONR                      
016400         MOVE W-ONORD-TIRODAT TO W-ONORD-SOR0-TIRODAT                     
016500       END-IF                                                             
016600       MOVE W-AREA TO SORT-POST                                           
016700       RELEASE SORT-POST                                                  
016800       PERFORM S03-LES-W46163-IN                                          
016900     END-PERFORM                                                          
017000     .                                                                    
017100     EJECT                                                                
017200 C-BEARBETA SECTION.                                                      
017300     SKIP2                                                                
017400     PERFORM S01-SORT-RETURN                                              
017500     MOVE W-ID TO SPAR-ID                                                 
017600     PERFORM UNTIL                                                        
017700      ( SORTFIL-EOF = JA )                                                
017800       MOVE W-IDLOPNR   TO W-ONORD-SOR0-IDLOPNR                           
017900       PERFORM S02-SKRIV-W46164-UT                                        
018000       PERFORM S01-SORT-RETURN                                            
018100       PERFORM UNTIL                                                      
018200        NOT ( SORTFIL-EOF NOT = JA AND W-ID = SPAR-ID )                   
018300         MOVE +0 TO W-ONORD-SOR0-IDRONR                                   
018400         MOVE +0 TO W-ONORD-SOR0-TIRODAT                                  
018500         MOVE W-IDLOPNR   TO W-ONORD-SOR0-IDLOPNR                         
018600         PERFORM S02-SKRIV-W46164-UT                                      
018700         PERFORM S01-SORT-RETURN                                          
018800       END-PERFORM                                                        
018900       MOVE W-ID TO SPAR-ID                                               
019000     END-PERFORM                                                          
019100     .                                                                    
019200     EJECT                                                                
019300 S01-SORT-RETURN    SECTION.                                              
019400     SKIP2                                                                
019500     RETURN SORTFIL           INTO W-AREA                                 
019600                      AT END MOVE JA TO SORTFIL-EOF                       
019700     END-RETURN                                                           
019800                                                                          
019900     ADD +1                     TO W-IDLOPNR                              
020000     MOVE W-ONORD-IDDISTR       TO W-ID-IDDISTR                           
020100     MOVE W-ONORD-IDKUNDNR      TO W-ID-IDKUNDNR                          
020200     MOVE W-ONORD-IDORDNR       TO W-ID-IDORDNR                           
020300     .                                                                    
020400                                                                          
020500     SKIP3                                                                
020600 S02-SKRIV-W46164-UT SECTION.                                             
020700     SKIP2                                                                
020800     WRITE UT-POST  FROM W-AREA                                           
020900                                                                          
021000     MOVE 'W46164'            TO POSTSUM-FDNAMN                           
021100     MOVE 'W46163D2'          TO POSTSUM-DDNAMN2                          
021200     MOVE W-ONORD-IDPTYP      TO POSTSUM-TRANSTYP                         
021300     CALL POSTSUM   USING POSTSUM-PARM                                    
021400     .                                                                    
021500                                                                          
021600     SKIP3                                                                
021700     EJECT                                                                
021800 S03-LES-W46163-IN SECTION.                                               
021900     SKIP2                                                                
022000     READ W46163-IN INTO W-ONORD-W461020                                  
022100     AT END MOVE JA TO INFIL-EOF                                          
022200     END-READ                                                             
022300     IF INFIL-EOF = NEJ                                                   
022400       MOVE 'W4616300'            TO POSTSUM-FDNAMN                       
022500       MOVE 'W4616300D2'          TO POSTSUM-DDNAMN2                      
022600       MOVE W-ONORD-IDPTYP        TO POSTSUM-TRANSTYP                     
022700       CALL POSTSUM USING POSTSUM-PARM                                    
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 Z-FINIT SECTION.                                                         
023200     SKIP2                                                                
023300     CLOSE   W46163-IN                                                    
023400     W46164-UT                                                            
023500     SKIP2                                                                
023600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023700*                                    SKRIVNA POSTER                       
023800                                                                          
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
