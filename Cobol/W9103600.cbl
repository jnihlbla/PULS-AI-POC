000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W9103600.                                        
000400 AUTHOR.                 GUNNAR LARSSON, IDK.                             
000500 DATE-WRITTEN.           DEC 1981.                                        
000600                                                                          
001600*                                                                         
001700*    FUNKTION:                                                            
001800*             PROGRAMMET LÄSER W44061                                     
001900*             OCH SKAPAR POSTER PÅ W91036                                 
002000*                                                                         
002100*    POSTER FÖR DISTRIKT NOAC-DISTRIKT SKOTTLAND, FRANKRIKE,              
002110*                        SPANIEN, ITALIEN, FINLAND                        
002120*                        OCH SCHWEIZ SKAPAS INTE.                         
002200                                                                          
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*- - - - - - - - - - - - - - INFILER:                                     
003100                                                                          
003200     SELECT  W44061-TRANS         ASSIGN  UT-S-W91036D1.                  
003300     SKIP2                                                                
003400*- - - - - - - - - - - - - - UTFILER:                                     
003500                                                                          
003600     SELECT  W91036-B02               ASSIGN  UT-S-W91036D2.              
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W44061-TRANS                                                         
004300     LABEL RECORD STANDARD                                                
004400     RECORDING      F                                                     
004500     BLOCK CONTAINS 0.                                                    
004600     SKIP2                                                                
004700*01  W44061-POST -COPY W44060      -L.                                    
004800     SKIP3                                                                
004900 FD  W91036-B02                                                           
005000     LABEL RECORD STANDARD                                                
005100     RECORDING   F                                                        
005200     BLOCK CONTAINS 0.                                                    
005300     SKIP2                                                                
005400*01  W91036-POST   -COPY W910B02        -L.                               
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700     SKIP2                                                                
005701                                                                          
005710*    -- CHECKED BY WY2000                                                 
005800*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
005900 77  PROGRAM-NAMN                PIC X(8)  VALUE 'W9103600'.              
006000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006100     SKIP2                                                                
006200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006300                                                                          
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006510 77  FTG-LV                      PIC 9(2)    VALUE 03.                    
006600                                                                          
006700     SKIP3                                                                
006800 01  E0F-FLAGGOR.                                                         
006900                                                                          
007000   03  W44061-EOF                PIC X(1)    VALUE 'N'.                   
007100     SKIP3                                                                
007200*                                                                         
008120                                                                          
008130**************************** PARAMETRAR TILL W460DIS1                     
008140                                                                          
008150*01   -COPY W460DIS1                                                      
008200     EJECT                                                                
008210*      --- VALID IDDC CODES                                               
008220*                                                                         
008230*01    -COPY WWDC99                                                       
008240       EJECT                                                              
008300                                                                          
008400 01  DYNAMISKA-SUBPROGRAM.                                                
008500*                                                                         
008600   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
008700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
008800   03  W009KSIF                  PIC X(8)    VALUE 'W009KSIF'.            
009100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
009110   03  W460DIS1                  PIC X(8)    VALUE 'W460DIS1'.            
009200     EJECT                                                                
009210*01  -COPY W460LISO                                                       
009220     EJECT                                                                
009300 01  DAGENS-DATUM.                                                        
009400*                                                                         
009500   03  DAGENS-TIAAMMDD           PIC 9(6)    VALUE ZERO.                  
009600   03  FILLER                    REDEFINES DAGENS-TIAAMMDD.               
009700     05  DAGENS-TIAAMMDD-A       PIC 9(1).                                
009800     05  DAGENS-TIAAMMDD-A2      PIC 9(1).                                
009900     05  FILLER                  PIC X(4).                                
010000     SKIP3                                                                
010100 01  W009KSIF-PARAM.                                                      
010200*                                                                         
010300   03  K-IDARTNR                 PIC 9(9)    VALUE ZERO.                  
010400   03  K-IDDISTR                 PIC 9(4)    VALUE ZERO.                  
010500   03  K-LGD-9                   PIC 9(1)    VALUE 9.                     
010600   03  K-LGD-4                   PIC 9(1)    VALUE 4.                     
010700   03  K-REKSIFFR                PIC 9(1)    VALUE ZERO.                  
010800     SKIP3                                                                
010900 01  FILLER                      PIC X(24)   VALUE                        
011000                                             'B02-AREA'.                  
011100*01  AREA     -COPY W910B02    -PRE B02-.                                 
011200     EJECT                                                                
011300 01  FILLER                      PIC X(24)   VALUE                        
011400                                             'W44061-AREA'.               
011500*01  AREA     -COPY W44060     -PRE W44061-.                              
011600 01  IDKUNDRF-X.                                                          
011700   03  IDKUNDRF-1                  PIC 9(5).                              
011800   03  FILLER                      PIC X(5).                              
011900     EJECT                                                                
012000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
012100                                                                          
012200*01  -COPY W0005       -PRE POSTSUM-.                                     
012300     EJECT                                                                
016600 PROCEDURE DIVISION.                                                      
016800     SKIP3                                                                
016900     PERFORM A-INIT                                                       
017000                                                                          
017100     PERFORM S01-LAS-W44061                                               
017200                                                                          
017300     PERFORM UNTIL W44061-EOF    = JA                                     
017400                                                                          
017500       MOVE W44061-RAD-IDDISTR  TO DIS1-IDDISTR                           
017600                                                                          
017610       CALL W460DIS1 USING DIS1-W460DIS1                                  
017620                                                                          
017630                                                                          
017800       IF DIS1-IDLANDX2 = ISO-SPANIEN                                     
018810         CONTINUE                                                         
018900       ELSE                                                               
019210         MOVE W44061-RAD-IDDISTR  TO B02-IDDISTR                          
019300         MOVE W44061-RAD-IDKUNDNR TO B02-IDKUNDNR                         
019700                                                                          
019800         MOVE W44061-RAD-IDKUNDRF TO IDKUNDRF-X                           
019900                                                                          
020000         IF  W44061-RAD-KDORDKL  < 5                                      
020100         AND (W44061-RAD-KDFAKTYP = 'R' OR 'N' OR 'K')                    
020700           EVALUATE TRUE                                                  
020800                                                                          
020900             WHEN W44061-RAD-KDSTARAD = '2'                               
021000               PERFORM B-BEH-STARAD-2                                     
021100                                                                          
021200             WHEN W44061-RAD-KDSTARAD = '3'                               
021300               PERFORM C-BEH-STARAD-3                                     
021400                                                                          
021500             WHEN W44061-RAD-KDSTARAD = '1'                               
021600               PERFORM D-BEH-STARAD-1                                     
021700                                                                          
021800             WHEN OTHER                                                   
021900               CONTINUE                                                   
022000                                                                          
022100           END-EVALUATE                                                   
022200         END-IF                                                           
022300       END-IF                                                             
022400                                                                          
022500       PERFORM S01-LAS-W44061                                             
022600     END-PERFORM                                                          
022700                                                                          
022800     PERFORM Z-FINIT                                                      
022900                                                                          
023000     MOVE ZERO                  TO RETURN-CODE                            
023100     GOBACK.                                                              
023200     EJECT                                                                
023300 A-INIT SECTION.                                                          
023400     SKIP3                                                                
023500     OPEN INPUT  W44061-TRANS                                             
023600          OUTPUT W91036-B02                                               
023700                                                                          
023800     ACCEPT DAGENS-TIAAMMDD     FROM DATE                                 
023900                                                                          
024000     MOVE PROGRAM-NAMN          TO POSTSUM-PROGNAMN.                      
024100     EJECT                                                                
024200 B-BEH-STARAD-2 SECTION.                                                  
024300     SKIP2                                                                
024400     PERFORM S05-RED-B02-GEM                                              
024500                                                                          
024600     MOVE W44061-RAD-DARODAT (3:6) TO B02-TIAAMMDD                        
024800     MOVE 2                        TO B02-KDRO                            
024900                                                                          
025000     PERFORM S02-SKRIV-W91036                                             
025100     .                                                                    
025200     EJECT                                                                
025300 C-BEH-STARAD-3 SECTION.                                                  
025400     SKIP2                                                                
025500     PERFORM S05-RED-B02-GEM                                              
025600                                                                          
025700     MOVE W44061-RAD-TIRES       TO B02-TIAAMMDD                          
025800                                                                          
025900     MOVE 1                      TO B02-KDRO                              
027300                                                                          
027400     PERFORM S02-SKRIV-W91036                                             
027500     .                                                                    
027600     EJECT                                                                
027700 D-BEH-STARAD-1 SECTION.                                                  
027800     SKIP2                                                                
027900     MOVE ZERO                   TO B02-KDRO                              
028000     IF  W44061-RAD-KDTPOTYP = 6                                          
028100       PERFORM DA-SKRIV-POST                                              
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 DA-SKRIV-POST                           SECTION.                         
029900                                                                          
030000     IF  W44061-RAD-FLTPOBEK = JA                                         
030100       MOVE 2                    TO B02-KDRO                              
030200     ELSE                                                                 
030300       MOVE 0                    TO B02-KDRO                              
030400     END-IF                                                               
030500                                                                          
030600     PERFORM S05-RED-B02-GEM                                              
030700     MOVE W44061-RAD-TIREGDAT  TO B02-TIAAMMDD                            
030800     PERFORM S02-SKRIV-W91036                                             
030900     .                                                                    
031000     EJECT                                                                
031100 S01-LAS-W44061 SECTION.                                                  
031200     SKIP2                                                                
031300     READ W44061-TRANS      INTO W44061-AREA                              
031400        AT END MOVE JA           TO W44061-EOF                            
031500     END-READ                                                             
031600      IF W44061-EOF          = NEJ                                        
031700        MOVE 'RAD'             TO POSTSUM-TRANSTYP                        
031800        MOVE 'W44061'          TO POSTSUM-FDNAMN                          
031900        MOVE 'W91036D1'        TO POSTSUM-DDNAMN2                         
032000        CALL   POSTSUM   USING    POSTSUM-PARM                            
032100      END-IF                                                              
032110      .                                                                   
032200     SKIP3                                                                
032300 S02-SKRIV-W91036 SECTION.                                                
032400     SKIP2                                                                
032500     WRITE W91036-POST          FROM B02-AREA                             
032600                                                                          
032700     MOVE B02-IDPTYP            TO POSTSUM-TRANSTYP                       
032800     MOVE 'W91036'              TO POSTSUM-FDNAMN                         
032900     MOVE 'W91036D2'            TO POSTSUM-DDNAMN2                        
033000     CALL  POSTSUM   USING         POSTSUM-PARM                           
033800     .                                                                    
033900     EJECT                                                                
034000 S05-RED-B02-GEM SECTION.                                                 
034100*                                                                         
034200*    REDIGERING AV DE B02-FÄLT SOM ÄR LIKA OAVSETT KDSTARAD.              
034300*                                                                         
034400*                                                                         
034500     MOVE 'B02'                  TO B02-IDPTYP                            
034600                                                                          
035300     MOVE W44061-RAD-IDDC        TO WS-IDDC                               
035500     IF CDC-SE                                                            
035600         MOVE 71                 TO B02-IDSUPPL                           
035700         MOVE 1                  TO B02-REKSUPPL                          
035800     ELSE                                                                 
035900         MOVE 72                 TO B02-IDSUPPL                           
036000         MOVE 9                  TO B02-REKSUPPL                          
036100     END-IF                                                               
037100                                                                          
037200     MOVE IDKUNDRF-1             TO B02-IDORDNR7                          
037300                                                                          
037400     MOVE W44061-RAD-IDARTNR     TO B02-IDARTNR                           
037500                                    K-IDARTNR                             
037600     CALL  W009KSIF   USING         K-IDARTNR                             
037700                                    K-LGD-9                               
037800                                    K-REKSIFFR                            
037900     MOVE K-REKSIFFR             TO B02-REKSIFFR                          
038100     MOVE W44061-RAD-KVART       TO B02-KVBEART                           
038600     MOVE W44061-RAD-KDORDKL     TO B02-KDORDER                           
038720     MOVE W44061-RAD-KDVRINFO    TO B02-KDVRINFO                          
038800     .                                                                    
038900     EJECT                                                                
039000 Z-FINIT SECTION.                                                         
039100     SKIP2                                                                
039200     CLOSE W44061-TRANS                                                   
039300           W91036-B02                                                     
039400                                                                          
039500     MOVE 'S'                   TO POSTSUM-OPKOD                          
039600     CALL   POSTSUM    USING       POSTSUM-PARM.                          
039700     EJECT                                                                
