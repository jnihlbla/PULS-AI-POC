000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3714S00.                                                
000400 AUTHOR.         ELEONOR ÖSTRÖM.                                          
000500 DATE-WRITTEN.   07/08/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        SLÅR IHOP POSTER MED SAMMA DC, FUNKTIONSGRUPP, DISTRIKT          
001200*        OCH ARTIKELNUMMER.                                               
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- BYTES UPPFÖLJNINGSUNDERLAG                                 
002700     SELECT W3714S                     ASSIGN TO W3714SD1.                
002800     SKIP2                                                                
002900*          --- UT FIL MED BYTESUNDERLAG TILL BORN                         
003000     SELECT W3714T                     ASSIGN TO W3714SD2.                
003100                                                                          
003200     SKIP2                                                                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W3714S                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W3714J  -PRE IN-  -L.                                          
004300                                                                          
004400 FD  W3714T                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W3714J -PRE UT-    -L.                                    
004900                                                                          
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300                                                                          
005400*    -- CHECKED BY WY2000                                                 
005500 77  IDPGM                       PIC X(8)    VALUE 'W3714S00'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900 77  W3714S-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W3714S                       VALUE 'J'.                   
006100     EJECT                                                                
006200                                                                          
006300                                                                          
006400 77  PRAVCOST-HITTAD             PIC X       VALUE 'N'.                   
006500 77  BEART-HITTAD                PIC X       VALUE 'N'.                   
006600                                                                          
006700 01  SPAR-IDARTNR                PIC S9(9)   COMP-3 VALUE ZERO.           
006800 01  SPAR-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
006900 01  SPAR-IDFKNGRP               PIC S9(5)   COMP-3 VALUE ZERO.           
007000 01  SPAR-IDDC                   PIC X(2)           VALUE SPACE.          
007100 01  SPAR-BEART                  PIC X(25)          VALUE SPACE.          
007200                                                                          
007300 01  W-SUMMA                     PIC S9(7)   COMP-3 VALUE ZERO.           
007400                                                                          
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000     EJECT                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008600     SKIP2                                                                
008700*    --- PARAMETRAR TILL ABEND                                            
008800                                                                          
008900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009200     SKIP2                                                                
009300 01  FELTEXT.                                                             
009400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL POSTSUM                                          
009800*                                                                         
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100*01  -COPY WDATAREA                                                       
010200     EJECT                                                                
010300 01  IN-AREA-START               PIC X(24)   VALUE                        
010400                                 'IN-AREA-START  '.                       
010500     SKIP2                                                                
010600                                                                          
010700*01  AREA -COPY W3714J     -PRE IN-                                       
010800     EJECT                                                                
010900                                                                          
011000                                                                          
011100 01  UT-AREA-START               PIC X(24)   VALUE                        
011200                                 'UT-AREA-START  '.                       
011300     SKIP2                                                                
011400                                                                          
011500*01  AREA -COPY W3714J     -PRE UT-                                       
011600     EJECT                                                                
011700                                                                          
011800 PROCEDURE DIVISION.                                                      
011900 MAIN SECTION.                                                            
012000     SKIP2                                                                
012100                                                                          
012200     PERFORM A-INIT                                                       
012300     PERFORM S01-LAES-W3714S                                              
012400     IF NOT END-OF-W3714S                                                 
012500       PERFORM S04-SPARA-UNDAN                                            
012600     END-IF                                                               
012700     PERFORM UNTIL END-OF-W3714S                                          
012800       PERFORM B-BEHANDLA                                                 
012900       PERFORM S01-LAES-W3714S                                            
013000     END-PERFORM                                                          
013100                                                                          
013200     PERFORM BD-SKRIV-SISTA-POSTEN                                        
013300                                                                          
013400     PERFORM Z-FINIT                                                      
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100                                                                          
014200     OPEN INPUT  W3714S                                                   
014300                                                                          
014400     OPEN OUTPUT W3714T                                                   
014500     SKIP2                                                                
014600     ACCEPT DAGENS-DATUM  FROM DATE                                       
014700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014800     PERFORM BB-NOLLSTALL                                                 
014900     .                                                                    
015000     EJECT                                                                
015100 B-BEHANDLA SECTION.                                                      
015200                                                                          
015300                                                                          
015400     IF SPAR-IDARTNR NOT = IN-IDARTNR                                     
015500        PERFORM BC-FLYTTA-TILL-UTFIL                                      
015600        PERFORM S11-SKRIV-W3714T                                          
015700        PERFORM S04-SPARA-UNDAN                                           
015800        PERFORM BB-NOLLSTALL                                              
015900        PERFORM BA-BERAKNA                                                
016000     ELSE                                                                 
016100        IF SPAR-IDDISTR  NOT = IN-IDDISTR                                 
016200           PERFORM BC-FLYTTA-TILL-UTFIL                                   
016300           PERFORM S11-SKRIV-W3714T                                       
016400           PERFORM S04-SPARA-UNDAN                                        
016500           PERFORM BB-NOLLSTALL                                           
016600           PERFORM BA-BERAKNA                                             
016700        ELSE                                                              
016800           IF SPAR-IDFKNGRP NOT = IN-IDFKNGRP                             
016900              PERFORM BC-FLYTTA-TILL-UTFIL                                
017000              PERFORM S11-SKRIV-W3714T                                    
017100              PERFORM S04-SPARA-UNDAN                                     
017200              PERFORM BB-NOLLSTALL                                        
017300              PERFORM BA-BERAKNA                                          
017400           ELSE                                                           
017500              PERFORM BA-BERAKNA                                          
017600           END-IF                                                         
017700        END-IF                                                            
017800     END-IF                                                               
017900                                                                          
018000     .                                                                    
018100     EJECT                                                                
018200 BA-BERAKNA SECTION.                                                      
018300                                                                          
018400     COMPUTE W-SUMMA  = W-SUMMA + IN-KVRETUR                              
018500                                                                          
018600     .                                                                    
018700     EJECT                                                                
018800 BB-NOLLSTALL SECTION.                                                    
018900                                                                          
019000     MOVE ZERO            TO W-SUMMA                                      
019100                                                                          
019200     .                                                                    
019300     EJECT                                                                
019400 BC-FLYTTA-TILL-UTFIL SECTION.                                            
019500                                                                          
019600     MOVE SPAR-IDARTNR        TO UT-IDARTNR                               
019700     MOVE SPAR-IDDISTR        TO UT-IDDISTR                               
019800     MOVE SPAR-IDDC           TO UT-IDDC                                  
019900     MOVE SPAR-IDFKNGRP       TO UT-IDFKNGRP                              
020000     MOVE SPAR-BEART          TO UT-BEART                                 
020100     MOVE W-SUMMA             TO UT-KVRETUR                               
020200                                                                          
020300     .                                                                    
020400     EJECT                                                                
020500 BD-SKRIV-SISTA-POSTEN SECTION.                                           
020600                                                                          
020700     PERFORM BC-FLYTTA-TILL-UTFIL                                         
020800     PERFORM S11-SKRIV-W3714T                                             
020900                                                                          
021000     .                                                                    
021100     EJECT                                                                
021200 Z-FINIT SECTION.                                                         
021300     CLOSE W3714S                                                         
021400           W3714T                                                         
021500     SKIP2                                                                
021600     MOVE 'S' TO POSTSUM-OPKOD                                            
021700     CALL POSTSUM USING POSTSUM-PARM                                      
021800     .                                                                    
021900     EJECT                                                                
022000 S01-LAES-W3714S  SECTION.                                                
022100                                                                          
022200     READ W3714S INTO IN-AREA                                             
022300     AT END                                                               
022400        MOVE HIGH-VALUE TO IN-AREA                                        
022500        SET END-OF-W3714S TO TRUE                                         
022600                                                                          
022700     NOT AT END                                                           
022800        MOVE 'W3714S' TO POSTSUM-FDNAMN                                   
022900        MOVE 'W3714SD1' TO POSTSUM-DDNAMN2                                
023000        MOVE 'IN  '       TO POSTSUM-TRANSTYP                             
023100        CALL POSTSUM USING POSTSUM-PARM                                   
023200     END-READ                                                             
023300     .                                                                    
023400     EJECT                                                                
023500 S04-SPARA-UNDAN SECTION.                                                 
023600                                                                          
023700     MOVE IN-IDDISTR      TO SPAR-IDDISTR                                 
023800     MOVE IN-IDDC         TO SPAR-IDDC                                    
023900     MOVE IN-IDARTNR      TO SPAR-IDARTNR                                 
024000     MOVE IN-IDFKNGRP     TO SPAR-IDFKNGRP                                
024100     MOVE IN-BEART        TO SPAR-BEART                                   
024200                                                                          
024300     .                                                                    
024400     EJECT                                                                
024500 S11-SKRIV-W3714T SECTION.                                                
024600                                                                          
024700     WRITE UT-POST FROM UT-AREA                                           
024800                                                                          
024900     MOVE 'W3714T' TO POSTSUM-FDNAMN                                      
025000     MOVE 'W3714SD2' TO POSTSUM-DDNAMN2                                   
025100     MOVE 'UT  '       TO POSTSUM-TRANSTYP                                
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400     EJECT                                                                
025500 S99-ABEND SECTION.                                                       
025600                                                                          
025700     SKIP2                                                                
025800     MOVE 'S' TO POSTSUM-OPKOD                                            
025900     CALL POSTSUM USING POSTSUM-PARM                                      
026000     CALL ABEND USING RKOD-ABEND                                          
026100                                                                          
026200     .                                                                    
026300     EJECT                                                                
