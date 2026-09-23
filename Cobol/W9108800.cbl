000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9108800.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   10/11/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKAPAR FIL MED BENÄMNINGAR TILL GGC                              
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600*    CHANGE LOG:                                                          
001700*                                                                         
001800*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
001900*                                                                         
002000*      14/08/28 - REDDY RAHUL     - ETRACKER 10238782                     
002100*                                   REDUCE THE SIZE OF EACH FILE          
002200*                                   SENT TO GGC TO LESS THAN 3MB.         
002300*                                   LOGIC OF SPLLITING FILES HAS          
002400*                                   BEEN MOVED TO PROC USING SORT.        
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- ARTIKELREGISTER                                            
003400     SELECT W01160                     ASSIGN TO W91088Z1.                
003500     SKIP2                                                                
003600*          --- BENÄMNINGAR FRÅN WDD3                                      
003700     SELECT W91087                     ASSIGN TO W91088Z2.                
003800     SKIP2                                                                
003900*          --- FIL TILL GGC MED BENÄMNINGAR                               
004000     SELECT W91088                     ASSIGN TO W91088D1.                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W01160                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W01160      -L.                                                
005000     SKIP3                                                                
005100 FD  W91087                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  -COPY W91087      -L.                                                
005600     SKIP3                                                                
005700 FD  W91088                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100*01  POST -COPY W91089 -PRE  UT-  -L.                                     
006200     SKIP3                                                                
006300     EJECT                                                                
006400*---                                                                      
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700 77  IDPGM                       PIC X(8)    VALUE 'W9108800'.            
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007100 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
007200     88  END-OF-W01160                       VALUE 'J'.                   
007300                                                                          
007400 77  W91087-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W91087                       VALUE 'J'.                   
007600                                                                          
007700 77  SW-VILLKOR                  PIC X       VALUE 'J'.                   
007800     88  SW-VILLKOR-OK                       VALUE 'J'.                   
007900     EJECT                                                                
008000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500                                                                          
008600 01  DAGENS-TIAAAAVVD            PIC 9(7).                                
008700 01  FILLER  REDEFINES DAGENS-TIAAAAVVD.                                  
008800         03  DAGENS-TISS         PIC 9(2).                                
008900         03  DAGENS-TIAAVVD      PIC 9(5).                                
009000 01  DAGENS-TIAAAAVVD-10         PIC 9(7).                                
009100 01  FILLER  REDEFINES DAGENS-TIAAAAVVD-10.                               
009200         03  DAGENS-TISS-10      PIC 9(2).                                
009300         03  DAGENS-TIAAVVD-10   PIC 9(5).                                
009400 01  WS-TIAAAAVVD                PIC 9(7)   VALUE ZERO.                   
009500 01  FILLER  REDEFINES WS-TIAAAAVVD.                                      
009600         03  WS-TISS             PIC 9(2).                                
009700         03  WS-TIAAVVD          PIC 9(5).                                
009800                                                                          
009900 01  IX                          PIC S9(3) COMP-3  VALUE +1.              
010000*                                                                         
010100*01  -COPY WWPRODSL                                                       
010200     EJECT                                                                
010300 01  DYNAMISKA-SUBPROGRAM.                                                
010400*                                                                         
010500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010800     SKIP2                                                                
010900*    --- PARAMETRAR TILL ABEND                                            
011000                                                                          
011100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011400     SKIP2                                                                
011500 01  FELTEXT.                                                             
011600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011800 01  ANT-UTPOST                  PIC S9(9)   COMP-3 VALUE ZERO.           
011900     EJECT                                                                
012000*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
012100*01  -COPY WDATAREA                                                       
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL POSTSUM                                          
012400*                                                                         
012500*01  -COPY W0005   -PRE  POSTSUM-                                         
012600     EJECT                                                                
012700 01  REG-AREA-START              PIC X(24)   VALUE                        
012800                                 'REG-AREA-START  '.                      
012900     SKIP2                                                                
013000                                                                          
013100*01  AREA -COPY W01160     -PRE REG-                                      
013200     EJECT                                                                
013300 01  IN-AREA-START               PIC X(24)   VALUE                        
013400                                 'IN-AREA-START  '.                       
013500     SKIP2                                                                
013600                                                                          
013700*01  AREA -COPY W91087     -PRE IN-                                       
013800     EJECT                                                                
013900 01  UT-AREA-START               PIC X(24)   VALUE                        
014000                                 'UT-AREA-START  '.                       
014100     SKIP2                                                                
014200                                                                          
014300*01  AREA -COPY W91089     -PRE UT-                                       
014400     EJECT                                                                
014500 PROCEDURE DIVISION.                                                      
014600 MAIN SECTION.                                                            
014700     SKIP2                                                                
014800                                                                          
014900     PERFORM A-INIT                                                       
015000     PERFORM S01-LAES-W01160                                              
015100     PERFORM S02-LAES-W91087                                              
015200     PERFORM UNTIL END-OF-W01160 OR END-OF-W91087                         
015300       IF REG-CLAG-IDARTNR = IN-IDARTNR                                   
015400          MOVE JA   TO SW-VILLKOR                                         
015500          PERFORM C-KOLLA-VILLKOR                                         
015600          IF SW-VILLKOR-OK                                                
015700             PERFORM B-FLYTTA-IN-UT                                       
015800             PERFORM S11-SKRIV-UT-FILES                                   
015900          END-IF                                                          
016000          PERFORM S01-LAES-W01160                                         
016100          PERFORM S02-LAES-W91087                                         
016200       ELSE                                                               
016300          IF REG-CLAG-IDARTNR < IN-IDARTNR                                
016400             PERFORM S01-LAES-W01160                                      
016500          ELSE                                                            
016600             PERFORM S02-LAES-W91087                                      
016700          END-IF                                                          
016800       END-IF                                                             
016900     END-PERFORM                                                          
017000                                                                          
017100                                                                          
017200     PERFORM Z-FINIT                                                      
017300                                                                          
017400     MOVE ZERO      TO RETURN-CODE                                        
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT SECTION.                                                          
017900                                                                          
018000     OPEN INPUT  W01160                                                   
018100                 W91087                                                   
018200                                                                          
018300     OPEN OUTPUT W91088                                                   
018400     SKIP2                                                                
018500     ACCEPT DAGENS-DATUM     FROM DATE                                    
018600     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
018700                                                                          
018800     MOVE 'IDAG'               TO DAT-KDDATFORM                           
018900     CALL WDATKONV USING DAT-KDDATFORM,                                   
019000                         DAT-I-TIDATUM,                                   
019100                         DAT-O-TIDATUM,                                   
019200                         DAT-KDSVAR                                       
019300                                                                          
019400     IF DAT-KDSVAR-FEL                                                    
019500        DISPLAY '****  FEL I WDATKONV  *******'                           
019600        PERFORM S99-ABEND                                                 
019700     END-IF                                                               
019800                                                                          
019900     MOVE DAT-TIAAVVD          TO DAGENS-TIAAVVD                          
020000     MOVE DAT-TISEKEL          TO DAGENS-TISS                             
020100     MOVE DAGENS-TIAAAAVVD     TO DAGENS-TIAAAAVVD-10                     
020200     SUBTRACT +10000         FROM DAGENS-TIAAAAVVD-10                     
020300     .                                                                    
020400     EJECT                                                                
020500 B-FLYTTA-IN-UT SECTION.                                                  
020600                                                                          
020700     MOVE IN-IDARTNR           TO UT-IDARTNR                              
020800     MOVE REG-CLAG-IDFKNGRP    TO UT-IDFKNGRP                             
020900     MOVE REG-CLAG-KDERS       TO UT-KDERS                                
021000     MOVE +1                   TO IX                                      
021100     PERFORM UNTIL IX > 17                                                
021200        MOVE IN-BEART-TAB (IX) TO UT-BEART-TAB (IX)                       
021300        ADD +1                 TO IX                                      
021400     END-PERFORM                                                          
021500     .                                                                    
021600     EJECT                                                                
021700 C-KOLLA-VILLKOR SECTION.                                                 
021800                                                                          
021900     MOVE REG-CLAG-TIERSDAT TO WS-TIAAVVD                                 
022000     IF REG-CLAG-TIERSDAT > 50000                                         
022100        MOVE 19             TO WS-TISS                                    
022200     ELSE                                                                 
022300        MOVE 20             TO WS-TISS                                    
022400     END-IF                                                               
022500     IF ((REG-CLAG-KDERS > 19 AND < 30) OR                                
022600         (REG-CLAG-KDERS-UTG > ZERO)) AND                                 
022700         (WS-TIAAAAVVD  < DAGENS-TIAAAAVVD-10)                            
022800         MOVE NEJ           TO SW-VILLKOR                                 
022900     END-IF                                                               
023000     IF REG-CLAG-PRARTSTD NOT > ZERO                                      
023100         MOVE NEJ           TO SW-VILLKOR                                 
023200     END-IF                                                               
023300     IF REG-CLAG-PRARTSJK NOT > ZERO                                      
023400         MOVE NEJ           TO SW-VILLKOR                                 
023500     END-IF                                                               
023600     IF REG-CLAG-IDFKNGRP NOT > ZERO                                      
023700         MOVE NEJ           TO SW-VILLKOR                                 
023800     END-IF                                                               
023900     IF REG-CLAG-FLLSRDEL NOT = JA                                        
024000         MOVE NEJ           TO SW-VILLKOR                                 
024100     END-IF                                                               
024200     MOVE REG-CLAG-KDPRODSL TO TEST-KDPRODSL                              
024300     IF KDPRODSL-BIMA-LOCAL                                               
024400         MOVE NEJ           TO SW-VILLKOR                                 
024500     END-IF                                                               
024600     IF REG-CLAG-KDERS-UTG  = 52 OR                                       
024700        REG-CLAG-KDERS      = 52                                          
024800         MOVE NEJ           TO SW-VILLKOR                                 
024900     END-IF                                                               
025000     MOVE REG-CLAG-TIFINLV  TO WS-TIAAVVD                                 
025100     IF REG-CLAG-TIFINLV  > 50000                                         
025200        MOVE 19             TO WS-TISS                                    
025300     ELSE                                                                 
025400        MOVE 20             TO WS-TISS                                    
025500     END-IF                                                               
025600     IF REG-CLAG-TIFINLV  > DAGENS-TIAAAAVVD                              
025700         MOVE NEJ           TO SW-VILLKOR                                 
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 Z-FINIT SECTION.                                                         
026200     CLOSE W01160                                                         
026300           W91087                                                         
026400           W91088                                                         
026500     SKIP2                                                                
026600     MOVE 'S'             TO POSTSUM-OPKOD                                
026700     CALL POSTSUM      USING POSTSUM-PARM                                 
026800     .                                                                    
026900     EJECT                                                                
027000 S01-LAES-W01160  SECTION.                                                
027100     READ W01160        INTO REG-AREA                                     
027200     AT END                                                               
027300        MOVE HIGH-VALUE   TO REG-AREA                                     
027400        SET END-OF-W01160 TO TRUE                                         
027500                                                                          
027600     NOT AT END                                                           
027700        MOVE 'W01160'     TO POSTSUM-FDNAMN                               
027800        MOVE 'W91088Z1'   TO POSTSUM-DDNAMN2                              
027900        MOVE SPACE        TO POSTSUM-TRANSTYP                             
028000        CALL POSTSUM   USING POSTSUM-PARM                                 
028100     END-READ                                                             
028200     .                                                                    
028300     EJECT                                                                
028400 S02-LAES-W91087  SECTION.                                                
028500     READ W91087        INTO IN-AREA                                      
028600     AT END                                                               
028700        MOVE HIGH-VALUE   TO IN-AREA                                      
028800        SET END-OF-W91087 TO TRUE                                         
028900                                                                          
029000     NOT AT END                                                           
029100        MOVE 'W91087'     TO POSTSUM-FDNAMN                               
029200        MOVE 'W91088Z2'   TO POSTSUM-DDNAMN2                              
029300        MOVE SPACE        TO POSTSUM-TRANSTYP                             
029400        CALL POSTSUM   USING POSTSUM-PARM                                 
029500     END-READ                                                             
029600     .                                                                    
029700     EJECT                                                                
029800 S11-SKRIV-UT-FILES SECTION.                                              
029900                                                                          
030000     ADD +1               TO ANT-UTPOST                                   
030100*--                                                                       
030200     WRITE UT-POST FROM UT-AREA                                           
030300                                                                          
030400     MOVE 'REC'                             TO POSTSUM-TRANSTYP           
030500     MOVE 'W91088'                          TO POSTSUM-FDNAMN             
030600     MOVE 'W91088D1'                        TO POSTSUM-DDNAMN2            
030700     CALL POSTSUM USING POSTSUM-PARM                                      
030800*----                                                                     
030900     .                                                                    
031000     EJECT                                                                
031100 S99-ABEND SECTION.                                                       
031200                                                                          
031300     SKIP2                                                                
031400     MOVE 'S'             TO POSTSUM-OPKOD                                
031500     CALL POSTSUM      USING POSTSUM-PARM                                 
031600     CALL ABEND        USING RKOD-ABEND                                   
031700     .                                                                    
