000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W5411000.                                                 
000300 AUTHOR.        CHRISTINA BRUHN.                                          
000400 DATE-WRITTEN.  SEPT 1985.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        PROGRAMMET SKAPAR W54110:                                        
001000*        FAKTURAPOSTER UNDER VECKAN MED DIFF STD - SJK                    
001100*        STÖRRE ÄN 10000 OCH 15% ELLER MINDRE ÄN -10%                     
001200*                                                                         
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*- - - - - - - - - - - - INFIL:                                           
002200                                                                          
002300     SELECT W54105                       ASSIGN TO W54110D1.              
002400                                                                          
002500*- - - - - - - - REPORT  TO BE SENT OUT BY EMAIL (D&P):                   
002600                                                                          
002700     SELECT W54110                       ASSIGN TO W54110D2.              
002800                                                                          
002900*- - - - - - - - - - - - SORTFIL:                                         
003000                                                                          
003100     SELECT SORTER                       ASSIGN TO W54110D4.              
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP2                                                                
003700 FD  W54105                                                               
003800     LABEL RECORD   STANDARD                                              
003900     RECORDING      F                                                     
004000     BLOCK CONTAINS 0.                                                    
004100                                                                          
004200*    -COPY W510942   -L.                                                  
004300                                                                          
004400 FD  W54110                                                               
004500     LABEL RECORD    STANDARD                                             
004600     RECORDING       V                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  W54110-POST                 PIC X(121).                              
005200     EJECT                                                                
005300 SD  SORTER.                                                              
005400                                                                          
005500*01  POST       -COPY W510942  -PRE SORT-.                                
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W5411000'.            
006000                                                                          
006100 77  JA                          PIC X(1)    VALUE 'J'.                   
006200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006300                                                                          
006400 77  W54105-EOF                  PIC X(1)    VALUE 'N'.                   
006500 77  SORTER-EOF                  PIC X(1)    VALUE 'N'.                   
006600                                                                          
006700*- - - - - - - - - - - - - - ARBETSFÄLT - - - - - - - - - - - -           
006800 01  ARBETS-AREOR.                                                        
006900     03  WS-DIFF              PIC S9(9)      VALUE ZERO  COMP-3.          
007000     03  WS-PROC              PIC S9(3)V9(2) VALUE ZERO  COMP-3.          
007100     EJECT                                                                
007200 01  VARIABLER.                                                           
007300     03  WS-RADNR2               PIC S9(3)   VALUE +99   COMP-3.          
007400     03  ANTAL-SIDOR2            PIC S9(3)   VALUE ZERO  COMP-3.          
007500     03  SPAR-PKOD2              PIC S9(3)   VALUE ZERO  COMP-3.          
007600     SKIP3                                                                
007700 01  SWITCHAR.                                                            
007800     03  SKRIV-W54110-SW         PIC X(1)    VALUE  'N'.                  
007900         88  SKRIV-W54110                    VALUE  'J'.                  
008000     SKIP3                                                                
008100*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
008200                                                                          
008300 01  RETURKODER.                                                          
008400   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
008500   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
008600   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
008700     SKIP3                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
009000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009100   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
009200     EJECT                                                                
009300 01  WDATUM                      PIC X(6)    VALUE 'WDATUM'.              
             EJECT                                                              
009500*01  -COPY WDATKORT                                                       
009600     EJECT                                                                
009700*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
009800                                                                          
009900*01  -COPY W0005  -PRE  POSTSUM-.                                         
010000     EJECT                                                                
010100 01  FILLER   PIC X(16)   VALUE '942-POST'.                               
010200                                                                          
010300*01  POST -PRE 942-  -COPY W510942                                        
010400     EJECT                                                                
015400 01  R2-RUBRAD-W54110.                                                    
015500     03  FILLER                   PIC X(2)    VALUE  SPACE.               
015600     03  FILLER                   PIC X(9)    VALUE  'ARTIKELNR'.         
015700     03  FILLER                   PIC X(1)    VALUE  ';'.                 
015800     03  FILLER                   PIC X(5)    VALUE  'DISTR'.             
015900     03  FILLER                   PIC X(1)    VALUE  ';'.                 
016000     03  FILLER                   PIC X(4)    VALUE  'PKOD'.              
016100     03  FILLER                   PIC X(1)    VALUE  ';'.                 
016200     03  FILLER                   PIC X(6)    VALUE  'FAKTNR'.            
016300     03  FILLER                   PIC X       VALUE  ';'.                 
016400     03  FILLER                   PIC X(5)    VALUE  'ANTAL'.             
016500     03  FILLER                   PIC X       VALUE  ';'.                 
016600     03  FILLER                   PIC X(6)    VALUE  'SJK/ST'.            
016700     03  FILLER                   PIC X(1)    VALUE  ';'.                 
016800     03  FILLER                   PIC X(8)    VALUE  'SJKVARDE'.          
016900     03  FILLER                   PIC X(1)    VALUE  ';'.                 
017000     03  FILLER                   PIC X(6)    VALUE  'STD/ST'.            
017100     03  FILLER                   PIC X(1)    VALUE  ';'.                 
017200     03  FILLER                   PIC X(8)    VALUE  'STDVARDE'.          
017300     03  FILLER                   PIC X(1)    VALUE  ';'.                 
017400     03  FILLER                   PIC X(4)    VALUE  'DIFF'.              
017500     03  FILLER                   PIC X(1)    VALUE  ';'.                 
017600     03  FILLER                   PIC X(8)    VALUE  'DIFF%'.             
017700     SKIP2                                                                
018400 01  R2-RAD-W54110.                                                       
018500     03  FILLER                   PIC X(2)    VALUE  SPACE.               
018600     03  R2-ARTNR                 PIC Z(8)9.                              
018700     03  FILLER                   PIC X(1)    VALUE  ';'.                 
018800     03  R2-DISTR                 PIC Z(3)9.                              
018900     03  FILLER                   PIC X(1)    VALUE  ';'.                 
019000     03  R2-PKOD                  PIC Z(2)9.                              
019100     03  FILLER                   PIC X(1)    VALUE  ';'.                 
019200     03  R2-FAKTNR                PIC Z(6)9.                              
019300     03  FILLER                   PIC X       VALUE  ';'.                 
019400     03  R2-KVLEVART              PIC Z(6)9.                              
019500     03  FILLER                   PIC X       VALUE  ';'.                 
019600     03  R2-SJK-ST                PIC Z(6)9.99.                           
019700     03  FILLER                   PIC X(1)    VALUE  ';'.                 
019800     03  R2-SJKVARDE              PIC Z(5)9.999.                          
019900     03  FILLER                   PIC X(1)    VALUE  ';'.                 
020000     03  R2-STD-ST                PIC Z(6)9.99.                           
020100     03  FILLER                   PIC X(1)    VALUE  ';'.                 
020200     03  R2-STDVARDE              PIC Z(5)9.999.                          
020300     03  FILLER                   PIC X(1)    VALUE  ';'.                 
020400     03  R2-DIFF                  PIC Z(2)9.9(3)-.                        
020500     03  FILLER                   PIC X(1)    VALUE  ';'.                 
020600     03  R2-DIFFPROC              PIC Z(3)9.99-.                          
020700     EJECT                                                                
020800 01  W54110-CNTL-REC1.                                                    
020900                                                                          
021000    03  FILLER                  PIC X(15)   VALUE                         
021100                                ' ¤DAPW54110-001'.                        
021200                                                                          
021300    03  FILLER                  PIC X(97)  VALUE SPACE.                   
021400                                                                          
021500    EJECT                                                                 
021600 01  W54110-CNTL-REC2.                                                    
021700                                                                          
021800     03  FILLER                  PIC X(9)    VALUE                        
021900                                 ' ¤DAPW541'.                             
022000                                                                          
022100     03  FILLER                  PIC X(112)  VALUE SPACE.                 
022200     EJECT                                                                
022300 PROCEDURE DIVISION.                                                      
022400 MAIN SECTION.                                                            
022500     PERFORM A-INIT                                                       
022600                                                                          
022700     SORT SORTER ASCENDING                                                
022800                 SORT-KDPRODSL                                            
022900                 SORT-IDARTNR                                             
023000                 SORT-IDFAKT                                              
023100                                                                          
023200     INPUT PROCEDURE B-BEARBETA                                           
023300     OUTPUT PROCEDURE C-SKAPA-LISTOR                                      
023400                                                                          
023500     PERFORM Z-AVSLUTA                                                    
023600                                                                          
023700     MOVE ZERO TO RETURN-CODE                                             
023800     GOBACK                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 A-INIT SECTION.                                                          
024200                                                                          
024300     OPEN INPUT  W54105                                                   
024400          OUTPUT W54110                                                   
024500                                                                          
024600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
           .                                                                    
           EJECT                                                                
024700                                                                          
025700 B-BEARBETA SECTION.                                                      
025800                                                                          
025900     PERFORM S01-LAS-W54105                                               
026000     PERFORM UNTIL W54105-EOF = JA                                        
026100       IF 942-IDPTYP = '942'                                              
026200         IF 942-KDFAKTYP = 'R'                                            
026300             IF 942-SUARTNTO > 10000.00 OR                                
026400                942-SUARTSJK > 10000.00 OR                                
026500                942-SUARTSTD > 10000.00                                   
026600               RELEASE SORT-POST FROM 942-POST                            
026700             END-IF                                                       
026800         END-IF                                                           
026900       END-IF                                                             
027000       PERFORM S01-LAS-W54105                                             
027100                                                                          
027200     END-PERFORM                                                          
027300     .                                                                    
027400     EJECT                                                                
027500 C-SKAPA-LISTOR SECTION.                                                  
027600                                                                          
027800     PERFORM S02-LAS-SORTER                                               
027900     PERFORM UNTIL SORTER-EOF = JA                                        
028000       COMPUTE WS-DIFF ROUNDED = 942-SUARTSJK - 942-SUARTSTD              
028100                                                                          
028300       IF 942-SUARTSTD NOT = ZERO                                         
028400         COMPUTE WS-PROC ROUNDED = WS-DIFF * 100 / 942-SUARTSTD           
028500         CONTINUE                                                         
028600       ELSE                                                               
028700         MOVE ZERO TO WS-PROC                                             
028800       END-IF                                                             
028900                                                                          
029000       MOVE NEJ TO SKRIV-W54110-SW                                        
029100                                                                          
029400       IF WS-PROC > 15.00 AND 942-SUARTSJK > 10000.00                     
029500         MOVE JA TO SKRIV-W54110-SW                                       
029600       END-IF                                                             
029900       IF WS-PROC < -10.00 AND 942-SUARTSTD > 10000.00                    
030000         MOVE JA TO SKRIV-W54110-SW                                       
030100       END-IF                                                             
030200                                                                          
030300                                                                          
030400       IF SKRIV-W54110                                                    
030500         IF ANTAL-SIDOR2 = ZERO                                           
030501            WRITE W54110-POST FROM W54110-CNTL-REC1                       
030502            WRITE W54110-POST FROM W54110-CNTL-REC2                       
030503            WRITE W54110-POST FROM R2-RUBRAD-W54110                       
030520         END-IF                                                           
030600         PERFORM CB-BEARBETNING-W54110                                    
030700       END-IF                                                             
030800       PERFORM S02-LAS-SORTER                                             
030900     END-PERFORM                                                          
031000     .                                                                    
031100     EJECT                                                                
031200 CB-BEARBETNING-W54110 SECTION.                                           
031300                                                                          
031400     IF 942-KVLEVART NOT = +0                                             
031500        COMPUTE R2-SJK-ST = 942-SUARTSJK / 942-KVLEVART                   
031600        COMPUTE R2-STD-ST = 942-SUARTSTD / 942-KVLEVART                   
031700     ELSE                                                                 
031800         MOVE ZERO       TO R2-SJK-ST                                     
031900                            R2-STD-ST                                     
032000     END-IF                                                               
032100     MOVE 942-KVLEVART       TO R2-KVLEVART                               
032200     DIVIDE 942-SUARTSJK BY 1000 GIVING R2-SJKVARDE                       
032300     DIVIDE 942-SUARTSTD BY 1000 GIVING R2-STDVARDE                       
032400     DIVIDE WS-DIFF BY 1000 GIVING R2-DIFF                                
032500                                                                          
032600     MOVE 942-IDARTNR  TO R2-ARTNR                                        
032700     MOVE 942-IDDISTR  TO R2-DISTR                                        
032800     MOVE 942-KDPRODSL TO R2-PKOD                                         
032900     MOVE 942-IDFAKT   TO R2-FAKTNR                                       
033000     MOVE WS-PROC      TO R2-DIFFPROC                                     
033100                                                                          
033400       PERFORM CBA-SKRIV-RAD-W54110                                       
033500     .                                                                    
033600     EJECT                                                                
033700 CBA-SKRIV-RAD-W54110 SECTION.                                            
033800                                                                          
034000     IF 942-KDPRODSL NOT = SPAR-PKOD2                                     
034100       MOVE +99             TO WS-RADNR2                                  
034200       MOVE 942-KDPRODSL    TO SPAR-PKOD2                                 
034300     END-IF                                                               
034800       PERFORM CBB-SKRIV-RUBRIK-W54110                                    
035400     .                                                                    
035500     SKIP3                                                                
035600 CBB-SKRIV-RUBRIK-W54110 SECTION.                                         
035700                                                                          
036000     ADD +1 TO ANTAL-SIDOR2                                               
036200                                                                          
036800     WRITE W54110-POST FROM R2-RAD-W54110                                 
037600     .                                                                    
037700     EJECT                                                                
037800 S01-LAS-W54105 SECTION.                                                  
037900                                                                          
038000     READ W54105      INTO 942-POST                                       
038100     AT END                                                               
038200         MOVE JA             TO W54105-EOF                                
038300     NOT AT END                                                           
038400         IF 942-IDPTYP = '942'                                            
038500             MOVE '942'      TO POSTSUM-TRANSTYP                          
038600             MOVE 'W54105'   TO POSTSUM-FDNAMN                            
038700             MOVE 'W54110D1' TO POSTSUM-DDNAMN2                           
038800             CALL POSTSUM USING POSTSUM-PARM                              
038900         END-IF                                                           
039000     END-READ                                                             
039100     .                                                                    
039200     SKIP3                                                                
039300 S02-LAS-SORTER SECTION.                                                  
039400                                                                          
039600     RETURN SORTER INTO 942-POST                                          
039700     AT END                                                               
039900         MOVE JA             TO SORTER-EOF                                
040100     NOT AT END                                                           
040300         MOVE '942'          TO POSTSUM-TRANSTYP                          
040400         MOVE 'SORTER'       TO POSTSUM-FDNAMN                            
040500         MOVE 'W54110D4'     TO POSTSUM-DDNAMN2                           
040600         CALL POSTSUM USING POSTSUM-PARM                                  
040700     END-RETURN                                                           
040800     .                                                                    
040900     SKIP3                                                                
041000 Z-AVSLUTA SECTION.                                                       
041100                                                                          
041200     CLOSE W54105                                                         
041300           W54110                                                         
041400                                                                          
041500     MOVE 'S' TO POSTSUM-OPKOD                                            
041600     CALL POSTSUM USING POSTSUM-PARM                                      
041700     .                                                                    
