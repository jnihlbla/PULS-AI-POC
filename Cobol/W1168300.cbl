000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1168300.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   03/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*      - KOPIERAR PRISFIL INNEHÅLLANDE NYA SJÄLVKOSTPRISER TILL           
000900*        MÅNADSKURS MED 'USA-PRISFIL', OCH KOMPLETTERAD                   
001000*        MED DE NYA SJÄLVKOSTNADSPRISERNA,                                
001100*        PRISERNA RÄKNAS OM TILL MARKNADSBOLAGSVALUTA                     
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002600*       --- INFIL FRÅN W11626                                             
002700     SELECT W11626                     ASSIGN TO W11683D1.                
002800     SKIP2                                                                
003200*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST                            
003300     SELECT W11683                     ASSIGN TO W11683D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
004500                                                                          
005300 FD  W11626                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  -COPY W11620B       -L.                                              
005800     SKIP3                                                                
006500 FD  W11683                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  POST -COPY W11620B  -PRE  UT-  -L.                                   
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200                                                                          
007300 77  IDPGM                       PIC X(8)      VALUE 'W1168300'.          
007400 77  JA                          PIC X         VALUE 'J'.                 
007500 77  NEJ                         PIC X         VALUE 'N'.                 
007600                                                                          
007700 77  WS-KDARTURS                 PIC 9(2)      VALUE ZERO.                
007800 77  WS-IDLEVNR                  PIC 9(5)      VALUE ZERO.                
007900 77  WS-IDLEVNR-LOC              PIC 9(5)      VALUE ZERO.                
008000 77  WS-DATUM                    PIC 9(6)      VALUE ZERO.                
008100                                                                          
008200 01  WS-PRARTSJK                 PIC 9(7)V9(2) VALUE ZERO.                
008300                                                                          
008600 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
008900                                                                          
009000 77  W11626-EOF-SW               PIC X         VALUE 'N'.                 
009100     88  END-OF-W11626                         VALUE 'J'.                 
009200                                                                          
009600 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
009700     EJECT                                                                
009800 01  WS-AAAAMMDD.                                                         
009900     03  WS-SEKEL                       PIC 9(2).                         
010000     03  WS-AAMMDD                      PIC 9(6).                         
010100 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
010200*                                                                         
010300 01  DYNAMISKA-SUBPROGRAM.                                                
010400*                                                                         
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011000     SKIP2                                                                
011100                                                                          
013500 01  FELTEXT.                                                             
013600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013800     EJECT                                                                
014200*    --- PARAMETRAR TILL POSTSUM                                          
014300*                                                                         
014400*01  -COPY W0005   -PRE  POSTSUM-                                         
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
014700*01 -COPY WDATAREA                                                        
014800     EJECT                                                                
014900 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
015000                                                                          
015100*01  AREA -COPY W11620B      -PRE IN-                                     
015200     EJECT                                                                
015700 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
015800                                                                          
015900*01  AREA -COPY W11620B      -PRE UT-                                     
016000     EJECT                                                                
016100*    --- ARBETS-AREOR TILL  IMS-SEKTIONERNA                               
016200*                                                                         
016300 01  FILLER                      PIC X(16)   VALUE 'IMS-NYCKLAR'.         
016400 01  NYCKLAR-TILL-DLI.                                                    
018000                                                                          
018100     03  W-WDB101KY-X.                                                    
018200         05  W-IDPARTNR             PIC X(9)  VALUE SPACE.                
018300         05  W-IDFTG                PIC 9(2)  VALUE ZERO.                 
018400                                                                          
018500     03   W-WDB1B-LOW-X.                                                  
018600         05  W-B-IDLANDX2-LOW      PIC X(2)  VALUE SPACE.                 
018700                                                                          
018800     03  W-WDB1B-HIGH-X.                                                  
018900         05  W-B-IDLANDX2-HIGH     PIC X(2)  VALUE HIGH-VALUE.            
019000                                                                          
019100     03  W-WDB1B1KY-LOW.                                                  
019200         05  W-IDLANDX2-LOW        PIC X(2)    VALUE SPACE.               
019300         05  W-IDMARKBO-LOW        PIC X(1)    VALUE 'A'.                 
019400         05  W-IDPARTNR-LOW        PIC X(9)    VALUE LOW-VALUE.           
019500         05  W-IDFTG-LOW           PIC 9(2)    VALUE ZERO.                
019600                                                                          
019700     03  W-WDB1B1KY-HIGH.                                                 
019800         05  W-IDLANDX2-HIGH       PIC X(2)    VALUE SPACE.               
019900         05  W-IDMARKBO-HIGH       PIC X(1)    VALUE 'G'.                 
020000         05  W-IDPARTNR-HIGH       PIC X(9)    VALUE HIGH-VALUE.          
020100         05  W-IDFTG-HIGH          PIC 9(2)    VALUE 99.                  
020200                                                                          
020300                                                                          
020400*    --- STATUS-KOD FRÅN IMS                                              
020500 01  STATUS-WS                   PIC XX.                                  
020600     88  SEGMENT-FINNS                       VALUE '  '.                  
020700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020900     88  BASEN-SLUT                          VALUE 'GB'.                  
021000     SKIP2                                                                
021100 01  GODK-STATUSKODER.                                                    
021200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021300     SKIP3                                                                
021400 01  SSA1                        PIC X(64).                               
021500 01  SSA2                        PIC X(64).                               
021600     EJECT                                                                
021700*    --- IMS FUNKTIONSKODER                                               
021800*01  -COPY W0003                                                          
021900     EJECT                                                                
022700 01  FILLER                  PIC X(16)   VALUE 'WDB1B1-POST'.             
022800 01  DLI-IO-WDB1B1.                                                       
022900*    03  -COPY WDB1B1                                                     
023000     EJECT                                                                
023100 01  DLI-IO-WDB101.                                                       
023200*    03  -COPY WDB101                                                     
023300     EJECT                                                                
023400 LINKAGE SECTION.                                                         
024100*01  -COPY W0008      -PRE WDB1-                                          
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008      -PRE  WDB1B-                                        
024500     05  FILLER                  PIC X(16).                               
024600     EJECT                                                                
024700 PROCEDURE DIVISION   USING  WDB1-PCB WDB1B-PCB.                          
024900                                                                          
025000 MAIN SECTION.                                                            
025100     ENTRY 'DLITCBL'  USING  WDB1-PCB WDB1B-PCB.                          
025300                                                                          
025400     PERFORM A-INIT                                                       
025500                                                                          
025600     PERFORM S01-LAES-W11626                                              
025700     PERFORM UNTIL END-OF-W11626                                          
025720         PERFORM S04-HAMTA-IDMARKBO                                       
025730                                                                          
025800         MOVE IN-PRARTSJK    TO WS-PRARTSJK                               
025900         PERFORM B-FLYTTA-SKRIV-UTPOST                                    
026000         PERFORM S01-LAES-W11626                                          
026100     END-PERFORM                                                          
026200                                                                          
026300     PERFORM Z-FINIT                                                      
026400                                                                          
026500     MOVE ZERO TO RETURN-CODE                                             
026600     GOBACK                                                               
026700     .                                                                    
026800     EJECT                                                                
026900 A-INIT SECTION.                                                          
027000                                                                          
027100     OPEN INPUT  W11626                                                   
027400          OUTPUT W11683                                                   
027500                                                                          
027600     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
028300     .                                                                    
028400     EJECT                                                                
028500 B-FLYTTA-SKRIV-UTPOST SECTION.                                           
028600                                                                          
028700     MOVE IN-W11620B         TO UT-W11620B                                
028900                                                                          
029000     WRITE UT-POST FROM UT-AREA                                           
029100                                                                          
029200     MOVE 'W11683'   TO POSTSUM-FDNAMN                                    
029300     MOVE 'W11683D2' TO POSTSUM-DDNAMN2                                   
029400     CALL POSTSUM USING POSTSUM-PARM                                      
029500     .                                                                    
029600 Z-FINIT SECTION.                                                         
029700                                                                          
029800     DISPLAY 'IDMARKBO ' WS-IDMARKBO                                      
029900     DISPLAY 'IDPARTNR ' W-IDPARTNR                                       
030000                                                                          
030100     CLOSE W11626                                                         
030200           W11683                                                         
030500                                                                          
030600     MOVE 'S'        TO POSTSUM-OPKOD                                     
030700     CALL POSTSUM USING POSTSUM-PARM                                      
030800     .                                                                    
030900     EJECT                                                                
031000 S01-LAES-W11626  SECTION.                                                
031100                                                                          
031200     READ W11626 INTO IN-AREA                                             
031300     AT END                                                               
031400        SET END-OF-W11626 TO TRUE                                         
031500     NOT AT END                                                           
031600        MOVE 'W11626'   TO POSTSUM-FDNAMN                                 
031700        MOVE 'W11683D1' TO POSTSUM-DDNAMN2                                
031800        CALL POSTSUM USING POSTSUM-PARM                                   
031900     END-READ                                                             
032000     .                                                                    
032100     EJECT                                                                
032200 S04-HAMTA-IDMARKBO SECTION.                                              
032300                                                                          
032700******************** TEST FIX                                             
032900     IF IN-IDLANDX2 = 'PX'                                                
033000       MOVE 'PT'         TO IN-IDLANDX2                                   
033100     END-IF                                                               
033200     IF IN-IDLANDX2 = 'X1' OR 'C1' OR 'TC'                                
033300       MOVE 'CN'         TO IN-IDLANDX2                                   
033400     END-IF                                                               
033500     IF IN-IDLANDX2 = 'X7'                                                
033600       MOVE 'DE'         TO IN-IDLANDX2                                   
033700     END-IF                                                               
033800     IF IN-IDLANDX2 = 'T5'                                                
033900       MOVE 'AU'         TO IN-IDLANDX2                                   
034000     END-IF                                                               
034100******************** TEST FIX                                             
034200     PERFORM S07-HAMTA-IDMARKBO                                           
034300     IF WS-IDMARKBO = SPACE                                               
034400       MOVE 'B'      TO WS-IDMARKBO                                       
034500     END-IF                                                               
034600                                                                          
035000     .                                                                    
035100     SKIP3                                                                
040100 S07-HAMTA-IDMARKBO  SECTION.                                             
040200     IF WS-IDMARKBO = SPACE                                               
040310       MOVE IN-IDLANDX2           TO W-B-IDLANDX2-LOW                     
040400                                     W-B-IDLANDX2-HIGH                    
040500                                     W-IDLANDX2-LOW                       
040600                                     W-IDLANDX2-HIGH                      
040700       PERFORM IMS-GU-WDB1B1                                              
040800       IF SEGMENT-FINNS                                                   
040900         MOVE SEQB-IDPARTNR           TO W-IDPARTNR                       
041000         MOVE SEQB-IDFTG              TO W-IDFTG                          
041100         PERFORM IMS-GU-WDB101                                            
041200         IF SEGMENT-FINNS                                                 
041300           MOVE BET-IDMARKBO          TO WS-IDMARKBO                      
041400         END-IF                                                           
041500       END-IF                                                             
041610       IF IN-IDLANDX2 = 'US'                                              
041700*            USA KAN FÅ FEL MARKNADSBOLAG                                 
041800         MOVE 'E'                     TO WS-IDMARKBO                      
041900       END-IF                                                             
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
043500******************************************                                
046100 IMS-GU-WDB101 SECTION.                                                   
046200     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
046300          DELIMITED BY SIZE INTO SSA1                                     
046400     MOVE '  GE' TO GODK-STATUSKODER                                      
046500     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
046600     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
046700     PERFORM IMS-STATUSKONTROLL                                           
046800     .                                                                    
046900     SKIP3                                                                
047000 IMS-GU-WDB1B1 SECTION.                                                   
047100     STRING 'WDB1B1  (WDB1B1KY=>' W-WDB1B1KY-LOW                          
047200                    '&WDB1B1KY=<' W-WDB1B1KY-HIGH ')'                     
047300          DELIMITED BY SIZE INTO SSA1                                     
047400     MOVE '    ' TO GODK-STATUSKODER                                      
047500     CALL CBLTDLI USING GU WDB1B-PCB DLI-IO-WDB1B1 SSA1                   
047600     MOVE WDB1B-STATUS-CODE TO STATUS-WS                                  
047700     PERFORM IMS-STATUSKONTROLL                                           
047800     .                                                                    
047900     SKIP3                                                                
048000 IMS-STATUSKONTROLL SECTION.                                              
048100                                                                          
048200     SET STATUS-IX TO 1                                                   
048300     SEARCH GODK-STATUS                                                   
048400       AT END CALL FELLOG                                                 
048500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
048600     END-SEARCH                                                           
048700     .                                                                    
048800     EJECT                                                                
