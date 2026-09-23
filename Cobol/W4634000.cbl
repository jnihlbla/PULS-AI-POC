000100                                                                          
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W4634000.                                                
000500 AUTHOR.         G÷RAN KJELLSON                                           
000600 DATE-WRITTEN.   VINTERN 2017/18                                          
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        TAR EMOT EDIPOSTER FR≈N DIREKTLEVERANT÷R                         
001300*        SKAPAR POSTER P≈ W46342-FIL SOM SKALL TILL                       
001400*        SALDOUPPDATERING                                                 
001600*                                                                         
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900*          --- EDI-FIL FRÂN DDGS-LEV                                      
003000     SELECT W46342                     ASSIGN TO W46340D1.                
003100                                                                          
003200*          --- GNB-FIL FRÂN EDI TILL SALDOUPPDATERING                     
003300     SELECT W46343                     ASSIGN TO W46340D2.                
003400                                                                          
003500                                                                          
004000 DATA DIVISION.                                                           
004100                                                                          
004200 FILE SECTION.                                                            
004300                                                                          
004400 FD  W46342                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  IN-POST         PIC X(1005).                                         
004900                                                                          
005000*01  -COPY WEDIDTM0      -L.                                              
005100                                                                          
005200*01  -COPY WEDINAD0      -L.                                              
005300                                                                          
005400*01  -COPY WEDIQTY2      -L.                                              
005500                                                                          
005600*01  -COPY WEDILIN1      -L.                                              
005700                                                                          
005800                                                                          
005900 FD  W46343                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W46343 -PRE  UT-  -L.                                     
006400                                                                          
006500                                                                          
007300 WORKING-STORAGE SECTION.                                                 
007400                                                                          
007500*    -- CHECKED BY WY2000                                                 
007600 77  IDPGM                       PIC X(8)    VALUE 'W4634000'.            
007700 77  JA                          PIC X       VALUE 'J'.                   
007800 77  NEJ                         PIC X       VALUE 'N'.                   
007900 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
008000                                                                          
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400                                                                          
008500 77  W46342-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W46342                       VALUE 'J'.                   
008700                                                                          
008800 77  FORM-FEL-SW                 PIC X       VALUE 'N'.                   
008900     88  FORM-FEL                            VALUE 'J'.                   
008910     88  PARTNO-FEL                          VALUE 'P'.                   
009000                                                                          
009100 77  SKRIV-RAD-SW                PIC X       VALUE 'N'.                   
009200     88  SKRIV-RAD                           VALUE 'J'.                   
009300                                                                          
009310 01  WS-QUANTITY                 PIC X(15)   VALUE SPACE.                 
009320                                                                          
009400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009500 01  FILLER REDEFINES DAGENS-DATUM.                                       
009600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009900                                                                          
010000                                                                          
010100 01  WS-DAGENS-DATUM-8           PIC 9(8).                                
010200 01  WS-DAGENS-KLOCKA.                                                    
010300     03  WS-DAGENS-KLOCKA-1-6    PIC 9(6).                                
010400     03  WS-DAGENS-KLOCKA-7-9    PIC 9(3).                                
010500                                                                          
010600 01  DYNAMISKA-SUBPROGRAM.                                                
010700*                                                                         
010800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011100                                                                          
011200*    --- PARAMETRAR TILL ABEND                                            
011300                                                                          
011400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011700                                                                          
011800                                                                          
011900 01  FILLER                     PIC X(10)   VALUE 'WDATAREA'.             
012000*01 -COPY WDATAREA                                                        
012100                                                                          
012200                                                                          
012300*    --- PARAMETRAR TILL POSTSUM                                          
012400*                                                                         
012500*01  -COPY W0005   -PRE  POSTSUM-                                         
012600                                                                          
012700 01  IN-AREA-START               PIC X(24)   VALUE                        
012800                                 'IN-AREA-START  '.                       
012900                                                                          
013000 01  IN-AREA.                                                             
013100     03  IN-AREA-0.                                                       
013200       05  IN-IDPTYP             PIC X(3).                                
013300       05  FILLER                PIC X(1002).                             
013400*   03  FILLER -COPY WEDIDTM0  -PRE IN-  -RED  IN-AREA-0                  
013500*   03  FILLER -COPY WEDINAD0  -PRE IN-  -RED  IN-AREA-0                  
013600*   03  FILLER -COPY WEDILIN1  -PRE IN-  -RED  IN-AREA-0                  
013700*   03  FILLER -COPY WEDIQTY2  -PRE IN-  -RED  IN-AREA-0                  
013800                                                                          
013900                                                                          
014000 01  UT-AREA-START               PIC X(24)   VALUE                        
014100                                 'UT-AREA-START  '.                       
014110                                                                          
014120*01  AREA -COPY W46343   -PRE UT-                                         
014200                                                                          
014300                                                                          
015100 PROCEDURE DIVISION.                                                      
015200 MAIN SECTION.                                                            
015300                                                                          
015400     PERFORM A-INIT                                                       
015500     PERFORM S01-LAES-W46342                                              
015600     PERFORM UNTIL END-OF-W46342                                          
015700       EVALUATE IN-IDPTYP                                                 
015800         WHEN 'DTM'                                                       
015900           PERFORM B-DTM                                                  
016000         WHEN 'NAD'                                                       
016100           PERFORM C-NAD                                                  
016200         WHEN 'LIN'                                                       
016300           PERFORM D-LIN                                                  
016400         WHEN 'QTY'                                                       
016500           PERFORM E-QTY                                                  
016600       END-EVALUATE                                                       
016700                                                                          
016800       IF  FORM-FEL                                                       
016900           DISPLAY FELTEXT                                                
017000           PERFORM S99-ABEND                                              
017100       END-IF                                                             
017200                                                                          
017300       PERFORM S01-LAES-W46342                                            
017400     END-PERFORM                                                          
017500                                                                          
017600     PERFORM Z-FINIT                                                      
017700                                                                          
017800     MOVE ZERO TO RETURN-CODE                                             
017900     GOBACK                                                               
018000     .                                                                    
018100                                                                          
018200                                                                          
018300 A-INIT SECTION.                                                          
018400     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
018500                                                                          
018600     OPEN INPUT  W46342                                                   
018700                                                                          
018800     OPEN OUTPUT W46343                                                   
018900                                                                          
019100     ACCEPT WS-DAGENS-KLOCKA         FROM TIME                            
019200     MOVE FUNCTION CURRENT-DATE(1:8) TO   WS-DAGENS-DATUM-8               
019300     ACCEPT DAGENS-DATUM             FROM DATE                            
019311                                                                          
019400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019500     MOVE SPACE TO UT-AREA                                                
019600     MOVE NEJ   TO FORM-FEL-SW                                            
019700     MOVE NEJ   TO SKRIV-RAD-SW                                           
019800     .                                                                    
019900                                                                          
020000                                                                          
020100 B-DTM SECTION.                                                           
020200     MOVE 'B-DTM           ' TO CURRENT-SECTION                           
020300                                                                          
020301     MOVE ZERO TO UT-TIINLMOT                                             
020330                                                                          
020400     MOVE "AAMMDD" TO DAT-KDDATFORM                                       
020500     MOVE IN-DTM-DATE-TIME(3:6) TO DAT-I-TIDATUM                          
020600     CALL WDATKONV USING                                                  
020700          DAT-KDDATFORM,                                                  
020800          DAT-I-TIDATUM,                                                  
020900          DAT-O-TIDATUM,                                                  
021000          DAT-KDSVAR                                                      
021100                                                                          
021101     IF DAT-KDSVAR-OK                                                     
021300        IF DAT-TISEKEL = IN-DTM-DATE-TIME(1:2)                            
021301           IF IN-DTM-QUAL = '182'                                         
021400              MOVE IN-DTM-DATE-TIME(1:8)    TO UT-DASUPREF                
021401              IF IN-DTM-DATE-TIME(9:4) NUMERIC                            
021403                 MOVE IN-DTM-DATE-TIME(9:4) TO UT-TISUPTID                
021405              ELSE                                                        
021406                 MOVE 'TISUPTID FELAKTIG'   TO FELTEXT-STR                
021407                 MOVE JA                    TO FORM-FEL-SW                
021408              END-IF                                                      
021410           ELSE                                                           
021411              IF IN-DTM-QUAL = '359'                                      
021412                 MOVE IN-DTM-DATE-TIME(1:8) TO UT-TIINLMOT                
021413              END-IF                                                      
021420           END-IF                                                         
022100        ELSE                                                              
022200           MOVE 'DASUPREF  FELAKTIG'        TO FELTEXT-STR                
022201*fix isf abend                                                            
022210           MOVE IN-DTM-DATE-TIME(1:8)       TO FELTEXT-STR(20:8)          
022211                                               UT-DASUPREF                
022220           MOVE IN-DTM-DATE-TIME(9:4)       TO UT-TISUPTID                
022230           DISPLAY FELTEXT                                                
022300*          MOVE JA                          TO FORM-FEL-SW                
022400        END-IF                                                            
022500     ELSE                                                                 
022510        IF IN-DTM-QUAL = '359' AND IN-DTM-DATE-TIME(3:6) = SPACE          
022520*       DELIVERY DATE IS SOME TIMES SPACE FOR '359'                       
022530           CONTINUE                                                       
022540        ELSE                                                              
022600           MOVE 'DASUPREF  FELAKTIG'           TO FELTEXT-STR             
022610*fix isf abend                                                            
022620           MOVE IN-DTM-DATE-TIME(1:8)       TO FELTEXT-STR(20:8)          
022621                                               UT-DASUPREF                
022622           MOVE IN-DTM-DATE-TIME(9:4)       TO UT-TISUPTID                
022623           DISPLAY FELTEXT                                                
022700*          MOVE JA                             TO FORM-FEL-SW             
022800        END-IF                                                            
022810     END-IF                                                               
022900     .                                                                    
023000                                                                          
023100                                                                          
023200 C-NAD SECTION.                                                           
023300     MOVE 'C-NAD           ' TO CURRENT-SECTION                           
023400                                                                          
023500      UNSTRING IN-NAD-PARTY-ID DELIMITED BY SPACE                         
023600                               INTO UT-IDLEVNR                            
023900     .                                                                    
024000                                                                          
024100                                                                          
025500 D-LIN SECTION.                                                           
025600     MOVE 'D-LIN           ' TO CURRENT-SECTION                           
025700                                                                          
025800     IF  SKRIV-RAD                                                        
025900         PERFORM S11-SKRIV-W46343                                         
026200         PERFORM S23-NOLLA-LIN                                            
026300     END-IF                                                               
026400                                                                          
026500     MOVE JA                         TO SKRIV-RAD-SW                      
026600                                                                          
026700     UNSTRING IN-LIN1-ITEMNO DELIMITED BY SPACE                           
026800                              INTO UT-IDARTNR                             
026900     IF  UT-IDARTNR NOT NUMERIC                                           
027000         MOVE 'IDARTNR EJ NUMERISKT' TO FELTEXT-STR                       
027010*fix isf abend                                                            
027020           MOVE IN-LIN1-ITEMNO              TO FELTEXT-STR(22:30)         
027050           DISPLAY FELTEXT                                                
027100         MOVE 'P'                    TO FORM-FEL-SW                       
027110         MOVE NEJ                    TO SKRIV-RAD-SW                      
027200     END-IF                                                               
027400     .                                                                    
027500                                                                          
027600                                                                          
027700 E-QTY SECTION.                                                           
027710     MOVE 'E-QTY           ' TO CURRENT-SECTION                           
027720                                                                          
027721      MOVE IN-QTY2-QUANTITY     TO WS-QUANTITY                            
027730      UNSTRING WS-QUANTITY DELIMITED BY SPACE                             
027740                              INTO UT-KVLS-DLEV                           
027750      IF UT-KVLS-DLEV NOT NUMERIC                                         
027760          MOVE 'KVLS-DLEV EJ NUMERISKT' TO FELTEXT-STR                    
027770          MOVE JA                       TO FORM-FEL-SW                    
027780      END-IF                                                              
027791     .                                                                    
027792                                                                          
027793                                                                          
027800 Z-FINIT SECTION.                                                         
027900     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
028000                                                                          
028100     IF  SKRIV-RAD                                                        
028200         PERFORM S11-SKRIV-W46343                                         
028500     END-IF                                                               
028600                                                                          
028700     CLOSE W46342                                                         
028800           W46343                                                         
029000                                                                          
029100     MOVE 'S' TO POSTSUM-OPKOD                                            
029200     CALL POSTSUM USING POSTSUM-PARM                                      
029300     .                                                                    
029400                                                                          
029500                                                                          
029600 S01-LAES-W46342  SECTION.                                                
029700     READ W46342 INTO IN-AREA                                             
029800     AT END                                                               
029900        MOVE HIGH-VALUE   TO IN-AREA                                      
030000        SET END-OF-W46342 TO TRUE                                         
030100                                                                          
030200     NOT AT END                                                           
030300        MOVE 'W46342'   TO POSTSUM-FDNAMN                                 
030400        MOVE 'W46340D1' TO POSTSUM-DDNAMN2                                
030500        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
030600        CALL POSTSUM USING POSTSUM-PARM                                   
030700     END-READ                                                             
030800     .                                                                    
030900                                                                          
031000                                                                          
031100 S11-SKRIV-W46343 SECTION.                                                
031200                                                                          
031300     WRITE UT-POST FROM UT-AREA                                           
031400                                                                          
031500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
031600     MOVE 'W46343'   TO POSTSUM-FDNAMN                                    
031700     MOVE 'W46340D2' TO POSTSUM-DDNAMN2                                   
031800     CALL POSTSUM USING POSTSUM-PARM                                      
031900     .                                                                    
032000                                                                          
032100                                                                          
033300 S23-NOLLA-LIN SECTION.                                                   
033400                                                                          
033500     MOVE ZERO              TO UT-IDARTNR                                 
033600                               UT-KVLS-DLEV                               
033700     .                                                                    
033800                                                                          
033900                                                                          
034700 S99-ABEND SECTION.                                                       
034800                                                                          
035000     MOVE 'S' TO POSTSUM-OPKOD                                            
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
035300     .                                                                    
