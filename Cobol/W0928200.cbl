000100 ID DIVISION.                                                             
000200     SKIP3                                                                
000300 PROGRAM-ID.     W0928200.                                                
000400*AUTHOR.         KJELL ANDRE.                                             
000500*DATE-WRITTEN.   JAN. 1979                                                
000600*    SKIP2                                                                
000700*REMARKS.                                                                 
000800*    FUNKTION.   PROGRAMMET ÄR EN SUBMODUL TILL TRATTEN, OCH              
000900*                BEHANDLAR R05:OR (UPPDATERING AV FÄLT PÅ ARTIKEL-        
001000*                REGISTRET)                                               
001100*                PROGRAMMETS HUVUDUPPGIFT ÄR ATT FÖRDELA OLIKA            
001200*                TYPER AV R05:OR TILL OLIKA FILER SOM GÅR TILL            
001300*                OLIKA SYSTEM. DETTA STYRS VIA CONSTANT-MEDLEM            
001400*                W092M002, SOM OCKSÅ ANVÄNDS I UT-TRATTEN.                
001500*                                                                         
001600*                                                                         
001700*    ABEND U0016: OM R05-TABELL BLIR FULL                                 
001800*          U0017: OM FELAKTIG "IDPTYP-GEN" I W092M002                     
001900*                                                                         
002000*    FELKODER:   021  GLURPKODEN SAKNAS PÅ REGISTRET W092M002             
002100*                020  FELAKTIG IDPTYP-GEN (044 ENDAST TILLÅTEN            
002200*                     FÖR IDLKTO)                                         
002300*                011  FELAKTIGT DATA                                      
002400*                     (KONTOLLERAS ENDAST FÖR IDLKTO)                     
002500*                0B3  KDTECKEN FEL (EJ + - ELLER SPACE)                   
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP3                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000     SKIP3                                                                
003100 FILE-CONTROL.                                                            
003200                                                                          
003300*                            ***  STYRTABELL                              
003400     SELECT W092M002 ASSIGN TO R05TAB.                                    
003500                                                                          
003600*                            ***  POSTTYP 042                             
003700*                            ***  RAKA OCH SPECIAL, W011                  
003800     SELECT W09279   ASSIGN TO W09206D7.                                  
003900                                                                          
004000*                            ***  POSTTYP 043                             
004100*                            ***  KOPPLADE ÄNDRINGAR, W217                
004200     SELECT W09280   ASSIGN TO W09206D8.                                  
004300                                                                          
004400*                            ***  POSTTYP 044                             
004500*                            ***  KONTOÄNDRINGAR TILL W540                
004600     SELECT W09276   ASSIGN TO W09206D5.                                  
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP3                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005200 FD  W092M002                                                             
005300     BLOCK CONTAINS 0 RECORDS                                             
005400     RECORDING MODE IS F.                                                 
005500     SKIP3                                                                
005600*01  R05-POST       -COPY  W092M002  -L.                                  
005700     EJECT                                                                
005800 FD  W09279                                                               
005900     BLOCK CONTAINS 0 RECORDS                                             
006000     RECORDING MODE IS V.                                                 
006100     SKIP3                                                                
006200*01  UTPOST-042     -COPY  W011042   -L.                                  
006300     EJECT                                                                
006400 FD  W09280                                                               
006500     BLOCK CONTAINS 0 RECORDS                                             
006600     RECORDING MODE IS V.                                                 
006700     SKIP3                                                                
006800*01  UTPOST-043     -COPY  W217043   -L.                                  
006900     EJECT                                                                
007000 FD  W09276                                                               
007100     BLOCK CONTAINS 0 RECORDS                                             
007200     RECORDING MODE IS V.                                                 
007300     SKIP3                                                                
007400*01  UTPOST-044     -COPY  W540044   -L.                                  
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007601                                                                          
007610*    -- CHECKED BY WY2000                                                 
007700*                                                                         
007800 77  JA                        PIC X      VALUE 'J'.                      
007900 77  NEJ                       PIC X      VALUE 'N'.                      
008000 77  W-IDFVARDE-NYTT           PIC X(25)  VALUE ZERO.                     
008100 77  W-IDFVARDE-BEF            PIC X(25)  VALUE ZERO.                     
008200 77  INDX                      PIC S9(9)  COMP SYNC.                      
008300                                                                          
008400                                                                          
008500*    --- SWITCHAR                                                         
008600 01  FEL-SW                    PIC X      VALUE 'N'.                      
008700     88  FEL                              VALUE 'J'.                      
008800     88  OK                               VALUE 'N'.                      
008900                                                                          
009000 01  CALL-SW                   PIC X      VALUE 'J'.                      
009100     88  FIRST-CALL                       VALUE 'J'.                      
009200     88  NOT-FIRST-CALL                   VALUE 'N'.                      
009300                                                                          
009400 01  EOF-W092M002-SW           PIC X      VALUE 'N'.                      
009500     88  EOF-W092M002                     VALUE 'J'.                      
009600                                                                          
009700                                                                          
009800 01  SUBPROGRAM.                                                          
009900     03  ABEND                 PIC X(8)   VALUE 'ABEND   '.               
010000     03  WDECEDIT              PIC X(8)   VALUE 'WDECEDIT'.               
010100                                                                          
010200     SKIP3                                                                
010300 01  PARM-TILL-ABEND.                                                     
010400     03  RKOD-ABEND          PIC S9(4)   COMP SYNC.                       
010500     SKIP3                                                                
010600 01  -COPY WDECAREA                                                       
010700     EJECT                                                                
010800*                              *********************************          
010900*                              *     0 4 2 - P O S T           *          
011000*                              *********************************          
011100                                                                          
011200 01  042-POST-START            PIC X(24)   VALUE                          
011300                                 '042-POST-START'.                        
011400                                                                          
011500*01  POST    -PRE     042-   -COPY   W011042.                             
011600     EJECT                                                                
011700*                              *********************************          
011800*                              *     0 4 3 - P O S T           *          
011900*                              *********************************          
012000                                                                          
012100 01  043-POST-START            PIC X(24)   VALUE                          
012200                                 '043-POST-START'.                        
012300                                                                          
012400*01  POST    -PRE     043-   -COPY   W217043.                             
012500     EJECT                                                                
012600*                              *********************************          
012700*                              *     0 4 4 - P O S T           *          
012800*                              *********************************          
012900                                                                          
013000 01  044-POST-START            PIC X(24)   VALUE                          
013100                                 '044-POST-START'.                        
013200                                                                          
013300*01  POST    -PRE     044-   -COPY   W540044.                             
013400     EJECT                                                                
013500 01  R05-TAB-START             PIC X(24)   VALUE                          
013600                                 'R05-TAB-START'.                         
013700                                                                          
013800 01  R05-IXMAX                 PIC S9(4) COMP  VALUE +200.                
013900 01  R05-TABELL.                                                          
014000     03  R05-TABELL-RAD  OCCURS 200                                       
014100                         ASCENDING KEY IS TAB-R05-IDELMT                  
014200                         INDEXED BY R05-IX.                               
014300       05  -COPY W092M002 -PRE TAB-                                       
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600     SKIP3                                                                
014700 01  TRANS-PARM.                                                          
014800     03  EOF-KOD             PIC X(3).                                    
014900                                                                          
015000     03  FELTAB  OCCURS 100.                                              
015100         05  FELKOD          PIC X(3).                                    
015200         05  FELTEXT         PIC X(15).                                   
015300                                                                          
015400     03  FILLER              PIC X(3).                                    
015500                                                                          
015600     03  INPOST              PIC X(100).                                  
015700                                                                          
015800     03  UTAREA.                                                          
015900*        05  ID-DEL  -COPY W092W001  -PRE ID-.                            
016000                                                                          
016100*        05  -COPY W092R05T  -PRE R05-.                                   
016200     EJECT                                                                
016300 PROCEDURE DIVISION USING TRANS-PARM.                                     
016400     SKIP2                                                                
016500     IF FIRST-CALL                                                        
016600       SET NOT-FIRST-CALL TO TRUE                                         
016700       PERFORM A-INIT                                                     
016800     END-IF                                                               
016900                                                                          
017000     IF EOF-KOD = 'EOF'                                                   
017100       PERFORM Z-FINIT                                                    
017200     ELSE                                                                 
017300       MOVE 0 TO INDX                                                     
017400       MOVE NEJ TO FEL-SW                                                 
017500                                                                          
017600       PERFORM B-SOEK-I-GLURPTAB                                          
017700       IF OK                                                              
017800         PERFORM D-KONTROLLERA-TECKEN                                     
017900       END-IF                                                             
018000                                                                          
018100       IF OK                                                              
018200         PERFORM C-SKRIV-POST                                             
018300       END-IF                                                             
018400       IF FEL                                                             
018500         ADD 1 TO INDX                                                    
018600         MOVE HIGH-VALUE TO FELKOD (INDX)                                 
018700       END-IF                                                             
018800     END-IF                                                               
018900     MOVE 0 TO RETURN-CODE                                                
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT      SECTION.                                                     
019400     SKIP3                                                                
019500     OPEN                                                                 
019600          INPUT  W092M002                                                 
019700          OUTPUT W09279 W09280 W09276                                     
019800                                                                          
019900                                                                          
020000* - - - - - - - - - - - - - - FYLL TABELLEN SÅ ATT DET GÅR                
020100*                             ATT SÖKA I DEN                              
020200     MOVE HIGH-VALUE TO R05-TABELL                                        
020300                                                                          
020400     SET R05-IX TO 1                                                      
020500     READ W092M002                                                        
020600       AT END    MOVE 'J' TO EOF-W092M002-SW                              
020700     END-READ                                                             
020800                                                                          
020900     PERFORM UNTIL EOF-W092M002                                           
021000                                                                          
021100       IF R05-IX >= R05-IXMAX                                             
021200         DISPLAY 'W09282 R05-TABELLEN ÄR FULL. ÖKA DEN'                   
021300         DISPLAY 'SÅ ATT ALLA POSTER I W092M002 FÅR PLATS'                
021400                                                                          
021500         MOVE 16 TO RKOD-ABEND                                            
021600         CALL ABEND USING RKOD-ABEND                                      
021700       ELSE                                                               
021800         MOVE R05-POST TO R05-TABELL-RAD (R05-IX)                         
021900       END-IF                                                             
022000                                                                          
022100       READ W092M002                                                      
022200         AT END    MOVE 'J' TO EOF-W092M002-SW                            
022300       END-READ                                                           
022400       SET R05-IX UP BY 1                                                 
022500                                                                          
022600     END-PERFORM                                                          
022700                                                                          
022800     CLOSE W092M002                                                       
022900     .                                                                    
023000     EJECT                                                                
023100 B-SOEK-I-GLURPTAB    SECTION.                                            
023200     SKIP2                                                                
023300     SEARCH ALL R05-TABELL-RAD                                            
023400       AT END                                                             
023500             ADD 1 TO INDX                                                
023600             MOVE JA TO FEL-SW                                            
023700             MOVE '021' TO FELKOD (INDX)                                  
023800             MOVE SPACE TO FELTEXT (INDX)                                 
023900       WHEN TAB-R05-IDELMT (R05-IX) = R05-IDELMT                          
024000     END-SEARCH                                                           
024100     .                                                                    
024200     EJECT                                                                
024300 C-SKRIV-POST   SECTION.                                                  
024400     SKIP2                                                                
024500*--------------------------- SKRIV UT TRANSEN                             
024600*                            PÅ DEN FIL SOM REGISTRET ANGER               
024700     EVALUATE TAB-R05-IDPTYP-GEN (R05-IX)                                 
024800     WHEN '042'                                                           
024900       PERFORM S100-SKRIV-POSTTYP-042                                     
025000     WHEN '043'                                                           
025100       PERFORM S300-SKRIV-POSTTYP-043                                     
025200     WHEN '044'                                                           
025300       IF R05-IDELMT = 'IDLKTO'                                           
025400         MOVE R05-IDFVARDE-NYTT TO DEC-IDFRIDATA                          
025500         MOVE 7                 TO DEC-KVHELTAL                           
025600         MOVE 0                 TO DEC-KVDECIMAL                          
025700         CALL WDECEDIT USING DEC-WDECAREA                                 
025800         IF DEC-KDSVAR-OK                                                 
025900           PERFORM S400-SKRIV-POSTTYP-044                                 
026000         ELSE                                                             
026100*          -- FELAKTIGT FORMAT PÅ KONTOT                                  
026200           ADD 1 TO INDX                                                  
026300           MOVE JA TO FEL-SW                                              
026400           MOVE '011' TO FELKOD (INDX)                                    
026500           MOVE SPACE TO FELTEXT (INDX)                                   
026600         END-IF                                                           
026700       ELSE                                                               
026800*        -- 044 MÅSTE VARA KONTO-ÄNDRING                                  
026900         ADD 1 TO INDX                                                    
027000         MOVE JA TO FEL-SW                                                
027100         MOVE '020' TO FELKOD (INDX)                                      
027200         MOVE SPACE TO FELTEXT (INDX)                                     
027300       END-IF                                                             
027400     WHEN OTHER                                                           
027500       DISPLAY 'W092M002 INNEHÅLLER FELAKTIG'                             
027600       ' IDPTYP-GEN: ' TAB-R05-IDPTYP-GEN (R05-IX)                        
027700       DISPLAY 'ENDAST 042, 043 ELLER 044 TILLÅTNA'                       
027800       MOVE 17 TO RKOD-ABEND                                              
027900       CALL ABEND USING RKOD-ABEND                                        
028000     END-EVALUATE                                                         
028100     .                                                                    
028200     EJECT                                                                
028300 D-KONTROLLERA-TECKEN  SECTION.                                           
028400     SKIP2                                                                
028500     IF R05-KDTECKEN-NYTT NOT = SPACE AND '+' AND '-'                     
028600       ADD 1 TO INDX                                                      
028700       MOVE JA TO FEL-SW                                                  
028800       MOVE '0B3' TO FELKOD (INDX)                                        
028900       MOVE SPACE TO FELTEXT (INDX)                                       
028910     END-IF                                                               
028920                                                                          
028930     IF R05-KDTECKEN-BEF  NOT = SPACE AND '+' AND '-'                     
028940       ADD 1 TO INDX                                                      
028950       MOVE JA TO FEL-SW                                                  
028960       MOVE '0B3' TO FELKOD (INDX)                                        
028970       MOVE SPACE TO FELTEXT (INDX)                                       
028980     END-IF                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 Z-FINIT        SECTION.                                                  
029300     SKIP3                                                                
029400     CLOSE                                                                
029500          W09279  W09280  W09276                                          
029600     .                                                                    
029700     EJECT                                                                
029800 S100-SKRIV-POSTTYP-042  SECTION.                                         
029900     SKIP2                                                                
030000*--------------------------- SKRIV R05:OR TILL W011                       
030100*                                                                         
030200     MOVE  '042'              TO  042-IDPTYP                              
030300     MOVE  R05-IDARTNR        TO  042-IDARTNR                             
030400     MOVE  R05-IDELMT         TO  042-IDELMT                              
030500     MOVE  R05-KDTECKEN-NYTT  TO  042-KDTECKEN-NYTT                       
030600     MOVE  R05-IDFVARDE-NYTT  TO  042-IDFVARDE-NYTT                       
030700     MOVE  R05-KDTECKEN-BEF   TO  042-KDTECKEN-BEF                        
030800     MOVE  R05-IDFVARDE-BEF   TO  042-IDFVARDE-BEF                        
030900                                                                          
031000     WRITE UTPOST-042 FROM 042-POST                                       
031100     .                                                                    
031200     SKIP3                                                                
031300 S300-SKRIV-POSTTYP-043       SECTION.                                    
031400     SKIP2                                                                
031500*---------------------------- SKRIV KOPPLADE R05:OR TILL W217             
031600*                                                                         
031700     MOVE  '043'              TO  043-IDPTYP                              
031800     MOVE  R05-IDARTNR        TO  043-IDARTNR                             
031900     MOVE  R05-IDELMT         TO  043-IDELMT                              
032000     MOVE  R05-KDTECKEN-NYTT  TO  043-KDTECKEN-NYTT                       
032100     MOVE  R05-IDFVARDE-NYTT  TO  043-IDFVARDE-NYTT                       
032200                                                                          
032300     WRITE UTPOST-043 FROM 043-POST                                       
032400     .                                                                    
032500     SKIP3                                                                
032600 S400-SKRIV-POSTTYP-044       SECTION.                                    
032700     SKIP2                                                                
032800*---------------------------- SKRIV R05:OR TILL W540 (KONTOÄNDRING        
032900*                                                                         
033000     MOVE  '044'           TO  044-IDPTYP                                 
033100     MOVE  R05-IDARTNR     TO  044-IDARTNR                                
033200     MOVE  DEC-IDEDITDATA  TO  044-IDLKTO                                 
033300                                                                          
033400     WRITE UTPOST-044 FROM 044-POST                                       
033500     .                                                                    
