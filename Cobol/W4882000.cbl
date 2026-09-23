000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W4882000.                                        
000400 AUTHOR.                 H JACOBSSON K-GRUPPEN VOLVO-DATA                 
000500 DATE-WRITTEN.           APRIL 84.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*      FUNKTION:                                                          
001000*                                                                         
001100*            LÄS DELSALDO-POSTER FRÅN HÖGLAGRET.                          
001200*            ALLA POSTER RÄKNAS,                                          
001300*                OM INTE SÄNDA OCH LÄSTA POSTER ÄR LIKA                   
001400*                SÅ SKICKAS ENBART EN 20-POST MED BARA                    
001500*                NOLLVÄRDEN. INGA 21-POSTER SÄNDS.                        
001600*                                                                         
001700*            SISTA  POSTEN IN ÄR EN SUMMAPOST.                            
001800*            FÖRSTA POSTEN UT ÄR EN SUMMAPOST.                            
001900*            POSTERNA SORTERAS.                                           
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*                            INFILER:                                     
002800     SELECT  W48819                   ASSIGN  W48820D1.                   
002900     SKIP2                                                                
003000*                            UTFILER:                                     
003100     SELECT  W48821                   ASSIGN  W48820D2.                   
003200     SKIP2                                                                
003300*                            SORTFIL:                                     
003400     SELECT  SORTFIL                  ASSIGN  W48820DS.                   
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W48819                                                               
004100     LABEL RECORD STANDARD                                                
004200     RECORDING      V                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004500*01  -COPY W488009      -PRE W48819- -L.                                  
004600*++INCLUDE W488009CC0                                                     
004700     SKIP2                                                                
004800*01  -COPY W488010      -PRE W48819- -L.                                  
004900*++INCLUDE W488010CC0                                                     
005000     SKIP2                                                                
005100*01  -COPY W488011      -PRE W48819- -L.                                  
005200*++INCLUDE W488011CC0                                                     
005300     EJECT                                                                
005400 FD  W48821                                                               
005500     LABEL RECORD STANDARD                                                
005600     RECORDING   F                                                        
005700     BLOCK CONTAINS 0.                                                    
005800     SKIP2                                                                
005900*01  POST  -COPY W488021      -PRE W48821- -L.                            
006000*++INCLUDE W488021CC0                                                     
006100     EJECT                                                                
006200 SD  SORTFIL                                                              
006300     RECORDING   F.                                                       
006400     SKIP2                                                                
006500*01  POST  -COPY W488021      -PRE SORT-.                                 
006600*++INCLUDE W488021CC0                                                     
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006900     SKIP2                                                                
006901                                                                          
006910*    -- CHECKED BY WY2000                                                 
007000*                             GENERERAT PROGRAM-NAMN                      
007100 77   PROGRAM-NAMN           VALUE 'W4882000'                             
007200                                 PIC X(8).                                
007300*                             GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)        VALUE 'J'.               
007600 77  NEJ                         PIC X(1)        VALUE 'N'.               
007700 77  RAETT                       PIC X(1)        VALUE 'R'.               
007800 77  FEL                         PIC X(1)        VALUE 'F'.               
007900 77  PLUSTECKEN                  PIC X(1)        VALUE '+'.               
008000 77  BLANKTECKEN                 PIC X(1)        VALUE ' '.               
008100 77  MINUSTECKEN                 PIC X(1)        VALUE '-'.               
008200                                                                          
008300*                             END-OF-FILE SWITCHAR                        
008400                                                                          
008500 77  W48819-EOF                  PIC X(1)        VALUE 'N'.               
008600     EJECT                                                                
008700*                             ARBETSFÄLT                                  
008800                                                                          
008900 77  W-IDPTYP-009                PIC  X(3)   VALUE '009'.                 
009000 77  W-IDPTYP-010                PIC  X(3)   VALUE '010'.                 
009100 77  W-IDPTYP-011                PIC  X(3)   VALUE '011'.                 
009200 77  W-IDPTYP-020                PIC  X(3)   VALUE '020'.                 
009300 77  W-IDPTYP-021                PIC  X(3)   VALUE '021'.                 
009400 77  W-SALDO-TEXT                PIC  X(8)   VALUE 'DELSALDO'.            
009500 77  W-NUM-KVBUFF-F              PIC  9(7)   VALUE ZERO.                  
009600 77  W-NUM-KVBUFF-OF             PIC  9(7)   VALUE ZERO.                  
009700 77  W-NUM-KVKOLLI-F             PIC  9(4)   VALUE ZERO.                  
009800 77  W-NUM-KVKOLLI-OF            PIC  9(4)   VALUE ZERO.                  
009900*                                                                         
010000 77  W-SPAR-POSTER-PDP           PIC S9(5)   VALUE ZERO  COMP-3.          
010100 77  W-SPAR-LASTA-POSTER         PIC S9(5)   VALUE ZERO  COMP-3.          
010200 77  W-SPAR-ANTAL-SUMMAPOSTER    PIC S9(5)   VALUE ZERO  COMP-3.          
010300     SKIP3                                                                
010400 01  W-IDARTNR-HELA.                                                      
010500     03  FILLER                  PIC X(1).                                
010600     03  W-IDARTNR-8             PIC X(8).                                
010700     SKIP3                                                                
010800 01  INPOST-TEST                 PIC X(1).                                
010900   88  INPOST-OK                             VALUE 'R'.                   
011000   88  INPOST-FEL                            VALUE 'F'.                   
011100*                                                                         
011200 01  DYNAMISKA-SUBPROGRAM.                                                
011300   03  POSTSUM                   PIC X(8)     VALUE 'POSTSUM '.           
011400   03  FELLOG                    PIC X(8)     VALUE 'FELLOG  '.           
011500     EJECT                                                                
011600*                             PARAMETRAR TILL POSTSUM                     
011700                                                                          
011800*01  -COPY W0005CCCC0  -PRE POSTSUM-.                                     
011900*++INCLUDE W0005CCCC0                                                     
012000     EJECT                                                                
012100 01  FILLER                      PIC X(24)   VALUE                        
012200                                            'INFIL-AREA-START'.           
012300     SKIP2                                                                
012400*01  INFIL-AREA -COPY W488009CC0 -L.                                      
012500*++INCLUDE W488009CC0                                                     
012600     SKIP2                                                                
012700*01  W488I09 -COPY W488009CC0 -PRE INFIL- -RED INFIL-AREA.                
012800*++INCLUDE W488009CC0                                                     
012900     SKIP2                                                                
013000*01  W488190 -COPY W488010CC0 -PRE INFIL- -RED INFIL-AREA.                
013100*++INCLUDE W488010CC0                                                     
013200     SKIP2                                                                
013300*01  W488191 -COPY W488011CC0 -PRE INFIL- -RED INFIL-AREA.                
013400*++INCLUDE W488011CC0                                                     
013500     EJECT                                                                
013600 01  FILLER                      PIC X(24)    VALUE                       
013700                                              'UTFIL-AREA-START'.         
013800     SKIP2                                                                
013900*01  UTFIL-AREA -COPY W488020CC0 -L.                                      
014000*++INCLUDE W488020CC0                                                     
014100     SKIP3                                                                
014200*01  W488U20 -COPY W488020CC0  -PRE UTFIL- -RED UTFIL-AREA.               
014300*++INCLUDE W488020CC0                                                     
014400     EJECT                                                                
014500*01  W488U21 -COPY W488021CC0  -PRE UTFIL- -RED UTFIL-AREA.               
014600*++INCLUDE W488021CC0                                                     
014700     EJECT                                                                
014800 PROCEDURE DIVISION.                                                      
014900     SKIP3                                                                
015000                                                                          
015100 STYR SECTION.                                                            
015200                                                                          
015300     PERFORM A-INIT                                                       
015400                                                                          
015500     IF INPOST-OK                                                         
015600         SORT SORTFIL                                                     
015700                 ASCENDING KEY SORT-ARTSUM-IDPTYP-021                     
015800                               SORT-ARTSUM-IDLOPNRF                       
015900                                                                          
016000                INPUT PROCEDURE B-FOERE-SORT                              
016100                                                                          
016200                GIVING W48821                                             
016300                                                                          
016400         IF SORT-RETURN > ZERO                                            
016500             DISPLAY '*** W4882000 - FEL VID SORTERING ***'               
016600             CALL FELLOG                                                  
016700         END-IF                                                           
016800     END-IF                                                               
016900                                                                          
017000     PERFORM Z-FINIT                                                      
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     OPEN INPUT  W48819                                                   
017900                                                                          
018000     PERFORM S11-LAS-W48819                                               
018100                                                                          
018200     IF INFIL-SALDO-TEXT = W-SALDO-TEXT                                   
018300         MOVE RAETT TO INPOST-TEST                                        
018400     ELSE                                                                 
018500         MOVE FEL TO INPOST-TEST                                          
018600     END-IF                                                               
018700                                                                          
018800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
018900     MOVE ZERO TO W-SPAR-LASTA-POSTER                                     
019000                                                                          
019100     MOVE SPACE TO UTFIL-AREA                                             
019200     .                                                                    
019300     EJECT                                                                
019400 B-FOERE-SORT SECTION.                                                    
019500                                                                          
019600     PERFORM S11-LAS-W48819                                               
019700                                                                          
019800     PERFORM UNTIL W48819-EOF = JA  OR                                    
019900                   INFIL-SALDO-IDPTYP-010 = W-IDPTYP-011                  
020000                                                                          
020100         ADD +1 TO W-SPAR-LASTA-POSTER                                    
020200         MOVE W-SPAR-LASTA-POSTER TO UTFIL-ARTSUM-IDLOPNRF                
020300                                                                          
020400         PERFORM BA-TESTA-INDATA                                          
020500                                                                          
020600         PERFORM S21-SKRIV-W48821-PT21                                    
020700         PERFORM S11-LAS-W48819                                           
020800     END-PERFORM                                                          
020900                                                                          
021000     IF W48819-EOF = NEJ                                                  
021100         INSPECT INFIL-SALDO-KVSALDOPOST                                  
021200                            REPLACING LEADING SPACE BY ZERO               
021300         IF INFIL-SALDO-KVSALDOPOST > ZERO                                
021400             MOVE INFIL-SALDO-KVSALDOPOST TO                              
021500                                UTFIL-SALDO-KVSALDOPOST-PDP               
021600         ELSE                                                             
021700             MOVE ZERO TO UTFIL-SALDO-KVSALDOPOST-PDP                     
021800         END-IF                                                           
021900     ELSE                                                                 
022000         MOVE ZERO TO UTFIL-SALDO-KVSALDOPOST-PDP                         
022100     END-IF                                                               
022200     MOVE W-SPAR-LASTA-POSTER TO UTFIL-SALDO-KVSALDOPOST-IBM              
022300     MOVE W-IDPTYP-020 TO UTFIL-SALDO-IDPTYP-020                          
022400     PERFORM S20-SKRIV-W48821-PT20                                        
022500     .                                                                    
022600     EJECT                                                                
022700 BA-TESTA-INDATA SECTION.                                                 
022800                                                                          
022900*    TESTNING AV INDATA                                          *        
023000                                                                          
023100     IF INFIL-SALDO-IDPTYP-010 NUMERIC                                    
023200         IF INFIL-SALDO-IDPTYP-010 = W-IDPTYP-010                         
023300             INSPECT INFIL-SALDO-IDARTNR                                  
023400                            REPLACING LEADING SPACE BY ZERO               
023500             INSPECT INFIL-SALDO-KVBUFF-F                                 
023600                            REPLACING LEADING SPACE BY ZERO               
023700             INSPECT INFIL-SALDO-KVBUFF-OF                                
023800                            REPLACING LEADING SPACE BY ZERO               
023900             INSPECT INFIL-SALDO-KVKOLLI-F                                
024000                            REPLACING LEADING SPACE BY ZERO               
024100             INSPECT INFIL-SALDO-KVKOLLI-OF                               
024200                            REPLACING LEADING SPACE BY ZERO               
024300             IF INFIL-SALDO-IDARTNR NUMERIC                               
024400                 IF INFIL-SALDO-IDARTNR > ZERO                            
024500                     MOVE INFIL-SALDO-IDARTNR TO W-IDARTNR-HELA           
024600                     MOVE W-IDARTNR-8 TO UTFIL-ARTSUM-IDARTNR             
024700                     IF INFIL-SALDO-KVBUFF-F NUMERIC                      
024800                       PERFORM BAA-CASE-TECKEN-KVBUFF-F                   
024900                       IF INFIL-SALDO-KVBUFF-OF NUMERIC                   
025000                           PERFORM BAB-CASE-TECKEN-KVBUFF-OF              
025100                       ELSE                                               
025200                           MOVE '206' TO UTFIL-ARTSUM-IDFELKOD            
025300                       END-IF                                             
025400                     ELSE                                                 
025500                       MOVE '205' TO UTFIL-ARTSUM-IDFELKOD                
025600                     END-IF                                               
025700                                                                          
025800                     IF INFIL-SALDO-KVKOLLI-F NUMERIC                     
025900                       PERFORM BAC-CASE-TECKEN-KVKOLLI-F                  
026000                       IF INFIL-SALDO-KVKOLLI-OF NUMERIC                  
026100                           PERFORM BAD-CASE-TECKEN-KVKOLLI-OF             
026200                       ELSE                                               
026300                           MOVE '208' TO UTFIL-ARTSUM-IDFELKOD            
026400                       END-IF                                             
026500                     ELSE                                                 
026600                       MOVE '207' TO UTFIL-ARTSUM-IDFELKOD                
026700                     END-IF                                               
026800                 ELSE                                                     
026900                     MOVE '203' TO UTFIL-ARTSUM-IDFELKOD                  
027000                 END-IF                                                   
027100             ELSE                                                         
027200                 MOVE '204' TO UTFIL-ARTSUM-IDFELKOD                      
027300             END-IF                                                       
027400             MOVE W-IDPTYP-021 TO UTFIL-ARTSUM-IDPTYP-021                 
027500         ELSE                                                             
027600             MOVE '202' TO UTFIL-ARTSUM-IDFELKOD                          
027700         END-IF                                                           
027800     ELSE                                                                 
027900         MOVE '201' TO UTFIL-ARTSUM-IDFELKOD                              
028000     END-IF                                                               
028100     EJECT                                                                
028200     .                                                                    
028300 BAA-CASE-TECKEN-KVBUFF-F SECTION.                                        
028400*                                                                         
028500*    TESTNING OM SALDOT IN ÄR PLUS, BLANKT=PLUS ELLER MINUS      *        
028600                                                                          
028700     MOVE INFIL-SALDO-KVBUFF-F TO W-NUM-KVBUFF-F                          
028800     MOVE ZERO TO UTFIL-ARTSUM-KVBUFF-F                                   
028900                                                                          
029000     EVALUATE INFIL-SALDO-KDTECKEN-BUFF-F                                 
029100     WHEN PLUSTECKEN                                                      
029200             ADD W-NUM-KVBUFF-F TO UTFIL-ARTSUM-KVBUFF-F                  
029300     WHEN BLANKTECKEN                                                     
029400             ADD W-NUM-KVBUFF-F TO UTFIL-ARTSUM-KVBUFF-F                  
029500     WHEN MINUSTECKEN                                                     
029600             SUBTRACT W-NUM-KVBUFF-F FROM UTFIL-ARTSUM-KVBUFF-F           
029700     END-EVALUATE                                                         
029800     EJECT                                                                
029900     .                                                                    
030000 BAB-CASE-TECKEN-KVBUFF-OF SECTION.                                       
030100*                                                                         
030200*    TESTNING OM SALDOT IN ÄR PLUS, BLANKT=PLUS ELLER MINUS      *        
030300                                                                          
030400     MOVE INFIL-SALDO-KVBUFF-OF TO W-NUM-KVBUFF-OF                        
030500     MOVE ZERO TO UTFIL-ARTSUM-KVBUFF-OF                                  
030600                                                                          
030700     EVALUATE INFIL-SALDO-KDTECKEN-BUFF-OF                                
030800     WHEN PLUSTECKEN                                                      
030900             ADD W-NUM-KVBUFF-OF TO UTFIL-ARTSUM-KVBUFF-OF                
031000     WHEN BLANKTECKEN                                                     
031100             ADD W-NUM-KVBUFF-OF TO UTFIL-ARTSUM-KVBUFF-OF                
031200     WHEN MINUSTECKEN                                                     
031300             SUBTRACT W-NUM-KVBUFF-OF FROM UTFIL-ARTSUM-KVBUFF-OF         
031400     END-EVALUATE                                                         
031500     EJECT                                                                
031600     .                                                                    
031700 BAC-CASE-TECKEN-KVKOLLI-F SECTION.                                       
031800*                                                                         
031900*    TESTNING OM SALDOT IN ÄR PLUS, BLANKT=PLUS ELLER MINUS      *        
032000                                                                          
032100     MOVE INFIL-SALDO-KVKOLLI-F TO W-NUM-KVKOLLI-F                        
032200     MOVE ZERO TO UTFIL-ARTSUM-KVKOLLI-F                                  
032300                                                                          
032400     EVALUATE INFIL-SALDO-KDTECKEN-BUFF-F                                 
032500     WHEN PLUSTECKEN                                                      
032600             ADD W-NUM-KVKOLLI-F TO UTFIL-ARTSUM-KVKOLLI-F                
032700     WHEN BLANKTECKEN                                                     
032800             ADD W-NUM-KVKOLLI-F TO UTFIL-ARTSUM-KVKOLLI-F                
032900     WHEN MINUSTECKEN                                                     
033000             SUBTRACT W-NUM-KVKOLLI-F FROM                                
033100                      UTFIL-ARTSUM-KVKOLLI-F                              
033200     END-EVALUATE                                                         
033300     EJECT                                                                
033400     .                                                                    
033500 BAD-CASE-TECKEN-KVKOLLI-OF SECTION.                                      
033600*                                                                         
033700*    TESTNING OM SALDOT IN ÄR PLUS, BLANKT=PLUS ELLER MINUS      *        
033800                                                                          
033900     MOVE INFIL-SALDO-KVKOLLI-OF TO W-NUM-KVKOLLI-OF                      
034000     MOVE ZERO TO UTFIL-ARTSUM-KVKOLLI-OF                                 
034100                                                                          
034200     EVALUATE INFIL-SALDO-KDTECKEN-KLI-OF                                 
034300     WHEN PLUSTECKEN                                                      
034400             ADD W-NUM-KVKOLLI-OF TO UTFIL-ARTSUM-KVKOLLI-OF              
034500     WHEN BLANKTECKEN                                                     
034600             ADD W-NUM-KVKOLLI-OF TO UTFIL-ARTSUM-KVKOLLI-OF              
034700     WHEN MINUSTECKEN                                                     
034800             SUBTRACT W-NUM-KVKOLLI-OF FROM                               
034900                      UTFIL-ARTSUM-KVKOLLI-OF                             
035000     END-EVALUATE                                                         
035100     .                                                                    
035200     EJECT                                                                
035300 Z-FINIT   SECTION.                                                       
035400*                                                                         
035500     CLOSE  W48819                                                        
035600                                                                          
035700     MOVE 'S' TO POSTSUM-OPKOD                                            
035800     CALL POSTSUM USING POSTSUM-PARM                                      
035900     .                                                                    
036000     EJECT                                                                
036100 S11-LAS-W48819 SECTION.                                                  
036200*                                                                         
036300     READ W48819        INTO INFIL-AREA                                   
036400         AT END                                                           
036500              MOVE JA TO W48819-EOF                                       
036600     END-READ                                                             
036700     .                                                                    
036800     EJECT                                                                
036900 S20-SKRIV-W48821-PT20 SECTION.                                           
037000*                                                                         
037100     RELEASE SORT-POST FROM UTFIL-AREA                                    
037200                                                                          
037300     MOVE 'W48821'   TO POSTSUM-FDNAMN                                    
037400     MOVE 'W48820D2' TO POSTSUM-DDNAMN2                                   
037500     MOVE '020'      TO POSTSUM-TRANSTYP                                  
037600     CALL POSTSUM USING POSTSUM-PARM                                      
037700                                                                          
037800     MOVE SPACE TO UTFIL-AREA                                             
037900     .                                                                    
038000     EJECT                                                                
038100 S21-SKRIV-W48821-PT21 SECTION.                                           
038200*                                                                         
038300     RELEASE SORT-POST FROM UTFIL-AREA                                    
038400                                                                          
038500     MOVE 'W48821'   TO POSTSUM-FDNAMN                                    
038600     MOVE 'W48820D2' TO POSTSUM-DDNAMN2                                   
038700     MOVE '021'      TO POSTSUM-TRANSTYP                                  
038800     CALL POSTSUM USING POSTSUM-PARM                                      
038900                                                                          
039000     MOVE SPACE TO UTFIL-AREA                                             
039100     .                                                                    
