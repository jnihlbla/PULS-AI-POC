000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W4883000.                                        
000400 AUTHOR.                 TOMMY JOHANSSON.                                 
000500 DATE-WRITTEN.           FEBR  84.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*      FUNKTION:                                                          
001000*            LÄSER IN DATA FRÅN DET LOKALA SYSTEMET.                      
001100*                                                                         
001200*            SISTA  POSTEN IN ÄR EN SUMMAPOST.                            
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*- - - - - - - - - - - - - - INFILER:                                     
002200                                                                          
002300     SELECT  W48829                   ASSIGN  W48830D1.                   
002400     SKIP2                                                                
002500*- - - - - - - - - - - - - - UTFILER:                                     
002600*                                     FELPOSTER                           
002700     SELECT  W48830                   ASSIGN  W48830D2.                   
002800*                                     RIKTIGA POSTER                      
002900     SELECT  W48831                   ASSIGN  W48830D3.                   
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W48829                                                               
003600     LABEL RECORD STANDARD                                                
003700     RECORDING      V                                                     
003800     BLOCK CONTAINS 0.                                                    
003900     SKIP2                                                                
004000*01  -COPY W488009                   -L.                                  
004100*++INCLUDE W488009CC0                                                     
004200     SKIP2                                                                
004300*01  -COPY W488010                   -L.                                  
004400*++INCLUDE W488010CC0                                                     
004500     SKIP2                                                                
004600*01  -COPY W488011                   -L.                                  
004700*++INCLUDE W488011CC0                                                     
004800     EJECT                                                                
004900 FD  W48830                                                               
005000     LABEL RECORD STANDARD                                                
005100     RECORDING   F                                                        
005200     BLOCK CONTAINS 0.                                                    
005300     SKIP2                                                                
005400*01  FEL-POST -COPY W488FEL1         -L.                                  
005500*++INCLUDE W488FEL1C0                                                     
005600     SKIP3                                                                
005700 FD  W48831                                                               
005800     LABEL RECORD STANDARD                                                
005900     RECORDING   F                                                        
006000     BLOCK CONTAINS 0.                                                    
006100     SKIP2                                                                
006200*01  031-POST -COPY W488031          -L.                                  
006300*++INCLUDE W488031CC0                                                     
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600     SKIP2                                                                
006601                                                                          
006610*    -- CHECKED BY WY2000                                                 
006700*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
006800 77   PROGRAM-NAMN           VALUE 'W4883000'                             
006900                                 PIC X(8).                                
007000     SKIP2                                                                
007100*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007200                                                                          
007300 77  JA                          PIC X(1)        VALUE 'J'.               
007400 77  NEJ                         PIC X(1)        VALUE 'N'.               
007500 77  RAETT                       PIC X(1)        VALUE 'R'.               
007600 77  FEL                         PIC X(1)        VALUE 'F'.               
007700 77  HELT-FEL                    PIC X(1)        VALUE 'A'.               
007800 77  PLUSTECKEN                  PIC X(1)        VALUE '+'.               
007900 77  BLANKTECKEN                 PIC X(1)        VALUE ' '.               
008000 77  MINUSTECKEN                 PIC X(1)        VALUE '-'.               
008100 77  W-IDPTYP-010                PIC X(3)        VALUE '010'.             
008200 77  W-IDPTYP-011                PIC X(3)        VALUE '011'.             
008300     SKIP2                                                                
008400*- - - - - - - - - - - - - -  END-OF-FILE SWITCHAR                        
008500                                                                          
008600 77  W48829-EOF                  PIC X(1)        VALUE 'N'.               
008700     EJECT                                                                
008800*- - - - - - - - - - - - - -  ARBETSFÄLT                                  
008900 77  W-SIST-INLAST-POST          PIC  9(3)   VALUE ZERO.                  
009000 77  W-NUM-KVBUFF-F              PIC  9(7)   VALUE ZERO.                  
009100 77  W-NUM-KVBUFF-OF             PIC  9(7)   VALUE ZERO.                  
009200 77  W-SPAR-KVBUFF-F             PIC S9(7)   VALUE ZERO  COMP-3.          
009300 77  W-SPAR-KVBUFF-OF            PIC S9(7)   VALUE ZERO  COMP-3.          
009400 77  W-NUM-KVKOLLI-F             PIC  9(7)   VALUE ZERO.                  
009500 77  W-NUM-KVKOLLI-OF            PIC  9(7)   VALUE ZERO.                  
009600 77  W-SPAR-KVKOLLI-F            PIC S9(7)   VALUE ZERO  COMP-3.          
009700 77  W-SPAR-KVKOLLI-OF           PIC S9(7)   VALUE ZERO  COMP-3.          
009800*                                                                         
009900 77  W-SPAR-POSTER-PDP           PIC S9(5)   VALUE ZERO  COMP-3.          
010000 77  W-SPAR-LASTA-POSTER         PIC S9(5)   VALUE ZERO  COMP-3.          
010010*      --- VALID IDDC CODES                                               
010020*                                                                         
010030*01    -COPY WWDCKONS                                                     
010040       EJECT                                                              
010100*                                                                         
010200 01  INPOST-TEST                 PIC X(1).                                
010300   88  INPOST-OK                             VALUE 'R'.                   
010400   88  INPOST-FEL                            VALUE 'F'.                   
010500   88  INPOST-ABEND                          VALUE 'A'.                   
010600     SKIP3                                                                
010700 01  DYNAMISKA-SUBPROGRAM.                                                
010800   03  POSTSUM                   PIC X(8)     VALUE 'POSTSUM '.           
010900   03  ABEND                     PIC X(8)     VALUE 'ABEND   '.           
011000     SKIP3                                                                
011100 01  RETURKODER.                                                          
011200   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)    COMP SYNC VALUE +16.        
011300     EJECT                                                                
011400*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
011500                                                                          
011600*01  -COPY W0005       -PRE POSTSUM-.                                     
011700*++INCLUDE W0005CCCC0                                                     
011800     EJECT                                                                
011900 01  FILLER                      PIC X(24)   VALUE                        
012000                                            'INFIL-AREA-START'.           
012100     SKIP2                                                                
012200 01  INFIL-AREA                  PIC X(80).                               
012300     SKIP2                                                                
012400*01  W488009 -COPY W488009    -PRE INFIL- -RED INFIL-AREA.                
012500*++INCLUDE W488009CC0                                                     
012600     SKIP2                                                                
012700*01  W488010 -COPY W488010    -PRE INFIL- -RED INFIL-AREA.                
012800*++INCLUDE W488010CC0                                                     
012900     SKIP2                                                                
013000*01  W488011 -COPY W488011    -PRE INFIL- -RED INFIL-AREA.                
013100*++INCLUDE W488011CC0                                                     
013200     EJECT                                                                
013300 01  FILLER                      PIC X(24)    VALUE                       
013400                                              'UT-AREA-START'.            
013500     SKIP2                                                                
013600*01  AREA  -COPY W488031    -PRE 031-.                                    
013700*++INCLUDE W488031CC0                                                     
013800     EJECT                                                                
013900*01  AREA  -COPY W488FEL1   -PRE FEL-.                                    
014000*++INCLUDE W488FEL1C0                                                     
014100     EJECT                                                                
014200 PROCEDURE DIVISION.                                                      
014300     SKIP3                                                                
014400     PERFORM A-INIT                                                       
014500                                                                          
014600     PERFORM S11-LAS-W48829                                               
014700                                                                          
014800     PERFORM UNTIL W48829-EOF = JA                                        
014900                                                                          
015000         IF INFIL-SALDO-IDPTYP-010 = '010' OR '011'                       
015100             PERFORM B-TESTA-INDATA                                       
015200                                                                          
015300             PERFORM C-SKRIV-UTDATA                                       
015400         END-IF                                                           
015500                                                                          
015600         PERFORM S11-LAS-W48829                                           
015700     END-PERFORM                                                          
015800                                                                          
015900     IF W-SIST-INLAST-POST NOT = W-IDPTYP-011                             
016000         MOVE HELT-FEL TO INPOST-TEST                                     
016100     END-IF                                                               
016200                                                                          
016300     IF INPOST-ABEND                                                      
016400         DISPLAY '*** FEL I UPPSÄNDNING FRÅN PDP ***'                     
016500         DISPLAY '*** PDP:N SÄGER  : ' W-SPAR-POSTER-PDP                  
016600         DISPLAY '*** W48830 SÄGER : ' W-SPAR-LASTA-POSTER                
016700         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
016800     ELSE                                                                 
016900                                                                          
017000         PERFORM Z-FINIT                                                  
017100         MOVE ZERO TO RETURN-CODE                                         
017200         GOBACK                                                           
017300     END-IF                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700     SKIP2                                                                
017800     OPEN INPUT  W48829                                                   
017900     OPEN OUTPUT W48830                                                   
018000                 W48831                                                   
018100                                                                          
018200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
018300     MOVE ZERO TO W-SPAR-LASTA-POSTER                                     
018400     .                                                                    
018500     EJECT                                                                
018600 B-TESTA-INDATA SECTION.                                                  
018700     SKIP2                                                                
018800     MOVE RAETT TO INPOST-TEST                                            
018900     IF INFIL-SALDO-IDPTYP-011 = W-IDPTYP-011                             
019000***    FÖR ATT TA BORT DEN TOMRAD SOM LIGGER SIST I FILEN                 
019100*                                                                         
019200         MOVE JA TO W48829-EOF                                            
019300         INSPECT INFIL-SALDO-KVSALDOPOST                                  
019400                            REPLACING LEADING SPACE BY ZERO               
019500         MOVE INFIL-SALDO-KVSALDOPOST TO W-SPAR-POSTER-PDP                
019600         IF W-SPAR-POSTER-PDP NOT = W-SPAR-LASTA-POSTER                   
019700             MOVE HELT-FEL TO INPOST-TEST                                 
019800         END-IF                                                           
019900     ELSE                                                                 
020000         ADD +1 TO W-SPAR-LASTA-POSTER                                    
020100         INSPECT INFIL-SALDO-IDARTNR                                      
020200                            REPLACING LEADING SPACE BY ZERO               
020300         INSPECT INFIL-SALDO-KVBUFF-F                                     
020400                            REPLACING LEADING SPACE BY ZERO               
020500         INSPECT INFIL-SALDO-KVBUFF-OF                                    
020600                            REPLACING LEADING SPACE BY ZERO               
020700         INSPECT INFIL-SALDO-KVKOLLI-F                                    
020800                            REPLACING LEADING SPACE BY ZERO               
020900         INSPECT INFIL-SALDO-KVKOLLI-OF                                   
021000                            REPLACING LEADING SPACE BY ZERO               
021100         IF INFIL-SALDO-IDARTNR NUMERIC                                   
021200             IF INFIL-SALDO-IDARTNR = ZERO                                
021300                 MOVE FEL TO INPOST-TEST                                  
021400             END-IF                                                       
021500         ELSE                                                             
021600             MOVE FEL TO INPOST-TEST                                      
021700         END-IF                                                           
021800         IF INFIL-SALDO-KVBUFF-F NOT NUMERIC                              
021900             MOVE FEL TO INPOST-TEST                                      
022000         END-IF                                                           
022100         IF INFIL-SALDO-KVBUFF-OF NOT NUMERIC                             
022200             MOVE FEL TO INPOST-TEST                                      
022300         END-IF                                                           
022400         IF INFIL-SALDO-KVKOLLI-F NOT NUMERIC                             
022500             MOVE FEL TO INPOST-TEST                                      
022600         END-IF                                                           
022700         IF INFIL-SALDO-KVKOLLI-OF NOT NUMERIC                            
022800             MOVE FEL TO INPOST-TEST                                      
022900         END-IF                                                           
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 C-SKRIV-UTDATA SECTION.                                                  
023400*                                                                         
023500*    SKRIVNING AV UTPOSTERNA FELAKTIG POST SKRIVS VIA FELPOSTEN           
023600*    UT PÅ FILEN.                                                         
023700                                                                          
023800     MOVE SPACE TO 031-AREA                                               
023900     IF INPOST-FEL                                                        
024000         MOVE '004'                     TO FEL-IDPTYP                     
024100         MOVE INFIL-SALDO-IDARTNR       TO FEL-IDARTNR                    
024200         MOVE WC-CDC-SE                 TO FEL-IDDC                       
024300         MOVE INFIL-SALDO-KVBUFF-F      TO FEL-KVBUFF-F                   
024400         MOVE INFIL-SALDO-KVBUFF-OF     TO FEL-KVBUFF-OF                  
024500         MOVE INFIL-SALDO-KVKOLLI-F     TO FEL-KVKOLLI-F                  
024600         MOVE INFIL-SALDO-KVKOLLI-OF    TO FEL-KVKOLLI-OF                 
024700         PERFORM S21-SKRIV-FELPOST                                        
024800     ELSE                                                                 
024900       IF INFIL-SALDO-IDPTYP-011 = W-IDPTYP-010                           
025000         MOVE ZERO TO W-SPAR-KVBUFF-F                                     
025100                      W-SPAR-KVBUFF-OF                                    
025200                      W-SPAR-KVKOLLI-F                                    
025300                      W-SPAR-KVKOLLI-OF                                   
025400         MOVE INFIL-SALDO-KVBUFF-F     TO W-NUM-KVBUFF-F                  
025500         MOVE INFIL-SALDO-KVBUFF-OF    TO W-NUM-KVBUFF-OF                 
025600         MOVE INFIL-SALDO-KVKOLLI-F    TO W-NUM-KVKOLLI-F                 
025700         MOVE INFIL-SALDO-KVKOLLI-OF   TO W-NUM-KVKOLLI-OF                
025800                                                                          
025900         EVALUATE INFIL-SALDO-KDTECKEN-BUFF-F                             
026000           WHEN PLUSTECKEN                                                
026100                  ADD W-NUM-KVBUFF-F TO W-SPAR-KVBUFF-F                   
026200           WHEN BLANKTECKEN                                               
026300                  ADD W-NUM-KVBUFF-F TO W-SPAR-KVBUFF-F                   
026400           WHEN MINUSTECKEN                                               
026500                  SUBTRACT W-NUM-KVBUFF-F FROM W-SPAR-KVBUFF-F            
026600         END-EVALUATE                                                     
026700                                                                          
026800         EVALUATE INFIL-SALDO-KDTECKEN-BUFF-OF                            
026900           WHEN PLUSTECKEN                                                
027000                 ADD W-NUM-KVBUFF-OF TO W-SPAR-KVBUFF-OF                  
027100           WHEN BLANKTECKEN                                               
027200                 ADD W-NUM-KVBUFF-OF TO W-SPAR-KVBUFF-OF                  
027300           WHEN MINUSTECKEN                                               
027400                 SUBTRACT W-NUM-KVBUFF-OF FROM W-SPAR-KVBUFF-OF           
027500         END-EVALUATE                                                     
027600                                                                          
027700         EVALUATE INFIL-SALDO-KDTECKEN-KLI-F                              
027800           WHEN PLUSTECKEN                                                
027900                  ADD W-NUM-KVKOLLI-F TO W-SPAR-KVKOLLI-F                 
028000           WHEN BLANKTECKEN                                               
028100                  ADD W-NUM-KVKOLLI-F TO W-SPAR-KVKOLLI-F                 
028200           WHEN MINUSTECKEN                                               
028300                  SUBTRACT W-NUM-KVKOLLI-F FROM W-SPAR-KVKOLLI-F          
028400         END-EVALUATE                                                     
028500                                                                          
028600         EVALUATE INFIL-SALDO-KDTECKEN-KLI-OF                             
028700           WHEN PLUSTECKEN                                                
028800                  ADD W-NUM-KVKOLLI-OF TO W-SPAR-KVKOLLI-OF               
028900           WHEN BLANKTECKEN                                               
029000                  ADD W-NUM-KVKOLLI-OF TO W-SPAR-KVKOLLI-OF               
029100           WHEN MINUSTECKEN                                               
029200                 SUBTRACT W-NUM-KVKOLLI-OF FROM W-SPAR-KVKOLLI-OF         
029300         END-EVALUATE                                                     
029400                                                                          
029500         MOVE '031'               TO 031-IDPTYP                           
029600         MOVE INFIL-SALDO-IDARTNR TO 031-IDARTNR                          
029700         MOVE WC-CDC-SE           TO 031-IDDC                             
029800         MOVE 1                   TO 031-ADBUFFOMR                        
029900         MOVE W-SPAR-KVBUFF-F     TO 031-KVBUFF-F                         
030000         MOVE W-SPAR-KVBUFF-OF    TO 031-KVBUFF-OF                        
030100         MOVE W-SPAR-KVKOLLI-F    TO 031-KVKOLLI-F                        
030200         MOVE W-SPAR-KVKOLLI-OF   TO 031-KVKOLLI-OF                       
030300         PERFORM S22-SKRIV-W48831                                         
030400       END-IF                                                             
030500     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 Z-FINIT   SECTION.                                                       
030900*                                                                         
031000     CLOSE  W48829                                                        
031100            W48830                                                        
031200            W48831                                                        
031300                                                                          
031400*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
031500*                               SKRIVNA POSTER                            
031600     MOVE 'S' TO POSTSUM-OPKOD                                            
031700     CALL POSTSUM USING POSTSUM-PARM                                      
031800     .                                                                    
031900     EJECT                                                                
032000 S11-LAS-W48829 SECTION.                                                  
032100*                                                                         
032200*    LÄSNING AV INDATA                                                    
032300*                                                                         
032400     READ W48829 INTO INFIL-AREA                                          
032500       AT END   MOVE JA TO W48829-EOF                                     
032600     END-READ                                                             
032700                                                                          
032800     IF W48829-EOF = NEJ                                                  
032900         IF INFIL-SALDO-IDPTYP-011 = '010' OR '011'                       
033000             MOVE INFIL-SALDO-IDPTYP-011 TO W-SIST-INLAST-POST            
033100         END-IF                                                           
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 S21-SKRIV-FELPOST SECTION.                                               
033600*                                                                         
033700*    SKRIVNING AV UT DATA                                                 
033800*                                                                         
033900     MOVE 'W48830'   TO POSTSUM-FDNAMN                                    
034000     MOVE 'W48830D2' TO POSTSUM-DDNAMN2                                   
034100     MOVE 'FEL'      TO POSTSUM-TRANSTYP                                  
034200     CALL POSTSUM USING POSTSUM-PARM                                      
034300                                                                          
034400     WRITE FEL-POST FROM FEL-AREA                                         
034500     .                                                                    
034600     EJECT                                                                
034700 S22-SKRIV-W48831 SECTION.                                                
034800*                                                                         
034900*    SKRIVNING AV UT DATA                                                 
035000*                                                                         
035100     MOVE 'W48831'   TO POSTSUM-FDNAMN                                    
035200     MOVE 'W48830D3' TO POSTSUM-DDNAMN2                                   
035300     MOVE '031'      TO POSTSUM-TRANSTYP                                  
035400     CALL POSTSUM USING POSTSUM-PARM                                      
035500                                                                          
035600     WRITE 031-POST FROM 031-AREA                                         
035700     .                                                                    
