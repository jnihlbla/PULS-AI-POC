000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2362800.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LISTA TRANSPORTTIDER                                             
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INLEV GÅGNA PERIODEN                                       
002500     SELECT W23626                     ASSIGN TO W23628D1.                
002600     SKIP2                                                                
002700*          --- LISTA TRANSPORTTIDER  SOM FIL                              
002800     SELECT W23628-001                 ASSIGN TO W23628D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W23626                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W23636      -L.                                                
003900     SKIP3                                                                
004000 FD  W23628-001                                                           
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300     SKIP2                                                                
004400 01  W23628-001-RAD              PIC X(125).                              
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W2362800'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  W23626-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W23626                       VALUE 'J'.                   
005400                                                                          
005500 01  ARBETSAREOR.                                                         
005600     03  IX-DEL                  PIC S9(5)    VALUE ZERO  COMP-3.         
005610     03  IX-TT                   PIC S9(5)    VALUE ZERO  COMP-3.         
005700     03  OLD-IDLEVNR             PIC X(5)     VALUE SPACE.                
005800     03  OLD-IDARTNR             PIC 9(9)     VALUE ZERO  COMP-3.         
005900     03  OLD-KVDAGAR-TT          PIC S9(3)    VALUE ZERO  COMP-3.         
006000     03  WS-KVDAGAR              PIC S9(3)    VALUE ZERO  COMP-3.         
006100     03  WS-TTILLF               PIC S9(3)    VALUE ZERO.                 
006200     03  WS-ANTAL                PIC 9(5)     VALUE ZERO.                 
006300     03  WS-TOTSUM               PIC 9(8)V99  VALUE ZERO.                 
006400     03  FILLER OCCURS 30.                                                
006500         05  WS-SUM              PIC 9(8)V99  VALUE ZERO.                 
006600     03  FILLER OCCURS 30.                                                
006700         05  WS-PROC             PIC 9(8)V99  VALUE ZERO.                 
006800     03  WS-W001-PROC            PIC 9(8)V99  VALUE ZERO.                 
006810     03  WS-RATT                 PIC 999V99   VALUE ZERO.                 
006900     03  WS-TIDIGA               PIC 999V99   VALUE ZERO.                 
007000     03  WS-SENA                 PIC 999V99   VALUE ZERO.                 
007100     03  WS-SNITT                PIC 9(8)V99  VALUE ZERO.                 
007200     03  WS-VIKT                 PIC 9(8)V99  VALUE ZERO.                 
007300     03  WS-SUMVIKT              PIC 9(8)V99  VALUE ZERO.                 
007310     03  TOT-TTILLF              PIC S9(3)    VALUE ZERO.                 
007320     03  TOT-ANTAL               PIC 9(7)     VALUE ZERO.                 
007330     03  TOT-TOTSUM              PIC 9(8)V99  VALUE ZERO.                 
007331     03  WS-TOTSUM-TOT           PIC 9(8)V99  VALUE ZERO.                 
007340     03  FILLER OCCURS 30.                                                
007350         05  TOT-SUM             PIC 9(8)V99  VALUE ZERO.                 
007360     03  FILLER OCCURS 30.                                                
007370         05  TOT-PROC            PIC 9(8)V99  VALUE ZERO.                 
007380     03  TOT-W001-PROC           PIC 9(8)V99  VALUE ZERO.                 
007390     03  TOT-PROC-RATT           PIC 999V99   VALUE ZERO.                 
007391     03  TOT-PROC-TIDIGA         PIC 999V99   VALUE ZERO.                 
007392     03  TOT-PROC-SENA           PIC 999V99   VALUE ZERO.                 
007393     03  TOT-SNITT               PIC 9(8)V99  VALUE ZERO.                 
007394     03  TOT-VIKT                PIC 9(8)V99  VALUE ZERO.                 
007395     03  TOT-SUMVIKT             PIC 9(8)V99  VALUE ZERO.                 
007396     03  WS-SUM-RATT             PIC 9(8)V99  VALUE ZERO.                 
007397     03  WS-SUM-TIDIGA           PIC 9(8)V99  VALUE ZERO.                 
007398     03  WS-SUM-SENA             PIC 9(8)V99  VALUE ZERO.                 
007400     03  WS-IDLOPNRM             PIC 9(9).                                
007500     03  FILLER REDEFINES WS-IDLOPNRM.                                    
007600         05  FILLER              PIC 9(1).                                
007700         05  WS-VVD              PIC 9(3).                                
007800         05  FILLER REDEFINES WS-VVD.                                     
007900             07  WS-VV           PIC 9(2).                                
008000             07  FILLER          PIC 9(1).                                
008100         05  WS-LLLL             PIC 9(4).                                
008200         05  FILLER              PIC 9(1).                                
008300     03  WS-TIAAVVD              PIC 9(5).                                
008400     03  FILLER REDEFINES WS-TIAAVVD.                                     
008500         05  WS-TIAA             PIC 9(2).                                
008600         05  WS-TIVVD            PIC 9(3).                                
008700     03  WS-TIAAMMDD             PIC 9(6).                                
008800     EJECT                                                                
008900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009000 01  FILLER REDEFINES DAGENS-DATUM.                                       
009100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009400 01  DAGENS-AA                   PIC 9(2).                                
009500 01  DAGENS-VV                   PIC 9(2).                                
009600 01  FOREG-AA                    PIC 9(2).                                
009700     EJECT                                                                
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900*                                                                         
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
010400     SKIP2                                                                
010500*    --- PARAMETRAR TILL ABEND                                            
010600                                                                          
010700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011000     SKIP2                                                                
011100 01  FELTEXT.                                                             
011200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL POSTSUM                                          
011600*                                                                         
011700*01  -COPY W0005   -PRE  POSTSUM-                                         
011800     EJECT                                                                
011900*01  -COPY WDATAREA                                                       
012000     EJECT                                                                
012100*01  -COPY WORKAREA                                                       
012200     EJECT                                                                
012300 01  IN-AREA-START               PIC X(24)   VALUE                        
012400                                 'IN-AREA-START  '.                       
012500     SKIP2                                                                
012600                                                                          
012700*01  AREA -COPY W23636     -PRE IN-                                       
012800     EJECT                                                                
012900 01  W001-AREA-START             PIC X(24)   VALUE                        
013000                                 'W001-AREA-START  '.                     
013100     SKIP2                                                                
013200 01  W001-HJALPAREOR.                                                     
013300*                                                                         
013400     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
013500     03  W001-MAX-RADER-PER-SIDA                                          
013600                                 PIC 9(3)    VALUE 42.                    
013700     03  W001-LISTNR             PIC X(11)   VALUE 'W23628-001'.          
013800     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
013900     03  W001-RADRAKNARE         PIC S9(3)   COMP-3 VALUE 999.            
014000     EJECT                                                                
014100 01  W001-RAD.                                                            
014200*                                                                         
014300     03  FILLER                  PIC X(121)  VALUE SPACE.                 
014400     EJECT                                                                
014500 01  W001-RUBRIK1.                                                        
014600*                                                                         
014700     03  FILLER                  PIC X(3)  VALUE SPACE.                   
014800     03  FILLER                  PIC X(21)                                
014900                                VALUE 'VOLVO CAR CUST SERV  '.            
015000     03  FILLER                  PIC X(12)                                
015100                                 VALUE 'W23628-001'.                      
015200     03  FILLER                  PIC X(54)                                
015300     VALUE 'PERIOD: DELIVERY PRECISION IN % OF TRANSPORT DAYS'.           
015400     03  W001-DATUM              PIC XXBXXBXX.                            
015500     03  FILLER                  PIC X(3)   VALUE SPACE.                  
015600     03  FILLER                  PIC X(5)                                 
015700                                 VALUE 'PAGE '.                           
015800     03  W001-SID                PIC Z(4)9.                               
015900     SKIP2                                                                
016000 01  W001-RUBRIKA.                                                        
016100     03  FILLER                  PIC X(1)  VALUE SPACE.                   
016200     03  FILLER                  PIC X(43)                                
016300     VALUE 'SUPPL   NO  AVER.  TT CORR.  LATE EARLY'.                     
016400     03  FILLER                  PIC X(41)                                
016500     VALUE '  0     1     2     3     4     5     6 '.                    
016600     03  FILLER                  PIC X(35)                                
016700     VALUE '   7     8     9    10    11   >11 '.                         
016800     SKIP2                                                                
016900 01  W001-RUBRIKB.                                                        
017000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
017100     03  FILLER                  PIC X(47)                                
017200     VALUE '           (DAYS)        %     %     %    '.                  
017300     EJECT                                                                
017400 01  W001-SNITTA.                                                         
017500     03  FILLER                  PIC X(1)   VALUE SPACE.                  
017600     03  W001-IDLEVNR-SNITT      PIC X(5)   VALUE SPACE.                  
017620     03  W001-ANT                PIC Z(05)  VALUE ZERO.                   
017700     03  FILLER                  PIC X(2)   VALUE SPACE.                  
017800     03  W001-SNITTAVV           PIC ZZ9.9  VALUE ZERO.                   
017900     03  FILLER                  PIC X(01)  VALUE SPACE.                  
018000     03  W001-TT                 PIC Z(03)  VALUE SPACE.                  
018100     03  FILLER                  PIC X(01)  VALUE SPACE.                  
018200     03  W001-RATT               PIC ZZ9.9  VALUE ZERO.                   
018300     03  FILLER                  PIC X(01)  VALUE SPACE.                  
018400     03  W001-SENA               PIC ZZ9.9  VALUE ZERO.                   
018500     03  FILLER                  PIC X(01)  VALUE SPACE.                  
018600     03  W001-TIDIGA             PIC ZZ9.9  VALUE ZERO.                   
018720     03  FILLER                  PIC X(02)  VALUE SPACE.                  
018800     03  FILLER OCCURS 13.                                                
018900         05  W001-PROC           PIC ZZ9.9 BLANK WHEN ZERO                
018910                                           VALUE ZERO.                    
019000         05  FILLER              PIC X(01) VALUE SPACE.                   
019010     EJECT                                                                
019020 01  W001-SNITT-TOT.                                                      
019030     03  FILLER                  PIC X(1)   VALUE SPACE.                  
019040     03  W001-IDLEVNR-TOT        PIC X(3)   VALUE SPACE.                  
019060     03  W001-ANT-TOT            PIC Z(07)  VALUE ZERO.                   
019070     03  FILLER                  PIC X(2)   VALUE SPACE.                  
019080     03  W001-SNITTAVV-TOT       PIC ZZ9.9  VALUE ZERO.                   
019090     03  FILLER                  PIC X(01)  VALUE SPACE.                  
019091     03  W001-TT-TOT             PIC Z(03)  VALUE SPACE.                  
019092     03  FILLER                  PIC X(01)  VALUE SPACE.                  
019093     03  W001-RATT-TOT           PIC ZZ9.9  VALUE ZERO.                   
019094     03  FILLER                  PIC X(01)  VALUE SPACE.                  
019095     03  W001-SENA-TOT           PIC ZZ9.9  VALUE ZERO.                   
019096     03  FILLER                  PIC X(01)  VALUE SPACE.                  
019097     03  W001-TIDIGA-TOT         PIC ZZ9.9  VALUE ZERO.                   
019098     03  FILLER                  PIC X(02)  VALUE SPACE.                  
019099     03  FILLER OCCURS 13.                                                
019100         05  W001-PROC-TOT       PIC ZZ9.9 BLANK WHEN ZERO                
019101                                           VALUE ZERO.                    
019102         05  FILLER              PIC X(01) VALUE SPACE.                   
019110     EJECT                                                                
019200 PROCEDURE DIVISION.                                                      
019300 MAIN SECTION.                                                            
019400     SKIP2                                                                
019500                                                                          
019600     PERFORM A-INIT                                                       
019700     PERFORM S01-LAES-W23626                                              
019800     MOVE IN-IDLEVNR    TO OLD-IDLEVNR                                    
019900     MOVE IN-KVDAGAR-TT TO OLD-KVDAGAR-TT                                 
020000     MOVE ZERO          TO OLD-IDARTNR                                    
020100     PERFORM UNTIL END-OF-W23626                                          
020200       IF IN-IDLEVNR NOT = OLD-IDLEVNR                                    
020300          PERFORM C-SUMMERA-OLD-LEV                                       
020400       END-IF                                                             
020500                                                                          
020600       PERFORM B-BERAKNA-RAD-ARTNR                                        
020700                                                                          
020800       PERFORM S01-LAES-W23626                                            
020900     END-PERFORM                                                          
021000     PERFORM C-SUMMERA-OLD-LEV                                            
021100                                                                          
021110     PERFORM D-SUMMERA-TOT-LEV                                            
021120                                                                          
021200     PERFORM Z-FINIT                                                      
021300                                                                          
021400     MOVE ZERO TO RETURN-CODE                                             
021500     GOBACK                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 A-INIT SECTION.                                                          
021900                                                                          
022000     OPEN INPUT  W23626                                                   
022100                                                                          
022200     OPEN OUTPUT W23628-001                                               
022300     SKIP2                                                                
022400     ACCEPT DAGENS-DATUM  FROM DATE                                       
022500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022600                                                                          
022700     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
022800     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
022900                          W001-DATUM                                      
023000                                                                          
023100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
023200                     DAT-O-TIDATUM DAT-KDSVAR                             
023300                                                                          
023400     IF DAT-KDSVAR-OK                                                     
023500       MOVE DAT-TIVV  TO DAGENS-VV                                        
023600       MOVE DAT-TIAA  TO DAGENS-AA                                        
023700       SUBTRACT 1 FROM DAGENS-AA GIVING FOREG-AA                          
023800     ELSE                                                                 
023900       DISPLAY ' FEL I DATKONV INIT'                                      
024000       PERFORM S99-ABEND                                                  
024100     END-IF                                                               
024200     .                                                                    
024300     EJECT                                                                
024400 B-BERAKNA-RAD-ARTNR SECTION.                                             
024500                                                                          
024600     PERFORM BA-FIXA-DATUM                                                
024700                                                                          
024800     COMPUTE IX-DEL = WS-KVDAGAR + 1                                      
024900     IF IX-DEL < 1                                                        
025000        MOVE 1            TO IX-DEL                                       
025100     END-IF                                                               
025200     IF IX-DEL > 30                                                       
025300        MOVE 30           TO IX-DEL                                       
025400     END-IF                                                               
025500     MOVE +1              TO WS-TTILLF                                    
025600     ADD  WS-TTILLF       TO WS-SUM   (IX-DEL)                            
025610     ADD  WS-TTILLF       TO TOT-SUM  (IX-DEL)                            
025700     ADD  +1              TO WS-ANTAL                                     
025710     ADD  +1              TO TOT-ANTAL                                    
025800     MOVE IN-IDARTNR      TO OLD-IDARTNR                                  
025900                                                                          
026000     PERFORM BB-ADDERA-SNITTBER                                           
026100     .                                                                    
026200     EJECT                                                                
026300 BA-FIXA-DATUM SECTION.                                                   
026400     SKIP2                                                                
026500     MOVE IN-IDLOPNRM          TO WS-IDLOPNRM                             
026600     MOVE WS-VVD               TO WS-TIVVD                                
026700     IF WS-VV > DAGENS-VV                                                 
026800        MOVE FOREG-AA          TO WS-TIAA                                 
026900     ELSE                                                                 
027000        MOVE DAGENS-AA         TO WS-TIAA                                 
027100     END-IF                                                               
027200     MOVE 'AAVVD'              TO DAT-KDDATFORM                           
027300     MOVE WS-TIAAVVD           TO DAT-I-TIDATUM                           
027400                                                                          
027500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
027600                     DAT-O-TIDATUM DAT-KDSVAR                             
027700                                                                          
027800     IF DAT-KDSVAR-OK                                                     
027900       MOVE DAT-TIAAMMDD       TO WS-TIAAMMDD                             
028000     ELSE                                                                 
028100       DISPLAY 'FEL I DATKONV 1 '                                         
028200       PERFORM S99-ABEND                                                  
028300     END-IF                                                               
028400                                                                          
028500     IF WS-TIAAMMDD = IN-TIAVIDAT                                         
028600        MOVE ZERO TO WS-KVDAGAR                                           
028700     ELSE                                                                 
028800      IF WS-TIAAMMDD > IN-TIAVIDAT                                        
028900        MOVE 001               TO WORK-KDCALL                             
028910        MOVE '11'              TO WORK-IDDC                               
029000        MOVE IN-TIAVIDAT       TO WORK-TIAAMMDD-FOM                       
029100        MOVE WS-TIAAMMDD       TO WORK-TIAAMMDD-TOM                       
029200        CALL WORKDAY  USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR        
029300        IF WORK-KDSVAR-FEL                                                
029400           DISPLAY ' FEL I WORKDAY 1 '                                    
029500           PERFORM S99-ABEND                                              
029600        END-IF                                                            
029700        MOVE WORK-KVWORKD      TO WS-KVDAGAR                              
029710        SUBTRACT 1           FROM WS-KVDAGAR                              
029800      ELSE                                                                
029900        MOVE 001               TO WORK-KDCALL                             
029910        MOVE '11'              TO WORK-IDDC                               
030000        MOVE IN-TIAVIDAT       TO WORK-TIAAMMDD-TOM                       
030100        MOVE WS-TIAAMMDD       TO WORK-TIAAMMDD-FOM                       
030200        CALL WORKDAY  USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR        
030300        IF WORK-KDSVAR-FEL                                                
030400           DISPLAY ' FEL I WORKDAY 2 '                                    
030500           PERFORM S99-ABEND                                              
030600        END-IF                                                            
030700        MOVE WORK-KVWORKD      TO WS-KVDAGAR                              
030710        SUBTRACT 1           FROM WS-KVDAGAR                              
030800        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
030900        DISPLAY ' ? ' IN-IDLEVNR ' ' IN-IDARTNR ' '                       
031000              'AVI:'  IN-TIAVIDAT  ' LOPNR: ' WS-TIAAMMDD                 
031100              ' KVD ' WS-KVDAGAR                                          
031200      END-IF                                                              
031300     END-IF                                                               
031400                                                                          
031600     .                                                                    
031700     EJECT                                                                
031800 BB-ADDERA-SNITTBER  SECTION.                                             
031900                                                                          
032000     IF WS-KVDAGAR < ZERO                                                 
032100        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
032200     END-IF                                                               
032300     COMPUTE WS-VIKT = WS-KVDAGAR * WS-TTILLF                             
032400     ADD WS-VIKT    TO WS-SUMVIKT                                         
032410     ADD WS-VIKT    TO TOT-SUMVIKT                                        
032500     .                                                                    
032600     EJECT                                                                
032700 C-SUMMERA-OLD-LEV   SECTION.                                             
032800                                                                          
032900     MOVE ZERO              TO WS-TOTSUM                                  
033000                                                                          
033100     MOVE +1                TO IX-DEL                                     
033200     PERFORM UNTIL IX-DEL > 30                                            
033300        ADD WS-SUM (IX-DEL) TO WS-TOTSUM                                  
033400        ADD +1              TO IX-DEL                                     
033500     END-PERFORM                                                          
033600                                                                          
033700     MOVE +1                TO IX-DEL                                     
033800     PERFORM UNTIL IX-DEL > 30                                            
033900        IF WS-TOTSUM > ZERO                                               
034000           COMPUTE WS-PROC (IX-DEL) ROUNDED =                             
034100                   100 * WS-SUM (IX-DEL) / WS-TOTSUM                      
034200        ELSE                                                              
034300           MOVE ZERO        TO WS-PROC (IX-DEL)                           
034400        END-IF                                                            
034410        IF IX-DEL > 12                                                    
034500           ADD  WS-PROC (IX-DEL) TO WS-W001-PROC                          
034501        ELSE                                                              
034510           MOVE WS-PROC (IX-DEL) TO W001-PROC (IX-DEL)                    
034520        END-IF                                                            
034600        ADD +1              TO IX-DEL                                     
034700     END-PERFORM                                                          
034800     MOVE WS-W001-PROC      TO W001-PROC (13)                             
034900                                                                          
035000     PERFORM CA-SKRIV-SNITTAVVIKELSE                                      
035100                                                                          
035200     MOVE +1                TO IX-DEL                                     
035300     PERFORM UNTIL IX-DEL > 30                                            
035400        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
035500        MOVE ZERO           TO WS-PROC (IX-DEL)                           
035600        ADD +1              TO IX-DEL                                     
035700     END-PERFORM                                                          
035740     MOVE ZERO              TO WS-W001-PROC                               
035800     MOVE IN-IDLEVNR        TO OLD-IDLEVNR                                
035900     MOVE IN-KVDAGAR-TT     TO OLD-KVDAGAR-TT                             
036000     MOVE ZERO              TO WS-SUMVIKT                                 
036100     MOVE ZERO              TO WS-ANTAL                                   
036200     MOVE ZERO              TO OLD-IDARTNR                                
036300     .                                                                    
036400     EJECT                                                                
036500 CA-SKRIV-SNITTAVVIKELSE SECTION.                                         
036600                                                                          
036700     MOVE OLD-IDLEVNR     TO W001-IDLEVNR-SNITT                           
036800     MOVE OLD-KVDAGAR-TT  TO W001-TT IX-TT                                
036900                                                                          
037000     IF WS-ANTAL > ZERO                                                   
037100        COMPUTE WS-SNITT ROUNDED =                                        
037200                                WS-SUMVIKT / WS-ANTAL                     
037300     ELSE                                                                 
037400        MOVE ZERO              TO WS-SNITT                                
037500     END-IF                                                               
037600     MOVE WS-SNITT             TO W001-SNITTAVV                           
037601     MOVE WS-ANTAL             TO W001-ANT                                
037610                                                                          
037620     ADD +1                    TO IX-TT                                   
037630     IF IX-TT > 30                                                        
037640       MOVE ZERO               TO W001-RATT                               
037650                                  W001-SENA                               
037660                                  W001-TIDIGA                             
037670     ELSE                                                                 
037700       MOVE WS-PROC (IX-TT)    TO W001-RATT                               
037710       ADD  WS-SUM  (IX-TT)    TO WS-SUM-RATT                             
037800       MOVE ZERO               TO WS-SENA WS-TIDIGA                       
037900                                                                          
038000       MOVE IX-TT              TO IX-DEL                                  
038010       ADD +1                  TO IX-DEL                                  
038100       PERFORM UNTIL IX-DEL > 30                                          
038200          ADD WS-PROC (IX-DEL) TO WS-SENA                                 
038210          ADD WS-SUM  (IX-DEL) TO WS-SUM-SENA                             
038300          ADD +1               TO IX-DEL                                  
038400       END-PERFORM                                                        
038500       MOVE WS-SENA            TO W001-SENA                               
038600                                                                          
038700       MOVE +1                 TO IX-DEL                                  
038800       PERFORM UNTIL IX-DEL >= IX-TT                                      
038900          ADD WS-PROC (IX-DEL) TO WS-TIDIGA                               
038910          ADD WS-SUM  (IX-DEL) TO WS-SUM-TIDIGA                           
039000          ADD +1               TO IX-DEL                                  
039100       END-PERFORM                                                        
039200       MOVE WS-TIDIGA          TO W001-TIDIGA                             
039210     END-IF                                                               
039220                                                                          
039300     MOVE W001-SNITTA          TO W001-RAD                                
039400     PERFORM S21-SKRIV-W23628-001                                         
039500     .                                                                    
039600     EJECT                                                                
039610 D-SUMMERA-TOT-LEV   SECTION.                                             
039620                                                                          
039630     MOVE ZERO              TO TOT-TOTSUM                                 
039640                                                                          
039650     MOVE +1                TO IX-DEL                                     
039660     PERFORM UNTIL IX-DEL > 30                                            
039670        ADD TOT-SUM (IX-DEL) TO TOT-TOTSUM                                
039680        ADD +1              TO IX-DEL                                     
039690     END-PERFORM                                                          
039691                                                                          
039692     MOVE +1                TO IX-DEL                                     
039693     PERFORM UNTIL IX-DEL > 30                                            
039694        IF TOT-TOTSUM > ZERO                                              
039695           COMPUTE TOT-PROC (IX-DEL) ROUNDED =                            
039696                   100 * TOT-SUM (IX-DEL) / TOT-TOTSUM                    
039697        ELSE                                                              
039698           MOVE ZERO        TO TOT-PROC (IX-DEL)                          
039699        END-IF                                                            
039700        IF IX-DEL > 12                                                    
039701           ADD  TOT-PROC (IX-DEL) TO TOT-W001-PROC                        
039702        ELSE                                                              
039703           MOVE TOT-PROC (IX-DEL) TO W001-PROC-TOT (IX-DEL)               
039704        END-IF                                                            
039705        ADD +1              TO IX-DEL                                     
039706     END-PERFORM                                                          
039707     MOVE TOT-W001-PROC     TO W001-PROC-TOT (13)                         
039708                                                                          
039709     PERFORM DA-SKRIV-SNITTAVVIKELSE                                      
039710                                                                          
039723     .                                                                    
039724     EJECT                                                                
039725 DA-SKRIV-SNITTAVVIKELSE SECTION.                                         
039726                                                                          
039727     MOVE 'TOT'           TO W001-IDLEVNR-TOT                             
039729                                                                          
039730     IF TOT-ANTAL > ZERO                                                  
039731        COMPUTE TOT-SNITT ROUNDED =                                       
039732                                TOT-SUMVIKT / TOT-ANTAL                   
039733     ELSE                                                                 
039734        MOVE ZERO              TO TOT-SNITT                               
039735     END-IF                                                               
039736     MOVE TOT-SNITT            TO W001-SNITTAVV-TOT                       
039737     MOVE TOT-ANTAL            TO W001-ANT-TOT                            
039738                                                                          
039739     COMPUTE WS-TOTSUM-TOT ROUNDED =                                      
039740             WS-SUM-RATT + WS-SUM-TIDIGA + WS-SUM-SENA                    
039750                                                                          
039760     COMPUTE TOT-PROC-RATT     ROUNDED =                                  
039761                   100 * WS-SUM-RATT   / WS-TOTSUM-TOT                    
039762     COMPUTE TOT-PROC-SENA     ROUNDED =                                  
039763                   100 * WS-SUM-SENA   / WS-TOTSUM-TOT                    
039764     COMPUTE TOT-PROC-TIDIGA   ROUNDED =                                  
039765                   100 * WS-SUM-TIDIGA / WS-TOTSUM-TOT                    
039766                                                                          
039767     MOVE TOT-PROC-RATT        TO W001-RATT-TOT                           
039768     MOVE TOT-PROC-SENA        TO W001-SENA-TOT                           
039769     MOVE TOT-PROC-TIDIGA      TO W001-TIDIGA-TOT                         
039770                                                                          
039771     MOVE W001-SNITT-TOT       TO W001-RAD                                
039772     MOVE 50                   TO W001-RADRAKNARE                         
039773                                                                          
039774     PERFORM S21-SKRIV-W23628-001                                         
039775     .                                                                    
039776     EJECT                                                                
039780 Z-FINIT SECTION.                                                         
039800     CLOSE W23626                                                         
039900           W23628-001                                                     
040000     SKIP2                                                                
040100     MOVE 'S' TO POSTSUM-OPKOD                                            
040200     CALL POSTSUM USING POSTSUM-PARM                                      
040300     .                                                                    
040400     EJECT                                                                
040500 S01-LAES-W23626  SECTION.                                                
040600     READ W23626 INTO IN-AREA                                             
040700     AT END                                                               
040800        SET END-OF-W23626 TO TRUE                                         
040900                                                                          
041000     NOT AT END                                                           
041100        MOVE 'W23626'   TO POSTSUM-FDNAMN                                 
041200        MOVE 'W23628D1' TO POSTSUM-DDNAMN2                                
041300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
041400        CALL POSTSUM USING POSTSUM-PARM                                   
041500     END-READ                                                             
041600     .                                                                    
041700     EJECT                                                                
041800 S21-SKRIV-W23628-001  SECTION.                                           
041900                                                                          
042000     MOVE 2 TO W001-SKIP                                                  
042100     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
042200       PERFORM S21A-SKRIV-RUBRIKER                                        
042300     END-IF                                                               
042400     SKIP2                                                                
042500     WRITE W23628-001-RAD FROM W001-RAD AFTER W001-SKIP                   
042600     SKIP2                                                                
042700     MOVE SPACE TO W001-RAD                                               
042800     ADD  +2 TO W001-RADRAKNARE                                           
042900     .                                                                    
043000     EJECT                                                                
043100 S21A-SKRIV-RUBRIKER SECTION.                                             
043200                                                                          
043300     ADD +1 TO W001-SIDRAKNARE                                            
043400     MOVE W001-SIDRAKNARE TO W001-SID                                     
043500     WRITE W23628-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
043600     WRITE W23628-001-RAD FROM W001-RUBRIKA AFTER 2                       
043700     WRITE W23628-001-RAD FROM W001-RUBRIKB AFTER 1                       
043800     MOVE +7 TO W001-RADRAKNARE                                           
043900     SKIP2                                                                
044000     MOVE 3 TO W001-SKIP                                                  
044100     .                                                                    
044200     EJECT                                                                
044300 S99-ABEND SECTION.                                                       
044400                                                                          
044500     SKIP2                                                                
044600     MOVE 'S' TO POSTSUM-OPKOD                                            
044700     CALL POSTSUM USING POSTSUM-PARM                                      
044800     CALL ABEND USING RKOD-ABEND                                          
044900     .                                                                    
