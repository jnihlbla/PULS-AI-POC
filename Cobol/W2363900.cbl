000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2363900.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LISTA INLEVERANSTIDER                                            
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
002400*          --- GÅGNA VECKANS INLEV                                        
002500     SELECT W23633                     ASSIGN TO W23639D1.                
002600     SKIP2                                                                
002700*          --- LISTA INLEVERANSTIDER                                      
002800     SELECT W23639-001                 ASSIGN TO W23639D2.                
002900     SKIP2                                                                
003000*          --- SORTERINGSFIL                                              
003100     SELECT SORTFIL                    ASSIGN TO W23639DS.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W23633                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W23633      -L.                                                
004200     SKIP3                                                                
004300 FD  W23639-001                                                           
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700 01  W23639-001-RAD              PIC X(121).                              
004800     SKIP2                                                                
004900 SD  SORTFIL.                                                             
005000                                                                          
005100*01  POST -COPY W23633      -PRE SORT-                                    
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500 77  IDPGM                       PIC X(8)    VALUE 'W2363900'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900 77  W23633-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W23633                       VALUE 'J'.                   
006100                                                                          
006200 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
006300     88  END-OF-SORTFIL                      VALUE 'J'.                   
006400                                                                          
006500 01  ARBETSAREOR.                                                         
006600     03  IX-DEL                  PIC S9(5)   VALUE ZERO  COMP-3.          
006700     03  IX-JFR                  PIC S9(5)   VALUE ZERO  COMP-3.          
006800     03  OLD-BEFT                PIC S9(3)   VALUE ZERO  COMP-3.          
006900     03  OLD-IDARTNR             PIC 9(9)    VALUE ZERO  COMP-3.          
007000     03  OLD-KVDAGAR-INLEV       PIC S9(3)   VALUE ZERO  COMP-3.          
007100     03  WS-KVDAGAR              PIC S9(3)   VALUE ZERO  COMP-3.          
007200     03  WS-TTILLF               PIC S9(3)   VALUE ZERO.                  
007300     03  WS-ANTAL                PIC 9(5)    VALUE ZERO.                  
007400     03  WS-TOTSUM               PIC 9(8)V99 VALUE ZERO.                  
007500     03  FILLER OCCURS 13.                                                
007600         05  WS-SUM              PIC 9(8)V99 VALUE ZERO.                  
007700     03  FILLER OCCURS 13.                                                
007800         05  WS-PROC             PIC 9(8)V99 VALUE ZERO.                  
007900     03  WS-RATT                 PIC 999V99  VALUE ZERO.                  
008000     03  WS-TIDIGA               PIC 999V99  VALUE ZERO.                  
008100     03  WS-SENA                 PIC 999V99  VALUE ZERO.                  
008200     03  WS-SNITT                PIC 9(8)V99 VALUE ZERO.                  
008300     03  WS-VIKT                 PIC 9(8)V99 VALUE ZERO.                  
008400     03  WS-SUMVIKT              PIC 9(8)V99 VALUE ZERO.                  
008500     03  TOT-ANTAL               PIC 9(6)    VALUE ZERO.                  
008600     03  TOT-TOTSUM              PIC 9(8)V99 VALUE ZERO.                  
008700     03  WS-TOTSUM-TOT           PIC 9(8)V99 VALUE ZERO.                  
008800     03  FILLER OCCURS 13.                                                
008900         05  TOT-SUM             PIC 9(8)V99 VALUE ZERO.                  
009000     03  FILLER OCCURS 13.                                                
009100         05  TOT-PROC            PIC 9(8)V99 VALUE ZERO.                  
009200     03  TOT-PROC-RATT           PIC 9(8)V99 VALUE ZERO.                  
009300     03  TOT-PROC-TIDIGA         PIC 9(8)V99 VALUE ZERO.                  
009400     03  TOT-PROC-SENA           PIC 9(8)V99 VALUE ZERO.                  
009500     03  TOT-SNITT               PIC 9(8)V99 VALUE ZERO.                  
009600     03  TOT-VIKT                PIC 9(8)V99 VALUE ZERO.                  
009700     03  TOT-SUMVIKT             PIC 9(8)V99 VALUE ZERO.                  
009800     03  WS-SUM-RATT             PIC 9(8)V99 VALUE ZERO.                  
009900     03  WS-SUM-TIDIGA           PIC 9(8)V99 VALUE ZERO.                  
010000     03  WS-SUM-SENA             PIC 9(8)V99 VALUE ZERO.                  
010100     03  WS-IDLOPNRM             PIC 9(9).                                
010200     03  FILLER REDEFINES WS-IDLOPNRM.                                    
010300         05  FILLER              PIC 9(1).                                
010400         05  WS-VVD              PIC 9(3).                                
010500         05  FILLER REDEFINES WS-VVD.                                     
010600             07  WS-VV           PIC 9(2).                                
010700             07  FILLER          PIC 9(1).                                
010800         05  WS-LLLL             PIC 9(4).                                
010900         05  FILLER              PIC 9(1).                                
011000     03  WS-TIAAVVD              PIC 9(5).                                
011100     03  FILLER REDEFINES WS-TIAAVVD.                                     
011200         05  WS-TIAA             PIC 9(2).                                
011300         05  WS-TIVVD            PIC 9(3).                                
011400     03  WS-TIAAMMDD             PIC 9(6).                                
011500     EJECT                                                                
011600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011700 01  FILLER REDEFINES DAGENS-DATUM.                                       
011800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012100 01  DAGENS-AA                   PIC 9(2).                                
012200 01  DAGENS-VV                   PIC 9(2).                                
012300 01  FOREG-AA                    PIC 9(2).                                
012400     EJECT                                                                
012500 01  DYNAMISKA-SUBPROGRAM.                                                
012600*                                                                         
012700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
013000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013100     SKIP2                                                                
013200*    --- PARAMETRAR TILL ABEND                                            
013300                                                                          
013400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013700     SKIP2                                                                
013800 01  FELTEXT.                                                             
013900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL POSTSUM                                          
014300*                                                                         
014400*01  -COPY W0005   -PRE  POSTSUM-                                         
014500     EJECT                                                                
014600*01  -COPY WORKAREA                                                       
014700     EJECT                                                                
014800*01  -COPY WDATAREA                                                       
014900     EJECT                                                                
015000 01  IN-AREA-START               PIC X(24)   VALUE                        
015100                                 'IN-AREA-START  '.                       
015200     SKIP2                                                                
015300                                                                          
015400*01  AREA -COPY W23633     -PRE IN-                                       
015500     EJECT                                                                
015600 01  W001-AREA-START             PIC X(24)   VALUE                        
015700                                 'W001-AREA-START  '.                     
015800     SKIP2                                                                
015900 01  W001-HJALPAREOR.                                                     
016000*                                                                         
016100     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
016200     03  W001-MAX-RADER-PER-SIDA                                          
016300                                 PIC 9(3)    VALUE 42.                    
016400     03  W001-LISTNR             PIC X(11)   VALUE 'W23639-001'.          
016500     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
016600     03  W001-RADRAKNARE         PIC S9(3)   COMP-3 VALUE 999.            
016700     EJECT                                                                
016800 01  W001-RAD.                                                            
016900*                                                                         
017000     03  FILLER                  PIC X(121)  VALUE SPACE.                 
017100     EJECT                                                                
017200 01  W001-RUBRIK1.                                                        
017300*                                                                         
017400     03  FILLER                  PIC X(3)  VALUE SPACE.                   
017500     03  FILLER                  PIC X(21)                                
017600                                VALUE 'VOLVO CAR CUST SERV  '.            
017700     03  FILLER                  PIC X(12)                                
017800                                 VALUE 'W23639-001'.                      
017900     03  FILLER                  PIC X(54)                                
018000         VALUE 'DELIVERY PRECISION IN % OF RECIVING DAYS'.                
018100     03  W001-DATUM              PIC XXBXXBXX.                            
018200     03  FILLER                  PIC X(3)   VALUE SPACE.                  
018300     03  FILLER                  PIC X(5)                                 
018400                                 VALUE 'PAGE '.                           
018500     03  W001-SID                PIC Z(4)9.                               
018600     SKIP2                                                                
018700 01  W001-RUBRIKA.                                                        
018800     03  FILLER                  PIC X(1)  VALUE SPACE.                   
018900     03  FILLER                  PIC X(43)                                
019000     VALUE 'PACK.    NO AVER.COMP.CORR.  LATE EARLY'.                     
019100     03  FILLER                  PIC X(41)                                
019200     VALUE '  0     1     2     3     4     5     6 '.                    
019300     03  FILLER                  PIC X(35)                                
019400     VALUE '   7     8     9    10    11   >11 '.                         
019500     SKIP2                                                                
019600 01  W001-RUBRIKB.                                                        
019700     03  FILLER                  PIC X(1)  VALUE SPACE.                   
019800     03  FILLER                  PIC X(44)                                
019900     VALUE ' TYPE                    %     %     %    '.                  
020000     EJECT                                                                
020100 01  W001-SNITTA.                                                         
020200     03  FILLER                  PIC X(1)   VALUE SPACE.                  
020300     03  W001-BEFT-SNITT         PIC Z(3)99 VALUE ZERO.                   
020400     03  FILLER                  PIC X(01)  VALUE SPACE.                  
020500     03  W001-ANT                PIC Z(5)   VALUE ZERO.                   
020600     03  FILLER                  PIC X(1)   VALUE SPACE.                  
020700     03  W001-SNITTAVV           PIC ZZ9.9  VALUE ZERO.                   
020800     03  FILLER                  PIC X(01)  VALUE SPACE.                  
020900     03  W001-JFRV               PIC Z(03)  VALUE SPACE.                  
021000     03  FILLER                  PIC X(01)  VALUE SPACE.                  
021100     03  W001-RATT               PIC ZZ9.9  VALUE ZERO                    
021200                                            BLANK WHEN ZERO.              
021300     03  FILLER                  PIC X(01)  VALUE SPACE.                  
021400     03  W001-SENA               PIC ZZ9.9  VALUE ZERO                    
021500                                            BLANK WHEN ZERO.              
021600     03  FILLER                  PIC X(01)  VALUE SPACE.                  
021700     03  W001-TIDIGA             PIC ZZ9.9  VALUE ZERO                    
021800                                            BLANK WHEN ZERO.              
021900     03  FILLER                  PIC X(02)  VALUE SPACE.                  
022000     03  FILLER OCCURS 13.                                                
022100         05  W001-PROC           PIC ZZ9.9 BLANK WHEN ZERO.               
022200         05  FILLER              PIC X(01) VALUE SPACE.                   
022300     EJECT                                                                
022400 01  W001-SNITT-TOT.                                                      
022500     03  FILLER                  PIC X(1)   VALUE SPACE.                  
022600     03  W001-BEFT-TOT           PIC X(5)   VALUE ZERO.                   
022700     03  W001-ANT-TOT            PIC Z(4)   VALUE ZERO.                   
022800     03  FILLER                  PIC X(1)   VALUE SPACE.                  
022900     03  W001-SNITTAVV-TOT       PIC ZZ9.9  VALUE ZERO.                   
023000     03  FILLER                  PIC X(03)  VALUE SPACE.                  
023100     03  W001-JFRV-TOT           PIC Z(03)  VALUE SPACE.                  
023200     03  FILLER                  PIC X(01)  VALUE SPACE.                  
023300     03  W001-RATT-TOT           PIC ZZ9.9  VALUE ZERO                    
023400                                            BLANK WHEN ZERO.              
023500     03  FILLER                  PIC X(01)  VALUE SPACE.                  
023600     03  W001-SENA-TOT           PIC ZZ9.9  VALUE ZERO                    
023700                                            BLANK WHEN ZERO.              
023800     03  FILLER                  PIC X(01)  VALUE SPACE.                  
023900     03  W001-TIDIGA-TOT         PIC ZZ9.9  VALUE ZERO                    
024000                                            BLANK WHEN ZERO.              
024100     03  FILLER                  PIC X(02)  VALUE SPACE.                  
024200     03  FILLER OCCURS 13.                                                
024300         05  W001-PROC-TOT       PIC ZZ9.9 BLANK WHEN ZERO.               
024400         05  FILLER              PIC X(01) VALUE SPACE.                   
024500     EJECT                                                                
024600 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
024700                                  'SORTWS-AREA-START  '.                  
024800     SKIP2                                                                
024900                                                                          
025000*01  AREA -COPY W23633      -PRE SORTWS-                                  
025100 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
025200     EJECT                                                                
025300 PROCEDURE DIVISION.                                                      
025400 MAIN SECTION.                                                            
025500                                                                          
025600     PERFORM A-INIT                                                       
025700                                                                          
025800     SORT SORTFIL ASCENDING KEY SORT-BEFT                                 
025900                                SORT-IDARTNR                              
026000                  INPUT PROCEDURE B-SORT-INPUT                            
026100                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
026200                                                                          
026300     IF SORT-RETURN NOT = 0                                               
026400       MOVE SORT-RETURN TO SORT-RETURN-X                                  
026500       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
026600       DELIMITED BY SIZE INTO FELTEXT-STR                                 
026700       DISPLAY FELTEXT                                                    
026800       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
026900       PERFORM S99-ABEND                                                  
027000     ELSE                                                                 
027100       PERFORM D-TOT-OUTPUT                                               
027200                                                                          
027300       PERFORM Z-FINIT                                                    
027400                                                                          
027500       MOVE ZERO TO RETURN-CODE                                           
027600       GOBACK                                                             
027700     END-IF                                                               
027800                                                                          
027900     .                                                                    
028000     EJECT                                                                
028100 A-INIT SECTION.                                                          
028200                                                                          
028300     OPEN INPUT  W23633                                                   
028400         OUTPUT  W23639-001                                               
028500                                                                          
028600     ACCEPT DAGENS-DATUM  FROM DATE                                       
028700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028800                                                                          
028900     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
029000     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
029100                          W001-DATUM                                      
029200                                                                          
029300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
029400                     DAT-O-TIDATUM DAT-KDSVAR                             
029500                                                                          
029600     IF DAT-KDSVAR-OK                                                     
029700       MOVE DAT-TIVV  TO DAGENS-VV                                        
029800       MOVE DAT-TIAA  TO DAGENS-AA                                        
029900       SUBTRACT 1 FROM DAGENS-AA GIVING FOREG-AA                          
030000     ELSE                                                                 
030100       DISPLAY ' FEL I DATKONV INIT'                                      
030200       PERFORM S99-ABEND                                                  
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 B-SORT-INPUT  SECTION.                                                   
030700                                                                          
030800     PERFORM S01-LAES-W23633                                              
030900     PERFORM UNTIL END-OF-W23633                                          
031000       MOVE IN-AREA TO SORTWS-AREA                                        
031100       PERFORM S31-SORT-RELEASE                                           
031200       PERFORM S01-LAES-W23633                                            
031300     END-PERFORM                                                          
031400     .                                                                    
031500     EJECT                                                                
031600 C-SORT-OUTPUT SECTION.                                                   
031700     SKIP2                                                                
031800     PERFORM S32-SORT-RETURN                                              
031900     MOVE SORTWS-BEFT          TO OLD-BEFT                                
032000     MOVE SORTWS-KVDAGAR-INLEV TO OLD-KVDAGAR-INLEV                       
032100     PERFORM UNTIL END-OF-SORTFIL                                         
032200       IF SORTWS-BEFT NOT = OLD-BEFT                                      
032300          PERFORM CB-SUMMERA-OLD-BEFT                                     
032400       END-IF                                                             
032500                                                                          
032600       PERFORM CA-BERAKNA-RAD-ARTNR                                       
032700                                                                          
032800       PERFORM S32-SORT-RETURN                                            
032900     END-PERFORM                                                          
033000                                                                          
033100     PERFORM CB-SUMMERA-OLD-BEFT                                          
033200     .                                                                    
033300     EJECT                                                                
033400 CA-BERAKNA-RAD-ARTNR SECTION.                                            
033500                                                                          
033600     PERFORM CAA-FIXA-DATUM                                               
033700                                                                          
033800     COMPUTE IX-DEL = WS-KVDAGAR + 1                                      
033900     IF IX-DEL < 1                                                        
034000        MOVE 1            TO IX-DEL                                       
034100     END-IF                                                               
034200     IF IX-DEL > 13                                                       
034300        MOVE 13           TO IX-DEL                                       
034400     END-IF                                                               
034500     MOVE +1              TO WS-TTILLF                                    
034600     ADD  WS-TTILLF       TO WS-SUM   (IX-DEL)                            
034700     ADD  WS-TTILLF       TO TOT-SUM  (IX-DEL)                            
034800     ADD  +1              TO WS-ANTAL                                     
034900     ADD  +1              TO TOT-ANTAL                                    
035000                                                                          
035100     PERFORM CAB-ADDERA-SNITTBER                                          
035200     .                                                                    
035300     EJECT                                                                
035400 CAA-FIXA-DATUM SECTION.                                                  
035500     SKIP2                                                                
035600     MOVE SORTWS-IDLOPNRM      TO WS-IDLOPNRM                             
035700     MOVE WS-VVD               TO WS-TIVVD                                
035800     IF WS-VV > DAGENS-VV                                                 
035900        MOVE FOREG-AA          TO WS-TIAA                                 
036000     ELSE                                                                 
036100        MOVE DAGENS-AA         TO WS-TIAA                                 
036200     END-IF                                                               
036300     MOVE 'AAVVD'              TO DAT-KDDATFORM                           
036400     MOVE WS-TIAAVVD           TO DAT-I-TIDATUM                           
036500                                                                          
036600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
036700                     DAT-O-TIDATUM DAT-KDSVAR                             
036800                                                                          
036900     IF DAT-KDSVAR-OK                                                     
037000       MOVE DAT-TIAAMMDD       TO WS-TIAAMMDD                             
037100     ELSE                                                                 
037200       DISPLAY 'FEL I DATKONV 1 '                                         
037300       PERFORM S99-ABEND                                                  
037400     END-IF                                                               
037500                                                                          
037600     IF WS-TIAAMMDD = SORTWS-TIUPPDAT                                     
037700        MOVE ZERO              TO WS-KVDAGAR                              
037800     ELSE                                                                 
037900      IF WS-TIAAMMDD > SORTWS-TIUPPDAT                                    
038000        MOVE 001               TO WORK-KDCALL                             
038100        MOVE '11'              TO WORK-IDDC                               
038200        MOVE SORTWS-TIUPPDAT   TO WORK-TIAAMMDD-FOM                       
038300        MOVE WS-TIAAMMDD       TO WORK-TIAAMMDD-TOM                       
038400        CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR         
038500        IF WORK-KDSVAR-FEL                                                
038600           DISPLAY ' FEL I WORKDAY 1 '                                    
038700           PERFORM S99-ABEND                                              
038800        END-IF                                                            
038900        MOVE WORK-KVWORKD      TO WS-KVDAGAR                              
039000        SUBTRACT 1           FROM WS-KVDAGAR                              
039100        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
039200        DISPLAY ' ? ' IN-IDLEVNR ' ' IN-IDARTNR ' '                       
039300              'R32:'  SORTWS-TIUPPDAT  ' LOPNR: ' WS-TIAAMMDD             
039400              ' KVD ' WS-KVDAGAR                                          
039500      ELSE                                                                
039600        MOVE 001               TO WORK-KDCALL                             
039700        MOVE '11'              TO WORK-IDDC                               
039800        MOVE SORTWS-TIUPPDAT   TO WORK-TIAAMMDD-TOM                       
039900        MOVE WS-TIAAMMDD       TO WORK-TIAAMMDD-FOM                       
040000        CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR         
040100        IF WORK-KDSVAR-FEL                                                
040200           DISPLAY ' FEL I WORKDAY 2 '                                    
040300           PERFORM S99-ABEND                                              
040400        END-IF                                                            
040500        MOVE WORK-KVWORKD      TO WS-KVDAGAR                              
040600        SUBTRACT 1 FROM WS-KVDAGAR GIVING WS-KVDAGAR                      
040700      END-IF                                                              
040800     END-IF                                                               
040900                                                                          
041000*****SUBTRACT SORTWS-KVDAGAR-INLEV FROM WS-KVDAGAR                        
041100*****         GIVING WS-KVDAGAR                                           
041200     .                                                                    
041300     EJECT                                                                
041400 CAB-ADDERA-SNITTBER  SECTION.                                            
041500                                                                          
041600     IF WS-KVDAGAR < ZERO                                                 
041700        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
041800     END-IF                                                               
041900     COMPUTE WS-VIKT = WS-KVDAGAR * WS-TTILLF                             
042000     ADD WS-VIKT    TO WS-SUMVIKT                                         
042100     ADD WS-VIKT    TO TOT-SUMVIKT                                        
042200     .                                                                    
042300     EJECT                                                                
042400 CB-SUMMERA-OLD-BEFT   SECTION.                                           
042500                                                                          
042600     MOVE ZERO              TO WS-TOTSUM                                  
042700                                                                          
042800     MOVE +1                TO IX-DEL                                     
042900     PERFORM UNTIL IX-DEL > 13                                            
043000        ADD WS-SUM (IX-DEL) TO WS-TOTSUM                                  
043100        ADD WS-SUM (IX-DEL) TO TOT-TOTSUM                                 
043200        ADD +1              TO IX-DEL                                     
043300     END-PERFORM                                                          
043400                                                                          
043500     MOVE +1                TO IX-DEL                                     
043600     PERFORM UNTIL IX-DEL > 13                                            
043700        IF WS-TOTSUM > ZERO                                               
043800           COMPUTE WS-PROC (IX-DEL) ROUNDED =                             
043900                   100 * WS-SUM (IX-DEL) / WS-TOTSUM                      
044000        ELSE                                                              
044100           MOVE ZERO        TO WS-PROC (IX-DEL)                           
044200        END-IF                                                            
044300        MOVE WS-PROC (IX-DEL) TO W001-PROC (IX-DEL)                       
044400        ADD +1              TO IX-DEL                                     
044500     END-PERFORM                                                          
044600                                                                          
044700                                                                          
044800     PERFORM CBA-SKRIV-SNITTAVVIKELSE                                     
044900                                                                          
045000     MOVE +1                TO IX-DEL                                     
045100     PERFORM UNTIL IX-DEL > 13                                            
045200        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
045300        MOVE ZERO           TO WS-PROC (IX-DEL)                           
045400        ADD +1              TO IX-DEL                                     
045500     END-PERFORM                                                          
045600     MOVE SORTWS-BEFT       TO OLD-BEFT                                   
045700     MOVE SORTWS-KVDAGAR-INLEV TO OLD-KVDAGAR-INLEV                       
045800     MOVE ZERO              TO WS-SUMVIKT                                 
045900     MOVE ZERO              TO WS-ANTAL                                   
046000     MOVE ZERO              TO OLD-IDARTNR                                
046100     .                                                                    
046200     EJECT                                                                
046300 CBA-SKRIV-SNITTAVVIKELSE SECTION.                                        
046400                                                                          
046500     MOVE OLD-BEFT        TO W001-BEFT-SNITT                              
046600     PERFORM CBAA-FIXA-JFR-VARDE                                          
046700                                                                          
046800     IF WS-ANTAL > ZERO                                                   
046900        COMPUTE WS-SNITT ROUNDED =                                        
047000                                WS-SUMVIKT / WS-ANTAL                     
047100     ELSE                                                                 
047200        MOVE ZERO            TO WS-SNITT                                  
047300     END-IF                                                               
047400     MOVE WS-SNITT           TO W001-SNITTAVV                             
047500     MOVE WS-ANTAL           TO W001-ANT                                  
047600     MOVE WS-PROC (IX-JFR)   TO W001-RATT                                 
047700     ADD  WS-SUM  (IX-JFR)   TO WS-SUM-RATT                               
047800     MOVE ZERO               TO WS-SENA WS-TIDIGA                         
047900                                                                          
048000     MOVE IX-JFR             TO IX-DEL                                    
048100     ADD +1                  TO IX-DEL                                    
048200     PERFORM UNTIL IX-DEL > 13                                            
048300        ADD WS-PROC (IX-DEL) TO WS-SENA                                   
048400        ADD WS-SUM  (IX-DEL) TO WS-SUM-SENA                               
048500        ADD +1               TO IX-DEL                                    
048600     END-PERFORM                                                          
048700     MOVE WS-SENA            TO W001-SENA                                 
048800                                                                          
048900     MOVE +1                 TO IX-DEL                                    
049000     PERFORM UNTIL IX-DEL >= IX-JFR                                       
049100        ADD WS-PROC (IX-DEL) TO WS-TIDIGA                                 
049200        ADD WS-SUM  (IX-DEL) TO WS-SUM-TIDIGA                             
049300        ADD +1               TO IX-DEL                                    
049400     END-PERFORM                                                          
049500     MOVE WS-TIDIGA          TO W001-TIDIGA                               
049600                                                                          
049700     MOVE W001-SNITTA        TO W001-RAD                                  
049800     PERFORM S21-SKRIV-W23639-001                                         
049900     .                                                                    
050000     EJECT                                                                
050001                                                                          
050010 CBAA-FIXA-JFR-VARDE      SECTION.                                        
050020                                                                          
050030     SKIP1                                                                
050040     MOVE ZERO           TO W001-JFRV                                     
050050                            IX-JFR                                        
050060*                 NEDAN:  LÅNGSAM MÅLNING                                 
050070     IF  OLD-BEFT  = +44                                                  
050080       MOVE +30          TO W001-JFRV                                     
050090                            IX-JFR                                        
050091*      *-- IX-JFR IS SET TO +13  AT END OF THIS SECTION                   
050092     END-IF                                                               
050093                                                                          
050094     IF  OLD-BEFT  = +57                                                  
050095       MOVE +15          TO W001-JFRV IX-JFR                              
050096*      *-- IX-JFR IS SET TO +13  AT END OF THIS SECTION                   
050097     END-IF                                                               
050098                                                                          
050099     IF  OLD-BEFT  = +46                                                  
050100     OR  OLD-BEFT  = +49                                                  
050101       MOVE +7           TO W001-JFRV IX-JFR                              
050102     END-IF                                                               
050103                                                                          
050104     IF (OLD-BEFT >= +21 AND <= +28)                                      
050105     OR  OLD-BEFT  = +84                                                  
050106     OR  OLD-BEFT  = +85                                                  
050107       MOVE +4           TO W001-JFRV IX-JFR                              
050108     END-IF                                                               
050109                                                                          
050110*                 NEDAN:  MÅLNING = FÖRP.KOD 41-49                        
050111     IF  OLD-BEFT  =  +6                                                  
050112     OR  OLD-BEFT  =  +9                                                  
050113     OR  OLD-BEFT  = +20                                                  
050117     OR  OLD-BEFT  = +38                                                  
050118     OR  OLD-BEFT  = +41                                                  
050119     OR  OLD-BEFT  = +42                                                  
050120     OR  OLD-BEFT  = +43                                                  
050121     OR  OLD-BEFT  = +45                                                  
050122     OR  OLD-BEFT  = +47                                                  
050124     OR  OLD-BEFT  = +66                                                  
050126     OR  OLD-BEFT  = +79                                                  
050127       MOVE +5           TO W001-JFRV IX-JFR                              
050128     END-IF                                                               
050129                                                                          
050130     IF  OLD-BEFT  = +10                                                  
050131     OR (OLD-BEFT >= +12 AND <= +16 )                                     
050132     OR  OLD-BEFT  = +18                                                  
050133     OR  OLD-BEFT  = +19                                                  
050134     OR  OLD-BEFT  = +32                                                  
050135     OR  OLD-BEFT  = +33                                                  
050136     OR  OLD-BEFT  = +34                                                  
050137     OR  OLD-BEFT  = +35                                                  
050138     OR  OLD-BEFT  = +36                                                  
050139     OR  OLD-BEFT  = +37                                                  
050140     OR  OLD-BEFT  = +39                                                  
050141     OR (OLD-BEFT >= +50 AND <= +54 )                                     
050142     OR  OLD-BEFT  = +56                                                  
050143     OR (OLD-BEFT >= +58 AND <= +62 )                                     
050144     OR  OLD-BEFT  = +63                                                  
050145     OR  OLD-BEFT  = +64                                                  
050146     OR  OLD-BEFT  = +65                                                  
050147     OR  OLD-BEFT  = +67                                                  
050148     OR  OLD-BEFT  = +68                                                  
050149     OR  OLD-BEFT  = +69                                                  
050150     OR  OLD-BEFT  = +81                                                  
050151     OR  OLD-BEFT  = +82                                                  
050152     OR  OLD-BEFT  = +83                                                  
050153     OR (OLD-BEFT >= +86 AND <= +89 )                                     
050154       MOVE +4           TO W001-JFRV IX-JFR                              
050155     END-IF                                                               
050156                                                                          
050157     IF  OLD-BEFT  =  +7                                                  
050158     OR  OLD-BEFT  =  +8                                                  
050159     OR  OLD-BEFT  = +48                                                  
050160     OR  OLD-BEFT  = +55                                                  
050161     OR  OLD-BEFT  = +74                                                  
050162     OR  OLD-BEFT  = +77                                                  
050163     OR  OLD-BEFT  = +78                                                  
050164     OR  OLD-BEFT  = +97                                                  
050165     OR  OLD-BEFT  = +98                                                  
050166       MOVE +3           TO W001-JFRV IX-JFR                              
050167     END-IF                                                               
050168                                                                          
050169     IF  OLD-BEFT  = +29                                                  
050170     OR  OLD-BEFT  = +30                                                  
050171     OR  OLD-BEFT  = +31                                                  
050172     OR  OLD-BEFT  = +73                                                  
050173     OR  OLD-BEFT  = +90                                                  
050174     OR  OLD-BEFT  = +91                                                  
050175       MOVE +2           TO W001-JFRV IX-JFR                              
050176     END-IF                                                               
050177                                                                          
050178     IF  OLD-BEFT <=  +5                                                  
050179     OR  OLD-BEFT  = +11                                                  
050180     OR  OLD-BEFT  = +17                                                  
050182     OR (OLD-BEFT >= +70 AND <= +72 )                                     
050183     OR  OLD-BEFT  = +80                                                  
050184     OR  OLD-BEFT  = +92                                                  
050185     OR  OLD-BEFT  = +94                                                  
050186     OR  OLD-BEFT  = +96                                                  
050187     OR  OLD-BEFT  = +99                                                  
050188       MOVE +1           TO W001-JFRV IX-JFR                              
050189     END-IF                                                               
050190                                                                          
050191     IF  OLD-BEFT  = +75                                                  
050192     OR  OLD-BEFT  = +76                                                  
050193     OR  OLD-BEFT  = +93                                                  
050194     OR  OLD-BEFT  = +95                                                  
050195        CONTINUE                                                          
050196*       -- FÅR INITIERINGSVÄRDET = ZERO                                   
050197     END-IF                                                               
050198                                                                          
050199     ADD +1 TO IX-JFR                                                     
050200     IF IX-JFR > + 13                                                     
050201       MOVE +13            TO IX-JFR                                      
050202     END-IF                                                               
050203     .                                                                    
050210     EJECT                                                                
050300                                                                          
059000 D-TOT-OUTPUT SECTION.                                                    
059100                                                                          
059200     MOVE ZERO              TO TOT-TOTSUM                                 
059300                                                                          
059400     MOVE +1                TO IX-DEL                                     
059500     PERFORM UNTIL IX-DEL > 13                                            
059600        ADD TOT-SUM (IX-DEL) TO TOT-TOTSUM                                
059700        ADD +1              TO IX-DEL                                     
059800     END-PERFORM                                                          
059900                                                                          
060000     MOVE +1                TO IX-DEL                                     
060100     PERFORM UNTIL IX-DEL > 13                                            
060200        IF TOT-TOTSUM > ZERO                                              
060300           COMPUTE TOT-PROC (IX-DEL) ROUNDED =                            
060400                   100 * TOT-SUM (IX-DEL) / TOT-TOTSUM                    
060500        ELSE                                                              
060600           MOVE ZERO        TO TOT-PROC (IX-DEL)                          
060700        END-IF                                                            
060800        MOVE TOT-PROC (IX-DEL) TO W001-PROC-TOT (IX-DEL)                  
060900        ADD +1              TO IX-DEL                                     
061000     END-PERFORM                                                          
061100                                                                          
061200                                                                          
061300     PERFORM DA-SKRIV-SNITTAVVIKELSE                                      
061400                                                                          
061500     .                                                                    
061600     EJECT                                                                
061610                                                                          
061700 DA-SKRIV-SNITTAVVIKELSE SECTION.                                         
061800                                                                          
061900     MOVE 'TOTAL'         TO W001-BEFT-TOT                                
062000                                                                          
062100     IF TOT-ANTAL > ZERO                                                  
062200        COMPUTE TOT-SNITT ROUNDED =                                       
062300                                TOT-SUMVIKT / TOT-ANTAL                   
062400     ELSE                                                                 
062500        MOVE ZERO            TO TOT-SNITT                                 
062600     END-IF                                                               
062700     MOVE TOT-SNITT          TO W001-SNITTAVV-TOT                         
062800     MOVE TOT-ANTAL          TO W001-ANT-TOT                              
062900                                                                          
063000     COMPUTE WS-TOTSUM-TOT ROUNDED =                                      
063100             WS-SUM-RATT + WS-SUM-TIDIGA + WS-SUM-SENA                    
063200                                                                          
063300     COMPUTE TOT-PROC-RATT     ROUNDED =                                  
063400                   100 * WS-SUM-RATT   / WS-TOTSUM-TOT                    
063500     COMPUTE TOT-PROC-SENA     ROUNDED =                                  
063600                   100 * WS-SUM-SENA   / WS-TOTSUM-TOT                    
063700     COMPUTE TOT-PROC-TIDIGA   ROUNDED =                                  
063800                   100 * WS-SUM-TIDIGA / WS-TOTSUM-TOT                    
063900                                                                          
064000     MOVE TOT-PROC-RATT        TO W001-RATT-TOT                           
064100     MOVE TOT-PROC-SENA        TO W001-SENA-TOT                           
064200     MOVE TOT-PROC-TIDIGA      TO W001-TIDIGA-TOT                         
064300                                                                          
064400     MOVE W001-SNITT-TOT     TO W001-RAD                                  
064500     MOVE 50                 TO W001-RADRAKNARE                           
064600                                                                          
064700     PERFORM S21-SKRIV-W23639-001                                         
064800     .                                                                    
064900     EJECT                                                                
065000 Z-FINIT SECTION.                                                         
065100                                                                          
065200     CLOSE W23633                                                         
065300           W23639-001                                                     
065400     SKIP2                                                                
065500     MOVE 'S' TO POSTSUM-OPKOD                                            
065600     CALL POSTSUM USING POSTSUM-PARM                                      
065700     .                                                                    
065800     EJECT                                                                
065900 S01-LAES-W23633  SECTION.                                                
066000     READ W23633 INTO IN-AREA                                             
066100     AT END                                                               
066200        SET END-OF-W23633 TO TRUE                                         
066300                                                                          
066400     NOT AT END                                                           
066500        MOVE 'W23633'   TO POSTSUM-FDNAMN                                 
066600        MOVE 'W23639D1' TO POSTSUM-DDNAMN2                                
066700        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
066800        CALL POSTSUM USING POSTSUM-PARM                                   
066900     END-READ                                                             
067000     .                                                                    
067100     EJECT                                                                
067200 S21-SKRIV-W23639-001  SECTION.                                           
067300                                                                          
067400     MOVE 2 TO W001-SKIP                                                  
067500     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
067600       PERFORM S21A-SKRIV-RUBRIKER                                        
067700     END-IF                                                               
067800     SKIP2                                                                
067900     WRITE W23639-001-RAD FROM W001-RAD AFTER W001-SKIP                   
068000     SKIP2                                                                
068100     MOVE SPACE TO W001-RAD                                               
068200     ADD  +2 TO W001-RADRAKNARE                                           
068300     .                                                                    
068400     EJECT                                                                
068500 S21A-SKRIV-RUBRIKER SECTION.                                             
068600                                                                          
068700     ADD +1 TO W001-SIDRAKNARE                                            
068800     MOVE W001-SIDRAKNARE TO W001-SID                                     
068900     WRITE W23639-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
069000     WRITE W23639-001-RAD FROM W001-RUBRIKA AFTER 2                       
069100     WRITE W23639-001-RAD FROM W001-RUBRIKB AFTER 1                       
069200     MOVE +7 TO W001-RADRAKNARE                                           
069300     SKIP2                                                                
069400     MOVE 3 TO W001-SKIP                                                  
069500     .                                                                    
069600     EJECT                                                                
069700 S31-SORT-RELEASE  SECTION.                                               
069800                                                                          
069900     RELEASE SORT-POST FROM SORTWS-AREA                                   
070000     .                                                                    
070100     EJECT                                                                
070200 S32-SORT-RETURN  SECTION.                                                
070300                                                                          
070400     RETURN SORTFIL INTO SORTWS-AREA                                      
070500     AT END                                                               
070600         SET END-OF-SORTFIL TO TRUE                                       
070700     .                                                                    
070800     EJECT                                                                
070900 S99-ABEND SECTION.                                                       
071000                                                                          
071100     SKIP2                                                                
071200     MOVE 'S' TO POSTSUM-OPKOD                                            
071300     CALL POSTSUM USING POSTSUM-PARM                                      
071400     CALL ABEND USING RKOD-ABEND                                          
071500     .                                                                    
