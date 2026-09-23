000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2364100.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LISTA LEVERANSPRECISION   PER ARTIKEL                            
001000*        FÖR INLEVERANSER GÅGNA PERIODEN                                  
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDP3                                       
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
002600*          --- LISTUNDERLAG                                               
002700     SELECT W23636                     ASSIGN TO W23641D1.                
002800     SKIP2                                                                
002900*          --- LISTA LEVERANSPRECISION                                    
003000     SELECT W23641-001                 ASSIGN TO W23641D2.                
003100     SKIP2                                                                
003200*          --- UTFIL LEVERANSPRECISION ARTIKLAR                           
003300     SELECT W23642                     ASSIGN TO W23641D3.                
003400     SKIP2                                                                
003500*          --- SORTERINGSFIL                                              
003600     SELECT SORTFIL                    ASSIGN TO W23635DS.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W23636                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W23636      -L.                                                
004700     SKIP3                                                                
004800 FD  W23641-001                                                           
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200 01  W23641-001-RAD              PIC X(121).                              
005300     SKIP3                                                                
005400 FD  W23642                                                               
005500     RECORDING      F                                                     
005600     BLOCK CONTAINS 0.                                                    
005700     SKIP2                                                                
005800*01  POST -COPY W23641   -L  -PRE UT-                                     
005900     SKIP2                                                                
006000 SD  SORTFIL.                                                             
006100                                                                          
006200*01  POST -COPY W23636      -PRE SORT-                                    
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006600 77  IDPGM                       PIC X(8)    VALUE 'W2364100'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  W23636-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W23636                       VALUE 'J'.                   
007200                                                                          
007300 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007400     88  END-OF-SORTFIL                      VALUE 'J'.                   
007500                                                                          
007600 01  ARBETSAREOR.                                                         
007700     03  IX-DEL                  PIC S9(5)   VALUE ZERO  COMP-3.          
007800     03  OLD-IDLEVNR             PIC X(5)    VALUE SPACE.                 
007900     03  OLD-IDANSK              PIC 9(3)    VALUE ZERO.                  
008000     03  MEM-IDANSK              PIC 9(3)    VALUE 999.                   
008100     03  OLD-IDLOPNRM            PIC 9(9)    VALUE ZERO  COMP-3.          
008200     03  OLD-IDARTNR             PIC 9(9)    VALUE ZERO  COMP-3.          
008300     03  WS-KVDAGAR              PIC S9(3)   VALUE ZERO  COMP-3.          
008400     03  WS-DEL                  PIC 99V999  VALUE ZERO.                  
008500     03  WS-DELPROC              PIC 999V9   VALUE ZERO.                  
008600     03  WS-ANTAL                PIC 9(5)    VALUE ZERO.                  
008700     03  WS-ANTAL-ART            PIC 9(5)    VALUE ZERO.                  
008800     03  WS-TOTSUM               PIC 9(8)V99 VALUE ZERO.                  
008900     03  WS-TOTSUM-ART           PIC 9(8)V99 VALUE ZERO.                  
009000     03  FILLER OCCURS 13.                                                
009100         05  WS-SUM              PIC 9(8)V99 VALUE ZERO.                  
009200     03  FILLER OCCURS 13.                                                
009300         05  WS-SUM-ART          PIC 9(8)V99 VALUE ZERO.                  
009400     03  FILLER OCCURS 13.                                                
009500         05  WS-PROC             PIC 9(8)V99 VALUE ZERO.                  
009600     03  FILLER OCCURS 13.                                                
009700         05  WS-PROC-ART         PIC 9(8)V99 VALUE ZERO.                  
009800     03  WS-RATT                 PIC 999V99  VALUE ZERO.                  
009900     03  WS-RATT-ART             PIC 999V99  VALUE ZERO.                  
010000     03  WS-TIDIGA               PIC 999V99  VALUE ZERO.                  
010100     03  WS-TIDIGA-ART           PIC 999V99  VALUE ZERO.                  
010200     03  WS-SENA                 PIC 999V99  VALUE ZERO.                  
010300     03  WS-SENA-ART             PIC 999V99  VALUE ZERO.                  
010400     03  WS-SNITT                PIC 9(8)V99 VALUE ZERO.                  
010500     03  WS-SNITT-ART            PIC 9(8)V99 VALUE ZERO.                  
010600     03  WS-VIKT                 PIC 9(8)V99 VALUE ZERO.                  
010700     03  WS-SUMVIKT              PIC 9(8)V99 VALUE ZERO.                  
010800     03  WS-SUMVIKT-ART          PIC 9(8)V99 VALUE ZERO.                  
010900     03  FILLER OCCURS 13.                                                
011000         05  W001-DEL-NUM        PIC 999V9   VALUE ZERO.                  
011100     EJECT                                                                
011200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011300 01  FILLER REDEFINES DAGENS-DATUM.                                       
011400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011700     EJECT                                                                
011800 01  DYNAMISKA-SUBPROGRAM.                                                
011900*                                                                         
012000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
012200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     SKIP2                                                                
012700*    --- PARAMETRAR TILL ABEND                                            
012800                                                                          
012900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013200     SKIP2                                                                
013300 01  FELTEXT.                                                             
013400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL DATKORT                                          
013800*                                                                         
013900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23641'.              
014000     SKIP2                                                                
014100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014200     SKIP2                                                                
014300*01  -COPY WDATKORT                                                       
014400     EJECT                                                                
014500*    --- PARAMETRAR TILL POSTSUM                                          
014600*                                                                         
014700*01  -COPY W0005   -PRE  POSTSUM-                                         
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL DATKONV                                          
015000*                                                                         
015100*01  -COPY WDATAREA                                                       
015200     EJECT                                                                
015300 01  IN-AREA-START               PIC X(24)   VALUE                        
015400                                 'IN-AREA-START  '.                       
015500     SKIP2                                                                
015600                                                                          
015700*01  AREA -COPY W23636     -PRE IN-                                       
015800     EJECT                                                                
015900 01  UT-AREA-START               PIC X(24)   VALUE                        
016000                                 'UT-AREA-START  '.                       
016100     SKIP2                                                                
016200 01  UT-AREA.                                                             
016300     03  FILLER                  PIC X(099).                              
016400*01  FILLER -COPY W23641     -PRE UT-     -RED  UT-AREA                   
016500     EJECT                                                                
016600 01  W001-AREA-START             PIC X(24)   VALUE                        
016700                                 'W001-AREA-START  '.                     
016800     SKIP2                                                                
016900 01  W001-HJALPAREOR.                                                     
017000*                                                                         
017100     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
017200     03  W001-MAX-RADER-PER-SIDA                                          
017300                                 PIC 9(3)    VALUE 35.                    
017400     03  W001-LISTNR             PIC X(11)   VALUE 'W23641-001'.          
017500     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
017600     03  W001-RADRAKNARE         PIC S9(3)   COMP-3 VALUE 999.            
017700     EJECT                                                                
017800 01  W001-RAD.                                                            
017900*                                                                         
018000     03  FILLER                  PIC X(121)  VALUE SPACE.                 
018100     EJECT                                                                
018200*01  W001-RUBRIK1.                                                        
018300*                                                                         
018400*    03  FILLER                  PIC X(3)  VALUE SPACE.                   
018500*    03  FILLER                  PIC X(21)                                
018600*                               VALUE 'VOLVO CAR CUST SERV  '.            
018700*    03  FILLER                  PIC X(12)                                
018800*                                VALUE 'W23641-001'.                      
018900*    03  FILLER                  PIC X(07)                                
019000*    VALUE 'PERIOD:'.                                                     
019100*    03  FILLER                  PIC X(47)                                
019200*    VALUE 'DELIVERY PRECISION IN % OF PRE ADVICES / PARTNO'.             
019300*    03  FILLER                  PIC X(2)  VALUE SPACE.                   
019400*    03  FILLER                  PIC X(1)  VALUE 'P'.                     
019500*    03  W001-TIRP               PIC 99    VALUE ZERO.                    
019600*    03  FILLER                  PIC X(3)  VALUE SPACE.                   
019700*    03  W001-DATUM              PIC XXBXXBXX.                            
019800*    03  FILLER                  PIC X(2)   VALUE SPACE.                  
019900*    03  FILLER                  PIC X(5)                                 
020000*                                VALUE 'PAGE '.                           
020100*    03  W001-SID                PIC Z(4)9.                               
020200*    03  FILLER                  PIC X(10) VALUE SPACE.                   
020300     EJECT                                                                
020400 01  W001-RUBRIK1.                                                        
020500*                                                                         
020600     03  FILLER                  PIC X(1)  VALUE SPACE.                   
020700     03  FILLER                  PIC X(12)                                
020800                                VALUE 'VOLVO CCS '.                       
020900     03  FILLER                  PIC X(12)                                
021000                                 VALUE 'W23641-001'.                      
021100     03  FILLER                  PIC X(08)                                
021200                                VALUE 'PERIOD:'.                          
021300     03  FILLER                  PIC X(34)                                
021400     VALUE 'DEL.PREC. IN % OF PRE ADV./ PARTNO'.                          
021500     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021600     03  FILLER                  PIC X(1)  VALUE 'P'.                     
021700     03  W001-TIRP               PIC 99    VALUE ZERO.                    
021800     03  FILLER                  PIC X(3)  VALUE SPACE.                   
021900     03  W001-DATUM              PIC XXBXXBXX.                            
022000     03  FILLER                  PIC X(2)   VALUE SPACE.                  
022100     03  FILLER                  PIC X(5)                                 
022200                                 VALUE 'PAGE '.                           
022300     03  W001-SID                PIC Z(4)9.                               
022400     SKIP2                                                                
022500 01  W001-RUBRIK2.                                                        
022600     03  FILLER                  PIC X(66)                                
022700                             VALUE '   P.PLAN     SUPPL      '.           
022800     03  FILLER                  PIC X(52)                                
022900                           VALUE ' DEVIATION IN DAYS '.                   
023000     03  FILLER                  PIC X(20) VALUE SPACE.                   
023100     SKIP2                                                                
023200 01  W001-RUBRIK3.                                                        
023300     03  FILLER                  PIC X(6)  VALUE SPACE.                   
023400     03  W001-IDANSK             PIC Z(3).                                
023500     03  FILLER                  PIC X(5)  VALUE SPACE.                   
023600     03  W001-IDLEVNR            PIC X(5)  VALUE SPACE.                   
023700     03  FILLER                  PIC X(13)                                
023800                                 VALUE '         '.                       
023900     03  FILLER                  PIC X(68) VALUE SPACE.                   
024000     03  FILLER                  PIC X(20) VALUE SPACE.                   
024100     SKIP2                                                                
024200 01  W001-RUBRIK4.                                                        
024300     03  FILLER                  PIC X(4)  VALUE SPACE.                   
024400     03  FILLER                  PIC X(39)                                
024500     VALUE 'PARTNO  NO AVER. CORR.  LATE   EARLY  '.                      
024600     03  FILLER                  PIC X(41)                                
024700     VALUE ' <5    -5    -4    -3    -2    -1     0 '.                    
024800     03  FILLER                  PIC X(35)                                
024900     VALUE '  +1    +2    +3    +4    +5    >5 '.                         
025000     03  FILLER                  PIC X(20) VALUE SPACE.                   
025100     SKIP2                                                                
025200 01  W001-RUBRIKA.                                                        
025300     03  FILLER                  PIC X(5)  VALUE SPACE.                   
025400     03  FILLER                  PIC X(38)                                
025500     VALUE 'SUPPL  NO AVER. CORR.  LATE   EARLY  '.                       
025600     03  FILLER                  PIC X(41)                                
025700     VALUE ' <5    -5    -4    -3    -2    -1     0 '.                    
025800     03  FILLER                  PIC X(35)                                
025900     VALUE '  +1    +2    +3    +4    +5    >5 '.                         
026000     03  FILLER                  PIC X(20) VALUE SPACE.                   
026100     SKIP2                                                                
026200 01  W001-RUBRIKB.                                                        
026300     03  FILLER                  PIC X(5)  VALUE SPACE.                   
026400     03  FILLER                  PIC X(37)                                
026500     VALUE '         (DAYS)    %     %      %    '.                       
026600     03  FILLER                  PIC X(58)  VALUE SPACE.                  
026700     03  FILLER                  PIC X(20) VALUE SPACE.                   
026800     EJECT                                                                
026900 01  W001-ARTRAD.                                                         
027000     03  FILLER                  PIC X(01)  VALUE SPACE.                  
027100     03  W001-IDARTNR            PIC Z(9).                                
027200     03  FILLER                  PIC X(1)   VALUE SPACE.                  
027300     03  W001-ANTAL-AVI          PIC Z(3)   VALUE ZERO.                   
027400     03  FILLER                  PIC X(1)   VALUE SPACE.                  
027500     03  W001-SNITTAVV-AVI       PIC ZZ9.9  VALUE ZERO.                   
027600     03  FILLER                  PIC X(01)  VALUE SPACE.                  
027700     03  W001-RATT-AVI           PIC ZZ9.9  VALUE ZERO.                   
027800     03  FILLER                  PIC X(01)  VALUE SPACE.                  
027900     03  W001-SENA-AVI           PIC ZZ9.9  VALUE ZERO.                   
028000     03  FILLER                  PIC X(03)  VALUE SPACE.                  
028100     03  W001-TIDIGA-AVI         PIC ZZ9.9  VALUE ZERO.                   
028200     03  FILLER                  PIC X(01)  VALUE SPACE.                  
028300     03  FILLER OCCURS 13.                                                
028400         05  W001-DEL            PIC ZZ9.9 BLANK WHEN ZERO.               
028500         05  FILLER              PIC X(01) VALUE SPACE.                   
028600     SKIP3                                                                
028700 01  W001-SNITTA.                                                         
028800     03  FILLER                  PIC X(5)   VALUE SPACE.                  
028900     03  W001-IDLEVNR-SNITT      PIC X(5)   VALUE SPACE.                  
029000     03  FILLER                  PIC X(1)   VALUE SPACE.                  
029100     03  W001-ANTAL              PIC Z(3)   VALUE ZERO.                   
029200     03  FILLER                  PIC X(1)   VALUE SPACE.                  
029300     03  W001-SNITTAVV           PIC ZZ9.9  VALUE ZERO.                   
029400     03  FILLER                  PIC X(01)  VALUE SPACE.                  
029500     03  W001-RATT               PIC ZZ9.9  VALUE ZERO.                   
029600     03  FILLER                  PIC X(01)  VALUE SPACE.                  
029700     03  W001-SENA               PIC ZZ9.9  VALUE ZERO.                   
029800     03  FILLER                  PIC X(03)  VALUE SPACE.                  
029900     03  W001-TIDIGA             PIC ZZ9.9  VALUE ZERO.                   
030000     03  FILLER                  PIC X(01)  VALUE SPACE.                  
030100     03  FILLER OCCURS 13.                                                
030200         05  W001-PROC           PIC ZZ9.9 BLANK WHEN ZERO.               
030300         05  FILLER              PIC X(01) VALUE SPACE.                   
030400     EJECT                                                                
030500 01  MEM-001.                                                             
030600     03  FILLER                  PIC X(80) VALUE ')SEND '.                
030700 01  MEM-002.                                                             
030800     03  FILLER                  PIC X(80)                                
030900                           VALUE 'TITLE PERIOD LEV.PREC.ART.'.            
031000 01  MEM-003.                                                             
031100     03  FILLER                  PIC X(80) VALUE 'OPTION FORCE'.          
031200 01  MEM-004.                                                             
031300     03  FILLER                  PIC X(05) VALUE 'DEST '.                 
031400     03  MEM-IDMAIL              PIC X(60) VALUE SPACE.                   
031500 01  MEM-005.                                                             
031600     03  FILLER                  PIC X(05) VALUE 'MEMO '.                 
031700 01  MEM-006.                                                             
031800     03  FILLER                  PIC X(05) VALUE ')END '.                 
031900 01  MEM-007.                                                             
032000     03  FILLER                  PIC X(20) VALUE 'LINESIZE 120'.          
032100     EJECT                                                                
032200 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
032300                                  'SORTWS-AREA-START  '.                  
032400     SKIP2                                                                
032500                                                                          
032600*01  AREA -COPY W23636      -PRE SORTWS-                                  
032700 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
032800     EJECT                                                                
032900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033000*                                                                         
033100                                                                          
033200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
033300     SKIP3                                                                
033400 01  NYCKLAR-TILL-DLI.                                                    
033500     03  W-KDARBTYP-X.                                                    
033600         05  W-KDARBTYP          PIC X(08)    VALUE 'ANSK'.               
033700     03  W-IDPERSON-X.                                                    
033800         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
033900     SKIP2                                                                
034000*    --- STATUS-KOD FRÅN IMS                                              
034100 01  STATUS-WS                   PIC XX.                                  
034200     88  SEGMENT-FINNS                       VALUE '  '.                  
034300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
034400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
034500     SKIP2                                                                
034600 01  GODK-STATUSKODER.                                                    
034700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034800     SKIP3                                                                
034900 01  SSA1                        PIC X(64).                               
035000 01  SSA2                        PIC X(64).                               
035100     EJECT                                                                
035200*    --- IMS FUNKTIONSKODER                                               
035300*01  -COPY W0003                                                          
035400     EJECT                                                                
035500*    ---  DLI INPUT-OUTPUT AREA                                           
035600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
035700 01  DLI-IO-WDP311.                                                       
035800*    03  -COPY WDP311                                                     
035900     EJECT                                                                
036000 LINKAGE SECTION.                                                         
036100                                                                          
036200                                                                          
036300*01  -COPY W0008  -PRE WDP3-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600 PROCEDURE DIVISION  USING WDP3-PCB.                                      
036700 MAIN SECTION.                                                            
036800     ENTRY 'DLITCBL' USING WDP3-PCB.                                      
036900                                                                          
037000                                                                          
037100     PERFORM A-INIT                                                       
037200                                                                          
037300     SORT SORTFIL ASCENDING KEY SORT-IDANSK                               
037400                                SORT-IDLEVNR                              
037500                                SORT-IDARTNR                              
037600                                SORT-IDLOPNRM                             
037700                  INPUT  PROCEDURE IN-SORT-INPUT                          
037800                  OUTPUT PROCEDURE UT-SORT-OUTPUT                         
037900                                                                          
038000     IF SORT-RETURN NOT = 0                                               
038100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
038200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
038300       DELIMITED BY SIZE INTO FELTEXT-STR                                 
038400       DISPLAY FELTEXT                                                    
038500       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
038600       PERFORM S99-ABEND                                                  
038700     ELSE                                                                 
038800       PERFORM Z-FINIT                                                    
038900                                                                          
039000       MOVE ZERO TO RETURN-CODE                                           
039100       GOBACK                                                             
039200     END-IF                                                               
039300     .                                                                    
039400     EJECT                                                                
039500 IN-SORT-INPUT SECTION.                                                   
039600                                                                          
039700     PERFORM S01-LAES-W23636                                              
039800     PERFORM UNTIL END-OF-W23636                                          
039900                                                                          
040000       MOVE IN-AREA TO SORTWS-AREA                                        
040100       PERFORM S31-SORT-RELEASE                                           
040200                                                                          
040300       PERFORM S01-LAES-W23636                                            
040400     END-PERFORM                                                          
040500                                                                          
040600     .                                                                    
040700     EJECT                                                                
040800 UT-SORT-OUTPUT SECTION.                                                  
040900                                                                          
041000     PERFORM S32-SORT-RETURN                                              
041100     MOVE SORTWS-IDLEVNR    TO OLD-IDLEVNR                                
041200                               W001-IDLEVNR                               
041300     MOVE SORTWS-IDANSK     TO OLD-IDANSK                                 
041400                               W001-IDANSK                                
041500     MOVE SORTWS-IDARTNR    TO OLD-IDARTNR                                
041600                               W001-IDARTNR                               
041700     MOVE SORTWS-IDLOPNRM   TO OLD-IDLOPNRM                               
041800     MOVE ZERO              TO WS-SUMVIKT WS-SUMVIKT-ART                  
041900     MOVE +1                TO WS-ANTAL   WS-ANTAL-ART                    
042000                                                                          
042100     PERFORM UNTIL END-OF-SORTFIL                                         
042200         IF SORTWS-IDLEVNR NOT = OLD-IDLEVNR OR                           
042300            SORTWS-IDANSK  NOT = OLD-IDANSK                               
042400            PERFORM C-SUMMERA-OLD-LEV                                     
042500            MOVE 50    TO W001-RADRAKNARE                                 
042600         END-IF                                                           
042700                                                                          
042800         PERFORM UNTIL (SORTWS-IDARTNR  NOT = OLD-IDARTNR)  OR            
042900*****              ****(SORTWS-IDLOPNRM NOT = OLD-IDLOPNRM) OR            
043000                       END-OF-SORTFIL                                     
043100           PERFORM B-BERAKNA-RAD-ARTNR                                    
043200           PERFORM S32-SORT-RETURN                                        
043300         END-PERFORM                                                      
043400                                                                          
043500         PERFORM D-SKRIV-RAD                                              
043600                                                                          
043700     END-PERFORM                                                          
043800                                                                          
043900     ADD +1            TO WS-ANTAL                                        
044000     PERFORM C-SUMMERA-OLD-LEV                                            
044100     .                                                                    
044200     EJECT                                                                
044300 A-INIT SECTION.                                                          
044400                                                                          
044500     OPEN INPUT  W23636                                                   
044600                                                                          
044700     OPEN OUTPUT W23641-001                                               
044800                 W23642                                                   
044900     SKIP2                                                                
045000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
045100     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
045200     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
045300     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
045400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
045500                                                                          
045600     MOVE +1                TO IX-DEL                                     
045700     PERFORM UNTIL IX-DEL > 13                                            
045800        MOVE ZERO           TO WS-SUM       (IX-DEL)                      
045900        MOVE ZERO           TO WS-SUM-ART   (IX-DEL)                      
046000        MOVE ZERO           TO WS-PROC      (IX-DEL)                      
046100        MOVE ZERO           TO WS-PROC-ART  (IX-DEL)                      
046200        MOVE ZERO           TO W001-DEL-NUM (IX-DEL)                      
046300        MOVE ZERO           TO W001-DEL     (IX-DEL)                      
046400        ADD +1              TO IX-DEL                                     
046500     END-PERFORM                                                          
046600                                                                          
046700     MOVE DAGENS-DATUM      TO W001-DATUM                                 
046800                                                                          
046900     MOVE DAGENS-DATUM      TO DAT-I-TIDATUM                              
047000     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
047100     CALL WDATKONV USING       DAT-KDDATFORM                              
047200                               DAT-I-TIDATUM                              
047300                               DAT-O-TIDATUM                              
047400                               DAT-KDSVAR                                 
047500     IF DAT-KDSVAR-OK                                                     
047600        MOVE DAT-TIRP       TO W001-TIRP                                  
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000 B-BERAKNA-RAD-ARTNR SECTION.                                             
048100                                                                          
048200     COMPUTE IX-DEL = SORTWS-KVDAGAR + 7                                  
048300     IF IX-DEL < 1                                                        
048400        MOVE 1            TO IX-DEL                                       
048500     END-IF                                                               
048600     IF IX-DEL > 13                                                       
048700        MOVE 13           TO IX-DEL                                       
048800     END-IF                                                               
048900                                                                          
049000     IF SORTWS-KVAVIS > ZERO                                              
049100        COMPUTE WS-DEL  ROUNDED =                                         
049200                        SORTWS-KVAVROP-AVB / SORTWS-KVAVIS                
049300     ELSE                                                                 
049400        MOVE ZERO         TO WS-DEL                                       
049500     END-IF                                                               
049600     ADD  WS-DEL          TO WS-SUM (IX-DEL) WS-SUM-ART (IX-DEL)          
049700     COMPUTE WS-DELPROC = WS-DEL * 100                                    
049800                                                                          
049900     IF SORTWS-IDLOPNRM NOT = OLD-IDLOPNRM                                
050000        ADD  +1           TO WS-ANTAL WS-ANTAL-ART                        
050100     END-IF                                                               
050200     MOVE SORTWS-IDLOPNRM TO OLD-IDLOPNRM                                 
050300     MOVE SORTWS-IDARTNR  TO OLD-IDARTNR                                  
050400                                                                          
050500     PERFORM BA-ADDERA-SNITTBER                                           
050600                                                                          
050700     PERFORM BB-SKRIV-ARTRAD                                              
050800     .                                                                    
050900     EJECT                                                                
051000 BA-ADDERA-SNITTBER  SECTION.                                             
051100                                                                          
051200     MOVE SORTWS-KVDAGAR TO WS-KVDAGAR                                    
051300     IF WS-KVDAGAR < ZERO                                                 
051400        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
051500     END-IF                                                               
051600     COMPUTE WS-VIKT ROUNDED = WS-KVDAGAR * WS-DEL                        
051700     ADD WS-VIKT    TO WS-SUMVIKT WS-SUMVIKT-ART                          
051800     .                                                                    
051900     EJECT                                                                
052000 BB-SKRIV-ARTRAD     SECTION.                                             
052100                                                                          
052200     ADD  WS-DELPROC         TO W001-DEL-NUM (IX-DEL)                     
052300     IF   WS-DELPROC > ZERO AND WS-DELPROC < 0.1                          
052400          ADD  0.1           TO W001-DEL-NUM (IX-DEL)                     
052500     END-IF                                                               
052600                                                                          
052700     MOVE SORTWS-IDARTNR     TO W001-IDARTNR                              
052800     .                                                                    
052900     EJECT                                                                
053000 C-SUMMERA-OLD-LEV   SECTION.                                             
053100                                                                          
053200     MOVE ZERO              TO WS-TOTSUM                                  
053300     SUBTRACT +1          FROM WS-ANTAL                                   
053400                                                                          
053500     MOVE +1                TO IX-DEL                                     
053600     PERFORM UNTIL IX-DEL > 13                                            
053700        ADD WS-SUM (IX-DEL) TO WS-TOTSUM                                  
053800        ADD +1              TO IX-DEL                                     
053900     END-PERFORM                                                          
054000                                                                          
054100     MOVE +1                TO IX-DEL                                     
054200     PERFORM UNTIL IX-DEL > 13                                            
054300        IF WS-TOTSUM > ZERO                                               
054400           COMPUTE WS-PROC (IX-DEL) ROUNDED =                             
054500                   100 * WS-SUM (IX-DEL) / WS-TOTSUM                      
054600        ELSE                                                              
054700           MOVE ZERO        TO WS-PROC (IX-DEL)                           
054800        END-IF                                                            
054900        MOVE WS-PROC (IX-DEL) TO W001-PROC (IX-DEL)                       
055000        ADD +1              TO IX-DEL                                     
055100     END-PERFORM                                                          
055200                                                                          
055300     PERFORM CA-SKRIV-SNITTAVVIKELSE                                      
055400                                                                          
055500     MOVE +1                TO IX-DEL                                     
055600     PERFORM UNTIL IX-DEL > 13                                            
055700        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
055800        MOVE ZERO           TO WS-PROC (IX-DEL)                           
055900        ADD +1              TO IX-DEL                                     
056000     END-PERFORM                                                          
056100     MOVE SORTWS-IDLEVNR    TO OLD-IDLEVNR                                
056200                               W001-IDLEVNR                               
056300     MOVE SORTWS-IDANSK     TO OLD-IDANSK                                 
056400                               W001-IDANSK                                
056500     MOVE SORTWS-IDARTNR    TO OLD-IDARTNR                                
056600                               W001-IDARTNR                               
056700     MOVE SORTWS-IDLOPNRM   TO OLD-IDLOPNRM                               
056800     MOVE ZERO              TO WS-SUMVIKT                                 
056900     MOVE +1                TO WS-ANTAL   WS-ANTAL-ART                    
057000     .                                                                    
057100     EJECT                                                                
057200 CA-SKRIV-SNITTAVVIKELSE SECTION.                                         
057300                                                                          
057400     MOVE OLD-IDLEVNR     TO W001-IDLEVNR-SNITT                           
057500                                                                          
057600     IF WS-ANTAL > ZERO                                                   
057700        COMPUTE WS-SNITT ROUNDED =                                        
057800                                WS-SUMVIKT / WS-ANTAL                     
057900     ELSE                                                                 
058000        MOVE ZERO            TO WS-SNITT                                  
058100     END-IF                                                               
058200     MOVE WS-SNITT           TO W001-SNITTAVV                             
058300     MOVE WS-ANTAL           TO W001-ANTAL                                
058400     MOVE WS-PROC (7)        TO W001-RATT                                 
058500     MOVE ZERO               TO WS-SENA WS-TIDIGA                         
058600                                                                          
058700     MOVE +8                 TO IX-DEL                                    
058800     PERFORM UNTIL IX-DEL > 13                                            
058900        ADD WS-PROC (IX-DEL) TO WS-SENA                                   
059000        ADD +1               TO IX-DEL                                    
059100     END-PERFORM                                                          
059200     MOVE WS-SENA            TO W001-SENA                                 
059300                                                                          
059400     MOVE +1                 TO IX-DEL                                    
059500     PERFORM UNTIL IX-DEL > 6                                             
059600        ADD WS-PROC (IX-DEL) TO WS-TIDIGA                                 
059700        ADD +1               TO IX-DEL                                    
059800     END-PERFORM                                                          
059900     MOVE WS-TIDIGA          TO W001-TIDIGA                               
060000     MOVE W001-SNITTA        TO W001-RAD                                  
060100     PERFORM S21-SKRIV-W23641-001                                         
060200                                                                          
060300     .                                                                    
060400     EJECT                                                                
060500 D-SKRIV-RAD         SECTION.                                             
060600                                                                          
060700     MOVE +1                TO IX-DEL                                     
060800     PERFORM UNTIL IX-DEL > 13                                            
060900      IF WS-ANTAL-ART > ZERO                                              
061000        COMPUTE W001-DEL-NUM (IX-DEL) ROUNDED =                           
061100                W001-DEL-NUM (IX-DEL) / WS-ANTAL-ART                      
061200********MOVE W001-DEL-NUM (IX-DEL)  TO W001-DEL (IX-DEL)                  
061300******ELSE                                                                
061400********MOVE ZERO           TO W001-DEL (IX-DEL)                          
061500      END-IF                                                              
061600        ADD +1              TO IX-DEL                                     
061700     END-PERFORM                                                          
061800     IF WS-ANTAL-ART > ZERO                                               
061900        COMPUTE WS-SNITT-ART ROUNDED =                                    
062000                                WS-SUMVIKT-ART / WS-ANTAL-ART             
062100     ELSE                                                                 
062200        MOVE ZERO           TO WS-SNITT-ART                               
062300     END-IF                                                               
062400     MOVE WS-SNITT-ART      TO W001-SNITTAVV-AVI                          
062500     MOVE WS-ANTAL-ART      TO W001-ANTAL-AVI                             
062600                                                                          
062700     MOVE ZERO              TO WS-TOTSUM-ART                              
062800     MOVE +1                TO IX-DEL                                     
062900     PERFORM UNTIL IX-DEL > 13                                            
063000        ADD WS-SUM-ART (IX-DEL) TO WS-TOTSUM-ART                          
063100        ADD +1              TO IX-DEL                                     
063200     END-PERFORM                                                          
063300                                                                          
063400     MOVE +1                TO IX-DEL                                     
063500     PERFORM UNTIL IX-DEL > 13                                            
063600        IF WS-TOTSUM-ART > ZERO                                           
063700           COMPUTE WS-PROC-ART (IX-DEL) ROUNDED =                         
063800                   100 * WS-SUM-ART (IX-DEL) / WS-TOTSUM-ART              
063900        ELSE                                                              
064000           MOVE ZERO        TO WS-PROC-ART (IX-DEL)                       
064100        END-IF                                                            
064200********MOVE WS-PROC-ART (IX-DEL) TO W001-PROC-ART (IX-DEL)               
064300        MOVE WS-PROC-ART (IX-DEL) TO W001-DEL      (IX-DEL)               
064400        ADD +1              TO IX-DEL                                     
064500     END-PERFORM                                                          
064600                                                                          
064700     MOVE WS-PROC-ART (7)   TO W001-RATT-AVI                              
064800     MOVE ZERO              TO WS-SENA-ART WS-TIDIGA-ART                  
064900                                                                          
065000     MOVE +8                TO IX-DEL                                     
065100     PERFORM UNTIL IX-DEL > 13                                            
065200        ADD WS-PROC-ART (IX-DEL) TO WS-SENA-ART                           
065300        ADD +1              TO IX-DEL                                     
065400     END-PERFORM                                                          
065500     MOVE WS-SENA-ART       TO W001-SENA-AVI                              
065600                                                                          
065700     MOVE +1                TO IX-DEL                                     
065800     PERFORM UNTIL IX-DEL > 6                                             
065900        ADD WS-PROC-ART (IX-DEL) TO WS-TIDIGA-ART                         
066000        ADD +1              TO IX-DEL                                     
066100     END-PERFORM                                                          
066200     MOVE WS-TIDIGA-ART     TO W001-TIDIGA-AVI                            
066300                                                                          
066400     MOVE W001-ARTRAD       TO W001-RAD                                   
066500                                                                          
066600     PERFORM S20-SKRIV-W23641-001                                         
066700                                                                          
066800     MOVE +1                TO IX-DEL                                     
066900     PERFORM UNTIL IX-DEL > 13                                            
067000        MOVE ZERO           TO WS-PROC-ART  (IX-DEL)                      
067100        MOVE ZERO           TO WS-SUM-ART   (IX-DEL)                      
067200        MOVE ZERO           TO W001-DEL     (IX-DEL)                      
067300        MOVE ZERO           TO W001-DEL-NUM (IX-DEL)                      
067400        ADD +1              TO IX-DEL                                     
067500     END-PERFORM                                                          
067600                                                                          
067700     IF SORTWS-IDLOPNRM NOT = OLD-IDLOPNRM                                
067800        ADD  +1             TO WS-ANTAL                                   
067900     END-IF                                                               
068000     MOVE +1                TO WS-ANTAL-ART                               
068100     MOVE ZERO              TO WS-SUMVIKT-ART                             
068200     MOVE SORTWS-IDLOPNRM   TO OLD-IDLOPNRM                               
068300     MOVE SORTWS-IDARTNR    TO OLD-IDARTNR                                
068400     .                                                                    
068500     EJECT                                                                
068600 Z-FINIT SECTION.                                                         
068700                                                                          
068800     MOVE MEM-006  TO UT-AREA                                             
068900     PERFORM S12-SKRIV-UTPOST-W23642                                      
069000                                                                          
069100     CLOSE W23636                                                         
069200           W23641-001                                                     
069300           W23642                                                         
069400     SKIP2                                                                
069500     MOVE 'S' TO POSTSUM-OPKOD                                            
069600     CALL POSTSUM USING POSTSUM-PARM                                      
069700     .                                                                    
069800     EJECT                                                                
069900 S01-LAES-W23636  SECTION.                                                
070000     READ W23636 INTO IN-AREA                                             
070100     AT END                                                               
070200        SET END-OF-W23636 TO TRUE                                         
070300                                                                          
070400     NOT AT END                                                           
070500        MOVE 'W23636'   TO POSTSUM-FDNAMN                                 
070600        MOVE 'W23641D1' TO POSTSUM-DDNAMN2                                
070700        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
070800        CALL POSTSUM USING POSTSUM-PARM                                   
070900     END-READ                                                             
071000     .                                                                    
071100     EJECT                                                                
071200 S12-SKRIV-UTPOST-W23642  SECTION.                                        
071300     IF MEM-IDMAIL NOT = SPACE                                            
071400       WRITE UT-POST FROM UT-AREA                                         
071500       MOVE 'W23642'   TO POSTSUM-FDNAMN                                  
071600       MOVE 'W23662D3' TO POSTSUM-DDNAMN2                                 
071700       MOVE 'UT'       TO POSTSUM-TRANSTYP                                
071800       CALL POSTSUM USING POSTSUM-PARM                                    
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200 S20-SKRIV-W23641-001  SECTION.                                           
072300                                                                          
072400     MOVE 1 TO W001-SKIP                                                  
072500     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
072600       PERFORM S20A-SKRIV-RUBRIKER                                        
072700     END-IF                                                               
072800     SKIP2                                                                
072900     WRITE W23641-001-RAD FROM W001-RAD AFTER W001-SKIP                   
073000*****MOVE W001-RAD (2:040) TO UT-AREA                                     
073100     MOVE W001-RAD (2:020) TO UT-AREA (1:20)                              
073200     MOVE W001-RAD(42:078) TO UT-AREA(21:78)                              
073300     PERFORM S12-SKRIV-UTPOST-W23642                                      
073400     SKIP2                                                                
073500     MOVE SPACE TO W001-RAD                                               
073600     ADD  +1 TO W001-RADRAKNARE                                           
073700     .                                                                    
073800     EJECT                                                                
073900 S20A-SKRIV-RUBRIKER SECTION.                                             
074000                                                                          
074100     ADD +1                     TO W001-SIDRAKNARE                        
074200     MOVE W001-SIDRAKNARE       TO W001-SID                               
074300                                                                          
074400     PERFORM S22A-SKAPA-MAIL-DEST                                         
074500                                                                          
074600     WRITE W23641-001-RAD       FROM W001-RUBRIK1 AFTER PAGE              
074700*****MOVE W001-RUBRIK1 (3:072)  TO UT-AREA                                
074800     MOVE W001-RUBRIK1          TO UT-AREA                                
074900     PERFORM S12-SKRIV-UTPOST-W23642                                      
075000*    MOVE SPACE                 TO UT-AREA                                
075100*    MOVE W001-RUBRIK1 (75:44)  TO UT-AREA (25:44)                        
075200*    PERFORM S12-SKRIV-UTPOST-W23642                                      
075300     WRITE W23641-001-RAD       FROM W001-RUBRIK2 AFTER 2                 
075400*****MOVE W001-RUBRIK2 (2:040)  TO UT-AREA                                
075500     MOVE W001-RUBRIK2 (2:020)  TO UT-AREA (1:20)                         
075600     MOVE W001-RUBRIK2(42:078)  TO UT-AREA(21:78)                         
075700     PERFORM S12-SKRIV-UTPOST-W23642                                      
075800     WRITE W23641-001-RAD       FROM W001-RUBRIK3 AFTER 1                 
075900     MOVE W001-RUBRIK3 (2:040)  TO UT-AREA                                
076000     PERFORM S12-SKRIV-UTPOST-W23642                                      
076100     WRITE W23641-001-RAD       FROM W001-RUBRIK4 AFTER 1                 
076200*****MOVE W001-RUBRIK4 (2:040)  TO UT-AREA                                
076300     MOVE W001-RUBRIK4 (2:020)  TO UT-AREA (1:20)                         
076400     MOVE W001-RUBRIK4(42:078)  TO UT-AREA(21:78)                         
076500     PERFORM S12-SKRIV-UTPOST-W23642                                      
076600     MOVE +7                    TO W001-RADRAKNARE                        
076700     SKIP2                                                                
076800     MOVE 2                     TO W001-SKIP                              
076900     .                                                                    
077000     EJECT                                                                
077100 S21-SKRIV-W23641-001  SECTION.                                           
077200                                                                          
077300     MOVE 2 TO W001-SKIP                                                  
077400     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
077500       PERFORM S21A-SKRIV-RUBRIKER                                        
077600     ELSE                                                                 
077700       PERFORM S21B-SKRIV-RUBRIKER                                        
077800     END-IF                                                               
077900     SKIP2                                                                
078000     WRITE W23641-001-RAD FROM W001-RAD AFTER W001-SKIP                   
078100*****MOVE W001-RAD (2:040) TO UT-AREA                                     
078200     MOVE W001-RAD (2:020) TO UT-AREA (1:20)                              
078300     MOVE W001-RAD(42:078) TO UT-AREA(21:78)                              
078400     PERFORM S12-SKRIV-UTPOST-W23642                                      
078500     SKIP2                                                                
078600     MOVE SPACE           TO W001-RAD                                     
078700     ADD  +2              TO W001-RADRAKNARE                              
078800     .                                                                    
078900     EJECT                                                                
079000 S21S-SKRIV-W23641-001  SECTION.                                          
079100                                                                          
079200     MOVE 2 TO W001-SKIP                                                  
079300     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
079400       PERFORM S21A-SKRIV-RUBRIKER                                        
079500     END-IF                                                               
079600     SKIP2                                                                
079700     WRITE W23641-001-RAD FROM W001-RAD AFTER W001-SKIP                   
079800**** MOVE W001-RAD (2:040) TO UT-AREA                                     
079900     MOVE W001-RAD (2:020) TO UT-AREA (1:20)                              
080000     MOVE W001-RAD(42:078) TO UT-AREA(21:78)                              
080100     PERFORM S12-SKRIV-UTPOST-W23642                                      
080200     SKIP2                                                                
080300     MOVE SPACE           TO W001-RAD                                     
080400     ADD  +2              TO W001-RADRAKNARE                              
080500     .                                                                    
080600     EJECT                                                                
080700 S21A-SKRIV-RUBRIKER SECTION.                                             
080800                                                                          
080900     ADD +1                    TO W001-SIDRAKNARE                         
081000     MOVE W001-SIDRAKNARE      TO W001-SID                                
081100     WRITE W23641-001-RAD      FROM W001-RUBRIK1 AFTER PAGE               
081200*****MOVE W001-RUBRIK1 (3:072) TO UT-AREA                                 
081300     MOVE W001-RUBRIK1         TO UT-AREA                                 
081400     PERFORM S12-SKRIV-UTPOST-W23642                                      
081500*    MOVE SPACE                TO UT-AREA                                 
081600*    MOVE W001-RUBRIK1 (75:44) TO UT-AREA (25:44)                         
081700*    PERFORM S12-SKRIV-UTPOST-W23642                                      
081800     WRITE W23641-001-RAD      FROM W001-RUBRIKA AFTER 2                  
081900*****MOVE W001-RUBRIKA (2:040) TO UT-AREA                                 
082000     MOVE W001-RUBRIKA (2:020) TO UT-AREA (1:20)                          
082100     MOVE W001-RUBRIKA(42:078) TO UT-AREA(21:78)                          
082200     PERFORM S12-SKRIV-UTPOST-W23642                                      
082300     WRITE W23641-001-RAD      FROM W001-RUBRIKB AFTER 1                  
082400*****MOVE W001-RUBRIKB (2:040) TO UT-AREA                                 
082500     MOVE W001-RUBRIKB (2:020) TO UT-AREA (1:20)                          
082600     MOVE W001-RUBRIKB(42:078) TO UT-AREA(21:78)                          
082700     PERFORM S12-SKRIV-UTPOST-W23642                                      
082800     MOVE +7                   TO W001-RADRAKNARE                         
082900     SKIP2                                                                
083000     MOVE 2                    TO W001-SKIP                               
083100     .                                                                    
083200     EJECT                                                                
083300 S21B-SKRIV-RUBRIKER SECTION.                                             
083400                                                                          
083500     WRITE W23641-001-RAD      FROM W001-RUBRIKA AFTER 2                  
083600*****MOVE W001-RUBRIKA (2:040) TO UT-AREA                                 
083700     MOVE W001-RUBRIKA (2:020) TO UT-AREA (1:20)                          
083800     MOVE W001-RUBRIKA(42:078) TO UT-AREA(21:78)                          
083900     PERFORM S12-SKRIV-UTPOST-W23642                                      
084000     WRITE W23641-001-RAD      FROM W001-RUBRIKB AFTER 1                  
084100*****MOVE W001-RUBRIKB (2:040) TO UT-AREA                                 
084200     MOVE W001-RUBRIKB (2:020) TO UT-AREA (1:20)                          
084300     MOVE W001-RUBRIKB(42:078) TO UT-AREA(21:78)                          
084400     PERFORM S12-SKRIV-UTPOST-W23642                                      
084500     ADD  +7                   TO W001-RADRAKNARE                         
084600     SKIP2                                                                
084700     MOVE 2                    TO W001-SKIP                               
084800     .                                                                    
084900     EJECT                                                                
085000 S22A-SKAPA-MAIL-DEST SECTION.                                            
085100                                                                          
085200     IF OLD-IDANSK (1:2) NOT = MEM-IDANSK (1:2)                           
085700        IF MEM-IDANSK NOT = 999                                           
085800           MOVE MEM-006   TO UT-AREA                                      
085900           PERFORM S12-SKRIV-UTPOST-W23642                                
086000        END-IF                                                            
086100        MOVE OLD-IDANSK   TO MEM-IDANSK                                   
086200        PERFORM S30-HAMTA-NAMN                                            
086300        IF MEM-IDMAIL NOT = SPACE                                         
086400          MOVE MEM-001      TO UT-AREA                                    
086500          PERFORM S12-SKRIV-UTPOST-W23642                                 
086600          MOVE MEM-002      TO UT-AREA                                    
086700          PERFORM S12-SKRIV-UTPOST-W23642                                 
086800          MOVE MEM-003      TO UT-AREA                                    
086900          PERFORM S12-SKRIV-UTPOST-W23642                                 
087000          MOVE MEM-007      TO UT-AREA                                    
087100          PERFORM S12-SKRIV-UTPOST-W23642                                 
087200                                                                          
087300                                                                          
087400          MOVE MEM-004      TO UT-AREA                                    
087500          PERFORM S12-SKRIV-UTPOST-W23642                                 
087600          MOVE MEM-005      TO UT-AREA                                    
087700          PERFORM S12-SKRIV-UTPOST-W23642                                 
087800        END-IF                                                            
087900     END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 S31-SORT-RELEASE  SECTION.                                               
088300                                                                          
088400     RELEASE SORT-POST FROM SORTWS-AREA                                   
088500     .                                                                    
088600     EJECT                                                                
088700 S32-SORT-RETURN  SECTION.                                                
088800                                                                          
088900     RETURN SORTFIL INTO SORTWS-AREA                                      
089000     AT END                                                               
089100         SET END-OF-SORTFIL TO TRUE                                       
089200     .                                                                    
089300     EJECT                                                                
089400 S99-ABEND SECTION.                                                       
089500                                                                          
089600     SKIP2                                                                
089700     MOVE 'S' TO POSTSUM-OPKOD                                            
089800     CALL POSTSUM USING POSTSUM-PARM                                      
089900     CALL ABEND USING RKOD-ABEND                                          
090000     .                                                                    
090100     EJECT                                                                
090200 S30-HAMTA-NAMN SECTION.                                                  
090300                                                                          
090400     MOVE OLD-IDANSK            TO W-IDPERSON                             
090500     PERFORM IMS-GET-WDP3-ANSKNAMN                                        
090600     IF SEGMENT-FINNS                                                     
090700        MOVE PERS-IDMAIL                   TO MEM-IDMAIL                  
090800     ELSE                                                                 
090900        MOVE SPACE                         TO MEM-IDMAIL                  
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300* --- IMS SEKTIONER ---                                                   
091400                                                                          
091500 IMS-GET-WDP3-ANSKNAMN SECTION.                                           
091600                                                                          
091700     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
091800          DELIMITED BY SIZE INTO SSA1                                     
091900     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
092000          DELIMITED BY SIZE INTO SSA2                                     
092100     MOVE '  GE' TO GODK-STATUSKODER                                      
092200     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
092300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
092400     PERFORM IMS-STATUSKONTROLL                                           
092500     .                                                                    
092600     EJECT                                                                
092700 IMS-STATUSKONTROLL SECTION.                                              
092800                                                                          
092900     SET STATUS-IX TO 1                                                   
093000     SEARCH GODK-STATUS                                                   
093100       AT END                                                             
093200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
093300           DELIMITED BY SIZE INTO FELTEXT                                 
093400         DISPLAY FELTEXT                                                  
093500         CALL FELLOG                                                      
093600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
093700         CONTINUE                                                         
093800     END-SEARCH                                                           
093900     .                                                                    
