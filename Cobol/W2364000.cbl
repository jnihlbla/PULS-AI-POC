000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2364000.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LISTA LEVERANSPRECISION   PER ARTIKEL                            
001000*        FÖR INLEVERANSER GÅGNA VECKAN                                    
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
002700     SELECT W23636                     ASSIGN TO W23640D1.                
002800     SKIP2                                                                
002900*          --- LISTA LEVERANSPRECISION                                    
003000     SELECT W23640-001                 ASSIGN TO W23640D2.                
003100     SKIP2                                                                
003200*          --- UTFIL LEVERANSPRECISION ARTIKLAR                           
003300     SELECT W23641                     ASSIGN TO W23640D3.                
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
004800 FD  W23640-001                                                           
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200 01  W23640-001-RAD              PIC X(121).                              
005300     SKIP3                                                                
005400 FD  W23641                                                               
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
006600 77  IDPGM                       PIC X(8)    VALUE 'W2364000'.            
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
012200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012500     SKIP2                                                                
012600*    --- PARAMETRAR TILL ABEND                                            
012700                                                                          
012800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013100     SKIP2                                                                
013200 01  FELTEXT.                                                             
013300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL DATKORT                                          
013700*                                                                         
013800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23640'.              
013900     SKIP2                                                                
014000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014100     SKIP2                                                                
014200*01  -COPY WDATKORT                                                       
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL POSTSUM                                          
014500*                                                                         
014600*01  -COPY W0005   -PRE  POSTSUM-                                         
014700     EJECT                                                                
014800 01  IN-AREA-START               PIC X(24)   VALUE                        
014900                                 'IN-AREA-START  '.                       
015000     SKIP2                                                                
015100                                                                          
015200*01  AREA -COPY W23636     -PRE IN-                                       
015300     EJECT                                                                
015400 01  UT-AREA-START               PIC X(24)   VALUE                        
015500                                 'UT-AREA-START  '.                       
015600     SKIP2                                                                
015700 01  UT-AREA.                                                             
015800     03  FILLER                  PIC X(099).                              
015900*01  FILLER -COPY W23641     -PRE UT-     -RED  UT-AREA                   
016000     EJECT                                                                
016100 01  W001-AREA-START             PIC X(24)   VALUE                        
016200                                 'W001-AREA-START  '.                     
016300     SKIP2                                                                
016400 01  W001-HJALPAREOR.                                                     
016500*                                                                         
016600     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
016700     03  W001-MAX-RADER-PER-SIDA                                          
016800                                 PIC 9(3)    VALUE 35.                    
016900     03  W001-LISTNR             PIC X(11)   VALUE 'W23640-001'.          
017000     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
017100     03  W001-RADRAKNARE         PIC S9(3)   COMP-3 VALUE 999.            
017200     EJECT                                                                
017300 01  W001-RAD.                                                            
017400*                                                                         
017500     03  FILLER                  PIC X(121)  VALUE SPACE.                 
017600     EJECT                                                                
017700*01  W001-RUBRIK1.                                                        
017800*                                                                         
017900*    03  FILLER                  PIC X(3)  VALUE SPACE.                   
018000*    03  FILLER                  PIC X(21)                                
018100*                               VALUE 'VOLVO CAR CUST SERV  '.            
018200*    03  FILLER                  PIC X(12)                                
018300*                                VALUE 'W23640-001'.                      
018400*    03  FILLER                  PIC X(47)                                
018500*    VALUE 'DELIVERY PRECISION IN % OF PRE ADVICES / PARTNO'.             
018600*    03  FILLER                  PIC X(5)   VALUE '    W'.                
018700*    03  W001-VECKA              PIC 99     VALUE ZERO.                   
018800*    03  FILLER                  PIC X(5)   VALUE SPACE.                  
018900*    03  W001-DATUM              PIC XXBXXBXX.                            
019000*    03  FILLER                  PIC X(3)   VALUE SPACE.                  
019100*    03  FILLER                  PIC X(5)                                 
019200*                                VALUE 'PAGE '.                           
019300*    03  W001-SID                PIC Z(4)9.                               
019400*    03  FILLER                  PIC X(15) VALUE SPACE.                   
019500     EJECT                                                                
019600 01  W001-RUBRIK1.                                                        
019700*                                                                         
019800     03  FILLER                  PIC X(1)  VALUE SPACE.                   
019900     03  FILLER                  PIC X(11)                                
020000                                VALUE 'VOLVO CCS  '.                      
020100     03  FILLER                  PIC X(12)                                
020200                                 VALUE 'W23640-001'.                      
020300     03  FILLER                  PIC X(33)                                
020400     VALUE 'DEL.PREC. IN % OF PRE ADV./PARTNO'.                           
020500     03  FILLER                  PIC X(4)   VALUE '   W'.                 
020600     03  W001-VECKA              PIC 99     VALUE ZERO.                   
020700     03  FILLER                  PIC X(4)   VALUE SPACE.                  
020800     03  W001-DATUM              PIC XXBXXBXX.                            
020900     03  FILLER                  PIC X(3)   VALUE SPACE.                  
021000     03  FILLER                  PIC X(5)                                 
021100                                 VALUE 'PAGE '.                           
021200     03  W001-SID                PIC Z(4)9.                               
021300     SKIP2                                                                
021400 01  W001-RUBRIK2.                                                        
021500     03  FILLER                  PIC X(66)                                
021600                             VALUE '   P.PLAN     SUPPL      '.           
021700     03  FILLER                  PIC X(52)                                
021800                           VALUE ' DEVIATION IN DAYS '.                   
021900     03  FILLER                  PIC X(20) VALUE SPACE.                   
022000     SKIP2                                                                
022100 01  W001-RUBRIK3.                                                        
022200     03  FILLER                  PIC X(6)  VALUE SPACE.                   
022300     03  W001-IDANSK             PIC Z(3).                                
022400     03  FILLER                  PIC X(5)  VALUE SPACE.                   
022500     03  W001-IDLEVNR            PIC X(5)  VALUE SPACE.                   
022600     03  FILLER                  PIC X(13)                                
022700                                 VALUE '         '.                       
022800     03  FILLER                  PIC X(68) VALUE SPACE.                   
022900     03  FILLER                  PIC X(20) VALUE SPACE.                   
023000     SKIP2                                                                
023100 01  W001-RUBRIK4.                                                        
023200     03  FILLER                  PIC X(4)  VALUE SPACE.                   
023300     03  FILLER                  PIC X(39)                                
023400     VALUE 'PARTNO  NO AVER. CORR.  LATE   EARLY  '.                      
023500     03  FILLER                  PIC X(41)                                
023600     VALUE ' <5    -5    -4    -3    -2    -1     0 '.                    
023700     03  FILLER                  PIC X(35)                                
023800     VALUE '  +1    +2    +3    +4    +5    >5 '.                         
023900     03  FILLER                  PIC X(20) VALUE SPACE.                   
024000     SKIP2                                                                
024100 01  W001-RUBRIKA.                                                        
024200     03  FILLER                  PIC X(5)  VALUE SPACE.                   
024300     03  FILLER                  PIC X(38)                                
024400     VALUE 'SUPPL  NO AVER. CORR.  LATE   EARLY  '.                       
024500     03  FILLER                  PIC X(41)                                
024600     VALUE ' <5    -5    -4    -3    -2    -1     0 '.                    
024700     03  FILLER                  PIC X(35)                                
024800     VALUE '  +1    +2    +3    +4    +5    >5 '.                         
024900     03  FILLER                  PIC X(20) VALUE SPACE.                   
025000     SKIP2                                                                
025100 01  W001-RUBRIKB.                                                        
025200     03  FILLER                  PIC X(5)  VALUE SPACE.                   
025300     03  FILLER                  PIC X(37)                                
025400     VALUE '         (DAYS)    %     %      %    '.                       
025500     03  FILLER                  PIC X(58)  VALUE SPACE.                  
025600     03  FILLER                  PIC X(20) VALUE SPACE.                   
025700     EJECT                                                                
025800 01  W001-ARTRAD.                                                         
025900     03  FILLER                  PIC X(01)  VALUE SPACE.                  
026000     03  W001-IDARTNR            PIC Z(9).                                
026100     03  FILLER                  PIC X(1)   VALUE SPACE.                  
026200     03  W001-ANTAL-AVI          PIC Z(3)   VALUE ZERO.                   
026300     03  FILLER                  PIC X(1)   VALUE SPACE.                  
026400     03  W001-SNITTAVV-AVI       PIC ZZ9.9  VALUE ZERO.                   
026500     03  FILLER                  PIC X(01)  VALUE SPACE.                  
026600     03  W001-RATT-AVI           PIC ZZ9.9  VALUE ZERO.                   
026700     03  FILLER                  PIC X(01)  VALUE SPACE.                  
026800     03  W001-SENA-AVI           PIC ZZ9.9  VALUE ZERO.                   
026900     03  FILLER                  PIC X(03)  VALUE SPACE.                  
027000     03  W001-TIDIGA-AVI         PIC ZZ9.9  VALUE ZERO.                   
027100     03  FILLER                  PIC X(01)  VALUE SPACE.                  
027200     03  FILLER OCCURS 13.                                                
027300         05  W001-DEL            PIC ZZ9.9 BLANK WHEN ZERO.               
027400         05  FILLER              PIC X(01) VALUE SPACE.                   
027500     SKIP3                                                                
027600 01  W001-SNITTA.                                                         
027700     03  FILLER                  PIC X(5)   VALUE SPACE.                  
027800     03  W001-IDLEVNR-SNITT      PIC X(5)   VALUE SPACE.                  
027900     03  FILLER                  PIC X(1)   VALUE SPACE.                  
028000     03  W001-ANTAL              PIC Z(3)   VALUE ZERO.                   
028100     03  FILLER                  PIC X(1)   VALUE SPACE.                  
028200     03  W001-SNITTAVV           PIC ZZ9.9  VALUE ZERO.                   
028300     03  FILLER                  PIC X(01)  VALUE SPACE.                  
028400     03  W001-RATT               PIC ZZ9.9  VALUE ZERO.                   
028500     03  FILLER                  PIC X(01)  VALUE SPACE.                  
028600     03  W001-SENA               PIC ZZ9.9  VALUE ZERO.                   
028700     03  FILLER                  PIC X(03)  VALUE SPACE.                  
028800     03  W001-TIDIGA             PIC ZZ9.9  VALUE ZERO.                   
028900     03  FILLER                  PIC X(01)  VALUE SPACE.                  
029000     03  FILLER OCCURS 13.                                                
029100         05  W001-PROC           PIC ZZ9.9 BLANK WHEN ZERO.               
029200         05  FILLER              PIC X(01) VALUE SPACE.                   
029300     EJECT                                                                
029400 01  MEM-001.                                                             
029500     03  FILLER                  PIC X(80) VALUE ')SEND '.                
029600 01  MEM-002.                                                             
029700     03  FILLER                  PIC X(80)                                
029800                           VALUE 'TITLE        LEV.PREC.ART.'.            
029900 01  MEM-003.                                                             
030000     03  FILLER                  PIC X(80) VALUE 'OPTION FORCE'.          
030100 01  MEM-004.                                                             
030200     03  FILLER                  PIC X(05) VALUE 'DEST '.                 
030300     03  MEM-IDMAIL              PIC X(60) VALUE SPACE.                   
030400 01  MEM-005.                                                             
030500     03  FILLER                  PIC X(05) VALUE 'MEMO '.                 
030600 01  MEM-006.                                                             
030700     03  FILLER                  PIC X(05) VALUE ')END '.                 
030800 01  MEM-007.                                                             
030900     03  FILLER                  PIC X(20) VALUE 'LINESIZE 120'.          
031000     EJECT                                                                
031100 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
031200                                  'SORTWS-AREA-START  '.                  
031300     SKIP2                                                                
031400                                                                          
031500*01  AREA -COPY W23636      -PRE SORTWS-                                  
031600 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
031700     EJECT                                                                
031800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031900*                                                                         
032000                                                                          
032100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032200     SKIP3                                                                
032300 01  NYCKLAR-TILL-DLI.                                                    
032400     03  W-KDARBTYP-X.                                                    
032500         05  W-KDARBTYP          PIC X(08)    VALUE 'ANSK'.               
032600     03  W-IDPERSON-X.                                                    
032700         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
032800     SKIP2                                                                
032900*    --- STATUS-KOD FRÅN IMS                                              
033000 01  STATUS-WS                   PIC XX.                                  
033100     88  SEGMENT-FINNS                       VALUE '  '.                  
033200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033400     SKIP2                                                                
033500 01  GODK-STATUSKODER.                                                    
033600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033700     SKIP3                                                                
033800 01  SSA1                        PIC X(64).                               
033900 01  SSA2                        PIC X(64).                               
034000     EJECT                                                                
034100*    --- IMS FUNKTIONSKODER                                               
034200*01  -COPY W0003                                                          
034300     EJECT                                                                
034400*    ---  DLI INPUT-OUTPUT AREA                                           
034500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
034600 01  DLI-IO-WDP311.                                                       
034700*    03  -COPY WDP311                                                     
034800     EJECT                                                                
034900 LINKAGE SECTION.                                                         
035000                                                                          
035100                                                                          
035200*01  -COPY W0008  -PRE WDP3-                                              
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500 PROCEDURE DIVISION  USING WDP3-PCB.                                      
035600 MAIN SECTION.                                                            
035700     ENTRY 'DLITCBL' USING WDP3-PCB.                                      
035800                                                                          
035900                                                                          
036000     PERFORM A-INIT                                                       
036100                                                                          
036200     SORT SORTFIL ASCENDING KEY SORT-IDANSK                               
036300                                SORT-IDLEVNR                              
036400                                SORT-IDARTNR                              
036500                                SORT-IDLOPNRM                             
036600                  INPUT  PROCEDURE IN-SORT-INPUT                          
036700                  OUTPUT PROCEDURE UT-SORT-OUTPUT                         
036800                                                                          
036900     IF SORT-RETURN NOT = 0                                               
037000       MOVE SORT-RETURN TO SORT-RETURN-X                                  
037100       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
037200       DELIMITED BY SIZE INTO FELTEXT-STR                                 
037300       DISPLAY FELTEXT                                                    
037400       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
037500       PERFORM S99-ABEND                                                  
037600     ELSE                                                                 
037700       PERFORM Z-FINIT                                                    
037800                                                                          
037900       MOVE ZERO TO RETURN-CODE                                           
038000       GOBACK                                                             
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 IN-SORT-INPUT SECTION.                                                   
038500                                                                          
038600     PERFORM S01-LAES-W23636                                              
038700     PERFORM UNTIL END-OF-W23636                                          
038800                                                                          
038900       MOVE IN-AREA TO SORTWS-AREA                                        
039000       PERFORM S31-SORT-RELEASE                                           
039100                                                                          
039200       PERFORM S01-LAES-W23636                                            
039300     END-PERFORM                                                          
039400                                                                          
039500     .                                                                    
039600     EJECT                                                                
039700 UT-SORT-OUTPUT SECTION.                                                  
039800                                                                          
039900     PERFORM S32-SORT-RETURN                                              
040000     MOVE SORTWS-IDLEVNR    TO OLD-IDLEVNR                                
040100                               W001-IDLEVNR                               
040200     MOVE SORTWS-IDANSK     TO OLD-IDANSK                                 
040300                               W001-IDANSK                                
040400     MOVE SORTWS-IDARTNR    TO OLD-IDARTNR                                
040500                               W001-IDARTNR                               
040600     MOVE SORTWS-IDLOPNRM   TO OLD-IDLOPNRM                               
040700     MOVE ZERO              TO WS-SUMVIKT WS-SUMVIKT-ART                  
040800     MOVE +1                TO WS-ANTAL   WS-ANTAL-ART                    
040900                                                                          
041000     PERFORM UNTIL END-OF-SORTFIL                                         
041100         IF SORTWS-IDLEVNR NOT = OLD-IDLEVNR OR                           
041200            SORTWS-IDANSK  NOT = OLD-IDANSK                               
041300            PERFORM C-SUMMERA-OLD-LEV                                     
041400            MOVE 50    TO W001-RADRAKNARE                                 
041500         END-IF                                                           
041600                                                                          
041700         PERFORM UNTIL (SORTWS-IDARTNR  NOT = OLD-IDARTNR)  OR            
041800*****              ****(SORTWS-IDLOPNRM NOT = OLD-IDLOPNRM) OR            
041900                       END-OF-SORTFIL                                     
042000           PERFORM B-BERAKNA-RAD-ARTNR                                    
042100           PERFORM S32-SORT-RETURN                                        
042200         END-PERFORM                                                      
042300                                                                          
042400         PERFORM D-SKRIV-RAD                                              
042500                                                                          
042600     END-PERFORM                                                          
042700                                                                          
042800     ADD +1            TO WS-ANTAL                                        
042900     PERFORM C-SUMMERA-OLD-LEV                                            
043000     .                                                                    
043100     EJECT                                                                
043200 A-INIT SECTION.                                                          
043300                                                                          
043400     OPEN INPUT  W23636                                                   
043500                                                                          
043600     OPEN OUTPUT W23640-001                                               
043700                 W23641                                                   
043800     SKIP2                                                                
043900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
044000     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
044100     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
044200     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
044300     MOVE D-VECKA   TO  W001-VECKA                                        
044400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
044500                                                                          
044600     MOVE +1                TO IX-DEL                                     
044700     PERFORM UNTIL IX-DEL > 13                                            
044800        MOVE ZERO           TO WS-SUM       (IX-DEL)                      
044900        MOVE ZERO           TO WS-SUM-ART   (IX-DEL)                      
045000        MOVE ZERO           TO WS-PROC      (IX-DEL)                      
045100        MOVE ZERO           TO WS-PROC-ART  (IX-DEL)                      
045200        MOVE ZERO           TO W001-DEL-NUM (IX-DEL)                      
045300        MOVE ZERO           TO W001-DEL     (IX-DEL)                      
045400        ADD +1              TO IX-DEL                                     
045500     END-PERFORM                                                          
045600                                                                          
045700     MOVE DAGENS-DATUM      TO W001-DATUM                                 
045800     .                                                                    
045900     EJECT                                                                
046000 B-BERAKNA-RAD-ARTNR SECTION.                                             
046100                                                                          
046200     COMPUTE IX-DEL = SORTWS-KVDAGAR + 7                                  
046300     IF IX-DEL < 1                                                        
046400        MOVE 1            TO IX-DEL                                       
046500     END-IF                                                               
046600     IF IX-DEL > 13                                                       
046700        MOVE 13           TO IX-DEL                                       
046800     END-IF                                                               
046900                                                                          
047000     IF SORTWS-KVAVIS > ZERO                                              
047100        COMPUTE WS-DEL  ROUNDED =                                         
047200                        SORTWS-KVAVROP-AVB / SORTWS-KVAVIS                
047300     ELSE                                                                 
047400        MOVE ZERO         TO WS-DEL                                       
047500     END-IF                                                               
047600     ADD  WS-DEL          TO WS-SUM (IX-DEL) WS-SUM-ART (IX-DEL)          
047700     COMPUTE WS-DELPROC = WS-DEL * 100                                    
047800                                                                          
047900     IF SORTWS-IDLOPNRM NOT = OLD-IDLOPNRM                                
048000        ADD  +1           TO WS-ANTAL WS-ANTAL-ART                        
048100     END-IF                                                               
048200     MOVE SORTWS-IDLOPNRM TO OLD-IDLOPNRM                                 
048300     MOVE SORTWS-IDARTNR  TO OLD-IDARTNR                                  
048400                                                                          
048500     PERFORM BA-ADDERA-SNITTBER                                           
048600                                                                          
048700     PERFORM BB-SKRIV-ARTRAD                                              
048800     .                                                                    
048900     EJECT                                                                
049000 BA-ADDERA-SNITTBER  SECTION.                                             
049100                                                                          
049200     MOVE SORTWS-KVDAGAR TO WS-KVDAGAR                                    
049300     IF WS-KVDAGAR < ZERO                                                 
049400        COMPUTE WS-KVDAGAR = -1 * WS-KVDAGAR                              
049500     END-IF                                                               
049600     COMPUTE WS-VIKT ROUNDED = WS-KVDAGAR * WS-DEL                        
049700     ADD WS-VIKT    TO WS-SUMVIKT WS-SUMVIKT-ART                          
049800     .                                                                    
049900     EJECT                                                                
050000 BB-SKRIV-ARTRAD     SECTION.                                             
050100                                                                          
050200     ADD  WS-DELPROC         TO W001-DEL-NUM (IX-DEL)                     
050300     IF   WS-DELPROC > ZERO AND WS-DELPROC < 0.1                          
050400          ADD  0.1           TO W001-DEL-NUM (IX-DEL)                     
050500     END-IF                                                               
050600                                                                          
050700     MOVE SORTWS-IDARTNR     TO W001-IDARTNR                              
050800     .                                                                    
050900     EJECT                                                                
051000 C-SUMMERA-OLD-LEV   SECTION.                                             
051100                                                                          
051200     MOVE ZERO              TO WS-TOTSUM                                  
051300     SUBTRACT +1          FROM WS-ANTAL                                   
051400                                                                          
051500     MOVE +1                TO IX-DEL                                     
051600     PERFORM UNTIL IX-DEL > 13                                            
051700        ADD WS-SUM (IX-DEL) TO WS-TOTSUM                                  
051800        ADD +1              TO IX-DEL                                     
051900     END-PERFORM                                                          
052000                                                                          
052100     MOVE +1                TO IX-DEL                                     
052200     PERFORM UNTIL IX-DEL > 13                                            
052300        IF WS-TOTSUM > ZERO                                               
052400           COMPUTE WS-PROC (IX-DEL) ROUNDED =                             
052500                   100 * WS-SUM (IX-DEL) / WS-TOTSUM                      
052600        ELSE                                                              
052700           MOVE ZERO        TO WS-PROC (IX-DEL)                           
052800        END-IF                                                            
052900        MOVE WS-PROC (IX-DEL) TO W001-PROC (IX-DEL)                       
053000        ADD +1              TO IX-DEL                                     
053100     END-PERFORM                                                          
053200                                                                          
053300     PERFORM CA-SKRIV-SNITTAVVIKELSE                                      
053400                                                                          
053500     MOVE +1                TO IX-DEL                                     
053600     PERFORM UNTIL IX-DEL > 13                                            
053700        MOVE ZERO           TO WS-SUM  (IX-DEL)                           
053800        MOVE ZERO           TO WS-PROC (IX-DEL)                           
053900        ADD +1              TO IX-DEL                                     
054000     END-PERFORM                                                          
054100     MOVE SORTWS-IDLEVNR    TO OLD-IDLEVNR                                
054200                               W001-IDLEVNR                               
054300     MOVE SORTWS-IDANSK     TO OLD-IDANSK                                 
054400                               W001-IDANSK                                
054500     MOVE SORTWS-IDARTNR    TO OLD-IDARTNR                                
054600                               W001-IDARTNR                               
054700     MOVE SORTWS-IDLOPNRM   TO OLD-IDLOPNRM                               
054800     MOVE ZERO              TO WS-SUMVIKT                                 
054900     MOVE +1                TO WS-ANTAL   WS-ANTAL-ART                    
055000     .                                                                    
055100     EJECT                                                                
055200 CA-SKRIV-SNITTAVVIKELSE SECTION.                                         
055300                                                                          
055400     MOVE OLD-IDLEVNR     TO W001-IDLEVNR-SNITT                           
055500                                                                          
055600     IF WS-ANTAL > ZERO                                                   
055700        COMPUTE WS-SNITT ROUNDED =                                        
055800                                WS-SUMVIKT / WS-ANTAL                     
055900     ELSE                                                                 
056000        MOVE ZERO            TO WS-SNITT                                  
056100     END-IF                                                               
056200     MOVE WS-SNITT           TO W001-SNITTAVV                             
056300     MOVE WS-ANTAL           TO W001-ANTAL                                
056400     MOVE WS-PROC (7)        TO W001-RATT                                 
056500     MOVE ZERO               TO WS-SENA WS-TIDIGA                         
056600                                                                          
056700     MOVE +8                 TO IX-DEL                                    
056800     PERFORM UNTIL IX-DEL > 13                                            
056900        ADD WS-PROC (IX-DEL) TO WS-SENA                                   
057000        ADD +1               TO IX-DEL                                    
057100     END-PERFORM                                                          
057200     MOVE WS-SENA            TO W001-SENA                                 
057300                                                                          
057400     MOVE +1                 TO IX-DEL                                    
057500     PERFORM UNTIL IX-DEL > 6                                             
057600        ADD WS-PROC (IX-DEL) TO WS-TIDIGA                                 
057700        ADD +1               TO IX-DEL                                    
057800     END-PERFORM                                                          
057900     MOVE WS-TIDIGA          TO W001-TIDIGA                               
058000     MOVE W001-SNITTA        TO W001-RAD                                  
058100     PERFORM S21-SKRIV-W23640-001                                         
058200                                                                          
058300     .                                                                    
058400     EJECT                                                                
058500 D-SKRIV-RAD         SECTION.                                             
058600                                                                          
058700     MOVE +1                TO IX-DEL                                     
058800     PERFORM UNTIL IX-DEL > 13                                            
058900      IF WS-ANTAL-ART > ZERO                                              
059000        COMPUTE W001-DEL-NUM (IX-DEL) ROUNDED =                           
059100                W001-DEL-NUM (IX-DEL) / WS-ANTAL-ART                      
059200********MOVE W001-DEL-NUM (IX-DEL)  TO W001-DEL (IX-DEL)                  
059300******ELSE                                                                
059400********MOVE ZERO           TO W001-DEL (IX-DEL)                          
059500      END-IF                                                              
059600        ADD +1              TO IX-DEL                                     
059700     END-PERFORM                                                          
059800     IF WS-ANTAL-ART > ZERO                                               
059900        COMPUTE WS-SNITT-ART ROUNDED =                                    
060000                                WS-SUMVIKT-ART / WS-ANTAL-ART             
060100     ELSE                                                                 
060200        MOVE ZERO           TO WS-SNITT-ART                               
060300     END-IF                                                               
060400     MOVE WS-SNITT-ART      TO W001-SNITTAVV-AVI                          
060500     MOVE WS-ANTAL-ART      TO W001-ANTAL-AVI                             
060600                                                                          
060700     MOVE ZERO              TO WS-TOTSUM-ART                              
060800     MOVE +1                TO IX-DEL                                     
060900     PERFORM UNTIL IX-DEL > 13                                            
061000        ADD WS-SUM-ART (IX-DEL) TO WS-TOTSUM-ART                          
061100        ADD +1              TO IX-DEL                                     
061200     END-PERFORM                                                          
061300                                                                          
061400     MOVE +1                TO IX-DEL                                     
061500     PERFORM UNTIL IX-DEL > 13                                            
061600        IF WS-TOTSUM-ART > ZERO                                           
061700           COMPUTE WS-PROC-ART (IX-DEL) ROUNDED =                         
061800                   100 * WS-SUM-ART (IX-DEL) / WS-TOTSUM-ART              
061900        ELSE                                                              
062000           MOVE ZERO        TO WS-PROC-ART (IX-DEL)                       
062100        END-IF                                                            
062200********MOVE WS-PROC-ART (IX-DEL) TO W001-PROC-ART (IX-DEL)               
062300        MOVE WS-PROC-ART (IX-DEL) TO W001-DEL      (IX-DEL)               
062400        ADD +1              TO IX-DEL                                     
062500     END-PERFORM                                                          
062600                                                                          
062700     MOVE WS-PROC-ART (7)   TO W001-RATT-AVI                              
062800     MOVE ZERO              TO WS-SENA-ART WS-TIDIGA-ART                  
062900                                                                          
063000     MOVE +8                TO IX-DEL                                     
063100     PERFORM UNTIL IX-DEL > 13                                            
063200        ADD WS-PROC-ART (IX-DEL) TO WS-SENA-ART                           
063300        ADD +1              TO IX-DEL                                     
063400     END-PERFORM                                                          
063500     MOVE WS-SENA-ART       TO W001-SENA-AVI                              
063600                                                                          
063700     MOVE +1                TO IX-DEL                                     
063800     PERFORM UNTIL IX-DEL > 6                                             
063900        ADD WS-PROC-ART (IX-DEL) TO WS-TIDIGA-ART                         
064000        ADD +1              TO IX-DEL                                     
064100     END-PERFORM                                                          
064200     MOVE WS-TIDIGA-ART     TO W001-TIDIGA-AVI                            
064300                                                                          
064400     MOVE W001-ARTRAD       TO W001-RAD                                   
064500                                                                          
064600     PERFORM S20-SKRIV-W23640-001                                         
064700                                                                          
064800     MOVE +1                TO IX-DEL                                     
064900     PERFORM UNTIL IX-DEL > 13                                            
065000        MOVE ZERO           TO WS-PROC-ART  (IX-DEL)                      
065100        MOVE ZERO           TO WS-SUM-ART   (IX-DEL)                      
065200        MOVE ZERO           TO W001-DEL     (IX-DEL)                      
065300        MOVE ZERO           TO W001-DEL-NUM (IX-DEL)                      
065400        ADD +1              TO IX-DEL                                     
065500     END-PERFORM                                                          
065600                                                                          
065700     IF SORTWS-IDLOPNRM NOT = OLD-IDLOPNRM                                
065800        ADD  +1             TO WS-ANTAL                                   
065900     END-IF                                                               
066000     MOVE +1                TO WS-ANTAL-ART                               
066100     MOVE ZERO              TO WS-SUMVIKT-ART                             
066200     MOVE SORTWS-IDLOPNRM   TO OLD-IDLOPNRM                               
066300     MOVE SORTWS-IDARTNR    TO OLD-IDARTNR                                
066400     .                                                                    
066500     EJECT                                                                
066600 Z-FINIT SECTION.                                                         
066700                                                                          
066800     MOVE MEM-006  TO UT-AREA                                             
066900     PERFORM S12-SKRIV-UTPOST-W23641                                      
067000                                                                          
067100     CLOSE W23636                                                         
067200           W23640-001                                                     
067300           W23641                                                         
067400     SKIP2                                                                
067500     MOVE 'S' TO POSTSUM-OPKOD                                            
067600     CALL POSTSUM USING POSTSUM-PARM                                      
067700     .                                                                    
067800     EJECT                                                                
067900 S01-LAES-W23636  SECTION.                                                
068000     READ W23636 INTO IN-AREA                                             
068100     AT END                                                               
068200        SET END-OF-W23636 TO TRUE                                         
068300                                                                          
068400     NOT AT END                                                           
068500        MOVE 'W23636'   TO POSTSUM-FDNAMN                                 
068600        MOVE 'W23640D1' TO POSTSUM-DDNAMN2                                
068700        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
068800        CALL POSTSUM USING POSTSUM-PARM                                   
068900     END-READ                                                             
069000     .                                                                    
069100     EJECT                                                                
069200 S12-SKRIV-UTPOST-W23641  SECTION.                                        
069300     IF MEM-IDMAIL NOT  = SPACE                                           
069400       WRITE UT-POST FROM UT-AREA                                         
069500       MOVE 'W23641'   TO POSTSUM-FDNAMN                                  
069600       MOVE 'W23662D3' TO POSTSUM-DDNAMN2                                 
069700       MOVE 'UT'       TO POSTSUM-TRANSTYP                                
069800       CALL POSTSUM USING POSTSUM-PARM                                    
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 S20-SKRIV-W23640-001  SECTION.                                           
070300                                                                          
070400     MOVE 1 TO W001-SKIP                                                  
070500     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                         
070600       PERFORM S20A-SKRIV-RUBRIKER                                        
070700     END-IF                                                               
070800     SKIP2                                                                
070900     WRITE W23640-001-RAD FROM W001-RAD AFTER W001-SKIP                   
071000     IF MEM-IDMAIL NOT  = SPACE                                           
071100*******MOVE W001-RAD (2:040) TO UT-AREA                                   
071200       MOVE W001-RAD (2:020) TO UT-AREA (1:20)                            
071300       MOVE W001-RAD(42:078) TO UT-AREA(21:78)                            
071400       PERFORM S12-SKRIV-UTPOST-W23641                                    
071500     END-IF                                                               
071600     SKIP2                                                                
071700     MOVE SPACE TO W001-RAD                                               
071800     ADD  +1 TO W001-RADRAKNARE                                           
071900     .                                                                    
072000     EJECT                                                                
072100 S20A-SKRIV-RUBRIKER SECTION.                                             
072200     ADD +1                     TO W001-SIDRAKNARE                        
072300     MOVE W001-SIDRAKNARE       TO W001-SID                               
072400                                                                          
072500     PERFORM S22A-SKAPA-MAIL-DEST                                         
072600                                                                          
072700*    IF MEM-IDMAIL NOT  = SPACE                                           
072800       WRITE W23640-001-RAD       FROM W001-RUBRIK1 AFTER PAGE            
072900*******MOVE W001-RUBRIK1 (3:072)  TO UT-AREA                              
073000       MOVE W001-RUBRIK1          TO UT-AREA                              
073100       PERFORM S12-SKRIV-UTPOST-W23641                                    
073200***    MOVE SPACE                 TO UT-AREA                              
073300***    MOVE W001-RUBRIK1 (75:44)  TO UT-AREA (25:44)                      
073400***    PERFORM S12-SKRIV-UTPOST-W23641                                    
073500       WRITE W23640-001-RAD       FROM W001-RUBRIK2 AFTER 2               
073600       MOVE W001-RUBRIK2 (2:040)  TO UT-AREA                              
073700       PERFORM S12-SKRIV-UTPOST-W23641                                    
073800       WRITE W23640-001-RAD       FROM W001-RUBRIK3 AFTER 1               
073900       MOVE W001-RUBRIK3 (2:040)  TO UT-AREA                              
074000       PERFORM S12-SKRIV-UTPOST-W23641                                    
074100       WRITE W23640-001-RAD       FROM W001-RUBRIK4 AFTER 1               
074200*******MOVE W001-RUBRIK4 (2:040)  TO UT-AREA                              
074300       MOVE W001-RUBRIK4 (2:020)  TO UT-AREA (1:20)                       
074400       MOVE W001-RUBRIK4(42:078)  TO UT-AREA(21:78)                       
074500       PERFORM S12-SKRIV-UTPOST-W23641                                    
074600       MOVE +7                    TO W001-RADRAKNARE                      
074700       SKIP2                                                              
074800       MOVE 2                     TO W001-SKIP                            
074900*    END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200 S21-SKRIV-W23640-001  SECTION.                                           
075300       MOVE 2 TO W001-SKIP                                                
075400       IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                       
075500         PERFORM S21A-SKRIV-RUBRIKER                                      
075600       ELSE                                                               
075700         PERFORM S21B-SKRIV-RUBRIKER                                      
075800       END-IF                                                             
075900       SKIP2                                                              
076000       WRITE W23640-001-RAD FROM W001-RAD AFTER W001-SKIP                 
076100*******MOVE W001-RAD (2:040) TO UT-AREA                                   
076200       MOVE W001-RAD (2:020) TO UT-AREA (1:20)                            
076300       MOVE W001-RAD(42:078) TO UT-AREA(21:78)                            
076400       PERFORM S12-SKRIV-UTPOST-W23641                                    
076500       SKIP2                                                              
076600       MOVE SPACE           TO W001-RAD                                   
076700       ADD  +2              TO W001-RADRAKNARE                            
076800     .                                                                    
076900     EJECT                                                                
077000 S21S-SKRIV-W23640-001  SECTION.                                          
077100       MOVE 2 TO W001-SKIP                                                
077200       IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA                       
077300         PERFORM S21A-SKRIV-RUBRIKER                                      
077400       END-IF                                                             
077500       SKIP2                                                              
077600       WRITE W23640-001-RAD FROM W001-RAD AFTER W001-SKIP                 
077700*******MOVE W001-RAD (2:040) TO UT-AREA                                   
077800       MOVE W001-RAD (2:020) TO UT-AREA (1:20)                            
077900       MOVE W001-RAD(42:078) TO UT-AREA(21:78)                            
078000       PERFORM S12-SKRIV-UTPOST-W23641                                    
078100       SKIP2                                                              
078200       MOVE SPACE           TO W001-RAD                                   
078300       ADD  +2              TO W001-RADRAKNARE                            
078400     .                                                                    
078500     EJECT                                                                
078600 S21A-SKRIV-RUBRIKER SECTION.                                             
078700       ADD +1                    TO W001-SIDRAKNARE                       
078800       MOVE W001-SIDRAKNARE      TO W001-SID                              
078900       WRITE W23640-001-RAD      FROM W001-RUBRIK1 AFTER PAGE             
079000*******MOVE W001-RUBRIK1 (3:072) TO UT-AREA                               
079100       MOVE W001-RUBRIK1         TO UT-AREA                               
079200       PERFORM S12-SKRIV-UTPOST-W23641                                    
079300***    MOVE SPACE                TO UT-AREA                               
079400***    MOVE W001-RUBRIK1 (75:44) TO UT-AREA (25:44)                       
079500***    PERFORM S12-SKRIV-UTPOST-W23641                                    
079600       WRITE W23640-001-RAD      FROM W001-RUBRIKA AFTER 2                
079700*******MOVE W001-RUBRIKA (2:040) TO UT-AREA                               
079800       MOVE W001-RUBRIKA (2:020) TO UT-AREA (1:20)                        
079900       MOVE W001-RUBRIKA(42:078) TO UT-AREA(21:78)                        
080000       PERFORM S12-SKRIV-UTPOST-W23641                                    
080100       WRITE W23640-001-RAD      FROM W001-RUBRIKB AFTER 1                
080200*******MOVE W001-RUBRIKB (2:040) TO UT-AREA                               
080300       MOVE W001-RUBRIKB (2:020) TO UT-AREA (1:20)                        
080400       MOVE W001-RUBRIKB(42:078) TO UT-AREA(21:78)                        
080500       PERFORM S12-SKRIV-UTPOST-W23641                                    
080600       MOVE +7                   TO W001-RADRAKNARE                       
080700       SKIP2                                                              
080800       MOVE 2                    TO W001-SKIP                             
080900     .                                                                    
081000     EJECT                                                                
081100 S21B-SKRIV-RUBRIKER SECTION.                                             
081200       WRITE W23640-001-RAD      FROM W001-RUBRIKA AFTER 2                
081300*******MOVE W001-RUBRIKA (2:040) TO UT-AREA                               
081400       MOVE W001-RUBRIKA (2:020) TO UT-AREA (1:20)                        
081500       MOVE W001-RUBRIKA(42:078) TO UT-AREA(21:78)                        
081600       PERFORM S12-SKRIV-UTPOST-W23641                                    
081700       WRITE W23640-001-RAD      FROM W001-RUBRIKB AFTER 1                
081800*******MOVE W001-RUBRIKB (2:040) TO UT-AREA                               
081900       MOVE W001-RUBRIKB (2:020) TO UT-AREA (1:20)                        
082000       MOVE W001-RUBRIKB(42:078) TO UT-AREA(21:78)                        
082100       PERFORM S12-SKRIV-UTPOST-W23641                                    
082200       ADD  +7                   TO W001-RADRAKNARE                       
082300       SKIP2                                                              
082400       MOVE 2                    TO W001-SKIP                             
082500     .                                                                    
082600     EJECT                                                                
082700 S22A-SKAPA-MAIL-DEST SECTION.                                            
082800                                                                          
082900     IF OLD-IDANSK (1:2) NOT = MEM-IDANSK (1:2)                           
083100                                                                          
083200        IF MEM-IDANSK NOT = 999                                           
083300           MOVE MEM-006   TO UT-AREA                                      
083400           PERFORM S12-SKRIV-UTPOST-W23641                                
083500        END-IF                                                            
083600        MOVE OLD-IDANSK   TO MEM-IDANSK                                   
083700                                                                          
083800        PERFORM S30-HAMTA-NAMN                                            
083900        IF MEM-IDMAIL NOT = SPACE                                         
084000                                                                          
084100          MOVE MEM-001      TO UT-AREA                                    
084200          PERFORM S12-SKRIV-UTPOST-W23641                                 
084300          MOVE MEM-002      TO UT-AREA                                    
084400          PERFORM S12-SKRIV-UTPOST-W23641                                 
084500          MOVE MEM-003      TO UT-AREA                                    
084600          PERFORM S12-SKRIV-UTPOST-W23641                                 
084700          MOVE MEM-007      TO UT-AREA                                    
084800          PERFORM S12-SKRIV-UTPOST-W23641                                 
084900                                                                          
085000          MOVE MEM-004      TO UT-AREA                                    
085100          PERFORM S12-SKRIV-UTPOST-W23641                                 
085200          MOVE MEM-005      TO UT-AREA                                    
085300          PERFORM S12-SKRIV-UTPOST-W23641                                 
085400        END-IF                                                            
085500     END-IF                                                               
085600     .                                                                    
085700     EJECT                                                                
085800 S31-SORT-RELEASE  SECTION.                                               
085900                                                                          
086000     RELEASE SORT-POST FROM SORTWS-AREA                                   
086100     .                                                                    
086200     EJECT                                                                
086300 S32-SORT-RETURN  SECTION.                                                
086400                                                                          
086500     RETURN SORTFIL INTO SORTWS-AREA                                      
086600     AT END                                                               
086700         SET END-OF-SORTFIL TO TRUE                                       
086800     .                                                                    
086900     EJECT                                                                
087000 S99-ABEND SECTION.                                                       
087100                                                                          
087200     SKIP2                                                                
087300     MOVE 'S' TO POSTSUM-OPKOD                                            
087400     CALL POSTSUM USING POSTSUM-PARM                                      
087500     CALL ABEND USING RKOD-ABEND                                          
087600     .                                                                    
087700     EJECT                                                                
087800 S30-HAMTA-NAMN SECTION.                                                  
087900     MOVE OLD-IDANSK            TO W-IDPERSON                             
088000     PERFORM IMS-GET-WDP3-ANSKNAMN                                        
088100     IF SEGMENT-FINNS                                                     
088200        MOVE PERS-IDMAIL                   TO MEM-IDMAIL                  
088300     ELSE                                                                 
088400        DISPLAY ' IDANSK SAKNAS ' OLD-IDANSK                              
088500        MOVE SPACE                         TO MEM-IDMAIL                  
088600     END-IF                                                               
088700     .                                                                    
088800     EJECT                                                                
088900* --- IMS SEKTIONER ---                                                   
089000                                                                          
089100 IMS-GET-WDP3-ANSKNAMN SECTION.                                           
089200                                                                          
089300     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
089400          DELIMITED BY SIZE INTO SSA1                                     
089500     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
089600          DELIMITED BY SIZE INTO SSA2                                     
089700     MOVE '  GE' TO GODK-STATUSKODER                                      
089800     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
089900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
090000     PERFORM IMS-STATUSKONTROLL                                           
090100     .                                                                    
090200     EJECT                                                                
090300 IMS-STATUSKONTROLL SECTION.                                              
090400                                                                          
090500     SET STATUS-IX TO 1                                                   
090600     SEARCH GODK-STATUS                                                   
090700       AT END                                                             
090800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
090900           DELIMITED BY SIZE INTO FELTEXT                                 
091000         DISPLAY FELTEXT                                                  
091100         CALL FELLOG                                                      
091200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
091300         CONTINUE                                                         
091400     END-SEARCH                                                           
091500     .                                                                    
