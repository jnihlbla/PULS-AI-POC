000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2142200.                                                
000300 AUTHOR.         ARVIDSSON LENA.                                          
000400 DATE-WRITTEN.   01/11/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700******************************************************************        
000800*                                                                *        
000900*    FUNKTION:                                                   *        
001000*        PGM SOM SKRIVER LARMRAPPORT, SÄKERHETSLAGER UNDERSKRIDET*        
001100*        FÖR ANSK 900 - 919.*                                             
001200*        SORTERAT PER ANSKAFFARE, LEVERANTÖRNR, ARTIKELNR.       *        
001300*                                                                *        
001400*        PROGRAMMET LÄSER      WDK6                              *        
001500*        PROGRAMMET LÄSER      WDF5                              *        
001600*        PROGRAMMET LÄSER      WDD9                              *        
001700*                                                                *        
001800*    ABENDKODER:                                                 *        
001900*        U0016 -  . . . .                                        *        
002000*        U1000 -  . . . .                                        *        
002100*                                                                *        
002200******************************************************************        
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- LARMLISTPOSTER                                             
003100     SELECT W21411                     ASSIGN TO W21422D1.                
003200     SKIP2                                                                
003300*          --- LARMLISTA                                                  
003400     SELECT W21422-001                 ASSIGN TO W21422D2.                
003500     SKIP2                                                                
003600*          --- SORTERINGSFIL                                              
003700     SELECT SORTFIL                    ASSIGN TO W21422DS.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W21411                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W2141101      -L.                                              
004800     SKIP3                                                                
004900 FD  W21422-001                                                           
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200     SKIP2                                                                
005300 01  W21422-001-RAD              PIC X(121).                              
005400     SKIP2                                                                
005500 SD  SORTFIL.                                                             
005600                                                                          
005700*01  POST -COPY W2141101      -PRE SORT-                                  
005800     EJECT                                                                
005900                                                                          
006000 WORKING-STORAGE SECTION.                                                 
006100                                                                          
006200 77  IDPGM                       PIC X(8)    VALUE 'W2142200'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500                                                                          
006510*01  -COPY WWDCKONS                                                       
006520                                                                          
006600 77  W21411-EOF-SW               PIC X       VALUE 'N'.                   
006700     88  END-OF-W21411                       VALUE 'J'.                   
006800                                                                          
006900 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007000     88  END-OF-SORTFIL                      VALUE 'J'.                   
007100     EJECT                                                                
007200 77  MAX-ANT-AVROP-RAD-I-TAB     PIC S9(9)   VALUE +3  COMP-3.            
007300*                                                                         
007400 01  WS-IDANSK-GAM               PIC S9(3)   VALUE ZERO.                  
007500*                                                                         
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700                                                                          
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200                                                                          
008300 01  WS-DAGENS-DATUM-AAVV        PIC 9(4).                                
008400                                                                          
008500 01  WS-DAGENS-DATUM-AAVV-3V     PIC 9(4).                                
008600     EJECT                                                                
008700*                                                                         
008800 01  WS-KVAVROP-SLAEP            PIC S9(7)   VALUE ZERO.                  
008900*                                                                         
009000 01  WS-DAAVROP-AVS              PIC S9(6).                               
009100*                                                                         
009200 01  FILLER REDEFINES WS-DAAVROP-AVS.                                     
009300     03  FILLER                  PIC S9(2).                               
009400     03  WS-TIAVROP-AVS          PIC S9(4).                               
009500*                                                                         
009600 01  WS-AVROP-TAB.                                                        
009700     03 WS-KVAVROP-TAB-RAD       OCCURS 3.                                
009800       05 WS-KVAVROP-TAB-KOL.                                             
009900         10 WS-KVAVROP-TAB       PIC S9(7)   COMP-3.                      
010000         10 WS-TIAVROP-AVS-TAB   PIC S9(4)   COMP-3.                      
010100*                                                                         
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300*                                                                         
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
011100     SKIP2                                                                
011200*    --- PARAMETRAR TILL ABEND                                            
011300                                                                          
011400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011700     SKIP2                                                                
011800 01  FELTEXT.                                                             
011900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012100     EJECT                                                                
012200*    --- PARAMETRAR TILL DATKORT                                          
012300                                                                          
012400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W21422'.              
012500     SKIP2                                                                
012600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012700     SKIP2                                                                
012800*01  -COPY WDATKORT                                                       
012900     EJECT                                                                
013000*    --- PARAMETRAR TILL POSTSUM                                          
013100                                                                          
013200*01  -COPY W0005   -PRE  POSTSUM-                                         
013300     EJECT                                                                
013400*01  -COPY WDATAREA                                                       
013500     EJECT                                                                
013600* VARIABLER TILL SUBPROGRAM W009VADD                                      
013700 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
013800 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
013900     EJECT                                                                
014000 01  IN-AREA-START               PIC X(24)   VALUE                        
014100                                 'IN-AREA-START  '.                       
014200     SKIP2                                                                
014300                                                                          
014400*01  AREA -COPY W2141101     -PRE IN-                                     
014500     EJECT                                                                
014600 01  W001-AREA-START             PIC X(24)   VALUE                        
014700                                 'W001-AREA-START  '.                     
014800     SKIP2                                                                
014900 01  W001-HJALPAREOR.                                                     
015000*                                                                         
015100     03  W001-SKIP               PIC 9(3)    COMP-3  VALUE 1.             
015200     03  W001-MAX-RADER-PER-SIDA                                          
015300                                 PIC 9(3)    VALUE 42.                    
015400     03  W001-LISTNR             PIC X(11)   VALUE 'W21422-001'.          
015500     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
015600     03  W001-RADRAKNARE         PIC S9(3)   COMP-3 VALUE 999.            
015700     03  IX-AVROP                PIC S9(9)   COMP-3.                      
015800     EJECT                                                                
015900 01  W001-RAD.                                                            
016000*                                                                         
016100     03  FILLER                  PIC X(121)  VALUE SPACE.                 
016200     EJECT                                                                
016300 01  W001-RUBRIK1.                                                        
016400*                                                                         
016500     03  FILLER                  PIC X(3).                                
016600     03  FILLER                  PIC X(21)                                
016700                                 VALUE 'VOLVO CAR CUST SERV  '.           
016800     03  FILLER                  PIC X(12)                                
016900                                 VALUE 'W21422-001  '.                    
017000     03  FILLER                  PIC X(40)                                
017100             VALUE 'LARMRAPPORT, SÄKERHETSLAGER UNDERSKRIDET'.            
017200     03  FILLER                  PIC X(11) VALUE SPACE.                   
017300     03  FILLER                  PIC X(7)  VALUE 'DATUM: '.               
017400     03  W001-DATUM              PIC XXBXXBXX.                            
017500     03  FILLER                  PIC X(4)  VALUE SPACE.                   
017600     03  FILLER                  PIC X(4)                                 
017700                                 VALUE 'SID '.                            
017800     03  W001-SID                PIC Z(4)9.                               
017900     03  FILLER                  PIC X(17) VALUE SPACE.                   
018000     SKIP2                                                                
018100 01  W001-RUBRIK2.                                                        
018200     03  FILLER                  PIC X(7)  VALUE '   ANSK'.               
018300     03  FILLER                  PIC X(8)  VALUE '   LEVNR'.              
018400     03  FILLER                  PIC X(6)  VALUE SPACE.                   
018500     03  FILLER                  PIC X(5)  VALUE 'ARTNR'.                 
018600     03  FILLER                  PIC X(3)  VALUE SPACE.                   
018700     03  FILLER                  PIC X(7)  VALUE 'BENÄMN.'.               
018800     03  FILLER                  PIC X(27) VALUE SPACE.                   
018900     03  FILLER                  PIC X(5)  VALUE 'LAGER'.                 
019000     03  FILLER                  PIC X(6)  VALUE SPACE.                   
019100     03  FILLER                  PIC X(2)  VALUE 'AK'.                    
019200     03  FILLER                  PIC X(6)  VALUE SPACE.                   
019300     03  FILLER                  PIC X(2)  VALUE 'RO'.                    
019400     03  FILLER                  PIC X(4)  VALUE SPACE.                   
019500     03  FILLER                  PIC X(4)  VALUE 'SLÄP'.                  
019600     03  FILLER                  PIC X(7)  VALUE SPACE.                   
019700     03  FILLER                  PIC X(9)  VALUE 'AVROP1   '.             
019800     03  FILLER                  PIC X(7)  VALUE 'S-LAGER'.               
019900     03  FILLER                  PIC X(17) VALUE SPACE.                   
020000     EJECT                                                                
020100 01  W001-RUBRIK3.                                                        
020200     03  FILLER                  PIC X(29) VALUE SPACE.                   
020300     03  FILLER                  PIC X(7)  VALUE 'LEV.BET'.               
020400     03  FILLER                  PIC X(63) VALUE SPACE.                   
020500     03  FILLER                  PIC X(6)  VALUE 'AVROP2'.                
020600     03  FILLER                  PIC X(27) VALUE SPACE.                   
020700     EJECT                                                                
020800 01  W001-RUBRIK4.                                                        
020900     03  FILLER                  PIC X(99) VALUE SPACE.                   
021000     03  FILLER                  PIC X(6)  VALUE 'AVROP3'.                
021100     03  FILLER                  PIC X(27) VALUE SPACE.                   
021200     EJECT                                                                
021300 01  W001-DETALJ1.                                                        
021400     03  FILLER                  PIC X(4)  VALUE SPACE.                   
021500     03  W001-IDANSK             PIC Z(2)9.                               
021600     03  FILLER                  PIC X(3)  VALUE SPACE.                   
021700     03  W001-IDLEVNR            PIC X(5).                                
021800     03  FILLER                  PIC X(2)  VALUE SPACE.                   
021900     03  W001-IDARTNR            PIC Z(8)9.                               
022000     03  FILLER                  PIC X(3)  VALUE SPACE.                   
022100     03  W001-BEART-SVE          PIC X(25).                               
022200     03  FILLER                  PIC X(7)  VALUE SPACE.                   
022300     03  W001-KVLS               PIC Z(6)9.                               
022400     03  FILLER                  PIC X(1)  VALUE SPACE.                   
022500     03  W001-KVAKS-SUM          PIC Z(6)9.                               
022600     03  FILLER                  PIC X(1)  VALUE SPACE.                   
022700     03  W001-KVROS              PIC Z(6)9.                               
022800     03  FILLER                  PIC X(1)  VALUE SPACE.                   
022900     03  W001-KVAVROP-SLAEP      PIC Z(6)9.                               
023000     03  FILLER                  PIC X(1)  VALUE SPACE.                   
023100     03  W001-KVAVROP-1          PIC Z(6)9 BLANK WHEN ZERO.               
023200     03  FILLER                  PIC X     VALUE '-'.                     
023300     03  W001-DAAVROP-AVS-1      PIC 9(4)  BLANK WHEN ZERO.               
023400     03  FILLER                  PIC X(3)  VALUE SPACE.                   
023500     03  W001-KVSLAGER           PIC Z(6)9.                               
023600     03  FILLER                  PIC X(17) VALUE SPACE.                   
023700     EJECT                                                                
023800 01  W001-DETALJ2.                                                        
023900     03  FILLER                  PIC X(29) VALUE SPACE.                   
024000     03  W001-BELEVART           PIC X(30).                               
024100     03  FILLER                  PIC X(34) VALUE SPACE.                   
024200     03  W001-KVAVROP-2          PIC Z(6)9 BLANK WHEN ZERO.               
024300     03  FILLER                  PIC X     VALUE '-'.                     
024400     03  W001-DAAVROP-AVS-2      PIC 9(4)  BLANK WHEN ZERO.               
024500     03  FILLER                  PIC X(27) VALUE SPACE.                   
024600 01  W001-DETALJ3.                                                        
024700     03  FILLER                  PIC X(93) VALUE SPACE.                   
024800     03  W001-KVAVROP-3          PIC Z(6)9 BLANK WHEN ZERO.               
024900     03  FILLER                  PIC X     VALUE '-'.                     
025000     03  W001-DAAVROP-AVS-3      PIC 9(4)  BLANK WHEN ZERO.               
025100     03  FILLER                  PIC X(27)   VALUE SPACE.                 
025200     EJECT                                                                
025300 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
025400                                  'SORTWS-AREA-START  '.                  
025500     SKIP2                                                                
025600                                                                          
025700*01  AREA -COPY W2141101      -PRE SORTWS-                                
025800 01  SORT-RETURN-X               PIC X(2)    VALUE SPACE.                 
025900     EJECT                                                                
026000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026100*                                                                         
026200     EJECT                                                                
026300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026400     SKIP3                                                                
026500 01  NYCKLAR-TILL-DLI.                                                    
026600     03  W-IDARTNR-X.                                                     
026700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026800     03  W-KDSEGKEY-X.                                                    
026900         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
027000     03  W-WDF5KEY-X.                                                     
027100         05  W-WDF5KEY           PIC X(4)    VALUE SPACE.                 
027110     03  W-WDD901KY-X.                                                    
027120         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
027130         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
027200     03  W-IDLEVNR-X.                                                     
027300         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
027400     03  W-WDD905KY-X.                                                    
027500         05  W-WDD905KY          PIC X(7)    VALUE SPACE.                 
027600     SKIP2                                                                
027700*    --- STATUS-KOD FRÅN IMS                                              
027800 01  STATUS-WS                   PIC XX.                                  
027900     88  SEGMENT-FINNS                       VALUE '  '.                  
028000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028200     SKIP2                                                                
028300 01  GODK-STATUSKODER.                                                    
028400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028500     SKIP3                                                                
028600 01  SSA1                        PIC X(64).                               
028700 01  SSA2                        PIC X(64).                               
028800     EJECT                                                                
028900*    --- IMS FUNKTIONSKODER                                               
029000*01  -COPY W0003                                                          
029100     EJECT                                                                
029200*    ---  DLI INPUT-OUTPUT AREA                                           
029300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
029400 01  DLI-IO-WDK601.                                                       
029500*    03  -COPY WDK601                                                     
029600     EJECT                                                                
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
029800 01  DLI-IO-WDK611.                                                       
029900*    03  -COPY WDK611                                                     
030000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF501'.                      
030100 01  DLI-IO-WDF501.                                                       
030200*    03  -COPY WDF501                                                     
030300     EJECT                                                                
030400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF502'.                      
030500 01  DLI-IO-WDF502.                                                       
030600*    03  -COPY WDF502                                                     
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
030800 01  DLI-IO-WDD901.                                                       
030900*    03  -COPY WDD901                                                     
031000     EJECT                                                                
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
031200 01  DLI-IO-WDD902.                                                       
031300*    03  -COPY WDD902                                                     
031400     EJECT                                                                
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
031600 01  DLI-IO-WDD905.                                                       
031700*    03  -COPY WDD905                                                     
031800     EJECT                                                                
031900                                                                          
032000 LINKAGE SECTION.                                                         
032100                                                                          
032200*01  -COPY W0008  -PRE WDK6-                                              
032300     05  FILLER                  PIC X.                                   
032400                                                                          
032500*01  -COPY W0008  -PRE WDF5-                                              
032600     05  FILLER                  PIC X.                                   
032700                                                                          
032800*01  -COPY W0008  -PRE WDD9-                                              
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100                                                                          
033200 PROCEDURE DIVISION  USING WDK6-PCB WDF5-PCB                              
033300                           WDD9-PCB.                                      
033400                                                                          
033500 MAIN SECTION.                                                            
033600     ENTRY 'DLITCBL' USING WDK6-PCB WDF5-PCB                              
033700                           WDD9-PCB.                                      
033800                                                                          
033900     PERFORM A-INIT                                                       
034000     PERFORM S01-LAES-W21411                                              
034100     IF W21411-EOF-SW = NEJ                                               
034200                                                                          
034300         SORT SORTFIL ASCENDING KEY SORT-IDANSK                           
034400                                    SORT-IDLEVNR                          
034500                                    SORT-IDARTNR                          
034600                    INPUT PROCEDURE B-SORT-INPUT                          
034700                   OUTPUT PROCEDURE C-SORT-OUTPUT                         
034800                                                                          
034900       IF SORT-RETURN NOT = 0                                             
035000         MOVE SORT-RETURN TO SORT-RETURN-X                                
035100         STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                    
035200             DELIMITED BY SIZE                                            
035300             INTO FELTEXT-STR                                             
035400         DISPLAY FELTEXT                                                  
035500         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                          
035600         PERFORM S99-ABEND                                                
035700       ELSE                                                               
035800         PERFORM Z-FINIT                                                  
035900                                                                          
036000         MOVE ZERO TO RETURN-CODE                                         
036100         GOBACK                                                           
036200       END-IF                                                             
036300     ELSE                                                                 
036400       PERFORM Z-FINIT                                                    
036500                                                                          
036600       MOVE ZERO TO RETURN-CODE                                           
036700       GOBACK                                                             
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 A-INIT SECTION.                                                          
037200                                                                          
037300     OPEN INPUT  W21411                                                   
037400                                                                          
037500     OPEN OUTPUT W21422-001                                               
037600                                                                          
037700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
037800     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
037900     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
038000     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
038100     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
038200     MOVE DAGENS-DATUM TO W001-DATUM                                      
038300                                                                          
038400     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
038500     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
038600                                                                          
038700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
038800                         DAT-O-TIDATUM DAT-KDSVAR                         
038900                                                                          
039000     IF DAT-KDSVAR-OK                                                     
039100       MOVE DAT-TIAAVV-GRP TO WS-DAGENS-DATUM-AAVV-3V                     
039200                              WS-DAGENS-DATUM-AAVV                        
039300       ADD +3 TO WS-DAGENS-DATUM-AAVV-3V                                  
039400     ELSE                                                                 
039500       PERFORM S99-ABEND                                                  
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 B-SORT-INPUT  SECTION.                                                   
040000                                                                          
040100     PERFORM S01-LAES-W21411                                              
040200     PERFORM UNTIL END-OF-W21411                                          
040300       IF IN-IDANSK >= 900 OR IN-IDANSK <= 919                            
040400         MOVE IN-AREA TO SORTWS-AREA                                      
040500         PERFORM S31-SORT-RELEASE                                         
040600       END-IF                                                             
040700       PERFORM S01-LAES-W21411                                            
040800     END-PERFORM                                                          
040900     .                                                                    
041000     EJECT                                                                
041100 C-SORT-OUTPUT SECTION.                                                   
041200                                                                          
041300     PERFORM S32-SORT-RETURN                                              
041400     PERFORM UNTIL END-OF-SORTFIL                                         
041500       PERFORM CA-SKAPA-W21422-001                                        
041600       PERFORM S32-SORT-RETURN                                            
041700     END-PERFORM                                                          
041800     .                                                                    
041900     EJECT                                                                
042000 CA-SKAPA-W21422-001 SECTION.                                             
042100                                                                          
042200******************************************************************        
042300****LÄSER OCH HÄMTAR VÄRDEN FRÅN COBOLCPY(W2141101), WDK6,    ****        
042400****WDD9 OCH WDF5.                                            ****        
042500******************************************************************        
042600                                                                          
042700     MOVE SORTWS-IDANSK     TO W001-IDANSK                                
042800     MOVE SORTWS-IDLEVNR    TO W001-IDLEVNR                               
042900                               W-IDLEVNR                                  
043000     MOVE SORTWS-IDARTNR    TO W001-IDARTNR                               
043100                               W-IDARTNR                                  
043200     MOVE SORTWS-BEART-SVE  TO W001-BEART-SVE                             
043300     PERFORM IMS-GET-WDK601                                               
043400     IF SEGMENT-SAKNAS                                                    
043500       MOVE ZERO  TO W001-KVLS                                            
043600                     W001-KVAKS-SUM                                       
043700                     W001-KVSLAGER                                        
043800                     CLAG-KDUART                                          
043900     ELSE                                                                 
044000       PERFORM IMS-GET-WDK611                                             
044100       IF SEGMENT-SAKNAS                                                  
044200         MOVE ZERO  TO W001-KVLS                                          
044300                       W001-KVAKS-SUM                                     
044400                       W001-KVSLAGER                                      
044500                       CLAG-KDUART                                        
044600       ELSE                                                               
044700         MOVE CLAG-KVLS          TO W001-KVLS                             
044800         ADD  CLAG-KVAKS-CDC     TO CLAG-KVAKS-PAV                        
044900                             GIVING W001-KVAKS-SUM                        
045000         MOVE CLAG-KVROS         TO W001-KVROS                            
045100         MOVE CLAG-KVSLAGER      TO W001-KVSLAGER                         
045200       END-IF                                                             
045300     END-IF                                                               
045400     MOVE ZERO TO WS-KVAVROP-TAB (1)                                      
045500                  WS-KVAVROP-TAB (2)                                      
045600                  WS-KVAVROP-TAB (3)                                      
045700     MOVE ZERO TO WS-TIAVROP-AVS-TAB (1)                                  
045800                  WS-TIAVROP-AVS-TAB (2)                                  
045900                  WS-TIAVROP-AVS-TAB (3)                                  
046000     MOVE W-IDARTNR  TO W-IDARTNR-D9                                      
046010     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
046100     PERFORM IMS-GET-WDD901                                               
046200     IF SEGMENT-SAKNAS                                                    
046300       MOVE ZERO  TO W001-KVAVROP-SLAEP                                   
046400                     W001-KVAVROP-1                                       
046500                     W001-KVAVROP-2                                       
046600                     W001-KVAVROP-3                                       
046700                     W001-DAAVROP-AVS-1                                   
046800                     W001-DAAVROP-AVS-2                                   
046900                     W001-DAAVROP-AVS-3                                   
047000     ELSE                                                                 
047100       PERFORM IMS-GET-WDD902                                             
047200       IF SEGMENT-SAKNAS                                                  
047300         MOVE ZERO  TO W001-KVAVROP-SLAEP                                 
047400                       W001-KVAVROP-1                                     
047500                       W001-KVAVROP-2                                     
047600                       W001-KVAVROP-3                                     
047700                       W001-DAAVROP-AVS-1                                 
047800                       W001-DAAVROP-AVS-2                                 
047900                       W001-DAAVROP-AVS-3                                 
048000       ELSE                                                               
048100         PERFORM CAA-LAES-WDD905                                          
048200       END-IF                                                             
048300     END-IF                                                               
048400     MOVE SPACE                  TO W001-BELEVART                         
048500     PERFORM IMS-GET-WDF501                                               
048600     IF SEGMENT-FINNS                                                     
048700       PERFORM IMS-GET-WDF502                                             
048800       IF SEGMENT-FINNS                                                   
048900         MOVE XLEV-BELEVART      TO W001-BELEVART                         
049000       END-IF                                                             
049100     END-IF                                                               
049200                                                                          
049300     MOVE WS-KVAVROP-TAB     (2) TO W001-KVAVROP-2                        
049400     MOVE WS-TIAVROP-AVS-TAB (2) TO W001-DAAVROP-AVS-2                    
049500     MOVE WS-KVAVROP-TAB     (3) TO W001-KVAVROP-3                        
049600     MOVE WS-TIAVROP-AVS-TAB (3) TO W001-DAAVROP-AVS-3                    
049700                                                                          
049800     IF CLAG-KDUART NOT = 'L'                                             
049900       MOVE W001-DETALJ1         TO W001-RAD                              
050000       PERFORM S21-SKRIV-W21422-001                                       
050100       MOVE W001-DETALJ2         TO W001-RAD                              
050200       PERFORM S21-SKRIV-W21422-001                                       
050300       MOVE W001-DETALJ3         TO W001-RAD                              
050400       PERFORM S21-SKRIV-W21422-001                                       
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800 CAA-LAES-WDD905 SECTION.                                                 
050900                                                                          
051000***************************************************************           
051100**** LÄSER SLÄP (KVANT) OCH AVROP (KVANT-AAVV) FRÅN WDD905.****           
051200***************************************************************           
051300                                                                          
051400     PERFORM IMS-GET-WDD905                                               
051500     MOVE ZERO TO WS-KVAVROP-SLAEP                                        
051600     PERFORM UNTIL WS-TIAVROP-AVS NOT < WS-DAGENS-DATUM-AAVV OR           
051700                   SEGMENT-SAKNAS                                         
051800       IF KDAVROP = 2                                                     
051900          ADD KVAVROP       TO WS-KVAVROP-SLAEP                           
052000       END-IF                                                             
052100       PERFORM IMS-GET-WDD905                                             
052200     END-PERFORM                                                          
052300     MOVE WS-KVAVROP-SLAEP  TO W001-KVAVROP-SLAEP                         
052400     PERFORM UNTIL WS-TIAVROP-AVS NOT <                                   
052500         WS-DAGENS-DATUM-AAVV-3V OR SEGMENT-SAKNAS                        
052600       IF KDAVROP = 2                                                     
052700         IF WS-TIAVROP-AVS-TAB (1) = 0                                    
052800           MOVE KVAVROP                TO WS-KVAVROP-TAB (1)              
052900           MOVE WS-TIAVROP-AVS         TO WS-TIAVROP-AVS-TAB (1)          
053000         ELSE                                                             
053100           IF WS-TIAVROP-AVS = WS-TIAVROP-AVS-TAB (1)                     
053200             ADD KVAVROP TO WS-KVAVROP-TAB (1)                            
053300           ELSE                                                           
053400             IF WS-TIAVROP-AVS-TAB (2) = 0                                
053500               MOVE KVAVROP            TO WS-KVAVROP-TAB (2)              
053600               MOVE WS-TIAVROP-AVS     TO WS-TIAVROP-AVS-TAB (2)          
053700             ELSE                                                         
053800               IF WS-TIAVROP-AVS = WS-TIAVROP-AVS-TAB (2)                 
053900                 ADD KVAVROP           TO WS-KVAVROP-TAB (2)              
054000               ELSE                                                       
054100                 IF WS-TIAVROP-AVS-TAB (3) = 0                            
054200                   MOVE KVAVROP        TO WS-KVAVROP-TAB (3)              
054300                   MOVE WS-TIAVROP-AVS TO WS-TIAVROP-AVS-TAB (3)          
054400                 ELSE                                                     
054500                   IF WS-TIAVROP-AVS = WS-TIAVROP-AVS-TAB (3)             
054600                     ADD KVAVROP       TO WS-KVAVROP-TAB (3)              
054700                   END-IF                                                 
054800                 END-IF                                                   
054900               END-IF                                                     
055000             END-IF                                                       
055100           END-IF                                                         
055200         END-IF                                                           
055300       END-IF                                                             
055400       PERFORM IMS-GET-WDD905                                             
055500     END-PERFORM                                                          
055600     MOVE WS-KVAVROP-TAB     (1) TO W001-KVAVROP-1                        
055700     MOVE WS-TIAVROP-AVS-TAB (1) TO W001-DAAVROP-AVS-1                    
055800     .                                                                    
055900     EJECT                                                                
056000 Z-FINIT SECTION.                                                         
056100     CLOSE W21411                                                         
056200           W21422-001                                                     
056300     SKIP2                                                                
056400     MOVE 'S' TO POSTSUM-OPKOD                                            
056500     CALL POSTSUM USING POSTSUM-PARM                                      
056600     .                                                                    
056700     EJECT                                                                
056800 S01-LAES-W21411  SECTION.                                                
056900     READ W21411 INTO IN-AREA                                             
057000     AT END                                                               
057100        MOVE HIGH-VALUE TO IN-AREA                                        
057200        SET END-OF-W21411 TO TRUE                                         
057300                                                                          
057400     NOT AT END                                                           
057500        MOVE 'W21411' TO POSTSUM-FDNAMN                                   
057600        MOVE 'W21422D1' TO POSTSUM-DDNAMN2                                
057700        MOVE SPACE TO POSTSUM-TRANSTYP                                    
057800        CALL POSTSUM USING POSTSUM-PARM                                   
057900     END-READ                                                             
058000     .                                                                    
058100     EJECT                                                                
058200 S21-SKRIV-W21422-001  SECTION.                                           
058300                                                                          
058400     MOVE 1 TO W001-SKIP                                                  
058500     IF W001-RADRAKNARE > W001-MAX-RADER-PER-SIDA OR                      
058600        WS-IDANSK-GAM < SORTWS-IDANSK                                     
058700       PERFORM S21A-SKRIV-RUBRIKER                                        
058800     END-IF                                                               
058900     SKIP2                                                                
059000     WRITE W21422-001-RAD FROM W001-RAD AFTER W001-SKIP                   
059100     MOVE SORTWS-IDANSK TO WS-IDANSK-GAM                                  
059200     SKIP2                                                                
059300     MOVE SPACE TO W001-RAD                                               
059400     ADD  +1 TO W001-RADRAKNARE                                           
059500     .                                                                    
059600     EJECT                                                                
059700 S21A-SKRIV-RUBRIKER SECTION.                                             
059800                                                                          
059900     ADD +1 TO W001-SIDRAKNARE                                            
060000     MOVE W001-SIDRAKNARE TO W001-SID                                     
060100     WRITE W21422-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
060200     WRITE W21422-001-RAD FROM W001-RUBRIK2 AFTER 2                       
060300     WRITE W21422-001-RAD FROM W001-RUBRIK3                               
060400     WRITE W21422-001-RAD FROM W001-RUBRIK4                               
060500     MOVE +7 TO W001-RADRAKNARE                                           
060600     SKIP2                                                                
060700     MOVE 2 TO W001-SKIP                                                  
060800     .                                                                    
060900     EJECT                                                                
061000 S31-SORT-RELEASE  SECTION.                                               
061100                                                                          
061200     RELEASE SORT-POST FROM SORTWS-AREA                                   
061300     .                                                                    
061400     EJECT                                                                
061500 S32-SORT-RETURN  SECTION.                                                
061600                                                                          
061700     RETURN SORTFIL INTO SORTWS-AREA                                      
061800     AT END                                                               
061900         SET END-OF-SORTFIL TO TRUE                                       
062000     .                                                                    
062100     EJECT                                                                
062200 S99-ABEND SECTION.                                                       
062300                                                                          
062400     SKIP2                                                                
062500     MOVE 'S' TO POSTSUM-OPKOD                                            
062600     CALL POSTSUM USING POSTSUM-PARM                                      
062700     CALL ABEND USING RKOD-ABEND                                          
062800     .                                                                    
062900     EJECT                                                                
063000 IMS-GET-WDK601 SECTION.                                                  
063100                                                                          
063200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
063300          DELIMITED BY SIZE INTO SSA1                                     
063400     MOVE '  GE' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
063600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     EJECT                                                                
064000 IMS-GET-WDK611 SECTION.                                                  
064100                                                                          
064200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
064300          DELIMITED BY SIZE INTO SSA1                                     
064400     MOVE '  GE' TO GODK-STATUSKODER                                      
064500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
064600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
064700     PERFORM IMS-STATUSKONTROLL                                           
064800     .                                                                    
064900     EJECT                                                                
065000 IMS-GET-WDF501 SECTION.                                                  
065100                                                                          
065200     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
065300          DELIMITED BY SIZE INTO SSA1                                     
065400     MOVE '  GE' TO GODK-STATUSKODER                                      
065500     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
065600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
065700     PERFORM IMS-STATUSKONTROLL                                           
065800     .                                                                    
065900     EJECT                                                                
066000 IMS-GET-WDF502 SECTION.                                                  
066100                                                                          
066200     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
066300          DELIMITED BY SIZE INTO SSA1                                     
066400     MOVE '  GE' TO GODK-STATUSKODER                                      
066500     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-WDF502 SSA1                   
066600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900     EJECT                                                                
067000 IMS-GET-WDD901 SECTION.                                                  
067100                                                                          
067200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
067300          DELIMITED BY SIZE INTO SSA1                                     
067400     MOVE '  GE' TO GODK-STATUSKODER                                      
067500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
067600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
067700     PERFORM IMS-STATUSKONTROLL                                           
067800     .                                                                    
067900     EJECT                                                                
068000 IMS-GET-WDD902 SECTION.                                                  
068100                                                                          
068200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
068300          DELIMITED BY SIZE INTO SSA1                                     
068400     MOVE '  GE' TO GODK-STATUSKODER                                      
068500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
068600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900     EJECT                                                                
069000 IMS-GET-WDD905 SECTION.                                                  
069100                                                                          
069200     STRING 'WDD905     '                                                 
069300          DELIMITED BY SIZE INTO SSA1                                     
069400     MOVE '  GE' TO GODK-STATUSKODER                                      
069500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
069600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     IF SEGMENT-FINNS                                                     
069900       MOVE DAAVROP-AVS TO WS-DAAVROP-AVS                                 
070000     ELSE                                                                 
070100       MOVE ZEROES      TO WS-DAAVROP-AVS                                 
070200     END-IF                                                               
070300     .                                                                    
070400     EJECT                                                                
070500 IMS-STATUSKONTROLL SECTION.                                              
070600                                                                          
070700     SET STATUS-IX TO 1                                                   
070800     SEARCH GODK-STATUS                                                   
070900       AT END                                                             
071000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
071100           DELIMITED BY SIZE INTO FELTEXT                                 
071200         DISPLAY FELTEXT                                                  
071300         CALL FELLOG                                                      
071400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
071500         CONTINUE                                                         
071600     END-SEARCH                                                           
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
