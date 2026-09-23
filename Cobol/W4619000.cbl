000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4619000.                                                 
001000*AUTHOR.        STIG MULLER.                                              
001100*DATE-WRITTEN.  JAN 1985.                                                 
001200                                                                          
001210*                                                                         
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        SKAPAR LISTA PÅ ÖVERSÄNDA TRANSAR TILL VIPS.                     
001800*        LOGGAR ANTAL TRANSAR                                             
001900*                                                                         
002000*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - INFIL:                                           
002900*                        - -  FIL TILL VIPS                               
003000     SELECT INFIL                        ASSIGN TO UT-S-W46190D1.         
003100     SKIP2                                                                
003200*- - - - - - - - - - - - LISTA:                                           
003300*                        - -  TILL IMPORTÖR                               
003400     SELECT W46190-001                   ASSIGN TO UT-S-W46190D2.         
003500     SKIP2                                                                
003600*- - - - - - - - - - - - UTFIL:                                           
003700*                        - -  LOGG AV SKICKADE TRANSAR                    
003800     SELECT W46191                       ASSIGN TO UT-S-W46190D3.         
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  INFIL                                                                
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800 01  INPOST                       PIC X(80).                              
004900     SKIP2                                                                
005000 FD  W46190-001                                                           
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS 0.                                                    
005300     SKIP2                                                                
005400 01  UT-RAD                      PIC X(121).                              
005500     SKIP2                                                                
005600 FD  W46191                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS 0.                                                    
005900     SKIP2                                                                
006000*01  UT-POST -COPY W46191     -L.                                         
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006301                                                                          
006310*    -- CHECKED BY WY2000                                                 
006400 77  IDPGM                       PIC X(8)    VALUE 'W4619000'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007720     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
008100     SKIP2                                                                
008200*- - - - - - - - - - - - - - FRAMMATNINGS PARAMETRAR                      
008300                                                                          
008400 77  ARB-SIDNR                   PIC S9(5)   COMP-3 VALUE +0.             
008500 77  STYR-TECKEN                 PIC S9.                                  
008600     EJECT                                                                
008700 01  DAGENS-DATUM.                                                        
008800     03 DAGENS-DATUM-AR          PIC 9(2).                                
008900     03 DAGENS-DATUM-MANAD       PIC 9(2).                                
009000     03 DAGENS-DATUM-DAG         PIC 9(2).                                
009100                                                                          
009200 01  DYNAMISKA-SUBPROGRAM.                                                
009300     03 ABEND                    PIC X(8)    VALUE 'ABEND'.               
009400     03 DATKORT                  PIC X(8)    VALUE 'DATKORT'.             
009500     03 POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.             
009600     03 WDSINFO                  PIC X(8)    VALUE 'WDSINFO'.             
009700     SKIP3                                                                
009800*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009900                                                                          
010000 01  RETURKODER.                                                          
010100     03 RKOD                     PIC S9(4)  COMP SYNC VALUE ZERO.         
010200     03 RKOD-ABEND-UTAN-DUMP     PIC S9(4)  COMP SYNC VALUE +16.          
010300     03 RKOD-ABEND-MED-DUMP      PIC S9(4)  COMP SYNC VALUE +1000.        
010400     EJECT                                                                
010500*- - - - - - - - - - - - - -  ARBETSAREAOR                                
010600                                                                          
010700 01  IN-AREA.                                                             
010800     03 IN-IDPTYP              PIC X(3).                                  
010900     03 FILLER                 PIC X(77).                                 
011000     SKIP2                                                                
011100*                             STARTKORT                                   
011200*01  FILLER -COPY W461RI0    -RED IN-AREA -PRE IN-.                       
011400     EJECT                                                                
011500*                             SLUTKORT                                    
011600*01  FILLER -COPY W461RI9    -RED IN-AREA -PRE IN-.                       
011800     EJECT                                                                
011900*01  W46191-AREA -COPY W46191.                                            
012100     EJECT                                                                
012200 01  TEST-IDDISTR PIC 9(5) COMP-3.                                        
012300 SKIP2                                                                    
012700 01  TABELL.                                                              
012800     03 IDPTYP-VARDEN                 PIC X(84) VALUE                     
012900                 'RIARIBRIDRIERIFRIGRIHRIIRIJRIKRILRIMRINRIORIPRIQ        
013000-    'RIRRISRITRIWRIXRIYRKARKBRKCRKDRKERKF'.                              
013100     03 IDPTYP REDEFINES IDPTYP-VARDEN OCCURS 28 PIC X(3).                
013200     03 SUMMOR-BEGYNNELSE-VARDEN.                                         
013300        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
013400        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
013500        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
013600        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
013700        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
013800        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
013900        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014000        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014100        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014200        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014300        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014400        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014500        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014600        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014700        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014800        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
014900        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015000        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015100        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015200        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015300        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015400        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015500        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015600        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015700        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015800        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
015900        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
016000        05 FILLER                     PIC S9(5) COMP-3 VALUE ZERO.        
016100     03 SUMMOR REDEFINES SUMMOR-BEGYNNELSE-VARDEN.                        
016200        05 IDPTYP-SUMMA OCCURS 28     PIC S9(5) COMP-3.                   
016300     EJECT                                                                
016400 01  BESKR-TABELL-AREA.                                                   
016500  03 BESKR-TABELL.                                                        
016600     05  BESKR-RIA                    PIC X(38) VALUE                     
016700     'ATTACHED BACKORDER                    '.                            
016800     05  BESKR-RIB                    PIC X(38) VALUE                     
016900     'ORDER CONF. ORDER HEADER              '.                            
017000     05  BESKR-RID                    PIC X(38) VALUE                     
017100     'ORDER CONF. REPLACEMENT               '.                            
017200     05  BESKR-RIE                    PIC X(38) VALUE                     
017300     'ORDER CONF. VARIABLE REPLACEMENT      '.                            
017400     05  BESKR-RIF                    PIC X(38) VALUE                     
017500     'ORDER CONF. VARIABLE REPLACEMENT- TEXT'.                            
017600     05  BESKR-RIG                    PIC X(38) VALUE                     
017700     'ORDER CONF. QUANTITY ADAPTION         '.                            
017800     05  BESKR-RIH                    PIC X(38) VALUE                     
017900     'ORDER CONF. AMMMENDMENT OF QUANTITY   '.                            
018000     05  BESKR-RII                    PIC X(38) VALUE                     
018100     'ORDER CONF. CANCELED ORDERLINES       '.                            
018200     05  BESKR-RIJ                    PIC X(38) VALUE                     
018300     'SERVICE DEGREE                        '.                            
018400     05  BESKR-RIK                    PIC X(38) VALUE                     
018500     'INVOICE HEADER. TRANS 1               '.                            
018600     05  BESKR-RIL                    PIC X(38) VALUE                     
018700     'INVOICE HEADER. TRANS 2               '.                            
018800     05  BESKR-RIM                    PIC X(38) VALUE                     
018900     'INVOICE REFERENCE                     '.                            
019000     05  BESKR-RIN                    PIC X(38) VALUE                     
019100     'INVOICE CASE                          '.                            
019200     05  BESKR-RIO                    PIC X(38) VALUE                     
019300     'INVOICE LINE, TRANS 1                 '.                            
019400     05  BESKR-RIP                    PIC X(38) VALUE                     
019500     'INVOICE LINE, TRANS 2                 '.                            
019600     05  BESKR-RIQ                    PIC X(38) VALUE                     
019700     'PACKING PROFORMA                      '.                            
019800     05  BESKR-RIR                    PIC X(38) VALUE                     
019900     'CANCELATION TRANSACTION               '.                            
020000     05  BESKR-RIS                    PIC X(38) VALUE                     
020100     'PARTS INFORMATION                     '.                            
020200     05  BESKR-RIT                    PIC X(38) VALUE                     
020300     'DSP ORDER LINE                        '.                            
020400     05  BESKR-RIW                    PIC X(38) VALUE                     
020500     'DSP ORDER RECONCILIATION              '.                            
020600     05  BESKR-RIX                    PIC X(38) VALUE                     
020700     'ON-ORDER HEADER                       '.                            
020800     05  BESKR-RIY                    PIC X(38) VALUE                     
020900     'ON-ORDER LINE                         '.                            
021000     05  BESKR-RKA                    PIC X(38) VALUE                     
021100     'PARTS DESCRIPTION                     '.                            
021200     05  BESKR-RKB                    PIC X(38) VALUE                     
021300     'CREDITING HEADER                      '.                            
021400     05  BESKR-RKC                    PIC X(38) VALUE                     
021500     'CREDITING LINE                        '.                            
021600     05  BESKR-RKD                    PIC X(38) VALUE                     
021700     'CREDITING ADDITIONAL COSTS            '.                            
021800     05  BESKR-RKE                    PIC X(38) VALUE                     
021900     'CREDITING TEXT-STRING                 '.                            
022000     05  BESKR-RKF                    PIC X(38) VALUE                     
022100     'EXCHANGE-TABLE                        '.                            
022200                                                                          
022300  03 BESKR-AREA-RED REDEFINES BESKR-TABELL.                               
022400     05  BESKR-AREA OCCURS 28         PIC X(38).                          
022500     EJECT                                                                
022600 01  NUMMER                           PIC S9(3) COMP-3.                   
022700 01  W-TOTAL-SUMMA                    PIC S9(7) COMP-3 VALUE ZERO.        
022800     EJECT                                                                
022900******************************************************************        
023000*    LIST AREA                                                   *        
023100******************************************************************        
023200 01  FILLER                      PIC X(24)   VALUE                        
023300                                            'LIST-AREA-START    '.        
023400     SKIP1                                                                
023500 01  HJALPAREOR.                                                          
023600*                                                                         
023700     03 MAX-RADER-PER-SIDA       PIC 9(3)    VALUE 42.                    
023800     03 MAX-POSITIONER-PER-RAD   PIC 9(3)    VALUE 121.                   
023900     03 LISTNR                   PIC X(11)   VALUE 'W46165-001'.          
024000     03 ANTAL-SKRIVNA-SIDOR      PIC S9(5)   COMP-3 VALUE ZERO.           
024100     EJECT                                                                
024200 01  RAD                     PIC X(121).                                  
024300     SKIP2                                                                
026600 01  SID-RUBRIK-1.                                                        
026700     03 FILLER               PIC X(2)  VALUE SPACE.                       
026800     03 FILLER               PIC X(11) VALUE 'CAR PARTS'.                 
026900     03 FILLER               PIC X(6)  VALUE SPACE.                       
027000     03 FILLER               PIC X(10) VALUE 'W46190-001'.                
027100     03 FILLER               PIC X(5)  VALUE SPACE.                       
027200     03 FILLER               PIC X(33) VALUE                              
027300                              'TRANSACTIONS SENT BACK TO VIPS   '.        
027400     03 FILLER               PIC X(5)  VALUE SPACE.                       
027500     03 FILLER               PIC X(4)  VALUE 'REF.'.                      
027600     03 FILLER               PIC X(2)  VALUE SPACE.                       
027700     03 REFERENS             PIC 9(4).                                    
027800     03 FILLER               PIC X(10) VALUE SPACE.                       
027900     03 FILLER               PIC X(4)  VALUE 'DATE'.                      
028000     03 FILLER               PIC X(4)  VALUE SPACE.                       
028100     03 DATUM-DAGENS         PIC X(6).                                    
028200     03 FILLER               PIC X(3)  VALUE SPACE.                       
028300     03 FILLER               PIC X(4)  VALUE 'PAGE'.                      
028400     03 FILLER               PIC X(1)  VALUE SPACE.                       
028500     03 SIDA                 PIC Z(4)9 VALUE '1'.                         
028600     03 FILLER               PIC X(2)  VALUE SPACE.                       
028700     EJECT                                                                
028800 01  SID-RUBRIK-2.                                                        
028900     03 FILLER               PIC X(19)  VALUE SPACE.                      
029000     03 FILLER               PIC X(6)   VALUE 'DISTR'.                    
029100     03 FILLER               PIC X(2)   VALUE SPACE.                      
029200     03 DISTRIKT             PIC Z(4)9.                                   
029300     03 FILLER               PIC X(4)   VALUE SPACE.                      
029400     03 FILLER               PIC X(4)   VALUE 'DATE'.                     
029500     03 FILLER               PIC X(2)   VALUE SPACE.                      
029600     03 DATUM-FILENS         PIC X(6).                                    
029700     03 FILLER               PIC X(4)   VALUE SPACE.                      
029800     03 FILLER               PIC X(4)   VALUE 'TIME'.                     
029900     03 FILLER               PIC X(2)   VALUE SPACE.                      
030000     03 TID-FILENS           PIC X(6).                                    
030100     03 FILLER               PIC X(57)  VALUE SPACE.                      
030200     EJECT                                                                
030300 01  RAD-RUBRIK.                                                          
030400     03 FILLER               PIC X(9)   VALUE SPACE.                      
030500     03 FILLER               PIC X(11)  VALUE 'RECORD TYPE'.              
030600     03 FILLER               PIC X(3)   VALUE SPACE.                      
030700     03 FILLER               PIC X(8)   VALUE 'NO. REC.'.                 
030800     03 FILLER               PIC X(9)   VALUE SPACE.                      
030900     03 FILLER               PIC X(11)  VALUE 'DESCRIPTION'.              
031000     03 FILLER               PIC X(80)  VALUE SPACE.                      
031100     SKIP3                                                                
031200 01  RAD-VARDEN.                                                          
031300     03 FILLER               PIC X(9)   VALUE SPACE.                      
031400     03 POSTTYP              PIC X(3).                                    
031500     03 FILLER               PIC X(14)  VALUE SPACE.                      
031600     03 POSTTYP-SUMMA        PIC Z(4)9.                                   
031700     03 FILLER               PIC X(9)   VALUE SPACE.                      
031800     03 BESKRIVNING          PIC X(38).                                   
031900     03 FILLER               PIC X(53)  VALUE SPACE.                      
032000     SKIP3                                                                
032100 01  TOTAL-VARDEN.                                                        
032200     03 FILLER               PIC X(9)   VALUE SPACE.                      
032300     03 FILLER               PIC X(5)   VALUE 'TOTAL'.                    
032400     03 FILLER               PIC X(12)  VALUE SPACE.                      
032500     03 TOTAL-SUMMA          PIC Z(4)9.                                   
032600     03 FILLER               PIC X(100) VALUE SPACE.                      
032700     EJECT                                                                
032800*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
032900                                                                          
033000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
033100     SKIP2                                                                
033200*01  -COPY WDATKORT                                                       
033400     EJECT                                                                
033500*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
033600                                                                          
033700*01  -COPY W0005       -PRE  POSTSUM-.                                    
033900     EJECT                                                                
034000*---------------------------LÄNKAREA                                      
034100*01  -COPY WDSAREA.                                                       
034300     EJECT                                                                
034400 PROCEDURE DIVISION.                                                      
034500     SKIP2                                                                
034600     PERFORM A-DATUM                                                      
034700     PERFORM B-INIT                                                       
034800     MOVE 'W46190D1' TO DDNAME                                            
034900     CALL WDSINFO USING WDSAREA                                           
035000     IF KDSVAR-OK                                                         
035200       MOVE IDGEN TO REFERENS LOGG-IDGEN                                  
035300       PERFORM C-RAEKNA                                                   
035400       PERFORM D-SKRIV-LISTA                                              
035500       PERFORM E-SKRIV-LOGG                                               
035600     ELSE                                                                 
035700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
035800     END-IF                                                               
035900     PERFORM Z-FINIT                                                      
036000     MOVE ZERO TO RETURN-CODE                                             
036100     GOBACK                                                               
036200     CONTINUE.                                                            
036300     EJECT                                                                
036400 A-DATUM SECTION.                                                         
036500*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
036600     SKIP2                                                                
036700     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
036800     MOVE D-AAR TO DAGENS-DATUM-AR                                        
036900     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
037000     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
037200     MOVE DAGENS-DATUM TO DATUM-DAGENS                                    
037300     CONTINUE.                                                            
037400     EJECT                                                                
037500 B-INIT SECTION.                                                          
037600     SKIP2                                                                
037700     OPEN INPUT INFIL OUTPUT W46190-001 W46191                            
037800     SKIP2                                                                
037900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
038000     CONTINUE.                                                            
038100     EJECT                                                                
038200 C-RAEKNA SECTION.                                                        
038300     SKIP2                                                                
038400     PERFORM S01-LAS-INFIL                                                
038500     PERFORM UNTIL                                                        
038600      ( INFIL-EOF = JA )                                                  
038700       IF IN-IDPTYP = 'RIA'                                               
038800         ADD +1 TO IDPTYP-SUMMA (1)                                       
038900       ELSE                                                               
039000         EVALUATE TRUE                                                    
039100         WHEN IN-IDPTYP = 'RIB'                                           
039200           ADD +1 TO IDPTYP-SUMMA (2)                                     
039300         WHEN IN-IDPTYP = 'RID'                                           
039400           ADD +1 TO IDPTYP-SUMMA (3)                                     
039500         WHEN IN-IDPTYP = 'RIE'                                           
039600           ADD +1 TO IDPTYP-SUMMA (4)                                     
039700         WHEN IN-IDPTYP = 'RIF'                                           
039800           ADD +1 TO IDPTYP-SUMMA (5)                                     
039900         WHEN IN-IDPTYP = 'RIG'                                           
040000           ADD +1 TO IDPTYP-SUMMA (6)                                     
040100         WHEN IN-IDPTYP = 'RIH'                                           
040200           ADD +1 TO IDPTYP-SUMMA (7)                                     
040300         WHEN IN-IDPTYP = 'RII'                                           
040400           ADD +1 TO IDPTYP-SUMMA (8)                                     
040500         WHEN IN-IDPTYP = 'RIJ'                                           
040600           ADD +1 TO IDPTYP-SUMMA (9)                                     
040700         WHEN IN-IDPTYP = 'RIK'                                           
040800           ADD +1 TO IDPTYP-SUMMA (10)                                    
040900         WHEN IN-IDPTYP = 'RIL'                                           
041000           ADD +1 TO IDPTYP-SUMMA (11)                                    
041100         WHEN IN-IDPTYP = 'RIM'                                           
041200           ADD +1 TO IDPTYP-SUMMA (12)                                    
041300         WHEN IN-IDPTYP = 'RIN'                                           
041400           ADD +1 TO IDPTYP-SUMMA (13)                                    
041500         WHEN IN-IDPTYP = 'RIO'                                           
041600           ADD +1 TO IDPTYP-SUMMA (14)                                    
041700         WHEN IN-IDPTYP = 'RIP'                                           
041800           ADD +1 TO IDPTYP-SUMMA (15)                                    
041900         WHEN IN-IDPTYP = 'RIQ'                                           
042000           ADD +1 TO IDPTYP-SUMMA (16)                                    
042100         WHEN IN-IDPTYP = 'RIR'                                           
042200           ADD +1 TO IDPTYP-SUMMA (17)                                    
042300         WHEN IN-IDPTYP = 'RIS'                                           
042400           ADD +1 TO IDPTYP-SUMMA (18)                                    
042500         WHEN IN-IDPTYP = 'RIT'                                           
042600           ADD +1 TO IDPTYP-SUMMA (19)                                    
042700         WHEN IN-IDPTYP = 'RIW'                                           
042800           ADD +1 TO IDPTYP-SUMMA (20)                                    
042900         WHEN IN-IDPTYP = 'RIX'                                           
043000           ADD +1 TO IDPTYP-SUMMA (21)                                    
043100         WHEN IN-IDPTYP = 'RIY'                                           
043200           ADD +1 TO IDPTYP-SUMMA (22)                                    
043300         WHEN IN-IDPTYP = 'RKA'                                           
043400           ADD +1 TO IDPTYP-SUMMA (23)                                    
043500         WHEN IN-IDPTYP = 'RKB'                                           
043600           ADD +1 TO IDPTYP-SUMMA (24)                                    
043700         WHEN IN-IDPTYP = 'RKC'                                           
043800           ADD +1 TO IDPTYP-SUMMA (25)                                    
043900         WHEN IN-IDPTYP = 'RKD'                                           
044000           ADD +1 TO IDPTYP-SUMMA (26)                                    
044100         WHEN IN-IDPTYP = 'RKE'                                           
044200           ADD +1 TO IDPTYP-SUMMA (27)                                    
044300         WHEN IN-IDPTYP = 'RKF'                                           
044400           ADD +1 TO IDPTYP-SUMMA (28)                                    
044500         WHEN IN-IDPTYP = 'RI0'                                           
044600           MOVE IN-START-IDDISTR     TO DISTRIKT                          
044700                                        LOGG-IDDISTR                      
044800                                        TEST-IDDISTR                      
044900           MOVE IN-START-TIFILDAT    TO DATUM-FILENS                      
045000                                        LOGG-TIFILDAT                     
045100           MOVE IN-START-TIHHMMSS    TO TID-FILENS                        
045200                                        LOGG-TIHHMMSS                     
045300         WHEN IN-IDPTYP = 'RI9'                                           
045400           MOVE IN-SLUT-KVTRANS      TO LOGG-KVTRANS                      
045500         END-EVALUATE                                                     
045600       END-IF                                                             
045700       PERFORM S01-LAS-INFIL                                              
045800     END-PERFORM                                                          
045900     CONTINUE.                                                            
046000     EJECT                                                                
046100 D-SKRIV-LISTA SECTION.                                                   
046200     SKIP2                                                                
046600     MOVE SID-RUBRIK-1               TO RAD                               
046800     MOVE +9                         TO STYR-TECKEN                       
046900     PERFORM S10-SKRIV-RAD                                                
047000     MOVE SID-RUBRIK-2               TO RAD                               
047100     MOVE +2                         TO STYR-TECKEN                       
047200     PERFORM S10-SKRIV-RAD                                                
047300     MOVE RAD-RUBRIK                 TO RAD                               
047400     MOVE +3                         TO STYR-TECKEN                       
047500     PERFORM S10-SKRIV-RAD                                                
047600     MOVE +2                         TO STYR-TECKEN                       
047700     MOVE 1                          TO NUMMER                            
047800     PERFORM UNTIL                                                        
047900      ( NUMMER > 28 )                                                     
048000       MOVE IDPTYP (NUMMER)         TO POSTTYP                            
048100       MOVE IDPTYP-SUMMA (NUMMER)   TO POSTTYP-SUMMA                      
048200       MOVE BESKR-AREA (NUMMER)     TO BESKRIVNING                        
048300       ADD  IDPTYP-SUMMA (NUMMER)   TO W-TOTAL-SUMMA                      
048400       MOVE RAD-VARDEN              TO RAD                                
048500       PERFORM S10-SKRIV-RAD                                              
048600       MOVE +1                      TO STYR-TECKEN                        
048700       ADD 1                        TO NUMMER                             
048800     END-PERFORM                                                          
048900     MOVE W-TOTAL-SUMMA TO TOTAL-SUMMA                                    
049000     MOVE TOTAL-VARDEN  TO RAD                                            
049100     MOVE +2            TO STYR-TECKEN                                    
049200     PERFORM S10-SKRIV-RAD                                                
049300     CONTINUE.                                                            
049400     EJECT                                                                
049500 E-SKRIV-LOGG SECTION.                                                    
049600     SKIP2                                                                
049700     WRITE UT-POST FROM W46191-AREA                                       
049800     MOVE 'W46191'               TO POSTSUM-FDNAMN                        
049900     MOVE 'W46190D3'             TO POSTSUM-DDNAMN2                       
050000     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
050100     CALL POSTSUM                USING POSTSUM-PARM                       
050200     CONTINUE.                                                            
050300     EJECT                                                                
050400 S01-LAS-INFIL SECTION.                                                   
050500     SKIP2                                                                
050600     READ   INFIL INTO IN-AREA                                            
050700     AT END MOVE JA TO INFIL-EOF                                          
050800     END-READ                                                             
050900                                                                          
051000     IF INFIL-EOF = NEJ                                                   
051100                                                                          
051200       MOVE 'W46190'            TO POSTSUM-FDNAMN                         
051300       MOVE 'W46190D1'          TO POSTSUM-DDNAMN2                        
051400       MOVE SPACE               TO POSTSUM-TRANSTYP                       
051500       CALL POSTSUM             USING POSTSUM-PARM                        
051600                                                                          
051700     END-IF                                                               
051800     CONTINUE.                                                            
051900     EJECT                                                                
052000 S10-SKRIV-RAD SECTION.                                                   
052100     SKIP2                                                                
052200     IF STYR-TECKEN = +9                                                  
052300       WRITE UT-RAD FROM RAD AFTER ADVANCING PAGE                         
052400     ELSE                                                                 
052500       WRITE UT-RAD FROM RAD AFTER ADVANCING STYR-TECKEN                  
052600     END-IF                                                               
052700     MOVE SPACE TO RAD                                                    
052800     CONTINUE.                                                            
052900     EJECT                                                                
053000 Z-FINIT SECTION.                                                         
053100     SKIP2                                                                
053200                                                                          
053300     CLOSE INFIL W46190-001 W46191                                        
053400     SKIP2                                                                
053500*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
053600*                                    SKRIVNA POSTER                       
053700                                                                          
053800     MOVE 'S' TO POSTSUM-OPKOD                                            
053900     CALL POSTSUM USING POSTSUM-PARM                                      
054000     CONTINUE.                                                            
054300                                                                          
