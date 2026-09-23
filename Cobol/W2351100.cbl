000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2351100.                                                
000300 AUTHOR.         INGER STENING.                                           
000400 DATE-WRITTEN.   04/12/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PGM SKAPAR FILER MED HJÄLP AV DATA FRÅN BILD 2423                
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK7                                       
001200*        PROGRAMMET LÄSER      WDD9                                       
001300*        PROGRAMMET LÄSER      WDF5                                       
001310*        PROGRAMMET LÄSER      WDB6                                       
001320*        PROGRAMMET LÄSER      WDK6                                       
001330*        PROGRAMMET LÄSER      W6D1                                       
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- PARAMETRAR FRÅN SOP                                        
002400     SELECT W235PP                     ASSIGN TO W23511D1.                
002500     SKIP2                                                                
002600*          --- UTFIL FÖR LEVERANSBESKED                                   
002700     SELECT W2351101                   ASSIGN TO W23511D2.                
002800     SKIP2                                                                
002900*          --- UTFIL FÖR AVROP                                            
003000     SELECT W2351102                   ASSIGN TO W23511D3.                
003100     SKIP2                                                                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W235PP                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100 01  PARM                PIC X(80).                                       
004200     SKIP3                                                                
004300 FD  W2351101                                                             
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  RECORD -COPY W2351101 -PRE  W2351101- -L.                            
004800     SKIP3                                                                
004900 FD  W2351102                                                             
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  RECORD -COPY W2351102 -PRE  W2351102- -L.                            
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W2351100'.            
005701                                                                          
005710 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005720 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005730                                                                          
005800 77  YES                         PIC X       VALUE 'J'.                   
005810 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  SW-SKRIV                    PIC X       VALUE 'N'.                   
006100 77  SW-FORSTA                   PIC X       VALUE 'N'.                   
006200 77  SW-FLER-LEVBSK              PIC X       VALUE 'N'.                   
006300 77  IX                          PIC S9(2)   VALUE ZERO.                  
006400 77  TELEV-IX                    PIC S9(2)   VALUE ZERO.                  
006500                                                                          
006600*01  -COPY WWDCKONS                                                       
006700                                                                          
006800 77  W235PP-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W235PP                       VALUE 'J'.                   
007000                                                                          
007100     SKIP2                                                                
007200 01  WS-AREA.                                                             
008400     03  W-KVART-TOT-C1          PIC S9(9)   VALUE ZERO COMP-3.           
008500     03  WS-KVAVIS               PIC S9(7)   VALUE ZERO COMP-3.           
008700     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
008900     03  WS-IDANSK               PIC 9(3)    VALUE ZERO.                  
009100                                                                          
009200*      --- VALID IDDC CODES                                               
009300*01    -COPY WWDC99                                                       
009400                                                                          
009500 01  WS-TIAAVVD                  PIC 9(7)    VALUE ZERO.                  
009600 01  FILLER REDEFINES WS-TIAAVVD.                                         
009700     03 WS-SEKEL                 PIC 9(2).                                
009800     03 WS-TIAAVV                PIC 9(4).                                
009900     03 WS-TID                   PIC 9(1).                                
010000                                                                          
010100 01  DAGENS-TIAAVVD              PIC 9(7)    VALUE ZERO.                  
010200 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
010300     03 DAGENS-SEKEL             PIC 9(2).                                
010400     03 DAGENS-TIAAVV            PIC 9(4).                                
010500     03 DAGENS-TID               PIC 9(1).                                
010600                                                                          
010700 01  WS-905-TIAAAAVVD            PIC 9(7)    VALUE ZERO.                  
010800 01  FILLER REDEFINES WS-905-TIAAAAVVD.                                   
010900     03 WS-905-TIAAAAVV          PIC 9(6).                                
011000     03 WS-905-TID               PIC 9(1).                                
011100                                                                          
011200                                                                          
011300 01  WS-AAAAVVD                  PIC 9(7)    VALUE ZERO.                  
011400 01  FILLER REDEFINES WS-AAAAVVD.                                         
011500     03  WS-AAAAVV               PIC 9(6).                                
011600     03  WS-DAG                  PIC 9(1).                                
011700                                                                          
011800 01  FELTEXT.                                                             
011900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012100     EJECT                                                                
012200                                                                          
012300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012400 01  FILLER REDEFINES DAGENS-DATUM.                                       
012500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012800     EJECT                                                                
012900                                                                          
013000 01  W009VADD-AREA.                                                       
013100     03  VADD-DATUM-AAVV         PIC S9(5) VALUE ZERO COMP-3.             
013200     03  VADD-ANTAL              PIC S9(3) VALUE ZERO COMP-3.             
013300                                                                          
013400 01  DYNAMISKA-SUBPROGRAM.                                                
013500*                                                                         
013600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
014000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
014200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
014400     EJECT                                                                
014500*    --- PARAMETRAR TILL ABEND                                            
014600                                                                          
014700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014801                                                                          
014810 01  ERROR-TEXT.                                                          
014820     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
014830     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
014900*    --- PARAMETRAR TILL DATKORT                                          
015000*                                                                         
015100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23511'.              
015110                                                                          
015300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
015500*01  -COPY WDATKORT                                                       
015600     EJECT                                                                
015700*01  -COPY WORKAREA                                                       
015800     EJECT                                                                
015900*01  -COPY WDATAREA                                                       
016000     EJECT                                                                
016100*    --- PARAMETRAR TILL POSTSUM                                          
016200*                                                                         
016300*01  -COPY W0005   -PRE  POSTSUM-                                         
016400     EJECT                                                                
016500                                                                          
016600 01  PARM-AREA-START             PIC X(24)   VALUE                        
016700                                 'PARM-AREA-START  '.                     
016800                                                                          
016900 01  PARM-AREA.                                                           
017000     03  PARM-IDANSK-FOM         PIC X(3).                                
017100     03  PARM-IDANSK-TOM         PIC X(3).                                
017200     03  PARM-IDLEVNR            PIC X(5).                                
017300     03  PARM-IDDC               PIC X(2).                                
017400     03  PARM-AVROP-VV           PIC X(2).                                
017500     03  PARM-FLSLAP             PIC X(1).                                
017600     03  PARM-FLLEVBESK          PIC X(1).                                
017700     03  PARM-MAIL               PIC X(57).                               
017800     03  FILLER                  PIC X(6).                                
017900 01  FILLER   REDEFINES PARM-AREA.                                        
018000     03  PNUM-IDANSK-FOM         PIC 9(3).                                
018100     03  PNUM-IDANSK-TOM         PIC 9(3).                                
018200     03  PNUM-IDLEVNR            PIC X(5).                                
018300     03  PNUM-IDDC               PIC X(2).                                
018400     03  PNUM-AVROP-VV           PIC 9(2).                                
018500     03  PNUM-FLSLAP             PIC X(1).                                
018600     03  PNUM-FLLEVBESK          PIC X(1).                                
018700     03  PNUM-MAIL               PIC X(57).                               
018800     03  FILLER                  PIC X(6).                                
018900     EJECT                                                                
019000                                                                          
019100 01  UT-AREA-START               PIC X(24)   VALUE                        
019200                                             'UT-AREA-START'.             
019300     SKIP2                                                                
019400 01  UT-AREA-1.                                                           
019500*   03  FILLER -COPY W2351101  -PRE UT1-                                  
019600*                                                                         
019700 01  UT-AREA-1B.                                                          
019800*   03  FILLER -COPY W2351101  -PRE UT1B-                                 
019900*                                                                         
020000 01  UT-AREA-2.                                                           
020100     03  UT-AREA-2.                                                       
020200         05  UT2-IDPTYP           PIC X(3).                               
020300         05  FILLER              PIC X(500).                              
020400*   03  FILLER -COPY W2351102 -PRE UT2-  -RED  UT-AREA-2                  
020500*                                                                         
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020800     SKIP3                                                                
020900 01  NYCKLAR-TILL-DLI.                                                    
020910     03  W-KDSEGKEY-X.                                                    
020920         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
020921     03  W-IDLAND-X.                                                      
020922         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
020930     03  W-IDDC-X.                                                        
020940         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
021000     03  W-IDARTNR-X.                                                     
021100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021200     03  W-WDD901KY-X.                                                    
021300         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
021400         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
021500     03  W-IDLEVNR-X.                                                     
021600         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
021700     03  W-WDD905KY-X.                                                    
021800         05  W-WDD905KY          PIC S9(7)   VALUE ZERO COMP-3.           
021900     03  W-DALEVBSK-X.                                                    
022000         05  W-DALEVBSK          PIC S9(8)   VALUE ZERO COMP-3.           
022100     03  W-IDLEVBSK-X.                                                    
022200         05  W-IDLEVBSK          PIC S9(1)   VALUE ZERO COMP-3.           
022300     03  W-W6D1HSEQ-X.                                                    
022400         05  W-IDARTNR-HSEQ      PIC S9(9)   VALUE ZERO  COMP-3.          
022500     03  WDK7A1KY-MIN-X.                                                  
022600         05  W-IDDC-K7-MIN        PIC X(02)  VALUE SPACE.                 
022700         05  FILLER               PIC X(12)  VALUE LOW-VALUE.             
022800     03  WDK7A1KY-MAX-X.                                                  
022900         05  W-IDDC-K7-MAX        PIC X(02)  VALUE SPACE.                 
023000         05  FILLER               PIC X(12)  VALUE HIGH-VALUE.            
023100     SKIP2                                                                
023200*    --- STATUS-KOD FRÅN IMS                                              
023810 01  STATUS-WS                   PIC XX.                                  
023820     88  SEGMENT-FOUND                       VALUE '  '.                  
023830     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
023840     88  SEGMENT-MISSING                     VALUE 'GE'.                  
023850     88  SEGMENT-END                         VALUE 'GB'.                  
023900     SKIP2                                                                
024000 01  GOOD-STATUSCODES.                                                    
024100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024200     SKIP3                                                                
024300 01  SSA1                        PIC X(128).                              
024400 01  SSA2                        PIC X(128).                              
024500 01  SSA3                        PIC X(128).                              
024600     EJECT                                                                
024700*    --- IMS FUNKTIONSKODER                                               
024800*01  -COPY W0003                                                          
024900     EJECT                                                                
025000*    ---  DLI INPUT-OUTPUT AREA                                           
025100                                                                          
025110 01  FILLER         PIC X(16)   VALUE 'DLI-IO-WDB601'.                    
025120 01   DLI-IO-WDB601.                                                      
025130*     03  -COPY WDB601                                                    
025140     EJECT                                                                
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
025300 01  DLI-IO-WDK611.                                                       
025400*    03  -COPY WDK611                                                     
025500     EJECT                                                                
025510 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
025520 01  DLI-IO-WDK7A1.                                                       
025530*    03  -COPY WDK7A1                                                     
025540     EJECT                                                                
025550 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
025560 01  DLI-IO-WDK701.                                                       
025570*    03  -COPY WDK701                                                     
025580     EJECT                                                                
025600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
025700 01  DLI-IO-WDK711.                                                       
025800*    03  -COPY WDK711                                                     
025900     EJECT                                                                
025901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
025902 01  DLI-IO-WDK712.                                                       
025903*    03  -COPY WDK712                                                     
025904     EJECT                                                                
025910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
025920 01  DLI-IO-WDK722.                                                       
025930*    03  -COPY WDK722                                                     
025940     EJECT                                                                
026000 01  DLI-IO-WDD901.                                                       
026100*    03  -COPY WDD901 -PRE 901-                                           
026200     EJECT                                                                
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
026400 01  DLI-IO-WDD902.                                                       
026500*    03  -COPY WDD902  -PRE 902-                                          
026600     EJECT                                                                
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
026800 01  DLI-IO-WDD905.                                                       
026900*    03  -COPY WDD905  -PRE 905-                                          
027000     EJECT                                                                
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
027200 01  DLI-IO-WDD924.                                                       
027300*    03  -COPY WDD924 -PRE 924-                                           
027400     EJECT                                                                
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
027600 01  DLI-IO-WDD925.                                                       
027700*    03  -COPY WDD925 -PRE 925-                                           
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
027900 01  DLI-IO-W6D111.                                                       
028000*    03  -COPY W6D111 -PRE INLA-                                          
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF501'.                      
028200 01  DLI-IO-WDF501.                                                       
028300*    03  -COPY WDF501                                                     
028400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF502'.                      
028500 01  DLI-IO-WDF502.                                                       
028600*    03  -COPY WDF502                                                     
028700     EJECT                                                                
028800 LINKAGE SECTION.                                                         
028900                                                                          
029200*01  -COPY W0008  -PRE WDK7A-                                             
029300     05  FILLER                  PIC X.                                   
029400                                                                          
029500*01  -COPY W0008  -PRE WDK7-                                              
029600     05  FILLER                  PIC X.                                   
029700                                                                          
029710*01  -COPY W0008  -PRE WDB6-                                              
029720     05  FILLER                  PIC X.                                   
029730                                                                          
029740*01  -COPY W0008  -PRE WDK6-                                              
029750     05  FILLER                  PIC X.                                   
029760                                                                          
029800*01  -COPY W0008  -PRE WDD9-                                              
029900     05  FILLER                  PIC X.                                   
030000                                                                          
030100*01  -COPY W0008  -PRE WDL9-                                              
030200     05  FILLER                  PIC X.                                   
030300                                                                          
030400*01  -COPY W0008  -PRE W6D1-                                              
030500     05  FILLER                  PIC X.                                   
030600                                                                          
030700*01  -COPY W0008  -PRE WDF5-                                              
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000 PROCEDURE DIVISION  USING WDK7A-PCB WDK7-PCB                             
031100     WDB6-PCB WDK6-PCB WDD9-PCB W6D1-PCB WDF5-PCB.                        
031200 MAIN SECTION.                                                            
031300     ENTRY 'DLITCBL' USING WDK7A-PCB WDK7-PCB                             
031301     WDB6-PCB WDK6-PCB WDD9-PCB W6D1-PCB WDF5-PCB.                        
031500                                                                          
031600     PERFORM A-INIT                                                       
031700                                                                          
031800     PERFORM S01-LAES-W235PP                                              
031810                                                                          
032000     PERFORM C-HAMTA-INFO                                                 
032100                                                                          
032200     PERFORM Z-FINIT                                                      
032350                                                                          
032400     MOVE ZERO TO RETURN-CODE                                             
032500     GOBACK                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 A-INIT SECTION.                                                          
032900     MOVE 'A-INIT              ' TO CURRENT-SECTION                       
033000                                                                          
033100     OPEN OUTPUT W2351101                                                 
033200                 W2351102                                                 
033300     OPEN INPUT  W235PP                                                   
033500                                                                          
034200                                                                          
034300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
034400     MOVE D-AAR             TO DAGENS-DATUM-AAR                           
034500     MOVE D-MAANAD          TO DAGENS-DATUM-MAANAD                        
034600     MOVE D-DAG             TO DAGENS-DATUM-DAG                           
034700     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
034800     .                                                                    
034900     EJECT                                                                
035000                                                                          
035100 C-HAMTA-INFO SECTION.                                                    
035110     MOVE 'C-HAMTA-INFO        ' TO CURRENT-SECTION                       
035120                                                                          
035200     MOVE PARM-IDLEVNR      TO W-IDLEVNR                                  
035300     MOVE LOW-VALUE         TO WDK7A1KY-MIN-X                             
035400     MOVE HIGH-VALUE        TO WDK7A1KY-MAX-X                             
035500     MOVE PARM-IDDC         TO W-IDDC-K7-MIN                              
035600                               W-IDDC-K7-MAX                              
036063                               W-IDDC                                     
036064     PERFORM IMS-GU-WDB601                                                
036065                                                                          
036200     PERFORM IMS-GN-WDK7A1                                                
036300     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
036920       PERFORM CE-LEVBESK-RENSA-UT1-UT2-AREA                              
036930                                                                          
037200       MOVE SEQA-IDARTNR          TO UT1-IDARTNR                          
037201                                     UT2-IDARTNR                          
037210                                     WS-IDARTNR                           
037220                                     W-IDARTNR                            
037230                                     W-IDARTNR-HSEQ                       
037231       PERFORM IMS-GU-WDK701                                              
037234       PERFORM IMS-GNP-WDK711                                             
037236       MOVE SLAG-IDLEVNR          TO UT1-IDLEVNR                          
037237                                     UT2-IDLEVNR                          
037239       MOVE SLAG-IDDC             TO UT1-IDDC                             
037240                                     UT2-IDDC                             
037273       PERFORM IMS-GNP-WDK722                                             
037275       IF SEGMENT-FOUND                                                   
037278          MOVE DCS-IDLANDX2          TO W-IDLAND                          
037279          PERFORM IMS-GNP-WDK712                                          
037280          IF SEGMENT-FOUND                                                
037290             MOVE LART-TIERSDAT-VIPS TO UT1-TIERSDAT-VIPS                 
037300             MOVE LART-TIERSDAT-VIPS TO UT2-TIERSDAT-VIPS                 
037310          ELSE                                                            
037320             MOVE +0                 TO UT1-TIERSDAT-VIPS                 
037330             MOVE +0                 TO UT2-TIERSDAT-VIPS                 
037340          END-IF                                                          
037400          MOVE XLAG-IDANSK        TO WS-IDANSK                            
037500          IF WS-IDANSK  >= PNUM-IDANSK-FOM                                
037600          AND WS-IDANSK <= PNUM-IDANSK-TOM                                
037700            MOVE XLAG-IDANSK      TO UT1-IDANSK                           
037800                                     UT2-IDANSK                           
037810            PERFORM IMS-GU-WDK611                                         
037820            IF SEGMENT-FOUND                                              
037900               MOVE CLAG-KDERS    TO UT1-KDERS                            
038000                                     UT2-KDERS                            
038001               MOVE CLAG-IDLEVNR-SHIP                                     
038002                                  TO UT1-IDLEVNR-SHIP                     
038003                                     UT2-IDLEVNR-SHIP                     
038020            END-IF                                                        
038100            MOVE SLAG-KVLS        TO UT1-KVLS                             
038400            COMPUTE UT1-KVROS = SLAG-KVROS-BULK +                         
038410                                SLAG-KVROS-DAG                            
038500            MOVE XLAG-KDAVT       TO UT1-KDAVT                            
038600                                     UT2-KDAVT                            
038700            COMPUTE UT1-KVAKS = SLAG-KVAKS-SDC +                          
038800                                SLAG-KVAKS-PAV                            
039000                                                                          
039100            PERFORM CA-AVROP-HAMTA-WDF5                                   
039200            PERFORM CB-AVROP-HAMTA-WDD9-AVROP                             
039300                                                                          
039400            IF PARM-FLLEVBESK = JA                                        
039500               PERFORM CC-LEVBESK-HAMTA-WDD9                              
039600               IF SW-SKRIV = JA                                           
039700                  PERFORM CD-LEVBESK-HAMTA-W6D1                           
039800                  MOVE 'FIR'      TO UT1-IDPTYP                           
039900                  MOVE UT-AREA-1  TO W2351101-RECORD                      
040000                  PERFORM S11-SKRIV-W2351101                              
040200                  MOVE NEJ        TO SW-SKRIV                             
040300               END-IF                                                     
040400            END-IF                                                        
040500          END-IF                                                          
040600       END-IF                                                             
040700       PERFORM IMS-GN-WDK7A1                                              
040800     END-PERFORM                                                          
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041200 CA-AVROP-HAMTA-WDF5 SECTION.                                             
041210     MOVE 'CA-AVROP-HAMTA-WDF5' TO CURRENT-SECTION                        
041220                                                                          
041400     PERFORM IMS-GET-WDF501                                               
041500     IF SEGMENT-FOUND                                                     
041600*****  HÄMTA BENÄMNING MED HÖGST BENÄMNINGSNR                             
041700       PERFORM IMS-GET-WDF502-LAST                                        
041800       IF SEGMENT-FOUND                                                   
041900         MOVE XLEV-BELEVART TO UT2-BELEV                                  
042200       END-IF                                                             
042500     END-IF                                                               
042600     .                                                                    
042700     EJECT                                                                
042800 CB-AVROP-HAMTA-WDD9-AVROP SECTION.                                       
042810     MOVE 'CB-AVROP-HAMTA-WDD9-AVROP' TO CURRENT-SECTION                  
042900                                                                          
043000     MOVE W-IDARTNR                TO W-IDARTNR-D9                        
043100     MOVE SLAG-IDDC                TO W-IDDC-D9                           
043200     PERFORM IMS-GU-WDD902                                                
043300     IF SEGMENT-FOUND                                                     
043500       PERFORM IMS-GNP-WDD905                                             
043600       MOVE 905-DAAVROP-AVS        TO WS-905-TIAAAAVV                     
043700       MOVE 905-TILEVDAG           TO WS-905-TID                          
043800       PERFORM CBA-OMVANDLA-DATUM                                         
043900       IF PARM-FLSLAP = NEJ                                               
044000         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                     
044100                       OR WS-905-TIAAAAVVD > WS-TIAAVVD                   
044200           IF 905-KDAVROP = 2                                             
044300           AND 905-KVAVROP > ZERO                                         
044400           AND WS-905-TIAAAAVVD >= DAGENS-TIAAVVD                         
044500             MOVE WS-905-TIAAAAVVD TO UT2-AVROPS-DAT                      
044600             MOVE 905-KVAVROP      TO UT2-KVAVROP                         
044700             PERFORM S12-SKRIV-W2351102                                   
044800           END-IF                                                         
044900           PERFORM IMS-GNP-WDD905                                         
045000           MOVE 905-DAAVROP-AVS    TO WS-905-TIAAAAVV                     
045100           MOVE 905-TILEVDAG       TO WS-905-TID                          
045200         END-PERFORM                                                      
045300       ELSE                                                               
045400         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                     
045500                       OR WS-905-TIAAAAVVD > WS-TIAAVVD                   
045600           IF 905-KDAVROP = 2                                             
045700           AND 905-KVAVROP > ZERO                                         
045800             MOVE WS-905-TIAAAAVVD  TO UT2-AVROPS-DAT                     
045900             MOVE 905-KVAVROP       TO UT2-KVAVROP                        
046000             PERFORM S12-SKRIV-W2351102                                   
046100           END-IF                                                         
046200           PERFORM IMS-GNP-WDD905                                         
046300           MOVE 905-DAAVROP-AVS     TO WS-905-TIAAAAVV                    
046400           MOVE 905-TILEVDAG        TO WS-905-TID                         
046500         END-PERFORM                                                      
046600       END-IF                                                             
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 CC-LEVBESK-HAMTA-WDD9 SECTION.                                           
047010     MOVE 'CC-LEVBESK-HAMTA-WDD9' TO CURRENT-SECTION                      
047020                                                                          
047100     MOVE NEJ          TO SW-FORSTA                                       
047200     MOVE W-IDARTNR    TO W-IDARTNR-D9                                    
047400     PERFORM IMS-GU-WDD902                                                
047500     IF SEGMENT-FOUND                                                     
047600       PERFORM IMS-GNP-WDD905                                             
047700       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                       
047800                     OR SW-FORSTA = JA                                    
047900         IF 905-KDAVROP = 2                                               
048000         AND  905-KVAVROP > ZERO                                          
048100           MOVE JA TO SW-FORSTA                                           
048200           MOVE 905-DAAVROP-AVS  TO WS-AAAAVV                             
048300           MOVE 905-TILEVDAG     TO WS-DAG                                
048400           MOVE WS-AAAAVVD       TO UT1-AVROPS-DAT                        
048500           MOVE 905-KVAVROP      TO UT1-KVAVROP                           
048600         ELSE                                                             
048700           PERFORM IMS-GNP-WDD905                                         
048800         END-IF                                                           
048900       END-PERFORM                                                        
048910                                                                          
049000       PERFORM IMS-GNP-WDD924                                             
049100       MOVE +1       TO IX                                                
049200       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                       
049300                     OR IX > 8                                            
049400         MOVE JA TO SW-SKRIV                                              
049500         IF IX = 1                                                        
049600           MOVE 924-LEV-DALEVBSK-AVS   TO UT1-DALEVBSK-AVS                
049700           MOVE 924-LEV-KVAVIS-BSKKVAR TO UT1-KVAVIS-BSKKVAR              
049800         ELSE                                                             
049900           MOVE 'SEC'                  TO UT1B-IDPTYP                     
050000           MOVE SEQA-IDARTNR           TO UT1B-IDARTNR                    
050100           MOVE 924-LEV-DALEVBSK-AVS   TO UT1B-DALEVBSK-AVS               
050200           MOVE 924-LEV-KVAVIS-BSKKVAR TO UT1B-KVAVIS-BSKKVAR             
050300           MOVE SPACE                  TO UT1B-IDLEVNR                    
050310                                          UT1B-IDDC                       
050400                                          UT1B-IDLEVNR-SHIP               
050410                                          UT1B-TELEVBSK-COMP              
050500           MOVE ZERO                   TO UT1B-IDANSK                     
050600                                          UT1B-TIBORT                     
050610                                          UT1B-KDERS                      
050700                                          UT1B-KVLS                       
050800                                          UT1B-KVAKS                      
050900                                          UT1B-KVROS                      
051000                                          UT1B-AVROPS-DAT                 
051100                                          UT1B-KVAVROP                    
051200                                          UT1B-KDAVT                      
051300                                          UT1B-TIERSDAT-VIPS              
051310                                          UT1B-KVART-FORAVIS              
051400           MOVE UT-AREA-1B             TO W2351101-RECORD                 
051500           PERFORM S11-SKRIV-W2351101                                     
051600         END-IF                                                           
051700         PERFORM IMS-GNP-WDD924                                           
051800         ADD +1      TO IX                                                
051900       END-PERFORM                                                        
052000       PERFORM IMS-GNP-WDD925                                             
052100       MOVE +1                       TO TELEV-IX                          
052200       MOVE SPACE                    TO UT1-TELEVBSK-COMP                 
052300       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                       
052400         IF 925-INFO-IDLEVBSK = 2 OR 4 OR 5 OR 6                          
052500           MOVE JA                   TO SW-SKRIV                          
052600           IF TELEV-IX = 1                                                
052700             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (1:80)          
052800           END-IF                                                         
052900           IF TELEV-IX = 2                                                
053000             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (81:80)         
053100           END-IF                                                         
053200           IF TELEV-IX = 3                                                
053300             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (162:80)        
053400           END-IF                                                         
053500           IF TELEV-IX = 4                                                
053600             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (243:80)        
053700           END-IF                                                         
053800                                                                          
053900           MOVE 925-INFO-TIBORT      TO UT1-TIBORT                        
054000           ADD +1    TO TELEV-IX                                          
054100         END-IF                                                           
054200         PERFORM IMS-GNP-WDD925                                           
054300       END-PERFORM                                                        
054400     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
054700 CD-LEVBESK-HAMTA-W6D1 SECTION.                                           
054710     MOVE 'CAD-LEVBESK-HAMTA-W6D1' TO CURRENT-SECTION                     
054800                                                                          
054900     MOVE WS-IDARTNR               TO W-IDARTNR-HSEQ                      
055000     MOVE ZERO                     TO W-KVART-TOT-C1                      
055100     PERFORM IMS-GN-W6D111-W6D1SEQ                                        
055200     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
055300       MOVE INLA-ART-IDDC          TO WS-IDDC                             
055400       IF (CDC-SE OR CDC-TR) AND                                          
055500           INLA-ART-IDLOPNRM  = ZERO                                      
055600         MOVE INLA-ART-KVAVIS      TO WS-KVAVIS                           
055700         IF INLA-ART-FLFEL = NEJ                                          
055800           ADD WS-KVAVIS           TO W-KVART-TOT-C1                      
055900         END-IF                                                           
056000       END-IF                                                             
056100       PERFORM IMS-GN-W6D111-W6D1SEQ                                      
056200     END-PERFORM                                                          
056300     MOVE W-KVART-TOT-C1           TO UT1-KVART-FORAVIS                   
056400     .                                                                    
056500     EJECT                                                                
056600 CE-LEVBESK-RENSA-UT1-UT2-AREA SECTION.                                   
056610     MOVE 'CAE-LEVBESK-RENSA-UT1-UT2-AREA'  TO CURRENT-SECTION            
056620                                                                          
056700     MOVE ZERO         TO UT1-IDARTNR                                     
056710                          UT1-IDANSK                                      
056800                          UT1-TIBORT                                      
056900                          UT1-DALEVBSK-AVS                                
057000                          UT1-KVAVIS-BSKKVAR                              
057100                          UT1-KDERS                                       
057200                          UT1-KVLS                                        
057300                          UT1-KVAKS                                       
057400                          UT1-KVROS                                       
057500                          UT1-AVROPS-DAT                                  
057600                          UT1-KVAVROP                                     
057700                          UT1-KDAVT                                       
057710                          UT1-TIERSDAT-VIPS                               
057720                          UT1-KVART-FORAVIS                               
057800     MOVE SPACE        TO UT1-IDDC                                        
057810                          UT1-IDLEVNR                                     
057811                          UT1-IDLEVNR-SHIP                                
057820                          UT1-TELEVBSK-COMP                               
057830                                                                          
057840                                                                          
057850     MOVE ZERO         TO UT2-IDARTNR                                     
057860                          UT2-KVAVROP                                     
057870                          UT2-AVROPS-DAT                                  
057891                          UT2-KDERS                                       
057892                          UT2-KDAVT                                       
057894                          UT2-IDANSK                                      
057898                          UT2-TIERSDAT-VIPS                               
057900     MOVE SPACE        TO UT2-BELEV                                       
057901                          UT2-IDDC                                        
057902                          UT2-IDLEVNR                                     
057903                          UT2-IDLEVNR-SHIP                                
057910     .                                                                    
058000     EJECT                                                                
058100                                                                          
058200 CBA-OMVANDLA-DATUM SECTION.                                              
058210     MOVE 'CABA-OMVANDLA-DATUM ' TO CURRENT-SECTION                       
058300                                                                          
058400     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
058500     MOVE DAGENS-DATUM           TO DAT-I-TIDATUM                         
058600                                                                          
058700     CALL WDATKONV USING DAT-KDDATFORM                                    
058800                       DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR             
058900                                                                          
059000     IF DAT-KDSVAR-OK                                                     
059100       MOVE DAT-TIAAVVD          TO WS-TIAAVVD                            
059200                                    DAGENS-TIAAVVD                        
059300       MOVE WS-TIAAVV            TO VADD-DATUM-AAVV                       
059400       MOVE PNUM-AVROP-VV        TO VADD-ANTAL                            
059500                                                                          
059600       CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                     
059700                                                                          
059800       MOVE VADD-DATUM-AAVV      TO WS-TIAAVV                             
059900       MOVE +20                  TO WS-SEKEL                              
060000                                    DAGENS-SEKEL                          
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 Z-FINIT SECTION.                                                         
060410     MOVE 'Z-FINIT             ' TO CURRENT-SECTION                       
060500                                                                          
060700     CLOSE W2351101                                                       
060800           W2351102                                                       
060900           W235PP                                                         
061000     SKIP2                                                                
061100     MOVE 'S' TO POSTSUM-OPKOD                                            
061200     CALL POSTSUM USING POSTSUM-PARM                                      
061300     .                                                                    
061400     EJECT                                                                
061500 S01-LAES-W235PP  SECTION.                                                
061510     MOVE 'S01-LAES-W235PP     ' TO CURRENT-SECTION                       
061600                                                                          
061700     READ W235PP INTO PARM-AREA                                           
061800     AT END                                                               
061900        SET END-OF-W235PP TO TRUE                                         
062000                                                                          
062100     NOT AT END                                                           
062200        MOVE 'W235PP'    TO POSTSUM-FDNAMN                                
062300        MOVE 'W23511D1'  TO POSTSUM-DDNAMN2                               
062400        MOVE 'PARM'      TO POSTSUM-TRANSTYP                              
062500        CALL POSTSUM  USING POSTSUM-PARM                                  
062600     END-READ                                                             
062700                                                                          
062800     .                                                                    
062900     EJECT                                                                
063000 S11-SKRIV-W2351101 SECTION.                                              
063010     MOVE 'S11-SKRIV-W2351101  ' TO CURRENT-SECTION                       
063100                                                                          
063200     WRITE W2351101-RECORD                                                
063300     MOVE 'W2351101' TO POSTSUM-FDNAMN                                    
063400     MOVE 'W23511D2' TO POSTSUM-DDNAMN2                                   
063500     CALL POSTSUM USING POSTSUM-PARM                                      
063600     .                                                                    
063700     EJECT                                                                
063800 S12-SKRIV-W2351102 SECTION.                                              
063810     MOVE 'S12-SKRIV-W2351102  ' TO CURRENT-SECTION                       
063900                                                                          
064000     WRITE W2351102-RECORD FROM UT2-W2351102                              
064100                                                                          
064200     MOVE UT2-IDPTYP TO POSTSUM-TRANSTYP                                  
064300     MOVE 'W2351102' TO POSTSUM-FDNAMN                                    
064400     MOVE 'W23511D3' TO POSTSUM-DDNAMN2                                   
064500     CALL POSTSUM USING POSTSUM-PARM                                      
064600     .                                                                    
064700     EJECT                                                                
064800* --- IMS SEKTIONER ---                                                   
064900                                                                          
065000     EJECT                                                                
065010 IMS-GU-WDB601    SECTION.                                                
065011     MOVE 'IMS-GU-WDB601 '      TO CURRENT-IMS-SECTION                    
065012                                                                          
065020     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
065030          DELIMITED BY SIZE INTO SSA1                                     
065040     MOVE '  ' TO GOOD-STATUSCODES                                        
065050     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
065060     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
065070     PERFORM IMS-STATUSCHECK                                              
065080     .                                                                    
065090                                                                          
065100 IMS-GN-WDK7A1 SECTION.                                                   
065200     MOVE 'IMS-GN-WDK7A1 '      TO CURRENT-IMS-SECTION                    
065201                                                                          
065210     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
065220                    '&WDK7A1KY<=' WDK7A1KY-MAX-X                          
065230                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
065600          DELIMITED BY SIZE INTO SSA1                                     
065700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
065800     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
065900     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
066000     PERFORM IMS-STATUSCHECK                                              
066100     .                                                                    
066200                                                                          
066300 IMS-GU-WDK701 SECTION.                                                   
066400     MOVE 'IMS-GU-WDK701    ' TO CURRENT-IMS-SECTION                      
066500                                                                          
066600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
066610          DELIMITED BY SIZE  INTO SSA1                                    
066620     MOVE '  '                 TO GOOD-STATUSCODES                        
066800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
066900     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
067000     PERFORM IMS-STATUSCHECK                                              
067100     .                                                                    
067200                                                                          
067201 IMS-GNP-WDK711      SECTION.                                             
067203     MOVE 'IMS-GNP-WDK711    ' TO CURRENT-IMS-SECTION                     
067204                                                                          
067205     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
067206          DELIMITED BY SIZE  INTO SSA1                                    
067209     MOVE '  '                 TO GOOD-STATUSCODES                        
067210     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
067211     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
067212     PERFORM IMS-STATUSCHECK                                              
067213     .                                                                    
067294                                                                          
067295 IMS-GNP-WDK712      SECTION.                                             
067297     MOVE 'IMS-GNP-WDK712    ' TO CURRENT-IMS-SECTION                     
067298                                                                          
067299     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
067300          DELIMITED BY SIZE  INTO SSA1                                    
067301     MOVE '  GE'               TO GOOD-STATUSCODES                        
067302     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK712 SSA1                   
067303     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
067304     PERFORM IMS-STATUSCHECK                                              
067305     .                                                                    
067306                                                                          
067307 IMS-GNP-WDK722      SECTION.                                             
067309     MOVE 'IMS-GNP-WDK722    ' TO CURRENT-IMS-SECTION                     
067310                                                                          
067311     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
067312          DELIMITED BY SIZE  INTO SSA1                                    
067313     MOVE 'WDK722 '            TO SSA2                                    
067314     MOVE '  GE'               TO GOOD-STATUSCODES                        
067315     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1 SSA2              
067316     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
067317     PERFORM IMS-STATUSCHECK                                              
067318     .                                                                    
067319                                                                          
067320 IMS-GU-WDK611 SECTION.                                                   
067321     MOVE 'IMS-GU-WDK611  '     TO CURRENT-IMS-SECTION                    
067322                                                                          
067323     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
067324          DELIMITED BY SIZE INTO SSA1                                     
067325     MOVE 'WDK611   ' TO SSA2                                             
067326     MOVE '  GE' TO GOOD-STATUSCODES                                      
067327     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
067328     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
067329     PERFORM IMS-STATUSCHECK                                              
067330     .                                                                    
067331                                                                          
067332 IMS-GU-WDD902 SECTION.                                                   
067340     MOVE 'IMS-GU-WDD902  '     TO CURRENT-IMS-SECTION                    
067400                                                                          
067500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
067600          DELIMITED BY SIZE INTO SSA1                                     
067700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
067800          DELIMITED BY SIZE INTO SSA2                                     
067900     MOVE '  GE' TO GOOD-STATUSCODES                                      
068000     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
068100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
068200     PERFORM IMS-STATUSCHECK                                              
068300     .                                                                    
068400     EJECT                                                                
068500 IMS-GNP-WDD905 SECTION.                                                  
068510     MOVE 'IMS-GNP-WDD905 '     TO CURRENT-IMS-SECTION                    
068600                                                                          
068700     MOVE 'WDD905   ' TO SSA1                                             
068800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
068900     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
069000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
069100     PERFORM IMS-STATUSCHECK                                              
069200     .                                                                    
069300     EJECT                                                                
069400 IMS-GNP-WDD924 SECTION.                                                  
069410     MOVE 'IMS-GNP-WDD924 '     TO CURRENT-IMS-SECTION                    
069500                                                                          
069600     MOVE 'WDD924   ' TO SSA1                                             
069700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
069800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
069900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
070000     PERFORM IMS-STATUSCHECK                                              
070100     .                                                                    
070200     EJECT                                                                
070300 IMS-GNP-WDD925 SECTION.                                                  
070310     MOVE 'IMS-GNP-WDD925 '     TO CURRENT-IMS-SECTION                    
070400                                                                          
070500     MOVE 'WDD925   ' TO SSA1                                             
070600     MOVE '  GE' TO GOOD-STATUSCODES                                      
070700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1                   
070800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
070900     PERFORM IMS-STATUSCHECK                                              
071000     .                                                                    
071100     EJECT                                                                
071200 IMS-GN-W6D111-W6D1SEQ SECTION.                                           
071210     MOVE 'IMS-GN-W6D111-W6D1SEQ' TO CURRENT-IMS-SECTION                  
071220                                                                          
071300     STRING 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
071400            DELIMITED BY SIZE INTO SSA1                                   
071500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
071600     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-W6D111 SSA1                    
071700     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
071800     PERFORM IMS-STATUSCHECK                                              
071900     .                                                                    
072000     EJECT                                                                
072100                                                                          
072200 IMS-GET-WDF501 SECTION.                                                  
072210     MOVE 'IMS-GET-WDF501'      TO CURRENT-IMS-SECTION                    
072300                                                                          
072400     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
072500          DELIMITED BY SIZE INTO SSA1                                     
072600     MOVE '  GE' TO GOOD-STATUSCODES                                      
072700     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
072800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
072900     PERFORM IMS-STATUSCHECK                                              
073000     .                                                                    
073100     SKIP3                                                                
073200 IMS-GET-WDF502-LAST SECTION.                                             
073210     MOVE 'IMS-GET-WDF502-LAST' TO CURRENT-IMS-SECTION                    
073300                                                                          
073400     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
073500          DELIMITED BY SIZE INTO SSA1                                     
073600     MOVE '  GE' TO GOOD-STATUSCODES                                      
073700     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-WDF502 SSA1                   
073800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
073900     PERFORM IMS-STATUSCHECK                                              
074000     .                                                                    
074100                                                                          
074200 IMS-STATUSCHECK SECTION.                                                 
074300                                                                          
074400     SET STATUS-IX TO 1                                                   
074500     SEARCH GOOD-STATUS                                                   
074600       AT END                                                             
074700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
074800           DELIMITED BY SIZE INTO ERROR-TEXT                              
074900         DISPLAY ERROR-TEXT                                               
075000         CALL FELLOG                                                      
075100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
075200         CONTINUE                                                         
075300     END-SEARCH                                                           
075400     .                                                                    
075500 S99-ABEND SECTION.                                                       
075600                                                                          
075700     SKIP2                                                                
075800     MOVE 'S' TO POSTSUM-OPKOD                                            
075900     CALL POSTSUM USING POSTSUM-PARM                                      
076000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
076100     .                                                                    
