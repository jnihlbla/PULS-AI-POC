000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2350800.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   04/12/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PGM SKAPAR FILER MED HJÄLP AV DATA FRÅN BILD 2323                
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*        PROGRAMMET LÄSER      WDD9                                       
001300*        PROGRAMMET LÄSER      WDF5                                       
001400*        PROGRAMMET LÄSER      W6D1                                       
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- PARAMETRAR FRÅN SOP                                        
002500     SELECT W235PP                     ASSIGN TO W23508D1.                
002600     SKIP2                                                                
002700*          --- UTFIL FÖR LEVERANSBESKED                                   
002800     SELECT W23508                     ASSIGN TO W23508D2.                
002900     SKIP2                                                                
003000*          --- UTFIL FÖR AVROP                                            
003100     SELECT W23509                     ASSIGN TO W23508D3.                
003200     SKIP2                                                                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W235PP                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100     SKIP2                                                                
004200 01  PARM                PIC X(80).                                       
004300     SKIP3                                                                
004400 FD  W23508                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W23508 -PRE  W23508-  -L.                                 
004900     SKIP3                                                                
005000 FD  W23509                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W23509 -PRE  W23509-  -L.                                 
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'W2350800'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  SW-SKRIV                    PIC X       VALUE 'N'.                   
006200 77  SW-FORSTA                   PIC X       VALUE 'N'.                   
006300 77  SW-FLER-LEVBSK              PIC X       VALUE 'N'.                   
006400 77  IX                          PIC S9(2)   VALUE ZERO.                  
006500 77  TELEV-IX                    PIC S9(2)   VALUE ZERO.                  
006600                                                                          
006610*01  -COPY WWDCKONS                                                       
006620                                                                          
006700 77  W235PP-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W235PP                       VALUE 'J'.                   
006900                                                                          
007000     SKIP2                                                                
007100 01  WS-AREA.                                                             
007300     03  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                 
008300     03  W-KVART-TOT-C1          PIC S9(9)   VALUE ZERO COMP-3.           
008400     03  WS-KVAVIS               PIC S9(7)   VALUE ZERO COMP-3.           
008600     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
008800     03  WS-IDANSK               PIC 9(3)    VALUE ZERO.                  
009000                                                                          
009100*      --- VALID IDDC CODES                                               
009200*01    -COPY WWDC99                                                       
009300                                                                          
009400 01  WS-TIAAVVD                  PIC 9(7)    VALUE ZERO.                  
009500 01  FILLER REDEFINES WS-TIAAVVD.                                         
009600     03 WS-SEKEL                 PIC 9(2).                                
009700     03 WS-TIAAVV                PIC 9(4).                                
009800     03 WS-TID                   PIC 9(1).                                
009900                                                                          
010000 01  DAGENS-TIAAVVD              PIC 9(7)    VALUE ZERO.                  
010100 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
010200     03 DAGENS-SEKEL             PIC 9(2).                                
010300     03 DAGENS-TIAAVV            PIC 9(4).                                
010400     03 DAGENS-TID               PIC 9(1).                                
010500                                                                          
010600 01  WS-905-TIAAAAVVD            PIC 9(7)    VALUE ZERO.                  
010700 01  FILLER REDEFINES WS-905-TIAAAAVVD.                                   
010800     03 WS-905-TIAAAAVV          PIC 9(6).                                
010900     03 WS-905-TID               PIC 9(1).                                
011000                                                                          
011100                                                                          
011200 01  WS-AAAAVVD                  PIC 9(7)    VALUE ZERO.                  
011300 01  FILLER REDEFINES WS-AAAAVVD.                                         
011400     03  WS-AAAAVV               PIC 9(6).                                
011500     03  WS-DAG                  PIC 9(1).                                
011600                                                                          
011700 01  FELTEXT.                                                             
011800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012000     EJECT                                                                
012100                                                                          
012200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012300 01  FILLER REDEFINES DAGENS-DATUM.                                       
012400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012700     EJECT                                                                
012800                                                                          
012900 01  W009VADD-AREA.                                                       
013000     03  VADD-DATUM-AAVV         PIC S9(5) VALUE ZERO COMP-3.             
013100     03  VADD-ANTAL              PIC S9(3) VALUE ZERO COMP-3.             
013200                                                                          
013300 01  DYNAMISKA-SUBPROGRAM.                                                
013400*                                                                         
013500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
013900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
014100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL ABEND                                            
014500                                                                          
014600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014800*    --- PARAMETRAR TILL DATKORT                                          
014900*                                                                         
015000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23508'.              
015100     SKIP2                                                                
015200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
015300     SKIP2                                                                
015400*01  -COPY WDATKORT                                                       
015500     EJECT                                                                
015600*01  -COPY WORKAREA                                                       
015700     EJECT                                                                
015800*01  -COPY WDATAREA                                                       
015900     EJECT                                                                
016000*    --- PARAMETRAR TILL POSTSUM                                          
016100*                                                                         
016200*01  -COPY W0005   -PRE  POSTSUM-                                         
016300     EJECT                                                                
016400                                                                          
016500 01  PARM-AREA-START             PIC X(24)   VALUE                        
016600                                 'PARM-AREA-START  '.                     
016700                                                                          
016800 01  PARM-AREA.                                                           
016900     03  PARM-IDANSK-FOM         PIC X(3).                                
017000     03  PARM-IDANSK-TOM         PIC X(3).                                
017100     03  PARM-IDLEVNR-2          PIC X(5).                                
017200     03  PARM-AVROP-VV           PIC X(2).                                
017300     03  PARM-FLSLAP             PIC X(1).                                
017400     03  PARM-FLLEVBESK          PIC X(1).                                
017500     03  PARM-MAIL               PIC X(57).                               
017600     03  FILLER                  PIC X(8).                                
017700 01  FILLER   REDEFINES PARM-AREA.                                        
017800     03  PNUM-IDANSK-FOM         PIC 9(3).                                
017900     03  PNUM-IDANSK-TOM         PIC 9(3).                                
018000     03  PNUM-IDLEVNR-2          PIC X(5).                                
018100     03  PNUM-AVROP-VV           PIC 9(2).                                
018200     03  PNUM-FLSLAP             PIC X(1).                                
018300     03  PNUM-FLLEVBESK          PIC X(1).                                
018400     03  PNUM-MAIL               PIC X(57).                               
018500     03  FILLER                  PIC X(8).                                
018600     EJECT                                                                
018700                                                                          
018800 01  UT-AREA-START               PIC X(24)   VALUE                        
018900                                             'UT-AREA-START'.             
019000     SKIP2                                                                
019100 01  UT-AREA-1.                                                           
019200*   03  FILLER -COPY W23508  -PRE UT1-                                    
019300*                                                                         
019400 01  UT-AREA-1B.                                                          
019500*   03  FILLER -COPY W23508  -PRE UT1B-                                   
019600*                                                                         
019700 01  UT-AREA-2.                                                           
019800     03  UT-AREA-2.                                                       
019900         05  UT2-IDPTYP           PIC X(3).                               
020000         05  FILLER              PIC X(500).                              
020100*   03  FILLER -COPY W23509  -PRE UT2-  -RED  UT-AREA-2                   
020200*                                                                         
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020500     SKIP3                                                                
020600 01  NYCKLAR-TILL-DLI.                                                    
020700     03  W-IDARTNR-X.                                                     
020800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020900     03  W-WDD901KY-X.                                                    
021000         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
021100         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
021200     03  W-IDLEVNR-X.                                                     
021300         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
021400     03  W-WDD905KY-X.                                                    
021500         05  W-WDD905KY          PIC S9(7)   VALUE ZERO COMP-3.           
021600     03  W-DALEVBSK-X.                                                    
021700         05  W-DALEVBSK          PIC S9(8)   VALUE ZERO COMP-3.           
021800     03  W-IDLEVBSK-X.                                                    
021900         05  W-IDLEVBSK          PIC S9(1)   VALUE ZERO COMP-3.           
022000     03  W-W6D1HSEQ-X.                                                    
022100         05  W-IDARTNR-HSEQ      PIC S9(9)   VALUE ZERO  COMP-3.          
022200     03  W-W6D101KY-X.                                                    
022300         05  W-W6D101KY          PIC X(22)   VALUE SPACE.                 
022400     03 W-WDK6A1KY-MIN-X.                                                 
022500         05 W-K6A1KY-IDLEVNR-MIN PIC X(5)    VALUE SPACE.                 
022600         05 W-K6A1KY-IDARTNR-MIN PIC S9(9)           COMP-3.              
022700     03 W-WDK6A1KY-MAX-X.                                                 
022800         05 W-K6A1KY-IDLEVNR-MAX PIC X(5)    VALUE SPACE.                 
022900         05 W-K6A1KY-IDARTNR-MAX PIC S9(9)           COMP-3.              
023000     SKIP2                                                                
023100                                                                          
023200     SKIP2                                                                
023300*    --- STATUS-KOD FRÅN IMS                                              
023400 01  STATUS-WS                   PIC XX.                                  
023500     88  SEGMENT-FINNS                       VALUE '  '.                  
023600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
023900     88  IMS-EJ-OK                           VALUE 'XD'.                  
024000     SKIP2                                                                
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400 01  SSA1                        PIC X(128).                              
024500 01  SSA2                        PIC X(128).                              
024600 01  SSA3                        PIC X(128).                              
024700     EJECT                                                                
024800*    --- IMS FUNKTIONSKODER                                               
024900*01  -COPY W0003                                                          
025000     EJECT                                                                
025100*    ---  DLI INPUT-OUTPUT AREA                                           
025200                                                                          
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6A1'.                      
025400 01  DLI-IO-WDK6A1.                                                       
025500*    03  -COPY WDK6A1                                                     
025600     EJECT                                                                
025700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
025800 01  DLI-IO-WDK601.                                                       
025900*    03  -COPY WDK601                                                     
026000     EJECT                                                                
026100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
026200 01  DLI-IO-WDK611.                                                       
026300*    03  -COPY WDK611                                                     
026400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
026500 01  DLI-IO-WDD901.                                                       
026600*    03  -COPY WDD901 -PRE 901-                                           
026700     EJECT                                                                
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
026900 01  DLI-IO-WDD902.                                                       
027000*    03  -COPY WDD902  -PRE 902-                                          
027100     EJECT                                                                
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
027300 01  DLI-IO-WDD905.                                                       
027400*    03  -COPY WDD905  -PRE 905-                                          
027500     EJECT                                                                
027600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
027700 01  DLI-IO-WDD924.                                                       
027800*    03  -COPY WDD924 -PRE 924-                                           
027900     EJECT                                                                
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
028100 01  DLI-IO-WDD925.                                                       
028200*    03  -COPY WDD925 -PRE 925-                                           
028300 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
028400 01  DLI-IO-W6D111.                                                       
028500*    03  -COPY W6D111 -PRE INLA-                                          
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF501'.                      
028700 01  DLI-IO-WDF501.                                                       
028800*    03  -COPY WDF501                                                     
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF502'.                      
029000 01  DLI-IO-WDF502.                                                       
029100*    03  -COPY WDF502                                                     
029200     EJECT                                                                
029300 LINKAGE SECTION.                                                         
029400                                                                          
029500*01  -COPY W0009   -PRE MSG-                                              
029600                                                                          
029700*01  -COPY W0008  -PRE WDK6A-                                             
029800     05  FILLER                  PIC X.                                   
029900                                                                          
030000*01  -COPY W0008  -PRE WDK6-                                              
030100     05  FILLER                  PIC X.                                   
030200                                                                          
030300*01  -COPY W0008  -PRE WDD9-                                              
030400     05  FILLER                  PIC X.                                   
030500                                                                          
030600*01  -COPY W0008  -PRE WDL9-                                              
030700     05  FILLER                  PIC X.                                   
030800                                                                          
030900*01  -COPY W0008  -PRE W6D1-                                              
031000     05  FILLER                  PIC X.                                   
031100                                                                          
031200*01  -COPY W0008  -PRE WDF5-                                              
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500 PROCEDURE DIVISION  USING MSG-PCB WDK6A-PCB WDK6-PCB WDD9-PCB            
031600     W6D1-PCB WDF5-PCB.                                                   
031700 MAIN SECTION.                                                            
031800     ENTRY 'DLITCBL' USING MSG-PCB WDK6A-PCB WDK6-PCB WDD9-PCB            
031900     W6D1-PCB WDF5-PCB.                                                   
032000                                                                          
032100     SKIP2                                                                
032200     PERFORM A-INIT                                                       
032300     PERFORM C-HAMTA-INFO                                                 
032400     PERFORM Z-FINIT                                                      
032500                                                                          
032600     MOVE ZERO TO RETURN-CODE                                             
032700     GOBACK                                                               
032800     .                                                                    
032900     EJECT                                                                
033000 A-INIT SECTION.                                                          
033100     SKIP2                                                                
033200                                                                          
033300     OPEN OUTPUT W23508                                                   
033400                 W23509                                                   
033500     OPEN INPUT  W235PP                                                   
033600                                                                          
033700     PERFORM S01-LAES-W235PP                                              
033800                                                                          
033900     IF END-OF-W235PP                                                     
034000        DISPLAY '*************************'                               
034100        DISPLAY 'PARAMETRAR SAKNAS'                                       
034200        DISPLAY '*************************'                               
034300        MOVE 'PARAMETRAR SAKNAS' TO FELTEXT-STR                           
034400        PERFORM S99-ABEND                                                 
034500     END-IF                                                               
034600     MOVE SPACE             TO UT1-IDLEVNR                                
034700                               UT2-IDLEVNR                                
034800                               WS-IDLEVNR                                 
034900     MOVE PARM-IDLEVNR-2    TO UT1-IDLEVNR                                
035000                               UT2-IDLEVNR                                
035100                               WS-IDLEVNR                                 
035200                                                                          
035300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
035400     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
035500     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
035600     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
035700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
036100 C-HAMTA-INFO SECTION.                                                    
036200     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
036300     MOVE LOW-VALUE         TO W-WDK6A1KY-MIN-X                           
036400     MOVE HIGH-VALUE        TO W-WDK6A1KY-MAX-X                           
036500     MOVE W-IDLEVNR         TO W-K6A1KY-IDLEVNR-MIN                       
036600                               W-K6A1KY-IDLEVNR-MAX                       
036700     PERFORM CA-HAMTA-DATA                                                
036800     .                                                                    
036900     EJECT                                                                
037000 CA-HAMTA-DATA SECTION.                                                   
037100     PERFORM IMS-GN-WDK6A1                                                
037200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
037300       MOVE SEQA-IDARTNR    TO UT1-IDARTNR                                
037400                               WS-IDARTNR                                 
037500                               W-IDARTNR                                  
037600                               W-IDARTNR-HSEQ                             
037700                               UT2-IDARTNR                                
037800                                                                          
037900       PERFORM IMS-GU-WDK611                                              
038000       IF SEGMENT-FINNS                                                   
038100         MOVE CLAG-IDANSK TO WS-IDANSK                                    
038200         IF WS-IDANSK >= PNUM-IDANSK-FOM                                  
038300         AND WS-IDANSK <= PNUM-IDANSK-TOM                                 
038400           MOVE CLAG-IDANSK TO UT1-IDANSK                                 
038500                                 UT2-IDANSK                               
038600           MOVE CLAG-KDERS  TO UT1-KDERS                                  
038700                                 UT2-KDERS                                
038800           MOVE CLAG-KVLS   TO UT1-KVLS                                   
038900           MOVE CLAG-KVROS  TO UT1-KVROS                                  
039000           MOVE CLAG-KDAVT  TO UT1-KDAVT                                  
039100                                 UT2-KDAVT                                
039200           COMPUTE UT1-KVAKS = CLAG-KVAKS-CDC +                           
039300                                   CLAG-KVAKS-PAV +                       
039400                                   CLAG-KVAKS-T                           
039500                                                                          
039600           PERFORM CAA-AVROP-HAMTA-WDF5                                   
039700           PERFORM CAB-AVROP-HAMTA-WDD9-AVROP                             
039800                                                                          
039900           IF PARM-FLLEVBESK = JA                                         
040000             PERFORM CAC-LEVBESK-HAMTA-WDD9                               
040100             IF SW-SKRIV = JA                                             
040200               PERFORM CAD-LEVBESK-HAMTA-W6D1                             
040300               MOVE 'FIR' TO UT1-IDPTYP                                   
040400               MOVE UT-AREA-1 TO W23508-POST                              
040500               PERFORM S11-SKRIV-W23508                                   
040600               PERFORM CAE-LEVBESK-RENSA-UT1-AREA                         
040700               MOVE NEJ TO SW-SKRIV                                       
040800             END-IF                                                       
040900           END-IF                                                         
041000         END-IF                                                           
041100       END-IF                                                             
041200       PERFORM IMS-GN-WDK6A1                                              
041300     END-PERFORM                                                          
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 CAA-AVROP-HAMTA-WDF5 SECTION.                                            
041800                                                                          
041900     PERFORM IMS-GET-WDF501                                               
042000     IF SEGMENT-FINNS                                                     
042100*****  HÄMTA BENÄMNING MED HÖGST BENÄMNINGSNR                             
042200       PERFORM IMS-GET-WDF502-LAST                                        
042300       IF SEGMENT-FINNS                                                   
042400         MOVE XLEV-BELEVART TO UT2-BELEV                                  
042500       ELSE                                                               
042600         MOVE SPACE     TO UT2-BELEV                                      
042700       END-IF                                                             
042800     ELSE                                                                 
042900       MOVE SPACE       TO UT2-BELEV                                      
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 CAB-AVROP-HAMTA-WDD9-AVROP SECTION.                                      
043400                                                                          
043500     MOVE W-IDARTNR TO W-IDARTNR-D9                                       
043510     MOVE WC-CDC-SE TO W-IDDC-D9                                          
043600     PERFORM IMS-GU-WDD902                                                
043700     IF SEGMENT-FINNS                                                     
043800       MOVE 902-KVBR               TO UT2-KVBR                            
043900       PERFORM IMS-GNP-WDD905                                             
044000       MOVE 905-DAAVROP-AVS        TO WS-905-TIAAAAVV                     
044100       MOVE 905-TILEVDAG           TO WS-905-TID                          
044200       PERFORM CABA-OMVANDLA-DATUM                                        
044300       IF PARM-FLSLAP = NEJ                                               
044400         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
044500                       OR WS-905-TIAAAAVVD > WS-TIAAVVD                   
044600           IF 905-KDAVROP = 2                                             
044700           AND 905-KVAVROP > ZERO                                         
044800           AND WS-905-TIAAAAVVD >= DAGENS-TIAAVVD                         
044900             MOVE WS-905-TIAAAAVVD TO UT2-AVROPS-DAT                      
045000             MOVE 905-KVAVROP      TO UT2-KVAVROP                         
045100             PERFORM S12-SKRIV-W23509                                     
045200           END-IF                                                         
045300           PERFORM IMS-GNP-WDD905                                         
045400           MOVE 905-DAAVROP-AVS    TO WS-905-TIAAAAVV                     
045500           MOVE 905-TILEVDAG       TO WS-905-TID                          
045600         END-PERFORM                                                      
045700       ELSE                                                               
045800         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
045900                       OR WS-905-TIAAAAVVD > WS-TIAAVVD                   
046000           IF 905-KDAVROP = 2                                             
046100           AND 905-KVAVROP > ZERO                                         
046200             MOVE WS-905-TIAAAAVVD  TO UT2-AVROPS-DAT                     
046300             MOVE 905-KVAVROP       TO UT2-KVAVROP                        
046400             PERFORM S12-SKRIV-W23509                                     
046500           END-IF                                                         
046600           PERFORM IMS-GNP-WDD905                                         
046700           MOVE 905-DAAVROP-AVS    TO WS-905-TIAAAAVV                     
046800           MOVE 905-TILEVDAG       TO WS-905-TID                          
046900         END-PERFORM                                                      
047000       END-IF                                                             
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 CAC-LEVBESK-HAMTA-WDD9 SECTION.                                          
047500     MOVE NEJ          TO SW-FORSTA                                       
047510     MOVE W-IDARTNR TO W-IDARTNR-D9                                       
047520     MOVE WC-CDC-SE TO W-IDDC-D9                                          
047600     PERFORM IMS-GU-WDD902                                                
047700     IF SEGMENT-FINNS                                                     
047800       PERFORM IMS-GNP-WDD905                                             
047900       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
048000                     OR SW-FORSTA = JA                                    
048100         IF 905-KDAVROP = 2                                               
048200         AND  905-KVAVROP > ZERO                                          
048300           MOVE JA TO SW-FORSTA                                           
048400           MOVE 905-DAAVROP-AVS  TO WS-AAAAVV                             
048500           MOVE 905-TILEVDAG     TO WS-DAG                                
048600           MOVE WS-AAAAVVD       TO UT1-AVROPS-DAT                        
048700           MOVE 905-KVAVROP      TO UT1-KVAVROP                           
048800         ELSE                                                             
048900           PERFORM IMS-GNP-WDD905                                         
049000         END-IF                                                           
049100       END-PERFORM                                                        
049200       PERFORM IMS-GNP-WDD924                                             
049300       MOVE +1       TO IX                                                
049400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
049500                     OR IX > 8                                            
049600         MOVE JA TO SW-SKRIV                                              
049700         IF IX = 1                                                        
049800           MOVE 924-LEV-DALEVBSK-AVS TO UT1-DALEVBSK-AVS                  
049900           MOVE 924-LEV-KVAVIS-BSKKVAR TO UT1-KVAVIS-BSKKVAR              
050000         ELSE                                                             
050100           MOVE 'SEC' TO UT1B-IDPTYP                                      
050200           MOVE SEQA-IDARTNR TO  UT1B-IDARTNR                             
050300           MOVE 924-LEV-DALEVBSK-AVS TO UT1B-DALEVBSK-AVS                 
050400           MOVE 924-LEV-KVAVIS-BSKKVAR TO UT1B-KVAVIS-BSKKVAR             
050500           MOVE SPACE        TO UT1B-IDLEVNR                              
050600                                UT1B-TELEVBSK-COMP                        
050700           MOVE ZERO         TO UT1B-IDANSK                               
050800                                UT1B-KDERS                                
050900                                UT1B-KVLS                                 
051000                                UT1B-KVAKS                                
051100                                UT1B-KVROS                                
051200                                UT1B-AVROPS-DAT                           
051300                                UT1B-KVAVROP                              
051400                                UT1B-KDAVT                                
051500                                UT1B-KVART-FORAVIS                        
051600           MOVE UT-AREA-1B   TO W23508-POST                               
051700           PERFORM S11-SKRIV-W23508                                       
051800         END-IF                                                           
051900         PERFORM IMS-GNP-WDD924                                           
052000         ADD +1      TO IX                                                
052100       END-PERFORM                                                        
052200       PERFORM IMS-GNP-WDD925                                             
052300       MOVE +1       TO TELEV-IX                                          
052400       MOVE SPACE TO UT1-TELEVBSK-COMP                                    
052500       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
052600         IF 925-INFO-IDLEVBSK = 2 OR 4 OR 5 OR 6                          
052700           MOVE JA TO SW-SKRIV                                            
052800           IF TELEV-IX = 1                                                
052900             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (1:80)          
053000           END-IF                                                         
053100           IF TELEV-IX = 2                                                
053200             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (81:80)         
053300           END-IF                                                         
053400           IF TELEV-IX = 3                                                
053500             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (162:80)        
053600           END-IF                                                         
053700           IF TELEV-IX = 4                                                
053800             MOVE 925-INFO-TELEVBSK  TO UT1-TELEVBSK-COMP (243:80)        
053900           END-IF                                                         
054000                                                                          
054100           MOVE 925-INFO-TIBORT       TO UT1-TIBORT                       
054200           ADD +1    TO TELEV-IX                                          
054300         END-IF                                                           
054400         PERFORM IMS-GNP-WDD925                                           
054500       END-PERFORM                                                        
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 CAD-LEVBESK-HAMTA-W6D1 SECTION.                                          
055000                                                                          
055100     MOVE WS-IDARTNR TO W-IDARTNR-HSEQ                                    
055200     MOVE ZERO       TO W-KVART-TOT-C1                                    
055300     PERFORM IMS-GN-W6D111-W6D1SEQ                                        
055400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
055500       MOVE INLA-ART-IDDC TO WS-IDDC                                      
055600       IF (CDC-SE OR CDC-TR) AND                                          
055700           INLA-ART-IDLOPNRM  = ZERO                                      
055800         MOVE INLA-ART-KVAVIS TO WS-KVAVIS                                
055900         IF INLA-ART-FLFEL = NEJ                                          
056000           ADD WS-KVAVIS TO W-KVART-TOT-C1                                
056100         END-IF                                                           
056200       END-IF                                                             
056300       PERFORM IMS-GN-W6D111-W6D1SEQ                                      
056400     END-PERFORM                                                          
056500     MOVE W-KVART-TOT-C1 TO UT1-KVART-FORAVIS                             
056600     .                                                                    
056700     EJECT                                                                
056800 CAE-LEVBESK-RENSA-UT1-AREA SECTION.                                      
056900     MOVE ZERO         TO UT1-IDANSK                                      
057000                          UT1-TIBORT                                      
057100                          UT1-DALEVBSK-AVS                                
057200                          UT1-KVAVIS-BSKKVAR                              
057300                          UT1-KDERS                                       
057400                          UT1-KVLS                                        
057500                          UT1-KVAKS                                       
057600                          UT1-KVROS                                       
057700                          UT1-AVROPS-DAT                                  
057800                          UT1-KVAVROP                                     
057900                          UT1-KDAVT                                       
058000     MOVE SPACE        TO UT1-TELEVBSK-COMP                               
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400 CABA-OMVANDLA-DATUM SECTION.                                             
058500                                                                          
058600     MOVE 'AAMMDD'                  TO DAT-KDDATFORM                      
058700     MOVE DAGENS-DATUM              TO DAT-I-TIDATUM                      
058800                                                                          
058900     CALL WDATKONV USING DAT-KDDATFORM                                    
059000                       DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR             
059100                                                                          
059200     IF DAT-KDSVAR-OK                                                     
059300       MOVE DAT-TIAAVVD    TO WS-TIAAVVD                                  
059400                              DAGENS-TIAAVVD                              
059500       MOVE WS-TIAAVV      TO VADD-DATUM-AAVV                             
059600       MOVE PNUM-AVROP-VV  TO VADD-ANTAL                                  
059700                                                                          
059800       CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                     
059900                                                                          
060000       MOVE VADD-DATUM-AAVV TO WS-TIAAVV                                  
060100       MOVE +20        TO WS-SEKEL                                        
060200                          DAGENS-SEKEL                                    
060300     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600 Z-FINIT SECTION.                                                         
060700                                                                          
060800                                                                          
060900     CLOSE W23508                                                         
061000           W23509                                                         
061100           W235PP                                                         
061200     SKIP2                                                                
061300     MOVE 'S' TO POSTSUM-OPKOD                                            
061400     CALL POSTSUM USING POSTSUM-PARM                                      
061500     .                                                                    
061600     EJECT                                                                
061700 S01-LAES-W235PP  SECTION.                                                
061800     SKIP2                                                                
061900     READ W235PP INTO PARM-AREA                                           
062000     AT END                                                               
062100        SET END-OF-W235PP TO TRUE                                         
062200                                                                          
062300     NOT AT END                                                           
062400        MOVE 'W235PP'    TO POSTSUM-FDNAMN                                
062500        MOVE 'W23508D1'  TO POSTSUM-DDNAMN2                               
062600        MOVE 'PARM'      TO POSTSUM-TRANSTYP                              
062700        CALL POSTSUM  USING POSTSUM-PARM                                  
062800     END-READ                                                             
062900                                                                          
063000     .                                                                    
063100     EJECT                                                                
063200 S11-SKRIV-W23508 SECTION.                                                
063300     SKIP2                                                                
063400     WRITE W23508-POST                                                    
063500     MOVE 'W23508 '  TO POSTSUM-FDNAMN                                    
063600     MOVE 'W23508D2' TO POSTSUM-DDNAMN2                                   
063700     CALL POSTSUM USING POSTSUM-PARM                                      
063800     .                                                                    
063900     EJECT                                                                
064000 S12-SKRIV-W23509 SECTION.                                                
064100     SKIP2                                                                
064200     WRITE W23509-POST FROM UT2-W23509                                    
064300                                                                          
064400     MOVE UT2-IDPTYP TO POSTSUM-TRANSTYP                                  
064500     MOVE 'W23509 '  TO POSTSUM-FDNAMN                                    
064600     MOVE 'W23508D3' TO POSTSUM-DDNAMN2                                   
064700     CALL POSTSUM USING POSTSUM-PARM                                      
064800     .                                                                    
064900     EJECT                                                                
065000* --- IMS SEKTIONER ---                                                   
065100                                                                          
065200     EJECT                                                                
065300 IMS-GN-WDK6A1 SECTION.                                                   
065400     STRING 'WDK6A1  (WDK6A1KY>=' W-WDK6A1KY-MIN-X                        
065500                    '&WDK6A1KY<=' W-WDK6A1KY-MAX-X ')'                    
065600     DELIMITED BY SIZE INTO SSA1                                          
065700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
065800     CALL CBLTDLI USING GN WDK6A-PCB DLI-IO-WDK6A1  SSA1                  
065900     MOVE WDK6A-STATUS-CODE TO STATUS-WS                                  
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     SKIP3                                                                
066300 IMS-GU-WDK611 SECTION.                                                   
066400                                                                          
066500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
066600          DELIMITED BY SIZE INTO SSA1                                     
066700     MOVE 'WDK611   ' TO SSA2                                             
066800     MOVE '  GE' TO GODK-STATUSKODER                                      
066900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
067000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
067100     PERFORM IMS-STATUSKONTROLL                                           
067200     .                                                                    
067300     EJECT                                                                
067400                                                                          
067500 IMS-GU-WDD902 SECTION.                                                   
067600                                                                          
067700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
067800          DELIMITED BY SIZE INTO SSA1                                     
067900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
068000          DELIMITED BY SIZE INTO SSA2                                     
068100     MOVE '  GE' TO GODK-STATUSKODER                                      
068200     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
068300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600     EJECT                                                                
068700 IMS-GNP-WDD905 SECTION.                                                  
068800                                                                          
068900     MOVE 'WDD905   ' TO SSA1                                             
069000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
069100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
069200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
069300     PERFORM IMS-STATUSKONTROLL                                           
069400     .                                                                    
069500     EJECT                                                                
069600 IMS-GNP-WDD924 SECTION.                                                  
069700                                                                          
069800     MOVE 'WDD924   ' TO SSA1                                             
069900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
070000     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
070100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     .                                                                    
070400     EJECT                                                                
070500 IMS-GNP-WDD925 SECTION.                                                  
070600                                                                          
070700     MOVE 'WDD925   ' TO SSA1                                             
070800     MOVE '  GE' TO GODK-STATUSKODER                                      
070900     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1                   
071000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
071100     PERFORM IMS-STATUSKONTROLL                                           
071200     .                                                                    
071300     EJECT                                                                
071400 IMS-GN-W6D111-W6D1SEQ SECTION.                                           
071500     STRING 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
071600            DELIMITED BY SIZE INTO SSA1                                   
071700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
071800     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-W6D111 SSA1                    
071900     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     EJECT                                                                
072300                                                                          
072400 IMS-GET-WDF501 SECTION.                                                  
072500                                                                          
072600     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
072700          DELIMITED BY SIZE INTO SSA1                                     
072800     MOVE '  GE' TO GODK-STATUSKODER                                      
072900     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
073000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     SKIP3                                                                
073400 IMS-GET-WDF502-LAST SECTION.                                             
073500                                                                          
073600     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
073700          DELIMITED BY SIZE INTO SSA1                                     
073800     MOVE '  GE' TO GODK-STATUSKODER                                      
073900     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-WDF502 SSA1                   
074000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
074100     PERFORM IMS-STATUSKONTROLL                                           
074200     .                                                                    
074300     SKIP3                                                                
074400 IMS-STATUSKONTROLL SECTION.                                              
074500     SKIP2                                                                
074600     SET STATUS-IX TO 1                                                   
074700     SEARCH GODK-STATUS                                                   
074800       AT END                                                             
074900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
075000           DELIMITED BY SIZE INTO FELTEXT                                 
075100         DISPLAY FELTEXT                                                  
075200         CALL FELLOG                                                      
075300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075400         CONTINUE                                                         
075500     END-SEARCH                                                           
075600     .                                                                    
075700 S99-ABEND SECTION.                                                       
075800                                                                          
075900     SKIP2                                                                
076000     MOVE 'S' TO POSTSUM-OPKOD                                            
076100     CALL POSTSUM USING POSTSUM-PARM                                      
076200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
076300     .                                                                    
076400     EJECT                                                                
