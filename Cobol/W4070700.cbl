000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4070700.                                                
000400 AUTHOR.         OLSSON SUSANNE.                                          
000500 DATE-WRITTEN.   08/05/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        MPP SOM ADMINISTRERAR PARAMETRAR FÖR FAKTURERING AV              
001000*        HANTERINGSKOSTNAD FÖR RETURER KOD 72,98 OCH (RR)RETUR-AV-        
001100*        RETUR.FINANSIELL KUND FAKTURERAS (PARMA ID) OCH REGLERNA         
001200*        LAGRAS I DB2-TABELL TP8SRET.VET MAN INTE PARMA-ID KAN MAN        
001300*        GE DISTR/KUND OCH PGM HÄMTAR PARMA-ID PÅ WDB2.AVGIFTEN ÄR        
001400*        OLIKA FÖR RESPEKTIVE ORSAKSKOD.                                  
001500*                                                                         
001810*        PROGRAMMET VIEW/UPDATE/INSERT/DELETE TP8GRET                     
001820*        PROGRAMMET VIEW/UPDATE/INSERT/DELETE TP8TRET                     
001830*        PROGRAMMET LÄSER      WDB2                                       
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T707                                              
002200*                     W4T707U                                             
002300*        MID:         W4I70701                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W4O70701                                            
002700*                                                                         
002800*    E'TRACKER 880053   DATED 2008-05                                     
003000*                                                                         
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4070700'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  INDX                        PIC S9(3)  VALUE +0   COMP SYNC.         
004600 77  MAX-INDX                    PIC S9(3)  VALUE +4   COMP SYNC.         
004700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
004800 77  WS-PRARTNTO-JFR1            PIC S9(7)V9(2) VALUE +0 COMP-3.          
004900 77  WS-PRARTNTO-JFR2            PIC S9(7)V9(2) VALUE +0 COMP-3.          
005000 77  WS-PRARTNTO-JFR3            PIC S9(7)V9(2) VALUE +0 COMP-3.          
005100 77  WS-PRARTNTO-JFR4            PIC S9(7)V9(2) VALUE +0 COMP-3.          
005200 77  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500                                                                          
005600                                                                          
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
006000                                                                          
006100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006200     88  INDATA-OK                           VALUE 'J'.                   
006300     88  INDATA-FEL                          VALUE 'N'.                   
006400                                                                          
006500 77  INDATA-SW-2                 PIC X       VALUE 'J'.                   
006600     88  INDATA-OK-2                         VALUE 'J'.                   
006700     88  INDATA-FEL-2                        VALUE 'N'.                   
006800                                                                          
006900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007000     88  EGEN-MID                            VALUE '4707'.                
007100     88  GODK-MID                            VALUE '4701' '4702'          
007200                                                   '4703' '4704'          
007300                                                   '4705' '4706'          
007400                                                   '4707' '4708'          
007500                                                   '4709'.                
007600     88  HELP-MID                            VALUE '0551'.                
007700     EJECT                                                                
007800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007900 01  GENERELLA-SUBPROGRAM.                                                
008000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008600     EJECT                                                                
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
008900     SKIP3                                                                
009000*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
009100*01 -COPY WDECAREA                                                        
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
009400     SKIP3                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009600*01 -COPY WMEDAREA                                                        
009700     EJECT                                                                
009800 01  MESSAGE-CODES.                                                       
009900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010100     03  ERR-CUST-MISSING        PIC X(3)    VALUE '137'.                 
010200     03  ERR-PRISUPPG-SAKNAS     PIC X(3)    VALUE '171'.                 
010300     03  ERR-BETALARE-SAKNAS     PIC X(3)    VALUE '145'.                 
010400     03  ERR-DIST-KUND-SAKNAS    PIC X(3)    VALUE '412'.                 
010500     03  ERR-INFO-MISSING        PIC X(3)    VALUE '413'.                 
010600     03  ERR-VALUTAKOD-SAKNAS    PIC X(3)    VALUE '148'.                 
010700     03  ERR-PRICE-MISSING       PIC X(3)    VALUE '301'.                 
010800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010900     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '245'.                 
011000     03  ERR-KOD-FINNS-EJ-REG    PIC X(3)    VALUE '132'.                 
011100     03  ERR-UPPLYSTA-FAELT-FEL  PIC X(3)    VALUE '409'.                 
011200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011310     03  INF-DELETE-DONE         PIC X(3)    VALUE '752'.                 
011400     03  INF-BEHANDLING-PAGAR    PIC X(3)    VALUE '130'.                 
011500     EJECT                                                                
011600                                                                          
011610 01  MESSAGE-LINE.                                                        
011620     03  ERR-DEFAULT-CUSTOMER    PIC X(40)   VALUE                        
011631         'CUSTOMER USES DEFAULT CUSTOMER 0 VALUES'.                       
011640                                                                          
011700* --- PARAMETRAR TILL SUBPROGRAM W005INIT                                 
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012000     SKIP3                                                                
012100*01 -COPY WMSGINIT                                                        
012200     EJECT                                                                
012300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
012400*                                                                         
012500 01  SPAR-AREA.                                                           
012600     03  SPAR-IDTRANS           PIC X(4)    VALUE '4707'.                 
012700     03  SPAR-IDDISTR-IN        PIC X(4)    VALUE SPACE.                  
012800     03  SPAR-IDKUNDNR-IN       PIC X(6)    VALUE SPACE.                  
012900     EJECT                                                                
013000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013100*                                                                         
013200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013300     SKIP3                                                                
013400*01  MID -COPY W4I70701                                                   
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013700     SKIP3                                                                
013800*01  -COPY WMSGAREA                                                       
013900     EJECT                                                                
014000     03  MOD REDEFINES MSG-AREA.                                          
014100*      05  -COPY W4O70701                                                 
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014400     SKIP3                                                                
014500*01  -COPY WMFSAREA                                                       
014600     EJECT                                                                
014700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014800*                                                                         
014900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015000     SKIP3                                                                
015100 01  NYCKLAR-TILL-DLI.                                                    
015200     03  W-IDGMT-X.                                                       
015300         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
015400         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
015500                                                                          
015600     03  W-IDGMT-MIN-X.                                                   
015700         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
015800         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
015900                                                                          
016000     03  W-IDGMT-MAX-X.                                                   
016100         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
016200         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
016300                                                                          
016400     03  W-WDB101KY-X.                                                    
016500         05 W-IDPARTNR           PIC X(9)    VALUE SPACE.                 
016600         05 W-IDFTG              PIC X(2)    VALUE SPACE.                 
016700                                                                          
016710     03  W-IDKUNDNR-N            PIC 9(6)    VALUE ZERO.                  
016720     03  W-IDKUNDNR-X            PIC X(6)    VALUE SPACE.                 
016730     03  W-IDKUNDNR-INPUT        PIC 9(6)    VALUE ZEROES.                
016760                                                                          
016800     SKIP2                                                                
016900*    --- STATUS-KOD FRÅN IMS                                              
017000 01  STATUS-WS                   PIC XX.                                  
017100     88  SEGMENT-FINNS                       VALUE '  '.                  
017200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017400     SKIP2                                                                
017500 01  GODK-STATUSKODER.                                                    
017600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017700     SKIP3                                                                
017800 01  SSA1                        PIC X(64).                               
017900 01  SSA2                        PIC X(64).                               
018000     EJECT                                                                
018100*    --- IMS FUNKTIONSKODER                                               
018200*01  -COPY W0003                                                          
018300     EJECT                                                                
018400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
018500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018600                                                                          
018700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
018800 01  DB2-WS.                                                              
018900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
019000         88  CURSOR-OK                       VALUE 000.                   
019100         88  RADER-FINNS                     VALUE 000.                   
019200         88  RADER-SAKNAS                    VALUE 100.                   
019300         88  RAD-DUBLETT                     VALUE 803.                   
019400         88  ATKOMST-FEL                     VALUE 904.                   
019500         88  RADER-SAKNAS-TOMT               VALUE 305.                   
019600                                                                          
019700     03  GODK-SQLCODEKODER.                                               
019800         05  GODK-SQLCODE OCCURS 5                                        
019900             INDEXED BY SQLCODE-IX PIC 9(3).                              
019910                                                                          
019920     03  SQLCODE-TP8TRET         PIC 9(3)    VALUE ZERO.                  
019930     03  SQLCODE-TP8GRET         PIC 9(3)    VALUE ZERO.                  
020000*                                                                         
020100*                                                                         
020200*    ---  DB2 HOST-COPYTEXTER                                             
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'TP8TRET-AREA'.        
020500*01  AREA -COPY TP8TRET -PRE  TRET-                                       
020600                                                                          
021100 01  FILLER                      PIC X(16)   VALUE 'TP8GRET-AREA'.        
021200*01  AREA -COPY TP8GRET -PRE  GRET-                                       
021300     EJECT                                                                
021400                                                                          
021500 01  FILLER                      PIC X(16)   VALUE 'TP8TRET-DCL '.        
021600     EXEC SQL INCLUDE TP8TRET  END-EXEC.                                  
021700                                                                          
022200 01  FILLER                      PIC X(16)   VALUE 'TP8GRET-DCL '.        
022300     EXEC SQL INCLUDE TP8GRET END-EXEC.                                   
022400     EJECT                                                                
022500                                                                          
022600*    ---  DLI INPUT-OUTPUT AREA                                           
022700                                                                          
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
022900 01  DLI-IO-WDB101.                                                       
023000*    03  -COPY WDB101                                                     
023100     EJECT                                                                
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
023300 01  DLI-IO-WDB201.                                                       
023400*    03  -COPY WDB201                                                     
023500     EJECT                                                                
023600 LINKAGE SECTION.                                                         
023700*01  -COPY W0009   -PRE MSG-                                              
023800*01  -COPY W0008   -PRE WDP7-                                             
023900     05  FILLER                  PIC X.                                   
024000                                                                          
024100*01  -COPY W0008  -PRE WDB1-                                              
024200     05  FILLER                  PIC X.                                   
024300                                                                          
024400*01  -COPY W0008  -PRE WDB2-                                              
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB1-PCB WDB2-PCB.            
024800                                                                          
024900 MAIN SECTION.                                                            
025000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB1-PCB WDB2-PCB.            
025100                                                                          
025200     PERFORM IMS-GET-MSG                                                  
025300     IF SEGMENT-FINNS                                                     
025400       PERFORM A-INIT                                                     
025500       PERFORM B-KOLLA-NYCKLAR                                            
025600       IF NYCKLAR-OK                                                      
025700         IF MFS-UPDATE                                                    
025800           PERFORM G-KOLLA-INPUT                                          
025900           IF  INDATA-OK                                                  
026000             PERFORM H-UPPDATERA                                          
026100           END-IF                                                         
026200           PERFORM G-KOLLA-INPUT-2                                        
026300           IF  INDATA-OK-2                                                
026400             PERFORM H-UPPDATERA-2                                        
026500           END-IF                                                         
026600         ELSE                                                             
026700           IF MFS-FIRST                                                   
026800             CONTINUE                                                     
026900           ELSE                                                           
027000             PERFORM E-SAMMA-SIDA                                         
027100           END-IF                                                         
027200         END-IF                                                           
027300         IF  INDATA-OK                                                    
027400         OR  INDATA-OK-2                                                  
027500           PERFORM F-LAES-VISA-INFO                                       
027600         END-IF                                                           
027700       END-IF                                                             
027800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O70701 + 4                      
027900       PERFORM IMS-INSERT-MSG                                             
028000     END-IF                                                               
028100                                                                          
028200     MOVE ZERO TO RETURN-CODE                                             
028300     GOBACK                                                               
028400     .                                                                    
028500     EJECT                                                                
028600                                                                          
028700 A-INIT SECTION.                                                          
028800     IF MSG-DUBBLA-TRANSKODER                                             
028900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I70701                 
029000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
029100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029200     ELSE                                                                 
029300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I70701                  
029400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
029500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029600     END-IF                                                               
029700                                                                          
029800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
029900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
030000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030100                                                                          
030200     MOVE LOW-VALUE TO MSG-AREA                                           
030300     MOVE 'W4O707N1' TO MFS-IDMOD                                         
030400     MOVE '4707' TO MOD-IDTRANS                                           
030500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
030600                                                                          
030700     MOVE SPACE                       TO MED-IDMFSINF                     
030800     MOVE SPACE                       TO MED-IDMFSFEL                     
030900                                                                          
031000     IF EGEN-MID OR HELP-MID                                              
031100       CONTINUE                                                           
031200     ELSE                                                                 
031300       MOVE SPACE TO MFS-KDTRTYP                                          
031400       MOVE '7' TO MFS-IDPFK                                              
031500     END-IF                                                               
031600                                                                          
031700     INITIALIZE GODK-SQLCODEKODER                                         
031800                                                                          
031900     MOVE LOW-VALUE         TO W-IDGMT-MIN-X                              
032000     MOVE HIGH-VALUE        TO W-IDGMT-MAX-X                              
032100     .                                                                    
032200     EJECT                                                                
032300                                                                          
032400 B-KOLLA-NYCKLAR SECTION.                                                 
032500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032600     MOVE '001'             TO MSGI-KDCALL                                
032700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
032800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032900     MOVE '4707'            TO MSGI-IDTRANS                               
033000     IF EGEN-MID                                                          
033300         MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                            
033400         MOVE MID-IDKUNDNR-IN  TO MSGI-IDKUNDNR                           
033500     END-IF                                                               
033600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
033700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
033800                                                                          
033900     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
034000     MOVE '2'             TO MFS-KDMFSFOR                                 
034100                                                                          
034200     MOVE JA TO NYCKLAR-SW                                                
034400                                                                          
037000*    -- KONTROLL AV IDDISTR                                               
037100     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
037200                                                                          
037300     IF MID-IDDISTR-IN NOT = ALL '+'                                      
037400        MOVE '7'   TO MFS-IDPFK                                           
037500        MOVE SPACE TO MFS-KDTRTYP                                         
037600     END-IF                                                               
037700                                                                          
037800     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
037900     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
038000        MOVE MSGI-IDDISTR       TO W-IDDISTR                              
038613     ELSE                                                                 
038614        MOVE NEJ TO NYCKLAR-SW                                            
038615     END-IF                                                               
038616                                                                          
038617     MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                                  
038618     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
038620                                                                          
038700*    -- KONTROLL AV IDKUNDNR                                              
038800     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
038900                                                                          
039000     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
039100       MOVE '7'         TO MFS-IDPFK                                      
039200       MOVE SPACE       TO MFS-KDTRTYP                                    
039300     END-IF                                                               
039310     IF MSGI-IDKUNDNR NOT = ALL '+'                                       
039400       INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
039500       IF MSGI-IDKUNDNR NUMERIC                                           
039600         MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                 
039610         MOVE W-IDKUNDNR           TO W-IDKUNDNR-N                        
039620         MOVE W-IDKUNDNR-N         TO W-IDKUNDNR-X                        
039630         INSPECT W-IDKUNDNR-X    REPLACING LEADING ZERO BY SPACE          
039640         MOVE W-IDKUNDNR-X         TO MOD-IDKUNDNR-UT                     
039700       ELSE                                                               
039710         MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                            
039720         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
039800         MOVE NEJ TO NYCKLAR-SW                                           
039900       END-IF                                                             
039910     ELSE                                                                 
039920       IF MSGI-IDDISTR    NOT = ALL '+'                                   
039930          MOVE ZEROES   TO W-IDKUNDNR                                     
039931       ELSE                                                               
039932          MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                
039940       END-IF                                                             
040000     END-IF                                                               
040040     IF MSGI-IDKUNDNR = ZERO                                              
040050        MOVE '     0' TO MOD-IDKUNDNR-UT                                  
040051                         W-IDKUNDNR-X                                     
040060     END-IF                                                               
040100                                                                          
040200*RC  IF EGEN-MID OR NYCKLAR-OK                                            
040300*RC    IF MFS-FIRST                                                       
040400*RC      IF  MID-IDDISTR-IN  = ALL '+'                                    
040500*RC        MOVE SPACE              TO MOD-IDDISTR-UT                      
040600*RC      ELSE                                                             
040700*RC        MOVE MSGI-IDDISTR       TO MOD-IDDISTR-UT                      
040710*RC        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE         
040800*RC      END-IF                                                           
040900*RC      IF  MID-IDKUNDNR-IN = ALL '+'                                    
041000*RC        MOVE SPACE              TO MOD-IDKUNDNR-UT                     
041100*RC      ELSE                                                             
041200*RC        MOVE MSGI-IDKUNDNR      TO MOD-IDKUNDNR-UT                     
041210*RC        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE        
041300*RC      END-IF                                                           
041400*RC    ELSE                                                               
041500*RC      IF SPAR-IDTRANS = '4707'                                         
041600*RC        IF SPAR-IDDISTR-IN = SPACE                                     
041700*RC          MOVE SPACE              TO MOD-IDDISTR-UT                    
041800*RC        ELSE                                                           
041900*RC          MOVE SPAR-IDDISTR-IN    TO MOD-IDDISTR-UT                    
042000*RC                                     W-IDDISTR                         
042100*RC        END-IF                                                         
042200*RC        IF SPAR-IDKUNDNR-IN = SPACE                                    
042300*RC          MOVE SPACE              TO MOD-IDKUNDNR-UT                   
042400*RC        ELSE                                                           
042500*RC          MOVE SPAR-IDKUNDNR-IN      TO MOD-IDKUNDNR-UT                
042600*RC                                        W-IDKUNDNR                     
042700*RC        END-IF                                                         
042800*RC      ELSE                                                             
042900*RC        MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                    
043000*RC                                     W-IDDISTR                         
043100*RC        MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                   
043200*RC                                     W-IDKUNDNR                        
043300*RC      END-IF                                                           
043400*RC    END-IF                                                             
044700*RC  ELSE                                                                 
044800*RC    MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
044900*RC                            MOD-IDKUNDNR-UT                            
045200*RC  END-IF                                                               
045300*RC                                                                       
045301*RC  INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
045302*RC  INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
045310     IF EGEN-MID OR NYCKLAR-OK                                            
045320        IF SPAR-IDTRANS = '4707' AND NOT MFS-FIRST                        
045330           IF SPAR-IDDISTR-IN = SPACE                                     
045340             MOVE SPACE              TO MOD-IDDISTR-UT                    
045350           ELSE                                                           
045360             MOVE SPAR-IDDISTR-IN    TO MOD-IDDISTR-UT                    
045370                                        W-IDDISTR                         
045380           END-IF                                                         
045390           IF SPAR-IDKUNDNR-IN = SPACE                                    
045391             MOVE SPACE              TO MOD-IDKUNDNR-UT                   
045392           ELSE                                                           
045393             MOVE SPAR-IDKUNDNR-IN      TO MOD-IDKUNDNR-UT                
045394                                           W-IDKUNDNR                     
045395           END-IF                                                         
045396           INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE        
045397           INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE        
045398        END-IF                                                            
045399     END-IF                                                               
045400                                                                          
045410     IF EGEN-MID  AND MFS-FIRST                                           
045500       IF MSGI-IDDISTR = ALL '+' AND                                      
045600          MSGI-IDKUNDNR = ALL '+'                                         
045800           MOVE NEJ TO NYCKLAR-SW                                         
046000       END-IF                                                             
046800     END-IF                                                               
046900                                                                          
047000     IF MFS-FIRST                                                         
047100       PERFORM MFS-RENSA-FAELT-IN                                         
047200       PERFORM MFS-RENSA-FAELT-IN-2                                       
047500        IF (MID-IDDISTR-IN = ALL '+')                                     
047600       AND (MID-IDKUNDNR-IN = ALL '+')                                    
047700         MOVE NEJ TO NYCKLAR-SW                                           
047800       END-IF                                                             
047900     END-IF                                                               
048000                                                                          
048100     IF NYCKLAR-FEL                                                       
048200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
048300       CALL WMEDKONV USING MED-WMEDAREA                                   
048400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
048500       PERFORM MFS-RENSA-FAELT-IN                                         
048600       PERFORM MFS-RENSA-FAELT-IN-2                                       
048700       PERFORM MFS-RENSA-FAELT-UT                                         
048800       PERFORM MFS-RENSA-FAELT-UT-2                                       
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200                                                                          
049300 E-SAMMA-SIDA SECTION.                                                    
049400     IF EGEN-MID OR HELP-MID                                              
049500       IF MID-INPUT = ALL '+'                                             
049600         PERFORM MFS-RENSA-FAELT-IN                                       
049710       ELSE                                                               
049720          IF MID-INPUT-2 = ALL '+'                                        
049730            PERFORM MFS-RENSA-FAELT-IN-2                                  
049800          ELSE                                                            
049900            MOVE INF-PRESS-PF11 TO MED-IDMFSINF                           
050000            CALL WMEDKONV USING MED-WMEDAREA                              
050100            MOVE MED-MFSINF TO MOD-TEMFSFEL                               
050200            PERFORM EA-MID-INDATA-TILL-MOD                                
050300            PERFORM MFS-ROER-EJ-FAELT-UT                                  
050400            PERFORM MFS-ROER-EJ-FAELT-UT-2                                
050410            PERFORM MFS-DONT-TOUCH-PART-FTG                               
050500            MOVE NEJ TO INDATA-SW                                         
050600          END-IF                                                          
050610       END-IF                                                             
050700     ELSE                                                                 
050800       PERFORM MFS-RENSA-FAELT-IN                                         
050900       PERFORM MFS-RENSA-FAELT-IN-2                                       
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300                                                                          
051400 EA-MID-INDATA-TILL-MOD SECTION.                                          
051500**** HANDLING FEE PRICING                                                 
051600     IF MID-KDBEHX = ALL '+'                                              
051700       MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-UPD                             
051800     ELSE                                                                 
051900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBEHX-ATTR                      
052000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEHX-UPD                           
052100     END-IF                                                               
052200                                                                          
052300     IF MID-KDANMORS-UPD = ALL '+'                                        
052400       MOVE MFS-RENSA-FAELT TO MOD-KDANMORS-UPD                           
052500     ELSE                                                                 
052600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDANMORS-ATTR                    
052700       MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-UPD                         
052800     END-IF                                                               
052900                                                                          
053000     IF MID-FLINVFEE-UPD = ALL '+'                                        
053100       MOVE MFS-RENSA-FAELT TO MOD-FLINVFEE-UPD                           
053200     ELSE                                                                 
053300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLINVFEE-ATTR                    
053400       MOVE MFS-ROER-EJ-FAELT TO MOD-FLINVFEE-UPD                         
053500     END-IF                                                               
053600                                                                          
053700     IF MID-PRARTNTO-UPD = ALL '+'                                        
053800       MOVE MFS-RENSA-FAELT TO MOD-PRARTNTO-UPD                           
053900     ELSE                                                                 
054000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRARTNTO-ATTR                    
054100       MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTNTO-UPD                         
054200     END-IF                                                               
054300                                                                          
054400     IF MID-FLINVLDC-UPD = ALL '+'                                        
054500       MOVE MFS-RENSA-FAELT TO MOD-FLINVLDC-UPD                           
054600     ELSE                                                                 
054700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLINVLDC-ATTR                    
054800       MOVE MFS-ROER-EJ-FAELT TO MOD-FLINVLDC-UPD                         
054900     END-IF                                                               
055000                                                                          
055100     IF MID-PRARTNTO-LDC-UPD = ALL '+'                                    
055200       MOVE MFS-RENSA-FAELT TO MOD-PRARTNTO-LDC-UPD                       
055300     ELSE                                                                 
055400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRARTNTO-LDC-ATTR                
055500       MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTNTO-LDC-UPD                     
055600     END-IF                                                               
055700                                                                          
055800**** VALUE LIMIT DESCREPANCY                                              
055900     IF MID-KDBEHX-GRET = ALL '+'                                         
056000       MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-GRET-UPD                        
056100     ELSE                                                                 
056200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBEHX-2-ATTR                    
056300       MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEHX-GRET-UPD                      
056400     END-IF                                                               
056500                                                                          
056600     IF MID-KDANMORS-GRET = ALL '+'                                       
056700       MOVE MFS-RENSA-FAELT TO MOD-KDANMORS-GRET-UPD                      
056800     ELSE                                                                 
056900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDANMORS-2-ATTR                  
057000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-GRET-UPD                    
057100     END-IF                                                               
057200                                                                          
057300     IF MID-SUARTBTO-GRET = ALL '+'                                       
057400       MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-GRET-UPD                      
057500     ELSE                                                                 
057600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUARTBTO-2-ATTR                  
057700       MOVE MFS-ROER-EJ-FAELT TO MOD-SUARTBTO-GRET-UPD                    
057800     END-IF                                                               
057900                                                                          
058000     IF MID-FLINVLDC-GRET = ALL '+'                                       
058100       MOVE MFS-RENSA-FAELT TO MOD-FLINVLDC-GRET-UPD                      
058200     ELSE                                                                 
058300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLINVLDC-2-ATTR                  
058400       MOVE MFS-ROER-EJ-FAELT TO MOD-FLINVLDC-GRET-UPD                    
058500     END-IF                                                               
058600                                                                          
058700     IF MID-SUARTBTO-LDC-GRET = ALL '+'                                   
058800       MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-LDC-GRET-UPD                  
058900     ELSE                                                                 
059000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUARTBTO-LDC-2-ATTR              
059100       MOVE MFS-ROER-EJ-FAELT TO MOD-SUARTBTO-LDC-GRET-UPD                
059200     END-IF                                                               
059300     .                                                                    
059400     EJECT                                                                
059500                                                                          
059600 F-LAES-VISA-INFO SECTION.                                                
059700*RC  IF MFS-UPDATE                                                        
059800*RC    PERFORM FA-LAES-GRUNDDATA                                          
059900*RC  ELSE                                                                 
060000*RC    IF MFS-ENTER                                                       
060100*RC      PERFORM FA-LAES-GRUNDDATA                                        
060200*RC    ELSE                                                               
060300*RC      IF MID-IDPARTNR-IN = ALL '+' AND                                 
060400*RC         MID-IDFTG-IN = ALL '+'                                        
060500*RC        PERFORM IMS-GU-WDB201                                          
060600*RC        IF SEGMENT-FINNS                                               
060700*RC          MOVE GMT-IDPARTNR     TO  W-IDPARTNR                         
060800*RC                                    SPAR-IDPARTNR-IN                   
060900*RC                                    MOD-IDPARTNR-UT                    
061000*RC          MOVE GMT-IDFTG        TO  W-IDFTG                            
061100*RC                                    SPAR-IDFTG-IN                      
061200*RC                                    MOD-IDFTG-UT                       
061300*RC                                                                       
061400*RC          PERFORM FA-LAES-GRUNDDATA                                    
061500*RC        ELSE                                                           
061600*RC          MOVE ERR-DIST-KUND-SAKNAS  TO MED-IDMFSFEL                   
061700*RC          CALL WMEDKONV USING MED-WMEDAREA                             
061800*RC          MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
061900*RC          PERFORM MFS-RENSA-FAELT-UT                                   
062000*RC          PERFORM MFS-RENSA-FAELT-UT-2                                 
062100*RC        END-IF                                                         
062200*RC      ELSE                                                             
062300*RC        MOVE MSGI-IDPARTNR TO SPAR-IDPARTNR-IN                         
062400*RC        MOVE MSGI-IDFTG    TO SPAR-IDFTG-IN                            
062500*RC                                                                       
062600*RC        PERFORM FA-LAES-GRUNDDATA                                      
062700*RC      END-IF                                                           
062800*RC    END-IF                                                             
062810*RC    CONTINUE                                                           
062900*RC  END-IF                                                               
063000                                                                          
063010     IF MFS-UPDATE OR MFS-ENTER                                           
063011         IF INDATA-OK OR INDATA-OK-2                                      
063020            PERFORM FA-LAES-GRUNDDATA                                     
063021         END-IF                                                           
063022     ELSE                                                                 
063023         MOVE MSGI-IDDISTR  TO SPAR-IDDISTR-IN                            
063024         MOVE MSGI-IDKUNDNR TO SPAR-IDKUNDNR-IN                           
063025         PERFORM FA-LAES-GRUNDDATA                                        
063030     END-IF                                                               
063100     MOVE '002'      TO MSGI-KDCALL                                       
063200     MOVE '4707'     TO SPAR-IDTRANS                                      
063300     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
063400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
063500     .                                                                    
063600     EJECT                                                                
063700                                                                          
063800 FA-LAES-GRUNDDATA SECTION.                                               
063900****  FOR HANDLING FEE                                                    
063910     PERFORM FAA-GET-PARTNR-FTG                                           
063920     IF INDATA-OK OR INDATA-OK-2                                          
064000        PERFORM DB2-DCL-OPN-TP8TRET-CRS                                   
064100        PERFORM DB2-FETCH-TP8TRET-CRS                                     
064110        MOVE SQLCODE                  TO SQLCODE-TP8TRET                  
064200        IF RADER-FINNS                                                    
064300          MOVE +1  TO INDX                                                
064400          PERFORM UNTIL RADER-SAKNAS OR INDX > MAX-INDX                   
064500                                                                          
064600            IF TRET-IDKUNDNR = 0                                          
064601               MOVE '0'               TO MOD-IDKUNDNR (INDX)              
064602            ELSE                                                          
064603               MOVE TRET-IDKUNDNR        TO W-IDKUNDNR-N                  
064604               MOVE W-IDKUNDNR-N         TO MOD-IDKUNDNR (INDX)           
064605               INSPECT MOD-IDKUNDNR(INDX) REPLACING                       
064606                                       LEADING ZERO BY SPACE              
064607            END-IF                                                        
064610            MOVE TRET-KDANMORS        TO MOD-KDANMORS (INDX)              
064620            MOVE TRET-PRARTNTO        TO MOD-PRARTNTO (INDX)              
064630            MOVE TRET-PRARTNTO-LDC    TO MOD-PRARTNTO-LDC (INDX)          
064700            IF TRET-FLINVFEE = 'J'                                        
064800              MOVE 'Y'                TO MOD-FLINVFEE (INDX)              
064900            ELSE                                                          
065000              MOVE TRET-FLINVFEE      TO MOD-FLINVFEE (INDX)              
065100            END-IF                                                        
065300            IF TRET-FLINVLDC = 'J'                                        
065400              MOVE 'Y'                TO MOD-FLINVLDC (INDX)              
065500            ELSE                                                          
065600              MOVE TRET-FLINVLDC      TO MOD-FLINVLDC (INDX)              
065700            END-IF                                                        
065910            MOVE TRET-IDUSER          TO MOD-IDUSER   (INDX)              
066000            MOVE TRET-DAUPPDAT        TO MOD-DAUPPDAT (INDX)              
066100                                                                          
066200            PERFORM DB2-FETCH-TP8TRET-CRS                                 
066300            ADD +1  TO INDX                                               
066400          END-PERFORM                                                     
066500        END-IF                                                            
066600        PERFORM DB2-CLOSE-TP8TRET-CRS                                     
066700                                                                          
066800*******  FOR VALUE LIMIT DESCREPANCY                                      
066900        PERFORM DB2-DCL-OPN-TP8GRET-CRS                                   
067000        PERFORM DB2-FETCH-TP8GRET-CRS                                     
067010        MOVE SQLCODE                  TO SQLCODE-TP8GRET                  
067100        IF RADER-FINNS                                                    
067200          MOVE +1  TO INDX                                                
067300          PERFORM UNTIL RADER-SAKNAS OR INDX > MAX-INDX                   
067502            IF GRET-IDKUNDNR = 0                                          
067503               MOVE '0'               TO MOD-IDKUNDNR-GRET (INDX)         
067504            ELSE                                                          
067505               MOVE GRET-IDKUNDNR     TO W-IDKUNDNR-N                     
067506               MOVE W-IDKUNDNR-N      TO MOD-IDKUNDNR-GRET (INDX)         
067507               INSPECT MOD-IDKUNDNR-GRET(INDX) REPLACING                  
067508                                       LEADING ZERO BY SPACE              
067509            END-IF                                                        
067510            MOVE GRET-KDANMORS        TO MOD-KDANMORS-GRET (INDX)         
067700            IF GRET-FLINVLDC = 'J'                                        
067800              MOVE 'Y'                TO MOD-FLINVLDC-GRET (INDX)         
067900            ELSE                                                          
068000              MOVE GRET-FLINVLDC      TO MOD-FLINVLDC-GRET (INDX)         
068100            END-IF                                                        
068110            MOVE GRET-SUARTBTO-MIN    TO MOD-SUARTBTO-GRET (INDX)         
068200            MOVE GRET-SUARTBTO-LDC-MIN                                    
068210                                    TO MOD-SUARTBTO-LDC-GRET(INDX)        
068300            MOVE GRET-IDUSER          TO MOD-IDUSER-GRET   (INDX)         
068400            MOVE GRET-DAUPPDAT        TO MOD-DAUPPDAT-GRET (INDX)         
068500                                                                          
068600            PERFORM DB2-FETCH-TP8GRET-CRS                                 
068700            ADD +1  TO INDX                                               
068800          END-PERFORM                                                     
068900        END-IF                                                            
069000                                                                          
069100        PERFORM DB2-CLOSE-TP8GRET-CRS                                     
069101     END-IF                                                               
069102***DISPLAY 'NO RECORDS EXISTS WHEN TRET&GRET HAVE NO ROWS***              
069103     IF SQLCODE-TP8TRET = 100 AND                                         
069104        SQLCODE-TP8GRET = 100                                             
069105*         MOVE ERR-DIST-KUND-SAKNAS  TO MED-IDMFSFEL                      
069106*         CALL WMEDKONV USING MED-WMEDAREA                                
069107*         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
069108          MOVE ERR-DEFAULT-CUSTOMER TO MOD-TEMFSFEL                       
069109     END-IF                                                               
069200     .                                                                    
069300     EJECT                                                                
069400                                                                          
069410 FAA-GET-PARTNR-FTG SECTION.                                              
069411                                                                          
069412     PERFORM IMS-GU-WDB201                                                
069413     IF SEGMENT-SAKNAS                                                    
069414**** CHECK WITH DEFAULT CUSTOMER 0                                        
069415        IF W-IDKUNDNR = 0                                                 
069416           MOVE W-IDDISTR    TO W-IDDISTR-MIN                             
069417                                W-IDDISTR-MAX                             
069418           PERFORM IMS-GU-WDB201-MIN-MAX                                  
069419           IF SEGMENT-FINNS                                               
069420              MOVE GMT-IDPARTNR    TO MOD-IDPARTNR-UT                     
069421              MOVE GMT-IDFTG       TO MOD-IDFTG-UT                        
069422           ELSE                                                           
069424             MOVE ERR-DIST-KUND-SAKNAS  TO MED-IDMFSFEL                   
069425*            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKUNDNR-ATTR                 
069426             CALL WMEDKONV USING MED-WMEDAREA                             
069427             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
069428             MOVE NEJ TO INDATA-SW                                        
069429                         INDATA-SW-2                                      
069430           END-IF                                                         
069431        ELSE                                                              
069432          MOVE ERR-DIST-KUND-SAKNAS  TO MED-IDMFSFEL                      
069433*         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKUNDNR-ATTR                    
069434          CALL WMEDKONV USING MED-WMEDAREA                                
069435          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
069436          MOVE NEJ TO INDATA-SW                                           
069437                      INDATA-SW-2                                         
069438        END-IF                                                            
069439     ELSE                                                                 
069440        MOVE GMT-IDPARTNR    TO MOD-IDPARTNR-UT                           
069441        MOVE GMT-IDFTG       TO MOD-IDFTG-UT                              
069442     END-IF                                                               
069443     .                                                                    
069444     EJECT                                                                
069450                                                                          
069500 G-KOLLA-INPUT SECTION.                                                   
069600     MOVE JA   TO INDATA-SW                                               
069700                                                                          
069800     IF MID-INPUT = ALL '+'                                               
069900       MOVE NEJ TO INDATA-SW                                              
070000     ELSE                                                                 
070100**** HANDLING FEE                                                         
070200       IF MID-KDBEHX = ALL '+'                                            
070300         MOVE NEJ TO INDATA-SW                                            
070400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBEHX-ATTR                       
070500       ELSE                                                               
070600         IF MID-KDBEHX = 'N' OR 'I' OR 'U' OR 'C' OR 'D'                  
070700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBEHX-ATTR                   
070710*          MOVE MID-KDBEHX TO MOD-KDBEHX-UPD                              
070800         ELSE                                                             
070900           MOVE NEJ TO INDATA-SW                                          
071000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBEHX-ATTR                     
071100         END-IF                                                           
071200       END-IF                                                             
071201*                                                                         
071202       MOVE W-IDKUNDNR        TO W-IDKUNDNR-INPUT                         
071203*                                                                         
071400       IF MID-KDANMORS-UPD = ALL '+'                                      
071500         MOVE NEJ TO INDATA-SW                                            
071600         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-ATTR                 
071700       ELSE                                                               
071800         IF MID-KDANMORS-UPD = '72' OR '98' OR 'RR'                       
071900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDANMORS-ATTR                 
071910*          MOVE MID-KDANMORS-UPD    TO MOD-KDANMORS-UPD                   
072000         ELSE                                                             
072100           MOVE NEJ TO INDATA-SW                                          
072200           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDANMORS-ATTR                 
072300         END-IF                                                           
072400       END-IF                                                             
072500                                                                          
072600       IF MID-FLINVFEE-UPD = ALL '+'                                      
072700         IF MID-KDBEHX = 'N' OR 'I'                                       
072800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVFEE-ATTR                 
072900           MOVE NEJ TO INDATA-SW                                          
073000         ELSE                                                             
073100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVFEE-ATTR                 
073200         END-IF                                                           
073300       ELSE                                                               
073400         IF MID-FLINVFEE-UPD = ALL 'J' OR 'Y' OR 'N'                      
073500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVFEE-ATTR                 
073510*          MOVE MID-FLINVFEE-UPD   TO MOD-FLINVFEE-UPD                    
073600         ELSE                                                             
073700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVFEE-ATTR                 
073800           MOVE NEJ TO INDATA-SW                                          
073900         END-IF                                                           
074000       END-IF                                                             
074100                                                                          
074200       IF MID-PRARTNTO-UPD = ALL '+'                                      
074300         IF MID-KDBEHX = 'N' OR 'I'                                       
074400           MOVE ERR-PRICE-MISSING    TO MED-IDMFSFEL                      
074500           MOVE MFS-NUM-FAELT-FEL    TO MOD-PRARTNTO-ATTR                 
074600           MOVE NEJ TO INDATA-SW                                          
074700         ELSE                                                             
074800           MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRARTNTO-ATTR                 
074900         END-IF                                                           
075000       ELSE                                                               
075100         MOVE ZERO                   TO WS-PRARTNTO-JFR1                  
075200         MOVE MID-PRARTNTO-UPD       TO DEC-IDFRIDATA                     
075300         MOVE +7                     TO DEC-KVHELTAL                      
075400         MOVE +2                     TO DEC-KVDECIMAL                     
075500         CALL WDECEDIT USING DEC-WDECAREA                                 
075600                                                                          
075700         IF DEC-KDSVAR-FEL                                                
075800           MOVE MFS-NUM-FAELT-FEL    TO MOD-PRARTNTO-ATTR                 
075900           MOVE NEJ                  TO INDATA-SW                         
076000         ELSE                                                             
076100           MOVE DEC-IDEDITDATA         TO WS-PRARTNTO-JFR1                
076200           IF WS-PRARTNTO-JFR1 > ZERO                                     
076300           OR WS-PRARTNTO-JFR1 = ZERO                                     
076400             MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRARTNTO-ATTR               
076410*            MOVE MID-PRARTNTO-UPD   TO MOD-PRARTNTO-UPD                  
076500           ELSE                                                           
076600             MOVE MFS-NUM-FAELT-FEL    TO MOD-PRARTNTO-ATTR               
076700             MOVE NEJ                  TO INDATA-SW                       
076800           END-IF                                                         
076900         END-IF                                                           
077000       END-IF                                                             
077100                                                                          
077200       IF MID-PRARTNTO-LDC-UPD = ALL '+'                                  
077300         IF MID-KDBEHX = 'N' OR 'I'                                       
077400           MOVE ERR-PRICE-MISSING    TO MED-IDMFSFEL                      
077500           MOVE MFS-NUM-FAELT-FEL    TO MOD-PRARTNTO-LDC-ATTR             
077600           MOVE NEJ TO INDATA-SW                                          
077700         ELSE                                                             
077800           MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRARTNTO-LDC-ATTR             
077900         END-IF                                                           
078000       ELSE                                                               
078100         MOVE ZERO                   TO WS-PRARTNTO-JFR2                  
078200         MOVE MID-PRARTNTO-LDC-UPD   TO DEC-IDFRIDATA                     
078300         MOVE +7                     TO DEC-KVHELTAL                      
078400         MOVE +2                     TO DEC-KVDECIMAL                     
078500         CALL WDECEDIT USING DEC-WDECAREA                                 
078600                                                                          
078700         IF DEC-KDSVAR-FEL                                                
078800           MOVE MFS-NUM-FAELT-FEL    TO MOD-PRARTNTO-LDC-ATTR             
078900           MOVE NEJ                  TO INDATA-SW                         
079000         ELSE                                                             
079100           MOVE DEC-IDEDITDATA         TO WS-PRARTNTO-JFR2                
079200           IF WS-PRARTNTO-JFR2 > ZERO                                     
079300           OR WS-PRARTNTO-JFR2 = ZERO                                     
079400             MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRARTNTO-ATTR               
079410*            MOVE MID-PRARTNTO-LDC-UPD TO MOD-PRARTNTO-LDC-UPD            
079500           ELSE                                                           
079600             MOVE MFS-NUM-FAELT-FEL    TO MOD-PRARTNTO-LDC-ATTR           
079700             MOVE NEJ                  TO INDATA-SW                       
079800           END-IF                                                         
079900         END-IF                                                           
080000       END-IF                                                             
080100                                                                          
080200       IF MID-FLINVLDC-UPD = ALL '+'                                      
080300         IF MID-KDBEHX = 'N' OR 'I'                                       
080400           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVLDC-ATTR                 
080500           MOVE NEJ TO INDATA-SW                                          
080600         ELSE                                                             
080700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVLDC-ATTR                 
080800         END-IF                                                           
080900       ELSE                                                               
081000         IF MID-FLINVLDC-UPD = 'J' OR 'Y' OR 'N'                          
081100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVLDC-ATTR                 
081110*          MOVE MID-FLINVLDC-UPD   TO MOD-FLINVLDC-UPD                    
081200         ELSE                                                             
081300           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVLDC-ATTR                 
081400           MOVE NEJ TO INDATA-SW                                          
081500         END-IF                                                           
081600       END-IF                                                             
081700     END-IF                                                               
081800                                                                          
081900     IF  MID-INPUT   = ALL '+'                                            
082000       MOVE NEJ TO INDATA-SW                                              
082100     ELSE                                                                 
082200* VALIDATE IF THE CUSTOMER EXISTS BEFORE INSERTING TO HANDLING FEE        
082300* TABLE AND DISCREPENCY TABLE                                             
082310       IF INDATA-OK                                                       
082311          IF W-IDKUNDNR-INPUT = 0                                         
082313             MOVE W-IDDISTR    TO W-IDDISTR-MIN                           
082314                                  W-IDDISTR-MAX                           
082315             PERFORM IMS-GU-WDB201-MIN-MAX                                
082316             IF SEGMENT-FINNS                                             
082317                MOVE GMT-IDPARTNR    TO W-IDPARTNR                        
082318                MOVE GMT-IDFTG       TO W-IDFTG                           
082319             END-IF                                                       
082320          ELSE                                                            
082330             MOVE W-IDKUNDNR-INPUT TO W-IDKUNDNR                          
082400             PERFORM IMS-GU-WDB201                                        
082401             MOVE W-IDKUNDNR-N     TO W-IDKUNDNR                          
082500             IF SEGMENT-SAKNAS                                            
082600               MOVE ERR-BETALARE-SAKNAS   TO MED-IDMFSFEL                 
082610*              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKUNDNR-ATTR               
082700               MOVE NEJ TO INDATA-SW                                      
082710             ELSE                                                         
082720               MOVE GMT-IDPARTNR    TO W-IDPARTNR                         
082730               MOVE GMT-IDFTG       TO W-IDFTG                            
082800             END-IF                                                       
082801          END-IF                                                          
082810       END-IF                                                             
083000       IF INDATA-FEL                                                      
083100         IF MED-IDMFSFEL  = SPACE                                         
083200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
083300         END-IF                                                           
083400         CALL WMEDKONV USING MED-WMEDAREA                                 
083500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
083600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
083610         PERFORM MFS-ROER-EJ-FAELT-UT-2                                   
083700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
083710         PERFORM MFS-DONT-TOUCH-PART-FTG                                  
083800       END-IF                                                             
083900     END-IF                                                               
084000     .                                                                    
084100     EJECT                                                                
084200                                                                          
084300 G-KOLLA-INPUT-2 SECTION.                                                 
084400     MOVE JA   TO INDATA-SW-2                                             
084500                                                                          
084600     IF MID-INPUT-2 = ALL '+'                                             
084700       MOVE NEJ TO INDATA-SW-2                                            
084800     ELSE                                                                 
084900**** VALUE LIMIT DESCREPANCY                                              
085000       IF MID-KDBEHX-GRET = ALL '+'                                       
085100         MOVE NEJ TO INDATA-SW-2                                          
085200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBEHX-2-ATTR                     
085300       ELSE                                                               
085400         IF MID-KDBEHX-GRET = 'N' OR 'I' OR 'U' OR 'C' OR 'D'             
085500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBEHX-2-ATTR                 
085510*          MOVE MID-KDBEHX-GRET       TO MOD-KDBEHX-GRET-UPD              
085600         ELSE                                                             
085700           MOVE NEJ TO INDATA-SW-2                                        
085800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBEHX-2-ATTR                   
085900         END-IF                                                           
086000       END-IF                                                             
086100*                                                                         
086101       MOVE W-IDKUNDNR        TO W-IDKUNDNR-INPUT                         
086102*                                                                         
086510       IF MID-KDANMORS-GRET = ALL '+'                                     
086520         MOVE NEJ TO INDATA-SW-2                                          
086530         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-2-ATTR               
086540       ELSE                                                               
086550         IF MID-KDANMORS-GRET = '72' OR '98' OR 'RR'                      
086560           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDANMORS-2-ATTR               
086570*          MOVE MID-KDANMORS-GRET   TO MOD-KDANMORS-GRET-UPD              
086580         ELSE                                                             
086590           MOVE NEJ TO INDATA-SW-2                                        
086591           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDANMORS-2-ATTR               
086592         END-IF                                                           
086593       END-IF                                                             
086594                                                                          
086700       IF MID-SUARTBTO-GRET = ALL '+'                                     
086800         IF MID-KDBEHX-GRET = 'N' OR 'I'                                  
086900           MOVE ERR-PRICE-MISSING    TO MED-IDMFSFEL                      
087000           MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-2-ATTR               
087100           MOVE NEJ TO INDATA-SW-2                                        
087200         ELSE                                                             
087300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-SUARTBTO-2-ATTR               
087400         END-IF                                                           
087500       ELSE                                                               
087600         MOVE ZERO                   TO WS-PRARTNTO-JFR3                  
087700         MOVE MID-SUARTBTO-GRET      TO DEC-IDFRIDATA                     
087800         MOVE +7                     TO DEC-KVHELTAL                      
087900         MOVE +2                     TO DEC-KVDECIMAL                     
088000         CALL WDECEDIT USING DEC-WDECAREA                                 
088100                                                                          
088200         IF DEC-KDSVAR-FEL                                                
088300           MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-2-ATTR               
088400           MOVE NEJ                  TO INDATA-SW-2                       
088500         ELSE                                                             
088600           MOVE DEC-IDEDITDATA         TO WS-PRARTNTO-JFR3                
088700           IF WS-PRARTNTO-JFR3 > ZERO                                     
088800           OR WS-PRARTNTO-JFR3 = ZERO                                     
088900             MOVE MFS-NUM-FAELT-RAETT  TO MOD-SUARTBTO-2-ATTR             
088910*            MOVE MID-SUARTBTO-GRET    TO MOD-SUARTBTO-GRET-UPD           
089000           ELSE                                                           
089100             MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-2-ATTR             
089200             MOVE NEJ                  TO INDATA-SW-2                     
089300           END-IF                                                         
089400         END-IF                                                           
089500       END-IF                                                             
089600                                                                          
089700       IF MID-FLINVLDC-GRET = ALL '+'                                     
089800         IF MID-KDBEHX-GRET = 'N' OR 'I'                                  
089900           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVLDC-2-ATTR               
090000           MOVE NEJ TO INDATA-SW-2                                        
090100         ELSE                                                             
090200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVLDC-2-ATTR               
090300         END-IF                                                           
090400       ELSE                                                               
090500         IF MID-FLINVLDC-GRET = 'J' OR 'Y' OR 'N'                         
090600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVLDC-2-ATTR               
090610*          MOVE MID-FLINVLDC-GRET    TO MOD-FLINVLDC-GRET-UPD             
090700         ELSE                                                             
090800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVLDC-2-ATTR               
090900           MOVE NEJ TO INDATA-SW-2                                        
091000         END-IF                                                           
091100       END-IF                                                             
091200                                                                          
091300       IF MID-SUARTBTO-LDC-GRET = ALL '+'                                 
091400         IF MID-KDBEHX-GRET = 'N' OR 'I'                                  
091500           MOVE ERR-PRICE-MISSING    TO MED-IDMFSFEL                      
091600           MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-LDC-2-ATTR           
091700           MOVE NEJ TO INDATA-SW-2                                        
091800         ELSE                                                             
091900           MOVE MFS-NUM-FAELT-RAETT  TO MOD-SUARTBTO-LDC-2-ATTR           
092000         END-IF                                                           
092100       ELSE                                                               
092200         MOVE ZERO                   TO WS-PRARTNTO-JFR4                  
092300         MOVE MID-SUARTBTO-LDC-GRET  TO DEC-IDFRIDATA                     
092400         MOVE +7                     TO DEC-KVHELTAL                      
092500         MOVE +2                     TO DEC-KVDECIMAL                     
092600         CALL WDECEDIT USING DEC-WDECAREA                                 
092700                                                                          
092800         IF DEC-KDSVAR-FEL                                                
092900           MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-LDC-2-ATTR           
093000           MOVE NEJ                  TO INDATA-SW-2                       
093100         ELSE                                                             
093200           MOVE DEC-IDEDITDATA         TO WS-PRARTNTO-JFR4                
093300           IF WS-PRARTNTO-JFR4 > ZERO                                     
093400           OR WS-PRARTNTO-JFR4 = ZERO                                     
093500             MOVE MFS-NUM-FAELT-RAETT  TO MOD-SUARTBTO-LDC-2-ATTR         
093510*            MOVE MID-SUARTBTO-LDC-GRET TO                                
093520*                                      MOD-SUARTBTO-LDC-GRET-UPD          
093600           ELSE                                                           
093700             MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-LDC-2-ATTR         
093800             MOVE NEJ                  TO INDATA-SW-2                     
093900           END-IF                                                         
094000         END-IF                                                           
094100       END-IF                                                             
094200     END-IF                                                               
094300                                                                          
094400     IF  MID-INPUT-2 = ALL '+'                                            
094500       MOVE NEJ TO INDATA-SW-2                                            
094600     ELSE                                                                 
095310       IF INDATA-OK-2                                                     
095311          IF W-IDKUNDNR-INPUT = 0                                         
095312             MOVE W-IDDISTR    TO W-IDDISTR-MIN                           
095313                                  W-IDDISTR-MAX                           
095314             PERFORM IMS-GU-WDB201-MIN-MAX                                
095315             IF SEGMENT-FINNS                                             
095316                MOVE GMT-IDPARTNR    TO W-IDPARTNR                        
095317                MOVE GMT-IDFTG       TO W-IDFTG                           
095318             END-IF                                                       
095320          ELSE                                                            
095321             MOVE W-IDKUNDNR-INPUT TO W-IDKUNDNR                          
095330             PERFORM IMS-GU-WDB201                                        
095340             MOVE W-IDKUNDNR-N     TO W-IDKUNDNR                          
095350             IF SEGMENT-SAKNAS                                            
095360               MOVE ERR-BETALARE-SAKNAS   TO MED-IDMFSFEL                 
095370*              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKUNDNR-ATTR               
095380               MOVE NEJ TO INDATA-SW-2                                    
095390             ELSE                                                         
095391               MOVE GMT-IDPARTNR    TO W-IDPARTNR                         
095392               MOVE GMT-IDFTG       TO W-IDFTG                            
095393             END-IF                                                       
095394          END-IF                                                          
095400       END-IF                                                             
095500       IF INDATA-FEL-2                                                    
095600         IF MED-IDMFSFEL  = SPACE                                         
095700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
095800         END-IF                                                           
095900         CALL WMEDKONV USING MED-WMEDAREA                                 
096000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
096100         PERFORM MFS-ROER-EJ-FAELT-UT                                     
096110         PERFORM MFS-ROER-EJ-FAELT-UT-2                                   
096200         PERFORM MFS-ROER-EJ-FAELT-IN-2                                   
096210         PERFORM MFS-DONT-TOUCH-PART-FTG                                  
096300       END-IF                                                             
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700                                                                          
096800 H-UPPDATERA SECTION.                                                     
096900     IF  INDATA-OK                                                        
097000       IF MID-KDBEHX = 'N' OR 'I'                                         
097001         INITIALIZE TRET-TP8TRET                                          
097010         MOVE W-IDDISTR       TO TRET-IDDISTR                             
097020         MOVE W-IDKUNDNR-INPUT  TO TRET-IDKUNDNR                          
097030         MOVE MID-KDANMORS-UPD TO TRET-KDANMORS                           
097100         MOVE W-IDPARTNR      TO TRET-IDPARTNR                            
097200         MOVE W-IDFTG         TO TRET-IDFTG                               
097210                                                                          
097400                                                                          
097500         IF MID-FLINVFEE-UPD = 'Y' OR 'J'                                 
097600           MOVE 'J'           TO TRET-FLINVFEE                            
097700         ELSE                                                             
097800           MOVE 'N'           TO TRET-FLINVFEE                            
097900         END-IF                                                           
098000                                                                          
098100         MOVE WS-PRARTNTO-JFR1 TO TRET-PRARTNTO                           
098200                                                                          
098300         IF MID-FLINVLDC-UPD = 'Y' OR 'J'                                 
098400           MOVE 'J'       TO TRET-FLINVLDC                                
098500         ELSE                                                             
098600           MOVE 'N'       TO TRET-FLINVLDC                                
098700         END-IF                                                           
098800                                                                          
098900         MOVE WS-PRARTNTO-JFR2 TO TRET-PRARTNTO-LDC                       
099000                                                                          
099100         MOVE MSG-SIGNON-USERID TO TRET-IDUSER                            
099200                                                                          
099300         MOVE FUNCTION CURRENT-DATE (1:8) TO TRET-DAREGDAT                
099400                                             TRET-DAUPPDAT                
099600         PERFORM DB2-INSERT-TP8TRET-TAB                                   
099700                                                                          
099800         IF RAD-DUBLETT                                                   
099900           MOVE ERR-RAD-FINNS-REDAN TO MED-IDMFSFEL                       
100000           CALL WMEDKONV USING MED-WMEDAREA                               
100100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
100200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
100210           PERFORM MFS-ROER-EJ-FAELT-UT-2                                 
100300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
100400           PERFORM MFS-LAES-IN-IGEN                                       
100401           PERFORM MFS-DONT-TOUCH-PART-FTG                                
100410           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-ATTR               
100500         ELSE                                                             
100600           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
100700           CALL WMEDKONV USING MED-WMEDAREA                               
100800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
100900           PERFORM MFS-FORM-ATTR                                          
101000           PERFORM MFS-RENSA-FAELT-IN                                     
101100         END-IF                                                           
101200       END-IF                                                             
101300                                                                          
101400       IF MID-KDBEHX = 'U' OR 'C'                                         
101500         PERFORM DB2-SELECT-TP8TRET-KOD                                   
101600         IF RADER-FINNS                                                   
104410           IF W-IDKUNDNR = 0                                              
104420              PERFORM H1-UPDATE-TP8TRET-CURSOR                            
104430           ELSE                                                           
104440              PERFORM S01-POPULATE-TP8TRET                                
104500              PERFORM DB2-UPDATE-TP8TRET-TAB                              
104600           END-IF                                                         
104700*          IF CURSOR-OK                                                   
104800             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
104900             CALL WMEDKONV USING MED-WMEDAREA                             
105000             MOVE MED-MFSINF TO MOD-TEMFSINF                              
105100             PERFORM MFS-FORM-ATTR                                        
105200             PERFORM MFS-RENSA-FAELT-IN                                   
105300*          END-IF                                                         
105400         ELSE                                                             
105500           MOVE ERR-KOD-FINNS-EJ-REG TO MED-IDMFSFEL                      
105600           CALL WMEDKONV USING MED-WMEDAREA                               
105700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
105800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
105810           PERFORM MFS-ROER-EJ-FAELT-UT-2                                 
105900           PERFORM MFS-ROER-EJ-FAELT-IN                                   
106000           PERFORM MFS-LAES-IN-IGEN                                       
106001           PERFORM MFS-DONT-TOUCH-PART-FTG                                
106010           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-ATTR               
106100         END-IF                                                           
106200       END-IF                                                             
106300                                                                          
106400       IF MID-KDBEHX = 'D'                                                
106500         PERFORM DB2-SELECT-TP8TRET-KOD                                   
106600         IF RADER-FINNS                                                   
106700           PERFORM DB2-DELETE-TP8TRET-TAB                                 
106800                                                                          
106900           IF CURSOR-OK                                                   
107000             MOVE INF-DELETE-DONE TO MED-IDMFSINF                         
107100             CALL WMEDKONV USING MED-WMEDAREA                             
107200             MOVE MED-MFSINF TO MOD-TEMFSINF                              
107300             PERFORM MFS-FORM-ATTR                                        
107400             PERFORM MFS-RENSA-FAELT-IN                                   
107500           END-IF                                                         
107600         ELSE                                                             
107700           MOVE ERR-KOD-FINNS-EJ-REG TO MED-IDMFSFEL                      
107800           CALL WMEDKONV USING MED-WMEDAREA                               
107900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
107920           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-ATTR               
108000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
108010           PERFORM MFS-ROER-EJ-FAELT-UT-2                                 
108100           PERFORM MFS-ROER-EJ-FAELT-IN                                   
108200           PERFORM MFS-LAES-IN-IGEN                                       
108201           PERFORM MFS-DONT-TOUCH-PART-FTG                                
108210           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-ATTR               
108300         END-IF                                                           
108400       END-IF                                                             
108500     END-IF                                                               
108600     .                                                                    
108700     EJECT                                                                
108701                                                                          
108900 H1-UPDATE-TP8TRET-CURSOR SECTION.                                        
108901                                                                          
108902     PERFORM DB2-DECLARE-OPEN-TP8TRET-CSR                                 
108903     PERFORM DB2-FETCH-TP8TRET-CSR                                        
108904     PERFORM UNTIL RADER-SAKNAS                                           
108905        PERFORM S01-POPULATE-TP8TRET                                      
108906        PERFORM DB2-UPDATE-TP8TRET-CSR                                    
108907        PERFORM DB2-FETCH-TP8TRET-CSR                                     
108908     END-PERFORM                                                          
108909     PERFORM DB2-CLOSE-TP8TRET-CSR                                        
108910     .                                                                    
108920     EJECT                                                                
108930                                                                          
108950 H-UPPDATERA-2 SECTION.                                                   
109000     IF  INDATA-OK-2                                                      
109100**** VALUE LIMIT DESCREPANCY                                              
109200       IF MID-KDBEHX-GRET = 'N' OR 'I'                                    
109201         INITIALIZE GRET-TP8GRET                                          
109210         MOVE W-IDDISTR       TO GRET-IDDISTR                             
109220         MOVE W-IDKUNDNR-INPUT  TO GRET-IDKUNDNR                          
109300         MOVE W-IDPARTNR      TO GRET-IDPARTNR                            
109400         MOVE W-IDFTG         TO GRET-IDFTG                               
109500         MOVE MID-KDANMORS-GRET TO GRET-KDANMORS                          
109600                                                                          
109700         MOVE WS-PRARTNTO-JFR3 TO GRET-SUARTBTO-MIN                       
109800                                                                          
109900         IF MID-FLINVLDC-GRET = 'Y' OR 'J'                                
110000           MOVE 'J'       TO GRET-FLINVLDC                                
110100         ELSE                                                             
110200           MOVE 'N'       TO GRET-FLINVLDC                                
110300         END-IF                                                           
110400                                                                          
110500         MOVE WS-PRARTNTO-JFR4 TO GRET-SUARTBTO-LDC-MIN                   
110600                                                                          
110700         MOVE MSG-SIGNON-USERID TO GRET-IDUSER                            
110800                                                                          
110900         MOVE FUNCTION CURRENT-DATE (1:8) TO GRET-DAREGDAT                
111000                                             GRET-DAUPPDAT                
111100                                                                          
111200         PERFORM DB2-INSERT-TP8GRET-TAB                                   
111300                                                                          
111400         IF RAD-DUBLETT                                                   
111500           MOVE ERR-RAD-FINNS-REDAN TO MED-IDMFSFEL                       
111600           CALL WMEDKONV USING MED-WMEDAREA                               
111700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
111800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
111810           PERFORM MFS-ROER-EJ-FAELT-UT-2                                 
111900           PERFORM MFS-ROER-EJ-FAELT-IN-2                                 
112000           PERFORM MFS-LAES-IN-IGEN-2                                     
112001           PERFORM MFS-DONT-TOUCH-PART-FTG                                
112010           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-2-ATTR             
112100         ELSE                                                             
112200           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
112300           CALL WMEDKONV USING MED-WMEDAREA                               
112400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
112500           PERFORM MFS-FORM-ATTR-2                                        
112600           PERFORM MFS-RENSA-FAELT-IN-2                                   
112610*          MOVE SPACES TO MID-INPUT-2                                     
112620*                         MOD-INDATA-2                                    
112700         END-IF                                                           
112800       END-IF                                                             
112900                                                                          
113000       IF MID-KDBEHX-GRET = 'U' OR 'C'                                    
113100         PERFORM DB2-SELECT-TP8GRET-KOD                                   
113200         IF RADER-FINNS                                                   
115510           IF W-IDKUNDNR = 0                                              
115520              PERFORM H2-UPDATE-TP8GRET-DEFAULT                           
115530           ELSE                                                           
115531              PERFORM S02-POPULATE-TP8GRET                                
115540              PERFORM DB2-UPDATE-TP8GRET-TAB                              
115550           END-IF                                                         
115560                                                                          
115600*          IF CURSOR-OK                                                   
115700             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
115800             CALL WMEDKONV USING MED-WMEDAREA                             
115900             MOVE MED-MFSINF TO MOD-TEMFSINF                              
116000             PERFORM MFS-FORM-ATTR-2                                      
116100             PERFORM MFS-RENSA-FAELT-IN-2                                 
116110*          MOVE SPACES TO MID-INPUT-2                                     
116120*                         MOD-INDATA-2                                    
116200*          END-IF                                                         
116300         ELSE                                                             
116400           MOVE ERR-KOD-FINNS-EJ-REG TO MED-IDMFSFEL                      
116500           CALL WMEDKONV USING MED-WMEDAREA                               
116600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
116700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
116710           PERFORM MFS-ROER-EJ-FAELT-UT-2                                 
116800           PERFORM MFS-ROER-EJ-FAELT-IN-2                                 
116900           PERFORM MFS-LAES-IN-IGEN-2                                     
116901           PERFORM MFS-DONT-TOUCH-PART-FTG                                
116910           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-2-ATTR             
117000         END-IF                                                           
117100       END-IF                                                             
117200                                                                          
117300       IF MID-KDBEHX-GRET = 'D'                                           
117400         PERFORM DB2-SELECT-TP8GRET-KOD                                   
117500         IF RADER-FINNS                                                   
117600           PERFORM DB2-DELETE-TP8GRET-TAB                                 
117700                                                                          
117800           IF CURSOR-OK                                                   
117900             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
118000             CALL WMEDKONV USING MED-WMEDAREA                             
118100             MOVE MED-MFSINF TO MOD-TEMFSINF                              
118200             PERFORM MFS-FORM-ATTR-2                                      
118300             PERFORM MFS-RENSA-FAELT-IN-2                                 
118310*          MOVE SPACES TO MID-INPUT-2                                     
118320*                         MOD-INDATA-2                                    
118400           END-IF                                                         
118500         ELSE                                                             
118600           MOVE ERR-KOD-FINNS-EJ-REG TO MED-IDMFSFEL                      
118700           CALL WMEDKONV USING MED-WMEDAREA                               
118800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
118900           PERFORM MFS-ROER-EJ-FAELT-UT                                   
118910           PERFORM MFS-ROER-EJ-FAELT-UT-2                                 
119000           PERFORM MFS-ROER-EJ-FAELT-IN-2                                 
119100           PERFORM MFS-LAES-IN-IGEN-2                                     
119101           PERFORM MFS-DONT-TOUCH-PART-FTG                                
119110           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDANMORS-2-ATTR             
119200         END-IF                                                           
119300       END-IF                                                             
119400     END-IF                                                               
119500     .                                                                    
119600     EJECT                                                                
119700                                                                          
119710 H2-UPDATE-TP8GRET-DEFAULT SECTION.                                       
119720                                                                          
119730     PERFORM DB2-DECLARE-OPEN-TP8GRET-CSR                                 
119740     PERFORM DB2-FETCH-TP8GRET-CSR                                        
119750     PERFORM UNTIL RADER-SAKNAS                                           
119751        PERFORM S02-POPULATE-TP8GRET                                      
119760        PERFORM DB2-UPDATE-TP8GRET-CSR                                    
119770        PERFORM DB2-FETCH-TP8GRET-CSR                                     
119780     END-PERFORM                                                          
119790     PERFORM DB2-CLOSE-TP8GRET-CSR                                        
119791     .                                                                    
119792     EJECT                                                                
119793                                                                          
119794 S01-POPULATE-TP8TRET SECTION.                                            
119795                                                                          
119796     IF MID-FLINVFEE-UPD NOT = '+'                                        
119797       IF MID-FLINVFEE-UPD = 'Y' OR 'J'                                   
119798         MOVE 'J'           TO TRET-FLINVFEE                              
119799       ELSE                                                               
119800         MOVE 'N'           TO TRET-FLINVFEE                              
119801       END-IF                                                             
119802     END-IF                                                               
119803                                                                          
119804     IF MID-PRARTNTO-UPD NOT = ALL '+'                                    
119805       MOVE WS-PRARTNTO-JFR1 TO TRET-PRARTNTO                             
119806     END-IF                                                               
119807                                                                          
119808     IF MID-FLINVLDC-UPD NOT = '+'                                        
119809       IF MID-FLINVLDC-UPD = 'Y' OR 'J'                                   
119810         MOVE 'J'       TO TRET-FLINVLDC                                  
119811       ELSE                                                               
119812         MOVE 'N'       TO TRET-FLINVLDC                                  
119813       END-IF                                                             
119814     END-IF                                                               
119815                                                                          
119816     IF MID-PRARTNTO-LDC-UPD NOT = ALL '+'                                
119817       MOVE WS-PRARTNTO-JFR2 TO TRET-PRARTNTO-LDC                         
119818     END-IF                                                               
119819                                                                          
119820     MOVE MSG-SIGNON-USERID TO TRET-IDUSER                                
119821                                                                          
119822     MOVE FUNCTION CURRENT-DATE (1:8) TO TRET-DAUPPDAT                    
119823     .                                                                    
119824     EJECT                                                                
119825                                                                          
119828                                                                          
119829 S02-POPULATE-TP8GRET SECTION.                                            
119830                                                                          
119831     IF MID-SUARTBTO-GRET NOT = ALL '+'                                   
119832       MOVE WS-PRARTNTO-JFR3 TO GRET-SUARTBTO-MIN                         
119833     END-IF                                                               
119834                                                                          
119835     IF MID-FLINVLDC-GRET NOT = '+'                                       
119836       IF MID-FLINVLDC-GRET = 'Y' OR 'J'                                  
119837         MOVE 'J'       TO GRET-FLINVLDC                                  
119838       ELSE                                                               
119839         MOVE 'N'       TO GRET-FLINVLDC                                  
119840       END-IF                                                             
119841     END-IF                                                               
119842                                                                          
119843     IF MID-SUARTBTO-LDC-GRET NOT = ALL '+'                               
119844       MOVE WS-PRARTNTO-JFR4 TO GRET-SUARTBTO-LDC-MIN                     
119845     END-IF                                                               
119846                                                                          
119847     MOVE MSG-SIGNON-USERID TO GRET-IDUSER                                
119848                                                                          
119849     MOVE FUNCTION CURRENT-DATE (1:8) TO GRET-DAUPPDAT                    
119851     .                                                                    
119852     EJECT                                                                
119860 MFS-RENSA-FAELT-UT SECTION.                                              
119900*    --- ALLA UTDATA-FÄLT                                                 
120000     MOVE +1 TO INDX                                                      
120100     PERFORM UNTIL INDX > MAX-INDX                                        
120200       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR (INDX)                        
120210                               MOD-KDANMORS (INDX)                        
120300                               MOD-FLINVFEE (INDX)                        
120400                               MOD-PRARTNTO (INDX)                        
120500                               MOD-FLINVLDC (INDX)                        
120600                               MOD-PRARTNTO-LDC (INDX)                    
120700                               MOD-IDUSER   (INDX)                        
120800                               MOD-DAUPPDAT (INDX)                        
120900       ADD +1 TO INDX                                                     
121000     END-PERFORM                                                          
121100     .                                                                    
121200     SKIP3                                                                
121300                                                                          
121400 MFS-RENSA-FAELT-UT-2 SECTION.                                            
121500*    --- ALLA UTDATA-FÄLT                                                 
121600     MOVE +1 TO INDX                                                      
121700     PERFORM UNTIL INDX > MAX-INDX                                        
121800       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-GRET (INDX)                   
121810                               MOD-KDANMORS-GRET (INDX)                   
121900                               MOD-SUARTBTO-GRET (INDX)                   
122000                               MOD-FLINVLDC-GRET (INDX)                   
122100                               MOD-SUARTBTO-LDC-GRET (INDX)               
122200                               MOD-IDUSER-GRET   (INDX)                   
122300                               MOD-DAUPPDAT-GRET (INDX)                   
122400       ADD +1 TO INDX                                                     
122500     END-PERFORM                                                          
122600     .                                                                    
122700     SKIP3                                                                
122800                                                                          
122900 MFS-RENSA-FAELT-IN SECTION.                                              
123000*    --- ALLA INDATA-FÄLT                                                 
123100     MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-UPD                               
123210                             MOD-KDANMORS-UPD                             
123300                             MOD-FLINVFEE-UPD                             
123400                             MOD-PRARTNTO-UPD                             
123500                             MOD-FLINVLDC-UPD                             
123600                             MOD-PRARTNTO-LDC-UPD                         
123700     .                                                                    
123800     EJECT                                                                
123900                                                                          
124000 MFS-RENSA-FAELT-IN-2 SECTION.                                            
124100*    --- ALLA INDATA-FÄLT                                                 
124200     MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-GRET-UPD                          
124310                             MOD-KDANMORS-GRET-UPD                        
124400                             MOD-SUARTBTO-GRET-UPD                        
124500                             MOD-FLINVLDC-GRET-UPD                        
124600                             MOD-SUARTBTO-LDC-GRET-UPD                    
124700     .                                                                    
124800     EJECT                                                                
124900                                                                          
125000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
125100*    --- ALLA UTDATA-FÄLT                                                 
125200     MOVE +1 TO INDX                                                      
125300     PERFORM UNTIL INDX > MAX-INDX                                        
125400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR (INDX)                        
125410                               MOD-KDANMORS (INDX)                        
125500                               MOD-FLINVFEE (INDX)                        
125600                               MOD-PRARTNTO (INDX)                        
125700                               MOD-FLINVLDC (INDX)                        
125800                               MOD-PRARTNTO-LDC (INDX)                    
125900                               MOD-IDUSER   (INDX)                        
126000                               MOD-DAUPPDAT (INDX)                        
126100       ADD +1 TO INDX                                                     
126200     END-PERFORM                                                          
126300     .                                                                    
126400     SKIP3                                                                
126500                                                                          
126600 MFS-ROER-EJ-FAELT-UT-2 SECTION.                                          
126700*    --- ALLA UTDATA-FÄLT                                                 
126800     MOVE +1 TO INDX                                                      
126900     PERFORM UNTIL INDX > MAX-INDX                                        
127000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-GRET (INDX)                   
127010                               MOD-KDANMORS-GRET (INDX)                   
127100                               MOD-SUARTBTO-GRET (INDX)                   
127200                               MOD-FLINVLDC-GRET (INDX)                   
127300                               MOD-SUARTBTO-LDC-GRET (INDX)               
127400                               MOD-IDUSER-GRET   (INDX)                   
127500                               MOD-DAUPPDAT-GRET (INDX)                   
127600       ADD +1 TO INDX                                                     
127700     END-PERFORM                                                          
127800     .                                                                    
127900     SKIP3                                                                
128000                                                                          
128100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
128200*    --- ALLA INDATA-FÄLT                                                 
128300     MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEHX-UPD                             
128410                               MOD-KDANMORS-UPD                           
128500                               MOD-FLINVFEE-UPD                           
128600                               MOD-PRARTNTO-UPD                           
128700                               MOD-FLINVLDC-UPD                           
128800                               MOD-PRARTNTO-LDC-UPD                       
128900     .                                                                    
129000     EJECT                                                                
129100                                                                          
129200 MFS-ROER-EJ-FAELT-IN-2 SECTION.                                          
129300*    --- ALLA INDATA-FÄLT                                                 
129400     MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEHX-GRET-UPD                        
129510                               MOD-KDANMORS-GRET-UPD                      
129600                               MOD-SUARTBTO-GRET-UPD                      
129700                               MOD-FLINVLDC-GRET-UPD                      
129800                               MOD-SUARTBTO-LDC-GRET-UPD                  
129900     .                                                                    
130000     EJECT                                                                
130100                                                                          
130200 MFS-FORM-ATTR SECTION.                                                   
130300*    --- ALLA INDATA-FÄLT                                                 
130400     MOVE MFS-FORMATETS-ATTR TO MOD-KDBEHX-ATTR                           
130510                                MOD-KDANMORS-ATTR                         
130600                                MOD-FLINVFEE-ATTR                         
130700                                MOD-PRARTNTO-ATTR                         
130800                                MOD-FLINVLDC-ATTR                         
130900                                MOD-PRARTNTO-LDC-ATTR                     
131000     .                                                                    
131100     SKIP2                                                                
131200                                                                          
131300 MFS-FORM-ATTR-2 SECTION.                                                 
131400*    --- ALLA INDATA-FÄLT                                                 
131500     MOVE MFS-FORMATETS-ATTR TO MOD-KDBEHX-2-ATTR                         
131610                                MOD-KDANMORS-2-ATTR                       
131700                                MOD-SUARTBTO-2-ATTR                       
131800                                MOD-FLINVLDC-2-ATTR                       
131900                                MOD-SUARTBTO-LDC-2-ATTR                   
132000     .                                                                    
132100     SKIP2                                                                
132200                                                                          
132300 MFS-LAES-IN-IGEN SECTION.                                                
132400*    --- ALLA INDATA-FÄLT                                                 
132500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBEHX-ATTR                        
132610                                   MOD-KDANMORS-ATTR                      
132700                                   MOD-FLINVFEE-ATTR                      
132800                                   MOD-PRARTNTO-ATTR                      
132900                                   MOD-FLINVLDC-ATTR                      
133000                                   MOD-PRARTNTO-LDC-ATTR                  
133100     .                                                                    
133200     EJECT                                                                
133300 MFS-LAES-IN-IGEN-2 SECTION.                                              
133400*    --- ALLA INDATA-FÄLT                                                 
133500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBEHX-2-ATTR                      
133610                                   MOD-KDANMORS-2-ATTR                    
133700                                   MOD-SUARTBTO-2-ATTR                    
133800                                   MOD-FLINVLDC-2-ATTR                    
133900                                   MOD-SUARTBTO-LDC-2-ATTR                
134000     .                                                                    
134100     EJECT                                                                
134200                                                                          
134210 MFS-DONT-TOUCH-PART-FTG SECTION.                                         
134220*    --- ALLA INDATA-FÄLT                                                 
134230     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPARTNR-UT                        
134240                                   MOD-IDFTG-UT                           
134290     .                                                                    
134291     EJECT                                                                
134292                                                                          
134300**--- IMS SEKTIONER ---                                                   
134400     SKIP3                                                                
134500 IMS-GET-MSG SECTION.                                                     
134600     MOVE '  QC' TO GODK-STATUSKODER                                      
134700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
134800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100     SKIP3                                                                
135200                                                                          
135300 IMS-INSERT-MSG SECTION.                                                  
135400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
135500     MOVE SPACE TO GODK-STATUSKODER                                       
135600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
135700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000     EJECT                                                                
136100                                                                          
136200 IMS-GU-WDB101 SECTION.                                                   
136300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
136400          DELIMITED BY SIZE INTO SSA1                                     
136500     MOVE '  GE' TO GODK-STATUSKODER                                      
136600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
136700     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000     EJECT                                                                
137100                                                                          
137200 IMS-GU-WDB201 SECTION.                                                   
137300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
137400          DELIMITED BY SIZE INTO SSA1                                     
137500     MOVE '  GE' TO GODK-STATUSKODER                                      
137600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
137700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
137800     PERFORM IMS-STATUSKONTROLL                                           
137900     .                                                                    
138000     EJECT                                                                
138100                                                                          
138200 IMS-GU-WDB201-MIN-MAX SECTION.                                           
138300     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
138400                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
138500          DELIMITED BY SIZE INTO SSA1                                     
138600     MOVE '  GE' TO GODK-STATUSKODER                                      
138700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
138800     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
138900     PERFORM IMS-STATUSKONTROLL                                           
139000     .                                                                    
139100     EJECT                                                                
139200                                                                          
139300 IMS-STATUSKONTROLL SECTION.                                              
139400     SET STATUS-IX TO 1                                                   
139500     SEARCH GODK-STATUS                                                   
139600       AT END                                                             
139700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
139800         DELIMITED BY SIZE INTO FELTEXT                                   
139900         CALL FELLOG                                                      
140000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
140100         CONTINUE                                                         
140200     END-SEARCH                                                           
140300     .                                                                    
140400     EJECT                                                                
140500                                                                          
140600 DB2-DCL-OPN-TP8TRET-CRS  SECTION.                                        
140700     EXEC SQL                                                             
140800         DECLARE TP8TRET-CRS CURSOR FOR                                   
140900                                                                          
141000         SELECT  IDKUNDNR                                                 
141010                ,KDANMORS                                                 
141100                ,PRARTNTO                                                 
141200                ,PRARTNTO_LDC                                             
141300                ,FLINVFEE                                                 
141400                ,FLINVLDC                                                 
141500                ,IDPARTNR                                                 
141600                ,IDUSER                                                   
141700                ,DAREGDAT                                                 
141710                ,DAUPPDAT                                                 
141720                ,IDFTG                                                    
141800                                                                          
141900           FROM    TP8TRET                                                
142000           WHERE   IDDISTR  = :W-IDDISTR                                  
142100             AND   IDKUNDNR = :W-IDKUNDNR                                 
142200                                                                          
142300           ORDER BY KDANMORS                                              
142400     END-EXEC.                                                            
142500                                                                          
142600     EXEC SQL OPEN TP8TRET-CRS END-EXEC.                                  
142700                                                                          
142800     MOVE 000100  TO GODK-SQLCODEKODER                                    
142900     MOVE SQLCODE TO SQLCODE-WS                                           
143000     PERFORM DB2-STATUS-KONTROLL                                          
143100     .                                                                    
143200     SKIP3                                                                
143300                                                                          
143400 DB2-FETCH-TP8TRET-CRS  SECTION.                                          
143500     SKIP2                                                                
143600     EXEC SQL                                                             
143700       FETCH TP8TRET-CRS                                                  
143800       INTO                                                               
143900         :TRET-IDKUNDNR                                                   
143910        ,:TRET-KDANMORS                                                   
144000        ,:TRET-PRARTNTO                                                   
144100        ,:TRET-PRARTNTO-LDC                                               
144200        ,:TRET-FLINVFEE                                                   
144300        ,:TRET-FLINVLDC                                                   
144400        ,:TRET-IDPARTNR                                                   
144500        ,:TRET-IDUSER                                                     
144510        ,:TRET-DAREGDAT                                                   
144520        ,:TRET-DAUPPDAT                                                   
144530        ,:TRET-IDFTG                                                      
144550        ,:TRET-DAUPPDAT                                                   
144600     END-EXEC.                                                            
144700                                                                          
144800     MOVE 000100  TO GODK-SQLCODEKODER                                    
144900     MOVE SQLCODE TO SQLCODE-WS                                           
145000     PERFORM DB2-STATUS-KONTROLL                                          
145100     .                                                                    
145200     SKIP3                                                                
145300                                                                          
145400 DB2-CLOSE-TP8TRET-CRS  SECTION.                                          
145500     EXEC SQL CLOSE TP8TRET-CRS END-EXEC                                  
145600     .                                                                    
145700     EJECT                                                                
145800                                                                          
145900 DB2-DCL-OPN-TP8GRET-CRS  SECTION.                                        
146000     EXEC SQL                                                             
146100         DECLARE TP8GRET-CRS CURSOR FOR                                   
146200                                                                          
146300         SELECT  IDKUNDNR                                                 
146310                ,KDANMORS                                                 
146400                ,FLINVLDC                                                 
146500                ,SUARTBTO_MIN                                             
146600                ,SUARTBTO_LDC_MIN                                         
146700                ,IDPARTNR                                                 
146800                ,IDUSER                                                   
146910                ,DAUPPDAT                                                 
147000                                                                          
147100           FROM    TP8GRET                                                
147200           WHERE   IDDISTR  = :W-IDDISTR                                  
147300             AND   IDKUNDNR = :W-IDKUNDNR                                 
147400                                                                          
147500           ORDER BY KDANMORS                                              
147600     END-EXEC.                                                            
147700                                                                          
147800     EXEC SQL OPEN TP8GRET-CRS END-EXEC.                                  
147900                                                                          
148000     MOVE 000100  TO GODK-SQLCODEKODER                                    
148100     MOVE SQLCODE TO SQLCODE-WS                                           
148200     PERFORM DB2-STATUS-KONTROLL                                          
148300     .                                                                    
148400     SKIP3                                                                
148500                                                                          
148600 DB2-FETCH-TP8GRET-CRS  SECTION.                                          
148700     SKIP2                                                                
148800     EXEC SQL                                                             
148900       FETCH TP8GRET-CRS                                                  
149000       INTO                                                               
149100         :GRET-IDKUNDNR                                                   
149110        ,:GRET-KDANMORS                                                   
149200        ,:GRET-FLINVLDC                                                   
149300        ,:GRET-SUARTBTO-MIN                                               
149400        ,:GRET-SUARTBTO-LDC-MIN                                           
149500        ,:GRET-IDPARTNR                                                   
149600        ,:GRET-IDUSER                                                     
149620        ,:GRET-DAUPPDAT                                                   
149700     END-EXEC.                                                            
149800                                                                          
149900     MOVE 000100  TO GODK-SQLCODEKODER                                    
150000     MOVE SQLCODE TO SQLCODE-WS                                           
150100     PERFORM DB2-STATUS-KONTROLL                                          
150200     .                                                                    
150300     SKIP3                                                                
150400                                                                          
150500 DB2-CLOSE-TP8GRET-CRS  SECTION.                                          
150600     EXEC SQL CLOSE TP8GRET-CRS END-EXEC                                  
150700     .                                                                    
150800     EJECT                                                                
150900                                                                          
151000 DB2-DELETE-TP8TRET-TAB  SECTION.                                         
151100     EXEC SQL                                                             
151200         DELETE FROM TP8TRET                                              
151300         WHERE   IDDISTR  = :W-IDDISTR                                    
151400           AND   IDKUNDNR = :W-IDKUNDNR                                   
151500           AND   KDANMORS = :MID-KDANMORS-UPD                             
151600     END-EXEC                                                             
151700                                                                          
151800     MOVE 000     TO GODK-SQLCODEKODER                                    
151900     MOVE SQLCODE TO SQLCODE-WS                                           
152000     PERFORM DB2-STATUS-KONTROLL                                          
152100     .                                                                    
152200     EJECT                                                                
152300                                                                          
152400 DB2-SELECT-TP8TRET-KOD  SECTION.                                         
152500     EXEC SQL                                                             
152600         SELECT  IDDISTR                                                  
152610                ,IDKUNDNR                                                 
152620                ,KDANMORS                                                 
152630                ,PRARTNTO                                                 
152640                ,PRARTNTO_LDC                                             
152650                ,FLINVFEE                                                 
152660                ,FLINVLDC                                                 
152670                ,IDPARTNR                                                 
152680                ,IDUSER                                                   
152690                ,DAREGDAT                                                 
152691                ,DAUPPDAT                                                 
152692                ,IDFTG                                                    
153300       INTO                                                               
153400                :TRET-IDDISTR                                             
153500               ,:TRET-IDKUNDNR                                            
153600               ,:TRET-KDANMORS                                            
153700               ,:TRET-PRARTNTO                                            
153800               ,:TRET-PRARTNTO-LDC                                        
153900               ,:TRET-FLINVFEE                                            
154000               ,:TRET-FLINVLDC                                            
154010               ,:TRET-IDPARTNR                                            
154020               ,:TRET-IDUSER                                              
154030               ,:TRET-DAREGDAT                                            
154040               ,:TRET-DAUPPDAT                                            
154050               ,:TRET-IDFTG                                               
154060                                                                          
154200       FROM      TP8TRET                                                  
154300                                                                          
154400       WHERE     IDDISTR  = :W-IDDISTR                                    
154500       AND       IDKUNDNR = :W-IDKUNDNR                                   
154600       AND       KDANMORS = :MID-KDANMORS-UPD                             
154700     END-EXEC                                                             
154800                                                                          
154900     MOVE 000100305  TO GODK-SQLCODEKODER                                 
155000     MOVE SQLCODE    TO SQLCODE-WS                                        
155100     PERFORM DB2-STATUS-KONTROLL                                          
155200     .                                                                    
155300     EJECT                                                                
155400                                                                          
155500 DB2-UPDATE-TP8TRET-TAB  SECTION.                                         
155600     EXEC SQL                                                             
155700         UPDATE TP8TRET                                                   
155800         SET IDKUNDNR = :TRET-IDKUNDNR                                    
155810            ,FLINVFEE = :TRET-FLINVFEE                                    
155900            ,PRARTNTO = :TRET-PRARTNTO                                    
156000            ,FLINVLDC = :TRET-FLINVLDC                                    
156100            ,PRARTNTO_LDC = :TRET-PRARTNTO-LDC                            
156200            ,IDUSER   = :TRET-IDUSER                                      
156300            ,DAUPPDAT = :TRET-DAUPPDAT                                    
156400                                                                          
156500         WHERE                                                            
156600               IDDISTR  = :W-IDDISTR                                      
156700           AND IDKUNDNR = :W-IDKUNDNR                                     
156800           AND KDANMORS = :MID-KDANMORS-UPD                               
156900     END-EXEC                                                             
157000                                                                          
157100     MOVE 000    TO GODK-SQLCODEKODER                                     
157200     MOVE SQLCODE TO SQLCODE-WS                                           
157300     PERFORM DB2-STATUS-KONTROLL                                          
157400     .                                                                    
157500     EJECT                                                                
157600                                                                          
157710 DB2-INSERT-TP8TRET-TAB  SECTION.                                         
157800     SKIP2                                                                
157900     EXEC SQL                                                             
158000         INSERT INTO TP8TRET                                              
158100           (                                                              
158200            IDDISTR                                                       
158300           ,IDKUNDNR                                                      
158400           ,KDANMORS                                                      
158500           ,PRARTNTO                                                      
158600           ,PRARTNTO_LDC                                                  
158700           ,FLINVFEE                                                      
158800           ,FLINVLDC                                                      
158900           ,IDPARTNR                                                      
159000           ,IDUSER                                                        
159100           ,DAREGDAT                                                      
159110           ,DAUPPDAT                                                      
159120           ,IDFTG                                                         
159200           )                                                              
159300         VALUES                                                           
159400           (                                                              
159500            :TRET-IDDISTR                                                 
159600           ,:TRET-IDKUNDNR                                                
159700           ,:TRET-KDANMORS                                                
159800           ,:TRET-PRARTNTO                                                
159900           ,:TRET-PRARTNTO-LDC                                            
160000           ,:TRET-FLINVFEE                                                
160100           ,:TRET-FLINVLDC                                                
160200           ,:TRET-IDPARTNR                                                
160300           ,:TRET-IDUSER                                                  
160400           ,:TRET-DAREGDAT                                                
160410           ,:TRET-DAUPPDAT                                                
160420           ,:TRET-IDFTG                                                   
160500           )                                                              
160600     END-EXEC.                                                            
160700                                                                          
160800     MOVE 000803  TO GODK-SQLCODEKODER                                    
160900     MOVE SQLCODE TO SQLCODE-WS                                           
161000     PERFORM DB2-STATUS-KONTROLL                                          
161100     .                                                                    
161200     EJECT                                                                
161300                                                                          
161400 DB2-DELETE-TP8GRET-TAB  SECTION.                                         
161500     EXEC SQL                                                             
161600         DELETE FROM TP8GRET                                              
161700         WHERE   IDDISTR  = :W-IDDISTR                                    
161800           AND   IDKUNDNR = :W-IDKUNDNR                                   
161900           AND   KDANMORS = :MID-KDANMORS-GRET                            
162000     END-EXEC                                                             
162100                                                                          
162200     MOVE 000     TO GODK-SQLCODEKODER                                    
162300     MOVE SQLCODE TO SQLCODE-WS                                           
162400     PERFORM DB2-STATUS-KONTROLL                                          
162500     .                                                                    
162600     EJECT                                                                
162700                                                                          
162800 DB2-SELECT-TP8GRET-KOD  SECTION.                                         
162900     EXEC SQL                                                             
163610         SELECT  IDDISTR                                                  
163620                ,IDKUNDNR                                                 
163630                ,KDANMORS                                                 
163640                ,FLINVLDC                                                 
163650                ,SUARTBTO_MIN                                             
163660                ,SUARTBTO_LDC_MIN                                         
163700       INTO                                                               
163800                :GRET-IDDISTR                                             
163900               ,:GRET-IDKUNDNR                                            
164000               ,:GRET-KDANMORS                                            
164100               ,:GRET-FLINVLDC                                            
164200               ,:GRET-SUARTBTO-MIN                                        
164300               ,:GRET-SUARTBTO-LDC-MIN                                    
164500                                                                          
164600       FROM      TP8GRET                                                  
164700                                                                          
164800       WHERE     IDDISTR  = :W-IDDISTR                                    
164900       AND       IDKUNDNR = :W-IDKUNDNR                                   
165000       AND       KDANMORS = :MID-KDANMORS-GRET                            
165100     END-EXEC                                                             
165200                                                                          
165300     MOVE 000100305  TO GODK-SQLCODEKODER                                 
165400     MOVE SQLCODE    TO SQLCODE-WS                                        
165500     PERFORM DB2-STATUS-KONTROLL                                          
165600     .                                                                    
165700     EJECT                                                                
165800                                                                          
165900 DB2-UPDATE-TP8GRET-TAB  SECTION.                                         
166000     EXEC SQL                                                             
166100         UPDATE TP8GRET                                                   
166200         SET IDKUNDNR     = :GRET-IDKUNDNR                                
166210            ,SUARTBTO_MIN = :GRET-SUARTBTO-MIN                            
166300            ,FLINVLDC     = :GRET-FLINVLDC                                
166400            ,SUARTBTO_LDC_MIN = :GRET-SUARTBTO-LDC-MIN                    
166500            ,IDUSER   = :GRET-IDUSER                                      
166600            ,DAUPPDAT = :GRET-DAUPPDAT                                    
166700                                                                          
166800         WHERE                                                            
166900               IDDISTR  = :W-IDDISTR                                      
167000           AND IDKUNDNR = :W-IDKUNDNR                                     
167100           AND KDANMORS = :MID-KDANMORS-GRET                              
167200     END-EXEC                                                             
167300                                                                          
167400     MOVE 000    TO GODK-SQLCODEKODER                                     
167500     MOVE SQLCODE TO SQLCODE-WS                                           
167600     PERFORM DB2-STATUS-KONTROLL                                          
167700     .                                                                    
167800     EJECT                                                                
167900                                                                          
168000 DB2-INSERT-TP8GRET-TAB  SECTION.                                         
168100     SKIP2                                                                
168200     EXEC SQL                                                             
168300         INSERT INTO TP8GRET                                              
168400           (                                                              
168500            IDDISTR                                                       
168600           ,IDKUNDNR                                                      
168700           ,KDANMORS                                                      
168800           ,FLINVLDC                                                      
168900           ,SUARTBTO_MIN                                                  
169000           ,SUARTBTO_LDC_MIN                                              
169100           ,IDPARTNR                                                      
169200           ,IDUSER                                                        
169300           ,DAREGDAT                                                      
169310           ,DAUPPDAT                                                      
169320           ,IDFTG                                                         
169400           )                                                              
169500         VALUES                                                           
169600           (                                                              
169700            :GRET-IDDISTR                                                 
169800           ,:GRET-IDKUNDNR                                                
169900           ,:GRET-KDANMORS                                                
170000           ,:GRET-FLINVLDC                                                
170100           ,:GRET-SUARTBTO-MIN                                            
170200           ,:GRET-SUARTBTO-LDC-MIN                                        
170300           ,:GRET-IDPARTNR                                                
170400           ,:GRET-IDUSER                                                  
170500           ,:GRET-DAREGDAT                                                
170510           ,:GRET-DAUPPDAT                                                
170520           ,:GRET-IDFTG                                                   
170600           )                                                              
170700     END-EXEC.                                                            
170800                                                                          
170900     MOVE 000803  TO GODK-SQLCODEKODER                                    
171000     MOVE SQLCODE TO SQLCODE-WS                                           
171100     PERFORM DB2-STATUS-KONTROLL                                          
171200     .                                                                    
171300     EJECT                                                                
171400                                                                          
171410 DB2-DECLARE-OPEN-TP8TRET-CSR SECTION.                                    
171420     SKIP2                                                                
171430     EXEC SQL DECLARE TP8TRET-UPD-CRS CURSOR FOR                          
171440         SELECT  IDDISTR                                                  
171460                ,IDKUNDNR                                                 
171470                ,KDANMORS                                                 
171480                ,PRARTNTO                                                 
171490                ,PRARTNTO_LDC                                             
171491                ,FLINVFEE                                                 
171492                ,FLINVLDC                                                 
171493                ,IDPARTNR                                                 
171494                ,IDUSER                                                   
171495                ,DAREGDAT                                                 
171496                ,DAUPPDAT                                                 
171497                ,IDFTG                                                    
171498          FROM TP8TRET                                                    
171499         WHERE  IDDISTR  = :W-IDDISTR                                     
171500           AND KDANMORS = :MID-KDANMORS-UPD                               
171510                                                                          
171515     FOR UPDATE OF  IDKUNDNR                                              
171516                   ,FLINVFEE                                              
171517                   ,PRARTNTO                                              
171518                   ,FLINVLDC                                              
171519                   ,PRARTNTO_LDC                                          
171520                   ,IDUSER                                                
171521                   ,DAUPPDAT                                              
171522                                                                          
171523     END-EXEC                                                             
171524                                                                          
171525     EXEC SQL OPEN TP8TRET-UPD-CRS                                        
171526     END-EXEC                                                             
171527                                                                          
171528                                                                          
171529     MOVE 000100305      TO GODK-SQLCODEKODER                             
171530     MOVE SQLCODE TO SQLCODE-WS                                           
171531     PERFORM DB2-STATUS-KONTROLL                                          
171532     .                                                                    
171533     EJECT                                                                
171534                                                                          
171535 DB2-FETCH-TP8TRET-CSR SECTION.                                           
171536                                                                          
171537     EXEC SQL FETCH TP8TRET-UPD-CRS INTO                                  
171538          :TRET-IDDISTR                                                   
171539         ,:TRET-IDKUNDNR                                                  
171540         ,:TRET-KDANMORS                                                  
171541         ,:TRET-PRARTNTO                                                  
171542         ,:TRET-PRARTNTO-LDC                                              
171543         ,:TRET-FLINVFEE                                                  
171544         ,:TRET-FLINVLDC                                                  
171545         ,:TRET-IDPARTNR                                                  
171546         ,:TRET-IDUSER                                                    
171547         ,:TRET-DAREGDAT                                                  
171548         ,:TRET-DAUPPDAT                                                  
171549         ,:TRET-IDFTG                                                     
171550     END-EXEC                                                             
171551     MOVE 000100305      TO GODK-SQLCODEKODER                             
171552     MOVE SQLCODE        TO SQLCODE-WS                                    
171553     PERFORM DB2-STATUS-KONTROLL                                          
171554     .                                                                    
171555     EJECT                                                                
171556                                                                          
171557 DB2-UPDATE-TP8TRET-CSR SECTION.                                          
171558     EXEC SQL                                                             
171559         UPDATE TP8TRET                                                   
171560         SET  IDKUNDNR = :TRET-IDKUNDNR                                   
171561             ,FLINVFEE = :TRET-FLINVFEE                                   
171562             ,PRARTNTO = :TRET-PRARTNTO                                   
171563             ,FLINVLDC = :TRET-FLINVLDC                                   
171564             ,PRARTNTO_LDC = :TRET-PRARTNTO-LDC                           
171565             ,IDUSER   = :TRET-IDUSER                                     
171566             ,DAUPPDAT = :TRET-DAUPPDAT                                   
171567         WHERE CURRENT OF TP8TRET-UPD-CRS                                 
171568     END-EXEC                                                             
171569                                                                          
171570     MOVE 000     TO GODK-SQLCODEKODER                                    
171571     MOVE SQLCODE TO SQLCODE-WS                                           
171572     PERFORM DB2-STATUS-KONTROLL                                          
171573     .                                                                    
171574     EJECT                                                                
171575 DB2-CLOSE-TP8TRET-CSR SECTION.                                           
171576     EXEC SQL CLOSE TP8TRET-UPD-CRS                                       
171577     END-EXEC                                                             
171578     .                                                                    
171579     EJECT                                                                
171580 DB2-DECLARE-OPEN-TP8GRET-CSR SECTION.                                    
171581     SKIP2                                                                
171582     EXEC SQL DECLARE TP8GRET-UPD-CRS CURSOR FOR                          
171583         SELECT  IDDISTR                                                  
171584                ,IDKUNDNR                                                 
171585                ,KDANMORS                                                 
171586                ,FLINVLDC                                                 
171587                ,SUARTBTO_MIN                                             
171588                ,SUARTBTO_LDC_MIN                                         
171589                ,IDPARTNR                                                 
171590                ,IDUSER                                                   
171591                ,DAREGDAT                                                 
171592                ,DAUPPDAT                                                 
171593                ,IDFTG                                                    
171594          FROM TP8GRET                                                    
171595         WHERE  IDDISTR  = :W-IDDISTR                                     
171596           AND KDANMORS = :MID-KDANMORS-GRET                              
171597                                                                          
171598     FOR UPDATE OF  IDKUNDNR                                              
171599                   ,SUARTBTO_MIN                                          
171600                   ,FLINVLDC                                              
171601                   ,SUARTBTO_LDC_MIN                                      
171602                   ,IDUSER                                                
171603                   ,DAUPPDAT                                              
171604                                                                          
171605     END-EXEC                                                             
171606                                                                          
171607     EXEC SQL OPEN TP8GRET-UPD-CRS                                        
171608     END-EXEC                                                             
171609                                                                          
171610                                                                          
171611     MOVE 000100305      TO GODK-SQLCODEKODER                             
171612     MOVE SQLCODE TO SQLCODE-WS                                           
171613     PERFORM DB2-STATUS-KONTROLL                                          
171614     .                                                                    
171615     EJECT                                                                
171616                                                                          
171617 DB2-FETCH-TP8GRET-CSR SECTION.                                           
171618                                                                          
171619     EXEC SQL FETCH TP8GRET-UPD-CRS INTO                                  
171620          :GRET-IDDISTR                                                   
171621         ,:GRET-IDKUNDNR                                                  
171622         ,:GRET-KDANMORS                                                  
171623         ,:GRET-FLINVLDC                                                  
171624         ,:GRET-SUARTBTO-MIN                                              
171625         ,:GRET-SUARTBTO-LDC-MIN                                          
171626         ,:GRET-IDPARTNR                                                  
171627         ,:GRET-IDUSER                                                    
171628         ,:GRET-DAREGDAT                                                  
171629         ,:GRET-DAUPPDAT                                                  
171630         ,:GRET-IDFTG                                                     
171631     END-EXEC                                                             
171632     MOVE 000100305      TO GODK-SQLCODEKODER                             
171633     MOVE SQLCODE        TO SQLCODE-WS                                    
171634     PERFORM DB2-STATUS-KONTROLL                                          
171635     .                                                                    
171636     EJECT                                                                
171637                                                                          
171638 DB2-UPDATE-TP8GRET-CSR SECTION.                                          
171639     EXEC SQL                                                             
171640         UPDATE TP8GRET                                                   
171641         SET  IDKUNDNR     = :GRET-IDKUNDNR                               
171642             ,SUARTBTO_MIN = :GRET-SUARTBTO-MIN                           
171643             ,FLINVLDC     = :GRET-FLINVLDC                               
171644             ,SUARTBTO_LDC_MIN = :GRET-SUARTBTO-LDC-MIN                   
171645             ,IDUSER   = :GRET-IDUSER                                     
171646             ,DAUPPDAT = :GRET-DAUPPDAT                                   
171647         WHERE CURRENT OF TP8GRET-UPD-CRS                                 
171648     END-EXEC                                                             
171649                                                                          
171650     MOVE 000     TO GODK-SQLCODEKODER                                    
171651     MOVE SQLCODE TO SQLCODE-WS                                           
171652     PERFORM DB2-STATUS-KONTROLL                                          
171653     .                                                                    
171654     EJECT                                                                
171655 DB2-CLOSE-TP8GRET-CSR SECTION.                                           
171656     EXEC SQL CLOSE TP8GRET-UPD-CRS                                       
171657     END-EXEC                                                             
171658     .                                                                    
171659     EJECT                                                                
171660 DB2-STATUS-KONTROLL  SECTION.                                            
171670     SET SQLCODE-IX TO 1                                                  
171700     SEARCH GODK-SQLCODE                                                  
171800       AT END                                                             
171900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
172000          DELIMITED BY SIZE INTO FELTEXT                                  
172100          CALL ABEND USING RKOD-ABEND-DB2                                 
172200       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
172300     END-SEARCH                                                           
172400     .                                                                    
