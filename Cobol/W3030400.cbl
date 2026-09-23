000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3030400.                                                
000400 AUTHOR.         STENHOLM                                                 
000500 DATE-WRITTEN.   OKT 1995.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION.                                                            
001000*        FRÅGEPROGRAM FÖR ARTIKELSTATISTIKEN.                             
001100*        TRE NYCKLAR FÖREKOMMER:ARTIKELNR, KONCERN OCH DISTRIKT           
001200*            -ENDAST ARTIKELNR ÄR GIVET:                                  
001300*                          SÖKNING SKER I TABELL FSG2 WORLD-VIDE          
001400*            -ARTIKELNR OCH DISTRIKT ÄR GIVNA:                            
001500*                          SÖKNING SKER I TABELL FSG4 DISTRIKT            
001600*            -ARTIKELNR OCH MARKNADSBOLAG/PRISOMRÅDE ÄR GIVNA:            
001700*                          SÖKNING SKER I TABELL FSG5 MB                  
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W3T304                                              
002100*        MID:         W3I30401                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W3O30401                                            
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W3030400'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  YA                          PIC X       VALUE 'Y'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003800                                                                          
003900 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
004000 77  IDDISTR-WS                  PIC X(4)    VALUE SPACE.                 
004100 77  IDPROMR-WS                  PIC X(3)    VALUE SPACE.                 
004200 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
004300 77  WS-IDDC-NUM                 PIC 9(2)    VALUE ZERO.                  
004500 77  SPAR-KDARTRAB               PIC 9(2)    VALUE ZERO.                  
004600 77  WS-RETAILPRIS               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004700 77  WS-RETAILPRIS-DO            PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004800 77  WS-RETAILPRIS-MO            PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004900 77  WS-RETAILPRIS-NOR-DO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005000 77  WS-RETAILPRIS-NOR-MO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005100 77  RAB-INDX                    PIC 9(2)    VALUE ZERO.                  
005110 77  W-WDB2-KDKUNDKAT            PIC 9(2)    VALUE ZERO.                  
005200                                                                          
005500 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
005600 77  WS-KDVALISO-MC              PIC X(3)   VALUE 'SEK'.                  
005800                                                                          
006300 77  WS-FLMRKVAL                 PIC X      VALUE SPACE.                  
006400                                                                          
006500 01  FELTEXT.                                                             
006600     03  FILLER                  PIC X(80)   VALUE SPACE.                 
006700     EJECT                                                                
007100                                                                          
007200 01  SW-AVT                      PIC X(1)    VALUE SPACE.                 
007300     88 AVT-FOUND                            VALUE 'J'.                   
007400     88 AVT-NOT-FOUND                        VALUE 'N'.                   
007500                                                                          
007600 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
007700                                                                          
007800 01  WS-DATUM                    PIC X(7)    VALUE SPACE.                 
007900 01  FILLER REDEFINES WS-DATUM.                                           
008000     03 WS-FILLER                PIC X(1).                                
008100     03 WS-DATUM6                PIC X(6).                                
008200                                                                          
008400 01  WS-IDPROMR-RENO             PIC X(3)    VALUE SPACE.                 
008500 01  WS-IDMARKBO-RENO            PIC X(1)    VALUE SPACE.                 
008600 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
008700 01  FILLER REDEFINES WS-IDPROMR.                                         
008800     03  WS-MARKBOLAG            PIC X(1).                                
008900     03  WS-IDPROMRN             PIC X(2).                                
009000                                                                          
009100 01  W-ARBETSFALT.                                                        
009200     03  W-RETOTBV               PIC S9(9)V9 VALUE +0   COMP-3.           
009300     03  W-REFSG                 PIC S9(9)V9 VALUE +0   COMP-3.           
009400     03  W-RELEVANT              PIC S9(9)V9 VALUE +0   COMP-3.           
009500                                                                          
009600     03  W-RED-TIONDEL-ASTERISK.                                          
009700         05  W-TIONDEL           PIC -(4).                                
009800         05  FILLER              PIC X   VALUE '*'.                       
009900     EJECT                                                                
010000                                                                          
010100 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
010200*01  FILLER    -COPY WWBYT02  -RED  TEST-IDARTNR.                         
010300     EJECT                                                                
010400*01  -COPY WWBYT16  .                                                     
010500*                                                                         
010600     EJECT                                                                
010700*01  -COPY WWDIST79 .                                                     
010710*                                                                         
010720     EJECT                                                                
010730*01  -COPY WWDCLAND .                                                     
010800 01  FILLER                      PIC X(16)   VALUE 'DL1-NYCKLAR'.         
010900 01    NYCKLAR-TILL-DLI-DB2.                                              
011000   03    W-WDGX3137-KEY.                                                  
011100     05    W-IDHTYP              PIC X(4)    VALUE '3137'.                
011200     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
011300                                                                          
011400   03    W-IDPROMR-X.                                                     
011500     05    W-IDPROMR             PIC X(3)    VALUE SPACE.                 
011600                                                                          
011700   03    W-IDSKYLT-X.                                                     
011800     05    W-IDSKYLT             PIC X(3)    VALUE 'S  '.                 
011900                                                                          
012000   03    W-IDARTNR-X.                                                     
012100     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
012200                                                                          
012300   03    W-IDDC-X.                                                        
012400     05    W-IDDC                PIC X(2)    VALUE SPACE.                 
012500                                                                          
012600   03    W-IDARTNR-BYT-X.                                                 
012700     05    W-IDARTNR-BYT         PIC S9(9)   VALUE ZERO  COMP-3.          
012800***** NYCKLAR TILL WDC2 *******************************                   
012900                                                                          
013000                                                                          
013100     03  W-WDC211KY-MIN-X.                                                
013200         05  W-IDARTNR-211-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
013300         05  W-DASTADAT-211-MIN  PIC 9(8)    VALUE ZERO.                  
013400                                                                          
013500     03  W-WDC211KY-MAX-X.                                                
013600         05  W-IDARTNR-211-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
013700         05  W-DASTADAT-211-MAX  PIC 9(8)    VALUE ZERO.                  
013800                                                                          
013900     03  W-WDC212KY-MIN-X.                                                
014000         05  W-KDARTKAM-212-MIN  PIC 9(5)    VALUE ZERO.                  
014100         05  W-DASTADAT-212-MIN  PIC 9(8)    VALUE ZERO.                  
014200                                                                          
014300     03  W-WDC212KY-MAX-X.                                                
014400         05  W-KDARTKAM-212-MAX  PIC 9(5)    VALUE ZERO.                  
014500         05  W-DASTADAT-212-MAX  PIC 9(8)    VALUE ZERO.                  
014600                                                                          
014700     03  W-DASTADAT-X.                                                    
014800         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
014900                                                                          
015000******************************************************************        
015100                                                                          
015200******* NYCKLAR TILL WDC1   *******************                           
015300                                                                          
015400     03  W-WDC101KY-X.                                                    
015500         05  W-IDARTNR-111       PIC S9(9)   VALUE ZERO COMP-3.           
015600         05  W-IDMARKBO-111      PIC X       VALUE SPACE.                 
015700***************************                                               
015800*** NYCKLAR TILL KUNDREG             ***********                          
015900     03    FILLER                  PIC X(16)   VALUE 'KUNDREG'.           
016000                                                                          
016100     03  W-IDGMT-MIN-X.                                                   
016200         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
016300         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
016400                                                                          
016500     03  W-IDGMT-MAX-X.                                                   
016600         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
016700         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE 9999999                
016800                                                        COMP-3.           
016900                                                                          
017000     03  W-IDDISTR-X.                                                     
017100         05  W-IDDISTR           PIC S9(5)       COMP-3.                  
017200                                                                          
017300*   NYCKLAR TILL BETALARREGISTRET    ***********                          
017400     03  W-WDB101KY-X.                                                    
017500         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
017600         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
019500                                                                          
019600*** SUBPROGRAM OCH PARAMETERAREOR                                         
019700                                                                          
019800 01  GENERELLA-SUBPOROGRAM.                                               
019900     03  WMEDKONV               PIC X(8) VALUE 'WMEDKONV'.                
020000     03  W005INIT               PIC X(8) VALUE 'W005INIT'.                
020100     03  CBLTDLI                PIC X(8) VALUE 'CBLTDLI '.                
020200     03  FELLOG                 PIC X(8) VALUE 'FELLOG  '.                
020300                                                                          
020400*                **** PARAMETRAR TILL WMEDKONV                            
020500*01      -COPY WMEDAREA                                                   
020600                                                                          
020700 01  MESSAGE-CODES.                                                       
020800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020900     03  ERR-ART-MISSING         PIC X(3)    VALUE '017'.                 
021000     03  PRICE-MISSING           PIC X(3)    VALUE '171'.                 
021100     EJECT                                                                
021200*                **** PARAMETRAR TILL W005INIT                            
021300*                                                                         
021400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
021500     SKIP3                                                                
021600*01      -COPY WMSGINIT                                                   
021700                                                                          
021800 01  GEMENSAMMA-SUBPROGRAM.                                               
021900     03  W335COST                PIC X(8)    VALUE 'W335COST'.            
022100                                                                          
022200*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
022300 01 FILLER                       PIC X(8)    VALUE 'W335COST'.            
022400*   -COPY W335COST                                                        
022500     EJECT                                                                
022900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023000******************************************************************        
023100*                                                                         
023200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
023300*                                                                         
023400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
023500     SKIP3                                                                
023600*01    MID -COPY W3I30401.                                                
023700     EJECT                                                                
023800*01    -COPY WMSGAREA                                                     
023900     EJECT                                                                
024000*  03    MOD -COPY W3O30401  -RED MSG-AREA.                               
024100     EJECT                                                                
024200*01    -COPY WMFSAREA                                                     
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)  VALUE 'FSG-AREA'.             
024500****    HÄR ANVÄNDS FSG2-COPYTEXTEN GENERELLT FÖR FSG2 OCH                
024600****    FSG4 I ANROP MOT DB2, DÅ DESSA FÖRUTOM NYCKLARNA ÄR LIKA.         
024700                                                                          
024800*01  FILLER -COPY FSG2 -PRE FSG-                                          
024900     EJECT                                                                
025000 01  FILLER                      PIC X(16)  VALUE 'FSG2-AREA'.            
025100       EXEC SQL INCLUDE FSG2     END-EXEC.                                
025200                                                                          
025300       EXEC SQL INCLUDE FSG4     END-EXEC.                                
025400                                                                          
025500       EXEC SQL INCLUDE FSG5     END-EXEC.                                
025600                                                                          
025700 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
025800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025900                                                                          
026000 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
026100 01  DB2-WS.                                                              
026200     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
026300         88  CURSOR-OK                      VALUE 000.                    
026400         88  RADER-FINNS                    VALUE 000.                    
026500         88  RADER-SAKNAS                   VALUE 100.                    
026600         88  904-KOD                        VALUE 904.                    
026700     03  GODK-SQLCODEKODER.                                               
026800         05  GODK-SQLCODE OCCURS 4                                        
026900             INDEXED BY SQLCODE-IX PIC 9(3).                              
027000     EJECT                                                                
027100                                                                          
027200**** IMS AREOR *******                                                    
027300 01    IMS-WS.                                                            
027400   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
027500     SKIP3                                                                
027600*                        **** STATUS-KOD FRÅN IMS                         
027700   03    STATUS-WS               PIC XX.                                  
027800     88    SEGMENT-FINNS                     VALUE '  '.                  
027900     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
028000     88    SEGMENT-SLUT                      VALUE 'GB'.                  
028100     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
028200     SKIP3                                                                
028300   03    GODK-STATUSKODER.                                                
028400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
028500     SKIP3                                                                
028600 01    SSA1                      PIC X(64).                               
028700 01    SSA2                      PIC X(64).                               
028800     EJECT                                                                
028900*                            IMS FUNKTIONSKODER                           
029000*01    -COPY W0003                                                        
029100     EJECT                                                                
029200******************************************************************        
029300*                                                                         
029400*        ARBETS-AREA  TILL DB2                                            
029500*                                                                         
029600*                            DLI INPUT-OUTPUT AREA                        
029700 01    DLI-IO-AREA.                                                       
029800   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
029900                                                                          
030000                                                                          
030100*  03    WDK601 -COPY WDK601 -PRE WDK601- -RED IO-AREA.                   
030200                                                                          
030300*  03    WDK611 -COPY WDK611 -PRE WDK611- -RED IO-AREA.                   
030400                                                                          
030500*  03    WDC101 -COPY WDC101 -PRE WDC1-     -RED IO-AREA.                 
030600                                                                          
030700*  03    WDD311 -COPY WDD311 -PRE WDD3-     -RED IO-AREA.                 
030800                                                                          
030900*  03    WLXXCJ11 -COPY WDGX3138 -PRE WDGX- -RED IO-AREA.                 
031000     EJECT                                                                
031100                                                                          
031200 01  FILLER                  PIC X(16)   VALUE 'WDC201       '.           
031300 01  DLI-IO-AREA2.                                                        
031400     03  IO-AREA2                PIC X(850)  VALUE SPACE.                 
031500     03  WDC201 REDEFINES IO-AREA2.                                       
031600*        05  -COPY WDC201  -PRE WDC2-                                     
031700     SKIP3                                                                
031800     03  WDC211 REDEFINES IO-AREA2.                                       
031900*        05  -COPY WDC211  -PRE WDC2-                                     
032000     EJECT                                                                
032100     03  WDC212 REDEFINES IO-AREA2.                                       
032200*        05  -COPY WDC212  -PRE WDC2-                                     
032300     EJECT                                                                
032400     03  WDC213 REDEFINES IO-AREA2.                                       
032500*        05  -COPY WDC213  -PRE WDC2-                                     
032600     EJECT                                                                
032700**   KUNDREGISTER                                                         
032800 01  FILLER                  PIC X(16)   VALUE 'WDB201       '.           
032900 01  DLI-IO-WDB201.                                                       
033000*    03  WDB201    -COPY WDB201 -PRE WDB2-                                
033100     EJECT                                                                
033200**   BETALARREGISTER                                                      
033300 01  DLI-IO-WDB101.                                                       
033400*  03  -COPY WDB101                                                       
033500     EJECT                                                                
033600**   BYTSARTIKLAR                                                         
033700 01  DLI-IO-AREA4.                                                        
033800     03  WDK611   -COPY WDK611 -PRE OBJ-                                  
033900     EJECT                                                                
034700 01  FILLER                  PIC X(16)   VALUE 'WDK711       '.           
034800 01  DLI-IO-WDK711.                                                       
034900*    03 -COPY WDK711                                                      
035000 01  FILLER                  PIC X(16)   VALUE 'WDK723       '.           
035100 01  DLI-IO-WDK723.                                                       
035200*    03 -COPY WDK723                                                      
035300     EJECT                                                                
035400 LINKAGE SECTION.                                                         
035500*01    -COPY W0009     -PRE MSG-                                          
035600                                                                          
035700*01    -COPY W0008     -PRE USEA-                                         
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01    -COPY W0008     -PRE WDK6-                                         
036100     05  FILLER                  PIC X.                                   
036200                                                                          
036300*01    -COPY W0008     -PRE WDB2-                                         
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01    -COPY W0008     -PRE WDD3-                                         
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01    -COPY W0008     -PRE WDGX-                                         
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01    -COPY W0008     -PRE WDC1-                                         
037300     05  FILLER                  PIC X.                                   
037400                                                                          
037500*01    -COPY W0008     -PRE WDC2-                                         
037600     05  FILLER                  PIC X.                                   
037700                                                                          
037800*01    -COPY W0008     -PRE WDB1-                                         
037900     05  FILLER                  PIC X.                                   
038400*01    -COPY W0008     -PRE WDK7-                                         
038500     05  FILLER                  PIC X.                                   
038600                                                                          
038700 01  COST-WDK6-PCB               PIC X.                                   
038800 01  COST-WDK7-PCB               PIC X.                                   
038900 01  COST-WDF1-PCB               PIC X.                                   
039000 01  COST-9305-PCB               PIC X.                                   
039100 01  COST-WDK72-PCB              PIC X.                                   
039200 01  COST-WDB6-PCB               PIC X.                                   
039400     EJECT                                                                
039500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
039600                 WDK6-PCB WDB2-PCB WDD3-PCB WDGX-PCB                      
039700                 WDC1-PCB WDC2-PCB WDB1-PCB                               
039800                 WDK7-PCB                                                 
039900                 COST-WDK6-PCB                                            
040000                 COST-WDK7-PCB                                            
040100                 COST-WDF1-PCB                                            
040200                 COST-9305-PCB                                            
040300                 COST-WDK72-PCB COST-WDB6-PCB.                            
040400 MAIN SECTION.                                                            
040500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
040600                 WDK6-PCB WDB2-PCB WDD3-PCB WDGX-PCB                      
040700                 WDC1-PCB WDC2-PCB WDB1-PCB                               
040800                 WDK7-PCB                                                 
040900                 COST-WDK6-PCB                                            
041000                 COST-WDK7-PCB                                            
041100                 COST-WDF1-PCB                                            
041200                 COST-9305-PCB                                            
041300                 COST-WDK72-PCB COST-WDB6-PCB.                            
041400                                                                          
041500     PERFORM IMS-GET-MSG                                                  
041600     IF SEGMENT-FINNS                                                     
041700         PERFORM A-INIT-SPARA-INPUT                                       
041800         PERFORM B-KOLLA-NYCKLAR                                          
041900         IF IDARTNR-WS NOT NUMERIC                                        
042000             MOVE '001'           TO MED-IDMFSFEL                         
042100             CALL WMEDKONV USING MED-WMEDAREA                             
042200             MOVE MED-MFSFEL      TO MOD-TEMFSFEL                         
042300         ELSE                                                             
042400             PERFORM F-CHECK-LOC-SOURCED                                  
042500             PERFORM E-PRISINFORMATION                                    
042600             PERFORM D-LAS-LAGG-UT-DLI-ARTINFO                            
042700             IF W-IDDISTR > ZERO                                          
042800                PERFORM DB2-SELECT-FSG4-TAB                               
042900             ELSE                                                         
043000               IF W-IDPROMR NOT = SPACE                                   
043100                  PERFORM DB2-SELECT-FSG5-TAB                             
043200               ELSE                                                       
043300                  PERFORM  DB2-SELECT-FSG2-TAB                            
043400               END-IF                                                     
043500             END-IF                                                       
043600             IF RADER-FINNS                                               
043700                 PERFORM C-LAGG-UT-DB2-ARTINFO                            
043800             ELSE                                                         
043900                 MOVE '005'           TO MED-IDMFSFEL                     
044000                 CALL WMEDKONV USING MED-WMEDAREA                         
044100                 MOVE MED-MFSFEL      TO MOD-TEMFSFEL                     
044200             END-IF                                                       
044300             IF W-IDDISTR > ZERO                                          
044400               MOVE W-IDDISTR TO DIST79-IDDISTR                           
044500               IF DIST79-DEALER-PRICE                                     
044600                 MOVE 'DNI USE 3305 FOR NET PRICE'                        
044610                           TO MOD-TEMFSFEL                                
044700                 MOVE ZERO TO MOD-DO-RAB-PRIS                             
044800                 MOVE ZERO TO MOD-MO-RAB-PRIS                             
044900               END-IF                                                     
045000             END-IF                                                       
045100         END-IF                                                           
045200         COMPUTE MSG-KVLL = LENGTH OF MOD-W3O30401 + 4                    
045300         PERFORM IMS-INSERT-MSG                                           
045400     END-IF                                                               
045500     MOVE ZERO TO RETURN-CODE                                             
045600     GOBACK                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 A-INIT-SPARA-INPUT SECTION.                                              
046000                                                                          
046100     IF MSG-DUBBLA-TRANSKODER                                             
046200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I30401               
046300         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
046400         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
046500         MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                
046600     ELSE                                                                 
046700         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I30401               
046800         MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                
046900         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
047000         MOVE ' '                           TO MFS-KDTRTYP                
047100     END-IF                                                               
047200                                                                          
047300     MOVE 'SEK'             TO MOD-KDVALISO                               
047600     .                                                                    
047700     EJECT                                                                
047800                                                                          
047900 B-KOLLA-NYCKLAR SECTION.                                                 
048000                                                                          
048100     MOVE  ALL '+'          TO MSGI-WMSGINIT                              
048200     MOVE '001'             TO MSGI-KDCALL                                
048300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
048400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
048500     MOVE '3304'            TO MSGI-IDTRANS                               
048600                                                                          
048700     IF MFS-IDTRANS = '3304'                                              
048800       IF MID-IDARTNR-IN = ALL '+' AND                                    
048900          MID-IDPROMR-IN = ALL '+' AND                                    
049000          MID-IDDISTR-IN = ALL '+' AND                                    
049100          MID-FLMRKVAL-IN = ALL '+'                                       
049110          IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                 
049200            MOVE MSGI-IDARTNR      TO MID-IDARTNR-IN                      
049201          ELSE                                                            
049202            MOVE '001'           TO MED-IDMFSFEL                          
049203            CALL WMEDKONV USING MED-WMEDAREA                              
049204            MOVE MED-MFSFEL      TO MOD-TEMFSFEL                          
049210          END-IF                                                          
049300       END-IF                                                             
049400       INSPECT MID-IDARTNR-IN REPLACING ALL SPACE BY ZERO                 
049500       IF MID-IDARTNR-IN NUMERIC                                          
049600          MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                          
049700       END-IF                                                             
049800       IF MID-IDPROMR-IN = ALL '+'                                        
049900          IF MID-IDDISTR-IN = ALL '+'                                     
050000             CONTINUE                                                     
050100          ELSE                                                            
050200              MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                        
050300              MOVE SPACE           TO MSGI-IDPROMR                        
050400          END-IF                                                          
050500       ELSE                                                               
050600         MOVE MID-IDPROMR-IN     TO MSGI-IDPROMR                          
050700         MOVE SPACE              TO MSGI-IDDISTR                          
050800       END-IF                                                             
050900       IF MID-FLMRKVAL-IN = '+'                                           
051000         CONTINUE                                                         
051100       ELSE                                                               
051200         MOVE MID-FLMRKVAL-IN    TO MSGI-FLMRKVAL                         
051300       END-IF                                                             
051400     ELSE                                                                 
051500       INSPECT MID-IDARTNR-IN REPLACING ALL SPACE BY ZERO                 
051600       IF MID-IDARTNR-IN NUMERIC                                          
051700          MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                          
051800       ELSE                                                               
051900          MOVE ALL '+'           TO MSGI-IDARTNR                          
052000       END-IF                                                             
052100       MOVE SPACE                TO MSGI-IDDISTR                          
052200       MOVE SPACE                TO MSGI-IDPROMR                          
052300     END-IF                                                               
052400                                                                          
052500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
052510     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
052600       MOVE MSGI-IDARTNR         TO IDARTNR-WS                            
052700                                    W-IDARTNR-111                         
052800                                    W-IDARTNR-211-MIN                     
052900                                    W-IDARTNR-211-MAX                     
053000                                    TEST-IDARTNR                          
053100                                    BYT16-IDARTNR                         
053110     ELSE                                                                 
053120       MOVE '001'           TO MED-IDMFSFEL                               
053130       CALL WMEDKONV USING MED-WMEDAREA                                   
053140       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
053150     END-IF                                                               
053200     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
053300     MOVE MSGI-IDDISTR           TO IDDISTR-WS                            
053400     IF IDDISTR-WS NOT NUMERIC                                            
053500        MOVE SPACE               TO IDDISTR-WS                            
053600     END-IF                                                               
053700     INSPECT IDDISTR-WS REPLACING ALL SPACE BY ZERO                       
053800     MOVE MSGI-IDMARKBO          TO W-IDMARKBO-111                        
053900     MOVE MSGI-IDPROMR           TO W-IDPROMR                             
054000                                    IDPROMR-WS                            
054100     MOVE MSGI-IDDC              TO IDDC-WS                               
054200     MOVE MSGI-FLMRKVAL          TO WS-FLMRKVAL                           
054300                                                                          
054400     MOVE LOW-VALUE              TO MSG-AREA                              
054500     MOVE 'W3O304N1'             TO MFS-IDMOD                             
054600     MOVE '3304'                 TO MOD-IDTRANS                           
054700     MOVE IDARTNR-WS             TO MOD-IDARTNR-UT                        
054800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
054900     MOVE IDDISTR-WS             TO MOD-IDDISTR-UT                        
055000     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
055100     INSPECT IDDC-WS REPLACING LEADING SPACE BY  ZERO                     
055200     MOVE IDPROMR-WS             TO MOD-IDPROMR-UT                        
055300     MOVE WS-FLMRKVAL            TO MOD-FLMRKVAL-UT                       
055400                                                                          
055500     MOVE IDARTNR-WS             TO W-IDARTNR                             
055600     MOVE IDDISTR-WS             TO W-IDDISTR                             
055700                                    W-IDDISTR-MIN                         
055800                                    W-IDDISTR-MAX                         
055900                                                                          
056000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
056100         MOVE +1 TO INDX                                                  
056200         MOVE 'S  '              TO MED-IDSKYLT                           
056300     ELSE                                                                 
056400         MOVE +2 TO INDX                                                  
056500         MOVE 'GB '              TO MED-IDSKYLT                           
056600     END-IF                                                               
056700                                                                          
056800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
056900                             MOD-IDDISTR-IN                               
057000                             MOD-IDPROMR-IN                               
057100                             MOD-FLMRKVAL-IN                              
057200                             MOD-TEMFSFEL                                 
057300                             MOD-TEMFSINF                                 
057400     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
057500     MOVE DAGENS-DATUM TO W-DASTADAT-211-MAX                              
057600                          W-DASTADAT-212-MAX                              
057700                          W-DASTADAT                                      
057800                                                                          
058400                                                                          
058500     IF W-IDMARKBO-111 NOT = SPACE                                        
058600       MOVE W-IDMARKBO-111    TO WS-IDMARKBO                              
058700       MOVE WS-KDVALISO-MC    TO MOD-KDVALISO                             
058800     END-IF                                                               
058900     MOVE DAGENS-DATUM(5:2)          TO COST-TIMM                         
059100     .                                                                    
059200     EJECT                                                                
059300 C-LAGG-UT-DB2-ARTINFO SECTION.                                           
059400                                                                          
059500     PERFORM CA-NOLLA-MODFALT                                             
059600                                                                          
059700****** RAD AF5  **************************************                    
059800                                                                          
059900     IF FSG-SUARTFSG-PER NOT = 0                                          
060000         MOVE FSG-SUARTFSG-PER            TO MOD-SUARTFSG-PER             
060100     END-IF                                                               
060200     IF FSG-SUARTFSG-PER NOT = 0                                          
060300         COMPUTE W-RETOTBV ROUNDED =                                      
060400            100 * FSG-SUTOTBV-PER / FSG-SUARTFSG-PER                      
060500                                                                          
060600         IF W-RETOTBV > -99.9 AND < 99.9                                  
060700             MOVE W-RETOTBV               TO MOD-RETOTBV-PER              
060800         ELSE                                                             
060900             COMPUTE W-TIONDEL ROUNDED = W-RETOTBV / 10                   
061000             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-RETOTBV-PER              
061100             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
061200         END-IF                                                           
061300     END-IF                                                               
061400     MOVE FSG-SUTOTBV-PER             TO MOD-SUTOTBV-PER                  
061500     MOVE FSG-SULEVANT-PER            TO MOD-SULEVANT-PER                 
062400     EJECT                                                                
062500                                                                          
062600****** RAD AF4  ********************************                          
062700                                                                          
062800     MOVE FSG-SUARTFSG-AAR            TO MOD-SUARTFSG-AAR                 
062900     IF FSG-SUARTFSG-FAAR NOT = 0                                         
063000         COMPUTE W-REFSG ROUNDED =                                        
063100         100 * (FSG-SUARTFSG-AAR - FSG-SUARTFSG-FAAR)/                    
063200                            FSG-SUARTFSG-FAAR                             
063300                                                                          
063400         IF W-REFSG > -99.9 AND < 99.9                                    
063500             MOVE W-REFSG                 TO MOD-REFSG-AAR                
063600         ELSE                                                             
063700             COMPUTE W-TIONDEL ROUNDED = W-REFSG / 10                     
063800             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-REFSG-AAR                
063900             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
064000         END-IF                                                           
064100     END-IF                                                               
064200     IF FSG-SUARTFSG-AAR NOT = 0                                          
064300         COMPUTE W-RETOTBV ROUNDED =                                      
064400            100 * FSG-SUTOTBV-AAR / FSG-SUARTFSG-AAR                      
064500                                                                          
064600         IF W-RETOTBV > -99.9 AND < 99.9                                  
064700             MOVE W-RETOTBV               TO MOD-RETOTBV-AAR              
064800         ELSE                                                             
064900             COMPUTE W-TIONDEL ROUNDED = W-RETOTBV / 10                   
065000             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-RETOTBV-AAR              
065100             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
065200         END-IF                                                           
065300     END-IF                                                               
065400     MOVE FSG-SUTOTBV-AAR             TO MOD-SUTOTBV-AAR                  
066300     MOVE FSG-SULEVANT-AAR            TO MOD-SULEVANT-AAR                 
066400     IF FSG-SULEVANT-FAAR NOT = 0                                         
066500         COMPUTE W-RELEVANT ROUNDED =                                     
066600         100 * (FSG-SULEVANT-AAR - FSG-SULEVANT-FAAR)/                    
066700                            FSG-SULEVANT-FAAR                             
066800                                                                          
066900         IF W-RELEVANT > -99.9 AND < 99.9                                 
067000             MOVE W-RELEVANT              TO MOD-RELEVANT-AAR             
067100         ELSE                                                             
067200             COMPUTE W-TIONDEL ROUNDED = W-RELEVANT / 10                  
067300             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-RELEVANT-AAR             
067400             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
067500         END-IF                                                           
067600     END-IF                                                               
067700     EJECT                                                                
067800                                                                          
067900****** RAD AF3  *************************************                     
068000                                                                          
068100     MOVE FSG-SUARTFSG-FAAR           TO MOD-SUARTFSG-FAAR                
068200     IF FSG-SUARTFSG-FAAR NOT = 0                                         
068300         COMPUTE W-RETOTBV ROUNDED =                                      
068400            100 * FSG-SUTOTBV-FAAR / FSG-SUARTFSG-FAAR                    
068500                                                                          
068600         IF W-RETOTBV > -99.9 AND < 99.9                                  
068700             MOVE W-RETOTBV               TO MOD-RETOTBV-FAAR             
068800         ELSE                                                             
068900             COMPUTE W-TIONDEL ROUNDED = W-RETOTBV / 10                   
069000             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-RETOTBV-FAAR             
069100             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
069200         END-IF                                                           
069300     END-IF                                                               
069400     MOVE FSG-SUTOTBV-FAAR            TO MOD-SUTOTBV-FAAR                 
069500     MOVE FSG-SULEVANT-FAAR           TO MOD-SULEVANT-FAAR                
070400     EJECT                                                                
070500                                                                          
070600****** RAD AF2  **************************************                    
070700                                                                          
070800     MOVE FSG-SUARTFSG-RAAR        TO MOD-SUARTFSG-RAAR                   
070900     IF FSG-SUARTFSG-FRAAR NOT = 0                                        
071000         COMPUTE W-REFSG ROUNDED =                                        
071100         100 * (FSG-SUARTFSG-RAAR - FSG-SUARTFSG-FRAAR)/                  
071200                            FSG-SUARTFSG-FRAAR                            
071300                                                                          
071400         IF W-REFSG > -99.9 AND < 99.9                                    
071500             MOVE W-REFSG                 TO MOD-REFSG-RAAR               
071600         ELSE                                                             
071700             COMPUTE W-TIONDEL ROUNDED = W-REFSG / 10                     
071800             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-REFSG-RAAR               
071900             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
072000         END-IF                                                           
072100     END-IF                                                               
072200     IF FSG-SUARTFSG-RAAR NOT = 0                                         
072300         COMPUTE W-RETOTBV ROUNDED =                                      
072400         100 * FSG-SUTOTBV-RAAR / FSG-SUARTFSG-RAAR                       
072500                                                                          
072600         IF W-RETOTBV > -99.9 AND < 99.9                                  
072700             MOVE W-RETOTBV               TO MOD-RETOTBV-RAAR             
072800         ELSE                                                             
072900             COMPUTE W-TIONDEL ROUNDED = W-RETOTBV / 10                   
073000             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-RETOTBV-RAAR             
073100             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
073200         END-IF                                                           
073300     END-IF                                                               
073400     MOVE FSG-SUTOTBV-RAAR            TO MOD-SUTOTBV-RAAR                 
074300     MOVE FSG-SULEVANT-RAAR           TO MOD-SULEVANT-RAAR                
074400     IF FSG-SULEVANT-FRAAR NOT = 0                                        
074500         COMPUTE W-RELEVANT ROUNDED =                                     
074600         100 * (FSG-SULEVANT-RAAR - FSG-SULEVANT-FRAAR)/                  
074700                            FSG-SULEVANT-FRAAR                            
074800                                                                          
074900         IF W-RELEVANT > -99.9 AND < 99.9                                 
075000             MOVE W-RELEVANT              TO MOD-RELEVANT-RAAR            
075100         ELSE                                                             
075200             COMPUTE W-TIONDEL ROUNDED = W-RELEVANT / 10                  
075300             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-RELEVANT-RAAR            
075400             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
075500         END-IF                                                           
075600     END-IF                                                               
075700     EJECT                                                                
075800                                                                          
075900****** RAD AF1  *************************************                     
076000                                                                          
076100     MOVE FSG-SUARTFSG-FRAAR          TO MOD-SUARTFSG-FRAAR               
076200     IF FSG-SUARTFSG-FRAAR NOT = 0                                        
076300         COMPUTE W-RETOTBV ROUNDED =                                      
076400         100 * FSG-SUTOTBV-FRAAR / FSG-SUARTFSG-FRAAR                     
076500                                                                          
076600         IF W-RETOTBV > -99.9 AND < 99.9                                  
076700             MOVE W-RETOTBV               TO MOD-RETOTBV-FRAAR            
076800         ELSE                                                             
076900             COMPUTE W-TIONDEL ROUNDED = W-RETOTBV / 10                   
077000             MOVE W-RED-TIONDEL-ASTERISK  TO MOD-RETOTBV-FRAAR            
077100             MOVE '*-VÄRDET ÄR I TIOTAL ' TO MOD-TEMFSINF                 
077200         END-IF                                                           
077300     END-IF                                                               
077400                                                                          
077500****** ÖVRIGA RADER AF2  ****************************                     
077600                                                                          
077700     MOVE FSG-SUTOTBV-FRAAR           TO MOD-SUTOTBV-FRAAR                
077800     MOVE FSG-SULEVANT-FRAAR          TO MOD-SULEVANT-FRAAR               
079900     .                                                                    
080000     EJECT                                                                
080100 CA-NOLLA-MODFALT SECTION.                                                
080200                                                                          
080300     MOVE ZERO                  TO MOD-SUARTFSG-PER                       
080400                                   MOD-RETOTBV-PER                        
080500                                   MOD-SUTOTBV-PER                        
080600                                   MOD-SULEVANT-PER                       
080700                                   MOD-SUARTFSG-AAR                       
080800                                   MOD-RETOTBV-AAR                        
080900                                   MOD-RETOTBV-AAR                        
081000                                   MOD-SUTOTBV-AAR                        
081100                                   MOD-SULEVANT-AAR                       
081200                                   MOD-RELEVANT-AAR                       
081300                                   MOD-SUARTFSG-FAAR                      
081400                                   MOD-RETOTBV-FAAR                       
081500                                   MOD-SUTOTBV-FAAR                       
081600                                   MOD-SULEVANT-FAAR                      
081700                                   MOD-SUARTFSG-RAAR                      
081800                                   MOD-REFSG-RAAR                         
081900                                   MOD-RETOTBV-RAAR                       
082000                                   MOD-SUTOTBV-RAAR                       
082100                                   MOD-SULEVANT-RAAR                      
082200                                   MOD-RELEVANT-RAAR                      
082300                                   MOD-SUARTFSG-FRAAR                     
082400                                   MOD-RETOTBV-FRAAR                      
082500                                   MOD-SUTOTBV-FRAAR                      
082600                                   MOD-SULEVANT-FRAAR                     
082700     .                                                                    
082800     EJECT                                                                
082900 D-LAS-LAGG-UT-DLI-ARTINFO SECTION.                                       
083000     SKIP2                                                                
083100                                                                          
083200     IF INDX = 2                                                          
083300         MOVE 'GB '                 TO W-IDSKYLT                          
083400     END-IF                                                               
083500     PERFORM IMS-GU-BENREG                                                
083600     IF SEGMENT-FINNS                                                     
083700         MOVE WDD3-TEXT-BEART       TO MOD-BEART-SVE                      
083800     END-IF                                                               
083900                                                                          
084000     PERFORM IMS-GU-WDGX-DATUM                                            
084100     IF SEGMENT-FINNS                                                     
084200         MOVE WDGX-3138-TIUPPDAT    TO MOD-TIUPPDAT                       
084300     ELSE                                                                 
084400         MOVE ZERO                  TO MOD-TIUPPDAT                       
084500                                       WDGX-3138-TIUPPDAT                 
084600     END-IF                                                               
084700     PERFORM IMS-GU-WDK601                                                
084800     IF SEGMENT-FINNS                                                     
084900       MOVE WDK601-ART-IDFKNGRP     TO MOD-IDFKNGRP                       
085000       MOVE WDK601-ART-KDPRODSL     TO MOD-KDPRODSL                       
085100       PERFORM IMS-GNP-WDK611                                             
085200       IF SEGMENT-FINNS                                                   
085300**********************************                                        
085400         MOVE IDARTNR-WS            TO COST-IDARTNR                       
085500         MOVE IDDC-WS               TO COST-IDDC                          
085600         MOVE +0                    TO COST-PRARTBES-MON                  
085700         MOVE +0                    TO COST-PRARTBES-MONLOC               
085800         MOVE +0                    TO COST-PRARTSJK-MON                  
085900         MOVE +0                    TO COST-PRARTSJK-MONLOC               
086000         MOVE SPACE                 TO COST-IDLEVNR                       
086100                                                                          
086200         CALL W335COST USING COST-W335COST COST-WDK6-PCB                  
086300                                           COST-WDK7-PCB                  
086400                                           COST-WDF1-PCB                  
086500                                           COST-9305-PCB                  
086600                                           COST-WDK72-PCB                 
086700                                           COST-WDB6-PCB                  
086900         MOVE COST-PRARTSJK-MON        TO MOD-PRARTSJK-MON                
087000         MOVE COST-PRARTBES-MONLOC     TO MOD-PRARTSJK-MONLOC             
087900**********************************                                        
088000         MOVE WDK611-CLAG-PRARTSTD     TO MOD-PRARTSTD                    
088100         MOVE WDK611-CLAG-TIAVIDAT-SEN TO MOD-TIAVIDAT-SEN                
088200         MOVE WDK611-CLAG-PRARTSJK     TO MOD-PRARTSJK                    
088300         MOVE WDK611-CLAG-KDERS        TO MOD-KDERS                       
089200         IF BYT02-RENOV                                                   
089300******KOLL OM RADIO****************'                                      
089400           IF BYT16-RADIO                                                 
089500            ADD +1000                   TO TEST-IDARTNR                   
089600           ELSE                                                           
089700            ADD +6000                   TO TEST-IDARTNR                   
089800           END-IF                                                         
089900***********************************                                       
090000           MOVE TEST-IDARTNR           TO W-IDARTNR-BYT                   
090100           PERFORM IMS-GU-WDK611                                          
090200           IF SEGMENT-FINNS                                               
090300             MOVE OBJ-CLAG-KVPOINT     TO MOD-KVPOINT                     
090400           ELSE                                                           
090500             MOVE WDK611-CLAG-KVPOINT  TO MOD-KVPOINT                     
090600           END-IF                                                         
090700         ELSE                                                             
090800           MOVE WDK611-CLAG-KVPOINT    TO MOD-KVPOINT                     
090900         END-IF                                                           
091000       ELSE                                                               
091100         MOVE ZERO                 TO MOD-PRARTSTD                        
091200                                      MOD-TIAVIDAT-SEN                    
091300                                      MOD-PRARTSJK                        
091400                                      MOD-KVPOINT                         
091500                                      MOD-PRARTSJK-MON                    
091600                                      MOD-PRARTSJK-MONLOC                 
091700       END-IF                                                             
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100 E-PRISINFORMATION SECTION.                                               
092200                                                                          
092300                                                                          
092400     IF IDDISTR-WS > ZERO                                                 
092500       PERFORM EE-HT-PROM-VIA-DISTRIKT                                    
092600     ELSE                                                                 
092700       IF WS-IDMARKBO = SPACE                                             
092800         MOVE 'SEK'             TO MOD-KDVALISO                           
092900       END-IF                                                             
093000     END-IF                                                               
093100                                                                          
093200******** FÖR ATT VIS RENO OCH LANDROVER PRISER FÖR BXX NORDIK MARK        
093300     MOVE W-IDPROMR TO WS-IDPROMR-RENO                                    
093400     MOVE W-IDMARKBO-111 TO WS-IDMARKBO-RENO                              
094200*****END FIX DEL 1                                                        
094300     PERFORM EA-LAES-GRUNDDATA                                            
094400*    IF SEGMENT-SAKNAS                                                    
094500*       MOVE PRICE-MISSING TO MED-IDMFSFEL                                
094600*                                                                         
094700*       CALL WMEDKONV USING MED-WMEDAREA                                  
094800*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
094900*    ELSE                                                                 
095000     IF SEGMENT-FINNS                                                     
095100       PERFORM IMS-GU-WDC201                                              
095200       IF SEGMENT-FINNS                                                   
095300         PERFORM IMS-GNP-WDC211                                           
095400         IF SEGMENT-FINNS                                                 
095500           PERFORM EB-FLYTTA-ARTRAB                                       
095600         ELSE                                                             
095700           PERFORM IMS-GNP-WDC212                                         
095800           IF SEGMENT-FINNS                                               
095900             PERFORM EC-FLYTTA-KAMPANJRAB                                 
096000           END-IF                                                         
096100         END-IF                                                           
096200         PERFORM IMS-GNP-WDC213                                           
096300         IF SEGMENT-FINNS                                                 
096400           PERFORM ED-FLYTTA-NORMALRAB                                    
096500         END-IF                                                           
096600       END-IF                                                             
096700     END-IF                                                               
096800******** FÖR ATT VIS RENO OCH LANDROVER PRISER FÖR BXX NORDIK MARK        
096900     MOVE  WS-IDPROMR-RENO    TO W-IDPROMR                                
097000     MOVE  WS-IDMARKBO-RENO   TO W-IDMARKBO-111                           
097100*****END FIX DEL 1                                                        
097200     .                                                                    
097300     EJECT                                                                
097400 EA-LAES-GRUNDDATA SECTION.                                               
097500     SKIP2                                                                
097600     PERFORM IMS-GU-WDC101                                                
097700     IF SEGMENT-FINNS                                                     
097800       MOVE WDC1-ART-PRARTBTO-MARK TO MOD-PRARTBTO-MARK                   
097900                                      WS-RETAILPRIS                       
099400       MOVE WDC1-ART-KDARTKAM      TO W-KDARTKAM-212-MIN                  
099500                                      W-KDARTKAM-212-MAX                  
099600                                      MOD-KDARTKAM                        
099610       IF BET-FLARTRAB = 'J'                                              
099611         MOVE WDC1-ART-KDARTRAB-ALT  TO SPAR-KDARTRAB                     
099612       ELSE                                                               
099613         MOVE WDC1-ART-KDARTRAB      TO SPAR-KDARTRAB                     
099614       END-IF                                                             
099800       IF WDC1-ART-TIUPPDAT > ZERO                                        
099900         MOVE WDC1-ART-TIUPPDAT      TO WS-DATUM                          
100000         MOVE WS-DATUM6              TO MOD-TIUPPDAT-BTO                  
100100       ELSE                                                               
100200         MOVE SPACE                  TO MOD-TIUPPDAT-BTO                  
100300       END-IF                                                             
100400     ELSE                                                                 
100500       MOVE ZERO                   TO MOD-PRARTBTO-MARK                   
100600                                      WS-RETAILPRIS                       
100700       MOVE ZERO                   TO W-KDARTKAM-212-MIN                  
100800                                      W-KDARTKAM-212-MAX                  
100900                                      MOD-KDARTKAM                        
101000       MOVE ZERO                   TO SPAR-KDARTRAB                       
101100       MOVE SPACE                  TO MOD-TIUPPDAT-BTO                    
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 EB-FLYTTA-ARTRAB SECTION.                                                
101600     SKIP2                                                                
101700     MOVE WDC2-ART-REARTRAB-DO   TO MOD-DO-RAB                            
101800     MOVE WDC2-ART-REARTRAB-BULK TO MOD-MO-RAB                            
101900     IF WS-RETAILPRIS NOT = ZERO                                          
102000       COMPUTE WS-RETAILPRIS-DO ROUNDED =                                 
102100              ((100 - WDC2-ART-REARTRAB-DO )                              
102200              * WS-RETAILPRIS) / 100                                      
102300       COMPUTE WS-RETAILPRIS-MO ROUNDED =                                 
102400              ((100 - WDC2-ART-REARTRAB-BULK )                            
102500              * WS-RETAILPRIS) / 100                                      
102600       MOVE WS-RETAILPRIS-DO     TO MOD-DO-RAB-PRIS                       
102700       MOVE WS-RETAILPRIS-MO     TO MOD-MO-RAB-PRIS                       
103600       MOVE MFS-RENSA-FAELT      TO MOD-KDARTKAM                          
103700       MOVE 'X'                  TO MOD-ARTNR-RAB-FINNS                   
103800     ELSE                                                                 
103900       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB                            
104000       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB                            
104100       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB-PRIS                       
104200       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB-PRIS                       
104300       MOVE MFS-RENSA-FAELT      TO MOD-ARTNR-RAB-FINNS                   
104400       MOVE ZERO                 TO WS-RETAILPRIS-DO                      
104500       MOVE ZERO                 TO WS-RETAILPRIS-MO                      
104600     END-IF                                                               
104700     .                                                                    
104800     EJECT                                                                
104900 EC-FLYTTA-KAMPANJRAB SECTION.                                            
105000     SKIP2                                                                
105100     MOVE WDC2-KAM-KDARTKAM      TO MOD-KDARTKAM                          
105200     MOVE WDC2-KAM-KDARTKAM      TO MOD-KDARTKAM-VALID                    
105300     MOVE WDC2-KAM-REARTRAB-DO   TO MOD-DO-RAB                            
105400     MOVE WDC2-KAM-REARTRAB-BULK TO MOD-MO-RAB                            
105500     IF WS-RETAILPRIS NOT = ZERO                                          
105600       COMPUTE WS-RETAILPRIS-DO ROUNDED =                                 
105700               ((100 - WDC2-KAM-REARTRAB-DO )                             
105800               * WS-RETAILPRIS) / 100                                     
105900       COMPUTE WS-RETAILPRIS-MO ROUNDED =                                 
106000               ((100 - WDC2-KAM-REARTRAB-BULK)                            
106100               * WS-RETAILPRIS ) / 100                                    
106200       MOVE WS-RETAILPRIS-DO     TO MOD-DO-RAB-PRIS                       
106300       MOVE WS-RETAILPRIS-MO     TO MOD-MO-RAB-PRIS                       
107200       MOVE MFS-RENSA-FAELT      TO MOD-ARTNR-RAB-FINNS                   
107300     ELSE                                                                 
107400       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB                            
107500       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB                            
107600       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB-PRIS                       
107700       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB-PRIS                       
107800       MOVE MFS-RENSA-FAELT      TO MOD-KDARTKAM                          
107900       MOVE ZERO                 TO WS-RETAILPRIS-DO                      
108000       MOVE ZERO                 TO WS-RETAILPRIS-MO                      
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 ED-FLYTTA-NORMALRAB SECTION.                                             
108500     SKIP2                                                                
108600******************************************************************        
108700**************** OM KDARTRAB ÄR NOLL SÅ HAR ARTIKELN INGEN  ******        
108800**************** RABATT UTAN DET ÄR RETAILPRISET SOM GÄLLER ******        
108900******************************************************************        
109000     IF SPAR-KDARTRAB > ZERO                                              
109100       MOVE SPAR-KDARTRAB TO RAB-INDX                                     
109200                             MOD-NORMAL-RABATT                            
109300       MOVE WDC2-RAB-REARTRAB-DO(RAB-INDX) TO MOD-DO-NOR-RAB              
109400       MOVE WDC2-RAB-REARTRAB-BULK(RAB-INDX) TO MOD-MO-NOR-RAB            
109500       IF WS-RETAILPRIS NOT = ZERO                                        
109600         COMPUTE WS-RETAILPRIS-NOR-DO ROUNDED =                           
109700                 ((100 - WDC2-RAB-REARTRAB-DO(RAB-INDX))                  
109800                 * WS-RETAILPRIS ) / 100                                  
109900         COMPUTE WS-RETAILPRIS-NOR-MO ROUNDED =                           
110000                 ((100 - WDC2-RAB-REARTRAB-BULK(RAB-INDX))                
110100                 * WS-RETAILPRIS ) / 100                                  
110200       ELSE                                                               
110300         MOVE ZERO             TO WS-RETAILPRIS-NOR-DO                    
110400         MOVE ZERO             TO WS-RETAILPRIS-NOR-MO                    
110500       END-IF                                                             
110600       MOVE WS-RETAILPRIS-NOR-DO TO MOD-DO-NOR-PRIS                       
110700       MOVE WS-RETAILPRIS-NOR-MO TO MOD-MO-NOR-PRIS                       
111600     ELSE                                                                 
111700       MOVE ZERO                 TO MOD-NORMAL-RABATT                     
111800       MOVE ZERO                 TO MOD-DO-NOR-RAB                        
111900       MOVE ZERO                 TO MOD-MO-NOR-RAB                        
112000       MOVE WS-RETAILPRIS        TO MOD-DO-NOR-PRIS                       
112100       MOVE WS-RETAILPRIS        TO MOD-MO-NOR-PRIS                       
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 EE-HT-PROM-VIA-DISTRIKT SECTION.                                         
113200     SKIP2                                                                
113300                                                                          
113400     PERFORM IMS-GET-WDB201                                               
113500     IF SEGMENT-FINNS                                                     
113600        MOVE WDB2-GMT-IDPARTNR    TO W-WDB1-IDPARTNR                      
113700        MOVE WDB2-GMT-IDFTG       TO W-WDB1-IDFTG                         
113710        MOVE WDB2-GMT-KDKUNDKAT   TO W-WDB2-KDKUNDKAT                     
113800                                                                          
113900        PERFORM IMS-GET-WDB101                                            
114000        IF SEGMENT-FINNS                                                  
114100           MOVE BET-BEBETRAD-1    TO MOD-BEKOPARE-RAD1                    
114200           MOVE BET-IDPROMR       TO WS-IDPROMR                           
114300                                     W-IDPROMR                            
114400           MOVE WS-MARKBOLAG      TO W-IDMARKBO-111                       
114500                                     WS-IDMARKBO                          
114700           MOVE WS-KDVALISO-MC    TO MOD-KDVALISO                         
114800        ELSE                                                              
114900           MOVE 'MISSING'         TO MOD-BEKOPARE-RAD1                    
115000           MOVE SPACE             TO MOD-KDVALISO                         
115100        END-IF                                                            
115200     END-IF                                                               
115300     .                                                                    
115400     EJECT                                                                
115500 F-CHECK-LOC-SOURCED SECTION.                                             
115600                                                                          
115700     MOVE NEJ                         TO SW-AVT                           
115710     SET DCLAND-IX TO 1                                                   
115900                                                                          
116000     PERFORM UNTIL AVT-FOUND OR                                           
116001                   DCLAND-IX > DCLAND-IX-MAX                              
116002       IF DCLAND-USA (DCLAND-IX) OR                                       
116003          DCLAND-CHINA (DCLAND-IX)                                        
116010         MOVE DCLAND-IDDC(DCLAND-IX)    TO W-IDDC                         
116100         PERFORM IMS-GU-WDK711                                            
116200         IF SEGMENT-FINNS                                                 
116300            PERFORM IMS-GNP-WDK723                                        
116400            IF SEGMENT-FINNS                                              
116500               MOVE JA                  TO SW-AVT                         
116900            END-IF                                                        
117300         END-IF                                                           
117310       END-IF                                                             
117320       SET DCLAND-IX UP BY 1                                              
117400     END-PERFORM                                                          
117500                                                                          
117600     IF AVT-FOUND                                                         
117700        IF MSGI-IDLAND-SPR = 'GB'                                         
117800           MOVE YA                    TO MOD-FLSOURCE                     
117900        ELSE                                                              
118000           MOVE JA                    TO MOD-FLSOURCE                     
118100        END-IF                                                            
118200     ELSE                                                                 
118300        MOVE NEJ                      TO MOD-FLSOURCE                     
118400     END-IF                                                               
118500     .                                                                    
118600     EJECT                                                                
128300* IMS SEKTIONER                                                           
128400                                                                          
128500 IMS-GET-MSG SECTION.                                                     
128600                                                                          
128700     MOVE '  QC' TO GODK-STATUSKODER                                      
128800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
128900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
129000     PERFORM IMS-STATUSKONTROLL                                           
129100     .                                                                    
129200     SKIP3                                                                
129300 IMS-INSERT-MSG SECTION.                                                  
129400                                                                          
129500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
129600         MOVE '0' TO MFS-KDHUVOMR                                         
129700     END-IF                                                               
129800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
129900     MOVE SPACE TO GODK-STATUSKODER                                       
130000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
130100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130200     PERFORM IMS-STATUSKONTROLL                                           
130300     .                                                                    
130400     EJECT                                                                
130500 IMS-GU-WDC101 SECTION.                                                   
130600     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
130700          DELIMITED BY SIZE INTO SSA1                                     
130800     MOVE '  GE' TO GODK-STATUSKODER                                      
130900     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-AREA SSA1                      
131000     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
131100     PERFORM IMS-STATUSKONTROLL                                           
131200     .                                                                    
131300     SKIP3                                                                
131400 IMS-GU-WDC201 SECTION.                                                   
131500     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
131600          DELIMITED BY SIZE INTO SSA1                                     
131700     MOVE '  GE' TO GODK-STATUSKODER                                      
131800     CALL CBLTDLI USING GU WDC2-PCB DLI-IO-AREA2 SSA1                     
131900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
132000     PERFORM IMS-STATUSKONTROLL                                           
132100     .                                                                    
132200     SKIP3                                                                
132300 IMS-GNP-WDC211 SECTION.                                                  
132400     STRING 'WDC211  (WDC211KY>=' W-WDC211KY-MIN-X                        
132500                    '&WDC211KY<=' W-WDC211KY-MAX-X ')'                    
132600          DELIMITED BY SIZE INTO SSA1                                     
132700     MOVE '  GE' TO GODK-STATUSKODER                                      
132800     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA2 SSA1                    
132900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
133000     PERFORM IMS-STATUSKONTROLL                                           
133100     .                                                                    
133200     EJECT                                                                
133300 IMS-GNP-WDC212 SECTION.                                                  
133400     STRING 'WDC212  (WDC212KY>=' W-WDC212KY-MIN-X                        
133500                    '&WDC212KY<=' W-WDC212KY-MAX-X ')'                    
133600          DELIMITED BY SIZE INTO SSA1                                     
133700     MOVE '  GE' TO GODK-STATUSKODER                                      
133800     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA2 SSA1                    
133900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
134000     PERFORM IMS-STATUSKONTROLL                                           
134100     .                                                                    
134200     SKIP3                                                                
134300 IMS-GNP-WDC213 SECTION.                                                  
134400     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
134500          DELIMITED BY SIZE INTO SSA1                                     
134600     STRING 'WDC213  (DASTADAT<=' W-DASTADAT-X ')'                        
134700          DELIMITED BY SIZE INTO SSA2                                     
134800     MOVE '  GE' TO GODK-STATUSKODER                                      
134900     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA2 SSA1 SSA2               
135000     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
135100     PERFORM IMS-STATUSKONTROLL                                           
135200     .                                                                    
135300     EJECT                                                                
135400 IMS-GU-BENREG  SECTION.                                                  
135500                                                                          
135600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
135700            DELIMITED BY SIZE INTO SSA1                                   
135800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
135900            DELIMITED BY SIZE INTO SSA2                                   
136000     MOVE '  GE' TO GODK-STATUSKODER                                      
136100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA SSA1 SSA2                 
136200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     .                                                                    
136500     SKIP3                                                                
136600 IMS-GU-WDGX-DATUM SECTION.                                               
136700                                                                          
136800     STRING 'WLXXCJ01(WDG3KEY  =' W-WDGX3137-KEY ')'                      
136900            DELIMITED BY SIZE INTO SSA1                                   
137000     MOVE 'WLXXCJ11 '        TO SSA2                                      
137100     MOVE '  GE' TO GODK-STATUSKODER                                      
137200     CALL CBLTDLI USING GU WDGX-PCB DLI-IO-AREA SSA1 SSA2                 
137300     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
137400     PERFORM IMS-STATUSKONTROLL                                           
137500     .                                                                    
137600     EJECT                                                                
137700 IMS-GU-WDK601 SECTION.                                                   
137800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
137900          DELIMITED BY SIZE INTO SSA1                                     
138000     MOVE '  GE' TO GODK-STATUSKODER                                      
138100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1                      
138200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
138300     PERFORM IMS-STATUSKONTROLL                                           
138400     .                                                                    
138500     SKIP3                                                                
138600 IMS-GNP-WDK611 SECTION.                                                  
138700     MOVE 'WDK611 ' TO SSA1                                               
138800     MOVE '  GE' TO GODK-STATUSKODER                                      
138900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1                      
139000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
139100     PERFORM IMS-STATUSKONTROLL                                           
139200     .                                                                    
139300     EJECT                                                                
139400 IMS-GU-WDK611 SECTION.                                                   
139500                                                                          
139600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-BYT-X ')'                     
139700            DELIMITED BY SIZE INTO SSA1                                   
139800     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
139900     MOVE '  GE' TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA4 SSA1 SSA2                
140100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400     EJECT                                                                
140500 IMS-GET-WDB101 SECTION.                                                  
140600                                                                          
140700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
140800          DELIMITED BY SIZE INTO SSA1                                     
140900     MOVE '  GE' TO GODK-STATUSKODER                                      
141000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
141100     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
141200     PERFORM IMS-STATUSKONTROLL                                           
141300     .                                                                    
141400     SKIP2                                                                
141500 IMS-GET-WDB201    SECTION.                                               
141600                                                                          
141700     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
141800                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
141900             DELIMITED BY SIZE INTO SSA1                                  
142000     MOVE '  GE' TO GODK-STATUSKODER                                      
142100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
142200     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     SKIP3                                                                
145100 IMS-GU-WDK711 SECTION.                                                   
145200                                                                          
145300     MOVE SPACES    TO SSA1                                               
145400                       SSA2                                               
145500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
145600            DELIMITED BY SIZE INTO SSA1                                   
145700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
145800            DELIMITED BY SIZE INTO SSA2                                   
145900     MOVE '  GE' TO GODK-STATUSKODER                                      
146000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
146100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     EJECT                                                                
146500 IMS-GNP-WDK723 SECTION.                                                  
146600                                                                          
146700     MOVE 'WDK723  '       TO SSA1                                        
146800     MOVE '  GEGB'         TO GODK-STATUSKODER                            
146900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK723 SSA1                    
147000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
147100     PERFORM IMS-STATUSKONTROLL                                           
147200     .                                                                    
147300     EJECT                                                                
147400 IMS-STATUSKONTROLL SECTION.                                              
147500                                                                          
147600     SET STATUS-IX TO 1                                                   
147700     SEARCH GODK-STATUS AT END CALL FELLOG                                
147800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
147900     END-SEARCH                                                           
148000     .                                                                    
148100     EJECT                                                                
148200 DB2-SELECT-FSG2-TAB SECTION.                                             
148300                                                                          
148400     MOVE 000100           TO GODK-SQLCODEKODER                           
148500     EXEC SQL                                                             
148600         SELECT SUARTFSG_PER,       SUARTFSG_AAR,                         
148700                SUARTFSG_RAAR_SPEC, SUARTFSG_RAAR_RAB,                    
148800                SUARTFSG_RAAR_MAN,  SUARTFSG_RAAR_KRE,                    
148900                SUARTFSG_FAAR,      SUARTFSG_RAAR,                        
149000                SUARTFSG_FRAAR,     SULEVANT_PER,                         
149100                SULEVANT_AAR,       SULEVANT_RAAR_SPEC,                   
149200                SULEVANT_RAAR_RAB,  SULEVANT_RAAR_MAN,                    
149300                SULEVANT_RAAR_KRE,  SULEVANT_FAAR,                        
149400                SULEVANT_RAAR,      SULEVANT_FRAAR,                       
149500                SUTOTBV_PER,        SUTOTBV_AAR,                          
149600                SUTOTBV_FAAR,       SUTOTBV_RAAR,                         
149700                SUTOTBV_FRAAR                                             
149800         INTO  :FSG-SUARTFSG-PER,       :FSG-SUARTFSG-AAR,                
149900               :FSG-SUARTFSG-RAAR-SPEC, :FSG-SUARTFSG-RAAR-RAB,           
150000               :FSG-SUARTFSG-RAAR-MAN,  :FSG-SUARTFSG-RAAR-KRE,           
150100               :FSG-SUARTFSG-FAAR,      :FSG-SUARTFSG-RAAR,               
150200               :FSG-SUARTFSG-FRAAR,     :FSG-SULEVANT-PER,                
150300               :FSG-SULEVANT-AAR,       :FSG-SULEVANT-RAAR-SPEC,          
150400               :FSG-SULEVANT-RAAR-RAB,  :FSG-SULEVANT-RAAR-MAN,           
150500               :FSG-SULEVANT-RAAR-KRE,  :FSG-SULEVANT-FAAR,               
150600               :FSG-SULEVANT-RAAR,      :FSG-SULEVANT-FRAAR,              
150700               :FSG-SUTOTBV-PER,        :FSG-SUTOTBV-AAR,                 
150800               :FSG-SUTOTBV-FAAR,       :FSG-SUTOTBV-RAAR,                
150900               :FSG-SUTOTBV-FRAAR                                         
151000         FROM FSG2                                                        
151100         WHERE IDARTNR = :W-IDARTNR                                       
151200     END-EXEC                                                             
151300     MOVE SQLCODE          TO SQLCODE-WS                                  
151400     PERFORM DB2-STATUSKONTROLL                                           
151500     .                                                                    
151600     EJECT                                                                
151700 DB2-SELECT-FSG4-TAB SECTION.                                             
151800                                                                          
151900     MOVE 000100           TO GODK-SQLCODEKODER                           
152000     EXEC SQL                                                             
152100         SELECT SUARTFSG_PER,       SUARTFSG_AAR,                         
152200                SUARTFSG_RAAR_SPEC, SUARTFSG_RAAR_RAB,                    
152300                SUARTFSG_RAAR_MAN,  SUARTFSG_RAAR_KRE,                    
152400                SUARTFSG_FAAR,      SUARTFSG_RAAR,                        
152500                SUARTFSG_FRAAR,     SULEVANT_PER,                         
152600                SULEVANT_AAR,       SULEVANT_RAAR_SPEC,                   
152700                SULEVANT_RAAR_RAB,  SULEVANT_RAAR_MAN,                    
152800                SULEVANT_RAAR_KRE,  SULEVANT_FAAR,                        
152900                SULEVANT_RAAR,      SULEVANT_FRAAR,                       
153000                SUTOTBV_PER,        SUTOTBV_AAR,                          
153100                SUTOTBV_FAAR,       SUTOTBV_RAAR,                         
153200                SUTOTBV_FRAAR                                             
153300         INTO  :FSG-SUARTFSG-PER,       :FSG-SUARTFSG-AAR,                
153400               :FSG-SUARTFSG-RAAR-SPEC, :FSG-SUARTFSG-RAAR-RAB,           
153500               :FSG-SUARTFSG-RAAR-MAN,  :FSG-SUARTFSG-RAAR-KRE,           
153600               :FSG-SUARTFSG-FAAR,      :FSG-SUARTFSG-RAAR,               
153700               :FSG-SUARTFSG-FRAAR,     :FSG-SULEVANT-PER,                
153800               :FSG-SULEVANT-AAR,       :FSG-SULEVANT-RAAR-SPEC,          
153900               :FSG-SULEVANT-RAAR-RAB,  :FSG-SULEVANT-RAAR-MAN,           
154000               :FSG-SULEVANT-RAAR-KRE,  :FSG-SULEVANT-FAAR,               
154100               :FSG-SULEVANT-RAAR,      :FSG-SULEVANT-FRAAR,              
154200               :FSG-SUTOTBV-PER,        :FSG-SUTOTBV-AAR,                 
154300               :FSG-SUTOTBV-FAAR,       :FSG-SUTOTBV-RAAR,                
154400               :FSG-SUTOTBV-FRAAR                                         
154500         FROM FSG4                                                        
154600         WHERE IDARTNR = :W-IDARTNR AND IDDISTR =:W-IDDISTR               
154700     END-EXEC                                                             
154800     MOVE SQLCODE          TO SQLCODE-WS                                  
154900     PERFORM DB2-STATUSKONTROLL                                           
155000     .                                                                    
155100     EJECT                                                                
155200 DB2-SELECT-FSG5-TAB SECTION.                                             
155300                                                                          
155400     MOVE 000100           TO GODK-SQLCODEKODER                           
155500     EXEC SQL                                                             
155600         SELECT SUARTFSG_PER,       SUARTFSG_AAR,                         
155700                SUARTFSG_RAAR_SPEC, SUARTFSG_RAAR_RAB,                    
155800                SUARTFSG_RAAR_MAN,  SUARTFSG_RAAR_KRE,                    
155900                SUARTFSG_FAAR,      SUARTFSG_RAAR,                        
156000                SUARTFSG_FRAAR,     SULEVANT_PER,                         
156100                SULEVANT_AAR,       SULEVANT_RAAR_SPEC,                   
156200                SULEVANT_RAAR_RAB,  SULEVANT_RAAR_MAN,                    
156300                SULEVANT_RAAR_KRE,  SULEVANT_FAAR,                        
156400                SULEVANT_RAAR,      SULEVANT_FRAAR,                       
156500                SUTOTBV_PER,        SUTOTBV_AAR,                          
156600                SUTOTBV_FAAR,       SUTOTBV_RAAR,                         
156700                SUTOTBV_FRAAR                                             
156800         INTO  :FSG-SUARTFSG-PER,       :FSG-SUARTFSG-AAR,                
156900               :FSG-SUARTFSG-RAAR-SPEC, :FSG-SUARTFSG-RAAR-RAB,           
157000               :FSG-SUARTFSG-RAAR-MAN,  :FSG-SUARTFSG-RAAR-KRE,           
157100               :FSG-SUARTFSG-FAAR,      :FSG-SUARTFSG-RAAR,               
157200               :FSG-SUARTFSG-FRAAR,     :FSG-SULEVANT-PER,                
157300               :FSG-SULEVANT-AAR,       :FSG-SULEVANT-RAAR-SPEC,          
157400               :FSG-SULEVANT-RAAR-RAB,  :FSG-SULEVANT-RAAR-MAN,           
157500               :FSG-SULEVANT-RAAR-KRE,  :FSG-SULEVANT-FAAR,               
157600               :FSG-SULEVANT-RAAR,      :FSG-SULEVANT-FRAAR,              
157700               :FSG-SUTOTBV-PER,        :FSG-SUTOTBV-AAR,                 
157800               :FSG-SUTOTBV-FAAR,       :FSG-SUTOTBV-RAAR,                
157900               :FSG-SUTOTBV-FRAAR                                         
158000         FROM FSG5                                                        
158100         WHERE IDARTNR = :W-IDARTNR AND IDPROMR =:W-IDPROMR               
158200     END-EXEC                                                             
158300     MOVE SQLCODE          TO SQLCODE-WS                                  
158400     PERFORM DB2-STATUSKONTROLL                                           
158500     .                                                                    
158600     EJECT                                                                
158700 DB2-STATUSKONTROLL SECTION.                                              
158800                                                                          
158900     SET SQLCODE-IX        TO 1                                           
159000     SEARCH GODK-SQLCODE AT END CALL FELLOG                               
159100       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
159200          CONTINUE                                                        
159300     END-SEARCH                                                           
159400     .                                                                    
