000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W9042900.                                                
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
002000*        TRANSAKTION: W90429T                                             
002100*        MID:         W90429I1                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W90429O1                                            
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W9042900'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003700                                                                          
003800 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
003900 77  IDDISTR-WS                  PIC X(4)    VALUE SPACE.                 
004000 77  IDPROMR-WS                  PIC X(3)    VALUE SPACE.                 
004100 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
004200 77  SPAR-KDARTRAB               PIC 9(2)    VALUE ZERO.                  
004300 77  WS-RETAILPRIS               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004400 77  WS-RETAILPRIS-DO            PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004500 77  WS-RETAILPRIS-MO            PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004600 77  WS-RETAILPRIS-NOR-DO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004700 77  WS-RETAILPRIS-NOR-MO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004800 77  RAB-INDX                    PIC 9(2)    VALUE ZERO.                  
004900                                                                          
005000 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
005100                                                                          
005200 01  WS-DATUM                    PIC X(7)    VALUE SPACE.                 
005300 01  FILLER REDEFINES WS-DATUM.                                           
005400     03 WS-FILLER                PIC X(1).                                
005500     03 WS-DATUM6                PIC X(6).                                
005600                                                                          
005700 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
005800 01  FILLER REDEFINES WS-IDPROMR.                                         
005900     03  WS-MARKBOLAG            PIC X(1).                                
006000     03  WS-IDPROMRN             PIC X(2).                                
006100                                                                          
006200 01  W-ARBETSFALT.                                                        
006300     03  W-RETOTBV               PIC S9(9)V9 VALUE +0   COMP-3.           
006400     03  W-REFSG                 PIC S9(9)V9 VALUE +0   COMP-3.           
006500     03  W-RELEVANT              PIC S9(9)V9 VALUE +0   COMP-3.           
006600                                                                          
006700     03  W-RED-TIONDEL-ASTERISK.                                          
006800         05  W-TIONDEL           PIC -(4).                                
006900         05  FILLER              PIC X   VALUE '*'.                       
007000     EJECT                                                                
007100                                                                          
007200 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
007300*01  FILLER    -COPY WWBYT02  -RED  TEST-IDARTNR.                         
007400     EJECT                                                                
007500*01  -COPY WWBYT16  .                                                     
007600*                                                                         
007700     EJECT                                                                
007800*01  -COPY WWDIST79 .                                                     
007900 01  FILLER                      PIC X(16)   VALUE 'DL1-NYCKLAR'.         
008000 01    NYCKLAR-TILL-DLI-DB2.                                              
008100   03    W-WDGX3137-KEY.                                                  
008200     05    W-IDHTYP              PIC X(4)    VALUE '3137'.                
008300     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
008400                                                                          
008500   03    W-IDPROMR-X.                                                     
008600     05    W-IDPROMR             PIC X(3) VALUE SPACE.                    
008700                                                                          
008800   03    W-IDARTNR-X.                                                     
008900     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
009000                                                                          
009100   03    W-IDARTNR-BYT-X.                                                 
009200     05    W-IDARTNR-BYT         PIC S9(9)   VALUE ZERO  COMP-3.          
009300***** NYCKLAR TILL WDC2 *******************************                   
009400                                                                          
009500                                                                          
009600     03  W-WDC211KY-MIN-X.                                                
009700         05  W-IDARTNR-211-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
009800         05  W-DASTADAT-211-MIN  PIC 9(8)    VALUE ZERO.                  
009900                                                                          
010000     03  W-WDC211KY-MAX-X.                                                
010100         05  W-IDARTNR-211-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
010200         05  W-DASTADAT-211-MAX  PIC 9(8)    VALUE ZERO.                  
010300                                                                          
010400     03  W-WDC212KY-MIN-X.                                                
010500         05  W-KDARTKAM-212-MIN  PIC 9(5)    VALUE ZERO.                  
010600         05  W-DASTADAT-212-MIN  PIC 9(8)    VALUE ZERO.                  
010700                                                                          
010800     03  W-WDC212KY-MAX-X.                                                
010900         05  W-KDARTKAM-212-MAX  PIC 9(5)    VALUE ZERO.                  
011000         05  W-DASTADAT-212-MAX  PIC 9(8)    VALUE ZERO.                  
011100                                                                          
011200     03  W-DASTADAT-X.                                                    
011300         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
011400                                                                          
011500******************************************************************        
011600                                                                          
011700******* NYCKLAR TILL WDC1   *******************                           
011800                                                                          
011900     03  W-WDC101KY-X.                                                    
012000         05  W-IDARTNR-111       PIC S9(9)   VALUE ZERO COMP-3.           
012100         05  W-IDMARKBO-111      PIC X       VALUE SPACE.                 
012200***************************                                               
012300*** NYCKLAR TILL KUNDREG             ***********                          
012400     03    FILLER                  PIC X(16)   VALUE 'KUNDREG'.           
012500                                                                          
012600     03  W-IDGMT-MIN-X.                                                   
012700         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
012800         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
012900                                                                          
013000     03  W-IDGMT-MAX-X.                                                   
013100         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
013200         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE 9999999                
013300                                                        COMP-3.           
013400                                                                          
013500     03  W-IDDISTR-X.                                                     
013600         05  W-IDDISTR           PIC S9(5)       COMP-3.                  
013700                                                                          
013800*   NYCKLAR TILL BETALARREGISTRET    ***********                          
013900     03  W-WDB101KY-X.                                                    
014000         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
014100         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
014200                                                                          
014300*** SUBPROGRAM OCH PARAMETERAREOR                                         
014400                                                                          
014500 01  GENERELLA-SUBPOROGRAM.                                               
014600     03  WMEDKONV               PIC X(8) VALUE 'WMEDKONV'.                
014700     03  W005INIT               PIC X(8) VALUE 'W005INIT'.                
014800     03  CBLTDLI                PIC X(8) VALUE 'CBLTDLI '.                
014900     03  FELLOG                 PIC X(8) VALUE 'FELLOG  '.                
015000                                                                          
015100*                **** PARAMETRAR TILL WMEDKONV                            
015200*01      -COPY WMEDAREA                                                   
015300                                                                          
015400 01  MESSAGE-CODES.                                                       
015500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015600     03  ERR-ART-MISSING         PIC X(3)    VALUE '017'.                 
015700     03  PRICE-MISSING           PIC X(3)    VALUE '171'.                 
015800     EJECT                                                                
015900*                **** PARAMETRAR TILL W005INIT                            
016000*                                                                         
016100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016200     SKIP3                                                                
016300*01      -COPY WMSGINIT                                                   
016400                                                                          
016500 01  GEMENSAMMA-SUBPROGRAM.                                               
016600     03  W335COST                PIC X(8)    VALUE 'W335COST'.            
016700                                                                          
016800*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
016900 01 FILLER                       PIC X(8)    VALUE 'W335COST'.            
017000*   -COPY W335COST                                                        
017100     EJECT                                                                
017200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300******************************************************************        
017400*                                                                         
017500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
017600*                                                                         
017700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
017800     SKIP3                                                                
017900*01    MID -COPY W90429I1.                                                
018000     EJECT                                                                
018100*01    -COPY WMSGAREA                                                     
018200     EJECT                                                                
018300*  03    MOD -COPY W90429O1  -RED MSG-AREA.                               
018400     EJECT                                                                
018500*01    -COPY WMFSAREA                                                     
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)  VALUE 'FSG-AREA'.             
018800****    HÄR ANVÄNDS FSG2-COPYTEXTEN GENERELLT FÖR FSG2 OCH                
018900****    FSG4 I ANROP MOT DB2, DÅ DESSA FÖRUTOM NYCKLARNA ÄR LIKA.         
019000                                                                          
019100*01  FILLER -COPY FSG2 -PRE FSG-                                          
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)  VALUE 'FSG2-AREA'.            
019400       EXEC SQL INCLUDE FSG2     END-EXEC.                                
019500                                                                          
019600       EXEC SQL INCLUDE FSG4     END-EXEC.                                
019700                                                                          
019800       EXEC SQL INCLUDE FSG5     END-EXEC.                                
019900                                                                          
020000 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
020100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
020200                                                                          
020300 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
020400 01  DB2-WS.                                                              
020500     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
020600         88  CURSOR-OK                      VALUE 000.                    
020700         88  RADER-FINNS                    VALUE 000.                    
020800         88  RADER-SAKNAS                   VALUE 100.                    
020900         88  904-KOD                        VALUE 904.                    
021000     03  GODK-SQLCODEKODER.                                               
021100         05  GODK-SQLCODE OCCURS 4                                        
021200             INDEXED BY SQLCODE-IX PIC 9(3).                              
021300     EJECT                                                                
021400                                                                          
021500**** IMS AREOR *******                                                    
021600 01    IMS-WS.                                                            
021700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
021800     SKIP3                                                                
021900*                        **** STATUS-KOD FRÅN IMS                         
022000   03    STATUS-WS               PIC XX.                                  
022100     88    SEGMENT-FINNS                     VALUE '  '.                  
022200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
022300     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
022400     SKIP3                                                                
022500   03    GODK-STATUSKODER.                                                
022600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
022700     SKIP3                                                                
022800 01    SSA1                      PIC X(64).                               
022900 01    SSA2                      PIC X(64).                               
023000     EJECT                                                                
023100*                            IMS FUNKTIONSKODER                           
023200*01    -COPY W0003                                                        
023300     EJECT                                                                
023400******************************************************************        
023500*                                                                         
023600*        ARBETS-AREA  TILL DB2                                            
023700*                                                                         
023800*                            DLI INPUT-OUTPUT AREA                        
023900 01    DLI-IO-AREA.                                                       
024000   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
024100                                                                          
024200                                                                          
024300*  03    WLARTC01 -COPY WDK601 -PRE ARTC01- -RED IO-AREA.                 
024400                                                                          
024500*  03    WLARTC11 -COPY WDK611 -PRE ARTC11- -RED IO-AREA.                 
024600                                                                          
024700*  03    WLPRIA01 -COPY WDC101 -PRE PRIA-   -RED IO-AREA.                 
024800                                                                          
024900*  03    WLXXCJ11 -COPY WDGX3138 -PRE WDGX- -RED IO-AREA.                 
025000     EJECT                                                                
025100                                                                          
025200 01  DLI-IO-AREA2.                                                        
025300     03  IO-AREA2                PIC X(850)  VALUE SPACE.                 
025400     03  WLPRIB01 REDEFINES IO-AREA2.                                     
025500*        05  -COPY WDC201  -PRE PRIB-                                     
025600     SKIP3                                                                
025700     03  WLPRIB11 REDEFINES IO-AREA2.                                     
025800*        05  -COPY WDC211  -PRE PRIB-                                     
025900     EJECT                                                                
026000     03  WLPRIB12 REDEFINES IO-AREA2.                                     
026100*        05  -COPY WDC212  -PRE PRIB-                                     
026200     EJECT                                                                
026300     03  WLPRIB13 REDEFINES IO-AREA2.                                     
026400*        05  -COPY WDC213  -PRE PRIB-                                     
026500     EJECT                                                                
026600**   KUNDREGISTER                                                         
026700 01  DLI-IO-WDB201.                                                       
026800*    03  WDB201    -COPY WDB201 -PRE WDB2-                                
026900     EJECT                                                                
027000**   BETALARREGISTER                                                      
027100 01  DLI-IO-WDB101.                                                       
027200*  03  -COPY WDB101                                                       
027300     EJECT                                                                
027400**   BYTSARTIKLAR                                                         
027500 01  DLI-IO-AREA4.                                                        
027600     03  WDK611   -COPY WDK611 -PRE OBJ-                                  
027700     EJECT                                                                
027800 LINKAGE SECTION.                                                         
027900*01    -COPY W0009     -PRE MSG-                                          
028000                                                                          
028100*01    -COPY W0008     -PRE USEA-                                         
028200     05  FILLER                  PIC X.                                   
028300     EJECT                                                                
028400*01    -COPY W0008     -PRE ARTC-                                         
028500     05  FILLER                  PIC X.                                   
028600                                                                          
028700*01    -COPY W0008     -PRE WDB2-                                         
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000*01    -COPY W0008     -PRE WDGX-                                         
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01    -COPY W0008     -PRE PRIA-                                         
029400     05  FILLER                  PIC X.                                   
029500                                                                          
029600*01    -COPY W0008     -PRE PRIB-                                         
029700     05  FILLER                  PIC X.                                   
029800                                                                          
029900*01    -COPY W0008     -PRE WDB1-                                         
030000     05  FILLER                  PIC X.                                   
030100 01  COST-WDK6-PCB               PIC X.                                   
030200 01  COST-WDK7-PCB               PIC X.                                   
030300 01  COST-WDF1-PCB               PIC X.                                   
030400 01  COST-9305-PCB               PIC X.                                   
030500 01  COST-WDK72-PCB              PIC X.                                   
030600 01  COST-WDB6-PCB               PIC X.                                   
030800     EJECT                                                                
030900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
031000                 ARTC-PCB WDB2-PCB WDGX-PCB                               
031100                 PRIA-PCB PRIB-PCB WDB1-PCB                               
031200                 COST-WDK6-PCB                                            
031300                 COST-WDK7-PCB                                            
031400                 COST-WDF1-PCB                                            
031500                 COST-9305-PCB                                            
031600                 COST-WDK72-PCB COST-WDB6-PCB.                            
031800 MAIN SECTION.                                                            
031900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
032000                 ARTC-PCB WDB2-PCB WDGX-PCB                               
032100                 PRIA-PCB PRIB-PCB WDB1-PCB                               
032200                 COST-WDK6-PCB                                            
032300                 COST-WDK7-PCB                                            
032400                 COST-WDF1-PCB                                            
032500                 COST-9305-PCB                                            
032600                 COST-WDK72-PCB COST-WDB6-PCB.                            
032800     PERFORM IMS-GET-MSG                                                  
032900     IF SEGMENT-FINNS                                                     
033000         PERFORM A-INIT-SPARA-INPUT                                       
033100         PERFORM B-KOLLA-NYCKLAR                                          
033200         IF IDARTNR-WS NOT NUMERIC                                        
033300             MOVE '001'           TO MED-IDMFSFEL                         
033400             CALL WMEDKONV USING MED-WMEDAREA                             
033500             MOVE MED-MFSFEL      TO MOD-TEMFSFEL                         
033600         ELSE                                                             
033700             PERFORM D-LAS-LAGG-UT-DLI-ARTINFO                            
033800             PERFORM E-PRISINFORMATION                                    
033900             IF W-IDDISTR > ZERO                                          
034000                PERFORM DB2-SELECT-FSG4-TAB                               
034100             ELSE                                                         
034200               IF W-IDPROMR NOT = SPACE                                   
034300                  PERFORM DB2-SELECT-FSG5-TAB                             
034400               ELSE                                                       
034500                  PERFORM  DB2-SELECT-FSG2-TAB                            
034600               END-IF                                                     
034700             END-IF                                                       
034800             IF RADER-FINNS                                               
034900                 PERFORM C-LAGG-UT-DB2-ARTINFO                            
035000             ELSE                                                         
035100                 MOVE '005'           TO MED-IDMFSFEL                     
035200                 CALL WMEDKONV USING MED-WMEDAREA                         
035300                 MOVE MED-MFSFEL      TO MOD-TEMFSFEL                     
035400             END-IF                                                       
035500             IF W-IDDISTR > ZERO                                          
035600                MOVE W-IDDISTR TO DIST79-IDDISTR                          
035700                IF DIST79-DEALER-PRICE                                    
035800             MOVE 'DDI/DN USE 3305 FOR NET PRICE' TO MOD-TEMFSFEL         
035900                END-IF                                                    
036000             END-IF                                                       
036100         END-IF                                                           
036200         COMPUTE MSG-KVLL = LENGTH OF MOD-W90429O1 + 4                    
036300         PERFORM IMS-INSERT-MSG                                           
036400     END-IF                                                               
036500     MOVE ZERO TO RETURN-CODE                                             
036600     GOBACK                                                               
036700     .                                                                    
036800     EJECT                                                                
036900 A-INIT-SPARA-INPUT SECTION.                                              
037000                                                                          
037100     IF MSG-DUBBLA-TRANSKODER                                             
037200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90429I1               
037300         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
037400         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
037500         MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                
037600     ELSE                                                                 
037700         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W90429I1               
037800         MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                
037900         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
038000         MOVE ' '                           TO MFS-KDTRTYP                
038100     END-IF                                                               
038200     SKIP2                                                                
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600 B-KOLLA-NYCKLAR SECTION.                                                 
038700                                                                          
038800     MOVE  ALL '+'          TO MSGI-WMSGINIT                              
038900     MOVE '001'             TO MSGI-KDCALL                                
039000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039100                               MSGI-IDLTERM-USER                          
039200     MOVE '9429'            TO MSGI-IDTRANS                               
039300                                                                          
039400     IF MFS-IDTRANS = '9429'                                              
039500       INSPECT MID-IDARTNR-IN REPLACING ALL SPACE BY ZERO                 
039600       IF MID-IDARTNR-IN NUMERIC                                          
039700          MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                             
039800       END-IF                                                             
039900     ELSE                                                                 
040000       INSPECT MID-IDARTNR-IN REPLACING ALL SPACE BY ZERO                 
040100       IF MID-IDARTNR-IN NUMERIC                                          
040200          MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                             
040300       ELSE                                                               
040400          MOVE ALL '+'           TO MSGI-IDARTNR                          
040500       END-IF                                                             
040600       MOVE SPACE                TO MSGI-IDDISTR                          
040700       MOVE SPACE                TO MSGI-IDPROMR                          
040800     END-IF                                                               
040900                                                                          
041000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041100     MOVE MSGI-IDARTNR TO        IDARTNR-WS                               
041200                                 W-IDARTNR-111                            
041300                                 W-IDARTNR-211-MIN                        
041400                                 W-IDARTNR-211-MAX                        
041500                                 TEST-IDARTNR                             
041600                                 BYT16-IDARTNR                            
041700     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
041800     MOVE MSGI-IDDISTR       TO IDDISTR-WS                                
041900     IF IDDISTR-WS NOT NUMERIC                                            
042000        MOVE SPACE                TO IDDISTR-WS                           
042100     END-IF                                                               
042200     INSPECT IDDISTR-WS REPLACING ALL SPACE BY ZERO                       
042300     MOVE MSGI-IDMARKBO          TO W-IDMARKBO-111                        
042400     MOVE MSGI-IDPROMR           TO W-IDPROMR                             
042500                                    IDPROMR-WS                            
042600     MOVE MSGI-IDDC              TO IDDC-WS                               
042700                                                                          
042800     MOVE LOW-VALUE              TO MSG-AREA                              
042900     MOVE 'W90429O1'             TO MFS-IDMOD                             
043000     MOVE '9429'                 TO MOD-IDTRANS                           
043100     INSPECT IDDC-WS REPLACING LEADING SPACE BY  ZERO.                    
043200                                                                          
043300     MOVE IDARTNR-WS             TO W-IDARTNR                             
043400     MOVE IDDISTR-WS             TO W-IDDISTR                             
043500                                    W-IDDISTR-MIN                         
043600                                    W-IDDISTR-MAX                         
043700                                                                          
043800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
043900         MOVE +1 TO INDX                                                  
044000         MOVE 'S  '              TO MED-IDSKYLT                           
044100     ELSE                                                                 
044200         MOVE +2 TO INDX                                                  
044300         MOVE 'GB '              TO MED-IDSKYLT                           
044400     END-IF                                                               
044500                                                                          
044600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
044700                             MOD-TEMFSINF                                 
044800     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
044900     MOVE DAGENS-DATUM TO W-DASTADAT-211-MAX                              
045000                          W-DASTADAT-212-MAX                              
045100                          W-DASTADAT                                      
045200     MOVE DAGENS-DATUM(5:2)          TO COST-TIMM                         
045300     .                                                                    
045400     EJECT                                                                
045500 C-LAGG-UT-DB2-ARTINFO SECTION.                                           
045600                                                                          
045700     PERFORM CA-NOLLA-MODFALT                                             
045800                                                                          
045900****** RAD AF5  **************************************                    
046000                                                                          
046100     IF FSG-SUARTFSG-PER NOT = 0                                          
046200         CONTINUE                                                         
046300     END-IF                                                               
046400     IF FSG-SUARTFSG-PER NOT = 0                                          
046500         COMPUTE W-RETOTBV ROUNDED =                                      
046600            100 * FSG-SUTOTBV-PER / FSG-SUARTFSG-PER                      
046700                                                                          
046800     END-IF                                                               
046900     MOVE FSG-SULEVANT-PER            TO MOD-SULEVANT-PER                 
047000     EJECT                                                                
047100                                                                          
047200****** RAD AF4  ********************************                          
047300                                                                          
047400     IF FSG-SUARTFSG-FAAR NOT = 0                                         
047500         COMPUTE W-REFSG ROUNDED =                                        
047600         100 * (FSG-SUARTFSG-AAR - FSG-SUARTFSG-FAAR)/                    
047700                            FSG-SUARTFSG-FAAR                             
047800                                                                          
047900     END-IF                                                               
048000     IF FSG-SUARTFSG-AAR NOT = 0                                          
048100         COMPUTE W-RETOTBV ROUNDED =                                      
048200            100 * FSG-SUTOTBV-AAR / FSG-SUARTFSG-AAR                      
048300                                                                          
048400     END-IF                                                               
048500     MOVE FSG-SULEVANT-AAR            TO MOD-SULEVANT-AAR                 
048600     IF FSG-SULEVANT-FAAR NOT = 0                                         
048700         COMPUTE W-RELEVANT ROUNDED =                                     
048800         100 * (FSG-SULEVANT-AAR - FSG-SULEVANT-FAAR)/                    
048900                            FSG-SULEVANT-FAAR                             
049000                                                                          
049100     END-IF                                                               
049200     EJECT                                                                
049300                                                                          
049400****** RAD AF3  *************************************                     
049500                                                                          
049600     IF FSG-SUARTFSG-FAAR NOT = 0                                         
049700         COMPUTE W-RETOTBV ROUNDED =                                      
049800            100 * FSG-SUTOTBV-FAAR / FSG-SUARTFSG-FAAR                    
049900                                                                          
050000     END-IF                                                               
050100     MOVE FSG-SULEVANT-FAAR           TO MOD-SULEVANT-FAAR                
050200     EJECT                                                                
050300                                                                          
050400****** RAD AF2  **************************************                    
050500                                                                          
050600     IF FSG-SUARTFSG-FRAAR NOT = 0                                        
050700         COMPUTE W-REFSG ROUNDED =                                        
050800         100 * (FSG-SUARTFSG-RAAR - FSG-SUARTFSG-FRAAR)/                  
050900                            FSG-SUARTFSG-FRAAR                            
051000                                                                          
051100     END-IF                                                               
051200     IF FSG-SUARTFSG-RAAR NOT = 0                                         
051300         COMPUTE W-RETOTBV ROUNDED =                                      
051400         100 * FSG-SUTOTBV-RAAR / FSG-SUARTFSG-RAAR                       
051500                                                                          
051600     END-IF                                                               
051700     MOVE FSG-SULEVANT-RAAR           TO MOD-SULEVANT-RAAR                
051800     IF FSG-SULEVANT-FRAAR NOT = 0                                        
051900         COMPUTE W-RELEVANT ROUNDED =                                     
052000         100 * (FSG-SULEVANT-RAAR - FSG-SULEVANT-FRAAR)/                  
052100                            FSG-SULEVANT-FRAAR                            
052200                                                                          
052300     END-IF                                                               
052400     EJECT                                                                
052500                                                                          
052600****** RAD AF1  *************************************                     
052700                                                                          
052800     IF FSG-SUARTFSG-FRAAR NOT = 0                                        
052900         COMPUTE W-RETOTBV ROUNDED =                                      
053000         100 * FSG-SUTOTBV-FRAAR / FSG-SUARTFSG-FRAAR                     
053100                                                                          
053200     END-IF                                                               
053300                                                                          
053400****** ÖVRIGA RADER AF2  ****************************                     
053500                                                                          
053600     MOVE FSG-SULEVANT-FRAAR          TO MOD-SULEVANT-FRAAR               
053700     .                                                                    
053800     EJECT                                                                
053900 CA-NOLLA-MODFALT SECTION.                                                
054000                                                                          
054100     MOVE ZERO                  TO                                        
054200                                   MOD-SULEVANT-PER                       
054300                                   MOD-SULEVANT-AAR                       
054400                                   MOD-SULEVANT-FAAR                      
054500                                   MOD-SULEVANT-RAAR                      
054600                                   MOD-SULEVANT-FRAAR                     
054700     .                                                                    
054800     EJECT                                                                
054900 D-LAS-LAGG-UT-DLI-ARTINFO SECTION.                                       
055000     SKIP2                                                                
055100                                                                          
055200     PERFORM IMS-GU-WDGX-DATUM                                            
055300     IF SEGMENT-FINNS                                                     
055400         CONTINUE                                                         
055500     ELSE                                                                 
055600         MOVE ZERO                  TO WDGX-3138-TIUPPDAT                 
055700     END-IF                                                               
055800     PERFORM IMS-GU-WDK601                                                
055900     IF SEGMENT-FINNS                                                     
056000       PERFORM IMS-GNP-WDK611                                             
056100       IF SEGMENT-FINNS                                                   
056200**********************************                                        
056300         MOVE IDARTNR-WS            TO COST-IDARTNR                       
056400         MOVE IDDC-WS               TO COST-IDDC                          
056500         MOVE +0                    TO COST-PRARTBES-MON                  
056600         MOVE +0                    TO COST-PRARTBES-MONLOC               
056700         MOVE +0                    TO COST-PRARTSJK-MON                  
056800         MOVE +0                    TO COST-PRARTSJK-MONLOC               
056900         MOVE SPACE                 TO COST-IDLEVNR                       
057000                                                                          
057100         CALL W335COST USING COST-W335COST COST-WDK6-PCB                  
057200                                           COST-WDK7-PCB                  
057300                                           COST-WDF1-PCB                  
057400                                           COST-9305-PCB                  
057500                                           COST-WDK72-PCB                 
057600                                           COST-WDB6-PCB                  
057800         IF BYT02-RENOV                                                   
057900******KOLL OM RADIO****************'                                      
058000           IF BYT16-RADIO                                                 
058100            ADD +1000                   TO TEST-IDARTNR                   
058200           ELSE                                                           
058300            ADD +6000                   TO TEST-IDARTNR                   
058400           END-IF                                                         
058500***********************************                                       
058600           MOVE TEST-IDARTNR           TO W-IDARTNR-BYT                   
058700           PERFORM IMS-GU-WDK611                                          
058800         END-IF                                                           
058900       END-IF                                                             
059000     END-IF                                                               
059100     .                                                                    
059200     EJECT                                                                
059300 E-PRISINFORMATION SECTION.                                               
059400                                                                          
059500                                                                          
059600     IF IDDISTR-WS > ZERO                                                 
059700       PERFORM EE-HT-PROM-VIA-DISTRIKT                                    
059800     END-IF                                                               
059900                                                                          
060000                                                                          
060100     PERFORM EA-LAES-GRUNDDATA                                            
060200     IF SEGMENT-FINNS                                                     
060300       PERFORM IMS-GU-WDC201                                              
060400       IF SEGMENT-FINNS                                                   
060500         PERFORM IMS-GNP-WDC211                                           
060600         IF SEGMENT-FINNS                                                 
060700           PERFORM EB-FLYTTA-ARTRAB                                       
060800         ELSE                                                             
060900           PERFORM IMS-GNP-WDC212                                         
061000           IF SEGMENT-FINNS                                               
061100             PERFORM EC-FLYTTA-KAMPANJRAB                                 
061200           END-IF                                                         
061300         END-IF                                                           
061400         PERFORM IMS-GNP-WDC213                                           
061500         IF SEGMENT-FINNS                                                 
061600           PERFORM ED-FLYTTA-NORMALRAB                                    
061700         END-IF                                                           
061800       END-IF                                                             
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200 EA-LAES-GRUNDDATA SECTION.                                               
062300     SKIP2                                                                
062400     PERFORM IMS-GU-WDC101                                                
062500     IF SEGMENT-FINNS                                                     
062600       MOVE PRIA-ART-PRARTBTO-MARK TO WS-RETAILPRIS                       
062700       MOVE PRIA-ART-KDARTKAM      TO W-KDARTKAM-212-MIN                  
062800                                      W-KDARTKAM-212-MAX                  
             IF BET-FLARTRAB = 'J'                                              
               MOVE PRIA-ART-KDARTRAB-ALT  TO SPAR-KDARTRAB                     
             ELSE                                                               
               MOVE PRIA-ART-KDARTRAB    TO SPAR-KDARTRAB                       
             END-IF                                                             
063000       IF PRIA-ART-TIUPPDAT > ZERO                                        
063100         MOVE PRIA-ART-TIUPPDAT      TO WS-DATUM                          
063200       END-IF                                                             
063300     ELSE                                                                 
063400       MOVE ZERO                   TO WS-RETAILPRIS                       
063500       MOVE ZERO                   TO W-KDARTKAM-212-MIN                  
063600                                      W-KDARTKAM-212-MAX                  
063700       MOVE ZERO                   TO SPAR-KDARTRAB                       
063800     END-IF                                                               
063900     .                                                                    
064000     EJECT                                                                
064100 EB-FLYTTA-ARTRAB SECTION.                                                
064200     SKIP2                                                                
064300     IF WS-RETAILPRIS NOT = ZERO                                          
064400       COMPUTE WS-RETAILPRIS-DO ROUNDED =                                 
064500              ((100 - PRIB-ART-REARTRAB-DO )                              
064600              * WS-RETAILPRIS) / 100                                      
064700       COMPUTE WS-RETAILPRIS-MO ROUNDED =                                 
064800              ((100 - PRIB-ART-REARTRAB-BULK )                            
064900              * WS-RETAILPRIS) / 100                                      
065000     ELSE                                                                 
065100       MOVE ZERO                 TO WS-RETAILPRIS-DO                      
065200       MOVE ZERO                 TO WS-RETAILPRIS-MO                      
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 EC-FLYTTA-KAMPANJRAB SECTION.                                            
065700     SKIP2                                                                
065800     IF WS-RETAILPRIS NOT = ZERO                                          
065900       COMPUTE WS-RETAILPRIS-DO ROUNDED =                                 
066000               ((100 - PRIB-KAM-REARTRAB-DO )                             
066100               * WS-RETAILPRIS) / 100                                     
066200       COMPUTE WS-RETAILPRIS-MO ROUNDED =                                 
066300               ((100 - PRIB-KAM-REARTRAB-BULK)                            
066400               * WS-RETAILPRIS ) / 100                                    
066500     ELSE                                                                 
066600       MOVE ZERO                 TO WS-RETAILPRIS-DO                      
066700       MOVE ZERO                 TO WS-RETAILPRIS-MO                      
066800     END-IF                                                               
066900     .                                                                    
067000     EJECT                                                                
067100 ED-FLYTTA-NORMALRAB SECTION.                                             
067200     SKIP2                                                                
067300******************************************************************        
067400**************** OM KDARTRAB ÄR NOLL SÅ HAR ARTIKELN INGEN  ******        
067500**************** RABATT UTAN DET ÄR RETAILPRISET SOM GÄLLER ******        
067600******************************************************************        
067700     IF SPAR-KDARTRAB > ZERO                                              
067800       MOVE SPAR-KDARTRAB TO RAB-INDX                                     
067900       IF WS-RETAILPRIS NOT = ZERO                                        
068000         COMPUTE WS-RETAILPRIS-NOR-DO ROUNDED =                           
068100                 ((100 - PRIB-RAB-REARTRAB-DO(RAB-INDX))                  
068200                 * WS-RETAILPRIS ) / 100                                  
068300         COMPUTE WS-RETAILPRIS-NOR-MO ROUNDED =                           
068400                 ((100 - PRIB-RAB-REARTRAB-BULK(RAB-INDX))                
068500                 * WS-RETAILPRIS ) / 100                                  
068600       ELSE                                                               
068700         MOVE ZERO             TO WS-RETAILPRIS-NOR-DO                    
068800         MOVE ZERO             TO WS-RETAILPRIS-NOR-MO                    
068900       END-IF                                                             
069000     END-IF                                                               
069100     .                                                                    
069200     EJECT                                                                
069300 EE-HT-PROM-VIA-DISTRIKT SECTION.                                         
069400     SKIP2                                                                
069500                                                                          
069600     PERFORM IMS-GET-WDB201                                               
069700     IF SEGMENT-FINNS                                                     
069800        MOVE WDB2-GMT-IDPARTNR    TO W-WDB1-IDPARTNR                      
069900        MOVE 57                   TO W-WDB1-IDFTG                         
070000                                                                          
070100        PERFORM IMS-GET-WDB101                                            
070200        IF SEGMENT-FINNS                                                  
070300           MOVE BET-IDPROMR       TO WS-IDPROMR                           
070400                                     W-IDPROMR                            
070500           MOVE WS-MARKBOLAG      TO W-IDMARKBO-111                       
070600        END-IF                                                            
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000* IMS SEKTIONER                                                           
071100                                                                          
071200 IMS-GET-MSG SECTION.                                                     
071300                                                                          
071400     MOVE '  QC' TO GODK-STATUSKODER                                      
071500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
071600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071700     PERFORM IMS-STATUSKONTROLL                                           
071800     .                                                                    
071900     SKIP3                                                                
072000 IMS-INSERT-MSG SECTION.                                                  
072100                                                                          
072200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
072300     MOVE SPACE TO GODK-STATUSKODER                                       
072400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
072500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     EJECT                                                                
072900 IMS-GU-WDC101 SECTION.                                                   
073000     STRING 'WLPRIA01(WDC101KY =' W-WDC101KY-X ')'                        
073100          DELIMITED BY SIZE INTO SSA1                                     
073200     MOVE '  GE' TO GODK-STATUSKODER                                      
073300     CALL CBLTDLI USING GU PRIA-PCB DLI-IO-AREA SSA1                      
073400     MOVE PRIA-STATUS-CODE TO STATUS-WS                                   
073500     PERFORM IMS-STATUSKONTROLL                                           
073600     .                                                                    
073700     SKIP3                                                                
073800 IMS-GU-WDC201 SECTION.                                                   
073900     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
074000          DELIMITED BY SIZE INTO SSA1                                     
074100     MOVE '  GE' TO GODK-STATUSKODER                                      
074200     CALL CBLTDLI USING GU PRIB-PCB DLI-IO-AREA2 SSA1                     
074300     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
074400     PERFORM IMS-STATUSKONTROLL                                           
074500     .                                                                    
074600     SKIP3                                                                
074700 IMS-GNP-WDC211 SECTION.                                                  
074800     STRING 'WLPRIB11(WDC211KY>=' W-WDC211KY-MIN-X                        
074900                    '&WDC211KY<=' W-WDC211KY-MAX-X ')'                    
075000          DELIMITED BY SIZE INTO SSA1                                     
075100     MOVE '  GE' TO GODK-STATUSKODER                                      
075200     CALL CBLTDLI USING GNP PRIB-PCB DLI-IO-AREA2 SSA1                    
075300     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600     EJECT                                                                
075700 IMS-GNP-WDC212 SECTION.                                                  
075800     STRING 'WLPRIB12(WDC212KY>=' W-WDC212KY-MIN-X                        
075900                    '&WDC212KY<=' W-WDC212KY-MAX-X ')'                    
076000          DELIMITED BY SIZE INTO SSA1                                     
076100     MOVE '  GE' TO GODK-STATUSKODER                                      
076200     CALL CBLTDLI USING GNP PRIB-PCB DLI-IO-AREA2 SSA1                    
076300     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
076400     PERFORM IMS-STATUSKONTROLL                                           
076500     .                                                                    
076600     SKIP3                                                                
076700 IMS-GNP-WDC213 SECTION.                                                  
076800     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
076900          DELIMITED BY SIZE INTO SSA1                                     
077000     STRING 'WLPRIB13(DASTADAT<=' W-DASTADAT-X ')'                        
077100          DELIMITED BY SIZE INTO SSA2                                     
077200     MOVE '  GE' TO GODK-STATUSKODER                                      
077300     CALL CBLTDLI USING GNP PRIB-PCB DLI-IO-AREA2 SSA1 SSA2               
077400     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     .                                                                    
077700     EJECT                                                                
077800 IMS-GU-WDGX-DATUM SECTION.                                               
077900                                                                          
078000     STRING 'WLXXCJ01(WDG3KEY  =' W-WDGX3137-KEY ')'                      
078100            DELIMITED BY SIZE INTO SSA1                                   
078200     MOVE 'WLXXCJ11 '        TO SSA2                                      
078300     MOVE '  GE' TO GODK-STATUSKODER                                      
078400     CALL CBLTDLI USING GU WDGX-PCB DLI-IO-AREA SSA1 SSA2                 
078500     MOVE WDGX-STATUS-CODE TO STATUS-WS                                   
078600     PERFORM IMS-STATUSKONTROLL                                           
078700     .                                                                    
078800     EJECT                                                                
078900 IMS-GU-WDK601 SECTION.                                                   
079000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
079100          DELIMITED BY SIZE INTO SSA1                                     
079200     MOVE '  GE' TO GODK-STATUSKODER                                      
079300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
079400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
079500     PERFORM IMS-STATUSKONTROLL                                           
079600     .                                                                    
079700     SKIP3                                                                
079800 IMS-GNP-WDK611 SECTION.                                                  
079900     MOVE 'WLARTC11 ' TO SSA1                                             
080000     MOVE '  GE' TO GODK-STATUSKODER                                      
080100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
080200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
080300     PERFORM IMS-STATUSKONTROLL                                           
080400     .                                                                    
080500     EJECT                                                                
080600 IMS-GU-WDK611 SECTION.                                                   
080700                                                                          
080800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-BYT-X ')'                     
080900            DELIMITED BY SIZE INTO SSA1                                   
081000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
081100     MOVE '  GE' TO GODK-STATUSKODER                                      
081200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
081300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
081400     PERFORM IMS-STATUSKONTROLL                                           
081500     .                                                                    
081600     EJECT                                                                
081700 IMS-GET-WDB101 SECTION.                                                  
081800                                                                          
081900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
082000          DELIMITED BY SIZE INTO SSA1                                     
082100     MOVE '  GE' TO GODK-STATUSKODER                                      
082200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
082300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     SKIP2                                                                
082700 IMS-GET-WDB201    SECTION.                                               
082800                                                                          
082900     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
083000                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
083100             DELIMITED BY SIZE INTO SSA1                                  
083200     MOVE '  GE' TO GODK-STATUSKODER                                      
083300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
083400     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
083500     PERFORM IMS-STATUSKONTROLL                                           
083600     .                                                                    
083700     SKIP3                                                                
083800 IMS-STATUSKONTROLL SECTION.                                              
083900                                                                          
084000     SET STATUS-IX TO 1                                                   
084100     SEARCH GODK-STATUS AT END CALL FELLOG                                
084200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
084300     END-SEARCH                                                           
084400     .                                                                    
084500     EJECT                                                                
084600 DB2-SELECT-FSG2-TAB SECTION.                                             
084700                                                                          
084800     MOVE 000100           TO GODK-SQLCODEKODER                           
084900     EXEC SQL                                                             
085000         SELECT SUARTFSG_PER,       SUARTFSG_AAR,                         
085100                SUARTFSG_RAAR_SPEC, SUARTFSG_RAAR_RAB,                    
085200                SUARTFSG_RAAR_MAN,  SUARTFSG_RAAR_KRE,                    
085300                SUARTFSG_FAAR,      SUARTFSG_RAAR,                        
085400                SUARTFSG_FRAAR,     SULEVANT_PER,                         
085500                SULEVANT_AAR,       SULEVANT_RAAR_SPEC,                   
085600                SULEVANT_RAAR_RAB,  SULEVANT_RAAR_MAN,                    
085700                SULEVANT_RAAR_KRE,  SULEVANT_FAAR,                        
085800                SULEVANT_RAAR,      SULEVANT_FRAAR,                       
085900                SUTOTBV_PER,        SUTOTBV_AAR,                          
086000                SUTOTBV_FAAR,       SUTOTBV_RAAR,                         
086100                SUTOTBV_FRAAR                                             
086200         INTO  :FSG-SUARTFSG-PER,       :FSG-SUARTFSG-AAR,                
086300               :FSG-SUARTFSG-RAAR-SPEC, :FSG-SUARTFSG-RAAR-RAB,           
086400               :FSG-SUARTFSG-RAAR-MAN,  :FSG-SUARTFSG-RAAR-KRE,           
086500               :FSG-SUARTFSG-FAAR,      :FSG-SUARTFSG-RAAR,               
086600               :FSG-SUARTFSG-FRAAR,     :FSG-SULEVANT-PER,                
086700               :FSG-SULEVANT-AAR,       :FSG-SULEVANT-RAAR-SPEC,          
086800               :FSG-SULEVANT-RAAR-RAB,  :FSG-SULEVANT-RAAR-MAN,           
086900               :FSG-SULEVANT-RAAR-KRE,  :FSG-SULEVANT-FAAR,               
087000               :FSG-SULEVANT-RAAR,      :FSG-SULEVANT-FRAAR,              
087100               :FSG-SUTOTBV-PER,        :FSG-SUTOTBV-AAR,                 
087200               :FSG-SUTOTBV-FAAR,       :FSG-SUTOTBV-RAAR,                
087300               :FSG-SUTOTBV-FRAAR                                         
087400         FROM FSG2                                                        
087500         WHERE IDARTNR = :W-IDARTNR                                       
087600     END-EXEC                                                             
087700     MOVE SQLCODE          TO SQLCODE-WS                                  
087800     PERFORM DB2-STATUSKONTROLL                                           
087900     .                                                                    
088000     EJECT                                                                
088100 DB2-SELECT-FSG4-TAB SECTION.                                             
088200                                                                          
088300     MOVE 000100           TO GODK-SQLCODEKODER                           
088400     EXEC SQL                                                             
088500         SELECT SUARTFSG_PER,       SUARTFSG_AAR,                         
088600                SUARTFSG_RAAR_SPEC, SUARTFSG_RAAR_RAB,                    
088700                SUARTFSG_RAAR_MAN,  SUARTFSG_RAAR_KRE,                    
088800                SUARTFSG_FAAR,      SUARTFSG_RAAR,                        
088900                SUARTFSG_FRAAR,     SULEVANT_PER,                         
089000                SULEVANT_AAR,       SULEVANT_RAAR_SPEC,                   
089100                SULEVANT_RAAR_RAB,  SULEVANT_RAAR_MAN,                    
089200                SULEVANT_RAAR_KRE,  SULEVANT_FAAR,                        
089300                SULEVANT_RAAR,      SULEVANT_FRAAR,                       
089400                SUTOTBV_PER,        SUTOTBV_AAR,                          
089500                SUTOTBV_FAAR,       SUTOTBV_RAAR,                         
089600                SUTOTBV_FRAAR                                             
089700         INTO  :FSG-SUARTFSG-PER,       :FSG-SUARTFSG-AAR,                
089800               :FSG-SUARTFSG-RAAR-SPEC, :FSG-SUARTFSG-RAAR-RAB,           
089900               :FSG-SUARTFSG-RAAR-MAN,  :FSG-SUARTFSG-RAAR-KRE,           
090000               :FSG-SUARTFSG-FAAR,      :FSG-SUARTFSG-RAAR,               
090100               :FSG-SUARTFSG-FRAAR,     :FSG-SULEVANT-PER,                
090200               :FSG-SULEVANT-AAR,       :FSG-SULEVANT-RAAR-SPEC,          
090300               :FSG-SULEVANT-RAAR-RAB,  :FSG-SULEVANT-RAAR-MAN,           
090400               :FSG-SULEVANT-RAAR-KRE,  :FSG-SULEVANT-FAAR,               
090500               :FSG-SULEVANT-RAAR,      :FSG-SULEVANT-FRAAR,              
090600               :FSG-SUTOTBV-PER,        :FSG-SUTOTBV-AAR,                 
090700               :FSG-SUTOTBV-FAAR,       :FSG-SUTOTBV-RAAR,                
090800               :FSG-SUTOTBV-FRAAR                                         
090900         FROM FSG4                                                        
091000         WHERE IDARTNR = :W-IDARTNR AND IDDISTR =:W-IDDISTR               
091100     END-EXEC                                                             
091200     MOVE SQLCODE          TO SQLCODE-WS                                  
091300     PERFORM DB2-STATUSKONTROLL                                           
091400     .                                                                    
091500     EJECT                                                                
091600 DB2-SELECT-FSG5-TAB SECTION.                                             
091700                                                                          
091800     MOVE 000100           TO GODK-SQLCODEKODER                           
091900     EXEC SQL                                                             
092000         SELECT SUARTFSG_PER,       SUARTFSG_AAR,                         
092100                SUARTFSG_RAAR_SPEC, SUARTFSG_RAAR_RAB,                    
092200                SUARTFSG_RAAR_MAN,  SUARTFSG_RAAR_KRE,                    
092300                SUARTFSG_FAAR,      SUARTFSG_RAAR,                        
092400                SUARTFSG_FRAAR,     SULEVANT_PER,                         
092500                SULEVANT_AAR,       SULEVANT_RAAR_SPEC,                   
092600                SULEVANT_RAAR_RAB,  SULEVANT_RAAR_MAN,                    
092700                SULEVANT_RAAR_KRE,  SULEVANT_FAAR,                        
092800                SULEVANT_RAAR,      SULEVANT_FRAAR,                       
092900                SUTOTBV_PER,        SUTOTBV_AAR,                          
093000                SUTOTBV_FAAR,       SUTOTBV_RAAR,                         
093100                SUTOTBV_FRAAR                                             
093200         INTO  :FSG-SUARTFSG-PER,       :FSG-SUARTFSG-AAR,                
093300               :FSG-SUARTFSG-RAAR-SPEC, :FSG-SUARTFSG-RAAR-RAB,           
093400               :FSG-SUARTFSG-RAAR-MAN,  :FSG-SUARTFSG-RAAR-KRE,           
093500               :FSG-SUARTFSG-FAAR,      :FSG-SUARTFSG-RAAR,               
093600               :FSG-SUARTFSG-FRAAR,     :FSG-SULEVANT-PER,                
093700               :FSG-SULEVANT-AAR,       :FSG-SULEVANT-RAAR-SPEC,          
093800               :FSG-SULEVANT-RAAR-RAB,  :FSG-SULEVANT-RAAR-MAN,           
093900               :FSG-SULEVANT-RAAR-KRE,  :FSG-SULEVANT-FAAR,               
094000               :FSG-SULEVANT-RAAR,      :FSG-SULEVANT-FRAAR,              
094100               :FSG-SUTOTBV-PER,        :FSG-SUTOTBV-AAR,                 
094200               :FSG-SUTOTBV-FAAR,       :FSG-SUTOTBV-RAAR,                
094300               :FSG-SUTOTBV-FRAAR                                         
094400         FROM FSG5                                                        
094500         WHERE IDARTNR = :W-IDARTNR AND IDPROMR =:W-IDPROMR               
094600     END-EXEC                                                             
094700     MOVE SQLCODE          TO SQLCODE-WS                                  
094800     PERFORM DB2-STATUSKONTROLL                                           
094900     .                                                                    
095000     EJECT                                                                
095100 DB2-STATUSKONTROLL SECTION.                                              
095200                                                                          
095300     SET SQLCODE-IX        TO 1                                           
095400     SEARCH GODK-SQLCODE AT END CALL FELLOG                               
095500       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
095600          CONTINUE                                                        
095700     END-SEARCH                                                           
095800     .                                                                    
