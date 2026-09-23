000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3017900.                                                
000300 AUTHOR.         WEB-ACADEMY / MARKUS A, STEFAN K, KENT J, CONNY E        
000400                            OCH LILLE KJELL A.                            
000500 DATE-WRITTEN.   00/07/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        DETTA ÄR ETT MPP-PROGRAM SOM KOMMUNICERAR MOT EN                 
001000*        JSP-SERVLET ISTÄLLET FÖR MOT MFS.                                
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDP7                                       
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*        PROGRAMMET LÄSER      WDD3                                       
001500*        PROGRAMMET LÄSER      WDK7                                       
001600*        PROGRAMMET UPPDATERAR WDA9                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W30179T                                             
002000*        MID:         W30179I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W30179O1                                            
002400*                                                                         
002500*    CHANGE LOG:                                                          
002600*                                                                         
002700*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002800*      ----------------------------------------------------------         
002900*      14/12/05 - REDDY RAHUL     - ADD REASON FOR SCRAP,                 
003000*                                   INVESTMENT AND ADJSTMENT QTY.         
003100*                                   SHOW HISTORY OF CHANGES MADE.         
003200*                                   E'TRACKER 10237349                    
003300*                                                                         
003400*      15/10/22 - REDDY RAHUL     - SHOW CORE NUMBERS IN NES              
003500*                                   E'TRACKER 10251636                    
003600*                                                                         
003700                                                                          
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(08)   VALUE 'W3017900'.            
004600                                                                          
004700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004900 77  FELTEXT2                    PIC X(80)   VALUE SPACE.                 
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  INDX                        PIC S9(4)   BINARY VALUE ZERO.           
005500 77  MAX-INDX                    PIC S9(4)   BINARY VALUE +250.           
005600 77  TRANS-TID                   PIC 9(9).                                
005700 77  ANTAL-RADER                 PIC 9(5).                                
005800 77  WS-FLUPD                    PIC X       VALUE 'N'.                   
005900 01  WS-KVRAD-TOT                PIC 9(6)    VALUE ZERO.                  
006000 01  WS-IMS-SECTION              PIC X(6).                                
006100                                                                          
006200 01  ALL-SPACE.                                                           
006300     03 FILLER                   PIC X(80)   VALUE SPACE.                 
006400                                                                          
006500 01  WS-IDPTYP.                                                           
006600     03 HEADER-NODETAIL          PIC X       VALUE 'H'.                   
006700     03 HEADER-DETAIL            PIC X       VALUE 'Q'.                   
006800     03 DETAIL-LINE              PIC X       VALUE 'D'.                   
006900                                                                          
007000 01  WS-QTY-TYP.                                                          
007100     03 WS-SCRAP                 PIC X       VALUE 'S'.                   
007200     03 WS-INVESTMENT            PIC X       VALUE 'I'.                   
007300     03 WS-ADJUSTMENT            PIC X       VALUE 'A'.                   
007400                                                                          
007500 01  WS-KDATGNES                 PIC X       VALUE SPACE.                 
007600     88 SCRAP                                VALUE 'S'.                   
007700     88 INVESTMENT                           VALUE 'I'.                   
007800     88 ADJUSTMENT                           VALUE 'A'.                   
007900                                                                          
008000 77  MID-DATA-SW                 PIC X       VALUE 'J'.                   
008100     88 MID-BLANK                            VALUE 'J'.                   
008200     88 MID-NOT-BLANK                        VALUE 'N'.                   
008300                                                                          
008400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008500                                                                          
008600 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
008700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008800     88  INDATA-OK                           VALUE 'J'.                   
008900     88  INDATA-NOT-OK                       VALUE 'N'.                   
009000                                                                          
009100 77  INDATA-FEL-SW               PIC X       VALUE 'N'.                   
009200     88  FEL-FINNS                           VALUE 'J'.                   
009300     88  INGA-FEL                            VALUE 'N'.                   
009400                                                                          
009500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009600     88  NYCKLAR-OK                          VALUE 'J'.                   
009700     88  NYCKLAR-FEL                         VALUE 'N'.                   
009800                                                                          
009900 77  MID-KOLL-SW                 PIC X(6)    VALUE SPACE.                 
010000     88  GODK-MID                            VALUE 'W30179'.              
010100                                                                          
010200 77  SW-NEW-PART                 PIC X       VALUE 'N'.                   
010300     88  NEW-PART                            VALUE 'J'.                   
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE 'ARBNYCKEL'.           
010600 01  WS-NYCKELGRUPP.                                                      
010700     03 WS-NYCKELFAELT           PIC X(10).                               
010800                                                                          
010900 01  FILLER                      PIC X(16)   VALUE 'DATUMFALT'.           
011000 01  DATUM.                                                               
011100     03 DAGENS-DATUM             PIC 9(8)       VALUE ZERO.               
011200     03 DAGENS-DATUM-GRP         REDEFINES DAGENS-DATUM.                  
011300        05 DAGENS-DATUM-DAAA           PIC 9(2).                          
011400        05 DAGENS-DATUM-DAAAMMDD       PIC 9(6).                          
011500     03 DAGENS-DATUM-GRP-2       REDEFINES DAGENS-DATUM.                  
011600        05 DAGENS-DATUM-DAAAMM         PIC 9(6).                          
011700        05 FILLER                      PIC X(2).                          
011800                                                                          
011900 01  FILLER                      PIC X(16)   VALUE 'PERIODFALT'.          
012000 01  PERIOD.                                                              
012100     03 WS-DAAAPP                PIC 9(6).                                
012200     03 WS-DAAAPP-ALFA.                                                   
012300        05 WS-DAAAPP-1-4         PIC 9(4).                                
012400        05 WS-DAAAPP-5-6         PIC X(2).                                
012500                                                                          
012600 01  FILLER                      PIC X(16)   VALUE 'ARBETSFALT'.          
012700 01  ARBETSFALT.                                                          
012800     03 WS-KVANTAL-LS-UPD        PIC S9(7)    VALUE ZERO COMP-3.          
012900     03 WS-KVANTAL-LS-SUM        PIC S9(7)    VALUE ZERO COMP-3.          
013000     03 WS-KVLS-REM              PIC S9(7)    VALUE ZERO COMP-3.          
013100     03 WS-SUSKROT2-REM          PIC S9(9)    VALUE ZERO COMP-3.          
013200     03 WS-SUINVEST-REM          PIC S9(9)    VALUE ZERO COMP-3.          
013300     03 WS-SUINVENT-REM          PIC S9(9)    VALUE ZERO COMP-3.          
013400                                                                          
013500 01  FILLER                      PIC X(16)  VALUE 'PERIOD-TABELL'.        
013600 01  PERIOD-TABLE.                                                        
013700     03 PERIOD-TABLE-LINE          OCCURS 250.                            
013800       05 WS-KVANTAL-SKROT-UPD     PIC S9(7) VALUE ZERO COMP-3.           
013900       05 WS-TESKROT-UPD           PIC X(20) VALUE SPACE.                 
014000       05 WS-KVANTAL-INVEST-UPD    PIC S9(7) VALUE ZERO COMP-3.           
014100       05 WS-KDINVEST-UPD          PIC X(01) VALUE SPACE.                 
014200       05 WS-KVANTAL-INVENT-UPD    PIC S9(7) VALUE ZERO COMP-3.           
014300       05 WS-TEINVENT-UPD          PIC X(20) VALUE SPACE.                 
014400                                                                          
014500                                                                          
014600 01  ERROR-CODES.                                                         
014700     03  NO-ERROR                    PIC X(3) VALUE '000'.                
014800     03  NOT-NUMERIC                 PIC X(3) VALUE '001'.                
014900     03  CORE-NOT-IN-DATABASE        PIC X(3) VALUE '002'.                
015000     03  NEGATIVE-STOCK-BAL          PIC X(3) VALUE '003'.                
015100     03  NEG-STOCK-BAL-TOTAL         PIC X(3) VALUE '004'.                
015200     03  MUST-BE-FILLED-IN           PIC X(3) VALUE '005'.                
015300     03  REMANUFACTURER-NOT-APPROVED PIC X(3) VALUE '011'.                
015400     03  CORE-NOT-APPROVED           PIC X(3) VALUE '012'.                
015500     03  PERIOD-ERROR                PIC X(3) VALUE '013'.                
015600     03  KEY-ERROR                   PIC X(3) VALUE '020'.                
015700     EJECT                                                                
015800                                                                          
015900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016000 01  GENERELLA-SUBPROGRAM.                                                
016100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
016700     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
016800     EJECT                                                                
016900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017000*                                                                         
017100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017200                                                                          
017300*    --- PARAMETRAR TILL ABEND                                            
017400                                                                          
017500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017700                                                                          
017800*01 -COPY WMSGINIT                                                        
017900     EJECT                                                                
018000*    --- PARAMETRAR TILL DATKONV                                          
018100*                                                                         
018200*01  -COPY WDATAREA                                                       
018300     EJECT                                                                
018400                                                                          
018500*    --- PARAMETRAR TILL DECEDIT                                          
018600*                                                                         
018700*01  -COPY WDECAREA                                                       
018800     EJECT                                                                
018900                                                                          
019000*01  -COPY WSECAREA                                                       
019100     EJECT                                                                
019200                                                                          
019300 01  MESSAGE-CODES.                                                       
019400     03  ERR-CORR-HILITE-FIELDS  PIC X(3)    VALUE '001'.                 
019500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
019700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
019800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019900     EJECT                                                                
020000 01  DISTRIKT                    PIC X(24) VALUE                          
020100                                 'DISTRIKTCOPYTEXT'.                      
020200 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
020300*01  FILLER -COPY WWDIS134    -RED TEST-IDDISTR.                          
020400                                                                          
020500 01  BYTESENHETER                PIC X(24) VALUE                          
020600                                 'BYTES-AREA-START'.                      
020700 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
020800                                                                          
020900*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
021000     EJECT                                                                
021100                                                                          
021200*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
021300*                                                                         
021400 01  FILLER                      PIC X(16)   VALUE 'SPARAREA'.            
021500 01  SPAR-AREA.                                                           
021600     03  SPAR-IDTRANS           PIC X(4)    VALUE '3179'.                 
021700     EJECT                                                                
021800 77  WS-IDDISTR-NUM               PIC 9(4)   VALUE ZERO.                  
021900 77  WS-IDARTNR-NUM               PIC 9(9)   VALUE ZERO.                  
022000 77  WS-DAAAPP-START-NUM          PIC 9(6)   VALUE ZERO.                  
022100 77  WS-DAAAPP-END-NUM            PIC 9(6)   VALUE ZERO.                  
022200 77  WS-KVPERIOD                  PIC S9(7)  COMP-3.                      
022300                                                                          
022400 01  FILLER                      PIC X(16)   VALUE 'BLADDRING'.           
022500*    -- BLÄDDRINGSRADNUMMER PÅ FÖRSTA RAD SOM VISAS PÅ WÄBB-SIDAN         
022600*    -- OBS: HAR INGET MED ORDER-RADNUMMER ATT GÖRA!                      
022700 77  WS-IDDIARAD-START            PIC S9(5)  COMP-3 VALUE +1.             
022800                                                                          
022900*    -- AKTUELLT ANTAL IFYLLDA RADER SOM LÄSTS IN FRÅN WEB-SIDAN          
023000 77  WS-KVDIARAD-AKT-IN           PIC S9(5)  COMP-3 VALUE +0.             
023100                                                                          
023200*    -- MAX ANTAL RADER SOM FÅR SKRIVAS UT TILL WÄBB-SIDAN                
023300 77  WS-KVDIARAD-MAX              PIC S9(5)  COMP-3 VALUE +250.           
023400                                                                          
023500*    -- AKTUELLT ANTAL RADER SOM SKRIVS UT PÅ WEB-SIDAN                   
023600 77  WS-KVDIARAD-AKT-UT           PIC S9(5)  COMP-3 VALUE +0.             
023700                                                                          
023800*    -- SENASTE UNIKA ORDER                                               
023900 77  SPAR-IDORDER                 PIC S9(7)  COMP-3 VALUE ZERO.           
024000*                                                                         
024100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024200*                                                                         
024300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024400                                                                          
024500 01  MID-AREA.                                                            
024600*    03 -COPY WMIDPREF                                                    
024700*    03 -COPY W30179I1                                                    
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025000                                                                          
025100 01  MOD-AREA.                                                            
025200*    03  -COPY WMODPREF                                                   
025300*    03  -COPY W30179O1                                                   
025400     EJECT                                                                
025500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025600                                                                          
025700 01  MFS-IDMOD                   PIC X(8).                                
025800     EJECT                                                                
025900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026000*                                                                         
026100 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
026200                                                                          
026300 01  NYCKLAR-TILL-DLI.                                                    
026400     03  W-IDARTNR-X.                                                     
026500         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
026600                                                                          
026700     03  W-IDSKYLT-X.                                                     
026800         05  W-IDSKYLT           PIC XXX    VALUE 'GB '.                  
026900                                                                          
027000     03  W-WDD3-IDARTNR-X.                                                
027100         05  W-WDD3-IDARTNR      PIC S9(9)  VALUE ZERO COMP-3.            
027200                                                                          
027300     03  W-WDA9-IDARTNR-X.                                                
027400         05 W-WDA9-IDARTNR       PIC S9(9)  VALUE ZERO  COMP-3.           
027500     03  W-WDA9-IDDISTR-X.                                                
027600         05 W-WDA9-IDDISTR       PIC S9(5)  VALUE ZERO  COMP-3.           
027700     03  W-WDA9-DAAAPP-X.                                                 
027800         05  W-WDA9-DAAAPP       PIC  9(6)  VALUE ZERO.                   
027900                                                                          
028000     03  W-WDA931KY-X.                                                    
028100         05  W-TIREGDAT          PIC S9(7)  VALUE ZERO COMP-3.            
028200         05  W-KDATGNES          PIC X(1)   VALUE SPACE.                  
028300         05  W-TEATGNES          PIC X(20)  VALUE SPACE.                  
028400                                                                          
028500     03  W-IDARTNR-K7-X.                                                  
028600         05  W-IDARTNR-K7        PIC S9(9)  VALUE ZERO COMP-3.            
028700                                                                          
028800     03  W-IDDC-K7-X.                                                     
028900         05  W-IDDC-K7           PIC X(2)   VALUE SPACE.                  
029000                                                                          
029100*    --- STATUS-KOD FRÅN IMS                                              
029200 01  STATUS-WS                   PIC XX.                                  
029300     88  SEGMENT-FINNS                       VALUE '  '.                  
029400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029600                                                                          
029700 01  GODK-STATUSKODER.                                                    
029800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029900                                                                          
030000 01  SSA1                        PIC X(64).                               
030100 01  SSA2                        PIC X(64).                               
030200 01  SSA3                        PIC X(64).                               
030300 01  SSA4                        PIC X(64).                               
030400     EJECT                                                                
030500*    --- IMS FUNKTIONSKODER                                               
030600*01  -COPY W0003                                                          
030700     EJECT                                                                
030800*    ---  DLI INPUT-OUTPUT AREA                                           
030900                                                                          
031000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
031100 01  DLI-IO-WDK601.                                                       
031200*    03  -COPY WDK601                                                     
031300     EJECT                                                                
031400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
031500 01  DLI-IO-WDD311.                                                       
031600*    03  -COPY WDD311                                                     
031700     EJECT                                                                
031800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
031900 01  DLI-IO-WDA901.                                                       
032000*    03  -COPY WDA901                                                     
032100     EJECT                                                                
032200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
032300 01  DLI-IO-WDA911.                                                       
032400*    03  -COPY WDA911                                                     
032500     EJECT                                                                
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA921'.                      
032700 01  DLI-IO-WDA921.                                                       
032800*    03  -COPY WDA921                                                     
032900     EJECT                                                                
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA931'.                      
033100 01  DLI-IO-WDA931.                                                       
033200*    03  -COPY WDA931                                                     
033300     EJECT                                                                
033400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
033500 01  DLI-IO-WDK701.                                                       
033600*    03  -COPY WDK701                                                     
033700     EJECT                                                                
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
033900 01  DLI-IO-WDK711.                                                       
034000*    03  -COPY WDK711                                                     
034100     EJECT                                                                
034200 LINKAGE SECTION.                                                         
034300*01  -COPY W0009   -PRE MSG-                                              
034400*01  -COPY W0008   -PRE USEA-                                             
034500     05  FILLER                  PIC X.                                   
034600                                                                          
034700*01  -COPY W0008  -PRE WDK6-                                              
034800     05  FILLER                  PIC X.                                   
034900                                                                          
035000*01  -COPY W0008  -PRE WDD3-                                              
035100     05  FILLER                  PIC X.                                   
035200                                                                          
035300*01  -COPY W0008  -PRE WDA9-                                              
035400     05  FILLER                  PIC X.                                   
035500                                                                          
035600*01  -COPY W0008  -PRE WDK7-                                              
035700     05  FILLER                  PIC X.                                   
035800     EJECT                                                                
035900                                                                          
036000 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
036100                           WDK6-PCB WDD3-PCB WDA9-PCB WDK7-PCB.           
036200 MAIN SECTION.                                                            
036300     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
036400                           WDK6-PCB WDD3-PCB WDA9-PCB WDK7-PCB.           
036500                                                                          
036600     PERFORM IMS-GET-MSG                                                  
036700     IF SEGMENT-FINNS                                                     
036800       PERFORM A-INIT                                                     
036900       PERFORM B-KOLLA-NYCKLAR                                            
037000       IF NYCKLAR-OK                                                      
037100         IF MID-KDPGMACT = 'U'                                            
037200           PERFORM G-KOLLA-INPUT                                          
037300           IF INDATA-OK                                                   
037400             PERFORM H-UPPDATERA                                          
037500           END-IF                                                         
037600         ELSE                                                             
037700           EVALUATE TRUE                                                  
037800             WHEN MID-KDPGMACT = 'Q' OR 'T'                               
037900               PERFORM C-FOERSTA-SIDA                                     
038000             WHEN MID-KDPGMACT = 'P'                                      
038100               PERFORM D-FOREGANDE-SIDA                                   
038200             WHEN OTHER                                                   
038300               CONTINUE                                                   
038400           END-EVALUATE                                                   
038500         END-IF                                                           
038600         PERFORM F-LAES-VISA-INFO                                         
038700       END-IF                                                             
038800                                                                          
038900       COMPUTE MOD-KVLL =                                                 
039000         LENGTH OF MOD-MODPREF + LENGTH OF MOD-W30179O1 + 4               
039100       END-COMPUTE                                                        
039200       PERFORM IMS-INSERT-MSG                                             
039300     END-IF                                                               
039400                                                                          
039500     MOVE ZERO TO RETURN-CODE                                             
039600     GOBACK                                                               
039700     .                                                                    
039800     EJECT                                                                
039900                                                                          
040000 A-INIT SECTION.                                                          
040100     MOVE 'W30179O1'     TO MFS-IDMOD                                     
040200     INITIALIZE MOD-W30179O1                                              
040300                MOD-MODPREF                                               
040400     MOVE MID-KDTRANS    TO MOD-KDTRANS                                   
040500     MOVE MID-KDPGMACT   TO MOD-KDPGMACT                                  
040600     MOVE MID-KDDIASTATE TO MOD-KDDIASTATE                                
040700                                                                          
040800     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041200 B-KOLLA-NYCKLAR SECTION.                                                 
041300     MOVE ALL '+'             TO MSGI-WMSGINIT                            
041400     MOVE '001'               TO MSGI-KDCALL                              
041500     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
041600     MOVE MID-IDUSER          TO MSGI-IDUSER                              
041700     MOVE '3179'              TO MSGI-IDTRANS                             
041800     MOVE MID-IDDISTR         TO MSGI-IDDISTR                             
041900                                                                          
042000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042100                                                                          
042200     MOVE JA                  TO NYCKLAR-SW                               
042300     MOVE NO-ERROR            TO MOD-KDFEL-KEYS                           
042400*                                                                         
042500*    -- KONTROLL AV DISTRIKT                                              
042600*                                                                         
042700     MOVE MSGI-IDDISTR        TO WS-NYCKELFAELT                           
042800     MOVE WS-NYCKELGRUPP      TO MOD-IDDISTR                              
042900                                                                          
043000     IF MSGI-IDDISTR NOT = ALL '+' AND                                    
043100        MSGI-IDDISTR NOT = ALL '0'                                        
043200       IF MSGI-IDDISTR NUMERIC                                            
043300         MOVE MSGI-IDDISTR    TO TEST-IDDISTR                             
043400         IF DIS134-BYTESRENOV                                             
043500           MOVE MSGI-IDDISTR  TO WS-IDDISTR-NUM                           
043600                                 MID-IDDISTR                              
043700         ELSE                                                             
043800           MOVE '109 REMANUFACTURER IS INVALID'                           
043900                              TO MOD-TEWEBERR                             
044000           MOVE NEJ           TO NYCKLAR-SW                               
044100         END-IF                                                           
044200       ELSE                                                               
044300         MOVE '110 REMANUFACTURER MUST BE NUMERIC'                        
044400                              TO MOD-TEWEBERR                             
044500         MOVE NEJ             TO NYCKLAR-SW                               
044600       END-IF                                                             
044700     ELSE                                                                 
044800       MOVE '111 REMANUFACTURER MUST BE ENTERED'                          
044900                              TO MOD-TEWEBERR                             
045000       MOVE NEJ               TO NYCKLAR-SW                               
045100     END-IF                                                               
045200*                                                                         
045300*    -- KONTROLLERA OM PERSON                                             
045400*    -- 1. FÅR ANVÄNDA BILDEN                                             
045500*    -- 2. ARBETAR MED "RÄTT" DISTRIKT                                    
045600*                                                                         
045700     IF NYCKLAR-OK                                                        
045800       MOVE MSGI-IDUSER      TO SEC-IDUSER                                
045900       MOVE '3179'           TO SEC-IDTRANS                               
046000       MOVE WS-IDDISTR-NUM   TO SEC-IDKEY                                 
046100       CALL WSECURIT USING      SEC-IDUSER                                
046200                                SEC-IDTRANS                               
046300                                SEC-IDKEY                                 
046400                                SEC-KDSVAR                                
046500       IF SEC-KDSVAR = 'F'                                                
046600         MOVE                                                             
046700           '114 YOUR ID IS NOT AUTHORIZED FOR THIS REMANUFACTURER'        
046800                             TO MOD-TEWEBERR                              
046900         MOVE NEJ            TO NYCKLAR-SW                                
047000       END-IF                                                             
047100     END-IF                                                               
047200*                                                                         
047300*    -- KONTROLL AV ARTIKELNUMMER                                         
047400*                                                                         
047500     IF NYCKLAR-OK                                                        
047600       IF MID-IDARTNR NOT = ALL '+'                                       
047700         IF MID-IDARTNR NUMERIC                                           
047800           MOVE MID-IDARTNR         TO WS-IDARTNR-NUM                     
047900                                       TEST-IDARTNR                       
048000           IF BYT03-OBJEKT                                                
048100             MOVE WS-IDARTNR-NUM    TO W-IDARTNR                          
048200                                       W-IDARTNR-K7                       
048300             PERFORM IMS-GU-WDK601                                        
048400             IF SEGMENT-SAKNAS                                            
048500               MOVE '025 PART NOT FOUND'                                  
048600                                    TO MOD-TEWEBERR                       
048700               MOVE NEJ             TO NYCKLAR-SW                         
048800             END-IF                                                       
048900           ELSE                                                           
049000             MOVE '023 PART NO IS INVALID'                                
049100                                    TO MOD-TEWEBERR                       
049200             MOVE NEJ               TO NYCKLAR-SW                         
049300           END-IF                                                         
049400         ELSE                                                             
049500           MOVE '024 PART NO MUST BE NUMERIC'                             
049600                                    TO MOD-TEWEBERR                       
049700           MOVE NEJ                 TO NYCKLAR-SW                         
049800         END-IF                                                           
049900       ELSE                                                               
050000         MOVE '026 PART NO MUST BE ENTERED'                               
050100                                    TO MOD-TEWEBERR                       
050200         MOVE NEJ                   TO NYCKLAR-SW                         
050300       END-IF                                                             
050400     END-IF                                                               
050500*                                                                         
050600*    -- KONTROLL AV PERIOD                                                
050700*                                                                         
050800     IF NYCKLAR-OK                                                        
050900       IF MID-DAAAPP-START NOT = ALL '+'                                  
051000         IF MID-DAAAPP-START NUMERIC                                      
051100           MOVE MID-DAAAPP-START  TO WS-DAAAPP-START-NUM                  
051200           COMPUTE WS-DAAAPP-START-NUM = WS-DAAAPP-START-NUM +            
051300                                         200000                           
051400           END-COMPUTE                                                    
051500           IF WS-DAAAPP-START-NUM > DAGENS-DATUM-DAAAMM                   
051600             MOVE '114 STARTING PERIOD GREATER THAN ACTUAL PERIOD'        
051700                                  TO MOD-TEWEBERR                         
051800             MOVE NEJ             TO NYCKLAR-SW                           
051900           END-IF                                                         
052000         ELSE                                                             
052100           MOVE '024 STARTING PERIOD MUST BE NUMERIC'                     
052200                                  TO MOD-TEWEBERR                         
052300           MOVE NEJ               TO NYCKLAR-SW                           
052400         END-IF                                                           
052500       ELSE                                                               
052600         MOVE '026 STARTING PERIOD MUST BE ENTERED'                       
052700                                  TO MOD-TEWEBERR                         
052800         MOVE NEJ                 TO NYCKLAR-SW                           
052900       END-IF                                                             
053000     END-IF                                                               
053100                                                                          
053200     IF NYCKLAR-OK                                                        
053300       IF MID-DAAAPP-END NOT = ALL '+'                                    
053400         IF MID-DAAAPP-END NUMERIC                                        
053500           MOVE MID-DAAAPP-END    TO WS-DAAAPP-END-NUM                    
053600           COMPUTE WS-DAAAPP-END-NUM = WS-DAAAPP-END-NUM +                
053700                                       200000                             
053800           END-COMPUTE                                                    
053900           IF WS-DAAAPP-END-NUM > DAGENS-DATUM-DAAAMM                     
054000             MOVE '115 ENDING PERIOD GREATER THAN ACTUAL PERIOD'          
054100                                  TO MOD-TEWEBERR                         
054200             MOVE NEJ             TO NYCKLAR-SW                           
054300           END-IF                                                         
054400         ELSE                                                             
054500           MOVE '024 ENDING PERIOD MUST BE NUMERIC'                       
054600                                  TO MOD-TEWEBERR                         
054700           MOVE NEJ               TO NYCKLAR-SW                           
054800         END-IF                                                           
054900       ELSE                                                               
055000         MOVE WS-DAAAPP-START-NUM TO WS-DAAAPP-END-NUM                    
055100       END-IF                                                             
055200     END-IF                                                               
055300                                                                          
055400     IF NYCKLAR-OK                                                        
055500       MOVE 'AAPP'                TO DAT-KDDATFORM                        
055600       MOVE WS-DAAAPP-START-NUM   TO DAT-I-TIDATUM                        
055700                                                                          
055800       CALL WDATKONV USING DAT-KDDATFORM                                  
055900                           DAT-I-TIDATUM                                  
056000                           DAT-O-TIDATUM                                  
056100                           DAT-KDSVAR                                     
056200                                                                          
056300       IF DAT-KDSVAR NOT = ' '                                            
056400         MOVE '023 STARTING PERIOD IS INVALID'                            
056500                                  TO MOD-TEWEBERR                         
056600         MOVE NEJ                 TO NYCKLAR-SW                           
056700       END-IF                                                             
056800     END-IF                                                               
056900                                                                          
057000     IF NYCKLAR-OK                                                        
057100       MOVE 'AAPP'               TO DAT-KDDATFORM                         
057200       MOVE WS-DAAAPP-END-NUM    TO DAT-I-TIDATUM                         
057300                                                                          
057400       CALL WDATKONV USING DAT-KDDATFORM                                  
057500                           DAT-I-TIDATUM                                  
057600                           DAT-O-TIDATUM                                  
057700                           DAT-KDSVAR                                     
057800                                                                          
057900       IF DAT-KDSVAR NOT = ' '                                            
058000         MOVE '023 ENDING PERIOD IS INVALID'                              
058100                                 TO MOD-TEWEBERR                          
058200         MOVE NEJ                TO NYCKLAR-SW                            
058300       END-IF                                                             
058400     END-IF                                                               
058500                                                                          
058600     IF NYCKLAR-OK                                                        
058700       IF WS-DAAAPP-START-NUM > WS-DAAAPP-END-NUM                         
058800         MOVE '101 STARTING PERIOD GREATER THAN ENDING PERIOD'            
058900                                          TO MOD-TEWEBERR                 
059000         MOVE NEJ                         TO NYCKLAR-SW                   
059100       END-IF                                                             
059200     END-IF                                                               
059300                                                                          
059400     IF NYCKLAR-OK                                                        
059500       COMPUTE WS-KVPERIOD =  WS-DAAAPP-END-NUM -                         
059600                              WS-DAAAPP-START-NUM                         
059700       END-COMPUTE                                                        
059800* MAX 25 PERIODER FÅR ANGES                                               
059900       IF WS-KVPERIOD > 200                                               
060000         MOVE '102 TOO MANY PERIODS (MAX 25)'                             
060100                                          TO MOD-TEWEBERR                 
060200         MOVE NEJ                         TO NYCKLAR-SW                   
060300       END-IF                                                             
060400     END-IF                                                               
060500*                                                                         
060600*    -- LAGRING AV RAD-STARTVÄRDEN                                        
060700*                                                                         
060800     IF NYCKLAR-OK                                                        
060900       IF MID-IDDIARAD NOT = ALL '+'                                      
061000         IF MID-IDDIARAD NUMERIC                                          
061100           MOVE MID-IDDIARAD TO WS-IDDIARAD-START                         
061200                                                                          
061300         ELSE                                                             
061400*          -- STARTA FRÅN BÖRJAN OM INGET ANNAT SÄGS                      
061500           MOVE 1 TO WS-IDDIARAD-START                                    
061600         END-IF                                                           
061700       ELSE                                                               
061800         CONTINUE                                                         
061900       END-IF                                                             
062000     END-IF                                                               
062100                                                                          
062200     IF NYCKLAR-FEL                                                       
062300       MOVE JA        TO MOD-FLFEL                                        
062400       MOVE KEY-ERROR TO MOD-KDFEL-KEYS                                   
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800                                                                          
062900 C-FOERSTA-SIDA SECTION.                                                  
063000     MOVE 1   TO WS-IDDIARAD-START                                        
063100     MOVE NEJ TO MOD-FLFEL                                                
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063500 D-FOREGANDE-SIDA SECTION.                                                
063600     SUBTRACT WS-KVDIARAD-MAX FROM WS-IDDIARAD-START                      
063700     IF WS-IDDIARAD-START < ZERO                                          
063800       MOVE 1 TO WS-IDDIARAD-START                                        
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200                                                                          
064300 F-LAES-VISA-INFO SECTION.                                                
064400     MOVE WS-IDARTNR-NUM      TO W-WDD3-IDARTNR                           
064500     PERFORM IMS-GU-WDD311                                                
064600     IF SEGMENT-FINNS                                                     
064700       MOVE TEXT-BEART        TO MOD-BEART                                
064800     ELSE                                                                 
064900       MOVE '********'        TO MOD-BEART                                
065000     END-IF                                                               
065100                                                                          
065200     MOVE WS-IDARTNR-NUM      TO W-WDA9-IDARTNR                           
065300     MOVE WS-IDDISTR-NUM      TO W-WDA9-IDDISTR                           
065400     PERFORM IMS-GU-WDA911                                                
065500                                                                          
065600     IF SEGMENT-SAKNAS                                                    
065700** FIX 061023 DÅ RENOVÖRER GÅR IN OCH TITTAR PÅ ARTIKLAR SOM EJ ÄR        
065800** /EÖ        UPPLAGDA PÅ DERAS DISTRIKT.                                 
065900       MOVE JA                TO SW-NEW-PART                              
066000       MOVE '116 PARTNO NOT FOR THIS REMANUFACTURER'                      
066100                              TO MOD-TEWEBERR                             
066200       MOVE ZERO              TO MOD-KVLS-REM                             
066300** FIX SLUT /NEDAN FELMEDDELANDE INNAN FIX.                               
066400*      MOVE '103 NEW PART FOR THIS REMANUFACTURER'                        
066500*                             TO MOD-TEWEBINF                             
066600*      MOVE ZERO              TO MOD-KVLS-REM                             
066700     ELSE                                                                 
066800       MOVE UPD-KVLS-REM      TO MOD-KVLS-REM                             
066900     END-IF                                                               
067000                                                                          
067100     IF NOT NEW-PART                                                      
067200       MOVE MSGI-IDDC      TO W-IDDC-K7                                   
067300                                                                          
067400       PERFORM IMS-GU-WDK711                                              
067500       IF SEGMENT-FINNS                                                   
067600         MOVE SLAG-KVLS    TO MOD-KVLS-91                                 
067700       ELSE                                                               
067800         MOVE ZERO         TO MOD-KVLS-91                                 
067900       END-IF                                                             
068000                                                                          
068100       PERFORM FA-LAES-RADDATA                                            
068200                                                                          
068300     END-IF                                                               
068400                                                                          
068500     IF MID-KDPGMACT = "U" AND MOD-FLFEL = JA                             
068600       CONTINUE                                                           
068700     ELSE                                                                 
068800       MOVE 1 TO INDX                                                     
068900       PERFORM UNTIL INDX > MAX-INDX                                      
069000         MOVE NO-ERROR          TO MOD-KDFEL-SCRAP        (INDX)          
069100                                   MOD-KDFEL-INVEST       (INDX)          
069200                                   MOD-KDFEL-INVENT       (INDX)          
069300                                   MOD-KDFEL-TESKROT-UPD  (INDX)          
069400                                   MOD-KDFEL-KDINVEST-UPD (INDX)          
069500                                   MOD-KDFEL-TEINVENT-UPD (INDX)          
069600         ADD 1 TO INDX                                                    
069700       END-PERFORM                                                        
069800     END-IF                                                               
069900                                                                          
070000     IF MID-IDDISTR NOT = ALL '+'                                         
070100       MOVE MID-IDDISTR         TO MSGI-IDDISTR                           
070200     END-IF                                                               
070300     MOVE '002'                 TO MSGI-KDCALL                            
070400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
070500     .                                                                    
070600     EJECT                                                                
070700                                                                          
070800 FA-LAES-RADDATA SECTION.                                                 
070900     MOVE ZERO                     TO WS-KVRAD-TOT                        
071000     MOVE WS-DAAAPP-START-NUM      TO W-WDA9-DAAAPP                       
071100     PERFORM IMS-GU-WDA921                                                
071200                                                                          
071300*    -- DAX ATT BÖRJA FYLLA I MODEN                                       
071400     MOVE 1 TO INDX                                                       
071500     PERFORM UNTIL INDX > WS-KVDIARAD-MAX OR                              
071600                   W-WDA9-DAAAPP > WS-DAAAPP-END-NUM                      
071700       IF SEGMENT-FINNS                                                   
071800         PERFORM IMS-GNP-WDA931                                           
071900         IF SEGMENT-FINNS                                                 
072000           MOVE HEADER-DETAIL     TO MOD-IDPTYP      (INDX)               
072100           MOVE UPP-DAAAPP        TO MOD-DAAAPP      (INDX)               
072200           MOVE ZERO              TO MOD-TIREGDAT    (INDX)               
072300           MOVE UPP-SUSKROT2-REM  TO MOD-SUSKROT2-REM(INDX)               
072400           MOVE UPP-SUINVEST-REM  TO MOD-SUINVEST-REM(INDX)               
072500           MOVE UPP-SUINVENT-REM  TO MOD-SUINVENT-REM(INDX)               
072600           COMPUTE INDX = INDX + 1                                        
072700           PERFORM FAA-READ-DETAIL-LINES                                  
072800         ELSE                                                             
072900           MOVE HEADER-NODETAIL   TO MOD-IDPTYP      (INDX)               
073000           MOVE UPP-DAAAPP        TO MOD-DAAAPP      (INDX)               
073100           MOVE ZERO              TO MOD-TIREGDAT    (INDX)               
073200           MOVE UPP-SUSKROT2-REM  TO MOD-SUSKROT2-REM(INDX)               
073300           MOVE UPP-SUINVEST-REM  TO MOD-SUINVEST-REM(INDX)               
073400           MOVE UPP-SUINVENT-REM  TO MOD-SUINVENT-REM(INDX)               
073500           COMPUTE INDX = INDX + 1                                        
073600         END-IF                                                           
073700       ELSE                                                               
073800         MOVE HEADER-NODETAIL   TO MOD-IDPTYP      (INDX)                 
073900         MOVE W-WDA9-DAAAPP     TO MOD-DAAAPP      (INDX)                 
074000         MOVE ZERO              TO MOD-TIREGDAT    (INDX)                 
074100         MOVE ZERO              TO MOD-SUSKROT2-REM(INDX)                 
074200                                   MOD-SUINVEST-REM(INDX)                 
074300                                   MOD-SUINVENT-REM(INDX)                 
074400         COMPUTE INDX = INDX + 1                                          
074500       END-IF                                                             
074600                                                                          
074700       ADD 1 TO W-WDA9-DAAAPP                                             
074800       MOVE W-WDA9-DAAAPP       TO WS-DAAAPP                              
074900       MOVE WS-DAAAPP           TO WS-DAAAPP-ALFA                         
075000       IF WS-DAAAPP-5-6 > '12'                                            
075100         MOVE '01'              TO WS-DAAAPP-5-6                          
075200         ADD  1                 TO WS-DAAAPP-1-4                          
075300         MOVE WS-DAAAPP-ALFA    TO WS-DAAAPP                              
075400         MOVE WS-DAAAPP         TO W-WDA9-DAAAPP                          
075500       END-IF                                                             
075600                                                                          
075700       PERFORM IMS-GU-WDA921                                              
075800     END-PERFORM                                                          
075900                                                                          
076000     COMPUTE WS-KVDIARAD-AKT-UT  = INDX - 1                               
076100     COMPUTE WS-KVRAD-TOT        = INDX - 1                               
076200     MOVE WS-IDDIARAD-START   TO MOD-IDDIARAD-START                       
076300     MOVE WS-KVDIARAD-AKT-UT  TO MOD-KVDIARAD-AKTUELLT                    
076400                                                                          
076500     MOVE WS-KVRAD-TOT        TO MOD-KVRAD-TOT                            
076600     .                                                                    
076700     EJECT                                                                
076800                                                                          
076900 FAA-READ-DETAIL-LINES SECTION.                                           
077000                                                                          
077100     PERFORM                                                              
077200       UNTIL SEGMENT-SAKNAS OR                                            
077300             INDX > WS-KVDIARAD-MAX                                       
077400       MOVE DETAIL-LINE       TO MOD-IDPTYP             (INDX)            
077500       MOVE ZERO              TO MOD-DAAAPP             (INDX)            
077600       MOVE ATG-TIREGDAT      TO MOD-TIREGDAT           (INDX)            
077700       MOVE ATG-KDATGNES      TO WS-KDATGNES                              
077800       MOVE SPACE             TO MOD-TESKROT-UPD        (INDX)            
077900       MOVE ZERO              TO MOD-SUSKROT2-REM       (INDX)            
078000       MOVE SPACE             TO MOD-KDINVEST-UPD       (INDX)            
078100       MOVE ZERO              TO MOD-SUINVEST-REM       (INDX)            
078200       MOVE SPACE             TO MOD-TEINVENT-UPD       (INDX)            
078300       MOVE ZERO              TO MOD-SUINVENT-REM       (INDX)            
078400       EVALUATE TRUE                                                      
078500         WHEN SCRAP                                                       
078600           MOVE ATG-TEATGNES  TO MOD-TESKROT-UPD        (INDX)            
078700           MOVE ATG-KVANTAL   TO MOD-SUSKROT2-REM       (INDX)            
078800         WHEN INVESTMENT                                                  
078900           MOVE ATG-TEATGNES  TO MOD-KDINVEST-UPD       (INDX)            
079000           MOVE ATG-KVANTAL   TO MOD-SUINVEST-REM       (INDX)            
079100         WHEN ADJUSTMENT                                                  
079200           MOVE ATG-TEATGNES  TO MOD-TEINVENT-UPD       (INDX)            
079300           MOVE ATG-KVANTAL   TO MOD-SUINVENT-REM       (INDX)            
079400       END-EVALUATE                                                       
079500       MOVE ZERO              TO                                          
079600                                 MOD-KVANTAL-SKROT-UPD  (INDX)            
079700                                 MOD-KVANTAL-INVEST-UPD (INDX)            
079800                                 MOD-KVANTAL-INVENT-UPD (INDX)            
079900       MOVE SPACE             TO WS-KDATGNES                              
080000       COMPUTE INDX = INDX + 1                                            
080100       PERFORM IMS-GNP-WDA931                                             
080200     END-PERFORM                                                          
080300     .                                                                    
080400     EJECT                                                                
080500                                                                          
080600 G-KOLLA-INPUT SECTION.                                                   
080700     MOVE JA                      TO INDATA-SW                            
080800                                                                          
080900     PERFORM GA-NOLLA-MOD                                                 
081000                                                                          
081100     MOVE +1 TO INDX                                                      
081200     PERFORM UNTIL INDX > MAX-INDX OR MID-NOT-BLANK                       
081300       IF MID-IDPTYP(INDX) = HEADER-DETAIL OR HEADER-NODETAIL             
081400         IF MID-KVANTAL-SKROT-UPD  (INDX) = ALL '+' AND                   
081500            MID-KVANTAL-INVEST-UPD (INDX) = ALL '+' AND                   
081600            MID-KVANTAL-INVENT-UPD (INDX) = ALL '+' AND                   
081700            MID-TESKROT-UPD        (INDX) = ALL '+' AND                   
081800            MID-KDINVEST-UPD       (INDX) = ALL '+' AND                   
081900            MID-TEINVENT-UPD       (INDX) = ALL '+'                       
082000            CONTINUE                                                      
082100          ELSE                                                            
082200           MOVE NEJ TO MID-DATA-SW                                        
082300         END-IF                                                           
082400       END-IF                                                             
082500       ADD +1 TO INDX                                                     
082600     END-PERFORM                                                          
082700                                                                          
082800     IF MID-BLANK                                                         
082900*       UPDATE OCH INGET INDATA                                           
083000       MOVE '004 UPDATE PRESSED, BUT NO DATA ENTERED'                     
083100                                  TO MOD-TEWEBERR                         
083200       MOVE NEJ TO INDATA-SW                                              
083300       MOVE JA TO MOD-FLFEL                                               
083400     ELSE                                                                 
083500       PERFORM GB-FORMELL-KONTROLL                                        
083600       IF INDATA-OK                                                       
083700         PERFORM GC-RELATIONSKONTROLL                                     
083800       END-IF                                                             
083900       IF INDATA-NOT-OK                                                   
084000         MOVE JA                  TO MOD-FLFEL                            
084100       END-IF                                                             
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500                                                                          
084600 GA-NOLLA-MOD SECTION.                                                    
084700     MOVE ZERO             TO WS-KVANTAL-LS-UPD                           
084800     PERFORM                                                              
084900     VARYING INDX FROM +1 BY +1                                           
085000       UNTIL INDX > MAX-INDX                                              
085100        MOVE ZERO          TO WS-KVANTAL-SKROT-UPD   (INDX)               
085200                              WS-KVANTAL-INVEST-UPD  (INDX)               
085300                              WS-KVANTAL-INVENT-UPD  (INDX)               
085400                              MOD-SUSKROT2-REM       (INDX)               
085500                              MOD-SUINVEST-REM       (INDX)               
085600                              MOD-SUINVENT-REM       (INDX)               
085700        MOVE SPACE         TO WS-TESKROT-UPD         (INDX)               
085800                              WS-KDINVEST-UPD        (INDX)               
085900                              WS-TEINVENT-UPD        (INDX)               
086000        MOVE NO-ERROR      TO MOD-KDFEL-SCRAP        (INDX)               
086100                              MOD-KDFEL-INVEST       (INDX)               
086200                              MOD-KDFEL-INVENT       (INDX)               
086300                              MOD-KDFEL-TESKROT-UPD  (INDX)               
086400                              MOD-KDFEL-KDINVEST-UPD (INDX)               
086500                              MOD-KDFEL-TEINVENT-UPD (INDX)               
086600     END-PERFORM                                                          
086700                                                                          
086800     MOVE NEJ              TO MOD-FLFEL                                   
086900     MOVE ZERO             TO MOD-KVLS-REM                                
087000     MOVE SPACE            TO MOD-BEART                                   
087100     .                                                                    
087200     EJECT                                                                
087300                                                                          
087400 GB-FORMELL-KONTROLL SECTION.                                             
087500     MOVE 1 TO INDX                                                       
087600     PERFORM UNTIL INDX > MAX-INDX OR                                     
087700                   MID-DAAAPP(INDX) = ALL '+'                             
087800       IF MID-DAAAPP(INDX) > ZERO AND                                     
087900          MID-IDPTYP(INDX) = HEADER-DETAIL OR HEADER-NODETAIL             
088000         IF MID-KVANTAL-INVENT-UPD(INDX) NOT = ALL '+' OR                 
088100            MID-TEINVENT-UPD      (INDX) NOT = ALL '+'                    
088200*    KOLLA KVANTAL-INVENT                                                 
088300           IF MID-KVANTAL-INVENT-UPD(INDX) NOT = ALL '+'                  
088400             MOVE MID-KVANTAL-INVENT-UPD(INDX)                            
088500                                TO DEC-IDFRIDATA                          
088600             MOVE 7             TO DEC-KVHELTAL                           
088700             MOVE 0             TO DEC-KVDECIMAL                          
088800             CALL WDECEDIT   USING DEC-WDECAREA                           
088900                                                                          
089000             IF DEC-KDSVAR-OK                                             
089100               IF DEC-IDEDITDATA NOT = ZERO                               
089200                 MOVE DEC-IDEDITDATA                                      
089300                                TO WS-KVANTAL-INVENT-UPD(INDX)            
089400                                   MOD-KVANTAL-INVENT-UPD(INDX)           
089500               ELSE                                                       
089600                 MOVE NEJ       TO INDATA-SW                              
089700                 MOVE MUST-BE-FILLED-IN                                   
089800                                TO MOD-KDFEL-INVENT(INDX)                 
089900                 MOVE '005 BOTH ADJUSTMENT QTY AND REASON MUST BE         
090000-                'FILLED IN'    TO MOD-TEWEBERR                           
090100               END-IF                                                     
090200             ELSE                                                         
090300               MOVE NEJ         TO INDATA-SW                              
090400                                                                          
090500               MOVE NOT-NUMERIC TO MOD-KDFEL-INVENT(INDX)                 
090600               MOVE '024 ADJUSTMENT QTY MUST BE NUMERIC'                  
090700                                TO MOD-TEWEBERR                           
090800             END-IF                                                       
090900           ELSE                                                           
091000             MOVE NEJ           TO INDATA-SW                              
091100             MOVE MUST-BE-FILLED-IN                                       
091200                                TO MOD-KDFEL-INVENT(INDX)                 
091300             MOVE '005 BOTH ADJUSTMENT QTY AND REASON MUST BE FILL        
091400-                 'ED IN'       TO MOD-TEWEBERR                           
091500           END-IF                                                         
091600*    REASON FOR UPDATE                                                    
091700           IF MID-TEINVENT-UPD (INDX) = ALL '+'                           
091800             MOVE NEJ           TO INDATA-SW                              
091900             MOVE MUST-BE-FILLED-IN                                       
092000                                TO MOD-KDFEL-TEINVENT-UPD (INDX)          
092100             MOVE '005 BOTH ADJUSTMENT QTY AND REASON MUST BE FILL        
092200-                 'ED IN'       TO MOD-TEWEBERR                           
092300             IF WS-KVANTAL-INVENT-UPD(INDX) < ZERO                        
092400                MOVE ZERO TO  MOD-KVANTAL-INVENT-UPD(INDX)                
092500             END-IF                                                       
092600           ELSE                                                           
092700             MOVE MID-TEINVENT-UPD (INDX)                                 
092800                                TO WS-TEINVENT-UPD (INDX)                 
092900                                   MOD-TEINVENT-UPD(INDX)                 
093000           END-IF                                                         
093100*    CHECK IF UPDATE IS ALREADY DONE FOR SAME REASON TODAY                
093200           IF INDATA-OK                                                   
093300             MOVE WS-ADJUSTMENT TO W-KDATGNES                             
093400             MOVE MID-TEINVENT-UPD (INDX)                                 
093500                                TO W-TEATGNES                             
093600             PERFORM GBA-CHECK-TODAYS-UPDATE                              
093700             IF INDATA-NOT-OK                                             
093800               MOVE 'SAME ADJUSTMENT REASON ALREADY EXIST FOR THIS        
093900-                   ' DATE'     TO MOD-TEWEBERR                           
094000             END-IF                                                       
094100           END-IF                                                         
094200         END-IF                                                           
094300                                                                          
094400         IF MID-KVANTAL-INVEST-UPD(INDX) NOT = ALL '+' OR                 
094500            MID-KDINVEST-UPD      (INDX) NOT = ALL '+'                    
094600*    KOLLA KVANTAL-INVEST                                                 
094700           IF MID-KVANTAL-INVEST-UPD(INDX) NOT = ALL '+'                  
094800             MOVE MID-KVANTAL-INVEST-UPD(INDX)                            
094900                                TO DEC-IDFRIDATA                          
095000             MOVE 7             TO DEC-KVHELTAL                           
095100             MOVE 0             TO DEC-KVDECIMAL                          
095200             CALL WDECEDIT   USING DEC-WDECAREA                           
095300                                                                          
095400             IF DEC-KDSVAR-OK                                             
095500               IF DEC-IDEDITDATA > ZERO                                   
095600                 MOVE DEC-IDEDITDATA                                      
095700                                TO WS-KVANTAL-INVEST-UPD(INDX)            
095800                                   MOD-KVANTAL-INVEST-UPD(INDX)           
095900               ELSE                                                       
096000                 MOVE NEJ       TO INDATA-SW                              
096100                 MOVE MUST-BE-FILLED-IN                                   
096200                                TO MOD-KDFEL-INVEST(INDX)                 
096300                 MOVE '005 BOTH INVESTMENT QTY AND TYPE MUST BE FI        
096400-                'LLED IN'      TO MOD-TEWEBERR                           
096500               END-IF                                                     
096600             ELSE                                                         
096700               MOVE NEJ         TO INDATA-SW                              
096800               MOVE NOT-NUMERIC TO MOD-KDFEL-INVEST(INDX)                 
096900               MOVE '024 INVESTED QTY MUST BE NUMERIC'                    
097000                                TO MOD-TEWEBERR                           
097100             END-IF                                                       
097200           ELSE                                                           
097300             MOVE NEJ           TO INDATA-SW                              
097400             MOVE MUST-BE-FILLED-IN                                       
097500                                TO MOD-KDFEL-INVEST(INDX)                 
097600             MOVE '005 BOTH INVESTMENT QTY AND TYPE MUST BE FILLED        
097700-                 ' IN'         TO MOD-TEWEBERR                           
097800           END-IF                                                         
097900           IF MID-KDINVEST-UPD (INDX) = ALL '+'                           
098000             MOVE NEJ           TO INDATA-SW                              
098100             MOVE MUST-BE-FILLED-IN                                       
098200                                TO MOD-KDFEL-KDINVEST-UPD (INDX)          
098300             MOVE '005 BOTH INVESTMENT QTY AND TYPE MUST BE FILLED        
098400-                 ' IN'         TO MOD-TEWEBERR                           
098500           ELSE                                                           
098600             MOVE MID-KDINVEST-UPD (INDX)                                 
098700                                TO WS-KDINVEST-UPD (INDX)                 
098800                                   MOD-KDINVEST-UPD(INDX)                 
098900           END-IF                                                         
099000*    CHECK IF UPDATE IS ALREADY DONE FOR SAME REASON TODAY                
099100           IF INDATA-OK                                                   
099200             MOVE WS-INVESTMENT TO W-KDATGNES                             
099300             MOVE MID-KDINVEST-UPD (INDX)                                 
099400                                TO W-TEATGNES                             
099500             PERFORM GBA-CHECK-TODAYS-UPDATE                              
099600             IF INDATA-NOT-OK                                             
099700               MOVE 'SAME INVESTMENT TYPE ALREADY EXIST FOR THIS D        
099800-                   'ATE'       TO MOD-TEWEBERR                           
099900             END-IF                                                       
100000           END-IF                                                         
100100         END-IF                                                           
100200                                                                          
100300         IF MID-KVANTAL-SKROT-UPD(INDX) NOT = ALL '+' OR                  
100400            MID-TESKROT-UPD      (INDX) NOT = ALL '+'                     
100500*    KOLLA KVANTAL-SKROT                                                  
100600           IF MID-KVANTAL-SKROT-UPD(INDX) NOT = ALL '+'                   
100700             MOVE MID-KVANTAL-SKROT-UPD(INDX)                             
100800                                TO DEC-IDFRIDATA                          
100900             MOVE 7             TO DEC-KVHELTAL                           
101000             MOVE 0             TO DEC-KVDECIMAL                          
101100             CALL WDECEDIT   USING DEC-WDECAREA                           
101200                                                                          
101300             IF DEC-KDSVAR-OK                                             
101400               IF DEC-IDEDITDATA > ZERO                                   
101500                 MOVE DEC-IDEDITDATA                                      
101600                                TO WS-KVANTAL-SKROT-UPD(INDX)             
101700                                   MOD-KVANTAL-SKROT-UPD(INDX)            
101800               ELSE                                                       
101900                 MOVE NEJ       TO INDATA-SW                              
102000                 MOVE MUST-BE-FILLED-IN                                   
102100                                TO MOD-KDFEL-SCRAP(INDX)                  
102200                 MOVE '005 BOTH SCRAPPING QTY AND REASON MUST BE F        
102300-                'ILLED IN'     TO MOD-TEWEBERR                           
102400               END-IF                                                     
102500             ELSE                                                         
102600               MOVE NEJ         TO INDATA-SW                              
102700               MOVE NOT-NUMERIC TO MOD-KDFEL-SCRAP(INDX)                  
102800               MOVE '024 SCRAPPED QTY MUST BE NUMERIC'                    
102900                                TO MOD-TEWEBERR                           
103000             END-IF                                                       
103100           ELSE                                                           
103200             MOVE NEJ           TO INDATA-SW                              
103300             MOVE MUST-BE-FILLED-IN                                       
103400                                TO MOD-KDFEL-SCRAP(INDX)                  
103500             MOVE '005 BOTH SCRAPPING QTY AND REASON MUST BE FILLE        
103600-                 'D IN'        TO MOD-TEWEBERR                           
103700           END-IF                                                         
103800           IF MID-TESKROT-UPD (INDX) = ALL '+'                            
103900             MOVE NEJ           TO INDATA-SW                              
104000             MOVE MUST-BE-FILLED-IN                                       
104100                                TO MOD-KDFEL-TESKROT-UPD (INDX)           
104200             MOVE '005 BOTH SCRAPPING QTY AND REASON MUST BE FILLE        
104300-                 'D IN'        TO MOD-TEWEBERR                           
104400           ELSE                                                           
104500             MOVE MID-TESKROT-UPD (INDX)                                  
104600                                TO WS-TESKROT-UPD (INDX)                  
104700                                   MOD-TESKROT-UPD(INDX)                  
104800           END-IF                                                         
104900*    CHECK IF UPDATE IS ALREADY DONE FOR SAME REASON TODAY                
105000           IF INDATA-OK                                                   
105100             MOVE WS-SCRAP      TO W-KDATGNES                             
105200             MOVE MID-TESKROT-UPD (INDX)                                  
105300                                TO W-TEATGNES                             
105400             PERFORM GBA-CHECK-TODAYS-UPDATE                              
105500             IF INDATA-NOT-OK                                             
105600              MOVE 'SAME SCRAP REASON ALREADY EXIST FOR THIS DATE'        
105700                                TO MOD-TEWEBERR                           
105800             END-IF                                                       
105900           END-IF                                                         
106000         END-IF                                                           
106100                                                                          
106200*    ÄNDRING FÅR ENDAST SKE I INNEVARANDE PERIOD                          
106300         IF MID-KVANTAL-SKROT-UPD (INDX) NOT = ALL '+' OR                 
106400            MID-TESKROT-UPD       (INDX) NOT = ALL '+' OR                 
106500            MID-KVANTAL-INVEST-UPD(INDX) NOT = ALL '+' OR                 
106600            MID-KDINVEST-UPD      (INDX) NOT = ALL '+' OR                 
106700            MID-KVANTAL-INVENT-UPD(INDX) NOT = ALL '+' OR                 
106800            MID-TEINVENT-UPD      (INDX) NOT = ALL '+'                    
106900           IF DAGENS-DATUM-DAAAMM NOT = MID-DAAAPP(INDX)                  
107000             MOVE NEJ            TO INDATA-SW                             
107100             MOVE '104 UPDATE NOT ALLOWED IN THIS PERIOD'                 
107200                                 TO MOD-TEWEBERR                          
107300             IF MID-KVANTAL-SKROT-UPD(INDX)  NOT = ALL '+'                
107400               MOVE PERIOD-ERROR TO MOD-KDFEL-SCRAP(INDX)                 
107500             END-IF                                                       
107600             IF MID-KVANTAL-INVEST-UPD(INDX)  NOT = ALL '+'               
107700               MOVE PERIOD-ERROR TO MOD-KDFEL-INVEST(INDX)                
107800             END-IF                                                       
107900             IF MID-KVANTAL-INVENT-UPD(INDX)  NOT = ALL '+'               
108000               MOVE PERIOD-ERROR TO MOD-KDFEL-INVENT(INDX)                
108100             END-IF                                                       
108200           END-IF                                                         
108300         END-IF                                                           
108400                                                                          
108500       END-IF                                                             
108600       ADD 1                     TO INDX                                  
108700     END-PERFORM                                                          
108800     .                                                                    
108900     EJECT                                                                
109000                                                                          
109100 GBA-CHECK-TODAYS-UPDATE SECTION.                                         
109200                                                                          
109300     MOVE WS-IDARTNR-NUM         TO W-WDA9-IDARTNR                        
109400     MOVE WS-IDDISTR-NUM         TO W-WDA9-IDDISTR                        
109500     MOVE MID-DAAAPP(INDX)       TO W-WDA9-DAAAPP                         
109600                                                                          
109700     MOVE FUNCTION CURRENT-DATE(3:6)                                      
109800                                 TO W-TIREGDAT                            
109900     PERFORM IMS-GU-WDA931                                                
110000     IF SEGMENT-FINNS                                                     
110100       MOVE NEJ                  TO INDATA-SW                             
110200     END-IF                                                               
110300     .                                                                    
110400     EJECT                                                                
110500                                                                          
110600 GC-RELATIONSKONTROLL SECTION.                                            
110700                                                                          
110800* FÖRHINDRA ATT NEGATIVA SALDON UPPSTÅR                                   
110900                                                                          
111000     MOVE WS-IDARTNR-NUM         TO W-WDA9-IDARTNR                        
111100     MOVE WS-IDDISTR-NUM         TO W-WDA9-IDDISTR                        
111200     PERFORM IMS-GU-WDA911                                                
111300     IF SEGMENT-FINNS                                                     
111400       MOVE UPD-KVLS-REM         TO WS-KVLS-REM                           
111500     ELSE                                                                 
111600       MOVE JA                   TO SW-NEW-PART                           
111700       MOVE ZERO                 TO WS-KVLS-REM                           
111800     END-IF                                                               
111900                                                                          
112000     MOVE 1 TO INDX                                                       
112100     PERFORM UNTIL INDX > MAX-INDX                                        
112200       MOVE ZERO                        TO WS-SUSKROT2-REM                
112300                                           WS-SUINVEST-REM                
112400                                           WS-SUINVENT-REM                
112500       IF WS-KVANTAL-SKROT-UPD(INDX) NOT = ZERO                           
112600       OR WS-KVANTAL-INVEST-UPD(INDX) NOT = ZERO                          
112700       OR WS-KVANTAL-INVENT-UPD(INDX) NOT = ZERO                          
112800         MOVE MID-DAAAPP(INDX)          TO W-WDA9-DAAAPP                  
112900         IF NOT NEW-PART                                                  
113000           PERFORM IMS-GNP-WDA921                                         
113100           IF SEGMENT-FINNS                                               
113200             IF WS-KVANTAL-SKROT-UPD(INDX) NOT = ZERO                     
113300               MOVE UPP-SUSKROT2-REM    TO WS-SUSKROT2-REM                
113400               COMPUTE WS-SUSKROT2-REM =                                  
113500                       WS-SUSKROT2-REM +                                  
113600                       WS-KVANTAL-SKROT-UPD(INDX)                         
113700               END-COMPUTE                                                
113800             END-IF                                                       
113900             IF WS-KVANTAL-INVEST-UPD(INDX) NOT = ZERO                    
114000               MOVE UPP-SUINVEST-REM    TO WS-SUINVEST-REM                
114100               COMPUTE WS-SUINVEST-REM =                                  
114200                       WS-SUINVEST-REM +                                  
114300                       WS-KVANTAL-INVEST-UPD(INDX)                        
114400               END-COMPUTE                                                
114500             END-IF                                                       
114600             IF WS-KVANTAL-INVENT-UPD(INDX) NOT = ZERO                    
114700               MOVE UPP-SUINVENT-REM    TO WS-SUINVENT-REM                
114800               COMPUTE WS-SUINVENT-REM =                                  
114900                       WS-SUINVENT-REM +                                  
115000                       WS-KVANTAL-INVENT-UPD(INDX)                        
115100               END-COMPUTE                                                
115200             END-IF                                                       
115300           ELSE                                                           
115400             IF WS-SUSKROT2-REM < ZERO                                    
115500               MOVE NEJ                 TO INDATA-SW                      
115600               MOVE NEGATIVE-STOCK-BAL  TO MOD-KDFEL-SCRAP(INDX)          
115700               MOVE '105 TOTAL SCRAPPING QTY WILL BE NEGATIVE'            
115800                                        TO MOD-TEWEBERR                   
115900             END-IF                                                       
116000             IF WS-SUINVEST-REM < ZERO                                    
116100               MOVE NEJ                 TO INDATA-SW                      
116200               MOVE NEGATIVE-STOCK-BAL  TO MOD-KDFEL-INVEST(INDX)         
116300               MOVE '106 TOTAL INVESTMENT QTY WILL BE NEGATIVE'           
116400                                        TO MOD-TEWEBERR                   
116500             END-IF                                                       
116600             IF WS-SUINVENT-REM < ZERO                                    
116700               MOVE NEJ                 TO INDATA-SW                      
116800               MOVE NEGATIVE-STOCK-BAL  TO MOD-KDFEL-INVENT(INDX)         
116900               MOVE '107 TOTAL ADJUSTMENT QTY WILL BE NEGATIVE'           
117000                                        TO MOD-TEWEBERR                   
117100             END-IF                                                       
117200           END-IF                                                         
117300         END-IF                                                           
117400       END-IF                                                             
117500       ADD 1 TO INDX                                                      
117600     END-PERFORM                                                          
117700                                                                          
117800*    GER SAMTLIGA FÖRÄNDRINGAR AV SKROTAT/INVESTERAT/INVENTERAT           
117900*    NEGATIVT LAGERSALDO?                                                 
118000                                                                          
118100     IF INDATA-OK                                                         
118200       MOVE 1 TO INDX                                                     
118300       PERFORM UNTIL INDX > MAX-INDX                                      
118400         IF WS-KVANTAL-SKROT-UPD(INDX) NOT = ZERO                         
118500           COMPUTE WS-KVLS-REM =                                          
118600                   WS-KVLS-REM -                                          
118700                   WS-KVANTAL-SKROT-UPD(INDX)                             
118800           END-COMPUTE                                                    
118900         END-IF                                                           
119000         IF WS-KVANTAL-INVEST-UPD(INDX) NOT = ZERO                        
119100           COMPUTE WS-KVLS-REM =                                          
119200                   WS-KVLS-REM +                                          
119300                   WS-KVANTAL-INVEST-UPD(INDX)                            
119400           END-COMPUTE                                                    
119500         END-IF                                                           
119600         IF WS-KVANTAL-INVENT-UPD(INDX) NOT = ZERO                        
119700           COMPUTE WS-KVLS-REM =                                          
119800                   WS-KVLS-REM +                                          
119900                   WS-KVANTAL-INVENT-UPD(INDX)                            
120000           END-COMPUTE                                                    
120100         END-IF                                                           
120200         ADD 1 TO INDX                                                    
120300       END-PERFORM                                                        
120400                                                                          
120500       IF WS-KVLS-REM < ZERO                                              
120600         MOVE NEJ                 TO INDATA-SW                            
120700         MOVE '108 TOTAL STOCK-BALANCE QTY WILL BE NEGATIVE'              
120800                                  TO MOD-TEWEBERR                         
120900       END-IF                                                             
121000     END-IF                                                               
121100     .                                                                    
121200     EJECT                                                                
121300                                                                          
121400 H-UPPDATERA SECTION.                                                     
121500     IF MID-NOT-BLANK                                                     
121600       MOVE WS-IDARTNR-NUM               TO W-WDA9-IDARTNR                
121700       MOVE WS-IDDISTR-NUM               TO W-WDA9-IDDISTR                
121800       PERFORM IMS-GHU-WDA901                                             
121900       IF SEGMENT-SAKNAS                                                  
122000         MOVE W-WDA9-IDARTNR             TO W-IDARTNR                     
122100         PERFORM IMS-GU-WDK601                                            
122200         MOVE ART-IDARTNR                TO UPB-IDARTNR                   
122300         MOVE ART-IDFKNGRP               TO UPB-IDFKNGRP                  
122400         PERFORM IMS-ISRT-WDA901                                          
122500       END-IF                                                             
122600                                                                          
122700       PERFORM IMS-GHU-WDA911                                             
122800       IF SEGMENT-SAKNAS                                                  
122900         PERFORM IMS-GHU-WDA901                                           
123000         MOVE W-WDA9-IDDISTR             TO UPD-IDDISTR                   
123100         MOVE ZERO                       TO UPD-KVLS-REM                  
123200         MOVE MSGI-IDUSER                TO UPD-IDUSER                    
123300         MOVE FUNCTION CURRENT-DATE(1:8) TO UPD-DAREGDAT                  
123400         MOVE FUNCTION CURRENT-DATE(9:7) TO UPD-TIREGTID                  
123500         PERFORM IMS-ISRT-WDA911                                          
123600         PERFORM IMS-GHU-WDA911                                           
123700       END-IF                                                             
123800                                                                          
123900       MOVE 1 TO INDX                                                     
124000       PERFORM UNTIL INDX >  MAX-INDX        OR                           
124100                     MID-DAAAPP(INDX) = ALL '+'                           
124200         IF WS-KVANTAL-SKROT-UPD(INDX)  NOT = ZERO                        
124300         OR WS-KVANTAL-INVEST-UPD(INDX) NOT = ZERO                        
124400         OR WS-KVANTAL-INVENT-UPD(INDX) NOT = ZERO                        
124500           MOVE MID-DAAAPP(INDX)              TO W-WDA9-DAAAPP            
124600           PERFORM IMS-GHNP-WDA921                                        
124700           IF SEGMENT-FINNS                                               
124800             IF WS-KVANTAL-SKROT-UPD(INDX) NOT = ZERO                     
124900               ADD WS-KVANTAL-SKROT-UPD(INDX)  TO UPP-SUSKROT2-REM        
125000               MOVE MSGI-IDUSER                TO UPP-IDUSER              
125100               MOVE FUNCTION CURRENT-DATE(1:8) TO UPP-DAREGDAT            
125200               MOVE FUNCTION CURRENT-DATE(9:7) TO UPP-TIREGTID            
125300             END-IF                                                       
125400             IF WS-KVANTAL-INVEST-UPD(INDX) NOT = ZERO                    
125500               ADD WS-KVANTAL-INVEST-UPD(INDX) TO UPP-SUINVEST-REM        
125600               MOVE MSGI-IDUSER                TO UPP-IDUSER              
125700               MOVE FUNCTION CURRENT-DATE(1:8) TO UPP-DAREGDAT            
125800               MOVE FUNCTION CURRENT-DATE(9:7) TO UPP-TIREGTID            
125900             END-IF                                                       
126000             IF WS-KVANTAL-INVENT-UPD(INDX) NOT = ZERO                    
126100               ADD WS-KVANTAL-INVENT-UPD(INDX) TO UPP-SUINVENT-REM        
126200               MOVE MSGI-IDUSER                TO UPP-IDUSER              
126300               MOVE FUNCTION CURRENT-DATE(1:8) TO UPP-DAREGDAT            
126400               MOVE FUNCTION CURRENT-DATE(9:7) TO UPP-TIREGTID            
126500             END-IF                                                       
126600             PERFORM IMS-REPL-WDA921                                      
126700           ELSE                                                           
126800             MOVE W-WDA9-DAAAPP               TO UPP-DAAAPP               
126900             MOVE WS-KVANTAL-SKROT-UPD(INDX)  TO UPP-SUSKROT2-REM         
127000             MOVE WS-KVANTAL-INVEST-UPD(INDX) TO UPP-SUINVEST-REM         
127100             MOVE WS-KVANTAL-INVENT-UPD(INDX) TO UPP-SUINVENT-REM         
127200             MOVE ZERO                        TO UPP-SUMOTT-REM           
127300                                                 UPP-SUSKROT1-REM         
127400             MOVE MSGI-IDUSER                 TO UPP-IDUSER               
127500             MOVE FUNCTION CURRENT-DATE(1:8)  TO UPP-DAREGDAT             
127600             MOVE FUNCTION CURRENT-DATE(9:7)  TO UPP-TIREGTID             
127700             PERFORM IMS-ISRT-WDA921                                      
127800                                                                          
127900           END-IF                                                         
128000           PERFORM HA-INSERT-DETAIL-LINE                                  
128100                                                                          
128200           MOVE ZERO  TO MOD-KVANTAL-SKROT-UPD(INDX)                      
128300                         MOD-KVANTAL-INVEST-UPD(INDX)                     
128400                         MOD-KVANTAL-INVENT-UPD(INDX)                     
128500           MOVE SPACE TO MOD-TESKROT-UPD  (INDX)                          
128600                         MOD-KDINVEST-UPD (INDX)                          
128700                         MOD-TEINVENT-UPD (INDX)                          
128800                                                                          
128900           COMPUTE WS-KVANTAL-LS-SUM =                                    
129000                   WS-KVANTAL-LS-SUM           -                          
129100                   WS-KVANTAL-SKROT-UPD(INDX)  +                          
129200                   WS-KVANTAL-INVEST-UPD(INDX) +                          
129300                   WS-KVANTAL-INVENT-UPD(INDX)                            
129400           END-COMPUTE                                                    
129500         END-IF                                                           
129600                                                                          
129700         ADD 1 TO INDX                                                    
129800       END-PERFORM                                                        
129900                                                                          
130000       MOVE WS-IDARTNR-NUM                    TO W-WDA9-IDARTNR           
130100       MOVE WS-IDDISTR-NUM                    TO W-WDA9-IDDISTR           
130200       PERFORM IMS-GHU-WDA911                                             
130300       IF SEGMENT-FINNS                                                   
130400         COMPUTE UPD-KVLS-REM =                                           
130500                 UPD-KVLS-REM + WS-KVANTAL-LS-SUM                         
130600         END-COMPUTE                                                      
               IF UPD-KVLS-REM < ZERO                                           
                  MOVE ZERO TO UPD-KVLS-REM                                     
               END-IF                                                           
130700         MOVE MSGI-IDUSER                     TO UPD-IDUSER               
130800         MOVE FUNCTION CURRENT-DATE(1:8)      TO UPD-DAREGDAT             
130900         MOVE FUNCTION CURRENT-DATE(9:7)      TO UPD-TIREGTID             
131000         PERFORM IMS-REPL-WDA911                                          
131100       END-IF                                                             
131200     END-IF                                                               
131300                                                                          
131400     MOVE '001 OK, UPDATE DONE'                 TO MOD-TEWEBINF           
131500     .                                                                    
131600     EJECT                                                                
131700                                                                          
131800 HA-INSERT-DETAIL-LINE SECTION.                                           
131900                                                                          
132000     MOVE FUNCTION CURRENT-DATE(3:6)    TO ATG-TIREGDAT                   
132100                                                                          
132200     IF WS-KVANTAL-SKROT-UPD(INDX) NOT = ZERO                             
132300       MOVE WS-SCRAP                    TO ATG-KDATGNES                   
132400       MOVE WS-TESKROT-UPD       (INDX) TO ATG-TEATGNES                   
132500       MOVE WS-KVANTAL-SKROT-UPD (INDX) TO ATG-KVANTAL                    
132600       PERFORM IMS-ISRT-WDA931                                            
132700     END-IF                                                               
132800                                                                          
132900     IF WS-KVANTAL-INVEST-UPD(INDX) NOT = ZERO                            
133000       MOVE WS-INVESTMENT               TO ATG-KDATGNES                   
133100       MOVE WS-KDINVEST-UPD      (INDX) TO ATG-TEATGNES                   
133200       MOVE WS-KVANTAL-INVEST-UPD(INDX) TO ATG-KVANTAL                    
133300       PERFORM IMS-ISRT-WDA931                                            
133400     END-IF                                                               
133500                                                                          
133600     IF WS-KVANTAL-INVENT-UPD(INDX) NOT = ZERO                            
133700       MOVE WS-ADJUSTMENT               TO ATG-KDATGNES                   
133800       MOVE WS-TEINVENT-UPD      (INDX) TO ATG-TEATGNES                   
133900       MOVE WS-KVANTAL-INVENT-UPD(INDX) TO ATG-KVANTAL                    
134000       PERFORM IMS-ISRT-WDA931                                            
134100     END-IF                                                               
134200                                                                          
134300     .                                                                    
134400     EJECT                                                                
134500                                                                          
134600* --- IMS SEKTIONER ---                                                   
134700*                                                                         
134800                                                                          
134900 IMS-GET-MSG SECTION.                                                     
135000     MOVE '  QC' TO GODK-STATUSKODER                                      
135100     CALL CBLTDLI USING GU MSG-PCB MID-AREA                               
135200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135300     PERFORM IMS-STATUSKONTROLL                                           
135400     .                                                                    
135500                                                                          
135600 IMS-INSERT-MSG SECTION.                                                  
135700     MOVE LOW-VALUE TO MOD-KDZ1 MOD-KDZ2                                  
135800     MOVE SPACE TO GODK-STATUSKODER                                       
135900     CALL CBLTDLI USING ISRT MSG-PCB MOD-AREA MFS-IDMOD                   
136000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     EJECT                                                                
136400                                                                          
136500 IMS-GU-WDK601 SECTION.                                                   
136600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
136700          DELIMITED BY SIZE INTO SSA1                                     
136800     MOVE '  GE' TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
137000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300     EJECT                                                                
137400                                                                          
137500 IMS-GU-WDD311 SECTION.                                                   
137600     STRING 'WDD301  (WDD3BSEQ =' W-WDD3-IDARTNR-X ')'                    
137700            DELIMITED BY SIZE INTO SSA1                                   
137800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
137900            DELIMITED BY SIZE INTO SSA2                                   
138000     MOVE '  GE' TO GODK-STATUSKODER                                      
138100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
138200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
138300     PERFORM IMS-STATUSKONTROLL                                           
138400     .                                                                    
138500     EJECT                                                                
138600                                                                          
138700 IMS-GU-WDA911 SECTION.                                                   
138800     STRING 'WDA901  (IDARTNR  =' W-WDA9-IDARTNR-X ')'                    
138900          DELIMITED BY SIZE INTO SSA1                                     
139000     STRING 'WDA911  (IDDISTR  =' W-WDA9-IDDISTR-X ')'                    
139100          DELIMITED BY SIZE INTO SSA2                                     
139200     MOVE '  GE' TO GODK-STATUSKODER                                      
139300     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA911 SSA1 SSA2               
139400     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
139500     PERFORM IMS-STATUSKONTROLL                                           
139600     .                                                                    
139700                                                                          
139800 IMS-GU-WDA921 SECTION.                                                   
139900     STRING 'WDA901  (IDARTNR  =' W-WDA9-IDARTNR-X ')'                    
140000          DELIMITED BY SIZE INTO SSA1                                     
140100     STRING 'WDA911  (IDDISTR  =' W-WDA9-IDDISTR-X ')'                    
140200          DELIMITED BY SIZE INTO SSA2                                     
140300     STRING 'WDA921  (DAAAPP   =' W-WDA9-DAAAPP-X ')'                     
140400          DELIMITED BY SIZE INTO SSA3                                     
140500     MOVE '  GE' TO GODK-STATUSKODER                                      
140600     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA921 SSA1 SSA2 SSA3          
140700     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000                                                                          
141100 IMS-GU-WDA931 SECTION.                                                   
141200     STRING 'WDA901  (IDARTNR  =' W-WDA9-IDARTNR-X ')'                    
141300          DELIMITED BY SIZE INTO SSA1                                     
141400     STRING 'WDA911  (IDDISTR  =' W-WDA9-IDDISTR-X ')'                    
141500          DELIMITED BY SIZE INTO SSA2                                     
141600     STRING 'WDA921  (DAAAPP   =' W-WDA9-DAAAPP-X ')'                     
141700          DELIMITED BY SIZE INTO SSA3                                     
141800     STRING 'WDA931  (WDA931KY =' W-WDA931KY-X ')'                        
141900          DELIMITED BY SIZE INTO SSA4                                     
142000     MOVE '  GE' TO GODK-STATUSKODER                                      
142100     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA931 SSA1 SSA2               
142200                        SSA3 SSA4                                         
142300     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
142400     PERFORM IMS-STATUSKONTROLL                                           
142500     .                                                                    
142600                                                                          
142700 IMS-GNP-WDA921 SECTION.                                                  
142800     STRING 'WDA921  (DAAAPP   =' W-WDA9-DAAAPP-X ')'                     
142900          DELIMITED BY SIZE INTO SSA1                                     
143000     MOVE '  GE' TO GODK-STATUSKODER                                      
143100     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA921 SSA1                   
143200     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
143300     PERFORM IMS-STATUSKONTROLL                                           
143400     .                                                                    
143500                                                                          
143600 IMS-GNP-WDA931 SECTION.                                                  
143700     MOVE 'WDA931   '         TO SSA1                                     
143800     MOVE '  GE' TO GODK-STATUSKODER                                      
143900     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA931 SSA1                   
144000     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
144100     PERFORM IMS-STATUSKONTROLL                                           
144200     .                                                                    
144300                                                                          
144400 IMS-GHU-WDA901 SECTION.                                                  
144500     STRING 'WDA901  (IDARTNR  =' W-WDA9-IDARTNR-X ')'                    
144600          DELIMITED BY SIZE INTO SSA1                                     
144700     MOVE '  GE' TO GODK-STATUSKODER                                      
144800     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
144900     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
145000     PERFORM IMS-STATUSKONTROLL                                           
145100     .                                                                    
145200                                                                          
145300 IMS-GHU-WDA911 SECTION.                                                  
145400     STRING 'WDA901  (IDARTNR  =' W-WDA9-IDARTNR-X ')'                    
145500          DELIMITED BY SIZE INTO SSA1                                     
145600     STRING 'WDA911  (IDDISTR  =' W-WDA9-IDDISTR-X ')'                    
145700          DELIMITED BY SIZE INTO SSA2                                     
145800     MOVE '  GE' TO GODK-STATUSKODER                                      
145900     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA911 SSA1 SSA2              
146000     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
146100     PERFORM IMS-STATUSKONTROLL                                           
146200     .                                                                    
146300                                                                          
146400 IMS-REPL-WDA911 SECTION.                                                 
146500     MOVE '  ' TO GODK-STATUSKODER                                        
146600     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA911                       
146700     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
146800     PERFORM IMS-STATUSKONTROLL                                           
146900     .                                                                    
147000                                                                          
147100 IMS-GHNP-WDA921 SECTION.                                                 
147200     STRING 'WDA921  (DAAAPP   =' W-WDA9-DAAAPP-X ')'                     
147300          DELIMITED BY SIZE INTO SSA1                                     
147400     MOVE '  GE' TO GODK-STATUSKODER                                      
147500     CALL CBLTDLI USING GHNP WDA9-PCB DLI-IO-WDA921 SSA1                  
147600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900                                                                          
148000 IMS-REPL-WDA921 SECTION.                                                 
148100     MOVE '  ' TO GODK-STATUSKODER                                        
148200     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA921                       
148300     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
148400     PERFORM IMS-STATUSKONTROLL                                           
148500     .                                                                    
148600                                                                          
148700 IMS-ISRT-WDA901 SECTION.                                                 
148800     MOVE 'WDA901   '      TO SSA1                                        
148900     MOVE '  '             TO GODK-STATUSKODER                            
149000     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA901 SSA1                  
149100     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
149200     PERFORM IMS-STATUSKONTROLL                                           
149300     .                                                                    
149400                                                                          
149500 IMS-ISRT-WDA911 SECTION.                                                 
149600     MOVE 'WDA911   '      TO SSA1                                        
149700     MOVE '  '             TO GODK-STATUSKODER                            
149800     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA911 SSA1                  
149900     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
150100     .                                                                    
150200                                                                          
150300 IMS-ISRT-WDA921 SECTION.                                                 
150400     MOVE 'WDA921   '      TO SSA1                                        
150500     MOVE '  '             TO GODK-STATUSKODER                            
150600     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA921 SSA1                  
150700     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
150800     PERFORM IMS-STATUSKONTROLL                                           
150900     .                                                                    
151000     EJECT                                                                
151100                                                                          
151200 IMS-ISRT-WDA931 SECTION.                                                 
151300     MOVE 'WDA931   '      TO SSA1                                        
151400     MOVE '  '             TO GODK-STATUSKODER                            
151500     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA931 SSA1                  
151600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
151700     PERFORM IMS-STATUSKONTROLL                                           
151800     .                                                                    
151900     EJECT                                                                
152000                                                                          
152100 IMS-GU-WDK711 SECTION.                                                   
152200                                                                          
152300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
152400           DELIMITED BY SIZE INTO SSA1                                    
152500     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
152600           DELIMITED BY SIZE INTO SSA2                                    
152700     MOVE '  GE'                    TO GODK-STATUSKODER                   
152800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
152900     MOVE WDK7-STATUS-CODE          TO STATUS-WS                          
153000     PERFORM IMS-STATUSKONTROLL                                           
153100     .                                                                    
153200     EJECT                                                                
153300 IMS-STATUSKONTROLL SECTION.                                              
153400                                                                          
153500     SET STATUS-IX TO 1                                                   
153600     SEARCH GODK-STATUS                                                   
153700       AT END                                                             
153800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
153900         DELIMITED BY SIZE INTO FELTEXT                                   
154000         CALL FELLOG                                                      
154100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
154200         CONTINUE                                                         
154300     END-SEARCH                                                           
154400     .                                                                    
