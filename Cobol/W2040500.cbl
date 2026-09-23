000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2040500.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   13/12/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SÄSONGSANALYS FÖR NDC KINA.                                      
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDK7                                       
001100*        PROGRAMMET LÄSER      WDD3                                       
001200*                              WDB6                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W2T405                                              
001600*                     W2T405U                                             
001700*        MID:         W2I40501                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O40501                                            
002100*                                                                         
002200*----                                                                     
002300*    2013-12-03  E'TRACKER: 10205381 CHINA PROCUREMENT SCREEN 5.          
002400*                                                                         
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300*    -COPY WY2000W1                                                       
003400     SKIP3                                                                
003500                                                                          
003600 77  IDPGM                       PIC X(08)   VALUE 'W2040500'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004400 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
004500 77  IX2                         PIC S9(3)   VALUE ZERO.                  
004600 77  IX                          PIC S9(3)   VALUE ZERO.                  
004700 77  MAX-IX                      PIC S9(3)   VALUE +12.                   
004800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 77  IDLEVNR-WS                  PIC X(5)    VALUE SPACES.                
005000                                                                          
005100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200 77  WS-TOT-VALANT               PIC 9(9)    VALUE ZERO.                  
005300 77  WS-TOT-SIMANT               PIC 9(9)    VALUE ZERO.                  
005400 77  WS-TOT-HISANT               PIC 9(9)    VALUE ZERO.                  
005500 77  WS-INDEX                    PIC 9(3)    VALUE ZERO.                  
005600 77  WS-INDEX-DEC                PIC 9(3)V9(3) VALUE ZERO.                
005700 77  WS-ANTAL                    PIC 9(9)    VALUE ZERO.                  
005800 77  WS-KVOI                     PIC 9(7)    VALUE ZERO.                  
005900 77  WS-SIMIX-SUM                PIC 9(4)    VALUE ZERO.                  
006000 77  WS-JUSTERA                  PIC S9V9(2) VALUE ZERO  COMP-3.          
006100                                                                          
006200 01  FILLER             PIC X(16) VALUE 'WWIDFTG      '.                  
006300*01 -COPY WWIDFTG                                                         
006400                                                                          
006500     EJECT                                                                
006600                                                                          
006700 77  SIM-INDEX-SW                PIC X       VALUE 'N'.                   
006800     88  SIM-INDEX-JA                        VALUE 'J'.                   
006900     88  SIM-INDEX-NEJ                       VALUE 'N'.                   
007000                                                                          
007100 77  SIM-ANTAL-SW                PIC X       VALUE 'N'.                   
007200     88  SIM-ANTAL-JA                        VALUE 'J'.                   
007300     88  SIM-ANTAL-NEJ                       VALUE 'N'.                   
007400                                                                          
007500 77  VISA-INFO-SW                PIC X       VALUE 'J'.                   
007600     88  VISA-INFO-OK                        VALUE 'J'.                   
007700     88  VISA-INFO-FEL                       VALUE 'N'.                   
007800                                                                          
007900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008000     88  INDATA-OK                           VALUE 'J'.                   
008100     88  INDATA-FEL                          VALUE 'N'.                   
008200                                                                          
008300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008400     88  NYCKLAR-OK                          VALUE 'J'.                   
008500     88  NYCKLAR-FEL                         VALUE 'N'.                   
008600                                                                          
008700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008800     88  EGEN-MID                            VALUE '2405'.                
008900     88  GODK-MID                            VALUE '2401' '2402'          
009000                                                   '2403' '2404'          
009100                                                   '2405' '2406'          
009200                                                   '2407' '2408'          
009300                                                   '2409'.                
009400     88  HELP-MID                            VALUE '0551'.                
009500     EJECT                                                                
009600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009700 01  GENERELLA-SUBPROGRAM.                                                
009800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
010300     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
010400     03  W271SEAS                PIC X(8)    VALUE 'W271SEAS'.            
010500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010800*01 -COPY WMEDAREA                                                        
010900     SKIP3                                                                
011000 01  MESSAGE-CODES.                                                       
011100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011600     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
011700     03  ERR-ARTIKEL-SAKNAS-DC   PIC X(3)    VALUE '305'.                 
011800     03  ERR-REFILL-PART         PIC X(3)    VALUE '434'.                 
011900     03  ERR-ART-HIST-SAKNAS     PIC X(3)    VALUE '214'.                 
012000     03  ERR-KONFLIKT            PIC X(3)    VALUE '002'.                 
012100     03  ERR-NO-UPDATE           PIC X(3)    VALUE '034'.                 
012200     03  ERR-NOT-AUTH               PIC X(3) VALUE '405'.                 
012300                                                                          
012400 01  MEDDELANDE.                                                          
012500     03  MED-1.                                                           
012600      05 FILLER                 PIC X(20)                                 
012700         VALUE 'TOTALEN AV INDEX ÄR '.                                    
012800      05 MED-1-INDEX            PIC Z(3)9.                                
012900      05 FILLER                 PIC X(16)                                 
013000         VALUE ' MÅSTE VARA 1200'.                                        
013100     03  MED-2.                                                           
013200      05 FILLER                 PIC X(18)                                 
013300         VALUE 'TOTAL OF INDEX IS '.                                      
013400      05 MED-2-INDEX            PIC Z(3)9.                                
013500      05 FILLER                 PIC X(16)                                 
013600         VALUE ' IT MUST BE 1200'.                                        
013700     03  MED-3                  PIC X(30)                                 
013800         VALUE 'INPUT SHOULD BE YYMMDD    '.                              
013900     03  MED-4                  PIC X(30)                                 
014000         VALUE 'INPUT MÅSTE VARA ÅÅMMDD   '.                              
014100                                                                          
014200     EJECT                                                                
014300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014600     SKIP3                                                                
014700*01 -COPY WMSGINIT                                                        
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL W271REFL                                         
015000*                                                                         
015100 01  FILLER                      PIC X(16)   VALUE 'W271REFL'.            
015200     SKIP3                                                                
015300*01 -COPY W271REFL                                                        
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL W271UTUP                                         
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'W271UTUP'.            
015800     SKIP3                                                                
015900*01 -COPY W271UTUP                                                        
016000     EJECT                                                                
016100*    --- PARAMETRAR TILL W271SEAS                                         
016200*                                                                         
016300 01  FILLER                      PIC X(16)   VALUE 'W271SEAS'.            
016400     SKIP3                                                                
016500*01 -COPY W271SEAS                                                        
016600     EJECT                                                                
016700*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
016800*01 -COPY WDATAREA                                                        
016900     EJECT                                                                
017000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
017100*                                                                         
017200************************************************************              
017300* MSGI-AREA ANVÄNDS FÖR ATT SPARA DET SOM LIGGER                          
017400*           I MOD FÖR DE FÄLT DÄR SAMMA FÄLT ANVÄNDS                      
017500*           FÖR MID OCH MOD                                               
017600* MSGI-AREA SPARAS UNDAN PÅ MSGI-SPAR-AREA I                              
017700*           NYCKELDATABASEN                                               
017800************************************************************              
017900*                                                                         
018000 01  SPAR-AREA.                                                           
018100     03  SPAR-IDTRANS           PIC X(4)    VALUE '2405'.                 
018200     03  SPAR-MSGI-SIMIX        OCCURS 12                                 
018300                                PIC 9(3)    VALUE ZERO.                   
018400     03  SPAR-MSGI-SIMANT       OCCURS 12                                 
018500                                PIC 9(7)    VALUE ZERO.                   
018600                                                                          
018700************************************************************              
018800     EJECT                                                                
018900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019000*                                                                         
019100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019200     SKIP3                                                                
019300*01  MID -COPY W2I40501                                                   
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019600     SKIP3                                                                
019700*01  -COPY WMSGAREA                                                       
019800     EJECT                                                                
019900     03  MOD REDEFINES MSG-AREA.                                          
020000*      05  -COPY W2O40501                                                 
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020300     SKIP3                                                                
020400*01  -COPY WMFSAREA                                                       
020500     EJECT                                                                
020600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020700*                                                                         
020800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020900     SKIP3                                                                
021000 01  NYCKLAR-TILL-DLI.                                                    
021100     03  W-IDARTNR-X.                                                     
021200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021300                                                                          
021400     03  W-IDDC-X.                                                        
021500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
021600                                                                          
021700     03  W-IDLAND-X.                                                      
021800         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
021900                                                                          
022000     03  W-IDSKYLT-X.                                                     
022100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
022200                                                                          
022300     03  W-IDDC-B6-X.                                                     
022400         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
022500     SKIP2                                                                
022600*    --- STATUS KODER FRÅN IMS                                            
022700 01  STATUS-WS                   PIC XX.                                  
022800     88  SEGMENT-FINNS                       VALUE '  '.                  
022900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023100     SKIP2                                                                
023200 01  GODK-STATUSKODER.                                                    
023300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023400     SKIP3                                                                
023500 01  SSA1                        PIC X(64).                               
023600 01  SSA2                        PIC X(64).                               
023700     EJECT                                                                
023800*    --- IMS FUNKTIONSKODER                                               
023900*01  -COPY W0003                                                          
024000     EJECT                                                                
024100*    ---  DLI INPUT-OUTPUT AREA                                           
024200                                                                          
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
024400 01  DLI-IO-WDB601.                                                       
024500*    03  -COPY WDB601                                                     
024600     EJECT                                                                
024700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
024800 01  DLI-IO-WDK711.                                                       
024900*    03  -COPY WDK711                                                     
025000     EJECT                                                                
025100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
025200 01  DLI-IO-WDK712.                                                       
025300*    03  -COPY WDK712                                                     
025400     EJECT                                                                
025500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
025600 01  DLI-IO-WDD311.                                                       
025700*    03  -COPY WDD311                                                     
025800     EJECT                                                                
025900 LINKAGE SECTION.                                                         
026000*01  -COPY W0009   -PRE MSG-                                              
026100*01  -COPY W0008   -PRE WDP7-                                             
026200     05  FILLER                  PIC X.                                   
026300                                                                          
026400*01  -COPY W0008  -PRE WDB6-                                              
026500     05  FILLER                  PIC X.                                   
026600                                                                          
026700*01  -COPY W0008  -PRE WDK7-                                              
026800     05  FILLER                  PIC X.                                   
026900                                                                          
027000*01  -COPY W0008  -PRE WDD3-                                              
027100     05  FILLER                  PIC X.                                   
027200                                                                          
027300                                                                          
027400 01  SEAS-WDK7-PCB               PIC X.                                   
027500 01  SEAS-WDL7-PCB               PIC X.                                   
027600 01  SEAS-WDL4-PCB               PIC X.                                   
027700 01  SEAS-WDK6-PCB               PIC X.                                   
027800                                                                          
027900 01  REFL-2501-PCB               PIC X.                                   
028000 01  REFL-WDB6-PCB               PIC X.                                   
028100 01  REFL-WDK7-PCB               PIC X.                                   
028200 01  REFL-UTIL-WDK6-PCB          PIC X.                                   
028300 01  REFL-UTIL-WDK7-PCB          PIC X.                                   
028400 01  REFL-UTIL-WDB6-PCB          PIC X.                                   
028500     EJECT                                                                
028600                                                                          
028700*****W271UTUP**********                                                   
028800 01  UTUP1-WDK7-PCB              PIC X.                                   
028900 01  UTUP1-WDB6-PCB              PIC X.                                   
029000 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
029100 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
029200 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
029300     EJECT                                                                
029400                                                                          
029500 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB WDK7-PCB             
029600                           WDD3-PCB                                       
029700                           SEAS-WDK7-PCB                                  
029800                           SEAS-WDL7-PCB                                  
029900                           SEAS-WDL4-PCB                                  
030000                           SEAS-WDK6-PCB                                  
030100                           REFL-2501-PCB                                  
030200                           REFL-WDB6-PCB                                  
030300                           REFL-WDK7-PCB                                  
030400                           REFL-UTIL-WDK6-PCB                             
030500                           REFL-UTIL-WDK7-PCB                             
030600                           REFL-UTIL-WDB6-PCB                             
030700                           UTUP1-WDK7-PCB                                 
030800                           UTUP1-WDB6-PCB                                 
030900                           UTUP1-UTIL-WDK6-PCB                            
031000                           UTUP1-UTIL-WDK7-PCB                            
031100                           UTUP1-UTIL-WDB6-PCB.                           
031200                                                                          
031300 MAIN SECTION.                                                            
031400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB WDK7-PCB             
031500                           WDD3-PCB                                       
031600                           SEAS-WDK7-PCB                                  
031700                           SEAS-WDL7-PCB                                  
031800                           SEAS-WDL4-PCB                                  
031900                           SEAS-WDK6-PCB                                  
032000                           REFL-2501-PCB                                  
032100                           REFL-WDB6-PCB                                  
032200                           REFL-WDK7-PCB                                  
032300                           REFL-UTIL-WDK6-PCB                             
032400                           REFL-UTIL-WDK7-PCB                             
032500                           REFL-UTIL-WDB6-PCB                             
032600                           UTUP1-WDK7-PCB                                 
032700                           UTUP1-WDB6-PCB                                 
032800                           UTUP1-UTIL-WDK6-PCB                            
032900                           UTUP1-UTIL-WDK7-PCB                            
033000                           UTUP1-UTIL-WDB6-PCB.                           
033100                                                                          
033200     PERFORM IMS-GET-MSG                                                  
033300     IF SEGMENT-FINNS                                                     
033400       PERFORM A-INIT                                                     
033500       PERFORM B-KOLLA-NYCKLAR                                            
033600       IF NYCKLAR-OK                                                      
033700         IF MFS-UPDATE                                                    
033800           PERFORM G-KOLLA-INPUT                                          
033900           IF INDATA-OK                                                   
034000             PERFORM H-UPPDATERA                                          
034100           ELSE                                                           
034200             MOVE NEJ TO VISA-INFO-SW                                     
034300           END-IF                                                         
034400         ELSE                                                             
034500           IF MFS-FIRST                                                   
034600             PERFORM C-FOERSTA-SIDA                                       
034700           ELSE                                                           
034800             PERFORM E-SAMMA-SIDA                                         
034900           END-IF                                                         
035000         END-IF                                                           
035100                                                                          
035200         IF VISA-INFO-OK                                                  
035300           PERFORM F-LAES-VISA-INFO                                       
035400                                                                          
035500           IF VISA-INFO-OK                                                
035600*--------    UPPDATERA BILDEN                                             
035700             PERFORM I-VISA-SIM-INFO                                      
035800           END-IF                                                         
035900         END-IF                                                           
036000*---                                                                      
036100*---     SAVE MFG SUPPLIER USING INIT-IO-AREA                             
036200         IF IDLEVNR-WS > SPACES                                           
036300            MOVE ALL '+'           TO MSGI-WMSGINIT                       
036400            MOVE '001'             TO MSGI-KDCALL                         
036500            MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                   
036600            MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                         
036700            MOVE '2405'            TO MSGI-IDTRANS                        
036800            MOVE IDLEVNR-WS        TO MSGI-IDLEVNR                        
036900                                                                          
037000            CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                    
037100         END-IF                                                           
037200       END-IF                                                             
037300                                                                          
037400*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
037500*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
037600       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O40501 + 4                      
037700       PERFORM IMS-INSERT-MSG                                             
037800     END-IF                                                               
037900                                                                          
038000     MOVE ZERO TO RETURN-CODE                                             
038100     GOBACK                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 A-INIT SECTION.                                                          
038500     MOVE 'A-INIT '   TO CURRENT-SECTION                                  
038600                                                                          
038700     IF MSG-DUBBLA-TRANSKODER                                             
038800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I40501                 
038900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
039000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
039100     ELSE                                                                 
039200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I40501                  
039300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039500     END-IF                                                               
039600                                                                          
039700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
040000                                                                          
040100     MOVE LOW-VALUE   TO MSG-AREA                                         
040200     MOVE 'W2O405N1'  TO MFS-IDMOD                                        
040300     MOVE '2405'      TO MOD-IDTRANS                                      
040400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040500                                                                          
040600     MOVE SPACE       TO MED-IDMFSINF                                     
040700     MOVE SPACE       TO MED-IDMFSFEL                                     
040800                                                                          
040900     IF EGEN-MID OR HELP-MID                                              
041000       CONTINUE                                                           
041100     ELSE                                                                 
041200       MOVE SPACE TO MFS-KDTRTYP                                          
041300       MOVE '7' TO MFS-IDPFK                                              
041400     END-IF                                                               
041500                                                                          
041600     ACCEPT DAGENS-DATUM FROM DATE                                        
041700                                                                          
041800     .                                                                    
041900     EJECT                                                                
042000 B-KOLLA-NYCKLAR SECTION.                                                 
042100     MOVE 'B-KOLLA-NYCKLAR '  TO CURRENT-SECTION                          
042200                                                                          
042300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
042400     MOVE '001'             TO MSGI-KDCALL                                
042500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
042600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042700     MOVE '2405'            TO MSGI-IDTRANS                               
042800     IF EGEN-MID                                                          
042900         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
043000         MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                            
043100     END-IF                                                               
043200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
043300     IF MSGI-SPAR-AREA (1:4) = '2405'                                     
043400       MOVE MSGI-SPAR-AREA TO SPAR-AREA                                   
043500     END-IF                                                               
043600                                                                          
043700*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
043800     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
043900                                                                          
044000     MOVE JA TO NYCKLAR-SW                                                
044100     MOVE JA TO VISA-INFO-SW                                              
044200                                                                          
044300                                                                          
044400*    -- KONTROLL AV IDARTNR                                               
044500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
044600                                                                          
044700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
044800       MOVE '7'         TO MFS-IDPFK                                      
044900       MOVE SPACE       TO MFS-KDTRTYP                                    
045000     END-IF                                                               
045100     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
045200     IF MSGI-IDARTNR NUMERIC                                              
045300       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
045400     ELSE                                                                 
045500       MOVE NEJ TO NYCKLAR-SW                                             
045600     END-IF                                                               
045700                                                                          
045800*    -- KONTROLL AV IDDC                                                  
045900     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
046000                                                                          
046100     IF MID-IDDC-IN NOT = ALL '+'                                         
046200       MOVE '7'         TO MFS-IDPFK                                      
046300       MOVE SPACE       TO MFS-KDTRTYP                                    
046400     END-IF                                                               
046500     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
046600                           W-IDDC-B6                                      
046700                                                                          
046800     IF MSGI-IDDC-KEY = ALL '+' OR SPACE OR ZEROS                         
046900       MOVE NEJ TO NYCKLAR-SW                                             
047000     ELSE                                                                 
047100       PERFORM IMS-GU-WDB601                                              
047200       IF SEGMENT-FINNS                                                   
047300         IF DCS-NDC-CN                                                    
047400         OR (DCS-NDC-NA AND DCS-USA)                                      
047500           CONTINUE                                                       
047600         ELSE                                                             
047700           MOVE ERR-WRONG-DC   TO MED-IDMFSFEL                            
047800           MOVE NEJ   TO NYCKLAR-SW                                       
047900         END-IF                                                           
048000       ELSE                                                               
048100         MOVE NEJ TO NYCKLAR-SW                                           
048200       END-IF                                                             
048300     END-IF                                                               
048400                                                                          
048500     IF GODK-MID OR NYCKLAR-OK                                            
048600       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
048700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZEROS BY SPACES           
048800       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
048900     ELSE                                                                 
049000       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
049100       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
049200     END-IF                                                               
049300                                                                          
049400     IF NYCKLAR-FEL                                                       
049500       IF MED-IDMFSFEL = SPACE                                            
049600         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
049700       END-IF                                                             
049800       CALL WMEDKONV USING MED-WMEDAREA                                   
049900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
050000       PERFORM MFS-RENSA-FAELT-IN                                         
050100       PERFORM MFS-RENSA-FAELT-UT                                         
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 C-FOERSTA-SIDA SECTION.                                                  
050600     MOVE 'C-FOERSTA-SIDA '  TO CURRENT-SECTION                           
050700                                                                          
050800     PERFORM MFS-RENSA-FAELT-IN                                           
050900     .                                                                    
051000     EJECT                                                                
051100 E-SAMMA-SIDA SECTION.                                                    
051200     MOVE 'E-SAMMA-SIDA '   TO CURRENT-SECTION                            
051300                                                                          
051400     IF EGEN-MID OR HELP-MID                                              
051500       IF MID-INPUT = ALL '+'                                             
051600         PERFORM MFS-RENSA-FAELT-IN                                       
051700       ELSE                                                               
051800         IF MFS-IDPFK = '1'                                               
051900           CONTINUE                                                       
052000         ELSE                                                             
052100           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
052200           CALL WMEDKONV USING MED-WMEDAREA                               
052300           MOVE MED-MFSINF TO MOD-TEMFSINF                                
052400         END-IF                                                           
052500         PERFORM EA-MID-INDATA-TILL-MOD                                   
052600       END-IF                                                             
052700     ELSE                                                                 
052800       PERFORM MFS-RENSA-FAELT-IN                                         
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 EA-MID-INDATA-TILL-MOD SECTION.                                          
053300     MOVE 'EA-MID-INDATA-TILL-MOD '  TO CURRENT-SECTION                   
053400                                                                          
053500* * * * * FÖR VARJE MID-FÄLT                                              
053600* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
053700* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
053800* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
053900                                                                          
054000     IF MID-TISPSEA = ALL '+'                                             
054100       MOVE MFS-RENSA-FAELT       TO MOD-TISPSEA                          
054200     ELSE                                                                 
054300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISPSEA-ATTR                     
054400       MOVE MID-TISPSEA           TO MOD-TISPSEA                          
054500     END-IF                                                               
054600                                                                          
054700     MOVE +1                 TO IX                                        
054800     PERFORM UNTIL IX > MAX-IX                                            
054900        IF MID-RESEASON-SIMIX (IX) = ALL '+'                              
055000          MOVE MFS-RENSA-FAELT       TO MOD-RESEASON-SIMIX (IX)           
055100        ELSE                                                              
055200          MOVE MFS-ADD-LAES-IN-FAELT TO                                   
055300                                     MOD-RESEASON-SIMIX-ATTR (IX)         
055400          INSPECT MID-RESEASON-SIMIX(IX) REPLACING                        
055500                                         LEADING SPACE BY ZERO            
055600                                                                          
055700          MOVE MID-RESEASON-SIMIX (IX) TO SPAR-MSGI-SIMIX (IX)            
055800        END-IF                                                            
055900                                                                          
056000        IF MID-KVOI-SIMANT (IX) = ALL '+'                                 
056100          MOVE MFS-RENSA-FAELT       TO MOD-KVOI-SIMANT (IX)              
056200        ELSE                                                              
056300          MOVE MFS-ADD-LAES-IN-FAELT TO                                   
056400                                     MOD-KVOI-SIMANT-ATTR (IX)            
056500          INSPECT MID-KVOI-SIMANT(IX) REPLACING                           
056600                                      LEADING SPACE BY ZERO               
056700                                                                          
056800          MOVE MID-KVOI-SIMANT (IX)  TO SPAR-MSGI-SIMANT (IX)             
056900        END-IF                                                            
057000        ADD +1   TO IX                                                    
057100     END-PERFORM                                                          
057200                                                                          
057300     .                                                                    
057400     EJECT                                                                
057500 F-LAES-VISA-INFO  SECTION.                                               
057600     MOVE 'F-LAES-VISA-INFO '  TO CURRENT-SECTION                         
057700                                                                          
057800     MOVE JA  TO VISA-INFO-SW                                             
057900                                                                          
058000     PERFORM IMS-GU-WDK711                                                
058100                                                                          
058200     IF SEGMENT-SAKNAS                                                    
058300       MOVE ERR-ARTIKEL-SAKNAS-DC    TO MED-IDMFSFEL                      
058400       CALL WMEDKONV USING MED-WMEDAREA                                   
058500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
058600       PERFORM MFS-RENSA-FAELT-IN                                         
058700       PERFORM MFS-RENSA-FAELT-UT                                         
058800       MOVE NEJ  TO VISA-INFO-SW                                          
058900     ELSE                                                                 
059000       IF SLAG-IDDC-REF NOT = SPACE                                       
059100         MOVE ERR-REFILL-PART  TO MED-IDMFSFEL                            
059200         CALL WMEDKONV USING MED-WMEDAREA                                 
059300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
059400         PERFORM MFS-RENSA-FAELT-IN                                       
059500         PERFORM MFS-RENSA-FAELT-UT                                       
059600         MOVE NEJ  TO VISA-INFO-SW                                        
059700       ELSE                                                               
059800         IF NOT EGEN-MID                                                  
059900         OR MID-IDARTNR-IN NOT = ALL '+'                                  
060000         OR MID-IDDC-IN NOT = ALL '+'                                     
060100                                                                          
060200           PERFORM FA-LAES-GRUNDDATA                                      
060300         ELSE                                                             
060400           IF MFS-UPDATE                                                  
060500             PERFORM FC-HAEMTA-INFO                                       
060600           ELSE                                                           
060700             PERFORM FB-BEHANDLA-SIMULERING                               
060800           END-IF                                                         
060900         END-IF                                                           
061000       END-IF                                                             
061100     END-IF                                                               
061200     .                                                                    
061300     EJECT                                                                
061400 FA-LAES-GRUNDDATA SECTION.                                               
061500     MOVE 'FA-LAES-GRUNDDATA  '  TO CURRENT-SECTION                       
061600                                                                          
061700******************************************************************        
061800*      -      HÄMTA SÄSONGSINDEX FRÅN HISTORIK MHA SUBPGM W271SEAS        
061900*      -      LÄGG UT HISTORIKINFO ÄVEN I SIMULERINGSDELEN                
062000*      -      HÄMTA INFO TILL VALID-DELEN FRÅN WDK711                     
062100*                       SLAG-RESEASON                                     
062200*                       SLAG-RESEASON * SLAG-KVPB-REF                     
062300******************************************************************        
062400                                                                          
062500     IF SLAG-DASPSEA > 0                                                  
062600        MOVE SLAG-DASPSEA(3:6) TO MOD-TISPSEA                             
062700     ELSE                                                                 
062800        MOVE MFS-RENSA-FAELT   TO MOD-TISPSEA                             
062900     END-IF                                                               
063000                                                                          
063100     IF SLAG-TIMANSEA > ZERO                                              
063200       MOVE SLAG-TIMANSEA      TO MOD-TIMANSEA                            
063300     ELSE                                                                 
063400       MOVE MFS-RENSA-FAELT    TO MOD-TIMANSEA                            
063500     END-IF                                                               
063600                                                                          
063700     MOVE SLAG-IDLEVNR         TO IDLEVNR-WS                              
063800                                                                          
063900     MOVE 'GB'                 TO W-IDSKYLT                               
064000     PERFORM IMS-GU-WDD311-BSEQ                                           
064100     IF SEGMENT-FINNS                                                     
064200        MOVE TEXT-BEART        TO MOD-BEART-ENG                           
064300     ELSE                                                                 
064400        MOVE MFS-RENSA-FAELT   TO MOD-BEART-ENG                           
064500     END-IF                                                               
064600                                                                          
064700*---                                                                      
064800                                                                          
064900     MOVE MSGI-IDDC-KEY    TO SEAS-IDDC                                   
065000     MOVE W-IDARTNR        TO SEAS-IDARTNR                                
065100     MOVE NEJ              TO SEAS-FLKVARTAL                              
065200                                                                          
065300     CALL W271SEAS USING SEAS-W271SEAS SEAS-WDK7-PCB                      
065400                                       SEAS-WDL7-PCB                      
065500                                       SEAS-WDL4-PCB                      
065600                                       SEAS-WDK6-PCB                      
065700                                                                          
065800     IF SEAS-KDSVAR = SPACE                                               
065900       MOVE SEAS-ANT-HIST-AR   TO MOD-ANT-HIST-AR                         
066000       MOVE SEAS-ANT-HIST-MAN  TO MOD-ANT-HIST-MAN                        
066100       MOVE SEAS-OSAKERHET     TO MOD-REOSAEK                             
066200                                                                          
066300       IF SEAS-SEASON-ARTIKEL = JA                                        
066400         MOVE 'Y'              TO MOD-FLSEASON                            
066500       ELSE                                                               
066600         MOVE 'N'              TO MOD-FLSEASON                            
066700       END-IF                                                             
066800                                                                          
066900       MOVE ZERO               TO WS-TOT-VALANT                           
067000       MOVE +1                 TO IX                                      
067100       PERFORM UNTIL IX > MAX-IX                                          
067200                                                                          
067300         COMPUTE WS-INDEX = 100 * SLAG-RESEASON (IX)                      
067400         MOVE WS-INDEX         TO MOD-RESEASON-VALIX (IX)                 
067500         COMPUTE WS-ANTAL ROUNDED = SLAG-RESEASON (IX) *                  
067600                                    SLAG-KVPB-REF                         
067700                                                                          
067800         MOVE WS-ANTAL         TO MOD-KVOI-VALANT (IX)                    
067900         ADD WS-ANTAL          TO WS-TOT-VALANT                           
068000         ADD +1                TO IX                                      
068100       END-PERFORM                                                        
068200                                                                          
068300       MOVE WS-TOT-VALANT      TO MOD-KVOI-TOT-VALANT                     
068400                                                                          
068500*----                                                                     
068600                                                                          
068700       MOVE ZERO               TO WS-TOT-HISANT                           
068800                                                                          
068900       MOVE +1                 TO IX                                      
069000       PERFORM UNTIL IX > MAX-IX                                          
069100                                                                          
069200         MOVE SEAS-RESEASON (IX) TO MOD-RESEASON-HISIX (IX)               
069300                                    SPAR-MSGI-SIMIX (IX)                  
069400                                                                          
069500         MOVE SEAS-KVOI (IX)   TO MOD-KVOI-HISANT (IX)                    
069600                                  WS-KVOI                                 
069700         ADD WS-KVOI           TO WS-TOT-HISANT                           
069800                                                                          
069900*---  MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                     
070000*---  WS-INDEX ÄR I %, DÄRFÖR / 100                                       
070100         COMPUTE SPAR-MSGI-SIMANT (IX) ROUNDED =                          
070200                   (SPAR-MSGI-SIMIX (IX) * SLAG-KVPB-REF) / 100           
070300                                                                          
070400         ADD +1                TO IX                                      
070500       END-PERFORM                                                        
070600                                                                          
070700       MOVE WS-TOT-HISANT      TO MOD-KVOI-TOT-HISANT                     
070800     ELSE                                                                 
070900**--   ARTIKELN SAKNAS WDL711, SE PGM W271SEAS                            
071000       MOVE ERR-ART-HIST-SAKNAS      TO MED-IDMFSFEL                      
071100       CALL WMEDKONV USING MED-WMEDAREA                                   
071200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
071300       PERFORM MFS-RENSA-FAELT-UT                                         
071400     END-IF                                                               
071500                                                                          
071600     .                                                                    
071700     EJECT                                                                
071800 FB-BEHANDLA-SIMULERING  SECTION.                                         
071900     MOVE 'FB-BEHANDLA-SIMULERING '   TO CURRENT-SECTION                  
072000                                                                          
072100*************************************************************             
072200**---   MFS-IDPFK = '1' ÄR LIKA MED PF17                                  
072300**---   ANVÄNDS FÖR ATT ÅTERSTÄLLA ALLA IX TILL 100                       
072400**---   DVS EJ SÄSONG                                                     
072500*************************************************************             
072600                                                                          
072700     IF MFS-IDPFK = '1'                                                   
072800       MOVE +1               TO IX                                        
072900       PERFORM UNTIL IX > MAX-IX                                          
073000         MOVE 100            TO SPAR-MSGI-SIMIX (IX)                      
073100                                                                          
073200*---    MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                   
073300*---    WS-INDEX ÄR I %, DÄRFÖR / 100                                     
073400                                                                          
073500         COMPUTE SPAR-MSGI-SIMANT (IX) ROUNDED =                          
073600                 (SPAR-MSGI-SIMIX (IX) * SLAG-KVPB-REF) / 100             
073700         ADD +1              TO IX                                        
073800       END-PERFORM                                                        
073900                                                                          
074000       PERFORM MFS-RENSA-FAELT-IN                                         
074100     ELSE                                                                 
074200       PERFORM FBA-KTRL-INPUT                                             
074300       IF INDATA-OK                                                       
074400         PERFORM FBB-SIMULERA-INDEX                                       
074500       ELSE                                                               
074600         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
074700         CALL WMEDKONV USING MED-WMEDAREA                                 
074800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
074900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
075000                                                                          
075100         MOVE NEJ TO VISA-INFO-SW                                         
075200       END-IF                                                             
075300     END-IF                                                               
075400     PERFORM MFS-ROER-EJ-FAELT-UT                                         
075500     IF MID-TISPSEA = ALL '+'                                             
075600       IF SLAG-DASPSEA > 0                                                
075700          MOVE SLAG-DASPSEA(3:6) TO MOD-TISPSEA                           
075800       ELSE                                                               
075900          MOVE MFS-RENSA-FAELT   TO MOD-TISPSEA                           
076000       END-IF                                                             
076100     END-IF                                                               
076200                                                                          
076300     .                                                                    
076400     EJECT                                                                
076500 FBA-KTRL-INPUT  SECTION.                                                 
076600     MOVE 'FBA-KTRL-INPUT '  TO CURRENT-SECTION                           
076700                                                                          
076800     MOVE JA  TO INDATA-SW                                                
076900                                                                          
077000     MOVE +1                 TO IX                                        
077100     PERFORM UNTIL IX > MAX-IX                                            
077200       IF MID-RESEASON-SIMIX (IX) NOT = ALL '+'                           
077300         MOVE JA             TO SIM-INDEX-SW                              
077400         INSPECT MID-RESEASON-SIMIX(IX) REPLACING                         
077500                               LEADING SPACE BY ZERO                      
077600         IF MID-RESEASON-SIMIX (IX) NOT NUMERIC                           
077700           MOVE MFS-NUM-FAELT-FEL                                         
077800                         TO MOD-RESEASON-SIMIX-ATTR (IX)                  
077900                                                                          
078000           MOVE NEJ TO INDATA-SW                                          
078100         END-IF                                                           
078200       END-IF                                                             
078300                                                                          
078400       IF MID-KVOI-SIMANT (IX) NOT = ALL '+'                              
078500         MOVE JA             TO SIM-ANTAL-SW                              
078600         INSPECT MID-KVOI-SIMANT (IX) REPLACING                           
078700                               LEADING SPACE BY ZERO                      
078800         IF MID-KVOI-SIMANT (IX) NOT NUMERIC                              
078900           MOVE MFS-NUM-FAELT-FEL                                         
079000                         TO MOD-KVOI-SIMANT-ATTR (IX)                     
079100                                                                          
079200           MOVE NEJ TO INDATA-SW                                          
079300         END-IF                                                           
079400       END-IF                                                             
079500       ADD +1                TO IX                                        
079600     END-PERFORM                                                          
079700                                                                          
079800     IF SIM-INDEX-JA                                                      
079900**************************************************************            
080000**------   KOLLA ATT SUMMAN BLIR 1200                                     
080100**************************************************************            
080200       MOVE ZERO             TO WS-SIMIX-SUM                              
080300       MOVE +1               TO IX                                        
080400       PERFORM UNTIL IX > MAX-IX                                          
080500         ADD SPAR-MSGI-SIMIX (IX)                                         
080600                             TO WS-SIMIX-SUM                              
080700         ADD +1              TO IX                                        
080800       END-PERFORM                                                        
080900                                                                          
081000       IF WS-SIMIX-SUM NOT = 1200                                         
081100         IF MSGI-IDLAND-SPR = 'SE'                                        
081200           MOVE WS-SIMIX-SUM TO MED-1-INDEX                               
081300           MOVE MED-1        TO MOD-TEMFSINF                              
081400         ELSE                                                             
081500           MOVE WS-SIMIX-SUM TO MED-2-INDEX                               
081600           MOVE MED-2        TO MOD-TEMFSINF                              
081700         END-IF                                                           
081800         MOVE +1           TO IX                                          
081900         PERFORM UNTIL IX > MAX-IX                                        
082000            MOVE MFS-ALFA-FAELT-FEL                                       
082100                            TO MOD-RESEASON-SIMIX-ATTR (IX)               
082200            ADD +1          TO IX                                         
082300         END-PERFORM                                                      
082400                                                                          
082500         MOVE NEJ TO INDATA-SW                                            
082600       END-IF                                                             
082700     END-IF                                                               
082800                                                                          
082900     .                                                                    
083000     EJECT                                                                
083100 FBB-SIMULERA-INDEX  SECTION.                                             
083200     MOVE 'FBB-SIMULERA-INDEX '  TO CURRENT-SECTION                       
083300                                                                          
083400     MOVE ZERO               TO WS-TOT-SIMANT                             
083500     MOVE +1                 TO IX                                        
083600     PERFORM UNTIL IX > MAX-IX                                            
083700       MOVE SPAR-MSGI-SIMANT (IX)                                         
083800                             TO WS-ANTAL                                  
083900       ADD WS-ANTAL          TO WS-TOT-SIMANT                             
084000       ADD +1                TO IX                                        
084100     END-PERFORM                                                          
084200                                                                          
084300     IF SIM-ANTAL-JA                                                      
084400                                                                          
084500       MOVE +1               TO IX                                        
084600                                                                          
084700       PERFORM UNTIL IX > MAX-IX                                          
084800         MOVE SPAR-MSGI-SIMANT (IX)                                       
084900                             TO WS-ANTAL                                  
085000         COMPUTE WS-INDEX-DEC ROUNDED =                                   
085100                 WS-ANTAL / (WS-TOT-SIMANT / 12)                          
085200                  ON SIZE ERROR                                           
085300                      MOVE ZERO   TO WS-INDEX-DEC                         
085400         END-COMPUTE                                                      
085500         COMPUTE SPAR-MSGI-SIMIX (IX) ROUNDED =                           
085600                 WS-INDEX-DEC * 100                                       
085700         ADD +1              TO IX                                        
085800       END-PERFORM                                                        
085900                                                                          
086000       MOVE ZERO             TO WS-ANTAL                                  
086100       MOVE +1               TO IX                                        
086200       PERFORM UNTIL IX > MAX-IX                                          
086300         ADD SPAR-MSGI-SIMIX (IX)                                         
086400                               TO WS-ANTAL                                
086500         ADD +1              TO IX                                        
086600       END-PERFORM                                                        
086700                                                                          
086800****                                                                      
086900****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
087000****                                                                      
087100                                                                          
087200       IF WS-ANTAL > +1200                                                
087300         MOVE -1             TO WS-JUSTERA                                
087400       ELSE                                                               
087500         MOVE +1             TO WS-JUSTERA                                
087600       END-IF                                                             
087700                                                                          
087800       PERFORM UNTIL WS-ANTAL = +1200                                     
087900                                                                          
088000         MOVE +1             TO IX                                        
088100         PERFORM UNTIL WS-ANTAL = +1200                                   
088200         OR IX > MAX-IX                                                   
088300           ADD WS-JUSTERA    TO SPAR-MSGI-SIMIX (IX)                      
088400                                WS-ANTAL                                  
088500           ADD +1            TO IX                                        
088600         END-PERFORM                                                      
088700       END-PERFORM                                                        
088800                                                                          
088900                                                                          
089000     ELSE                                                                 
089100*      (SIM-INDEX-JA)                                                     
089200                                                                          
089300       MOVE +1               TO IX                                        
089400       MOVE ZERO             TO WS-TOT-SIMANT                             
089500                                                                          
089600       PERFORM UNTIL IX > MAX-IX                                          
089700                                                                          
089800         COMPUTE SPAR-MSGI-SIMANT (IX) ROUNDED =                          
089900*   MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                       
090000*   WS-INDEX ÄR I %, DÄRFÖR / 100                                         
090100                 (SPAR-MSGI-SIMIX (IX) * SLAG-KVPB-REF) / 100             
090200         ADD SPAR-MSGI-SIMANT (IX)                                        
090300                             TO WS-TOT-SIMANT                             
090400                                                                          
090500         ADD +1              TO IX                                        
090600                                                                          
090700       END-PERFORM                                                        
090800                                                                          
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200 FC-HAEMTA-INFO  SECTION.                                                 
091300     MOVE 'FC-HAEMTA-INFO '      TO CURRENT-SECTION                       
091400                                                                          
091500     IF SLAG-DASPSEA > 0                                                  
091600        MOVE SLAG-DASPSEA(3:6) TO MOD-TISPSEA                             
091700     ELSE                                                                 
091800        MOVE MFS-RENSA-FAELT   TO MOD-TISPSEA                             
091900     END-IF                                                               
092000                                                                          
092100     IF SLAG-TIMANSEA > ZERO                                              
092200       MOVE SLAG-TIMANSEA      TO MOD-TIMANSEA                            
092300     ELSE                                                                 
092400       MOVE MFS-RENSA-FAELT    TO MOD-TIMANSEA                            
092500     END-IF                                                               
092600                                                                          
092700     MOVE 'GB'                 TO W-IDSKYLT                               
092800     PERFORM IMS-GU-WDD311-BSEQ                                           
092900     IF SEGMENT-FINNS                                                     
093000        MOVE TEXT-BEART        TO MOD-BEART-ENG                           
093100     ELSE                                                                 
093200        MOVE MFS-RENSA-FAELT   TO MOD-BEART-ENG                           
093300     END-IF                                                               
093400                                                                          
093500*---                                                                      
093600                                                                          
093700     MOVE MSGI-IDDC-KEY    TO SEAS-IDDC                                   
093800     MOVE W-IDARTNR        TO SEAS-IDARTNR                                
093900     MOVE NEJ              TO SEAS-FLKVARTAL                              
094000                                                                          
094100     CALL W271SEAS USING SEAS-W271SEAS SEAS-WDK7-PCB                      
094200                                       SEAS-WDL7-PCB                      
094300                                       SEAS-WDL4-PCB                      
094400                                       SEAS-WDK6-PCB                      
094500                                                                          
094600     IF SEAS-KDSVAR = SPACE                                               
094700       MOVE SEAS-ANT-HIST-AR   TO MOD-ANT-HIST-AR                         
094800       MOVE SEAS-ANT-HIST-MAN  TO MOD-ANT-HIST-MAN                        
094900       MOVE SEAS-OSAKERHET     TO MOD-REOSAEK                             
095000                                                                          
095100       IF SEAS-SEASON-ARTIKEL = JA                                        
095200         MOVE 'Y'              TO MOD-FLSEASON                            
095300       ELSE                                                               
095400         MOVE 'N'              TO MOD-FLSEASON                            
095500       END-IF                                                             
095600                                                                          
095700       MOVE ZERO               TO WS-TOT-VALANT                           
095800       MOVE +1                 TO IX                                      
095900       PERFORM UNTIL IX > MAX-IX                                          
096000                                                                          
096100         COMPUTE WS-INDEX = 100 * SLAG-RESEASON (IX)                      
096200         MOVE WS-INDEX         TO MOD-RESEASON-VALIX (IX)                 
096300         COMPUTE WS-ANTAL ROUNDED = SLAG-RESEASON (IX) *                  
096400                                    SLAG-KVPB-REF                         
096500                                                                          
096600         MOVE WS-ANTAL         TO MOD-KVOI-VALANT (IX)                    
096700         ADD WS-ANTAL          TO WS-TOT-VALANT                           
096800         ADD +1                TO IX                                      
096900       END-PERFORM                                                        
097000                                                                          
097100       MOVE WS-TOT-VALANT      TO MOD-KVOI-TOT-VALANT                     
097200                                                                          
097300*----                                                                     
097400                                                                          
097500       MOVE ZERO               TO WS-TOT-HISANT                           
097600                                                                          
097700       MOVE +1                 TO IX                                      
097800       PERFORM UNTIL IX > MAX-IX                                          
097900                                                                          
098000         MOVE SEAS-RESEASON (IX) TO MOD-RESEASON-HISIX (IX)               
098100                                                                          
098200         MOVE SEAS-KVOI (IX)   TO MOD-KVOI-HISANT (IX)                    
098300                                  WS-KVOI                                 
098400         ADD WS-KVOI           TO WS-TOT-HISANT                           
098500         ADD +1                TO IX                                      
098600       END-PERFORM                                                        
098700                                                                          
098800       MOVE WS-TOT-HISANT      TO MOD-KVOI-TOT-HISANT                     
098900     ELSE                                                                 
099000*---   ARTIKELN SAKNAS WDL711, SE PGM W271SEAS                            
099100       MOVE ERR-ART-HIST-SAKNAS      TO MED-IDMFSFEL                      
099200       CALL WMEDKONV USING MED-WMEDAREA                                   
099300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
099400       PERFORM MFS-RENSA-FAELT-UT                                         
099500     END-IF                                                               
099600                                                                          
099700     .                                                                    
099800     EJECT                                                                
099900 G-KOLLA-INPUT SECTION.                                                   
100000     MOVE 'G-KOLLA-INPUT '  TO CURRENT-SECTION                            
100100                                                                          
100200     MOVE NEJ   TO SIM-INDEX-SW                                           
100300                   SIM-ANTAL-SW                                           
100400     MOVE JA    TO INDATA-SW                                              
100500                                                                          
100600                                                                          
100700     IF MID-INPUT = ALL '+'                                               
100800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
100900       CALL WMEDKONV USING MED-WMEDAREA                                   
101000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
101100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
101200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
101300       MOVE NEJ TO INDATA-SW                                              
101400     ELSE                                                                 
101500       PERFORM GA-CHECK-DC-FTG-USER                                       
101600       IF INDATA-OK                                                       
101700         IF MID-TISPSEA  = ALL '+'                                        
101800           MOVE MFS-NUM-FAELT-RAETT TO MOD-TISPSEA-ATTR                   
101900         ELSE                                                             
102000           IF MID-TISPSEA(1:1) = SPACE                                    
102100           OR MID-TISPSEA(2:1) = SPACE                                    
102200           OR MID-TISPSEA(3:1) = SPACE                                    
102300           OR MID-TISPSEA(4:1) = SPACE                                    
102400           OR MID-TISPSEA(5:1) = SPACE                                    
102500           OR MID-TISPSEA(6:1) = SPACE                                    
102600                                                                          
102700             IF MSGI-IDLAND-SPR = 'SE'                                    
102800               MOVE MED-4             TO MOD-TEMFSINF                     
102900             ELSE                                                         
103000               MOVE MED-3             TO MOD-TEMFSINF                     
103100             END-IF                                                       
103200             MOVE MFS-NUM-FAELT-FEL TO MOD-TISPSEA-ATTR                   
103300             MOVE NEJ TO INDATA-SW                                        
103400           ELSE                                                           
103500             MOVE 'AAMMDD'       TO DAT-KDDATFORM                         
103600             MOVE MID-TISPSEA    TO DAT-I-TIDATUM                         
103700                                                                          
103800             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
103900                                 DAT-O-TIDATUM DAT-KDSVAR                 
104000                                                                          
104100             IF DAT-KDSVAR-OK                                             
104200               MOVE MID-TISPSEA                                           
104300                              TO TMP1-YYMMDD                              
104400               MOVE DAGENS-DATUM                                          
104500                              TO TMP2-YYMMDD                              
104600               PERFORM WY2000P1                                           
104700               IF TMP1-YYMMDD < TMP2-YYMMDD                               
104800                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISPSEA-ATTR               
104900                 MOVE NEJ TO INDATA-SW                                    
105000               ELSE                                                       
105100                 MOVE MFS-NUM-FAELT-RAETT TO MOD-TISPSEA-ATTR             
105200               END-IF                                                     
105300             ELSE                                                         
105400               MOVE MFS-NUM-FAELT-FEL TO MOD-TISPSEA-ATTR                 
105500               MOVE NEJ TO INDATA-SW                                      
105600             END-IF                                                       
105700           END-IF                                                         
105800         END-IF                                                           
105900                                                                          
106000                                                                          
106100         MOVE +1                 TO IX                                    
106200         PERFORM UNTIL IX > MAX-IX                                        
106300           IF MID-RESEASON-SIMIX (IX) NOT = ALL '+'                       
106400             MOVE JA             TO SIM-INDEX-SW                          
106500                                                                          
106600             INSPECT MID-RESEASON-SIMIX (IX) REPLACING                    
106700                                    LEADING SPACE BY ZERO                 
106800             IF MID-RESEASON-SIMIX (IX) NUMERIC                           
106900               MOVE MID-RESEASON-SIMIX (IX)                               
107000                                 TO SPAR-MSGI-SIMIX (IX)                  
107100               MOVE MFS-NUM-FAELT-RAETT TO                                
107200                                       MOD-RESEASON-SIMIX-ATTR(IX)        
107300             ELSE                                                         
107400               MOVE MFS-NUM-FAELT-FEL TO                                  
107500                                       MOD-RESEASON-SIMIX-ATTR(IX)        
107600               MOVE NEJ TO INDATA-SW                                      
107700             END-IF                                                       
107800           END-IF                                                         
107900                                                                          
108000           IF MID-KVOI-SIMANT (IX) NOT = ALL '+'                          
108100             MOVE JA             TO SIM-ANTAL-SW                          
108200                                                                          
108300             INSPECT MID-KVOI-SIMANT (IX) REPLACING                       
108400                                    LEADING SPACE BY ZERO                 
108500             IF MID-KVOI-SIMANT (IX) NUMERIC                              
108600               MOVE MID-KVOI-SIMANT (IX) TO                               
108700                                       SPAR-MSGI-SIMANT(IX)               
108800               MOVE MFS-NUM-FAELT-RAETT TO                                
108900                                       MOD-KVOI-SIMANT-ATTR(IX)           
109000             ELSE                                                         
109100               MOVE MFS-NUM-FAELT-FEL TO                                  
109200                                       MOD-KVOI-SIMANT-ATTR(IX)           
109300               MOVE NEJ TO INDATA-SW                                      
109400             END-IF                                                       
109500           END-IF                                                         
109600                                                                          
109700           ADD +1                TO IX                                    
109800         END-PERFORM                                                      
109900                                                                          
110000                                                                          
110100         IF INDATA-FEL                                                    
110200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
110300           CALL WMEDKONV USING MED-WMEDAREA                               
110400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
110500           PERFORM MFS-ROER-EJ-FAELT-UT                                   
110600           PERFORM MFS-ROER-EJ-FAELT-IN                                   
110700         ELSE                                                             
110800           PERFORM GB-KOLLA-OM-OK-UPPDATERA                               
110900                                                                          
111000           IF INDATA-FEL                                                  
111100             IF MED-IDMFSFEL = SPACE                                      
111200               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
111300             END-IF                                                       
111400             CALL WMEDKONV USING MED-WMEDAREA                             
111500             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
111600             PERFORM MFS-ROER-EJ-FAELT-UT                                 
111700             PERFORM MFS-ROER-EJ-FAELT-IN                                 
111800           END-IF                                                         
111900         END-IF                                                           
112000       ELSE                                                               
112100         MOVE ERR-NOT-AUTH     TO MED-IDMFSFEL                            
112200         CALL WMEDKONV      USING MED-WMEDAREA                            
112300         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
112400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
112500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
112600       END-IF                                                             
112700     END-IF                                                               
112800     .                                                                    
112900     EJECT                                                                
113000 GA-CHECK-DC-FTG-USER SECTION.                                            
113100     MOVE 'GA-CHECK-DC-FTG-USER ' TO CURRENT-SECTION                      
113200                                                                          
113300     PERFORM IMS-GU-WDB601                                                
113400     IF SEGMENT-FINNS                                                     
113500        IF  DCS-NDC-CN                                                    
113600        OR (DCS-NDC-NA AND DCS-USA)                                       
113700           MOVE MSGI-IDFTG          TO WS-IDFTG                           
113800           IF (DCS-NDC-CN AND IDFTG-CN)                                   
113900           OR (DCS-NDC-NA AND IDFTG-US)                                   
114000           OR MSGI-IDFTG  = WC-IDFTG-PV                                   
114100              CONTINUE                                                    
114200           ELSE                                                           
114300              MOVE NEJ              TO INDATA-SW                          
114400           END-IF                                                         
114500        ELSE                                                              
114600           MOVE NEJ                 TO INDATA-SW                          
114700        END-IF                                                            
114800     ELSE                                                                 
114900        MOVE NEJ                    TO INDATA-SW                          
115000     END-IF                                                               
115100                                                                          
115200     .                                                                    
115300     EJECT                                                                
115400 GB-KOLLA-OM-OK-UPPDATERA  SECTION.                                       
115500     MOVE 'GB-KOLLA-OM-OK-UPPDATERA '  TO CURRENT-SECTION                 
115600                                                                          
115700**--   INTE MÖJLIGT ATT SIMULERA MED BÅDE ANTAL OCH INDEX                 
115800**--   SAMTIDIGT                                                          
115900                                                                          
116000     IF SIM-ANTAL-JA                                                      
116100     AND SIM-INDEX-JA                                                     
116200       MOVE NEJ TO INDATA-SW                                              
116300       MOVE ERR-KONFLIKT       TO MED-IDMFSFEL                            
116400                                                                          
116500       MOVE +1                 TO IX                                      
116600       PERFORM UNTIL IX > MAX-IX                                          
116700         IF MID-RESEASON-SIMIX (IX) NOT = ALL '+'                         
116800           MOVE MFS-NUM-FAELT-FEL                                         
116900                      TO MOD-RESEASON-SIMIX-ATTR (IX)                     
117000         END-IF                                                           
117100                                                                          
117200         IF MID-KVOI-SIMANT (IX) NOT = ALL '+'                            
117300           MOVE MFS-NUM-FAELT-FEL                                         
117400                      TO MOD-KVOI-SIMANT-ATTR (IX)                        
117500         END-IF                                                           
117600         ADD +1                TO IX                                      
117700       END-PERFORM                                                        
117800     ELSE                                                                 
117900       IF SIM-INDEX-JA                                                    
118000**************************************************************            
118100**------   KOLLA ATT SUMMAN BLIR 1200                                     
118200**************************************************************            
118300         MOVE ZERO             TO WS-SIMIX-SUM                            
118400         MOVE +1               TO IX                                      
118500         PERFORM UNTIL IX > MAX-IX                                        
118600           ADD SPAR-MSGI-SIMIX (IX)                                       
118700                               TO WS-SIMIX-SUM                            
118800           ADD +1              TO IX                                      
118900         END-PERFORM                                                      
119000                                                                          
119100         IF WS-SIMIX-SUM NOT = 1200                                       
119200           IF MSGI-IDLAND-SPR = 'SE'                                      
119300             MOVE WS-SIMIX-SUM TO MED-1-INDEX                             
119400             MOVE MED-1        TO MOD-TEMFSINF                            
119500           ELSE                                                           
119600             MOVE WS-SIMIX-SUM TO MED-2-INDEX                             
119700             MOVE MED-2        TO MOD-TEMFSINF                            
119800           END-IF                                                         
119900           MOVE +1           TO IX                                        
120000           PERFORM UNTIL IX > MAX-IX                                      
120100              MOVE MFS-ALFA-FAELT-FEL                                     
120200                              TO MOD-RESEASON-SIMIX-ATTR (IX)             
120300              ADD +1          TO IX                                       
120400           END-PERFORM                                                    
120500                                                                          
120600           MOVE NEJ TO INDATA-SW                                          
120700         END-IF                                                           
120800       END-IF                                                             
120900     END-IF                                                               
121000                                                                          
121100     .                                                                    
121200     EJECT                                                                
121300 H-UPPDATERA SECTION.                                                     
121400     MOVE 'H-UPPDATERA  '    TO CURRENT-SECTION                           
121500                                                                          
121600*** KINA SKALL ANVÄNDA MATERIALPRIS ISTÄLLET FÖR BESTÄLLNINGSPRIS         
121700*** NDC-CN OR LDC-CN FLYTTAR MATERIALPRIS TILL W271REFL                   
121800*** ---                                                                   
121900     INITIALIZE  REFL-W271REFL                                            
122000     MOVE DCS-IDLANDX2           TO W-IDLAND                              
122100     PERFORM IMS-GU-WDK712                                                
122200     IF SEGMENT-FINNS                                                     
122300       MOVE LART-PRMATRL         TO REFL-PRARTBES                         
122400     ELSE                                                                 
122500       MOVE ZERO                 TO REFL-PRARTBES                         
122600     END-IF                                                               
122700                                                                          
122800     PERFORM IMS-GHU-WDK711                                               
122900     IF SEGMENT-FINNS                                                     
123000                                                                          
123100       IF MID-TISPSEA NOT = ALL '+'                                       
123200         MOVE 'AAMMDD'           TO DAT-KDDATFORM                         
123300         MOVE MID-TISPSEA        TO DAT-I-TIDATUM                         
123400                                                                          
123500         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
123600                         DAT-O-TIDATUM DAT-KDSVAR                         
123700                                                                          
123800         IF DAT-KDSVAR-OK                                                 
123900           MOVE DAT-TISEKEL      TO SLAG-DASPSEA (1:2)                    
124000           MOVE MID-TISPSEA      TO SLAG-DASPSEA (3:6)                    
124100         ELSE                                                             
124200           MOVE ZERO             TO SLAG-DASPSEA                          
124300         END-IF                                                           
124400       END-IF                                                             
124500                                                                          
124600       MOVE +1                   TO IX                                    
124700       MOVE ZERO                 TO WS-TOT-VALANT                         
124800       PERFORM UNTIL IX > MAX-IX                                          
124900                                                                          
125000         MOVE SPAR-MSGI-SIMIX (IX)                                        
125100                                 TO WS-INDEX                              
125200                                    MOD-RESEASON-VALIX (IX)               
125300         COMPUTE SLAG-RESEASON (IX) = WS-INDEX / 100                      
125400         COMPUTE WS-ANTAL ROUNDED   = SLAG-RESEASON (IX) *                
125500                                      SLAG-KVPB-REF                       
125600         MOVE WS-ANTAL           TO MOD-KVOI-VALANT (IX)                  
125700         ADD WS-ANTAL            TO WS-TOT-VALANT                         
125800         ADD +1                  TO IX                                    
125900       END-PERFORM                                                        
126000                                                                          
126100       IF NOT   (SLAG-RESEASON (1)  = 1.00                                
126200       AND       SLAG-RESEASON (2)  = 1.00                                
126300       AND       SLAG-RESEASON (3)  = 1.00                                
126400       AND       SLAG-RESEASON (4)  = 1.00                                
126500       AND       SLAG-RESEASON (5)  = 1.00                                
126600       AND       SLAG-RESEASON (6)  = 1.00                                
126700       AND       SLAG-RESEASON (7)  = 1.00                                
126800       AND       SLAG-RESEASON (8)  = 1.00                                
126900       AND       SLAG-RESEASON (9)  = 1.00                                
127000       AND       SLAG-RESEASON (10) = 1.00                                
127100       AND       SLAG-RESEASON (11) = 1.00                                
127200       AND       SLAG-RESEASON (12) = 1.00)                               
127300       AND       SLAG-FLREFBEO = JA                                       
127400         MOVE NEJ              TO SLAG-FLREFBEO                           
127500       END-IF                                                             
127600                                                                          
127700       MOVE WS-TOT-VALANT      TO MOD-KVOI-TOT-VALANT                     
127800                                                                          
127900       MOVE DAGENS-DATUM       TO SLAG-TIMANSEA                           
128000                                                                          
128100       PERFORM HA-BERAKNA-REFPKT                                          
128200                                                                          
128300       PERFORM IMS-REPL-WDK711                                            
128400                                                                          
128500       MOVE INF-UPDATE-DONE    TO MED-IDMFSINF                            
128600       CALL WMEDKONV        USING MED-WMEDAREA                            
128700       MOVE MED-MFSINF         TO MOD-TEMFSINF                            
128800       PERFORM MFS-FORM-ATTR                                              
128900       PERFORM MFS-RENSA-FAELT-IN                                         
129000     ELSE                                                                 
129100       MOVE ERR-NO-UPDATE  TO MED-IDMFSFEL                                
129200       CALL WMEDKONV USING MED-WMEDAREA                                   
129300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
129400       PERFORM MFS-FORM-ATTR                                              
129500       PERFORM MFS-RENSA-FAELT-IN                                         
129600     END-IF                                                               
129700     .                                                                    
129800     EJECT                                                                
129900 HA-BERAKNA-REFPKT SECTION.                                               
130000     MOVE 'HA-BERAKNA-REFPKT '   TO CURRENT-SECTION                       
130100                                                                          
130200     MOVE SLAG-TIREFPKT      TO TMP1-YYMMDD                               
130300     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
130400     PERFORM WY2000P1                                                     
130500     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
130600                                                                          
130700*                                                                         
130800*--- KVREFPKT FRÅN BASEN GÄLLER PGA MANUELLT DATUM ÄR SATT                
130900*                                                                         
131000       CONTINUE                                                           
131100                                                                          
131200     ELSE                                                                 
131300       MOVE ZERO             TO REFL-NDC-KVDAGAR-TBT-DC                   
131400                                                                          
131500       MOVE W-IDDC           TO REFL-IDDC                                 
131600       MOVE W-IDARTNR        TO REFL-IDARTNR                              
131700                                                                          
131800*-- LÄGGER MAN SPACE HÄR (NDC-CN) - TAS INGA LEDTIDER MED I               
131900*-- W271REFL.SLAG-IDDC-REF = SPACE FÖR LOCAL SOURCED PARTS.               
132000       MOVE SLAG-IDDC-REF    TO REFL-IDDC-REF                             
132100                                                                          
132200       MOVE SLAG-IDREFTAB    TO REFL-IDREFTAB                             
132300       MOVE SLAG-FLWILSON    TO REFL-FLWILSON                             
132400                                                                          
132500       MOVE SLAG-TIREFPKT    TO TMP1-YYMMDD                               
132600       MOVE DAGENS-DATUM     TO TMP2-YYMMDD                               
132700       PERFORM WY2000P1                                                   
132800       IF TMP1-YYMMDD   >= TMP2-YYMMDD                                    
132900*                                                                         
133000*----  MANUELL PÅFYLLNADSPUNKT GÄLLER                                     
133100*                                                                         
133200         MOVE SLAG-KVREFPKT  TO REFL-IN-KVREFPKT                          
133300       ELSE                                                               
133400         MOVE ZERO           TO REFL-IN-KVREFPKT                          
133500       END-IF                                                             
133600                                                                          
133700       MOVE SLAG-TIREFPAF    TO TMP1-YYMMDD                               
133800       MOVE DAGENS-DATUM     TO TMP2-YYMMDD                               
133900       PERFORM WY2000P1                                                   
134000       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
134100*                                                                         
134200*----  MANUELL PÅFYLLNADSKVANTITET ÄR SATT                                
134300*                                                                         
134400         MOVE SLAG-KVREFBER                                               
134500                             TO REFL-IN-KVREFBER                          
134600       ELSE                                                               
134700         MOVE ZERO           TO REFL-IN-KVREFBER                          
134800       END-IF                                                             
134900                                                                          
135000       MOVE SLAG-IDLEVNR     TO REFL-IN-IDLEVNR-DC                        
135100                                                                          
135200*                                                                         
135300*----  SÄKERHETSLAGER KINA = REFILLPUNKT I REFILLEN.                      
135400*----  SÄKERHETSLAGER KINA:BEHOV INOM TRANSP.TID EJ INKLUDERAT.           
135500*                                                                         
135600       MOVE ZERO             TO REFL-NDC-KVDAGAR-TBT-DC                   
135700                                                                          
135800                                                                          
135900       MOVE +1               TO IX                                        
136000       PERFORM UNTIL IX > 12                                              
136100          MOVE SLAG-RESEASON(IX)                                          
136200                             TO REFL-RESEASON(IX)                         
136300          ADD +1             TO IX                                        
136400       END-PERFORM                                                        
136500                                                                          
136600       MOVE SLAG-FLFLYG      TO REFL-FLFLYG                               
136700                                                                          
136800       INITIALIZE UTUP-W271UTUP                                           
136900       MOVE 004              TO UTUP-KDCALL                               
137000       MOVE W-IDARTNR        TO UTUP-IDARTNR                              
137100       MOVE W-IDDC           TO UTUP-IDDC                                 
137200       MOVE SLAG-IDDC-REF    TO UTUP-IDDC-REF                             
137300                                                                          
137400       CALL W271UTUP USING UTUP-W271UTUP                                  
137500                           UTUP1-WDK7-PCB                                 
137600                           UTUP1-WDB6-PCB                                 
137700                           UTUP1-UTIL-WDK6-PCB                            
137800                           UTUP1-UTIL-WDK7-PCB                            
137900                           UTUP1-UTIL-WDB6-PCB                            
138000       IF UTUP-KDSVAR-OK                                                  
138100          MOVE UTUP-LEADTID-BEHOV                                         
138200                             TO REFL-IN-LEADTID-BEHOV                     
138300       ELSE                                                               
138400          DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                            
138500          CALL FELLOG                                                     
138600       END-IF                                                             
138700                                                                          
138800       CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                    
138900                                         REFL-WDB6-PCB                    
139000                                         REFL-WDK7-PCB                    
139100                                         REFL-UTIL-WDK6-PCB               
139200                                         REFL-UTIL-WDK7-PCB               
139300                                         REFL-UTIL-WDB6-PCB               
139400                                                                          
139500       MOVE REFL-KVREFPKT    TO SLAG-KVREFPKT                             
139600                                                                          
139700     END-IF                                                               
139800     .                                                                    
139900     EJECT                                                                
140000 I-VISA-SIM-INFO   SECTION.                                               
140100     MOVE 'I-VISA-SIM-INFO '    TO CURRENT-SECTION                        
140200                                                                          
140300     MOVE ZERO         TO WS-TOT-SIMANT                                   
140400     MOVE +1             TO IX                                            
140500     PERFORM UNTIL IX > MAX-IX                                            
140600       MOVE SPAR-MSGI-SIMIX (IX)                                          
140700                       TO MOD-RESEASON-SIMIX (IX)                         
140800       MOVE SPAR-MSGI-SIMANT (IX)                                         
140900                       TO MOD-KVOI-SIMANT (IX)                            
141000       ADD SPAR-MSGI-SIMANT (IX)                                          
141100                       TO WS-TOT-SIMANT                                   
141200       ADD +1          TO IX                                              
141300     END-PERFORM                                                          
141400     MOVE WS-TOT-SIMANT  TO MOD-KVOI-TOT-SIMANT                           
141500                                                                          
141600     MOVE '002'      TO MSGI-KDCALL                                       
141700     MOVE '2405'     TO SPAR-IDTRANS                                      
141800     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
141900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
142000                                                                          
142100     .                                                                    
142200     EJECT                                                                
142300                                                                          
142400 MFS-RENSA-FAELT-UT SECTION.                                              
142500                                                                          
142600*    --- ALLA UTDATA-FÄLT                                                 
142700     MOVE MFS-RENSA-FAELT TO MOD-TIMANSEA                                 
142800                             MOD-FLSEASON                                 
142900                             MOD-BEART-ENG                                
143000                             MOD-REOSAEK                                  
143100                             MOD-ANT-HIST-AR                              
143200                             MOD-ANT-HIST-MAN                             
143300                             MOD-KVOI-TOT-VALANT                          
143400                             MOD-KVOI-TOT-SIMANT                          
143500                             MOD-KVOI-TOT-HISANT                          
143600                                                                          
143700     MOVE +1                 TO IX                                        
143800     PERFORM UNTIL IX > MAX-IX                                            
143900       MOVE MFS-RENSA-FAELT TO MOD-RESEASON-VALIX (IX)                    
144000                               MOD-KVOI-VALANT (IX)                       
144100       ADD +1                TO IX                                        
144200     END-PERFORM                                                          
144300                                                                          
144400     MOVE +1                 TO IX                                        
144500     PERFORM UNTIL IX > MAX-IX                                            
144600       MOVE MFS-RENSA-FAELT TO MOD-RESEASON-HISIX (IX)                    
144700                               MOD-KVOI-HISANT (IX)                       
144800       ADD +1                TO IX                                        
144900     END-PERFORM                                                          
145000                                                                          
145100     .                                                                    
145200     SKIP3                                                                
145300 MFS-RENSA-FAELT-IN SECTION.                                              
145400                                                                          
145500*    --- ALLA INDATA-FÄLT                                                 
145600     MOVE MFS-RENSA-FAELT   TO MOD-TISPSEA                                
145700                                                                          
145800     MOVE +1                TO IX                                         
145900     PERFORM UNTIL IX > MAX-IX                                            
146000       MOVE MFS-RENSA-FAELT TO MOD-RESEASON-SIMIX (IX)                    
146100                               MOD-KVOI-SIMANT (IX)                       
146200       ADD +1               TO IX                                         
146300     END-PERFORM                                                          
146400                                                                          
146500     .                                                                    
146600     EJECT                                                                
146700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
146800                                                                          
146900*    --- ALLA UTDATA-FÄLT                                                 
147000     MOVE MFS-ROER-EJ-FAELT TO MOD-TIMANSEA                               
147100                               MOD-FLSEASON                               
147200                               MOD-BEART-ENG                              
147300                               MOD-REOSAEK                                
147400                               MOD-ANT-HIST-AR                            
147500                               MOD-ANT-HIST-MAN                           
147600                               MOD-KVOI-TOT-VALANT                        
147700                               MOD-KVOI-TOT-SIMANT                        
147800                               MOD-KVOI-TOT-HISANT                        
147900                                                                          
148000     MOVE +1                 TO IX                                        
148100     PERFORM UNTIL IX > MAX-IX                                            
148200       MOVE MFS-ROER-EJ-FAELT TO MOD-RESEASON-VALIX (IX)                  
148300                                 MOD-KVOI-VALANT (IX)                     
148400       ADD +1                TO IX                                        
148500     END-PERFORM                                                          
148600                                                                          
148700     MOVE +1                 TO IX                                        
148800     PERFORM UNTIL IX > MAX-IX                                            
148900       MOVE MFS-ROER-EJ-FAELT TO MOD-RESEASON-HISIX (IX)                  
149000                                 MOD-KVOI-HISANT (IX)                     
149100       ADD +1                TO IX                                        
149200     END-PERFORM                                                          
149300                                                                          
149400     .                                                                    
149500     SKIP3                                                                
149600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
149700                                                                          
149800*    --- ALLA INDATA-FÄLT                                                 
149900     MOVE MFS-ROER-EJ-FAELT TO MOD-TISPSEA                                
150000                                                                          
150100     MOVE +1                TO IX                                         
150200     PERFORM UNTIL IX > MAX-IX                                            
150300       MOVE MFS-ROER-EJ-FAELT TO MOD-RESEASON-SIMIX (IX)                  
150400                                 MOD-KVOI-SIMANT (IX)                     
150500       ADD +1               TO IX                                         
150600     END-PERFORM                                                          
150700     .                                                                    
150800     EJECT                                                                
150900 MFS-FORM-ATTR SECTION.                                                   
151000                                                                          
151100*    --- ALLA INDATA-FÄLT                                                 
151200     MOVE MFS-FORMATETS-ATTR TO MOD-TISPSEA-ATTR                          
151300                                                                          
151400     MOVE +1                 TO IX2                                       
151500     PERFORM UNTIL IX2 > 12                                               
151600       MOVE MFS-FORMATETS-ATTR                                            
151700                                TO MOD-RESEASON-SIMIX-ATTR (IX2)          
151800                                   MOD-KVOI-SIMANT-ATTR    (IX2)          
151900       ADD +1                   TO IX2                                    
152000     END-PERFORM                                                          
152100     .                                                                    
152200     SKIP2                                                                
152300 MFS-LAES-IN-IGEN SECTION.                                                
152400                                                                          
152500*    --- ALLA INDATA-FÄLT                                                 
152600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISPSEA-ATTR                       
152700                                                                          
152800                                                                          
152900     MOVE +1                 TO IX2                                       
153000     PERFORM UNTIL IX2 > 12                                               
153100       MOVE MFS-ADD-LAES-IN-FAELT                                         
153200                                TO MOD-RESEASON-SIMIX-ATTR (IX2)          
153300                                   MOD-KVOI-SIMANT-ATTR    (IX2)          
153400       ADD +1                   TO IX2                                    
153500     END-PERFORM                                                          
153600                                                                          
153700     .                                                                    
153800     EJECT                                                                
153900* --- IMS SEKTIONER ---                                                   
154000     SKIP3                                                                
154100 IMS-GET-MSG SECTION.                                                     
154200     MOVE 'IMS-GET-MSG '  TO DBS-SECTION                                  
154300                                                                          
154400     MOVE '  QC' TO GODK-STATUSKODER                                      
154500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
154600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
154700     PERFORM IMS-STATUSKONTROLL                                           
154800     .                                                                    
154900     SKIP3                                                                
155000 IMS-INSERT-MSG SECTION.                                                  
155100     MOVE 'IMS-INSERT-MSG '  TO DBS-SECTION                               
155200                                                                          
155300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
155400     MOVE SPACE TO GODK-STATUSKODER                                       
155500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
155600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
155700     PERFORM IMS-STATUSKONTROLL                                           
155800     .                                                                    
155900     EJECT                                                                
156000 IMS-GU-WDB601 SECTION.                                                   
156100     MOVE 'IMS-GU-WDB601 '   TO DBS-SECTION                               
156200                                                                          
156300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
156400          DELIMITED BY SIZE INTO SSA1                                     
156500     MOVE '  GE' TO GODK-STATUSKODER                                      
156600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
156700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
156800     PERFORM IMS-STATUSKONTROLL                                           
156900     .                                                                    
157000     EJECT                                                                
157100 IMS-GU-WDK711 SECTION.                                                   
157200     MOVE 'IMS-GU-WDK711 '   TO DBS-SECTION                               
157300                                                                          
157400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
157500          DELIMITED BY SIZE INTO SSA1                                     
157600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
157700          DELIMITED BY SIZE INTO SSA2                                     
157800     MOVE '  GE' TO GODK-STATUSKODER                                      
157900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
158000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
158100     PERFORM IMS-STATUSKONTROLL                                           
158200     .                                                                    
158300     EJECT                                                                
158400 IMS-GU-WDK712   SECTION.                                                 
158500     MOVE 'IMS-GU-WDK712 '   TO DBS-SECTION                               
158600                                                                          
158700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
158800          DELIMITED BY SIZE INTO SSA1                                     
158900     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
159000          DELIMITED BY SIZE INTO SSA2                                     
159100     MOVE '  GE' TO GODK-STATUSKODER                                      
159200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
159300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
159400     PERFORM IMS-STATUSKONTROLL                                           
159500     .                                                                    
159600     EJECT                                                                
159700 IMS-GHU-WDK711 SECTION.                                                  
159800     MOVE 'IMS-GHU-WDK711 '   TO DBS-SECTION                              
159900                                                                          
160000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
160100          DELIMITED BY SIZE INTO SSA1                                     
160200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
160300          DELIMITED BY SIZE INTO SSA2                                     
160400     MOVE '  ' TO GODK-STATUSKODER                                        
160500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711  SSA1 SSA2             
160600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900     SKIP3                                                                
161000 IMS-REPL-WDK711 SECTION.                                                 
161100     MOVE 'IMS-REPL-WDK711 '   TO DBS-SECTION                             
161200                                                                          
161300     MOVE '  ' TO GODK-STATUSKODER                                        
161400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
161500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
161600     PERFORM IMS-STATUSKONTROLL                                           
161700     .                                                                    
161800     EJECT                                                                
161900 IMS-GU-WDD311-BSEQ SECTION.                                              
162000     MOVE 'IMS-GU-WDD311-BSEQ '  TO DBS-SECTION                           
162100                                                                          
162200     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
162300          DELIMITED BY SIZE INTO SSA1                                     
162400     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
162500          DELIMITED BY SIZE INTO SSA2                                     
162600     MOVE '  GE' TO GODK-STATUSKODER                                      
162700     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
162800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
162900     PERFORM IMS-STATUSKONTROLL                                           
163000     .                                                                    
163100     EJECT                                                                
163200 IMS-STATUSKONTROLL SECTION.                                              
163300                                                                          
163400     SET STATUS-IX TO 1                                                   
163500     SEARCH GODK-STATUS                                                   
163600       AT END                                                             
163700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
163800         DELIMITED BY SIZE INTO FELTEXT                                   
163900         CALL FELLOG                                                      
164000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
164100         CONTINUE                                                         
164200     END-SEARCH                                                           
164300     .                                                                    
164400     EJECT                                                                
164500*    -COPY WY2000P1                                                       
