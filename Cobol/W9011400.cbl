000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W9011400.                                                
000400 AUTHOR.         EVA LUNDELL.                                             
000500 DATE-WRITTEN.   95/05/03.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*        PROGRAMMET VISAR LEVERANSINFORMATION FÖR CDC                     
000900*        FÖR EXTERNA ANVÄNDARE                                            
001000*        OBS TVÅ INGÅNGAR :                                               
001100*                FRÅN VOLVO VISION = TRANS W90114T                        
001200*                FRÅN PULS         = TRANS W9T114                         
001300*                                                                         
001400*        PROGRAMMET LÄSER              WDP7                               
001500*                                      WDD3                               
001600*                                      WDD9                               
001700*                                      WDK6                               
001800*                                      WDK9                               
001900*                                      WDA5                               
002000*                                      WDL2                               
002100*                                                                         
002200*    INDATA.                                                              
002300*                                                                         
002400*      FRÅN PULS                                                          
002500*        TRANSAKTION: W9T114                                              
002600*        MID:         W9I11401                                            
002700*                                                                         
002800*      FRÅN VOLVO VISION                                                  
002900*        TRANSAKTION: W90114T                                             
003000*        MID:         W9I114V1                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*                                                                         
003400*      FRÅN PULS                                                          
003500*        MOD:         W9O11401                                            
003600*                                                                         
003700*      FRÅN VOLVO VISION                                                  
003800*        MOD:         W9O114V1                                            
      *    STORY 2235799 / ADD 'REFILL PART' MESSAGE                            
      *                    ADD A "FX" PRESS TO JUMP TO 911X SCREENS             
      *    STORY 2574989 / ABEND FIX IF USER ENTERS NON NUMERIC ARTNR           
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400 WORKING-STORAGE SECTION.                                                 
004500*    -COPY WY2000W3                                                       
004600     SKIP3                                                                
004700*    -COPY WY2000W2                                                       
004800     SKIP3                                                                
004900*    -COPY WY2000W1                                                       
005000     SKIP3                                                                
005100 77  IDPGM                       PIC X(08)   VALUE 'W9011400'.            
005200                                                                          
005300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005500                                                                          
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 77  KVAK-SALDO                  PIC S9(7)  COMP-3 VALUE ZERO.            
005900 77  NASTA-INLEV                 PIC S9(7)  COMP-3.                       
006000 77  VECKOR-TILL-PUBLICERING     PIC S9(7)  COMP-3.                       
006100 77  SISTA-INLEV-DATUM           PIC 9(6).                                
006200 77  TABELL-IX                   PIC S9(5)  COMP-3.                       
006300 77  MOD-IX                      PIC S9(5)  COMP-3.                       
006400 77  SPARA-IX                    PIC S9(5)  COMP-3.                       
006500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +443 COMP SYNC.         
006600 77  MAX-ANTAL-RADER             PIC S9(2)  VALUE ZERO.                   
006700 77  SPARA-LAGSTA-DATUM          PIC S9(7)  COMP-3.                       
006800 77  SPARA-IDLEVNR               PIC X(5).                                
006900 77  SPARA-TIDISPIN              PIC 9(6).                                
007000 77  SPARA-TIAVROP               PIC 9(5).                                
007100 77  SPARA-TIAVROP-DISP          PIC 9(4).                                
007200 77  SPAR-IDTRANS                PIC X(4)   VALUE '9114'.                 
007300 77  SPAR-DISPLAY                PIC X(20)  VALUE 'DISPLAY-TEST'.         
007400 01  WS.                                                                  
007500   03 WS-RESTKVANT-DAG           PIC S9(7)  VALUE ZERO.                   
007600   03 WS-RESTKVANT-BULK          PIC S9(7)  VALUE ZERO.                   
007700   03 WS-RETUR                   PIC S9(7)  COMP-3 VALUE ZERO.            
007800   03 WS-AKT-TIAA                PIC 9(2)   VALUE ZERO.                   
007900   03 WS-AKT-TIMM                PIC 9(2)   VALUE ZERO.                   
008000   03 WS-TAB-AVROP               OCCURS 5.                                
008100      05 WS-TAB-TIAAMMDD         PIC 9(6)   VALUE ZERO.                   
008200      05 WS-TAB-KVAVROP          PIC 9(7)   VALUE ZERO.                   
008300   03 WS-TIAAVV                  PIC 9(4)   VALUE ZERO.                   
008400   03 WS-TIAAVVD                 PIC 9(5).                                
008500   03 FILLER REDEFINES           WS-TIAAVVD.                              
008600      05 WS-TIAA                 PIC 9(2).                                
008700      05 WS-TIVV                 PIC 9(2).                                
008800      05 WS-TID                  PIC 9.                                   
008900   03 WS-IDARTNR-NUM             PIC 9(9)   VALUE ZERO.                   
008900   03 WS-IDARTNR                 PIC X(9)   VALUE ZERO.                   
009000                                                                          
009100*01  -COPY WWDCKONS                                                       
009110                                                                          
009120 01  WS-TEMFSINF.                                                         
009200   03 WS-A                       PIC X      VALUE SPACE.                  
009300                                                                          
009400 01  W-TIDISPIN.                                                          
009500     03  W-TIDISPIN-DATUM        PIC 9(5).                                
009600     03  W-TIDISPIN-GRP REDEFINES W-TIDISPIN-DATUM.                       
009700         05  W-TIDISPIN-AA       PIC 9(2).                                
009800         05  W-TIDISPIN-VV       PIC 9(2).                                
009900         05  W-TIDISPIN-DD       PIC 9(1).                                
010000                                                                          
010100 01  W-AVROP.                                                             
010200     03  W-AVROP-DATUM           PIC 9(5).                                
010300     03  W-AVROP-GRP REDEFINES W-AVROP-DATUM.                             
010400         05  W-AVROP-AA          PIC 9(2).                                
010500         05  W-AVROP-VV          PIC 9(2).                                
010600         05  W-AVROP-DD          PIC 9(1).                                
010700     03  W-AVROP-FRAM-GRP        PIC 9(5).                                
010800     03  W-AVROP-FRAM REDEFINES W-AVROP-FRAM-GRP.                         
010900         05  W-FRAM-AAVV        PIC 9(4).                                 
011000         05  W-FRAM-D           PIC 9(1).                                 
011100                                                                          
011200 01  W-DAGENS-DATUM.                                                      
011300     03  W-DAGENS-AAVVD          PIC 9(5).                                
011400     03  W-DAGENS-GRP REDEFINES W-DAGENS-AAVVD.                           
011500         05  W-DAGENS-AAVV       PIC 9(4).                                
011600         05  W-DAGENS-DD         PIC 9(1).                                
011700                                                                          
011800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011900                                                                          
012000                                                                          
012100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012200     88  NYCKLAR-OK                          VALUE 'J'.                   
012300     88  NYCKLAR-FEL                         VALUE 'N'.                   
012400                                                                          
012500 77  FLAGGA-WDD9                 PIC X       VALUE 'J'.                   
012600                                                                          
012700 77  FLAGGA-WDD924               PIC X       VALUE 'J'.                   
012800     88  WDD924-FINNS                        VALUE 'J'.                   
012900     88  WDD924-FINNS-INTE                   VALUE 'N'.                   
013000                                                                          
013100 77  FLAGGA-WDD925               PIC X       VALUE 'J'.                   
013200     88  WDD925-FINNS                        VALUE 'J'.                   
013300     88  WDD925-FINNS-INTE                   VALUE 'N'.                   
013400                                                                          
013500                                                                          
013600 77  FLAGGA-TRANS-TYP            PIC X       VALUE 'J'.                   
013700     88  PULS-TRANS                          VALUE 'J'.                   
013800     88  VOLVO-VISION-TRANS                  VALUE 'N'.                   
013900                                                                          
014000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014100     88  EGEN-MID                            VALUE '9114'.                
014200     88  GODK-MID                            VALUE '9111' '9112'          
014300                                                   '9113' '9114'          
014400                                                   '9115' '9116'          
014500                                                   '9117' '9118'          
014600                                                   '9119'.                
014700     88  HELP-MID                            VALUE '0551'.                
014800                                                                          
014900                                                                          
015000 01  DATUM.                                                               
015100     05  DAGENS-DATUM            PIC S9(7) COMP-3.                        
015200                                                                          
015300 01  LEVERANSBESKED-TABELL.                                               
015400     03  TABELL  OCCURS 100.                                              
015500         05  TILEVBSK-DISP           PIC 9(6).                            
015600         05  KVAVIS-BSKKVAR          PIC 9(7).                            
015700         05  TAB-IDLEVNR             PIC X(5).                            
015800                                                                          
015900                                                                          
016000                                                                          
016100     EJECT                                                                
016200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016300 01  GENERELLA-SUBPROGRAM.                                                
016400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016900     EJECT                                                                
017000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017100*01 -COPY WMSGINIT                                                        
017200     EJECT                                                                
017300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017400*01 -COPY WMEDAREA                                                        
017500                                                                          
017600     SKIP3                                                                
017700 01  MESSAGE-CODES.                                                       
017800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017900     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
018000     03  ERR-ARTIKEL-UTGANGEN    PIC X(3)    VALUE '018'.                 
018100     03  ERR-NO-ORDER            PIC X(3)    VALUE '248'.                 
018200     03  ERR-NO-BACKORDERS       PIC X(3)    VALUE '403'.                 
           03  ERR-REFILL-PART         PIC X(3)    VALUE '434'.                 
018300     EJECT                                                                
018400*01  -COPY WDATAREA                                                       
018500     EJECT                                                                
018600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018700*                                                                         
018800 01  FILLER                      PIC X(16)  VALUE 'MID-AREA PULS'.        
018900     SKIP3                                                                
019000*01  MID -COPY W9I11401 -PRE MID-                                         
019100     EJECT                                                                
019200*                                                                         
019300 01  FILLER                      PIC X(16)  VALUE 'MID-AREA     '.        
019400 01  FILLER                      PIC X(16)  VALUE 'VOLVO VISION '.        
019500     SKIP3                                                                
019600*01  MID -COPY W9I114V1 -PRE MID2-                                        
019700*         IDVTYP ÄR INLAGD FÖR FRAMTIDA BRUK, OM VOLVO VISION             
019800*         KOMMER ATT KÖRA MED OLIKA VERSIONER MOT PULS                    
019900*         AV DETTA PROGRAM DVS OLIKA MID ELLER MOD'AR                     
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020200     SKIP3                                                                
020300*01  -COPY WMSGAREA                                                       
020400     EJECT                                                                
020500*                                           MOD FÖR PULS                  
020600     03  MOD REDEFINES MSG-AREA.                                          
020700*      05  -COPY W9O11401 -PRE MOD-                                       
020800     EJECT                                                                
020900*                                           MOD FÖR VOLVO VISION          
021000     03  MOD REDEFINES MSG-AREA.                                          
021100*      05  -COPY W9O114V1 -PRE MOD2-                                      
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021400     SKIP3                                                                
021500*01  -COPY WMFSAREA                                                       
021600     EJECT                                                                
021700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021800*                                                                         
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100     SKIP3                                                                
022200 01  NYCKLAR-TILL-DLI.                                                    
022300     03  W-IDARTNR-X.                                                     
022400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
022500     03  W-WDD901KY-X.                                                    
022510         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
022520         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
022600     03  W-IDSKYLT-X.                                                     
022700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
022800     03  W-IDLEVNR-X.                                                     
022900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
023000     03  ALT-IDLEVNR-X.                                                   
023100         05  ALT-IDLEVNR         PIC X(5)    VALUE SPACE.                 
023200     03  W-TILEVBSK-X.                                                    
023300         05  W-TILEVBSK          PIC S9(7)   VALUE ZERO COMP-3.           
023400     03  W-IDLEVBSK-X.                                                    
023500         05  W-IDLEVBSK          PIC S9(1)   VALUE ZERO COMP-3.           
023600     03  W-KDAVROP-X.                                                     
023700         05  W-KDAVROP           PIC S9(1)   VALUE +2   COMP-3.           
023800     03  W-KDSEGKEY-X.                                                    
023900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
024000     03  W-IDPTYP-X.                                                      
024100       05  W-IDPTYP      PIC X(3)   VALUE SPACE.                          
024200                                                                          
024300     03  W-WDA5A1KY-MIN.                                                  
024400         05  W-IDARTNR-N3-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
024500         05  FILLER               PIC X(35).                              
025400                                                                          
025500     03  W-WDA5A1KY-MAX.                                                  
025600         05  W-IDARTNR-N3-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
025700         05  FILLER               PIC X(35).                              
026600     SKIP2                                                                
026700*    --- STATUS-KOD FRÅN IMS                                              
026800 01  STATUS-WS                   PIC XX.                                  
026900     88  SEGMENT-FINNS                       VALUE '  '.                  
027000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027200     SKIP2                                                                
027300 01  GODK-STATUSKODER.                                                    
027400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027500     SKIP3                                                                
027600 01  SSA1                        PIC X(128).                              
027700 01  SSA2                        PIC X(128).                              
027800 01  SSA3                        PIC X(128).                              
027900     EJECT                                                                
028000*    --- IMS FUNKTIONSKODER                                               
028100*01  -COPY W0003                                                          
028200     EJECT                                                                
028300*    ---  DLI INPUT-OUTPUT AREA                                           
028400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
028500     SKIP3                                                                
028600                                                                          
028700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
028800 01  DLI-IO-WDD301.                                                       
028900*    03  -COPY WDD301                                                     
029000     EJECT                                                                
029100                                                                          
029200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
029300 01  DLI-IO-WDD311.                                                       
029400*    03  -COPY WDD311                                                     
029500     EJECT                                                                
029600                                                                          
029700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD901'.                      
029800 01  DLI-IO-WDD901.                                                       
029900*    03  -COPY WDD901                                                     
030000     EJECT                                                                
030100                                                                          
030200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD905'.                      
030300 01  DLI-IO-WDD905.                                                       
030400*    03  -COPY WDD905                                                     
030500     EJECT                                                                
030600                                                                          
030700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD924'.                      
030800 01  DLI-IO-WDD924.                                                       
030900*    03  -COPY WDD924                                                     
031000     EJECT                                                                
031100                                                                          
031200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD925'.                      
031300 01  DLI-IO-WDD925.                                                       
031400*    03  -COPY WDD925                                                     
031500     EJECT                                                                
031600                                                                          
031700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
031800 01  DLI-IO-WDK601.                                                       
031900*    03  -COPY WDK601                                                     
032000     EJECT                                                                
032100                                                                          
032200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
032300 01  DLI-IO-WDK611.                                                       
032400*    03  -COPY WDK611                                                     
032500     EJECT                                                                
032600                                                                          
032700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK901'.                      
032800 01  DLI-IO-WDK901.                                                       
032900*    03  -COPY WDK901                                                     
033000     EJECT                                                                
033100                                                                          
033200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDA5A1'.                      
033300 01  DLI-IO-WDA5A1.                                                       
033400*    03  -COPY WDA5A1                                                     
033500     EJECT                                                                
033600                                                                          
033700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL201'.                      
033800 01  DLI-IO-WDL201.                                                       
033900*    03  -COPY WDL201                                                     
034000     EJECT                                                                
034100                                                                          
034200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL221'.                      
034300 01  DLI-IO-WDL221.                                                       
034400*    03  -COPY WDL221                                                     
034500                                                                          
034600                                                                          
034700 LINKAGE SECTION.                                                         
034800                                                                          
034900*01  -COPY W0009   -PRE MSG-                                              
035000     EJECT                                                                
035100*01  -COPY W0008  -PRE WDP7-                                              
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE WDD3-                                              
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE WDD9-                                              
035800         05  KFBA-IDARTNR     PIC S9(9) COMP-3.                           
035900         05  KFBA-IDLEVNR     PIC X(5).                                   
036000     EJECT                                                                
036100*01  -COPY W0008  -PRE WDK6-                                              
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400*01  -COPY W0008  -PRE WDK9-                                              
036500     05  FILLER                  PIC X.                                   
036600     EJECT                                                                
036700*01  -COPY W0008  -PRE WDA5-                                              
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000*01  -COPY W0008  -PRE WDL2-                                              
037100     05  FILLER                  PIC X.                                   
037200                                                                          
037300     EJECT                                                                
037400 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
037500                                   WDD3-PCB WDD9-PCB WDK6-PCB             
037600                                   WDK9-PCB WDA5-PCB WDL2-PCB.            
037700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
037800                                   WDD3-PCB WDD9-PCB WDK6-PCB             
037900                                   WDK9-PCB WDA5-PCB WDL2-PCB.            
038000                                                                          
038100     PERFORM IMS-GET-MSG                                                  
038200     IF SEGMENT-FINNS                                                     
038300       PERFORM A-INIT                                                     
038400       PERFORM B-KOLLA-NYCKLAR                                            
038500       IF NYCKLAR-OK                                                      
038600         PERFORM C-LAES-VISA-INFO                                         
038700       END-IF                                                             
038800*      MOVE WS-TEMFSINF    TO MOD-TEMFSINF                                
038900       PERFORM IMS-INSERT-MSG                                             
039000     END-IF                                                               
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 A-INIT SECTION.                                                          
039700                                                                          
039800     IF MSG-KDTRANS-1 (3:1) = 'T'                                         
039900       MOVE JA             TO FLAGGA-TRANS-TYP                            
040000     ELSE                                                                 
040100       MOVE NEJ            TO FLAGGA-TRANS-TYP                            
040200     END-IF                                                               
040300                                                                          
040400     IF PULS-TRANS                                                        
040500                                                                          
040600       IF MSG-DUBBLA-TRANSKODER                                           
040700         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I11401               
040800         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
040900         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
041000       ELSE                                                               
041100         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W9I11401                 
041200         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
041300         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
041400       END-IF                                                             
041500                                                                          
041600       MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                    
041700       MOVE MSG-IDPFK TO MFS-IDPFK                                        
041800       MOVE MFS-IDTRANS TO W-IDTRANS                                      
041900                                                                          
042000       MOVE LOW-VALUE TO MSG-AREA                                         
042100       MOVE 'W9O114N1' TO MFS-IDMOD                                       
042200**     MOVE SPAR-IDTRANS TO MOD-IDTRANS                                   
042300       MOVE '9114' TO MOD-IDTRANS                                         
042400       MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                  
042500       PERFORM MFS-RENSA-FAELT-UT                                         
042600                                                                          
042700                                                                          
042800*      --- OM SVAR TILL SKÄRM:      MSG-KVLL = MOD-LÄNGD + 4              
042900*      --- OM PROGRAM-TILL-PROGRAM-SWITCH:   = MOD-LÄNGD + 17             
043000       COMPUTE MSG-KVLL = LENGTH OF MOD-W9O11401 + 4                      
043100**     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
043200       MOVE 'GB' TO MED-IDSKYLT                                           
043300                                                                          
043400       ACCEPT DAGENS-DATUM FROM DATE                                      
043500                                                                          
043600                                                                          
043700       IF EGEN-MID OR HELP-MID                                            
043800         CONTINUE                                                         
043900       ELSE                                                               
044000         MOVE SPACE TO MFS-KDTRTYP                                        
044100         MOVE '7' TO MFS-IDPFK                                            
044200       END-IF                                                             
044300                                                                          
044400     ELSE                                                                 
044500                                                                          
044600*        (VOLVO-VISION-TRANS)                                             
044700                                                                          
044800       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
044900                           TO MID2-W9I114V1                               
045000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
045100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
045200       MOVE MSG-KDTRTYP    TO MFS-KDTRTYP                                 
045300       MOVE MSG-IDPFK      TO MFS-IDPFK                                   
045400       MOVE MFS-IDTRANS    TO W-IDTRANS                                   
045500                                                                          
045600*      --- OM SVAR TILL SKÄRM:      MSG-KVLL = MOD-LÄNGD + 4              
045700*      --- OM PROGRAM-TILL-PROGRAM-SWITCH:   = MOD-LÄNGD + 17             
045800       COMPUTE MSG-KVLL = LENGTH OF MOD2-W9O114V1 + 4                     
045900                                                                          
046000       MOVE LOW-VALUE      TO MSG-AREA                                    
046100*      MOVE 'W9O114N1'     TO MFS-IDMOD                                   
046200       MOVE SPACE          TO MFS-IDMOD                                   
046300       MOVE '9114'         TO MOD2-IDTRANS                                
046400       MOVE MID2-IDARTNR   TO MOD2-IDARTNR                                
046500       MOVE SPACE          TO MOD2-TELEVBSK-EXT                           
046600                              MOD2-TELEVBSK-EXT2                          
046700       MOVE ZERO           TO MOD2-KVROS-DAG                              
046800                              MOD2-KVROS-BULK                             
046900                              MOD2-KVOKS-VOR                              
047000                              MOD2-KVAKS                                  
047100                              MOD2-IDMFSFEL                               
047200                                                                          
047300       MOVE +1 TO TABELL-IX                                               
047400       PERFORM UNTIL TABELL-IX > 4                                        
047500          MOVE ZERO          TO MOD2-TILEVBSK-DISP  (TABELL-IX)           
047600                                MOD2-KVAVIS-BSKKVAR (TABELL-IX)           
047700          ADD +1 TO TABELL-IX                                             
047800       END-PERFORM                                                        
047900                                                                          
048000       MOVE +1 TO TABELL-IX                                               
048100       PERFORM UNTIL TABELL-IX > 5                                        
048200          MOVE ZERO          TO MOD2-TIDATUM-INL (TABELL-IX)              
048300                                MOD2-KVAVROP     (TABELL-IX)              
048400          ADD +1 TO TABELL-IX                                             
048500       END-PERFORM                                                        
048600                                                                          
048700       ACCEPT DAGENS-DATUM FROM DATE                                      
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 B-KOLLA-NYCKLAR SECTION.                                                 
049200                                                                          
049300     IF PULS-TRANS                                                        
049400                                                                          
049500*    -- KONTROLL AV IDARTNR                                               
049600       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                             
049700                                                                          
049800       MOVE ALL '+' TO MSGI-WMSGINIT                                      
049900       MOVE '001'           TO MSGI-KDCALL                                
050000       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
050100       MOVE '9114'          TO MSGI-IDTRANS                               
050200       MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                          
050300       IF MFS-IDTRANS = '9114'                                            
050400       OR (MID-IDARTNR-IN NUMERIC                                         
050500       AND MID-IDARTNR-IN > ZERO)                                         
050600           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
050700       END-IF                                                             
050800       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
050900       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
051000                                                                          
051100       IF MID-IDARTNR-IN NOT = ALL '+'                                    
051200         MOVE '7'       TO MFS-IDPFK                                      
051300         MOVE SPACE     TO MFS-KDTRTYP                                    
051400       END-IF                                                             
051500                                                                          
051600       IF WS-IDARTNR NUMERIC                                              
                 MOVE WS-IDARTNR   TO W-IDARTNR                                 
                 IF W-IDARTNR < 0                                               
                    MOVE NEJ TO NYCKLAR-SW                                      
                 END-IF                                                         
051900       ELSE                                                               
052000         MOVE NEJ TO NYCKLAR-SW                                           
052100       END-IF                                                             
052200                                                                          
052300       IF NYCKLAR-OK                                                      
052400         MOVE W-IDARTNR         TO MOD-IDARTNR-UT                         
052500         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
052600       ELSE                                                               
052700         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
052800       END-IF                                                             
052900                                                                          
053000       IF NYCKLAR-FEL                                                     
053100         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
053200         CALL WMEDKONV USING MED-WMEDAREA                                 
053300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
053400       END-IF                                                             
053500                                                                          
053600     ELSE                                                                 
053700                                                                          
053800*        (VOLVO-VISION-TRANS)                                             
053900                                                                          
054000       IF MID2-IDARTNR NUMERIC                                            
054100         MOVE MID2-IDARTNR   TO WS-IDARTNR-NUM                            
054200         MOVE WS-IDARTNR-NUM TO W-IDARTNR                                 
054300                                MOD2-IDARTNR                              
054400       ELSE                                                               
054500         MOVE NEJ            TO NYCKLAR-SW                                
054600         MOVE 'B10'          TO MOD2-IDMFSFEL                             
054700         MOVE MID2-IDARTNR   TO MOD2-IDARTNR                              
054800       END-IF                                                             
054900     END-IF                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 C-LAES-VISA-INFO SECTION.                                                
055300                                                                          
055400                                                                          
055500     PERFORM IMS-GU-K601                                                  
055600                                                                          
055700     IF SEGMENT-FINNS                                                     
055800                                                                          
055900        MOVE ART-IDLEVNR TO SPARA-IDLEVNR                                 
056000                                                                          
056100        PERFORM CA-BERAEKNA-DATUM                                         
056200                                                                          
056300        MOVE ART-TIFINLV        TO TMP1-YYWWD                             
056400        MOVE DAT-TIAAVVD        TO TMP2-YYWWD                             
056500        PERFORM WY2000P2                                                  
056600        IF TMP1-YYWWD <= TMP2-YYWWD                                       
056700                                                                          
056800            PERFORM IMS-GNP-K611                                          
056900                                                                          
057000            IF SEGMENT-FINNS                                              
                      IF CLAG-IDDC-REF NOT = SPACE                              
                         MOVE ERR-REFILL-PART TO MED-IDMFSFEL                   
                         CALL WMEDKONV USING MED-WMEDAREA                       
                         MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                   
                      END-IF                                                    
057100                IF (CLAG-KDUART = 'M' OR 'S')                             
057200                   OR (CLAG-KDERS = 52)                                   
057300                   OR (CLAG-FLLSRDEL = 'N')                               
057400                     IF PULS-TRANS                                        
057500                       MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL            
057600                       CALL WMEDKONV USING MED-WMEDAREA                   
057700                       MOVE MED-MFSFEL TO MOD-TEMFSFEL                    
057800                     ELSE                                                 
057900                       MOVE 'B10'              TO MOD2-IDMFSFEL           
058000                     END-IF                                               
058100                ELSE                                                      
058200                    IF CLAG-KDERS > +10                                   
058300                      IF PULS-TRANS                                       
058400                        MOVE ERR-ARTIKEL-UTGANGEN TO MED-IDMFSINF         
058500                        CALL WMEDKONV USING MED-WMEDAREA                  
058600                        MOVE MED-MFSINF TO MOD-TEMFSINF                   
058700                      ELSE                                                
058800                        MOVE 'B10'             TO MOD2-IDMFSFEL           
058900                      END-IF                                              
059000                    END-IF                                                
059100                                                                          
059200                   PERFORM CB-REDIGERA-DEFAULT                            
059300                   PERFORM CC-LAES-VISA-BENAMNINGAR                       
059400                   PERFORM CD-VISA-KVROS                                  
059500                   PERFORM CE-VISA-KVAK-SALDO                             
059600                   PERFORM CF-LAES-VISA-WDD924-WDD925                     
059700                END-IF                                                    
059800                                                                          
059900                                                                          
060000                IF (FLAGGA-WDD9 = JA)                                     
060100                  IF (FLAGGA-WDD924 = NEJ) AND                            
060200                     (FLAGGA-WDD925 = NEJ)                                
060300                      PERFORM CG-LAES-VISA-AVROP                          
060400                  END-IF                                                  
060500                END-IF                                                    
060600             ELSE                                                         
060700                IF PULS-TRANS                                             
060800                  MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                 
060900                  CALL WMEDKONV USING MED-WMEDAREA                        
061000                  MOVE MED-MFSFEL TO MOD-TEMFSFEL                         
061100                ELSE                                                      
061200                  MOVE 'B10'                   TO MOD2-IDMFSFEL           
061300                END-IF                                                    
061400            END-IF                                                        
061500        ELSE                                                              
061600          IF PULS-TRANS                                                   
061700            MOVE ERR-NO-ORDER TO MED-IDMFSFEL                             
061800            CALL WMEDKONV USING MED-WMEDAREA                              
061900            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
062000          ELSE                                                            
062100            MOVE 'B10'                         TO MOD2-IDMFSFEL           
062200          END-IF                                                          
062300        END-IF                                                            
062400     ELSE                                                                 
062500        IF PULS-TRANS                                                     
062600          MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                         
062700          CALL WMEDKONV USING MED-WMEDAREA                                
062800          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
062900        ELSE                                                              
063000          MOVE 'B10'                           TO MOD2-IDMFSFEL           
063100        END-IF                                                            
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 CA-BERAEKNA-DATUM SECTION.                                               
063600                                                                          
063700                                                                          
063800     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
063900     CALL WDATKONV USING                                                  
064000          DAT-KDDATFORM,                                                  
064100          DAT-I-TIDATUM,                                                  
064200          DAT-O-TIDATUM,                                                  
064300          DAT-KDSVAR                                                      
064400                                                                          
064500     IF DAT-KDSVAR-OK                                                     
064600         ADD +5 TO DAT-TIVV                                               
064700         IF DAT-TIVV > 52                                                 
064800             COMPUTE DAT-TIVV = DAT-TIVV - 52                             
064900             ADD +1 TO DAT-TIAA-VECKA                                     
065000         END-IF                                                           
065100                                                                          
065200                                                                          
065300     ELSE                                                                 
065400         CALL FELLOG                                                      
065500     END-IF                                                               
065600                                                                          
065700     .                                                                    
065800     EJECT                                                                
065900 CB-REDIGERA-DEFAULT SECTION.                                             
066000     SKIP2                                                                
066100     IF PULS-TRANS                                                        
066200       MOVE 'NONE' TO MOD-TILEVBSK-DISP (1)                               
066300       MOVE 'YET'  TO MOD-KVAVIS-BSKKVAR (1)                              
066400       MOVE 'NONE' TO MOD-TELEVBSK-EXT                                    
066500       MOVE SPACE  TO MOD-TELEVBSK-EXT2                                   
066600                      MOD-TELEVBSK-EXT3                                   
066700                      MOD-TELEVBSK-EXT4                                   
066800     ELSE                                                                 
066900       MOVE SPACE            TO MOD2-TELEVBSK-EXT                         
067000                                MOD2-TELEVBSK-EXT2                        
067100                                MOD2-TELEVBSK-EXT3                        
067200                                MOD2-TELEVBSK-EXT4                        
067300     END-IF                                                               
067400     .                                                                    
067500     EJECT                                                                
067600 CC-LAES-VISA-BENAMNINGAR SECTION.                                        
067700                                                                          
067800     PERFORM IMS-GU-D301                                                  
067900                                                                          
068000     IF SEGMENT-FINNS                                                     
068100         MOVE 'GB ' TO W-IDSKYLT-X                                        
068200         PERFORM IMS-GNP-D311                                             
068300                                                                          
068400         IF SEGMENT-FINNS                                                 
068500           IF PULS-TRANS                                                  
068600             MOVE TEXT-BEART TO MOD-BEART-ENG                             
068700           END-IF                                                         
068800         END-IF                                                           
068900                                                                          
069000         MOVE 'S  ' TO W-IDSKYLT-X                                        
069100         PERFORM IMS-GNP-D311                                             
069200                                                                          
069300         IF SEGMENT-FINNS                                                 
069400           IF PULS-TRANS                                                  
069500             MOVE TEXT-BEART TO MOD-BEART-SVE                             
069600           END-IF                                                         
069700         END-IF                                                           
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 CD-VISA-KVROS SECTION.                                                   
070200     SKIP2                                                                
070300     PERFORM IMS-GU-K901                                                  
070400     IF SEGMENT-FINNS                                                     
070500       IF PULS-TRANS                                                      
070600         MOVE ART-KVOKS-VOR                                               
070700                             TO MOD-KVOKS-VOR                             
070800       ELSE                                                               
070900         MOVE ART-KVOKS-VOR                                               
071000                             TO MOD2-KVOKS-VOR                            
071100       END-IF                                                             
071200     ELSE                                                                 
071300       IF PULS-TRANS                                                      
071400         MOVE ZERO           TO MOD-KVOKS-VOR                             
071500                                ART-KVPRERO-DAG                           
071600                                ART-KVPRERO-BULK                          
071700       ELSE                                                               
071800         MOVE ZERO           TO MOD2-KVOKS-VOR                            
071900                                ART-KVPRERO-DAG                           
072000                                ART-KVPRERO-BULK                          
072100       END-IF                                                             
072200     END-IF                                                               
072300                                                                          
072400     IF  CLAG-KVROS       = 0                                             
072500     AND ART-KVPRERO-DAG = ZERO                                           
072600     AND ART-KVPRERO-BULK = ZERO                                          
072700       IF PULS-TRANS                                                      
072800         MOVE ERR-NO-BACKORDERS                                           
072900                             TO MED-IDMFSINF                              
073000         CALL WMEDKONV USING MED-WMEDAREA                                 
073100         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
073200         MOVE ZERO           TO MOD-KVROS-DAG                             
073300                                MOD-KVROS-BULK                            
073400       ELSE                                                               
073500         MOVE ZERO           TO MOD2-KVROS-DAG                            
073600                                MOD2-KVROS-BULK                           
073700       END-IF                                                             
073800     ELSE                                                                 
073900                                                                          
074000         PERFORM S01-LAES-WDA5-ENTER                                      
074100         ADD ART-KVPRERO-DAG                                              
074200                             TO WS-RESTKVANT-DAG                          
074300         ADD ART-KVPRERO-BULK                                             
074400                             TO WS-RESTKVANT-BULK                         
074500         IF PULS-TRANS                                                    
074600           MOVE WS-RESTKVANT-DAG                                          
074700                             TO MOD-KVROS-DAG                             
074800           MOVE WS-RESTKVANT-BULK                                         
074900                             TO MOD-KVROS-BULK                            
075000         ELSE                                                             
075100           MOVE WS-RESTKVANT-DAG                                          
075200                             TO MOD2-KVROS-DAG                            
075300           MOVE WS-RESTKVANT-BULK                                         
075400                             TO MOD2-KVROS-BULK                           
075500         END-IF                                                           
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 CE-VISA-KVAK-SALDO SECTION.                                              
076000     SKIP2                                                                
076100     IF PULS-TRANS                                                        
076200       MOVE ZERO             TO MOD-KVAKS                                 
076300     ELSE                                                                 
076400       MOVE ZERO             TO MOD2-KVAKS                                
076500     END-IF                                                               
076600                                                                          
076700     IF CLAG-TIDISPIN > 0                                                 
076800         PERFORM CEA-BERAKNA-NASTA-INLEV                                  
076900         MOVE CLAG-TIDISPIN        TO TMP1-YYMMDD                         
077000         MOVE DAGENS-DATUM         TO TMP2-YYMMDD                         
077100         PERFORM WY2000P1                                                 
077200         MOVE DAT-TIAAVVD          TO TMP1-YYWWD                          
077300         MOVE W-TIDISPIN-DATUM     TO TMP2-YYWWD                          
077400         PERFORM WY2000P2                                                 
077500         IF  (TMP1-YYMMDD >= TMP2-YYMMDD)                                 
077600         AND (TMP1-YYWWD  <= TMP2-YYWWD)                                  
077700                                                                          
077800             COMPUTE KVAK-SALDO =                                         
077900                     CLAG-KVAKS-CDC +                                     
078000                     CLAG-KVAKS-PAV +                                     
078100                     CLAG-KVAKS-T                                         
078200             IF KVAK-SALDO > 0                                            
078300                PERFORM IMS-GU-L201                                       
078400                IF SEGMENT-FINNS                                          
078500                   MOVE '310' TO W-IDPTYP                                 
078600                   PERFORM IMS-GU-L221                                    
078700                   PERFORM UNTIL SEGMENT-SAKNAS                           
078800                      IF MOT-KDRT = 07                                    
078900                         COMPUTE WS-RETUR =                               
079000                              MOT-KVAVIS - MOT-KVANTMOT                   
079100                         SUBTRACT WS-RETUR FROM KVAK-SALDO                
079200                      END-IF                                              
079300                      PERFORM IMS-GU-L221                                 
079400                   END-PERFORM                                            
079500                END-IF                                                    
079600             END-IF                                                       
079700             IF PULS-TRANS                                                
079800               MOVE KVAK-SALDO                                            
079900                             TO MOD-KVAKS                                 
080000             ELSE                                                         
080100               MOVE KVAK-SALDO                                            
080200                             TO MOD2-KVAKS                                
080300             END-IF                                                       
080400                                                                          
080500         END-IF                                                           
080600     END-IF                                                               
080700     .                                                                    
080800     EJECT                                                                
080900 CEA-BERAKNA-NASTA-INLEV SECTION.                                         
081000                                                                          
081100     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
081200     CALL WDATKONV USING                                                  
081300          DAT-KDDATFORM,                                                  
081400          DAT-I-TIDATUM,                                                  
081500          DAT-O-TIDATUM,                                                  
081600          DAT-KDSVAR                                                      
081700                                                                          
081800     IF DAT-KDSVAR-OK                                                     
081900         MOVE DAT-TIAAVVD  TO W-TIDISPIN-DATUM                            
082000         ADD +4 TO W-TIDISPIN-VV                                          
082100         IF W-TIDISPIN-VV > 52                                            
082200             COMPUTE W-TIDISPIN-VV = W-TIDISPIN-VV - 52                   
082300             ADD +1 TO W-TIDISPIN-AA                                      
082400         END-IF                                                           
082500     ELSE                                                                 
082600         CALL FELLOG                                                      
082700     END-IF                                                               
082800                                                                          
082900     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
083000     MOVE CLAG-TIDISPIN TO DAT-I-TIDATUM                                  
083100     CALL WDATKONV USING                                                  
083200          DAT-KDDATFORM,                                                  
083300          DAT-I-TIDATUM,                                                  
083400          DAT-O-TIDATUM,                                                  
083500          DAT-KDSVAR                                                      
083600                                                                          
083700                                                                          
083800     IF DAT-KDSVAR-FEL                                                    
083900         CALL FELLOG                                                      
084000     END-IF                                                               
084100     .                                                                    
084200     EJECT                                                                
084300 CF-LAES-VISA-WDD924-WDD925 SECTION.                                      
084400                                                                          
084500     MOVE SPARA-IDLEVNR  TO W-IDLEVNR                                     
084600                                                                          
084610     MOVE W-IDARTNR      TO W-IDARTNR-D9                                  
084620     MOVE WC-CDC-SE      TO W-IDDC-D9                                     
084700     PERFORM IMS-GU-D901                                                  
084800                                                                          
084900     IF SEGMENT-FINNS                                                     
085000         MOVE +1 TO TABELL-IX                                             
085100         PERFORM IMS-GNP-D924                                             
085200         IF SEGMENT-FINNS                                                 
085300                                                                          
085400             PERFORM UNTIL SEGMENT-SAKNAS OR                              
085500                           TABELL-IX > 100                                
085600                                                                          
085700                 MOVE LEV-TILEVBSK-DISP          TO TMP1-YYMMDD           
085800                 MOVE DAGENS-DATUM               TO TMP2-YYMMDD           
085900                 PERFORM WY2000P1                                         
086000                 IF TMP1-YYMMDD >= TMP2-YYMMDD                            
086100                    MOVE LEV-TILEVBSK-DISP       TO                       
086200                         TILEVBSK-DISP (TABELL-IX)                        
086300                                                                          
086400                    MOVE LEV-KVAVIS-BSKKVAR TO                            
086500                             KVAVIS-BSKKVAR (TABELL-IX)                   
086600                                                                          
086700                    MOVE KFBA-IDLEVNR            TO                       
086800                             TAB-IDLEVNR (TABELL-IX)                      
086900                                                                          
087000                    MOVE TABELL-IX TO MAX-ANTAL-RADER                     
087100                    ADD +1 TO TABELL-IX                                   
087200                 END-IF                                                   
087300                                                                          
087400                                                                          
087500                 PERFORM IMS-GNP-D924                                     
087600                                                                          
087700             END-PERFORM                                                  
087800                                                                          
087900             IF MAX-ANTAL-RADER > 0                                       
088000                 PERFORM CFA-SORTERA-TABELL                               
088100             ELSE                                                         
088200                 MOVE NEJ TO FLAGGA-WDD924                                
088300             END-IF                                                       
088400                                                                          
088500         ELSE                                                             
088600             MOVE NEJ TO FLAGGA-WDD924                                    
088700         END-IF                                                           
088800                                                                          
088900         MOVE +2                     TO W-IDLEVBSK                        
089000         PERFORM IMS-GNP-D925                                             
089100                                                                          
089200         IF SEGMENT-FINNS                                                 
089300             MOVE INFO-TIBORT        TO TMP1-YYMMDD                       
089400             MOVE DAGENS-DATUM       TO TMP2-YYMMDD                       
089500             PERFORM WY2000P1                                             
089600             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
089700               IF PULS-TRANS                                              
089800                 MOVE INFO-TELEVBSK TO MOD-TELEVBSK-EXT                   
089900               ELSE                                                       
090000                 MOVE INFO-TELEVBSK TO MOD2-TELEVBSK-EXT                  
090100               END-IF                                                     
090200               MOVE +4               TO W-IDLEVBSK                        
090300               PERFORM IMS-GNP-D925                                       
090400               IF SEGMENT-FINNS                                           
090500                 IF PULS-TRANS                                            
090600                   MOVE INFO-TELEVBSK TO MOD-TELEVBSK-EXT2                
090700                 ELSE                                                     
090800                   MOVE INFO-TELEVBSK TO MOD2-TELEVBSK-EXT2               
090900                 END-IF                                                   
091000               END-IF                                                     
091100               MOVE +5               TO W-IDLEVBSK                        
091200               PERFORM IMS-GNP-D925                                       
091300               IF SEGMENT-FINNS                                           
091400                 IF PULS-TRANS                                            
091500                   MOVE INFO-TELEVBSK TO MOD-TELEVBSK-EXT3                
091600                 ELSE                                                     
091700                   MOVE INFO-TELEVBSK TO MOD2-TELEVBSK-EXT3               
091800                 END-IF                                                   
091900               END-IF                                                     
092000               MOVE +6               TO W-IDLEVBSK                        
092100               PERFORM IMS-GNP-D925                                       
092200               IF SEGMENT-FINNS                                           
092300                 IF PULS-TRANS                                            
092400                   MOVE INFO-TELEVBSK                                     
092500                                 TO MOD-TELEVBSK-EXT4                     
092600                 ELSE                                                     
092700                   MOVE INFO-TELEVBSK                                     
092800                                 TO MOD2-TELEVBSK-EXT4                    
092900                 END-IF                                                   
093000               END-IF                                                     
093100             ELSE                                                         
093200                 MOVE NEJ TO FLAGGA-WDD925                                
093300             END-IF                                                       
093400         ELSE                                                             
093500             MOVE NEJ TO FLAGGA-WDD925                                    
093600         END-IF                                                           
093700     ELSE                                                                 
093800         MOVE NEJ TO FLAGGA-WDD9                                          
093900         MOVE NEJ TO FLAGGA-WDD924                                        
094000         MOVE NEJ TO FLAGGA-WDD925                                        
094100     END-IF                                                               
094200                                                                          
094300     IF FLAGGA-WDD9 = JA AND WDD924-FINNS AND                             
094400        WDD925-FINNS-INTE                                                 
094500        IF ALT-IDLEVNR NOT = SPARA-IDLEVNR                                
094600           MOVE +2                   TO W-IDLEVBSK                        
094700           PERFORM IMS-GNP-D925-ALT                                       
094800                                                                          
094900           IF SEGMENT-FINNS                                               
095000              MOVE INFO-TIBORT        TO TMP1-YYMMDD                      
095100              MOVE DAGENS-DATUM       TO TMP2-YYMMDD                      
095200              PERFORM WY2000P1                                            
095300              IF TMP1-YYMMDD >= TMP2-YYMMDD                               
095400                 MOVE JA  TO FLAGGA-WDD925                                
095500                 IF PULS-TRANS                                            
095600                   MOVE INFO-TELEVBSK                                     
095700                             TO MOD-TELEVBSK-EXT                          
095800                 ELSE                                                     
095900                   MOVE INFO-TELEVBSK                                     
096000                             TO MOD2-TELEVBSK-EXT                         
096100                 END-IF                                                   
096200                 MOVE +4             TO W-IDLEVBSK                        
096300                 PERFORM IMS-GNP-D925-ALT                                 
096400                 IF SEGMENT-FINNS                                         
096500                   IF PULS-TRANS                                          
096600                      MOVE INFO-TELEVBSK                                  
096700                             TO MOD-TELEVBSK-EXT2                         
096800                   ELSE                                                   
096900                      MOVE INFO-TELEVBSK                                  
097000                             TO MOD2-TELEVBSK-EXT2                        
097100                   END-IF                                                 
097200                 END-IF                                                   
097300                 MOVE +5             TO W-IDLEVBSK                        
097400                 PERFORM IMS-GNP-D925-ALT                                 
097500                 IF SEGMENT-FINNS                                         
097600                   IF PULS-TRANS                                          
097700                      MOVE INFO-TELEVBSK                                  
097800                             TO MOD-TELEVBSK-EXT3                         
097900                   ELSE                                                   
098000                      MOVE INFO-TELEVBSK                                  
098100                             TO MOD2-TELEVBSK-EXT3                        
098200                   END-IF                                                 
098300                 END-IF                                                   
098400                 MOVE +6             TO W-IDLEVBSK                        
098500                 PERFORM IMS-GNP-D925-ALT                                 
098600                 IF SEGMENT-FINNS                                         
098700                   IF PULS-TRANS                                          
098800                      MOVE INFO-TELEVBSK                                  
098900                             TO MOD-TELEVBSK-EXT4                         
099000                   ELSE                                                   
099100                      MOVE INFO-TELEVBSK                                  
099200                             TO MOD2-TELEVBSK-EXT4                        
099300                   END-IF                                                 
099400                 END-IF                                                   
099500              END-IF                                                      
099600           END-IF                                                         
099700        END-IF                                                            
099800     END-IF                                                               
099900     .                                                                    
100000     EJECT                                                                
100100                                                                          
100200                                                                          
100300 CFA-SORTERA-TABELL SECTION.                                              
100400                                                                          
100500                                                                          
100600                                                                          
100700     MOVE +1 TO TABELL-IX SPARA-IX MOD-IX                                 
100800     MOVE 999999 TO SPARA-LAGSTA-DATUM                                    
100900                                                                          
101000     PERFORM UNTIL MOD-IX > 4                                             
101100                                                                          
101200         PERFORM UNTIL TABELL-IX > MAX-ANTAL-RADER                        
101300                                                                          
101400             MOVE TILEVBSK-DISP (TABELL-IX) TO TMP1-YYMMDD                
101500             MOVE SPARA-LAGSTA-DATUM        TO TMP2-YYMMDD                
101600             PERFORM WY2000P1                                             
101700             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
101800                                                                          
101900                 MOVE TILEVBSK-DISP (TABELL-IX) TO                        
102000                      SPARA-LAGSTA-DATUM                                  
102100                 MOVE TABELL-IX TO SPARA-IX                               
102200             END-IF                                                       
102300                                                                          
102400             ADD +1 TO TABELL-IX                                          
102500         END-PERFORM                                                      
102600                                                                          
102700         IF SPARA-LAGSTA-DATUM < 999999                                   
102800             IF PULS-TRANS                                                
102900               MOVE TILEVBSK-DISP (SPARA-IX)                              
103000                  TO MOD-TILEVBSK-DISP (MOD-IX)                           
103100               MOVE KVAVIS-BSKKVAR (SPARA-IX) TO                          
103200                  MOD-KVAVIS-BSKKVAR (MOD-IX)                             
103300               INSPECT MOD-KVAVIS-BSKKVAR (MOD-IX)                        
103400                  REPLACING LEADING ZEROS BY SPACE                        
103500             ELSE                                                         
103600               MOVE TILEVBSK-DISP (SPARA-IX)                              
103700                  TO MOD2-TILEVBSK-DISP (MOD-IX)                          
103800               MOVE KVAVIS-BSKKVAR (SPARA-IX) TO                          
103900                  MOD2-KVAVIS-BSKKVAR (MOD-IX)                            
104000             END-IF                                                       
104100             IF MOD-IX = 1                                                
104200                MOVE TAB-IDLEVNR (SPARA-IX) TO                            
104300                  ALT-IDLEVNR                                             
104400             END-IF                                                       
104500             MOVE 999999 TO TILEVBSK-DISP (SPARA-IX)                      
104600                                                                          
104700             MOVE 999999 TO SPARA-LAGSTA-DATUM                            
104800         END-IF                                                           
104900                                                                          
105000         MOVE +1 TO TABELL-IX                                             
105100         ADD +1 TO MOD-IX                                                 
105200     END-PERFORM                                                          
105300     .                                                                    
105400     EJECT                                                                
105500 CG-LAES-VISA-AVROP SECTION.                                              
105600                                                                          
105700     PERFORM CGA-BERAKNA-NASTA-AVROP                                      
105800     PERFORM IMS-GNP-D905-FIRST                                           
105900     IF SEGMENT-FINNS                                                     
106000         PERFORM CGB-LAES-VISA-AVROP                                      
106100     END-IF                                                               
106200                                                                          
106300     .                                                                    
106400     EJECT                                                                
106500                                                                          
106600 CGA-BERAKNA-NASTA-AVROP SECTION.                                         
106700     SKIP2                                                                
106800     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
106900     CALL WDATKONV USING                                                  
107000          DAT-KDDATFORM,                                                  
107100          DAT-I-TIDATUM,                                                  
107200          DAT-O-TIDATUM,                                                  
107300          DAT-KDSVAR                                                      
107400                                                                          
107500     IF DAT-KDSVAR-OK                                                     
107600         MOVE DAT-TIAAVVD  TO W-AVROP-DATUM                               
107700                              W-DAGENS-AAVVD                              
107800         ADD +4 TO W-AVROP-VV                                             
107900         IF W-AVROP-VV > 52                                               
108000             COMPUTE W-AVROP-VV = W-AVROP-VV - 52                         
108100             ADD +1 TO W-AVROP-AA                                         
108200         END-IF                                                           
108300         MOVE W-AVROP-DATUM TO W-AVROP-FRAM-GRP                           
108400     ELSE                                                                 
108500         CALL FELLOG                                                      
108600     END-IF                                                               
108700                                                                          
108800     MOVE DAT-TIAAVVD        TO WS-TIAAVVD                                
108900     MOVE 5                  TO WS-TID                                    
109000     MOVE 1                  TO TABELL-IX                                 
109100                                                                          
109200     PERFORM UNTIL TABELL-IX > 5                                          
109300       MOVE WS-TIAAVVD       TO DAT-I-TIDATUM                             
109400       MOVE 'AAVVD'          TO DAT-KDDATFORM                             
109500       CALL WDATKONV USING      DAT-KDDATFORM                             
109600                                DAT-I-TIDATUM                             
109700                                DAT-O-TIDATUM                             
109800                                DAT-KDSVAR                                
109900       IF DAT-KDSVAR-FEL                                                  
110000         MOVE 'FEL VID ANROP TILL DATKONV 1' TO FELTEXT                   
110100         CALL FELLOG                                                      
110200       ELSE                                                               
110300         MOVE DAT-TIAAMMDD   TO WS-TAB-TIAAMMDD (TABELL-IX)               
110400         MOVE ZERO           TO WS-TAB-KVAVROP  (TABELL-IX)               
110500       END-IF                                                             
110600       ADD 1                 TO TABELL-IX                                 
110700                                WS-TIVV                                   
110800       IF WS-TIVV > 52                                                    
110900         ADD 1               TO WS-TIAA                                   
111000         MOVE 1              TO WS-TIVV                                   
111100       END-IF                                                             
111200     END-PERFORM                                                          
111300     .                                                                    
111400     EJECT                                                                
111500 CGB-LAES-VISA-AVROP SECTION.                                             
111600     SKIP2                                                                
111700     PERFORM UNTIL SEGMENT-SAKNAS                                         
111800     OR TIAVRDAT-INL < DAGENS-DATUM                                       
111900       MOVE TIAVRDAT-INL             TO DAT-I-TIDATUM                     
112000       MOVE 'AAMMDD'                 TO DAT-KDDATFORM                     
112100       CALL WDATKONV USING              DAT-KDDATFORM                     
112200                                  DAT-I-TIDATUM                           
112300                                  DAT-O-TIDATUM                           
112400                                  DAT-KDSVAR                              
112500       IF DAT-KDSVAR-FEL                                                  
112600         MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT                   
112700         CALL FELLOG                                                      
112800       ELSE                                                               
112900         MOVE DAT-TIAAVV-GRP         TO WS-TIAAVV                         
113000         MOVE WS-TIAAVV              TO TMP1-YYWW                         
113100       END-IF                                                             
113200       MOVE W-DAGENS-AAVV            TO TMP2-YYWW                         
113300       MOVE W-FRAM-AAVV              TO TMP3-YYWW                         
113400       PERFORM WY2000Q3                                                   
113500       IF (TMP1-YYWW >= TMP2-YYWW )                                       
113600       AND (TMP1-YYWW <= TMP3-YYWW)                                       
113700                                                                          
113800         MOVE DAT-TIAAVVD    TO WS-TIAAVVD                                
113900         ADD 2               TO WS-TID                                    
114000                                                                          
114100         IF WS-TID > 5                                                    
114200           SUBTRACT 5        FROM WS-TID                                  
114300         END-IF                                                           
114400                                                                          
114500         MOVE WS-TIAAVVD     TO DAT-I-TIDATUM                             
114600         MOVE 'AAVVD'        TO DAT-KDDATFORM                             
114700         CALL WDATKONV USING DAT-KDDATFORM                                
114800                             DAT-I-TIDATUM                                
114900                             DAT-O-TIDATUM                                
115000                             DAT-KDSVAR                                   
115100         IF DAT-KDSVAR-FEL                                                
115200           MOVE 'FEL VID ANROP TILL DATKONV 3' TO FELTEXT                 
115300           CALL FELLOG                                                    
115400         END-IF                                                           
115500                                                                          
115600         IF DAT-TIAAMMDD <= WS-TAB-TIAAMMDD (1)                           
115700           ADD KVAVROP       TO WS-TAB-KVAVROP (1)                        
115800         ELSE                                                             
115900           IF DAT-TIAAMMDD <= WS-TAB-TIAAMMDD (2)                         
116000             ADD KVAVROP TO WS-TAB-KVAVROP (2)                            
116100           ELSE                                                           
116200             IF DAT-TIAAMMDD <= WS-TAB-TIAAMMDD (3)                       
116300               ADD KVAVROP TO WS-TAB-KVAVROP (3)                          
116400             ELSE                                                         
116500               IF DAT-TIAAMMDD <= WS-TAB-TIAAMMDD (4)                     
116600                 ADD KVAVROP TO WS-TAB-KVAVROP (4)                        
116700               ELSE                                                       
116800                 ADD KVAVROP TO WS-TAB-KVAVROP (5)                        
116900               END-IF                                                     
117000             END-IF                                                       
117100           END-IF                                                         
117200         END-IF                                                           
117300       END-IF                                                             
117400       PERFORM IMS-GNP-D905                                               
117500     END-PERFORM                                                          
117600                                                                          
117700     IF SEGMENT-FINNS                                                     
117800     AND TIAVRDAT-INL < DAGENS-DATUM                                      
117900*            SLÄP - VISA INGA AVROP                                       
118000       CONTINUE                                                           
118100     ELSE                                                                 
118200       MOVE +1 TO MOD-IX                                                  
118300       PERFORM UNTIL MOD-IX > 5                                           
118400         IF PULS-TRANS                                                    
118500           MOVE WS-TAB-TIAAMMDD (MOD-IX)                                  
118600                             TO MOD-TIDATUM-INL (MOD-IX)                  
118700           MOVE WS-TAB-KVAVROP (MOD-IX)                                   
118800                             TO MOD-KVAVROP     (MOD-IX)                  
118900         ELSE                                                             
119000           MOVE WS-TAB-TIAAMMDD (MOD-IX)                                  
119100                             TO MOD2-TIDATUM-INL (MOD-IX)                 
119200           MOVE WS-TAB-KVAVROP (MOD-IX)                                   
119300                             TO MOD2-KVAVROP    (MOD-IX)                  
119400         END-IF                                                           
119500         ADD +1              TO MOD-IX                                    
119600       END-PERFORM                                                        
119700     END-IF                                                               
119800     .                                                                    
119900     EJECT                                                                
120000 S01-LAES-WDA5-ENTER SECTION.                                             
120100                                                                          
120200                                                                          
120300     MOVE LOW-VALUE          TO W-WDA5A1KY-MIN                            
120400     MOVE HIGH-VALUE         TO W-WDA5A1KY-MAX                            
120500                                                                          
120600     MOVE W-IDARTNR          TO W-IDARTNR-N3-MIN                          
120700                                W-IDARTNR-N3-MAX                          
120800     PERFORM IMS-GN-A5A1                                                  
120900                                                                          
121000     MOVE ZERO               TO WS-RESTKVANT-DAG                          
121100                                WS-RESTKVANT-BULK                         
121200     PERFORM UNTIL SEGMENT-SAKNAS                                         
121300                                                                          
121400       IF  SEQA-IDDC = '11'                                               
121500       AND SEQA-KDSTARAD = 2                                              
121600          IF SEQA-KDRAPRIO = 10                                           
121700          OR SEQA-KDRAPRIO = 20                                           
121800          OR SEQA-KDRAPRIO = 30                                           
121900          OR SEQA-KDRAPRIO = 40                                           
122000             ADD SEQA-KVART  TO WS-RESTKVANT-DAG                          
122100          ELSE                                                            
122200             ADD SEQA-KVART  TO WS-RESTKVANT-BULK                         
122300          END-IF                                                          
122400       END-IF                                                             
122500                                                                          
122600       PERFORM IMS-GN-A5A1                                                
122700                                                                          
122800     END-PERFORM                                                          
122900     .                                                                    
123000     EJECT                                                                
123100 MFS-RENSA-FAELT-UT SECTION.                                              
123200                                                                          
123300*    --- ALLA UTDATA-FÄLT                                                 
123400                                                                          
123500     MOVE MFS-RENSA-FAELT TO MOD-BEART-SVE                                
123600     MOVE MFS-RENSA-FAELT TO                                              
123700                             MOD-BEART-ENG                                
123800                             MOD-TELEVBSK-EXT                             
123900                             MOD-TELEVBSK-EXT2                            
124000                             MOD-TELEVBSK-EXT3                            
124100                             MOD-TELEVBSK-EXT4                            
124200                             MOD-KVROS-DAG                                
124300                             MOD-KVROS-BULK                               
124400                             MOD-KVAKS                                    
124500                                                                          
124600     MOVE +1 TO MOD-IX                                                    
124700     PERFORM UNTIL MOD-IX > 4                                             
124800         MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-DISP (MOD-IX)               
124900                                 MOD-KVAVIS-BSKKVAR (MOD-IX)              
125000                                 MOD-TIDATUM-INL (MOD-IX)                 
125100                                 MOD-KVAVROP (MOD-IX)                     
125200         ADD +1 TO MOD-IX                                                 
125300     END-PERFORM                                                          
125400     .                                                                    
125500     EJECT                                                                
125600 IMS-GET-MSG SECTION.                                                     
125700                                                                          
125800     MOVE '  QC' TO GODK-STATUSKODER                                      
125900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
126000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126100     PERFORM IMS-STATUSKONTROLL                                           
126200     .                                                                    
126300     SKIP3                                                                
126400 IMS-INSERT-MSG SECTION.                                                  
126500                                                                          
126600     IF ENGLISH-TEXT                                                      
126700       MOVE 'N' TO MFS-KDHUVOMR                                           
126800     END-IF                                                               
126900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
127000     MOVE SPACE TO GODK-STATUSKODER                                       
127100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
127200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127300     PERFORM IMS-STATUSKONTROLL                                           
127400     .                                                                    
127500     EJECT                                                                
127600 IMS-GU-D301 SECTION.                                                     
127700                                                                          
127800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
127900          DELIMITED BY SIZE INTO SSA1                                     
128000     MOVE '  GE' TO GODK-STATUSKODER                                      
128100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
128200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
128300     PERFORM IMS-STATUSKONTROLL                                           
128400     .                                                                    
128500     EJECT                                                                
128600 IMS-GNP-D311 SECTION.                                                    
128700                                                                          
128800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE '  GE' TO GODK-STATUSKODER                                      
129100     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
129200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
129300     PERFORM IMS-STATUSKONTROLL                                           
129400     .                                                                    
129500     EJECT                                                                
129600 IMS-GU-D901 SECTION.                                                     
129700                                                                          
129800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
129900          DELIMITED BY SIZE INTO SSA1                                     
130000     MOVE '  GE' TO GODK-STATUSKODER                                      
130100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
130200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
130300     PERFORM IMS-STATUSKONTROLL                                           
130400     .                                                                    
130500     EJECT                                                                
130600 IMS-GNP-D924 SECTION.                                                    
130700                                                                          
130800     STRING 'WDD924     '                                                 
130900          DELIMITED BY SIZE INTO SSA1                                     
131000     MOVE '  GE' TO GODK-STATUSKODER                                      
131100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
131200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
131300     PERFORM IMS-STATUSKONTROLL                                           
131400     .                                                                    
131500     EJECT                                                                
131600 IMS-GNP-D925 SECTION.                                                    
131700                                                                          
131800     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
131900          DELIMITED BY SIZE INTO SSA1                                     
132000     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
132100          DELIMITED BY SIZE INTO SSA2                                     
132200     MOVE '  GE' TO GODK-STATUSKODER                                      
132300     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1 SSA2              
132400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
132500     PERFORM IMS-STATUSKONTROLL                                           
132600     .                                                                    
132700     SKIP3                                                                
132800 IMS-GNP-D925-ALT SECTION.                                                
132900                                                                          
133000     STRING 'WDD902  *F(IDLEVNR  =' ALT-IDLEVNR-X ')'                     
133100          DELIMITED BY SIZE INTO SSA1                                     
133200     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
133300          DELIMITED BY SIZE INTO SSA2                                     
133400     MOVE '  GE' TO GODK-STATUSKODER                                      
133500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1 SSA2              
133600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
133700     PERFORM IMS-STATUSKONTROLL                                           
133800     .                                                                    
133900     EJECT                                                                
134000 IMS-GNP-D905-FIRST SECTION.                                              
134100     SKIP2                                                                
134200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
134300          DELIMITED BY SIZE INTO SSA1                                     
134400     STRING 'WDD905  *F(KDAVROP  =' W-KDAVROP-X ')'                       
134500          DELIMITED BY SIZE INTO SSA2                                     
134600     MOVE '  GE' TO GODK-STATUSKODER                                      
134700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
134800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100     EJECT                                                                
135200 IMS-GNP-D905 SECTION.                                                    
135300     SKIP2                                                                
135400     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
135500          DELIMITED BY SIZE INTO SSA1                                     
135600     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
135700          DELIMITED BY SIZE INTO SSA2                                     
135800     MOVE '  GE' TO GODK-STATUSKODER                                      
135900     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
136000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     EJECT                                                                
136400 IMS-GU-K601 SECTION.                                                     
136500                                                                          
136600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
136700          DELIMITED BY SIZE INTO SSA1                                     
136800     MOVE '  GE' TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
137000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300     EJECT                                                                
137400 IMS-GNP-K611 SECTION.                                                    
137500                                                                          
137600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
137700          DELIMITED BY SIZE INTO SSA1                                     
137800     MOVE '  GE' TO GODK-STATUSKODER                                      
137900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
138000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
138100     PERFORM IMS-STATUSKONTROLL                                           
138200     .                                                                    
138300     EJECT                                                                
138400 IMS-GU-K901           SECTION.                                           
138500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
138600            DELIMITED BY SIZE INTO SSA1                                   
138700     MOVE '  GE' TO GODK-STATUSKODER                                      
138800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
138900     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
139000     PERFORM IMS-STATUSKONTROLL                                           
139100     .                                                                    
139200     EJECT                                                                
139300 IMS-GN-A5A1 SECTION.                                                     
139400                                                                          
139500     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN                          
139600                    '&WDA5A1KY<=' W-WDA5A1KY-MAX  ')'                     
139700            DELIMITED BY SIZE INTO SSA1                                   
139800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
139900     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-WDA5A1 SSA1                    
140000     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
140100     PERFORM IMS-STATUSKONTROLL                                           
140200     .                                                                    
140300     EJECT                                                                
140400 IMS-GU-L201 SECTION.                                                     
140500*                                                                         
140600     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
140700             DELIMITED BY SIZE INTO SSA1                                  
140800     MOVE '  GE' TO GODK-STATUSKODER                                      
140900     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
141000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
141100     PERFORM IMS-STATUSKONTROLL                                           
141200     .                                                                    
141300     EJECT                                                                
141400 IMS-GU-L221 SECTION.                                                     
141500*                                                                         
141600     MOVE 'WDL211   ' TO SSA1                                             
141700     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
141800             DELIMITED BY SIZE INTO SSA2                                  
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
142100     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     EJECT                                                                
142500 IMS-STATUSKONTROLL SECTION.                                              
142600                                                                          
142700     SET STATUS-IX TO 1                                                   
142800     SEARCH GODK-STATUS                                                   
142900       AT END                                                             
143000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
143100         DELIMITED BY SIZE INTO FELTEXT                                   
143200         CALL FELLOG                                                      
143300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
143400         CONTINUE                                                         
143500     END-SEARCH                                                           
143600     .                                                                    
143700     EJECT                                                                
143800*    -COPY WY2000P1                                                       
143900     EJECT                                                                
144000*    -COPY WY2000P2                                                       
144100     EJECT                                                                
144200*    -COPY WY2000Q3                                                       
