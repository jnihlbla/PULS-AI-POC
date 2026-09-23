000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2034400.                                                
000300 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000400 DATE-WRITTEN.   SEP 2002.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV CROSS DOCKING INFO PÅ WDK711                      
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*                              WDK6                                       
001400*                              WDP7 (USERDATABASEN)                       
001500*                              W6D1 (INLEVERANSER)                        
001600*                              WDD3 (BENÄMNINGAR)                         
001700*                              W6G1 (VERIFIERAR LAGEROMRÅDEN)             
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WDP7 (USERDATABASEN)                       
002000*                              WDK7                                       
002100*                              WDK6                                       
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W2T344                                              
002500*        MID:         W2I34401                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W2O34401                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W2034400'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  FOERSTA-RADEN               PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  DAGENS-DATUM                PIC X(6)    VALUE SPACE.                 
004800                                                                          
004900 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  IX-K6                       PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  RAD-MAX                     PIC S9(4)  VALUE +11   COMP SYNC.        
005200 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
005300 77  IDSID-RAKN                  PIC S9(3)  VALUE ZERO.                   
005400*                                                                         
005500*    --- TAB-MAX GÄLLER FÖR                                               
005600*    --- TAB-BORTTAG-K6, TAB-BORTTAG SAMT TAB-BEFINTLIG                   
005700*                                                                         
005800 77  TAB-MAX                     PIC S9(4)  VALUE +4    COMP SYNC.        
005900                                                                          
006000     EJECT                                                                
006100 01  FILLER                      PIC X(16) VALUE 'TAB BORTTAG K6'.        
006200                                                                          
006300*    --- TABELL FÖR LAGEROMRÅDEN SOM SKA TAS BORT FRÅN WDK6               
006400                                                                          
006500 01  TAB-BORTTAG-K6.                                                      
006600  05 IX-BORT-K6                  PIC S9(3)   VALUE ZERO.                  
006700  05 FILLER  OCCURS 4.                                                    
006800   07 TAB-LO-CD-BORT-K6          PIC 9(3)    VALUE ZERO.                  
006900                                                                          
007000                                                                          
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16) VALUE 'TABELL BORTTAG'.        
007300                                                                          
007400*    --- TABELL FÖR RADER SOM SKA TAS BORT                                
007500                                                                          
007600 01  TAB-BORTTAG.                                                         
007700  05 IX-BORT                     PIC S9(3)   VALUE ZERO.                  
007800  05 FILLER  OCCURS 4.                                                    
007900   07 TAB-LO-CD-BORTTAG          PIC 9(3)    VALUE ZERO.                  
008000   07 TAB-ANT-BORTTAG            PIC 9(3)    VALUE ZERO.                  
008100                                                                          
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)                                
008400                                         VALUE 'TABELL BEFINTLIG'.        
008500                                                                          
008600*    --- TABELL FÖR LAGEROMRÅDEN CROSS DOCKING SOM ANVÄNDS                
008700                                                                          
008800 01  TAB-BEFINTLIG.                                                       
008900  05 IX-BEF                      PIC S9(3)   VALUE ZERO.                  
009000  05 FILLER  OCCURS 4.                                                    
009100   07 TAB-LO-CD-BEF              PIC 9(3)    VALUE ZERO.                  
009200   07 TAB-ANT-BEF                PIC 9(3)    VALUE ZERO.                  
009300                                                                          
009400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009500     88  INDATA-OK                           VALUE 'J'.                   
009600     88  INDATA-FEL                          VALUE 'N'.                   
009700                                                                          
009800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009900     88  NYCKLAR-OK                          VALUE 'J'.                   
010000     88  NYCKLAR-FEL                         VALUE 'N'.                   
010100                                                                          
010200 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
010300     88  UPD-RAD-JA                          VALUE 'J'.                   
010400     88  UPD-RAD-NEJ                         VALUE 'N'.                   
010500                                                                          
010600 77  NY-RAD-SW                   PIC X       VALUE 'N'.                   
010700     88  NY-RAD-JA                           VALUE 'J'.                   
010800     88  NY-RAD-NEJ                          VALUE 'N'.                   
010900                                                                          
011000                                                                          
011100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011200     88  EGEN-MID                            VALUE '2344'.                
011300     88  GODK-MID                            VALUE '2344'.                
011400     88  HELP-MID                            VALUE '0551'.                
011500                                                                          
011510 77  CDART-SW                    PIC X       VALUE 'J'.                   
011520     88  CDART-OK                            VALUE 'J'.                   
011530     88  CDART-FEL                           VALUE 'N'.                   
011540                                                                          
011600*    --- GENERELLA ARBETSAREAOR.                                          
011700                                                                          
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'WS'.                  
012000     SKIP3                                                                
012100 01  WS.                                                                  
012200*********************************************************                 
012300*    WS-MSGI-AREA-2344                                                    
012400*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
012500*           (I MSGI-SPAR-AREA)                                            
012600*********************************************************                 
012700  05 WS-MSGI-AREA-2344.                                                   
012800    10 WS-MSGI-IDTRANS-2344      PIC X(4)    VALUE '2344'.                
012900    10 WS-MSGI-SSA-KEY-ENTER.                                             
013000      15  WS-ENTER-IDDC          PIC X(2)    VALUE SPACE.                 
013100    10 WS-MSGI-SSA-KEY-NEXT.                                              
013200      15  WS-NEXT-IDDC           PIC X(2)    VALUE SPACE.                 
013300    10 WS-MSGI-SCREEN-GRP OCCURS 11.                                      
013400      15  WS-MSGI-IDDC           PIC X(2)    VALUE SPACE.                 
013500      15  WS-MSGI-ADLAGOMR-CD    PIC 9(3)    VALUE ZERO.                  
013600    10 FILLER                    PIC X(900)  VALUE SPACE.                 
013700                                                                          
013800  05 FILLER                      PIC X(16)   VALUE 'WS-IDARTNR'.          
013900  05 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
014000  05 WS-IDDC                     PIC X(2)    VALUE SPACE.                 
014100  05 WS-TEMFSFEL                 PIC X(40)   VALUE SPACE.                 
014200  05 WS-TEMFSINFO                PIC X(40)   VALUE SPACE.                 
014300  05 WS-ADLAGOMR-NUM             PIC 9(3)    VALUE ZERO.                  
014400                                                                          
014500  05 FILLER                      PIC X(16)   VALUE 'WS-SECTION'.          
014600  05 WS-SECTION                  PIC X(16)   VALUE SPACE.                 
014700*                                                                         
017500       EJECT                                                              
017600     EJECT                                                                
017700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017800 01  GENERELLA-SUBPROGRAM.                                                
017900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
018100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
018500     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
018600     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
018700     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
018800     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
018900     EJECT                                                                
019000*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
019100 01  TABENTRY-PARM.                                                       
019200     03  STEGLANGD               PIC S9(9) COMP  VALUE 88.                
019300     03  ANTAL                   PIC S9(9) COMP.                          
019400     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 10.                
019500                                                                          
019600     EJECT                                                                
019700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019800*01 -COPY WMSGINIT                                                        
019900     EJECT                                                                
020000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020100*01 -COPY WMEDAREA                                                        
020200     SKIP3                                                                
020300 01  MESSAGE-CODES.                                                       
020400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
020500     03  CONFLICT                PIC X(3)    VALUE '002'.                 
020600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020700     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
020800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
020900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
021000     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
021100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
021200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
021300     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
021400     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
021500     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
021600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021700     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
021800     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
021900     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
022000     SKIP3                                                                
022100 01  FELTEXTER.                                                           
022200     03  MED-1                  PIC X(40)                                 
022300         VALUE 'FEL FÖRPACKNINGSTYP           '.                          
022400     03  MED-2                  PIC X(40)                                 
022500         VALUE 'LO SAKNAS, ANVÄND BILD 6106   '.                          
022600     03  MED-3                  PIC X(40)                                 
022700         VALUE 'EJ TILLÅTET, SALDO FINNS KVAR '.                          
022800     03  MED-4                  PIC X(40)                                 
022900         VALUE 'EJ TILLÅTET, 4 LAGEROMR FINNS '.                          
023000     03  MED-5                  PIC X(40)                                 
023100         VALUE 'EJ TILLÅTET, INLEVERANSER FINNS '.                        
023200     03  MED-6                  PIC X(40)                                 
023300         VALUE 'OBS EJ AUTOMATISK REFILLBEORDRING'.                       
023400     03  MED-7                  PIC X(40)                                 
023500         VALUE 'OBS MIN HEM EJ = Q3 : ÄNDRA !!!   '.                      
023600     03  MED-8                  PIC X(40)                                 
023700         VALUE 'OBS Q3 EJ UPPDATERAD          '.                          
023710     03  MED-9                  PIC X(40)                                 
023720         VALUE 'EJ CD ARTIKEL'.                                           
023800                                                                          
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
024100     SKIP3                                                                
024200 01  DAT-IO-AREA.                                                         
024300*    03  -COPY WDATAREA                                                   
024400     EJECT                                                                
024500                                                                          
024600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024700*                                                                         
024800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024900     SKIP3                                                                
025000*01  MID -COPY W2I34401                                                   
025100     EJECT                                                                
025200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025300     SKIP3                                                                
025400*01  -COPY WMSGAREA                                                       
025500     EJECT                                                                
025600     03  MOD REDEFINES MSG-AREA.                                          
025700*      05  -COPY W2O34401                                                 
025800     EJECT                                                                
025900*    --- AREOR FÖR W006KOM SUBMODUL                                       
026000*                                                                         
026100 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
026200*01  -COPY WMSGKOM                                                        
026300     EJECT                                                                
026400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026500     SKIP3                                                                
026600*01  -COPY WMFSAREA                                                       
026700     EJECT                                                                
026800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026900*                                                                         
027000     EJECT                                                                
027100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027200     SKIP3                                                                
027300 01  NYCKLAR-TILL-DLI.                                                    
027400     03  W-IDARTNR-X.                                                     
027500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027600                                                                          
027700     03  W-IDDC-X.                                                        
027800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
027900                                                                          
028000     03  W-IDSKYLT-X.                                                     
028100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
028200                                                                          
028300     03  W-KDSEGKEY-X.                                                    
028400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
028500                                                                          
028600     03  W-W6GXKEY-X.                                                     
028700         05  FILLER          PIC X(4)    VALUE '6005'.                    
028800         05  FILLER          PIC X(2)    VALUE '11'.                      
028900         05  FILLER          PIC X(24)   VALUE LOW-VALUE.                 
029000     03  W-ADINLOMR-X.                                                    
029100         05  W-ADINLOMR      PIC X(4)    VALUE SPACE.                     
029200                                                                          
029300                                                                          
029400                                                                          
029500     EJECT                                                                
029600*    --- STATUS-KOD FRÅN IMS                                              
029700 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
029800 01  STATUS-WS                   PIC XX.                                  
029900     88  SEGMENT-FINNS                       VALUE '  '.                  
030000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030100     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
030200                                                   'GB'.                  
030300     SKIP2                                                                
030400 01  GODK-STATUSKODER.                                                    
030500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030600     SKIP3                                                                
030700     EJECT                                                                
030800 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
030900     SKIP3                                                                
031000 01  SSA1                        PIC X(160).                              
031100 01  SSA2                        PIC X(128).                              
031200 01  SSA3                        PIC X(128).                              
031300     EJECT                                                                
031400                                                                          
031500*    --- IMS FUNKTIONSKODER                                               
031600*01  -COPY W0003                                                          
031700     EJECT                                                                
031800*    ---  DLI INPUT-OUTPUT AREA                                           
031900     EJECT                                                                
032000                                                                          
032100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
032200 01  DLI-IO-WDK601.                                                       
032300*    03  -COPY WDK601                                                     
032400     EJECT                                                                
032500                                                                          
032600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
032700 01  DLI-IO-WDK611.                                                       
032800*    03  -COPY WDK611                                                     
032900     EJECT                                                                
033000                                                                          
033100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
033200 01  DLI-IO-WDK701.                                                       
033300*    03  -COPY WDK701                                                     
033400     EJECT                                                                
033500                                                                          
033600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
033700 01  DLI-IO-WDK711.                                                       
033800*    03  -COPY WDK711                                                     
033900     EJECT                                                                
034000                                                                          
034100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
034200 01  DLI-IO-WDD301.                                                       
034300*    03  -COPY WDD301                                                     
034400     EJECT                                                                
034500                                                                          
034600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
034700 01  DLI-IO-WDD311.                                                       
034800*    03  -COPY WDD311                                                     
034900     EJECT                                                                
035000                                                                          
035100     EJECT                                                                
035200 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6G130'.                      
035300 01  DLI-IO-W6G130.                                                       
035400*    03  -COPY W6GX6006                                                   
035500     EJECT                                                                
035600                                                                          
035700     EJECT                                                                
035800 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6D1I11'.                     
035900 01  DLI-IO-W6D1I11.                                                      
036000*    03  -COPY W6D111 -PRE I-                                             
036100     EJECT                                                                
036200                                                                          
036300     EJECT                                                                
036400                                                                          
036500 LINKAGE SECTION.                                                         
036600                                                                          
036700*01  -COPY W0009   -PRE MSG-                                              
036800     EJECT                                                                
036900*01  -COPY W0008   -PRE WDP7-                                             
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008   -PRE WDK6-                                             
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008   -PRE WDK7-                                             
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008   -PRE WDD3-                                             
037900     05  FILLER                  PIC X.                                   
038000     EJECT                                                                
038100*01  -COPY W0008   -PRE W6G1-                                             
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008   -PRE W6D1-                                             
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB                      
038800                           WDK7-PCB WDD3-PCB W6G1-PCB W6D1-PCB.           
038900 MAIN SECTION.                                                            
039000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB                      
039100                           WDK7-PCB WDD3-PCB W6G1-PCB W6D1-PCB.           
039200                                                                          
039300     PERFORM IMS-GET-MSG                                                  
039400                                                                          
039500     IF SEGMENT-FINNS                                                     
039600        PERFORM A-INIT                                                    
039700        PERFORM B-KOLLA-NYCKLAR                                           
039800                                                                          
039900        IF NYCKLAR-OK                                                     
040000                                                                          
040100           IF MFS-UPDATE                                                  
040200              PERFORM G-KOLLA-INPUT                                       
040300                                                                          
040400              IF INDATA-OK                                                
040500                 PERFORM H-UPPDATERA                                      
040600              END-IF                                                      
040700                                                                          
040800           ELSE                                                           
040900             IF MFS-FIRST                                                 
041000                PERFORM C-FOERSTA-SIDA                                    
041100             ELSE                                                         
041200                IF MFS-NEXT                                               
041300                   PERFORM D-NAESTA-SIDA                                  
041400                ELSE                                                      
041500                   PERFORM E-SAMMA-SIDA                                   
041600                END-IF                                                    
041700             END-IF                                                       
041800           END-IF                                                         
041900                                                                          
042000           IF INDATA-OK                                                   
042100              PERFORM F-LAES-VISA-INFO                                    
042200           ELSE                                                           
042300              PERFORM MFS-STAENG-FAELT-RADER-IN                           
042400           END-IF                                                         
042500                                                                          
042600* ---    UPPDATERA MSGI-SPAR-AREA                                         
042700           MOVE '002'           TO MSGI-KDCALL                            
042800           MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                      
042900           MOVE MSG-SIGNON-USERID                                         
043000                                TO MSGI-IDUSER                            
043100           MOVE '2344'          TO MSGI-IDTRANS                           
043200           MOVE WS-MSGI-AREA-2344                                         
043300                                TO MSGI-SPAR-AREA                         
043400           CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                     
043500                                                                          
043600        END-IF                                                            
043700                                                                          
043800        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O34401 + 4                     
043900        PERFORM IMS-INSERT-MSG                                            
044000     END-IF                                                               
044100                                                                          
044200     MOVE ZERO TO RETURN-CODE                                             
044300     GOBACK                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 A-INIT SECTION.                                                          
044700                                                                          
044800     IF MSG-DUBBLA-TRANSKODER                                             
044900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34401                 
045000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
045100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
045200     ELSE                                                                 
045300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I34401                  
045400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
045500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
045600     END-IF                                                               
045700                                                                          
045800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
045900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
046000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
046100                                                                          
046200     MOVE LOW-VALUE TO MSG-AREA                                           
046300     MOVE 'W2O344N1' TO MFS-IDMOD                                         
046400     MOVE '2344' TO MOD-IDTRANS                                           
046500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
046600                                                                          
046700     IF EGEN-MID OR HELP-MID                                              
046800        CONTINUE                                                          
046900     ELSE                                                                 
047000        MOVE SPACE TO MFS-KDTRTYP                                         
047100        MOVE '7'   TO MFS-IDPFK                                           
047200     END-IF                                                               
047300     ACCEPT DAGENS-DATUM FROM DATE                                        
047400     MOVE 'S  '              TO MED-IDSKYLT                               
047500     MOVE SPACE              TO WS-TEMFSFEL                               
047600                                WS-TEMFSINFO                              
047700     .                                                                    
047800     EJECT                                                                
047900 B-KOLLA-NYCKLAR SECTION.                                                 
048000                                                                          
048100     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
048200                                WS-MSGI-SSA-KEY-NEXT                      
048300                                                                          
048400******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
048500******   I SLUTET AV PROGRAMMET                                           
048600     MOVE ALL '+'            TO MSGI-WMSGINIT                             
048700     MOVE '001'              TO MSGI-KDCALL                               
048800     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
048900     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
049000     MOVE '2344'             TO MSGI-IDTRANS                              
049100     IF EGEN-MID                                                          
049200       MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                              
049300     ELSE                                                                 
049400       IF  MID-IDARTNR-IN NUMERIC                                         
049500       AND MID-IDARTNR-IN > ZERO                                          
049600         MOVE MID-IDARTNR-IN                                              
049700                             TO MSGI-IDARTNR                              
049800       END-IF                                                             
049900     END-IF                                                               
050000     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
050100                                                                          
050200     IF EGEN-MID                                                          
050300     AND MSGI-SPAR-AREA(1:4) = '2344'                                     
050400       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-2344                         
050500     END-IF                                                               
050600                                                                          
050700                                                                          
050800     MOVE JA TO NYCKLAR-SW                                                
050900     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
051000                                                                          
051100     MOVE MSGI-IDARTNR       TO WS-IDARTNR                                
051200     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
051300                                                                          
051400     IF MID-IDARTNR-IN = ALL '+'                                          
051500       CONTINUE                                                           
051600     ELSE                                                                 
051700       MOVE '7'         TO MFS-IDPFK                                      
051800       MOVE SPACE       TO MFS-KDTRTYP                                    
051900     END-IF                                                               
052000                                                                          
052100     IF WS-IDARTNR NUMERIC                                                
052200       CONTINUE                                                           
052300     ELSE                                                                 
052400       MOVE NEJ              TO NYCKLAR-SW                                
052500     END-IF                                                               
052600                                                                          
052700     MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                            
052800                                W-IDARTNR                                 
052900     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
053000                                                                          
053100                                                                          
053200*  -- KONTROLL AV IDDC                                                    
053300                                                                          
053400     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
053500                                                                          
053600     IF EGEN-MID                                                          
053700       IF MID-IDDC-IN = ALL '+'                                           
053800         MOVE MID-IDDC-UT    TO WS-IDDC                                   
053900       ELSE                                                               
054000         MOVE MID-IDDC-IN    TO WS-IDDC                                   
054100         MOVE '7'            TO MFS-IDPFK                                 
054200         MOVE SPACE          TO MFS-KDTRTYP                               
054300       END-IF                                                             
054400     ELSE                                                                 
054500         MOVE SPACE          TO WS-IDDC                                   
054600     END-IF                                                               
054700     MOVE WS-IDDC            TO MOD-IDDC-UT                               
054800                                                                          
054900                                                                          
055000     IF NYCKLAR-FEL                                                       
055100        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
055200        CALL WMEDKONV USING MED-WMEDAREA                                  
055300                                                                          
055400        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
055500        PERFORM MFS-RENSA-FAELT-IN                                        
055600        PERFORM MFS-RENSA-FAELT-UT                                        
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 C-FOERSTA-SIDA SECTION.                                                  
056100                                                                          
056200     MOVE 'S  '              TO MED-IDSKYLT                               
056300     MOVE INF-FIRST-PAGE     TO MED-IDMFSFEL                              
056400     CALL WMEDKONV USING MED-WMEDAREA                                     
056500     MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                              
056600                                                                          
056700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
056800     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
056900                                WS-MSGI-SSA-KEY-NEXT                      
057000     PERFORM MFS-RENSA-FAELT-IN                                           
057100     .                                                                    
057200     EJECT                                                                
057300 D-NAESTA-SIDA SECTION.                                                   
057400                                                                          
057500     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
057600     .                                                                    
057700     EJECT                                                                
057800 E-SAMMA-SIDA SECTION.                                                    
057900                                                                          
058000     IF MID-INPUT = ALL '+'                                               
058100       PERFORM MFS-RENSA-FAELT-IN                                         
058200     ELSE                                                                 
058300       IF EGEN-MID OR HELP-MID                                            
058400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
058500         CALL WMEDKONV USING MED-WMEDAREA                                 
058600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
058700         PERFORM MFS-LAES-IN-IGEN                                         
058800                                                                          
058900         PERFORM EA-MID-INDATA-TILL-MOD                                   
059000       ELSE                                                               
059100         PERFORM MFS-RENSA-FAELT-IN                                       
059200       END-IF                                                             
059300     END-IF                                                               
059400     .                                                                    
059500 EA-MID-INDATA-TILL-MOD SECTION.                                          
059600* * * * * FÖR VARJE MID-FÄLT                                              
059700* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
059800* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
059900                                                                          
060000     MOVE +1 TO IX                                                        
060100     PERFORM UNTIL IX > RAD-MAX                                           
060200       IF MID-CMD (IX) = ALL '+'                                          
060300         MOVE MFS-RENSA-FAELT                                             
060400                             TO MOD-CMD(IX)                               
060500       ELSE                                                               
060600         MOVE MID-CMD (IX)   TO MOD-CMD (IX)                              
060700       END-IF                                                             
060800                                                                          
060900       IF MID-ADLAGOMR-CD (IX) = ALL '+'                                  
061000         MOVE MFS-RENSA-FAELT                                             
061100                             TO MOD-ADLAGOMR-CD-IN (IX)                   
061200       ELSE                                                               
061300         MOVE MID-ADLAGOMR-CD (IX)                                        
061400                             TO MOD-ADLAGOMR-CD-IN (IX)                   
061500       END-IF                                                             
061600                                                                          
061700       IF MID-KVDAGAR-CDBEH (IX) = ALL '+'                                
061800         MOVE MFS-RENSA-FAELT                                             
061900                             TO MOD-KVDAGAR-CDBEH-IN (IX)                 
062000       ELSE                                                               
062100         MOVE MID-KVDAGAR-CDBEH (IX)                                      
062200                             TO MOD-KVDAGAR-CDBEH-IN (IX)                 
062300       END-IF                                                             
062400                                                                          
062500       IF MID-FLCDREL (IX) = ALL '+'                                      
062600         MOVE MFS-RENSA-FAELT                                             
062700                             TO MOD-FLCDREL-IN (IX)                       
062800       ELSE                                                               
062900         MOVE MID-FLCDREL (IX)                                            
063000                             TO MOD-FLCDREL-IN (IX)                       
063100       END-IF                                                             
063210                                                                          
063220       IF MID-TIDATUM-CROSS (IX) = ALL '+'                                
063230         MOVE MFS-RENSA-FAELT                                             
063240                             TO MOD-TIDATUM-CROSS-IN (IX)                 
063250       ELSE                                                               
063260         MOVE MID-TIDATUM-CROSS (IX)                                      
063270                             TO MOD-TIDATUM-CROSS-IN (IX)                 
063280       END-IF                                                             
063290                                                                          
063300       ADD +1 TO IX                                                       
063400     END-PERFORM                                                          
063500                                                                          
063600     IF MID-NY-IDDC = ALL '+'                                             
063700       MOVE MFS-RENSA-FAELT  TO MOD-NY-IDDC                               
063800     ELSE                                                                 
063900       MOVE MID-NY-IDDC      TO MOD-NY-IDDC                               
064000     END-IF                                                               
064100                                                                          
064200     IF MID-NY-ADLAGOMR-CD = ALL '+'                                      
064300       MOVE MFS-RENSA-FAELT  TO MOD-NY-ADLAGOMR-CD                        
064400     ELSE                                                                 
064500       MOVE MID-NY-ADLAGOMR-CD                                            
064600                             TO MOD-NY-ADLAGOMR-CD                        
064700     END-IF                                                               
064800                                                                          
064900     IF MID-NY-KVDAGAR-CDBEH = ALL '+'                                    
065000       MOVE MFS-RENSA-FAELT  TO MOD-NY-KVDAGAR-CDBEH                      
065100     ELSE                                                                 
065200       MOVE MID-NY-KVDAGAR-CDBEH                                          
065300                             TO MOD-NY-KVDAGAR-CDBEH                      
065400     END-IF                                                               
065500                                                                          
065600     IF MID-NY-FLCDREL = ALL '+'                                          
065700       MOVE MFS-RENSA-FAELT  TO MOD-NY-FLCDREL                            
065800     ELSE                                                                 
065900       MOVE MID-NY-FLCDREL                                                
066000                             TO MOD-NY-FLCDREL                            
066100     END-IF                                                               
066200     .                                                                    
066300     EJECT                                                                
066400 F-LAES-VISA-INFO SECTION.                                                
066500                                                                          
066600     PERFORM FA-HAEMTA-BENAEMNING                                         
066700                                                                          
066800     IF EGEN-MID                                                          
066900     AND WS-IDDC NOT = SPACE                                              
067000        MOVE 1               TO IX                                        
067100        MOVE WS-IDARTNR      TO W-IDARTNR                                 
067200        MOVE WS-IDDC         TO W-IDDC                                    
067300        PERFORM IMS-GHU-K711                                              
067400        IF SEGMENT-FINNS                                                  
067500          IF SLAG-ADLAGOMR-CD > ZERO                                      
067600            MOVE SLAG-IDDC   TO MOD-IDDC (IX)                             
067700                                WS-ENTER-IDDC                             
067800                                WS-MSGI-IDDC (IX)                         
067900            MOVE SLAG-ADLAGOMR-CD                                         
068000                             TO MOD-ADLAGOMR-CD (IX)                      
068100                                WS-MSGI-ADLAGOMR-CD (IX)                  
068200            MOVE SLAG-KVDAGAR-CDBEH                                       
068300                             TO MOD-KVDAGAR-CDBEH (IX)                    
068310            MOVE SLAG-TIDATUM-CROSS                                       
068320                             TO MOD-TIDATUM-CROSS (IX)                    
068330            INSPECT MOD-TIDATUM-CROSS (IX)                                
068340                             REPLACING LEADING ZERO BY SPACE              
068400            IF  SLAG-FLCDREL = JA                                         
068500            AND MSGI-IDLAND-SPR = 'GB'                                    
068600              MOVE YES       TO MOD-FLCDREL (IX)                          
068700            ELSE                                                          
068800              MOVE SLAG-FLCDREL                                           
068900                             TO MOD-FLCDREL (IX)                          
069000            END-IF                                                        
069100          END-IF                                                          
069200        ELSE                                                              
069300          MOVE SPACE         TO WS-MSGI-IDDC (IX)                         
069400          MOVE ZERO          TO WS-MSGI-ADLAGOMR-CD (IX)                  
069500          MOVE MFS-STAENG-FAELT                                           
069600                             TO MOD-CMD-ATTR (IX)                         
069700                                MOD-ADLAGOMR-CD-ATTR (IX)                 
069800                                MOD-KVDAGAR-CDBEH-ATTR (IX)               
069900                                MOD-FLCDREL-ATTR (IX)                     
069910                                MOD-TIDATUM-CROSS-ATTR (IX)               
070000          MOVE MFS-RENSA-FAELT                                            
070100                             TO MOD-CMD (IX)                              
070200                                MOD-IDDC (IX)                             
070300                                MOD-ADLAGOMR-CD (IX)                      
070400                                MOD-KVDAGAR-CDBEH (IX)                    
070500                                MOD-FLCDREL (IX)                          
070510                                MOD-TIDATUM-CROSS (IX)                    
070600        END-IF                                                            
070700        ADD 1                TO IX                                        
070800                                                                          
070900     ELSE                                                                 
071000        MOVE WS-IDARTNR      TO W-IDARTNR                                 
071100        IF  WS-ENTER-IDDC = SPACE                                         
071200        AND WS-NEXT-IDDC = SPACE                                          
071300          PERFORM IMS-GU-K701                                             
071400          IF SEGMENT-FINNS                                                
071500            PERFORM IMS-GNP-K711                                          
071600          END-IF                                                          
071700        ELSE                                                              
071800          IF WS-ENTER-IDDC NOT = SPACE                                    
071900            MOVE WS-ENTER-IDDC                                            
072000                             TO W-IDDC                                    
072100          ELSE                                                            
072200            MOVE WS-NEXT-IDDC                                             
072300                             TO W-IDDC                                    
072400          END-IF                                                          
072500          PERFORM IMS-GU-K701                                             
072600          IF SEGMENT-FINNS                                                
072700            PERFORM IMS-GNP-K711                                          
072800            PERFORM UNTIL SEGMENT-SAKNAS                                  
072900            OR SLAG-IDDC = W-IDDC                                         
073000                                                                          
073100              PERFORM IMS-GNP-K711                                        
073200            END-PERFORM                                                   
073300          END-IF                                                          
073400        END-IF                                                            
073500        MOVE 1               TO IX                                        
073600        PERFORM UNTIL SEGMENT-SAKNAS                                      
073700        OR IX > RAD-MAX                                                   
073800                                                                          
073900          IF SLAG-ADLAGOMR-CD > ZERO                                      
074000            IF IX = 1                                                     
074100              MOVE SLAG-IDDC TO WS-ENTER-IDDC                             
074200            END-IF                                                        
074300            MOVE SLAG-IDDC   TO MOD-IDDC (IX)                             
074400                                WS-MSGI-IDDC (IX)                         
074500            MOVE SLAG-ADLAGOMR-CD                                         
074600                             TO MOD-ADLAGOMR-CD (IX)                      
074700                                WS-MSGI-ADLAGOMR-CD (IX)                  
074800            MOVE SLAG-KVDAGAR-CDBEH                                       
074900                             TO MOD-KVDAGAR-CDBEH (IX)                    
074910            MOVE SLAG-TIDATUM-CROSS                                       
074920                             TO MOD-TIDATUM-CROSS (IX)                    
074930            INSPECT MOD-TIDATUM-CROSS (IX)                                
074940                             REPLACING LEADING ZERO BY SPACE              
075000            IF  SLAG-FLCDREL = JA                                         
075100            AND MSGI-IDLAND-SPR = 'GB'                                    
075200              MOVE YES       TO MOD-FLCDREL (IX)                          
075300            ELSE                                                          
075400              MOVE SLAG-FLCDREL                                           
075500                             TO MOD-FLCDREL (IX)                          
075600            END-IF                                                        
075700            ADD 1            TO IX                                        
075800          END-IF                                                          
075900                                                                          
076000          PERFORM IMS-GNP-K711                                            
076100        END-PERFORM                                                       
076200                                                                          
076300       IF SEGMENT-FINNS                                                   
076400          MOVE SLAG-IDDC     TO WS-NEXT-IDDC                              
076500          MOVE 'S  '         TO MED-IDSKYLT                               
076600          MOVE INF-MORE-INFO-EXISTS                                       
076700                             TO MED-IDMFSFEL                              
076800          CALL WMEDKONV USING MED-WMEDAREA                                
076900          MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                              
077000       ELSE                                                               
077100          MOVE SPACE         TO WS-MSGI-SSA-KEY-NEXT                      
077200       END-IF                                                             
077300     END-IF                                                               
077400                                                                          
077500     PERFORM UNTIL IX > RAD-MAX                                           
077600                                                                          
077700       MOVE SPACE            TO WS-MSGI-IDDC (IX)                         
077800       MOVE ZERO             TO WS-MSGI-ADLAGOMR-CD (IX)                  
077900       MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR (IX)                         
078000                                MOD-ADLAGOMR-CD-ATTR (IX)                 
078100                                MOD-KVDAGAR-CDBEH-ATTR (IX)               
078200                                MOD-FLCDREL-ATTR (IX)                     
078210                                MOD-TIDATUM-CROSS-ATTR (IX)               
078300       MOVE MFS-RENSA-FAELT  TO MOD-CMD (IX)                              
078400                                MOD-ADLAGOMR-CD-IN (IX)                   
078500                                MOD-KVDAGAR-CDBEH-IN (IX)                 
078600                                MOD-FLCDREL-IN (IX)                       
078610                                MOD-TIDATUM-CROSS-IN (IX)                 
078700                                                                          
078800       ADD 1                 TO IX                                        
078900                                                                          
079000     END-PERFORM                                                          
079100                                                                          
079200     IF WS-TEMFSINFO NOT = SPACE                                          
079300*                                                                         
079400*************  MED-6 ELLER MED-7 VID UPPDATERING    **************        
079500*                                                                         
079600        MOVE WS-TEMFSINFO    TO MOD-TEMFSFEL                              
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000 FA-HAEMTA-BENAEMNING SECTION.                                            
080100                                                                          
080200     PERFORM IMS-GU-D301-BSEQ                                             
080300     IF SEGMENT-FINNS                                                     
080400       MOVE MED-IDSKYLT      TO W-IDSKYLT                                 
080500       PERFORM IMS-GNP-D311                                               
080600       IF SEGMENT-FINNS                                                   
080700         MOVE TEXT-BEART     TO MOD-BEART                                 
080800       ELSE                                                               
080900         MOVE MFS-RENSA-FAELT                                             
081000                             TO MOD-BEART                                 
081100       END-IF                                                             
081200     END-IF                                                               
081300     .                                                                    
081400     EJECT                                                                
081500 G-KOLLA-INPUT SECTION.                                                   
081600     MOVE 'G-KOLLA-INPUT'    TO WS-SECTION                                
081700                                                                          
081800     MOVE JA                 TO INDATA-SW                                 
081900     MOVE NEJ                TO UPD-RAD-SW                                
082000                                NY-RAD-SW                                 
082100                                                                          
082200     IF  MID-INPUT      = ALL '+'                                         
082300        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
082400        MOVE NEJ TO INDATA-SW                                             
082500     ELSE                                                                 
082600                                                                          
082700        MOVE ZERO            TO TAB-ANT-BORTTAG (1)                       
082800                                TAB-ANT-BORTTAG (2)                       
082900                                TAB-ANT-BORTTAG (3)                       
083000                                TAB-ANT-BORTTAG (4)                       
083100                                TAB-LO-CD-BORTTAG (1)                     
083200                                TAB-LO-CD-BORTTAG (2)                     
083300                                TAB-LO-CD-BORTTAG (3)                     
083400                                TAB-LO-CD-BORTTAG (4)                     
083500        MOVE +1              TO IX                                        
083600        PERFORM UNTIL IX     > RAD-MAX                                    
083700                                                                          
083800         IF MID-RAD (IX) NOT = ALL '+'                                    
083900            MOVE JA          TO UPD-RAD-SW                                
084000          IF MID-CMD (IX) NOT = ALL '+'                                   
084300            IF MID-CMD (IX) = 'B'                                         
084400            OR MID-CMD (IX) = 'D'                                         
084500              MOVE MFS-ALFA-FAELT-RAETT                                   
084600                             TO MOD-CMD-ATTR(IX)                          
084700              PERFORM GA-UPD-TABELL                                       
084800            ELSE                                                          
084900              MOVE MFS-ALFA-FAELT-FEL                                     
085000                             TO MOD-CMD-ATTR (IX)                         
085100              MOVE ERR-CORR-HILITE-FLDS                                   
085200                             TO MED-IDMFSFEL                              
085300              MOVE NEJ       TO INDATA-SW                                 
085400            END-IF                                                        
085500          ELSE                                                            
085600            MOVE MFS-RENSA-FAELT                                          
085700                             TO MOD-CMD-ATTR (IX)                         
085800                                                                          
085810            PERFORM GC-KOLLA-CD-ARTIKEL                                   
085820                                                                          
085830            IF CDART-FEL                                                  
085840              MOVE MED-9 TO WS-TEMFSFEL                                   
085850              MOVE NEJ TO INDATA-SW                                       
085860            ELSE                                                          
085900              IF MID-ADLAGOMR-CD (IX) = ALL '+'                           
086000                MOVE MFS-RENSA-FAELT                                      
086100                               TO MOD-ADLAGOMR-CD (IX)                    
086200              ELSE                                                        
086300                IF MID-ADLAGOMR-CD (IX) NUMERIC                           
086400                  IF MID-ADLAGOMR-CD (IX) NOT =                           
086500                     WS-MSGI-ADLAGOMR-CD (IX)                             
086600                       PERFORM IMS-GU-K611                                
086700                       MOVE MID-ADLAGOMR-CD (IX)                          
086800                               TO WS-ADLAGOMR-NUM                         
086900                       MOVE 1 TO IX-K6                                    
087000                       PERFORM UNTIL IX-K6 > TAB-MAX                      
087100                       OR CLAG-ADLAGOMR-CD (IX-K6) = ZERO                 
087200                       OR CLAG-ADLAGOMR-CD (IX-K6) =                      
087300                          WS-ADLAGOMR-NUM                                 
087400                         ADD 1 TO IX-K6                                   
087500                       END-PERFORM                                        
087600                       IF IX-K6 > TAB-MAX                                 
087700                         MOVE MFS-NUM-FAELT-FEL                           
087800                                  TO MOD-ADLAGOMR-CD-ATTR (IX)            
087900                         MOVE MED-4 TO WS-TEMFSFEL                        
088000                         MOVE NEJ TO INDATA-SW                            
088100                       ELSE                                               
088200                         PERFORM GA-UPD-TABELL                            
088300                       END-IF                                             
088400                  END-IF                                                  
088500                  MOVE MFS-NUM-FAELT-FEL                                  
088600                               TO MOD-ADLAGOMR-CD-ATTR (IX)               
088700                ELSE                                                      
088800                  MOVE MFS-NUM-FAELT-FEL                                  
088900                               TO MOD-ADLAGOMR-CD-ATTR (IX)               
089000                  MOVE ERR-CORR-HILITE-FLDS                               
089100                               TO MED-IDMFSFEL                            
089200                  MOVE NEJ   TO INDATA-SW                                 
089300                END-IF                                                    
089400              END-IF                                                      
089500                                                                          
089600              IF MID-KVDAGAR-CDBEH (IX) = ALL '+'                         
089700                MOVE MFS-RENSA-FAELT                                      
089800                               TO MOD-KVDAGAR-CDBEH-ATTR (IX)             
089900              ELSE                                                        
090000                IF MID-KVDAGAR-CDBEH (IX) NUMERIC                         
090100                  MOVE MFS-NUM-FAELT-FEL                                  
090200                               TO MOD-KVDAGAR-CDBEH-ATTR (IX)             
090300                ELSE                                                      
090400                  MOVE MFS-NUM-FAELT-FEL                                  
090500                               TO MOD-KVDAGAR-CDBEH-ATTR (IX)             
090600                  MOVE ERR-CORR-HILITE-FLDS                               
090700                               TO MED-IDMFSFEL                            
090800                  MOVE NEJ   TO INDATA-SW                                 
090900                END-IF                                                    
091000              END-IF                                                      
091100                                                                          
091200              IF MID-FLCDREL (IX) = ALL '+'                               
091300                MOVE MFS-RENSA-FAELT                                      
091400                               TO MOD-FLCDREL-ATTR (IX)                   
091500              ELSE                                                        
091600                IF MID-FLCDREL (IX) = JA                                  
091700                OR MID-FLCDREL (IX) = YES                                 
091800                OR MID-FLCDREL (IX) = NEJ                                 
091900                  MOVE MFS-ALFA-FAELT-RAETT                               
092000                               TO MOD-FLCDREL-ATTR (IX)                   
092100                ELSE                                                      
092200                  MOVE MFS-ALFA-FAELT-FEL                                 
092300                               TO MOD-FLCDREL-ATTR (IX)                   
092400                  MOVE ERR-CORR-HILITE-FLDS                               
092500                               TO MED-IDMFSFEL                            
092600                  MOVE NEJ   TO INDATA-SW                                 
092700                END-IF                                                    
092800              END-IF                                                      
092801                                                                          
092810              IF MID-TIDATUM-CROSS (IX) = ALL '+'                         
092820                MOVE MFS-RENSA-FAELT                                      
092830                               TO MOD-TIDATUM-CROSS-ATTR (IX)             
092840              ELSE                                                        
092850                IF MID-TIDATUM-CROSS (IX) NUMERIC                         
092851                AND (MID-TIDATUM-CROSS (IX) > DAGENS-DATUM                
092852                OR MID-TIDATUM-CROSS (IX) = ZERO)                         
092853***               VALIDATE USER ENTERDED DELAY DATE                       
092854***                                                                       
092856                  MOVE 'AAMMDD'                                           
092857                               TO DAT-KDDATFORM                           
092858                  MOVE MID-TIDATUM-CROSS (IX)                             
092859                               TO DAT-I-TIDATUM                           
092860                                                                          
092861                  CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM         
092862                                      DAT-O-TIDATUM DAT-KDSVAR            
092863                                                                          
092864                  IF DAT-KDSVAR-OK                                        
092865                  OR MID-TIDATUM-CROSS (IX) = ZERO                        
092866                     MOVE MFS-NUM-FAELT-RAETT                             
092870                               TO MOD-TIDATUM-CROSS-ATTR (IX)             
092880                  ELSE                                                    
092890                     MOVE MFS-NUM-FAELT-FEL                               
092891                               TO MOD-TIDATUM-CROSS-ATTR (IX)             
092892                     MOVE ERR-CORR-HILITE-FLDS                            
092893                               TO MED-IDMFSFEL                            
092894                     MOVE NEJ TO INDATA-SW                                
092895                  END-IF                                                  
092896                ELSE                                                      
092900                  MOVE MFS-NUM-FAELT-FEL                                  
092901                               TO MOD-TIDATUM-CROSS-ATTR (IX)             
092902                  MOVE ERR-CORR-HILITE-FLDS                               
092903                               TO MED-IDMFSFEL                            
092904                  MOVE NEJ   TO INDATA-SW                                 
092906                END-IF                                                    
092907              END-IF                                                      
092908            END-IF                                                        
092909                                                                          
092910          END-IF                                                          
093000         END-IF                                                           
093100                                                                          
093200            ADD 1 TO IX                                                   
093300        END-PERFORM                                                       
093400                                                                          
093500        IF TAB-LO-CD-BORTTAG (1) > ZERO                                   
093600          PERFORM GB-KTRL-TABELL                                          
093700        END-IF                                                            
093800                                                                          
093900                                                                          
094000        IF   MID-NY-IDDC = ALL '+'                                        
094100        AND MID-NY-ADLAGOMR-CD = ALL '+'                                  
094200        AND MID-NY-KVDAGAR-CDBEH = ALL '+'                                
094300        AND MID-NY-FLCDREL = ALL '+'                                      
094400           CONTINUE                                                       
094500        ELSE                                                              
094510          PERFORM GC-KOLLA-CD-ARTIKEL                                     
094520                                                                          
094530          IF CDART-FEL                                                    
094540            MOVE MED-9 TO WS-TEMFSFEL                                     
094550            MOVE NEJ TO INDATA-SW                                         
094560          ELSE                                                            
094600            MOVE JA          TO NY-RAD-SW                                 
094700            MOVE MID-NY-IDDC TO W-IDDC                                    
094800            PERFORM IMS-GHU-K711                                          
094900            IF SEGMENT-FINNS                                              
095000              MOVE MFS-ALFA-FAELT-RAETT                                   
095100                               TO MOD-NY-IDDC-ATTR                        
095200              IF SLAG-ADLAGOMR-CD = ZERO                                  
095300                PERFORM IMS-GU-K611                                       
095400                IF SEGMENT-FINNS                                          
095500                AND (CLAG-BEFT = 75                                       
095600                OR CLAG-BEFT = 76                                         
095700                OR CLAG-BEFT = 92                                         
095800                OR CLAG-BEFT = 95                                         
095900                OR CLAG-BEFT = 99)                                        
096000                  INSPECT MID-NY-ADLAGOMR-CD                              
096100                            REPLACING LEADING SPACE BY ZERO               
096200                  IF MID-NY-ADLAGOMR-CD NUMERIC                           
096300                    MOVE MID-NY-ADLAGOMR-CD                               
096400                               TO WS-ADLAGOMR-NUM                         
096500                    MOVE 1   TO IX-K6                                     
096600                    PERFORM UNTIL IX-K6 > TAB-MAX                         
096700                    OR CLAG-ADLAGOMR-CD (IX-K6) = ZERO                    
096800                    OR CLAG-ADLAGOMR-CD (IX-K6) =                         
096900                       WS-ADLAGOMR-NUM                                    
097000                      ADD 1  TO IX-K6                                     
097100                    END-PERFORM                                           
097200                    IF IX-K6 > TAB-MAX                                    
097300                      MOVE MFS-NUM-FAELT-FEL                              
097400                               TO MOD-NY-ADLAGOMR-CD-ATTR                 
097500                      MOVE MED-4 TO WS-TEMFSFEL                           
097600                      MOVE NEJ TO INDATA-SW                               
097700                    ELSE                                                  
097800                      MOVE MID-NY-ADLAGOMR-CD                             
097900                               TO W-ADINLOMR                              
098000                      PERFORM IMS-GU-W6G130                               
098100                      IF SEGMENT-FINNS                                    
098200                      AND 6006-FLCDOMR = JA                               
098300                        MOVE MFS-NUM-FAELT-RAETT                          
098400                               TO MOD-NY-ADLAGOMR-CD-ATTR                 
098500                      ELSE                                                
098600                        MOVE MFS-NUM-FAELT-FEL                            
098700                               TO MOD-NY-ADLAGOMR-CD-ATTR                 
098800                        MOVE MED-2                                        
098900                               TO WS-TEMFSFEL                             
099000                        MOVE NEJ TO INDATA-SW                             
099100                      END-IF                                              
099200                    END-IF                                                
099300                  ELSE                                                    
099400                    MOVE MFS-NUM-FAELT-FEL                                
099500                               TO MOD-NY-ADLAGOMR-CD-ATTR                 
099600                      MOVE ERR-CORR-HILITE-FLDS                           
099700                               TO MED-IDMFSFEL                            
099800                    MOVE NEJ TO INDATA-SW                                 
099900                  END-IF                                                  
100000                  INSPECT MID-NY-KVDAGAR-CDBEH                            
100100                            REPLACING LEADING SPACE BY ZERO               
100200                  IF MID-NY-KVDAGAR-CDBEH NUMERIC                         
100300                    MOVE MFS-NUM-FAELT-RAETT                              
100400                               TO MOD-NY-KVDAGAR-CDBEH-ATTR               
100500                  ELSE                                                    
100600                    MOVE MFS-NUM-FAELT-FEL                                
100700                               TO MOD-NY-KVDAGAR-CDBEH-ATTR               
100800                    MOVE ERR-CORR-HILITE-FLDS                             
100900                               TO MED-IDMFSFEL                            
101000                    MOVE NEJ TO INDATA-SW                                 
101100                  END-IF                                                  
101200                  IF MID-NY-FLCDREL = JA                                  
101300                  OR MID-NY-FLCDREL = YES                                 
101400                  OR MID-NY-FLCDREL = NEJ                                 
101500                    MOVE MFS-ALFA-FAELT-RAETT                             
101600                               TO MOD-NY-FLCDREL-ATTR                     
101700                  ELSE                                                    
101800                    MOVE MFS-ALFA-FAELT-FEL                               
101900                               TO MOD-NY-FLCDREL-ATTR                     
102000                    MOVE ERR-CORR-HILITE-FLDS                             
102100                               TO MED-IDMFSFEL                            
102200                    MOVE NEJ TO INDATA-SW                                 
102300                  END-IF                                                  
102400                ELSE                                                      
102500                  MOVE MFS-ALFA-FAELT-FEL                                 
102600                               TO MOD-NY-IDDC-ATTR                        
102700                  MOVE MED-1 TO WS-TEMFSFEL                               
102800                  MOVE NEJ   TO INDATA-SW                                 
102900                END-IF                                                    
103000              ELSE                                                        
103100                MOVE MFS-ALFA-FAELT-FEL                                   
103200                               TO MOD-NY-IDDC-ATTR                        
103300                MOVE MFS-NUM-FAELT-FEL                                    
103400                               TO MOD-NY-ADLAGOMR-CD-ATTR                 
103500                MOVE ERR-CORR-HILITE-FLDS                                 
103600                               TO MED-IDMFSFEL                            
103700                MOVE NEJ     TO INDATA-SW                                 
103800              END-IF                                                      
103900            ELSE                                                          
104000                MOVE MFS-ALFA-FAELT-FEL                                   
104100                               TO MOD-NY-IDDC-ATTR                        
104200                MOVE ERR-CORR-HILITE-FLDS                                 
104300                               TO MED-IDMFSFEL                            
104400                MOVE NEJ     TO INDATA-SW                                 
104500            END-IF                                                        
104510          END-IF                                                          
104600        END-IF                                                            
104700     END-IF                                                               
104800                                                                          
104900     IF INDATA-FEL                                                        
105000        IF WS-TEMFSFEL = SPACE                                            
105100          CALL WMEDKONV USING MED-WMEDAREA                                
105200          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
105300        ELSE                                                              
105400          MOVE WS-TEMFSFEL  TO MOD-TEMFSFEL                               
105500        END-IF                                                            
105600        PERFORM MFS-ROER-EJ-FAELT-UT                                      
105700        PERFORM MFS-ROER-EJ-FAELT-IN                                      
105800     ELSE                                                                 
105900                                                                          
106000         IF UPD-RAD-JA                                                    
106100         AND NY-RAD-JA                                                    
106200*****                                                                     
106300*****     INTE MÖJLIGT ATT BÅDE ÄNDRA PÅ BEFINTLIG RAD                    
106400*****     OCH UPPDATERA EN NY                                             
106500*****                                                                     
106600             MOVE NEJ        TO INDATA-SW                                 
106700             MOVE CONFLICT   TO MED-IDMFSFEL                              
106800             CALL WMEDKONV USING MED-WMEDAREA                             
106900             MOVE MED-TEMFSFEL                                            
107000                             TO MOD-TEMFSFEL                              
107100             PERFORM MFS-ROER-EJ-FAELT-IN                                 
107200             PERFORM MFS-ROER-EJ-FAELT-UT                                 
107300         END-IF                                                           
107400     END-IF                                                               
107500     .                                                                    
107600     EJECT                                                                
107700 GA-UPD-TABELL SECTION.                                                   
107800     MOVE 'GA-UPD-TABELL'    TO WS-SECTION                                
107900                                                                          
108000     MOVE 1                  TO IX-BORT                                   
108100     PERFORM UNTIL IX-BORT > TAB-MAX                                      
108200        IF TAB-LO-CD-BORTTAG (IX-BORT) = ZERO                             
108300          MOVE WS-MSGI-ADLAGOMR-CD (IX)                                   
108400                             TO TAB-LO-CD-BORTTAG (IX-BORT)               
108500          ADD 1              TO TAB-ANT-BORTTAG (IX-BORT)                 
108600          MOVE TAB-MAX       TO IX-BORT                                   
108700        ELSE                                                              
108800          IF WS-MSGI-ADLAGOMR-CD (IX) =                                   
108900                             TAB-LO-CD-BORTTAG (IX-BORT)                  
109000            ADD 1            TO TAB-ANT-BORTTAG (IX-BORT)                 
109100            MOVE TAB-MAX     TO IX-BORT                                   
109200          END-IF                                                          
109300        END-IF                                                            
109400        ADD 1                TO IX-BORT                                   
109500     END-PERFORM                                                          
109600                                                                          
109700     .                                                                    
109800     EJECT                                                                
109900 GB-KTRL-TABELL SECTION.                                                  
110000     MOVE 'GB-KTRL-TABELL'   TO WS-SECTION                                
110100                                                                          
110200     PERFORM IMS-GU-K701                                                  
110300     PERFORM IMS-GNP-K711                                                 
110400                                                                          
110500     PERFORM UNTIL SEGMENT-SAKNAS                                         
110600       IF SLAG-ADLAGOMR-CD > ZERO                                         
110700         MOVE 1              TO IX-BEF                                    
110800         PERFORM UNTIL IX-BEF > TAB-MAX                                   
110900           IF TAB-LO-CD-BEF (IX-BEF) = ZERO                               
111000             MOVE SLAG-ADLAGOMR-CD                                        
111100                             TO TAB-LO-CD-BEF (IX-BEF)                    
111200             ADD 1           TO TAB-ANT-BEF (IX-BEF)                      
111300             MOVE TAB-MAX    TO IX-BEF                                    
111400           ELSE                                                           
111500             IF SLAG-ADLAGOMR-CD = TAB-LO-CD-BEF (IX-BEF)                 
111600               ADD 1         TO TAB-ANT-BEF (IX-BEF)                      
111700               MOVE TAB-MAX  TO IX-BEF                                    
111800             END-IF                                                       
111900           END-IF                                                         
112000           ADD 1             TO IX-BEF                                    
112100         END-PERFORM                                                      
112200       END-IF                                                             
112300       PERFORM IMS-GNP-K711                                               
112400     END-PERFORM                                                          
112500                                                                          
112600                                                                          
112700     MOVE ZERO               TO TAB-LO-CD-BORT-K6 (1)                     
112800                                TAB-LO-CD-BORT-K6 (2)                     
112900                                TAB-LO-CD-BORT-K6 (3)                     
113000                                TAB-LO-CD-BORT-K6 (4)                     
113100                                                                          
113200     PERFORM IMS-GU-K611                                                  
113300     MOVE 1                  TO IX-BORT                                   
113400     PERFORM UNTIL IX-BORT > TAB-MAX                                      
113500     OR TAB-LO-CD-BORTTAG (IX-BORT) = ZERO                                
113600       MOVE 1                TO IX-BEF                                    
113700       PERFORM UNTIL IX-BEF > TAB-MAX                                     
113800       OR TAB-LO-CD-BEF (IX-BEF) = ZERO                                   
113900          IF  (TAB-LO-CD-BEF (IX-BEF) =                                   
114000               TAB-LO-CD-BORTTAG (IX-BORT))                               
114100          AND (TAB-ANT-BEF (IX-BEF) =                                     
114200               TAB-ANT-BORTTAG (IX-BORT))                                 
114300            MOVE 1           TO IX-K6                                     
114400            PERFORM UNTIL IX-K6 > TAB-MAX                                 
114500               IF TAB-LO-CD-BORTTAG (IX-BORT) =                           
114600                  CLAG-ADLAGOMR-CD (IX-K6)                                
114700                                                                          
114800                 IF CLAG-KVLS-CD (IX-K6) > ZERO                           
114900                 OR CLAG-KVRESS-CD (IX-K6) > ZERO                         
115000                                                                          
115100                   MOVE 1    TO RAD-IX                                    
115200                   PERFORM UNTIL RAD-IX > RAD-MAX                         
115300                     IF MID-CMD (RAD-IX) NOT = ALL '+'                    
115400                       MOVE MFS-ALFA-FAELT-FEL                            
115500                             TO MOD-CMD-ATTR (RAD-IX)                     
115600                     END-IF                                               
115700                     ADD 1   TO RAD-IX                                    
115800                   END-PERFORM                                            
115900                   MOVE MED-3 TO WS-TEMFSFEL                              
116000                   MOVE NEJ TO INDATA-SW                                  
116100                                                                          
116200                 ELSE                                                     
116300                                                                          
116400                   PERFORM IMS-GU-W6D1ISEQ                                
116500                   PERFORM UNTIL SEGMENT-SAKNAS                           
116600                   OR I-ART-ADTRDEST(1:2) = 'CD'                          
116700                     PERFORM IMS-GN-W6D1ISEQ                              
116800                   END-PERFORM                                            
116900                   IF SEGMENT-FINNS                                       
117000                     MOVE MED-5 TO WS-TEMFSFEL                            
117100                     MOVE NEJ TO INDATA-SW                                
117200                   ELSE                                                   
117300                                                                          
117400                     MOVE 1  TO IX-BORT-K6                                
117500                     PERFORM UNTIL IX-BORT-K6 > TAB-MAX                   
117600                       IF TAB-LO-CD-BORT-K6 (IX-BORT-K6) = ZERO           
117700                         MOVE CLAG-ADLAGOMR-CD (IX-K6)                    
117800                             TO TAB-LO-CD-BORT-K6 (IX-BORT-K6)            
117900                         MOVE TAB-MAX                                     
118000                             TO IX-BORT-K6                                
118100                       END-IF                                             
118200                       ADD 1 TO IX-BORT-K6                                
118300                     END-PERFORM                                          
118400                   END-IF                                                 
118500                 END-IF                                                   
118600                 MOVE TAB-MAX                                             
118700                             TO IX-K6                                     
118800               END-IF                                                     
118900               ADD 1         TO IX-K6                                     
119000            END-PERFORM                                                   
119100          END-IF                                                          
119200          ADD 1              TO IX-BEF                                    
119300       END-PERFORM                                                        
119400       ADD 1                 TO IX-BORT                                   
119500     END-PERFORM                                                          
119600     .                                                                    
119700     EJECT                                                                
119701                                                                          
119710 GC-KOLLA-CD-ARTIKEL SECTION.                                             
119720     MOVE 'GC-KOLLA-CD-ARTIKEL'    TO WS-SECTION                          
119730                                                                          
119740     PERFORM IMS-GU-K701                                                  
119750     IF SEGMENT-FINNS                                                     
119760       PERFORM IMS-GNP-K711                                               
119770       PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                         
119780         IF SLAG-IDDC (1:1) = '4' OR '5'                                  
119790           IF SLAG-IDLEVNR NOT = '1441'                                   
119791             MOVE NEJ        TO CDART-SW                                  
119792           END-IF                                                         
119793         END-IF                                                           
119794         PERFORM IMS-GNP-K711                                             
119795       END-PERFORM                                                        
119796     END-IF                                                               
119797     .                                                                    
119798     EJECT                                                                
119799                                                                          
119800 H-UPPDATERA SECTION.                                                     
119900     MOVE 'H-UPPDATERA'      TO WS-SECTION                                
120000                                                                          
120100     IF UPD-RAD-JA                                                        
120200                                                                          
120300       MOVE +1 TO IX                                                      
120400       PERFORM UNTIL IX > RAD-MAX                                         
120500          IF MID-RAD (IX) NOT = ALL '+'                                   
120600             PERFORM HA-UPD-RAD                                           
120700          END-IF                                                          
120800          ADD 1 TO IX                                                     
120900       END-PERFORM                                                        
121000                                                                          
121100       IF TAB-LO-CD-BORT-K6 (1) > ZERO                                    
121200         PERFORM IMS-GHU-K611                                             
121300         PERFORM HC-BORTTAG-LO                                            
121400         PERFORM IMS-REPL-K611                                            
121500       END-IF                                                             
121600                                                                          
121700     ELSE                                                                 
121800                                                                          
121900       PERFORM HB-NY-RAD                                                  
122000     END-IF                                                               
122100                                                                          
122200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
122300     CALL WMEDKONV USING MED-WMEDAREA                                     
122400     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
122500     PERFORM MFS-RENSA-FAELT-IN                                           
122600                                                                          
122700     PERFORM IMS-GU-K611                                                  
122800                                                                          
122900     IF  SEGMENT-FINNS                                                    
123000       IF CLAG-KVQPACK-3 = ZERO                                           
123100         MOVE MED-8                TO WS-TEMFSINFO                        
123200       END-IF                                                             
123300     END-IF                                                               
123400     .                                                                    
123500     EJECT                                                                
123600 HA-UPD-RAD SECTION.                                                      
123700     MOVE 'HA-UPD-RAD'       TO WS-SECTION                                
123800                                                                          
123900     MOVE WS-MSGI-IDDC (IX)  TO W-IDDC                                    
124000     PERFORM IMS-GHU-K711                                                 
124100                                                                          
124200     IF MID-CMD (IX) = 'B'                                                
124300     OR MID-CMD (IX) = 'D'                                                
124400       MOVE ZERO             TO SLAG-ADLAGOMR-CD                          
124500                                SLAG-KVDAGAR-CDBEH                        
124510                                SLAG-TIDATUM-CROSS                        
124600       MOVE NEJ              TO SLAG-FLCDREL                              
124700                                                                          
124800       PERFORM IMS-REPL-K711                                              
124900                                                                          
125000       PERFORM IMS-GU-K701                                                
125100       PERFORM IMS-GNP-K711                                               
125200                                                                          
125300       PERFORM UNTIL SEGMENT-SAKNAS                                       
125400       OR SLAG-ADLAGOMR-CD > ZERO                                         
125500         PERFORM IMS-GNP-K711                                             
125600       END-PERFORM                                                        
125700                                                                          
125800       IF SEGMENT-SAKNAS                                                  
125900         PERFORM IMS-GHU-K611                                             
126000         MOVE NEJ            TO CLAG-FLCDART                              
126100         PERFORM IMS-REPL-K611                                            
126200       END-IF                                                             
126300     ELSE                                                                 
126400                                                                          
126500       IF MID-ADLAGOMR-CD (IX) = ALL '+'                                  
126600         CONTINUE                                                         
126700       ELSE                                                               
126800**??                                                                      
126900         MOVE MID-ADLAGOMR-CD(IX)                                         
127000                             TO WS-ADLAGOMR-NUM                           
127100         PERFORM IMS-GHU-K611                                             
127200         MOVE 1                  TO IX-K6                                 
127300         PERFORM UNTIL IX-K6 > TAB-MAX                                    
127400         OR CLAG-ADLAGOMR-CD (IX-K6) = ZERO                               
127500         OR CLAG-ADLAGOMR-CD (IX-K6) = WS-ADLAGOMR-NUM                    
127600            ADD 1                 TO IX-K6                                
127700         END-PERFORM                                                      
127800         IF IX-K6 > TAB-MAX                                               
127900           CALL FELLOG                                                    
128000         ELSE                                                             
128100           IF CLAG-ADLAGOMR-CD (IX-K6) = ZERO                             
128200             MOVE MID-ADLAGOMR-CD(IX)                                     
128300                             TO CLAG-ADLAGOMR-CD (IX-K6)                  
128400           END-IF                                                         
128500         END-IF                                                           
128600                                                                          
128700         PERFORM IMS-REPL-K611                                            
128800                                                                          
128900         MOVE MID-ADLAGOMR-CD(IX)                                         
129000                             TO SLAG-ADLAGOMR-CD                          
129100**                                                                        
129200       END-IF                                                             
129300                                                                          
129400       IF MID-KVDAGAR-CDBEH (IX) = ALL '+'                                
129500         CONTINUE                                                         
129600       ELSE                                                               
129700         MOVE MID-KVDAGAR-CDBEH (IX)                                      
129800                             TO SLAG-KVDAGAR-CDBEH                        
129900       END-IF                                                             
129910                                                                          
129920       IF MID-TIDATUM-CROSS (IX) = ALL '+'                                
129930         CONTINUE                                                         
129940       ELSE                                                               
129950         MOVE MID-TIDATUM-CROSS (IX)                                      
129960                             TO SLAG-TIDATUM-CROSS                        
129970       END-IF                                                             
130000                                                                          
130100       IF MID-FLCDREL (IX) = ALL '+'                                      
130200         CONTINUE                                                         
130300       ELSE                                                               
130400         IF MID-FLCDREL (IX) = YES                                        
130500           MOVE JA           TO SLAG-FLCDREL                              
130600         ELSE                                                             
130700           MOVE MID-FLCDREL (IX)                                          
130800                             TO SLAG-FLCDREL                              
130900         END-IF                                                           
131000         IF  SLAG-FLCDREL      = JA                                       
131100         AND SLAG-FLREFBEO NOT = JA                                       
131200           MOVE MED-6        TO WS-TEMFSINFO                              
131300         END-IF                                                           
131400       END-IF                                                             
131500                                                                          
131600       PERFORM IMS-REPL-K711                                              
131700     END-IF                                                               
131800     .                                                                    
131900     EJECT                                                                
132000 HB-NY-RAD SECTION.                                                       
132100     MOVE 'HB-NY-RAD'        TO WS-SECTION                                
132200                                                                          
132300     MOVE MID-NY-IDDC        TO W-IDDC                                    
132400     PERFORM IMS-GHU-K711                                                 
132500                                                                          
132600     MOVE MID-NY-ADLAGOMR-CD TO SLAG-ADLAGOMR-CD                          
132700     MOVE MID-NY-KVDAGAR-CDBEH                                            
132800                             TO SLAG-KVDAGAR-CDBEH                        
132900     MOVE MID-NY-FLCDREL     TO SLAG-FLCDREL                              
133000     IF SLAG-FLCDREL = 'Y'                                                
133100       MOVE JA               TO SLAG-FLCDREL                              
133200     END-IF                                                               
133300                                                                          
133400     IF  SLAG-FLCDREL      = JA                                           
133500     AND SLAG-FLREFBEO NOT = JA                                           
133600       MOVE MED-6            TO WS-TEMFSINFO                              
133700     END-IF                                                               
133800                                                                          
133900     PERFORM IMS-REPL-K711                                                
134000                                                                          
134100     PERFORM IMS-GHU-K611                                                 
134200     MOVE JA                 TO CLAG-FLCDART                              
134300                                                                          
134400     MOVE 1                  TO IX-K6                                     
134500     PERFORM UNTIL IX-K6 > TAB-MAX                                        
134600     OR CLAG-ADLAGOMR-CD (IX-K6) = ZERO                                   
134700     OR CLAG-ADLAGOMR-CD (IX-K6) =                                        
134800        SLAG-ADLAGOMR-CD                                                  
134900       ADD 1                 TO IX-K6                                     
135000     END-PERFORM                                                          
135100     IF IX-K6 > TAB-MAX                                                   
135200        CALL FELLOG                                                       
135300     ELSE                                                                 
135400       IF CLAG-ADLAGOMR-CD (IX-K6) = ZERO                                 
135500         MOVE SLAG-ADLAGOMR-CD                                            
135600                             TO CLAG-ADLAGOMR-CD (IX-K6)                  
135700       END-IF                                                             
135800     END-IF                                                               
135900                                                                          
136000     PERFORM IMS-REPL-K611                                                
136100     MOVE SPACE              TO WS-ENTER-IDDC                             
136200                                WS-NEXT-IDDC                              
136300     .                                                                    
136400     EJECT                                                                
136500 HC-BORTTAG-LO SECTION.                                                   
136600     MOVE 'HC-BORTTAG-LO'    TO WS-SECTION                                
136700                                                                          
136800     MOVE 1                  TO IX-BORT-K6                                
136900     PERFORM UNTIL IX-BORT-K6 > TAB-MAX                                   
137000     OR TAB-LO-CD-BORT-K6 (IX-BORT-K6) = ZERO                             
137100                                                                          
137200       MOVE 1                TO IX-K6                                     
137300       PERFORM UNTIL IX-K6 > TAB-MAX                                      
137400       OR CLAG-ADLAGOMR-CD (IX-K6) =                                      
137500          TAB-LO-CD-BORT-K6 (IX-BORT-K6)                                  
137600         ADD 1               TO IX-K6                                     
137700       END-PERFORM                                                        
137800                                                                          
137900       IF IX-K6 <= TAB-MAX                                                
138000         MOVE ZERO           TO CLAG-ADLAGOMR-CD (IX-K6)                  
138100                                CLAG-ADGANG-CD (IX-K6)                    
138200                                CLAG-ADPLATS-CD (IX-K6)                   
138300                                CLAG-KVLS-CD (IX-K6)                      
138400                                CLAG-KVRESS-CD (IX-K6)                    
138500                                                                          
138600         IF IX-K6 = 1                                                     
138700           MOVE CLAG-ADLAGOMR-CD (2)                                      
138800                             TO CLAG-ADLAGOMR-CD (1)                      
138900           MOVE CLAG-ADGANG-CD (2)                                        
139000                             TO CLAG-ADGANG-CD (1)                        
139100           MOVE CLAG-ADPLATS-CD (2)                                       
139200                             TO CLAG-ADPLATS-CD (1)                       
139300           MOVE CLAG-KVLS-CD (2)                                          
139400                             TO CLAG-KVLS-CD (1)                          
139500           MOVE CLAG-KVRESS-CD (2)                                        
139600                             TO CLAG-KVRESS-CD (1)                        
139700                                                                          
139800           MOVE CLAG-ADLAGOMR-CD (3)                                      
139900                             TO CLAG-ADLAGOMR-CD (2)                      
140000           MOVE CLAG-ADGANG-CD (3)                                        
140100                             TO CLAG-ADGANG-CD (2)                        
140200           MOVE CLAG-ADPLATS-CD (3)                                       
140300                             TO CLAG-ADPLATS-CD (2)                       
140400           MOVE CLAG-KVLS-CD (3)                                          
140500                             TO CLAG-KVLS-CD (2)                          
140600           MOVE CLAG-KVRESS-CD (3)                                        
140700                             TO CLAG-KVRESS-CD (2)                        
140800                                                                          
140900           MOVE CLAG-ADLAGOMR-CD (4)                                      
141000                             TO CLAG-ADLAGOMR-CD (3)                      
141100           MOVE CLAG-ADGANG-CD (4)                                        
141200                             TO CLAG-ADGANG-CD (3)                        
141300           MOVE CLAG-ADPLATS-CD (4)                                       
141400                             TO CLAG-ADPLATS-CD (3)                       
141500           MOVE CLAG-KVLS-CD (4)                                          
141600                             TO CLAG-KVLS-CD (3)                          
141700           MOVE CLAG-KVRESS-CD (4)                                        
141800                             TO CLAG-KVRESS-CD (3)                        
141900                                                                          
142000           MOVE ZERO         TO CLAG-ADLAGOMR-CD (4)                      
142100                                CLAG-ADGANG-CD (4)                        
142200                                CLAG-ADPLATS-CD (4)                       
142300                                CLAG-KVLS-CD (4)                          
142400                                CLAG-KVRESS-CD (4)                        
142500         END-IF                                                           
142600         IF IX-K6 = 2                                                     
142700           MOVE CLAG-ADLAGOMR-CD (3)                                      
142800                             TO CLAG-ADLAGOMR-CD (2)                      
142900           MOVE CLAG-ADGANG-CD (3)                                        
143000                             TO CLAG-ADGANG-CD (2)                        
143100           MOVE CLAG-ADPLATS-CD (3)                                       
143200                             TO CLAG-ADPLATS-CD (2)                       
143300           MOVE CLAG-KVLS-CD (3)                                          
143400                             TO CLAG-KVLS-CD (2)                          
143500           MOVE CLAG-KVRESS-CD (3)                                        
143600                             TO CLAG-KVRESS-CD (2)                        
143700                                                                          
143800           MOVE CLAG-ADLAGOMR-CD (4)                                      
143900                             TO CLAG-ADLAGOMR-CD (3)                      
144000           MOVE CLAG-ADGANG-CD (4)                                        
144100                             TO CLAG-ADGANG-CD (3)                        
144200           MOVE CLAG-ADPLATS-CD (4)                                       
144300                             TO CLAG-ADPLATS-CD (3)                       
144400           MOVE CLAG-KVLS-CD (4)                                          
144500                             TO CLAG-KVLS-CD (3)                          
144600           MOVE CLAG-KVRESS-CD (4)                                        
144700                             TO CLAG-KVRESS-CD (3)                        
144800                                                                          
144900           MOVE ZERO         TO CLAG-ADLAGOMR-CD (4)                      
145000                                CLAG-ADGANG-CD (4)                        
145100                                CLAG-ADPLATS-CD (4)                       
145200                                CLAG-KVLS-CD (4)                          
145300                                CLAG-KVRESS-CD (4)                        
145400         END-IF                                                           
145500         IF IX-K6 = 3                                                     
145600           MOVE CLAG-ADLAGOMR-CD (4)                                      
145700                             TO CLAG-ADLAGOMR-CD (3)                      
145800           MOVE CLAG-ADGANG-CD (4)                                        
145900                             TO CLAG-ADGANG-CD (3)                        
146000           MOVE CLAG-ADPLATS-CD (4)                                       
146100                             TO CLAG-ADPLATS-CD (3)                       
146200           MOVE CLAG-KVLS-CD (4)                                          
146300                             TO CLAG-KVLS-CD (3)                          
146400           MOVE CLAG-KVRESS-CD (4)                                        
146500                             TO CLAG-KVRESS-CD (3)                        
146600                                                                          
146700           MOVE ZERO         TO CLAG-ADLAGOMR-CD (4)                      
146800                                CLAG-ADGANG-CD (4)                        
146900                                CLAG-ADPLATS-CD (4)                       
147000                                CLAG-KVLS-CD (4)                          
147100                                CLAG-KVRESS-CD (4)                        
147200         END-IF                                                           
147300                                                                          
147400                                                                          
147500       END-IF                                                             
147600       ADD 1                 TO IX-BORT-K6                                
147700     END-PERFORM                                                          
147800     .                                                                    
147900     EJECT                                                                
148000 MFS-RENSA-FAELT-UT SECTION.                                              
148100                                                                          
148200     MOVE +1 TO IX                                                        
148300     PERFORM UNTIL IX > RAD-MAX                                           
148400       MOVE MFS-RENSA-FAELT  TO MOD-IDDC (IX)                             
148500       ADD +1 TO IX                                                       
148600     END-PERFORM                                                          
148700     .                                                                    
148800     EJECT                                                                
148900 MFS-RENSA-FAELT-IN SECTION.                                              
149000                                                                          
149100*    --- ALLA INDATA-FÄLT                                                 
149200     MOVE +1 TO IX                                                        
149300     PERFORM UNTIL IX > RAD-MAX                                           
149400       MOVE MFS-RENSA-FAELT  TO MOD-CMD (IX)                              
149500                                MOD-ADLAGOMR-CD-IN (IX)                   
149600                                MOD-KVDAGAR-CDBEH-IN (IX)                 
149700                                MOD-FLCDREL-IN (IX)                       
149710                                MOD-TIDATUM-CROSS-IN (IX)                 
149800       ADD +1 TO IX                                                       
149900     END-PERFORM                                                          
150000     MOVE MFS-RENSA-FAELT    TO MOD-NY-IDDC                               
150100                                MOD-NY-ADLAGOMR-CD                        
150200                                MOD-NY-KVDAGAR-CDBEH                      
150300                                MOD-NY-FLCDREL                            
150400     .                                                                    
150500     EJECT                                                                
150600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
150700                                                                          
150800*    --- ALLA UTDATA-FÄLT                                                 
150900     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                                 
151000     MOVE +1 TO IX                                                        
151100     PERFORM UNTIL IX > RAD-MAX                                           
151200       MOVE MFS-ROER-EJ-FAELT                                             
151300                             TO MOD-IDDC (IX)                             
151400                                MOD-ADLAGOMR-CD (IX)                      
151500                                MOD-KVDAGAR-CDBEH (IX)                    
151600                                MOD-FLCDREL (IX)                          
151610                                MOD-TIDATUM-CROSS (IX)                    
151700       ADD +1 TO IX                                                       
151800     END-PERFORM                                                          
151900     .                                                                    
152000     EJECT                                                                
152100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
152200                                                                          
152300*    --- ALLA INDATA-FÄLT                                                 
152400     MOVE +1 TO IX                                                        
152500     PERFORM UNTIL IX > RAD-MAX                                           
152600       MOVE MFS-ROER-EJ-FAELT                                             
152700                             TO MOD-CMD (IX)                              
152800                                MOD-ADLAGOMR-CD-IN (IX)                   
152900                                MOD-KVDAGAR-CDBEH-IN (IX)                 
153000                                MOD-FLCDREL-IN (IX)                       
153010                                MOD-TIDATUM-CROSS-IN (IX)                 
153100       ADD +1                TO IX                                        
153200     END-PERFORM                                                          
153300     MOVE MFS-ROER-EJ-FAELT  TO MOD-NY-IDDC                               
153400                                MOD-NY-ADLAGOMR-CD                        
153500                                MOD-NY-KVDAGAR-CDBEH                      
153600                                MOD-NY-FLCDREL                            
153700     .                                                                    
153800     EJECT                                                                
153900 MFS-LAES-IN-IGEN SECTION.                                                
154000                                                                          
154100*    --- ALLA INDATA-FÄLT                                                 
154200     MOVE +1 TO IX                                                        
154300     PERFORM UNTIL IX > RAD-MAX                                           
154400       MOVE MFS-ADD-LAES-IN-FAELT                                         
154500                             TO MOD-CMD-ATTR (IX)                         
154600                                MOD-ADLAGOMR-CD-ATTR (IX)                 
154700                                MOD-KVDAGAR-CDBEH-ATTR (IX)               
154800                                MOD-FLCDREL-ATTR (IX)                     
154810                                MOD-TIDATUM-CROSS-ATTR (IX)               
154900       ADD +1                TO IX                                        
155000     END-PERFORM                                                          
155100     MOVE MFS-ADD-LAES-IN-FAELT                                           
155200                             TO MOD-NY-IDDC-ATTR                          
155300                                MOD-NY-ADLAGOMR-CD-ATTR                   
155400                                MOD-NY-KVDAGAR-CDBEH-ATTR                 
155500                                MOD-NY-FLCDREL-ATTR                       
155600     .                                                                    
155700     EJECT                                                                
155800 MFS-STAENG-FAELT-RADER-IN    SECTION.                                    
155900                                                                          
156000     MOVE 1                  TO IX                                        
156100     PERFORM UNTIL IX > RAD-MAX                                           
156200                                                                          
156300       IF WS-MSGI-IDDC (IX) = SPACE                                       
156400         MOVE MFS-STAENG-FAELT                                            
156500                             TO MOD-CMD-ATTR (IX)                         
156600                                MOD-ADLAGOMR-CD-ATTR (IX)                 
156700                                MOD-KVDAGAR-CDBEH-ATTR (IX)               
156800                                MOD-FLCDREL-ATTR (IX)                     
156810                                MOD-TIDATUM-CROSS-ATTR (IX)               
156900         MOVE MFS-RENSA-FAELT                                             
157000                             TO MOD-CMD (IX)                              
157100                                MOD-ADLAGOMR-CD-IN (IX)                   
157200                                MOD-KVDAGAR-CDBEH-IN (IX)                 
157300                                MOD-FLCDREL-IN (IX)                       
157310                                MOD-TIDATUM-CROSS-IN (IX)                 
157400       END-IF                                                             
157500                                                                          
157600       ADD 1                 TO IX                                        
157700                                                                          
157800     END-PERFORM                                                          
157900     .                                                                    
158000     EJECT                                                                
158100* --- IMS SEKTIONER ---                                                   
158200     SKIP3                                                                
158300 IMS-GET-MSG SECTION.                                                     
158400     MOVE '  QC' TO GODK-STATUSKODER                                      
158500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
158600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
158700     PERFORM IMS-STATUSKONTROLL                                           
158800     .                                                                    
158900     SKIP2                                                                
159000 IMS-INSERT-MSG SECTION.                                                  
159100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
159200     MOVE SPACE TO GODK-STATUSKODER                                       
159300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
159400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
159500     PERFORM IMS-STATUSKONTROLL                                           
159600     .                                                                    
159700     EJECT                                                                
159800 IMS-GU-K611 SECTION.                                                     
159900                                                                          
160000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
160100            DELIMITED BY SIZE INTO SSA1                                   
160200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
160300          DELIMITED BY SIZE INTO SSA2                                     
160400     MOVE '  GE' TO GODK-STATUSKODER                                      
160500     CALL CBLTDLI USING GU    WDK6-PCB DLI-IO-WDK611 SSA1 SSA2            
160600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900     EJECT                                                                
161000 IMS-GHU-K611 SECTION.                                                    
161100                                                                          
161200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
161300            DELIMITED BY SIZE INTO SSA1                                   
161400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
161500          DELIMITED BY SIZE INTO SSA2                                     
161600     MOVE '  GE' TO GODK-STATUSKODER                                      
161700     CALL CBLTDLI USING GHU    WDK6-PCB DLI-IO-WDK611 SSA1 SSA2           
161800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
161900     PERFORM IMS-STATUSKONTROLL                                           
162000     .                                                                    
162100     EJECT                                                                
162200 IMS-REPL-K611 SECTION.                                                   
162300                                                                          
162400     MOVE '  ' TO GODK-STATUSKODER                                        
162500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
162600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162700     PERFORM IMS-STATUSKONTROLL                                           
162800     .                                                                    
162900     EJECT                                                                
163000 IMS-GU-K701 SECTION.                                                     
163100                                                                          
163200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
163300          DELIMITED BY SIZE INTO SSA1                                     
163400     MOVE '  GE' TO GODK-STATUSKODER                                      
163500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
163600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
163700     PERFORM IMS-STATUSKONTROLL                                           
163800     .                                                                    
163900     EJECT                                                                
164000 IMS-GNP-K711 SECTION.                                                    
164100                                                                          
164200     MOVE 'WDK711   ' TO SSA1                                             
164300     MOVE '  GE' TO GODK-STATUSKODER                                      
164400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
164500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
164600     PERFORM IMS-STATUSKONTROLL                                           
164700     .                                                                    
164800     EJECT                                                                
164900 IMS-GHU-K711 SECTION.                                                    
165000                                                                          
165100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
165200          DELIMITED BY SIZE INTO SSA1                                     
165300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
165400          DELIMITED BY SIZE INTO SSA2                                     
165500     MOVE '  GE' TO GODK-STATUSKODER                                      
165600     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
165700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
165800     PERFORM IMS-STATUSKONTROLL                                           
165900     .                                                                    
166000     SKIP3                                                                
166100 IMS-REPL-K711 SECTION.                                                   
166200                                                                          
166300     MOVE '  ' TO GODK-STATUSKODER                                        
166400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
166500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
166600     PERFORM IMS-STATUSKONTROLL                                           
166700     .                                                                    
166800     EJECT                                                                
166900 IMS-GU-D301-BSEQ SECTION.                                                
167000                                                                          
167100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
167200          DELIMITED BY SIZE INTO SSA1                                     
167300     MOVE '  GE' TO GODK-STATUSKODER                                      
167400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
167500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
167600     PERFORM IMS-STATUSKONTROLL                                           
167700     .                                                                    
167800     SKIP3                                                                
167900 IMS-GNP-D311 SECTION.                                                    
168000                                                                          
168100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
168200          DELIMITED BY SIZE INTO SSA1                                     
168300     MOVE '  GE' TO GODK-STATUSKODER                                      
168400     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
168500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
168600     PERFORM IMS-STATUSKONTROLL                                           
168700     .                                                                    
168800     EJECT                                                                
168900                                                                          
169000 IMS-GU-W6G130 SECTION.                                                   
169100                                                                          
169200     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-X ')'                         
169300             DELIMITED BY SIZE INTO SSA1                                  
169400     STRING 'W6G130  (ADINLOMR =' W-ADINLOMR-X ')'                        
169500             DELIMITED BY SIZE INTO SSA2                                  
169600     MOVE '  GE' TO GODK-STATUSKODER                                      
169700     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-W6G130 SSA1 SSA2               
169800     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
169900     PERFORM IMS-STATUSKONTROLL                                           
170000     .                                                                    
170100     EJECT                                                                
170200 IMS-GU-W6D1ISEQ  SECTION.                                                
170300     STRING 'W6D111  (W6D1ISEQ =' W-IDARTNR-X ')'                         
170400             DELIMITED BY SIZE INTO SSA1                                  
170500     MOVE '  GE'                 TO GODK-STATUSKODER                      
170600     CALL CBLTDLI USING GU W6D1-PCB DLI-IO-W6D1I11 SSA1                   
170700     MOVE W6D1-STATUS-CODE       TO STATUS-WS                             
170800     PERFORM IMS-STATUSKONTROLL                                           
170900     .                                                                    
171000     EJECT                                                                
171100 IMS-GN-W6D1ISEQ  SECTION.                                                
171200     STRING 'W6D111  (W6D1ISEQ =' W-IDARTNR-X ')'                         
171300             DELIMITED BY SIZE INTO SSA1                                  
171400     MOVE '  GE'                 TO GODK-STATUSKODER                      
171500     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-W6D1I11 SSA1                   
171600     MOVE W6D1-STATUS-CODE       TO STATUS-WS                             
171700     PERFORM IMS-STATUSKONTROLL                                           
171800     .                                                                    
171900     EJECT                                                                
172000                                                                          
172100 IMS-STATUSKONTROLL SECTION.                                              
172200     SET STATUS-IX TO 1                                                   
172300     SEARCH GODK-STATUS                                                   
172400       AT END                                                             
172500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
172600         DELIMITED BY SIZE INTO FELTEXT                                   
172700         CALL FELLOG                                                      
172800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
172900         CONTINUE                                                         
173000     END-SEARCH                                                           
173100     .                                                                    
