000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2017200.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   00/11/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR LARM (LEVERANSPRECISION)                                   
000900*                                                                         
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDD4                                       
001200*        PROGRAMMET LÄSER EV   WDK6                                       
001300*                              WLXXBX (WDR2)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T172                                              
001700*        MID:         W2I17201                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O17201                                            
002100*                                                                         
002200*                                                                         
002300*   ÄNDRINGAR:                                                            
002400*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002500*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002600*                  INTE ÄR LIKA MED LEVERANTÖRNR PÅ WDD4                  
002700*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002800*                                                                         
002900*   ETRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                      
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W2017200'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004900 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  MAX-IX                      PIC S9(4)  VALUE +15   COMP SYNC.        
005100 77  IDARTNR-WS                  PIC X(9).                                
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005500     88  INDATA-OK                           VALUE 'J'.                   
005600     88  INDATA-FEL                          VALUE 'N'.                   
005700                                                                          
005800 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
005900     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
006000     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
006100                                                                          
006200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006300     88  NYCKLAR-OK                          VALUE 'J'.                   
006400     88  NYCKLAR-FEL                         VALUE 'N'.                   
006500                                                                          
006600 77  SW-DLET                     PIC X       VALUE 'N'.                   
006700 77  SW-HOPP-2102                PIC X(1)    VALUE 'N'.                   
006800 77  SW-SELECT                   PIC X(1)    VALUE 'N'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  EGEN-MID                            VALUE '2172'.                
007200     88  GODK-MID                            VALUE '2171' '2172'          
007300                                                   '2173' '2174'          
007400                                                   '2175' '2176'          
007500                                                   '2177' '2178'          
007600                                                   '2179'.                
007700     88  HELP-MID                            VALUE '0551'.                
007800     EJECT                                                                
007900*    --- ARBETSFÄLT                                                       
008000 01  ARBETSFAELT.                                                         
008100     03  WS-IDARTNR              PIC X(9)    VALUE SPACE.                 
008200     03  WS-IDARTNR-NUM REDEFINES WS-IDARTNR PIC 9(9).                    
008300     03  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                 
008400     03  WS-IDLEVNR-8            PIC X(8)    VALUE SPACE.                 
008500     03  WS-IDANSK               PIC X(3)    VALUE SPACE.                 
008600     03  WS-IDANSK-NUM REDEFINES WS-IDANSK   PIC 9(3).                    
008700     03  WS-KDLARM               PIC X(3)    VALUE SPACE.                 
008800     03  WS-KDLARM-NUM REDEFINES WS-KDLARM   PIC 9(3).                    
008810     03  WS-KDOTFREK             PIC X(1)    VALUE SPACE.                 
008900     03  WDK6-IDLEVNR            PIC X(5)    VALUE SPACE.                 
009000     03  W-TIKLOCK               PIC S9(9)   VALUE ZERO COMP-3.           
009100                                                                          
009200     03  WS-TEORSLRM-220         PIC X(25)                                
009300                                 VALUE 'PREADVICE DEVIATION'.             
009400*****                            VALUE 'AVISERINGSDIFFERENS'.             
009500     03  WS-TEORSLRM-225         PIC X(25)                                
009600                                 VALUE 'NOT ARRIVED    '.                 
009700*****                            VALUE 'EJ INLEVERERAD '.                 
009800     03  WS-TEORSLRM-226         PIC X(25)                                
009900                                 VALUE 'QUANTITY DEVIATION'.              
010000*****                            VALUE 'KVANTAVVIKELSE '.                 
010100     03  WS-TEORSLRM-230         PIC X(25)                                
010200                                 VALUE 'TOO EARLY               '.        
010300*****                            VALUE 'TIDIG INLEVERANS        '.        
010400     03  WS-TEORSLRM-235         PIC X(25)                                
010500                                 VALUE 'NOT SCHEDULED           '.        
010600*****                            VALUE 'AVROP SAKNAS            '.        
010700     03  WS-DAREGDAT             PIC 9(8).                                
010800     03  FILLER REDEFINES WS-DAREGDAT.                                    
010900         05  FILLER              PIC 9(2).                                
011000         05  WS-TIREGDAT         PIC 9(6).                                
011100     EJECT                                                                
011200*    --- DATUMFÄLT                                                        
011300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011400 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
011500 01  WS-ALARM-AAVV               PIC 9(4)    VALUE ZERO.                  
011600     EJECT                                                                
011700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011800 01  GENERELLA-SUBPROGRAM.                                                
011900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012600*01 -COPY WMEDAREA                                                        
012700     SKIP3                                                                
012800 01  MESSAGE-CODES.                                                       
012900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
013700     EJECT                                                                
013800                                                                          
013900 01  SPECIAL-MEDDELANDEN.                                                 
014000     03  MED-1                   PIC X(40)   VALUE                        
014100         'BEHÖRIGHETSKONTROLL AKTIVERAD '.                                
014200                                                                          
014300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014600     SKIP3                                                                
014700*01 -COPY WMSGINIT                                                        
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL WDATKONV                                         
015000*01 -COPY WDATAREA                                                        
015100     EJECT                                                                
015200*                                                                         
015300 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
015400*                                                                         
015500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015600*                                                                         
015700 01  SPAR-AREA.                                                           
015800     03  SPAR-IDTRANS              PIC X(4)  VALUE SPACE.                 
015900     03  SPAR-DAREGDAT-ENTER       PIC 9(8)  VALUE ZERO.                  
016000     03  SPAR-DAREGDAT-NEXT        PIC 9(8)  VALUE ZERO.                  
016100     03  SPAR-TIKLOCK-ENTER        PIC S9(9) VALUE ZERO COMP-3.           
016200     03  SPAR-TIKLOCK-NEXT         PIC S9(9) VALUE ZERO COMP-3.           
016300     03  SPAR-IDARTNR-MIN          PIC S9(9) VALUE ZERO COMP-3.           
016400     03  SPAR-IDARTNR-MAX          PIC S9(9) VALUE ZERO COMP-3.           
016500     03  SPAR-IDANSK-MIN           PIC S9(3) VALUE ZERO COMP-3.           
016600     03  SPAR-IDANSK-MAX           PIC S9(3) VALUE ZERO COMP-3.           
016700     03  SPAR-IDLEVNR-MIN          PIC X(5)  VALUE SPACE.                 
016800     03  SPAR-IDLEVNR-MAX          PIC X(5)  VALUE SPACE.                 
016900     03  SPAR-KDLARM-MIN           PIC S9(3) VALUE ZERO COMP-3.           
017000     03  SPAR-KDLARM-MAX           PIC S9(3) VALUE ZERO COMP-3.           
017100     03  SPAR-TAB.                                                        
017200         05  SPAR-RAD   OCCURS 15.                                        
017300             07  SPAR-RAD-DAREGDAT PIC 9(8).                              
017400             07  SPAR-RAD-TIKLOCK  PIC S9(9)        COMP-3.               
017500             07  SPAR-RAD-IDARTNR  PIC S9(9)        COMP-3.               
017600     03  SPAR-RAD-IX               PIC S9(3)        COMP-3.               
017700     EJECT                                                                
017800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018100     SKIP3                                                                
018200*01  MID -COPY W2I17201                                                   
018300     EJECT                                                                
018400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018500     SKIP3                                                                
018600*01  -COPY WMSGAREA                                                       
018700     EJECT                                                                
018800     03  MOD REDEFINES MSG-AREA.                                          
018900*      05  -COPY W2O17201                                                 
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019200     SKIP3                                                                
019300*01  -COPY WMFSAREA                                                       
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)  VALUE                         
019600                                            'WDR301-DATA-AREA'.           
019700     SKIP3                                                                
019800*01  -COPY W214ALOG                                                       
019900     EJECT                                                                
020000 01  FILLER          PIC X(16) VALUE 'PROG-TO-PROG-SW'.                   
020100 01  W-PROG-TO-PROG-SW.                                                   
020200     03  P-WS-LL     PIC S9(4)  VALUE +50 COMP SYNC.                      
020300     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
020400     03  KDTRANS-WS  PIC X(8)   VALUE 'W2T102 '.                          
020500     03  P-IDTRANS   PIC X(4)   VALUE '2172'.                             
020600     03  P-KDMFSFOR  PIC X(1)   VALUE '1'.                                
020700*    03  MID   -COPY W2I10201     -PRE PROGSW-.                           
020800     EJECT                                                                
020900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021000*                                                                         
021100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021200     SKIP3                                                                
021300 01  NYCKLAR-TILL-DLI.                                                    
021400                                                                          
021500   03  W-IDARTNR-X.                                                       
021600     05  W-IDARTNR            PIC S9(9)   VALUE ZERO      COMP-3.         
021700   03 W-KDSEGKEY-X.                                                       
021800     05  W-KDSEGKEY           PIC X       VALUE '1'.                      
021900                                                                          
022000   03  W-IDDC-D4-X.                                                       
022100     05  W-IDDC-D4            PIC X(2)    VALUE '11'.                     
022200                                                                          
022300   03  W-WDD4-MIN-X.                                                      
022400                                                                          
022500     05  W-IDARTNR-MIN-X.                                                 
022600         07  W-IDARTNR-MIN    PIC S9(9)   VALUE ZERO      COMP-3.         
022700     05  W-IDANSK-MIN-X.                                                  
022800         07  W-IDANSK-MIN     PIC S9(3)   VALUE ZERO      COMP-3.         
022900     05  W-IDLEVNR-MIN-X.                                                 
023000         07  W-IDLEVNR-MIN    PIC X(5)    VALUE SPACE.                    
023100     05  W-KDLARM-MIN-X.                                                  
023200         07  W-KDLARM-MIN     PIC S9(3)   VALUE ZERO      COMP-3.         
023300                                                                          
023400   03  W-WDD4-MAX-X.                                                      
023500                                                                          
023600     05  W-IDARTNR-MAX-X.                                                 
023700         07  W-IDARTNR-MAX    PIC S9(9)   VALUE 99999999  COMP-3.         
023800     05  W-IDANSK-MAX-X.                                                  
023900         07  W-IDANSK-MAX     PIC S9(3)   VALUE 999       COMP-3.         
024000     05  W-IDLEVNR-MAX-X.                                                 
024100         07  W-IDLEVNR-MAX    PIC X(5)    VALUE '99999'.                  
024200     05  W-KDLARM-MAX-X.                                                  
024300         07  W-KDLARM-MAX     PIC S9(3)   VALUE 999       COMP-3.         
024400****                                                                      
024500   03  W-WDD4ASEQ-X.                                                      
024600                                                                          
024700     05  W-IDANSK-ASEQ-X.                                                 
024800         07  W-IDANSK-ASEQ    PIC S9(3)   VALUE ZERO      COMP-3.         
024900     05  W-IDLEVNR-ASEQ-X.                                                
025000         07  W-IDLEVNR-ASEQ   PIC X(5)    VALUE SPACE.                    
025100     05  W-IDARTNR-ASEQ-X.                                                
025200         07  W-IDARTNR-ASEQ   PIC S9(9)   VALUE ZERO      COMP-3.         
025300     05  W-DAREGDAT-9KOMPL-ASEQ-X.                                        
025400         07  W-DAREGDAT-9KOMPL-ASEQ PIC 9(08)  VALUE ZERO.                
025500     05  W-TIKLOCK-9KOMPL-ASEQ-X.                                         
025600         07  W-TIKLOCK-9KOMPL-ASEQ  PIC S9(9)  VALUE ZERO COMP-3.         
025700****                                                                      
025800   03  W-WDD4ASEQ-MIN-X.                                                  
025900                                                                          
026000     05  W-IDANSK-ASEQ-MIN-X.                                             
026100         07  W-IDANSK-ASEQ-MIN     PIC S9(3)   VALUE ZERO COMP-3.         
026200     05  W-IDLEVNR-ASEQ-MIN-X.                                            
026300         07  W-IDLEVNR-ASEQ-MIN    PIC X(5)    VALUE SPACE.               
026400     05  W-IDARTNR-ASEQ-MIN-X.                                            
026500         07  W-IDARTNR-ASEQ-MIN    PIC S9(9)   VALUE ZERO COMP-3.         
026600     05  W-DAREGDAT-9KOMPL-ASEQ-MIN-X.                                    
026700         07  W-DAREGDAT-9KOMPL-ASEQ-MIN PIC 9(08)    VALUE ZERO.          
026800     05  W-TIKLOCK-9KOMPL-ASEQ-MIN-X.                                     
026900        07  W-TIKLOCK-9KOMPL-ASEQ-MIN PIC S9(9) VALUE ZERO COMP-3.        
027000                                                                          
027100   03  W-WDD4ASEQ-MAX-X.                                                  
027200                                                                          
027300     05  W-IDANSK-ASEQ-MAX-X.                                             
027400         07  W-IDANSK-ASEQ-MAX PIC S9(3)       VALUE 999  COMP-3.         
027500     05  W-IDLEVNR-ASEQ-MAX-X.                                            
027600         07  W-IDLEVNR-ASEQ-MAX    PIC X(5)    VALUE '99999'.             
027700     05  W-IDARTNR-ASEQ-MAX-X.                                            
027800         07  W-IDARTNR-ASEQ-MAX  PIC S9(9) VALUE 99999999  COMP-3.        
027900     05  W-DAREGDAT-9KOMPL-ASEQ-MAX-X.                                    
028000         07  W-DAREGDAT-9KOMPL-ASEQ-MAX PIC 9(08) VALUE 99999999.         
028100     05  W-TIKLOCK-9KOMPL-ASEQ-MAX-X.                                     
028200         07 W-TIKLOCK-9KOMPL-ASEQ-MAX PIC S9(9)                           
028300                                           VALUE 999999999 COMP-3.        
028400****                                                                      
028500                                                                          
028600*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
028700     03  W-WDD401KY-X.                                                    
028800         05  W-DAREGDAT-9KOMPL   PIC 9(08)    VALUE ZERO.                 
028900         05  W-TIKLOCK-9KOMPL    PIC S9(9)    VALUE ZERO COMP-3.          
029000                                                                          
029100     03  W-WDGXKEY-2231-X.                                                
029200         05  FILLER              PIC X(4)     VALUE '2231'.               
029300         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
029400     03  W-WDGXKEY-2232-X.                                                
029500         05  W-IDANSK-L          PIC S9(3)    VALUE ZERO COMP-3.          
029600         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
029700                                                                          
029800     SKIP2                                                                
029900*    --- STATUS-KOD FRÅN IMS                                              
030000 01  STATUS-WS                   PIC XX.                                  
030100     88  SEGMENT-FINNS                       VALUE '  '.                  
030200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030300     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
030400     SKIP2                                                                
030500 01  GODK-STATUSKODER.                                                    
030600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030700     SKIP3                                                                
030800 01  SSA1                        PIC X(256).                              
030900 01  SSA2                        PIC X(64).                               
031000     EJECT                                                                
031100*    --- IMS FUNKTIONSKODER                                               
031200*01  -COPY W0003                                                          
031300     EJECT                                                                
031400*    ---  DLI INPUT-OUTPUT AREA                                           
031500                                                                          
031600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD401'.             
031700 01  DLI-IO-WDD401.                                                       
031800*    03  -COPY WDD401                                                     
031900     EJECT                                                                
032000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
032100     SKIP3                                                                
032200 01  DLI-IO-WDK601.                                                       
032300*    03  -COPY WDK601                                                     
032400     EJECT                                                                
032500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
032600     SKIP3                                                                
032700 01  DLI-IO-WDK611.                                                       
032800*    03  -COPY WDK611                                                     
032900     EJECT                                                                
033000 01  DLI-IO-AREA-BX.                                                      
033100     03  IO-AREA-BX          PIC X(50)  VALUE SPACE.                      
033200     SKIP3                                                                
033300     03  WLXXBX01 REDEFINES IO-AREA-BX.                                   
033400*        05  -COPY WDGX01     -PRE XXBX-                                  
033500     SKIP3                                                                
033600     03  WLXXBX20 REDEFINES IO-AREA-BX.                                   
033700*        05  -COPY WDGX2232   -PRE XXBX-                                  
033800     EJECT                                                                
033900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDR301'.             
034000 01  DLI-IO-WDR301.                                                       
034100*    03  -COPY WDR301                                                     
034200     EJECT                                                                
034300 LINKAGE SECTION.                                                         
034400*01  -COPY W0009   -PRE MSG-                                              
034500                                                                          
034600*01  -COPY W0009   -PRE ALT-                                              
034700                                                                          
034800*01  -COPY W0008   -PRE USEA-                                             
034900     05  FILLER                  PIC X.                                   
035000                                                                          
035100*01  -COPY W0008   -PRE WDD4-                                             
035200     05  FILLER                  PIC X.                                   
035300                                                                          
035400*01  -COPY W0008   -PRE WDK6-                                             
035500     05  FILLER                  PIC X.                                   
035600                                                                          
035700*01  -COPY W0008   -PRE XXBX-                                             
035800     05  FILLER                  PIC X.                                   
035900                                                                          
036000*01  -COPY W0008   -PRE WDD4A-                                            
036100     05  FILLER                  PIC X.                                   
036200*01  -COPY W0008   -PRE WDR3-                                             
036300     05  FILLER                  PIC X.                                   
036400     EJECT                                                                
036500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDD4-PCB              
036600                                                    WDK6-PCB              
036700                                                    XXBX-PCB              
036800                                                    WDD4A-PCB             
036900                                                    WDR3-PCB.             
037000 MAIN SECTION.                                                            
037100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDD4-PCB              
037200                                                    WDK6-PCB              
037300                                                    XXBX-PCB              
037400                                                    WDD4A-PCB             
037500                                                    WDR3-PCB.             
037600                                                                          
037700     PERFORM IMS-GET-MSG                                                  
037800     IF SEGMENT-FINNS                                                     
037900       PERFORM A-INIT                                                     
038000       PERFORM B-KOLLA-NYCKLAR                                            
038100       IF NYCKLAR-OK                                                      
038200                                                                          
038300           PERFORM G-KOLLA-INPUT                                          
038400                                                                          
038500           IF MFS-UPDATE OR MFS-SPLIT                                     
038600             IF INDATA-OK                                                 
038700                IF MFS-UPDATE                                             
038800                   PERFORM H-UPPDATERA                                    
038900                END-IF                                                    
039000                IF MFS-SPLIT                                              
039100                   PERFORM K-PPSW-2102                                    
039200                END-IF                                                    
039300             END-IF                                                       
039400           ELSE                                                           
039500             IF MFS-FIRST                                                 
039600               PERFORM C-FOERSTA-SIDA                                     
039700             ELSE                                                         
039800               IF MFS-NEXT                                                
039900                 PERFORM D-NAESTA-SIDA                                    
040000               ELSE                                                       
040100                 PERFORM E-SAMMA-SIDA                                     
040200               END-IF                                                     
040300             END-IF                                                       
040400           END-IF                                                         
040500                                                                          
040600           IF MFS-ENTER AND SW-SELECT = JA                                
040700              CONTINUE                                                    
040800           ELSE                                                           
040900              PERFORM F-LAES-VISA-INFO                                    
041000           END-IF                                                         
041100                                                                          
041200       END-IF                                                             
041300*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
041400*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
041500       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O17201 + 4                      
041600       PERFORM IMS-INSERT-MSG                                             
041700     END-IF                                                               
041800                                                                          
041900     MOVE ZERO TO RETURN-CODE                                             
042000     GOBACK                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 A-INIT SECTION.                                                          
042400                                                                          
042500     IF MSG-DUBBLA-TRANSKODER                                             
042600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I17201                 
042700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
042800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
042900     ELSE                                                                 
043000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I17201                  
043100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
043200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
043300     END-IF                                                               
043400                                                                          
043500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
043600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
043700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
043800                                                                          
043900     MOVE LOW-VALUE TO MSG-AREA                                           
044000     MOVE 'W2O17201' TO MFS-IDMOD                                         
044100     MOVE '2172' TO MOD-IDTRANS                                           
044200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
044300     MOVE 'GB'            TO MED-IDSKYLT                                  
044400                                                                          
044500     IF EGEN-MID OR HELP-MID                                              
044600       CONTINUE                                                           
044700     ELSE                                                                 
044800       MOVE SPACE TO MFS-KDTRTYP                                          
044900       MOVE '7' TO MFS-IDPFK                                              
045000     END-IF                                                               
045100                                                                          
045200     MOVE NEJ                    TO SW-SELECT                             
045300     MOVE NEJ                    TO SW-HOPP-2102                          
045400     IF MFS-IDPFK       = '9'                                             
045500        MOVE JA                  TO SW-HOPP-2102                          
045600        MOVE LOW-VALUE           TO PROGSW-W2I10201                       
045700     END-IF                                                               
045800***  GET CURRENT DATE                                                     
045900     ACCEPT DAGENS-DATUM       FROM DATE                                  
046000     ACCEPT W-TIKLOCK          FROM TIME                                  
046100     MOVE   DAGENS-DATUM         TO DAT-I-TIDATUM                         
046200*                                                                         
046300     PERFORM S01-CONVERT-DATE-FMT                                         
046400*                                                                         
046500     MOVE DAT-TIAAVV-GRP         TO DAGENS-AAVV                           
046600     .                                                                    
046700     EJECT                                                                
046800 B-KOLLA-NYCKLAR SECTION.                                                 
046900                                                                          
047000     MOVE JA TO NYCKLAR-SW                                                
047100                                                                          
047200*    -- KONTROLL AV IDARTNR                                               
047300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
047400                             MOD-IDLEVNR-IN                               
047500                             MOD-IDANSK-IN                                
047600                             MOD-KDLARM-IN                                
047610                             MOD-KDOTFREK-IN                              
047700                                                                          
047900     IF  MID-IDARTNR-IN = ALL '+'                                         
048000     AND MID-IDLEVNR-IN = ALL '+'                                         
048100     AND MID-IDANSK-IN  = ALL '+'                                         
048200     AND MID-KDLARM-IN  = ALL '+'                                         
048210     AND MID-KDOTFREK-IN  = ALL '+'                                       
048300       CONTINUE                                                           
048400     ELSE                                                                 
048900       MOVE '7'              TO MFS-IDPFK                                 
049000       MOVE SPACE            TO MFS-KDTRTYP                               
049100     END-IF                                                               
049200     IF  MID-IDARTNR-IN = '00000000 '                                     
049201       MOVE ZERO TO MID-IDARTNR-IN                                        
049202     END-IF                                                               
049203     IF  MID-IDANSK-IN = '00 '                                            
049204       MOVE ZERO TO MID-IDANSK-IN                                         
049205     END-IF                                                               
049206     IF  MID-KDLARM-IN = '00 '                                            
049207       MOVE ZERO TO MID-KDLARM-IN                                         
049208     END-IF                                                               
049210                                                                          
049300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
049400     MOVE '001'             TO MSGI-KDCALL                                
049500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049700     MOVE '2172'            TO MSGI-IDTRANS                               
049800     IF EGEN-MID                                                          
050301       MOVE MID-IDANSK-IN    TO MSGI-IDPERSON                             
050310       IF MID-IDANSK-IN NOT = ALL '+'                                     
050320         MOVE 'ANSK'         TO MSGI-KDARBTYP                             
050330       ELSE                                                               
050340         MOVE SPACE          TO MSGI-KDARBTYP                             
050350       END-IF                                                             
050360       MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                              
050370       MOVE MID-KDLARM-IN    TO MSGI-KDLARM                               
050371       IF MID-IDARTNR-IN NUMERIC                                          
050372         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
050373       END-IF                                                             
050380       MOVE MID-KDOTFREK-IN  TO MSGI-KDOTFREK                             
050900     ELSE                                                                 
051000       MOVE ZERO             TO MSGI-IDARTNR                              
051100       MOVE '7'              TO MFS-IDPFK                                 
051200       MOVE SPACE            TO MFS-KDTRTYP                               
051300     END-IF                                                               
051400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051401     MOVE MSGI-IDPERSON    TO WS-IDANSK                                   
051430     MOVE MSGI-IDLEVNR     TO WS-IDLEVNR                                  
051440     MOVE MSGI-KDLARM      TO WS-KDLARM                                   
051441     MOVE MSGI-IDARTNR     TO WS-IDARTNR                                  
051450     MOVE MSGI-KDOTFREK    TO WS-KDOTFREK                                 
051470     INSPECT WS-IDANSK  REPLACING LEADING SPACE BY ZERO                   
051480     INSPECT WS-KDLARM  REPLACING LEADING SPACE BY ZERO                   
051481     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
051490                                                                          
051492     MOVE WS-IDANSK        TO MOD-IDANSK-UT                               
051493     MOVE WS-IDLEVNR       TO MOD-IDLEVNR-UT                              
051494     MOVE WS-KDLARM        TO MOD-KDLARM-UT                               
051495     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
051496     MOVE WS-KDOTFREK      TO MOD-KDOTFREK-UT                             
051498     INSPECT MOD-IDANSK-UT  REPLACING LEADING ZERO BY SPACE               
051499     INSPECT MOD-KDLARM-UT  REPLACING LEADING ZERO BY SPACE               
051500     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
051510                                                                          
051600     MOVE MSGI-SPAR-AREA       TO SPAR-AREA                               
051610     IF SPAR-IDARTNR-MIN NOT NUMERIC                                      
051620       MOVE ZERO               TO SPAR-IDARTNR-MIN                        
051630     END-IF                                                               
051640     IF SPAR-IDARTNR-MAX NOT NUMERIC                                      
051650       MOVE ZERO               TO SPAR-IDARTNR-MAX                        
051660     END-IF                                                               
051700                                                                          
051800     IF EGEN-MID                                                          
051900       IF MID-IDANSK-IN = ALL '+'                                         
052200         IF WS-IDANSK NOT NUMERIC                                         
052300            MOVE ZERO          TO WS-IDANSK-NUM                           
052400         END-IF                                                           
052500       ELSE                                                               
052700         MOVE '7'              TO MFS-IDPFK                               
052800         MOVE SPACE            TO MFS-KDTRTYP                             
052900       END-IF                                                             
053000     ELSE                                                                 
053200       PERFORM BA-FINNS-IDARTNR-IDANSK                                    
053300     END-IF                                                               
053400     IF WS-IDANSK NUMERIC                                                 
053410       IF WS-IDANSK-NUM > ZERO                                            
053500         MOVE WS-IDANSK-NUM    TO W-IDANSK-MIN                            
053600                                  W-IDANSK-MAX                            
053700                                  W-IDANSK-ASEQ-MIN                       
053800                                  W-IDANSK-ASEQ-MAX                       
053900                                  W-IDANSK-ASEQ                           
054000       END-IF                                                             
054100     ELSE                                                                 
054300       MOVE NEJ                TO NYCKLAR-SW                              
054500     END-IF                                                               
054600                                                                          
054700     IF EGEN-MID                                                          
054800       IF MID-IDLEVNR-IN = ALL '+'                                        
054810         CONTINUE                                                         
055000       ELSE                                                               
055200         MOVE '7'              TO MFS-IDPFK                               
055300         MOVE SPACE            TO MFS-KDTRTYP                             
055400       END-IF                                                             
055700     END-IF                                                               
055800     IF WS-IDLEVNR > SPACE                                                
055900       MOVE WS-IDLEVNR         TO W-IDLEVNR-MIN                           
056000                                  W-IDLEVNR-MAX                           
056100                                  W-IDLEVNR-ASEQ-MIN                      
056200                                  W-IDLEVNR-ASEQ-MAX                      
056300                                  W-IDLEVNR-ASEQ                          
056500     ELSE                                                                 
056600       IF WS-IDLEVNR NOT = SPACE                                          
056700          MOVE NEJ             TO NYCKLAR-SW                              
056800       ELSE                                                               
056900          IF WDK6-IDLEVNR > SPACE                                         
057000             MOVE WDK6-IDLEVNR TO WS-IDLEVNR                              
057100                                  W-IDLEVNR-MIN                           
057200                                  W-IDLEVNR-MAX                           
057300                                  W-IDLEVNR-ASEQ-MIN                      
057400                                  W-IDLEVNR-ASEQ-MAX                      
057500                                  W-IDLEVNR-ASEQ                          
057600                                  MOD-IDLEVNR-UT                          
058300          END-IF                                                          
058400       END-IF                                                             
058500     END-IF                                                               
058600                                                                          
058700     IF EGEN-MID                                                          
058800       IF MID-KDLARM-IN = ALL '+'                                         
059100         IF WS-KDLARM NOT NUMERIC                                         
059200            MOVE ZERO          TO WS-KDLARM-NUM                           
059300         END-IF                                                           
059400       ELSE                                                               
059600         MOVE '7'              TO MFS-IDPFK                               
059700         MOVE SPACE            TO MFS-KDTRTYP                             
059800       END-IF                                                             
060100     END-IF                                                               
060200     IF WS-KDLARM NUMERIC  AND WS-KDLARM-NUM > ZERO                       
060300       MOVE WS-KDLARM-NUM      TO W-KDLARM-MIN                            
060400                                  W-KDLARM-MAX                            
060600     ELSE                                                                 
060700       IF WS-KDLARM NOT = '000'                                           
060800          MOVE NEJ             TO NYCKLAR-SW                              
060900       END-IF                                                             
061000     END-IF                                                               
061100                                                                          
061200     IF EGEN-MID                                                          
061300       IF MID-IDARTNR-IN = ALL '+'                                        
061600         IF WS-IDARTNR NOT NUMERIC                                        
061700            MOVE ZERO          TO WS-IDARTNR-NUM                          
061800         END-IF                                                           
061900       ELSE                                                               
062100         MOVE '7'              TO MFS-IDPFK                               
062200         MOVE SPACE            TO MFS-KDTRTYP                             
062300       END-IF                                                             
062600     END-IF                                                               
062700     IF WS-IDARTNR NUMERIC AND WS-IDARTNR-NUM > ZERO                      
062800       MOVE WS-IDARTNR-NUM     TO W-IDARTNR-MIN                           
062900                                  W-IDARTNR-MAX                           
063000                                  W-IDARTNR-ASEQ-MIN                      
063100                                  W-IDARTNR-ASEQ-MAX                      
063200                                  W-IDARTNR-ASEQ                          
063310       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
063400     ELSE                                                                 
063500       IF WS-IDARTNR NOT = '000000000'                                    
063600          MOVE NEJ             TO NYCKLAR-SW                              
063700       END-IF                                                             
063800     END-IF                                                               
063801                                                                          
063810     IF EGEN-MID                                                          
063895       IF MID-KDOTFREK-IN = ALL '+'                                       
063896         CONTINUE                                                         
063899       ELSE                                                               
063900         IF MID-KDOTFREK-IN = 'H' OR 'L' OR ' '                           
063901           CONTINUE                                                       
063904         ELSE                                                             
063905           MOVE NEJ TO NYCKLAR-SW                                         
063906         END-IF                                                           
063907       END-IF                                                             
063910     END-IF                                                               
063920                                                                          
064000     IF NYCKLAR-FEL                                                       
064100       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
064200       CALL WMEDKONV USING MED-WMEDAREA                                   
064300       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
064400       PERFORM MFS-RENSA-FAELT-IN                                         
064500       PERFORM MFS-RENSA-FAELT-UT                                         
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900 BA-FINNS-IDARTNR-IDANSK SECTION.                                         
065000                                                                          
065100     IF MSGI-IDARTNR NUMERIC                                              
065200        MOVE MSGI-IDARTNR   TO W-IDARTNR                                  
065201                                                                          
065210        MOVE '  GE' TO GODK-STATUSKODER                                   
065300        PERFORM IMS-GU-K601                                               
065400        IF SEGMENT-FINNS                                                  
065500           MOVE ART-IDLEVNR TO WDK6-IDLEVNR                               
065600           PERFORM IMS-GNP-K611                                           
065700           IF SEGMENT-FINNS                                               
065800              MOVE CLAG-IDANSK TO WS-IDANSK-NUM                           
065900                                  W-IDANSK-L                              
066000              PERFORM IMS-GET-XXBX-2232                                   
066100              IF SEGMENT-FINNS                                            
066200                MOVE XXBX-2232-IDANSK-LARM                                
066300                               TO WS-IDANSK-NUM                           
066400              END-IF                                                      
066500           END-IF                                                         
066600        END-IF                                                            
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000 C-FOERSTA-SIDA SECTION.                                                  
067100                                                                          
067200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
067300     CALL WMEDKONV USING MED-WMEDAREA                                     
067400     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
067500                                                                          
067600     PERFORM MFS-RENSA-FAELT-IN                                           
067700     .                                                                    
067800     EJECT                                                                
067900 D-NAESTA-SIDA SECTION.                                                   
068000                                                                          
068100     IF SPAR-IDTRANS = '2172'                                             
068200       MOVE SPAR-IDANSK-MIN    TO W-IDANSK-MIN                            
068300                                  W-IDANSK-ASEQ-MIN                       
068400                                  W-IDANSK-ASEQ                           
068500       MOVE SPAR-IDLEVNR-MIN   TO W-IDLEVNR-MIN                           
068600                                  W-IDLEVNR-ASEQ-MIN                      
068700                                  W-IDLEVNR-ASEQ                          
068800       MOVE SPAR-IDARTNR-MIN   TO W-IDARTNR-MIN                           
068900                                  W-IDARTNR-ASEQ-MIN                      
069000                                  W-IDARTNR-ASEQ                          
069100       MOVE SPAR-DAREGDAT-NEXT TO W-DAREGDAT-9KOMPL                       
069200                                  W-DAREGDAT-9KOMPL-ASEQ                  
069300                                  W-DAREGDAT-9KOMPL-ASEQ-MIN              
069400       MOVE SPAR-TIKLOCK-NEXT  TO W-TIKLOCK-9KOMPL                        
069500                                  W-TIKLOCK-9KOMPL-ASEQ                   
069600                                  W-TIKLOCK-9KOMPL-ASEQ-MIN               
069700       MOVE SPAR-KDLARM-MIN    TO W-KDLARM-MIN                            
069800       MOVE SPAR-IDARTNR-MAX   TO W-IDARTNR-MAX                           
069900       MOVE SPAR-IDANSK-MAX    TO W-IDANSK-MAX                            
070000       MOVE SPAR-IDLEVNR-MAX   TO W-IDLEVNR-MAX                           
070100       MOVE SPAR-KDLARM-MAX    TO W-KDLARM-MAX                            
070200     ELSE                                                                 
070300       PERFORM MFS-RENSA-FAELT-IN                                         
070400     END-IF                                                               
070500     .                                                                    
070600     EJECT                                                                
070700 E-SAMMA-SIDA SECTION.                                                    
070800                                                                          
070900     IF SPAR-IDTRANS = '2172' OR '0551'                                   
071000       MOVE SPAR-IDANSK-MIN     TO W-IDANSK-MIN                           
071100                                   W-IDANSK-ASEQ-MIN                      
071200                                   W-IDANSK-ASEQ                          
071300       MOVE SPAR-IDLEVNR-MIN    TO W-IDLEVNR-MIN                          
071400                                   W-IDLEVNR-ASEQ-MIN                     
071500                                   W-IDLEVNR-ASEQ                         
071600       MOVE SPAR-IDARTNR-MIN    TO W-IDARTNR-MIN                          
071700                                   W-IDARTNR-ASEQ-MIN                     
071800                                   W-IDARTNR-ASEQ                         
071900       MOVE SPAR-DAREGDAT-ENTER TO W-DAREGDAT-9KOMPL                      
072000                                   W-DAREGDAT-9KOMPL-ASEQ                 
072100                                   W-DAREGDAT-9KOMPL-ASEQ-MIN             
072200       MOVE SPAR-TIKLOCK-ENTER  TO W-TIKLOCK-9KOMPL                       
072300                                   W-TIKLOCK-9KOMPL-ASEQ                  
072400                                   W-TIKLOCK-9KOMPL-ASEQ-MIN              
072500       MOVE SPAR-KDLARM-MIN     TO W-KDLARM-MIN                           
072600       MOVE SPAR-IDARTNR-MAX    TO W-IDARTNR-MAX                          
072700       MOVE SPAR-IDANSK-MAX     TO W-IDANSK-MAX                           
072800       MOVE SPAR-IDLEVNR-MAX    TO W-IDLEVNR-MAX                          
072900       MOVE SPAR-KDLARM-MAX     TO W-KDLARM-MAX                           
073000       IF MID-MID-INPUT = ALL '+'                                         
073100         PERFORM MFS-RENSA-FAELT-IN                                       
073110         MOVE MFS-ROER-EJ-FAELT TO MOD-TEMFSFEL                           
073200       ELSE                                                               
073300         MOVE JA             TO SW-SELECT                                 
073400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
073500         CALL WMEDKONV USING MED-WMEDAREA                                 
073600         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
073700         PERFORM EA-MID-INDATA-TILL-MOD                                   
073800       END-IF                                                             
073900     ELSE                                                                 
074000       PERFORM MFS-RENSA-FAELT-IN                                         
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 EA-MID-INDATA-TILL-MOD SECTION.                                          
074500                                                                          
074600* * * * * FÖR VARJE MID-FÄLT                                              
074700* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
074800* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
074900* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
075000     MOVE +1 TO IX                                                        
075100     PERFORM UNTIL IX > MAX-IX                                            
075200        IF MID-SELECT-ARTIKEL (IX) NOT = ALL '+'                          
075300           MOVE MID-SELECT-ARTIKEL (IX)   TO                              
075400                MOD-SELECT-ARTIKEL (IX)                                   
075500           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
075600                MOD-SELECT-ARTIKEL-ATTR (IX)                              
075700        ELSE                                                              
075800           MOVE MFS-RENSA-FAELT       TO MOD-SELECT-ARTIKEL (IX)          
075900        END-IF                                                            
076000        ADD +1 TO IX                                                      
076100     END-PERFORM                                                          
076200     .                                                                    
076300     EJECT                                                                
076400 F-LAES-VISA-INFO SECTION.                                                
076500                                                                          
076600     IF WS-IDANSK NUMERIC  AND WS-IDANSK-NUM  > ZERO AND                  
076700        WS-IDLEVNR > SPACE                                                
076800***     IDANSK OCH IDLEVNR IFYLLT > NOLL / SPACE                          
076900        PERFORM FA-LAS-SEK-INDEX                                          
077000     ELSE                                                                 
077100                                                                          
077200       MOVE +1 TO IX                                                      
077300       IF (MFS-FIRST OR MFS-SPLIT OR MFS-UPDATE) AND                      
077400          SW-DLET = 'N'                                                   
077500          PERFORM IMS-GET-WDD401                                          
077600       ELSE                                                               
077700          IF SW-DLET = 'J'                                                
077800             PERFORM IMS-GU-WDD401                                        
077900          ELSE                                                            
078000             PERFORM IMS-GHU-WDD401                                       
078100          END-IF                                                          
078200       END-IF                                                             
078300                                                                          
078400       IF SEGMENT-FINNS                                                   
078500         MOVE LAK-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-ENTER                  
078600         MOVE LAK-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-ENTER                   
078700       ELSE                                                               
078800         MOVE ZERO             TO SPAR-DAREGDAT-ENTER                     
078900         MOVE ZERO             TO SPAR-TIKLOCK-ENTER                      
079000       END-IF                                                             
079100       MOVE W-IDARTNR-MIN      TO SPAR-IDARTNR-MIN                        
079200       MOVE W-IDANSK-MIN       TO SPAR-IDANSK-MIN                         
079300       MOVE W-IDLEVNR-MIN      TO SPAR-IDLEVNR-MIN                        
079400       MOVE W-KDLARM-MIN       TO SPAR-KDLARM-MIN                         
079500       MOVE W-IDARTNR-MAX      TO SPAR-IDARTNR-MAX                        
079600       MOVE W-IDANSK-MAX       TO SPAR-IDANSK-MAX                         
079700       MOVE W-IDLEVNR-MAX      TO SPAR-IDLEVNR-MAX                        
079800       MOVE W-KDLARM-MAX       TO SPAR-KDLARM-MAX                         
079900* PAH TEST                                                                
080000*    STRING                                                               
080100*         DELIMITED BY SIZE INTO MOD-TEMFSINF                             
080200                                                                          
080300       PERFORM UNTIL IX > MAX-IX                                          
080400                 OR SEGMENT-SAKNAS                                        
080500         IF SEGMENT-FINNS                                                 
080502          MOVE LAK-IDARTNR    TO W-IDARTNR                                
080503                                                                          
080504          MOVE '    ' TO GODK-STATUSKODER                                 
080505          PERFORM IMS-GU-K601                                             
080508          PERFORM IMS-GNP-K611                                            
080509                                                                          
080520          IF WS-KDOTFREK = CLAG-KDOTFREK OR SPACE                         
080600                                                                          
080700*          --- SELEKTERA FÖR BEGRÄNSAD VISNING P.G.A. SEC-IDLEV           
080800           MOVE LAK-IDLEVNR TO WS-IDLEVNR-8                               
080900                                                                          
081000           PERFORM S2-SECURITY-CHECK-SUPPLIER                             
081100           IF PASSED-SECURITY-CHECK                                       
081200                                                                          
081300             MOVE LAK-DAREGDAT-9KOMPL TO SPAR-RAD-DAREGDAT(IX)            
081400             MOVE LAK-TIKLOCK-9KOMPL TO SPAR-RAD-TIKLOCK (IX)             
081500             MOVE LAK-IDARTNR        TO SPAR-RAD-IDARTNR (IX)             
081600             COMPUTE WS-DAREGDAT =                                        
081700                     99999999 - LAK-DAREGDAT-9KOMPL                       
081800             MOVE WS-TIREGDAT   TO MOD-TIREGDAT (IX)                      
081900             MOVE LAK-FLNYLARM  TO MOD-FLNYLARM (IX)                      
082000             MOVE LAK-IDARTNR   TO MOD-IDARTNR  (IX)                      
082100             MOVE LAK-IDLEVNR   TO MOD-IDLEVNR  (IX)                      
082200             MOVE LAK-TIAAMMDD  TO MOD-TIPLANDAT(IX)                      
082300             MOVE LAK-KVAVIS    TO MOD-KVAVIS   (IX)                      
082400             MOVE LAK-KVAVROP   TO MOD-KVAVROP  (IX)                      
082500             MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL (IX)              
082600             IF LAK-KDLARM = 220                                          
082700               MOVE WS-TEORSLRM-220 TO MOD-TEORSLRM (IX)                  
082800             ELSE                                                         
082900               IF LAK-KDLARM = 225                                        
083000                 IF LAK-KVAVIS > ZERO                                     
083100*                  TIDIGARE 226                                           
083200                   MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)              
083300                 ELSE                                                     
083400                   MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)              
083500                   MOVE ZERO          TO MOD-KVAVIS   (IX)                
083600                 END-IF                                                   
083700               ELSE                                                       
083800                 IF LAK-KDLARM = 230                                      
083900                   MOVE WS-TEORSLRM-230 TO MOD-TEORSLRM (IX)              
084000                 ELSE                                                     
084100                   MOVE WS-TEORSLRM-235 TO MOD-TEORSLRM (IX)              
084200                 END-IF                                                   
084300               END-IF                                                     
084400             END-IF                                                       
084410             MOVE CLAG-KDOTFREK             TO MOD-KDOTFREK (IX)          
084500             ADD 1 TO IX                                                  
084600           ELSE                                                           
084700             MOVE MED-1 TO MOD-TEMFSFEL                                   
084800           END-IF                                                         
084810          END-IF                                                          
084900         END-IF                                                           
085000         PERFORM IMS-GET-WDD401                                           
085100       END-PERFORM                                                        
085200                                                                          
085300       IF SEGMENT-FINNS                                                   
085400         MOVE LAK-DAREGDAT-9KOMPL  TO SPAR-DAREGDAT-NEXT                  
085500         MOVE LAK-TIKLOCK-9KOMPL   TO SPAR-TIKLOCK-NEXT                   
085600         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
085700         CALL WMEDKONV USING MED-WMEDAREA                                 
085800         MOVE MED-TEMFSINF  TO MOD-TEMFSINF                               
085900       ELSE                                                               
086000         PERFORM UNTIL IX > MAX-IX                                        
086100           MOVE ZERO            TO SPAR-RAD-DAREGDAT(IX)                  
086200                                   SPAR-RAD-TIKLOCK (IX)                  
086300                                   SPAR-RAD-IDARTNR (IX)                  
086400           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR  (IX)                      
086500                                   MOD-IDLEVNR  (IX)                      
086600                                   MOD-TEORSLRM (IX)                      
086610                                   MOD-KDOTFREK (IX)                      
086700                                   MOD-TIREGDAT (IX)                      
086800                                   MOD-TIPLANDAT(IX)                      
086900                                   MOD-KVAVIS   (IX)                      
087000                                   MOD-KVAVROP  (IX)                      
087100                                   MOD-FLNYLARM (IX)                      
087200                                   MOD-SELECT-ARTIKEL (IX)                
087300           ADD 1                TO IX                                     
087400         END-PERFORM                                                      
087500         MOVE ZERO                 TO SPAR-DAREGDAT-NEXT                  
087600         MOVE ZERO                 TO SPAR-TIKLOCK-NEXT                   
087700       END-IF                                                             
087800                                                                          
087900       MOVE '002'      TO MSGI-KDCALL                                     
088000       MOVE '2172'     TO SPAR-IDTRANS                                    
088100       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
088200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
088300     END-IF                                                               
088400     .                                                                    
088500     EJECT                                                                
088600 FA-LAS-SEK-INDEX SECTION.                                                
088700                                                                          
088800*      NÄR BÅDE IDANSK OCH IDLEVNR ÄR IFYLLT SKALL VI                     
088900*      LÄSA VIA DET SEKUNDÄRA INDEXET, SÅ ATT VI FÅR                      
089000*      RADERNA I ARTIKELNUMMERORDNING                                     
089100                                                                          
089200       MOVE +1 TO IX                                                      
089300       IF (MFS-FIRST OR MFS-SPLIT OR MFS-UPDATE) AND                      
089400          SW-DLET = 'N'                                                   
089500          PERFORM IMS-GN-SEK-WDD401                                       
089600       ELSE                                                               
089700          IF SW-DLET = 'J'                                                
089800             PERFORM IMS-GU-SEK-WDD401                                    
089900          ELSE                                                            
090000             PERFORM IMS-GET-SEK-WDD401                                   
090100          END-IF                                                          
090200       END-IF                                                             
090300                                                                          
090400       IF SEGMENT-FINNS                                                   
090500         MOVE LAK-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-ENTER                  
090600         MOVE LAK-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-ENTER                   
090700       ELSE                                                               
090800         MOVE ZERO             TO SPAR-DAREGDAT-ENTER                     
090900         MOVE ZERO             TO SPAR-TIKLOCK-ENTER                      
091000       END-IF                                                             
091100                                                                          
091200       MOVE W-IDARTNR-MIN      TO SPAR-IDARTNR-MIN                        
091300       MOVE W-IDANSK-MIN       TO SPAR-IDANSK-MIN                         
091400       MOVE W-IDLEVNR-MIN      TO SPAR-IDLEVNR-MIN                        
091500       MOVE W-KDLARM-MIN       TO SPAR-KDLARM-MIN                         
091600       MOVE W-IDARTNR-MAX      TO SPAR-IDARTNR-MAX                        
091700       MOVE W-IDANSK-MAX       TO SPAR-IDANSK-MAX                         
091800       MOVE W-IDLEVNR-MAX      TO SPAR-IDLEVNR-MAX                        
091900       MOVE W-KDLARM-MAX       TO SPAR-KDLARM-MAX                         
092000                                                                          
092100       PERFORM UNTIL IX > MAX-IX                                          
092200                  OR SEGMENT-SAKNAS                                       
092300         IF SEGMENT-FINNS                                                 
092310          MOVE LAK-IDARTNR    TO W-IDARTNR                                
092311                                                                          
092312          MOVE '    ' TO GODK-STATUSKODER                                 
092320          PERFORM IMS-GU-K601                                             
092360          PERFORM IMS-GNP-K611                                            
092370                                                                          
092391          IF WS-KDOTFREK = CLAG-KDOTFREK OR SPACE                         
092400*          --- SELEKTERA FÖR BEGRÄNSAD VISNING P.G.A. SEC-IDLEV           
092500           MOVE LAK-IDLEVNR TO WS-IDLEVNR-8                               
092600                                                                          
092700           PERFORM S2-SECURITY-CHECK-SUPPLIER                             
092800           IF PASSED-SECURITY-CHECK                                       
092900                                                                          
093000             IF LAK-KDLARM <= W-KDLARM-MAX                                
093100             AND LAK-KDLARM >= W-KDLARM-MIN                               
093200                MOVE LAK-DAREGDAT-9KOMPL TO SPAR-RAD-DAREGDAT(IX)         
093300                MOVE LAK-TIKLOCK-9KOMPL TO SPAR-RAD-TIKLOCK (IX)          
093400                MOVE LAK-IDARTNR     TO SPAR-RAD-IDARTNR (IX)             
093500                COMPUTE WS-DAREGDAT =                                     
093600                        99999999 - LAK-DAREGDAT-9KOMPL                    
093700                MOVE WS-TIREGDAT TO MOD-TIREGDAT (IX)                     
093800                MOVE LAK-FLNYLARM TO MOD-FLNYLARM (IX)                    
093900                MOVE LAK-IDARTNR TO MOD-IDARTNR (IX)                      
094000                MOVE LAK-IDLEVNR TO MOD-IDLEVNR (IX)                      
094100                MOVE LAK-TIAAMMDD TO MOD-TIPLANDAT(IX)                    
094200                MOVE LAK-KVAVIS TO MOD-KVAVIS   (IX)                      
094300                MOVE LAK-KVAVROP TO MOD-KVAVROP (IX)                      
094400                MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL (IX)           
094500                IF LAK-KDLARM = 220                                       
094600                  MOVE WS-TEORSLRM-220 TO MOD-TEORSLRM (IX)               
094700                ELSE                                                      
094800                  IF LAK-KDLARM = 225                                     
094900                    IF LAK-KVAVIS > ZERO                                  
095000*                     TIDIGARE 226                                        
095100                      MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)           
095200                    ELSE                                                  
095300                      MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)           
095400                      MOVE ZERO       TO MOD-KVAVIS   (IX)                
095500                    END-IF                                                
095600                  ELSE                                                    
095700                    IF LAK-KDLARM = 230                                   
095800                      MOVE WS-TEORSLRM-230 TO MOD-TEORSLRM (IX)           
095900                    ELSE                                                  
096000                      MOVE WS-TEORSLRM-235 TO MOD-TEORSLRM (IX)           
096100                    END-IF                                                
096200                  END-IF                                                  
096300                END-IF                                                    
096310                MOVE CLAG-KDOTFREK         TO MOD-KDOTFREK (IX)           
096400                ADD 1 TO IX                                               
096500             END-IF                                                       
096600           ELSE                                                           
096700             MOVE MED-1 TO MOD-TEMFSFEL                                   
096800           END-IF                                                         
096810          END-IF                                                          
096900         END-IF                                                           
097000         PERFORM IMS-GN-SEK-WDD401                                        
097100                                                                          
097200       END-PERFORM                                                        
097300                                                                          
097400       IF SEGMENT-FINNS                                                   
097500*        -- FLER SIDOR KAN VISAS, SPARA VÄRDEN                            
097600         MOVE LAK-DAREGDAT-9KOMPL  TO SPAR-DAREGDAT-NEXT                  
097700         MOVE LAK-TIKLOCK-9KOMPL   TO SPAR-TIKLOCK-NEXT                   
097800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
097900         CALL WMEDKONV USING MED-WMEDAREA                                 
098000         MOVE MED-TEMFSINF  TO MOD-TEMFSINF                               
098100       ELSE                                                               
098200         PERFORM UNTIL IX > MAX-IX                                        
098300*          --- DE SISTA ÅTERSTÅENDE RADERNA PÅ SKÄRMEN                    
098400           MOVE ZERO            TO SPAR-RAD-DAREGDAT(IX)                  
098500                                   SPAR-RAD-TIKLOCK (IX)                  
098600                                   SPAR-RAD-IDARTNR (IX)                  
098700           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR  (IX)                      
098800                                   MOD-IDLEVNR  (IX)                      
098900                                   MOD-TEORSLRM (IX)                      
098910                                   MOD-KDOTFREK (IX)                      
099000                                   MOD-TIREGDAT (IX)                      
099100                                   MOD-TIPLANDAT(IX)                      
099200                                   MOD-KVAVIS   (IX)                      
099300                                   MOD-KVAVROP  (IX)                      
099400                                   MOD-FLNYLARM (IX)                      
099500                                   MOD-SELECT-ARTIKEL (IX)                
099600           ADD 1 TO IX                                                    
099700         END-PERFORM                                                      
099800         MOVE ZERO                 TO SPAR-DAREGDAT-NEXT                  
099900         MOVE ZERO                 TO SPAR-TIKLOCK-NEXT                   
100000       END-IF                                                             
100100                                                                          
100200       MOVE '002'      TO MSGI-KDCALL                                     
100300       MOVE '2172'     TO SPAR-IDTRANS                                    
100400       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
100500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
100600     .                                                                    
100700     EJECT                                                                
100800 G-KOLLA-INPUT SECTION.                                                   
100900                                                                          
101000     MOVE JA  TO INDATA-SW                                                
101100                                                                          
101200     MOVE +1 TO IX                                                        
101300     PERFORM UNTIL IX > MAX-IX                                            
101400                                                                          
101500       IF MID-SELECT-ARTIKEL (IX) NOT = '+'                               
101600        IF MID-SELECT-ARTIKEL (IX) = 'B' OR 'D' OR 'S'                    
101700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ARTIKEL-ATTR (IX)        
101800        ELSE                                                              
101900         IF MID-SELECT-ARTIKEL (IX) = LOW-VALUE OR                        
102000            MID-SELECT-ARTIKEL (IX) < 'A'       OR                        
102100            MID-SELECT-ARTIKEL (IX) > 'Z'                                 
102200           MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ARTIKEL-ATTR (IX)        
102300         ELSE                                                             
102400           MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ARTIKEL-ATTR (IX)        
102500           MOVE NEJ TO INDATA-SW                                          
102600         END-IF                                                           
102700        END-IF                                                            
102800       ELSE                                                               
102900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ARTIKEL-ATTR (IX)        
103000       END-IF                                                             
103100       ADD +1 TO IX                                                       
103200     END-PERFORM                                                          
103300                                                                          
103400     IF INDATA-FEL                                                        
103500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
103600         CALL WMEDKONV USING MED-WMEDAREA                                 
103700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
103800     END-IF                                                               
103900     PERFORM MFS-ROER-EJ-FAELT-UT                                         
104000     PERFORM MFS-ROER-EJ-FAELT-IN                                         
104100     .                                                                    
104200     EJECT                                                                
104300 H-UPPDATERA SECTION.                                                     
104400                                                                          
104500*    SÖK IGENOM VILKA RADER SOM ÄR MARKERADE FÖR BORTTAG                  
104600*                                                                         
104700     MOVE NEJ TO SW-DLET                                                  
104800     MOVE +1  TO IX                                                       
104900     PERFORM UNTIL IX > MAX-IX                                            
105000       IF MID-SELECT-ARTIKEL (IX) = 'B' OR 'D'                            
105100*        HÄMTA NYCKLAR FRÅN SPAR                                          
105200         MOVE SPAR-RAD-DAREGDAT (IX) TO W-DAREGDAT-9KOMPL                 
105300         MOVE SPAR-RAD-TIKLOCK  (IX) TO W-TIKLOCK-9KOMPL                  
105400         PERFORM IMS-GHU-WDD401                                           
105500         IF SEGMENT-FINNS                                                 
105600            COMPUTE WS-DAREGDAT =                                         
105700                             99999999 - LAK-DAREGDAT-9KOMPL               
105800            PERFORM IMS-DLET-WDD401                                       
105900            MOVE JA TO SW-DLET                                            
106000            PERFORM HA-SAVE-ALARM                                         
106100         END-IF                                                           
106200       END-IF                                                             
106300       ADD +1 TO IX                                                       
106400     END-PERFORM                                                          
106500                                                                          
106600     IF SW-DLET = JA                                                      
106700       MOVE SPAR-RAD-DAREGDAT (1) TO W-DAREGDAT-9KOMPL                    
106800       MOVE SPAR-RAD-TIKLOCK  (1) TO W-TIKLOCK-9KOMPL                     
106900       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
107000       CALL WMEDKONV USING MED-WMEDAREA                                   
107100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
107200       PERFORM MFS-FORM-ATTR                                              
107300       PERFORM MFS-RENSA-FAELT-IN                                         
107400* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 HA-SAVE-ALARM SECTION.                                                   
107900                                                                          
108000***  SAVE ALARM IN WDR3 IF ALARM CREATED IN CURRENT WEEK                  
108100     MOVE WS-TIREGDAT       TO DAT-I-TIDATUM                              
108200                                                                          
108300     PERFORM S01-CONVERT-DATE-FMT                                         
108400     IF DAT-KDSVAR = ' '                                                  
108500        MOVE DAT-TIAAVV-GRP TO WS-ALARM-AAVV                              
108600        IF WS-ALARM-AAVV     = DAGENS-AAVV                                
108700           PERFORM HAA-LOG-WDR3                                           
108800        END-IF                                                            
108900     END-IF                                                               
109000     .                                                                    
109100     EJECT                                                                
109200 HAA-LOG-WDR3 SECTION.                                                    
109300                                                                          
109400     MOVE 'W2017200'        TO FIL-IDPGM                                  
109500     MOVE DAGENS-DATUM      TO FIL-TIREGDAT                               
109600     ADD  +1                TO W-TIKLOCK                                  
109700     MOVE W-TIKLOCK         TO FIL-TIKLOCK                                
109800     MOVE 1                 TO FIL-IDSEKVNR                               
109900     MOVE 'W214ALOG'        TO FIL-IDCPYTXT                               
110000*                                                                         
110100     MOVE 'WDD4'            TO ALOG-IDSYSTEM                              
110200     MOVE ZERO              TO ALOG-TIAAVVD                               
110300     MOVE LAK-KDLARM        TO ALOG-KDLARM                                
110400     MOVE LAK-IDARTNR       TO ALOG-IDARTNR                               
110500     MOVE LAK-IDANSK        TO ALOG-IDANSK                                
110600     MOVE LAK-IDLEVNR       TO ALOG-IDLEVNR                               
110700     MOVE ZERO              TO ALOG-IDDISTR                               
110800     MOVE WS-TIREGDAT       TO ALOG-TIREGDAT                              
110900     MOVE LAK-TIAAMMDD      TO ALOG-TIPLANDAT                             
111000     MOVE LAK-KVAVIS        TO ALOG-KVAVIS                                
111100     MOVE LAK-KVAVROP       TO ALOG-KVAVROP                               
111200*                                                                         
111300     MOVE ALOG-W214ALOG     TO FIL-WDR301-DATA                            
111400     PERFORM IMS-ISRT-WDR301                                              
111500     .                                                                    
111600     EJECT                                                                
111700 K-PPSW-2102            SECTION.                                          
111800                                                                          
111900     MOVE +1  TO IX                                                       
112000     PERFORM UNTIL IX > MAX-IX                                            
112100       IF MID-SELECT-ARTIKEL (IX) = 'S'                                   
112200*        HÄMTA NYCKLAR FRÅN SPAR                                          
112300         MOVE SPAR-RAD-DAREGDAT (IX) TO W-DAREGDAT-9KOMPL                 
112400         MOVE SPAR-RAD-TIKLOCK  (IX) TO W-TIKLOCK-9KOMPL                  
112500         PERFORM IMS-GHU-WDD401                                           
112600         IF SEGMENT-FINNS                                                 
112700            MOVE SPACE               TO LAK-FLNYLARM                      
112800            PERFORM IMS-REPL-WDD401                                       
112900         END-IF                                                           
113000         MOVE SPAR-RAD-IDARTNR (IX)  TO PROGSW-IDARTNR-IN                 
113100         MOVE '2102'                 TO P-IDTRANS                         
113200                                                                          
113300         PERFORM IMS-INSERT-ALT                                           
113400                                                                          
113500         MOVE ZERO TO RETURN-CODE                                         
113600         GOBACK                                                           
113700       END-IF                                                             
113800       ADD +1 TO IX                                                       
113900     END-PERFORM                                                          
114000     .                                                                    
114100     EJECT                                                                
114200 S01-CONVERT-DATE-FMT SECTION.                                            
114300                                                                          
114400     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
114500     CALL WDATKONV       USING DAT-KDDATFORM                              
114600                               DAT-I-TIDATUM                              
114700                               DAT-O-TIDATUM                              
114800                               DAT-KDSVAR                                 
114900     .                                                                    
115000     EJECT                                                                
115100                                                                          
115200 S2-SECURITY-CHECK-SUPPLIER SECTION.                                      
115300     SKIP2                                                                
115400*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
115500                                                                          
115600     IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                            
115700     OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                      
115800*      --- BEHÖRIG USER                                                   
115900       SET PASSED-SECURITY-CHECK TO TRUE                                  
116000     ELSE                                                                 
116100       SET BLOCKED-SECURITY-CHECK TO TRUE                                 
116200     END-IF                                                               
116300     .                                                                    
116400     EJECT                                                                
116500                                                                          
116600 MFS-RENSA-FAELT-UT SECTION.                                              
116700                                                                          
116800*    --- ALLA UTDATA-FÄLT                                                 
116900* ?  --- INKL. BLÄDDRINGSNYCKLAR                                          
117000* ?  MOVE MFS-RENSA-FAELT TO MOD-XXXXXXXX                                 
117100* ? SPAR ?                   MOD-XXXXXXXX                                 
117200     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
117300     .                                                                    
117400     SKIP3                                                                
117500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
117600                                                                          
117700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
117800     MOVE +1 TO IX                                                        
117900     PERFORM UNTIL IX > MAX-IX                                            
118000*      MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT (IX)                          
118100*                              MOD-TIKLOCK  (IX)                          
118200       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR  (IX)                          
118300                               MOD-IDLEVNR  (IX)                          
118400                               MOD-TEORSLRM (IX)                          
118410                               MOD-KDOTFREK (IX)                          
118500                               MOD-FLNYLARM (IX)                          
118700                               MOD-TIREGDAT (IX)                          
118800                               MOD-TIPLANDAT(IX)                          
118900                               MOD-KVAVIS   (IX)                          
119000                               MOD-KVAVROP  (IX)                          
119100                               MOD-SELECT-ARTIKEL (IX)                    
119200       ADD +1 TO IX                                                       
119300     END-PERFORM                                                          
119400     .                                                                    
119500     SKIP3                                                                
119600 MFS-RENSA-FAELT-IN SECTION.                                              
119700                                                                          
119800*    --- ALLA INDATA-FÄLT                                                 
119900     MOVE +1 TO IX                                                        
120000     PERFORM UNTIL IX > MAX-IX                                            
120100       MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL (IX)                    
120200       ADD +1 TO IX                                                       
120300     END-PERFORM                                                          
120400     .                                                                    
120500     EJECT                                                                
120600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
120700                                                                          
120800*    --- ALLA UTDATA-FÄLT                                                 
120900*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
121000* ?  MOVE MFS-ROER-EJ-FAELT TO MOD-XXXXXXXX                               
121100* ?  SPAR ?                    MOD-XXXXXXXX                               
121200     MOVE +1 TO IX                                                        
121300     PERFORM UNTIL IX > MAX-IX                                            
121400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
121500       ADD +1 TO IX                                                       
121600     END-PERFORM                                                          
121700     .                                                                    
121800     SKIP2                                                                
121900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
122000                                                                          
122100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
122200*    MOVE MFS-ROER-EJ-FAELT TO MOD-TIREGDAT (IX)                          
122300*                              MOD-TIKLOCK  (IX)                          
122400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR  (IX)                          
122500                               MOD-IDLEVNR  (IX)                          
122600                               MOD-TEORSLRM (IX)                          
122610                               MOD-KDOTFREK (IX)                          
122700                               MOD-FLNYLARM (IX)                          
122900                               MOD-TIREGDAT (IX)                          
123000                               MOD-TIPLANDAT(IX)                          
123100                               MOD-KVAVIS   (IX)                          
123200                               MOD-KVAVROP  (IX)                          
123300     .                                                                    
123400     SKIP3                                                                
123500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
123600                                                                          
123700*    --- ALLA INDATA-FÄLT                                                 
123800     MOVE +1 TO IX                                                        
123900     PERFORM UNTIL IX > MAX-IX                                            
124000       MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT-ARTIKEL (IX)                  
124100       ADD +1 TO IX                                                       
124200     END-PERFORM                                                          
124300     .                                                                    
124400     EJECT                                                                
124500 MFS-FORM-ATTR SECTION.                                                   
124600                                                                          
124700*    --- ALLA INDATA-FÄLT                                                 
124800     MOVE +1 TO IX                                                        
124900     PERFORM UNTIL IX > MAX-IX                                            
125000       MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ARTIKEL-ATTR (IX)            
125100       ADD +1 TO IX                                                       
125200     END-PERFORM                                                          
125300     .                                                                    
125400     SKIP2                                                                
125500 MFS-LAES-IN-IGEN SECTION.                                                
125600                                                                          
125700*    --- ALLA INDATA-FÄLT                                                 
125800     MOVE +1 TO IX                                                        
125900     PERFORM UNTIL IX > MAX-IX                                            
126000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SELECT-ARTIKEL-ATTR (IX)         
126100       ADD +1 TO IX                                                       
126200     END-PERFORM                                                          
126300     .                                                                    
126400     EJECT                                                                
126500* --- IMS SEKTIONER ---                                                   
126600     SKIP3                                                                
126700 IMS-GET-MSG SECTION.                                                     
126800                                                                          
126900     MOVE '  QC' TO GODK-STATUSKODER                                      
127000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
127100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127200     PERFORM IMS-STATUSKONTROLL                                           
127300     .                                                                    
127400     SKIP3                                                                
127500 IMS-INSERT-MSG SECTION.                                                  
127600                                                                          
127700     IF MSGI-IDLAND-SPR = 'SE'                                            
127800       MOVE '0' TO MFS-KDHUVOMR                                           
127900     END-IF                                                               
128000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
128100     MOVE SPACE TO GODK-STATUSKODER                                       
128200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
128300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
128400     PERFORM IMS-STATUSKONTROLL                                           
128500     .                                                                    
128600     EJECT                                                                
128700 IMS-GET-WDD401   SECTION.                                                
128800                                                                          
128900     STRING 'WDD401  (IDARTNR >=' W-IDARTNR-MIN-X                         
129000                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
129100                    '&IDANSK  >=' W-IDANSK-MIN-X                          
129200                    '&IDANSK  <=' W-IDANSK-MAX-X                          
129300                    '&IDLEVNR >=' W-IDLEVNR-MIN-X                         
129400                    '&IDLEVNR <=' W-IDLEVNR-MAX-X                         
129500                    '&KDLARM  >=' W-KDLARM-MIN-X                          
129600                    '&KDLARM  <=' W-KDLARM-MAX-X                          
129700                    '&IDDC     =' W-IDDC-D4-X ')'                         
129800          DELIMITED BY SIZE INTO SSA1                                     
129900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
130000     CALL CBLTDLI USING GN  WDD4-PCB DLI-IO-WDD401 SSA1                   
130100     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
130200     PERFORM IMS-STATUSKONTROLL                                           
130300     .                                                                    
130400     SKIP3                                                                
130500 IMS-GU-WDD401   SECTION.                                                 
130600                                                                          
130700     STRING 'WDD401  (WDD401KY>=' W-WDD401KY-X                            
130800                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
130900                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
131000                    '&IDANSK  >=' W-IDANSK-MIN-X                          
131100                    '&IDANSK  <=' W-IDANSK-MAX-X                          
131200                    '&IDLEVNR >=' W-IDLEVNR-MIN-X                         
131300                    '&IDLEVNR <=' W-IDLEVNR-MAX-X                         
131400                    '&KDLARM  >=' W-KDLARM-MIN-X                          
131500                    '&KDLARM  <=' W-KDLARM-MAX-X                          
131600                    '&IDDC     =' W-IDDC-D4-X ')'                         
131700          DELIMITED BY SIZE INTO SSA1                                     
131800     MOVE '  GE' TO GODK-STATUSKODER                                      
131900     CALL CBLTDLI USING GU WDD4-PCB DLI-IO-WDD401 SSA1                    
132000     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     .                                                                    
132300     EJECT                                                                
132400 IMS-GHU-WDD401   SECTION.                                                
132500                                                                          
132600     STRING 'WDD401  (WDD401KY =' W-WDD401KY-X ')'                        
132700          DELIMITED BY SIZE INTO SSA1                                     
132800     MOVE '  GE' TO GODK-STATUSKODER                                      
132900     CALL CBLTDLI USING GHU WDD4-PCB DLI-IO-WDD401 SSA1                   
133000     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300     SKIP3                                                                
133400 IMS-REPL-WDD401   SECTION.                                               
133500                                                                          
133600     MOVE '  ' TO GODK-STATUSKODER                                        
133700     CALL CBLTDLI USING REPL WDD4-PCB DLI-IO-WDD401                       
133800     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
133900     PERFORM IMS-STATUSKONTROLL                                           
134000     .                                                                    
134100     SKIP3                                                                
134200 IMS-DLET-WDD401   SECTION.                                               
134300                                                                          
134400     MOVE '  ' TO GODK-STATUSKODER                                        
134500     CALL CBLTDLI USING DLET WDD4-PCB DLI-IO-WDD401                       
134600     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
134700     PERFORM IMS-STATUSKONTROLL                                           
134800     .                                                                    
134900     EJECT                                                                
135000 IMS-GN-SEK-WDD401   SECTION.                                             
135100                                                                          
135200     STRING 'WDD401  (WDD4ASEQ>=' W-WDD4ASEQ-MIN-X                        
135300                    '&WDD4ASEQ<=' W-WDD4ASEQ-MAX-X                        
135400                    '&IDDC     =' W-IDDC-D4-X ')'                         
135500          DELIMITED BY SIZE INTO SSA1                                     
135600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
135700     CALL CBLTDLI USING GN  WDD4A-PCB DLI-IO-WDD401 SSA1                  
135800     MOVE WDD4A-STATUS-CODE TO STATUS-WS                                  
135900     PERFORM IMS-STATUSKONTROLL                                           
136000     .                                                                    
136100     SKIP3                                                                
136200 IMS-GU-SEK-WDD401   SECTION.                                             
136300                                                                          
136400     STRING 'WDD401  (WDD4ASEQ>=' W-WDD4ASEQ-MIN-X                        
136500                    '&WDD4ASEQ<=' W-WDD4ASEQ-MAX-X ')'                    
136600          DELIMITED BY SIZE INTO SSA1                                     
136700     MOVE '  GE' TO GODK-STATUSKODER                                      
136800     CALL CBLTDLI USING GU WDD4A-PCB DLI-IO-WDD401 SSA1                   
136900     MOVE WDD4A-STATUS-CODE TO STATUS-WS                                  
137000     PERFORM IMS-STATUSKONTROLL                                           
137100     .                                                                    
137200     EJECT                                                                
137300 IMS-GET-SEK-WDD401   SECTION.                                            
137400                                                                          
137500**** STRING 'WDD401  (WDD4ASEQ =' W-WDD4ASEQ-X ')'                        
137600****      DELIMITED BY SIZE INTO SSA1                                     
137700     STRING 'WDD401  (DAREGDAT =' W-DAREGDAT-9KOMPL-ASEQ-X                
137800                    '&TIKLOCK  =' W-TIKLOCK-9KOMPL-ASEQ-X ')'             
137900          DELIMITED BY SIZE INTO SSA1                                     
138000     MOVE '  GE' TO GODK-STATUSKODER                                      
138100     CALL CBLTDLI USING GU WDD4A-PCB DLI-IO-WDD401 SSA1                   
138200     MOVE WDD4A-STATUS-CODE TO STATUS-WS                                  
138300     PERFORM IMS-STATUSKONTROLL                                           
138400     .                                                                    
138500     EJECT                                                                
138600 IMS-INSERT-ALT SECTION.                                                  
138700     MOVE SPACE TO GODK-STATUSKODER                                       
138800     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
138900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
139000     PERFORM IMS-STATUSKONTROLL                                           
139100     .                                                                    
139200     EJECT                                                                
139300 IMS-GU-K601 SECTION.                                                     
139400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
139500          DELIMITED BY SIZE INTO SSA1                                     
139600*** GODK-STATUSKODER ARE SET BEFORE PERFORM OF THIS SECTION               
139700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
139800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
139900     PERFORM IMS-STATUSKONTROLL                                           
140000     .                                                                    
140100     SKIP3                                                                
140200 IMS-GNP-K611 SECTION.                                                    
140300     MOVE 'WDK611  '       TO SSA1                                        
140400*** GODK-STATUSKODER ARE SET BEFORE PERFORM OF THIS SECTION               
140500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
140600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900     EJECT                                                                
141000 IMS-GET-XXBX-2231 SECTION.                                               
141100                                                                          
141200     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
141300          DELIMITED BY SIZE INTO SSA1                                     
141400     MOVE '  ' TO GODK-STATUSKODER                                        
141500     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA-BX SSA1                   
141600     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
141700     PERFORM IMS-STATUSKONTROLL                                           
141800     .                                                                    
141900     EJECT                                                                
142000 IMS-GET-XXBX-2232 SECTION.                                               
142100                                                                          
142200     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
142300          DELIMITED BY SIZE INTO SSA1                                     
142400     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
142500          DELIMITED BY SIZE INTO SSA2                                     
142600     MOVE '  GE' TO GODK-STATUSKODER                                      
142700     CALL CBLTDLI USING GU  XXBX-PCB DLI-IO-AREA-BX SSA1 SSA2             
142800     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
142900     PERFORM IMS-STATUSKONTROLL                                           
143000     .                                                                    
143100     EJECT                                                                
143200 IMS-ISRT-WDR301 SECTION.                                                 
143300                                                                          
143400     STRING 'WDR301      '                                                
143500          DELIMITED BY SIZE INTO SSA1                                     
143600     MOVE '   ' TO GODK-STATUSKODER                                       
143700     CALL CBLTDLI USING ISRT WDR3-PCB DLI-IO-WDR301 SSA1                  
143800     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
143900     PERFORM IMS-STATUSKONTROLL                                           
144000     .                                                                    
144100                                                                          
144200                                                                          
144300 IMS-STATUSKONTROLL SECTION.                                              
144400                                                                          
144500     SET STATUS-IX TO 1                                                   
144600     SEARCH GODK-STATUS                                                   
144700       AT END                                                             
144800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
144900         DELIMITED BY SIZE INTO FELTEXT                                   
145000         CALL FELLOG                                                      
145100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
145200         CONTINUE                                                         
145300     END-SEARCH                                                           
145400     .                                                                    
