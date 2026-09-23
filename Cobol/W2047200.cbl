000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2047200.                                                
000300 AUTHOR.         G KJELLSON                                               
000400 DATE-WRITTEN.   AUGUSTI 2012                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR LARM (LEVERANSPRECISION)                                   
000900*        KOPIERAT FRÅN W2017200 FÖR ATT HANTERA KINA-LARM                 
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDD4                                       
001200*        PROGRAMMET LÄSER EV   WDK6                                       
001300*                              WDK7                                       
001400*                              WDR2                                       
001410*                              WDB6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2N472                                              
001800*        MID:         W2I47201                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O47201                                            
002200*                                                                         
002300*                                                                         
002400*   ÄNDRINGAR:                                                            
002500*                                                                         
002510*    2017-09-18    E'TRACKER 10302687 - LOCAL SOURCING                    
002530*                                                                         
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(08)   VALUE 'W2047200'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004420                                                                          
004430 77  W-KDLARM                    PIC X(3)    VALUE SPACE.                 
004450     88  GODK-LARM                           VALUE '220' '225'            
004460                                                   '230' '235'.           
004500                                                                          
004600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004700 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  MAX-IX                      PIC S9(4)  VALUE +15   COMP SYNC.        
004900 77  IDARTNR-WS                  PIC X(9).                                
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300     88  INDATA-OK                           VALUE 'J'.                   
005400     88  INDATA-FEL                          VALUE 'N'.                   
005500                                                                          
005600 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
005700     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
005800     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400 77  SW-DLET                     PIC X       VALUE 'N'.                   
006500 77  SW-HOPP-2402                PIC X(1)    VALUE 'N'.                   
006600 77  SW-SELECT                   PIC X(1)    VALUE 'N'.                   
006700                                                                          
006710 77  W-DC-OK                     PIC X(1)    VALUE SPACE.                 
006720                                                                          
006800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006900     88  EGEN-MID                            VALUE '2472'.                
007000     88  GODK-MID                            VALUE '2471' '2472'          
007100                                                   '2473' '2474'          
007200                                                   '2475' '2476'          
007300                                                   '2477' '2478'          
007400                                                   '2479'.                
007500     88  HELP-MID                            VALUE '0551'.                
007600     EJECT                                                                
007700*    --- ARBETSFÄLT                                                       
007800 01  ARBETSFAELT.                                                         
007900     03  WS-IDARTNR              PIC X(8)    VALUE SPACE.                 
008000     03  WS-IDARTNR-NUM REDEFINES WS-IDARTNR PIC 9(8).                    
008100     03  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                 
008200     03  WS-IDLEVNR-8            PIC X(8)    VALUE SPACE.                 
008300     03  WS-IDANSK               PIC X(3)    VALUE SPACE.                 
008410     03  WS-IDANSK-NUM           PIC 9(3)    VALUE ZERO.                  
008500     03  WS-KDLARM               PIC X(3)    VALUE SPACE.                 
008700     03  WDK6-IDLEVNR            PIC X(5)    VALUE SPACE.                 
008800     03  WS-TEORSLRM-220         PIC X(20)                                
008900                                 VALUE 'PREADVICE DEVIATION '.            
009000*****                            VALUE 'AVISERINGSDIFFERENS '.            
009100     03  WS-TEORSLRM-225         PIC X(20)                                
009200                                 VALUE 'NOT ARRIVED         '.            
009300*****                            VALUE 'EJ INLEVERERAD      '.            
009700     03  WS-TEORSLRM-230         PIC X(20)                                
009800                                 VALUE 'TOO EARLY           '.            
009900*****                            VALUE 'TIDIG INLEVERANS    '.            
010000     03  WS-TEORSLRM-235         PIC X(20)                                
010100                                 VALUE 'NOT SCHEDULED       '.            
010200*****                            VALUE 'AVROP SAKNAS        '.            
010300     03  WS-DAREGDAT             PIC 9(8).                                
010400     03  FILLER REDEFINES WS-DAREGDAT.                                    
010500         05  FILLER              PIC 9(2).                                
010600         05  WS-TIREGDAT         PIC 9(6).                                
010610                                                                          
010617     EJECT                                                                
010618 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
010619*01 -COPY WWIDFTG                                                         
010620     EJECT                                                                
010630*01  -COPY WWDCKONS                                                       
010700     EJECT                                                                
010800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010900 01  GENERELLA-SUBPROGRAM.                                                
011000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011600*01 -COPY WMEDAREA                                                        
011700     SKIP3                                                                
011800 01  MESSAGE-CODES.                                                       
011900     03  ERR-CORR-HILITE-FLDS          PIC X(3)    VALUE '001'.           
012000     03  INF-PRESS-PF11                PIC X(3)    VALUE '003'.           
012100     03  INF-FIRST-PAGE                PIC X(3)    VALUE '006'.           
012200     03  ERR-PF11-AND-NO-DATA          PIC X(3)    VALUE '011'.           
012300     03  ERR-PARTNO-MISSING            PIC X(3)    VALUE '017'.           
012400     03  INF-UPDATE-DONE               PIC X(3)    VALUE '101'.           
012500     03  INF-MORE-INFO-EXISTS          PIC X(3)    VALUE '105'.           
012600     03  ERR-WRONG-KEY                 PIC X(3)    VALUE '401'.           
012700     03  ERR-NOT-AUTHORIZED            PIC X(3)    VALUE '405'.           
012800     03  ERR-ONLY-DELETE-OK            PIC X(3)    VALUE '427'.           
012900     03  ERR-AUTHORIZE-CHECK-ACTIVATED PIC X(3)    VALUE '428'.           
013000     03  INF-ALERT-MISSING             PIC X(3)    VALUE '429'.           
013100     03  ERR-DELETE-NOT-ALLOWED        PIC X(3)    VALUE '777'.           
013200     EJECT                                                                
013300                                                                          
013400 01  SPECIAL-MEDDELANDEN.                                                 
013500     03  MED-1                   PIC X(40)   VALUE                        
013600         'Behörighetskontroll aktiverad '.                                
013700                                                                          
013800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013900*                                                                         
014000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014100     SKIP3                                                                
014200*01 -COPY WMSGINIT                                                        
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
014500*                                                                         
014600*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
014700*                                                                         
014800 01  SPAR-AREA.                                                           
014900     03  SPAR-IDTRANS              PIC X(4)  VALUE SPACE.                 
015000     03  SPAR-DAREGDAT-ENTER       PIC 9(8)  VALUE ZERO.                  
015100     03  SPAR-DAREGDAT-NEXT        PIC 9(8)  VALUE ZERO.                  
015200     03  SPAR-TIKLOCK-ENTER        PIC S9(9) VALUE ZERO COMP-3.           
015300     03  SPAR-TIKLOCK-NEXT         PIC S9(9) VALUE ZERO COMP-3.           
015400     03  SPAR-IDARTNR-MIN          PIC S9(9) VALUE ZERO COMP-3.           
015500     03  SPAR-IDARTNR-MAX          PIC S9(9) VALUE ZERO COMP-3.           
015600     03  SPAR-IDANSK-MIN           PIC S9(3) VALUE ZERO COMP-3.           
015700     03  SPAR-IDANSK-MAX           PIC S9(3) VALUE ZERO COMP-3.           
015800     03  SPAR-IDLEVNR-MIN          PIC X(5)  VALUE SPACE.                 
015900     03  SPAR-IDLEVNR-MAX          PIC X(5)  VALUE SPACE.                 
016000     03  SPAR-KDLARM-MIN           PIC S9(3) VALUE ZERO COMP-3.           
016100     03  SPAR-KDLARM-MAX           PIC S9(3) VALUE ZERO COMP-3.           
016110     03  SPAR-IDDC-MIN             PIC X(2)  VALUE SPACE.                 
016120     03  SPAR-IDDC-MAX             PIC X(2)  VALUE SPACE.                 
016200     03  SPAR-TAB.                                                        
016300         05  SPAR-RAD   OCCURS 15.                                        
016400             07  SPAR-RAD-DAREGDAT PIC 9(8).                              
016500             07  SPAR-RAD-TIKLOCK  PIC S9(9)        COMP-3.               
016600             07  SPAR-RAD-IDARTNR  PIC S9(9)        COMP-3.               
016610             07  SPAR-RAD-IDDC     PIC  X(2).                             
016700     03  SPAR-RAD-IX               PIC S9(3)        COMP-3.               
016800     EJECT                                                                
016900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017000*                                                                         
017100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017200     SKIP3                                                                
017300*01  MID -COPY W2I47201                                                   
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017600     SKIP3                                                                
017700*01  -COPY WMSGAREA                                                       
017800     EJECT                                                                
017900     03  MOD REDEFINES MSG-AREA.                                          
018000*      05  -COPY W2O47201                                                 
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018300     SKIP3                                                                
018400*01  -COPY WMFSAREA                                                       
018500     EJECT                                                                
018600 01  FILLER          PIC X(16) VALUE 'PROG-TO-PROG-SW'.                   
018700 01  W-PROG-TO-PROG-SW.                                                   
018800     03  P-WS-LL     PIC S9(4)  VALUE +50 COMP SYNC.                      
018900     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
019000     03  KDTRANS-WS  PIC X(8)   VALUE 'W2T402 '.                          
019100     03  P-IDTRANS   PIC X(4)   VALUE '2472'.                             
019200     03  P-KDMFSFOR  PIC X(1)   VALUE '2'.                                
019300*    03  MID   -COPY W2I40201     -PRE PROGSW-.                           
019400     EJECT                                                                
019500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019800     SKIP3                                                                
019900 01  NYCKLAR-TILL-DLI.                                                    
020000                                                                          
020100   03  W-IDARTNR-X.                                                       
020200     05  W-IDARTNR            PIC S9(9)   VALUE ZERO      COMP-3.         
020300   03 W-KDSEGKEY-X.                                                       
020400     05  W-KDSEGKEY           PIC X       VALUE '1'.                      
020410   03 W-IDDC-X.                                                           
020420     05  W-IDDC               PIC X(2)    VALUE SPACE.                    
020421   03 W-IDDC-MIN-X.                                                       
020422     05  FILLER               PIC X(1)    VALUE SPACE.                    
020423     05  W-IDDC2-MIN          PIC X(1)    VALUE SPACE.                    
020430   03 W-IDDC-MAX-X.                                                       
020440     05  FILLER               PIC X(1)    VALUE SPACE.                    
020450     05  W-IDDC2-MAX          PIC X(1)    VALUE SPACE.                    
020500                                                                          
020600   03  W-WDD4-MIN-X.                                                      
020700                                                                          
020800     05  W-IDARTNR-MIN-X.                                                 
020900         07  W-IDARTNR-MIN    PIC S9(9)   VALUE ZERO      COMP-3.         
021000     05  W-IDANSK-MIN-X.                                                  
021100         07  W-IDANSK-MIN     PIC S9(3)   VALUE ZERO      COMP-3.         
021200     05  W-IDLEVNR-MIN-X.                                                 
021300         07  W-IDLEVNR-MIN    PIC X(5)    VALUE SPACE.                    
021400     05  W-KDLARM-MIN-X.                                                  
021500         07  W-KDLARM-MIN     PIC S9(3)   VALUE ZERO      COMP-3.         
021600                                                                          
021700   03  W-WDD4-MAX-X.                                                      
021800                                                                          
021900     05  W-IDARTNR-MAX-X.                                                 
022000         07  W-IDARTNR-MAX    PIC S9(9)   VALUE 99999999  COMP-3.         
022100     05  W-IDANSK-MAX-X.                                                  
022200         07  W-IDANSK-MAX     PIC S9(3)   VALUE 999       COMP-3.         
022300     05  W-IDLEVNR-MAX-X.                                                 
022400         07  W-IDLEVNR-MAX    PIC X(5)    VALUE '99999'.                  
022500     05  W-KDLARM-MAX-X.                                                  
022600         07  W-KDLARM-MAX     PIC S9(3)   VALUE 999       COMP-3.         
022700****                                                                      
022800   03  W-WDD4ASEQ-X.                                                      
022900                                                                          
023000     05  W-IDANSK-ASEQ-X.                                                 
023100         07  W-IDANSK-ASEQ    PIC S9(3)   VALUE ZERO      COMP-3.         
023200     05  W-IDLEVNR-ASEQ-X.                                                
023300         07  W-IDLEVNR-ASEQ   PIC X(5)    VALUE SPACE.                    
023400     05  W-IDARTNR-ASEQ-X.                                                
023500         07  W-IDARTNR-ASEQ   PIC S9(9)   VALUE ZERO      COMP-3.         
023600     05  W-DAREGDAT-9KOMPL-ASEQ-X.                                        
023700         07  W-DAREGDAT-9KOMPL-ASEQ PIC 9(08)  VALUE ZERO.                
023800     05  W-TIKLOCK-9KOMPL-ASEQ-X.                                         
023900         07  W-TIKLOCK-9KOMPL-ASEQ  PIC S9(9)  VALUE ZERO COMP-3.         
024000****                                                                      
024100   03  W-WDD4ASEQ-MIN-X.                                                  
024200                                                                          
024300     05  W-IDANSK-ASEQ-MIN-X.                                             
024400         07  W-IDANSK-ASEQ-MIN     PIC S9(3)   VALUE ZERO COMP-3.         
024500     05  W-IDLEVNR-ASEQ-MIN-X.                                            
024600         07  W-IDLEVNR-ASEQ-MIN    PIC X(5)    VALUE SPACE.               
024700     05  W-IDARTNR-ASEQ-MIN-X.                                            
024800         07  W-IDARTNR-ASEQ-MIN    PIC S9(9)   VALUE ZERO COMP-3.         
024900     05  W-DAREGDAT-9KOMPL-ASEQ-MIN-X.                                    
025000         07  W-DAREGDAT-9KOMPL-ASEQ-MIN PIC 9(08)    VALUE ZERO.          
025100     05  W-TIKLOCK-9KOMPL-ASEQ-MIN-X.                                     
025200        07  W-TIKLOCK-9KOMPL-ASEQ-MIN PIC S9(9) VALUE ZERO COMP-3.        
025330                                                                          
025400   03  W-WDD4ASEQ-MAX-X.                                                  
025500                                                                          
025600     05  W-IDANSK-ASEQ-MAX-X.                                             
025700         07  W-IDANSK-ASEQ-MAX PIC S9(3)       VALUE 999  COMP-3.         
025800     05  W-IDLEVNR-ASEQ-MAX-X.                                            
025900         07  W-IDLEVNR-ASEQ-MAX    PIC X(5)    VALUE '99999'.             
026000     05  W-IDARTNR-ASEQ-MAX-X.                                            
026100         07  W-IDARTNR-ASEQ-MAX  PIC S9(9) VALUE 99999999  COMP-3.        
026200     05  W-DAREGDAT-9KOMPL-ASEQ-MAX-X.                                    
026300         07  W-DAREGDAT-9KOMPL-ASEQ-MAX PIC 9(08) VALUE 99999999.         
026400     05  W-TIKLOCK-9KOMPL-ASEQ-MAX-X.                                     
026500         07 W-TIKLOCK-9KOMPL-ASEQ-MAX PIC S9(9)                           
026600                                           VALUE 999999999 COMP-3.        
026800                                                                          
026900*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
027000     03  W-WDD401KY-X.                                                    
027100         05  W-DAREGDAT-9KOMPL   PIC 9(08)    VALUE ZERO.                 
027200         05  W-TIKLOCK-9KOMPL    PIC S9(9)    VALUE ZERO COMP-3.          
027300                                                                          
027400     03  W-WDGXKEY-2231-X.                                                
027500         05  FILLER              PIC X(4)     VALUE '2231'.               
027600         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
027700     03  W-WDGXKEY-2232-X.                                                
027800         05  W-IDANSK-L          PIC S9(3)    VALUE ZERO COMP-3.          
027900         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
028000                                                                          
028100     SKIP2                                                                
028200*    --- STATUS-KOD FRÅN IMS                                              
028300 01  STATUS-WS                   PIC XX.                                  
028400     88  SEGMENT-FINNS                       VALUE '  '.                  
028500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028600     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
028700     SKIP2                                                                
028800 01  GODK-STATUSKODER.                                                    
028900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029000     SKIP3                                                                
029100 01  ALL-SSA.                                                             
029110     03 SSA1                     PIC X(500).                              
029200     03 SSA2                     PIC X(500).                              
029210     03 SSA3                     PIC X(500).                              
029300     EJECT                                                                
029400*    --- IMS FUNKTIONSKODER                                               
029500*01  -COPY W0003                                                          
029600     EJECT                                                                
029700*    ---  DLI INPUT-OUTPUT AREA                                           
029800                                                                          
029900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401'.                      
030000 01  DLI-IO-WDD401.                                                       
030100*    03  -COPY WDD401                                                     
030200     EJECT                                                                
030300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
030400     SKIP3                                                                
030500 01  DLI-IO-WDK601.                                                       
030600*    03  -COPY WDK601                                                     
030700     EJECT                                                                
030800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
030900     SKIP3                                                                
031000 01  DLI-IO-WDK611.                                                       
031100*    03  -COPY WDK611                                                     
031200     EJECT                                                                
031210 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
031220     SKIP3                                                                
031230 01  DLI-IO-WDK701.                                                       
031240*    03  -COPY WDK701                                                     
031250     EJECT                                                                
031260 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
031270     SKIP3                                                                
031280 01  DLI-IO-WDK711.                                                       
031290*    03  -COPY WDK711                                                     
031291     EJECT                                                                
031292 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK722'.             
031293     SKIP3                                                                
031294 01  DLI-IO-WDK722.                                                       
031295*    03  -COPY WDK722                                                     
031296     EJECT                                                                
031300 01  DLI-IO-AREA-WDR2.                                                    
031400     03  IO-AREA-WDR2            PIC X(50)  VALUE SPACE.                  
031500     SKIP3                                                                
031600     03  WD3201 REDEFINES IO-AREA-WDR2.                                   
031700*        05  -COPY WDGX01     -PRE WDR2-                                  
031800     SKIP3                                                                
031900     03  WDR220 REDEFINES IO-AREA-WDR2.                                   
032000*        05  -COPY WDGX2232   -PRE WDR2-                                  
032100     EJECT                                                                
032110 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
032120     SKIP3                                                                
032130 01  DLI-IO-WDB601.                                                       
032140*    03  -COPY WDB601                                                     
032200 LINKAGE SECTION.                                                         
032300*01  -COPY W0009   -PRE MSG-                                              
032400                                                                          
032500*01  -COPY W0009   -PRE ALT-                                              
032600                                                                          
032700*01  -COPY W0008   -PRE USEA-                                             
032800     05  FILLER                  PIC X.                                   
032900                                                                          
033000*01  -COPY W0008  -PRE WDD4-                                              
033100     05  FILLER                  PIC X.                                   
033200                                                                          
033300*01  -COPY W0008  -PRE WDK6-                                              
033400     05  FILLER                  PIC X.                                   
033500                                                                          
033510*01  -COPY W0008  -PRE WDK7-                                              
033520     05  FILLER                  PIC X.                                   
033530                                                                          
033600*01  -COPY W0008  -PRE WDR2-                                              
033700     05  FILLER                  PIC X.                                   
033800                                                                          
033900*01  -COPY W0008  -PRE WDD4A-                                             
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034110*01  -COPY W0008  -PRE WDB6-                                              
034120     05  FILLER                  PIC X.                                   
034130     EJECT                                                                
034140*01  -COPY W0008  -PRE WDB6A-                                             
034150     05  FILLER                  PIC X.                                   
034160     EJECT                                                                
034200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDD4-PCB              
034300                                                    WDK6-PCB              
034310                                                    WDK7-PCB              
034400                                                    WDR2-PCB              
034500                                                    WDD4A-PCB             
034510                                                    WDB6-PCB              
034520                                                    WDB6A-PCB.            
034600 MAIN SECTION.                                                            
034700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDD4-PCB              
034800                                                    WDK6-PCB              
034810                                                    WDK7-PCB              
034900                                                    WDR2-PCB              
034910                                                    WDD4A-PCB             
034920                                                    WDB6-PCB              
034930                                                    WDB6A-PCB.            
035100                                                                          
035200     PERFORM IMS-GET-MSG                                                  
035300     IF SEGMENT-FINNS                                                     
035400       PERFORM A-INIT                                                     
035500       PERFORM B-KOLLA-NYCKLAR                                            
035600       IF NYCKLAR-OK                                                      
035700                                                                          
035800           PERFORM G-KOLLA-INPUT                                          
035900                                                                          
036000           IF MFS-UPDATE OR MFS-SPLIT                                     
036100             IF INDATA-OK                                                 
036200                IF MFS-UPDATE                                             
036300                   PERFORM H-UPPDATERA                                    
036400                END-IF                                                    
036500                IF MFS-SPLIT                                              
036600                   PERFORM K-PPSW-2402                                    
036700                END-IF                                                    
036800             END-IF                                                       
036900           ELSE                                                           
037000             IF MFS-FIRST                                                 
037100               PERFORM C-FOERSTA-SIDA                                     
037200             ELSE                                                         
037300               IF MFS-NEXT                                                
037400                 PERFORM D-NAESTA-SIDA                                    
037500               ELSE                                                       
037600                 PERFORM E-SAMMA-SIDA                                     
037700               END-IF                                                     
037800             END-IF                                                       
037900           END-IF                                                         
038000                                                                          
038100           IF MFS-ENTER AND SW-SELECT = JA                                
038200              CONTINUE                                                    
038300           ELSE                                                           
038400              PERFORM F-LAES-VISA-INFO                                    
038500           END-IF                                                         
038600                                                                          
038700       END-IF                                                             
038800*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
038900*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
039000       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O47201 + 4                      
039100       PERFORM IMS-INSERT-MSG                                             
039200     END-IF                                                               
039300                                                                          
039400     MOVE ZERO TO RETURN-CODE                                             
039500     GOBACK                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 A-INIT SECTION.                                                          
039900                                                                          
040000     IF MSG-DUBBLA-TRANSKODER                                             
040100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I47201                 
040200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
040300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
040400     ELSE                                                                 
040500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I47201                  
040600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
040700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
040800     END-IF                                                               
040900                                                                          
041000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
041100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
041200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
041300                                                                          
041400     MOVE LOW-VALUE TO MSG-AREA                                           
041500     MOVE 'W2O472N1' TO MFS-IDMOD                                         
041600     MOVE '2472' TO MOD-IDTRANS                                           
041700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
041800     MOVE 'GB'            TO MED-IDSKYLT                                  
041900                                                                          
042000     IF EGEN-MID OR HELP-MID                                              
042100       CONTINUE                                                           
042200     ELSE                                                                 
042300       MOVE SPACE TO MFS-KDTRTYP                                          
042400       MOVE '7' TO MFS-IDPFK                                              
042500     END-IF                                                               
042600                                                                          
042700     MOVE NEJ                    TO SW-SELECT                             
042800     MOVE NEJ                    TO SW-HOPP-2402                          
042900     IF MFS-IDPFK       = '9'                                             
043000        MOVE JA                  TO SW-HOPP-2402                          
043100        MOVE LOW-VALUE           TO PROGSW-MID-W2I40201                   
043200     END-IF                                                               
043210                                                                          
043220     MOVE LOW-VALUE   TO W-WDD4ASEQ-MIN-X                                 
043230     MOVE HIGH-VALUE  TO W-WDD4ASEQ-MAX-X                                 
043300     .                                                                    
043400     EJECT                                                                
043500 B-KOLLA-NYCKLAR SECTION.                                                 
043600                                                                          
043900     PERFORM BA-KOLLA-NYA-NYCKLAR                                         
044000                                                                          
044100     MOVE ALL '+'             TO MSGI-WMSGINIT                            
044200     MOVE '001'               TO MSGI-KDCALL                              
044300     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
044400     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
044500     MOVE '2472'              TO MSGI-IDTRANS                             
044600     IF EGEN-MID                                                          
044702        IF MID-IDLEVNR-IN NOT = ALL '+'                                   
044703          MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                             
044704        END-IF                                                            
044710        IF MID-IDANSK-IN NOT = ALL '+'                                    
044800          MOVE MID-IDANSK-IN  TO MSGI-IDPERSON                            
044900          MOVE 'ANSK'         TO MSGI-KDARBTYP                            
044910        END-IF                                                            
044911        IF MID-KDLARM-IN NOT = ALL '+'                                    
044912          MOVE MID-KDLARM-IN  TO MSGI-KDLARM                              
044914        END-IF                                                            
044915        IF MID-IDARTNR-IN NOT = ALL '+'                                   
044916          MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                             
044917        END-IF                                                            
044920        IF MID-IDDC-IN NOT = ALL '+'                                      
045000          MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                            
045010        END-IF                                                            
045200     ELSE                                                                 
045300        MOVE ZERO             TO MSGI-IDARTNR                             
045400        MOVE '7'              TO MFS-IDPFK                                
045500        MOVE SPACE            TO MFS-KDTRTYP                              
045600     END-IF                                                               
045700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
045900                                                                          
046000*    - LANGUAGE TO BE USED BY MEDKONV                                     
046100     MOVE MSGI-IDLAND-SPR     TO MED-IDSKYLT                              
046200                                                                          
046300     MOVE JA  TO NYCKLAR-SW                                               
046400                                                                          
046500     PERFORM BB-CHECK-IDARTNR                                             
046600     PERFORM BC-CHECK-IDDC                                                
046700     PERFORM BD-CHECK-IDANSK                                              
046800     PERFORM BE-CHECK-IDLEVNR                                             
046900     PERFORM BF-CHECK-KDLARM                                              
047000     PERFORM BG-CHECK-KEY-COMBINATION                                     
047100                                                                          
047200                                                                          
047300     IF GODK-MID OR NYCKLAR-OK                                            
047400       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
047500       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
047600       MOVE MSGI-IDPERSON    TO MOD-IDANSK-UT                             
047700       MOVE MSGI-IDLEVNR     TO MOD-IDLEVNR-UT                            
047810       MOVE MSGI-KDLARM      TO MOD-KDLARM-UT                             
048000       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
048100       INSPECT MOD-IDANSK-UT  REPLACING LEADING ZERO BY SPACE             
048200       INSPECT MOD-KDLARM-UT  REPLACING LEADING ZERO BY SPACE             
048300       INSPECT MOD-IDLEVNR-UT REPLACING LEADING ZERO BY SPACE             
048310       INSPECT MOD-IDDC-UT    REPLACING LEADING ZERO BY SPACE             
048400     ELSE                                                                 
048500       MOVE MFS-ERASE-FIELD  TO MOD-IDARTNR-UT                            
048600       MOVE MFS-ERASE-FIELD  TO MOD-STRECK                                
048700       MOVE MFS-ERASE-FIELD  TO MOD-REKSIFFR                              
048800       MOVE MFS-ERASE-FIELD  TO MOD-IDDC-UT                               
048900       MOVE MFS-ERASE-FIELD  TO MOD-IDANSK-UT                             
049000       MOVE MFS-ERASE-FIELD  TO MOD-IDLEVNR-UT                            
049100       MOVE MFS-ERASE-FIELD  TO MOD-KDLARM-UT                             
049200     END-IF                                                               
049300                                                                          
049400     IF NYCKLAR-FEL                                                       
049500       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
049600       CALL WMEDKONV USING MED-WMEDAREA                                   
049700       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
049800       PERFORM MFS-RENSA-FAELT-IN                                         
049900       PERFORM MFS-RENSA-FAELT-UT                                         
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 BA-KOLLA-NYA-NYCKLAR SECTION.                                            
050400                                                                          
050700     IF  MID-IDARTNR-IN = ALL '+'                                         
050800     AND MID-IDDC-IN    = ALL '+'                                         
050900     AND MID-IDLEVNR-IN = ALL '+'                                         
051000     AND MID-IDANSK-IN  = ALL '+'                                         
051100     AND MID-KDLARM-IN  = ALL '+'                                         
051200        CONTINUE                                                          
051300     ELSE                                                                 
051400        MOVE ZERO    TO MID-IDARTNR-UT                                    
051500        MOVE SPACE   TO MID-IDDC-UT                                       
051600        MOVE SPACE   TO MID-IDLEVNR-UT                                    
051700        MOVE ZERO    TO MID-IDANSK-UT                                     
051800        MOVE ZERO    TO MID-KDLARM-UT                                     
051900        MOVE '7'     TO MFS-IDPFK                                         
052000        MOVE SPACE   TO MFS-KDTRTYP                                       
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400 BB-CHECK-IDARTNR SECTION.                                                
052700                                                                          
052800     MOVE MFS-ERASE-FIELD     TO MOD-IDARTNR-IN                           
052900                                                                          
053000     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
053100     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
053200*    ARTIKEL GIVEN PÅ BILDEN                                              
053300        MOVE MSGI-IDARTNR             TO W-IDARTNR                        
053400        PERFORM IMS-GU-K701                                               
053500        IF SEGMENT-FINNS                                                  
053600           PERFORM IMS-GU-K601                                            
053700           IF SEGMENT-FINNS                                               
053800              MOVE '-'                TO MOD-STRECK                       
053900              MOVE ART-REKSIFFR       TO MOD-REKSIFFR                     
054000           ELSE                                                           
054100              MOVE ERR-PARTNO-MISSING TO MED-IDMFSFEL                     
054200              MOVE NEJ                TO NYCKLAR-SW                       
054400           END-IF                                                         
054500        ELSE                                                              
054600           MOVE ERR-PARTNO-MISSING    TO MED-IDMFSFEL                     
054700           MOVE NEJ                   TO NYCKLAR-SW                       
054900        END-IF                                                            
055000     ELSE                                                                 
055100        MOVE ZERO                     TO W-IDARTNR                        
055200     END-IF                                                               
055210                                                                          
055220     IF W-IDARTNR = ZERO                                                  
055230       MOVE ZERO      TO W-IDARTNR-MIN                                    
055260       MOVE 999999999 TO W-IDARTNR-MAX                                    
055290     ELSE                                                                 
055291       MOVE W-IDARTNR TO W-IDARTNR-MIN                                    
055292                         W-IDARTNR-ASEQ                                   
055293                         W-IDARTNR-ASEQ-MIN                               
055294                         W-IDARTNR-MAX                                    
055296                         W-IDARTNR-ASEQ-MAX                               
055297     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500 BC-CHECK-IDDC    SECTION.                                                
055800                                                                          
055900     MOVE MFS-ERASE-FIELD     TO MOD-IDDC-IN                              
057300                                                                          
057310     IF MSGI-IDDC-KEY = ALL '+' OR SPACE                                  
057320        MOVE MSGI-IDFTG        TO WS-IDFTG                                
057330        IF IDFTG-US                                                       
057340           MOVE '41'           TO W-IDDC-MIN-X                            
057350           MOVE '49'           TO W-IDDC-MAX-X                            
057351           MOVE '40'           TO MSGI-IDDC-KEY                           
057360        ELSE                                                              
057370           IF IDFTG-CN                                                    
057371              MOVE '71'        TO W-IDDC-MIN-X                            
057372              MOVE '79'        TO W-IDDC-MAX-X                            
057373              MOVE '70'        TO MSGI-IDDC-KEY                           
057391           ELSE                                                           
057392              MOVE NEJ         TO NYCKLAR-SW                              
057394           END-IF                                                         
057395        END-IF                                                            
057396     ELSE                                                                 
057397        MOVE MSGI-IDDC-KEY    TO W-IDDC-MIN-X                             
057398                                 W-IDDC-MAX-X                             
057399        IF MSGI-IDDC-KEY(2:1) = '0'                                       
057400           MOVE '1'           TO W-IDDC2-MIN                              
057401           MOVE '9'           TO W-IDDC2-MAX                              
057402        END-IF                                                            
057403     END-IF                                                               
057404                                                                          
057407     MOVE NEJ                 TO W-DC-OK                                  
057408     PERFORM IMS-GU-WDB601-MIN-MAX                                        
057409     PERFORM UNTIL SEGMENT-SAKNAS                                         
057410                OR W-DC-OK = JA                                           
057412        IF DCS-NDC-CN                                                     
057413        OR (DCS-NDC-NA AND DCS-USA)                                       
057414           MOVE JA            TO W-DC-OK                                  
057415        END-IF                                                            
057416        PERFORM IMS-GN-WDB601-MIN-MAX                                     
057417     END-PERFORM                                                          
057418                                                                          
057419     IF W-DC-OK = NEJ                                                     
057420        MOVE NEJ              TO NYCKLAR-SW                               
057421     END-IF                                                               
057430     .                                                                    
057500     EJECT                                                                
057600 BD-CHECK-IDANSK  SECTION.                                                
058100                                                                          
058200     MOVE MFS-ERASE-FIELD TO MOD-IDANSK-IN                                
058300                                                                          
059202     MOVE MSGI-IDPERSON  TO WS-IDANSK                                     
059203     INSPECT WS-IDANSK  REPLACING LEADING SPACE BY ZERO                   
059204                                                                          
059210     IF WS-IDANSK NOT NUMERIC                                             
059211        MOVE NEJ         TO NYCKLAR-SW                                    
059213     ELSE                                                                 
059215        IF WS-IDANSK = ALL ZERO                                           
059220          MOVE ZERO      TO W-IDANSK-MIN                                  
059250          MOVE 999       TO W-IDANSK-MAX                                  
059280        ELSE                                                              
059290          MOVE WS-IDANSK TO W-IDANSK-MIN                                  
059291                            W-IDANSK-ASEQ                                 
059292                            W-IDANSK-ASEQ-MIN                             
059293                            W-IDANSK-MAX                                  
059294                            W-IDANSK-ASEQ-MAX                             
059295        END-IF                                                            
059296     END-IF                                                               
059300     .                                                                    
059400     EJECT                                                                
059500 BE-CHECK-IDLEVNR SECTION.                                                
059800                                                                          
059900     MOVE MFS-ERASE-FIELD     TO MOD-IDLEVNR-IN                           
060000                                                                          
060100     MOVE MSGI-IDLEVNR        TO WS-IDLEVNR                               
060110     IF WS-IDLEVNR = ZERO or SPACE                                        
060120       MOVE SPACE      TO W-IDLEVNR-MIN                                   
060150       MOVE '99999'    TO W-IDLEVNR-MAX                                   
060180     ELSE                                                                 
060190       MOVE WS-IDLEVNR TO W-IDLEVNR-MIN                                   
060191                          W-IDLEVNR-ASEQ                                  
060192                          W-IDLEVNR-ASEQ-MIN                              
060193                          W-IDLEVNR-MAX                                   
060194                          W-IDLEVNR-ASEQ-MAX                              
060195     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 BF-CHECK-KDLARM  SECTION.                                                
060700                                                                          
060800     MOVE MFS-ERASE-FIELD     TO MOD-KDLARM-IN                            
060801                                                                          
060804     MOVE MSGI-KDLARM         TO WS-KDLARM                                
060806     INSPECT WS-KDLARM  REPLACING LEADING SPACE BY ZERO                   
060813     IF WS-KDLARM = ALL ZERO                                              
060814       MOVE ZERO              TO W-KDLARM-MIN                             
060815       MOVE 999               TO W-KDLARM-MAX                             
060816     ELSE                                                                 
060817       MOVE WS-KDLARM         TO W-KDLARM                                 
060818       IF GODK-LARM                                                       
060819          MOVE WS-KDLARM      TO W-KDLARM-MIN                             
060820                                 W-KDLARM-MAX                             
060821       ELSE                                                               
060822          MOVE NEJ            TO NYCKLAR-SW                               
060824       END-IF                                                             
060825     END-IF                                                               
060910     .                                                                    
060911     EJECT                                                                
060912 BG-CHECK-KEY-COMBINATION SECTION.                                        
060913                                                                          
060914     IF  W-IDARTNR  = ZERO                                                
060915     AND WS-IDANSK  = ZERO                                                
060916     AND WS-IDLEVNR = ZERO                                                
060917     AND WS-KDLARM  = ZERO                                                
060919        MOVE NEJ     TO NYCKLAR-SW                                        
060946     END-IF                                                               
060950     .                                                                    
061000     EJECT                                                                
061100 BA-FINNS-IDARTNR-IDANSK SECTION.                                         
061200                                                                          
061300     IF MSGI-IDARTNR NUMERIC                                              
061400        MOVE MSGI-IDARTNR   TO W-IDARTNR                                  
061500        PERFORM IMS-GU-K701                                               
061501        IF SEGMENT-FINNS                                                  
061510           PERFORM IMS-GU-K601                                            
061600           IF SEGMENT-FINNS                                               
061610              MOVE '-'            TO MOD-STRECK                           
061620              MOVE ART-REKSIFFR   TO MOD-REKSIFFR                         
061700              MOVE ART-IDLEVNR TO WDK6-IDLEVNR                            
061800              PERFORM IMS-GU-K722                                         
061900              IF SEGMENT-FINNS                                            
062000                 MOVE XLAG-IDANSK TO WS-IDANSK-NUM                        
062100                                     W-IDANSK-L                           
062200                 PERFORM IMS-GET-WDR2-2232                                
062300                 IF SEGMENT-FINNS                                         
062400                   MOVE WDR2-2232-IDANSK-LARM                             
062500                                  TO WS-IDANSK-NUM                        
062600                 END-IF                                                   
062700              END-IF                                                      
062800           END-IF                                                         
062810        END-IF                                                            
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200 C-FOERSTA-SIDA SECTION.                                                  
063300                                                                          
063400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
063500     CALL WMEDKONV    USING MED-WMEDAREA                                  
063600     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
063700                                                                          
063800     PERFORM MFS-RENSA-FAELT-IN                                           
063900     .                                                                    
064000     EJECT                                                                
064100 D-NAESTA-SIDA SECTION.                                                   
064200                                                                          
064300     IF SPAR-IDTRANS = '2472'                                             
064400       MOVE SPAR-IDANSK-MIN    TO W-IDANSK-MIN                            
064500                                  W-IDANSK-ASEQ-MIN                       
064600                                  W-IDANSK-ASEQ                           
064700       MOVE SPAR-IDLEVNR-MIN   TO W-IDLEVNR-MIN                           
064800                                  W-IDLEVNR-ASEQ-MIN                      
064900                                  W-IDLEVNR-ASEQ                          
065000       MOVE SPAR-IDARTNR-MIN   TO W-IDARTNR-MIN                           
065100                                  W-IDARTNR-ASEQ-MIN                      
065200                                  W-IDARTNR-ASEQ                          
065210       MOVE SPAR-IDDC-MIN      TO W-IDDC-MIN-X                            
065300       MOVE SPAR-DAREGDAT-NEXT TO W-DAREGDAT-9KOMPL                       
065400                                  W-DAREGDAT-9KOMPL-ASEQ                  
065500                                  W-DAREGDAT-9KOMPL-ASEQ-MIN              
065600       MOVE SPAR-TIKLOCK-NEXT  TO W-TIKLOCK-9KOMPL                        
065700                                  W-TIKLOCK-9KOMPL-ASEQ                   
065800                                  W-TIKLOCK-9KOMPL-ASEQ-MIN               
065900       MOVE SPAR-KDLARM-MIN    TO W-KDLARM-MIN                            
066000       MOVE SPAR-IDARTNR-MAX   TO W-IDARTNR-MAX                           
066100       MOVE SPAR-IDANSK-MAX    TO W-IDANSK-MAX                            
066200       MOVE SPAR-IDLEVNR-MAX   TO W-IDLEVNR-MAX                           
066300       MOVE SPAR-KDLARM-MAX    TO W-KDLARM-MAX                            
066310       MOVE SPAR-IDDC-MAX      TO W-IDDC-MAX-X                            
066400     ELSE                                                                 
066500       PERFORM MFS-RENSA-FAELT-IN                                         
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 E-SAMMA-SIDA SECTION.                                                    
067000                                                                          
067100     IF SPAR-IDTRANS = '2472' OR '0551'                                   
067200       MOVE SPAR-IDANSK-MIN     TO W-IDANSK-MIN                           
067300                                   W-IDANSK-ASEQ-MIN                      
067400                                   W-IDANSK-ASEQ                          
067500       MOVE SPAR-IDLEVNR-MIN    TO W-IDLEVNR-MIN                          
067600                                   W-IDLEVNR-ASEQ-MIN                     
067700                                   W-IDLEVNR-ASEQ                         
067800       MOVE SPAR-IDARTNR-MIN    TO W-IDARTNR-MIN                          
067900                                   W-IDARTNR-ASEQ-MIN                     
068000                                   W-IDARTNR-ASEQ                         
068010       MOVE SPAR-IDDC-MIN       TO W-IDDC-MIN-X                           
068100       MOVE SPAR-DAREGDAT-ENTER TO W-DAREGDAT-9KOMPL                      
068200                                   W-DAREGDAT-9KOMPL-ASEQ                 
068300                                   W-DAREGDAT-9KOMPL-ASEQ-MIN             
068400       MOVE SPAR-TIKLOCK-ENTER  TO W-TIKLOCK-9KOMPL                       
068500                                   W-TIKLOCK-9KOMPL-ASEQ                  
068600                                   W-TIKLOCK-9KOMPL-ASEQ-MIN              
068700       MOVE SPAR-KDLARM-MIN     TO W-KDLARM-MIN                           
068800       MOVE SPAR-IDARTNR-MAX    TO W-IDARTNR-MAX                          
068900       MOVE SPAR-IDANSK-MAX     TO W-IDANSK-MAX                           
069000       MOVE SPAR-IDLEVNR-MAX    TO W-IDLEVNR-MAX                          
069100       MOVE SPAR-KDLARM-MAX     TO W-KDLARM-MAX                           
069110       MOVE SPAR-IDDC-MAX       TO W-IDDC-MAX-X                           
069210       IF MID-SELECT-ARTIKEL(01) = ALL '+'                                
069220      AND MID-SELECT-ARTIKEL(02) = ALL '+'                                
069230      AND MID-SELECT-ARTIKEL(03) = ALL '+'                                
069240      AND MID-SELECT-ARTIKEL(04) = ALL '+'                                
069250      AND MID-SELECT-ARTIKEL(05) = ALL '+'                                
069260      AND MID-SELECT-ARTIKEL(06) = ALL '+'                                
069270      AND MID-SELECT-ARTIKEL(07) = ALL '+'                                
069280      AND MID-SELECT-ARTIKEL(08) = ALL '+'                                
069290      AND MID-SELECT-ARTIKEL(09) = ALL '+'                                
069291      AND MID-SELECT-ARTIKEL(10) = ALL '+'                                
069292      AND MID-SELECT-ARTIKEL(11) = ALL '+'                                
069293      AND MID-SELECT-ARTIKEL(12) = ALL '+'                                
069294      AND MID-SELECT-ARTIKEL(13) = ALL '+'                                
069295      AND MID-SELECT-ARTIKEL(14) = ALL '+'                                
069296      AND MID-SELECT-ARTIKEL(15) = ALL '+'                                
069300         PERFORM MFS-RENSA-FAELT-IN                                       
069400       ELSE                                                               
069500         MOVE JA             TO SW-SELECT                                 
069600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
069700         CALL WMEDKONV USING MED-WMEDAREA                                 
069800         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
069900         PERFORM EA-MID-INDATA-TILL-MOD                                   
070000       END-IF                                                             
070100     ELSE                                                                 
070200       PERFORM MFS-RENSA-FAELT-IN                                         
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 EA-MID-INDATA-TILL-MOD SECTION.                                          
070700                                                                          
070800* * * * * FÖR VARJE MID-FÄLT                                              
070900* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
071000* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
071100* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
071200     MOVE +1 TO IX                                                        
071300     PERFORM UNTIL IX > MAX-IX                                            
071400        IF MID-SELECT-ARTIKEL (IX) NOT = ALL '+'                          
071500          MOVE MID-SELECT-ARTIKEL (IX)   TO                               
071600               MOD-SELECT-ARTIKEL (IX)                                    
071700          MOVE MFS-ADD-LAES-IN-FAELT     TO                               
071800               MOD-SELECT-ARTIKEL-ATTR (IX)                               
071900        ELSE                                                              
072000          MOVE MFS-RENSA-FAELT           TO MOD-SELECT-ARTIKEL(IX)        
072100        END-IF                                                            
072200        ADD +1 TO IX                                                      
072300     END-PERFORM                                                          
072400     .                                                                    
072500     EJECT                                                                
072600 F-LAES-VISA-INFO SECTION.                                                
072700                                                                          
072800     IF WS-IDANSK NUMERIC  AND WS-IDANSK-NUM  > ZERO AND                  
072900        WS-IDLEVNR > SPACE                                                
073000***     IDANSK OCH IDLEVNR IFYLLT > NOLL / SPACE                          
073100        PERFORM FA-LAS-SEK-INDEX                                          
073200     ELSE                                                                 
073300                                                                          
073400       MOVE +1 TO IX                                                      
073500       IF (MFS-FIRST OR MFS-SPLIT OR MFS-UPDATE) AND                      
073600          SW-DLET = 'N'                                                   
073700          PERFORM IMS-GET-WDD401                                          
073800       ELSE                                                               
073900          IF SW-DLET = 'J'                                                
074000             PERFORM IMS-GU-WDD401                                        
074100          ELSE                                                            
074200             PERFORM IMS-GHU-WDD401                                       
074300          END-IF                                                          
074400       END-IF                                                             
074500                                                                          
074600       IF SEGMENT-FINNS                                                   
074700         MOVE LAK-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-ENTER                  
074800         MOVE LAK-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-ENTER                   
074900       ELSE                                                               
075000         MOVE ZERO             TO SPAR-DAREGDAT-ENTER                     
075100         MOVE ZERO             TO SPAR-TIKLOCK-ENTER                      
075200       END-IF                                                             
075300       MOVE W-IDARTNR-MIN      TO SPAR-IDARTNR-MIN                        
075400       MOVE W-IDANSK-MIN       TO SPAR-IDANSK-MIN                         
075500       MOVE W-IDLEVNR-MIN      TO SPAR-IDLEVNR-MIN                        
075600       MOVE W-KDLARM-MIN       TO SPAR-KDLARM-MIN                         
075610       MOVE W-IDDC-MIN-X       TO SPAR-IDDC-MIN                           
075700       MOVE W-IDARTNR-MAX      TO SPAR-IDARTNR-MAX                        
075800       MOVE W-IDANSK-MAX       TO SPAR-IDANSK-MAX                         
075900       MOVE W-IDLEVNR-MAX      TO SPAR-IDLEVNR-MAX                        
076000       MOVE W-KDLARM-MAX       TO SPAR-KDLARM-MAX                         
076010       MOVE W-IDDC-MAX-X       TO SPAR-IDDC-MAX                           
076100                                                                          
076200       PERFORM UNTIL IX > MAX-IX                                          
076300                 OR SEGMENT-SAKNAS                                        
076510         MOVE LAK-KDLARM TO W-KDLARM                                      
076520         IF GODK-LARM                                                     
076600*        --- SELEKTERA FÖR BEGRÄNSAD VISNING P.G.A. SEC-IDLEV             
076700           MOVE LAK-IDLEVNR TO WS-IDLEVNR-8                               
076800                                                                          
076900           PERFORM S2-SECURITY-CHECK-SUPPLIER                             
077000           IF PASSED-SECURITY-CHECK                                       
077100                                                                          
077200             MOVE LAK-DAREGDAT-9KOMPL TO SPAR-RAD-DAREGDAT(IX)            
077300             MOVE LAK-TIKLOCK-9KOMPL TO SPAR-RAD-TIKLOCK (IX)             
077400             MOVE LAK-IDARTNR        TO SPAR-RAD-IDARTNR (IX)             
077410             MOVE LAK-IDDC           TO SPAR-RAD-IDDC    (IX)             
077500             COMPUTE WS-DAREGDAT =                                        
077600                     99999999 - LAK-DAREGDAT-9KOMPL                       
077700             MOVE WS-TIREGDAT   TO MOD-TIREGDAT (IX)                      
077800             MOVE LAK-IDDC      TO MOD-IDDC     (IX)                      
077810             MOVE LAK-FLNYLARM  TO MOD-FLNYLARM (IX)                      
077900             MOVE LAK-IDARTNR   TO WS-IDARTNR-NUM                         
077901             INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE           
077902             MOVE WS-IDARTNR    TO MOD-IDARTNR  (IX)                      
078000             MOVE LAK-IDLEVNR   TO MOD-IDLEVNR  (IX)                      
078100             MOVE LAK-TIAAMMDD  TO MOD-TIPLANDAT(IX)                      
078200             MOVE LAK-KVAVIS    TO MOD-KVAVIS   (IX)                      
078300             MOVE LAK-KVAVROP   TO MOD-KVAVROP  (IX)                      
078310             MOVE LAK-KDLARM    TO MOD-KDLARM   (IX)                      
078400             MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL (IX)              
078500             IF LAK-KDLARM = 220                                          
078600               MOVE WS-TEORSLRM-220 TO MOD-TEORSLRM (IX)                  
078700             ELSE                                                         
078800               IF LAK-KDLARM = 225                                        
078900                 IF LAK-KVAVIS > ZERO                                     
079000*                  TIDIGARE 226                                           
079100                   MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)              
079200                 ELSE                                                     
079300                   MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)              
079400                   MOVE ZERO          TO MOD-KVAVIS   (IX)                
079500                 END-IF                                                   
079600               ELSE                                                       
079700                 IF LAK-KDLARM = 230                                      
079800                   MOVE WS-TEORSLRM-230 TO MOD-TEORSLRM (IX)              
079900                 ELSE                                                     
080000                   MOVE WS-TEORSLRM-235 TO MOD-TEORSLRM (IX)              
080100                 END-IF                                                   
080200               END-IF                                                     
080300             END-IF                                                       
080400             ADD 1 TO IX                                                  
080500           ELSE                                                           
080600             MOVE MED-1 TO MOD-TEMFSFEL                                   
080700           END-IF                                                         
080710         END-IF                                                           
080900         PERFORM IMS-GET-WDD401                                           
081000       END-PERFORM                                                        
081100                                                                          
081200       IF SEGMENT-FINNS                                                   
081300         MOVE LAK-DAREGDAT-9KOMPL  TO SPAR-DAREGDAT-NEXT                  
081400         MOVE LAK-TIKLOCK-9KOMPL   TO SPAR-TIKLOCK-NEXT                   
081500         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
081600         CALL WMEDKONV USING MED-WMEDAREA                                 
081700         MOVE MED-TEMFSINF  TO MOD-TEMFSINF                               
081800       ELSE                                                               
081900         PERFORM UNTIL IX > MAX-IX                                        
082000           MOVE ZERO            TO SPAR-RAD-DAREGDAT(IX)                  
082100                                   SPAR-RAD-TIKLOCK (IX)                  
082200                                   SPAR-RAD-IDARTNR (IX)                  
082210           MOVE SPACE           TO SPAR-RAD-IDDC    (IX)                  
082300           MOVE MFS-RENSA-FAELT TO MOD-IDDC     (IX)                      
082310                                   MOD-IDARTNR  (IX)                      
082400                                   MOD-IDLEVNR  (IX)                      
082500                                   MOD-KDLARM   (IX)                      
082510                                   MOD-TEORSLRM (IX)                      
082600                                   MOD-TIREGDAT (IX)                      
082700                                   MOD-TIPLANDAT(IX)                      
082800                                   MOD-KVAVIS   (IX)                      
082900                                   MOD-KVAVROP  (IX)                      
083000                                   MOD-FLNYLARM (IX)                      
083100                                   MOD-SELECT-ARTIKEL (IX)                
083200           ADD 1                TO IX                                     
083300         END-PERFORM                                                      
083400         MOVE ZERO                 TO SPAR-DAREGDAT-NEXT                  
083500         MOVE ZERO                 TO SPAR-TIKLOCK-NEXT                   
083600       END-IF                                                             
083700                                                                          
083800       MOVE '002'      TO MSGI-KDCALL                                     
083900       MOVE '2472'     TO SPAR-IDTRANS                                    
084000       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
084100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500 FA-LAS-SEK-INDEX SECTION.                                                
084600                                                                          
084700*      NÄR BÅDE IDANSK OCH IDLEVNR ÄR IFYLLT SKALL VI                     
084800*      LÄSA VIA DET SEKUNDÄRA INDEXET, SÅ ATT VI FÅR                      
084900*      RADERNA I ARTIKELNUMMERORDNING                                     
085000                                                                          
085100       MOVE +1 TO IX                                                      
085200       IF (MFS-FIRST OR MFS-SPLIT OR MFS-UPDATE) AND                      
085300          SW-DLET = 'N'                                                   
085400          PERFORM IMS-GN-SEK-WDD401                                       
085500       ELSE                                                               
085600          IF SW-DLET = 'J'                                                
085700             PERFORM IMS-GU-SEK-WDD401                                    
085800          ELSE                                                            
085900             PERFORM IMS-GET-SEK-WDD401                                   
086000          END-IF                                                          
086100       END-IF                                                             
086200                                                                          
086300       IF SEGMENT-FINNS                                                   
086400         MOVE LAK-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-ENTER                  
086500         MOVE LAK-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-ENTER                   
086600       ELSE                                                               
086700         MOVE ZERO             TO SPAR-DAREGDAT-ENTER                     
086800         MOVE ZERO             TO SPAR-TIKLOCK-ENTER                      
086900       END-IF                                                             
087000                                                                          
087100       MOVE W-IDARTNR-MIN      TO SPAR-IDARTNR-MIN                        
087200       MOVE W-IDANSK-MIN       TO SPAR-IDANSK-MIN                         
087300       MOVE W-IDLEVNR-MIN      TO SPAR-IDLEVNR-MIN                        
087400       MOVE W-KDLARM-MIN       TO SPAR-KDLARM-MIN                         
087410       MOVE W-IDDC-MIN-X       TO SPAR-IDDC-MIN                           
087500       MOVE W-IDARTNR-MAX      TO SPAR-IDARTNR-MAX                        
087600       MOVE W-IDANSK-MAX       TO SPAR-IDANSK-MAX                         
087700       MOVE W-IDLEVNR-MAX      TO SPAR-IDLEVNR-MAX                        
087800       MOVE W-KDLARM-MAX       TO SPAR-KDLARM-MAX                         
087810       MOVE W-IDDC-MAX-X       TO SPAR-IDDC-MAX                           
087900                                                                          
088000       PERFORM UNTIL IX > MAX-IX                                          
088100                  Or SEGMENT-SAKNAS                                       
088200         IF SEGMENT-FINNS                                                 
088300*          --- SELEKTERA FÖR BEGRÄNSAD VISNING P.G.A. SEC-IDLEV           
088400           MOVE LAK-IDLEVNR TO WS-IDLEVNR-8                               
088500                                                                          
088600           PERFORM S2-SECURITY-CHECK-SUPPLIER                             
088700           IF PASSED-SECURITY-CHECK                                       
088800                                                                          
088810             MOVE LAK-KDLARM TO W-KDLARM                                  
088900             IF GODK-LARM                                                 
088910               IF  W-KDLARM-MIN = W-KDLARM-MAX                            
088920               AND W-KDLARM-MIN NOT = LAK-KDLARM                          
088930                 CONTINUE                                                 
088940               ELSE                                                       
089000                                                                          
089100                MOVE LAK-DAREGDAT-9KOMPL TO SPAR-RAD-DAREGDAT(IX)         
089200                MOVE LAK-TIKLOCK-9KOMPL  TO SPAR-RAD-TIKLOCK (IX)         
089300                MOVE LAK-IDARTNR         TO SPAR-RAD-IDARTNR (IX)         
089310                MOVE LAK-IDDC            TO SPAR-RAD-IDDC    (IX)         
089400                COMPUTE WS-DAREGDAT =                                     
089500                        99999999 - LAK-DAREGDAT-9KOMPL                    
089600                MOVE WS-TIREGDAT       TO MOD-TIREGDAT (IX)               
089700                MOVE LAK-IDDC          TO MOD-IDDC     (IX)               
089800                MOVE LAK-FLNYLARM      TO MOD-FLNYLARM (IX)               
089810                MOVE LAK-IDARTNR       TO WS-IDARTNR-NUM                  
089820                INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE        
089830                MOVE WS-IDARTNR        TO MOD-IDARTNR  (IX)               
089900                MOVE LAK-IDLEVNR       TO MOD-IDLEVNR  (IX)               
090000                MOVE LAK-TIAAMMDD      TO MOD-TIPLANDAT(IX)               
090100                MOVE LAK-KVAVIS        TO MOD-KVAVIS   (IX)               
090200                MOVE LAK-KVAVROP       TO MOD-KVAVROP  (IX)               
090210                MOVE LAK-KDLARM        TO MOD-KDLARM   (IX)               
090300                MOVE MFS-RENSA-FAELT   TO MOD-SELECT-ARTIKEL (IX)         
090400                IF LAK-KDLARM = 220                                       
090500                  MOVE WS-TEORSLRM-220 TO MOD-TEORSLRM (IX)               
090600                ELSE                                                      
090700                  IF LAK-KDLARM = 225                                     
090800                    IF LAK-KVAVIS > ZERO                                  
090900*                     TIDIGARE 226                                        
091000                      MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)           
091100                    ELSE                                                  
091200                      MOVE WS-TEORSLRM-225 TO MOD-TEORSLRM (IX)           
091300                      MOVE ZERO       TO MOD-KVAVIS   (IX)                
091400                    END-IF                                                
091500                  ELSE                                                    
091600                    IF LAK-KDLARM = 230                                   
091700                      MOVE WS-TEORSLRM-230 TO MOD-TEORSLRM (IX)           
091800                    ELSE                                                  
091900                      MOVE WS-TEORSLRM-235 TO MOD-TEORSLRM (IX)           
092000                    END-IF                                                
092100                  END-IF                                                  
092200                END-IF                                                    
092300                ADD 1 TO IX                                               
092400               END-IF                                                     
092410             END-IF                                                       
092500           Else                                                           
092600             MOVE MED-1 TO MOD-TEMFSFEL                                   
092700           END-IF                                                         
092800         END-IF                                                           
092900         PERFORM IMS-GN-SEK-WDD401                                        
093100       END-PERFORM                                                        
093200                                                                          
093300       IF SEGMENT-FINNS                                                   
093400*        -- FLER SIDOR KAN VISAS, SPARA VÄRDEN                            
093500         MOVE LAK-DAREGDAT-9KOMPL  TO SPAR-DAREGDAT-NEXT                  
093600         MOVE LAK-TIKLOCK-9KOMPL   TO SPAR-TIKLOCK-NEXT                   
093700         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
093800         CALL WMEDKONV USING MED-WMEDAREA                                 
093900         MOVE MED-TEMFSINF  TO MOD-TEMFSINF                               
094000       ELSE                                                               
094100         PERFORM UNTIL IX > MAX-IX                                        
094200*          --- de sista återstående raderna på skärmen                    
094300           MOVE ZERO            TO SPAR-RAD-DAREGDAT(IX)                  
094400                                   SPAR-RAD-TIKLOCK (IX)                  
094500                                   SPAR-RAD-IDARTNR (IX)                  
094510           MOVE SPACE           TO SPAR-RAD-IDDC    (IX)                  
094600           MOVE MFS-RENSA-FAELT TO MOD-IDDC     (IX)                      
094610                                   MOD-IDARTNR  (IX)                      
094700                                   MOD-IDLEVNR  (IX)                      
094800                                   MOD-KDLARM   (IX)                      
094810                                   MOD-TEORSLRM (IX)                      
094900                                   MOD-TIREGDAT (IX)                      
095000                                   MOD-TIPLANDAT(IX)                      
095100                                   MOD-KVAVIS   (IX)                      
095200                                   MOD-KVAVROP  (IX)                      
095300                                   MOD-FLNYLARM (IX)                      
095400                                   MOD-SELECT-ARTIKEL (IX)                
095500           ADD 1 TO IX                                                    
095600         END-PERFORM                                                      
095700         MOVE ZERO                 TO SPAR-DAREGDAT-NEXT                  
095800         MOVE ZERO                 TO SPAR-TIKLOCK-NEXT                   
095900       END-IF                                                             
096000                                                                          
096100       MOVE '002'      TO MSGI-KDCALL                                     
096200       MOVE '2472'     TO SPAR-IDTRANS                                    
096300       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
096400       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
096500     .                                                                    
096600     EJECT                                                                
096700 G-KOLLA-INPUT SECTION.                                                   
096800                                                                          
096900     MOVE JA  TO INDATA-SW                                                
097011                                                                          
097100     MOVE +1 TO IX                                                        
097200     PERFORM UNTIL IX > MAX-IX                                            
097300                                                                          
097400      IF MID-SELECT-ARTIKEL (IX) = '+' OR SPACE                           
097401       MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ARTIKEL-ATTR(IX)           
097410      ELSE                                                                
097500       IF MID-SELECT-ARTIKEL (IX) = 'D'                                   
097510        PERFORM GA-AUTH-USER-CHECK                                        
097520        IF INDATA-OK                                                      
097600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ARTIKEL-ATTR(IX)        
097610        ELSE                                                              
097620          MOVE MFS-ALFA-FAELT-FEL   TO MOD-SELECT-ARTIKEL-ATTR(IX)        
097621          MOVE NEJ                  TO INDATA-SW                          
097622          MOVE ERR-NOT-AUTHORIZED   TO MED-IDMFSFEL                       
097630        END-IF                                                            
097631       ELSE                                                               
097632        IF MID-SELECT-ARTIKEL (IX) = 'S'                                  
097634          MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ARTIKEL-ATTR(IX)        
097635        ELSE                                                              
097637          MOVE MFS-ALFA-FAELT-FEL   TO MOD-SELECT-ARTIKEL-ATTR(IX)        
097638          MOVE NEJ                  TO INDATA-SW                          
097639          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
097640        END-IF                                                            
098600       END-IF                                                             
098900      END-IF                                                              
099000      ADD +1 TO IX                                                        
099100     END-PERFORM                                                          
099210                                                                          
099300     IF INDATA-FEL                                                        
099500        CALL WMEDKONV USING MED-WMEDAREA                                  
099600        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
099700     END-IF                                                               
099710                                                                          
099800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
099900     PERFORM MFS-ROER-EJ-FAELT-IN                                         
100000     .                                                                    
100100     EJECT                                                                
100110 GA-AUTH-USER-CHECK SECTION.                                              
100120                                                                          
100180     MOVE MID-IDDC(IX)                TO W-IDDC                           
100190     PERFORM IMS-GU-B601                                                  
100191     IF SEGMENT-FINNS                                                     
100192       IF DCS-NDC-CN                                                      
100193       OR (DCS-NDC-NA AND DCS-USA)                                        
100194         MOVE MSGI-IDFTG              TO WS-IDFTG                         
100195         IF (DCS-NDC-CN AND IDFTG-CN)                                     
100196         OR (DCS-NDC-NA AND IDFTG-US)                                     
100197         OR MSGI-IDFTG  = WC-IDFTG-PV                                     
100198           CONTINUE                                                       
100199         ELSE                                                             
100202           MOVE NEJ                   TO INDATA-SW                        
100203           MOVE ERR-NOT-AUTHORIZED    TO MED-IDMFSFEL                     
100204         END-IF                                                           
100205       ELSE                                                               
100206         MOVE NEJ                     TO INDATA-SW                        
100207         MOVE ERR-NOT-AUTHORIZED      TO MED-IDMFSFEL                     
100208       END-IF                                                             
100209     ELSE                                                                 
100210        MOVE NEJ                      TO INDATA-SW                        
100211        MOVE ERR-NOT-AUTHORIZED       TO MED-IDMFSFEL                     
100212     END-IF                                                               
100213     .                                                                    
100214     EJECT                                                                
100215                                                                          
100220 H-UPPDATERA SECTION.                                                     
100300                                                                          
100400*    SÖK IGENOM VILKA RADER SOM ÄR MARKERADE FÖR BORTTAG                  
100500*                                                                         
100600     MOVE NEJ TO SW-DLET                                                  
100700     MOVE +1  TO IX                                                       
100800     PERFORM UNTIL IX > MAX-IX                                            
100900       IF MID-SELECT-ARTIKEL (IX) = 'B' OR 'D'                            
101000*        HÄMTA NYCKLAR FRÅN SPAR                                          
101100         MOVE SPAR-RAD-DAREGDAT (IX) TO W-DAREGDAT-9KOMPL                 
101200         MOVE SPAR-RAD-TIKLOCK  (IX) TO W-TIKLOCK-9KOMPL                  
101300         PERFORM IMS-GHU-WDD401                                           
101400         IF SEGMENT-FINNS                                                 
101500            PERFORM IMS-DLET-WDD401                                       
101600            MOVE JA TO SW-DLET                                            
101700         END-IF                                                           
101800       END-IF                                                             
101900       ADD +1 TO IX                                                       
102000     END-PERFORM                                                          
102100                                                                          
102200     IF SW-DLET = JA                                                      
102300       MOVE SPAR-RAD-DAREGDAT (1) TO W-DAREGDAT-9KOMPL                    
102400       MOVE SPAR-RAD-TIKLOCK  (1) TO W-TIKLOCK-9KOMPL                     
102500       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
102600       CALL WMEDKONV USING MED-WMEDAREA                                   
102700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
102800       PERFORM MFS-FORM-ATTR                                              
102900       PERFORM MFS-RENSA-FAELT-IN                                         
103000* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
103100     END-IF                                                               
103200     .                                                                    
103300     EJECT                                                                
103400 K-PPSW-2402            SECTION.                                          
103500                                                                          
103600     MOVE +1  TO IX                                                       
103700     PERFORM UNTIL IX > MAX-IX                                            
103800       IF MID-SELECT-ARTIKEL (IX) = 'S'                                   
103900*        HÄMTA NYCKLAR FRÅN SPAR                                          
104000         MOVE SPAR-RAD-DAREGDAT (IX) TO W-DAREGDAT-9KOMPL                 
104100         MOVE SPAR-RAD-TIKLOCK  (IX) TO W-TIKLOCK-9KOMPL                  
104200         PERFORM IMS-GHU-WDD401                                           
104300         IF SEGMENT-FINNS                                                 
104400            MOVE SPACE               TO LAK-FLNYLARM                      
104500            PERFORM IMS-REPL-WDD401                                       
104600         END-IF                                                           
104700         MOVE SPAR-RAD-IDARTNR (IX)  TO PROGSW-MID-IDARTNR-IN             
104710         MOVE SPAR-RAD-IDDC    (IX)  TO PROGSW-MID-IDDC-IN                
104800         MOVE '2402'                 TO P-IDTRANS                         
104900                                                                          
105000         PERFORM IMS-INSERT-ALT                                           
105100                                                                          
105200         MOVE ZERO TO RETURN-CODE                                         
105300         GOBACK                                                           
105400       END-IF                                                             
105500       ADD +1 TO IX                                                       
105600     END-PERFORM                                                          
105700     .                                                                    
105800     EJECT                                                                
105900                                                                          
106000 S2-SECURITY-CHECK-SUPPLIER SECTION.                                      
106100     SKIP2                                                                
106200*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
106300                                                                          
106400     IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                            
106500     OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                      
106600*      --- BEHÖRIG USER                                                   
106700       SET PASSED-SECURITY-CHECK TO TRUE                                  
106800     ELSE                                                                 
106900       SET BLOCKED-SECURITY-CHECK TO TRUE                                 
107000     END-IF                                                               
107100     .                                                                    
107200     EJECT                                                                
107400 MFS-RENSA-FAELT-UT SECTION.                                              
107500                                                                          
107600     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
107700     .                                                                    
107800     SKIP3                                                                
107900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
108000                                                                          
108100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
108200     MOVE +1 TO IX                                                        
108300     PERFORM UNTIL IX > MAX-IX                                            
108400       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR  (IX)                          
108500                               MOD-IDLEVNR  (IX)                          
108600                               MOD-IDDC     (IX)                          
108700                               MOD-TEORSLRM (IX)                          
108800                               MOD-FLNYLARM (IX)                          
108900                               MOD-KDLARM   (IX)                          
109000                               MOD-TEORSLRM (IX)                          
109100                               MOD-TIREGDAT (IX)                          
109200                               MOD-TIPLANDAT(IX)                          
109300                               MOD-KVAVIS   (IX)                          
109400                               MOD-KVAVROP  (IX)                          
109500                               MOD-SELECT-ARTIKEL (IX)                    
109600       ADD +1 TO IX                                                       
109700     END-PERFORM                                                          
109800     .                                                                    
109900     SKIP3                                                                
110000 MFS-RENSA-FAELT-IN SECTION.                                              
110100                                                                          
110200*    --- ALLA INDATA-FÄLT                                                 
110300     MOVE +1 TO IX                                                        
110400     PERFORM UNTIL IX > MAX-IX                                            
110500       MOVE MFS-RENSA-FAELT TO MOD-SELECT-ARTIKEL (IX)                    
110600       ADD +1 TO IX                                                       
110700     END-PERFORM                                                          
110800     .                                                                    
110900     EJECT                                                                
111000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
111100                                                                          
111200     MOVE +1 TO IX                                                        
111300     PERFORM UNTIL IX > MAX-IX                                            
111400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
111500       ADD +1 TO IX                                                       
111600     END-PERFORM                                                          
111700     .                                                                    
111800     SKIP2                                                                
111900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
112000                                                                          
112100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
112200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR  (IX)                          
112300                               MOD-IDDC     (IX)                          
112400                               MOD-IDLEVNR  (IX)                          
112500                               MOD-KDLARM   (IX)                          
112600                               MOD-TEORSLRM (IX)                          
112700                               MOD-FLNYLARM (IX)                          
112800                               MOD-TEORSLRM (IX)                          
112900                               MOD-TIREGDAT (IX)                          
113000                               MOD-TIPLANDAT(IX)                          
113100                               MOD-KVAVIS   (IX)                          
113200                               MOD-KVAVROP  (IX)                          
113300     .                                                                    
113400     SKIP3                                                                
113500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
113600                                                                          
113700*    --- ALLA INDATA-FÄLT                                                 
113800     MOVE +1 TO IX                                                        
113900     PERFORM UNTIL IX > MAX-IX                                            
114000       MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT-ARTIKEL (IX)                  
114100       ADD +1 TO IX                                                       
114200     END-PERFORM                                                          
114300     .                                                                    
114400     EJECT                                                                
114500 MFS-FORM-ATTR SECTION.                                                   
114600                                                                          
114700*    --- ALLA INDATA-FÄLT                                                 
114800     MOVE +1 TO IX                                                        
114900     PERFORM UNTIL IX > MAX-IX                                            
115000      MOVE MFS-FORMATETS-ATTR      TO MOD-SELECT-ARTIKEL-ATTR (IX)        
115100       ADD +1 TO IX                                                       
115200     END-PERFORM                                                          
115300     .                                                                    
115400     SKIP2                                                                
115500 MFS-LAES-IN-IGEN SECTION.                                                
115600                                                                          
115700*    --- ALLA INDATA-FÄLT                                                 
115800     MOVE +1 TO IX                                                        
115900     PERFORM UNTIL IX > MAX-IX                                            
116000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SELECT-ARTIKEL-ATTR (IX)         
116100       ADD +1 TO IX                                                       
116200     END-PERFORM                                                          
116300     .                                                                    
116400     EJECT                                                                
116500* --- IMS SEKTIONER ---                                                   
116600     SKIP3                                                                
116700 IMS-GET-MSG SECTION.                                                     
116800                                                                          
116900     MOVE '  QC' TO GODK-STATUSKODER                                      
117000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
117100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400     SKIP3                                                                
117500 IMS-INSERT-MSG SECTION.                                                  
117600                                                                          
118000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
118100     MOVE SPACE TO GODK-STATUSKODER                                       
118200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
118300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
118400     PERFORM IMS-STATUSKONTROLL                                           
118500     .                                                                    
118600     EJECT                                                                
118700 IMS-GET-WDD401   SECTION.                                                
118800                                                                          
118810     MOVE SPACE               TO ALL-SSA                                  
118900     STRING 'WDD401  (IDARTNR >=' W-IDARTNR-MIN-X                         
119000                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
119100                    '&IDANSK  >=' W-IDANSK-MIN-X                          
119200                    '&IDANSK  <=' W-IDANSK-MAX-X                          
119300                    '&IDLEVNR >=' W-IDLEVNR-MIN-X                         
119400                    '&IDLEVNR <=' W-IDLEVNR-MAX-X                         
119500                    '&KDLARM  >=' W-KDLARM-MIN-X                          
119600                    '&KDLARM  <=' W-KDLARM-MAX-X                          
119610                    '&IDDC    >=' W-IDDC-MIN-X                            
119620                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
119700          DELIMITED BY SIZE INTO SSA1                                     
119800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
119900     CALL CBLTDLI USING GN  WDD4-PCB DLI-IO-WDD401 SSA1                   
120000     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
120100     PERFORM IMS-STATUSKONTROLL                                           
120200     .                                                                    
120300     SKIP3                                                                
120400 IMS-GU-WDD401   SECTION.                                                 
120500                                                                          
120510     MOVE SPACE               TO ALL-SSA                                  
120600     STRING 'WDD401  (WDD401KY>=' W-WDD401KY-X                            
120700                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
120800                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
120900                    '&IDANSK  >=' W-IDANSK-MIN-X                          
121000                    '&IDANSK  <=' W-IDANSK-MAX-X                          
121100                    '&IDLEVNR >=' W-IDLEVNR-MIN-X                         
121200                    '&IDLEVNR <=' W-IDLEVNR-MAX-X                         
121300                    '&KDLARM  >=' W-KDLARM-MIN-X                          
121400                    '&KDLARM  <=' W-KDLARM-MAX-X                          
121410                    '&IDDC    >=' W-IDDC-MIN-X                            
121420                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
121500          DELIMITED BY SIZE INTO SSA1                                     
121600     MOVE '  GE' TO GODK-STATUSKODER                                      
121700     CALL CBLTDLI USING GU WDD4-PCB DLI-IO-WDD401 SSA1                    
121800     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
121900     PERFORM IMS-STATUSKONTROLL                                           
122000     .                                                                    
122100     EJECT                                                                
122200 IMS-GHU-WDD401   SECTION.                                                
122300                                                                          
122310     MOVE SPACE               TO ALL-SSA                                  
122400     STRING 'WDD401  (WDD401KY =' W-WDD401KY-X ')'                        
122500          DELIMITED BY SIZE INTO SSA1                                     
122600     MOVE '  GE' TO GODK-STATUSKODER                                      
122700     CALL CBLTDLI USING GHU WDD4-PCB DLI-IO-WDD401 SSA1                   
122800     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
122900     PERFORM IMS-STATUSKONTROLL                                           
123000     .                                                                    
123100     SKIP3                                                                
123200 IMS-REPL-WDD401   SECTION.                                               
123300                                                                          
123310     MOVE SPACE               TO ALL-SSA                                  
123400     MOVE '  ' TO GODK-STATUSKODER                                        
123500     CALL CBLTDLI USING REPL WDD4-PCB DLI-IO-WDD401                       
123600     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     .                                                                    
123900     SKIP3                                                                
124000 IMS-DLET-WDD401   SECTION.                                               
124100                                                                          
124110     MOVE SPACE               TO ALL-SSA                                  
124200     MOVE '  ' TO GODK-STATUSKODER                                        
124300     CALL CBLTDLI USING DLET WDD4-PCB DLI-IO-WDD401                       
124400     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
124500     PERFORM IMS-STATUSKONTROLL                                           
124600     .                                                                    
124700     EJECT                                                                
125710 IMS-GN-SEK-WDD401   SECTION.                                             
125730                                                                          
125731     MOVE SPACE               TO ALL-SSA                                  
125740     STRING 'WDD401  (WDD4ASEQ>=' W-WDD4ASEQ-MIN-X                        
125750                    '&WDD4ASEQ<=' W-WDD4ASEQ-MAX-X                        
125751                    '&IDDC    >=' W-IDDC-MIN-X                            
125752                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
125760          DELIMITED BY SIZE INTO SSA1                                     
125770     MOVE '  GEGB' TO GODK-STATUSKODER                                    
125780     CALL CBLTDLI USING GN  WDD4A-PCB DLI-IO-WDD401 SSA1                  
125790     MOVE WDD4A-STATUS-CODE TO STATUS-WS                                  
125791     PERFORM IMS-STATUSKONTROLL                                           
125792     .                                                                    
125800     SKIP3                                                                
126810 IMS-GU-SEK-WDD401   SECTION.                                             
126830                                                                          
126831     MOVE SPACE               TO ALL-SSA                                  
126840     STRING 'WDD401  (WDD4ASEQ>=' W-WDD4ASEQ-MIN-X                        
126850                    '&WDD4ASEQ<=' W-WDD4ASEQ-MAX-X                        
126861                    '&IDDC    >=' W-IDDC-MIN-X                            
126862                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
126863          DELIMITED BY SIZE INTO SSA1                                     
126870     MOVE '  GE' TO GODK-STATUSKODER                                      
126881     CALL CBLTDLI USING GU WDD4A-PCB DLI-IO-WDD401 SSA1                   
126890     MOVE WDD4A-STATUS-CODE TO STATUS-WS                                  
126891     PERFORM IMS-STATUSKONTROLL                                           
126892     .                                                                    
126900     EJECT                                                                
127000 IMS-GET-SEK-WDD401   SECTION.                                            
127100                                                                          
127200**** STRING 'WDD401  (WDD4ASEQ =' W-WDD4ASEQ-X ')'                        
127300****      DELIMITED BY SIZE INTO SSA1                                     
127310     MOVE SPACE               TO ALL-SSA                                  
127400     STRING 'WDD401  (DAREGDAT =' W-DAREGDAT-9KOMPL-ASEQ-X                
127500                    '&TIKLOCK  =' W-TIKLOCK-9KOMPL-ASEQ-X ')'             
127600          DELIMITED BY SIZE INTO SSA1                                     
127700     MOVE '  GE' TO GODK-STATUSKODER                                      
127800     CALL CBLTDLI USING GU WDD4A-PCB DLI-IO-WDD401 SSA1                   
127900     MOVE WDD4A-STATUS-CODE TO STATUS-WS                                  
128000     PERFORM IMS-STATUSKONTROLL                                           
128100     .                                                                    
128200     EJECT                                                                
128300 IMS-INSERT-ALT SECTION.                                                  
128310     MOVE SPACE               TO ALL-SSA                                  
128400     MOVE SPACE TO GODK-STATUSKODER                                       
128500     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
128600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
128700     PERFORM IMS-STATUSKONTROLL                                           
128800     .                                                                    
128900     EJECT                                                                
129000 IMS-GU-K601 SECTION.                                                     
129010     MOVE SPACE               TO ALL-SSA                                  
129100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
129200          DELIMITED BY SIZE INTO SSA1                                     
129300     MOVE '  GE' TO GODK-STATUSKODER                                      
129400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
129500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
129600     PERFORM IMS-STATUSKONTROLL                                           
129700     .                                                                    
129800     SKIP3                                                                
129900 IMS-GNP-K611 SECTION.                                                    
129910     MOVE SPACE               TO ALL-SSA                                  
130000     MOVE 'WDK611  '       TO SSA1                                        
130100     MOVE '  GE' TO GODK-STATUSKODER                                      
130200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
130300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
130400     PERFORM IMS-STATUSKONTROLL                                           
130500     .                                                                    
130600     EJECT                                                                
130610 IMS-GU-K701 SECTION.                                                     
130611     MOVE SPACE               TO ALL-SSA                                  
130620     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
130630          DELIMITED BY SIZE INTO SSA1                                     
130640     MOVE '  GE' TO GODK-STATUSKODER                                      
130650     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
130660     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
130670     PERFORM IMS-STATUSKONTROLL                                           
130680     .                                                                    
130690     SKIP3                                                                
130691 IMS-GU-K722 SECTION.                                                     
130692     MOVE SPACE               TO ALL-SSA                                  
130693     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
130694          DELIMITED BY SIZE INTO SSA1                                     
130695     STRING 'WDK711  (IDDC     =' W-IDDC-MIN-X ')'                        
130696          DELIMITED BY SIZE INTO SSA2                                     
130697     MOVE 'WDK722 '           TO SSA3                                     
130698     MOVE '  GE' TO GODK-STATUSKODER                                      
130699     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
130700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
130701     PERFORM IMS-STATUSKONTROLL                                           
130702     .                                                                    
130703     SKIP3                                                                
130710 IMS-GET-WDR2-2232 SECTION.                                               
130800                                                                          
130810     MOVE SPACE               TO ALL-SSA                                  
130900     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
131000          DELIMITED BY SIZE INTO SSA1                                     
131100     STRING 'WDR220  (WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
131200          DELIMITED BY SIZE INTO SSA2                                     
131300     MOVE '  GE' TO GODK-STATUSKODER                                      
131400     CALL CBLTDLI USING GU  WDR2-PCB DLI-IO-AREA-WDR2 SSA1 SSA2           
131500     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
131600     PERFORM IMS-STATUSKONTROLL                                           
131700     .                                                                    
131800     EJECT                                                                
131810 IMS-GU-B601 SECTION.                                                     
131811     MOVE SPACE               TO ALL-SSA                                  
131820     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
131830          DELIMITED BY SIZE INTO SSA1                                     
131840     MOVE '  GE' TO GODK-STATUSKODER                                      
131850     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
131860     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
131870     PERFORM IMS-STATUSKONTROLL                                           
131880     .                                                                    
131881                                                                          
131890 IMS-GU-WDB601-MIN-MAX SECTION.                                           
131891                                                                          
131892     MOVE SPACE             TO ALL-SSA                                    
131893     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-MIN-X                            
131894                    '&WDB6ASEQ<=' W-IDDC-MAX-X ')'                        
131897       DELIMITED BY SIZE  INTO SSA1                                       
131898     MOVE '  GE'            TO GODK-STATUSKODER                           
131899     CALL CBLTDLI USING GU WDB6A-PCB DLI-IO-WDB601 SSA1                   
131900     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
131901     PERFORM IMS-STATUSKONTROLL                                           
131902     .                                                                    
131910                                                                          
131920 IMS-GN-WDB601-MIN-MAX SECTION.                                           
131930                                                                          
131940     MOVE SPACE             TO ALL-SSA                                    
131950     STRING 'WDB601  (WDB6ASEQ>=' W-IDDC-MIN-X                            
131960                    '&WDB6ASEQ<=' W-IDDC-MAX-X ')'                        
131970       DELIMITED BY SIZE  INTO SSA1                                       
131980     MOVE '  GBGE'          TO GODK-STATUSKODER                           
131990     CALL CBLTDLI USING GN WDB6A-PCB DLI-IO-WDB601 SSA1                   
131991     MOVE WDB6A-STATUS-CODE TO STATUS-WS                                  
131992     PERFORM IMS-STATUSKONTROLL                                           
131993     .                                                                    
131994                                                                          
132000 IMS-STATUSKONTROLL SECTION.                                              
132100                                                                          
132200     SET STATUS-IX TO 1                                                   
132300     SEARCH GODK-STATUS                                                   
132400       AT END                                                             
132500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
132600         DELIMITED BY SIZE INTO FELTEXT                                   
132700         CALL FELLOG                                                      
132800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
132900         CONTINUE                                                         
133000     END-SEARCH                                                           
133100     .                                                                    
