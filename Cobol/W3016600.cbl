000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3016600.                                                
000400 AUTHOR.         BO HAMMARIN.                                             
000500 DATE-WRITTEN.   APRIL-2000.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PGM HANTERAR INFORMATION FÖR CORE-PARTS                          
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDA9                                       
001400*        PROGRAMMET LÄSER      WDM6 VIA WDM6ESEQ                          
001500*        PROGRAMMET LÄSER      WDD3 VIA WDD3BSEQ                          
001600*        PROGRAMMET LÄSER      WDR4 (WDGX3162)                            
001700*        PROGRAMMET LÄSER      WDR4 (WDGX3176)                            
001800*        PROGRAMMET LÄSER      BYART (DB2)                                
001900*    INDATA.                                                              
002000*        TRANSAKTION: W3T166                                              
002100*        MID:         W3I16601                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W3O16601                                            
002500*                                                                         
002600*    CHANGE LOG:                                                          
002700*                                                                         
002800*    DIGAMBAR/20020714                                                    
002900*    WDM6E INDEX IS CHANGED TO REFER THE IDBYTREP-9KOMPL INSTEAD          
003000*    OF THE IDBYTREP THIS IS TO SHOW THE DETAILS IN DESCENDING            
003100*    ORDER OF THE IDBYTREP.                                               
003200*                                                                         
003300*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
003400*      ----------------------------------------------------------         
003500*      14/11/11 - REDDY RAHUL     - CHANGES FOR CHINA EXCHANGE.           
003600*                                   MODIFY TO DISPLAY MORE DC'S.          
003700*      22/06/30 - SRINADH NADIMPALLI - REMOVE RETURNS=3 COLUMN            
003900                                                                          
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 DATA DIVISION.                                                           
004300     EJECT                                                                
004400                                                                          
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(08)   VALUE 'W3016600'.            
004900                                                                          
005000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005200                                                                          
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500                                                                          
005600*    --- ARBETSFÄLT FÖR DIVERSE TILLSTÅND                                 
005700 77  WDA911-SW                   PIC X       VALUE 'J'.                   
005800     88  WDA911-SAKNAS                       VALUE 'N'.                   
005900                                                                          
006000 77  WDGX3172-SW                 PIC X       VALUE 'J'.                   
006100     88  WDGX3172-SAKNAS                     VALUE 'N'.                   
006200                                                                          
006300 77  WDGX3174-SW                 PIC X       VALUE 'J'.                   
006400     88  WDGX3174-SAKNAS                     VALUE 'N'.                   
006500                                                                          
006600 77  WDGX3176-SW                 PIC X       VALUE 'J'.                   
006700     88  WDGX3176-SAKNAS                     VALUE 'N'.                   
006800                                                                          
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300*                                                                         
007400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007500     88  EGEN-MID                            VALUE '3166'.                
007600     88  GODK-MID                            VALUE '3164'                 
007700                                                   '3165'                 
007800                                                   '3166'.                
007900     88  HELP-MID                            VALUE '0551'.                
008000     EJECT                                                                
008100                                                                          
008200*                                                                         
008300 01  WS-IDDC-START               PIC X(02)   VALUE SPACE.                 
008400 01  ROW-IX                      PIC 9(02)   VALUE ZERO.                  
008500 01  MAX-ROW-IX                  PIC 9(02)   VALUE 7.                     
008600                                                                          
008700 01  MAX-DC-IX                   PIC 9(03)   VALUE 100.                   
008800 01  DC-IX                       PIC 9(03).                               
008900                                                                          
009000 01  NDC-TABLE.                                                           
009100     03  NDC-DATA OCCURS 100.                                             
009200         05  WS-IDDC-R           PIC  X(02)  VALUE SPACE.                 
009300         05  WS-IDDC-TYP-R       PIC  X(01)  VALUE SPACE.                 
009400         05  WS-KVLS-R           PIC  S9(7)  COMP-3 VALUE ZERO.           
009500         05  WS-KVADV-R          PIC  S9(7)  COMP-3 VALUE ZERO.           
009600         05  WS-KVTOT-R          PIC  S9(7)  COMP-3 VALUE ZERO.           
009700         05  WS-KVTRA-R          PIC  S9(7)  COMP-3 VALUE ZERO.           
009800*        05  WS-KVRET-R          PIC  S9(7)  COMP-3 VALUE ZERO.           
009900*        05  WS-KVRETUR-R        PIC  X(01)  VALUE 'N'.                   
010000*            88  NDC-KVRETUR                 VALUE 'J'.                   
010100*            88  NDC-KVRETUR-NO              VALUE 'N'.                   
010200         05  WS-IDDC-VALID       PIC  X(01)  VALUE 'N'.                   
010300                                                                          
010400 01  FILLER                      PIC  X(16)  VALUE 'DIVERSE   '.          
010500 01  WS-IDARTNR-9                PIC  9(9).                               
010600 01  WS-KVLS                     PIC  S9(7)  COMP-3 VALUE ZERO.           
010700 01  WS-KVLS-11                  PIC  S9(7)  COMP-3 VALUE ZERO.           
010800 01  WS-KVLS-91                  PIC  S9(7)  COMP-3 VALUE ZERO.           
010900 01  WS-KVLS-INT                 PIC  S9(7)  COMP-3 VALUE ZERO.           
011000 01  WS-KVLS-REM                 PIC  S9(7)  COMP-3 VALUE ZERO.           
011100 01  WS-KVLS-TOT                 PIC  S9(7)  COMP-3 VALUE ZERO.           
011200 01  WS-KVADV                    PIC  S9(7)  COMP-3 VALUE ZERO.           
011300 01  WS-KVADV-11                 PIC  S9(7)  COMP-3 VALUE ZERO.           
011400 01  WS-KVADV-91                 PIC  S9(7)  COMP-3 VALUE ZERO.           
011500 01  WS-KVADV-INT                PIC  S9(7)  COMP-3 VALUE ZERO.           
011600 01  WS-KVADV-REM                PIC  S9(7)  COMP-3 VALUE ZERO.           
011700 01  WS-KVADV-TOT                PIC  S9(7)  COMP-3 VALUE ZERO.           
011800 01  WS-KVTOT-11                 PIC  S9(7)  COMP-3 VALUE ZERO.           
011900 01  WS-KVTOT-91                 PIC  S9(7)  COMP-3 VALUE ZERO.           
012000 01  WS-KVTOT-INT                PIC  S9(7)  COMP-3 VALUE ZERO.           
012100 01  WS-KVTOT-REM                PIC  S9(7)  COMP-3 VALUE ZERO.           
012200 01  WS-KVTOT-TOT                PIC  S9(7)  COMP-3 VALUE ZERO.           
012300 01  WS-KVTRA-11                 PIC  S9(7)  COMP-3 VALUE ZERO.           
012400 01  WS-KVTRA-91                 PIC  S9(7)  COMP-3 VALUE ZERO.           
012500 01  WS-KVTRA-INT                PIC  S9(7)  COMP-3 VALUE ZERO.           
012600 01  WS-KVRET-11                 PIC  S9(7)  COMP-3 VALUE ZERO.           
012700 01  WS-KVRET-91                 PIC  S9(7)  COMP-3 VALUE ZERO.           
012800 01  WS-KVRET-INT                PIC  S9(7)  COMP-3 VALUE ZERO.           
012900     EJECT                                                                
013000 01  SPAR-AREA.                                                           
013100     03  SPAR-IDTRANS            PIC X(04)   VALUE '3166'.                
013200     03  SPAR-IDDC-ENTER         PIC X(02).                               
013300     03  SPAR-IDDC-NEXT          PIC X(02).                               
013400     EJECT                                                                
013500 77  W-SDC-NL-KVRETUR            PIC X(01)   VALUE 'N'.                   
013600     88  SDC-NL-KVRETUR                      VALUE 'J'.                   
013700     88  SDC-NL-KVRETUR-NO                   VALUE 'N'.                   
013800                                                                          
013900 77  W-CDC-SE-KVRETUR            PIC X(01)   VALUE 'N'.                   
014000     88  CDC-SE-KVRETUR                      VALUE 'J'.                   
014100     88  CDC-SE-KVRETUR-NO                   VALUE 'N'.                   
014200                                                                          
014300 77  W-ALL-KVRETUR               PIC X(01)   VALUE 'N'.                   
014400     88  ALL-KVRETUR                         VALUE 'J'.                   
014500     88  ALL-KVRETUR-NO                      VALUE 'N'.                   
014600                                                                          
014700                                                                          
014800 01  FILLER                      PIC  X(16)  VALUE 'BYTES-DIST'.          
014900 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
015000*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
015100     EJECT                                                                
015200                                                                          
015300 01  FILLER                      PIC  X(16)  VALUE 'BYTES-ART '.          
015400 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
015500*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
015600                                                                          
015700*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
015800                                                                          
015900*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
016000                                                                          
016100     EJECT                                                                
016200*    --- VALID IDDC CODES                                                 
016300*                                                                         
016400*01  -COPY WWDC99                                                         
016500     EJECT                                                                
016600*    --- VALID IDDC CODES                                                 
016700*                                                                         
016800*01  -COPY WWDCKONS                                                       
016900     EJECT                                                                
017000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017100 01  GENERELLA-SUBPROGRAM.                                                
017200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017700     EJECT                                                                
017800                                                                          
017900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018000*01 -COPY WMEDAREA                                                        
018100                                                                          
018200 01  MESSAGE-CODES.                                                       
018300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018400     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
018500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
018700     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
018800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018900     EJECT                                                                
019000                                                                          
019100*01  -COPY WDATAREA                                                       
019200     EJECT                                                                
019300                                                                          
019400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019500*                                                                         
019600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019700*01 -COPY WMSGINIT                                                        
019800     EJECT                                                                
019900                                                                          
020000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020100*                                                                         
020200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020300*01  MID -COPY W3I16601                                                   
020400     EJECT                                                                
020500                                                                          
020600 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
020700*01  -COPY WMSGAREA                                                       
020800     EJECT                                                                
020900                                                                          
021000     03  MOD REDEFINES MSG-AREA.                                          
021100*      05  -COPY W3O16601                                                 
021200     EJECT                                                                
021300                                                                          
021400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021500*01  -COPY WMFSAREA                                                       
021600     EJECT                                                                
021700                                                                          
021800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100 01  NYCKLAR-TILL-DLI.                                                    
022200     03  W-IDARTNR-K6-REN-X.                                              
022300         05  W-IDARTNR-K6-REN    PIC S9(9)   VALUE ZERO COMP-3.           
022400     03  W-KDSEGKEY-K6-REN-X.                                             
022500         05  W-KDSEGKEY-K6-REN   PIC X(1)    VALUE '1'.                   
022600     03  W-IDARTNR-K6-X.                                                  
022700         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
022800     03  W-KDSEGKEY-K6-X.                                                 
022900         05  W-KDSEGKEY-K6       PIC X(1)    VALUE '1'.                   
023000     03  W-IDARTNR-K7-X.                                                  
023100         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
023200     03  W-IDDC-K7-X.                                                     
023300         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
023400     03  W-WDGXKEY-3161-X.                                                
023500         05  W-IDHTYP-3161       PIC X(4)    VALUE '3161'.                
023600         05  W-IDDISTR-3161      PIC S9(5)   VALUE ZERO COMP-3.           
023700         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
023800     03  W-KY3162-X.                                                      
023900         05  W-DAORDREG-3162     PIC 9(8)    VALUE ZERO.                  
024000         05  W-IDORDER-3162      PIC S9(7)   VALUE ZERO.                  
024100         05  W-IDARTNR-3162      PIC S9(9)   VALUE ZERO.                  
024200     03  W-WDGXKEY-3171-X.                                                
024300         05  W-IDHTYP-3171       PIC X(4)    VALUE '3171'.                
024400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
024500     03  W-IDFAKT-3172-X.                                                 
024600         05  W-IDFAKT-3172       PIC S9(7)   VALUE ZERO COMP-3.           
024700     03  W-IDKOLLI-3174-X.                                                
024800         05  W-IDKOLLI-3174      PIC S9(5)   VALUE ZERO COMP-3.           
024900     03  W-IDARTNR-3176-X.                                                
025000         05  W-IDARTNR-3176      PIC S9(9)   VALUE ZERO COMP-3.           
025100     03  W-IDARTNR-A9-X.                                                  
025200         05  W-IDARTNR-A9        PIC S9(9)   VALUE ZERO COMP-3.           
025300     03  W-IDDISTR-A9-X.                                                  
025400         05  W-IDDISTR-A9        PIC S9(5)   VALUE ZERO COMP-3.           
025500     03  W-IDARTNR-D3-X.                                                  
025600         05  W-IDARTNR-D3        PIC S9(9)   VALUE ZERO COMP-3.           
025700     03  W-IDSKYLT-D3-X.                                                  
025800         05  W-IDSKYLT-D3        PIC X(3)    VALUE 'GB'.                  
025900*    03  W-WDM6ESEQ-MIN-X.                                                
026000*        05  W-IDARTNR-M6E-MIN   PIC S9(9)  VALUE ZERO COMP-3.            
026100*    03  W-WDM6ESEQ-MAX-X.                                                
026200*        05  W-IDARTNR-M6E-MAX   PIC S9(9)  VALUE ZERO COMP-3.            
026300*    03  W-WDM601KY-X.                                                    
026400*        05  W-IDDISTR-M6        PIC S9(5)   VALUE ZERO COMP-3.           
026500*        05  W-IDBYTRAP-M6       PIC S9(7)   VALUE ZERO COMP-3.           
026600     03  W-IDBYTRAD-M6-X.                                                 
026700         05  W-IDBYTRAD-M6       PIC S9(5)   VALUE ZERO COMP-3.           
026800                                                                          
026900*    --- STATUS-KOD FRÅN IMS                                              
027000 01  STATUS-WS                   PIC XX.                                  
027100     88  SEGMENT-FINNS                       VALUE '  '.                  
027200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
027400                                                                          
027500 01  GODK-STATUSKODER.                                                    
027600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027700                                                                          
027800 01  SSA1                        PIC X(96).                               
027900 01  SSA2                        PIC X(64).                               
028000 01  SSA3                        PIC X(64).                               
028100                                                                          
028200     EJECT                                                                
028300 01  NYCKLAR-TILL-DB2.                                                    
028400     03  W-IDARTNR-BYART-X.                                               
028500         05  W-IDARTNR-BYART     PIC S9(9)   VALUE ZERO COMP-3.           
028600                                                                          
028700 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
028800*01  -COPY BYART -PRE BYART-                                              
028900     EJECT                                                                
029000 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
029100       EXEC SQL INCLUDE BYART END-EXEC.                                   
029200     SKIP3                                                                
029300 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
029400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
029500                                                                          
029600 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
029700 01  DB2-WS.                                                              
029800     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
029900         88  CURSOR-OK                       VALUE 000.                   
030000         88  RADER-FINNS                     VALUE 000.                   
030100         88  RADER-SAKNAS                    VALUE 100.                   
030200         88  ATKOMST-FEL                     VALUE 904.                   
030300     03  GODK-SQLCODEKODER.                                               
030400         05  GODK-SQLCODE OCCURS 5                                        
030500             INDEXED BY SQLCODE-IX PIC 9(3).                              
030600                                                                          
030700*    --- IMS FUNKTIONSKODER                                               
030800*01  -COPY W0003                                                          
030900     EJECT                                                                
031000                                                                          
031100*    ---  DLI INPUT-OUTPUT AREA                                           
031200                                                                          
031300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
031400 01  DLI-IO-WDK601.                                                       
031500*    03  -COPY WDK601                                                     
031600     EJECT                                                                
031700                                                                          
031800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
031900 01  DLI-IO-WDK611.                                                       
032000*    03  -COPY WDK611                                                     
032100     EJECT                                                                
032200                                                                          
032300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
032400 01  DLI-IO-WDK701.                                                       
032500*    03  -COPY WDK701                                                     
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
032900 01  DLI-IO-WDK711.                                                       
033000*    03  -COPY WDK711                                                     
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
033400 01  DLI-IO-WDA901.                                                       
033500*    03  -COPY WDA901                                                     
033600     EJECT                                                                
033700                                                                          
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
033900 01  DLI-IO-WDA911.                                                       
034000*    03  -COPY WDA911                                                     
034100     EJECT                                                                
034200                                                                          
034300*01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM6'.                        
034400*01  DLI-IO-WDM6.                                                         
034500*    03  DLI-IO-WDM611.                                                   
034600*        05  -COPY WDM611                                                 
034700*    03  DLI-IO-WDM601.                                                   
034800*        05  -COPY WDM601                                                 
034900*    EJECT                                                                
035000                                                                          
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3161'.                    
035200 01  DLI-IO-WDGX3161.                                                     
035300*    03  -COPY WDGX3161                                                   
035400     EJECT                                                                
035500                                                                          
035600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3162'.                    
035700 01  DLI-IO-WDGX3162.                                                     
035800*    03  -COPY WDGX3162                                                   
035900     EJECT                                                                
036000                                                                          
036100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3171'.                    
036200 01  DLI-IO-WDGX3171.                                                     
036300*    03  -COPY WDGX01                                                     
036400     EJECT                                                                
036500                                                                          
036600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3172'.                    
036700 01  DLI-IO-WDGX3172.                                                     
036800*    03  -COPY WDGX3172                                                   
036900     EJECT                                                                
037000                                                                          
037100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3174'.                    
037200 01  DLI-IO-WDGX3174.                                                     
037300*    03  -COPY WDGX3174                                                   
037400     EJECT                                                                
037500                                                                          
037600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3176'.                    
037700 01  DLI-IO-WDGX3176.                                                     
037800*    03  -COPY WDGX3176                                                   
037900     EJECT                                                                
038000                                                                          
038100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311  '.                    
038200 01  DLI-IO-WDD311.                                                       
038300*    03  -COPY WDD311                                                     
038400     EJECT                                                                
038500                                                                          
038600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
038700 01  DLI-IO-WDB601.                                                       
038800     03  -COPY WDB601                                                     
038900                                                                          
039000 LINKAGE SECTION.                                                         
039100*01  -COPY W0009   -PRE MSG-                                              
039200                                                                          
039300*01  -COPY W0008   -PRE USEA-                                             
039400     05  FILLER                  PIC X.                                   
039500                                                                          
039600*01  -COPY W0008   -PRE WDK6-                                             
039700     05  FILLER                  PIC X.                                   
039800                                                                          
039900*01  -COPY W0008   -PRE WDK7-                                             
040000     05  FILLER                  PIC X.                                   
040100                                                                          
040200*01  -COPY W0008   -PRE WDA9-                                             
040300     05  FILLER                  PIC X.                                   
040400                                                                          
040800*01  -COPY W0008   -PRE WDD3B-                                            
040900     05  FILLER                  PIC X.                                   
041000                                                                          
041100*01  -COPY W0008   -PRE 3161-                                             
041200     05  FILLER                  PIC X.                                   
041300                                                                          
041400*01  -COPY W0008   -PRE 3171-                                             
041500     05  FILLER                  PIC X.                                   
041600                                                                          
041700*01  -COPY W0008   -PRE WDB6-                                             
041800     05  FILLER                  PIC X.                                   
041900     EJECT                                                                
042000                                                                          
042100 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
042200                           WDK6-PCB  WDK7-PCB                             
042300                           WDA9-PCB                                       
042400*                          WDM6ESQ-PCB                                    
042500                           WDD3B-PCB                                      
042600                           3161-PCB  3171-PCB                             
042700                           WDB6-PCB.                                      
042800 MAIN SECTION.                                                            
042900     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
043000                           WDK6-PCB  WDK7-PCB                             
043100                           WDA9-PCB                                       
043200*                          WDM6ESQ-PCB                                    
043300                           WDD3B-PCB                                      
043400                           3161-PCB  3171-PCB                             
043500                           WDB6-PCB.                                      
043600                                                                          
043700     PERFORM IMS-GET-MSG                                                  
043800                                                                          
043900     IF SEGMENT-FINNS                                                     
044000       PERFORM A-INIT                                                     
044100       PERFORM B-KOLLA-NYCKLAR                                            
044200       IF NYCKLAR-OK                                                      
044300         IF MFS-FIRST                                                     
044400           PERFORM C-FOERSTA-SIDA                                         
044500         ELSE                                                             
044600           IF MFS-NEXT                                                    
044700             PERFORM D-NAESTA-SIDA                                        
044800           ELSE                                                           
044900             PERFORM E-SAMMA-SIDA                                         
045000           END-IF                                                         
045100         END-IF                                                           
045200         PERFORM F-LAES-VISA-INFO-MAIN                                    
045300       END-IF                                                             
045400       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O16601 + 4                      
045500       PERFORM IMS-INSERT-MSG                                             
045600     END-IF                                                               
045700                                                                          
045800     MOVE ZERO TO RETURN-CODE                                             
045900     GOBACK                                                               
046000     .                                                                    
046100     EJECT                                                                
046200                                                                          
046300 A-INIT SECTION.                                                          
046400     IF MSG-DUBBLA-TRANSKODER                                             
046500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I16601                 
046600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
046700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
046800     ELSE                                                                 
046900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I16601                 
047000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
047100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
047200     END-IF                                                               
047300                                                                          
047400     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
047500     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
047600     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
047700                                                                          
047800     MOVE LOW-VALUE                       TO MSG-AREA                     
047900     MOVE 'W3O166N1'                      TO MFS-IDMOD                    
048000     MOVE '3166'                          TO MOD-IDTRANS                  
048100     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
048200                                             MOD-TEMFSINF                 
048300                                                                          
048400     IF EGEN-MID OR HELP-MID                                              
048500       CONTINUE                                                           
048600     ELSE                                                                 
048700       MOVE SPACE                         TO MFS-KDTRTYP                  
048800       MOVE '7'                           TO MFS-IDPFK                    
048900     END-IF                                                               
049000                                                                          
049100     MOVE 'GB'                            TO MED-IDSKYLT                  
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500 B-KOLLA-NYCKLAR SECTION.                                                 
049600     MOVE ALL '+'            TO MSGI-WMSGINIT                             
049700     MOVE '001'              TO MSGI-KDCALL                               
049800     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
049900     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
050000     MOVE '3166'             TO MSGI-IDTRANS                              
050100     IF GODK-MID                                                          
050200       IF MID-IDARTNR-IN = ALL '+'                                        
050300         MOVE MID-IDARTNR-UT TO WS-IDARTNR-9                              
050400       ELSE                                                               
050500         MOVE MID-IDARTNR-IN TO WS-IDARTNR-9                              
050600       END-IF                                                             
050700       MOVE WS-IDARTNR-9     TO MSGI-IDARTNR                              
050800     END-IF                                                               
050900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051000                                                                          
051100     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
051200                                                                          
051300     MOVE JA TO NYCKLAR-SW                                                
051400                                                                          
051500*    -- KONTROLL AV IDARTNR                                               
051600     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
051700                                                                          
051800     IF MID-IDARTNR-IN NOT = ALL '+'                                      
051900       MOVE '7'              TO MFS-IDPFK                                 
052000       MOVE SPACE            TO MFS-KDTRTYP                               
052100     END-IF                                                               
052200     INSPECT MSGI-IDARTNR REPLACING ALL     SPACE BY ZERO                 
052300     IF MSGI-IDARTNR NUMERIC                                              
052400       MOVE MSGI-IDARTNR     TO TEST-IDARTNR                              
052500       IF NOT BYT03-OBJEKT AND NOT BYT02-RENOV                            
052600         MOVE NEJ            TO NYCKLAR-SW                                
052700       ELSE                                                               
052800         IF BYT02-RENOV                                                   
052900           IF BYT16-BYTES                                                 
053000             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
053100                                    6000                                  
053200             END-COMPUTE                                                  
053300           ELSE                                                           
053400             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
053500                                    1000                                  
053600             END-COMPUTE                                                  
053700           END-IF                                                         
053800         END-IF                                                           
053900         MOVE TEST-IDARTNR   TO W-IDARTNR-K6                              
054000                                W-IDARTNR-K7                              
054100                                W-IDARTNR-A9                              
054200*                               W-IDARTNR-M6E-MIN                         
054300*                               W-IDARTNR-M6E-MAX                         
054400                                W-IDARTNR-D3                              
054500                                W-IDARTNR-3176                            
054600       END-IF                                                             
054700     ELSE                                                                 
054800       MOVE NEJ              TO NYCKLAR-SW                                
054900     END-IF                                                               
055000                                                                          
055100     IF GODK-MID OR NYCKLAR-OK                                            
055200       MOVE MSGI-IDARTNR(2:8) TO MOD-IDARTNR-UT                           
055300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
055400     ELSE                                                                 
055500       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                           
055600     END-IF                                                               
055700                                                                          
055800     IF NYCKLAR-FEL                                                       
055900       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
056000       CALL WMEDKONV USING MED-WMEDAREA                                   
056100       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
056200       PERFORM MFS-RENSA-FAELT-IN                                         
056300       PERFORM MFS-RENSA-FAELT-UT                                         
056400     END-IF                                                               
056500     .                                                                    
056600     EJECT                                                                
056700                                                                          
056800 C-FOERSTA-SIDA SECTION.                                                  
056900                                                                          
057000     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
057100     CALL WMEDKONV            USING MED-WMEDAREA                          
057200     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
057300     MOVE SPACE                  TO SPAR-IDDC-ENTER                       
057400                                    SPAR-IDDC-NEXT                        
057500                                                                          
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 D-NAESTA-SIDA SECTION.                                                   
058000                                                                          
058100     IF SPAR-IDTRANS = '3166'                                             
058200       MOVE SPAR-IDDC-NEXT       TO WS-IDDC-START                         
058300     ELSE                                                                 
058400       PERFORM MFS-RENSA-FAELT-IN                                         
058500     END-IF                                                               
058600                                                                          
058700     .                                                                    
058800     EJECT                                                                
058900                                                                          
059000 E-SAMMA-SIDA SECTION.                                                    
059100                                                                          
059200     IF SPAR-IDTRANS = '3166'                                             
059300       MOVE SPAR-IDDC-ENTER      TO WS-IDDC-START                         
059400     ELSE                                                                 
059500       PERFORM MFS-RENSA-FAELT-IN                                         
059600     END-IF                                                               
059700                                                                          
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100 F-LAES-VISA-INFO-MAIN SECTION.                                           
060200                                                                          
060300     PERFORM S02-LOAD-DC-TABLE                                            
060400     PERFORM IMS-GU-WDK601                                                
060500     IF SEGMENT-SAKNAS                                                    
060600       MOVE ERR-PART-MISSING     TO MED-IDMFSFEL                          
060700       CALL WMEDKONV          USING MED-WMEDAREA                          
060800       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
060900       PERFORM MFS-RENSA-FAELT-UT                                         
061000     ELSE                                                                 
061100       PERFORM IMS-GNP-WDK611                                             
061200       IF SEGMENT-FINNS                                                   
061300         ADD CLAG-KVLS           TO WS-KVLS-INT                           
061400                                    WS-KVLS-TOT                           
061500                                    WS-KVLS-11                            
061600                                    WS-KVTOT-11                           
061700                                    WS-KVTOT-INT                          
061800       END-IF                                                             
061900* BENÄMNING WDD3                                                          
062000       PERFORM IMS-GU-WDD311-BSEQ                                         
062100       IF SEGMENT-FINNS                                                   
062200         MOVE TEXT-BEART         TO MOD-BEART-CORE                        
062300       ELSE                                                               
062400         MOVE 'UNKNOWN'          TO MOD-BEART-CORE                        
062500       END-IF                                                             
062600       PERFORM FA-INFO-WDK7                                               
062700       PERFORM FB-INFO-WDA9-WDGX3162                                      
062800//     PERFORM FC-INFO-WDM6                                               
062900       PERFORM FD-MOVE-TO-MOD                                             
063000     END-IF                                                               
063100                                                                          
063200     MOVE '002'                  TO MSGI-KDCALL                           
063300     MOVE '3166'                 TO SPAR-IDTRANS                          
063400     MOVE SPAR-AREA              TO MSGI-SPAR-AREA                        
063500     CALL W005INIT            USING MSGI-WMSGINIT USEA-PCB                
063600     .                                                                    
063700     EJECT                                                                
063800                                                                          
063900 FA-INFO-WDK7 SECTION.                                                    
064000     PERFORM IMS-GU-WDK701                                                
064100                                                                          
064200     PERFORM UNTIL SEGMENT-SAKNAS                                         
064300       PERFORM IMS-GNP-WDK711                                             
064400       IF SEGMENT-FINNS                                                   
064500         MOVE ZERO               TO WS-KVLS                               
064600         ADD SLAG-KVLS           TO WS-KVLS                               
064700                                    WS-KVLS-INT                           
064800                                    WS-KVLS-TOT                           
064900         MOVE SLAG-IDDC          TO WS-IDDC                               
065000         IF SDC-NL-ET                                                     
065100           ADD WS-KVLS           TO WS-KVLS-91                            
065200                                    WS-KVTOT-91                           
065300                                    WS-KVTOT-INT                          
065400         ELSE                                                             
065500           IF NDC                                                         
065600             PERFORM S01-SET-DC-IX                                        
065700             ADD WS-KVLS         TO WS-KVLS-R  (DC-IX)                    
065800                                    WS-KVTOT-R (DC-IX)                    
065900                                    WS-KVTOT-INT                          
066000             MOVE JA             TO WS-IDDC-VALID(DC-IX)                  
066100           END-IF                                                         
066200         END-IF                                                           
066300       END-IF                                                             
066400     END-PERFORM                                                          
066500                                                                          
066600     PERFORM FAA-INFO-WDGX3176                                            
066700                                                                          
066800                                                                          
066900     .                                                                    
067000     EJECT                                                                
067100                                                                          
067200 FAA-INFO-WDGX3176 SECTION.                                               
067300                                                                          
067400     PERFORM IMS-GU-WDGX3171                                              
067500                                                                          
067600     PERFORM UNTIL WDGX3172-SAKNAS                                        
067700       PERFORM IMS-GNP-WDGX3172                                           
067800                                                                          
067900       IF SEGMENT-FINNS AND                                               
068000          3172-KDTRSTAT < 4                                               
068100         MOVE 'J'                     TO WDGX3174-SW                      
068200                                                                          
068300         PERFORM UNTIL WDGX3174-SAKNAS                                    
068400           MOVE 3172-IDFAKT           TO W-IDFAKT-3172                    
068500                                                                          
068600           PERFORM IMS-GNP-WDGX3174                                       
068700           IF SEGMENT-FINNS                                               
068800             MOVE 3174-IDKOLLI        TO W-IDKOLLI-3174                   
068900             PERFORM UNTIL SEGMENT-SAKNAS                                 
069000               PERFORM IMS-GNP-WDGX3176                                   
069100               IF SEGMENT-FINNS                                           
069200*                                                                         
069300                 MOVE 3172-IDDC-SEND  TO WS-IDDC                          
069400                 IF SDC-NL-ET                                             
069500                   ADD  3176-KVANTMOT TO WS-KVTRA-91                      
069600                                         WS-KVTRA-INT                     
069700                 ELSE                                                     
069800                   IF NDC                                                 
069900                     PERFORM S01-SET-DC-IX                                
070000                     ADD 3176-KVANTMOT                                    
070100                                      TO WS-KVTRA-R (DC-IX)               
070200                                         WS-KVTRA-INT                     
070300                     MOVE JA          TO WS-IDDC-VALID(DC-IX)             
070400                   ELSE                                                   
070500                     IF CDC-SE                                            
070600                       ADD  3176-KVANTMOT TO WS-KVTRA-11                  
070700                                             WS-KVTRA-INT                 
070800                     END-IF                                               
070900                   END-IF                                                 
071000                 END-IF                                                   
071100*                                                                         
071200                 MOVE 3172-IDDC-REC   TO WS-IDDC                          
071300                 IF SDC-NL-ET                                             
071400                   ADD  3176-KVANTMOT TO WS-KVADV-TOT                     
071500                                         WS-KVADV-INT                     
071600                                         WS-KVTOT-INT                     
071700                                         WS-KVADV-91                      
071800                                         WS-KVTOT-91                      
071900                 ELSE                                                     
072000                   IF NDC                                                 
072100                     PERFORM S01-SET-DC-IX                                
072200                     ADD 3176-KVANTMOT                                    
072300                                      TO WS-KVADV-TOT                     
072400                                         WS-KVADV-INT                     
072500                                         WS-KVTOT-INT                     
072600                                         WS-KVADV-R (DC-IX)               
072700                                         WS-KVTOT-R (DC-IX)               
072800                     MOVE JA          TO WS-IDDC-VALID(DC-IX)             
072900                   ELSE                                                   
073000                     IF CDC-SE                                            
073100                       ADD  3176-KVANTMOT TO WS-KVADV-TOT                 
073200                                             WS-KVADV-INT                 
073300                                             WS-KVTOT-INT                 
073400                                             WS-KVADV-11                  
073500                                             WS-KVTOT-11                  
073600                     END-IF                                               
073700                   END-IF                                                 
073800                 END-IF                                                   
073900               END-IF                                                     
074000             END-PERFORM                                                  
074100                                                                          
074200           ELSE                                                           
074300             MOVE NEJ                 TO WDGX3174-SW                      
074400           END-IF                                                         
074500         END-PERFORM                                                      
074600                                                                          
074700       ELSE                                                               
074800         IF SEGMENT-SAKNAS                                                
074900           MOVE NEJ                   TO WDGX3172-SW                      
075000         END-IF                                                           
075100       END-IF                                                             
075200     END-PERFORM                                                          
075300                                                                          
075400     .                                                                    
075500     EJECT                                                                
075600                                                                          
075700 FB-INFO-WDA9-WDGX3162 SECTION.                                           
075800     PERFORM IMS-GU-WDA901                                                
075900     IF SEGMENT-FINNS                                                     
076000       MOVE JA                       TO WDA911-SW                         
076100       MOVE ZERO                     TO WS-KVLS                           
076200                                        WS-KVADV                          
076300       IF BYT16-BYTES                                                     
076400         COMPUTE W-IDARTNR-BYART = TEST-IDARTNR -                         
076500                                    6000                                  
076600       ELSE                                                               
076700         COMPUTE W-IDARTNR-BYART = TEST-IDARTNR -                         
076800                                    1000                                  
076900       END-IF                                                             
077000       PERFORM DB2-SELECT-BYART                                           
077100       IF RADER-FINNS                                                     
077200         MOVE BYART-IDDISTR-RENOV TO W-IDDISTR-A9                         
077300         PERFORM IMS-GNP-WDA911                                           
077400         IF SEGMENT-FINNS                                                 
077500* FIX SOM UTÖKAS FÖR VARJE RENOVÖR SOM ANSLUTS TILL WEB:EN                
077600           MOVE UPD-IDDISTR          TO TEST-IDDISTR                      
077700           IF DIS134-BYTESREN-WEB                                         
077800             ADD UPD-KVLS-REM        TO WS-KVLS                           
077900                                        WS-KVLS-TOT                       
078000             MOVE UPD-IDDISTR        TO W-IDDISTR-3161                    
078100             PERFORM IMS-GU-WDGX3161                                      
078200             IF SEGMENT-FINNS                                             
078300                                                                          
078400               PERFORM UNTIL SEGMENT-SAKNAS                               
078500                 PERFORM IMS-GNP-WDGX3162                                 
078600                 IF SEGMENT-FINNS                                         
078700                   IF 3162-IDARTNR = UPB-IDARTNR AND                      
078800                      3162-IDUSER  = SPACE                                
078900                     ADD 3162-KVAVIS TO WS-KVADV                          
079000                                        WS-KVADV-TOT                      
079100                   END-IF                                                 
079200                 END-IF                                                   
079300               END-PERFORM                                                
079400                                                                          
079500             END-IF                                                       
079600           END-IF                                                         
079700         END-IF                                                           
079800       END-IF                                                             
079900                                                                          
080000       MOVE WS-KVLS                TO WS-KVLS-REM                         
080100       MOVE WS-KVADV               TO WS-KVADV-REM                        
080200       COMPUTE WS-KVTOT-REM     = WS-KVLS +                               
080300                                  WS-KVADV                                
080400       END-COMPUTE                                                        
080500     END-IF                                                               
080600     COMPUTE WS-KVTOT-TOT     = WS-KVLS-TOT +                             
080700                                WS-KVADV-TOT                              
080800     END-COMPUTE                                                          
080900     .                                                                    
081000     EJECT                                                                
081100                                                                          
081200*FC-INFO-WDM6 SECTION.                                                    
081300*    PERFORM IMS-GN-WDM611-WDM601-ESEQ                                    
081400                                                                          
081500**** HERE LOGIC IS TO SHOW THE LATEST REPORTS KVRETUR-URSP                
081600*    CHANGES DONE TO KEEP THE SAME LOGIC                                  
081700*    SINCE THE WDM6E INDEX IS CHANGED TO REFER THE IDBYTRAP-9KOMP         
081800*    INSTEAD OF THE IDBYTRAP, ORDER OF THE DATA IS CHANGED. SO            
081900*    FIRST VALID RECORD NEED TO SHOW INSTEAD OF THE LAST (LAST            
082000**** RECORD WAS SHOWED BEFORE THE CHANGE IN INDEX WDM6E)                  
082100*                                                                         
082200*    PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
082300*                  ALL-KVRETUR                                            
082400*                                                                         
082500*      IF RAPP-KDBYTSTA-RAPP < 4                                          
082600*        MOVE RAPP-IDDC          TO WS-IDDC                               
082700*        IF SDC-NL-ET                                                     
082800*          IF SDC-NL-KVRETUR-NO                                           
082900*            COMPUTE WS-KVRET-91 = OBJ-KVRETUR-URSP                       
083000*            END-COMPUTE                                                  
083100*            ADD WS-KVRET-91     TO WS-KVRET-INT                          
083200*            MOVE JA             TO W-SDC-NL-KVRETUR                      
083300*          END-IF                                                         
083400*        ELSE                                                             
083500*          IF NDC                                                         
083600*            PERFORM S01-SET-DC-IX                                        
083700*            IF NDC-KVRETUR-NO (DC-IX)                                    
083800*              COMPUTE WS-KVRET-R (DC-IX) = OBJ-KVRETUR-URSP              
083900*              END-COMPUTE                                                
084000*              ADD WS-KVRET-R (DC-IX)                                     
084100*                                TO WS-KVRET-INT                          
084200*              MOVE JA           TO WS-KVRETUR-R (DC-IX)                  
084300*              MOVE JA           TO WS-IDDC-VALID(DC-IX)                  
084400*            END-IF                                                       
084500*          ELSE                                                           
084600*            IF CDC-SE                                                    
084700*              IF CDC-SE-KVRETUR-NO                                       
084800*                COMPUTE WS-KVRET-11 = OBJ-KVRETUR-URSP                   
084900*                END-COMPUTE                                              
085000*                ADD WS-KVRET-11     TO WS-KVRET-INT                      
085100*                MOVE JA             TO W-CDC-SE-KVRETUR                  
085200*              END-IF                                                     
085300*            END-IF                                                       
085400*          END-IF                                                         
085500*        END-IF                                                           
085600*                                                                         
085700*        MOVE JA                 TO W-ALL-KVRETUR                         
085800*        IF SDC-NL-KVRETUR AND CDC-SE-KVRETUR                             
085900*          PERFORM                                                        
086000*          VARYING DC-IX FROM 1 BY 1                                      
086100*            UNTIL DC-IX > MAX-DC-IX                                      
086200*               OR ALL-KVRETUR-NO                                         
086300*            IF WS-IDDC-R (DC-IX) > SPACE                                 
086400*              IF NDC-KVRETUR (DC-IX)                                     
086500*                CONTINUE                                                 
086600*              ELSE                                                       
086700*                MOVE NEJ        TO W-ALL-KVRETUR                         
086800*              END-IF                                                     
086900*            END-IF                                                       
087000*          END-PERFORM                                                    
087100*        ELSE                                                             
087200*          MOVE NEJ              TO W-ALL-KVRETUR                         
087300*        END-IF                                                           
087400*                                                                         
087500*      END-IF                                                             
087600*      PERFORM IMS-GN-WDM611-WDM601-ESEQ                                  
087700*    END-PERFORM                                                          
087800*                                                                         
087900*    .                                                                    
088000*    EJECT                                                                
088100*                                                                         
088200 FD-MOVE-TO-MOD SECTION.                                                  
088300                                                                          
088400     MOVE WS-KVLS-INT            TO MOD-KVLS-INT                          
088500     MOVE WS-KVADV-INT           TO MOD-KVADV-INT                         
088600     MOVE WS-KVTOT-INT           TO MOD-KVTOT-INT                         
088700     MOVE WS-KVTRA-INT           TO MOD-KVTRA-INT                         
088800*    MOVE WS-KVRET-INT           TO MOD-KVRET-INT                         
088900*                                                                         
089000     MOVE WS-KVLS-REM            TO MOD-KVLS-REM                          
089100     MOVE WS-KVADV-REM           TO MOD-KVADV-REM                         
089200     MOVE WS-KVTOT-REM           TO MOD-KVTOT-REM                         
089300*                                                                         
089400     MOVE WS-KVLS-TOT            TO MOD-KVLS-TOT                          
089500     MOVE WS-KVADV-TOT           TO MOD-KVADV-TOT                         
089600     MOVE WS-KVTOT-TOT           TO MOD-KVTOT-TOT                         
089700*                                                                         
089800     MOVE WS-KVLS-11             TO MOD-KVLS-11                           
089900     MOVE WS-KVADV-11            TO MOD-KVADV-11                          
090000     MOVE WS-KVTOT-11            TO MOD-KVTOT-11                          
090100     MOVE WS-KVTRA-11            TO MOD-KVTRA-11                          
090200*    MOVE WS-KVRET-11            TO MOD-KVRET-11                          
090300*                                                                         
090400     MOVE WS-KVLS-91             TO MOD-KVLS-91                           
090500     MOVE WS-KVADV-91            TO MOD-KVADV-91                          
090600     MOVE WS-KVTOT-91            TO MOD-KVTOT-91                          
090700     MOVE WS-KVTRA-91            TO MOD-KVTRA-91                          
090800*    MOVE WS-KVRET-91            TO MOD-KVRET-91                          
090900*                                                                         
091000     PERFORM                                                              
091100     VARYING DC-IX FROM 1 BY 1                                            
091200       UNTIL WS-IDDC-R (DC-IX) >= WS-IDDC-START                           
091300     END-PERFORM                                                          
091400                                                                          
091500     MOVE 1                      TO ROW-IX                                
091600     MOVE WS-IDDC-R (DC-IX)      TO SPAR-IDDC-ENTER                       
091700                                                                          
091800     PERFORM                                                              
091900     VARYING DC-IX FROM DC-IX BY 1                                        
092000       UNTIL DC-IX > MAX-DC-IX                                            
092100          OR ROW-IX > MAX-ROW-IX                                          
092200          OR WS-IDDC-R (DC-IX) = SPACE                                    
092300       IF WS-IDDC-R (DC-IX) > SPACE AND                                   
092400          WS-IDDC-VALID(DC-IX) = JA                                       
092500         MOVE WS-IDDC-TYP-R (DC-IX)                                       
092600                                 TO MOD-DC-TYP (ROW-IX)                   
092700         MOVE WS-IDDC-R  (DC-IX) TO MOD-IDDC   (ROW-IX)                   
092800         MOVE WS-KVLS-R  (DC-IX) TO MOD-KVLS   (ROW-IX)                   
092900         MOVE WS-KVADV-R (DC-IX) TO MOD-KVADV  (ROW-IX)                   
093000         MOVE WS-KVTOT-R (DC-IX) TO MOD-KVTOT  (ROW-IX)                   
093100         MOVE WS-KVTRA-R (DC-IX) TO MOD-KVTRA  (ROW-IX)                   
093200*        MOVE WS-KVRET-R (DC-IX) TO MOD-KVRET  (ROW-IX)                   
093300         ADD +1                  TO ROW-IX                                
093400       END-IF                                                             
093500     END-PERFORM                                                          
093600                                                                          
093700     IF WS-IDDC-R (DC-IX) > SPACE                                         
093800       MOVE WS-IDDC-R (DC-IX)    TO SPAR-IDDC-NEXT                        
093900       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
094000       CALL WMEDKONV          USING MED-WMEDAREA                          
094100       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
094200     ELSE                                                                 
094300       MOVE SPAR-IDDC-ENTER      TO SPAR-IDDC-NEXT                        
094400       MOVE INF-LAST-PAGE        TO MED-IDMFSINF                          
094500       CALL WMEDKONV          USING MED-WMEDAREA                          
094600       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
094700     END-IF                                                               
094800                                                                          
094900     PERFORM                                                              
095000     VARYING ROW-IX FROM ROW-IX BY 1                                      
095100       UNTIL ROW-IX >= MAX-ROW-IX                                         
095200       MOVE MFS-RENSA-FAELT      TO MOD-DC-TYP (ROW-IX)                   
095300                                    MOD-IDDC   (ROW-IX)                   
095400                                    MOD-KVLS   (ROW-IX)                   
095500                                    MOD-KVADV  (ROW-IX)                   
095600                                    MOD-KVTOT  (ROW-IX)                   
095700                                    MOD-KVTRA  (ROW-IX)                   
095800*                                   MOD-KVRET  (ROW-IX)                   
095900     END-PERFORM                                                          
096000                                                                          
096100     .                                                                    
096200     EJECT                                                                
096300                                                                          
096400 S01-SET-DC-IX SECTION.                                                   
096500                                                                          
096600     MOVE 1 TO DC-IX                                                      
096700     PERFORM UNTIL DC-IX > MAX-DC-IX                                      
096800                OR WS-IDDC-R(DC-IX) = WS-IDDC                             
096900        ADD 1 TO DC-IX                                                    
097000     END-PERFORM                                                          
097100                                                                          
097200     IF DC-IX > MAX-DC-IX                                                 
097300        MOVE 'WS-IDDC SAKNAS I DC-TABELLEN' TO FELTEXT                    
097400        CALL FELLOG                                                       
097500     END-IF                                                               
097600     .                                                                    
097700     EJECT                                                                
097800                                                                          
097900 S02-LOAD-DC-TABLE SECTION.                                               
098000                                                                          
098100     PERFORM IMS-GN-WDB601                                                
098200     MOVE 1 TO DC-IX                                                      
098300     PERFORM UNTIL SEGMENT-SLUT                                           
098400                OR DC-IX > MAX-DC-IX                                      
098500        IF DCS-NDC                                                        
098600           MOVE DCS-IDDC TO WS-IDDC-R(DC-IX)                              
098700           MOVE 'N'      TO WS-IDDC-TYP-R (DC-IX)                         
098800           ADD 1 TO DC-IX                                                 
098900        END-IF                                                            
099000        PERFORM IMS-GN-WDB601                                             
099100     END-PERFORM                                                          
099200                                                                          
099300     IF DC-IX > MAX-DC-IX                                                 
099400        MOVE 'DC-TABELL FULL' TO FELTEXT                                  
099500        CALL FELLOG                                                       
099600     END-IF                                                               
099700                                                                          
099800     .                                                                    
099900     EJECT                                                                
100000                                                                          
100100 MFS-RENSA-FAELT-UT SECTION.                                              
100200*    --- ALLA UTDATA-FÄLT                                                 
100300     MOVE MFS-RENSA-FAELT TO MOD-BEART-CORE                               
100400                             MOD-KVLS-11                                  
100500                             MOD-KVLS-91                                  
100600                             MOD-KVLS-INT                                 
100700                             MOD-KVLS-REM                                 
100800                             MOD-KVLS-TOT                                 
100900                             MOD-KVADV-11                                 
101000                             MOD-KVADV-91                                 
101100                             MOD-KVADV-INT                                
101200                             MOD-KVADV-REM                                
101300                             MOD-KVADV-TOT                                
101400                             MOD-KVTOT-11                                 
101500                             MOD-KVTOT-91                                 
101600                             MOD-KVTOT-INT                                
101700                             MOD-KVTOT-REM                                
101800                             MOD-KVTOT-TOT                                
101900                             MOD-KVTRA-11                                 
102000                             MOD-KVTRA-91                                 
102100                             MOD-KVTRA-INT                                
102200*                            MOD-KVRET-11                                 
102300*                            MOD-KVRET-91                                 
102400*                            MOD-KVRET-INT                                
102500                                                                          
102600     PERFORM                                                              
102700     VARYING ROW-IX FROM 1 BY 1                                           
102800       UNTIL ROW-IX > MAX-ROW-IX                                          
102900       MOVE MFS-RENSA-FAELT      TO MOD-DC-TYP (ROW-IX)                   
103000                                    MOD-IDDC   (ROW-IX)                   
103100                                    MOD-KVLS   (ROW-IX)                   
103200                                    MOD-KVADV  (ROW-IX)                   
103300                                    MOD-KVTOT  (ROW-IX)                   
103400                                    MOD-KVTRA  (ROW-IX)                   
103500*                                   MOD-KVRET  (ROW-IX)                   
103600     END-PERFORM                                                          
103700                                                                          
103800     .                                                                    
103900     EJECT                                                                
104000                                                                          
104100 MFS-RENSA-FAELT-IN SECTION.                                              
104200*    --- ALLA INDATA-FÄLT                                                 
104300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
104400     .                                                                    
104500     EJECT                                                                
104600                                                                          
104700* --- IMS SEKTIONER ---                                                   
104800 IMS-GET-MSG SECTION.                                                     
104900     MOVE '  QC'          TO GODK-STATUSKODER                             
105000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
105100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105200     PERFORM IMS-STATUSKONTROLL                                           
105300     .                                                                    
105400                                                                          
105500 IMS-INSERT-MSG SECTION.                                                  
105600     IF MSGI-IDLAND-SPR = 'GB'                                            
105700       MOVE 'N'           TO MFS-KDHUVOMR                                 
105800     END-IF                                                               
105900     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
106000     MOVE SPACE           TO GODK-STATUSKODER                             
106100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
106200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106300     PERFORM IMS-STATUSKONTROLL                                           
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700 IMS-GU-WDK601 SECTION.                                                   
106800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
106900          DELIMITED BY SIZE INTO SSA1                                     
107000     MOVE '  GE'           TO GODK-STATUSKODER                            
107100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
107200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500                                                                          
107600 IMS-GNP-WDK611 SECTION.                                                  
107700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-X ')'                     
107800          DELIMITED BY SIZE INTO SSA1                                     
107900     MOVE '  GE'           TO GODK-STATUSKODER                            
108000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
108100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
108200     PERFORM IMS-STATUSKONTROLL                                           
108300     .                                                                    
108400     EJECT                                                                
108500                                                                          
108600 IMS-GU-WDK701 SECTION.                                                   
108700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
108800          DELIMITED BY SIZE INTO SSA1                                     
108900     MOVE '  GE'           TO GODK-STATUSKODER                            
109000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
109100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
109200     PERFORM IMS-STATUSKONTROLL                                           
109300     .                                                                    
109400                                                                          
109500 IMS-GNP-WDK711 SECTION.                                                  
109600     MOVE 'WDK711   '      TO SSA1                                        
109700     MOVE '  GE'           TO GODK-STATUSKODER                            
109800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
109900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
110000     PERFORM IMS-STATUSKONTROLL                                           
110100     .                                                                    
110200     EJECT                                                                
110300                                                                          
110400 IMS-GU-WDGX3161 SECTION.                                                 
110500     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3161-X ')'                    
110600          DELIMITED BY SIZE INTO SSA1                                     
110700     MOVE '  GE'           TO GODK-STATUSKODER                            
110800     CALL CBLTDLI USING GU  3161-PCB DLI-IO-WDGX3161 SSA1                 
110900     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     .                                                                    
111200                                                                          
111300 IMS-GNP-WDGX3162 SECTION.                                                
111400     MOVE 'WDGX3162  '     TO SSA1                                        
111500     MOVE '  GE'           TO GODK-STATUSKODER                            
111600     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
111700     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
111800     PERFORM IMS-STATUSKONTROLL                                           
111900     .                                                                    
112000     EJECT                                                                
112100                                                                          
112200 IMS-GU-WDGX3171 SECTION.                                                 
112300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
112400          DELIMITED BY SIZE INTO SSA1                                     
112500     MOVE '  '             TO GODK-STATUSKODER                            
112600     CALL CBLTDLI USING GU  3171-PCB DLI-IO-WDGX3171 SSA1                 
112700     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
112800     PERFORM IMS-STATUSKONTROLL                                           
112900     .                                                                    
113000                                                                          
113100 IMS-GNP-WDGX3172 SECTION.                                                
113200     MOVE 'WDGX3172  '     TO SSA1                                        
113300     MOVE '  GE'           TO GODK-STATUSKODER                            
113400     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3172 SSA1                 
113500     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
113600     PERFORM IMS-STATUSKONTROLL                                           
113700     .                                                                    
113800                                                                          
113900 IMS-GNP-WDGX3174 SECTION.                                                
114000     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
114100          DELIMITED BY SIZE INTO SSA1                                     
114200     MOVE 'WDGX3174  '     TO SSA2                                        
114300     MOVE '  GE'           TO GODK-STATUSKODER                            
114400     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3174 SSA1 SSA2            
114500     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
114600     PERFORM IMS-STATUSKONTROLL                                           
114700     .                                                                    
114800                                                                          
114900 IMS-GNP-WDGX3176 SECTION.                                                
115000     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
115100          DELIMITED BY SIZE INTO SSA1                                     
115200     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-3174-X ')'                    
115300          DELIMITED BY SIZE INTO SSA2                                     
115400     STRING 'WDGX3176(IDARTNRO =' W-IDARTNR-3176-X ')'                    
115500          DELIMITED BY SIZE INTO SSA3                                     
115600     MOVE '  GE'           TO GODK-STATUSKODER                            
115700     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3176 SSA1 SSA2            
115800                                                     SSA3                 
115900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116200     EJECT                                                                
116300                                                                          
116400 IMS-GU-WDA901 SECTION.                                                   
116500     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-A9-X ')'                      
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     MOVE '  GE'           TO GODK-STATUSKODER                            
116800     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
116900     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
117000     PERFORM IMS-STATUSKONTROLL                                           
117100     .                                                                    
117200                                                                          
117300 IMS-GNP-WDA911 SECTION.                                                  
117400     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-A9-X ')'                      
117500          DELIMITED BY SIZE INTO SSA1                                     
117600     MOVE '  GE'           TO GODK-STATUSKODER                            
117700     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA911 SSA1                   
117800     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
117900     PERFORM IMS-STATUSKONTROLL                                           
118000     .                                                                    
118100     EJECT                                                                
118200                                                                          
118300 IMS-GU-WDD311-BSEQ SECTION.                                              
118400     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-D3-X ')'                      
118500     DELIMITED BY SIZE INTO SSA1                                          
118600     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-D3-X ')'                      
118700     DELIMITED BY SIZE INTO SSA2                                          
118800     MOVE '  GE'            TO GODK-STATUSKODER                           
118900     CALL CBLTDLI USING GU WDD3B-PCB DLI-IO-WDD311 SSA1 SSA2              
119000     MOVE WDD3B-STATUS-CODE TO STATUS-WS                                  
119100     PERFORM IMS-STATUSKONTROLL                                           
119200     .                                                                    
119300     EJECT                                                                
119400                                                                          
119500*IMS-GN-WDM611-WDM601-ESEQ SECTION.                                       
119600*                                                                         
119700*    STRING 'WDM611  *D(WDM6ESEQ>=' W-WDM6ESEQ-MIN-X                      
119800*                     '&WDM6ESEQ<=' W-WDM6ESEQ-MAX-X ')'                  
119900*           DELIMITED BY SIZE INTO SSA1                                   
120000*    MOVE   'WDM601  '        TO   SSA2                                   
120100*    MOVE '  GEGB'            TO GODK-STATUSKODER                         
120200*    CALL CBLTDLI USING GN WDM6ESQ-PCB DLI-IO-WDM6 SSA1 SSA2              
120300*    MOVE WDM6ESQ-STATUS-CODE TO STATUS-WS                                
120400*    PERFORM IMS-STATUSKONTROLL                                           
120500*    .                                                                    
120600*                                                                         
120700*    SKIP2                                                                
120800 IMS-GN-WDB601 SECTION.                                                   
120900     MOVE 'WDB601 '        TO SSA1                                        
121000     MOVE '  GB' TO GODK-STATUSKODER                                      
121100     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
121200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
121300     PERFORM IMS-STATUSKONTROLL                                           
121400     .                                                                    
121500                                                                          
121600     SKIP2                                                                
121700 IMS-STATUSKONTROLL SECTION.                                              
121800     SET STATUS-IX TO 1                                                   
121900     SEARCH GODK-STATUS                                                   
122000       AT END                                                             
122100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
122200         DELIMITED BY SIZE INTO FELTEXT                                   
122300         CALL FELLOG                                                      
122400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
122500         CONTINUE                                                         
122600     END-SEARCH                                                           
122700     .                                                                    
122800     EJECT                                                                
122900                                                                          
123000 DB2-SELECT-BYART SECTION.                                                
123100                                                                          
123200     MOVE 000100           TO GODK-SQLCODEKODER                           
123300     EXEC SQL                                                             
123400         SELECT IDDISTR_RENOV                                             
123500         INTO :BYART-IDDISTR-RENOV                                        
123600         FROM BYART                                                       
123700         WHERE IDARTNR_BYT = :W-IDARTNR-BYART                             
123800     END-EXEC                                                             
123900     MOVE SQLCODE          TO SQLCODE-WS                                  
124000     PERFORM DB2-STATUSKONTROLL                                           
124100     .                                                                    
124200     EJECT                                                                
124300 DB2-STATUSKONTROLL  SECTION.                                             
124400                                                                          
124500     SET SQLCODE-IX TO 1                                                  
124600     SEARCH GODK-SQLCODE                                                  
124700       AT END CALL FELLOG                                                 
124800       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
124900     END-SEARCH                                                           
125000     .                                                                    
