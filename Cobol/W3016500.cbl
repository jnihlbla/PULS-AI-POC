000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3016500.                                                
000400 AUTHOR.         BO HAMMARIN.                                             
000500 DATE-WRITTEN.   MARS-2000.                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PGM HANTERAR INFORMATION FÖR CORE-PARTS                          
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDA9                                       
001400*        PROGRAMMET LÄSER      WDD3 VIA WDD3BSEQ                          
001500*        PROGRAMMET LÄSER      WDR4 (WDGX3162)                            
001600*        PROGRAMMET LÄSER      WDR4 (WDGX3176)                            
001700*        PROGRAMMET LÄSER      FSG2 (DB2)                                 
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WDK6                                       
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W3T165                                              
002300*                     W3T165U                                             
002400*        MID:         W3I16501                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W3O16501                                            
002800*                                                                         
002900*    REMARKS: ÄNDRING (RÄTTAT) 041021 MED ETRACKER NR 1311109.            
003000*                                                                         
003100*    CHANGE LOG:                                                          
003200*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
003300*                                                                         
003400*      14/03/11 - REDDY RAHUL     - ETRACKER 10198833                     
003500*                                   CORE ALARM CHANGES                    
003600*                                                                         
003700                                                                          
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200                                                                          
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'W3016500'.            
004700                                                                          
004800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005000                                                                          
005100 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  WS-JAMFOR-PROC              PIC 9(4)    VALUE ZERO.                  
005500 77  W-IDARTNR                   PIC S9(9)   COMP-3 VALUE ZERO.           
005500 77  W-IDARTNR-BYART             PIC S9(9)   COMP-3 VALUE ZERO.           
005600                                                                          
005700*    --- ARBETSFÄLT FÖR DIVERSE TILLSTÅND                                 
006100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006200     88  NYCKLAR-OK                          VALUE 'J'.                   
006300     88  NYCKLAR-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006600     88  INDATA-OK                           VALUE 'J'.                   
006700     88  INDATA-FEL                          VALUE 'N'.                   
006800                                                                          
006900 77  BYT02-SW                    PIC X       VALUE 'N'.                   
007000     88  BYT02-YES                           VALUE 'J'.                   
007100     88  BYT02-NO                            VALUE 'N'.                   
007200                                                                          
007300 77  WDGX3172-SW                 PIC X       VALUE 'J'.                   
007400     88  WDGX3172-SAKNAS                     VALUE 'N'.                   
007500                                                                          
007600 77  WDGX3174-SW                 PIC X       VALUE 'J'.                   
007700     88  WDGX3174-SAKNAS                     VALUE 'N'.                   
007800                                                                          
007900 77  WDGX3176-SW                 PIC X       VALUE 'J'.                   
008000     88  WDGX3176-SAKNAS                     VALUE 'N'.                   
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '3165'.                
008400     88  GODK-MID                            VALUE '3164'                 
008500                                                   '3165'                 
008600                                                   '3166'                 
008700                                                   '2171'.                
008800     88  HELP-MID                            VALUE '0551'.                
008900     EJECT                                                                
009000                                                                          
009100 01  FILLER                      PIC  X(16)  VALUE 'DIVERSE   '.          
009200 01  WS-IDARTNR-9                PIC  9(9).                               
009300 01  WS-UPPD-FAELT.                                                       
009400   03  WS-RELARM-PER             PIC  9(3).                               
009500   03  WS-RERETUR                PIC  9(3).                               
009600   03  WS-REREUSE                PIC  9(3).                               
009700   03  WS-FLLARM-ACT             PIC  X.                                  
009800 01  WS-RELARM-FAC               PIC  9V9(2) VALUE ZERO.                  
009900 01  WS-KVLS-CORE                PIC  S9(9)  COMP-3 VALUE ZERO.           
010000 01  WS-KVLS-REN                 PIC  S9(9)  COMP-3 VALUE ZERO.           
010100                                                                          
010200     EJECT                                                                
010300                                                                          
010400 01  FILLER                      PIC  X(16)  VALUE 'BYTES-DIST'.          
010500 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
010600*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
010700     EJECT                                                                
010800                                                                          
010900 01  FILLER                      PIC  X(16)  VALUE 'BYTES-ART '.          
011000 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
011100*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
011200     EJECT                                                                
011300                                                                          
011400*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
011500     EJECT                                                                
011600                                                                          
011700*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
011800     EJECT                                                                
011900                                                                          
012000*    --- VALID IDDC CODES                                                 
012100*                                                                         
012200*01  -COPY WWDC99                                                         
012300     EJECT                                                                
012400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012500 01  GENERELLA-SUBPROGRAM.                                                
012600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013000     EJECT                                                                
013100                                                                          
013200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013300*01 -COPY WMEDAREA                                                        
013400                                                                          
013500 01  MESSAGE-CODES.                                                       
013600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013700     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
013800     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '029'.                 
013900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014000     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
014100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014200     EJECT                                                                
014300                                                                          
014400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014700*01 -COPY WMSGINIT                                                        
014800     EJECT                                                                
014900                                                                          
015000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015300*01  MID -COPY W3I16501                                                   
015400     EJECT                                                                
015500                                                                          
015600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015700*01  -COPY WMSGAREA                                                       
015800     EJECT                                                                
015900                                                                          
016000     03  MOD REDEFINES MSG-AREA.                                          
016100*      05  -COPY W3O16501                                                 
016200     EJECT                                                                
016300                                                                          
016400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016500*01  -COPY WMFSAREA                                                       
016600     EJECT                                                                
016700                                                                          
016800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016900*                                                                         
017000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017100 01  NYCKLAR-TILL-DLI.                                                    
017200     03  W-IDARTNR-K6-REN-X.                                              
017300         05  W-IDARTNR-K6-REN    PIC S9(9)   VALUE ZERO COMP-3.           
017400     03  W-KDSEGKEY-K6-REN-X.                                             
017500         05  W-KDSEGKEY-K6-REN   PIC X(1)    VALUE '1'.                   
017600     03  W-IDARTNR-K6-X.                                                  
017700         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
017800     03  W-KDSEGKEY-K6-X.                                                 
017900         05  W-KDSEGKEY-K6       PIC X(1)    VALUE '1'.                   
018000     03  W-IDARTNR-K7-X.                                                  
018100         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
018200     03  W-IDDC-K7-X.                                                     
018300         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
018400     03  W-WDGXKEY-3161-X.                                                
018500         05  W-IDHTYP-3161       PIC X(4)    VALUE '3161'.                
018600         05  W-IDDISTR-3161      PIC S9(5)   VALUE ZERO COMP-3.           
018700         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
018800     03  W-KY3162-X.                                                      
018900         05  W-DAORDREG-3162     PIC 9(8)    VALUE ZERO.                  
019000         05  W-IDORDER-3162      PIC S9(7)   VALUE ZERO.                  
019100         05  W-IDARTNR-3162      PIC S9(9)   VALUE ZERO.                  
019200     03  W-IDARTNR-A9-X.                                                  
019300         05  W-IDARTNR-A9        PIC S9(9)   VALUE ZERO COMP-3.           
019400     03  W-IDDISTR-A9-X.                                                  
019500         05  W-IDDISTR-A9        PIC S9(5)   VALUE ZERO COMP-3.           
019600     03  W-IDARTNR-D3-X.                                                  
019700         05  W-IDARTNR-D3        PIC S9(9)   VALUE ZERO COMP-3.           
019800     03  W-IDSKYLT-D3-X.                                                  
019900         05  W-IDSKYLT-D3        PIC X(3)    VALUE 'GB'.                  
020000     03  W-WDGXKEY-3171-X.                                                
020100         05  W-IDHTYP-3171       PIC X(4)    VALUE '3171'.                
020200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
020300     03  W-IDFAKT-3172-X.                                                 
020400         05  W-IDFAKT-3172       PIC S9(7)   VALUE ZERO COMP-3.           
020500     03  W-IDKOLLI-3174-X.                                                
020600         05  W-IDKOLLI-3174      PIC S9(5)   VALUE ZERO COMP-3.           
020700     03  W-IDARTNR-3176-X.                                                
020800         05  W-IDARTNR-3176      PIC S9(9)   VALUE ZERO COMP-3.           
020900                                                                          
021000*    --- STATUS-KOD FRÅN IMS                                              
021100 01  STATUS-WS                   PIC XX.                                  
021200     88  SEGMENT-FINNS                       VALUE '  '.                  
021300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021500                                                                          
021600 01  GODK-STATUSKODER.                                                    
021700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021800                                                                          
021900 01  SSA1                        PIC X(64).                               
022000 01  SSA2                        PIC X(64).                               
022100 01  SSA3                        PIC X(64).                               
022200     EJECT                                                                
022300 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
022400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
022500                                                                          
022600 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
022700 01  DB2-WS.                                                              
022800     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
022900         88  CURSOR-OK                       VALUE 000.                   
023000         88  RADER-FINNS                     VALUE 000.                   
023100         88  RADER-SAKNAS                    VALUE 100.                   
023200         88  ATKOMST-FEL                     VALUE 904.                   
023300     03  GODK-SQLCODEKODER.                                               
023400         05  GODK-SQLCODE OCCURS 5                                        
023500             INDEXED BY SQLCODE-IX PIC 9(3).                              
023600     EJECT                                                                
023700*    --- IMS FUNKTIONSKODER                                               
023800*01  -COPY W0003                                                          
023900     EJECT                                                                
024000                                                                          
024100*    ---  DLI INPUT-OUTPUT AREA                                           
024200                                                                          
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
024400 01  DLI-IO-WDK601.                                                       
024500*    03  -COPY WDK601                                                     
024600     EJECT                                                                
024700                                                                          
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
024900 01  DLI-IO-WDK611.                                                       
025000*    03  -COPY WDK611                                                     
025100     EJECT                                                                
025200                                                                          
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK628'.                      
025400 01  DLI-IO-WDK628.                                                       
025500*    03  -COPY WDK628                                                     
025600     EJECT                                                                
025700                                                                          
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
025900 01  DLI-IO-WDK701.                                                       
026000*    03  -COPY WDK701                                                     
026100     EJECT                                                                
026200                                                                          
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
026400 01  DLI-IO-WDK711.                                                       
026500*    03  -COPY WDK711                                                     
026600     EJECT                                                                
026700                                                                          
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
026900 01  DLI-IO-WDA901.                                                       
027000*    03  -COPY WDA901                                                     
027100     EJECT                                                                
027200                                                                          
027300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
027400 01  DLI-IO-WDA911.                                                       
027500*    03  -COPY WDA911                                                     
027600     EJECT                                                                
027700                                                                          
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3161'.                    
027900 01  DLI-IO-WDGX3161.                                                     
028000*    03  -COPY WDGX3161                                                   
028100     EJECT                                                                
028200                                                                          
028300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3162'.                    
028400 01  DLI-IO-WDGX3162.                                                     
028500*    03  -COPY WDGX3162                                                   
028600     EJECT                                                                
028700                                                                          
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311  '.                    
028900 01  DLI-IO-WDD311.                                                       
029000*    03  -COPY WDD311                                                     
029100     EJECT                                                                
029200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3171'.                    
029300 01  DLI-IO-WDGX3171.                                                     
029400*    03  -COPY WDGX01                                                     
029500     EJECT                                                                
029600                                                                          
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3172'.                    
029800 01  DLI-IO-WDGX3172.                                                     
029900*    03  -COPY WDGX3172                                                   
030000     EJECT                                                                
030100                                                                          
030200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3174'.                    
030300 01  DLI-IO-WDGX3174.                                                     
030400*    03  -COPY WDGX3174                                                   
030500     EJECT                                                                
030600                                                                          
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3176'.                    
030800 01  DLI-IO-WDGX3176.                                                     
030900*    03  -COPY WDGX3176                                                   
031000     EJECT                                                                
031100 01  FILLER         PIC X(16) VALUE 'FSG2-AREA-1     '.                   
031200*01  -COPY FSG2 -PRE FSG-                                                 
031300     EJECT                                                                
031400 01  FILLER                      PIC X(16)   VALUE 'FSG2-AREA-2'.         
031500       EXEC SQL INCLUDE FSG2  END-EXEC.                                   
031000     EJECT                                                                
031100 01  FILLER         PIC X(16) VALUE 'BYART-AREA-1    '.                   
031200*01  -COPY BYART -PRE BYART-                                              
031300     EJECT                                                                
031400 01  FILLER                      PIC X(16)   VALUE 'BYART-AREA-2'.        
031500       EXEC SQL INCLUDE BYART  END-EXEC.                                  
031600     EJECT                                                                
031700                                                                          
031800 LINKAGE SECTION.                                                         
031900*01  -COPY W0009   -PRE MSG-                                              
032000                                                                          
032100*01  -COPY W0008   -PRE USEA-                                             
032200     05  FILLER                  PIC X.                                   
032300                                                                          
032400*01  -COPY W0008   -PRE WDK6-                                             
032500     05  FILLER                  PIC X.                                   
032600                                                                          
032700*01  -COPY W0008   -PRE WDK7-                                             
032800     05  FILLER                  PIC X.                                   
032900                                                                          
033000*01  -COPY W0008   -PRE WDA9-                                             
033100     05  FILLER                  PIC X.                                   
033200                                                                          
033300*01  -COPY W0008   -PRE WDD3B-                                            
033400     05  FILLER                  PIC X.                                   
033500                                                                          
033600*01  -COPY W0008   -PRE 3161-                                             
033700     05  FILLER                  PIC X.                                   
033800                                                                          
033900*01  -COPY W0008   -PRE 3171-                                             
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034200                                                                          
034300 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
034400                           WDK6-PCB WDK7-PCB WDA9-PCB WDD3B-PCB           
034500                           3161-PCB 3171-PCB.                             
034600 MAIN SECTION.                                                            
034700     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
034800                           WDK6-PCB WDK7-PCB WDA9-PCB WDD3B-PCB           
034900                           3161-PCB 3171-PCB.                             
035000                                                                          
035100     PERFORM IMS-GET-MSG                                                  
035200                                                                          
035300     IF SEGMENT-FINNS                                                     
035400       PERFORM A-INIT                                                     
035500       PERFORM B-KOLLA-NYCKLAR                                            
035600       IF NYCKLAR-OK                                                      
035700         IF MFS-UPDATE                                                    
035800           PERFORM G-KOLLA-INPUT                                          
035900           IF INDATA-OK                                                   
036000             PERFORM H-UPPDATERA                                          
036100           END-IF                                                         
036200         END-IF                                                           
036300         IF NYCKLAR-OK AND                                                
036400            INDATA-OK                                                     
036500           PERFORM F-LAES-VISA-INFO                                       
036600         END-IF                                                           
036700       END-IF                                                             
036800       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O16501 + 4                      
036900       PERFORM IMS-INSERT-MSG                                             
037000     END-IF                                                               
037100                                                                          
037200     MOVE ZERO TO RETURN-CODE                                             
037300     GOBACK                                                               
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 A-INIT SECTION.                                                          
037800                                                                          
037900     IF MSG-DUBBLA-TRANSKODER                                             
038000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I16501                 
038100       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
038200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
038300     ELSE                                                                 
038400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I16501                 
038500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
038600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
038700     END-IF                                                               
038800                                                                          
038900     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
039000     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
039100     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
039200                                                                          
039300     MOVE LOW-VALUE                       TO MSG-AREA                     
039400     MOVE 'W3O165N1'                      TO MFS-IDMOD                    
039500     MOVE '3165'                          TO MOD-IDTRANS                  
039600     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
039700                                             MOD-TEMFSINF                 
039800                                                                          
039900     IF EGEN-MID OR HELP-MID                                              
040000       CONTINUE                                                           
040100     ELSE                                                                 
040200       MOVE SPACE                         TO MFS-KDTRTYP                  
040300       MOVE '7'                           TO MFS-IDPFK                    
040400     END-IF                                                               
040500                                                                          
040600     MOVE 'GB'                            TO MED-IDSKYLT                  
040700     PERFORM MFS-FORM-ATTR                                                
040800     .                                                                    
040900     EJECT                                                                
041000                                                                          
041100 B-KOLLA-NYCKLAR SECTION.                                                 
041200     MOVE ALL '+'            TO MSGI-WMSGINIT                             
041300     MOVE '001'              TO MSGI-KDCALL                               
041400     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
041500     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
041600     MOVE '3165'             TO MSGI-IDTRANS                              
041700     IF GODK-MID                                                          
041800       IF MID-IDARTNR-IN = ALL '+'                                        
041900         MOVE MID-IDARTNR-UT TO WS-IDARTNR-9                              
042000       ELSE                                                               
042100         MOVE MID-IDARTNR-IN TO WS-IDARTNR-9                              
042200       END-IF                                                             
042300       MOVE WS-IDARTNR-9     TO MSGI-IDARTNR                              
042400     END-IF                                                               
042500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042600                                                                          
042700     MOVE JA TO NYCKLAR-SW                                                
042800                                                                          
042900*    -- KONTROLL AV IDARTNR                                               
043000     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
043100                                                                          
043200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
043300       MOVE '7'              TO MFS-IDPFK                                 
043400       MOVE SPACE            TO MFS-KDTRTYP                               
043500     END-IF                                                               
043600     INSPECT MSGI-IDARTNR REPLACING ALL     SPACE BY ZERO                 
043700     IF MSGI-IDARTNR NUMERIC                                              
043800       MOVE MSGI-IDARTNR     TO TEST-IDARTNR                              
043900       IF NOT BYT03-OBJEKT AND NOT BYT02-RENOV                            
044000         MOVE NEJ            TO NYCKLAR-SW                                
044100       ELSE                                                               
044200         IF BYT02-RENOV                                                   
044300           SET BYT02-YES     TO TRUE                                      
044400           IF BYT16-BYTES                                                 
044500             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
044600                                    6000                                  
044700             END-COMPUTE                                                  
044800           ELSE                                                           
044900             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
045000                                    1000                                  
045100             END-COMPUTE                                                  
045200           END-IF                                                         
045300         END-IF                                                           
045400         MOVE TEST-IDARTNR   TO W-IDARTNR-K6                              
045500                                W-IDARTNR-K7                              
045600                                W-IDARTNR-A9                              
045700                                W-IDARTNR-D3                              
045800                                W-IDARTNR-3176                            
045900                                W-IDARTNR                                 
046000       END-IF                                                             
046100     ELSE                                                                 
046200       MOVE NEJ              TO NYCKLAR-SW                                
046300     END-IF                                                               
046400                                                                          
046500     IF GODK-MID OR NYCKLAR-OK                                            
046600       MOVE MSGI-IDARTNR(2:8) TO MOD-IDARTNR-UT                           
046700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
046800     ELSE                                                                 
046900       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                           
047000     END-IF                                                               
047100                                                                          
047200     IF NYCKLAR-FEL                                                       
047300       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
047400       CALL WMEDKONV USING MED-WMEDAREA                                   
047500       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
047600       PERFORM MFS-RENSA-FAELT-IN                                         
047700       PERFORM MFS-RENSA-FAELT-UT                                         
047800     END-IF                                                               
047900     .                                                                    
048000     EJECT                                                                
048100                                                                          
048200 F-LAES-VISA-INFO SECTION.                                                
048300                                                                          
048400*    CORE PART LÄSES                                                      
048500     PERFORM IMS-GU-WDK601                                                
048600     IF SEGMENT-SAKNAS                                                    
048700       MOVE ERR-PART-MISSING   TO MED-IDMFSFEL                            
048800       CALL WMEDKONV USING MED-WMEDAREA                                   
048900       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
049000       PERFORM MFS-RENSA-FAELT-UT                                         
049100     ELSE                                                                 
049200                                                                          
049300       IF BYT16-BYTES                                                     
049400         COMPUTE W-IDARTNR-K6-REN = W-IDARTNR-K6 -                        
049500                                    6000                                  
049600       ELSE                                                               
049700         COMPUTE W-IDARTNR-K6-REN = W-IDARTNR-K6 -                        
049800                                    1000                                  
049900       END-IF                                                             
050000                                                                          
050100*    RENOVATED PART LÄSES                                                 
050200       PERFORM IMS-GU-REN-WDK601                                          
050300       IF SEGMENT-FINNS                                                   
050400         PERFORM IMS-GNP-REN-WDK611                                       
050500         MOVE CLAG-KVLS          TO WS-KVLS-REN                           
050600         ADD  CLAG-KVAKS-CDC     TO WS-KVLS-REN                           
050700         ADD  CLAG-KVAKS-PAV     TO WS-KVLS-REN                           
050800         MOVE CLAG-IDANSK        TO MOD-IDANSK                            
050900       END-IF                                                             
051000                                                                          
051100       PERFORM IMS-GNP-REN-WDK628                                         
051200       IF SEGMENT-SAKNAS                                                  
051300         MOVE ZERO             TO BYT-RELARM-FAC                          
051400                                  BYT-RELARM-PER                          
051500                                  BYT-RERETUR                             
051600                                  BYT-REREUSE                             
051700         MOVE SPACE            TO BYT-FLLARM-ACT                          
051800       END-IF                                                             
051900                                                                          
052000       MOVE BYT-RELARM-FAC     TO MOD-RELARM-FAC                          
052100       MOVE BYT-FLLARM-ACT     TO MOD-FLLARM-ACT                          
052200       MOVE BYT-RELARM-PER     TO MOD-RELARM-PER                          
052300       MOVE BYT-RERETUR        TO MOD-RERETUR                             
052400       MOVE BYT-REREUSE        TO MOD-REREUSE                             
052500                                                                          
052600*    CORE PART BENÄMNING LÄSES                                            
052700       PERFORM IMS-GU-WDD311-BSEQ                                         
052800       IF SEGMENT-FINNS                                                   
052900         MOVE TEXT-BEART       TO MOD-BEART-CORE                          
053000       ELSE                                                               
053100         MOVE 'UNKNOWN'        TO MOD-BEART-CORE                          
053200       END-IF                                                             
053300                                                                          
053400       IF BYT16-BYTES                                                     
053500         SUBTRACT 6000         FROM W-IDARTNR-D3                          
053600       ELSE                                                               
053700         SUBTRACT 1000         FROM W-IDARTNR-D3                          
053800       END-IF                                                             
053900                                                                          
054000       MOVE W-IDARTNR-D3       TO MOD-IDARTNR-REN                         
054100                                                                          
054200*    RENOVATED PART BENÄMNING LÄSES                                       
054300       PERFORM IMS-GU-WDD311-BSEQ                                         
054400       IF SEGMENT-FINNS                                                   
054500         MOVE TEXT-BEART       TO MOD-BEART-REN                           
054600       ELSE                                                               
054700         MOVE 'UNKNOWN'        TO MOD-BEART-REN                           
054800       END-IF                                                             
054900                                                                          
055000*      CORE PART LÄSES                                                    
055100       PERFORM IMS-GU-WDK601                                              
055200       MOVE W-IDARTNR-K6       TO MOD-IDARTNR-CORE                        
055300                                                                          
055400       PERFORM IMS-GNP-WDK611                                             
055500       MOVE CLAG-KVLS          TO WS-KVLS-CORE                            
055600       MOVE CLAG-KVPOINT       TO MOD-KVPOINT                             
055700                                                                          
055800*      CORE PART LÄSES                                                    
055900       PERFORM IMS-GU-WDK701                                              
056000       PERFORM UNTIL SEGMENT-SAKNAS                                       
056100         PERFORM IMS-GNP-WDK711                                           
056200         IF SEGMENT-FINNS                                                 
056300           ADD SLAG-KVLS       TO WS-KVLS-CORE                            
056400         END-IF                                                           
056500       END-PERFORM                                                        
056600                                                                          
056700*      RENOVATED PART LÄSES                                               
056800       IF BYT16-BYTES                                                     
056900         SUBTRACT 6000         FROM W-IDARTNR-K7                          
057000       ELSE                                                               
057100         SUBTRACT 1000         FROM W-IDARTNR-K7                          
057200       END-IF                                                             
             MOVE W-IDARTNR-K7       TO   W-IDARTNR-BYART                       
057300                                                                          
057400       PERFORM IMS-GU-WDK701                                              
057500       PERFORM UNTIL SEGMENT-SAKNAS                                       
057600         PERFORM IMS-GNP-WDK711                                           
057700         IF SEGMENT-FINNS                                                 
057800           ADD SLAG-KVLS       TO WS-KVLS-REN                             
057900           ADD SLAG-KVAKS-SDC  TO WS-KVLS-REN                             
058000           ADD SLAG-KVAKS-PAV  TO WS-KVLS-REN                             
058100         END-IF                                                           
058200       END-PERFORM                                                        
058300       MOVE WS-KVLS-REN        TO MOD-KVLS-REN                            
058400                                                                          
058500* CORE PART FOLLOW-UP/RENOVATOR CONFIRMATION LÄSES                        
             PERFORM DB2-SELECT-BYART-TAB                                       
             IF RADER-FINNS                                                     
                MOVE BYART-IDDISTR-RENOV TO W-IDDISTR-A9                        
059000          PERFORM IMS-GU-WDA911                                           
059100          IF SEGMENT-FINNS                                                
059200* FIX SOM UTÖKAS FÖR VARJE RENOVÖR SOM ANSLUTS TILL WEB:EN                
059300             MOVE UPD-IDDISTR    TO TEST-IDDISTR                          
059400             IF DIS134-BYTESREN-WEB                                       
059500                ADD UPD-KVLS-REM  TO WS-KVLS-CORE                         
059600             END-IF                                                       
059700                                                                          
059800             MOVE UPD-IDDISTR  TO W-IDDISTR-3161                          
059900             PERFORM IMS-GU-WDGX3161                                      
060000             IF SEGMENT-FINNS                                             
060100               PERFORM UNTIL SEGMENT-SAKNAS                               
060200                 PERFORM IMS-GNP-WDGX3162                                 
060300                 IF SEGMENT-FINNS                                         
060400                   IF 3162-IDARTNR = W-IDARTNR-A9 AND                     
060500                      3162-IDUSER  = SPACE                                
060600                      ADD 3162-KVAVIS TO WS-KVLS-CORE                     
060800                   END-IF                                                 
060900                 END-IF                                                   
061000               END-PERFORM                                                
061100             END-IF                                                       
061400          END-IF                                                          
061400       END-IF                                                             
061700                                                                          
061800       PERFORM IMS-GU-WDGX3171                                            
061900                                                                          
062000       PERFORM UNTIL WDGX3172-SAKNAS                                      
062100         PERFORM IMS-GNP-WDGX3172                                         
062200                                                                          
062300         IF SEGMENT-FINNS AND                                             
062400            3172-KDTRSTAT < 4                                             
062500           MOVE JA                      TO WDGX3174-SW                    
062600                                                                          
062700           PERFORM UNTIL WDGX3174-SAKNAS                                  
062800             MOVE 3172-IDFAKT           TO W-IDFAKT-3172                  
062900                                                                          
063000             PERFORM IMS-GNP-WDGX3174                                     
063100             IF SEGMENT-FINNS                                             
063200               MOVE 3174-IDKOLLI        TO W-IDKOLLI-3174                 
063300               PERFORM UNTIL SEGMENT-SAKNAS                               
063400                 PERFORM IMS-GNP-WDGX3176                                 
063500                 IF SEGMENT-FINNS                                         
063600                   MOVE 3172-IDDC-REC   TO WS-IDDC                        
063700                   IF SDC-NL-ET                                           
064000                     ADD  3176-KVANTMOT TO WS-KVLS-CORE                   
064100                   END-IF                                                 
064200                 END-IF                                                   
064300               END-PERFORM                                                
064400             ELSE                                                         
064500               MOVE NEJ                 TO WDGX3174-SW                    
064600             END-IF                                                       
064700           END-PERFORM                                                    
064800         ELSE                                                             
064900           IF SEGMENT-SAKNAS                                              
065000             MOVE NEJ                   TO WDGX3172-SW                    
065100           END-IF                                                         
065200         END-IF                                                           
065300       END-PERFORM                                                        
065400                                                                          
065500       MOVE WS-KVLS-CORE       TO MOD-KVLS-CORE                           
065600                                                                          
065700*      RENOVATED PART LÄSES                                               
065800       IF BYT16-BYTES                                                     
065900         SUBTRACT 6000         FROM W-IDARTNR                             
066000       ELSE                                                               
066100         SUBTRACT 1000         FROM W-IDARTNR                             
066200       END-IF                                                             
066300                                                                          
066400       PERFORM DB2-SELECT-FSG2-TAB                                        
066500       IF RADER-SAKNAS                                                    
066600         MOVE ZERO             TO MOD-SULEVANT-RAAR                       
066700       ELSE                                                               
066800         MOVE FSG-SULEVANT-RAAR                                           
066900                               TO MOD-SULEVANT-RAAR                       
067000       END-IF                                                             
067100                                                                          
067200     END-IF                                                               
067300                                                                          
067400     MOVE '002'                TO MSGI-KDCALL                             
067500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
067600     .                                                                    
067700     EJECT                                                                
067800                                                                          
067900 G-KOLLA-INPUT SECTION.                                                   
068000                                                                          
068100*    UPDATE IS ALLOWED ONLY IF THE PART IS A REMANUFACTURED PART          
068200     IF BYT02-YES                                                         
068300*      KONTROLL CORE-RETURN-RATE-PERCENTAGE                               
068400       INSPECT MID-RERETUR REPLACING LEADING SPACE BY ZERO                
068500       IF MID-RERETUR NOT = ALL '+'                                       
068600         IF MID-RERETUR NUMERIC                                           
068700           MOVE MID-RERETUR            TO WS-RERETUR                      
068800                                          MOD-RERETUR                     
068900           MOVE MFS-NUM-FAELT-RAETT    TO MOD-RERETUR-ATTR                
069000         ELSE                                                             
069100           MOVE NEJ                    TO INDATA-SW                       
069200           MOVE MFS-NUM-FAELT-FEL      TO MOD-RERETUR-ATTR                
069300         END-IF                                                           
069400       END-IF                                                             
069500                                                                          
069600*      KONTROLL CORE-REUSE-RATE-PERCENTAGE                                
069700       INSPECT MID-REREUSE REPLACING LEADING SPACE BY ZERO                
069800       IF MID-REREUSE NOT = ALL '+'                                       
069900         IF MID-REREUSE NUMERIC                                           
070000           MOVE MID-REREUSE            TO WS-REREUSE                      
070100                                          MOD-REREUSE                     
070200           MOVE MFS-NUM-FAELT-RAETT    TO MOD-REREUSE-ATTR                
070300         ELSE                                                             
070400           MOVE NEJ                    TO INDATA-SW                       
070500           MOVE MFS-NUM-FAELT-FEL      TO MOD-REREUSE-ATTR                
070600         END-IF                                                           
070700       END-IF                                                             
070800                                                                          
070900*      KONTROLL CORE-ALARM-PERCENTAGE                                     
071000       INSPECT MID-RELARM-PER REPLACING LEADING SPACE BY ZERO             
071100       IF MID-RELARM-PER NOT = ALL '+'                                    
071200         IF MID-RELARM-PER NUMERIC                                        
071300           MOVE MID-RELARM-PER         TO WS-RELARM-PER                   
071400                                          MOD-RELARM-PER                  
071500           MOVE MFS-NUM-FAELT-RAETT    TO MOD-RELARM-PER-ATTR             
071600         ELSE                                                             
071700           MOVE NEJ                    TO INDATA-SW                       
071800           MOVE MFS-NUM-FAELT-FEL      TO MOD-RELARM-PER-ATTR             
071900         END-IF                                                           
072000       END-IF                                                             
072100                                                                          
072200*      KONTROLL ACTIVATE-ALARM-FLAG                                       
072300       IF MID-FLLARM-ACT NOT = ALL '+'                                    
072400         IF MID-FLLARM-ACT = YES OR NEJ                                   
072500           MOVE MID-FLLARM-ACT         TO WS-FLLARM-ACT                   
072600                                          MOD-FLLARM-ACT                  
072700           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLLARM-ACT-ATTR             
072800         ELSE                                                             
072900           MOVE NEJ                    TO INDATA-SW                       
073000           MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLLARM-ACT-ATTR             
073100         END-IF                                                           
073200       END-IF                                                             
073300                                                                          
073400       IF INDATA-FEL                                                      
073500         MOVE ERR-CORR-HILITE-FLDS     TO MED-IDMFSFEL                    
073600         CALL WMEDKONV USING MED-WMEDAREA                                 
073700         MOVE MED-MFSFEL               TO MOD-TEMFSFEL                    
073800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
073900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
074000       END-IF                                                             
074100     ELSE                                                                 
074200       MOVE NEJ                        TO INDATA-SW                       
074300       MOVE ERR-UPDATE-NOT-ALLOWED     TO MED-IDMFSFEL                    
074400       CALL WMEDKONV USING MED-WMEDAREA                                   
074500       MOVE MED-MFSFEL                 TO MOD-TEMFSFEL                    
074600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
074700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100                                                                          
075200 H-UPPDATERA SECTION.                                                     
075300     IF BYT16-BYTES                                                       
075400       COMPUTE W-IDARTNR-K6-REN = W-IDARTNR-K6 -                          
075500                                  6000                                    
075600     ELSE                                                                 
075700       COMPUTE W-IDARTNR-K6-REN = W-IDARTNR-K6 -                          
075800                                  1000                                    
075900     END-IF                                                               
076000                                                                          
076100     PERFORM IMS-GHU-REN-WDK628                                           
076200                                                                          
076300     IF SEGMENT-FINNS                                                     
076400       IF MID-RERETUR NOT = ALL '+'                                       
076500         MOVE WS-RERETUR       TO BYT-RERETUR                             
076600       END-IF                                                             
076700                                                                          
076800       IF MID-REREUSE NOT = ALL '+'                                       
076900         MOVE WS-REREUSE       TO BYT-REREUSE                             
077000       END-IF                                                             
077100                                                                          
077200       IF MID-RERETUR NOT = ALL '+' OR                                    
077300          MID-REREUSE NOT = ALL '+'                                       
077400         COMPUTE WS-RELARM-FAC  ROUNDED = (BYT-RERETUR / 100) *           
077500                                          (BYT-REREUSE / 100)             
               IF WS-RELARM-FAC = ZERO AND                                      
                  BYT-RERETUR > ZERO AND                                        
                  BYT-REREUSE > ZERO                                            
                  MOVE 0.01 TO WS-RELARM-FAC                                    
               END-IF                                                           
077600         MOVE WS-RELARM-FAC    TO BYT-RELARM-FAC                          
077700       END-IF                                                             
077800                                                                          
077900       IF MID-RELARM-PER NOT = ALL '+'                                    
078000         MOVE WS-RELARM-PER    TO BYT-RELARM-PER                          
078100       END-IF                                                             
078200                                                                          
078300       IF MID-FLLARM-ACT NOT = ALL '+'                                    
078400         MOVE WS-FLLARM-ACT    TO BYT-FLLARM-ACT                          
078500       END-IF                                                             
078600                                                                          
078700       PERFORM IMS-REPL-REN-WDK628                                        
078800     ELSE                                                                 
078900       PERFORM IMS-GHU-REN-WDK611                                         
079000                                                                          
079100       MOVE W-IDARTNR-K6-REN TO BYT-IDARTNR                               
079200       MOVE ZERO             TO BYT-DAREGDAT                              
079300                                BYT-KVVECKOR                              
079400                                BYT-TIKLOCK                               
079500       MOVE SPACE            TO BYT-IDUSER                                
079600       MOVE WS-RERETUR       TO BYT-RERETUR                               
079700       MOVE WS-REREUSE       TO BYT-REREUSE                               
079800       COMPUTE WS-RELARM-FAC ROUNDED = (BYT-RERETUR / 100) *              
079900                                       (BYT-REREUSE / 100)                
080000       MOVE WS-RELARM-FAC    TO BYT-RELARM-FAC                            
080100       MOVE WS-RELARM-PER    TO BYT-RELARM-PER                            
080200       MOVE WS-FLLARM-ACT    TO BYT-FLLARM-ACT                            
080300       PERFORM IMS-ISRT-REN-WDK628                                        
080400     END-IF                                                               
080500     MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                                
080600     CALL WMEDKONV USING MED-WMEDAREA                                     
080700     MOVE MED-MFSINF       TO MOD-TEMFSINF                                
080800     PERFORM MFS-FORM-ATTR                                                
080900     PERFORM MFS-RENSA-FAELT-IN                                           
081000     .                                                                    
081100     EJECT                                                                
081200                                                                          
081300 MFS-RENSA-FAELT-UT SECTION.                                              
081400*    --- ALLA UTDATA-FÄLT                                                 
081500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-CORE                             
081600                             MOD-BEART-CORE                               
081700                             MOD-IDARTNR-REN                              
081800                             MOD-BEART-REN                                
081900                             MOD-IDANSK                                   
082000                             MOD-RELARM-FAC                               
082100                             MOD-KVPOINT                                  
082200                             MOD-RERETUR                                  
082300                             MOD-REREUSE                                  
082400                             MOD-RELARM-PER                               
082500                             MOD-KVLS-REN                                 
082600                             MOD-KVLS-CORE                                
082700                             MOD-SULEVANT-RAAR                            
082800                             MOD-FLLARM-ACT                               
082900     .                                                                    
083000                                                                          
083100 MFS-RENSA-FAELT-IN SECTION.                                              
083200*    --- ALLA INDATA-FÄLT                                                 
083300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
083400                             MOD-RERETUR                                  
083500                             MOD-REREUSE                                  
083600                             MOD-RELARM-PER                               
083700                             MOD-FLLARM-ACT                               
083800     .                                                                    
083900                                                                          
084000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
084100*    --- ALLA UTDATA-FÄLT                                                 
084200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-CORE                           
084300                               MOD-BEART-CORE                             
084400                               MOD-IDARTNR-REN                            
084500                               MOD-BEART-REN                              
084600                               MOD-IDANSK                                 
084700                               MOD-RELARM-FAC                             
084800                               MOD-KVPOINT                                
084900                               MOD-RERETUR                                
085000                               MOD-REREUSE                                
085100                               MOD-RELARM-PER                             
085200                               MOD-KVLS-REN                               
085300                               MOD-KVLS-CORE                              
085400                               MOD-SULEVANT-RAAR                          
085500                               MOD-FLLARM-ACT                             
085600     .                                                                    
085700                                                                          
085800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
085900*    --- ALLA INDATA-FÄLT                                                 
086000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
086100                               MOD-RERETUR                                
086200                               MOD-REREUSE                                
086300                               MOD-RELARM-PER                             
086400                               MOD-FLLARM-ACT                             
086500     .                                                                    
086600     EJECT                                                                
086700                                                                          
086800 MFS-FORM-ATTR SECTION.                                                   
086900*    --- ALLA INDATA-FÄLT                                                 
087000     MOVE MFS-FORMATETS-ATTR TO MOD-RERETUR-ATTR                          
087100                                MOD-REREUSE-ATTR                          
087200                                MOD-RELARM-PER-ATTR                       
087300                                MOD-FLLARM-ACT-ATTR                       
087400                                                                          
087500     .                                                                    
087600     EJECT                                                                
087700                                                                          
087800* --- IMS SEKTIONER ---                                                   
087900 IMS-GET-MSG SECTION.                                                     
088000     MOVE '  QC'          TO GODK-STATUSKODER                             
088100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
088200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088500                                                                          
088600 IMS-INSERT-MSG SECTION.                                                  
088700     IF MSGI-IDLAND-SPR = 'GB'                                            
088800       MOVE 'N'           TO MFS-KDHUVOMR                                 
088900     END-IF                                                               
089000     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
089100     MOVE SPACE           TO GODK-STATUSKODER                             
089200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
089300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089400     PERFORM IMS-STATUSKONTROLL                                           
089500     .                                                                    
089600     EJECT                                                                
089700                                                                          
089800 IMS-GU-REN-WDK601 SECTION.                                               
089900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-REN-X ')'                  
090000          DELIMITED BY SIZE INTO SSA1                                     
090100     MOVE '  GE'           TO GODK-STATUSKODER                            
090200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
090300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600                                                                          
090700 IMS-GNP-REN-WDK611 SECTION.                                              
090800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-REN-X ')'                 
090900          DELIMITED BY SIZE INTO SSA1                                     
091000     MOVE '  '             TO GODK-STATUSKODER                            
091100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
091200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
091300     PERFORM IMS-STATUSKONTROLL                                           
091400     .                                                                    
091500                                                                          
091600 IMS-GU-WDK601 SECTION.                                                   
091700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
091800          DELIMITED BY SIZE INTO SSA1                                     
091900     MOVE '  GE'           TO GODK-STATUSKODER                            
092000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
092100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
092200     PERFORM IMS-STATUSKONTROLL                                           
092300     .                                                                    
092400                                                                          
092500 IMS-GNP-WDK611 SECTION.                                                  
092600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-X ')'                     
092700          DELIMITED BY SIZE INTO SSA1                                     
092800     MOVE '  '             TO GODK-STATUSKODER                            
092900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
093000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
093100     PERFORM IMS-STATUSKONTROLL                                           
093200     .                                                                    
093300                                                                          
093400 IMS-GNP-REN-WDK628 SECTION.                                              
093500     STRING 'WDK628  (IDARTNR  =' W-IDARTNR-K6-REN-X ')'                  
093600          DELIMITED BY SIZE INTO SSA1                                     
093700     MOVE '  GE'           TO GODK-STATUSKODER                            
093800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK628 SSA1                   
093900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200                                                                          
094300 IMS-GHU-REN-WDK611 SECTION.                                              
094400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-REN-X ')'                  
094500          DELIMITED BY SIZE INTO SSA1                                     
094600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-REN-X ')'                 
094700          DELIMITED BY SIZE INTO SSA2                                     
094800     MOVE '  '             TO GODK-STATUSKODER                            
094900     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
095000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
095100     PERFORM IMS-STATUSKONTROLL                                           
095200     .                                                                    
095300                                                                          
095400 IMS-GHU-REN-WDK628 SECTION.                                              
095500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-REN-X ')'                  
095600          DELIMITED BY SIZE INTO SSA1                                     
095700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-REN-X ')'                 
095800          DELIMITED BY SIZE INTO SSA2                                     
095900     STRING 'WDK628  (IDARTNR  =' W-IDARTNR-K6-REN-X ')'                  
096000          DELIMITED BY SIZE INTO SSA3                                     
096100     MOVE '  GE'           TO GODK-STATUSKODER                            
096200     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK628 SSA1 SSA2 SSA3         
096300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
096400     PERFORM IMS-STATUSKONTROLL                                           
096500     .                                                                    
096600                                                                          
096700 IMS-REPL-REN-WDK628 SECTION.                                             
096800     MOVE '  '             TO GODK-STATUSKODER                            
096900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK628                       
097000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
097100     PERFORM IMS-STATUSKONTROLL                                           
097200     .                                                                    
097300                                                                          
097400 IMS-ISRT-REN-WDK628 SECTION.                                             
097500     MOVE 'WDK628   '      TO SSA1                                        
097600     MOVE '  '             TO GODK-STATUSKODER                            
097700     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK628 SSA1                  
097800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100     EJECT                                                                
098200                                                                          
098300 IMS-GU-WDK701 SECTION.                                                   
098400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     MOVE '  GE'           TO GODK-STATUSKODER                            
098700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
098800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
098900     PERFORM IMS-STATUSKONTROLL                                           
099000     .                                                                    
099100                                                                          
099200 IMS-GNP-WDK711 SECTION.                                                  
099300     MOVE 'WDK711   '      TO SSA1                                        
099400     MOVE '  GE'           TO GODK-STATUSKODER                            
099500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
099600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
099700     PERFORM IMS-STATUSKONTROLL                                           
099800     .                                                                    
099900     EJECT                                                                
100000                                                                          
100100 IMS-GU-WDGX3161 SECTION.                                                 
100200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3161-X ')'                    
100300          DELIMITED BY SIZE INTO SSA1                                     
100400     MOVE '  GE'           TO GODK-STATUSKODER                            
100500     CALL CBLTDLI USING GU  3161-PCB DLI-IO-WDGX3161 SSA1                 
100600     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
100700     PERFORM IMS-STATUSKONTROLL                                           
100800     .                                                                    
100900                                                                          
101000 IMS-GNP-WDGX3162 SECTION.                                                
101100     MOVE 'WDGX3162  '     TO SSA1                                        
101200     MOVE '  GE'           TO GODK-STATUSKODER                            
101300     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
101400     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     EJECT                                                                
101800                                                                          
101900 IMS-GU-WDA911 SECTION.                                                   
102000     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-A9-X ')'                      
102100          DELIMITED BY SIZE INTO SSA1                                     
102000     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-A9-X ')'                      
102100          DELIMITED BY SIZE INTO SSA2                                     
102200     MOVE '  GE'           TO GODK-STATUSKODER                            
102300     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA911 SSA1 SSA2               
102400     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700 IMS-GU-WDD311-BSEQ SECTION.                                              
103800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-D3-X ')'                      
103900     DELIMITED BY SIZE INTO SSA1                                          
104000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-D3-X ')'                      
104100     DELIMITED BY SIZE INTO SSA2                                          
104200     MOVE '  GE'            TO GODK-STATUSKODER                           
104300     CALL CBLTDLI USING GU WDD3B-PCB DLI-IO-WDD311 SSA1 SSA2              
104400     MOVE WDD3B-STATUS-CODE TO STATUS-WS                                  
104500     PERFORM IMS-STATUSKONTROLL                                           
104600     .                                                                    
104700     EJECT                                                                
104800                                                                          
104900 IMS-GU-WDGX3171 SECTION.                                                 
105000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
105100          DELIMITED BY SIZE INTO SSA1                                     
105200     MOVE '  '             TO GODK-STATUSKODER                            
105300     CALL CBLTDLI USING GU  3171-PCB DLI-IO-WDGX3171 SSA1                 
105400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     .                                                                    
105700                                                                          
105800 IMS-GNP-WDGX3172 SECTION.                                                
105900     MOVE 'WDGX3172  '     TO SSA1                                        
106000     MOVE '  GE'           TO GODK-STATUSKODER                            
106100     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3172 SSA1                 
106200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
106300     PERFORM IMS-STATUSKONTROLL                                           
106400     .                                                                    
106500                                                                          
106600 IMS-GNP-WDGX3174 SECTION.                                                
106700     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
106800          DELIMITED BY SIZE INTO SSA1                                     
106900     MOVE 'WDGX3174  '     TO SSA2                                        
107000     MOVE '  GE'           TO GODK-STATUSKODER                            
107100     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3174 SSA1 SSA2            
107200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500                                                                          
107600 IMS-GNP-WDGX3176 SECTION.                                                
107700     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
107800          DELIMITED BY SIZE INTO SSA1                                     
107900     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-3174-X ')'                    
108000          DELIMITED BY SIZE INTO SSA2                                     
108100     STRING 'WDGX3176(IDARTNRO =' W-IDARTNR-3176-X ')'                    
108200          DELIMITED BY SIZE INTO SSA3                                     
108300     MOVE '  GE'           TO GODK-STATUSKODER                            
108400     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3176 SSA1 SSA2            
108500                                                     SSA3                 
108600     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
108700     PERFORM IMS-STATUSKONTROLL                                           
108800     .                                                                    
108900     EJECT                                                                
109000                                                                          
109100 IMS-STATUSKONTROLL SECTION.                                              
109200     SET STATUS-IX TO 1                                                   
109300     SEARCH GODK-STATUS                                                   
109400       AT END                                                             
109500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
109600         DELIMITED BY SIZE INTO FELTEXT                                   
109700         CALL FELLOG                                                      
109800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
109900         CONTINUE                                                         
110000     END-SEARCH                                                           
110100     .                                                                    
110200 DB2-SELECT-FSG2-TAB SECTION.                                             
110300                                                                          
110400     MOVE 000100           TO GODK-SQLCODEKODER                           
110500     EXEC SQL                                                             
110600         SELECT SULEVANT_RAAR                                             
110700         INTO :FSG-SULEVANT-RAAR                                          
110800         FROM FSG2                                                        
110900         WHERE IDARTNR = :W-IDARTNR                                       
111000     END-EXEC                                                             
111100     MOVE SQLCODE          TO SQLCODE-WS                                  
111200     PERFORM DB2-STATUSKONTROLL                                           
111300     .                                                                    
110200 DB2-SELECT-BYART-TAB SECTION.                                            
110300                                                                          
110400     MOVE 000100           TO GODK-SQLCODEKODER                           
110500     EXEC SQL                                                             
110600         SELECT IDDISTR_RENOV                                             
110700         INTO :BYART-IDDISTR-RENOV                                        
110800         FROM BYART                                                       
110900         WHERE IDARTNR_BYT = :W-IDARTNR-BYART                             
111000     END-EXEC                                                             
111100     MOVE SQLCODE          TO SQLCODE-WS                                  
111200     PERFORM DB2-STATUSKONTROLL                                           
111300     .                                                                    
111400     EJECT                                                                
111500 DB2-STATUSKONTROLL  SECTION.                                             
111600                                                                          
111700     SET SQLCODE-IX TO 1                                                  
111800     SEARCH GODK-SQLCODE                                                  
111900       AT END CALL FELLOG                                                 
112000       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
112100     END-SEARCH                                                           
112200     .                                                                    
