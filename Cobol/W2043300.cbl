000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2043300.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   12/09/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING ANSKAFFNINGSDATA                                     
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDB6                                       
001100*        PROGRAMMET LÄSER      WDD2                                       
001200*        PROGRAMMET LÄSER      WDD3                                       
001300*        PROGRAMMET LÄSER      WDD7                                       
001400*        PROGRAMMET LÄSER      WDK6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T433                                              
001800*        MID:         W2I43301                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O43301                                            
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800*    -COPY WY2000W1                                                       
002900*    -COPY WY2000W2                                                       
003000                                                                          
003100 77  IDPGM                       PIC X(08)   VALUE 'W2043300'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003900                                                                          
004000 01  CURRENT-SECTION             PIC X(24)   VALUE SPACE.                 
004100 01  CURRENT-IMS-SECTION         PIC X(24)   VALUE SPACE.                 
004200                                                                          
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400                                                                          
004500 01  WS-TILEVBEG-AAMMDD-X.                                                
004600     03 WS-TILEVBEG-AAMMDD       PIC 9(6)    VALUE ZERO.                  
004700                                                                          
004800 01  WS-TILEVBEG-X.                                                       
004900     03 WS-TILEVBEG              PIC 9(4)    VALUE ZERO.                  
005000                                                                          
005100 01  WS-CURRENT-WEEK-X.                                                   
005200     03 WS-CURRENT-WEEK          PIC 9(4)    VALUE ZERO.                  
005300                                                                          
005400 01  WS-KVPROG-X.                                                         
005500     03 WS-KVPROG                PIC 9(7)    VALUE ZERO.                  
005600                                                                          
005700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005800     88  INDATA-OK                           VALUE 'J'.                   
005900     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006200     88  NYCKLAR-OK                          VALUE 'J'.                   
006300     88  NYCKLAR-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77  UPD-SW                      PIC X       VALUE 'N'.                   
006600     88  UPPDATERING-GJORD                   VALUE 'J'.                   
006700     88  INGEN-UPPDATERING-GJORD             VALUE 'N'.                   
006800                                                                          
006900 77  WS-KDPRODSL-TEST            PIC 9(02).                               
007000     88 KDPRODSL-LYNC                        VALUE 31 33 34 35            
007100                                                   36 37 38 39.           
007200                                                                          
007300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007400     88  EGEN-MID                            VALUE '2433'.                
007500     88  GODK-MID                            VALUE '2431' '2432'          
007600                                                   '2433' '2434'          
007700                                                   '2435' '2436'          
007800                                                   '2437' '2438'          
007900                                                   '2439' '2441'.         
008000     88  HELP-MID                            VALUE '0551'.                
008100                                                                          
008200                                                                          
008300 01  WS-VARIABLES.                                                        
008400     03 WS-TEST-EMBQ2            PIC 9(2)    VALUE ZERO.                  
008500        88 GOOD-EMBQ2                        VALUE 20 25 30               
008600                                                   35 40 45               
008700                                                   50 80.                 
008800     03 WS-TEST-BEFT             PIC 9(2)    VALUE ZERO.                  
008900        88 GOOD-BEFT                         VALUE 93 95 98               
009000                                                   99.                    
009100     03 WS-PRMATRL               PIC S9(7)V9(2) COMP-3                    
009200                                             VALUE ZERO.                  
009300     03 WS-PRMATRL-EDIT          PIC S9(7)V9(2)                           
009400                                             VALUE ZERO.                  
009500     03 WS-CDC-PRICE             PIC S9(7)V9(2)      COMP-3               
009600                                             VALUE ZERO.                  
009700     03 WS-KVPB-REF              PIC S9(6)V9         COMP-3               
009800                                             VALUE ZERO.                  
009900     03 WS-DAPUBL-TIAAMMDD       PIC 9(6)    VALUE ZERO.                  
010000     03 WS-TILEVBEG-AAVVD        PIC 9(5)    VALUE ZERO.                  
010100     03 WS-KDANSKQ               PIC X(1)    VALUE ZERO.                  
010200     03 WS-STRECK-1              PIC X(1)    VALUE '-'.                   
010300     03 WS-IDINK                 PIC 9(3)    VALUE ZERO.                  
010400     03 WS-INDX                  PIC 9(2)    VALUE ZERO.                  
010500     03 WS-MAX-INDX              PIC 9(2)    VALUE 12.                    
010600     03 SW-FOUND                 PIC X(1)    VALUE 'N'.                   
010700     03 WS-CURRENT-YYWWD         PIC 9(5)    VALUE ZERO.                  
010800     03 WS-CURRENT-YYWWD-2AAR    PIC 9(5)    VALUE ZERO.                  
010900     EJECT                                                                
011000                                                                          
011100*01 -COPY WWPRODSL                                                        
011200                                                                          
011300 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
011400*01 -COPY WWIDFTG                                                         
011500     EJECT                                                                
011600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011700 01  GENERELLA-SUBPROGRAM.                                                
011800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012100     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
012200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
012500                                                                          
012600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012700 01  FILLER               PIC X(16)   VALUE  'WMEDKONV-AREA'.             
012800*01 -COPY WMEDAREA                                                        
012900                                                                          
013000*    ---- PARAMETRAR TILL DATUM-KONVERTERING                              
013100 01  FILLER               PIC X(16)   VALUE  'WDATKONV-AREA'.             
013200*01  -COPY WDATAREA.                                                      
013300                                                                          
013400*01  -COPY WDECAREA.                                                      
013500                                                                          
013600*01  -COPY WWDCKONS.                                                      
013700                                                                          
013800*01  -COPY WWLNDKON.                                                      
013900                                                                          
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
014200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014300     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
014400     03  ERR-EJ-NUMERISK         PIC X(3)    VALUE '020'.                 
014500     03  ERR-DC-SAKNAS           PIC X(3)    VALUE '026'.                 
014600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014700     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
014800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015000                                                                          
015100 01  INFO-MEDDELANDEN.                                                    
015200     03  MED-1                   PIC X(30) VALUE                          
015300            'LYNC & CO PART              '.                               
015400                                                                          
015500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015800*01 -COPY WMSGINIT                                                        
015900                                                                          
016000*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
016100*                                                                         
016200 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
016300*01 -COPY W005WDK7                                                        
016400                                                                          
016500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016800*01  MID -COPY W2I43301                                                   
016900                                                                          
017000                                                                          
017100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017200*01  -COPY WMSGAREA                                                       
017300     03  MOD REDEFINES MSG-AREA.                                          
017400*      05  -COPY W2O43301 -PRE MOD-                                       
017500                                                                          
017600*                                                                         
017700 01  FILLER                      PIC X(16)   VALUE 'MID-2441'.            
017800*01  MID -COPY W2I44101         -PRE 2441-                                
017900                                                                          
018000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018100*01  -COPY WMFSAREA                                                       
018200                                                                          
018300                                                                          
018400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018500*                                                                         
018600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018700 01  NYCKLAR-TILL-DLI.                                                    
018800     03  W-IDDC-X.                                                        
018900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019000     03  W-IDARTNR-X.                                                     
019100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019200                                                                          
019300     03  W-WDD7A1KY-MIN.                                                  
019400         05  W-IDARTNR-MIN7       PIC S9(9)  COMP-3 VALUE ZERO.           
019500         05  FILLER               PIC X(7)   VALUE LOW-VALUE.             
019600                                                                          
019700     03  W-WDD7A1KY-MAX.                                                  
019800         05  W-IDARTNR-MAX7       PIC S9(9)  COMP-3 VALUE ZERO.           
019900         05  FILLER               PIC X(7)   VALUE HIGH-VALUE.            
020000                                                                          
020100     03  W-IDBENNR-X.                                                     
020200         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
020300     03  W-IDSKYLT-X.                                                     
020400         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
020500     03  W-IDKORTNR-X.                                                    
020600         05  W-IDKORTNR          PIC S9(3)   VALUE ZERO COMP-3.           
020700     03  W-KDNOTTYP-X.                                                    
020800         05  W-KDNOTTYP          PIC S9(1)   VALUE ZERO COMP-3.           
020900     03  W-IDLAND-X.                                                      
021000         05  W-IDLANDX2          PIC X(2)    VALUE SPACES.                
021100                                                                          
021200     03  W-1141KEY-X.                                                     
021300         05  FILLER               PIC X(04)  VALUE '1141'.                
021400         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
021500                                                                          
021600     03  W-1142KEY-X.                                                     
021700         05  FILLER               PIC X(01)  VALUE '5'.                   
021800                                                                          
021900*    --- STATUS KODER FRÅN IMS                                            
022000 01  STATUS-WS                   PIC XX.                                  
022100     88  SEGMENT-FINNS                       VALUE '  '.                  
022200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022400                                                                          
022500                                                                          
022600 01  GODK-STATUSKODER.                                                    
022700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022800                                                                          
022900                                                                          
023000 01  SSA1                        PIC X(64).                               
023100 01  SSA2                        PIC X(64).                               
023200 01  SSA3                        PIC X(64).                               
023300                                                                          
023400                                                                          
023500*    --- IMS FUNKTIONSKODER                                               
023600*01  -COPY W0003                                                          
023700                                                                          
023800*    ---  DLI INPUT-OUTPUT AREA                                           
023900                                                                          
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
024100 01  DLI-IO-WDB601.                                                       
024200*    03  -COPY WDB601                                                     
024300                                                                          
024400                                                                          
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
024600 01  DLI-IO-WDD201.                                                       
024700*    03  -COPY WDD201 -PRE D201-                                          
024800                                                                          
024900                                                                          
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
025100 01  DLI-IO-WDD301.                                                       
025200*    03  -COPY WDD301                                                     
025300                                                                          
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
025500 01  DLI-IO-WDD311.                                                       
025600*    03  -COPY WDD311                                                     
025700                                                                          
025800                                                                          
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD7A1'.                      
026000 01  DLI-IO-WDD7A1.                                                       
026100*    03  -COPY WDD7A1                                                     
026200                                                                          
026300                                                                          
026400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
026500 01  DLI-IO-WDK601.                                                       
026600*    03  -COPY WDK601                                                     
026700                                                                          
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
026900 01  DLI-IO-WDK611.                                                       
027000*    03  -COPY WDK611                                                     
027100                                                                          
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK625'.                      
027300 01  DLI-IO-WDK625.                                                       
027400*    03  -COPY WDK625                                                     
027500                                                                          
027600                                                                          
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
027800 01  DLI-IO-WDK711.                                                       
027900*    03  -COPY WDK711                                                     
028000                                                                          
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
028200 01  DLI-IO-WDK712.                                                       
028300*    03  -COPY WDK712                                                     
028400                                                                          
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
028600 01  DLI-IO-WDK722.                                                       
028700*    03  -COPY WDK722                                                     
028800                                                                          
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
029000 01  DLI-IO-WDC901.                                                       
029100*    03  -COPY WDC901                                                     
029200                                                                          
029300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1142'.                    
029400 01  DLI-IO-WDGX1142.                                                     
029500*    03  -COPY WDGX1142                                                   
029600                                                                          
029700 LINKAGE SECTION.                                                         
029800*01  -COPY W0009   -PRE MSG-                                              
029900*01  -COPY W0008   -PRE WDP7-                                             
030000     05  FILLER                  PIC X.                                   
030100                                                                          
030200*01  -COPY W0008  -PRE WDB6-                                              
030300     05  FILLER                  PIC X.                                   
030400                                                                          
030500*01  -COPY W0008  -PRE WDD2-                                              
030600     05  FILLER                  PIC X.                                   
030700                                                                          
030800*01  -COPY W0008  -PRE WDD3-                                              
030900     05  FILLER                  PIC X.                                   
031000                                                                          
031100*01  -COPY W0008  -PRE WDD7-                                              
031200     05  FILLER                  PIC X.                                   
031300                                                                          
031400*01  -COPY W0008  -PRE WDK6-                                              
031500     05  FILLER                  PIC X.                                   
031600                                                                          
031700*01  -COPY W0008  -PRE WDK7-                                              
031800     05  FILLER                  PIC X.                                   
031900                                                                          
032000*01  -COPY W0008  -PRE WDK7-2-                                            
032100     05  FILLER                  PIC X.                                   
032200                                                                          
032300*01  -COPY W0008  -PRE WDC9-                                              
032400     05  FILLER                  PIC X.                                   
032500                                                                          
032600*01  -COPY W0008  -PRE WDG2-                                              
032700     05  FILLER                  PIC X.                                   
032800                                                                          
032900                                                                          
033000 PROCEDURE DIVISION  USING MSG-PCB  WDP7-PCB WDB6-PCB WDD2-PCB            
033100                           WDD3-PCB WDD7-PCB WDK6-PCB WDK7-PCB            
033200                           WDK7-2-PCB        WDC9-PCB WDG2-PCB.           
033300                                                                          
033400 MAIN SECTION.                                                            
033500     ENTRY 'DLITCBL' USING MSG-PCB  WDP7-PCB WDB6-PCB WDD2-PCB            
033600                           WDD3-PCB WDD7-PCB WDK6-PCB WDK7-PCB            
033700                           WDK7-2-PCB        WDC9-PCB WDG2-PCB.           
033800                                                                          
033900     PERFORM IMS-GET-MSG                                                  
034000     IF SEGMENT-FINNS                                                     
034100        PERFORM A-INIT                                                    
034200        PERFORM B-KOLLA-NYCKLAR                                           
034300        IF NYCKLAR-OK                                                     
034400           IF MFS-UPDATE                                                  
034500              PERFORM G-KOLLA-INPUT                                       
034600              IF INDATA-OK AND UPPDATERING-GJORD                          
034700                 PERFORM H-UPDATE                                         
034800              END-IF                                                      
034900           ELSE                                                           
035000              PERFORM E-SAMMA-SIDA                                        
035100           END-IF                                                         
035200           IF INDATA-OK                                                   
035300              PERFORM F-LAES-VISA-INFO                                    
035400           END-IF                                                         
035500        END-IF                                                            
035600        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O43301 + 4                     
035700        PERFORM IMS-INSERT-MSG                                            
035800     END-IF                                                               
035900                                                                          
036000     MOVE ZERO TO RETURN-CODE                                             
036100     GOBACK                                                               
036200     .                                                                    
036300                                                                          
036400                                                                          
036500 A-INIT SECTION.                                                          
036600     MOVE 'A-INIT'           TO CURRENT-SECTION                           
036700                                                                          
036800     IF MSG-DUBBLA-TRANSKODER                                             
036900        MOVE MSG-IDTRANS-2                    TO MFS-IDTRANS              
037000        IF MFS-IDTRANS = '2441'                                           
037100           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO 2441-MID                 
037200        ELSE                                                              
037300           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I43301             
037400        END-IF                                                            
037500        MOVE MSG-KDMFSFOR-2                   TO MFS-KDMFSFOR             
037600     ELSE                                                                 
037700        MOVE MSG-IDTRANS-1                    TO MFS-IDTRANS              
037800        IF MFS-IDTRANS = '2441'                                           
037900           MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO 2441-MID                 
038000        ELSE                                                              
038100           MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I43301             
038200        END-IF                                                            
038300        MOVE MSG-KDMFSFOR-1  TO MFS-KDMFSFOR                              
038400     END-IF                                                               
038500                                                                          
038600     MOVE MSG-KDTRTYP        TO MFS-KDTRTYP                               
038700     MOVE MSG-IDPFK          TO MFS-IDPFK                                 
038800     MOVE MFS-IDTRANS        TO W-IDTRANS                                 
038900                                                                          
039000     MOVE LOW-VALUE          TO MSG-AREA                                  
039100     MOVE 'W2O433N1'         TO MFS-IDMOD                                 
039200     MOVE '2433'             TO MOD-IDTRANS                               
039300     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSFEL MOD-TEMFSINF                 
039400                                                                          
039500     IF MFS-IDTRANS = '2441'                                              
039600        MOVE 1              TO WS-INDX                                    
039700        PERFORM UNTIL WS-INDX > WS-MAX-INDX OR                            
039800                      SW-FOUND = JA                                       
039900           IF 2441-MID-KDCMD(WS-INDX) = ALL '+'                           
040000              ADD 1         TO WS-INDX                                    
040100           ELSE                                                           
040200              INSPECT 2441-MID-IDARTNR (WS-INDX) REPLACING                
040300                      LEADING SPACE BY ZERO                               
040400              MOVE 2441-MID-IDARTNR (WS-INDX)                             
040500                                 TO MID-IDARTNR-IN                        
040600              MOVE 2441-MID-IDDC (WS-INDX)                                
040700                                 TO MID-IDDC-IN                           
040800              MOVE JA            TO SW-FOUND                              
040900           END-IF                                                         
041000        END-PERFORM                                                       
041100     ELSE                                                                 
041200       IF EGEN-MID OR HELP-MID                                            
041300          CONTINUE                                                        
041400       ELSE                                                               
041500          MOVE SPACE           TO MFS-KDTRTYP                             
041600          MOVE '7'             TO MFS-IDPFK                               
041700       END-IF                                                             
041800     END-IF                                                               
041900                                                                          
042000     ACCEPT DAGENS-DATUM FROM DATE                                        
042100                                                                          
042200     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
042300     MOVE DAGENS-DATUM          TO DAT-I-TIDATUM                          
042400                                                                          
042500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
042600                         DAT-O-TIDATUM DAT-KDSVAR                         
042700                                                                          
042800     IF DAT-KDSVAR-OK                                                     
042900        MOVE DAT-TIAAVVD        TO WS-CURRENT-YYWWD                       
043000     END-IF                                                               
043100     .                                                                    
043200                                                                          
043300                                                                          
043400 B-KOLLA-NYCKLAR SECTION.                                                 
043500     MOVE 'B-KOLLA-NYCKLAR'  TO CURRENT-SECTION                           
043600                                                                          
043700     MOVE ALL '+'            TO MSGI-WMSGINIT                             
043800     MOVE '001'              TO MSGI-KDCALL                               
043900     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
044000     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
044100     MOVE '2433'             TO MSGI-IDTRANS                              
044200     IF GODK-MID                                                          
044300        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
044400        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
044500     END-IF                                                               
044600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
044700                                                                          
044800*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
044900     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
045000                                                                          
045100     MOVE JA                 TO NYCKLAR-SW                                
045200     MOVE SPACE              TO MED-IDMFSFEL                              
045300                                                                          
045400*    -- KONTROLL AV IDARTNR                                               
045500     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
045600                                                                          
045700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
045800        MOVE '7'             TO MFS-IDPFK                                 
045900        MOVE SPACE           TO MFS-KDTRTYP                               
046000     END-IF                                                               
046100                                                                          
046200     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
046300     IF MSGI-IDARTNR NUMERIC                                              
046400        MOVE MSGI-IDARTNR    TO W-IDARTNR                                 
046500        PERFORM IMS-GU-WDK601                                             
046600        IF SEGMENT-SAKNAS                                                 
046700           MOVE ERR-ARTIKEL-SAKNAS                                        
046800                             TO MED-IDMFSFEL                              
046900           MOVE NEJ          TO NYCKLAR-SW                                
047000           PERFORM MFS-CLOSE-FIELD-IN                                     
047100        ELSE                                                              
047200           MOVE ART-KDPRODSL TO WS-KDPRODSL-TEST                          
047300           IF KDPRODSL-LYNC                                               
047400              MOVE NEJ       TO NYCKLAR-SW                                
047500              PERFORM MFS-CLOSE-FIELD-IN                                  
047600           END-IF                                                         
047700        END-IF                                                            
047800     ELSE                                                                 
047900        MOVE ERR-EJ-NUMERISK TO MED-IDMFSFEL                              
048000        MOVE NEJ TO NYCKLAR-SW                                            
048100     END-IF                                                               
048200                                                                          
048300     INSPECT MID-IDARTNR-IN REPLACING LEADING ZERO BY SPACE               
048400                                                                          
048500*    -- KONTROLL AV IDDC                                                  
048600     IF NYCKLAR-OK                                                        
048700       MOVE MFS-RENSA-FAELT       TO MOD-IDDC-IN                          
048800                                                                          
048900       IF MID-IDDC-IN NOT = ALL '+'                                       
049000          MOVE '7'                TO MFS-IDPFK                            
049100          MOVE SPACE              TO MFS-KDTRTYP                          
049200       END-IF                                                             
049300       MOVE MSGI-IDDC-KEY         TO W-IDDC                               
049400       PERFORM IMS-GU-WDB601                                              
049500       IF SEGMENT-SAKNAS                                                  
049600          MOVE ERR-DC-SAKNAS      TO MED-IDMFSFEL                         
049700          MOVE NEJ                TO NYCKLAR-SW                           
049800       ELSE                                                               
049900          IF DCS-NDC-CN                                                   
050000          OR DCS-USA                                                      
050100            IF DCS-IDDC = '92'                                            
050200              MOVE NEJ            TO NYCKLAR-SW                           
050300            ELSE                                                          
050400              MOVE DCS-IDLANDX2   TO W-IDLANDX2                           
050500            END-IF                                                        
050600          ELSE                                                            
050700            MOVE NEJ              TO NYCKLAR-SW                           
050800          END-IF                                                          
050900       END-IF                                                             
051000     END-IF                                                               
051100                                                                          
051200     IF GODK-MID OR NYCKLAR-OK                                            
051300        MOVE MSGI-IDDC-KEY        TO MOD-IDDC-UT                          
051400        MOVE MSGI-IDARTNR         TO MOD-IDARTNR-UT                       
051500        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
051600        MOVE WS-STRECK-1          TO MOD-STRECK-1                         
051700        MOVE ART-REKSIFFR         TO MOD-REKSIFFR                         
051800     ELSE                                                                 
051900        MOVE MFS-RENSA-FAELT      TO MOD-IDDC-UT                          
052000                                     MOD-IDARTNR-UT                       
052100     END-IF                                                               
052200                                                                          
052300     IF NYCKLAR-FEL                                                       
052400        IF KDPRODSL-LYNC                                                  
052500           MOVE MED-1             TO MOD-TEMFSFEL                         
052600        ELSE                                                              
052700           IF MED-IDMFSFEL = SPACE                                        
052800              MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                         
052900           END-IF                                                         
053000           CALL WMEDKONV USING MED-WMEDAREA                               
053100           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
053200        END-IF                                                            
053300        PERFORM MFS-RENSA-FAELT-IN                                        
053400        PERFORM MFS-RENSA-FAELT-UT                                        
053500     END-IF                                                               
053600     .                                                                    
053700                                                                          
053800 E-SAMMA-SIDA SECTION.                                                    
053900     MOVE 'E-SAMMA-SIDA   '  TO CURRENT-SECTION                           
054000                                                                          
054100     IF EGEN-MID OR HELP-MID                                              
054200        IF UPPDATERING-GJORD                                              
054300           PERFORM EA-MID-INDATA-TO-MOD                                   
054400        ELSE                                                              
054500           PERFORM MFS-RENSA-FAELT-IN                                     
054600        END-IF                                                            
054700     END-IF                                                               
054800     .                                                                    
054900 EA-MID-INDATA-TO-MOD SECTION.                                            
055000     MOVE 'EA-MID-TO-MOD  '  TO CURRENT-SECTION                           
055100                                                                          
055200     IF MID-TILEVBEG = ALL '+'                                            
055300       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-TILEVBEG                      
055400     ELSE                                                                 
055500       MOVE MID-TILEVBEG             TO MOD-TILEVBEG                      
055600       MOVE MFS-ADD-READ-FIELD       TO MOD-TILEVBEG-ATTR                 
055700     END-IF                                                               
055800                                                                          
055900     IF MID-KVPROG = ALL '+'                                              
056000       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-KVPROG                        
056100     ELSE                                                                 
056200       MOVE MID-KVPROG               TO MOD-KVPROG                        
056300       MOVE MFS-ADD-READ-FIELD       TO MOD-KVPROG-ATTR                   
056400     END-IF                                                               
056500                                                                          
056600     IF MID-PRMATRL = ALL '+'                                             
056700       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-PRMATRL                       
056800     ELSE                                                                 
056900       MOVE MFS-ADD-READ-FIELD       TO MOD-PRMATRL-ATTR                  
057000     END-IF                                                               
057100                                                                          
057200     IF MID-KVPB-REF = ALL '+'                                            
057300       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-KVPB-REF                      
057400     ELSE                                                                 
057500       MOVE MFS-ADD-READ-FIELD       TO MOD-KVPB-REF-ATTR                 
057600     END-IF                                                               
057700                                                                          
057800     IF MID-TIREFMPB = ALL '+'                                            
057900       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-TIREFMPB                      
058000     ELSE                                                                 
058100       MOVE MID-TIREFMPB             TO MOD-TIREFMPB                      
058200       MOVE MFS-ADD-READ-FIELD       TO MOD-TIREFMPB-ATTR                 
058300     END-IF                                                               
058400                                                                          
058500     IF MID-DAPUBL = ALL '+'                                              
058600       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-DAPUBL                        
058700     ELSE                                                                 
058800       MOVE MID-DAPUBL               TO MOD-DAPUBL                        
058900       MOVE MFS-ADD-READ-FIELD       TO MOD-DAPUBL-ATTR                   
059000     END-IF                                                               
059100                                                                          
059200     IF MID-IDINK = ALL '+'                                               
059300       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDINK                         
059400     ELSE                                                                 
059500       MOVE MID-IDINK                TO MOD-IDINK                         
059600       MOVE MFS-ADD-READ-FIELD       TO MOD-IDINK-ATTR                    
059700     END-IF                                                               
059800                                                                          
059900     IF MID-IDANSK = ALL '+'                                              
060000       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDANSK                        
060100     ELSE                                                                 
060200       MOVE MID-IDANSK               TO MOD-IDANSK                        
060300       MOVE MFS-ADD-READ-FIELD       TO MOD-IDANSK-ATTR                   
060400     END-IF                                                               
060500                                                                          
060600     .                                                                    
060700                                                                          
060800 F-LAES-VISA-INFO SECTION.                                                
060900     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
061000                                                                          
061100     PERFORM IMS-GU-WDK601                                                
061200     MOVE ART-REKSIFFR        TO MOD-REKSIFFR                             
061300     MOVE ART-IDAO(1)         TO MOD-IDAO                                 
061400     MOVE ART-KDSORT          TO MOD-KDSORT                               
061500     IF ART-KDSORT = 'SW'                                                 
061600        MOVE 'SOFTWARE'       TO MOD-TEMFSFEL                             
061700     END-IF                                                               
061800     MOVE ART-IDLEVNR         TO MOD-IDLEVNR                              
061900                                                                          
062000     MOVE ART-TIFINLV         TO MOD-TIFINLV                              
062100                                                                          
062200     PERFORM FA-VISA-D311-INFO                                            
062300     PERFORM FB-VISA-K611-INFO                                            
062400     PERFORM FC-VISA-K625-INFO                                            
062500     PERFORM FD-VISA-D201-INFO                                            
062600     PERFORM FE-VISA-ERS-INFO                                             
062700     PERFORM FF-VISA-K711-K722-INFO                                       
062800     PERFORM FG-VISA-K712-INFO                                            
062900     PERFORM FH-VISA-C901-INFO                                            
063000     PERFORM MFS-LAES-IN-FAELT-IN                                         
063100     .                                                                    
063200                                                                          
063300 FA-VISA-D311-INFO SECTION.                                               
063400     MOVE 'FA-VISA-D311-INFO  ' TO CURRENT-SECTION                        
063500                                                                          
063600     MOVE 'GB' TO W-IDSKYLT                                               
063700     PERFORM IMS-GU-WDD311                                                
063800     IF SEGMENT-FINNS                                                     
063900        MOVE TEXT-BEART TO MOD-BEART-ENG                                  
064000     ELSE                                                                 
064100        MOVE 'UNKNOWN'  TO MOD-BEART-ENG                                  
064200     END-IF                                                               
064300     .                                                                    
064400                                                                          
064500 FB-VISA-K611-INFO SECTION.                                               
064600     MOVE 'FB-VISA-K611-INFO  ' TO CURRENT-SECTION                        
064700                                                                          
064800     PERFORM IMS-GU-WDK611                                                
064900     IF SEGMENT-FINNS                                                     
065000        MOVE CLAG-IDBERED       TO MOD-IDBERED                            
065100        MOVE CLAG-IDKAT(1)      TO MOD-IDKAT(1)                           
065200        MOVE CLAG-IDKAT(2)      TO MOD-IDKAT(2)                           
065300        MOVE CLAG-IDKAT(3)      TO MOD-IDKAT(3)                           
065400        MOVE CLAG-IDPROENH(1)   TO MOD-IDPROENH(1)                        
065500        MOVE CLAG-IDPROENH(2)   TO MOD-IDPROENH(2)                        
065600        MOVE CLAG-IDPROJ        TO MOD-IDPROJ                             
065700        MOVE CLAG-PRARTSTD      TO MOD-PRARTSTD                           
065800        MOVE CLAG-IDANSK        TO MOD-IDANSK-CDC                         
065900        MOVE CLAG-IDINK         TO MOD-IDINK-CDC                          
066000        MOVE CLAG-KDERS         TO MOD-KDERS                              
066100        IF CLAG-KDERS > 10                                                
066200*          MOVE MFS-STAENG-FAELT TO MOD-TILEVBEG-ATTR                     
066300*          MOVE MFS-STAENG-FAELT TO MOD-KVPROG-ATTR                       
066400           MOVE 'REPLACED'       TO MOD-TEMFSFEL                          
066500        END-IF                                                            
066600        IF CLAG-PRARTSTD = ZERO                                           
066700           MOVE ZERO            TO MOD-IDLEVNR                            
066800           MOVE ZERO            TO MOD-IDANSK-CDC                         
066900           MOVE ZERO            TO MOD-IDINK-CDC                          
067000        END-IF                                                            
067100     END-IF                                                               
067200     .                                                                    
067300                                                                          
067400 FC-VISA-K625-INFO SECTION.                                               
067500     MOVE 'FC-VISA-K625-INFO  ' TO CURRENT-SECTION                        
067600                                                                          
067700     MOVE 6  TO W-KDNOTTYP                                                
067800     PERFORM IMS-GU-WDK625                                                
067900     IF SEGMENT-FINNS                                                     
068000        MOVE NOT-TEARTNOT       TO MOD-TEARTNOT                           
068100     END-IF                                                               
068200                                                                          
068300     MOVE 3  TO W-KDNOTTYP                                                
068400     PERFORM IMS-GU-WDK625                                                
068500     IF SEGMENT-FINNS                                                     
068600        MOVE NOT-TEARTNOT       TO MOD-TEARTNOT-PP                        
068700     END-IF                                                               
068800     .                                                                    
068900                                                                          
069000 FD-VISA-D201-INFO SECTION.                                               
069100     MOVE 'FD-VISA-D201-INFO  ' TO CURRENT-SECTION                        
069200                                                                          
069300     PERFORM IMS-GU-WDD201                                                
069400     IF SEGMENT-FINNS                                                     
069500        MOVE D201-ART-TEORSAK        TO MOD-TEORSAK                       
069600        MOVE D201-ART-IDPROJK        TO MOD-IDPROJK                       
069700        MOVE D201-ART-FLBYTES        TO MOD-FLBYTES                       
069800        MOVE D201-ART-IDARTNR-MOTSV  TO MOD-IDARTNR-MOTSV                 
069900        MOVE D201-ART-KVARTVAGN      TO MOD-KVARTVAGN                     
070000     END-IF                                                               
070100     .                                                                    
070200                                                                          
070300 FE-VISA-ERS-INFO         SECTION.                                        
070400     MOVE 'FE-VISA-ERS-INFO   ' TO CURRENT-SECTION                        
070500                                                                          
070600*WDD7A1 SEKUNDÄR INDEXINGÅNG                                              
070700                                                                          
070800     MOVE W-IDARTNR               TO W-IDARTNR-MIN7                       
070900                                     W-IDARTNR-MAX7                       
071000     PERFORM IMS-GN-WDD7A1                                                
071100     IF SEGMENT-FINNS                                                     
071200        MOVE ERS-IDARTNR          TO MOD-IDARTNR-TILLK                    
071300        PERFORM IMS-GN-WDD7A1                                             
071400        IF SEGMENT-FINNS                                                  
071500           MOVE 'AO'              TO MOD-MFL                              
071600        ELSE                                                              
071700           MOVE MFS-RENSA-FAELT   TO MOD-MFL                              
071800        END-IF                                                            
071900*WDK611 HÄMTA ERSÄTTNINGSKOD FÖR DEN/DE ERSATTA ARTIKLARNA                
072000        MOVE ERS-IDARTNR             TO W-IDARTNR                         
072100        PERFORM IMS-GU-WDK611                                             
072200        MOVE CLAG-KDERS              TO MOD-KDERS                         
072300                                                                          
072400********Å T E R S T Ä L L  N Y C K E L N                                  
072500        MOVE W-IDARTNR-MIN7          TO W-IDARTNR                         
072600     ELSE                                                                 
072700        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-TILLK                    
072800                                     MOD-KDERS                            
072900     END-IF                                                               
073000     .                                                                    
073100                                                                          
073200 FF-VISA-K711-K722-INFO SECTION.                                          
073300     MOVE 'FF-VISA-K711-INFO  ' TO CURRENT-SECTION                        
073400                                                                          
073500     PERFORM IMS-GU-WDK711                                                
073600     IF SEGMENT-FINNS                                                     
073700        IF SLAG-KVPB-REF > ZERO                                           
073800           MOVE SLAG-KVPB-REF   TO MOD-KVPB-REF                           
073900           MOVE MFS-STAENG-FAELT                                          
074000                                TO MOD-KVPB-REF-ATTR                      
074100        END-IF                                                            
074200                                                                          
074300        IF SLAG-TIREFMPB       > 0                                        
074400          MOVE SLAG-TIREFMPB    TO MOD-TIREFMPB                           
074500        ELSE                                                              
074600          MOVE MFS-RENSA-FAELT  TO MOD-TIREFMPB                           
074700        END-IF                                                            
074800        PERFORM IMS-GNP-WDK722                                            
074900        IF SEGMENT-FINNS                                                  
075000           PERFORM FFA-MOVE-K722-DATA                                     
075100        ELSE                                                              
075200           MOVE MFS-RENSA-FAELT TO MOD-IDINK                              
075300                                   MOD-IDANSK                             
075400        END-IF                                                            
075500     ELSE                                                                 
075600        MOVE MFS-RENSA-FAELT    TO MOD-KVPB-REF                           
075700                                   MOD-TIREFMPB                           
075800     END-IF                                                               
075900     .                                                                    
076000 FFA-MOVE-K722-DATA SECTION.                                              
076100     MOVE 'FFA-MOVE-K722-DATA ' TO CURRENT-SECTION                        
076200                                                                          
076300     INSPECT XLAG-IDINK REPLACING LEADING SPACE BY ZERO                   
076400     IF XLAG-IDINK > ZERO                                                 
076500       MOVE XLAG-IDINK(1:3)     TO MOD-IDINK                              
076600     END-IF                                                               
076700                                                                          
076800     IF XLAG-IDANSK > ZERO                                                
076900       MOVE XLAG-IDANSK         TO MOD-IDANSK                             
077000     END-IF                                                               
077100     .                                                                    
077200 FG-VISA-K712-INFO SECTION.                                               
077300     MOVE 'FG-VISA-K712   '  TO CURRENT-SECTION                           
077400                                                                          
077500     PERFORM IMS-GU-WDK712                                                
077600     IF SEGMENT-FINNS                                                     
077700       IF LART-PRMATRL > ZERO                                             
077800          MOVE LART-PRMATRL    TO MOD-PRMATRL                             
077900                                  WS-PRMATRL                              
078000       ELSE                                                               
078100          MOVE MFS-RENSA-FAELT TO MOD-PRMATRL                             
078200                                  WS-PRMATRL                              
078300       END-IF                                                             
078400                                                                          
078500       IF LART-DAPUBL > ZERO                                              
078600          MOVE LART-DAPUBL(3:6)   TO DAT-I-TIDATUM                        
078700          MOVE 'AAMMDD'           TO DAT-KDDATFORM                        
078800          CALL WDATKONV        USING DAT-KDDATFORM                        
078900                                     DAT-I-TIDATUM                        
079000                                     DAT-O-TIDATUM                        
079100                                     DAT-KDSVAR                           
079200                                                                          
079300          IF DAT-KDSVAR-OK                                                
079400             MOVE DAT-TIAAVVD-GRP TO MOD-DAPUBL                           
079500          END-IF                                                          
079600       ELSE                                                               
079700          MOVE MFS-RENSA-FAELT    TO MOD-DAPUBL                           
079800       END-IF                                                             
079900     ELSE                                                                 
080000       MOVE MFS-RENSA-FAELT       TO MOD-PRMATRL                          
080100                                     MOD-DAPUBL                           
080200     END-IF                                                               
080300     .                                                                    
080400                                                                          
080500 FH-VISA-C901-INFO SECTION.                                               
080600     MOVE 'FH-VISA-C901-INFO'   TO CURRENT-SECTION                        
080700                                                                          
080800     PERFORM IMS-GU-WDC901                                                
080900     IF SEGMENT-FINNS                                                     
081000        MOVE KART-TILEVBEG      TO DAT-I-TIDATUM                          
081100        MOVE 'AAMMDD'           TO DAT-KDDATFORM                          
081200        CALL WDATKONV        USING DAT-KDDATFORM                          
081300                                   DAT-I-TIDATUM                          
081400                                   DAT-O-TIDATUM                          
081500                                   DAT-KDSVAR                             
081600                                                                          
081700        IF DAT-KDSVAR-OK                                                  
081800           MOVE DAT-TIAAVV-GRP  TO MOD-TILEVBEG                           
081900        ELSE                                                              
082000           MOVE MFS-RENSA-FAELT TO MOD-TILEVBEG                           
082100        END-IF                                                            
082200                                                                          
082300*       IF KART-KDANSKQ = 2 AND KART-KVPROG > ZERO                        
082400           MOVE KART-KVPROG     TO MOD-KVPROG                             
082500*       END-IF                                                            
082600                                                                          
082700        IF KART-KDANSKQ = 1                                               
082800           MOVE KART-IDANSK     TO MOD-IDANSK                             
082900        END-IF                                                            
083000     ELSE                                                                 
083100        MOVE MFS-RENSA-FAELT    TO MOD-TILEVBEG                           
083200                                   MOD-KVPROG                             
083300     END-IF                                                               
083400     .                                                                    
083500 G-KOLLA-INPUT SECTION.                                                   
083600     MOVE 'G-KOLLA-INPUT        ' TO CURRENT-SECTION                      
083700                                                                          
083800     MOVE JA                      TO INDATA-SW                            
083900                                                                          
084000     IF MID-INPUT = ALL '+'                                               
084100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
084200        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
084300        CALL WMEDKONV  USING MED-WMEDAREA                                 
084400        MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                         
084500        MOVE NEJ TO INDATA-SW                                             
084600     ELSE                                                                 
084700        PERFORM GG-AUTH-USER-CHECK                                        
084800        IF INDATA-OK                                                      
084900           PERFORM GA-KOLLA-TILEVBEG                                      
085000           IF INDATA-OK                                                   
085100              PERFORM GB-KOLLA-KVPROG                                     
085200              PERFORM GC-KOLLA-IDINK-IDANSK                               
085300              PERFORM GD-KOLLA-K7-DATA                                    
085400              PERFORM GE-KOLLA-PURCH-CREATION                             
085500              PERFORM GF-KOLLA-PRARTSTD                                   
085600           ELSE                                                           
085700              MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRMATRL-ATTR              
085800                                            MOD-KVPB-REF-ATTR             
085900                                            MOD-TIREFMPB-ATTR             
086000                                            MOD-DAPUBL-ATTR               
086100                                            MOD-KVPROG-ATTR               
086200                                            MOD-IDANSK-ATTR               
086300                                            MOD-IDINK-ATTR                
086400           END-IF                                                         
086500           IF INDATA-FEL                                                  
086600              PERFORM MFS-ROER-EJ-FAELT-UT                                
086700              MOVE ERR-UPPLYSTA-FEL      TO MED-IDMFSFEL                  
086800              CALL WMEDKONV           USING MED-WMEDAREA                  
086900              MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                  
087000           END-IF                                                         
087100           IF INGEN-UPPDATERING-GJORD                                     
087200              PERFORM MFS-ROER-EJ-FAELT-UT                                
087300           END-IF                                                         
087400        END-IF                                                            
087500     END-IF                                                               
087600     .                                                                    
087700                                                                          
087800 GA-KOLLA-TILEVBEG       SECTION.                                         
087900     MOVE 'GA-KOLLA-TILEVBEG    ' TO CURRENT-SECTION                      
088000                                                                          
088100     MOVE JA                    TO UPD-SW                                 
088200     IF MID-TILEVBEG NOT = ALL '+'                                        
088300        MOVE MID-TILEVBEG       TO WS-TILEVBEG-X                          
088400        MOVE WS-TILEVBEG        TO DAT-I-TIDATUM                          
088500        MOVE 'AAVV'             TO DAT-KDDATFORM                          
088600        CALL WDATKONV  USING    DAT-KDDATFORM                             
088700                                DAT-I-TIDATUM                             
088800                                DAT-O-TIDATUM                             
088900                                DAT-KDSVAR                                
089000                                                                          
089100        IF DAT-KDSVAR-OK                                                  
089200           MOVE DAT-TIAAMMDD    TO WS-TILEVBEG-AAMMDD                     
089300           MOVE DAT-TIAAVVD-GRP TO WS-TILEVBEG-AAVVD                      
089400        ELSE                                                              
089500           MOVE ZERO            TO WS-TILEVBEG-AAMMDD                     
089600                                   WS-TILEVBEG-AAVVD                      
089700           MOVE NEJ             TO INDATA-SW                              
089800           MOVE MFS-ALFA-FAELT-FEL                                        
089900                                TO MOD-TILEVBEG-ATTR                      
090000        END-IF                                                            
090100     END-IF                                                               
090200                                                                          
090300     IF INDATA-OK                                                         
090400       IF WS-TILEVBEG > ZERO                                              
090500         MOVE DAGENS-DATUM TO DAT-I-TIDATUM                               
090600         MOVE 'AAMMDD'     TO DAT-KDDATFORM                               
090700         CALL WDATKONV  USING DAT-KDDATFORM                               
090800                              DAT-I-TIDATUM                               
090900                              DAT-O-TIDATUM                               
091000                              DAT-KDSVAR                                  
091100                                                                          
091200         IF DAT-KDSVAR-OK                                                 
091300           MOVE DAT-TIAAVV-GRP                                            
091400                           TO WS-CURRENT-WEEK                             
091500         ELSE                                                             
091600           MOVE ZERO       TO WS-CURRENT-WEEK                             
091700         END-IF                                                           
091800         MOVE WS-TILEVBEG  TO DAT-I-TIDATUM                               
091900         MOVE 'AAVV'       TO DAT-KDDATFORM                               
092000         CALL WDATKONV  USING DAT-KDDATFORM                               
092100                              DAT-I-TIDATUM                               
092200                              DAT-O-TIDATUM                               
092300                              DAT-KDSVAR                                  
092400                                                                          
092500         IF DAT-KDSVAR-OK                                                 
092600            MOVE DAT-TIAAMMDD           TO WS-TILEVBEG-AAMMDD             
092700** THE DATE SHOULD NOT BE MORE THAN 2 YEARS IN FUTURE                     
092800            IF DAT-TIAAMMDD - DAGENS-DATUM > 20000                        
092900               MOVE MFS-ALFA-FAELT-FEL    TO MOD-TILEVBEG-ATTR            
093000               MOVE NEJ                   TO INDATA-SW                    
093100            ELSE                                                          
093200               MOVE WS-TILEVBEG     TO TMP1-YYWWD                         
093300               MOVE WS-CURRENT-WEEK TO TMP2-YYWWD                         
093400               PERFORM WY2000P2                                           
093500               IF TMP1-YYWWD < TMP2-YYWWD                                 
093600                  MOVE MFS-ALFA-FAELT-FEL TO MOD-TILEVBEG-ATTR            
093700                  MOVE NEJ                TO INDATA-SW                    
093800               ELSE                                                       
093900                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-TILEVBEG-ATTR          
094000               END-IF                                                     
094100            END-IF                                                        
094200         ELSE                                                             
094300            MOVE MFS-ALFA-FAELT-FEL     TO MOD-TILEVBEG-ATTR              
094400            MOVE NEJ                    TO INDATA-SW                      
094500         END-IF                                                           
094600       ELSE                                                               
094700         MOVE MFS-ALFA-FAELT-RAETT      TO MOD-TILEVBEG-ATTR              
094800       END-IF                                                             
094900     END-IF                                                               
095000                                                                          
095100     IF INDATA-OK                                                         
095200        PERFORM IMS-GU-WDK722                                             
095300        IF SEGMENT-FINNS                                                  
095400           IF XLAG-KDAVT = 1                                              
095500              MOVE 'VALID AGREEMENT EXISTS' TO MOD-TEMFSFEL               
095600              MOVE NEJ                      TO UPD-SW                     
095700              MOVE MFS-ALFA-FAELT-RAETT     TO MOD-TILEVBEG-ATTR          
095800           END-IF                                                         
095900        ELSE                                                              
096000           MOVE ZERO TO XLAG-IDINK                                        
096100        END-IF                                                            
096200        PERFORM IMS-GU-WDC901                                             
096300        IF SEGMENT-SAKNAS                                                 
096400           MOVE ZERO TO KART-TILEVBEG                                     
096500        END-IF                                                            
096600                                                                          
096700        IF MID-TILEVBEG NOT = ALL '+'                                     
096800           IF  MID-IDINK = ALL '+' OR SPACE OR ZERO                       
096900              MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDINK-ATTR             
097000              MOVE NEJ TO INDATA-SW                                       
097100           END-IF                                                         
097200        ELSE                                                              
097300           IF KART-TILEVBEG NOT = ZERO                                    
097400              IF MID-IDINK = SPACE OR ZERO                                
097500                 MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDINK-ATTR             
097600                 MOVE NEJ TO INDATA-SW                                    
097700              END-IF                                                      
097800           END-IF                                                         
097900        END-IF                                                            
098000     END-IF                                                               
098100                                                                          
098200     .                                                                    
098300                                                                          
098400 GB-KOLLA-KVPROG         SECTION.                                         
098500     MOVE 'GB-KOLLA-KVPROG      '   TO CURRENT-SECTION                    
098600                                                                          
098700     IF MID-KVPROG NOT = ALL '+'                                          
098800        MOVE JA                     TO INDATA-SW                          
098900        INSPECT MID-KVPROG   REPLACING LEADING SPACE BY ZERO              
099000        MOVE MID-KVPROG             TO WS-KVPROG-X                        
099100     END-IF                                                               
099200                                                                          
099300     IF INDATA-OK                                                         
099400       IF WS-KVPROG NUMERIC                                               
099500          MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVPROG-ATTR                    
099600       ELSE                                                               
099700          MOVE MFS-NUM-FAELT-FEL    TO MOD-KVPROG-ATTR                    
099800          MOVE NEJ                  TO INDATA-SW                          
099900          MOVE WS-KVPROG-X          TO MOD-KVPROG                         
100000       END-IF                                                             
100100     END-IF                                                               
100200     .                                                                    
100300                                                                          
100400 GC-KOLLA-IDINK-IDANSK SECTION.                                           
100500                                                                          
100600     MOVE 'GC-KOLLA-IDINK-IDANSK'   TO CURRENT-SECTION                    
100700     IF MID-IDINK NOT = ALL '+'                                           
100800       INSPECT MID-IDINK REPLACING LEADING SPACES BY ZEROS                
100900       IF MID-IDINK(1:3) NUMERIC                                          
101000          MOVE MID-IDINK(1:3)       TO WS-IDINK                           
101100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDINK-ATTR                     
101200       ELSE                                                               
101300         IF MID-IDINK(2:2) NUMERIC                                        
101400           MOVE MID-IDINK(2:2)       TO WS-IDINK                          
101500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDINK-ATTR                    
101600         ELSE                                                             
101700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDINK-ATTR                    
101800           MOVE NEJ                  TO INDATA-SW                         
101900         END-IF                                                           
102000       END-IF                                                             
102100     END-IF                                                               
102200                                                                          
102300     IF MID-IDANSK NOT = ALL '+'                                          
102400       INSPECT MID-IDANSK REPLACING LEADING SPACES BY ZEROS               
102500       IF MID-IDANSK NUMERIC                                              
102600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDANSK-ATTR                    
102700       ELSE                                                               
102800          MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDANSK-ATTR                    
102900          MOVE NEJ                  TO INDATA-SW                          
103000       END-IF                                                             
103100     END-IF                                                               
103200     .                                                                    
103300                                                                          
103400 GD-KOLLA-K7-DATA SECTION.                                                
103500     MOVE 'GD-KOLLA-K7-DATA     '   TO CURRENT-SECTION                    
103600                                                                          
103700     IF MID-PRMATRL NOT = ALL '+'                                         
103800       INSPECT MID-PRMATRL REPLACING LEADING SPACE BY ZEROS               
103900       MOVE MID-PRMATRL               TO DEC-IDFRIDATA                    
104000       MOVE 7                         TO DEC-KVHELTAL                     
104100       MOVE 2                         TO DEC-KVDECIMAL                    
104200       CALL WDECEDIT USING DEC-WDECAREA                                   
104300       IF DEC-KDSVAR-OK AND DEC-IDEDITDATA > 0                            
104400          MOVE MFS-ALFA-FAELT-RAETT   TO MOD-PRMATRL-ATTR                 
104500          MOVE DEC-IDEDITDATA         TO WS-PRMATRL-EDIT                  
104600       ELSE                                                               
104700          MOVE MFS-ALFA-FAELT-FEL     TO MOD-PRMATRL-ATTR                 
104800          MOVE NEJ                    TO INDATA-SW                        
104900       END-IF                                                             
105000     END-IF                                                               
105100                                                                          
105200     IF MID-KVPB-REF                  NOT = ALL '+'                       
105300        MOVE MID-KVPB-REF              TO DEC-IDFRIDATA                   
105400        MOVE 6                         TO DEC-KVHELTAL                    
105500        MOVE 1                         TO DEC-KVDECIMAL                   
105600        CALL WDECEDIT USING DEC-WDECAREA                                  
105700        IF DEC-KDSVAR-OK                                                  
105800           MOVE DEC-IDEDITDATA         TO WS-KVPB-REF                     
105900           PERFORM IMS-GU-WDK711                                          
106000           IF SEGMENT-FINNS                                               
106100              IF SLAG-KVPB-REF NUMERIC AND SLAG-KVPB-REF > ZERO           
106200                 IF SLAG-KVPB-REF NOT = WS-KVPB-REF                       
106300                    MOVE 'CANNOT UPDATE'   TO MOD-TEMFSFEL                
106400                    MOVE MFS-NUM-FAELT-FEL TO MOD-KVPB-REF-ATTR           
106500                    MOVE NEJ               TO UPD-SW                      
106600                 END-IF                                                   
106700              END-IF                                                      
106800           END-IF                                                         
106900        ELSE                                                              
107000           MOVE MFS-NUM-FAELT-FEL       TO MOD-KVPB-REF-ATTR              
107100           MOVE NEJ                     TO INDATA-SW                      
107200        END-IF                                                            
107300     END-IF                                                               
107400                                                                          
107500     IF MID-KVPB-REF                  NOT = ALL '+'                       
107600     AND UPPDATERING-GJORD                                                
107700         IF DEC-KDSVAR-OK                                                 
107800           MOVE DEC-IDEDITDATA          TO WS-KVPB-REF                    
107900           PERFORM IMS-GU-WDK711                                          
108000           IF (WS-KVPB-REF              =  ZERO AND                       
108100               SEGMENT-FINNS                    AND                       
108200               SLAG-KVPB-REF            >  ZERO)    OR                    
108300              (SEGMENT-SAKNAS                   AND                       
108400               WS-KVPB-REF             >=  0.1)     OR                    
108500              (SEGMENT-FINNS                    AND                       
108600               SLAG-KVPB-REF            =  ZERO AND                       
108700               WS-KVPB-REF             >=  0.1)     OR                    
108800              (SEGMENT-FINNS                    AND                       
108900               SLAG-KVPB-REF            >  ZERO AND                       
109000               WS-KVPB-REF             >=  0.1)                           
109100              MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVPB-REF-ATTR              
109200           ELSE                                                           
109300              MOVE MFS-NUM-FAELT-FEL    TO MOD-KVPB-REF-ATTR              
109400              MOVE NEJ                  TO INDATA-SW                      
109500           END-IF                                                         
109600         END-IF                                                           
109700     END-IF                                                               
109800                                                                          
109900     IF MID-TIREFMPB                  NOT = ALL '+'                       
110000       IF  MID-TIREFMPB NUMERIC                                           
110100       AND MID-TIREFMPB > DAGENS-DATUM                                    
110200         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
110300         MOVE MID-TIREFMPB            TO DAT-I-TIDATUM                    
110400         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
110500                             DAT-O-TIDATUM DAT-KDSVAR                     
110600         IF DAT-KDSVAR-OK OR DAT-I-TIDATUM = 0                            
110700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TIREFMPB-ATTR                
110800         ELSE                                                             
110900           MOVE MFS-ALFA-FAELT-FEL    TO MOD-TIREFMPB-ATTR                
111000           MOVE NEJ                   TO INDATA-SW                        
111100         END-IF                                                           
111200       ELSE                                                               
111300         MOVE MFS-ALFA-FAELT-FEL      TO MOD-TIREFMPB-ATTR                
111400         MOVE NEJ                     TO INDATA-SW                        
111500       END-IF                                                             
111600     END-IF                                                               
111700                                                                          
111800     IF MID-DAPUBL NOT = ALL '+'                                          
111900      IF MID-DAPUBL NUMERIC                                               
112000         MOVE 'AAVVD'                  TO DAT-KDDATFORM                   
112100         MOVE MID-DAPUBL               TO DAT-I-TIDATUM                   
112200         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
112300                             DAT-O-TIDATUM DAT-KDSVAR                     
112400         IF DAT-KDSVAR-OK OR DAT-I-TIDATUM = 0                            
112500           MOVE MFS-NUM-FAELT-RAETT    TO MOD-DAPUBL-ATTR                 
112600         ELSE                                                             
112700           MOVE MFS-NUM-FAELT-FEL      TO MOD-DAPUBL-ATTR                 
112800           MOVE NEJ                    TO INDATA-SW                       
112900         END-IF                                                           
113000                                                                          
113100***PUBDATE NOT MORE THEN 2 YEARS IN THE FUTURE                            
113200         IF INDATA-OK                                                     
113300           IF MID-DAPUBL > ZERO                                           
113400             COMPUTE WS-CURRENT-YYWWD-2AAR =                              
113500                     WS-CURRENT-YYWWD + 2000                              
113600             IF MID-DAPUBL > WS-CURRENT-YYWWD-2AAR                        
113700               MOVE NEJ            TO INDATA-SW                           
113800               MOVE MFS-NUM-FAELT-FEL  TO  MOD-DAPUBL-ATTR                
113900             ELSE                                                         
114000               CONTINUE                                                   
114100             END-IF                                                       
114200           END-IF                                                         
114300         END-IF                                                           
114400                                                                          
114500***PUBDATE NOT BEFORE TODAYS DATE                                         
114600         IF INDATA-OK                                                     
114700           IF DAT-TIAAMMDD >= DAGENS-DATUM                                
114800              MOVE MFS-NUM-FAELT-RAETT   TO MOD-DAPUBL-ATTR               
114900              MOVE DAT-TIAAMMDD          TO WS-DAPUBL-TIAAMMDD            
115000           ELSE                                                           
115100              MOVE MFS-NUM-FAELT-FEL     TO MOD-DAPUBL-ATTR               
115200              MOVE NEJ                   TO INDATA-SW                     
115300           END-IF                                                         
115400         END-IF                                                           
115500      ELSE                                                                
115600        MOVE MFS-NUM-FAELT-FEL         TO MOD-DAPUBL-ATTR                 
115700        MOVE NEJ                       TO INDATA-SW                       
115800      END-IF                                                              
115900     END-IF                                                               
116000     .                                                                    
116100                                                                          
116200 GE-KOLLA-PURCH-CREATION SECTION.                                         
116300     MOVE 'GE-KOLLA-PURCH-CREAT'    TO CURRENT-SECTION                    
116400                                                                          
116500     IF  MID-TILEVBEG = ALL '+'                                           
116600     AND MID-KVPROG   = ALL '+'                                           
116700     AND MID-IDINK    = ALL '+'                                           
116800     AND MID-IDANSK   = ALL '+'                                           
116900        CONTINUE                                                          
117000     ELSE                                                                 
117100       PERFORM IMS-GU-WDK601                                              
117200       IF SEGMENT-FINNS                                                   
117300          MOVE ART-KDPRODSL          TO TEST-KDPRODSL                     
117400          IF KDPRODSL-EMB   OR                                            
117500             KDPRODSL-BIMA  OR                                            
117600             KDPRODSL-LOCAL                                               
117700             MOVE 'PURCHASE NOT ALLOWED'                                  
117800                                     TO MOD-TEMFSFEL                      
117900             MOVE NEJ                TO UPD-SW                            
118000          END-IF                                                          
118100                                                                          
118200          IF ART-KDSORT = 'SW'                                            
118300             MOVE 'SOFTWARE'         TO MOD-TEMFSFEL                      
118400             MOVE NEJ                TO UPD-SW                            
118500          END-IF                                                          
118600       END-IF                                                             
118700                                                                          
118800       PERFORM IMS-GU-WDK611                                              
118900       IF SEGMENT-FINNS                                                   
119000          IF CLAG-KDERS > 10                                              
119100             MOVE 'REPLACED'         TO MOD-TEMFSFEL                      
119200             MOVE NEJ                TO UPD-SW                            
119300          END-IF                                                          
119400          IF CLAG-PRARTSTD = ZERO                                         
119500             IF MID-PRMATRL = ALL '+'                                     
119600                MOVE MFS-ALFA-FAELT-FEL                                   
119700                                     TO MOD-PRMATRL-ATTR                  
119800                MOVE NEJ             TO INDATA-SW                         
119900             END-IF                                                       
120000          END-IF                                                          
120100       END-IF                                                             
120200                                                                          
120300       MOVE '0'                      TO WS-KDANSKQ                        
120400       IF SEGMENT-FINNS                                                   
120500          PERFORM IMS-GU-WDK712                                           
120600          IF  SEGMENT-FINNS                                               
120700             MOVE LART-BEFT          TO WS-TEST-BEFT                      
120800             IF GOOD-BEFT                                                 
120900                MOVE '2'             TO WS-KDANSKQ                        
121000             END-IF                                                       
121100          END-IF                                                          
121200          IF WS-KDANSKQ = '0'                                             
121300             PERFORM IMS-GU-WDK611                                        
121400             IF SEGMENT-FINNS                                             
121500                MOVE CLAG-KDEMBKOD-2 TO WS-TEST-EMBQ2                     
121600                MOVE CLAG-BEFT       TO WS-TEST-BEFT                      
121700                IF GOOD-EMBQ2 OR GOOD-BEFT                                
121800                  MOVE '2'           TO WS-KDANSKQ                        
121900                ELSE                                                      
122000                  MOVE '4'           TO WS-KDANSKQ                        
122100                END-IF                                                    
122200             END-IF                                                       
122300          END-IF                                                          
122400       END-IF                                                             
122500     END-IF                                                               
122600     .                                                                    
122700                                                                          
122800 GF-KOLLA-PRARTSTD       SECTION.                                         
122900     MOVE 'GF-KOLLA-PRARTSTD   '    TO CURRENT-SECTION                    
123000                                                                          
123100     PERFORM IMS-GU-WDK611                                                
123200     IF SEGMENT-FINNS                                                     
123300        IF CLAG-PRARTSTD = ZERO                                           
123400           IF MID-PRMATRL = ALL '+'                                       
123500              MOVE MFS-ALFA-FAELT-FEL TO MOD-PRMATRL-ATTR                 
123600              MOVE NEJ                TO INDATA-SW                        
123700           END-IF                                                         
123800        END-IF                                                            
123900     END-IF                                                               
124000     .                                                                    
124100                                                                          
124200 GG-AUTH-USER-CHECK SECTION.                                              
124300     MOVE 'GG-AUTH-USER-CHECK '     TO CURRENT-SECTION                    
124400                                                                          
124500     PERFORM IMS-GU-WDB601                                                
124600     IF SEGMENT-FINNS                                                     
124700       IF DCS-NDC-CN                                                      
124800       OR (DCS-NDC-NA AND DCS-USA)                                        
124900          MOVE MSGI-IDFTG           TO WS-IDFTG                           
125000          IF (DCS-NDC-CN AND IDFTG-CN)                                    
125100          OR (DCS-NDC-NA AND IDFTG-US)                                    
125200          OR MSGI-IDFTG  = WC-IDFTG-PV                                    
125300             CONTINUE                                                     
125400          ELSE                                                            
125500            MOVE NEJ                TO INDATA-SW                          
125600            MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSINF                       
125700            CALL WMEDKONV        USING MED-WMEDAREA                       
125800            MOVE MED-MFSINF         TO MOD-TEMFSFEL                       
125900            PERFORM MFS-ROER-EJ-FAELT-UT                                  
126000          END-IF                                                          
126100       ELSE                                                               
126200         MOVE NEJ                   TO INDATA-SW                          
126300         MOVE ERR-NOT-AUTHORIZED    TO MED-IDMFSINF                       
126400         CALL WMEDKONV           USING MED-WMEDAREA                       
126500         MOVE MED-MFSINF            TO MOD-TEMFSFEL                       
126600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
126700       END-IF                                                             
126800     ELSE                                                                 
126900       MOVE NEJ                     TO INDATA-SW                          
127000       MOVE ERR-NOT-AUTHORIZED      TO MED-IDMFSINF                       
127100       CALL WMEDKONV             USING MED-WMEDAREA                       
127200       MOVE MED-MFSINF              TO MOD-TEMFSFEL                       
127300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
127400     END-IF                                                               
127500     .                                                                    
127600     EJECT                                                                
127700                                                                          
127800 H-UPDATE SECTION.                                                        
127900     MOVE 'H-UPDATE            '    TO CURRENT-SECTION                    
128000                                                                          
128100     PERFORM IMS-GU-WDK601                                                
128200     IF SEGMENT-FINNS                                                     
128300        PERFORM IMS-GNP-WDK611                                            
128400        IF SEGMENT-FINNS                                                  
128500           IF CLAG-PRARTSTD = ZERO                                        
128600              PERFORM HB-UPDATE-WDK6                                      
128700              PERFORM HA-UPDATE-WDK7                                      
128800           ELSE                                                           
128900              PERFORM HA-UPDATE-WDK7                                      
129000           END-IF                                                         
129100           IF INDATA-OK                                                   
129200              PERFORM HC-CREATE-PURCH-RQST                                
129300                                                                          
129400              PERFORM HE-UPDATE-WDD2-KDANSKQ                              
129500                                                                          
129600              IF ART-KDPRODSL = +18                                       
129700                 PERFORM HD-INSERT-WDGX1142                               
129800              END-IF                                                      
129900                                                                          
130000              MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                    
130100              CALL WMEDKONV         USING MED-WMEDAREA                    
130200              MOVE MED-MFSINF          TO MOD-TEMFSINF                    
130300           END-IF                                                         
130400        END-IF                                                            
130500     END-IF                                                               
130600     .                                                                    
130700                                                                          
130800 HA-UPDATE-WDK7 SECTION.                                                  
130900     MOVE 'HA-UPDATE-WDK7      '    TO CURRENT-SECTION                    
131000                                                                          
131100                                                                          
131200     PERFORM IMS-GHU-WDK711                                               
131300     IF SEGMENT-FINNS                                                     
131400        IF MID-TIREFMPB NOT = ALL '+'                                     
131500           MOVE MID-TIREFMPB  TO SLAG-TIREFMPB                            
131600        END-IF                                                            
131700        IF MID-KVPB-REF NOT = ALL '+'                                     
131800           MOVE WS-KVPB-REF   TO SLAG-KVPB-REF                            
131900        END-IF                                                            
132000                                                                          
132100        IF MID-TIREFMPB NOT = ALL '+'                                     
132200        OR MID-KVPB-REF NOT = ALL '+'                                     
132300           PERFORM IMS-REPL-WDK711                                        
132400        END-IF                                                            
132500                                                                          
132600        PERFORM HAB-UPDATE-WDK712                                         
132700                                                                          
132800        PERFORM IMS-GHNP-WDK722                                           
132900        IF SEGMENT-FINNS                                                  
133000           MOVE  1            TO XLAG-IDPLANGR-AG                         
133100           PERFORM IMS-REPL-WDK722                                        
133200        ELSE                                                              
133300           MOVE ALL '+'       TO WDK7-W005WDK7                            
133400           MOVE  1            TO WDK7-IDPLANGR-AG                         
133500           PERFORM S01-NEW-WDK722                                         
133600        END-IF                                                            
133700     ELSE                                                                 
133800        PERFORM HAA-NEW-WDK711                                            
133900        PERFORM HAB-UPDATE-WDK712                                         
134000        MOVE ALL '+'       TO WDK7-W005WDK7                               
134100        MOVE  1            TO WDK7-IDPLANGR-AG                            
134200        PERFORM S01-NEW-WDK722                                            
134300     END-IF                                                               
134400                                                                          
134500     .                                                                    
134600                                                                          
134700 HAA-NEW-WDK711 SECTION.                                                  
134800     MOVE 'HAA-NEW-WDK711      '    TO CURRENT-SECTION                    
134900                                                                          
135000     MOVE ALL '+'      TO WDK7-W005WDK7                                   
135100     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
135200     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
135300     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
135400                          WDK7-IDDC                                       
135500     IF MID-TIREFMPB NOT = ALL '+'                                        
135600        MOVE MID-TIREFMPB  TO WDK7-TIREFMPB                               
135700     END-IF                                                               
135800     IF MID-KVPB-REF NOT = ALL '+'                                        
135900        MOVE WS-KVPB-REF TO WDK7-KVPB-REF                                 
136000     END-IF                                                               
136100     MOVE '9998'       TO WDK7-IDLEVNR OF WDK7-WDK711                     
136200     CALL W005WDK7  USING WDK7-W005WDK7 WDB6-PCB                          
136300                          WDK6-PCB WDK7-2-PCB                             
136400     .                                                                    
136500                                                                          
136600 HAB-UPDATE-WDK712 SECTION.                                               
136700     MOVE 'HAB-UPDATE-WDK712   '    TO CURRENT-SECTION                    
136800                                                                          
136900     IF MID-PRMATRL NOT = ALL '+'                                         
137000     OR MID-DAPUBL NOT = ALL '+'                                          
137100       PERFORM IMS-GHU-WDK712                                             
137200       IF SEGMENT-FINNS                                                   
137300          IF MID-PRMATRL NOT = ALL '+'                                    
137400            IF LART-KDMATRPR = '2'                                        
137500               MOVE WS-PRMATRL-EDIT  TO LART-PRMATRL                      
137600            END-IF                                                        
137700          END-IF                                                          
137800                                                                          
137900          IF MID-DAPUBL NOT = ALL '+'                                     
138000             MOVE 20                 TO LART-DAPUBL(1:2)                  
138100             MOVE WS-DAPUBL-TIAAMMDD TO LART-DAPUBL(3:6)                  
138200          END-IF                                                          
138300                                                                          
138400          PERFORM IMS-REPL-WDK712                                         
138500                                                                          
138600       ELSE                                                               
138700          PERFORM HABA-NEW-WDK712                                         
138800       END-IF                                                             
138900     END-IF                                                               
139000     .                                                                    
139100                                                                          
139200 HABA-NEW-WDK712 SECTION.                                                 
139300                                                                          
139400     MOVE 'HABA-NEW-WDK712     '    TO CURRENT-SECTION                    
139500                                                                          
139600     MOVE ALL '+'      TO WDK7-W005WDK7                                   
139700     MOVE 'WDK712'     TO WDK7-IDSEGM                                     
139800     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
139900     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
140000     MOVE W-IDLANDX2   TO WDK7-IDLANDX2                                   
140100     IF MID-PRMATRL NOT = ALL '+'                                         
140200        MOVE WS-PRMATRL-EDIT TO WDK7-PRMATRL                              
140300                                WDK7-PRARTSJK                             
140400     END-IF                                                               
140500     IF MID-DAPUBL NOT = ALL '+'                                          
140600        MOVE 20                 TO WDK7-DAPUBL(1:2)                       
140700        MOVE WS-DAPUBL-TIAAMMDD TO WDK7-DAPUBL(3:6)                       
140800     END-IF                                                               
140900     MOVE '2'          TO WDK7-KDMATRPR                                   
141000                                                                          
141100     CALL W005WDK7  USING WDK7-W005WDK7 WDB6-PCB                          
141200                          WDK6-PCB WDK7-2-PCB                             
141300     .                                                                    
141400 HB-UPDATE-WDK6 SECTION.                                                  
141500     MOVE 'HB-UPDATE-WDK6      '    TO CURRENT-SECTION                    
141600                                                                          
141700     PERFORM IMS-GHU-WDK601                                               
141800     IF SEGMENT-FINNS                                                     
141900        IF ART-IDLEVNR = SPACE                                            
142000           MOVE '9998'       TO ART-IDLEVNR                               
142100           PERFORM IMS-REPL-WDK601                                        
142200        END-IF                                                            
142300     END-IF                                                               
142400                                                                          
142500     PERFORM IMS-GHU-WDK611                                               
142600     IF SEGMENT-FINNS                                                     
142700        COMPUTE WS-CDC-PRICE ROUNDED = WS-PRMATRL-EDIT * 1.04853          
142800        MOVE WS-CDC-PRICE    TO CLAG-PRARTSTD                             
142900                                CLAG-PRARTSJK                             
143000                                CLAG-PRINK                                
143100        MOVE 0               TO CLAG-KDTIPPR                              
143200        MOVE 1               TO CLAG-IDPLANGR-AG                          
143300        IF CLAG-IDLEVNR-SHIP = SPACE                                      
143400           MOVE '9998'       TO CLAG-IDLEVNR-SHIP                         
143500           MOVE SPACE        TO CLAG-IDDC-REF                             
143600        END-IF                                                            
143700        PERFORM IMS-REPL-WDK611                                           
143800     END-IF                                                               
143900                                                                          
144000     .                                                                    
144100 HC-CREATE-PURCH-RQST SECTION.                                            
144200     MOVE 'HC-CREATE-PURCH-RQST'    TO CURRENT-SECTION                    
144300                                                                          
144400     IF MID-TILEVBEG NOT = ALL '+'                                        
144500     OR MID-KVPROG   NOT = ALL '+'                                        
144600       PERFORM IMS-GHU-WDC901                                             
144700       IF SEGMENT-FINNS                                                   
144800         PERFORM HCA-UPDATE-WDC9-DATA                                     
144900           IF  KART-KVPROG = ZERO                                         
145000           AND KART-KDANSKQ = '4'                                         
145100               PERFORM IMS-DLET-WDC901                                    
145200           ELSE                                                           
145300               PERFORM IMS-REPL-WDC901                                    
145400           END-IF                                                         
145500       ELSE                                                               
145600         INITIALIZE KART-WDC901                                           
145700         MOVE W-IDARTNR       TO KART-IDARTNR                             
145800         MOVE W-IDDC          TO KART-IDDC                                
145900         PERFORM HCA-UPDATE-WDC9-DATA                                     
146000         PERFORM IMS-ISRT-WDC901                                          
146100       END-IF                                                             
146200     END-IF                                                               
146300                                                                          
146400     IF MID-IDINK NOT = ALL '+'                                           
146500     OR MID-IDANSK NOT = ALL '+'                                          
146600       PERFORM IMS-GHU-WDK722                                             
146700       IF SEGMENT-FINNS                                                   
146800          IF MID-IDINK NOT = ALL '+'                                      
146900             IF WS-IDINK = ZERO                                           
147000                MOVE ZERO     TO XLAG-IDINK                               
147100             ELSE                                                         
147200                MOVE SPACE    TO XLAG-IDINK                               
147300                MOVE WS-IDINK TO XLAG-IDINK(1:3)                          
147400             END-IF                                                       
147500          END-IF                                                          
147600          IF MID-IDANSK NOT = ALL '+'                                     
147700             MOVE MID-IDANSK  TO XLAG-IDANSK                              
147800          ELSE                                                            
147900            IF XLAG-IDANSK NOT NUMERIC                                    
148000            AND KART-IDANSK NUMERIC                                       
148100               MOVE KART-IDANSK TO XLAG-IDANSK                            
148200            END-IF                                                        
148300          END-IF                                                          
148400                                                                          
148500          PERFORM IMS-REPL-WDK722                                         
148600       ELSE                                                               
148700          MOVE ALL '+'      TO WDK7-W005WDK7                              
148800          IF MID-IDINK NOT = ALL '+'                                      
148900             MOVE MID-IDINK   TO WDK7-IDINK                               
149000          END-IF                                                          
149100          IF MID-IDANSK NOT = ALL '+'                                     
149200             MOVE MID-IDANSK  TO WDK7-IDANSK                              
149300          ELSE                                                            
149400            IF KART-IDANSK NUMERIC                                        
149500               MOVE KART-IDANSK TO WDK7-IDANSK                            
149600            END-IF                                                        
149700          END-IF                                                          
149800          PERFORM S01-NEW-WDK722                                          
149900       END-IF                                                             
150000     END-IF                                                               
150100     .                                                                    
150200 HCA-UPDATE-WDC9-DATA SECTION.                                            
150300     MOVE 'HCA-UPDATE-WDC9-DATA'    TO CURRENT-SECTION                    
150400                                                                          
150500     IF MID-TILEVBEG NOT = ALL '+'                                        
150600        MOVE WS-TILEVBEG-AAMMDD TO KART-TILEVBEG                          
150700     END-IF                                                               
150800                                                                          
150900     IF MID-KVPROG NOT = ALL '+'                                          
151000        MOVE MID-KVPROG         TO KART-KVPROG                            
151100     END-IF                                                               
151200                                                                          
151300     MOVE ZERO                  TO KART-TIINKOP                           
151400     MOVE WS-KDANSKQ            TO KART-KDANSKQ                           
151500     MOVE DAGENS-DATUM          TO KART-TIREGDAT                          
151600                                                                          
151700* ANNULLATION AV KÖPFÖRSLAG                                               
151800     IF MID-KVPROG = ZERO                                                 
151900        MOVE ZERO               TO KART-KVPROG                            
152000                                   KART-TILEVBEG                          
152100     END-IF                                                               
152200     .                                                                    
152300                                                                          
152400 HD-INSERT-WDGX1142 SECTION.                                              
152500     MOVE 'HD-INSERT-WDGX1142  '    TO CURRENT-SECTION                    
152600                                                                          
152700     PERFORM IMS-GU-WDG202                                                
152800     IF SEGMENT-SAKNAS                                                    
152900        MOVE SPACE              TO 1142-WDGX1142                          
153000        MOVE W-1142KEY-X        TO 1142-KDSEGKEY                          
153100        MOVE ART-IDARTNR        TO 1142-IDARTNR                           
153200        PERFORM IMS-ISRT-WDG202                                           
153300     END-IF                                                               
153400     .                                                                    
153500                                                                          
153600                                                                          
153700 HE-UPDATE-WDD2-KDANSKQ SECTION.                                          
153800     MOVE 'HE-UPDATE-WDD2-KDANSKQ ' TO CURRENT-SECTION                    
153900                                                                          
154000     PERFORM IMS-GHU-WDD201                                               
154100     IF SEGMENT-FINNS                                                     
154200        IF D201-ART-KDANSKQ = '1'                                         
154300           MOVE '0'             TO D201-ART-KDANSKQ                       
154400           PERFORM IMS-REPL-WDD201                                        
154500        END-IF                                                            
154600     END-IF                                                               
154700     .                                                                    
154800                                                                          
154900                                                                          
155000 S01-NEW-WDK722 SECTION.                                                  
155100     MOVE 'S01-NEW-WDK722      '    TO CURRENT-SECTION                    
155200                                                                          
155300     MOVE 'WDK722'     TO WDK7-IDSEGM                                     
155400     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
155500     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
155600     CALL W005WDK7  USING WDK7-W005WDK7 WDB6-PCB                          
155700                          WDK6-PCB WDK7-2-PCB                             
155800     .                                                                    
155900 MFS-RENSA-FAELT-UT SECTION.                                              
156000                                                                          
156100*    --- ALLA UTDATA-FÄLT                                                 
156200     MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                                
156300                             MOD-TIFINLV                                  
156400                             MOD-IDBERED                                  
156500                             MOD-TEORSAK                                  
156600                             MOD-IDAO                                     
156700                             MOD-IDKAT(1)                                 
156800                             MOD-IDKAT(2)                                 
156900                             MOD-IDKAT(3)                                 
157000                             MOD-IDPROENH(1)                              
157100                             MOD-IDPROENH(2)                              
157200                             MOD-IDPROJ                                   
157300                             MOD-TEARTNOT                                 
157400                             MOD-IDPROJK                                  
157500                             MOD-TEARTNOT-PP                              
157600                             MOD-FLBYTES                                  
157700                             MOD-IDARTNR-TILLK                            
157800                             MOD-KDERS                                    
157900                             MOD-MFL                                      
158000                             MOD-IDARTNR-MOTSV                            
158100                             MOD-KVARTVAGN                                
158200                             MOD-IDLEVNR                                  
158300                             MOD-PRARTSTD                                 
158400                             MOD-IDANSK-CDC                               
158500                             MOD-IDINK-CDC                                
158600                             MOD-KDSORT                                   
158700     .                                                                    
158800                                                                          
158900 MFS-RENSA-FAELT-IN SECTION.                                              
159000                                                                          
159100*    --- ALLA INDATA-FÄLT                                                 
159200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
159300                             MOD-IDDC-IN                                  
159400                             MOD-PRMATRL                                  
159500                             MOD-KVPB-REF                                 
159600                             MOD-TIREFMPB                                 
159700                             MOD-DAPUBL                                   
159800                             MOD-TILEVBEG                                 
159900                             MOD-KVPROG                                   
160000                             MOD-IDANSK                                   
160100                             MOD-IDINK                                    
160200     .                                                                    
160300                                                                          
160400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
160500                                                                          
160600*    --- ALLA UTDATA-FÄLT                                                 
160700     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-ENG                              
160800                               MOD-TIFINLV                                
160900                               MOD-IDBERED                                
161000                               MOD-TEORSAK                                
161100                               MOD-IDAO                                   
161200                               MOD-IDKAT(1)                               
161300                               MOD-IDKAT(2)                               
161400                               MOD-IDKAT(3)                               
161500                               MOD-IDPROENH(1)                            
161600                               MOD-IDPROENH(2)                            
161700                               MOD-IDPROJ                                 
161800                               MOD-TEARTNOT                               
161900                               MOD-IDPROJK                                
162000                               MOD-TEARTNOT-PP                            
162100                               MOD-FLBYTES                                
162200                               MOD-IDARTNR-TILLK                          
162300                               MOD-KDERS                                  
162400                               MOD-MFL                                    
162500                               MOD-IDARTNR-MOTSV                          
162600                               MOD-KVARTVAGN                              
162700                               MOD-IDLEVNR                                
162800                               MOD-PRARTSTD                               
162900                               MOD-IDANSK-CDC                             
163000                               MOD-IDINK-CDC                              
163100                               MOD-KDSORT                                 
163200                               MOD-PRMATRL                                
163300                               MOD-KVPB-REF                               
163400                               MOD-TIREFMPB                               
163500                               MOD-DAPUBL                                 
163600                               MOD-TILEVBEG                               
163700                               MOD-KVPROG                                 
163800                               MOD-IDANSK                                 
163900                               MOD-IDINK                                  
164000     .                                                                    
164100                                                                          
164200                                                                          
164300 MFS-LAES-IN-FAELT-IN  SECTION.                                           
164400                                                                          
164500*    --- ALLA UTDATA-FÄLT                                                 
164600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRMATRL-ATTR                       
164700                                   MOD-KVPB-REF-ATTR                      
164800                                   MOD-TIREFMPB-ATTR                      
164900                                   MOD-DAPUBL-ATTR                        
165000                                   MOD-TILEVBEG-ATTR                      
165100                                   MOD-KVPROG-ATTR                        
165200                                   MOD-IDANSK-ATTR                        
165300                                   MOD-IDINK-ATTR                         
165400     .                                                                    
165500                                                                          
165600                                                                          
165700 MFS-CLOSE-FIELD-IN       SECTION.                                        
165800                                                                          
165900*    --- ALLA INDATA-FÄLT                                                 
166000     MOVE MFS-CLOSE-FIELD       TO MOD-PRMATRL-ATTR                       
166100                                   MOD-KVPB-REF-ATTR                      
166200                                   MOD-TIREFMPB-ATTR                      
166300                                   MOD-DAPUBL-ATTR                        
166400                                   MOD-TILEVBEG-ATTR                      
166500                                   MOD-KVPROG-ATTR                        
166600                                   MOD-IDANSK-ATTR                        
166700                                   MOD-IDINK-ATTR                         
166800     .                                                                    
166900                                                                          
167000                                                                          
167100* --- IMS SEKTIONER ---                                                   
167200                                                                          
167300 IMS-GET-MSG SECTION.                                                     
167400     MOVE 'IMS-GET-MSG'     TO CURRENT-IMS-SECTION                        
167500                                                                          
167600     MOVE '  QC'            TO GODK-STATUSKODER                           
167700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
167800     MOVE MSG-STATUS-CODE   TO STATUS-WS                                  
167900     PERFORM IMS-STATUSKONTROLL                                           
168000     .                                                                    
168100                                                                          
168200 IMS-INSERT-MSG SECTION.                                                  
168300     MOVE 'IMS-INSERT-MSG'  TO CURRENT-IMS-SECTION                        
168400                                                                          
168500*    IF MSGI-IDLAND-SPR = 'SE'                                            
168600*      MOVE '0'             TO MFS-KDHUVOMR                               
168700*    END-IF                                                               
168800     MOVE LOW-VALUE         TO MSG-KDZ1 MSG-KDZ2                          
168900     MOVE SPACE             TO GODK-STATUSKODER                           
169000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
169100     MOVE MSG-STATUS-CODE   TO STATUS-WS                                  
169200     PERFORM IMS-STATUSKONTROLL                                           
169300     .                                                                    
169400                                                                          
169500                                                                          
169600 IMS-GU-WDB601 SECTION.                                                   
169700     MOVE 'IMS-GU-WDB601 '  TO CURRENT-IMS-SECTION                        
169800                                                                          
169900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
170000          DELIMITED BY SIZE INTO SSA1                                     
170100     MOVE '  GE'            TO GODK-STATUSKODER                           
170200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
170300     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
170400     PERFORM IMS-STATUSKONTROLL                                           
170500     .                                                                    
170600                                                                          
170700 IMS-GU-WDD201 SECTION.                                                   
170800     MOVE 'IMS-GU-WDD201 '  TO CURRENT-IMS-SECTION                        
170900                                                                          
171000     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
171100          DELIMITED BY SIZE INTO SSA1                                     
171200     MOVE '  GE'            TO GODK-STATUSKODER                           
171300     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
171400     MOVE WDD2-STATUS-CODE  TO STATUS-WS                                  
171500     PERFORM IMS-STATUSKONTROLL                                           
171600     .                                                                    
171700                                                                          
171800 IMS-GHU-WDD201 SECTION.                                                  
171900     MOVE 'IMS-GHU-WDD201 '  TO CURRENT-IMS-SECTION                       
172000                                                                          
172100     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
172200          DELIMITED BY SIZE INTO SSA1                                     
172300     MOVE '  GE'            TO GODK-STATUSKODER                           
172400     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
172500     MOVE WDD2-STATUS-CODE  TO STATUS-WS                                  
172600     PERFORM IMS-STATUSKONTROLL                                           
172700     .                                                                    
172800                                                                          
172900 IMS-REPL-WDD201 SECTION.                                                 
173000     MOVE 'IMS-REPL-WDD201' TO CURRENT-IMS-SECTION                        
173100                                                                          
173200     MOVE '  ' TO GODK-STATUSKODER                                        
173300     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
173400     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
173500     PERFORM IMS-STATUSKONTROLL                                           
173600     .                                                                    
173700                                                                          
173800 IMS-GU-WDD311 SECTION.                                                   
173900     MOVE 'IMS-GU-WDD311 '  TO CURRENT-IMS-SECTION                        
174000                                                                          
174100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
174200          DELIMITED BY SIZE INTO SSA1                                     
174300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
174400          DELIMITED BY SIZE INTO SSA2                                     
174500     MOVE '  GE'            TO GODK-STATUSKODER                           
174600     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
174700     MOVE WDD3-STATUS-CODE  TO STATUS-WS                                  
174800     PERFORM IMS-STATUSKONTROLL                                           
174900     .                                                                    
175000                                                                          
175100                                                                          
175200 IMS-GN-WDD7A1 SECTION.                                                   
175300     MOVE 'IMS-GN-WDD7A1 '  TO CURRENT-IMS-SECTION                        
175400                                                                          
175500     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
175600                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
175700             DELIMITED BY SIZE INTO SSA1                                  
175800     MOVE '  GE'           TO GODK-STATUSKODER                            
175900     CALL CBLTDLI USING GN WDD7-PCB DLI-IO-WDD7A1 SSA1                    
176000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
176100     PERFORM IMS-STATUSKONTROLL                                           
176200     .                                                                    
176300                                                                          
176400 IMS-GU-WDK611 SECTION.                                                   
176500     MOVE 'IMS-GU-WDK611 '  TO CURRENT-IMS-SECTION                        
176600                                                                          
176700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
176800          DELIMITED BY SIZE INTO SSA1                                     
176900     STRING 'WDK611  (KDSEGKEY =1)'                                       
177000          DELIMITED BY SIZE INTO SSA2                                     
177100     MOVE '  GE'            TO GODK-STATUSKODER                           
177200     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
177300     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
177400     PERFORM IMS-STATUSKONTROLL                                           
177500     .                                                                    
177600                                                                          
177700 IMS-GU-WDK601 SECTION.                                                   
177800     MOVE 'IMS-GU-WDK601 '  TO CURRENT-IMS-SECTION                        
177900                                                                          
178000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
178100          DELIMITED BY SIZE INTO SSA1                                     
178200     MOVE '  GE'            TO GODK-STATUSKODER                           
178300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
178400     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
178500     PERFORM IMS-STATUSKONTROLL                                           
178600     .                                                                    
178700                                                                          
178800 IMS-GNP-WDK611 SECTION.                                                  
178900     MOVE 'IMS-GNP-WDK611 '  TO CURRENT-IMS-SECTION                       
179000                                                                          
179100     STRING 'WDK611  (KDSEGKEY =1)'                                       
179200          DELIMITED BY SIZE INTO SSA1                                     
179300     MOVE '  GE'            TO GODK-STATUSKODER                           
179400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
179500     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
179600     PERFORM IMS-STATUSKONTROLL                                           
179700     .                                                                    
179800                                                                          
179900 IMS-GU-WDK625 SECTION.                                                   
180000     MOVE 'IMS-GU-WDK625 '  TO CURRENT-IMS-SECTION                        
180100                                                                          
180200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
180300          DELIMITED BY SIZE INTO SSA1                                     
180400     STRING 'WDK611  (KDSEGKEY =1)'                                       
180500          DELIMITED BY SIZE INTO SSA2                                     
180600     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
180700          DELIMITED BY SIZE INTO SSA3                                     
180800     MOVE '  GE'            TO GODK-STATUSKODER                           
180900     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK625 SSA1 SSA2 SSA3         
181000     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
181100     PERFORM IMS-STATUSKONTROLL                                           
181200     .                                                                    
181300                                                                          
181400 IMS-GU-WDK711 SECTION.                                                   
181500     MOVE 'IMS-GU-WDK711 '  TO CURRENT-IMS-SECTION                        
181600                                                                          
181700     MOVE SPACES              TO SSA1                                     
181800                                 SSA2                                     
181900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
182000          DELIMITED BY SIZE INTO SSA1                                     
182100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
182200          DELIMITED BY SIZE INTO SSA2                                     
182300     MOVE '  GE' TO GODK-STATUSKODER                                      
182400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
182500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
182600     PERFORM IMS-STATUSKONTROLL                                           
182700     .                                                                    
182800                                                                          
182900                                                                          
183000 IMS-GU-WDK712 SECTION.                                                   
183100     MOVE 'IMS-GU-WDK712 '  TO CURRENT-IMS-SECTION                        
183200                                                                          
183300     MOVE SPACES              TO SSA1                                     
183400                                 SSA2                                     
183500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
183600          DELIMITED BY SIZE INTO SSA1                                     
183700     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
183800          DELIMITED BY SIZE INTO SSA2                                     
183900     MOVE '  GE' TO GODK-STATUSKODER                                      
184000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
184100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
184200     PERFORM IMS-STATUSKONTROLL                                           
184300     .                                                                    
184400                                                                          
184500                                                                          
184600 IMS-GNP-WDK722 SECTION.                                                  
184700     MOVE 'IMS-GNP-WDK722'  TO CURRENT-IMS-SECTION                        
184800                                                                          
184900     MOVE 'WDK722   ' TO SSA1                                             
185000     MOVE '  GE' TO GODK-STATUSKODER                                      
185100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
185200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
185300     PERFORM IMS-STATUSKONTROLL                                           
185400     .                                                                    
185500                                                                          
185600                                                                          
185700 IMS-GU-WDK722 SECTION.                                                   
185800     MOVE 'IMS-GU-WDK722'   TO CURRENT-IMS-SECTION                        
185900                                                                          
186000     MOVE SPACES              TO SSA1                                     
186100                                 SSA2                                     
186200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
186300          DELIMITED BY SIZE INTO SSA1                                     
186400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
186500          DELIMITED BY SIZE INTO SSA2                                     
186600     MOVE 'WDK722   ' TO SSA3                                             
186700     MOVE '  GE'              TO GODK-STATUSKODER                         
186800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722                         
186900                        SSA1 SSA2 SSA3                                    
187000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
187100     PERFORM IMS-STATUSKONTROLL                                           
187200     .                                                                    
187300                                                                          
187400                                                                          
187500 IMS-GHU-WDK711 SECTION.                                                  
187600     MOVE 'IMS-GHU-WDK711'  TO CURRENT-IMS-SECTION                        
187700                                                                          
187800     MOVE SPACES              TO SSA1                                     
187900                                 SSA2                                     
188000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
188100          DELIMITED BY SIZE INTO SSA1                                     
188200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
188300          DELIMITED BY SIZE INTO SSA2                                     
188400     MOVE '  GE' TO GODK-STATUSKODER                                      
188500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
188600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900                                                                          
189000                                                                          
189100 IMS-REPL-WDK711 SECTION.                                                 
189200     MOVE 'IMS-REPL-WDK711' TO CURRENT-IMS-SECTION                        
189300                                                                          
189400     MOVE '  ' TO GODK-STATUSKODER                                        
189500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
189600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
189700     PERFORM IMS-STATUSKONTROLL                                           
189800     .                                                                    
189900                                                                          
190000                                                                          
190100 IMS-GHNP-WDK722 SECTION.                                                 
190200     MOVE 'IMS-GHNP-WDK722' TO CURRENT-IMS-SECTION                        
190300                                                                          
190400     MOVE 'WDK722   ' TO SSA1                                             
190500     MOVE '  GE' TO GODK-STATUSKODER                                      
190600     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK722 SSA1                  
190700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
190800     PERFORM IMS-STATUSKONTROLL                                           
190900     .                                                                    
191000                                                                          
191100                                                                          
191200 IMS-GHU-WDK722 SECTION.                                                  
191300     MOVE 'IMS-GHU-WDK722'  TO CURRENT-IMS-SECTION                        
191400                                                                          
191500     MOVE SPACES              TO SSA1                                     
191600                                 SSA2                                     
191700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
191800          DELIMITED BY SIZE INTO SSA1                                     
191900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
192000          DELIMITED BY SIZE INTO SSA2                                     
192100     MOVE 'WDK722   ' TO SSA3                                             
192200     MOVE '  GE'              TO GODK-STATUSKODER                         
192300     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722                        
192400                        SSA1 SSA2 SSA3                                    
192500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
192600     PERFORM IMS-STATUSKONTROLL                                           
192700     .                                                                    
192800                                                                          
192900                                                                          
193000 IMS-REPL-WDK722 SECTION.                                                 
193100     MOVE 'IMS-REPL-WDK722' TO CURRENT-IMS-SECTION                        
193200                                                                          
193300     MOVE '  ' TO GODK-STATUSKODER                                        
193400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
193500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
193600     PERFORM IMS-STATUSKONTROLL                                           
193700     .                                                                    
193800                                                                          
193900                                                                          
194000 IMS-GHU-WDK712 SECTION.                                                  
194100     MOVE 'IMS-GHU-WDK712'  TO CURRENT-IMS-SECTION                        
194200                                                                          
194300     MOVE SPACES              TO SSA1                                     
194400                                 SSA2                                     
194500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
194600          DELIMITED BY SIZE INTO SSA1                                     
194700     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
194800          DELIMITED BY SIZE INTO SSA2                                     
194900     MOVE '  GE' TO GODK-STATUSKODER                                      
195000     CALL CBLTDLI USING GHU WDK7-2-PCB DLI-IO-WDK712 SSA1 SSA2            
195100     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
195200     PERFORM IMS-STATUSKONTROLL                                           
195300     .                                                                    
195400                                                                          
195500                                                                          
195600 IMS-REPL-WDK712 SECTION.                                                 
195700     MOVE 'IMS-REPL-WDK712' TO CURRENT-IMS-SECTION                        
195800                                                                          
195900     MOVE '  '             TO GODK-STATUSKODER                            
196000     CALL CBLTDLI USING REPL WDK7-2-PCB DLI-IO-WDK712                     
196100     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400                                                                          
196500                                                                          
196600 IMS-GU-WDC901 SECTION.                                                   
196700     MOVE 'IMS-GU-WDC901'   TO CURRENT-IMS-SECTION                        
196800                                                                          
196900     MOVE SPACES    TO SSA1                                               
197000     STRING 'WDC901  (IDARTNR  =' W-IDARTNR-X                             
197100                    '&IDDC     =' W-IDDC-X ')'                            
197200             DELIMITED BY SIZE INTO SSA1                                  
197300     MOVE '  GE' TO GODK-STATUSKODER                                      
197400     CALL CBLTDLI USING GU WDC9-PCB DLI-IO-WDC901 SSA1                    
197500     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
197600     PERFORM IMS-STATUSKONTROLL                                           
197700     .                                                                    
197800                                                                          
197900                                                                          
198000 IMS-GHU-WDC901 SECTION.                                                  
198100     MOVE 'IMS-GHU-WDC901'  TO CURRENT-IMS-SECTION                        
198200                                                                          
198300     MOVE SPACES    TO SSA1                                               
198400     STRING 'WDC901  (IDARTNR  =' W-IDARTNR-X                             
198500                    '&IDDC     =' W-IDDC-X ')'                            
198600             DELIMITED BY SIZE INTO SSA1                                  
198700     MOVE '  GE' TO GODK-STATUSKODER                                      
198800     CALL CBLTDLI USING GHU WDC9-PCB DLI-IO-WDC901 SSA1                   
198900     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
199000     PERFORM IMS-STATUSKONTROLL                                           
199100     .                                                                    
199200                                                                          
199300                                                                          
199400 IMS-REPL-WDC901 SECTION.                                                 
199500     MOVE 'IMS-REPL-WDC901' TO CURRENT-IMS-SECTION                        
199600                                                                          
199700     MOVE 'WDC901  '   TO SSA1                                            
199800     MOVE '  '  TO GODK-STATUSKODER                                       
199900     CALL CBLTDLI USING REPL WDC9-PCB DLI-IO-WDC901 SSA1                  
200000     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
200100     PERFORM IMS-STATUSKONTROLL                                           
200200     .                                                                    
200300                                                                          
200400                                                                          
200500 IMS-DLET-WDC901 SECTION.                                                 
200600     MOVE 'IMS-DLET-WDC901' TO CURRENT-IMS-SECTION                        
200700                                                                          
200800     MOVE '  '  TO GODK-STATUSKODER                                       
200900     CALL CBLTDLI USING DLET WDC9-PCB DLI-IO-WDC901                       
201000     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
201100     PERFORM IMS-STATUSKONTROLL                                           
201200     .                                                                    
201300                                                                          
201400                                                                          
201500 IMS-ISRT-WDC901 SECTION.                                                 
201600     MOVE 'IMS-ISRT-WDC901' TO CURRENT-IMS-SECTION                        
201700                                                                          
201800     MOVE 'WDC901  '   TO SSA1                                            
201900     MOVE '  '  TO GODK-STATUSKODER                                       
202000     CALL CBLTDLI USING ISRT WDC9-PCB DLI-IO-WDC901 SSA1                  
202100     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
202200     PERFORM IMS-STATUSKONTROLL                                           
202300     .                                                                    
202400                                                                          
202500                                                                          
202600 IMS-GHU-WDK601 SECTION.                                                  
202700     MOVE 'IMS-GHU-WDK601'  TO CURRENT-IMS-SECTION                        
202800                                                                          
202900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
203000          DELIMITED BY SIZE INTO SSA1                                     
203100     MOVE '  GE'            TO GODK-STATUSKODER                           
203200     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
203300     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
203400     PERFORM IMS-STATUSKONTROLL                                           
203500     .                                                                    
203600                                                                          
203700                                                                          
203800 IMS-REPL-WDK601 SECTION.                                                 
203900     MOVE 'IMS-REPL-WDK601' TO CURRENT-IMS-SECTION                        
204000                                                                          
204100     MOVE 'WDK601  '   TO SSA1                                            
204200     MOVE '  '  TO GODK-STATUSKODER                                       
204300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK601 SSA1                  
204400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
204500     PERFORM IMS-STATUSKONTROLL                                           
204600     .                                                                    
204700                                                                          
204800                                                                          
204900 IMS-GHU-WDK611 SECTION.                                                  
205000     MOVE 'IMS-GHU-WDK611'  TO CURRENT-IMS-SECTION                        
205100                                                                          
205200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
205300          DELIMITED BY SIZE INTO SSA1                                     
205400     STRING 'WDK611  (KDSEGKEY =1)'                                       
205500          DELIMITED BY SIZE INTO SSA2                                     
205600     MOVE '  GE'            TO GODK-STATUSKODER                           
205700     CALL CBLTDLI USING GHU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2             
205800     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
205900     PERFORM IMS-STATUSKONTROLL                                           
206000     .                                                                    
206100                                                                          
206200                                                                          
206300 IMS-REPL-WDK611 SECTION.                                                 
206400     MOVE 'IMS-REPL-WDK611' TO CURRENT-IMS-SECTION                        
206500                                                                          
206600     MOVE 'WDK611  '   TO SSA1                                            
206700     MOVE '  '  TO GODK-STATUSKODER                                       
206800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611 SSA1                  
206900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
207000     PERFORM IMS-STATUSKONTROLL                                           
207100     .                                                                    
207200                                                                          
207300 IMS-GU-WDG202        SECTION.                                            
207400     MOVE 'IMS-GU-WDG202 '  TO CURRENT-IMS-SECTION                        
207500                                                                          
207600     STRING 'WDG201  (WDGXKEY  =' W-1141KEY-X ')'                         
207700             DELIMITED BY SIZE INTO SSA1                                  
207800     STRING 'WDG202  (KDSEGKEY =' W-1142KEY-X                             
207900                    '&IDARTNR  =' W-IDARTNR-X ')'                         
208000             DELIMITED BY SIZE INTO SSA2                                  
208100     MOVE '  GE' TO GODK-STATUSKODER                                      
208200     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX1142                       
208300                                    SSA1 SSA2                             
208400     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
208500     PERFORM IMS-STATUSKONTROLL                                           
208600     .                                                                    
208700                                                                          
208800 IMS-ISRT-WDG202 SECTION.                                                 
208900     MOVE 'IMS-ISRT-WDG202' TO CURRENT-IMS-SECTION                        
209000                                                                          
209100     MOVE 'WDG202   '     TO SSA1                                         
209200     MOVE '  '   TO GODK-STATUSKODER                                      
209300     CALL CBLTDLI USING ISRT WDG2-PCB DLI-IO-WDGX1142 SSA1                
209400     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
209500     PERFORM IMS-STATUSKONTROLL                                           
209600     .                                                                    
209700                                                                          
209800 IMS-STATUSKONTROLL SECTION.                                              
209900                                                                          
210000     SET STATUS-IX TO 1                                                   
210100     SEARCH GODK-STATUS                                                   
210200       AT END                                                             
210300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
210400         DELIMITED BY SIZE INTO FELTEXT                                   
210500         CALL FELLOG                                                      
210600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
210700         CONTINUE                                                         
210800     END-SEARCH                                                           
210900     .                                                                    
211000*    -COPY WY2000P1                                                       
211100                                                                          
211200*    -COPY WY2000P2                                                       
