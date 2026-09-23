000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4045600.                                                
000300 AUTHOR.         SKOGLUND LENA.                                           
000400 DATE-WRITTEN.   03/07/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THE PROGRAM UPDATES   WDB9                                       
000900*        SHIPPING DOCUMENT PARAMETERS                                     
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSACTION: W4T456 W4T456U                                      
001300*        MID:         W4I45601                                            
001400*                                                                         
001500*    OUTDATA.                                                             
001600*        MOD:         W4O45601                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400 77  IDPGM                       PIC X(08)   VALUE 'W4045600'.            
002500                                                                          
002600 01  FILLER                  PIC X(16) VALUE   'KOLLA HÄR------:'.        
002700 01  WS-SEC                  PIC X(4)  VALUE   '****'.                    
002800 01  WS-TEXT                 PIC X(40) VALUE                              
002900                       '----+----+----+----+----+----+----+----+'.        
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  YES                         PIC X       VALUE 'Y'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700                                                                          
003800*    --- INDEX FOR SCROLL LINES                                           
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +9    COMP SYNC.        
004100                                                                          
004200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004300 01  IX-INDEXVARIABLER.                                                   
004400     03  IX-IDKUND               PIC S9(9)   VALUE ZERO COMP SYNC.        
004500     03  IX-IDKUNDNR             PIC S9(9)   VALUE ZERO COMP SYNC.        
004600                                                                          
004700 01  WS-IDKUND.                                                           
004800     03 WS-IDLEVNR-X.                                                     
004900        05 WS-IDLEVNR            PIC X(5).                                
005000        05 WS-IDLEVNR-SPACE      PIC X(1).                                
005100     03 FILLER   REDEFINES WS-IDLEVNR-X.                                  
005200        05 WS-IDKUND-TKN         PIC X(1)  OCCURS 6.                      
005300                                                                          
005400 01  FILLER.                                                              
005500     03 WS-IDKUNDNR              PIC 9(6).                                
005600     03 FILLER   REDEFINES WS-IDKUNDNR.                                   
005700        05 WS-IDKUNDNR-TKN       PIC X(1)  OCCURS 6.                      
005800                                                                          
005900 01  WS-IDKUNDNR-OUT             PIC Z(5)9.                               
006000                                                                          
006100 01  WS-FLSKRIV-ONDEM-UPD        PIC X       VALUE SPACE.                 
006200                                                                          
006300 01  WS-KEY-FIRST-PAGE.                                                   
006400     03 WS-IDDC                  PIC X(2)    VALUE SPACE.                 
006500     03 WS-IDDISTR               PIC S9(5)   VALUE ZERO  COMP-3.          
006600                                                                          
006700 01  WS-WDB901KY-UPD.                                                     
006800     03 WS-IDDC-UPD              PIC X(2)    VALUE SPACE.                 
006900     03 WS-IDDISTR-UPD           PIC S9(5)   VALUE ZERO  COMP-3.          
007000     03 WS-IDKUND-UPD            PIC X(10)   VALUE SPACE.                 
007100     03 WS-IDDC-REC-UPD          PIC X(2)    VALUE SPACE.                 
007200                                                                          
007300 01  WS-KVDAGAR                  PIC 9(3).                                
007400 01  WS-CHANGE-INDX              PIC S9      VALUE ZERO.                  
007500 01  WS-KDTRPDOCT                PIC X(1)    VALUE SPACE.                 
007600 01  W-IDPRTLST-UPD              PIC X(8)    VALUE SPACE.                 
007700                                                                          
007800 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
007900     88  INDATA-OK                           VALUE 'Y'.                   
008000     88  INDATA-WRONG                        VALUE 'N'.                   
008100                                                                          
008200 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
008300     88  KEYS-OK                             VALUE 'Y'.                   
008400     88  KEYS-WRONG                          VALUE 'N'.                   
008500                                                                          
008600 77  UPD-ID-SW                   PIC X       VALUE 'Y'.                   
008700     88  UPD-ID-WRONG                        VALUE 'N'.                   
008800                                                                          
008900 77  DOC-SW                     PIC X        VALUE 'N'.                   
009000     88  DOC-MISSING                         VALUE 'N'.                   
009100     88  DOC-FOUND                           VALUE 'Y'.                   
009200                                                                          
009300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009400     88  OWN-MID                             VALUE '4456'.                
009500     88  GOOD-MID                            VALUE '4456'.                
009600     88  HELP-MID                            VALUE '0551'.                
009700     EJECT                                                                
009800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009900 01  GENERAL-SUBPROGRAMS.                                                 
010000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
010500     EJECT                                                                
010600*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010700*01 -COPY WMEDAREA                                                        
010800     SKIP3                                                                
010900 01  MESSAGE-CODES.                                                       
011000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011300     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
011400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011600     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
011700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011900     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
012000     EJECT                                                                
012100*    --- PARAMETERS FOR W006PRT                                           
012200*   -COPY W006PRT                                                         
012300     EJECT                                                                
012400                                                                          
012500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
012600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012700     SKIP3                                                                
012800*01 -COPY WMSGINIT                                                        
012900     EJECT                                                                
013000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
013100*                                                                         
013200 01  FILLER                      PIC X(16) VALUE                          
013300                                               'SAVE-AREA-------'.        
013400 01  SAVE-AREA.                                                           
013500     03  SAVE-IDTRANS            PIC X(4)    VALUE '4456'.                
013600     03  SAVE-ENTER.                                                      
013700        05 SAVE-IDDC-ENTER       PIC X(2).                                
013800        05 SAVE-IDDISTR-ENTER    PIC S9(5)        COMP-3.                 
013900        05 SAVE-IDKUND-ENTER     PIC X(10).                               
014000        05 SAVE-IDDC-REC-ENTER   PIC X(2).                                
014100     03 SAVE-NEXT.                                                        
014200        05 SAVE-IDDC-NEXT        PIC X(2).                                
014300        05 SAVE-IDDISTR-NEXT     PIC S9(5)        COMP-3.                 
014400        05 SAVE-IDKUND-NEXT      PIC X(10).                               
014500        05 SAVE-IDDC-REC-NEXT    PIC X(2).                                
014600     03 SAVE-COPY.                                                        
014700        05 SAVE-IDDC-COPY        PIC X(2).                                
014800        05 SAVE-IDDISTR-COPY     PIC S9(5)        COMP-3.                 
014900        05 SAVE-IDKUND-COPY      PIC X(10).                               
015000        05 SAVE-IDDC-REC-COPY    PIC X(2).                                
015100     03 SAVE-ROWS.                                                        
015200        05 SAVE-ROW OCCURS 9.                                             
015300           07 SAVE-IDDC          PIC X(2).                                
015400           07 SAVE-IDDISTR       PIC S9(5)        COMP-3.                 
015500           07 SAVE-IDKUND        PIC X(10).                               
015600           07 SAVE-IDDC-REC      PIC X(2).                                
015700     EJECT                                                                
015800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016100*01  MID -COPY W4I45601                                                   
016200     EJECT                                                                
016300                                                                          
016400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016500*01  -COPY WMSGAREA                                                       
016600     03  MOD REDEFINES MSG-AREA.                                          
016700*      05  -COPY W4O45601                                                 
016800                                                                          
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017100     SKIP3                                                                
017200*01  -COPY WMFSAREA                                                       
017300     EJECT                                                                
017400                                                                          
017500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017600 01  KEYS-TO-DLI.                                                         
017700                                                                          
017800     03  W-WDB901KY-X.                                                    
017900         05  W-IDDC              PIC X(2)     VALUE SPACE.                
018000         05  W-IDDISTR           PIC S9(5)    VALUE ZERO  COMP-3.         
018100         05  W-IDKUND-X.                                                  
018200            07 W-IDKUND          PIC X(10)    VALUE SPACE.                
018300            07 W-IDKUNDNR-FILLER REDEFINES    W-IDKUND.                   
018400               09 W-IDKUNDNR     PIC 9(6).                                
018500               09 FILLER         PIC X(4).                                
018600            07 W-IDLEVNR-FILLER REDEFINES     W-IDKUND.                   
018700               09 W-IDLEVNR      PIC X(5).                                
018800               09 FILLER         PIC X(5).                                
018900         05  W-IDDC-REC          PIC X(2)     VALUE SPACE.                
019000                                                                          
019100     03  W-WDB9ASEQ-X.                                                    
019200          05 W-ASEQ-IDDISTR      PIC S9(5)    VALUE ZERO  COMP-3.         
019300                                                                          
019400     03  W-WDB901KY-MIN.                                                  
019500         05  W-IDDC-MIN          PIC X(2)     VALUE LOW-VALUE.            
019600         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
019700         05  W-IDKUND-MIN        PIC X(10)    VALUE LOW-VALUE.            
019800         05  W-IDDC-REC-MIN      PIC X(2)     VALUE LOW-VALUE.            
019900                                                                          
020000     03  W-WDB901KY-MAX.                                                  
020100         05  W-IDDC-MAX          PIC X(2)     VALUE HIGH-VALUE.           
020200         05  W-IDDISTR-MAX       PIC S9(5)    VALUE +99999 COMP-3.        
020300         05  W-IDKUND-MAX        PIC X(10)    VALUE HIGH-VALUE.           
020400         05  W-IDDC-REC-MAX      PIC X(2)     VALUE HIGH-VALUE.           
020500                                                                          
020600     03  W-WDB9A1KY-MIN.                                                  
020700         05  W-IDDISTR-A1-MIN    PIC S9(5)    VALUE ZERO  COMP-3.         
020800         05  W-IDDC-A1-MIN       PIC X(2)     VALUE LOW-VALUE.            
020900         05  W-IDKUND-A1-MIN     PIC X(10)    VALUE LOW-VALUE.            
021000         05  W-IDDC-REC-A1-MIN   PIC X(2)     VALUE LOW-VALUE.            
021100                                                                          
021200     03  W-WDB9A1KY-MAX.                                                  
021300         05  W-IDDISTR-A1-MAX    PIC S9(5)    VALUE +99999 COMP-3.        
021400         05  W-IDDC-A1-MAX       PIC X(2)     VALUE HIGH-VALUE.           
021500         05  W-IDKUND-A1-MAX     PIC X(10)    VALUE HIGH-VALUE.           
021600         05  W-IDDC-REC-A1-MAX   PIC X(2)     VALUE HIGH-VALUE.           
021700                                                                          
021800     03  W-DOC                   PIC X(12)    VALUE SPACE.                
021900                                                                          
022000     03  W-WDB201KEY-MIN-X.                                               
022100         05  W-IDDISTR-WDB2      PIC S9(5)    VALUE ZERO   COMP-3.        
022200         05  W-IDKUNDNR-WDB2     PIC S9(7)    VALUE ZERO   COMP-3.        
022300                                                                          
022400     03  W-IDDC-B6-X.                                                     
022500         05 W-IDDC-B6                  PIC X(2).                          
022600                                                                          
022700                                                                          
022800*    --- STATUS-KOD FRÅN IMS                                              
022900 01  STATUS-WS                   PIC XX.                                  
023000     88  SEGMENT-FOUND                       VALUE '  '.                  
023100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
023200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
023300     88  END-OF-DATA                         VALUE 'GB'.                  
023400                                                                          
023500 01  GOOD-STATUSCODES.                                                    
023600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023700                                                                          
023800 01  FILLER                      PIC X(8)    VALUE '--SSA1--'.            
023900 01  SSA1                        PIC X(80).                               
024000 01  SSA2                        PIC X(64).                               
024100 01  SSA-WDB6                    PIC X(64).                               
024200                                                                          
024300     EJECT                                                                
024400*    --- IMS FUNCTION CODES                                               
024500*01  -COPY W0003                                                          
024600                                                                          
024700     EJECT                                                                
024800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB901'.         
024900 01  DLI-IO-WDB901.                                                       
025000*    03  -COPY WDB901                                                     
025100                                                                          
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB9A1'.         
025400 01  DLI-IO-WDB9A1.                                                       
025500*    03  -COPY WDB9A1                                                     
025600                                                                          
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201 '.        
025900 01  DLI-IO-WDB201.                                                       
026000*    03  -COPY WDB201                                                     
026100                                                                          
026200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026300 01   DLI-IO-AREA-B601.                                                   
026400*     03  -COPY WDB601                                                    
026500                                                                          
026600     EJECT                                                                
026700 LINKAGE SECTION.                                                         
026800*01  -COPY W0009   -PRE MSG-                                              
026900*01  -COPY W0008   -PRE WDP7-                                             
027000     05  FILLER                  PIC X.                                   
027100                                                                          
027200*01  -COPY W0008  -PRE WDB9-                                              
027300     05  FILLER                  PIC X.                                   
027400                                                                          
027500*01  -COPY W0008  -PRE WDB9ASEQ-                                          
027600     05  FILLER                  PIC X.                                   
027700                                                                          
027800*01  -COPY W0008  -PRE WDB9A-                                             
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100*01  -COPY W0008  -PRE WDB2-                                              
028200     05  FILLER                  PIC X.                                   
028300                                                                          
028400*01  -COPY W0008  -PRE WDB6-                                              
028500     05  FILLER                  PIC X.                                   
028600                                                                          
028700     EJECT                                                                
028800 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB9-PCB                      
028900                           WDB9ASEQ-PCB WDB9A-PCB WDB2-PCB                
029000                           WDB6-PCB.                                      
029100 MAIN SECTION.                                                            
029200     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB9-PCB                      
029300                           WDB9ASEQ-PCB WDB9A-PCB WDB2-PCB                
029400                           WDB6-PCB.                                      
029500                                                                          
029600     PERFORM IMS-GET-MSG                                                  
029700     IF SEGMENT-FOUND                                                     
029800       PERFORM A-INIT                                                     
029900       PERFORM B-CHECK-KEYS                                               
030000       IF KEYS-OK                                                         
030100         IF MFS-UPDATE                                                    
030200           PERFORM G-CHECK-INPUT                                          
030300           IF INDATA-OK                                                   
030400             PERFORM H-UPDATE                                             
030500           END-IF                                                         
030600         ELSE                                                             
030700           IF MFS-FIRST                                                   
030800             PERFORM C-FIRST-PAGE                                         
030900           ELSE                                                           
031000             IF MFS-NEXT                                                  
031100               PERFORM D-NEXT-PAGE                                        
031200             ELSE                                                         
031300               PERFORM E-SAME-PAGE                                        
031400             END-IF                                                       
031500           END-IF                                                         
031600         END-IF                                                           
031700         PERFORM F-READ-SHOW-INFO                                         
031800       END-IF                                                             
031900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O45601 + 4                      
032000       PERFORM IMS-INSERT-MSG                                             
032100     END-IF                                                               
032200                                                                          
032300     MOVE ZERO                     TO RETURN-CODE                         
032400     GOBACK                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 A-INIT SECTION.                                                          
032800     MOVE 'A'                      TO WS-SEC                              
032900                                                                          
033000     IF MSG-DOUBLE-TRANSACTIONS                                           
033100       MOVE MSG-INDATA-MINUS-2-TRANSACT                                   
033200                                   TO MID-W4I45601                        
033300       MOVE MSG-IDTRANS-2          TO MFS-IDTRANS                         
033400       MOVE MSG-KDMFSFOR-2         TO MFS-KDMFSFOR                        
033500     ELSE                                                                 
033600       MOVE MSG-INDATA-MINUS-1-TRANSACT                                   
033700                                   TO MID-W4I45601                        
033800       MOVE MSG-IDTRANS-1          TO MFS-IDTRANS                         
033900       MOVE MSG-KDMFSFOR-1         TO MFS-KDMFSFOR                        
034000     END-IF                                                               
034100                                                                          
034200     MOVE MSG-KDTRTYP              TO MFS-KDTRTYP                         
034300     MOVE MSG-IDPFK                TO MFS-IDPFK                           
034400     MOVE MFS-IDTRANS              TO W-IDTRANS                           
034500                                                                          
034600     MOVE LOW-VALUE                TO MSG-AREA                            
034700     MOVE 'W4O456N1'               TO MFS-IDMOD                           
034800     MOVE '4456'                   TO MOD-IDTRANS                         
034900     MOVE MFS-ERASE-FIELD          TO MOD-TEMFSFEL                        
035000                                      MOD-TEMFSINF                        
035100                                                                          
035200     IF OWN-MID                                                           
035300       CONTINUE                                                           
035400     ELSE                                                                 
035500       MOVE SPACE                  TO MFS-KDTRTYP                         
035600       MOVE '7'                    TO MFS-IDPFK                           
035700     END-IF                                                               
035800                                                                          
035900     MOVE NOO                      TO WS-FLSKRIV-ONDEM-UPD                
036000     .                                                                    
036100     EJECT                                                                
036200 B-CHECK-KEYS SECTION.                                                    
036300     MOVE 'B'                      TO WS-SEC                              
036400                                                                          
036500     MOVE ALL '+'                  TO MSGI-WMSGINIT                       
036600     MOVE '001'                    TO MSGI-KDCALL                         
036700     MOVE MSG-LTERM-NAME           TO MSGI-IDLTERM-USER                   
036800     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
036900     MOVE '4456'                   TO MSGI-IDTRANS                        
037000                                                                          
037100     IF OWN-MID                                                           
037200       MOVE MID-IDDC-IN            TO MSGI-IDDC-KEY                       
037300       MOVE MID-IDDISTR-IN         TO MSGI-IDDISTR                        
037400       MOVE MID-KDTRPDOCT-IN       TO MSGI-KDTRPDOCT                      
037500     END-IF                                                               
037600                                                                          
037700     CALL W005INIT              USING MSGI-WMSGINIT                       
037800                                      WDP7-PCB                            
037900                                                                          
038000     MOVE MSGI-SPAR-AREA           TO SAVE-AREA                           
038100     MOVE 'GB '                    TO MED-IDSKYLT                         
038200                                                                          
038300     MOVE MFS-ERASE-FIELD          TO MOD-IDDISTR-IN                      
038400                                      MOD-IDDC-IN                         
038500                                      MOD-KDTRPDOCT-IN                    
038600                                                                          
038700     IF  MID-IDDC-IN         = ALL '+'                                    
038800     AND MID-IDDISTR-IN      = ALL '+'                                    
038900     AND MID-KDTRPDOCT-IN    = ALL '+'                                    
039000       CONTINUE                                                           
039100     ELSE                                                                 
039200       MOVE '7'                    TO MFS-IDPFK                           
039300       MOVE SPACE                  TO MFS-KDTRTYP                         
039400     END-IF                                                               
039500                                                                          
039600     MOVE YES                      TO KEYS-SW                             
039700                                                                          
039800     IF MSGI-IDDC-KEY  = '0 '                                             
039900       MOVE SPACE                  TO MSGI-IDDC-KEY                       
040000     END-IF                                                               
040100                                                                          
040200     IF MSGI-IDDC-KEY = ALL '+'                                           
040300       MOVE NOO                    TO KEYS-SW                             
040400     ELSE                                                                 
040500       IF MSGI-IDDC-KEY > SPACE                                           
040600         MOVE MSGI-IDDC-KEY        TO W-IDDC-MIN                          
040700         MOVE MSGI-IDDC-KEY        TO W-IDDC-MAX                          
040800         MOVE MSGI-IDDC-KEY        TO WS-IDDC                             
040900       END-IF                                                             
041000     END-IF                                                               
041100                                                                          
041200     IF MSGI-IDDISTR    = '000 '                                          
041300       MOVE '0000'                 TO MSGI-IDDISTR                        
041400     END-IF                                                               
041500                                                                          
041600     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
041700     IF MSGI-IDDISTR NUMERIC                                              
041800       IF MSGI-IDDISTR > ZERO                                             
041900         MOVE MSGI-IDDISTR         TO W-IDDISTR-MIN                       
042000         MOVE MSGI-IDDISTR         TO W-IDDISTR-MAX                       
042100         MOVE MSGI-IDDISTR         TO WS-IDDISTR                          
042200       END-IF                                                             
042300     ELSE                                                                 
042400       MOVE NOO                    TO KEYS-SW                             
042500     END-IF                                                               
042600                                                                          
042700     IF MSGI-KDTRPDOCT = 'A' OR 'B' OR 'C'                                
042800                      OR 'D' OR 'E' OR 'F' OR 'G'                         
042900                      OR 'H' OR 'I' OR 'J'                                
043000       MOVE MSGI-KDTRPDOCT         TO WS-KDTRPDOCT                        
043100     ELSE                                                                 
043200       IF MSGI-KDTRPDOCT  = '+' OR SPACE                                  
043300         CONTINUE                                                         
043400       ELSE                                                               
043500         MOVE NOO                  TO KEYS-SW                             
043600       END-IF                                                             
043700     END-IF                                                               
043800                                                                          
043900*--                                                                       
044000*--  STRING '*'      MFS-IDPFK                                            
044100*--         '*SW'    W-IDTRANS                                            
044200*--         '*SAVE'  SAVE-IDTRANS '*'                                     
044300*--                       DELIMITED BY SIZE INTO WS-TEXT                  
044400*--  MOVE WS-TEXT                  TO MOD-TEMFSFEL                        
044500                                                                          
044600     IF GOOD-MID OR  KEYS-OK                                              
044700       MOVE MSGI-IDDC-KEY          TO MOD-IDDC-UT                         
044800       MOVE MSGI-IDDISTR           TO MOD-IDDISTR-UT                      
044900       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
045000       MOVE MSGI-KDTRPDOCT         TO MOD-KDTRPDOCT-UT                    
045100     ELSE                                                                 
045200       MOVE MFS-ERASE-FIELD        TO MOD-IDDISTR-UT                      
045300                                      MOD-IDDC-UT                         
045400                                      MOD-KDTRPDOCT-UT                    
045500     END-IF                                                               
045600                                                                          
045700     IF  KEYS-WRONG                                                       
045800       MOVE ERR-WRONG-KEY          TO MED-IDMFSFEL                        
045900       CALL WMEDKONV            USING MED-WMEDAREA                        
046000       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
046100       PERFORM MFS-ERASE-FIELD-IN                                         
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 C-FIRST-PAGE SECTION.                                                    
046600     MOVE 'C'                      TO WS-SEC                              
046700                                                                          
046800     MOVE INF-FIRST-PAGE           TO MED-IDMFSINF                        
046900     CALL WMEDKONV              USING MED-WMEDAREA                        
047000     MOVE MED-MFSINF               TO MOD-TEMFSFEL                        
047100                                                                          
047200     PERFORM S01-MID-UPD-TO-MOD                                           
047300     PERFORM MFS-ERASE-FIELD-IN                                           
047400     .                                                                    
047500     EJECT                                                                
047600 D-NEXT-PAGE SECTION.                                                     
047700     MOVE 'D'                      TO WS-SEC                              
047800                                                                          
047900     IF SAVE-IDTRANS =  '4456'                                            
048000       MOVE SAVE-IDDC-NEXT         TO W-IDDC-MIN                          
048100       MOVE SAVE-IDDISTR-NEXT      TO W-IDDISTR-MIN                       
048200       MOVE SAVE-IDKUND-NEXT       TO W-IDKUND-MIN                        
048300       MOVE SAVE-IDDC-REC-NEXT     TO W-IDDC-REC-MIN                      
048400                                                                          
048500       PERFORM S01-MID-UPD-TO-MOD                                         
048600     ELSE                                                                 
048700       PERFORM MFS-ERASE-FIELD-IN                                         
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 E-SAME-PAGE SECTION.                                                     
049200     MOVE 'E'                      TO WS-SEC                              
049300                                                                          
049400     IF SAVE-IDTRANS = '4456'                                             
049500       MOVE SAVE-IDDC-ENTER        TO W-IDDC-MIN                          
049600       MOVE SAVE-IDDISTR-ENTER     TO W-IDDISTR-MIN                       
049700       MOVE SAVE-IDKUND-ENTER      TO W-IDKUND-MIN                        
049800       MOVE SAVE-IDDC-REC-ENTER    TO W-IDDC-REC-MIN                      
049900                                                                          
050000       IF  MID-INPUT = ALL '+'                                            
050100       AND MID-UPD   = ALL '+'                                            
050200         PERFORM MFS-ERASE-FIELD-IN                                       
050300       ELSE                                                               
050400         MOVE INF-PRESS-PF11       TO MED-IDMFSINF                        
050500         CALL WMEDKONV          USING MED-WMEDAREA                        
050600         MOVE MED-MFSINF           TO MOD-TEMFSFEL                        
050700         PERFORM EA-MID-INDATA-TO-MOD                                     
050800       END-IF                                                             
050900     ELSE                                                                 
051000       PERFORM MFS-ERASE-FIELD-IN                                         
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 EA-MID-INDATA-TO-MOD SECTION.                                            
051500                                                                          
051600     PERFORM S02-CHECK-KDCMD                                              
051700                                                                          
051800     IF WS-CHANGE-INDX = ZERO                                             
051900       PERFORM S01-MID-UPD-TO-MOD                                         
052000     END-IF                                                               
052100                                                                          
052200     IF INDATA-WRONG                                                      
052300       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
052400       MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                        
052500       CALL WMEDKONV            USING MED-WMEDAREA                        
052600       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
052700     END-IF                                                               
052800     .                                                                    
052900     EJECT                                                                
053000 F-READ-SHOW-INFO SECTION.                                                
053100     MOVE 'F'                      TO WS-SEC                              
053200                                                                          
053300     IF WS-CHANGE-INDX > ZERO                                             
053400       PERFORM FD-WDB901-TO-UPD                                           
053500     END-IF                                                               
053600                                                                          
053700     PERFORM FA-READ-BASICDATA                                            
053800                                                                          
053900     IF SEGMENT-MISSING                                                   
054000       IF NOT MFS-ENTER                                                   
054100         MOVE KEYS-ARE-MISSING     TO MED-IDMFSFEL                        
054200         CALL WMEDKONV          USING MED-WMEDAREA                        
054300         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
054400                                                                          
054500         MOVE W-WDB901KY-MIN       TO SAVE-ENTER                          
054600                                      SAVE-NEXT                           
054700       END-IF                                                             
054800                                                                          
054900       PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX           
055000         PERFORM MFS-ERASE-LINE-FIELD-OUT                                 
055100         MOVE MFS-CLOSE-FIELD      TO MOD-KDCMD-ATTR   (INDX)             
055200       END-PERFORM                                                        
055300       MOVE LOW-VALUE              TO SAVE-ROWS                           
055400     ELSE                                                                 
055500       PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX           
055600         IF SEGMENT-FOUND                                                 
055700           PERFORM FC-WDB901-TO-ROW                                       
055800           PERFORM FB-READ-LINEDATA                                       
055900         ELSE                                                             
056000           PERFORM MFS-ERASE-LINE-FIELD-OUT                               
056100           MOVE MFS-CLOSE-FIELD    TO MOD-KDCMD-ATTR   (INDX)             
056200           MOVE LOW-VALUE          TO SAVE-ROW         (INDX)             
056300         END-IF                                                           
056400       END-PERFORM                                                        
056500                                                                          
056600       MOVE W-WDB901KY-MIN         TO SAVE-ENTER                          
056700                                                                          
056800       IF SEGMENT-FOUND                                                   
056900         MOVE DOK-IDDC             TO SAVE-IDDC-NEXT                      
057000         MOVE DOK-IDDISTR          TO SAVE-IDDISTR-NEXT                   
057100         MOVE DOK-IDKUND           TO SAVE-IDKUND-NEXT                    
057200         MOVE DOK-IDDC-REC         TO SAVE-IDDC-REC-NEXT                  
057300                                                                          
057400         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
057500         CALL WMEDKONV          USING MED-WMEDAREA                        
057600         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
057700       ELSE                                                               
057800         MOVE SAVE-ENTER           TO SAVE-NEXT                           
057900                                                                          
058000         MOVE INF-LAST-PAGE        TO MED-IDMFSINF                        
058100         CALL WMEDKONV          USING MED-WMEDAREA                        
058200         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
058300       END-IF                                                             
058400     END-IF                                                               
058500                                                                          
058600     MOVE '002'                    TO MSGI-KDCALL                         
058700     MOVE '4456'                   TO SAVE-IDTRANS                        
058800     MOVE SAVE-AREA                TO MSGI-SPAR-AREA                      
058900     CALL W005INIT              USING MSGI-WMSGINIT                       
059000                                      WDP7-PCB                            
059100     .                                                                    
059200     EJECT                                                                
059300 FA-READ-BASICDATA SECTION.                                               
059400     MOVE 'FA'                     TO WS-SEC                              
059500                                                                          
059600     MOVE HIGH-VALUE               TO W-WDB901KY-MAX                      
059700     MOVE +99999                   TO W-IDDISTR-MAX                       
059800                                                                          
059900     IF MSGI-IDDC-KEY > SPACE                                             
060000       MOVE MSGI-IDDC-KEY          TO W-IDDC-MAX                          
060100     END-IF                                                               
060200                                                                          
060300     IF  WS-IDDISTR > ZERO                                                
060400       MOVE MSGI-IDDISTR           TO W-IDDISTR-MAX                       
060500     END-IF                                                               
060600                                                                          
060700     EVALUATE WS-KDTRPDOCT                                                
060800     WHEN 'A'                                                             
060900         MOVE '&KVCOPKLI >0'       TO W-DOC                               
061000     WHEN 'B'                                                             
061100         MOVE '&KVCOPVER >0'       TO W-DOC                               
061200     WHEN 'C'                                                             
061300         MOVE '&KVCOPSTA >0'       TO W-DOC                               
061400     WHEN 'D'                                                             
061500         MOVE '&KVCOPSPE >0'       TO W-DOC                               
061600     WHEN 'E'                                                             
061700         MOVE '&KVCOPPAC >0'       TO W-DOC                               
061800     WHEN 'F'                                                             
061900         MOVE '&KVCOPGMT >0'       TO W-DOC                               
062000     WHEN 'G'                                                             
062100         MOVE '&KVCOPKUL >0'       TO W-DOC                               
062200     WHEN 'H'                                                             
062300         MOVE '&KVCOPNAP >0'       TO W-DOC                               
062400     WHEN 'I'                                                             
062500         MOVE '&KVCOPBLA >0'       TO W-DOC                               
062600     WHEN 'J'                                                             
062700         MOVE '&KVCOPTRP >0'       TO W-DOC                               
062800     END-EVALUATE                                                         
062900                                                                          
063000     IF  WS-IDDC        = SPACE                                           
063100     AND WS-IDDISTR     > ZERO                                            
063200       MOVE WS-IDDISTR             TO W-ASEQ-IDDISTR                      
063300       IF W-IDDC-REC-MIN = LOW-VALUE                                      
063400         PERFORM IMS-GU-WDB901-ASEQ                                       
063500       ELSE                                                               
063600         PERFORM IMS-GU-WDB901-ASEQ-QUAL                                  
063700       END-IF                                                             
063800     ELSE                                                                 
063900       IF W-DOC > SPACE                                                   
064000         PERFORM IMS-GU-WDB901-DOC                                        
064100       ELSE                                                               
064200         PERFORM IMS-GU-WDB901                                            
064300       END-IF                                                             
064400     END-IF                                                               
064500                                                                          
064600     IF  WS-IDDC        = SPACE                                           
064700     AND WS-IDDISTR     > ZERO                                            
064800     AND WS-KDTRPDOCT   > SPACE                                           
064900       MOVE NOO                    TO DOC-SW                              
065000       PERFORM UNTIL NOT                                                  
065100       (  SEGMENT-FOUND                                                   
065200        AND DOC-MISSING)                                                  
065300         PERFORM S03-CHECK-DOC                                            
065400         IF DOC-MISSING                                                   
065500           PERFORM IMS-GN-WDB901-ASEQ                                     
065600         END-IF                                                           
065700       END-PERFORM                                                        
065800     END-IF                                                               
065900     .                                                                    
066000     EJECT                                                                
066100 FB-READ-LINEDATA SECTION.                                                
066200     MOVE 'FB'                     TO WS-SEC                              
066300                                                                          
066400     IF  WS-IDDC    = SPACE                                               
066500     AND WS-IDDISTR > ZERO                                                
066600       PERFORM IMS-GN-WDB901-ASEQ                                         
066700     ELSE                                                                 
066800       PERFORM IMS-GN-WDB901                                              
066900     END-IF                                                               
067000                                                                          
067100     IF  WS-IDDC        = SPACE                                           
067200     AND WS-IDDISTR     > ZERO                                            
067300     AND WS-KDTRPDOCT   > SPACE                                           
067400       MOVE NOO                    TO DOC-SW                              
067500       PERFORM UNTIL NOT                                                  
067600       (  SEGMENT-FOUND                                                   
067700        AND DOC-MISSING)                                                  
067800         PERFORM S03-CHECK-DOC                                            
067900         IF DOC-MISSING                                                   
068000           PERFORM IMS-GN-WDB901-ASEQ                                     
068100         END-IF                                                           
068200       END-PERFORM                                                        
068300     END-IF                                                               
068400     .                                                                    
068500     EJECT                                                                
068600 FC-WDB901-TO-ROW SECTION.                                                
068700     MOVE 'FC'                     TO WS-SEC                              
068800                                                                          
068900     MOVE DOK-IDDC                 TO MOD-IDDC         (INDX)             
069000     MOVE DOK-IDDISTR              TO MOD-IDDISTR      (INDX)             
069100                                                                          
069200     IF DCS-IDDC NOT = DOK-IDDC                                           
069300        MOVE DOK-IDDC              TO W-IDDC-B6                           
069400        PERFORM IMS-GU-WDB601                                             
069500     END-IF                                                               
069600     IF DCS-DDC                                                           
069700*......SUPPLIER NUMBER PIC X(5)                                           
069800       MOVE DOK-IDLEVNR            TO MOD-IDKUNDNR     (INDX)             
069900     ELSE                                                                 
070000       IF DOK-IDKUNDNR NUMERIC                                            
070100*........CUSTOMER NUMBER PIC 9(6)                                         
070200         MOVE DOK-IDKUNDNR         TO WS-IDKUNDNR-OUT                     
070300         MOVE WS-IDKUNDNR-OUT      TO MOD-IDKUNDNR     (INDX)             
070400       ELSE                                                               
070500         MOVE SPACE                TO MOD-IDKUNDNR     (INDX)             
070600       END-IF                                                             
070700     END-IF                                                               
070800                                                                          
070900     MOVE DOK-IDDC-REC             TO MOD-IDDC-REC     (INDX)             
071000                                                                          
071100     MOVE DOK-IDPRTLST             TO PRT-IDPRTLST                        
071200     MOVE 1                        TO PRT-KDCALL                          
071300     CALL W006PRT               USING PRT-W006PRT                         
071400     IF PRT-IDLTERM = 'NJOSD'                                             
071401     OR PRT-IDLTERM = 'NJOVC'                                             
071410       MOVE 'NOPRINT'              TO MOD-IDLTERM      (INDX)             
071411     ELSE                                                                 
071420       MOVE PRT-IDLTERM            TO MOD-IDLTERM      (INDX)             
071430     END-IF                                                               
071500                                                                          
071600     MOVE DOK-IDUSER               TO MOD-IDUSER       (INDX)             
071700     MOVE DOK-KVCOPIES-GMTL        TO MOD-KVCOPIES-GMTL(INDX)             
071800     MOVE DOK-KVCOPIES-KLIS        TO MOD-KVCOPIES-KLIS(INDX)             
071900     MOVE DOK-KVCOPIES-PACK        TO MOD-KVCOPIES-PACK(INDX)             
072000     MOVE DOK-KVCOPIES-SPED        TO MOD-KVCOPIES-SPED(INDX)             
072100     MOVE DOK-KVCOPIES-STAT        TO MOD-KVCOPIES-STAT(INDX)             
072200     MOVE DOK-KVCOPIES-VERS        TO MOD-KVCOPIES-VERS(INDX)             
072300     MOVE DOK-KVCOPIES-KULB        TO MOD-KVCOPIES-KULB(INDX)             
072400     MOVE DOK-KVCOPIES-NAPR        TO MOD-KVCOPIES-NAPR(INDX)             
072500     MOVE DOK-KVCOPIES-BLAD        TO MOD-KVCOPIES-BLAD(INDX)             
072600     MOVE DOK-KVCOPIES-TRPT        TO MOD-KVCOPIES-TRPT(INDX)             
072700     MOVE DOK-TIUPPDAT             TO MOD-TIUPPDAT     (INDX)             
072800     MOVE DOK-KVDAGAR              TO MOD-KVDAGAR      (INDX)             
072900     MOVE DOK-FLSKRIV-ONDEM        TO MOD-FLSKRIV-ONDEM (INDX)            
073000                                                                          
073100     MOVE DOK-IDDC                 TO SAVE-IDDC        (INDX)             
073200     MOVE DOK-IDDISTR              TO SAVE-IDDISTR     (INDX)             
073300     MOVE DOK-IDKUND               TO SAVE-IDKUND      (INDX)             
073400     MOVE DOK-IDDC-REC             TO SAVE-IDDC-REC    (INDX)             
073500     .                                                                    
073600     EJECT                                                                
073700 FD-WDB901-TO-UPD SECTION.                                                
073800     MOVE 'FD'                     TO WS-SEC                              
073900                                                                          
074000     MOVE WS-CHANGE-INDX           TO INDX                                
074100     MOVE SAVE-IDDC      (INDX)    TO W-IDDC                              
074200     MOVE SAVE-IDDISTR   (INDX)    TO W-IDDISTR                           
074300     MOVE SAVE-IDKUND    (INDX)    TO W-IDKUND                            
074400     MOVE SAVE-IDDC-REC  (INDX)    TO W-IDDC-REC                          
074500     PERFORM IMS-GHU-WDB901                                               
074600                                                                          
074700     IF SEGMENT-FOUND                                                     
074800       MOVE SAVE-IDDC    (INDX)    TO SAVE-IDDC-COPY                      
074900       MOVE SAVE-IDDISTR (INDX)    TO SAVE-IDDISTR-COPY                   
075000       MOVE SAVE-IDKUND  (INDX)    TO SAVE-IDKUND-COPY                    
075100       MOVE SAVE-IDDC-REC(INDX)    TO SAVE-IDDC-REC-COPY                  
075200                                                                          
075300       MOVE DOK-IDDC               TO MOD-IDDC-UPD                        
075400       MOVE DOK-IDDISTR            TO MOD-IDDISTR-UPD                     
075500       IF DCS-IDDC NOT = DOK-IDDC                                         
075600          MOVE DOK-IDDC            TO W-IDDC-B6                           
075700          PERFORM IMS-GU-WDB601                                           
075800       END-IF                                                             
075900                                                                          
076000       IF DCS-DDC                                                         
076100*........SUPPLIER NUMBER  PIC X(5)                                        
076200         MOVE DOK-IDLEVNR          TO MOD-IDKUNDNR-UPD                    
076300       ELSE                                                               
076400         IF DOK-IDKUNDNR NUMERIC                                          
076500*..........CUSTOMER   NUMBER PIC 9(6)                                     
076600           MOVE DOK-IDKUNDNR       TO WS-IDKUNDNR-OUT                     
076700           MOVE WS-IDKUNDNR-OUT    TO MOD-IDKUNDNR-UPD                    
076800         ELSE                                                             
076900           MOVE SPACE              TO MOD-IDKUNDNR-UPD                    
077000         END-IF                                                           
077100       END-IF                                                             
077200                                                                          
077300       MOVE DOK-IDDC-REC           TO MOD-IDDC-REC-UPD                    
077400                                                                          
077500       MOVE DOK-IDPRTLST           TO PRT-IDPRTLST                        
077600       MOVE 1                      TO PRT-KDCALL                          
077700       CALL W006PRT             USING PRT-W006PRT                         
077800*      MOVE PRT-IDLTERM            TO MOD-IDLTERM-UPD                     
077811       IF PRT-IDLTERM = 'NJOSD'                                           
077812       OR PRT-IDLTERM = 'NJOVC'                                           
077820         MOVE 'NOPRINT'            TO MOD-IDLTERM-UPD                     
077830       ELSE                                                               
077840         MOVE PRT-IDLTERM          TO MOD-IDLTERM-UPD                     
077850       END-IF                                                             
077860                                                                          
077900                                                                          
078000       MOVE DOK-KVCOPIES-GMTL      TO MOD-KVCOPIES-GMTL-UPD               
078100       MOVE DOK-KVCOPIES-KLIS      TO MOD-KVCOPIES-KLIS-UPD               
078200       MOVE DOK-KVCOPIES-PACK      TO MOD-KVCOPIES-PACK-UPD               
078300       MOVE DOK-KVCOPIES-SPED      TO MOD-KVCOPIES-SPED-UPD               
078400       MOVE DOK-KVCOPIES-STAT      TO MOD-KVCOPIES-STAT-UPD               
078500       MOVE DOK-KVCOPIES-VERS      TO MOD-KVCOPIES-VERS-UPD               
078600       MOVE DOK-KVCOPIES-KULB      TO MOD-KVCOPIES-KULB-UPD               
078700       MOVE DOK-KVCOPIES-NAPR      TO MOD-KVCOPIES-NAPR-UPD               
078800       MOVE DOK-KVCOPIES-BLAD      TO MOD-KVCOPIES-BLAD-UPD               
078900       MOVE DOK-KVCOPIES-TRPT      TO MOD-KVCOPIES-TRPT-UPD               
079000       MOVE DOK-KVDAGAR            TO MOD-KVDAGAR-UPD                     
079100       MOVE DOK-FLSKRIV-ONDEM      TO MOD-FLSKRIV-ONDEM-UPD               
079200       PERFORM MFS-ADD-READ-FIELD-UPD                                     
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600 G-CHECK-INPUT SECTION.                                                   
079700     MOVE 'G'                      TO WS-SEC                              
079800                                                                          
079900     MOVE YES                      TO INDATA-SW                           
080000     IF  MID-INPUT = ALL '+'                                              
080100     AND MID-UPD   = ALL '+'                                              
080200       MOVE ERR-PF11-AND-NO-DATA   TO MED-IDMFSFEL                        
080300       CALL WMEDKONV            USING MED-WMEDAREA                        
080400       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
080500                                                                          
080600       MOVE NOO                    TO INDATA-SW                           
080700     ELSE                                                                 
080800       MOVE SPACE                  TO MED-IDMFSFEL                        
080900       PERFORM S02-CHECK-KDCMD                                            
081000       PERFORM S01-MID-UPD-TO-MOD                                         
081100       IF NOT MID-UPD = ALL '+'                                           
081200         PERFORM GA-CHECK-UPD-ROW                                         
081300         IF INDATA-OK                                                     
081400           PERFORM GB-CHECK-SAMFAKT                                       
081500           IF INDATA-OK                                                   
081600             PERFORM GC-CHECK-UPD-ROW-EXIST                               
081700           END-IF                                                         
081800         END-IF                                                           
081900       END-IF                                                             
082000     END-IF                                                               
082100                                                                          
082200     IF INDATA-WRONG                                                      
082300       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
082400       IF UPD-ID-WRONG                                                    
082500         CONTINUE                                                         
082600       ELSE                                                               
082700         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
082800         CALL WMEDKONV          USING MED-WMEDAREA                        
082900         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
083000       END-IF                                                             
083100                                                                          
083200*......SHOW PAGE WITH ERRORS                                              
083300       MOVE SAVE-IDDC-ENTER        TO W-IDDC-MIN                          
083400       MOVE SAVE-IDDISTR-ENTER     TO W-IDDISTR-MIN                       
083500       MOVE SAVE-IDKUND-ENTER      TO W-IDKUND-MIN                        
083600       MOVE SAVE-IDDC-REC-ENTER    TO W-IDDC-REC-MIN                      
083700     END-IF                                                               
083800     .                                                                    
083900     EJECT                                                                
084000 GA-CHECK-UPD-ROW SECTION.                                                
084100     MOVE 'GA'                     TO WS-SEC                              
084200                                                                          
084300     IF  MID-IDDC-UPD NOT = ALL '+'                                       
084400     AND MID-IDDC-UPD > SPACE                                             
084500       IF DCS-IDDC NOT = MID-IDDC-UPD                                     
084600          MOVE MID-IDDC-UPD        TO W-IDDC-B6                           
084700          PERFORM IMS-GU-WDB601                                           
084800       END-IF                                                             
084900       IF DCS-KDDC NOT = SPACE                                            
085000         MOVE MFS-ALPHA-FIELD-OK   TO MOD-IDDC-UPD-ATTR                   
085100         MOVE MID-IDDC-UPD         TO W-IDDC                              
085200       ELSE                                                               
085300         MOVE MFS-ALPHA-FIELD-WRONG                                       
085400                                   TO MOD-IDDC-UPD-ATTR                   
085500         MOVE NOO                  TO INDATA-SW                           
085600       END-IF                                                             
085700     ELSE                                                                 
085800       MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDDC-UPD-ATTR                   
085900       MOVE NOO                    TO INDATA-SW                           
086000     END-IF                                                               
086100                                                                          
086200     INSPECT MID-IDDISTR-UPD REPLACING LEADING SPACE BY ZERO              
086300     IF MID-IDDISTR-UPD NUMERIC                                           
086400       MOVE MFS-NUM-FIELD-OK       TO MOD-IDDISTR-UPD-ATTR                
086500       MOVE MID-IDDISTR-UPD        TO W-IDDISTR                           
086600     ELSE                                                                 
086700       MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDDISTR-UPD-ATTR                
086800       MOVE NOO                    TO INDATA-SW                           
086900     END-IF                                                               
087000                                                                          
087100     IF  MID-IDKUNDNR-UPD NOT = ALL '+'                                   
087200     AND MID-IDKUNDNR-UPD > SPACE                                         
087300       MOVE MID-IDKUNDNR-UPD       TO WS-IDKUND                           
087400                                                                          
087500       IF DCS-DDC                                                         
087600*........SUPPLIER NUMBER X(5)                                             
087700         IF WS-IDLEVNR-SPACE > SPACE                                      
087800           MOVE MFS-ALPHA-FIELD-WRONG                                     
087900                                   TO MOD-IDKUNDNR-UPD-ATTR               
088000           MOVE NOO                TO INDATA-SW                           
088100         ELSE                                                             
088200           MOVE MID-IDKUNDNR-UPD   TO W-IDLEVNR                           
088300           MOVE MFS-ALPHA-FIELD-OK TO MOD-IDKUNDNR-UPD-ATTR               
088400         END-IF                                                           
088500                                                                          
088600       ELSE                                                               
088700*........CUSTOMER   NUMBER 9(6)                                           
088800         MOVE +6                   TO IX-IDKUNDNR                         
088900         MOVE  000000              TO WS-IDKUNDNR                         
089000         PERFORM VARYING IX-IDKUND FROM +6 BY -1                          
089100                                         UNTIL IX-IDKUND < +1             
089200*..........JUSTIFY RIGHT                                                  
089300           IF WS-IDKUND-TKN(IX-IDKUND) > SPACE                            
089400             MOVE WS-IDKUND-TKN(IX-IDKUND)                                
089500                                   TO WS-IDKUNDNR-TKN(IX-IDKUNDNR)        
089600             SUBTRACT +1         FROM IX-IDKUNDNR                         
089700           END-IF                                                         
089800         END-PERFORM                                                      
089900         IF  WS-IDKUNDNR  NUMERIC                                         
090000         AND WS-IDKUNDNR > ZERO                                           
090100           MOVE WS-IDKUNDNR        TO W-IDKUNDNR                          
090200           MOVE MFS-NUM-FIELD-OK   TO MOD-IDKUNDNR-UPD-ATTR               
090300         ELSE                                                             
090400           MOVE MFS-NUM-FIELD-WRONG                                       
090500                                   TO MOD-IDKUNDNR-UPD-ATTR               
090600           MOVE NOO                TO INDATA-SW                           
090700         END-IF                                                           
090800       END-IF                                                             
090900     END-IF                                                               
091000                                                                          
091100     IF MID-KVCOPIES-GMTL-UPD NUMERIC                                     
091200       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-GMTL-UPD-ATTR          
091300     ELSE                                                                 
091400       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-GMTL-UPD-ATTR          
091500       MOVE NOO                    TO INDATA-SW                           
091600     END-IF                                                               
091700                                                                          
091800     IF MID-KVCOPIES-KLIS-UPD NUMERIC                                     
091900       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-KLIS-UPD-ATTR          
092000     ELSE                                                                 
092100       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-KLIS-UPD-ATTR          
092200       MOVE NOO                    TO INDATA-SW                           
092300     END-IF                                                               
092400                                                                          
092500     IF MID-KVCOPIES-PACK-UPD NUMERIC                                     
092600       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-PACK-UPD-ATTR          
092700     ELSE                                                                 
092800       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-PACK-UPD-ATTR          
092900       MOVE NOO                    TO INDATA-SW                           
093000     END-IF                                                               
093100                                                                          
093200     IF MID-KVCOPIES-SPED-UPD NUMERIC                                     
093300       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-SPED-UPD-ATTR          
093400     ELSE                                                                 
093500       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-SPED-UPD-ATTR          
093600       MOVE NOO                    TO INDATA-SW                           
093700     END-IF                                                               
093800                                                                          
093900     IF MID-KVCOPIES-STAT-UPD NUMERIC                                     
094000       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-STAT-UPD-ATTR          
094100     ELSE                                                                 
094200       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-STAT-UPD-ATTR          
094300       MOVE NOO                    TO INDATA-SW                           
094400     END-IF                                                               
094500                                                                          
094600     IF MID-KVCOPIES-VERS-UPD NUMERIC                                     
094700       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-VERS-UPD-ATTR          
094800     ELSE                                                                 
094900       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-VERS-UPD-ATTR          
095000       MOVE NOO                    TO INDATA-SW                           
095100     END-IF                                                               
095200                                                                          
095300     IF MID-KVCOPIES-KULB-UPD NUMERIC                                     
095400       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-KULB-UPD-ATTR          
095500     ELSE                                                                 
095600       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-KULB-UPD-ATTR          
095700       MOVE NOO                    TO INDATA-SW                           
095800     END-IF                                                               
095900                                                                          
096000     IF MID-KVCOPIES-NAPR-UPD NUMERIC                                     
096100       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-NAPR-UPD-ATTR          
096200     ELSE                                                                 
096300       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-NAPR-UPD-ATTR          
096400       MOVE NOO                    TO INDATA-SW                           
096500     END-IF                                                               
096600     IF MID-KVCOPIES-BLAD-UPD NUMERIC                                     
096700       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-BLAD-UPD-ATTR          
096800     ELSE                                                                 
096900       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-BLAD-UPD-ATTR          
097000       MOVE NOO                    TO INDATA-SW                           
097100     END-IF                                                               
097200     IF MID-KVCOPIES-TRPT-UPD NUMERIC                                     
097300       MOVE MFS-NUM-FIELD-OK       TO MOD-KVCOPIES-TRPT-UPD-ATTR          
097400     ELSE                                                                 
097500       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVCOPIES-TRPT-UPD-ATTR          
097600       MOVE NOO                    TO INDATA-SW                           
097700     END-IF                                                               
097800                                                                          
097900     IF  MID-IDDC-REC-UPD NOT = ALL '+'                                   
098000     AND MID-IDDC-REC-UPD > SPACE                                         
098100       IF DCS-IDDC NOT = MID-IDDC-REC-UPD                                 
098200          MOVE MID-IDDC-REC-UPD    TO W-IDDC-B6                           
098300          PERFORM IMS-GU-WDB601                                           
098400       END-IF                                                             
098500       IF DCS-KDDC NOT = SPACE                                            
098600         MOVE MFS-ALPHA-FIELD-OK   TO MOD-IDDC-REC-UPD-ATTR               
098700         MOVE MID-IDDC-REC-UPD     TO W-IDDC-REC                          
098800       ELSE                                                               
098900         MOVE MFS-ALPHA-FIELD-WRONG                                       
099000                                   TO MOD-IDDC-REC-UPD-ATTR               
099100         MOVE NOO                  TO INDATA-SW                           
099200       END-IF                                                             
099300     ELSE                                                                 
099400       MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDDC-REC-UPD-ATTR               
099500       MOVE NOO                    TO INDATA-SW                           
099600     END-IF                                                               
099700                                                                          
099800     IF  MID-IDLTERM-UPD NOT = ALL '+'                                    
099900     AND MID-IDLTERM-UPD > SPACE                                          
100000                                                                          
100100       IF MID-IDLTERM-UPD = 'NOPRINT'                                     
100200         MOVE 'NJOSD'              TO PRT-IDLTERM                         
100201       ELSE                                                               
100210         MOVE MID-IDLTERM-UPD      TO PRT-IDLTERM                         
100220       END-IF                                                             
100300       MOVE 2                      TO PRT-KDCALL                          
100400       CALL W006PRT             USING PRT-W006PRT                         
100500       IF PRT-KDSVAR = 'F'                                                
100600         MOVE ERR-WRONG-PRINTER    TO MED-IDMFSFEL                        
100700         CALL WMEDKONV          USING MED-WMEDAREA                        
100800         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
100900         MOVE MFS-ALPHA-FIELD-WRONG                                       
101000                                   TO MOD-IDLTERM-UPD-ATTR                
101100         MOVE NOO                  TO INDATA-SW                           
101200       ELSE                                                               
101300         MOVE PRT-IDPRTLST         TO W-IDPRTLST-UPD                      
101400         MOVE MFS-ALPHA-FIELD-OK   TO MOD-IDLTERM-UPD-ATTR                
101500       END-IF                                                             
101600     ELSE                                                                 
101700       MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDLTERM-UPD-ATTR                
101800       MOVE NOO                    TO INDATA-SW                           
101900     END-IF                                                               
102000                                                                          
102100     INSPECT MID-KVDAGAR-UPD REPLACING LEADING SPACE BY ZERO              
102200     IF MID-KVDAGAR-UPD NUMERIC                                           
102300       MOVE MFS-NUM-FIELD-OK       TO MOD-KVDAGAR-UPD-ATTR                
102400       MOVE MID-KVDAGAR-UPD        TO WS-KVDAGAR                          
102500     ELSE                                                                 
102600       MOVE MFS-NUM-FIELD-WRONG    TO MOD-KVDAGAR-UPD-ATTR                
102700       MOVE NOO                    TO INDATA-SW                           
102800     END-IF                                                               
102900                                                                          
103000     IF  MID-FLSKRIV-ONDEM-UPD NOT = ALL '+'                              
103100       MOVE MFS-ALPHA-FIELD-OK     TO MOD-FLSKRIV-ONDEM-UPD-ATTR          
103200       IF MID-FLSKRIV-ONDEM-UPD = YES                                     
103300         MOVE JA                   TO WS-FLSKRIV-ONDEM-UPD                
103400       ELSE                                                               
103500         MOVE MID-FLSKRIV-ONDEM-UPD  TO WS-FLSKRIV-ONDEM-UPD              
103600       END-IF                                                             
103700     ELSE                                                                 
103800       MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-FLSKRIV-ONDEM-UPD-ATTR          
103900       MOVE NOO                    TO INDATA-SW                           
104000     END-IF                                                               
104100                                                                          
104200     MOVE W-WDB901KY-X             TO WS-WDB901KY-UPD                     
104300     .                                                                    
104400     EJECT                                                                
104500 GB-CHECK-SAMFAKT  SECTION.                                               
104600     MOVE 'GB-'                    TO WS-SEC                              
104700                                                                          
104800     IF DCS-DDC                                                           
104900       CONTINUE                                                           
105000     ELSE                                                                 
105100       IF W-IDKUNDNR NUMERIC                                              
105200         MOVE W-IDDISTR            TO W-IDDISTR-WDB2                      
105300         MOVE W-IDKUNDNR           TO W-IDKUNDNR-WDB2                     
105400         PERFORM IMS-GU-WDB201                                            
105500         IF SEGMENT-MISSING                                               
105600           MOVE MFS-NUM-FIELD-WRONG                                       
105700                                   TO MOD-IDKUNDNR-UPD-ATTR               
105800           MOVE NOO                TO INDATA-SW                           
105900           MOVE NOO                TO UPD-ID-SW                           
106000           MOVE 'DISTRICT + CUSTOMER MISSING'                             
106100                                   TO MOD-TEMFSFEL                        
106200         ELSE                                                             
106300           IF GMT-FLSAMFAK = 'J'                                          
106400             MOVE MFS-NUM-FIELD-WRONG                                     
106500                                   TO MOD-IDKUNDNR-UPD-ATTR               
106600             MOVE NOO              TO UPD-ID-SW                           
106700             MOVE NOO              TO INDATA-SW                           
106800             MOVE 'CO-INVOICING, CUSTOMER NOT ALLOWED'                    
106900                                   TO MOD-TEMFSFEL                        
107000           END-IF                                                         
107100         END-IF                                                           
107200       END-IF                                                             
107300     END-IF                                                               
107400     .                                                                    
107500     EJECT                                                                
107600 GC-CHECK-UPD-ROW-EXIST SECTION.                                          
107700     MOVE 'GC-'                    TO WS-SEC                              
107800                                                                          
107900     IF  SAVE-IDDC-COPY     > SPACE                                       
108000     AND SAVE-IDDC-COPY     = W-IDDC                                      
108100     AND SAVE-IDDISTR-COPY  = W-IDDISTR                                   
108200     AND SAVE-IDKUND-COPY   = W-IDKUND                                    
108300     AND SAVE-IDDC-REC-COPY = W-IDDC-REC                                  
108400*......EXISTING ROW IS BEING CHANGED, OK                                  
108500       CONTINUE                                                           
108600     ELSE                                                                 
108700       MOVE WS-WDB901KY-UPD        TO W-WDB901KY-X                        
108800       PERFORM IMS-GHU-WDB901                                             
108900       IF SEGMENT-FOUND                                                   
109000*........ROW EXISTS BUT HAS NOT BEEN COPIED TO UPD-ROW                    
109100         MOVE NOO                  TO INDATA-SW                           
109200         MOVE NOO                  TO UPD-ID-SW                           
109300         MOVE 'DATA EXISTS, SELECT BEFORE UPDATE'                         
109400                                   TO MOD-TEMFSFEL                        
109500       END-IF                                                             
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900 H-UPDATE SECTION.                                                        
110000     MOVE 'H'                      TO WS-SEC                              
110100                                                                          
110200     PERFORM HB-HANDLE-DELETE                                             
110300     IF NOT MID-UPD = ALL '+'                                             
110400       PERFORM HA-HANDLE-UPDATE-ROW                                       
110500     END-IF                                                               
110600     PERFORM MFS-FORM-ATTR                                                
110700     PERFORM MFS-ERASE-FIELD-IN                                           
110800                                                                          
110900     MOVE INF-UPDATE-DONE          TO MED-IDMFSINF                        
111000     CALL WMEDKONV              USING MED-WMEDAREA                        
111100     MOVE MED-MFSINF               TO MOD-TEMFSFEL                        
111200                                                                          
111300     PERFORM HC-KEYS-FIRST-PAGE                                           
111400     .                                                                    
111500     EJECT                                                                
111600 HA-HANDLE-UPDATE-ROW SECTION.                                            
111700     MOVE 'HA'                     TO WS-SEC                              
111800                                                                          
111900     MOVE WS-WDB901KY-UPD          TO W-WDB901KY-X                        
112000     PERFORM IMS-GHU-WDB901                                               
112100     PERFORM HAA-DATA-TO-WDB901                                           
112200     IF SEGMENT-FOUND                                                     
112300       PERFORM IMS-REPL-WDB901                                            
112400     ELSE                                                                 
112500       PERFORM IMS-ISRT-WDB901                                            
112600     END-IF                                                               
112700     PERFORM HAB-UPDATE-KVDAGAR                                           
112800     .                                                                    
112900     EJECT                                                                
113000 HAA-DATA-TO-WDB901 SECTION.                                              
113100     MOVE 'HAA'                    TO WS-SEC                              
113200                                                                          
113300     MOVE W-IDDC                   TO DOK-IDDC                            
113400     MOVE W-IDDISTR                TO DOK-IDDISTR                         
113500     MOVE W-IDKUND                 TO DOK-IDKUND                          
113600     MOVE W-IDDC-REC               TO DOK-IDDC-REC                        
113700     MOVE W-IDPRTLST-UPD           TO DOK-IDPRTLST                        
113800     MOVE MSGI-IDUSER              TO DOK-IDUSER                          
113900     MOVE MID-KVCOPIES-GMTL-UPD    TO DOK-KVCOPIES-GMTL                   
114000     MOVE MID-KVCOPIES-KLIS-UPD    TO DOK-KVCOPIES-KLIS                   
114100     MOVE MID-KVCOPIES-PACK-UPD    TO DOK-KVCOPIES-PACK                   
114200     MOVE MID-KVCOPIES-SPED-UPD    TO DOK-KVCOPIES-SPED                   
114300     MOVE MID-KVCOPIES-STAT-UPD    TO DOK-KVCOPIES-STAT                   
114400     MOVE MID-KVCOPIES-VERS-UPD    TO DOK-KVCOPIES-VERS                   
114500     MOVE MID-KVCOPIES-KULB-UPD    TO DOK-KVCOPIES-KULB                   
114600     MOVE MID-KVCOPIES-NAPR-UPD    TO DOK-KVCOPIES-NAPR                   
114700     MOVE MID-KVCOPIES-BLAD-UPD    TO DOK-KVCOPIES-BLAD                   
114800     MOVE MID-KVCOPIES-TRPT-UPD    TO DOK-KVCOPIES-TRPT                   
114900     MOVE FUNCTION CURRENT-DATE(3:6)                                      
115000                                   TO DOK-TIUPPDAT                        
115100     MOVE WS-KVDAGAR               TO DOK-KVDAGAR                         
115200     MOVE WS-FLSKRIV-ONDEM-UPD     TO DOK-FLSKRIV-ONDEM                   
115300     .                                                                    
115400     EJECT                                                                
115500 HAB-UPDATE-KVDAGAR SECTION.                                              
115600     MOVE 'HAB'                    TO WS-SEC                              
115700                                                                          
115800     MOVE W-IDDISTR                TO W-IDDISTR-A1-MIN                    
115900     MOVE W-IDDISTR                TO W-IDDISTR-A1-MAX                    
116000     PERFORM IMS-GU-WDB9A1                                                
116100     PERFORM UNTIL NOT SEGMENT-FOUND                                      
116200       IF NOT WS-KVDAGAR = SEQA-KVDAGAR                                   
116300         MOVE SEQA-IDDC            TO W-IDDC                              
116400         MOVE SEQA-IDDISTR         TO W-IDDISTR                           
116500         MOVE SEQA-IDKUND          TO W-IDKUND                            
116600         MOVE SEQA-IDDC-REC        TO W-IDDC-REC                          
116700         PERFORM IMS-GHU-WDB901                                           
116800         MOVE WS-KVDAGAR           TO DOK-KVDAGAR                         
116900         PERFORM IMS-REPL-WDB901                                          
117000       END-IF                                                             
117100       PERFORM IMS-GN-WDB9A1                                              
117200     END-PERFORM                                                          
117300     .                                                                    
117400     EJECT                                                                
117500 HB-HANDLE-DELETE SECTION.                                                
117600     MOVE 'HB-'                    TO WS-SEC                              
117700                                                                          
117800     PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX             
117900       IF MID-KDCMD        (INDX) = 'D'                                   
118000         MOVE SAVE-IDDC    (INDX)  TO W-IDDC                              
118100         MOVE SAVE-IDDISTR (INDX)  TO W-IDDISTR                           
118200         MOVE SAVE-IDKUND  (INDX)  TO W-IDKUND                            
118300         MOVE SAVE-IDDC-REC(INDX)  TO W-IDDC-REC                          
118400         PERFORM IMS-GHU-WDB901                                           
118500         PERFORM IMS-DLET-WDB901                                          
118600       END-IF                                                             
118700     END-PERFORM                                                          
118800     .                                                                    
118900     EJECT                                                                
119000 HC-KEYS-FIRST-PAGE SECTION.                                              
119100     MOVE 'HC-'                    TO WS-SEC                              
119200                                                                          
119300     MOVE LOW-VALUE                TO W-WDB901KY-MIN                      
119400     MOVE WS-IDDC                  TO W-IDDC-MIN                          
119500     MOVE WS-IDDISTR               TO W-IDDISTR-MIN                       
119600     .                                                                    
119700     EJECT                                                                
119800 S01-MID-UPD-TO-MOD SECTION.                                              
119900     MOVE 'S01'                    TO WS-SEC                              
120000                                                                          
120100     IF MID-IDDC-UPD          = ALL '+'                                   
120200       MOVE MFS-ERASE-FIELD        TO MOD-IDDC-UPD                        
120300     ELSE                                                                 
120400       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDC-UPD                        
120500       MOVE MFS-ADD-READ-FIELD     TO MOD-IDDC-UPD-ATTR                   
120600     END-IF                                                               
120700                                                                          
120800     IF MID-IDDISTR-UPD       = ALL '+'                                   
120900       MOVE MFS-ERASE-FIELD        TO MOD-IDDISTR-UPD                     
121000     ELSE                                                                 
121100       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UPD                     
121200       MOVE MFS-ADD-READ-FIELD     TO MOD-IDDISTR-UPD-ATTR                
121300     END-IF                                                               
121400                                                                          
121500     IF MID-IDKUNDNR-UPD      = ALL '+'                                   
121600       MOVE MFS-ERASE-FIELD        TO MOD-IDKUNDNR-UPD                    
121700     ELSE                                                                 
121800       IF MID-IDKUNDNR-UPD = '0     '                                     
121900          MOVE SPACE               TO MID-IDKUNDNR-UPD                    
122000       ELSE                                                               
122100         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDKUNDNR-UPD                  
122200       END-IF                                                             
122300       MOVE MFS-ADD-READ-FIELD     TO MOD-IDKUNDNR-UPD-ATTR               
122400     END-IF                                                               
122500                                                                          
122600     IF  MID-KVCOPIES-GMTL-UPD = ALL '+'                                  
122700     AND MID-IDDC-UPD          = ALL '+'                                  
122800       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-GMTL-UPD               
122900     ELSE                                                                 
123000       IF MID-KVCOPIES-GMTL-UPD > '+'                                     
123100         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-GMTL-UPD             
123200       ELSE                                                               
123300         MOVE '0'                  TO MID-KVCOPIES-GMTL-UPD               
123400         MOVE '0'                  TO MOD-KVCOPIES-GMTL-UPD               
123500       END-IF                                                             
123600       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-GMTL-UPD-ATTR          
123700     END-IF                                                               
123800                                                                          
123900     IF  MID-KVCOPIES-KLIS-UPD = ALL '+'                                  
124000     AND MID-IDDC-UPD          = ALL '+'                                  
124100       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-KLIS-UPD               
124200     ELSE                                                                 
124300       IF MID-KVCOPIES-KLIS-UPD > '+'                                     
124400         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-KLIS-UPD             
124500       ELSE                                                               
124600         MOVE '0'                  TO MID-KVCOPIES-KLIS-UPD               
124700         MOVE '0'                  TO MOD-KVCOPIES-KLIS-UPD               
124800       END-IF                                                             
124900       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-KLIS-UPD-ATTR          
125000     END-IF                                                               
125100                                                                          
125200     IF  MID-KVCOPIES-PACK-UPD = ALL '+'                                  
125300     AND MID-IDDC-UPD          = ALL '+'                                  
125400         MOVE MFS-ERASE-FIELD      TO MOD-KVCOPIES-PACK-UPD               
125500     ELSE                                                                 
125600       IF MID-KVCOPIES-PACK-UPD > '+'                                     
125700         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-PACK-UPD             
125800       ELSE                                                               
125900         MOVE '0'                  TO MID-KVCOPIES-PACK-UPD               
126000         MOVE '0'                  TO MOD-KVCOPIES-PACK-UPD               
126100       END-IF                                                             
126200       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-PACK-UPD-ATTR          
126300     END-IF                                                               
126400                                                                          
126500     IF  MID-KVCOPIES-SPED-UPD = ALL '+'                                  
126600     AND MID-IDDC-UPD          = ALL '+'                                  
126700         MOVE MFS-ERASE-FIELD      TO MOD-KVCOPIES-SPED-UPD               
126800     ELSE                                                                 
126900       IF MID-KVCOPIES-SPED-UPD > '+'                                     
127000         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-SPED-UPD             
127100       ELSE                                                               
127200         MOVE '0'                  TO MID-KVCOPIES-SPED-UPD               
127300         MOVE '0'                  TO MOD-KVCOPIES-SPED-UPD               
127400       END-IF                                                             
127500       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-SPED-UPD-ATTR          
127600     END-IF                                                               
127700                                                                          
127800     IF  MID-KVCOPIES-STAT-UPD = ALL '+'                                  
127900     AND MID-IDDC-UPD          = ALL '+'                                  
128000       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-STAT-UPD               
128100     ELSE                                                                 
128200       IF MID-KVCOPIES-STAT-UPD > '+'                                     
128300         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-STAT-UPD             
128400       ELSE                                                               
128500         MOVE '0'                  TO MID-KVCOPIES-STAT-UPD               
128600         MOVE '0'                  TO MOD-KVCOPIES-STAT-UPD               
128700       END-IF                                                             
128800       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-STAT-UPD-ATTR          
128900     END-IF                                                               
129000                                                                          
129100     IF  MID-KVCOPIES-VERS-UPD = ALL '+'                                  
129200     AND MID-IDDC-UPD          = ALL '+'                                  
129300       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-VERS-UPD               
129400     ELSE                                                                 
129500       IF MID-KVCOPIES-VERS-UPD > '+'                                     
129600         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-VERS-UPD             
129700       ELSE                                                               
129800         MOVE '0'                  TO MID-KVCOPIES-VERS-UPD               
129900         MOVE '0'                  TO MOD-KVCOPIES-VERS-UPD               
130000       END-IF                                                             
130100       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-VERS-UPD-ATTR          
130200     END-IF                                                               
130300                                                                          
130400     IF  MID-KVCOPIES-KULB-UPD = ALL '+'                                  
130500     AND MID-IDDC-UPD          = ALL '+'                                  
130600       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-KULB-UPD               
130700     ELSE                                                                 
130800       IF MID-KVCOPIES-KULB-UPD > '+'                                     
130900         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-KULB-UPD             
131000       ELSE                                                               
131100         MOVE '0'                  TO MID-KVCOPIES-KULB-UPD               
131200         MOVE '0'                  TO MOD-KVCOPIES-KULB-UPD               
131300       END-IF                                                             
131400       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-KULB-UPD-ATTR          
131500     END-IF                                                               
131600                                                                          
131700     IF  MID-KVCOPIES-NAPR-UPD = ALL '+'                                  
131800     AND MID-IDDC-UPD          = ALL '+'                                  
131900       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-NAPR-UPD               
132000     ELSE                                                                 
132100       IF MID-KVCOPIES-NAPR-UPD > '+'                                     
132200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-NAPR-UPD             
132300       ELSE                                                               
132400         MOVE '0'                  TO MID-KVCOPIES-NAPR-UPD               
132500         MOVE '0'                  TO MOD-KVCOPIES-NAPR-UPD               
132600       END-IF                                                             
132700       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-NAPR-UPD-ATTR          
132800     END-IF                                                               
132900                                                                          
133000     IF  MID-KVCOPIES-BLAD-UPD = ALL '+'                                  
133100     AND MID-IDDC-UPD          = ALL '+'                                  
133200       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-BLAD-UPD               
133300     ELSE                                                                 
133400       IF MID-KVCOPIES-BLAD-UPD > '+'                                     
133500         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-BLAD-UPD             
133600       ELSE                                                               
133700         MOVE '0'                  TO MID-KVCOPIES-BLAD-UPD               
133800         MOVE '0'                  TO MOD-KVCOPIES-BLAD-UPD               
133900       END-IF                                                             
134000       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-BLAD-UPD-ATTR          
134100     END-IF                                                               
134200                                                                          
134300     IF  MID-KVCOPIES-TRPT-UPD = ALL '+'                                  
134400     AND MID-IDDC-UPD          = ALL '+'                                  
134500       MOVE MFS-ERASE-FIELD        TO MOD-KVCOPIES-TRPT-UPD               
134600     ELSE                                                                 
134700       IF MID-KVCOPIES-TRPT-UPD > '+'                                     
134800         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVCOPIES-TRPT-UPD             
134900       ELSE                                                               
135000         MOVE '0'                  TO MID-KVCOPIES-TRPT-UPD               
135100         MOVE '0'                  TO MOD-KVCOPIES-TRPT-UPD               
135200       END-IF                                                             
135300       MOVE MFS-ADD-READ-FIELD     TO MOD-KVCOPIES-TRPT-UPD-ATTR          
135400     END-IF                                                               
135500                                                                          
135600     IF MID-IDDC-REC-UPD = ALL '+'                                        
135700       MOVE MFS-ERASE-FIELD        TO MOD-IDDC-REC-UPD                    
135800     ELSE                                                                 
135900       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDC-REC-UPD                    
136000       MOVE MFS-ADD-READ-FIELD     TO MOD-IDDC-REC-UPD-ATTR               
136100     END-IF                                                               
136200                                                                          
136300     IF MID-IDLTERM-UPD = ALL '+'                                         
136400       MOVE MFS-ERASE-FIELD        TO MOD-IDLTERM-UPD                     
136500     ELSE                                                                 
136600       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDLTERM-UPD                     
136700       MOVE MFS-ADD-READ-FIELD     TO MOD-IDLTERM-UPD-ATTR                
136800     END-IF                                                               
136900                                                                          
137000     IF MID-KVDAGAR-UPD = ALL '+'                                         
137100       MOVE MFS-ERASE-FIELD        TO MOD-KVDAGAR-UPD                     
137200     ELSE                                                                 
137300       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-UPD                     
137400       MOVE MFS-ADD-READ-FIELD     TO MOD-KVDAGAR-UPD-ATTR                
137500     END-IF                                                               
137600                                                                          
137700     IF MID-FLSKRIV-ONDEM-UPD = ALL '+'                                   
137800       MOVE MFS-ERASE-FIELD        TO MOD-FLSKRIV-ONDEM-UPD               
137900     ELSE                                                                 
138000       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLSKRIV-ONDEM-UPD               
138100       MOVE MFS-ADD-READ-FIELD     TO MOD-FLSKRIV-ONDEM-UPD-ATTR          
138200     END-IF                                                               
138300     .                                                                    
138400     EJECT                                                                
138500 S02-CHECK-KDCMD       SECTION.                                           
138600     MOVE 'S02'                    TO WS-SEC                              
138700                                                                          
138800     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
138900                                                                          
139000       IF MID-KDCMD (INDX) = ALL '+'                                      
139100       OR MID-KDCMD (INDX) = SPACE                                        
139200       OR MID-KDCMD (INDX) < SPACE                                        
139300         MOVE MFS-RENSA-FAELT      TO MOD-KDCMD        (INDX)             
139400       ELSE                                                               
139500         IF  MID-KDCMD (INDX) = 'C'                                       
139600         AND WS-CHANGE-INDX = ZERO                                        
139700         AND NOT MFS-UPDATE                                               
139800           MOVE INDX               TO WS-CHANGE-INDX                      
139900           MOVE MFS-RENSA-FAELT    TO MOD-KDCMD        (INDX)             
140000         ELSE                                                             
140100           IF  MID-KDCMD (INDX) = 'D'                                     
140200             MOVE MFS-ADD-READ-FIELD                                      
140300                                   TO MOD-KDCMD-ATTR   (INDX)             
140400             MOVE MFS-DO-NOT-TOUCH-FIELD                                  
140500                                   TO MOD-KDCMD        (INDX)             
140600           ELSE                                                           
140700             MOVE MFS-ALPHA-FIELD-WRONG                                   
140800                                   TO MOD-KDCMD-ATTR   (INDX)             
140900             MOVE NOO              TO INDATA-SW                           
141000           END-IF                                                         
141100         END-IF                                                           
141200       END-IF                                                             
141300     END-PERFORM                                                          
141400     .                                                                    
141500     EJECT                                                                
141600 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
141700     MOVE '12'                     TO WS-SEC                              
141800                                                                          
141900     MOVE MFS-ERASE-FIELD          TO MOD-KDCMD        (INDX)             
142000                                      MOD-IDDC         (INDX)             
142100                                      MOD-IDDISTR      (INDX)             
142200                                      MOD-IDKUNDNR     (INDX)             
142300                                      MOD-IDDC-REC     (INDX)             
142400                                      MOD-IDLTERM      (INDX)             
142500                                      MOD-IDUSER       (INDX)             
142600                                      MOD-KVCOPIES-GMTL(INDX)             
142700                                      MOD-KVCOPIES-KLIS(INDX)             
142800                                      MOD-KVCOPIES-PACK(INDX)             
142900                                      MOD-KVCOPIES-SPED(INDX)             
143000                                      MOD-KVCOPIES-STAT(INDX)             
143100                                      MOD-KVCOPIES-VERS(INDX)             
143200                                      MOD-KVCOPIES-KULB(INDX)             
143300                                      MOD-KVCOPIES-NAPR(INDX)             
143400                                      MOD-KVCOPIES-BLAD(INDX)             
143500                                      MOD-KVCOPIES-TRPT(INDX)             
143600                                      MOD-TIUPPDAT     (INDX)             
143700                                      MOD-KVDAGAR      (INDX)             
143800                                      MOD-FLSKRIV-ONDEM (INDX)            
143900     .                                                                    
144000     SKIP3                                                                
144100 MFS-ERASE-FIELD-IN SECTION.                                              
144200     MOVE '13'                     TO WS-SEC                              
144300                                                                          
144400     PERFORM VARYING INDX FROM 1 BY +1 UNTIL INDX > MAX-INDX              
144500       MOVE MFS-ERASE-FIELD        TO MOD-KDCMD        (INDX)             
144600     END-PERFORM                                                          
144700                                                                          
144800     MOVE MFS-ERASE-FIELD          TO MOD-IDDC-UPD                        
144900                                      MOD-IDDISTR-UPD                     
145000                                      MOD-IDKUNDNR-UPD                    
145100                                      MOD-IDDC-REC-UPD                    
145200                                      MOD-IDLTERM-UPD                     
145300                                      MOD-KVCOPIES-GMTL-UPD               
145400                                      MOD-KVCOPIES-KLIS-UPD               
145500                                      MOD-KVCOPIES-PACK-UPD               
145600                                      MOD-KVCOPIES-SPED-UPD               
145700                                      MOD-KVCOPIES-STAT-UPD               
145800                                      MOD-KVCOPIES-VERS-UPD               
145900                                      MOD-KVCOPIES-KULB-UPD               
146000                                      MOD-KVCOPIES-NAPR-UPD               
146100                                      MOD-KVCOPIES-BLAD-UPD               
146200                                      MOD-KVCOPIES-TRPT-UPD               
146300                                      MOD-KVDAGAR-UPD                     
146400                                      MOD-FLSKRIV-ONDEM-UPD               
146500                                                                          
146600     MOVE SPACE                    TO SAVE-IDDC-COPY                      
146700     MOVE ZERO                     TO SAVE-IDDISTR-COPY                   
146800     MOVE SPACE                    TO SAVE-IDKUND-COPY                    
146900     MOVE SPACE                    TO SAVE-IDDC-REC-COPY                  
147000     .                                                                    
147100     EJECT                                                                
147200 MFS-ADD-READ-FIELD-UPD  SECTION.                                         
147300     MOVE '16'                     TO WS-SEC                              
147400                                                                          
147500     MOVE MFS-ADD-READ-FIELD       TO MOD-IDDC-UPD-ATTR                   
147600                                      MOD-IDDISTR-UPD-ATTR                
147700                                      MOD-IDKUNDNR-UPD-ATTR               
147800                                      MOD-KVCOPIES-GMTL-UPD-ATTR          
147900                                      MOD-KVCOPIES-KLIS-UPD-ATTR          
148000                                      MOD-KVCOPIES-PACK-UPD-ATTR          
148100                                      MOD-KVCOPIES-SPED-UPD-ATTR          
148200                                      MOD-KVCOPIES-STAT-UPD-ATTR          
148300                                      MOD-KVCOPIES-VERS-UPD-ATTR          
148400                                      MOD-KVCOPIES-KULB-UPD-ATTR          
148500                                      MOD-KVCOPIES-NAPR-UPD-ATTR          
148600                                      MOD-KVCOPIES-BLAD-UPD-ATTR          
148700                                      MOD-KVCOPIES-TRPT-UPD-ATTR          
148800                                      MOD-IDDC-REC-UPD-ATTR               
148900                                      MOD-IDLTERM-UPD-ATTR                
149000                                      MOD-KVDAGAR-UPD-ATTR                
149100                                      MOD-FLSKRIV-ONDEM-UPD-ATTR          
149200     .                                                                    
149300     EJECT                                                                
149400 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
149500     MOVE '17'                     TO WS-SEC                              
149600                                                                          
149700     PERFORM VARYING INDX FROM 1 BY +1 UNTIL INDX > MAX-INDX              
149800       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD        (INDX)             
149900     END-PERFORM                                                          
150000                                                                          
150100     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDDC-UPD                        
150200                                      MOD-IDDISTR-UPD                     
150300                                      MOD-IDKUNDNR-UPD                    
150400                                      MOD-IDDC-REC-UPD                    
150500                                      MOD-IDLTERM-UPD                     
150600                                      MOD-KVCOPIES-GMTL-UPD               
150700                                      MOD-KVCOPIES-KLIS-UPD               
150800                                      MOD-KVCOPIES-PACK-UPD               
150900                                      MOD-KVCOPIES-SPED-UPD               
151000                                      MOD-KVCOPIES-STAT-UPD               
151100                                      MOD-KVCOPIES-VERS-UPD               
151200                                      MOD-KVCOPIES-KULB-UPD               
151300                                      MOD-KVCOPIES-NAPR-UPD               
151400                                      MOD-KVCOPIES-BLAD-UPD               
151500                                      MOD-KVCOPIES-TRPT-UPD               
151600                                      MOD-KVDAGAR-UPD                     
151700                                      MOD-FLSKRIV-ONDEM-UPD               
151800     .                                                                    
151900     EJECT                                                                
152000 MFS-FORM-ATTR SECTION.                                                   
152100     MOVE '18'                     TO WS-SEC                              
152200                                                                          
152300     PERFORM VARYING INDX FROM 1 BY +1 UNTIL INDX > MAX-INDX              
152400       MOVE MFS-FORMAT-DEFAULT-ATTR                                       
152500                                   TO MOD-KDCMD        (INDX)             
152600     END-PERFORM                                                          
152700                                                                          
152800     MOVE MFS-FORMAT-DEFAULT-ATTR  TO MOD-IDDC-UPD                        
152900                                      MOD-IDDISTR-UPD                     
153000                                      MOD-IDKUNDNR-UPD                    
153100                                      MOD-IDDC-REC-UPD                    
153200                                      MOD-IDLTERM-UPD                     
153300                                      MOD-KVCOPIES-GMTL-UPD               
153400                                      MOD-KVCOPIES-KLIS-UPD               
153500                                      MOD-KVCOPIES-PACK-UPD               
153600                                      MOD-KVCOPIES-SPED-UPD               
153700                                      MOD-KVCOPIES-STAT-UPD               
153800                                      MOD-KVCOPIES-VERS-UPD               
153900                                      MOD-KVCOPIES-KULB-UPD               
154000                                      MOD-KVCOPIES-NAPR-UPD               
154100                                      MOD-KVCOPIES-BLAD-UPD               
154200                                      MOD-KVCOPIES-TRPT-UPD               
154300                                      MOD-KVDAGAR-UPD                     
154400                                      MOD-FLSKRIV-ONDEM-UPD               
154500     .                                                                    
154600     EJECT                                                                
154700 S03-CHECK-DOC SECTION.                                                   
154800     MOVE 'S03'                    TO WS-SEC                              
154900                                                                          
155000     EVALUATE TRUE                                                        
155100     WHEN WS-KDTRPDOCT      = 'A'                                         
155200      AND DOK-KVCOPIES-KLIS > ZERO                                        
155300         MOVE YES                  TO DOC-SW                              
155400                                                                          
155500     WHEN WS-KDTRPDOCT      = 'B'                                         
155600      AND DOK-KVCOPIES-VERS > ZERO                                        
155700         MOVE YES                  TO DOC-SW                              
155800                                                                          
155900     WHEN WS-KDTRPDOCT      = 'C'                                         
156000      AND DOK-KVCOPIES-STAT > ZERO                                        
156100         MOVE YES                  TO DOC-SW                              
156200                                                                          
156300     WHEN WS-KDTRPDOCT      = 'D'                                         
156400      AND DOK-KVCOPIES-SPED > ZERO                                        
156500         MOVE YES                  TO DOC-SW                              
156600                                                                          
156700     WHEN WS-KDTRPDOCT      = 'E'                                         
156800      AND DOK-KVCOPIES-PACK > ZERO                                        
156900         MOVE YES                  TO DOC-SW                              
157000                                                                          
157100     WHEN WS-KDTRPDOCT      = 'F'                                         
157200      AND DOK-KVCOPIES-GMTL > ZERO                                        
157300         MOVE YES                  TO DOC-SW                              
157400                                                                          
157500     WHEN WS-KDTRPDOCT      = 'G'                                         
157600      AND DOK-KVCOPIES-KULB > ZERO                                        
157700         MOVE YES                  TO DOC-SW                              
157800                                                                          
157900     WHEN WS-KDTRPDOCT      = 'H'                                         
158000      AND DOK-KVCOPIES-NAPR > ZERO                                        
158100         MOVE YES                  TO DOC-SW                              
158200                                                                          
158300     WHEN WS-KDTRPDOCT      = 'I'                                         
158400      AND DOK-KVCOPIES-BLAD > ZERO                                        
158500         MOVE YES                  TO DOC-SW                              
158600                                                                          
158700     WHEN WS-KDTRPDOCT      = 'J'                                         
158800      AND DOK-KVCOPIES-TRPT > ZERO                                        
158900         MOVE YES                  TO DOC-SW                              
159000                                                                          
159100     END-EVALUATE                                                         
159200     .                                                                    
159300     EJECT                                                                
159400 IMS-GET-MSG SECTION.                                                     
159500     MOVE '21'                     TO WS-SEC                              
159600                                                                          
159700     MOVE '  QC'                   TO GOOD-STATUSCODES                    
159800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
159900     MOVE MSG-STATUS-CODE          TO STATUS-WS                           
160000     PERFORM IMS-STATUSCHECK                                              
160100     .                                                                    
160200     SKIP3                                                                
160300 IMS-INSERT-MSG SECTION.                                                  
160400     MOVE '22'                     TO WS-SEC                              
160500                                                                          
160600     MOVE LOW-VALUE                TO MSG-KDZ1 MSG-KDZ2                   
160700     MOVE SPACE                    TO GOOD-STATUSCODES                    
160800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
160900     MOVE MSG-STATUS-CODE          TO STATUS-WS                           
161000     PERFORM IMS-STATUSCHECK                                              
161100     .                                                                    
161200     EJECT                                                                
161300 IMS-GHU-WDB901 SECTION.                                                  
161400     MOVE '31'                     TO WS-SEC                              
161500                                                                          
161600     STRING 'WDB901  (WDB901KY =' W-WDB901KY-X ')'                        
161700          DELIMITED BY SIZE INTO SSA1                                     
161800     MOVE '  GE'                   TO GOOD-STATUSCODES                    
161900     CALL CBLTDLI USING GHU WDB9-PCB DLI-IO-WDB901 SSA1                   
162000     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
162100     PERFORM IMS-STATUSCHECK                                              
162200     .                                                                    
162300     SKIP3                                                                
162400 IMS-GU-WDB901 SECTION.                                                   
162500     MOVE '32'                     TO WS-SEC                              
162600     STRING 'WDB901  (WDB901KY>=' W-WDB901KY-MIN                          
162700                    '&WDB901KY<=' W-WDB901KY-MAX ')'                      
162800               DELIMITED BY SIZE INTO SSA1                                
162900     MOVE '  GE'                   TO GOOD-STATUSCODES                    
163000     CALL CBLTDLI USING GU WDB9-PCB DLI-IO-WDB901 SSA1                    
163100     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
163200     PERFORM IMS-STATUSCHECK                                              
163300     .                                                                    
163400     SKIP3                                                                
163500 IMS-GU-WDB901-DOC SECTION.                                               
163600     MOVE '33'                     TO WS-SEC                              
163700                                                                          
163800     STRING 'WDB901  (WDB901KY>=' W-WDB901KY-MIN                          
163900                    '&WDB901KY<=' W-WDB901KY-MAX                          
164000                                           W-DOC ')'                      
164100               DELIMITED BY SIZE INTO SSA1                                
164200                                                                          
164300     MOVE '  GE'                   TO GOOD-STATUSCODES                    
164400     CALL CBLTDLI USING GU WDB9-PCB DLI-IO-WDB901 SSA1                    
164500     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
164600     PERFORM IMS-STATUSCHECK                                              
164700     .                                                                    
164800     SKIP3                                                                
164900 IMS-GN-WDB901 SECTION.                                                   
165000     MOVE '34'                     TO WS-SEC                              
165100                                                                          
165200     MOVE '  GEGB'                 TO GOOD-STATUSCODES                    
165300     CALL CBLTDLI USING GN WDB9-PCB DLI-IO-WDB901 SSA1                    
165400     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
165500     PERFORM IMS-STATUSCHECK                                              
165600     .                                                                    
165700     SKIP3                                                                
165800 IMS-GU-WDB901-ASEQ SECTION.                                              
165900     MOVE '35'                     TO WS-SEC                              
166000                                                                          
166100     STRING 'WDB901  (WDB9ASEQ =' W-WDB9ASEQ-X ')'                        
166200               DELIMITED BY SIZE INTO SSA1                                
166300     MOVE '  GE'                   TO GOOD-STATUSCODES                    
166400     CALL CBLTDLI USING GU  WDB9ASEQ-PCB DLI-IO-WDB901 SSA1               
166500     MOVE WDB9ASEQ-STATUS-CODE         TO STATUS-WS                       
166600     PERFORM IMS-STATUSCHECK                                              
166700     .                                                                    
166800     SKIP3                                                                
166900 IMS-GU-WDB901-ASEQ-QUAL SECTION.                                         
167000     MOVE '36'                     TO WS-SEC                              
167100                                                                          
167200     STRING 'WDB901  (WDB9ASEQ =' W-WDB9ASEQ-X                            
167300                    '&IDDC     =' W-IDDC-MIN                              
167400                    '&IDKUND   =' W-IDKUND-MIN                            
167500                    '&IDDCREC  =' W-IDDC-REC-MIN  ')'                     
167600               DELIMITED BY SIZE INTO SSA1                                
167700     MOVE '  GE'                   TO GOOD-STATUSCODES                    
167800     CALL CBLTDLI USING GU  WDB9ASEQ-PCB DLI-IO-WDB901 SSA1               
167900     MOVE WDB9ASEQ-STATUS-CODE         TO STATUS-WS                       
168000     PERFORM IMS-STATUSCHECK                                              
168100     .                                                                    
168200     SKIP3                                                                
168300 IMS-GN-WDB901-ASEQ SECTION.                                              
168400     MOVE '37'                     TO WS-SEC                              
168500                                                                          
168600     STRING 'WDB901  (WDB9ASEQ =' W-WDB9ASEQ-X    ')'                     
168700               DELIMITED BY SIZE INTO SSA1                                
168800     MOVE '  GEGB'                 TO GOOD-STATUSCODES                    
168900     CALL CBLTDLI USING GN  WDB9ASEQ-PCB DLI-IO-WDB901 SSA1               
169000     MOVE WDB9ASEQ-STATUS-CODE         TO STATUS-WS                       
169100     PERFORM IMS-STATUSCHECK                                              
169200     .                                                                    
169300     SKIP3                                                                
169400 IMS-GU-WDB9A1      SECTION.                                              
169500     MOVE '38'                     TO WS-SEC                              
169600                                                                          
169700     STRING 'WDB9A1  (WDB9A1KY>=' W-WDB9A1KY-MIN                          
169800                    '&WDB9A1KY<=' W-WDB9A1KY-MAX ')'                      
169900               DELIMITED BY SIZE INTO SSA1                                
170000     MOVE '  GE'                   TO GOOD-STATUSCODES                    
170100     CALL CBLTDLI USING GU  WDB9A-PCB DLI-IO-WDB9A1 SSA1                  
170200     MOVE WDB9A-STATUS-CODE         TO STATUS-WS                          
170300     PERFORM IMS-STATUSCHECK                                              
170400     .                                                                    
170500     SKIP3                                                                
170600                                                                          
170700 IMS-GN-WDB9A1      SECTION.                                              
170800     MOVE '39'                     TO WS-SEC                              
170900     STRING 'WDB9A1  (WDB9A1KY>=' W-WDB9A1KY-MIN                          
171000                    '&WDB9A1KY<=' W-WDB9A1KY-MAX ')'                      
171100               DELIMITED BY SIZE INTO SSA1                                
171200     MOVE '  GEGB'                 TO GOOD-STATUSCODES                    
171300     CALL CBLTDLI USING GN  WDB9A-PCB DLI-IO-WDB9A1 SSA1                  
171400     MOVE WDB9A-STATUS-CODE        TO STATUS-WS                           
171500     PERFORM IMS-STATUSCHECK                                              
171600     .                                                                    
171700     SKIP3                                                                
171800 IMS-ISRT-WDB901 SECTION.                                                 
171900     MOVE '40'                     TO WS-SEC                              
172000                                                                          
172100     MOVE 'WDB901 '                TO SSA1                                
172200     MOVE '  II'                   TO GOOD-STATUSCODES                    
172300     CALL CBLTDLI USING ISRT WDB9-PCB DLI-IO-WDB901 SSA1                  
172400     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
172500     PERFORM IMS-STATUSCHECK                                              
172600     .                                                                    
172700     SKIP3                                                                
172800 IMS-REPL-WDB901 SECTION.                                                 
172900     MOVE '41'                     TO WS-SEC                              
173000                                                                          
173100     MOVE '  '                     TO GOOD-STATUSCODES                    
173200     CALL CBLTDLI USING REPL WDB9-PCB DLI-IO-WDB901                       
173300     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
173400     PERFORM IMS-STATUSCHECK                                              
173500     .                                                                    
173600     SKIP3                                                                
173700 IMS-DLET-WDB901 SECTION.                                                 
173800     MOVE '42'                     TO WS-SEC                              
173900                                                                          
174000     MOVE '  '                     TO GOOD-STATUSCODES                    
174100     CALL CBLTDLI USING DLET WDB9-PCB DLI-IO-WDB901                       
174200     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
174300     PERFORM IMS-STATUSCHECK                                              
174400     .                                                                    
174500     EJECT                                                                
174600 IMS-GU-WDB201 SECTION.                                                   
174700     MOVE '43'                     TO WS-SEC                              
174800                                                                          
174900     STRING 'WDB201  (IDGMT    =' W-WDB201KEY-MIN-X ')'                   
175000               DELIMITED BY SIZE INTO SSA1                                
175100     MOVE '  GE'                   TO GOOD-STATUSCODES                    
175200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
175300     MOVE WDB2-STATUS-CODE         TO STATUS-WS                           
175400     PERFORM IMS-STATUSCHECK                                              
175500     .                                                                    
175600     EJECT                                                                
175700 IMS-GU-WDB601    SECTION.                                                
175800     MOVE '44'                     TO WS-SEC                              
175900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
176000          DELIMITED BY SIZE INTO SSA-WDB6                                 
176100     MOVE '  GE' TO GOOD-STATUSCODES                                      
176200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA-WDB6             
176300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
176400     PERFORM IMS-STATUSCHECK                                              
176500     IF SEGMENT-MISSING                                                   
176600         MOVE SPACE TO DCS-KDDC                                           
176700     END-IF                                                               
176800     .                                                                    
176900 IMS-STATUSCHECK SECTION.                                                 
177000                                                                          
177100     SET STATUS-IX                 TO 1                                   
177200     SEARCH GOOD-STATUS                                                   
177300       AT END                                                             
177400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
177500         DELIMITED BY SIZE INTO ERROR-TEXT                                
177600         CALL FELLOG                                                      
177700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
177800         CONTINUE                                                         
177900     END-SEARCH                                                           
178000     .                                                                    
