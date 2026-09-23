000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2011100.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   FEB 1990.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        VISAR ALLMÄN LEVERANTÖRS-INFO.                                   
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W2T111                                              
001400*        MID:         W2I11101                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W2O11101                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600*                                                                         
002700 77  IDPGM                   PIC X(8)    VALUE 'W2011100'.                
002800 77  JA                      PIC X       VALUE 'J'.                       
002900 77  NEJ                     PIC X       VALUE 'N'.                       
003000 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
003100 77  MAX-IDATTENT            PIC S9(9)   VALUE +2   COMP SYNC.            
003200 77  MAX-PG                  PIC S9(3)   VALUE +8   COMP SYNC.            
003300 77  MAX-TILEVDAGAR          PIC 9(5)    VALUE 5.                         
003400 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +750 COMP SYNC.            
003500 77  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                     
003600 77  WS-KVDAGAR-TTC1         PIC X(2)    VALUE SPACE.                     
003700 77  WS-KVDAGAR-TTC1-IN      PIC 9(2)    VALUE ZERO.                      
003800 77  WS-KVVECKOR-LVAR-IN     PIC 9(2)V9  VALUE ZERO.                      
003900 77  WS-KVDAGAR-AVIAVV-IN    PIC X       VALUE SPACE.                     
004000 77  WS-KVDAGAR-INLAVV-IN    PIC X       VALUE SPACE.                     
004100 77  WS-IDANSK-PG            PIC 9(3)    VALUE ZERO.                      
004200 77  WDF1-IX                 PIC S9(9)   COMP SYNC.                       
004300 77  SPARA-IDATTENT          PIC  S9(3)  VALUE ZERO  COMP-3.              
004400 77  WS-DAG-POS              PIC X(2)    VALUE SPACE.                     
004500   88  WS-DAG-POS-GOOD                   VALUE 'MO' 'TU' 'WE' 'TH'        
004600                                               'MÅ' 'TI' 'ON' 'TO'        
004700                                               'FR'.                      
004800   88  WS-DAG-MO                         VALUE 'MO' 'MÅ'.                 
004900   88  WS-DAG-TU                         VALUE 'TU' 'TI'.                 
005000   88  WS-DAG-WE                         VALUE 'WE' 'ON'.                 
005100   88  WS-DAG-TH                         VALUE 'TH' 'TO'.                 
005200   88  WS-DAG-FR                         VALUE 'FR'.                      
005300                                                                          
005400 77  WS-DAG-POS-SW           PIC X       VALUE 'N'.                       
005500   88  WS-DAG-POS-YES                    VALUE 'J'.                       
005600   88  WS-DAG-POS-NO                     VALUE 'N'.                       
005700                                                                          
005800 77  WS-INSERT-SW            PIC X       VALUE 'N'.                       
005900   88  WS-INSERT-YES                     VALUE 'J'.                       
006000                                                                          
006100 77  INDATA-SW               PIC X       VALUE 'J'.                       
006200   88  INDATA-OK                         VALUE 'J'.                       
006300   88  INDATA-FEL                        VALUE 'N'.                       
006400                                                                          
006500 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
006600   88  NYCKLAR-OK                        VALUE 'J'.                       
006700   88  NYCKLAR-FEL                       VALUE 'N'.                       
006800                                                                          
006900 77  TILEVDAG-CDC-SW         PIC X       VALUE 'N'.                       
007000   88  TILEVDAG-CDC                      VALUE 'J'.                       
007100                                                                          
007200 77  TILEVDAG-NDC-SW         PIC X       VALUE 'N'.                       
007300   88  TILEVDAG-NDC                      VALUE 'J'.                       
007400                                                                          
007500 77  LOGG-NDC-2214-SW        PIC X       VALUE 'N'.                       
007600   88  LOGG-NDC-2214                     VALUE 'J'.                       
007700                                                                          
007800 77  IDANSK-UPD-CN-UPD-SW    PIC X       VALUE 'N'.                       
007900   88  IDANSK-UPD-CN-JA                  VALUE 'J'.                       
008000                                                                          
008100 77  IDANSK-UPD-CDC-UPD-SW    PIC X       VALUE 'N'.                      
008200   88  IDANSK-UPD-CDC-JA                  VALUE 'J'.                      
008300                                                                          
008400 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
008500   88  EGEN-TRANS                        VALUE '2111'.                    
008600   88  GODK-TRANS                        VALUE '2111'.                    
008700*                                              '2112' '2113'              
008800*                                              '2114' '2115'.             
008900   88  HELP-TRANS                        VALUE '0551'.                    
009000     EJECT                                                                
009100     EJECT                                                                
009200*01  -COPY WWDCKONS                                                       
009300*                                                                         
009400 01  WORK-AREAS.                                                          
009500     05  W-DATUM-X.                                                       
009600         10  W-DATUM         PIC 9(6).                                    
009700                                                                          
009800     05  W-TIME-X.                                                        
009900         10  W-TIME-TT       PIC 9(2).                                    
010000         10  FILLER          PIC 9(2).                                    
010100         10  W-TIME-SS       PIC 9(2).                                    
010200         10  FILLER          PIC 9(2).                                    
010300     05  W-TIME-N            REDEFINES W-TIME-X                           
010400                             PIC 9(8).                                    
010500     05  W-TIKLOCK           PIC S9(9)  VALUE 0   COMP-3.                 
010600     05  W-IDSEKVNR          PIC S9(3)  VALUE 0   COMP-3.                 
010700                                                                          
010800 01  PARM-TESYMBV.                                                        
010900     03  FILLER                  PIC X(8)    VALUE 'IDLEVNR('.            
011000     03  PARM-IDLEVNR            PIC X(5).                                
011100     03  FILLER                  PIC X(1)    VALUE ')'.                   
011200     03  FILLER                  PIC X(3)    VALUE 'DC('.                 
011300     03  PARM-IDDC               PIC X(2).                                
011400     03  FILLER                  PIC X(1)    VALUE ')'.                   
011500                                                                          
011600 01  PARM-W224S1.                                                         
011700     03  FILLER                  PIC X(8)    VALUE 'IDLEVNR('.            
011800     03  PARM-IDLEVNR-W224S1     PIC X(5).                                
011900     03  FILLER                  PIC X(1)    VALUE ')'.                   
012000     03  FILLER                  PIC X(3)    VALUE 'DC('.                 
012100     03  PARM-IDDC-W224S1        PIC X(2).                                
012200     03  FILLER                  PIC X(1)    VALUE ')'.                   
012300                                                                          
012400 01  PARM-W224S2.                                                         
012500     03  FILLER                  PIC X(8)    VALUE 'IDLEVNR('.            
012600     03  PARM-W224S2-IDLEVNR     PIC X(5).                                
012700     03  FILLER                  PIC X(1)    VALUE ')'.                   
012800     03  FILLER                  PIC X(3)    VALUE 'DC('.                 
012900     03  PARM-W224S2-IDDC        PIC X(2).                                
013000     03  FILLER                  PIC X(1)    VALUE ')'.                   
013100     03  FILLER                  PIC X(3)    VALUE 'AT('.                 
013200     03  PARM-W224S2-KVVECKOR-AT PIC X(2).                                
013300     03  FILLER                  PIC X(1)    VALUE ')'.                   
013400     03  FILLER                  PIC X(3)    VALUE 'LT('.                 
013500     03  PARM-W224S2-KVVECKOR-LT PIC X(2).                                
013600     03  FILLER                  PIC X(1)    VALUE ')'.                   
013700     03  FILLER                  PIC X(3)    VALUE 'TT('.                 
013800     03  PARM-W224S2-KVDAGAR-TT  PIC X(2).                                
013900     03  FILLER                  PIC X(1)    VALUE ')'.                   
014000     03  FILLER                  PIC X(42)   VALUE SPACE.                 
014100                                                                          
014200 01  GENERELLA-SUBPROGRAM.                                                
014300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
014400   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
014500   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
014600   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
014700   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
014800     EJECT                                                                
014900                                                                          
015000 01  MESSAGE-CODES.                                                       
015100     03  ERR-HILITE-FIELDS-WRONG PIC X(3)    VALUE '409'.                 
015200     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
015300     03  ERR-NOT-AUTH            PIC X(3)    VALUE '405'.                 
015400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015800     03  ERR-FIELDS-NOT-NUMERIC  PIC X(3)    VALUE '020'.                 
015900     03  ERR-WRONG-SUPP-NO       PIC X(3)    VALUE '092'.                 
016000     03  ERR-SUPP-MISSING        PIC X(3)    VALUE '273'.                 
016100     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '005'.                 
016200     03  ERR-SUPP-NOT-UPD-DC     PIC X(3)    VALUE '437'.                 
016300     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
016400     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
016500     EJECT                                                                
016600                                                                          
016700*    --- AREA FÖR SOP ANROP                                               
016800 01  W-PROG-TO-PROG-SW.                                                   
016900*03 -COPY WMSGSOP                                                         
017000******************************************************************        
017100*                                                                         
017200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
017300*                                                                         
017400 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
017500     SKIP3                                                                
017600*01  MID -COPY W2I11101                                                   
017700     EJECT                                                                
017800*01  -COPY WMSGAREA                                                       
017900     EJECT                                                                
018000*  03  MOD -COPY W2O11101 -RED MSG-AREA.                                  
018100     EJECT                                                                
018200*01  -COPY WMFSAREA                                                       
018300     EJECT                                                                
018400                                                                          
018500 01  FILLER             PIC X(16) VALUE 'WMSGINIT     '.                  
018600*                    **** PARAMETRAR TILL W005INIT                        
018700*01   -COPY WMSGINIT.                                                     
018800     EJECT                                                                
018900 01  FILLER             PIC X(16) VALUE 'WMEDKONV     '.                  
019000*01   -COPY WMEDAREA.                                                     
019100     EJECT                                                                
019200 01  FILLER             PIC X(16) VALUE 'WDECEDIT     '.                  
019300*01   -COPY WDECAREA.                                                     
019400     EJECT                                                                
019500 01  FILLER             PIC X(16) VALUE 'WWDC99       '.                  
019600*01   -COPY WWDC99.                                                       
019700     EJECT                                                                
019800 01  FILLER             PIC X(16) VALUE 'DC-WWDC99    '.                  
019900*01   -COPY WWDC99   -PRE DC-                                             
020000     EJECT                                                                
020100 01  FILLER             PIC X(16) VALUE 'WWIDFTG      '.                  
020200*01 -COPY WWIDFTG                                                         
020300                                                                          
020400     EJECT                                                                
020500******************************************************************        
020600*                                                                         
020700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020800*                                                                         
020900 01  IMS-WS.                                                              
021000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
021100     SKIP3                                                                
021200*                        **** STATUS-KOD FRÅN IMS                         
021300   03  STATUS-WS             PIC XX.                                      
021400     88  SEGMENT-FINNS                   VALUE '  '.                      
021500     88  SEGMENT-UPD                     VALUE '  '.                      
021600     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
021700     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
021800     SKIP3                                                                
021900   03  GODK-STATUSKODER.                                                  
022000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022100     SKIP3                                                                
022200 01  NYCKLAR-TILL-DLI.                                                    
022300   03  W-IDLEVNR-X.                                                       
022400     05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                     
022500   03  W-IDDC-X.                                                          
022600     05  W-IDDC              PIC X(2)    VALUE SPACE.                     
022700   03  W-IDATTENT-X.                                                      
022800     05  W-IDATTENT          PIC  S9(3)  VALUE ZERO  COMP-3.              
022900                                                                          
023000   03  W-WDGXKEY-2255-X.                                                  
023100     05  W-IDHTYP-2255       PIC X(4)    VALUE '2255'.                    
023200         05  FILLER          PIC X(26)   VALUE LOW-VALUE.                 
023300   03  W-IDDC-2256-X.                                                     
023400     05  W-IDDC-2256         PIC  X(2)   VALUE SPACE.                     
023500   03  W-IDLEVNR-2256-X.                                                  
023600     05  W-IDLEVNR-2256      PIC  X(2)   VALUE SPACE.                     
023700     SKIP3                                                                
023800 01    SSA1                  PIC X(64).                                   
023900 01    SSA2                  PIC X(64).                                   
024000     EJECT                                                                
024100*                            IMS FUNKTIONSKODER                           
024200*01    -COPY W0003                                                        
024300     EJECT                                                                
024400*                            DLI INPUT-OUTPUT AREA                        
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
024600 01  DLI-IO-WDF101.                                                       
024700*    03  -COPY WDF101 -PRE WDF1-                                          
024800     EJECT                                                                
024900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
025000 01  DLI-IO-WDF106.                                                       
025100*    03  -COPY WDF106 -PRE WDF1-                                          
025200     EJECT                                                                
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF107'.                      
025400 01  DLI-IO-WDF107.                                                       
025500*    03  -COPY WDF107                                                     
025600     EJECT                                                                
025700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
025800 01  DLI-IO-WDF116.                                                       
025900*    03  -COPY WDF116 -PRE WDF1-                                          
026000     EJECT                                                                
026100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
026200 01  DLI-IO-WDB601.                                                       
026300*    03  -COPY WDB601 -PRE WDB601-                                        
026400     EJECT                                                                
026500*                            DLI INPUT-OUTPUT AREA WDR301                 
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR301'.                      
026700 01  DLI-IO-AREA-WDR301.                                                  
026800*  03  -COPY WDR301                                                       
026900                                                                          
027000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2256'.                    
027100 01  DLI-IO-AREA-2256.                                                    
027200*    03  -COPY WDGX2256 -PRE HTR-                                         
027300     EJECT                                                                
027400 LINKAGE SECTION.                                                         
027500*01  -COPY W0009     -PRE MSG-                                            
027600     EJECT                                                                
027700*01  -COPY W0009     -PRE ALT-                                            
027800                                                                          
027900*01  -COPY W0008     -PRE WDF1-                                           
028000     05  FILLER           PIC X.                                          
028100     EJECT                                                                
028200*01  -COPY W0008     -PRE WDB6-                                           
028300     05  FILLER           PIC X.                                          
028400     EJECT                                                                
028500*01  -COPY W0008     -PRE WDP7-                                           
028600     05  FILLER           PIC X.                                          
028700     EJECT                                                                
028800*01  -COPY W0008     -PRE WDR3-                                           
028900     05  FILLER           PIC X.                                          
029000     EJECT                                                                
029100*01  -COPY W0008     -PRE WDG3-                                           
029200     05  FILLER           PIC X.                                          
029300     EJECT                                                                
029400 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB WDF1-PCB WDB6-PCB              
029500                           WDP7-PCB WDR3-PCB WDG3-PCB.                    
029600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDF1-PCB WDB6-PCB              
029700                           WDP7-PCB WDR3-PCB WDG3-PCB.                    
029800                                                                          
029900     PERFORM IMS-GET-MSG                                                  
030000     IF SEGMENT-FINNS                                                     
030100       PERFORM A-INIT                                                     
030200       PERFORM B-KOLLA-NYCKLAR                                            
030300       IF NYCKLAR-OK                                                      
030400         IF MFS-UPDATE OR MFS-UPD-V                                       
030500           PERFORM G-CHECK-INPUT                                          
030600           IF INDATA-OK                                                   
030700             PERFORM H-UPDATE                                             
030800           END-IF                                                         
030900         ELSE                                                             
031000           IF MFS-FIRST                                                   
031100             PERFORM C-BEHANDLA-LEVERANTOER                               
031200           ELSE                                                           
031300             IF MFS-NEXT                                                  
031400               PERFORM D-NAESTA-SIDA                                      
031500             ELSE                                                         
031600               PERFORM E-SAMMA-SIDA                                       
031700             END-IF                                                       
031800           END-IF                                                         
031900         END-IF                                                           
032000         IF INDATA-OK OR                                                  
032100            MED-IDMFSFEL = ERR-NOT-AUTH                                   
032200            PERFORM F-LAES-VISA-INFO                                      
032300         END-IF                                                           
032400       END-IF                                                             
032500       MOVE MAX-MOD-LAENGD       TO MSG-KVLL                              
032600       PERFORM IMS-INSERT-MSG                                             
032700     END-IF                                                               
032800                                                                          
032900     MOVE ZERO                   TO RETURN-CODE                           
033000     GOBACK                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 A-INIT SECTION.                                                          
033400                                                                          
033500     IF MSG-DUBBLA-TRANSKODER                                             
033600       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
033700                                 TO MID-W2I11101                          
033800       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
033900       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
034000     ELSE                                                                 
034100       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
034200                                 TO MID-W2I11101                          
034300       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
034400       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
034500     END-IF                                                               
034600                                                                          
034700     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
034800     MOVE MSG-IDPFK              TO MFS-IDPFK                             
034900     MOVE MFS-IDTRANS            TO W-IDTRANS                             
035000                                                                          
035100     MOVE LOW-VALUE              TO MSG-AREA                              
035200     MOVE '2111'                 TO MOD-IDTRANS                           
035300     IF SWEDISH-TEXT                                                      
035400        MOVE 'W2O11101'          TO MFS-IDMOD                             
035500     ELSE                                                                 
035600        MOVE 'W2O111N1'          TO MFS-IDMOD                             
035700     END-IF                                                               
035800     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL                          
035900                                    MOD-TEMFSINF                          
036000                                                                          
036100     IF EGEN-TRANS OR HELP-TRANS                                          
036200       CONTINUE                                                           
036300     ELSE                                                                 
036400       MOVE SPACE                TO MFS-KDTRTYP                           
036500       MOVE '7'                  TO MFS-IDPFK                             
036600     END-IF                                                               
036700     ACCEPT W-DATUM-X     FROM DATE                                       
036800     ACCEPT W-TIME-X      FROM TIME                                       
036900     MOVE W-TIME-N        TO W-TIKLOCK                                    
037000     .                                                                    
037100     EJECT                                                                
037200 B-KOLLA-NYCKLAR SECTION.                                                 
037300                                                                          
037400     MOVE ALL '+'                TO MSGI-WMSGINIT                         
037500     MOVE '001'                  TO MSGI-KDCALL                           
037600     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
037700     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
037800     MOVE '2111'                 TO MSGI-IDTRANS                          
037900     IF GODK-TRANS                                                        
038000       MOVE MID-IDLEVNR-IN       TO MSGI-IDLEVNR                          
038100       MOVE MID-IDDC-IN          TO MSGI-IDDC-KEY                         
038200     ELSE                                                                 
038300       MOVE ALL '+'              TO MID-IDDC-IN                           
038400                                    MID-IDLEVNR-IN                        
038500     END-IF                                                               
038600     CALL W005INIT            USING MSGI-WMSGINIT                         
038700                                    WDP7-PCB                              
038800                                                                          
038900     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
039000     MOVE JA                     TO NYCKLAR-SW                            
039100                                                                          
039200     MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR-IN                        
039300                                                                          
039400     IF MID-IDLEVNR-IN = ALL '+'                                          
039500       MOVE MSGI-IDLEVNR         TO WS-IDLEVNR                            
039600     ELSE                                                                 
039700       MOVE MID-IDLEVNR-IN       TO WS-IDLEVNR                            
039800       MOVE '7'                  TO MFS-IDPFK                             
039900       MOVE SPACE                TO MFS-KDTRTYP                           
040000     END-IF                                                               
040100                                                                          
040200     IF WS-IDLEVNR NOT = SPACE                                            
040300       MOVE WS-IDLEVNR           TO W-IDLEVNR                             
040400     ELSE                                                                 
040500       MOVE NEJ                  TO NYCKLAR-SW                            
040600       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
040700     END-IF                                                               
040800                                                                          
040900     MOVE MFS-RENSA-FAELT        TO MOD-IDDC-IN                           
041000                                                                          
041100     IF MID-IDDC-IN = ALL '+'                                             
041200       MOVE MSGI-IDDC-KEY        TO WS-IDDC                               
041300       IF CDC-SE OR NDC-CN OR NDC-US                                      
041400          CONTINUE                                                        
041500       ELSE                                                               
041600          MOVE WC-CDC-SE         TO WS-IDDC                               
041700       END-IF                                                             
041800     ELSE                                                                 
041900       MOVE MID-IDDC-IN          TO WS-IDDC                               
042000       MOVE '7'                  TO MFS-IDPFK                             
042100       MOVE SPACE                TO MFS-KDTRTYP                           
042200     END-IF                                                               
042300                                                                          
042400     IF WS-IDDC = SPACE                                                   
042500       MOVE MSGI-IDDC            TO WS-IDDC                               
042600     END-IF                                                               
042700                                                                          
042800     IF CDC-SE OR NDC-CN OR NDC-US                                        
042900       MOVE WS-IDDC              TO W-IDDC                                
043000     ELSE                                                                 
043100       MOVE NEJ                  TO NYCKLAR-SW                            
043200       MOVE ERR-WRONG-DC         TO MED-IDMFSFEL                          
043300     END-IF                                                               
043400                                                                          
043500     IF GODK-TRANS OR NYCKLAR-OK                                          
043600       MOVE WS-IDLEVNR           TO MOD-IDLEVNR-UT                        
043700       MOVE WS-IDDC              TO MOD-IDDC-UT                           
043800     ELSE                                                                 
043900       IF NYCKLAR-FEL                                                     
044000         MOVE WS-IDLEVNR         TO MOD-IDLEVNR-UT                        
044100         MOVE WS-IDDC            TO MOD-IDDC-UT                           
044200       ELSE                                                               
044300         MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-UT                        
044400                                    MOD-IDDC-UT                           
044500       END-IF                                                             
044600     END-IF                                                               
044700                                                                          
044800     IF NYCKLAR-FEL                                                       
044900       CALL WMEDKONV          USING MED-WMEDAREA                          
045000       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
045100       PERFORM MFS-ERASE-FIELD-IN                                         
045200       PERFORM MFS-ERASE-FIELD-UT                                         
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 C-BEHANDLA-LEVERANTOER SECTION.                                          
045700                                                                          
045800     PERFORM MFS-ERASE-FIELD-IN                                           
045900     .                                                                    
046000     EJECT                                                                
046100 D-NAESTA-SIDA SECTION.                                                   
046200                                                                          
046300*    PERFORM MFS-ROER-EJ-BILD                                             
046400     MOVE MID-IDATTENT-NEXT TO W-IDATTENT                                 
046500     .                                                                    
046600     EJECT                                                                
046700 E-SAMMA-SIDA SECTION.                                                    
046800                                                                          
046900     IF EGEN-TRANS OR HELP-TRANS                                          
047000       IF MID-INPUT = ALL '+' AND MID-INPUT1 = ALL '+'                    
047100         PERFORM MFS-ERASE-FIELD-IN                                       
047200       ELSE                                                               
047300         IF MID-INPUT1 NOT = ALL '+'                                      
047400           MOVE INF-PRESS-PF23 TO MED-IDMFSINF                            
047600         ELSE                                                             
047700           MOVE INF-PRESS-PF11     TO MED-IDMFSINF                        
047800         END-IF                                                           
047900         CALL WMEDKONV        USING MED-WMEDAREA                          
048000         MOVE MED-MFSINF         TO MOD-TEMFSINF                          
048100         PERFORM EA-MID-INDATA-TO-MOD                                     
048200       END-IF                                                             
048300     ELSE                                                                 
048400       PERFORM MFS-ERASE-FIELD-IN                                         
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 EA-MID-INDATA-TO-MOD SECTION.                                            
048900                                                                          
049100     IF MID-KVDAGAR-TTC1-IN NOT = ALL '+'                                 
049200       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
049300                                 TO MOD-KVDAGAR-TTC1-IN                   
049400       MOVE MFS-ADD-READ-FIELD   TO MOD-KVDAGAR-TTC1-IN-ATTR              
049500     ELSE                                                                 
049600       MOVE MFS-ERASE-FIELD      TO MOD-KVDAGAR-TTC1-IN                   
049700     END-IF                                                               
049800                                                                          
049900     IF MID-KVVECKOR-LVAR-IN NOT = ALL '+'                                
050000       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
050100                                 TO MOD-KVVECKOR-LVAR-IN                  
050200       MOVE MFS-ADD-READ-FIELD   TO MOD-KVVECKOR-LVAR-IN-ATTR             
050300     ELSE                                                                 
050400       MOVE MFS-ERASE-FIELD      TO MOD-KVVECKOR-LVAR-IN                  
050500     END-IF                                                               
050600                                                                          
050700     IF MID-KVDAGAR-AVIAVV-IN NOT = ALL '+'                               
050800       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
050900                                 TO MOD-KVDAGAR-AVIAVV-IN                 
051000       MOVE MFS-ADD-READ-FIELD   TO MOD-KVDAGAR-AVIAVV-IN-ATTR            
051100     ELSE                                                                 
051200       MOVE MFS-ERASE-FIELD      TO MOD-KVDAGAR-AVIAVV-IN                 
051300     END-IF                                                               
051400                                                                          
051500     IF MID-KVDAGAR-INLAVV-IN NOT = ALL '+'                               
051600       MOVE MFS-DO-NOT-TOUCH-FIELD                                        
051700                                 TO MOD-KVDAGAR-INLAVV-IN                 
051800       MOVE MFS-ADD-READ-FIELD   TO MOD-KVDAGAR-INLAVV-IN-ATTR            
051900     ELSE                                                                 
052000       MOVE MFS-ERASE-FIELD      TO MOD-KVDAGAR-INLAVV-IN                 
052100     END-IF                                                               
052200                                                                          
052300     PERFORM                                                              
052400     VARYING INDX FROM +1 BY +1                                           
052500       UNTIL INDX > MAX-PG                                                
052600       IF MID-IDANSK-PG-IN (INDX) NOT = ALL '+'                           
052700         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
052800                                 TO MOD-IDANSK-PG-IN (INDX)               
052900         MOVE MFS-ADD-READ-FIELD TO MOD-IDANSK-PG-IN-ATTR (INDX)          
053000       ELSE                                                               
053100         MOVE MFS-ERASE-FIELD    TO MOD-IDANSK-PG-IN (INDX)               
053200       END-IF                                                             
053300     END-PERFORM                                                          
053400                                                                          
053500     PERFORM                                                              
053600     VARYING INDX FROM +1 BY +1                                           
053700       UNTIL INDX > MAX-TILEVDAGAR                                        
053800       IF MID-DAG-POS-IN (INDX) NOT = ALL '+'                             
053900         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
054000                                 TO MOD-DAG-POS (INDX)                    
054100         MOVE MFS-ADD-READ-FIELD TO MOD-DAG-POS-ATTR (INDX)               
054200       ELSE                                                               
054300         MOVE MFS-DO-NOT-TOUCH-FIELD                                      
054400                                 TO MOD-DAG-POS (INDX)                    
054500       END-IF                                                             
054600     END-PERFORM                                                          
054700                                                                          
054800     .                                                                    
054900     EJECT                                                                
055000 F-LAES-VISA-INFO SECTION.                                                
055100                                                                          
055200     PERFORM IMS-GET-WDF1-ROT                                             
055300     IF SEGMENT-SAKNAS                                                    
055400       MOVE ERR-SUPP-MISSING     TO MED-IDMFSFEL                          
055500       CALL WMEDKONV          USING MED-WMEDAREA                          
055600       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
055700       PERFORM MFS-ERASE-FIELD-UT                                         
055800     ELSE                                                                 
055900       MOVE WDF1-LEV-IDLEVNR-MOTSV                                        
056000                                 TO MOD-IDLEVNR-MOTSV                     
056100                                                                          
056200       IF WDF1-LEV-FLRSADR = JA                                           
056300         IF SWEDISH-TEXT                                                  
056400           MOVE 'J'              TO MOD-FLRSADR                           
056500         ELSE                                                             
056600           MOVE 'Y'              TO MOD-FLRSADR                           
056700         END-IF                                                           
056800       ELSE                                                               
056900         MOVE 'N'                TO MOD-FLRSADR                           
057000       END-IF                                                             
057100       MOVE WDF1-LEV-KDLEVTYP    TO MOD-KDLEVTYP                          
057200       MOVE WDF1-LEV-KVVECKOR-LT TO MOD-KVVECKOR-LT                       
057300                                                                          
057400       PERFORM IMS-GET-WDF1-ADRESS                                        
057500       IF SEGMENT-FINNS                                                   
057600         MOVE WDF1-ADR-BELEV-VCC TO MOD-BELEV-VCC                         
057700         MOVE WDF1-ADR-ADLEV-RAD1                                         
057800                                 TO MOD-ADLEV-RAD1                        
057900         MOVE WDF1-ADR-ADLEV-RAD2-VCC                                     
058000                                 TO MOD-ADLEV-RAD2-VCC                    
058100         MOVE WDF1-ADR-ADLEV-ORT-VCC                                      
058200                                 TO MOD-ADLEV-ORT-VCC                     
058300         MOVE WDF1-ADR-ADLEVLND  TO MOD-ADLEVLND                          
058400         MOVE WDF1-ADR-IDLANDX2  TO MOD-IDLANDX2                          
058500         MOVE WDF1-ADR-IDLEVTLF  TO MOD-IDLEVTLF                          
058600         MOVE WDF1-ADR-IDLEVFAX  TO MOD-IDLEVFAX                          
058700       ELSE                                                               
058800         MOVE MFS-ERASE-FIELD    TO MOD-BELEV-VCC                         
058900                                    MOD-ADLEV-RAD1                        
059000                                    MOD-ADLEV-RAD2-VCC                    
059100                                    MOD-ADLEV-ORT-VCC                     
059200                                    MOD-ADLEVLND                          
059300                                    MOD-IDLANDX2                          
059400                                    MOD-IDLEVTLF                          
059500                                    MOD-IDLEVFAX                          
059600       END-IF                                                             
059700                                                                          
059800       IF CDC-SE                                                          
059900         MOVE 1                  TO INDX                                  
060000         PERFORM                                                          
060100           UNTIL INDX > MAX-PG                                            
060200           MOVE WDF1-LEV-IDANSK-PG(INDX)                                  
060300                                 TO MOD-IDANSK-PG-UT (INDX)               
060400           ADD 1                 TO INDX                                  
060500         END-PERFORM                                                      
060600                                                                          
060700         MOVE WDF1-LEV-KVDAGAR-TTC1                                       
060800                                 TO MOD-KVDAGAR-TTC1-UT                   
060900                                                                          
061000         MOVE +1                 TO INDX                                  
061100                                                                          
061200         IF WDF1-LEV-TILEVDAG(1) = 1                                      
061300           MOVE 'MO'             TO MOD-SPAR-DAG (INDX)                   
061400           ADD +1                TO INDX                                  
061500         END-IF                                                           
061600                                                                          
061700         IF WDF1-LEV-TILEVDAG(2) = 2                                      
061800           MOVE 'TU'             TO MOD-SPAR-DAG (INDX)                   
061900           ADD +1                TO INDX                                  
062000         END-IF                                                           
062100                                                                          
062200         IF WDF1-LEV-TILEVDAG(3) = 3                                      
062300           MOVE 'WE'             TO MOD-SPAR-DAG (INDX)                   
062400           ADD +1                TO INDX                                  
062500         END-IF                                                           
062600                                                                          
062700         IF WDF1-LEV-TILEVDAG(4) = 4                                      
062800           MOVE 'TH'             TO MOD-SPAR-DAG (INDX)                   
062900           ADD +1                TO INDX                                  
063000         END-IF                                                           
063100                                                                          
063200         IF WDF1-LEV-TILEVDAG(5) = 5                                      
063300           MOVE 'FR'             TO MOD-SPAR-DAG (INDX)                   
063400         END-IF                                                           
063500                                                                          
063600         IF INDX < MAX-TILEVDAGAR                                         
063700           ADD 1                 TO INDX                                  
063800           PERFORM                                                        
063900             UNTIL INDX > MAX-TILEVDAGAR                                  
064000             MOVE MFS-ERASE-FIELD                                         
064100                                 TO MOD-SPAR-DAG (INDX)                   
064200             ADD 1               TO INDX                                  
064300           END-PERFORM                                                    
064400         END-IF                                                           
064500                                                                          
064600         MOVE WDF1-LEV-KVVECKOR-LVAR                                      
064700                                 TO MOD-KVVECKOR-LVAR-UT                  
064800                                                                          
064900         MOVE WDF1-LEV-KVDAGAR-AVIAVV                                     
065000                                 TO MOD-KVDAGAR-AVIAVV-UT                 
065100                                                                          
065200         MOVE WDF1-LEV-KVDAGAR-INLAVV                                     
065300                                 TO MOD-KVDAGAR-INLAVV-UT                 
065400       ELSE                                                               
065500         PERFORM IMS-GET-WDF116                                           
065600         IF SEGMENT-FINNS                                                 
065700           MOVE 1                TO INDX                                  
065800           PERFORM                                                        
065900             UNTIL INDX > MAX-PG                                          
066000             MOVE WDF1-NDC-IDANSK-PG(INDX)                                
066100                                 TO MOD-IDANSK-PG-UT (INDX)               
066200             ADD 1               TO INDX                                  
066300           END-PERFORM                                                    
066400                                                                          
066500           MOVE WDF1-NDC-KVDAGAR-TT                                       
066600                                 TO MOD-KVDAGAR-TTC1-UT                   
066700                                                                          
066800           MOVE +1               TO INDX                                  
066900                                                                          
067000           IF WDF1-NDC-TILEVDAG(1) = 1                                    
067100             MOVE 'MO'           TO MOD-SPAR-DAG (INDX)                   
067200             ADD +1              TO INDX                                  
067300           END-IF                                                         
067400                                                                          
067500           IF WDF1-NDC-TILEVDAG(2) = 2                                    
067600             MOVE 'TU'           TO MOD-SPAR-DAG (INDX)                   
067700             ADD +1              TO INDX                                  
067800           END-IF                                                         
067900                                                                          
068000           IF WDF1-NDC-TILEVDAG(3) = 3                                    
068100             MOVE 'WE'           TO MOD-SPAR-DAG (INDX)                   
068200             ADD +1              TO INDX                                  
068300           END-IF                                                         
068400                                                                          
068500           IF WDF1-NDC-TILEVDAG(4) = 4                                    
068600             MOVE 'TH'           TO MOD-SPAR-DAG (INDX)                   
068700             ADD +1              TO INDX                                  
068800           END-IF                                                         
068900                                                                          
069000           IF WDF1-NDC-TILEVDAG(5) = 5                                    
069100             MOVE 'FR'           TO MOD-SPAR-DAG (INDX)                   
069200           END-IF                                                         
069300                                                                          
069400           IF INDX < MAX-TILEVDAGAR                                       
069500             ADD 1               TO INDX                                  
069600             PERFORM                                                      
069700               UNTIL INDX > MAX-TILEVDAGAR                                
069800               MOVE MFS-ERASE-FIELD                                       
069900                                 TO MOD-SPAR-DAG (INDX)                   
070000               ADD 1             TO INDX                                  
070100             END-PERFORM                                                  
070200           END-IF                                                         
070300                                                                          
070400           MOVE WDF1-NDC-KVVECKOR-LVAR                                    
070500                                 TO MOD-KVVECKOR-LVAR-UT                  
070600                                                                          
070700           MOVE WDF1-NDC-KVDAGAR-AVIAVV                                   
070800                                 TO MOD-KVDAGAR-AVIAVV-UT                 
070900                                                                          
071000           MOVE WDF1-NDC-KVDAGAR-INLAVV                                   
071100                                 TO MOD-KVDAGAR-INLAVV-UT                 
071200         ELSE                                                             
071300           MOVE MFS-ERASE-FIELD  TO MOD-KVDAGAR-AVIAVV-UT                 
071400                                    MOD-KVDAGAR-INLAVV-UT                 
071500                                    MOD-KVDAGAR-TTC1-UT                   
071600                                    MOD-KVVECKOR-LVAR-UT                  
071700           PERFORM                                                        
071800           VARYING INDX FROM 1 BY 1                                       
071900             UNTIL INDX > MAX-TILEVDAGAR                                  
072000             MOVE MFS-ERASE-FIELD                                         
072100                                 TO MOD-SPAR-DAG (INDX)                   
072200           END-PERFORM                                                    
072300                                                                          
072400           PERFORM                                                        
072500           VARYING INDX FROM 1 BY 1                                       
072600             UNTIL INDX > MAX-PG                                          
072700             MOVE MFS-ERASE-FIELD                                         
072800                                 TO MOD-IDANSK-PG-UT (INDX)               
072900           END-PERFORM                                                    
073000                                                                          
073100           MOVE ERR-SUPP-NOT-UPD-DC                                       
073200                                 TO MED-IDMFSFEL                          
073300           CALL WMEDKONV      USING MED-WMEDAREA                          
073400           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
073500         END-IF                                                           
073600       END-IF                                                             
073700                                                                          
073800       SET WS-DAG-POS-NO         TO TRUE                                  
073900       PERFORM                                                            
074000       VARYING INDX FROM 1 BY 1                                           
074100         UNTIL INDX > MAX-TILEVDAGAR OR                                   
074200               WS-DAG-POS-YES                                             
074300         IF MID-DAG-POS-IN (INDX) = ALL '+'                               
074400           CONTINUE                                                       
074500         ELSE                                                             
074600           SET WS-DAG-POS-YES    TO TRUE                                  
074700         END-IF                                                           
074800       END-PERFORM                                                        
074900                                                                          
075000       IF MFS-QUERY AND                                                   
075100          MFS-ENTER AND                                                   
075200          WS-DAG-POS-YES                                                  
075300         CONTINUE                                                         
075400       ELSE                                                               
075500         PERFORM                                                          
075600         VARYING INDX FROM 1 BY 1                                         
075700           UNTIL INDX > MAX-TILEVDAGAR                                    
075800           MOVE MOD-SPAR-DAG (INDX)                                       
075900                                 TO MOD-DAG-POS (INDX)                    
076000         END-PERFORM                                                      
076100       END-IF                                                             
076200*                                                                         
076300* FETCH AND DISPLAY ATTENTION INFORMATION                                 
076400       MOVE +1 TO INDX                                                    
076500       PERFORM IMS-GET-WDF1-ROT                                           
076600       PERFORM IMS-GNP-WDF107                                             
076700       IF SEGMENT-FINNS                                                   
076800         MOVE ATT-IDATTENT TO MOD-IDATTENT-ENTER                          
076900                              SPARA-IDATTENT                              
077000       ELSE                                                               
077100         MOVE ZERO         TO MOD-IDATTENT-ENTER                          
077200       END-IF                                                             
077300                                                                          
077400       PERFORM UNTIL INDX > MAX-IDATTENT                                  
077500         IF SEGMENT-FINNS                                                 
077600           IF ATT-BELEV = SPACE AND                                       
077700              ATT-IDLEVTLF-KLEV = SPACE AND                               
077800              ATT-IDMAIL = SPACE AND                                      
077900              ATT-TENOTE = SPACE                                          
078000                                                                          
078100             PERFORM IMS-GNP-WDF107                                       
078200           ELSE                                                           
078300             MOVE ATT-IDATTENT      TO MOD-IDATTENT(INDX)                 
078400             MOVE ATT-BELEV         TO MOD-BELEV   (INDX)                 
078500             MOVE ATT-IDMAIL        TO MOD-IDMAIL  (INDX)                 
078600             MOVE ATT-IDLEVTLF-KLEV TO MOD-IDLEVTLF-KLEV(INDX)            
078700             MOVE ATT-TENOTE        TO MOD-TENOTE  (INDX)                 
078800             PERFORM IMS-GNP-WDF107                                       
078900             ADD +1 TO INDX                                               
079000           END-IF                                                         
079100         ELSE                                                             
079200           MOVE MFS-RENSA-FAELT TO MOD-IDATTENT(INDX)                     
079300                                   MOD-BELEV   (INDX)                     
079400                                   MOD-IDMAIL  (INDX)                     
079500                                   MOD-IDLEVTLF-KLEV(INDX)                
079600                                   MOD-TENOTE  (INDX)                     
079700           ADD +1 TO INDX                                                 
079800         END-IF                                                           
079900       END-PERFORM                                                        
080000                                                                          
080100       IF SEGMENT-FINNS                                                   
080200         MOVE ATT-IDATTENT TO MOD-IDATTENT-NEXT                           
080300       ELSE                                                               
080400         MOVE SPARA-IDATTENT TO MOD-IDATTENT-NEXT                         
080500         MOVE INF-LAST-PAGE TO MED-IDMFSINF                               
080600         CALL WMEDKONV        USING MED-WMEDAREA                          
080700         MOVE MED-MFSINF         TO MOD-TEMFSINF                          
080800         END-IF                                                           
080900       END-IF                                                             
081000     .                                                                    
081100     EJECT                                                                
081200                                                                          
081300 G-CHECK-INPUT SECTION.                                                   
081400                                                                          
081500* CDC USERS (IDFTG=57) CAN UPDATE CDC AND NDC-CN AND NDC-US.              
081600* CN USERS (IDFTG=60) CAN UPDATE ALL NDC-CN.                              
081700* US USERS (IDFTG=53) CAN UPDATE ALL NDC-US.                              
081800* ALL NDC'S WITH LOCAL SOURCING MUST LOGG-NDC-2214.                       
081900                                                                          
082000     MOVE JA                     TO INDATA-SW                             
082100     MOVE MSGI-IDDC              TO DC-WS-IDDC                            
082200                                                                          
082300     PERFORM GA-CHECK-DC-FTG-USER                                         
082400                                                                          
082500     IF INDATA-FEL                                                        
082600        MOVE NEJ                 TO INDATA-SW                             
082700        MOVE ERR-NOT-AUTH        TO MED-IDMFSFEL                          
082800        CALL WMEDKONV         USING MED-WMEDAREA                          
082900        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
083000     ELSE                                                                 
083100        MOVE NEJ                 TO LOGG-NDC-2214-SW                      
083200        MOVE '++'                TO WS-KVDAGAR-TTC1                       
083300        IF MID-INPUT = ALL '+' AND MID-INPUT1 = ALL '+'                   
083400          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
083500          CALL WMEDKONV       USING MED-WMEDAREA                          
083600          MOVE MED-MFSFEL        TO MOD-TEMFSFEL                          
083700          PERFORM MFS-DONT-TOUCH-FIELD-IN                                 
083800          PERFORM MFS-DONT-TOUCH-FIELD-UT                                 
083900          MOVE NEJ               TO INDATA-SW                             
084000        ELSE                                                              
084100          IF MID-INPUT1 NOT = ALL '+' AND MFS-UPDATE                      
084300            MOVE INF-PRESS-PF23 TO MED-IDMFSINF                           
084400            CALL WMEDKONV USING MED-WMEDAREA                              
084500            MOVE MED-TEMFSINF TO MOD-TEMFSINF                             
084600            PERFORM MFS-DONT-TOUCH-FIELD-IN                               
084700            PERFORM MFS-DONT-TOUCH-FIELD-UT                               
084800            MOVE NEJ             TO INDATA-SW                             
084900          END-IF                                                          
085000        END-IF                                                            
085100        IF INDATA-OK                                                      
085200          PERFORM IMS-GHU-WDF101                                          
085300          IF SEGMENT-SAKNAS                                               
085400            MOVE ERR-KEYS-MISSING                                         
085500                                    TO MED-IDMFSFEL                       
085600            MOVE NEJ             TO INDATA-SW                             
085700            CALL WMEDKONV     USING MED-WMEDAREA                          
085800            MOVE MED-MFSFEL      TO MOD-TEMFSFEL                          
085900            PERFORM MFS-ERASE-FIELD-UT                                    
086000          ELSE                                                            
086100            IF NDC-CN OR NDC-US                                           
086200              PERFORM IMS-GHU-WDF116                                      
086300              IF SEGMENT-SAKNAS                                           
086400                SET WS-INSERT-YES TO TRUE                                 
086500                PERFORM GB-INIT-WDF116                                    
086600              END-IF                                                      
086700            END-IF                                                        
086800            IF MID-KVDAGAR-TTC1-IN NOT = ALL '+'                          
086900              MOVE MID-KVDAGAR-TTC1-IN                                    
087000                                    TO WS-KVDAGAR-TTC1-IN                 
087100                                       WS-KVDAGAR-TTC1                    
087200              INSPECT WS-KVDAGAR-TTC1-IN REPLACING LEADING SPACE          
087300                                                        BY ZERO           
087400              MOVE MFS-DO-NOT-TOUCH-FIELD                                 
087500                                    TO MOD-KVDAGAR-TTC1-IN                
087600              IF WS-KVDAGAR-TTC1-IN NUMERIC                               
087700                MOVE MFS-NUM-FIELD-OK                                     
087800                                    TO MOD-KVDAGAR-TTC1-IN-ATTR           
087900                IF CDC-SE                                                 
088000                  MOVE WS-KVDAGAR-TTC1-IN                                 
088100                                    TO WDF1-LEV-KVDAGAR-TTC1              
088200                ELSE                                                      
088300                  MOVE WS-KVDAGAR-TTC1-IN                                 
088400                                    TO WDF1-NDC-KVDAGAR-TT                
088500                  IF NDC-CN OR NDC-US                                     
088600                     MOVE JA     TO LOGG-NDC-2214-SW                      
088700                  END-IF                                                  
088800                END-IF                                                    
088900              ELSE                                                        
089000                MOVE MFS-NUM-FIELD-WRONG                                  
089100                                    TO MOD-KVDAGAR-TTC1-IN-ATTR           
089200                MOVE NEJ         TO INDATA-SW                             
089300              END-IF                                                      
089400            ELSE                                                          
089500              MOVE MFS-ERASE-FIELD TO MOD-KVDAGAR-TTC1-IN                 
089600            END-IF                                                        
089700                                                                          
089800            IF MID-KVVECKOR-LVAR-IN NOT = ALL '+'                         
089900              MOVE MFS-DO-NOT-TOUCH-FIELD                                 
090000                                    TO MOD-KVVECKOR-LVAR-IN               
090100              MOVE MID-KVVECKOR-LVAR-IN                                   
090200                                    TO DEC-IDFRIDATA                      
090300              MOVE 2             TO DEC-KVHELTAL                          
090400              MOVE 1             TO DEC-KVDECIMAL                         
090500              CALL WDECEDIT USING DEC-WDECAREA                            
090600              IF DEC-KDSVAR-OK                                            
090700                MOVE DEC-IDEDITDATA TO WS-KVVECKOR-LVAR-IN                
090800                                                                          
090900                IF WS-KVVECKOR-LVAR-IN NUMERIC                            
091000                  MOVE MFS-NUM-FIELD-OK                                   
091100                                    TO MOD-KVVECKOR-LVAR-IN-ATTR          
091200                  IF CDC-SE                                               
091300                    MOVE WS-KVVECKOR-LVAR-IN                              
091400                                    TO WDF1-LEV-KVVECKOR-LVAR             
091500                  ELSE                                                    
091600                    MOVE WS-KVVECKOR-LVAR-IN                              
091700                                    TO WDF1-NDC-KVVECKOR-LVAR             
091800                  END-IF                                                  
091900                ELSE                                                      
092000                  MOVE MFS-NUM-FIELD-WRONG                                
092100                                    TO MOD-KVVECKOR-LVAR-IN-ATTR          
092200                  MOVE NEJ       TO INDATA-SW                             
092300                END-IF                                                    
092400              ELSE                                                        
092500                MOVE MFS-NUM-FIELD-WRONG                                  
092600                                    TO MOD-KVVECKOR-LVAR-IN-ATTR          
092700                MOVE NEJ         TO INDATA-SW                             
092800              END-IF                                                      
092900            ELSE                                                          
093000              MOVE MFS-ERASE-FIELD TO MOD-KVVECKOR-LVAR-IN                
093100            END-IF                                                        
093200                                                                          
093300            IF MID-KVDAGAR-AVIAVV-IN NOT = ALL '+'                        
093400              MOVE MID-KVDAGAR-AVIAVV-IN                                  
093500                                    TO WS-KVDAGAR-AVIAVV-IN               
093600              INSPECT WS-KVDAGAR-AVIAVV-IN REPLACING LEADING SPACE        
093700                                                          BY ZERO         
093800              MOVE MFS-DO-NOT-TOUCH-FIELD                                 
093900                                    TO MOD-KVDAGAR-AVIAVV-IN              
094000              IF WS-KVDAGAR-AVIAVV-IN NUMERIC AND                         
094100                 WS-KVDAGAR-AVIAVV-IN = 0 OR 1 OR 2                       
094200                MOVE MFS-NUM-FIELD-OK                                     
094300                                    TO MOD-KVDAGAR-AVIAVV-IN-ATTR         
094400                IF CDC-SE                                                 
094500                  MOVE WS-KVDAGAR-AVIAVV-IN                               
094600                                    TO WDF1-LEV-KVDAGAR-AVIAVV            
094700                ELSE                                                      
094800                  MOVE WS-KVDAGAR-AVIAVV-IN                               
094900                                    TO WDF1-NDC-KVDAGAR-AVIAVV            
095000                END-IF                                                    
095100              ELSE                                                        
095200                MOVE MFS-NUM-FIELD-WRONG                                  
095300                                    TO MOD-KVDAGAR-AVIAVV-IN-ATTR         
095400                MOVE NEJ         TO INDATA-SW                             
095500              END-IF                                                      
095600            ELSE                                                          
095700              MOVE MFS-ERASE-FIELD TO MOD-KVDAGAR-AVIAVV-IN               
095800            END-IF                                                        
095900                                                                          
096000            IF MID-KVDAGAR-INLAVV-IN NOT = ALL '+'                        
096100              MOVE MID-KVDAGAR-INLAVV-IN                                  
096200                                    TO WS-KVDAGAR-INLAVV-IN               
096300              INSPECT WS-KVDAGAR-INLAVV-IN REPLACING LEADING SPACE        
096400                                                          BY ZERO         
096500              MOVE MFS-DO-NOT-TOUCH-FIELD                                 
096600                                    TO MOD-KVDAGAR-INLAVV-IN              
096700              IF WS-KVDAGAR-INLAVV-IN NUMERIC AND                         
096800                 WS-KVDAGAR-INLAVV-IN = 0 OR 1 OR 2                       
096900                MOVE MFS-NUM-FIELD-OK                                     
097000                                    TO MOD-KVDAGAR-INLAVV-IN-ATTR         
097100                IF CDC-SE                                                 
097200                  MOVE WS-KVDAGAR-INLAVV-IN                               
097300                                    TO WDF1-LEV-KVDAGAR-INLAVV            
097400                ELSE                                                      
097500                  MOVE WS-KVDAGAR-INLAVV-IN                               
097600                                    TO WDF1-NDC-KVDAGAR-INLAVV            
097700                END-IF                                                    
097800              ELSE                                                        
097900                MOVE MFS-NUM-FIELD-WRONG                                  
098000                                    TO MOD-KVDAGAR-INLAVV-IN-ATTR         
098100                MOVE NEJ         TO INDATA-SW                             
098200              END-IF                                                      
098300            ELSE                                                          
098400              MOVE MFS-ERASE-FIELD TO MOD-KVDAGAR-INLAVV-IN               
098500            END-IF                                                        
098600                                                                          
098700            MOVE NEJ TO IDANSK-UPD-CN-UPD-SW                              
098800                        IDANSK-UPD-CDC-UPD-SW                             
098900            PERFORM                                                       
099000            VARYING INDX FROM 1 BY 1                                      
099100              UNTIL INDX > MAX-PG                                         
099200              IF MID-IDANSK-PG-IN (INDX) NOT = ALL '+'                    
099300                MOVE MID-IDANSK-PG-IN (INDX)                              
099400                                    TO WS-IDANSK-PG                       
099500                INSPECT WS-IDANSK-PG REPLACING LEADING SPACE              
099600                                                    BY ZERO               
099700                MOVE MFS-DO-NOT-TOUCH-FIELD                               
099800                                    TO MOD-IDANSK-PG-IN (INDX)            
099900                IF WS-IDANSK-PG NUMERIC AND                               
100000                   WS-IDANSK-PG < 1000                                    
100100                  MOVE MFS-NUM-FIELD-OK                                   
100200                                   TO MOD-IDANSK-PG-IN-ATTR (INDX)        
100300                  IF CDC-SE                                               
100400                    MOVE WS-IDANSK-PG                                     
100500                                    TO WDF1-LEV-IDANSK-PG (INDX)          
100600                    MOVE JA TO IDANSK-UPD-CDC-UPD-SW                      
100700                  ELSE                                                    
100800                    MOVE WS-IDANSK-PG                                     
100900                                    TO WDF1-NDC-IDANSK-PG (INDX)          
101000                    MOVE JA TO IDANSK-UPD-CN-UPD-SW                       
101100                  END-IF                                                  
101200                ELSE                                                      
101300                  MOVE MFS-NUM-FIELD-WRONG                                
101400                                   TO MOD-IDANSK-PG-IN-ATTR (INDX)        
101500                  MOVE NEJ       TO INDATA-SW                             
101600                END-IF                                                    
101700              END-IF                                                      
101800            END-PERFORM                                                   
101900                                                                          
102000            PERFORM                                                       
102100            VARYING INDX FROM 1 BY 1                                      
102200              UNTIL INDX > MAX-TILEVDAGAR                                 
102300              IF MID-DAG-POS-IN (INDX) NOT = ALL '+'                      
102400                MOVE MID-DAG-POS-IN (INDX)                                
102500                                    TO WS-DAG-POS                         
102600*               MOVE MFS-DO-NOT-TOUCH-FIELD                               
102700*                                   TO MOD-DAG-POS (INDX)                 
102800                IF WS-DAG-POS-GOOD OR                                     
102900                   MID-DAG-POS-IN (INDX) = SPACE                          
103000*                 MOVE MFS-ALPHA-FIELD-OK                                 
103100*                                   TO MOD-DAG-POS-ATTR (INDX)            
103200                  PERFORM GC-HANDLE-DAG-POS                               
103300                ELSE                                                      
103400                  MOVE MFS-ALPHA-FIELD-WRONG                              
103500                                    TO MOD-DAG-POS-ATTR (INDX)            
103600                  MOVE NEJ       TO INDATA-SW                             
103700                END-IF                                                    
103800              END-IF                                                      
103900            END-PERFORM                                                   
104000                                                                          
104100            IF INDATA-FEL                                                 
104300              MOVE ERR-HILITE-FIELDS-WRONG                                
104400                                    TO MED-IDMFSFEL                       
104500              CALL WMEDKONV   USING MED-WMEDAREA                          
104600              MOVE MED-MFSFEL    TO MOD-TEMFSFEL                          
104700              PERFORM MFS-DONT-TOUCH-FIELD-IN                             
104800              PERFORM MFS-DONT-TOUCH-FIELD-UT                             
104900            END-IF                                                        
105000                                                                          
105100          END-IF                                                          
105200        END-IF                                                            
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600                                                                          
105700 GA-CHECK-DC-FTG-USER SECTION.                                            
105800                                                                          
105900     PERFORM IMS-GU-WDB601                                                
106000     IF SEGMENT-FINNS                                                     
106100        IF WDB601-DCS-CDC                                                 
106200        OR WDB601-DCS-NDC-CN                                              
106300        OR (WDB601-DCS-NDC-NA AND WDB601-DCS-USA)                         
106400           MOVE MSGI-IDFTG    TO WS-IDFTG                                 
106500           IF (WDB601-DCS-NDC-CN AND IDFTG-CN)                            
106600           OR (WDB601-DCS-NDC-NA AND IDFTG-US)                            
106700           OR MSGI-IDFTG  = WC-IDFTG-PV                                   
106800              CONTINUE                                                    
106900           ELSE                                                           
107000              MOVE NEJ           TO INDATA-SW                             
107100           END-IF                                                         
107200        ELSE                                                              
107300           MOVE NEJ              TO INDATA-SW                             
107400        END-IF                                                            
107500     ELSE                                                                 
107600        MOVE NEJ                 TO INDATA-SW                             
107700     END-IF                                                               
107800     .                                                                    
107900     EJECT                                                                
108000 GB-INIT-WDF116 SECTION.                                                  
108100                                                                          
108200     MOVE WS-IDDC                TO WDF1-NDC-IDDC                         
108300     MOVE ZERO                   TO WDF1-NDC-KVDAGAR-AVIAVV               
108400                                    WDF1-NDC-KVDAGAR-INLAVV               
108500                                    WDF1-NDC-KVDAGAR-TBT                  
108600                                    WDF1-NDC-KVDAGAR-TT                   
108700                                    WDF1-NDC-KVVECKOR-LVAR                
108800     PERFORM                                                              
108900     VARYING INDX FROM 1 BY 1                                             
109000       UNTIL INDX > MAX-TILEVDAGAR                                        
109100       MOVE ZERO                 TO WDF1-NDC-TILEVDAG (INDX)              
109200     END-PERFORM                                                          
109300                                                                          
109400     PERFORM                                                              
109500     VARYING INDX FROM 1 BY 1                                             
109600       UNTIL INDX > MAX-PG                                                
109700       MOVE ZERO                 TO WDF1-NDC-IDANSK-PG (INDX)             
109800     END-PERFORM                                                          
109900                                                                          
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300 GC-HANDLE-DAG-POS SECTION.                                               
110400                                                                          
110500     IF MID-DAG-POS-IN (INDX) = MID-SPAR-DAG(INDX)                        
110600       CONTINUE                                                           
110700     ELSE                                                                 
110800       MOVE MID-SPAR-DAG (INDX)  TO WS-DAG-POS                            
110900       EVALUATE TRUE                                                      
111000         WHEN WS-DAG-MO AND CDC-SE                                        
111100            MOVE ZERO            TO WDF1-LEV-TILEVDAG (1)                 
111200         WHEN WS-DAG-TU AND CDC-SE                                        
111300            MOVE ZERO            TO WDF1-LEV-TILEVDAG (2)                 
111400         WHEN WS-DAG-WE AND CDC-SE                                        
111500            MOVE ZERO            TO WDF1-LEV-TILEVDAG (3)                 
111600         WHEN WS-DAG-TH AND CDC-SE                                        
111700            MOVE ZERO            TO WDF1-LEV-TILEVDAG (4)                 
111800         WHEN WS-DAG-FR AND CDC-SE                                        
111900            MOVE ZERO            TO WDF1-LEV-TILEVDAG (5)                 
112000         WHEN WS-DAG-MO AND (NDC-CN OR NDC-US)                            
112100            MOVE ZERO            TO WDF1-NDC-TILEVDAG (1)                 
112200         WHEN WS-DAG-TU AND (NDC-CN OR NDC-US)                            
112300            MOVE ZERO            TO WDF1-NDC-TILEVDAG (2)                 
112400         WHEN WS-DAG-WE AND (NDC-CN OR NDC-US)                            
112500            MOVE ZERO            TO WDF1-NDC-TILEVDAG (3)                 
112600         WHEN WS-DAG-TH AND (NDC-CN OR NDC-US)                            
112700            MOVE ZERO            TO WDF1-NDC-TILEVDAG (4)                 
112800         WHEN WS-DAG-FR AND (NDC-CN OR NDC-US)                            
112900            MOVE ZERO            TO WDF1-NDC-TILEVDAG (5)                 
113000         WHEN OTHER                                                       
113100            CONTINUE                                                      
113200       END-EVALUATE                                                       
113300                                                                          
113400       MOVE MID-DAG-POS-IN(INDX) TO WS-DAG-POS                            
113500       EVALUATE TRUE                                                      
113600         WHEN WS-DAG-MO AND CDC-SE                                        
113700            MOVE 1               TO WDF1-LEV-TILEVDAG (1)                 
113800            MOVE JA              TO TILEVDAG-CDC-SW                       
113900         WHEN WS-DAG-TU AND CDC-SE                                        
114000            MOVE 2               TO WDF1-LEV-TILEVDAG (2)                 
114100            MOVE JA              TO TILEVDAG-CDC-SW                       
114200         WHEN WS-DAG-WE AND CDC-SE                                        
114300            MOVE 3               TO WDF1-LEV-TILEVDAG (3)                 
114400            MOVE JA              TO TILEVDAG-CDC-SW                       
114500         WHEN WS-DAG-TH AND CDC-SE                                        
114600            MOVE 4               TO WDF1-LEV-TILEVDAG (4)                 
114700            MOVE JA              TO TILEVDAG-CDC-SW                       
114800         WHEN WS-DAG-FR AND CDC-SE                                        
114900            MOVE 5               TO WDF1-LEV-TILEVDAG (5)                 
115000            MOVE JA              TO TILEVDAG-CDC-SW                       
115100         WHEN WS-DAG-MO AND (NDC-CN OR NDC-US)                            
115200            MOVE 1               TO WDF1-NDC-TILEVDAG (1)                 
115300            MOVE JA              TO TILEVDAG-NDC-SW                       
115400         WHEN WS-DAG-TU AND (NDC-CN OR NDC-US)                            
115500            MOVE 2               TO WDF1-NDC-TILEVDAG (2)                 
115600            MOVE JA              TO TILEVDAG-NDC-SW                       
115700         WHEN WS-DAG-WE AND (NDC-CN OR NDC-US)                            
115800            MOVE 3               TO WDF1-NDC-TILEVDAG (3)                 
115900            MOVE JA              TO TILEVDAG-NDC-SW                       
116000         WHEN WS-DAG-TH AND (NDC-CN OR NDC-US)                            
116100            MOVE 4               TO WDF1-NDC-TILEVDAG (4)                 
116200            MOVE JA              TO TILEVDAG-NDC-SW                       
116300         WHEN WS-DAG-FR AND (NDC-CN OR NDC-US)                            
116400            MOVE 5               TO WDF1-NDC-TILEVDAG (5)                 
116500            MOVE JA              TO TILEVDAG-NDC-SW                       
116600         WHEN OTHER                                                       
116700            CONTINUE                                                      
116800       END-EVALUATE                                                       
116900     END-IF                                                               
117000                                                                          
117100     IF TILEVDAG-CDC                                                      
117200       PERFORM GCA-CREATE-WDR3                                            
117300     END-IF                                                               
117400                                                                          
117500     IF TILEVDAG-NDC                                                      
117600       PERFORM GBC-CEATE-EVENT-2256                                       
117700     END-IF                                                               
117800     .                                                                    
117900     EJECT                                                                
118000                                                                          
118100 GCA-CREATE-WDR3 SECTION.                                                 
118200     MOVE 'W2011100'        TO FIL-IDPGM                                  
118300     MOVE W-DATUM           TO FIL-TIREGDAT                               
118400     ADD +1                 TO W-TIKLOCK                                  
118500     MOVE W-TIKLOCK         TO FIL-TIKLOCK                                
118600     ADD +1                 TO W-IDSEKVNR                                 
118700     MOVE W-IDSEKVNR        TO FIL-IDSEKVNR                               
118800     MOVE 'W221'            TO FIL-CT-IDSYSTEM                            
118900     MOVE 'H01'             TO FIL-CT-IDPTYP                              
119000     MOVE ' '               TO FIL-CT-IDVTYP                              
119100     MOVE WS-IDLEVNR        TO FIL-WDR301-DATA                            
119200     .                                                                    
119300     EJECT                                                                
119400                                                                          
119500 GBC-CEATE-EVENT-2256 SECTION.                                            
119600                                                                          
119700     MOVE WS-IDDC           TO HTR-2256-IDDC                              
119800     MOVE WS-IDLEVNR        TO HTR-2256-IDLEVNR                           
119900     .                                                                    
120000     EJECT                                                                
120100 H-UPDATE SECTION.                                                        
120200                                                                          
120300     IF CDC-SE                                                            
120400       PERFORM IMS-REPL-WDF101                                            
120500       IF SEGMENT-UPD                                                     
120600         IF IDANSK-UPD-CDC-JA                                             
120700           PERFORM S01-STARTA-W224B1                                      
120800         END-IF                                                           
120900                                                                          
121000         IF WS-KVDAGAR-TTC1 NOT = ALL '+'                                 
121100            PERFORM S03-START-W224S2                                      
121200         END-IF                                                           
121300                                                                          
121400       END-IF                                                             
121500       IF TILEVDAG-CDC                                                    
121600         PERFORM IMS-ISRT-WDR301                                          
121700       END-IF                                                             
121800     END-IF                                                               
121900                                                                          
122000     IF NDC-CN OR NDC-US                                                  
122100       IF WS-INSERT-YES                                                   
122200         PERFORM IMS-ISRT-WDF116                                          
122300         IF SEGMENT-UPD OR SEGMENT-FINNS-REDAN                            
122400           IF IDANSK-UPD-CN-JA                                            
122500             PERFORM S01-STARTA-W224B1                                    
122600           END-IF                                                         
122700         END-IF                                                           
122800       ELSE                                                               
122900         PERFORM IMS-REPL-WDF116                                          
123000         IF SEGMENT-UPD                                                   
123100           IF IDANSK-UPD-CN-JA                                            
123200             PERFORM S01-STARTA-W224B1                                    
123300           END-IF                                                         
123400         END-IF                                                           
123500       END-IF                                                             
123600       IF TILEVDAG-NDC                                                    
123700         PERFORM IMS-ISRT-WDGX2256                                        
123800       END-IF                                                             
123900       IF LOGG-NDC-2214                                                   
124000          PERFORM S02-START-W224S1                                        
124100       END-IF                                                             
124200     END-IF                                                               
124300                                                                          
124400     PERFORM MFS-ERASE-FIELD-IN                                           
124500     MOVE INF-UPDATE-DONE        TO MED-IDMFSFEL                          
124600     CALL WMEDKONV            USING MED-WMEDAREA                          
124700     MOVE MED-MFSFEL             TO MOD-TEMFSFEL                          
124800                                                                          
124900     .                                                                    
125000     EJECT                                                                
125100                                                                          
125200 S01-STARTA-W224B1 SECTION.                                               
125300                                                                          
125400     MOVE WS-IDLEVNR        TO PARM-IDLEVNR                               
125500     IF CDC-SE                                                            
125600       MOVE '11'            TO PARM-IDDC                                  
125700     ELSE                                                                 
125800       MOVE WS-IDDC         TO PARM-IDDC                                  
125900     END-IF                                                               
126000     MOVE '2111'       TO MSGSOP-IDTRANS                                  
126100     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
126200     MOVE 'W224B1    ' TO MSGSOP-IDPROCESS                                
126300     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
126400     MOVE PARM-TESYMBV TO MSGSOP-TESYMBV                                  
126500     MOVE 'UPDATE STARTED' TO MOD-TEMFSINF                                
126600     PERFORM IMS-INSERT-ALT-MSG                                           
126700     .                                                                    
126800     EJECT                                                                
126900                                                                          
127000 S02-START-W224S1 SECTION.                                                
127100                                                                          
127200     MOVE '2111'            TO MSGSOP-IDTRANS                             
127300     MOVE MFS-KDMFSFOR      TO MSGSOP-KDMFSFOR                            
127400     MOVE 'W224S1    '      TO MSGSOP-IDPROCESS                           
127500     MOVE 'O'               TO MSGSOP-KDSOPFUNK                           
127600     MOVE WS-IDDC           TO PARM-IDDC-W224S1                           
127700     MOVE WS-IDLEVNR        TO PARM-IDLEVNR-W224S1                        
127800     MOVE PARM-W224S1       TO MSGSOP-TESYMBV                             
127900     MOVE 'UPDATE STARTED'  TO MOD-TEMFSINF                               
128000     PERFORM IMS-INSERT-ALT-MSG                                           
128100     .                                                                    
128200     EJECT                                                                
128300 S03-START-W224S2 SECTION.                                                
128400                                                                          
128500     MOVE '2111'            TO MSGSOP-IDTRANS                             
128600     MOVE MFS-KDMFSFOR      TO MSGSOP-KDMFSFOR                            
128700     MOVE 'W224S2    '      TO MSGSOP-IDPROCESS                           
128800     MOVE 'O'               TO MSGSOP-KDSOPFUNK                           
128900     MOVE WS-IDDC           TO PARM-W224S2-IDDC                           
129000     MOVE WS-IDLEVNR        TO PARM-W224S2-IDLEVNR                        
129100     MOVE '++'              TO PARM-W224S2-KVVECKOR-AT                    
129200     MOVE '++'              TO PARM-W224S2-KVVECKOR-LT                    
129300     MOVE WS-KVDAGAR-TTC1   TO PARM-W224S2-KVDAGAR-TT                     
129400     MOVE PARM-W224S2       TO MSGSOP-TESYMBV                             
129500     MOVE 'UPDATE STARTED'  TO MOD-TEMFSINF                               
129600     PERFORM IMS-INSERT-ALT-MSG                                           
129700     .                                                                    
129800     EJECT                                                                
129900 MFS-ERASE-FIELD-IN SECTION.                                              
130000                                                                          
130100     MOVE MFS-RENSA-FAELT        TO MOD-KVDAGAR-TTC1-IN                   
130200                                    MOD-KVVECKOR-LVAR-IN                  
130300                                    MOD-KVDAGAR-AVIAVV-IN                 
130400                                    MOD-KVDAGAR-INLAVV-IN                 
130500     PERFORM                                                              
130600     VARYING INDX FROM 1 BY 1                                             
130700       UNTIL INDX > MAX-PG                                                
130800       MOVE MFS-RENSA-FAELT      TO MOD-IDANSK-PG-IN (INDX)               
130900     END-PERFORM                                                          
131000                                                                          
131100     PERFORM                                                              
131200     VARYING INDX FROM 1 BY 1                                             
131300       UNTIL INDX > MAX-TILEVDAGAR                                        
131400       MOVE MFS-RENSA-FAELT      TO MOD-DAG-POS (INDX)                    
131500     END-PERFORM                                                          
131600                                                                          
131700     .                                                                    
131800     EJECT                                                                
131900 MFS-ERASE-FIELD-UT SECTION.                                              
132000                                                                          
132100     MOVE MFS-RENSA-FAELT        TO MOD-BELEV-VCC                         
132200                                    MOD-ADLEV-RAD1                        
132300                                    MOD-ADLEV-RAD2-VCC                    
132400                                    MOD-ADLEV-ORT-VCC                     
132500                                    MOD-IDLEVNR-MOTSV                     
132600                                    MOD-ADLEVLND                          
132700                                    MOD-IDLANDX2                          
132800                                    MOD-IDLEVTLF                          
132900                                    MOD-IDLEVFAX                          
133000                                    MOD-FLRSADR                           
133100                                    MOD-KDLEVTYP                          
133200                                    MOD-KVDAGAR-TTC1-UT                   
133300                                    MOD-KVVECKOR-LT                       
133400                                    MOD-KVVECKOR-LVAR-UT                  
133500                                    MOD-KVDAGAR-AVIAVV-UT                 
133600                                    MOD-KVDAGAR-INLAVV-UT                 
133700     PERFORM                                                              
133800     VARYING INDX FROM 1 BY 1                                             
133900       UNTIL INDX > MAX-PG                                                
134000       MOVE MFS-RENSA-FAELT      TO MOD-IDANSK-PG-UT (INDX)               
134100     END-PERFORM                                                          
134200                                                                          
134300     .                                                                    
134400     EJECT                                                                
134500 MFS-DONT-TOUCH-FIELD-IN SECTION.                                         
134600                                                                          
134700     MOVE MFS-ROER-EJ-FAELT      TO MOD-KVDAGAR-TTC1-IN                   
134800                                    MOD-KVVECKOR-LVAR-IN                  
134900                                    MOD-KVDAGAR-AVIAVV-IN                 
135000                                    MOD-KVDAGAR-INLAVV-IN                 
135100     PERFORM                                                              
135200     VARYING INDX FROM 1 BY 1                                             
135300       UNTIL INDX > MAX-PG                                                
135400       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDANSK-PG-IN (INDX)               
135500     END-PERFORM                                                          
135600                                                                          
135700     .                                                                    
135800     SKIP2                                                                
135900 MFS-DONT-TOUCH-FIELD-UT SECTION.                                         
136000                                                                          
136100     MOVE MFS-ROER-EJ-FAELT      TO MOD-BELEV-VCC                         
136200                                    MOD-ADLEV-RAD1                        
136300                                    MOD-ADLEV-RAD2-VCC                    
136400                                    MOD-ADLEV-ORT-VCC                     
136500                                    MOD-IDLEVNR-MOTSV                     
136600                                    MOD-ADLEVLND                          
136700                                    MOD-IDLANDX2                          
136800                                    MOD-IDLEVTLF                          
136900                                    MOD-IDLEVFAX                          
137000                                    MOD-FLRSADR                           
137100                                    MOD-KDLEVTYP                          
137200                                    MOD-KVDAGAR-TTC1-UT                   
137300                                    MOD-KVVECKOR-LT                       
137400                                    MOD-KVVECKOR-LVAR-UT                  
137500                                    MOD-KVDAGAR-AVIAVV-UT                 
137600                                    MOD-KVDAGAR-INLAVV-UT                 
137700     PERFORM                                                              
137800     VARYING INDX FROM 1 BY 1                                             
137900       UNTIL INDX > MAX-PG                                                
138000       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDANSK-PG-UT (INDX)               
138100     END-PERFORM                                                          
138200                                                                          
138300                                                                          
138400     PERFORM                                                              
138500     VARYING INDX FROM 1 BY 1                                             
138600       UNTIL INDX > MAX-TILEVDAGAR                                        
138700       MOVE MFS-ROER-EJ-FAELT    TO MOD-DAG-POS (INDX)                    
138800                                    MOD-SPAR-DAG (INDX)                   
138900     END-PERFORM                                                          
139000                                                                          
139100     .                                                                    
139200     SKIP2                                                                
139300* IMS SEKTIONER                                                           
139400     SKIP3                                                                
139500 IMS-GET-MSG SECTION.                                                     
139600                                                                          
139700     MOVE '  QC'                 TO GODK-STATUSKODER                      
139800     CALL CBLTDLI             USING GU                                    
139900                                    MSG-PCB                               
140000                                    MSG-IO-AREA                           
140100     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400     SKIP3                                                                
140500 IMS-INSERT-MSG SECTION.                                                  
140600                                                                          
140700     MOVE LOW-VALUE              TO MSG-KDZ1                              
140800                                    MSG-KDZ2                              
140900     MOVE SPACE                  TO GODK-STATUSKODER                      
141000     CALL CBLTDLI             USING ISRT                                  
141100                                    MSG-PCB                               
141200                                    MSG-IO-AREA                           
141300                                    MFS-IDMOD                             
141400     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
141500     PERFORM IMS-STATUSKONTROLL                                           
141600     .                                                                    
141700     EJECT                                                                
141800 IMS-GET-WDF1-ROT SECTION.                                                
141900                                                                          
142000     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
142100             DELIMITED BY SIZE INTO SSA1                                  
142200     MOVE '  GE'                 TO GODK-STATUSKODER                      
142300     CALL CBLTDLI             USING GU                                    
142400                                    WDF1-PCB                              
142500                                    DLI-IO-WDF101                         
142600                                    SSA1                                  
142700     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000                                                                          
143100 IMS-GHU-WDF101 SECTION.                                                  
143200                                                                          
143300     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
143400             DELIMITED BY SIZE INTO SSA1                                  
143500     MOVE '  GE'                 TO GODK-STATUSKODER                      
143600     CALL CBLTDLI             USING GHU                                   
143700                                    WDF1-PCB                              
143800                                    DLI-IO-WDF101                         
143900                                    SSA1                                  
144000     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
144100     PERFORM IMS-STATUSKONTROLL                                           
144200     .                                                                    
144300                                                                          
144400 IMS-REPL-WDF101 SECTION.                                                 
144500                                                                          
144600     MOVE '  '                   TO GODK-STATUSKODER                      
144700     CALL CBLTDLI             USING REPL                                  
144800                                    WDF1-PCB                              
144900                                    DLI-IO-WDF101                         
145000     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300                                                                          
145400 IMS-GET-WDF1-ADRESS SECTION.                                             
145500                                                                          
145600     MOVE 'WDF106   '            TO SSA1                                  
145700     MOVE '  GE'                 TO GODK-STATUSKODER                      
145800     CALL CBLTDLI             USING GNP                                   
145900                                    WDF1-PCB                              
146000                                    DLI-IO-WDF106                         
146100                                    SSA1                                  
146200     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
146300     PERFORM IMS-STATUSKONTROLL                                           
146400     .                                                                    
146500                                                                          
146600 IMS-GET-WDF116 SECTION.                                                  
146700                                                                          
146800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
146900             DELIMITED BY SIZE INTO SSA1                                  
147000     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
147100             DELIMITED BY SIZE INTO SSA2                                  
147200     MOVE '  GE'                 TO GODK-STATUSKODER                      
147300     CALL CBLTDLI             USING GU                                    
147400                                    WDF1-PCB                              
147500                                    DLI-IO-WDF116                         
147600                                    SSA1 SSA2                             
147700     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
147800     PERFORM IMS-STATUSKONTROLL                                           
147900     .                                                                    
148000                                                                          
148100 IMS-GNP-WDF107 SECTION.                                                  
148200                                                                          
148300     STRING 'WDF107  (IDATTENT=>' W-IDATTENT-X ')'                        
148400          DELIMITED BY SIZE INTO SSA1                                     
148500     MOVE '  GE' TO GODK-STATUSKODER                                      
148600     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF107 SSA1                   
148700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     .                                                                    
149000     EJECT                                                                
149100 IMS-GHU-WDF116 SECTION.                                                  
149200                                                                          
149300     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
149400             DELIMITED BY SIZE INTO SSA1                                  
149500     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
149600             DELIMITED BY SIZE INTO SSA2                                  
149700     MOVE '  GE'                 TO GODK-STATUSKODER                      
149800     CALL CBLTDLI             USING GHU                                   
149900                                    WDF1-PCB                              
150000                                    DLI-IO-WDF116                         
150100                                    SSA1 SSA2                             
150200     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
150300     PERFORM IMS-STATUSKONTROLL                                           
150400     .                                                                    
150500                                                                          
150600 IMS-REPL-WDF116 SECTION.                                                 
150700                                                                          
150800     MOVE '  '                   TO GODK-STATUSKODER                      
150900     CALL CBLTDLI             USING REPL                                  
151000                                    WDF1-PCB                              
151100                                    DLI-IO-WDF116                         
151200     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
151300     PERFORM IMS-STATUSKONTROLL                                           
151400     .                                                                    
151500                                                                          
151600 IMS-ISRT-WDF116 SECTION.                                                 
151700                                                                          
151800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
151900             DELIMITED BY SIZE INTO SSA1                                  
152000     MOVE 'WDF116 '              TO SSA2                                  
152100     MOVE '  II'                 TO GODK-STATUSKODER                      
152200     CALL CBLTDLI             USING ISRT                                  
152300                                    WDF1-PCB                              
152400                                    DLI-IO-WDF116                         
152500                                    SSA1 SSA2                             
152600     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
152700     PERFORM IMS-STATUSKONTROLL                                           
152800     .                                                                    
152900                                                                          
153000 IMS-GU-WDB601 SECTION.                                                   
153100                                                                          
153200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
153300          DELIMITED BY SIZE INTO SSA1                                     
153400     MOVE '  GE'              TO GODK-STATUSKODER                         
153500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
153600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
153700     PERFORM IMS-STATUSKONTROLL                                           
153800     .                                                                    
153900                                                                          
154000 IMS-ISRT-WDR301 SECTION.                                                 
154100     STRING 'WDR301      '                                                
154200          DELIMITED BY SIZE INTO SSA1                                     
154300     MOVE '   ' TO GODK-STATUSKODER                                       
154400     CALL CBLTDLI USING ISRT WDR3-PCB DLI-IO-AREA-WDR301 SSA1             
154500     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     .                                                                    
154800                                                                          
154900 IMS-ISRT-WDGX2256 SECTION.                                               
155000     MOVE 'WDGX2256 '      TO SSA1                                        
155100     MOVE '  II'           TO GODK-STATUSKODER                            
155200     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-AREA-2256 SSA1               
155300     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
155400     PERFORM IMS-STATUSKONTROLL                                           
155500     .                                                                    
155600                                                                          
155700 IMS-INSERT-ALT-MSG SECTION.                                              
155800                                                                          
155900     MOVE SPACE TO GODK-STATUSKODER                                       
156000     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
156100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400                                                                          
156500 IMS-STATUSKONTROLL SECTION.                                              
156600                                                                          
156700     SET STATUS-IX               TO 1                                     
156800     SEARCH GODK-STATUS                                                   
156900       AT END CALL FELLOG                                                 
157000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
157100     END-SEARCH                                                           
157200     .                                                                    
