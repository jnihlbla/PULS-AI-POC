000100 ID DIVISION.                                                             
000200 PROGRAM-ID. W5010100.                                                    
000300 AUTHOR. S.OHLSSON - M-A EVERBÄCK.                                        
000400 DATE-WRITTEN. OCTOBER 76 - APRIL 79.                                     
000500*    FUNKTION.                                                            
000600*        TP-PROGRAM FÖR EKONOMI (INVENTERINGSBILD)                        
000700*        PROGRAMMET LÄSER DATABASEN WDK6-ARTIKELREG CDC                   
000800*        PROGRAMMET LÄSER DATABASEN WDK7-ARTIKELREG SDC/LDC               
000900*        WDD9-LEVERANSPLANEREG,                                           
001000*        WDH1-INVENTERINGSREG OCH WDD8-ARTIKELREG.                        
001100*                                                                         
001200*        LÄGGER UT CDC-/SDC- OCH LDC-POSTER.                              
001300*                                                                         
001400*    SUBPROGRAM.                                                          
001500*        FELLOG                                                           
001600*    SKIP3                                                                
001700*                                                                         
001800*   ÄNDRINGAR:                                                            
001900*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002000*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002100*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002200*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002300*                                                                         
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP3                                                                
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77      IDPGM           PIC X(8)    VALUE 'W5010100'.                    
003100 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
003200 77      WS-IDDC         PIC X(2)    VALUE SPACE.                         
003300 77      INDX            PIC S9(9)   VALUE ZERO COMP SYNC.                
003400 77      SPRAK-IX        PIC S9      COMP-3.                              
003500 77      W-SUMMA1        PIC S9(6)V9 COMP-3.                              
003600 77      W-SUMMA2        PIC S9(6)V9 COMP-3.                              
003700 77      W-SDC-KVLS      PIC S9(7)   VALUE ZERO COMP-3.                   
003800 77      W-SDC-KVAKS-SDC PIC S9(7)   VALUE ZERO COMP-3.                   
003900 77      W-SDC-KVAKS-PAV PIC S9(7)   VALUE ZERO COMP-3.                   
004000 77      W-SDC-KVEFRS    PIC S9(7)   VALUE ZERO COMP-3.                   
004100 77      W-SDC-KVUTRS    PIC S9(7)   VALUE ZERO COMP-3.                   
004200 77      W-SDC-KVINVS    PIC S9(7)   VALUE ZERO COMP-3.                   
004300 77      WS-KVAKS-TOTCDC PIC S9(7)   VALUE ZERO COMP-3.                   
004400 77      WS-KVAKS-TOTSDC PIC S9(7)   VALUE ZERO COMP-3.                   
004500                                                                          
004600*01  -COPY WWDCKONS                                                       
004700                                                                          
004800                                                                          
004900 77  WS-IDLEVNR-8        PIC X(8)               VALUE SPACE.              
005000                                                                          
005100 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
005200     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
005300     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
005400                                                                          
005500 01  KONSTANTER.                                                          
005600     03  JA              PIC X       VALUE 'J'.                           
005700     03  NEJ             PIC X       VALUE 'N'.                           
005800     03  ARTIKEL-RETT    PIC X.                                           
005900     SKIP3                                                                
006000 01  DIVERSE.                                                             
006010     03  DAGENS-AAAAMMDD PIC 9(8)  VALUE ZERO.                            
006020     03  WS-PRARTBES     PIC X(1)    VALUE 'N'.                           
006100     03  WS-TISEGKEY     PIC 9(9)    VALUE ZERO.                          
006200     03  FILLER  REDEFINES  WS-TISEGKEY.                                  
006300      05 FILLER          PIC X(2).                                        
006400      05 WS-TIREGDAT     PIC 9(6).                                        
006500      05 FILLER          PIC X.                                           
006600     SKIP3                                                                
006700 01  W-IDARTNR-X.                                                         
006800     03  W-IDARTNR       PIC S9(9)   COMP-3.                              
006900     SKIP3                                                                
006901 01  W-DAPRLIST-X.                                                        
006904     03  W-DAPRLIST      PIC 9(8)  VALUE ZERO.                            
006910 01  W-WDD901KY-X.                                                        
006920     03  W-IDARTNR-D9    PIC S9(9)   COMP-3.                              
006921     03  W-IDDC-D9       PIC X(2).                                        
006930     SKIP3                                                                
007000 01  W-IDDC-X.                                                            
007100     03  W-IDDC          PIC X(2)    VALUE SPACE.                         
007200     SKIP3                                                                
007300 01  W-KDSEGKEY-X.                                                        
007400     03  W-KDSEGKEY      PIC X(1)    VALUE '1'.                           
007500     SKIP3                                                                
007600 01  W-WDD811KY-X.                                                        
007700     03  W-IDDC-WDD8     PIC X(2)    VALUE '11'.                          
007800     03  W-ADBUFFOMR     PIC S9(3)   COMP-3   VALUE +1.                   
007900     03  W-DABUFPAF      PIC  9(8)            VALUE ZERO.                 
008000     03  W-ADBUFFGANG    PIC S9(3)   COMP-3   VALUE +0.                   
008100     03  W-ADBUFFPL      PIC S9(5)   COMP-3   VALUE +0.                   
008200                                                                          
008300 01  W-IDSKYLT-X.                                                         
008400     03  W-IDSKYLT       PIC X(3)    VALUE 'S  '.                         
008500                                                                          
008600 01  W-WDH1KEY-MIN-X.                                                     
008700     03  W-IDDC-WDH1-MIN PIC X(2).                                        
008800     03  W-KDINVKAT-MIN  PIC S9(3)  COMP-3.                               
008900     03  W-TISEGKEY-MIN  PIC S9(9)  COMP-3 VALUE ZERO.                    
009000     03  W-DAREGDAT-SORT-MIN    PIC 9(8) VALUE ZERO.                      
009100 01  W-WDH1KEY-MAX-X.                                                     
009200     03  W-IDDC-WDH1-MAX PIC X(2).                                        
009300     03  W-KDINVKAT-MAX  PIC S9(3)  COMP-3.                               
009400     03  W-TISEGKEY-MAX  PIC S9(9)  COMP-3 VALUE +999999999.              
009500     03  W-DAREGDAT-SORT-MIN    PIC 9(8) VALUE 99999999.                  
009600     SKIP2                                                                
009700 01  W-IDDC-WDK7-MIN-X.                                                   
009800     03  W-IDDC-WDK7-MIN PIC X(2).                                        
009900 01  W-IDDC-WDK7-MAX-X.                                                   
010000     03  W-IDDC-WDK7-MAX PIC X(2).                                        
010100                                                                          
010200 01  W-IDDC-B6-X.                                                         
010300     03 W-IDDC-B6                  PIC X(2).                              
010400     EJECT                                                                
010500 01      FELMED.                                                          
010600   03    FILLER          PIC X(35)   VALUE                                
010700                           'ARTIKELNUMMER EJ NUMERISKT'.                  
010800   03    FILLER          PIC X(35)   VALUE                                
010900                           'PART NUMBER IS NOT NUMERIC'.                  
011000   03    FILLER          PIC X(35)   VALUE                                
011100                           'ARTIKELN SAKNAS PÅ ARTIKELREGISTRET'.         
011200   03    FILLER          PIC X(35)   VALUE                                
011300                           'THIS PARTNO. IS NOT IN THE DATABASE'.         
011400   03    FILLER          PIC X(35)   VALUE                                
011500                           'ARTIKELN ÄR BORTTAGEN'.                       
011600   03    FILLER          PIC X(35)   VALUE                                
011700                           'THIS PARTNO. HAS BEEN DELETED'.               
011800 01  MEDDELANDEN  REDEFINES FELMED.                                       
011900   03    FEL-1           PIC X(35)  OCCURS 2.                             
012000   03    FEL-2           PIC X(35)  OCCURS 2.                             
012100   03    FEL-3           PIC X(35)  OCCURS 2.                             
012200                                                                          
012300 01      INVENTERINGS-TEXTER.                                             
012400   03    FILLER          PIC X(12)   VALUE '  BEGÄRD.INV'.                
012500   03    FILLER          PIC X(12)   VALUE '   REQUESTED'.                
012600   03    FILLER          PIC X(12)   VALUE '      FYS.AV'.                
012700   03    FILLER          PIC X(12)   VALUE '    PHYS DEV'.                
012800   03    FILLER          PIC X(12)   VALUE ' ERSÄTTN.INV'.                
012900   03    FILLER          PIC X(12)   VALUE 'SUPERSESSION'.                
013000   03    FILLER          PIC X(12)   VALUE '  LEVANM.INV'.                
013100   03    FILLER          PIC X(12)   VALUE ' DISCREPANCY'.                
013200   03    FILLER          PIC X(12)   VALUE '  URVALS.INV'.                
013300   03    FILLER          PIC X(12)   VALUE '   SELECTION'.                
013400*                                                                         
013500 01      FILLER REDEFINES INVENTERINGS-TEXTER.                            
013600   03    INV-TEXT-GRUPP OCCURS  5.                                        
013700     05  INV-TEXT OCCURS  2  PIC X(12).                                   
013800*                                                                         
013900 01  GENERELLA-SUBPROGRAM.                                                
014000     03  CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
014100     03  FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
014200     03  W005INIT        PIC X(8)    VALUE 'W005INIT'.                    
014300     03  WMEDKONV        PIC X(8)    VALUE 'WMEDKONV'.                    
014400     EJECT                                                                
014500 01  MESSAGE-CODES.                                                       
014600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014700     03  CONFLICT                PIC X(3)    VALUE '002'.                 
014800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014900     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
015000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015100     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
015200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015300     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
015400     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
015500     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
015600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015700     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
015800     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
015900     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
016000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016100     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016400*01      -COPY WMEDAREA                                                   
016500     EJECT                                                                
016600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016700*01 -COPY WMSGINIT                                                        
016800     EJECT                                                                
016900*****  TP-AREOR                                                           
017000*                                                                         
017100*01  MID -COPY W5I10101 -PRE MID-.                                        
017200     SKIP3                                                                
017300*01  -COPY WMSGAREA.                                                      
017400     SKIP3                                                                
017500*    03  MOD -COPY W5O10101  -PRE MOD- -RED MSG-AREA.                     
017600     SKIP3                                                                
017700*01  -COPY WMFSAREA.                                                      
017800     EJECT                                                                
017900*****  ARBETSAREOR FÖR IMS-SEKTIONERNA.                                   
018000*                                                                         
018100 01  IMS-WS.                                                              
018200   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
018300     SKIP3                                                                
018400*****  STATUSKOD FRÅN IMS                                                 
018500   03    STATUS-WS       PIC XX.                                          
018600     88  SEGMENT-FINNS               VALUE '  '.                          
018700     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
018800     SKIP3                                                                
018900   03    GODK-STATUSKODER.                                                
019000     05  GODK-STATUS  OCCURS 5  INDEXED BY STATUS-IX  PIC XX.             
019100     SKIP3                                                                
019200   03  SSA1             PIC X(96).                                        
019300   03  SSA2             PIC X(96).                                        
019400     EJECT                                                                
019500*****  IMS-FUNKTIONSKODER                                                 
019600     SKIP3                                                                
019700*  03    -COPY  W0003.                                                    
019800     SKIP3                                                                
019900*****  DLI I/O-AREA                                                       
020000                                                                          
020100 01  DLI-IO-WLARTC01.                                                     
020200*    03  -COPY WDK601                                                     
020300     EJECT                                                                
020400 01  DLI-IO-WLARTC11.                                                     
020500*    03  -COPY WDK611                                                     
020600     EJECT                                                                
020610 01  DLI-IO-WLARTC21.                                                     
020620*    03  -COPY WDK621                                                     
020630     EJECT                                                                
020700 01  DLI-IO-WLARTS01.                                                     
020800*    03  -COPY WDK701                                                     
020900     EJECT                                                                
021000 01  DLI-IO-WLARTS11.                                                     
021100*    03  -COPY WDK711                                                     
021200     EJECT                                                                
021300 01  DLI-IO-WDH101.                                                       
021400*    03  -COPY WDH101                                                     
021500     EJECT                                                                
021600 01  DLI-IO-WDH111.                                                       
021700*    03  -COPY WDH111 -PRE INVKOE-                                        
021800     EJECT                                                                
021900 01  DLI-IO-WLARTD11.                                                     
022000*    03  -COPY WDD811 -PRE ARTD-                                          
022100     EJECT                                                                
022200 01  DLI-IO-WLINLB11.                                                     
022300*    03  -COPY WDD902 -PRE INLB11-                                        
022400     EJECT                                                                
022500 01  DLI-IO-WLBENA11.                                                     
022600*    03  -COPY WDD311 -PRE BEN-                                           
022700     EJECT                                                                
022800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022900 01   DLI-IO-AREA-B601.                                                   
023000*     03  -COPY WDB601                                                    
023100 LINKAGE SECTION.                                                         
023200*    -COPY W0009 -PRE MSG-                                                
023300                                                                          
023400     EJECT                                                                
023500*    -COPY W0008     -PRE USEA-.                                          
023600         05  FILLER        PIC X.                                         
023700*    -COPY W0008 -PRE INVA-.                                              
023800       05  INVA-KONKAT-KEY PIC X.                                         
023900     EJECT                                                                
024000*    -COPY W0008  -PRE ARTC-.                                             
024100       05  ARTC-KONKAT-KEY PIC X.                                         
024200                                                                          
024300*    -COPY W0008  -PRE ARTD-.                                             
024400       05  ARTD-KONKAT-KEY PIC X.                                         
024500     EJECT                                                                
024600*    -COPY W0008  -PRE BEN-.                                              
024700       05  BEN-KONKAT-KEY PIC X.                                          
024800                                                                          
024900*    -COPY W0008  -PRE INLB-.                                             
025000       05  INLB-KONKAT-KEY PIC X.                                         
025100     EJECT                                                                
025200*    -COPY W0008  -PRE ARTS-.                                             
025300       05  ARTS-KONKAT-KEY PIC X.                                         
025400     EJECT                                                                
025500*    -COPY W0008  -PRE WDB6-.                                             
025600       05  ARTS-KONKAT-KEY PIC X.                                         
025700     EJECT                                                                
025800 PROCEDURE DIVISION USING MSG-PCB                                         
025900                          USEA-PCB                                        
026000                          INVA-PCB                                        
026100                          ARTC-PCB                                        
026200                          ARTD-PCB                                        
026300                          BEN-PCB                                         
026400                          INLB-PCB                                        
026500                          ARTS-PCB                                        
026600                          WDB6-PCB.                                       
026700     ENTRY 'DLITCBL' USING MSG-PCB                                        
026800                           USEA-PCB                                       
026900                           INVA-PCB                                       
027000                           ARTC-PCB                                       
027100                           ARTD-PCB                                       
027200                           BEN-PCB                                        
027300                           INLB-PCB                                       
027400                           ARTS-PCB                                       
027500                           WDB6-PCB.                                      
027600                                                                          
027700     PERFORM IMS-GET-MSG                                                  
027800     IF SEGMENT-FINNS                                                     
027900       PERFORM A-KOLLA-NYCKLAR                                            
028000       IF ARTIKEL-RETT = JA                                               
028100         PERFORM S1-SECURITY-CHECK-PARTNO-IDLEV                           
028200         IF PASSED-SECURITY-CHECK                                         
028300           PERFORM C-INFORMATIONSBILD                                     
028400         END-IF                                                           
028500       END-IF                                                             
028600       PERFORM IMS-ISRT-MSG                                               
028700     END-IF                                                               
028800     MOVE ZERO TO RETURN-CODE                                             
028900     GOBACK                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 A-KOLLA-NYCKLAR SECTION.                                                 
029300     SKIP2                                                                
029400     IF MSG-DUBBLA-TRANSKODER                                             
029500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10101                 
029600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
029700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029800     ELSE                                                                 
029900       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W5I10101                   
030000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
030100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030200     END-IF                                                               
030300     SKIP2                                                                
030400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030500     MOVE '001'             TO MSGI-KDCALL                                
030600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030700     MOVE '5101'               TO MSGI-IDTRANS                            
030800     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
030900                                                                          
031000     IF MFS-IDTRANS = '5101'                                              
031100     OR (MID-IDARTNR1 NUMERIC                                             
031200     AND MID-IDARTNR1 > ZERO)                                             
031300         MOVE MID-IDARTNR1 TO MSGI-IDARTNR                                
031400     END-IF                                                               
031500                                                                          
031600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031700     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
031800                                                                          
031900     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
032000                                                                          
032100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
032200       MOVE +1 TO SPRAK-IX                                                
032300       MOVE 'S  ' TO W-IDSKYLT                                            
032400     ELSE                                                                 
032500       MOVE +2 TO SPRAK-IX                                                
032600       MOVE 'GB ' TO W-IDSKYLT                                            
032700     END-IF                                                               
032800                                                                          
032900     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
033000     IF IDARTNR-WS NUMERIC                                                
033100       MOVE JA TO ARTIKEL-RETT                                            
033200       MOVE IDARTNR-WS TO W-IDARTNR                                       
033300     ELSE                                                                 
033400       MOVE NEJ TO ARTIKEL-RETT                                           
033500       MOVE FEL-1 (SPRAK-IX) TO MOD-MESSAGE                               
033600     END-IF                                                               
033700                                                                          
033800     IF MID-IDDC-IN = ALL '+'                                             
033900       MOVE MID-IDDC-UT TO WS-IDDC                                        
034000       INSPECT WS-IDDC REPLACING LEADING SPACE BY ZERO                    
034100     ELSE                                                                 
034200       MOVE MID-IDDC-IN TO WS-IDDC                                        
034300     END-IF                                                               
034400                                                                          
034500     MOVE WS-IDDC TO W-IDDC-B6                                            
034600     PERFORM IMS-GU-WDB601                                                
034700     IF DCS-CDC OR WS-IDDC = SPACES OR ZERO                               
034800       MOVE ZERO        TO W-IDDC                                         
034900                           WS-IDDC                                        
035000     ELSE                                                                 
035100       MOVE WS-IDDC     TO W-IDDC                                         
035200     END-IF                                                               
035300                                                                          
035400     SKIP2                                                                
035500     MOVE LOW-VALUE TO MOD-W5O10101                                       
035600     MOVE 'W5O101N1' TO MFS-IDMOD                                         
035700     MOVE '5101' TO MOD-TRANS-NUMMER                                      
035800                                                                          
035900     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
036000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
036100     MOVE WS-IDDC         TO MOD-IDDC-UT                                  
036200     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
036300                                                                          
036400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
036500     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
036600                                                                          
036700     COMPUTE  MSG-KVLL = LENGTH OF MOD-W5O10101 + 4                       
036800     .                                                                    
036900     EJECT                                                                
037000                                                                          
037100 C-INFORMATIONSBILD SECTION.                                              
037200                                                                          
037300*                                 WLARTC01-WDK601, ARTIKELINFO            
037400                                                                          
037500     PERFORM IMS-GU-ARTIKEL-WDK601                                        
037600     IF SEGMENT-FINNS                                                     
037700       MOVE '-'              TO MOD-BINDESTRECK                           
037800       MOVE ART-REKSIFFR     TO MOD-REKSIFFR                              
037900       MOVE ART-KDSORT       TO MOD-KDSORT                                
038000       MOVE ART-KDPRODSL     TO MOD-KDPRODSL                              
038100       MOVE ART-TIERSDAT     TO MOD-TIERSDAT                              
038200       MOVE ART-TIFINLV      TO MOD-TIFINLV                               
038300       MOVE ART-IDLEVNR      TO MOD-IDLEVNR                               
038400       IF ART-KDERS-UTG > 0                                               
038500         MOVE FEL-3(SPRAK-IX) TO MOD-MESSAGE                              
038600       END-IF                                                             
038700       IF ART-FLIART = JA                                                 
038800         MOVE 'Y'            TO MOD-KDIART                                
038900       ELSE                                                               
039000         MOVE 'N'            TO MOD-KDIART                                
039100       END-IF                                                             
039200                                                                          
039300*                                 WLARTC11-WDK611,ARTIKEL-                
039400*                                 CLAGER INFO                             
039500                                                                          
039600       PERFORM IMS-GNP-WDK611                                             
039700       IF SEGMENT-FINNS                                                   
039800         MOVE +1 TO INDX                                                  
039900                                                                          
040000         MOVE CLAG-KDVVKL         TO MOD-KDVVKL                           
040100         MOVE CLAG-KDHF           TO MOD-KDHF                             
040200         MOVE CLAG-IDANSK         TO MOD-IDANSK                           
040300         MOVE CLAG-KDPSLLOC       TO MOD-KDPSLLOC                         
040400                                                                          
040500         COMPUTE W-SUMMA1 = CLAG-KVPB-SATS + CLAG-KVPB-SEP                
040600         MOVE W-SUMMA1            TO MOD-KVPB-TOT (INDX)                  
040700         MOVE CLAG-KVPB-SATS      TO MOD-KVPB-SATS (INDX)                 
040800         MOVE CLAG-IDLEVNR-SEN    TO MOD-IDLEVNR-SEN (INDX)               
040900         MOVE CLAG-TIAVIDAT-SEN   TO MOD-TIAVIDAT-SEN (INDX)              
041000         MOVE CLAG-KVAVIS-SEN     TO MOD-KVAVIS-SEN (INDX)                
041100                                                                          
041200         MOVE CLAG-PRHEMTAG       TO MOD-PRHEMTAG                         
041300         MOVE CLAG-PRINK          TO MOD-PRINK                            
041400         MOVE CLAG-PRARTSTD       TO MOD-PRARTSTD                         
041401                                                                          
041410         MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD               
041420         COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                  
041430         PERFORM IMS-GNP-WDK621                                           
041440         IF SEGMENT-SAKNAS                                                
041450           MOVE CLAG-PRARTSTD       TO MOD-PRARTBES                       
041460         ELSE                                                             
041470           MOVE NEJ                 TO WS-PRARTBES                        
041480           PERFORM UNTIL  SEGMENT-SAKNAS                                  
041490             IF PRL-SUINLEV-PR > ZERO                                     
041491               MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                      
041492               SET SEGMENT-SAKNAS TO TRUE                                 
041493             ELSE                                                         
041494               IF WS-PRARTBES = NEJ                                       
041495                 MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                     
041496                 MOVE JA              TO WS-PRARTBES                      
041497               END-IF                                                     
041498               PERFORM IMS-GNP-WDK621                                     
041499             END-IF                                                       
041500           END-PERFORM                                                    
041501         END-IF                                                           
041600         MOVE CLAG-PRARTSJK       TO MOD-PRARTSJK                         
041700         MOVE CLAG-KDVTH          TO MOD-KDVTH                            
041800         MOVE CLAG-KDKG           TO MOD-KDKG                             
041900                                                                          
042000         MOVE CLAG-TIINVDAT       TO MOD-TIINVDAT (INDX)                  
042100         MOVE CLAG-KVINVS         TO MOD-KVINVS   (INDX)                  
042200                                                                          
042300         MOVE CLAG-KDGK           TO MOD-KDGK                             
042400         MOVE CLAG-KDLTK          TO MOD-KDLTK                            
042500         MOVE CLAG-KVQPACK-1      TO MOD-KVQPACK-1                        
042600                                                                          
042700         MOVE CLAG-KDERS          TO MOD-KDERS                            
042800         MOVE CLAG-KVUTRS         TO MOD-KVUTRS   (INDX)                  
042900         COMPUTE WS-KVAKS-TOTCDC = CLAG-KVAKS-CDC +                       
043000                                   CLAG-KVAKS-PAV                         
043100         MOVE WS-KVAKS-TOTCDC     TO MOD-KVAKS-CDC                        
043200         MOVE CLAG-KVAKS-T        TO MOD-KVAKS-T                          
043300         MOVE CLAG-KVEFRS         TO MOD-KVEFRS   (INDX)                  
043400         MOVE CLAG-KVLS           TO MOD-KVLS     (INDX)                  
043500         MOVE CLAG-KVROS          TO MOD-KVROS    (INDX)                  
043600         MOVE CLAG-KVRESS         TO MOD-KVRESS   (INDX)                  
043700         MOVE CLAG-ADLAGOMR       TO MOD-ADLAGOMR (INDX)                  
043800         MOVE CLAG-ADGANG         TO MOD-ADGANG   (INDX)                  
043900         MOVE CLAG-ADPLATS        TO MOD-ADPLATS  (INDX)                  
044000       END-IF                                                             
044100*                                                                         
044200*                                   WLARTS11-WDK711, BENÄMNING            
044300*                                                                         
044400       IF WS-IDDC = ZERO                                                  
044500         PERFORM CA-SDC-RED-INFO                                          
044600       ELSE                                                               
044700         PERFORM IMS-GU-WDK711                                            
044800       END-IF                                                             
044900                                                                          
045000       IF SEGMENT-FINNS                                                   
045100         MOVE +2 TO INDX                                                  
045200         MOVE ZERO              TO MOD-KVPB-TOT     (INDX)                
045300                                   MOD-KVPB-SATS    (INDX)                
045400         MOVE SLAG-TIINVDAT     TO MOD-TIINVDAT     (INDX)                
045500         MOVE SLAG-KVINVS       TO MOD-KVINVS       (INDX)                
045600         MOVE SLAG-KVUTRS       TO MOD-KVUTRS       (INDX)                
045700                                                                          
045800         COMPUTE WS-KVAKS-TOTSDC = SLAG-KVAKS-SDC +                       
045900                                   SLAG-KVAKS-PAV                         
046000                                                                          
046100         MOVE WS-KVAKS-TOTSDC   TO MOD-KVAKS-SDC                          
046200         MOVE SLAG-KVLS         TO MOD-KVLS         (INDX)                
046300         MOVE SLAG-KVEFRS       TO MOD-KVEFRS       (INDX)                
046400         MOVE SLAG-ADLAGOMR     TO MOD-ADLAGOMR     (INDX)                
046500         MOVE SLAG-ADGANG       TO MOD-ADGANG       (INDX)                
046600         MOVE SLAG-ADPLATS      TO MOD-ADPLATS      (INDX)                
046700       END-IF                                                             
046800*                                                                         
046900*                                                                         
047000*                                                                         
047100*                                   WLBENA11-WDD303, BENÄMNING            
047200*                                                                         
047300       PERFORM IMS-GU-BEN                                                 
047400       IF SEGMENT-FINNS                                                   
047500         MOVE BEN-TEXT-BEART TO MOD-BEART-SVE                             
047600       ELSE                                                               
047700         MOVE SPACE TO MOD-BEART-SVE                                      
047800       END-IF                                                             
047900*                                                                         
048000*                                                                         
048100*                            WLINLB11-WDD902, LEVERANTÖRINFO              
048200*                                                                         
048300       MOVE W-IDARTNR    TO W-IDARTNR-D9                                  
048310       IF WS-IDDC = ZERO                                                  
048320          MOVE WC-CDC-SE TO W-IDDC-D9                                     
048330       ELSE                                                               
048340          MOVE WS-IDDC   TO W-IDDC-D9                                     
048350       END-IF                                                             
048400       PERFORM IMS-GET-LEVERANTEUR-WDD902                                 
048500       IF SEGMENT-FINNS                                                   
048600         MOVE INLB11-KVBR TO MOD-KVBR                                     
048700       END-IF                                                             
048800*                                                                         
048900*                                                                         
049000*                                   WDH101-WDH101, INVENT. ROT            
049100*                                                                         
049200       PERFORM IMS-GET-INVENTERING-WDH101                                 
049300                                                                          
049400         IF SEGMENT-FINNS                                                 
049500         MOVE LOW-VALUE       TO W-IDDC-WDH1-MIN                          
049600         MOVE HIGH-VALUE      TO W-IDDC-WDH1-MAX                          
049700         MOVE +00             TO W-KDINVKAT-MIN                           
049800         MOVE +099            TO W-KDINVKAT-MAX                           
049900         PERFORM IMS-GET-INVENTERING-WDH111                               
050000         PERFORM UNTIL SEGMENT-SAKNAS                                     
050100                                                                          
050200*                                     WDH111-WDH111,   INVENT.INFO        
050300*                                                                         
050400           IF SEGMENT-FINNS                                               
050500             IF INVKOE-INV-IDDC NOT = W-IDDC-B6                           
050600                MOVE INVKOE-INV-IDDC TO W-IDDC-B6                         
050700                PERFORM IMS-GU-WDB601                                     
050800             END-IF                                                       
051000             IF DCS-CDC                                                   
051100               MOVE +1 TO INDX                                            
051200             ELSE                                                         
051300               MOVE +2 TO INDX                                            
051400             END-IF                                                       
051500                                                                          
051600             IF WS-IDDC = ZERO OR DCS-CDC OR                              
051700               INVKOE-INV-IDDC = WS-IDDC                                  
051800               MOVE INVKOE-INV-TISEGKEY TO WS-TISEGKEY                    
051900                                                                          
052000               EVALUATE INVKOE-INV-KDINVKAT                               
052100                 WHEN 1                                                   
052200                   MOVE INV-TEXT (1, INDX) TO MOD-KDINVKAT(INDX)          
052300                   MOVE WS-TIREGDAT        TO MOD-TIM-INV (INDX)          
052400                 WHEN 2                                                   
052500                   MOVE INV-TEXT (2, INDX) TO MOD-KDINVKAT(INDX)          
052600                   MOVE WS-TIREGDAT        TO MOD-TIM-INV (INDX)          
052700                 WHEN 3                                                   
052800                   MOVE INV-TEXT (3, INDX) TO MOD-KDINVKAT(INDX)          
052900                   MOVE WS-TIREGDAT        TO MOD-TIM-INV (INDX)          
053000                 WHEN 4                                                   
053100                   MOVE INV-TEXT (4, INDX) TO MOD-KDINVKAT(INDX)          
053200                   MOVE WS-TIREGDAT        TO MOD-TIM-INV (INDX)          
053300                 WHEN 9                                                   
053400                   MOVE INV-TEXT (5, INDX) TO MOD-KDINVKAT(INDX)          
053500                   MOVE WS-TIREGDAT        TO MOD-TIM-INV (INDX)          
053600               END-EVALUATE                                               
053700             END-IF                                                       
053900                                                                          
054000             PERFORM IMS-GET-INVENTERING-WDH111                           
054100           END-IF                                                         
054200         END-PERFORM                                                      
054300       END-IF                                                             
054400                                                                          
054500       PERFORM IMS-GET-SALDOREG-WDD811                                    
054600                                                                          
054700       IF SEGMENT-FINNS                                                   
054800         MOVE ARTD-SALDO-KVBUFF-OF    TO MOD-KVBUFF-OF                    
054900         MOVE ARTD-SALDO-KVBUFF-F     TO MOD-KVBUFF-F                     
055000       ELSE                                                               
055100         MOVE ZERO                    TO MOD-KVBUFF-OF                    
055200                                         MOD-KVBUFF-F                     
055300       END-IF                                                             
055400     ELSE                                                                 
055500       MOVE FEL-2 (SPRAK-IX)          TO MOD-MESSAGE                      
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900 CA-SDC-RED-INFO SECTION.                                                 
056000                                                                          
056100     PERFORM IMS-GU-WDK701                                                
056200     IF SEGMENT-FINNS                                                     
056300       MOVE LOW-VALUE       TO W-IDDC-WDK7-MIN                            
056400       MOVE HIGH-VALUE      TO W-IDDC-WDK7-MAX                            
056500       PERFORM IMS-GNP-WDK711                                             
056600       IF SEGMENT-FINNS                                                   
056700         IF SLAG-IDDC NOT = W-IDDC-B6                                     
056800            MOVE SLAG-IDDC TO W-IDDC-B6                                   
056900            PERFORM IMS-GU-WDB601                                         
057000         END-IF                                                           
057100                                                                          
057300         MOVE SLAG-TIINVDAT      TO MOD-TIINVDAT (2)                      
057400         MOVE SLAG-KVINVS        TO MOD-KVINVS   (2)                      
057600       END-IF                                                             
057700                                                                          
057800       PERFORM UNTIL SEGMENT-SAKNAS                                       
057900                                                                          
058000         ADD SLAG-KVLS           TO W-SDC-KVLS                            
058100         ADD SLAG-KVAKS-SDC      TO W-SDC-KVAKS-SDC                       
058200         ADD SLAG-KVAKS-PAV      TO W-SDC-KVAKS-PAV                       
058300         ADD SLAG-KVEFRS         TO W-SDC-KVEFRS                          
058400         ADD SLAG-KVUTRS         TO W-SDC-KVUTRS                          
058500                                                                          
058600         PERFORM IMS-GNP-WDK711                                           
058700       END-PERFORM                                                        
058800                                                                          
058900       MOVE +2 TO INDX                                                    
059000                                                                          
059100       MOVE ZERO                TO MOD-KVPB-TOT     (INDX)                
059200                                   MOD-KVPB-SATS    (INDX)                
059300       MOVE W-SDC-KVUTRS        TO MOD-KVUTRS       (INDX)                
059400                                                                          
059500       COMPUTE WS-KVAKS-TOTSDC = W-SDC-KVAKS-SDC +                        
059600                                 W-SDC-KVAKS-PAV                          
059700                                                                          
059800       MOVE WS-KVAKS-TOTSDC     TO MOD-KVAKS-SDC                          
059900       MOVE W-SDC-KVEFRS        TO MOD-KVEFRS       (INDX)                
060000       MOVE W-SDC-KVLS          TO MOD-KVLS         (INDX)                
060100       MOVE MFS-RENSA-FAELT     TO MOD-ADLAGOMR     (INDX)                
060200                                   MOD-ADGANG       (INDX)                
060300                                   MOD-ADPLATS      (INDX)                
060400     END-IF                                                               
060500     .                                                                    
060600     EJECT                                                                
060700                                                                          
060800 S1-SECURITY-CHECK-PARTNO-IDLEV SECTION.                                  
060900     SKIP2                                                                
061000*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
061100     PERFORM IMS-GU-ARTIKEL-WDK601                                        
061200     IF  SEGMENT-FINNS                                                    
061300       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
061400       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
061500       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
061600*        --- USER AUTHORIZED                                              
061700         SET PASSED-SECURITY-CHECK TO TRUE                                
061800       ELSE                                                               
061900*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
062000         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
062100         CALL WMEDKONV USING MED-WMEDAREA                                 
062200         MOVE MED-TEMFSFEL TO MOD-MESSAGE                                 
062300                                                                          
062400         SET BLOCKED-SECURITY-CHECK TO TRUE                               
062500       END-IF                                                             
062600     ELSE                                                                 
062700*      --- ARTIKEL SAKNAS                                                 
062800       MOVE FEL-2 (SPRAK-IX)          TO MOD-MESSAGE                      
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200                                                                          
063300*                                                                         
063400*****  IMS-SEKTIONER.                                                     
063500*                                                                         
063600*****  CALL MOT MSG                                                       
063700 IMS-GET-MSG SECTION.                                                     
063800                                                                          
063900     MOVE '  QC' TO GODK-STATUSKODER                                      
064000     CALL CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                           
064100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064200     PERFORM IMS-STATUS-KONTROLL                                          
064300     .                                                                    
064400     SKIP3                                                                
064500 IMS-ISRT-MSG SECTION.                                                    
064600                                                                          
064700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
064800       MOVE '0' TO MFS-KDHUVOMR                                           
064900     END-IF                                                               
065000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
065100     MOVE SPACE TO GODK-STATUSKODER                                       
065200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
065300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065400     PERFORM IMS-STATUS-KONTROLL                                          
065500     .                                                                    
065600     EJECT                                                                
065700 IMS-GU-ARTIKEL-WDK601 SECTION.                                           
065800                                                                          
065900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
066000     DELIMITED BY SIZE INTO SSA1                                          
066100     MOVE '  GE' TO GODK-STATUSKODER                                      
066200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
066300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
066400     PERFORM IMS-STATUS-KONTROLL                                          
066500     .                                                                    
066600     SKIP3                                                                
066700 IMS-GNP-WDK611 SECTION.                                                  
066800                                                                          
066900     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
067000     DELIMITED BY SIZE INTO SSA1                                          
067100     MOVE '  GE' TO GODK-STATUSKODER                                      
067200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
067300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
067400     PERFORM IMS-STATUS-KONTROLL                                          
067500     .                                                                    
067600     SKIP3                                                                
067610 IMS-GNP-WDK621 SECTION.                                                  
067620                                                                          
067630     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
067640     DELIMITED BY SIZE INTO SSA1                                          
067650     MOVE '  GE' TO GODK-STATUSKODER                                      
067660     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC21 SSA1                 
067670     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
067680     PERFORM IMS-STATUS-KONTROLL                                          
067690     .                                                                    
067691     SKIP3                                                                
067700 IMS-GU-WDK711 SECTION.                                                   
067800                                                                          
067900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
068000     DELIMITED BY SIZE INTO SSA1                                          
068100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
068200     DELIMITED BY SIZE INTO SSA2                                          
068300     MOVE '  GE' TO GODK-STATUSKODER                                      
068400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
068500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
068600     PERFORM IMS-STATUS-KONTROLL                                          
068700     .                                                                    
068800     SKIP2                                                                
068900 IMS-GU-WDK701 SECTION.                                                   
069000                                                                          
069100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
069200     DELIMITED BY SIZE INTO SSA1                                          
069300     MOVE '  GE' TO GODK-STATUSKODER                                      
069400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS01 SSA1                  
069500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
069600     PERFORM IMS-STATUS-KONTROLL                                          
069700     .                                                                    
069800     SKIP2                                                                
069900 IMS-GNP-WDK711 SECTION.                                                  
070000                                                                          
070100     STRING 'WLARTS11(IDDC    >=' W-IDDC-WDK7-MIN-X                       
070200                    '&IDDC    <=' W-IDDC-WDK7-MAX-X ')'                   
070300            DELIMITED BY SIZE INTO SSA1                                   
070400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
070500     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WLARTS11 SSA1                 
070600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
070700     PERFORM IMS-STATUS-KONTROLL                                          
070800     .                                                                    
070900     SKIP3                                                                
071000     EJECT                                                                
071100 IMS-GET-LEVERANTEUR-WDD902 SECTION.                                      
071200                                                                          
071300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
071400     DELIMITED BY SIZE INTO SSA1                                          
071500     MOVE 'WLINLB11 ' TO SSA2                                             
071600     MOVE '  GE' TO GODK-STATUSKODER                                      
071700     CALL CBLTDLI USING GU INLB-PCB DLI-IO-WLINLB11 SSA1 SSA2             
071800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
071900     PERFORM IMS-STATUS-KONTROLL                                          
072000     .                                                                    
072100     SKIP3                                                                
072200 IMS-GET-INVENTERING-WDH101 SECTION.                                      
072300                                                                          
072400     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
072500     DELIMITED BY SIZE INTO SSA1                                          
072600     MOVE '  GE' TO GODK-STATUSKODER                                      
072700     CALL CBLTDLI USING GU INVA-PCB DLI-IO-WDH101 SSA1                    
072800     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
072900     PERFORM IMS-STATUS-KONTROLL                                          
073000     .                                                                    
073100     SKIP3                                                                
073200 IMS-GET-INVENTERING-WDH111 SECTION.                                      
073300                                                                          
073400     STRING 'WDH111  (WDH111KY=>' W-WDH1KEY-MIN-X                         
073500                    '&WDH111KY=<' W-WDH1KEY-MAX-X                         
073600                    '&FLINVBEH =' NEJ ')'                                 
073700            DELIMITED BY SIZE INTO SSA1                                   
073800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
073900     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-WDH111 SSA1                   
074000     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
074100     PERFORM IMS-STATUS-KONTROLL                                          
074200     .                                                                    
074300     SKIP3                                                                
074400 IMS-GET-SALDOREG-WDD811 SECTION.                                         
074500                                                                          
074600     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
074700     DELIMITED BY SIZE INTO SSA1                                          
074800     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
074900     DELIMITED BY SIZE INTO SSA2                                          
075000     MOVE '  GE' TO GODK-STATUSKODER                                      
075100     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-WLARTD11 SSA1 SSA2             
075200     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
075300     PERFORM IMS-STATUS-KONTROLL                                          
075400     .                                                                    
075500     SKIP3                                                                
075600 IMS-GU-BEN  SECTION.                                                     
075700                                                                          
075800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
075900     DELIMITED BY SIZE INTO SSA1                                          
076000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
076100     DELIMITED BY SIZE INTO SSA2                                          
076200     MOVE '  GE' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GU BEN-PCB DLI-IO-WLBENA11 SSA1 SSA2              
076400     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
076500     PERFORM IMS-STATUS-KONTROLL                                          
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-GU-WDB601    SECTION.                                                
076900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
077000          DELIMITED BY SIZE INTO SSA1                                     
077100     MOVE '  GE' TO GODK-STATUSKODER                                      
077200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
077300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
077400     PERFORM IMS-STATUS-KONTROLL                                          
077500     IF SEGMENT-SAKNAS                                                    
077600         MOVE SPACE TO DCS-KDDC                                           
077700     END-IF                                                               
077800     .                                                                    
077900 IMS-STATUS-KONTROLL SECTION.                                             
078000     SET STATUS-IX TO 1                                                   
078100     SEARCH GODK-STATUS                                                   
078200       AT END CALL FELLOG                                                 
078300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
078400     END-SEARCH                                                           
078500     .                                                                    
