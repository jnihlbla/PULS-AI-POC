000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W4039500.                                                
001000*AUTHOR.         LOTTA LANDSTEN.                                          
001100*DATE-WRITTEN.   FEB -86.                                                 
001200                                                                          
001500*    FUNKTION.                                                            
001600*        BILD 4395.                                                       
001700*        PACKNINGSRAPPORTERING ORDERVIS                                   
001800*        DELNING EXPORT.                                                  
001900                                                                          
002000                                                                          
002100*    INDATA.                                                              
002200*        TRANSAKTION: W4T395                                              
002300*        MID:         W4I39501-MID.                                       
002400                                                                          
002500*    UTDATA.                                                              
002600*        MOD:         W4O39501-MOD.                                       
002700*                     W4O39601-MOD.                                       
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP3                                                                
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003310                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W4039500'.            
004100 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77    RADIND                    PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77    WS-IND-FEL                PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77    MAX-RADIND                PIC S9(9)   VALUE +13  COMP SYNC.        
004500 77    4316-IND                  PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77    MAX-4316-IND              PIC S9(9)   VALUE +12  COMP SYNC.        
004700 77    MAX-RADIND-PLUS-1         PIC S9(9)   VALUE +14  COMP SYNC.        
004800 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +274 COMP SYNC.        
004900 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +48  COMP SYNC.        
005000 77    M4396-MOD-LAENGD          PIC S9(4)   VALUE +82  COMP SYNC.        
005100 77    RAETT                     PIC X       VALUE 'R'.                   
005200 77    FEL                       PIC X       VALUE 'F'.                   
005300 77    JA                        PIC X       VALUE 'J'.                   
005400 77    NEJ                       PIC X       VALUE 'N'.                   
005500 77    KOLLI-SW                  PIC X       VALUE 'N'.                   
005600     EJECT                                                                
005610                                                                          
005620 01  DYNAMISKA-SUBPROGRAM.                                                
005630     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005640     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005650                                                                          
005700 01    FILLER                    PIC X(16)                                
005800                                 VALUE 'WS-MODNAMN'.                      
005900 01    WS-MODNAMN.                                                        
006000   03    FILLER                  PIC X       VALUE 'W'.                   
006100   03    WS-MOD-IDTRANS-POS-1    PIC X       VALUE SPACE.                 
006200   03    FILLER                  PIC X       VALUE 'O'.                   
006300   03    WS-MOD-IDTRANS-POS-2-4  PIC X(3)    VALUE SPACE.                 
006400   03    FILLER                  PIC X(2)    VALUE '01'.                  
006500                                                                          
006600 01    FILLER                    PIC X(16)                                
006700                                 VALUE 'WS-IDTRANS-MOD'.                  
006800 01    WS-IDTRANS-MOD.                                                    
006900   03    WS-IDTRANS-POS-1-MOD    PIC X       VALUE SPACE.                 
007000   03    WS-IDTRANS-POS-2-4-MOD  PIC X(3)    VALUE SPACE.                 
007100                                                                          
007200 01    FILLER                    PIC X(16)   VALUE 'DIVERSE'.             
007300 01    DIVERSE.                                                           
007400                                                                          
007600   03    W-IDPRODNR              PIC S9(7)   VALUE ZERO.                  
007601                                                                          
007610   03    WS-PRTVAL               PIC X(2)    VALUE SPACE.                 
007700                                                                          
007800   03    WS-FLRAD-KLAR-TEST      PIC X       VALUE SPACE.                 
007900         88  WS-FLRAD-KLAR                   VALUE 'J'.                   
008000                                                                          
008100   03    WS-FLPRODNR-KLAR-TEST   PIC X       VALUE SPACE.                 
008200         88  WS-FLPRODNR-KLAR                VALUE 'J'.                   
008300                                                                          
008400   03    WS-FLMAN-RAPP           PIC X       VALUE SPACE.                 
008500                                                                          
008600   03    WS-FLVISA-RADNR         PIC X       VALUE SPACE.                 
008700                                                                          
008800   03    WS-FLSIDA1              PIC X       VALUE SPACE.                 
008900                                                                          
009000   03    WS-INDATA-TEST          PIC X       VALUE SPACE.                 
009100         88  WS-INDATA-FEL                   VALUE 'F'.                   
009200         88  WS-INDATA-RAETT                 VALUE 'R'.                   
009300                                                                          
009400   03    WS-IDTRANS              PIC X(4)    VALUE SPACE.                 
009500         88  WS-EGEN-BILD        VALUE '4395'.                            
009600                                                                          
009700   03    WS-IDRADNR-NUM          PIC 9(4)    VALUE ZERO.                  
009800                                                                          
009900   03    WS-IDKOLLI-FOM          PIC 9(5)    VALUE ZERO.                  
010000                                                                          
010100   03    WS-IDKOLLI-TOM          PIC 9(5)    VALUE ZERO.                  
010200                                                                          
010300   03    WS-IDKOLLI              PIC 9(5)    VALUE ZERO.                  
010400                                                                          
010500   03    WS-ANT-KOLLI            PIC S9(5)   VALUE +0  COMP-3.            
010600                                                                          
010700   03    WS-KVLEVART             PIC S9(6)   VALUE +0  COMP-3.            
010800                                                                          
010900   03    WS-KVLEVART-DEC      PIC S9(6)V9(5) VALUE +0.                    
011000   03    WS-KVLEVART-RED REDEFINES WS-KVLEVART-DEC.                       
011100     05    FILLER               PIC S9(6).                                
011200     05    WS-KVLEVART-DEC-RED  PIC 9(5).                                 
011300                                                                          
011400   03    WS-KVLEVART-TOT         PIC S9(6)   VALUE +0  COMP-3.            
011500                                                                          
011600   03    WS-KVLEVART-SUM         PIC S9(6)   VALUE +0  COMP-3.            
011700                                                                          
011800   03    WS-KVLEVART-INT-SUM     PIC S9(6)   VALUE +0  COMP-3.            
011900                                                                          
012000   03    WS-ANT-RAD-INT          PIC S9(6)   VALUE +0  COMP-3.            
012100   03    WS-ANT-RAD-SEGM         PIC S9(6)   VALUE +0  COMP-3.            
012200   03    MAX-ANT-RAD             PIC S9(6)   VALUE +100 COMP-3.           
012300                                                                          
012400   03    WS-IDRADNR-FOM          PIC 9(4)    VALUE ZERO.                  
012500   03    WS-IDRADNR-TOM          PIC 9(4)    VALUE ZERO.                  
012600                                                                          
012700                                                                          
012800     EJECT                                                                
012900                                                                          
013000 01    FILLER                    PIC X(16)                                
013100                                 VALUE 'NYCKLAR-TILL-DLI'.                
013200 01    NYCKLAR-TILL-DLI.                                                  
013300                                                                          
013400   03    W-WDGXKEY-4311-X.                                                
013500     05    FILLER                PIC X(4)    VALUE '4311'.                
013510     05    W-IDDC-4311           PIC X(2).                                
013600     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
013800                                                                          
013900   03    W-WDGXKEY-4312-X.                                                
014000     05    W-IDPRODNR-4312       PIC S9(7)   VALUE ZERO  COMP-3.          
014100     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
014200                                                                          
014300   03    W-WDGXKEY-4314-X.                                                
014400     05    W-IDRADNR-4314        PIC S9(5)   VALUE ZERO  COMP-3.          
014500     05    FILLER                PIC X(7)    VALUE LOW-VALUE.             
014600                                                                          
014700   03    W-KDBEHAND-4314-X.                                               
014800     05    W-KDBEHAND-RAD-4314   PIC S9      VALUE ZERO  COMP-3.          
014900                                                                          
015000   03    W-WDGXKEY-4315-X.                                                
015100     05    FILLER                PIC X(4)    VALUE '4315'.                
015200     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
015300                                                                          
015400   03    W-WDGXKEY-4316-X.                                                
015500     05    W-IDPRODNR-4316       PIC S9(7)   VALUE ZERO  COMP-3.          
015600     05    W-IDPTYP-4316         PIC X(3)    VALUE SPACE.                 
015700     05    W-IDKOLLI-4316        PIC S9(5)   VALUE ZERO  COMP-3.          
015800     05    FILLER                PIC X(10)   VALUE LOW-VALUE.             
015900                                                                          
016000     EJECT                                                                
016100 01    FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
016200 01    MEDDELANDE.                                                        
016300                                                                          
016400   03    FEL1.                                                            
016500      05    FILLER               PIC X(40)   VALUE                        
016600           '748. UPPLYSTA FÄLT FEL'.                                      
016700      05    FILLER               PIC X(40)   VALUE                        
016800           '748. VERLICHTE ZONE FOUTIEF'.                                 
016900   03    FILLER  REDEFINES FEL1.                                          
017000      05    FEL-1                PIC X(40)   OCCURS 2.                    
017100                                                                          
017200   03    FEL2.                                                            
017300      05    FILLER               PIC X(40)   VALUE                        
017400           '754. FEL I NYCKEL BÖRJA FRÅN GRUND-BILD'.                     
017500      05    FILLER               PIC X(40)   VALUE                        
017600           '754. HERBEGIN VANAF BASISBEELD'.                              
017700   03    FILLER  REDEFINES FEL2.                                          
017800      05    FEL-2                PIC X(40)   OCCURS 2.                    
017900                                                                          
018000   03    FEL3.                                                            
018100      05    FILLER               PIC X(40)   VALUE                        
018200           '750. INGÅNG VIA ANNAN MENY'.                                  
018300      05    FILLER               PIC X(40)   VALUE                        
018400           '750. TOEGANGELIJKK VIA EEN ANDERE MENU'.                      
018500   03    FILLER  REDEFINES FEL3.                                          
018600      05    FEL-3                PIC X(40)   OCCURS 2.                    
018700                                                                          
018800   03    FEL4.                                                            
018900      05    FILLER               PIC X(40)   VALUE                        
019000           '762. GÅ TILL BÖRJA OM-BILDEN'.                                
019100      05    FILLER               PIC X(40)   VALUE                        
019200           '762. DIST-KLANT-ORDER BESTAAT NIET'.                          
019300   03    FILLER  REDEFINES FEL4.                                          
019400      05    FEL-4                PIC X(40)   OCCURS 2.                    
019500                                                                          
019600     EJECT                                                                
019700******************************************************************        
019800*                                                                         
019900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
020000*                                                                         
020100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
020200     SKIP3                                                                
020300 01    FILLER                    PIC X(16)                                
020400                                 VALUE 'MID W4I39401 MID'.                
020500*01    -COPY W4I39501C0                                                   
020700     EJECT                                                                
020800*01    -COPY WMSGAREA                                                     
021000     EJECT                                                                
021100*  03  MOD -COPY W4O39501C0 -RED MSG-AREA                                 
021300     EJECT                                                                
021400*  03  MOD -COPY W4O39601C0 -RED MSG-AREA -PRE M4396-                     
021600     EJECT                                                                
021700 01    FILLER                    PIC X(16)   VALUE 'P-TO-P-SW2'.          
021800 01    P-TO-P-SW2.                                                        
021900       03  PTOP2-LL              PIC S9(4)   VALUE +89 COMP SYNC.         
022000       03  PTOP2-Z1              PIC X       VALUE LOW-VALUE.             
022100       03  PTOP2-Z2              PIC X       VALUE LOW-VALUE.             
022200       03  PTOP2-TRANSKOD        PIC X(7)    VALUE 'W4T396U'.             
022300       03  FILLER                PIC X       VALUE SPACE.                 
022400       03  FILLER                PIC X(4)    VALUE '4395'.                
022500       03  PTOP2-KDMFSFOR        PIC X(1).                                
022600       03  PTOP2-IDPRODNR-IN     PIC X(7).                                
022700       03  PTOP2-IDPRODNR-UT     PIC X(7).                                
022800       03  PTOP2-IDDISTR-UT      PIC X(4).                                
022900       03  PTOP2-IDKUNDNR-UT     PIC X(6).                                
023000       03  PTOP2-KDFRAKT-UT      PIC X(2).                                
023100       03  PTOP2-IDORDNR-UT      PIC X(5).                                
023200       03  PTOP2-KDORDKL-UT      PIC X(1).                                
023210       03  PTOP2-IDDC-UT         PIC X(2).                                
023220       03  PTOP2-PRTVAL-ADRESSFL PIC X(2).                                
023300     EJECT                                                                
023400*01    -COPY WMFSAREA                                                     
023600     EJECT                                                                
023700******************************************************************        
023800*                                                                         
023900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024000*                                                                         
024100 01    IMS-WS.                                                            
024200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
024300     SKIP3                                                                
024400*                        **** STATUS-KOD FRÅN IMS                         
024500   03    STATUS-WS               PIC XX.                                  
024600     88    SEGMENT-FINNS                     VALUE '  '.                  
024700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
024800     SKIP3                                                                
024900   03    GODK-STATUSKODER.                                                
025000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
025100     SKIP3                                                                
025200 01    SSA1                      PIC X(96).                               
025300 01    SSA2                      PIC X(96).                               
025400     EJECT                                                                
025500*                            IMS FUNKTIONSKODER                           
025600*01    -COPY W0003                                                        
025800     EJECT                                                                
025900*                            DLI INPUT-OUTPUT AREA                        
026000 01    DLI-IO-AREA.                                                       
026100   03    IO-AREA                 PIC X(370)  VALUE SPACE.                 
026200     SKIP3                                                                
026300*  03    XXDK11 -COPY WDGX4312C0  -RED IO-AREA                            
026500     EJECT                                                                
026600*  03    XXDK21 -COPY WDGX4314C0  -RED IO-AREA                            
026800     EJECT                                                                
026900*  03    XXDL11 -COPY WDGX4316C0  -RED IO-AREA                            
027100     EJECT                                                                
027200*    08    AREA -COPY W4I31501C0 -RED 4316-FILLER -PRE 4316-A-            
027400     EJECT                                                                
027500*    08    AREA -COPY W4I31401C0 -RED 4316-FILLER -PRE 4316-B-            
027700     EJECT                                                                
027800 LINKAGE SECTION.                                                         
027900*01    -COPY W0009     -PRE MSG-                                          
028100     EJECT                                                                
028200*01    -COPY W0009     -PRE ALT2-                                         
028400     EJECT                                                                
028500*01    -COPY W0008     -PRE XXDK-                                         
028700        05 FILLER                PIC X.                                   
028800     EJECT                                                                
028900*01    -COPY W0008     -PRE XXDL-                                         
029100        05 FILLER                PIC X.                                   
029200     EJECT                                                                
029300 PROCEDURE DIVISION USING MSG-PCB ALT2-PCB                                
029400     XXDK-PCB XXDL-PCB.                                                   
029500                                                                          
029600     ENTRY 'DLITCBL' USING MSG-PCB ALT2-PCB XXDK-PCB XXDL-PCB.            
029700                                                                          
029800     PERFORM IMS-GET-MSG                                                  
029900                                                                          
030000     IF SEGMENT-FINNS                                                     
030100       PERFORM A-INIT-SPARA-INPUT                                         
030200       IF (MID-IDPRODNR-UT NUMERIC                                        
030300       AND MID-IDDISTR-UT  NUMERIC                                        
030400       AND MID-IDKUNDNR-UT NUMERIC                                        
030500       AND MID-KDFRAKT-UT  NUMERIC                                        
030600       AND MID-IDORDNR-UT NUMERIC                                         
030700       AND MID-KDORDKL-UT  NUMERIC)                                       
030800                                                                          
031000         MOVE MID-IDDC-UT           TO W-IDDC-4311                        
031100         MOVE MID-IDPRODNR-UT       TO W-IDPRODNR                         
031200         MOVE W-IDPRODNR            TO W-IDPRODNR-4312                    
031300                                       W-IDPRODNR-4316                    
031400         MOVE MFS-IDTRANS  TO WS-IDTRANS                                  
031500         IF  MFS-UPDATE                                                   
031600         AND WS-EGEN-BILD                                                 
031700           PERFORM B-INDATA-KOLL                                          
031800           IF WS-INDATA-RAETT                                             
031900             IF WS-FLVISA-RADNR = JA                                      
032000               PERFORM D-VISA-RADNR                                       
032100               MOVE MFS-ADD-SAETT-CURSOR TO                               
032200                    MOD-KVLEVART-DELAT1-ATTR                              
032300               MOVE WS-FLMAN-RAPP TO MOD-FLMAN-RAPP                       
032400               MOVE WS-FLSIDA1    TO MOD-FLSIDA1                          
032500               MOVE MAX-MOD-LAENGD TO MSG-KVLL                            
032600             ELSE                                                         
032700               PERFORM C-REL-KOLL                                         
032800               IF WS-INDATA-RAETT                                         
032900                 PERFORM E-UPPDATERA                                      
033000                 PERFORM F-KOLLA-4314                                     
033100                 PERFORM S99-NAESTA-TRANS                                 
033200               ELSE                                                       
033300                 MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                        
033400                 PERFORM S01-ROER-EJ-FAELT                                
033500                 MOVE MAX-MOD-LAENGD TO MSG-KVLL                          
033600               END-IF                                                     
033700             END-IF                                                       
033800           ELSE                                                           
033900             MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                            
034000             PERFORM S01-ROER-EJ-FAELT                                    
034100             MOVE MAX-MOD-LAENGD TO MSG-KVLL                              
034200           END-IF                                                         
034300         ELSE                                                             
034400           MOVE FEL-3 (INDX) TO MOD-TEMFSFEL                              
034500           MOVE MFS-IDTRANS TO WS-IDTRANS-MOD                             
034600           MOVE WS-IDTRANS-POS-1-MOD TO WS-MOD-IDTRANS-POS-1              
034700           MOVE WS-IDTRANS-POS-2-4-MOD TO                                 
034800                               WS-MOD-IDTRANS-POS-2-4                     
034900           MOVE WS-MODNAMN TO MFS-IDMOD                                   
035000           MOVE MIN-MOD-LAENGD TO MSG-KVLL                                
035100         END-IF                                                           
035200       ELSE                                                               
035300         MOVE FEL-2 (INDX) TO MOD-TEMFSFEL                                
035400         PERFORM S01-ROER-EJ-FAELT                                        
035500         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
035600       END-IF                                                             
035700       IF KOLLI-SW = 'N'                                                  
035800         PERFORM IMS-INSERT-MSG                                           
035900       END-IF                                                             
036000     END-IF                                                               
036100     MOVE ZERO TO RETURN-CODE                                             
036200     GOBACK                                                               
036300     CONTINUE.                                                            
036400     EJECT                                                                
036500 A-INIT-SPARA-INPUT SECTION.                                              
036600                                                                          
036700     IF MSG-DUBBLA-TRANSKODER                                             
036800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I39501                 
036900       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
037000       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
037100       MOVE MSG-IDPFK            TO MFS-IDPFK                             
037200     ELSE                                                                 
037300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I39501                  
037400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
037500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
037600       MOVE ' ' TO MFS-IDPFK                                              
037700     END-IF                                                               
037800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
037900     MOVE LOW-VALUE TO MSG-AREA                                           
038000     MOVE 'W4O39501' TO MFS-IDMOD                                         
038100     MOVE '4395' TO MOD-IDTRANS                                           
038200                                                                          
038300     IF SWEDISH-TEXT                                                      
038400       MOVE +1 TO INDX                                                    
038500     ELSE                                                                 
038600       MOVE +2 TO INDX                                                    
038700     END-IF                                                               
038800     INSPECT MID-IDPRODNR-UT REPLACING LEADING SPACE BY ZERO              
038900     INSPECT MID-IDDISTR-UT REPLACING LEADING SPACE BY ZERO               
039000     INSPECT MID-IDKUNDNR-UT REPLACING LEADING SPACE BY ZERO              
039100     INSPECT MID-KDFRAKT-UT REPLACING LEADING SPACE BY ZERO               
039200     INSPECT MID-IDORDNR-UT REPLACING LEADING SPACE BY ZERO               
039300                                                                          
039400     MOVE MID-IDPRODNR-UT    TO MOD-IDPRODNR-UT                           
039500     MOVE MID-IDDISTR-UT     TO MOD-IDDISTR-UT                            
039600     MOVE MID-IDKUNDNR-UT    TO MOD-IDKUNDNR-UT                           
039700     MOVE MID-KDFRAKT-UT     TO MOD-KDFRAKT-UT                            
039800     MOVE MID-IDORDNR-UT     TO MOD-IDORDNR-UT                            
039900     MOVE MID-KDORDKL-UT     TO MOD-KDORDKL-UT                            
039910     MOVE MID-IDDC-UT        TO MOD-IDDC-UT                               
039920     MOVE MID-PRTVAL-ADRESSFL TO MOD-PRTVAL-ADRESSFL                      
039930                                 WS-PRTVAL                                
040000                                                                          
040100     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
040200     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
040300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
040400     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
040500     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
040600                                                                          
040700     MOVE MID-FLMAN-RAPP      TO MOD-FLMAN-RAPP                           
040800     MOVE MID-FLSIDA1         TO MOD-FLSIDA1                              
040900                                                                          
041000     IF  MID-SUM = ALL '+'                                                
041100       MOVE MFS-RENSA-FAELT TO MOD-SUM                                    
041200     END-IF                                                               
041300     IF  MID-IDRADNR-SENAST = ALL '+'                                     
041400       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-SENAST                         
041500     END-IF                                                               
041600     MOVE MFS-RENSA-FAELT   TO MOD-IDRADNR                                
041700                               MOD-KVLEVART                               
041800                               MOD-KVLEVART-DELAT1                        
041900                               MOD-IDKOLLI-FOM                            
042000                               MOD-IDKOLLI-TOM                            
042100                                                                          
042200     MOVE NEJ               TO KOLLI-SW                                   
042300     MOVE +1                TO RADIND                                     
042400                                                                          
042500     PERFORM UNTIL                                                        
042600      NOT ( RADIND < MAX-RADIND-PLUS-1 )                                  
042700       MOVE MFS-RENSA-FAELT   TO MOD-KVLEVART-DELAT2 (RADIND)             
042800                                 MOD-IDKOLLI         (RADIND)             
042900       ADD +1                 TO RADIND                                   
043000     END-PERFORM                                                          
043100     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL                               
043200                               MOD-TEMFSINF                               
043300                                                                          
043400     MOVE NEJ               TO WS-FLRAD-KLAR-TEST                         
043500                               WS-FLPRODNR-KLAR-TEST                      
043600                               WS-FLVISA-RADNR                            
043700                                                                          
043800     MOVE +0                TO WS-ANT-KOLLI                               
043900                               WS-KVLEVART                                
044000                               WS-KVLEVART-SUM                            
044100                               WS-KVLEVART-INT-SUM                        
044200                               WS-IDKOLLI-FOM                             
044300                               WS-IDKOLLI-TOM                             
044400     CONTINUE.                                                            
044500                                                                          
044600     EJECT                                                                
044700 B-INDATA-KOLL SECTION.                                                   
044800                                                                          
044900     MOVE RAETT              TO WS-INDATA-TEST                            
045000     MOVE MID-FLMAN-RAPP     TO WS-FLMAN-RAPP                             
045100     MOVE MID-FLSIDA1        TO WS-FLSIDA1                                
045200                                                                          
045300     IF  WS-FLMAN-RAPP = '+'                                              
045400                                                                          
045500       IF  MID-IDRADNR = ALL '+'                                          
045600         MOVE JA       TO WS-FLVISA-RADNR                                 
045700         MOVE NEJ      TO WS-FLMAN-RAPP                                   
045800         PERFORM S03-STAENG-IDRADNR                                       
045900       ELSE                                                               
046000         MOVE JA       TO WS-FLMAN-RAPP                                   
046100         PERFORM BA-KOLLA-IDRADNR                                         
046200         PERFORM BB-KOLLA-INTERV                                          
046300         PERFORM BC-KOLLA-RADER                                           
046400         PERFORM BD-KOLLA-KOMB                                            
046500       END-IF                                                             
046600     ELSE                                                                 
046700       EVALUATE TRUE                                                      
046800       WHEN WS-FLMAN-RAPP = JA                                            
046900         PERFORM BA-KOLLA-IDRADNR                                         
047000         PERFORM BB-KOLLA-INTERV                                          
047100         PERFORM BC-KOLLA-RADER                                           
047200         PERFORM BD-KOLLA-KOMB                                            
047300                                                                          
047400         IF  WS-FLSIDA1 = JA                                              
047500           PERFORM S03-STAENG-IDRADNR                                     
047600         END-IF                                                           
047700        WHEN OTHER                                                        
047800         PERFORM S03-STAENG-IDRADNR                                       
047900                                                                          
048000         PERFORM BB-KOLLA-INTERV                                          
048100         PERFORM BC-KOLLA-RADER                                           
048200         PERFORM BD-KOLLA-KOMB                                            
048300                                                                          
048400       END-EVALUATE                                                       
048500     END-IF                                                               
048600     IF  MID-SUM = ALL '+'                                                
048700     OR  MID-SUM NUMERIC                                                  
048800       CONTINUE                                                           
048900     ELSE                                                                 
049000       MOVE FEL            TO WS-INDATA-TEST                              
049100       MOVE FEL-4 (INDX)   TO MOD-TEMFSFEL                                
049200     END-IF                                                               
049300     CONTINUE.                                                            
049400     EJECT                                                                
049500 BA-KOLLA-IDRADNR SECTION.                                                
049600                                                                          
049700     IF MID-IDRADNR = ALL '+'                                             
049800       MOVE FEL                   TO WS-INDATA-TEST                       
049900       MOVE MFS-NUM-FAELT-FEL TO                                          
050000                            MOD-IDRADNR-ATTR                              
050100     ELSE                                                                 
050200       IF MID-IDRADNR NUMERIC                                             
050300         MOVE MFS-NUM-FAELT-RAETT TO                                      
050400                              MOD-IDRADNR-ATTR                            
050500       ELSE                                                               
050600         MOVE FEL               TO WS-INDATA-TEST                         
050700         MOVE MFS-NUM-FAELT-FEL TO                                        
050800                              MOD-IDRADNR-ATTR                            
050900       END-IF                                                             
051000     END-IF                                                               
051100     CONTINUE.                                                            
051200     EJECT                                                                
051300 BB-KOLLA-INTERV SECTION.                                                 
051400                                                                          
051500     IF  MID-KVLEVART-DELAT1 = ALL '+'                                    
051600     AND MID-IDKOLLI-FOM     = ALL '+'                                    
051700     AND MID-IDKOLLI-TOM     = ALL '+'                                    
051800       MOVE MFS-NUM-FAELT-RAETT TO                                        
051900                            MOD-KVLEVART-DELAT1-ATTR                      
052000                            MOD-IDKOLLI-FOM-ATTR                          
052100                            MOD-IDKOLLI-TOM-ATTR                          
052200     ELSE                                                                 
052300       IF MID-KVLEVART-DELAT1 = ALL '+'                                   
052400         MOVE FEL               TO WS-INDATA-TEST                         
052500         MOVE MFS-NUM-FAELT-FEL TO                                        
052600                              MOD-KVLEVART-DELAT1-ATTR                    
052700       ELSE                                                               
052800         EVALUATE TRUE                                                    
052900         WHEN MID-KVLEVART-DELAT1 NUMERIC                                 
053000           IF MID-KVLEVART-DELAT1 = ZERO                                  
053100             MOVE FEL               TO WS-INDATA-TEST                     
053200             MOVE MFS-NUM-FAELT-FEL TO                                    
053300                                  MOD-KVLEVART-DELAT1-ATTR                
053400           ELSE                                                           
053500             MOVE MFS-NUM-FAELT-RAETT TO                                  
053600                                MOD-KVLEVART-DELAT1-ATTR                  
053700           END-IF                                                         
053800          WHEN OTHER                                                      
053900           MOVE FEL               TO WS-INDATA-TEST                       
054000           MOVE MFS-NUM-FAELT-FEL TO                                      
054100                                MOD-KVLEVART-DELAT1-ATTR                  
054200         END-EVALUATE                                                     
054300       END-IF                                                             
054400       IF MID-IDKOLLI-FOM = ALL '+'                                       
054500         MOVE FEL               TO WS-INDATA-TEST                         
054600         MOVE MFS-NUM-FAELT-FEL TO                                        
054700                              MOD-IDKOLLI-FOM-ATTR                        
054800       ELSE                                                               
054900         EVALUATE TRUE                                                    
055000             WHEN (MID-IDKOLLI-FOM NUMERIC) AND (MID-IDKOLLI-FOM          
055100           >                                                              
055200            0)                                                            
055300           MOVE MID-IDKOLLI-FOM     TO WS-IDKOLLI-FOM                     
055400           MOVE MFS-NUM-FAELT-RAETT TO                                    
055500                                MOD-IDKOLLI-FOM-ATTR                      
055600          WHEN OTHER                                                      
055700           MOVE FEL               TO WS-INDATA-TEST                       
055800           MOVE MFS-NUM-FAELT-FEL TO                                      
055900                                MOD-IDKOLLI-FOM-ATTR                      
056000         END-EVALUATE                                                     
056100       END-IF                                                             
056200       IF MID-IDKOLLI-TOM = ALL '+'                                       
056300         MOVE FEL               TO WS-INDATA-TEST                         
056400         MOVE MFS-NUM-FAELT-FEL TO                                        
056500                              MOD-IDKOLLI-TOM-ATTR                        
056600       ELSE                                                               
056700         EVALUATE TRUE                                                    
056800             WHEN (MID-IDKOLLI-TOM NUMERIC) AND (MID-IDKOLLI-TOM          
056900           >                                                              
057000            0)                                                            
057100           MOVE MID-IDKOLLI-TOM     TO WS-IDKOLLI-TOM                     
057200           MOVE MFS-NUM-FAELT-RAETT TO                                    
057300                                MOD-IDKOLLI-TOM-ATTR                      
057400          WHEN OTHER                                                      
057500           MOVE FEL               TO WS-INDATA-TEST                       
057600           MOVE MFS-NUM-FAELT-FEL TO                                      
057700                                MOD-IDKOLLI-TOM-ATTR                      
057800         END-EVALUATE                                                     
057900       END-IF                                                             
058000       IF WS-INDATA-RAETT                                                 
058100         IF MID-IDKOLLI-TOM > MID-IDKOLLI-FOM                             
058200           CONTINUE                                                       
058300         ELSE                                                             
058400           MOVE FEL               TO WS-INDATA-TEST                       
058500           MOVE MFS-NUM-FAELT-FEL TO                                      
058600                              MOD-IDKOLLI-TOM-ATTR                        
058700         END-IF                                                           
058800       END-IF                                                             
058900     END-IF                                                               
059000     CONTINUE.                                                            
059100     EJECT                                                                
059200 BC-KOLLA-RADER SECTION.                                                  
059300                                                                          
059400     MOVE +1          TO RADIND                                           
059500                                                                          
059600     PERFORM UNTIL                                                        
059700      NOT ( RADIND < MAX-RADIND-PLUS-1 )                                  
059800       IF MID-RAD (RADIND) NOT = ALL '+'                                  
059900                                                                          
060000         IF MID-KVLEVART-DELAT2 (RADIND) = ALL '+'                        
060100           MOVE FEL               TO WS-INDATA-TEST                       
060200           MOVE MFS-NUM-FAELT-FEL TO                                      
060300                    MOD-KVLEVART-DELAT2-ATTR (RADIND)                     
060400         ELSE                                                             
060500           EVALUATE TRUE                                                  
060600           WHEN MID-KVLEVART-DELAT2 (RADIND) NUMERIC                      
060700             IF MID-KVLEVART-DELAT2 (RADIND) = ZERO                       
060800               MOVE FEL               TO WS-INDATA-TEST                   
060900               MOVE MFS-NUM-FAELT-FEL TO                                  
061000                        MOD-KVLEVART-DELAT2-ATTR (RADIND)                 
061100             ELSE                                                         
061200               COMPUTE WS-KVLEVART-SUM = WS-KVLEVART-SUM +                
061300                          MID-KVLEVART-DELAT2 (RADIND)                    
061400               MOVE MFS-NUM-FAELT-RAETT TO                                
061500                        MOD-KVLEVART-DELAT2-ATTR (RADIND)                 
061600             END-IF                                                       
061700            WHEN OTHER                                                    
061800             MOVE FEL               TO WS-INDATA-TEST                     
061900             MOVE MFS-NUM-FAELT-FEL TO                                    
062000                      MOD-KVLEVART-DELAT2-ATTR (RADIND)                   
062100           END-EVALUATE                                                   
062200         END-IF                                                           
062300         IF MID-IDKOLLI (RADIND) = ALL '+'                                
062400           MOVE FEL               TO WS-INDATA-TEST                       
062500           MOVE MFS-NUM-FAELT-FEL TO                                      
062600                    MOD-IDKOLLI-ATTR (RADIND)                             
062700         ELSE                                                             
062800           EVALUATE TRUE                                                  
062900               WHEN (MID-IDKOLLI (RADIND) NUMERIC) AND                    
063000             (MID-IDKOLLI                                                 
063100              (RADIND) > 0)                                               
063200             MOVE MFS-NUM-FAELT-RAETT TO                                  
063300                      MOD-IDKOLLI-ATTR (RADIND)                           
063400            WHEN OTHER                                                    
063500             MOVE FEL               TO WS-INDATA-TEST                     
063600             MOVE MFS-NUM-FAELT-FEL TO                                    
063700                      MOD-IDKOLLI-ATTR (RADIND)                           
063800           END-EVALUATE                                                   
063900         END-IF                                                           
064000       END-IF                                                             
064100       ADD +1          TO RADIND                                          
064200     END-PERFORM                                                          
064300     CONTINUE.                                                            
064400     EJECT                                                                
064500 BD-KOLLA-KOMB SECTION.                                                   
064600                                                                          
064700     IF  WS-INDATA-RAETT                                                  
064800       IF  MID-KVLEVART-DELAT1 = ALL '+'                                  
064900       AND MID-IDKOLLI-FOM     = ALL '+'                                  
065000       AND MID-IDKOLLI-TOM     = ALL '+'                                  
065100       AND MID-RADER           = ALL '+'                                  
065200         MOVE FEL               TO WS-INDATA-TEST                         
065300         MOVE MFS-NUM-FAELT-FEL TO                                        
065400                              MOD-KVLEVART-DELAT1-ATTR                    
065500         MOVE MFS-NUM-FAELT-FEL TO                                        
065600                              MOD-IDKOLLI-FOM-ATTR                        
065700         MOVE MFS-NUM-FAELT-FEL TO                                        
065800                              MOD-IDKOLLI-TOM-ATTR                        
065900       END-IF                                                             
066000     END-IF                                                               
066100     CONTINUE.                                                            
066200     EJECT                                                                
066300                                                                          
066400 C-REL-KOLL SECTION.                                                      
066500                                                                          
066600     PERFORM IMS-GET-XXDK-4311                                            
066700     MOVE MID-IDRADNR TO W-IDRADNR-4314                                   
066800                                                                          
066900     IF WS-FLMAN-RAPP = JA                                                
067000       PERFORM IMS-GET-XXDK-4314-STAT-GE                                  
067100                                                                          
067200       IF SEGMENT-SAKNAS                                                  
067300         MOVE FEL               TO WS-INDATA-TEST                         
067400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR                       
067500       ELSE                                                               
067600         IF 4314-KDBEHAND-RAD = +2                                        
067700           MOVE FEL               TO WS-INDATA-TEST                       
067800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR                     
067900         END-IF                                                           
068000       END-IF                                                             
068100     ELSE                                                                 
068200       PERFORM IMS-GET-XXDK-4314-STAT-BLANK                               
068300     END-IF                                                               
068400     IF WS-INDATA-RAETT                                                   
068500                                                                          
068600       IF MID-KVLEVART-DELAT1 NOT = ALL '+'                               
068700         PERFORM CA-BERAKNA-INTERV                                        
068800         COMPUTE WS-KVLEVART-SUM =                                        
068900                 WS-KVLEVART-SUM + MID-KVLEVART-DELAT1                    
069000       END-IF                                                             
069100       IF MID-SUM NOT = ALL '+'                                           
069200         COMPUTE WS-KVLEVART-TOT = WS-KVLEVART-TOT + MID-SUM              
069300         CONTINUE                                                         
069400       END-IF                                                             
069500       COMPUTE WS-KVLEVART-TOT =                                          
069600               WS-KVLEVART-TOT + WS-KVLEVART-SUM                          
069700                                                                          
069800       IF WS-KVLEVART-TOT < 4314-KVLEVART                                 
069900*****  1 SIDA TILL                                                        
070000         CONTINUE                                                         
070100       ELSE                                                               
070200         EVALUATE TRUE                                                    
070300         WHEN WS-KVLEVART-TOT = 4314-KVLEVART                             
070400           MOVE JA                   TO WS-FLRAD-KLAR-TEST                
070500          WHEN OTHER                                                      
070600           MOVE FEL         TO WS-INDATA-TEST                             
070700           MOVE +1          TO RADIND                                     
070800                                                                          
070900           PERFORM UNTIL                                                  
071000            NOT ( RADIND < MAX-RADIND-PLUS-1 )                            
071100             IF MID-KVLEVART-DELAT2 (RADIND) NOT = ALL '+'                
071200               MOVE RADIND     TO WS-IND-FEL                              
071300             END-IF                                                       
071400             ADD +1           TO RADIND                                   
071500           END-PERFORM                                                    
071600           IF WS-IND-FEL > 0                                              
071700             MOVE MFS-NUM-FAELT-FEL TO                                    
071800                      MOD-KVLEVART-DELAT2-ATTR (WS-IND-FEL)               
071900           ELSE                                                           
072000             MOVE MFS-NUM-FAELT-FEL TO                                    
072100                      MOD-KVLEVART-DELAT1-ATTR                            
072200           END-IF                                                         
072300         END-EVALUATE                                                     
072400       END-IF                                                             
072500     END-IF                                                               
072600     CONTINUE.                                                            
072700     EJECT                                                                
072800 CA-BERAKNA-INTERV SECTION.                                               
072900                                                                          
073000     COMPUTE WS-ANT-KOLLI = WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1           
073100                                                                          
073200     MOVE MID-KVLEVART-DELAT1 TO WS-KVLEVART                              
073300     COMPUTE WS-KVLEVART-DEC =                                            
073400             WS-KVLEVART / WS-ANT-KOLLI                                   
073500                                                                          
073600     IF  WS-KVLEVART-DEC-RED > 0                                          
073700       MOVE FEL         TO WS-INDATA-TEST                                 
073800       MOVE MFS-NUM-FAELT-FEL TO                                          
073900                          MOD-KVLEVART-DELAT1-ATTR                        
074000     ELSE                                                                 
074100       MOVE WS-KVLEVART-DEC   TO WS-KVLEVART                              
074200     END-IF                                                               
074300     CONTINUE.                                                            
074400     EJECT                                                                
074500 D-VISA-RADNR SECTION.                                                    
074600                                                                          
074700     PERFORM IMS-GET-XXDK-4311                                            
074800     PERFORM IMS-GET-XXDK-4314                                            
074900     MOVE 4314-IDRADNR   TO WS-IDRADNR-NUM                                
075000     MOVE WS-IDRADNR-NUM TO MOD-IDRADNR                                   
075100     MOVE 4314-KVLEVART  TO MOD-KVLEVART                                  
075200     CONTINUE.                                                            
075300                                                                          
075400     EJECT                                                                
075500 E-UPPDATERA SECTION.                                                     
075600                                                                          
075700     PERFORM IMS-GET-XXDK-4312-F                                          
075800                                                                          
075900     IF 4312-KDBEHAND-DEL NOT = +3                                        
076000       MOVE +3          TO 4312-KDBEHAND-DEL                              
076100       PERFORM IMS-REPL-XXDK                                              
076200     END-IF                                                               
076300     PERFORM IMS-GET-XXDL-4315                                            
076400                                                                          
076500     IF MID-IDKOLLI-FOM NOT = ALL '+'                                     
076600       MOVE MID-IDKOLLI-FOM     TO WS-IDKOLLI                             
076700                                                                          
076800       PERFORM UNTIL                                                      
076900        NOT ( WS-IDKOLLI < MID-IDKOLLI-TOM OR WS-IDKOLLI =                
077000          MID-IDKOLLI-TOM )                                               
077100         PERFORM EA-4316-KOLLI                                            
077200         ADD +1              TO WS-IDKOLLI                                
077300       END-PERFORM                                                        
077400     END-IF                                                               
077500     IF MID-RADER NOT = ALL '+'                                           
077600       MOVE +1          TO RADIND                                         
077700                                                                          
077800       PERFORM UNTIL                                                      
077900        NOT ( RADIND < MAX-RADIND-PLUS-1 )                                
078000         IF MID-RAD (RADIND) NOT = ALL '+'                                
078100           MOVE MID-IDKOLLI (RADIND) TO WS-IDKOLLI                        
078200           MOVE MID-KVLEVART-DELAT2 (RADIND) TO WS-KVLEVART               
078300           PERFORM EA-4316-KOLLI                                          
078400         END-IF                                                           
078500         ADD +1       TO RADIND                                           
078600       END-PERFORM                                                        
078700     END-IF                                                               
078800     CONTINUE.                                                            
078900     EJECT                                                                
079000 EA-4316-KOLLI SECTION.                                                   
079100                                                                          
079200     MOVE '002'                TO W-IDPTYP-4316                           
079300     MOVE WS-IDKOLLI           TO W-IDKOLLI-4316                          
079400     PERFORM IMS-GET-XXDL-4316-F                                          
079500     MOVE +1              TO 4316-IND                                     
079600                                                                          
079700     IF SEGMENT-FINNS                                                     
079800                                                                          
079900*****  OM SISTA RADEN I SEGMENTET INNEHÅLLER +,                           
080000*****  RÄKNAS ANTAL RADER SÅ ATT DET INTE                                 
080100*****  ÖVERSTIGER MAX-ANTAL-RADER                                         
080200                                                                          
080300       IF 4316-A-MID-RAD (MAX-4316-IND) = ALL '+'                         
080400         MOVE +0        TO WS-ANT-RAD-SEGM                                
080500                                                                          
080600         PERFORM UNTIL                                                    
080700          NOT ( ((4316-IND < MAX-4316-IND OR 4316-IND =                   
080800            MAX-4316-IND) AND (4316-A-MID-RAD (4316-IND) NOT =            
080900            ALL '+' )) )                                                  
081000           IF 4316-A-MID-IDRADNR-TOM (4316-IND) = ALL '+'                 
081100             ADD +1            TO WS-ANT-RAD-SEGM                         
081200           ELSE                                                           
081300             MOVE 4316-A-MID-IDRADNR-FOM (4316-IND) TO                    
081400                          WS-IDRADNR-FOM                                  
081500             MOVE 4316-A-MID-IDRADNR-TOM (4316-IND) TO                    
081600                          WS-IDRADNR-TOM                                  
081700             COMPUTE WS-ANT-RAD-INT =                                     
081800             WS-IDRADNR-TOM - WS-IDRADNR-FOM + 1                          
081900           END-IF                                                         
082000           ADD +1            TO 4316-IND                                  
082100         END-PERFORM                                                      
082200       END-IF                                                             
082300       IF  (4316-A-MID-RAD (MAX-4316-IND) = ALL '+')                      
082400       AND (WS-ANT-RAD-SEGM < MAX-ANT-RAD)                                
082500         PERFORM EAB-KOLLI-002                                            
082600         PERFORM IMS-REPL-XXDL                                            
082700                                                                          
082800       ELSE                                                               
082900         MOVE '003'                TO W-IDPTYP-4316                       
083000         PERFORM IMS-GET-XXDL-4316-L                                      
083100                                                                          
083200         IF SEGMENT-FINNS                                                 
083300                                                                          
083400*****  OM SISTA RADEN I SEGMENTET INNEHÅLLER +,                           
083500*****  RÄKNAS ANTAL RADER SÅ ATT DET INTE                                 
083600*****  ÖVERSTIGER MAX-ANTAL-RADER                                         
083700                                                                          
083800           IF 4316-B-MID-RAD (MAX-4316-IND) = ALL '+'                     
083900             MOVE +0        TO WS-ANT-RAD-SEGM                            
084000             MOVE +1        TO 4316-IND                                   
084100                                                                          
084200             PERFORM UNTIL                                                
084300              NOT ( ((4316-IND < MAX-4316-IND OR 4316-IND =               
084400                MAX-4316-IND) AND (4316-B-MID-RAD (4316-IND) NOT          
084500               =                                                          
084600                ALL '+' )) )                                              
084700               IF 4316-B-MID-IDRADNR-TOM (4316-IND) = ALL '+'             
084800                 ADD +1            TO WS-ANT-RAD-SEGM                     
084900               ELSE                                                       
085000                 MOVE 4316-B-MID-IDRADNR-FOM (4316-IND)                   
085100                           TO WS-IDRADNR-FOM                              
085200                 MOVE 4316-B-MID-IDRADNR-TOM (4316-IND)                   
085300                           TO WS-IDRADNR-TOM                              
085400                 COMPUTE WS-ANT-RAD-INT =                                 
085500                 WS-IDRADNR-TOM - WS-IDRADNR-FOM + 1                      
085600               END-IF                                                     
085700               ADD +1            TO 4316-IND                              
085800             END-PERFORM                                                  
085900           END-IF                                                         
086000           IF  (4316-B-MID-RAD (MAX-4316-IND) = ALL '+')                  
086100           AND (WS-ANT-RAD-SEGM < MAX-ANT-RAD)                            
086200             PERFORM EAD-KOLLI-003                                        
086300             PERFORM IMS-REPL-XXDL                                        
086400           ELSE                                                           
086500             MOVE +1        TO 4316-IND                                   
086600             MOVE LOW-VALUE  TO IO-AREA                                   
086700             PERFORM EAC-4316-003                                         
086800             PERFORM EAD-KOLLI-003                                        
086900             PERFORM IMS-ISRT-XXDL-4316-STAT-II                           
087000           END-IF                                                         
087100         ELSE                                                             
087200           MOVE +1        TO 4316-IND                                     
087300           MOVE LOW-VALUE  TO IO-AREA                                     
087400           PERFORM EAC-4316-003                                           
087500           PERFORM EAD-KOLLI-003                                          
087600           PERFORM IMS-ISRT-XXDL-4316-STAT-II                             
087700         END-IF                                                           
087800       END-IF                                                             
087900     ELSE                                                                 
088000       MOVE LOW-VALUE  TO IO-AREA                                         
088100       PERFORM EAA-4316-002                                               
088200       PERFORM EAB-KOLLI-002                                              
088300       PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                              
088400     END-IF                                                               
088500     CONTINUE.                                                            
088600     EJECT                                                                
088700 EAA-4316-002 SECTION.                                                    
088800                                                                          
088900     MOVE ALL '+'                 TO 4316-WDGX4316                        
089000     MOVE MID-IDPRODNR-UT         TO 4316-IDPRODNR                        
089100     MOVE '002'                   TO 4316-IDPTYP                          
089200     MOVE LOW-VALUE               TO 4316-LOWVALUE                        
089300     MOVE ZERO                    TO 4316-KDTRSTAT                        
089400     MOVE +312                    TO 4316-LL                              
089500     MOVE LOW-VALUE               TO 4316-Z1                              
089600     MOVE LOW-VALUE               TO 4316-Z2                              
089700     MOVE 'W4T315'                TO 4316-KDTRANS                         
089800     MOVE '4395'                  TO 4316-IDTRANS                         
089900     MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                        
090000                                                                          
090100     MOVE ZERO                 TO 4316-A-MID-IDANSTNR-UT                  
090200                                                                          
090300     MOVE MID-IDDISTR-UT       TO 4316-A-MID-IDDISTR-IN                   
090400                                  4316-A-MID-IDDISTR-UT                   
090401                                                                          
090410     MOVE MID-IDDC-UT          TO 4316-A-MID-IDDC-IN                      
090420                                  4316-A-MID-IDDC-UT                      
090500                                                                          
090600     MOVE MID-IDKUNDNR-UT      TO 4316-A-MID-IDKUNDNR-IN                  
090700                                  4316-A-MID-IDKUNDNR-UT                  
090800                                                                          
090900     MOVE MID-IDORDNR-UT       TO 4316-A-MID-IDORDNR-IN                   
091000                                  4316-A-MID-IDORDNR-UT                   
091100                                                                          
091200     MOVE WS-IDKOLLI           TO 4316-IDKOLLI                            
091300                                  4316-A-MID-IDKOLLI-IN                   
091400                                  4316-A-MID-IDKOLLI-UT                   
091500                                                                          
091600     MOVE MID-IDPRODNR-UT      TO 4316-A-MID-IDPRODNR-IN                  
091700                                  4316-A-MID-IDPRODNR-UT                  
091701     MOVE 'U'                  TO 4316-A-MID-PRTVAL-FOLJEFL               
091710     MOVE WS-PRTVAL            TO 4316-A-MID-PRTVAL-ADRESSFL              
091800     CONTINUE.                                                            
091900                                                                          
092000                                                                          
092100     EJECT                                                                
092200 EAB-KOLLI-002 SECTION.                                                   
092300                                                                          
092400     MOVE MID-IDRADNR TO                                                  
092500                      4316-A-MID-IDRADNR-FOM (4316-IND)                   
092600                                                                          
092700     MOVE WS-KVLEVART TO                                                  
092800                      4316-A-MID-KVLEVART (4316-IND)                      
092900     CONTINUE.                                                            
093000                                                                          
093100     EJECT                                                                
093200 EAC-4316-003    SECTION.                                                 
093300                                                                          
093400     MOVE ALL '+'                 TO 4316-WDGX4316                        
093500     MOVE MID-IDPRODNR-UT         TO 4316-IDPRODNR                        
093600     MOVE '003'                   TO 4316-IDPTYP                          
093700     MOVE LOW-VALUE               TO 4316-LOWVALUE                        
093800     MOVE ZERO                    TO 4316-KDTRSTAT                        
093900     MOVE +277                    TO 4316-LL                              
094000     MOVE LOW-VALUE               TO 4316-Z1                              
094100     MOVE LOW-VALUE               TO 4316-Z2                              
094200     MOVE 'W4T314'                TO 4316-KDTRANS                         
094300     MOVE '4395'                  TO 4316-IDTRANS                         
094400     MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                        
094500                                                                          
094600     MOVE ZERO                    TO 4316-B-MID-IDANSTNR-UT               
094700                                                                          
094800     MOVE MID-IDDISTR-UT          TO 4316-B-MID-IDDISTR-IN                
094900                                     4316-B-MID-IDDISTR-UT                
095000                                                                          
095010     MOVE MID-IDDC-UT             TO 4316-B-MID-IDDC-IN                   
095020                                     4316-B-MID-IDDC-UT                   
095030                                                                          
095100     MOVE MID-IDKUNDNR-UT         TO 4316-B-MID-IDKUNDNR-IN               
095200                                     4316-B-MID-IDKUNDNR-UT               
095300                                                                          
095400     MOVE MID-IDORDNR-UT          TO 4316-B-MID-IDORDNR-IN                
095500                                     4316-B-MID-IDORDNR-UT                
095600                                                                          
095700     MOVE WS-IDKOLLI              TO 4316-IDKOLLI                         
095800                                     4316-B-MID-IDKOLLI-IN                
095900                                     4316-B-MID-IDKOLLI-UT                
096000                                                                          
096100     MOVE MID-IDPRODNR-UT         TO 4316-B-MID-IDPRODNR-IN               
096200                                     4316-B-MID-IDPRODNR-UT               
096300     MOVE 'J'                     TO 4316-B-MID-FLFORTSK                  
096400     MOVE 0                       TO 4316-B-MID-IDRADNR-FOM-S             
096500     MOVE 0                       TO 4316-B-MID-IDRADNR-TOM-S             
096600     MOVE 0                       TO 4316-B-MID-KVLEVART-S                
096601     MOVE 'UU'                    TO 4316-B-MID-KDPRTVAL-FOLJEFL          
096610     MOVE WS-PRTVAL               TO 4316-B-MID-KDPRTVAL-ADRESSFL         
096700     CONTINUE.                                                            
096800                                                                          
096900                                                                          
097000     EJECT                                                                
097100 EAD-KOLLI-003 SECTION.                                                   
097200                                                                          
097300     MOVE MID-IDRADNR TO                                                  
097400                      4316-B-MID-IDRADNR-FOM (4316-IND)                   
097500                                                                          
097600     MOVE WS-KVLEVART TO                                                  
097700                      4316-B-MID-KVLEVART (4316-IND)                      
097800     CONTINUE.                                                            
097900                                                                          
098000     EJECT                                                                
098100 F-KOLLA-4314 SECTION.                                                    
098200                                                                          
098300     IF WS-FLRAD-KLAR                                                     
098400       PERFORM IMS-GET-XXDK-4314-STAT-BLANK                               
098500       MOVE +2         TO 4314-KDBEHAND-RAD                               
098600       PERFORM IMS-REPL-XXDK                                              
098700                                                                          
098800       IF WS-FLMAN-RAPP = JA                                              
098900         MOVE +1           TO W-KDBEHAND-RAD-4314                         
099000         PERFORM IMS-GET-XXDK-4314-KDBEHAND-F                             
099100         IF SEGMENT-SAKNAS                                                
099200           MOVE JA         TO WS-FLPRODNR-KLAR-TEST                       
099300         END-IF                                                           
099400       ELSE                                                               
099500         PERFORM IMS-GET-XXDK-4314-OKSSA                                  
099600         IF SEGMENT-FINNS                                                 
099700           MOVE 4314-IDRADNR    TO WS-IDRADNR-NUM                         
099800           MOVE WS-IDRADNR-NUM  TO MOD-IDRADNR                            
099900           MOVE 4314-KVLEVART   TO MOD-KVLEVART                           
100000         ELSE                                                             
100100           MOVE JA         TO WS-FLPRODNR-KLAR-TEST                       
100200         END-IF                                                           
100300       END-IF                                                             
100400     END-IF                                                               
100500     CONTINUE.                                                            
100600     EJECT                                                                
100700 S01-ROER-EJ-FAELT SECTION.                                               
100800                                                                          
100900     IF  MID-SUM NOT = ALL '+'                                            
101000       MOVE MFS-ROER-EJ-FAELT TO  MOD-SUM                                 
101100     END-IF                                                               
101200     IF  MID-IDRADNR-SENAST NOT = ALL '+'                                 
101300       MOVE MFS-ROER-EJ-FAELT TO  MOD-IDRADNR-SENAST                      
101400     END-IF                                                               
101500     IF  MID-IDRADNR NOT = ALL '+'                                        
101600       MOVE MFS-ROER-EJ-FAELT TO  MOD-IDRADNR                             
101700     END-IF                                                               
101800     IF  MID-KVLEVART NOT = ALL '+'                                       
101900       MOVE MFS-ROER-EJ-FAELT TO  MOD-KVLEVART                            
102000     END-IF                                                               
102100     IF  MID-KVLEVART-DELAT1 NOT = ALL '+'                                
102200       MOVE MFS-ROER-EJ-FAELT TO  MOD-KVLEVART-DELAT1                     
102300     END-IF                                                               
102400     IF  MID-IDKOLLI-FOM NOT = ALL '+'                                    
102500       MOVE MFS-ROER-EJ-FAELT TO  MOD-IDKOLLI-FOM                         
102600     END-IF                                                               
102700     IF  MID-IDKOLLI-TOM NOT = ALL '+'                                    
102800       MOVE MFS-ROER-EJ-FAELT TO  MOD-IDKOLLI-TOM                         
102900     END-IF                                                               
103000     MOVE +1                TO RADIND                                     
103100                                                                          
103200     PERFORM UNTIL                                                        
103300      NOT ( RADIND < MAX-RADIND-PLUS-1 )                                  
103400       IF  MID-KVLEVART-DELAT2 (RADIND) NOT = ALL '+'                     
103500         MOVE MFS-ROER-EJ-FAELT TO                                        
103600                       MOD-KVLEVART-DELAT2 (RADIND)                       
103700       END-IF                                                             
103800       IF  MID-IDKOLLI (RADIND) NOT = ALL '+'                             
103900         MOVE MFS-ROER-EJ-FAELT TO  MOD-IDKOLLI (RADIND)                  
104000       END-IF                                                             
104100       ADD +1                 TO RADIND                                   
104200     END-PERFORM                                                          
104300     CONTINUE.                                                            
104400     EJECT                                                                
104500 S02-FORMATETS-ATTR SECTION.                                              
104600                                                                          
104700                                                                          
104800     MOVE MFS-FORMATETS-ATTR TO MOD-KVLEVART-DELAT1-ATTR                  
104900     MOVE MFS-FORMATETS-ATTR TO MOD-IDKOLLI-FOM-ATTR                      
105000     MOVE MFS-FORMATETS-ATTR TO MOD-IDKOLLI-TOM-ATTR                      
105100                                                                          
105200     MOVE +1                TO RADIND                                     
105300                                                                          
105400     PERFORM UNTIL                                                        
105500      NOT ( RADIND < MAX-RADIND-PLUS-1 )                                  
105600       MOVE MFS-FORMATETS-ATTR TO                                         
105700                          MOD-KVLEVART-DELAT2-ATTR (RADIND)               
105800                          MOD-IDKOLLI-ATTR         (RADIND)               
105900                                                                          
106000       ADD +1                 TO RADIND                                   
106100     END-PERFORM                                                          
106200     CONTINUE.                                                            
106300     EJECT                                                                
106400 S03-STAENG-IDRADNR SECTION.                                              
106500                                                                          
106600     MOVE MFS-STAENG-FAELT  TO                                            
106700                     MOD-IDRADNR-ATTR                                     
106800     CONTINUE.                                                            
106900                                                                          
107000     EJECT                                                                
107100 S04-OEPPNA-IDRADNR SECTION.                                              
107200                                                                          
107300     MOVE MFS-OEPPNA-NUM-FAELT TO                                         
107400                     MOD-IDRADNR-ATTR                                     
107500     CONTINUE.                                                            
107600                                                                          
107700     EJECT                                                                
107800 S99-NAESTA-TRANS SECTION.                                                
107900                                                                          
108000     IF WS-FLPRODNR-KLAR                                                  
108100       PERFORM IMS-GET-XXDK-4312-F                                        
108200                                                                          
108300       MOVE +2 TO 4312-KDBEHAND-DEL                                       
108400       PERFORM IMS-REPL-XXDK                                              
108500       MOVE MID-IDPRODNR-UT    TO PTOP2-IDPRODNR-IN                       
108600                                  PTOP2-IDPRODNR-UT                       
108700       MOVE MID-IDDISTR-UT     TO PTOP2-IDDISTR-UT                        
108800       MOVE MID-IDKUNDNR-UT    TO PTOP2-IDKUNDNR-UT                       
108900       MOVE MID-KDFRAKT-UT     TO PTOP2-KDFRAKT-UT                        
109000       MOVE MID-IDORDNR-UT     TO PTOP2-IDORDNR-UT                        
109100       MOVE MID-KDORDKL-UT     TO PTOP2-KDORDKL-UT                        
109110       MOVE MID-IDDC-UT        TO PTOP2-IDDC-UT                           
109120       MOVE WS-PRTVAL          TO PTOP2-PRTVAL-ADRESSFL                   
109200       MOVE MFS-KDMFSFOR       TO PTOP2-KDMFSFOR                          
109300       PERFORM IMS-INSERT-MSG-ALT2-PCB                                    
109400       MOVE 'J' TO KOLLI-SW                                               
109500     ELSE                                                                 
109600       IF WS-FLRAD-KLAR                                                   
109700         MOVE JA       TO MOD-FLSIDA1                                     
109800         MOVE MID-IDRADNR TO MOD-IDRADNR-SENAST                           
109900                                                                          
110000         IF WS-FLMAN-RAPP = JA                                            
110100           PERFORM S04-OEPPNA-IDRADNR                                     
110200         END-IF                                                           
110300         MOVE MFS-RENSA-FAELT   TO MOD-SUM                                
110400       ELSE                                                               
110500         MOVE NEJ      TO MOD-FLSIDA1                                     
110600         PERFORM S03-STAENG-IDRADNR                                       
110700         MOVE WS-KVLEVART-TOT   TO MOD-SUM                                
110800         MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR                            
110900                                   MOD-KVLEVART                           
111000                                   MOD-IDRADNR-SENAST                     
111100       END-IF                                                             
111200       MOVE WS-FLMAN-RAPP TO MOD-FLMAN-RAPP                               
111300       PERFORM S02-FORMATETS-ATTR                                         
111400                                                                          
111500       IF MOD-FLSIDA1 = NEJ                                               
111600       OR WS-FLMAN-RAPP = NEJ                                             
111700         MOVE MFS-ADD-SAETT-CURSOR TO                                     
111800                  MOD-KVLEVART-DELAT1-ATTR                                
111900       END-IF                                                             
112000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
112100     END-IF                                                               
112200     CONTINUE.                                                            
112300     EJECT                                                                
112400* IMS SEKTIONER                                                           
112500     SKIP3                                                                
112600 IMS-GET-MSG SECTION.                                                     
112700     MOVE '  QC' TO GODK-STATUSKODER                                      
112800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
112900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
113000     PERFORM IMS-STATUSKONTROLL                                           
113100     CONTINUE.                                                            
113200     SKIP3                                                                
113300 IMS-INSERT-MSG SECTION.                                                  
113400     IF ENGLISH-TEXT                                                      
113500       MOVE NEJ TO MFS-KDHUVOMR                                           
113600     END-IF                                                               
113700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
113800     MOVE SPACE TO GODK-STATUSKODER                                       
113900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
114000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114100     PERFORM IMS-STATUSKONTROLL                                           
114200     CONTINUE.                                                            
114300     EJECT                                                                
114400 IMS-INSERT-MSG-ALT2-PCB SECTION.                                         
114500     MOVE SPACE TO GODK-STATUSKODER                                       
114600     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
114700     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
114800     PERFORM IMS-STATUSKONTROLL                                           
114900     CONTINUE.                                                            
115000     EJECT                                                                
115100 IMS-GET-XXDK-4311 SECTION.                                               
115200     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
115300            DELIMITED BY SIZE INTO SSA1                                   
115400     MOVE '  '     TO GODK-STATUSKODER                                    
115500     CALL CBLTDLI USING GU XXDK-PCB DLI-IO-AREA SSA1                      
115600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
115700     PERFORM IMS-STATUSKONTROLL                                           
115800     CONTINUE.                                                            
115900     SKIP3                                                                
116000 IMS-GET-XXDK-4312-F SECTION.                                             
116100     STRING 'WLXXDK11*F(WDGXKEY  =' W-WDGXKEY-4312-X ')'                  
116200            DELIMITED BY SIZE INTO SSA1                                   
116300     MOVE '  '     TO GODK-STATUSKODER                                    
116400     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA SSA1                    
116500     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
116600     PERFORM IMS-STATUSKONTROLL                                           
116700     CONTINUE.                                                            
116800     SKIP3                                                                
116900 IMS-GET-XXDK-4314-STAT-GE SECTION.                                       
117000     STRING 'WLXXDK11*F(WDGXKEY  =' W-WDGXKEY-4312-X ')'                  
117100            DELIMITED BY SIZE INTO SSA1                                   
117200     STRING 'WLXXDK21(WDGXKEY  =' W-WDGXKEY-4314-X ')'                    
117300            DELIMITED BY SIZE INTO SSA2                                   
117400     MOVE '  GE'   TO GODK-STATUSKODER                                    
117500     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA SSA1 SSA2                
117600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
117700     PERFORM IMS-STATUSKONTROLL                                           
117800     CONTINUE.                                                            
117900     SKIP3                                                                
118000 IMS-GET-XXDK-4314-STAT-BLANK SECTION.                                    
118100     STRING 'WLXXDK11*F(WDGXKEY  =' W-WDGXKEY-4312-X ')'                  
118200            DELIMITED BY SIZE INTO SSA1                                   
118300     STRING 'WLXXDK21(WDGXKEY  =' W-WDGXKEY-4314-X ')'                    
118400            DELIMITED BY SIZE INTO SSA2                                   
118500     MOVE '  '     TO GODK-STATUSKODER                                    
118600     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA SSA1 SSA2               
118700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
118800     PERFORM IMS-STATUSKONTROLL                                           
118900     CONTINUE.                                                            
119000     SKIP3                                                                
119100 IMS-GET-XXDK-4314 SECTION.                                               
119200     STRING 'WLXXDK11(WDGXKEY  =' W-WDGXKEY-4312-X ')'                    
119300            DELIMITED BY SIZE INTO SSA1                                   
119400     MOVE 'WLXXDK21 ' TO SSA2                                             
119500     MOVE '  '     TO GODK-STATUSKODER                                    
119600     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA SSA1 SSA2                
119700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
119800     PERFORM IMS-STATUSKONTROLL                                           
119900     CONTINUE.                                                            
120000     SKIP3                                                                
120100 IMS-GET-XXDK-4314-KDBEHAND-F SECTION.                                    
120200     STRING 'WLXXDK11(WDGXKEY  =' W-WDGXKEY-4312-X ')'                    
120300            DELIMITED BY SIZE INTO SSA1                                   
120400     STRING 'WLXXDK21*F(KDBEHAND =' W-KDBEHAND-4314-X ')'                 
120500            DELIMITED BY SIZE INTO SSA2                                   
120600     MOVE '  GE'   TO GODK-STATUSKODER                                    
120700     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA SSA1 SSA2                
120800     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
120900     PERFORM IMS-STATUSKONTROLL                                           
121000     CONTINUE.                                                            
121100     SKIP3                                                                
121200 IMS-GET-XXDK-4314-OKSSA SECTION.                                         
121300     STRING 'WLXXDK11(WDGXKEY  =' W-WDGXKEY-4312-X ')'                    
121400            DELIMITED BY SIZE INTO SSA1                                   
121500     MOVE 'WLXXDK21 ' TO SSA2                                             
121600     MOVE '  GE'   TO GODK-STATUSKODER                                    
121700     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA SSA1 SSA2                
121800     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
121900     PERFORM IMS-STATUSKONTROLL                                           
122000     CONTINUE.                                                            
122100     SKIP3                                                                
122200 IMS-REPL-XXDK SECTION.                                                   
122300     MOVE '  '   TO GODK-STATUSKODER                                      
122400     CALL CBLTDLI USING REPL XXDK-PCB DLI-IO-AREA                         
122500     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
122600     PERFORM IMS-STATUSKONTROLL                                           
122700     CONTINUE.                                                            
122800                                                                          
122900     EJECT                                                                
123000 IMS-GET-XXDL-4315 SECTION.                                               
123100     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
123200            DELIMITED BY SIZE INTO SSA1                                   
123300     MOVE '  '     TO GODK-STATUSKODER                                    
123400     CALL CBLTDLI USING GU XXDL-PCB DLI-IO-AREA SSA1                      
123500     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
123600     PERFORM IMS-STATUSKONTROLL                                           
123700     CONTINUE.                                                            
123800     SKIP3                                                                
123900 IMS-GET-XXDL-4316-F SECTION.                                             
124000     STRING 'WLXXDL11*F(WDGXKEY  =' W-WDGXKEY-4316-X ')'                  
124100            DELIMITED BY SIZE INTO SSA1                                   
124200     MOVE '  GE'     TO GODK-STATUSKODER                                  
124300     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA SSA1                    
124400     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
124500     PERFORM IMS-STATUSKONTROLL                                           
124600     CONTINUE.                                                            
124700     SKIP3                                                                
124800 IMS-GET-XXDL-4316-L SECTION.                                             
124900     STRING 'WLXXDL11*L(WDGXKEY  =' W-WDGXKEY-4316-X ')'                  
125000            DELIMITED BY SIZE INTO SSA1                                   
125100     MOVE '  GE'     TO GODK-STATUSKODER                                  
125200     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA SSA1                    
125300     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
125400     PERFORM IMS-STATUSKONTROLL                                           
125500     CONTINUE.                                                            
125600     SKIP3                                                                
125700 IMS-ISRT-XXDL-4316-STAT-BLANK SECTION.                                   
125800     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
125900            DELIMITED BY SIZE INTO SSA1                                   
126000     MOVE 'WLXXDL11 '  TO SSA2                                            
126100     MOVE '  '       TO GODK-STATUSKODER                                  
126200     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA SSA1 SSA2               
126300     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
126400     PERFORM IMS-STATUSKONTROLL                                           
126500     CONTINUE.                                                            
126600                                                                          
126700                                                                          
126800                                                                          
126900 IMS-ISRT-XXDL-4316-STAT-II SECTION.                                      
127000     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
127100            DELIMITED BY SIZE INTO SSA1                                   
127200     MOVE 'WLXXDL11 '  TO SSA2                                            
127300     MOVE '  II'     TO GODK-STATUSKODER                                  
127400     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA SSA1 SSA2               
127500     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     CONTINUE.                                                            
127800     SKIP3                                                                
127900 IMS-REPL-XXDL SECTION.                                                   
128000     MOVE '  '   TO GODK-STATUSKODER                                      
128100     CALL CBLTDLI USING REPL XXDL-PCB DLI-IO-AREA                         
128200     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
128300     PERFORM IMS-STATUSKONTROLL                                           
128400     CONTINUE.                                                            
128500     EJECT                                                                
128600 IMS-STATUSKONTROLL SECTION.                                              
128700     SET STATUS-IX TO 1                                                   
128800     SEARCH GODK-STATUS                                                   
128810       AT END                                                             
128820         CALL FELLOG                                                      
128900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
129000     END-SEARCH                                                           
129400     CONTINUE.                                                            
