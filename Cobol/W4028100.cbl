000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4028100.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   MARS-91.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        BILD 4281 - FRÅGA PÅ ORDERBEKRÄFTELSE DISTRIKT.                  
001100*        PROGRAMMET VISAR ALLA ORDERBEKRÄFTELSERADER FÖR                  
001200*        ANGIVNA NYCKLAR.                                                 
001300*                                                                         
001400*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001500*        PROGRAMMET LÄSER      WLORQM (WDQ1)                              
001600*                              WLORQI (WDQ2)                              
001700*                              WLPROC (WDE8)                              
001710*                                      WDB6                               
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T281                                              
002100*        MID:         W4I28101                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O28101                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4028100'.            
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003700 77  MOD-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-MOD-INDX                PIC S9(4)  VALUE +13   COMP SYNC.        
003900 77  HELP-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004000                                                                          
004100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  CLAGER-INDX                 PIC S9(1)  VALUE +0    COMP-3.           
004300 77  CLAGER-INDX-SPAR            PIC S9(1)  VALUE +0    COMP-3.           
004400 77  MAX-CLAGER-INDX             PIC S9(1)  VALUE +2    COMP-3.           
004500                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
004800 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005000 77  WS-KDFRAKT                  PIC X(2)    VALUE SPACE.                 
005100 77  WS-KDORDKL                  PIC X(1)    VALUE SPACE.                 
005200 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
005300 77  WS-TIREGDAT-NUM             PIC 9(6)    VALUE ZERO.                  
005400 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
005500 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
005600 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 77  WS-KDFRAKT-NUM              PIC 9(2)    VALUE ZERO.                  
005800 77  WS-KDORDKL-NUM              PIC 9(1)    VALUE ZERO.                  
005900 77  WS-ANT-CMD                  PIC S9(3) COMP-3 VALUE ZERO.             
006000 77  OHUV-KDORDKL-SPAR           PIC S9      VALUE +0 COMP-3.             
006100 77  WDQ2-STATUS-WS              PIC X(2)    VALUE SPACE.                 
006101 01  W-SPAR-IDKUNDRF.                                                     
006102     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
006103     03  FILLER                  PIC X(3)    VALUE '+++'.                 
006200                                                                          
006400********************************************************                  
006500                                                                          
006600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006700     88  NYCKLAR-OK                          VALUE 'J'.                   
006800     88  NYCKLAR-FEL                         VALUE 'N'.                   
006900                                                                          
007000 77  PROFORMA-SW                 PIC X       VALUE 'N'.                   
007100     88  PROFORMA                            VALUE 'J' 'Y'.               
007200                                                                          
007300 77  FIRST-TIME-WDQ1-SW          PIC X       VALUE 'J'.                   
007400     88  FIRST-TIME-WDQ1                     VALUE 'J'.                   
007500                                                                          
007600 77  FIRST-TIME-WDQ2-SW          PIC X       VALUE 'J'.                   
007700     88  FIRST-TIME-WDQ2                     VALUE 'J'.                   
007800                                                                          
007900 77  KDFRAKT-SW                  PIC X       VALUE 'N'.                   
008000     88  KDFRAKT-IFYLLD                      VALUE 'J'.                   
008100                                                                          
008200 77  KDORDKL-SW                  PIC X       VALUE 'N'.                   
008300     88  KDORDKL-IFYLLD                      VALUE 'J'.                   
008400                                                                          
008500 77  WDQ1-SEGMENT-SW             PIC X       VALUE 'N'.                   
008600     88  WDQ1-SEGMENT-FINNS                  VALUE 'J'.                   
008700                                                                          
008800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008900     88  ALLT-OK                             VALUE 'J'.                   
009000     88  ALLT-INTE-OK                        VALUE 'N'.                   
009100     88  PROFORMA-FEL                        VALUE 'A'.                   
009200                                                                          
009300 77  SID-SW                      PIC X       VALUE 'N'.                   
009400     88  MER-INFO-FINNS                      VALUE 'J'.                   
009500                                                                          
009600                                                                          
009700 77  BLAEDDRING-SW               PIC X       VALUE 'N'.                   
009800     88  BLAEDDRING                          VALUE 'J'.                   
009900                                                                          
010000 77  LAES-WDQ2-SW                PIC X       VALUE 'J'.                   
010100     88  LAES-EJ-WDQ2                        VALUE 'N'.                   
010200                                                                          
010300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010400     88  EGEN-MID                            VALUE '4281'.                
010500     88  GODK-MID                            VALUE '4281' '4282'          
010600                                                   '4283' '4284'.         
010700     EJECT                                                                
010800                                                                          
010900 01  WS-IDKUNDRF                 PIC X(10).                               
011000                                                                          
011100                                                                          
011200 01  WS-IDKUNDRF7-X              PIC X(7).                                
011300 01  WS-IDORDNR7-X               PIC X(7).                                
011400                                                                          
011440       EJECT                                                              
011500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011600 01  GENERELLA-SUBPROGRAM.                                                
011700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
012100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012400*01 -COPY WMSGINIT                                                        
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
012700*   -COPY W006PRT                                                         
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013000*   -COPY WMEDAREA                                                        
013100     SKIP3                                                                
013200 01  MESSAGE-CODES.                                                       
013300     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
013400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013500     03  INF-FYLL-I-CMD          PIC X(3)    VALUE '048'.                 
013600     03  INF-ORDBEK-SAKNAS       PIC X(3)    VALUE '058'.                 
013700     03  ERR-TOM-RAD             PIC X(3)    VALUE '080'.                 
013800     03  ERR-TRYCK-PF4           PIC X(3)    VALUE '081'.                 
013900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014100     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
014200     03  INF-UTSKRIVEN           PIC X(3)    VALUE '?!!'.                 
014300     EJECT                                                                
014400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014700     SKIP3                                                                
014800*01  MID -COPY W4I28101    -PRE MID-                                      
014900     EJECT                                                                
015000                                                                          
015100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015200     SKIP3                                                                
015300*01  -COPY WMSGAREA                                                       
015400     EJECT                                                                
015500                                                                          
015600     03  MOD REDEFINES MSG-AREA.                                          
015700*      05  -COPY W4O28101    -PRE MOD-                                    
015800     EJECT                                                                
015900                                                                          
016000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016100     SKIP3                                                                
016200*01  -COPY WMFSAREA                                                       
016300     EJECT                                                                
016400                                                                          
016500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800     SKIP3                                                                
016900 01  NYCKLAR-TILL-DLI.                                                    
017000******* NYCKEL FÖR ATT LÄSA WDQ1 *************************                
017100                                                                          
017200     03  W-WDQ101KY-MIN-X.                                                
017300       05    W-IDORDER-MIN       PIC S9(7)    VALUE ZERO COMP-3.          
017400       05    W-IDARTNR-MIN       PIC S9(9)    VALUE ZERO COMP-3.          
017500       05    W-IDLOPNR-MIN       PIC S9(3)    VALUE ZERO COMP-3.          
017600       05    W-IDSEKVNR-MIN      PIC S9(3)    VALUE ZERO COMP-3.          
017700       05    W-IDDC-MIN          PIC  X(2)    VALUE SPACE.                
017800       05    W-KDORDBEK-MIN      PIC 9(2)     VALUE ZERO.                 
017900     SKIP3                                                                
018000                                                                          
018100     03  W-WDQ101KY-MAX-X.                                                
018200       05    W-IDORDER-MAX       PIC S9(7)    VALUE ZERO COMP-3.          
018300       05    W-IDARTNR-MAX       PIC S9(9)    VALUE ZERO COMP-3.          
018400       05    W-IDLOPNR-MAX       PIC S9(3)    VALUE ZERO COMP-3.          
018500       05    W-IDSEKVNR-MAX      PIC S9(3)    VALUE ZERO COMP-3.          
018600       05    W-IDDC-MAX          PIC  X(2)    VALUE SPACE.                
018700       05    W-KDORDBEK-MAX      PIC 9(2)     VALUE ZERO.                 
018800     SKIP3                                                                
018900******* NYCKEL FÖR ATT LÄSA WDQ201 VIA WDQ2C1 ************                
019000                                                                          
019100     03  W-IDGMTREF-MIN-X.                                                
019200         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO COMP-3.          
019300         05  W-IDKUNDNR-MIN      PIC S9(7)    VALUE ZERO COMP-3.          
019400         05  W-IDKUNDRF-MIN      PIC X(10)    VALUE SPACE.                
019500         05  W-IDORDNR7-FILLER-MIN REDEFINES W-IDKUNDRF-MIN.              
019600             07  W-IDORDNR7-MIN  PIC 9(7).                                
019700             07  FILLER          PIC X(3).                                
019800                                                                          
019900     03  W-IDGMTREF-MAX-X.                                                
020000         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO COMP-3.          
020100         05  W-IDKUNDNR-MAX      PIC S9(7)    VALUE ZERO COMP-3.          
020200         05  W-IDKUNDRF-MAX      PIC X(10)    VALUE SPACE.                
020300         05  W-IDORDNR7-FILLER-MAX REDEFINES W-IDKUNDRF-MAX.              
020400             07  W-IDORDNR7-MAX  PIC 9(7).                                
020500             07  FILLER          PIC X(3).                                
020600                                                                          
020700******* NYCKEL FÖR ATT LÄSA WDE801 ***********************                
020800                                                                          
020900     03  W-WDE801KY-MIN-X.                                                
021000         05  W-E8-IDDISTR-MIN      PIC S9(5)  COMP-3.                     
021100         05  W-E8-IDKUNDNR-MIN     PIC S9(7)  COMP-3.                     
021200         05  W-E8-IDKUNDRF-MIN     PIC  X(10) VALUE SPACE.                
021300                                                                          
021400     03  W-WDE801KY-MAX-X.                                                
021500         05  W-E8-IDDISTR-MAX      PIC S9(5)  COMP-3.                     
021600         05  W-E8-IDKUNDNR-MAX     PIC S9(7)  COMP-3.                     
021700         05  W-E8-IDKUNDRF-MAX     PIC  X(10) VALUE SPACE.                
021800                                                                          
021900**********************************************************                
022000                                                                          
022100     03  W-IDSYSTEM-X.                                                    
022200         05 W-IDSYSTEM     PIC  X(4)      VALUE 'PROF'.                   
022300                                                                          
022400     03  W-TIREGDAT-X.                                                    
022500         05 W-TIREGDAT     PIC S9(7)      VALUE ZERO COMP-3.              
022600                                                                          
022700     03  W-IDDC-X.                                                        
022800         05 W-IDDC         PIC  X(2)      VALUE SPACE.                    
022900                                                                          
023000                                                                          
023100******* DIREKTNYCKEL TILL XXKL PRINTERTABELL    **********                
023200                                                                          
023210     03  W-IDDC-B6-X.                                                     
023220         05 W-IDDC-B6                  PIC X(2).                          
023300                                                                          
023400*    --- STATUS-KOD FRÅN IMS                                              
023500 01  STATUS-WS                   PIC XX.                                  
023600     88  SEGMENT-FINNS                       VALUE '  '.                  
023700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023800     88  BASEN-SLUT                          VALUE 'GB'.                  
023900     SKIP2                                                                
024000                                                                          
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400                                                                          
024500 01  SSA1                        PIC X(130).                              
024600 01  SSA2                        PIC X(64).                               
024700     EJECT                                                                
024800                                                                          
024900*    --- IMS FUNKTIONSKODER                                               
025000*01  -COPY W0003                                                          
025100     EJECT                                                                
025200                                                                          
025300*    ---  DLI INPUT-OUTPUT AREA                                           
025400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-WDQ1'.         
025500     SKIP3                                                                
025600                                                                          
025700 01  DLI-IO-AREA-WDQ1.                                                    
026000     03  WLORQM01.                                                        
026100*        05  -COPY WDQ101                                                 
026200     EJECT                                                                
026300 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-WDQ2'.         
026400     SKIP3                                                                
026500                                                                          
026600 01  DLI-IO-AREA-WDQ2.                                                    
026900     03  WLORQI01.                                                        
027000*        05  -COPY WDQ201                                                 
027100     EJECT                                                                
027200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-WDE8'.         
027300     SKIP3                                                                
027400                                                                          
027500 01  DLI-IO-AREA-WDE8.                                                    
027800     03  WLPROC01.                                                        
027900*        05  -COPY WDE801                                                 
027910                                                                          
027920 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
027930 01   DLI-IO-AREA-B601.                                                   
027940*     03  -COPY WDB601                                                    
028000     EJECT                                                                
028100*---MSG-AERA FÖR HOPP TILL 4294-UTSKRIFT ORDERBEKRÄFTELSER                
028200 01  FILLER                PIC X(16)  VALUE '4294-MSG-IO-AREA'.           
028300 01  4294-MSG-IO-AREA.                                                    
028400     03  4294-LL              PIC S9(4)  VALUE +748 COMP SYNC.            
028500     03  4294-Z1              PIC X.                                      
028600     03  4294-Z2              PIC X.                                      
028700     03  4294-TRANSKOD        PIC X(8)   VALUE 'W4T294X '.                
028800     03  4294-IDTRANS         PIC X(4)   VALUE '4281'.                    
028900     03  4294-SPRAK           PIC X.                                      
029000     03  4294-FILLER          PIC X(731).                                 
029100                                                                          
029200 LINKAGE SECTION.                                                         
029300                                                                          
029400*01  -COPY W0009      -PRE MSG-                                           
029500     SKIP3                                                                
029600*01  -COPY W0009      -PRE 4294-                                          
029700     EJECT                                                                
029800*01  -COPY W0008      -PRE USEA-                                          
029900     05  FILLER                  PIC X.                                   
030000     SKIP3                                                                
030100*01  -COPY W0008      -PRE ORQM-                                          
030200     05  FILLER                  PIC X.                                   
030300     SKIP3                                                                
030400*01  -COPY W0008      -PRE ORQI-                                          
030500     05  ORQI-IDDISTR                 PIC S9(5) COMP-3.                   
030600     05  ORQI-IDKUNDNR                PIC S9(7) COMP-3.                   
030700     05  ORQI-IDKUNDRF                PIC X(10).                          
030800     05  IDORDNR7-FILLER REDEFINES ORQI-IDKUNDRF.                         
030900         07 ORQI-IDORDNR7    PIC 9(7).                                    
031000         07 FILLER           PIC X(3).                                    
031100     EJECT                                                                
031200*01  -COPY W0008      -PRE PROC-                                          
031300     05  PROC-IDDISTR                 PIC S9(5) COMP-3.                   
031400     05  PROC-IDKUNDNR                PIC S9(7) COMP-3.                   
031500     05  PROC-IDKUNDRF                PIC X(10).                          
031600     05  IDORDNR7-PROC-FILLER REDEFINES PROC-IDKUNDRF.                    
031700         07 PROC-IDORDNR7    PIC 9(7).                                    
031800         07 FILLER           PIC X(3).                                    
031810     SKIP3                                                                
031820*01  -COPY W0008      -PRE WDB6-                                          
031830     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000 PROCEDURE DIVISION  USING MSG-PCB 4294-PCB USEA-PCB ORQM-PCB             
032100                          ORQI-PCB PROC-PCB WDB6-PCB.                     
032200     ENTRY 'DLITCBL' USING MSG-PCB 4294-PCB USEA-PCB ORQM-PCB             
032300                          ORQI-PCB PROC-PCB WDB6-PCB.                     
032400                                                                          
032500     PERFORM IMS-GET-MSG                                                  
032600     IF SEGMENT-FINNS                                                     
032700        PERFORM A-INIT                                                    
032800        PERFORM B-KOLLA-NYCKLAR                                           
032900                                                                          
033000        IF ALLT-OK                                                        
033100           IF MFS-PRINT                                                   
033200              PERFORM C-PRINT-ORDER                                       
033300           ELSE                                                           
033400              IF MFS-FIRST                                                
033500                 PERFORM D-FOERSTA-SIDA                                   
033600              ELSE                                                        
033700                 IF MFS-NEXT                                              
033800                    PERFORM E-NAESTA-SIDA                                 
033900                 ELSE                                                     
034000                    PERFORM F-SAMMA-SIDA                                  
034100                 END-IF                                                   
034200              END-IF                                                      
034300           END-IF                                                         
034400           IF ALLT-OK                                                     
034500               PERFORM G-LAES-VISA-INFO                                   
034600           END-IF                                                         
034700        END-IF                                                            
034800        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O28101 + 4                     
034900        PERFORM IMS-INSERT-MSG                                            
035000     END-IF                                                               
035100                                                                          
035200     MOVE ZERO TO RETURN-CODE                                             
035300     GOBACK                                                               
035400     .                                                                    
035500     EJECT                                                                
035600                                                                          
035700 A-INIT SECTION.                                                          
035800                                                                          
035900     ACCEPT DAGENS-DATUM FROM DATE                                        
036000                                                                          
036100     IF MSG-DUBBLA-TRANSKODER                                             
036200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I28101                 
036300       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
036400       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
036500     ELSE                                                                 
036600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I28101                  
036700       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
036800       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
036900     END-IF                                                               
037000                                                                          
037100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
037200     MOVE MSG-IDPFK            TO MFS-IDPFK                               
037300     MOVE MFS-IDTRANS          TO W-IDTRANS                               
037400                                                                          
037500     MOVE LOW-VALUE            TO MSG-AREA                                
037600     MOVE 'W4O28101'           TO MFS-IDMOD                               
037700     MOVE '4281'               TO MOD-IDTRANS                             
037800     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
037900                                                                          
038000     IF NOT EGEN-MID                                                      
038100       MOVE SPACE              TO MFS-KDTRTYP                             
038200       MOVE '7'                TO MFS-IDPFK                               
038300       MOVE MFS-RENSA-FAELT    TO MOD-PROFORMA                            
038400       MOVE NEJ                TO MID-PROFORMA                            
038500     END-IF                                                               
038600                                                                          
038700     IF ENGLISH-TEXT                                                      
038800       MOVE +2                 TO SPRAK-IX                                
038900       MOVE 'GB '              TO MED-IDSKYLT                             
039000     ELSE                                                                 
039100       MOVE +1                 TO SPRAK-IX                                
039200       MOVE 'S  '              TO MED-IDSKYLT                             
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600                                                                          
039700 B-KOLLA-NYCKLAR SECTION.                                                 
039800                                                                          
039900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
040000     MOVE '001'             TO MSGI-KDCALL                                
040100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040110     MOVE '4281'            TO MSGI-IDTRANS                               
040120     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040200     IF EGEN-MID                                                          
040300        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
040400        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
040500        MOVE MID-KDFRAKT-IN  TO MSGI-KDFRAKT                              
040501        MOVE MID-IDORDNR7-IN   TO W-SPAR-IDORDNR7                         
040510        MOVE W-SPAR-IDKUNDRF   TO MSGI-IDKUNDRF                           
040600     END-IF                                                               
040700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040800     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
040900                                                                          
041000     MOVE +1                  TO MOD-INDX                                 
041100     MOVE MFS-RENSA-FAELT     TO MOD-KDPRT                                
041200     PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                                
041300        MOVE MFS-RENSA-FAELT  TO MOD-KDCMD(MOD-INDX)                      
041400        ADD +1                TO MOD-INDX                                 
041500     END-PERFORM                                                          
041600                                                                          
041700     MOVE JA TO NYCKLAR-SW                                                
041800                                                                          
041900     MOVE LOW-VALUE            TO W-WDQ101KY-MIN-X                        
042000                                  W-IDGMTREF-MIN-X                        
042100                                  W-WDE801KY-MIN-X                        
042200                                                                          
042300     MOVE HIGH-VALUE           TO W-WDQ101KY-MAX-X                        
042400                                  W-IDGMTREF-MAX-X                        
042500                                  W-WDE801KY-MAX-X                        
042600                                                                          
042700     PERFORM BA-KOLLA-DISTRIKT                                            
042800     PERFORM BB-KOLLA-KUNDNR                                              
042900     PERFORM BC-KOLLA-DATUM                                               
043000     PERFORM BD-KOLLA-IDDC                                                
043100     PERFORM BE-KOLLA-FRAKTKOD                                            
043200     PERFORM BF-KOLLA-ORDERKLASS                                          
043300                                                                          
043400     PERFORM BG-KOLLA-IDORDER                                             
043500     PERFORM BH-KOLLA-PROFORMA                                            
043600                                                                          
043700     IF NYCKLAR-OK                                                        
043800        MOVE W-IDDISTR-MIN        TO MOD-IDDISTR-ENTER                    
043900                                   MOD-IDDISTR-NEXT                       
044000        MOVE W-IDKUNDNR-MIN       TO MOD-IDKUNDNR-ENTER                   
044100                                   MOD-IDKUNDNR-NEXT                      
044200        MOVE W-IDORDNR7-MIN       TO MOD-IDORDNR7-ENTER                   
044300                                   MOD-IDORDNR7-NEXT                      
044400        IF MFS-PRINT                                                      
044500           PERFORM BI-KOLLA-PRINTER-ID                                    
044600           IF ALLT-OK                                                     
044700              PERFORM BJ-KOLLA-RADER                                      
044800           END-IF                                                         
044900        ELSE                                                              
045000           IF MFS-ENTER                                                   
045100              IF MID-KDPRT NOT = ALL '+'                                  
045200                 MOVE ERR-TRYCK-PF4 TO MED-IDMFSFEL                       
045300                 CALL WMEDKONV USING MED-WMEDAREA                         
045400                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
045500                 MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRT                      
045600                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-ATTR              
045700                 PERFORM MFS-ROER-EJ-FAELT-UT                             
045800                 MOVE NEJ      TO ALLT-SW                                 
045900                 PERFORM BK-KOLLA-CMD                                     
046000              ELSE                                                        
046100                 PERFORM BK-KOLLA-CMD                                     
046200              END-IF                                                      
046300           END-IF                                                         
046400        END-IF                                                            
046500     ELSE                                                                 
046600        IF PROFORMA-FEL                                                   
046700           MOVE NEJ TO ALLT-SW                                            
046800           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
046900           CALL WMEDKONV USING MED-WMEDAREA                               
047000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
047100           PERFORM MFS-RENSA-FAELT-UT                                     
047200        ELSE                                                              
047300           MOVE NEJ TO ALLT-SW                                            
047400           IF MOD-IDDISTR-UT = '   0'                                     
047500              MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                          
047600              PERFORM MFS-RENSA-FAELT-UT                                  
047700           ELSE                                                           
047800              MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                       
047900              PERFORM MFS-ROER-EJ-FAELT-UT                                
048000           END-IF                                                         
048100           CALL WMEDKONV USING MED-WMEDAREA                               
048200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
048300        END-IF                                                            
048400     END-IF                                                               
048500     .                                                                    
048600     EJECT                                                                
048700                                                                          
048800 BA-KOLLA-DISTRIKT SECTION.                                               
048900                                                                          
049000     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
049100                                                                          
049200     IF MID-IDDISTR-IN         NOT = ALL '+'                              
049300       MOVE '7'                TO MFS-IDPFK                               
049400       MOVE SPACE              TO MFS-KDTRTYP                             
049500     END-IF                                                               
049600     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
049700       MOVE MSGI-IDDISTR       TO WS-IDDISTR-NUM                          
049800       MOVE WS-IDDISTR-NUM     TO W-IDDISTR-MIN                           
049900                                  W-IDDISTR-MAX                           
050000                                  W-E8-IDDISTR-MIN                        
050100                                  W-E8-IDDISTR-MAX                        
050200     ELSE                                                                 
050300       MOVE NEJ                TO NYCKLAR-SW                              
050400     END-IF                                                               
050500                                                                          
050600     IF WS-IDDISTR-NUM = ZERO                                             
050700       MOVE '   0'             TO MOD-IDDISTR-UT                          
050800     ELSE                                                                 
050900       MOVE MSGI-IDDISTR       TO MOD-IDDISTR-UT                          
051000       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
051100     END-IF                                                               
051200                                                                          
051300     .                                                                    
051400     EJECT                                                                
051500 BB-KOLLA-KUNDNR SECTION.                                                 
051600                                                                          
051700     MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR-IN                         
051800                                                                          
051900     IF MID-IDKUNDNR-IN        NOT = ALL '+'                              
052000       MOVE '7'                TO MFS-IDPFK                               
052100       MOVE SPACE              TO MFS-KDTRTYP                             
052200     END-IF                                                               
052300                                                                          
052400     IF MSGI-IDKUNDNR          =  SPACE                                   
052500        MOVE 'ALLA  '          TO WS-IDKUNDNR                             
052600     ELSE                                                                 
052700        MOVE MSGI-IDKUNDNR     TO WS-IDKUNDNR                             
052800     END-IF                                                               
052900                                                                          
053000     IF WS-IDKUNDNR NUMERIC                                               
053100        MOVE WS-IDKUNDNR       TO WS-IDKUNDNR-NUM                         
053200        MOVE WS-IDKUNDNR-NUM   TO W-IDKUNDNR-MIN                          
053300                                  W-IDKUNDNR-MAX                          
053400                                  W-E8-IDKUNDNR-MIN                       
053500                                  W-E8-IDKUNDNR-MAX                       
053600     ELSE                                                                 
053700       IF WS-IDKUNDNR = 'ALLA  '                                          
053800          MOVE +0000000        TO W-IDKUNDNR-MIN                          
053900                                  W-E8-IDKUNDNR-MIN                       
054000          MOVE +9999999        TO W-IDKUNDNR-MAX                          
054100                                  W-E8-IDKUNDNR-MAX                       
054200          MOVE SPACE           TO WS-IDKUNDNR                             
054300       ELSE                                                               
054400          MOVE NEJ             TO NYCKLAR-SW                              
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800     MOVE WS-IDKUNDNR          TO MOD-IDKUNDNR-UT                         
054900     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
055000     .                                                                    
055100     EJECT                                                                
055200                                                                          
055300 BC-KOLLA-DATUM SECTION.                                                  
055400                                                                          
055500     MOVE MFS-RENSA-FAELT      TO MOD-DATUM-IN                            
055600                                                                          
055700     IF MID-DATUM-IN = ALL '+'                                            
055800       MOVE MID-DATUM-UT       TO WS-TIREGDAT                             
055900     ELSE                                                                 
056000       MOVE MID-DATUM-IN       TO WS-TIREGDAT                             
056100       MOVE '7'                TO MFS-IDPFK                               
056200       MOVE SPACE              TO MFS-KDTRTYP                             
056300     END-IF                                                               
056400                                                                          
056500     INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO                  
056600                                                                          
056700     IF WS-TIREGDAT NUMERIC                                               
056800        MOVE WS-TIREGDAT        TO WS-TIREGDAT-NUM                        
056900        IF WS-TIREGDAT-NUM > ZERO                                         
057000           MOVE WS-TIREGDAT-NUM TO MOD-DATUM-UT                           
057100                                     W-TIREGDAT                           
057200        ELSE                                                              
057300           MOVE DAGENS-DATUM    TO MOD-DATUM-UT                           
057400                                   W-TIREGDAT                             
057500        END-IF                                                            
057600     ELSE                                                                 
057700        MOVE DAGENS-DATUM      TO MOD-DATUM-UT                            
057800                                  W-TIREGDAT                              
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 BD-KOLLA-IDDC   SECTION.                                                 
058300                                                                          
058400     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
058500     IF MID-IDDC-IN = ALL '+'                                             
058600         MOVE MID-IDDC-UT     TO W-IDDC-B6                                
058700                                 MOD-IDDC-UT                              
058800     ELSE                                                                 
058900       MOVE MID-IDDC-IN       TO W-IDDC-B6                                
059000                                 MOD-IDDC-UT                              
059100       MOVE '7'         TO MFS-IDPFK                                      
059200       MOVE SPACE       TO MFS-KDTRTYP                                    
059300     END-IF                                                               
059310     PERFORM IMS-GU-WDB601                                                
059400                                                                          
059500     IF DCS-KDDC = SPACE OR DCS-DDC                                       
059510       MOVE MSGI-IDDC  TO W-IDDC-MIN                                      
059520                          W-IDDC-MAX                                      
059530                          W-IDDC                                          
059540                          MOD-IDDC-UT                                     
059550     ELSE                                                                 
059600       MOVE DCS-IDDC   TO W-IDDC-MIN                                      
059700                          W-IDDC-MAX                                      
059800                          W-IDDC                                          
060300     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600 BE-KOLLA-FRAKTKOD SECTION.                                               
060700                                                                          
060800     MOVE MFS-RENSA-FAELT      TO MOD-KDFRAKT-IN                          
060900                                                                          
061000     IF MID-KDFRAKT-IN         NOT = ALL '+'                              
061500       MOVE '7'                TO MFS-IDPFK                               
061600       MOVE SPACE              TO MFS-KDTRTYP                             
061700     END-IF                                                               
061800                                                                          
061900     IF MSGI-KDFRAKT NUMERIC                                              
062000       IF MSGI-KDFRAKT         > ZERO                                     
062100         MOVE MSGI-KDFRAKT     TO WS-KDFRAKT-NUM                          
062200         MOVE JA               TO KDFRAKT-SW                              
062300       END-IF                                                             
062400     ELSE                                                                 
062500       MOVE NEJ                TO NYCKLAR-SW                              
062600     END-IF                                                               
062700                                                                          
062800     MOVE MSGI-KDFRAKT         TO MOD-KDFRAKT-UT                          
062900     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
063000     .                                                                    
063100     EJECT                                                                
063200 BF-KOLLA-ORDERKLASS SECTION.                                             
063300                                                                          
063400     MOVE MFS-RENSA-FAELT       TO MOD-KDORDKL-IN                         
063500                                                                          
063600                                                                          
063700     IF EGEN-MID                                                          
063800        IF MID-KDORDKL-IN NOT = ALL '+'                                   
063900          MOVE MID-KDORDKL-IN   TO WS-KDORDKL                             
064000          MOVE '7'              TO MFS-IDPFK                              
064100          MOVE SPACE            TO MFS-KDTRTYP                            
064200        ELSE                                                              
064300          IF MID-IDDISTR-IN = ALL '+' AND                                 
064400             MID-IDKUNDNR-IN = ALL '+' AND                                
064500             MID-IDDC-IN     = ALL '+' AND                                
064600             MID-KDFRAKT-IN = ALL '+'                                     
064700             MOVE MID-KDORDKL-UT TO WS-KDORDKL                            
064800          ELSE                                                            
064900             MOVE MID-KDORDKL-IN TO WS-KDORDKL                            
065000          END-IF                                                          
065100          IF WS-KDORDKL = SPACE                                           
065200             MOVE '+'           TO WS-KDORDKL                             
065300          END-IF                                                          
065400        END-IF                                                            
065500        IF WS-KDORDKL = ALL '+'                                           
065600           MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-UT                         
065700        ELSE                                                              
065800           IF WS-KDORDKL NUMERIC                                          
065900               MOVE WS-KDORDKL  TO WS-KDORDKL-NUM                         
066000               MOVE JA          TO KDORDKL-SW                             
066100               IF WS-KDORDKL-NUM > 4                                      
066200                  MOVE NEJ      TO NYCKLAR-SW                             
066300               END-IF                                                     
066400           ELSE                                                           
066500             MOVE NEJ           TO NYCKLAR-SW                             
066600           END-IF                                                         
066700           MOVE WS-KDORDKL      TO MOD-KDORDKL-UT                         
066800        END-IF                                                            
066900     ELSE                                                                 
067000        MOVE MFS-RENSA-FAELT   TO MOD-KDORDKL-UT                          
067100     END-IF                                                               
067200                                                                          
067300     .                                                                    
067400     EJECT                                                                
067500 BG-KOLLA-IDORDER SECTION.                                                
067600                                                                          
067700     MOVE MSGI-IDKUNDRF (1:7)  TO MOD-IDORDNR7-UT                         
068700     .                                                                    
068800     EJECT                                                                
068900 BH-KOLLA-PROFORMA SECTION.                                               
069000                                                                          
069100     MOVE MFS-RENSA-FAELT TO MOD-PROFORMA                                 
069200     IF MID-PROFORMA = ALL '+'                                            
069300        MOVE NEJ TO MOD-PROFORMA                                          
069400                    MOD-PROFORMA-UT                                       
069500                    MID-PROFORMA                                          
069600        MOVE MID-PROFORMA TO PROFORMA-SW                                  
069700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-PROFORMA-ATTR                    
069800     ELSE                                                                 
069900        IF MID-PROFORMA = NEJ                                             
070000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-PROFORMA-ATTR                 
070100           MOVE NEJ TO MOD-PROFORMA                                       
070200                       MOD-PROFORMA-UT                                    
070300           MOVE MID-PROFORMA TO PROFORMA-SW                               
070400        ELSE                                                              
070500           IF MID-PROFORMA = 'J' OR 'Y'                                   
070600              MOVE MID-PROFORMA TO MOD-PROFORMA                           
070700                                   MOD-PROFORMA-UT                        
070800              MOVE MFS-ALFA-FAELT-RAETT TO MOD-PROFORMA-ATTR              
070900              MOVE MID-PROFORMA TO PROFORMA-SW                            
071000           ELSE                                                           
071100              MOVE MFS-ALFA-FAELT-FEL TO MOD-PROFORMA-ATTR                
071200              MOVE MFS-ROER-EJ-FAELT  TO MOD-PROFORMA                     
071300              MOVE NEJ TO NYCKLAR-SW                                      
071400              MOVE NEJ TO ALLT-SW                                         
071500           END-IF                                                         
071600        END-IF                                                            
071700     END-IF                                                               
071800                                                                          
071900     IF MID-PROFORMA NOT = MID-PROFORMA-UT                                
072000        MOVE '7'                TO MFS-IDPFK                              
072100        MOVE SPACE              TO MFS-KDTRTYP                            
072200     END-IF                                                               
072300     .                                                                    
072400     EJECT                                                                
072500 BI-KOLLA-PRINTER-ID SECTION.                                             
072600                                                                          
072700     MOVE JA                   TO ALLT-SW                                 
072800     MOVE 1                    TO PRT-KDCALL                              
072900     MOVE MID-KDPRT            TO PRT-IDPRTLST                            
073000                                                                          
073100     CALL W006PRT  USING PRT-W006PRT                                      
073200                                                                          
073300     IF PRT-IDLTERM = 'SAKNAS  '                                          
073400       MOVE ERR-FEL-PRINTER    TO MED-IDMFSFEL                            
073500       CALL WMEDKONV USING MED-WMEDAREA                                   
073600       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
073700       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDPRT                               
073800       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-ATTR                          
073900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
074000       MOVE NEJ                TO ALLT-SW                                 
074100       MOVE +1 TO MOD-INDX                                                
074200       PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                              
074300          IF MID-KDCMD(MOD-INDX) NOT = ALL '+'                            
074400             MOVE MFS-ROER-EJ-FAELT                                       
074500                               TO MOD-KDCMD(MOD-INDX)                     
074600          END-IF                                                          
074700          ADD +1               TO MOD-INDX                                
074800       END-PERFORM                                                        
074900     ELSE                                                                 
075000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-ATTR                        
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 BJ-KOLLA-RADER SECTION.                                                  
075500                                                                          
075600     MOVE +1                   TO MOD-INDX                                
075700     MOVE +0                   TO WS-ANT-CMD                              
075800                                                                          
075900     PERFORM UNTIL MOD-INDX > MAX-MOD-INDX OR                             
076000       ALLT-INTE-OK                                                       
076100                                                                          
076200       IF MID-KDCMD (MOD-INDX) NOT = ALL '+'                              
076300          IF MID-KDCMD (MOD-INDX) = 'X'                                   
076400                                                                          
076500            ADD +1             TO WS-ANT-CMD                              
076600            MOVE MID-IDORDNR7(MOD-INDX)                                   
076700                                  TO WS-IDKUNDRF7-X                       
076800            INSPECT WS-IDKUNDRF7-X                                        
076900                    REPLACING LEADING SPACE BY ZERO                       
077000            IF WS-IDKUNDRF7-X NOT > ZERO                                  
077100              MOVE ERR-TOM-RAD TO MED-IDMFSFEL                            
077200              CALL WMEDKONV USING MED-WMEDAREA                            
077300              MOVE MED-MFSFEL  TO MOD-TEMFSFEL                            
077400              MOVE NEJ         TO ALLT-SW                                 
077500              MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD(MOD-INDX)               
077600              MOVE MFS-ALFA-FAELT-FEL TO                                  
077700                            MOD-KDCMD-ATTR(MOD-INDX)                      
077800              PERFORM MFS-ROER-EJ-FAELT-UT                                
077900            ELSE                                                          
078000              MOVE MFS-ALFA-FAELT-RAETT TO                                
078100                               MOD-KDCMD-ATTR(MOD-INDX)                   
078200              MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD(MOD-INDX)               
078300              MOVE MID-IDORDNR7(MOD-INDX)                                 
078400                                  TO WS-IDORDNR7-X                        
078500              INSPECT WS-IDORDNR7-X                                       
078600                    REPLACING LEADING SPACE BY ZERO                       
078700            END-IF                                                        
078800          ELSE                                                            
078900            IF MED-IDMFSFEL NOT = '001'                                   
079000              MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                       
079100              CALL WMEDKONV USING MED-WMEDAREA                            
079200              MOVE MED-MFSFEL  TO MOD-TEMFSFEL                            
079300              PERFORM MFS-ROER-EJ-FAELT-UT                                
079400            END-IF                                                        
079500            MOVE MFS-ALFA-FAELT-FEL TO                                    
079600                             MOD-KDCMD-ATTR(MOD-INDX)                     
079700            MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD(MOD-INDX)                 
079800            MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRT                           
079900            MOVE 'W'               TO ALLT-SW                             
080000          END-IF                                                          
080100       ELSE                                                               
080200          MOVE MFS-ALFA-FAELT-RAETT TO                                    
080300                        MOD-KDCMD-ATTR(MOD-INDX)                          
080400          MOVE MFS-RENSA-FAELT TO MOD-KDCMD(MOD-INDX)                     
080500       END-IF                                                             
080600       ADD +1                  TO MOD-INDX                                
080700     END-PERFORM                                                          
080800                                                                          
080900     IF ALLT-SW = 'W'                                                     
081000        MOVE NEJ               TO ALLT-SW                                 
081100     END-IF                                                               
081200                                                                          
081300     IF ALLT-OK                                                           
081400        IF MOD-INDX > MAX-MOD-INDX AND                                    
081500           WS-ANT-CMD = +0                                                
081600           MOVE INF-FYLL-I-CMD TO MED-IDMFSINF                            
081700           CALL WMEDKONV USING MED-WMEDAREA                               
081800           MOVE MED-MFSINF     TO MOD-TEMFSFEL                            
081900           MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRT                            
082000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
082100           MOVE NEJ            TO ALLT-SW                                 
082200        END-IF                                                            
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600 BK-KOLLA-CMD SECTION.                                                    
082700                                                                          
082800     MOVE +1                   TO MOD-INDX                                
082900     MOVE JA                   TO ALLT-SW                                 
083000                                                                          
083100     PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                                
083200                                                                          
083300                                                                          
083400       IF MID-KDCMD (MOD-INDX) NOT = ALL '+'                              
083500           MOVE ERR-TRYCK-PF4  TO MED-IDMFSFEL                            
083600           CALL WMEDKONV USING MED-WMEDAREA                               
083700           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
083800           MOVE MFS-ROER-EJ-FAELT                                         
083900                               TO MOD-KDCMD(MOD-INDX)                     
084000                                  MOD-KDPRT                               
084100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
084200           MOVE NEJ            TO ALLT-SW                                 
084300                                                                          
084400       END-IF                                                             
084500                                                                          
084600       ADD +1                  TO MOD-INDX                                
084700     END-PERFORM                                                          
084800     .                                                                    
084900     EJECT                                                                
085000                                                                          
085100 C-PRINT-ORDER SECTION.                                                   
085200                                                                          
085300                                                                          
085400     IF ALLT-OK                                                           
085500       PERFORM CA-TRANS-4294                                              
085600                                                                          
085700       IF ENGLISH-TEXT                                                    
085800          MOVE 'ORDERCONFIRMATIONS ARE QUEUED FOR PRINT-OUT'              
085900                           TO MOD-TEMFSINF                                
086000       ELSE                                                               
086100          MOVE 'ORDERBEKRÄFTELSER KÖADE FÖR UTSKRIFT'                     
086200                      TO MOD-TEMFSINF                                     
086300       END-IF                                                             
086400     END-IF                                                               
086500                                                                          
086600     MOVE +1 TO MOD-INDX                                                  
086700     PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                                
086800        MOVE MFS-RENSA-FAELT   TO MOD-KDCMD(MOD-INDX)                     
086900        ADD +1                 TO MOD-INDX                                
087000     END-PERFORM                                                          
087100     MOVE JA TO ALLT-SW                                                   
087200     .                                                                    
087300     EJECT                                                                
087400                                                                          
087500 CA-TRANS-4294  SECTION.                                                  
087600                                                                          
087700     MOVE MFS-KDMFSFOR         TO 4294-SPRAK                              
087800     MOVE MID-W4I28101                                                    
087900                   TO 4294-FILLER                                         
088000                                                                          
088100     PERFORM IMS-INSERT-TRANS4294                                         
088200     .                                                                    
088300     EJECT                                                                
088400                                                                          
088500 D-FOERSTA-SIDA SECTION.                                                  
088600                                                                          
088700     MOVE INF-FIRST-PAGE       TO MED-IDMFSFEL                            
088800     CALL WMEDKONV USING MED-WMEDAREA                                     
088900     MOVE MED-MFSFEL           TO MOD-TEMFSFEL                            
089000     .                                                                    
089100     EJECT                                                                
089200                                                                          
089300 E-NAESTA-SIDA SECTION.                                                   
089400     MOVE SPACE                TO W-IDKUNDRF-MIN                          
089500     MOVE MID-IDDISTR-NEXT     TO W-IDDISTR-MIN                           
089600                                  W-IDDISTR-MAX                           
089700                                  W-E8-IDDISTR-MIN                        
089800                                  W-E8-IDDISTR-MAX                        
089900     MOVE MID-IDKUNDNR-NEXT    TO W-IDKUNDNR-MIN                          
090000                                  W-E8-IDKUNDNR-MIN                       
090100     MOVE MID-IDORDNR7-NEXT    TO W-IDORDNR7-MIN                          
090200     MOVE W-IDKUNDRF-MIN       TO W-E8-IDKUNDRF-MIN                       
090300                                                                          
090400     MOVE JA TO ALLT-SW                                                   
090500     .                                                                    
090600     EJECT                                                                
090700 F-SAMMA-SIDA SECTION.                                                    
090800                                                                          
090900     MOVE SPACE                TO W-IDKUNDRF-MIN                          
091000     MOVE MID-IDDISTR-ENTER    TO W-IDDISTR-MIN                           
091100                                  W-IDDISTR-MAX                           
091200                                  W-E8-IDDISTR-MIN                        
091300                                  W-E8-IDDISTR-MAX                        
091400     MOVE MID-IDKUNDNR-ENTER   TO W-IDKUNDNR-MIN                          
091500                                  W-E8-IDKUNDNR-MIN                       
091600     MOVE MID-IDORDNR7-ENTER   TO W-IDORDNR7-MIN                          
091700     MOVE W-IDKUNDRF-MIN       TO W-E8-IDKUNDRF-MIN                       
091800                                                                          
091900     MOVE JA TO ALLT-SW                                                   
092000     .                                                                    
092100     EJECT                                                                
092200                                                                          
092300 G-LAES-VISA-INFO SECTION.                                                
092400                                                                          
092500     MOVE +1                   TO MOD-INDX                                
092600     MOVE JA                   TO FIRST-TIME-WDQ2-SW                      
092700     MOVE 'JF'                 TO STATUS-WS                               
092800     MOVE NEJ                  TO SID-SW                                  
092900     MOVE NEJ                  TO BLAEDDRING-SW                           
093000                                                                          
093100     IF PROFORMA                                                          
093200        PERFORM IMS-07-GU-PROC-WDE801                                     
093300     ELSE                                                                 
093400        PERFORM IMS-01-GU-ORQI-WDQ201                                     
093500        MOVE STATUS-WS  TO WDQ2-STATUS-WS                                 
093600     END-IF                                                               
093700                                                                          
093800     IF SEGMENT-FINNS                                                     
093900        IF PROFORMA                                                       
094000           PERFORM GA-LAES-FOERSTA-WDQ1-POSTEN                            
094100           MOVE '  ' TO STATUS-WS                                         
094200           PERFORM GE-KOLLA-AVSLUT-ELLER-FORTS                            
094300        ELSE                                                              
094400           MOVE OHUV-KDORDKL  TO OHUV-KDORDKL-SPAR                        
094500           PERFORM GA-LAES-FOERSTA-WDQ1-POSTEN                            
094600           MOVE '  ' TO STATUS-WS                                         
094700           PERFORM GE-KOLLA-AVSLUT-ELLER-FORTS                            
094800        END-IF                                                            
094900     ELSE                                                                 
095000        PERFORM GE-KOLLA-AVSLUT-ELLER-FORTS                               
095100     END-IF                                                               
095200                                                                          
095300                                                                          
095400     PERFORM UNTIL WDQ2-STATUS-WS = 'GE' OR 'GB' OR                       
095500                   MOD-INDX > MAX-MOD-INDX                                
095600                                                                          
095700        PERFORM GB-LAES-VIDARE                                            
095800     END-PERFORM                                                          
095900                                                                          
096000     IF MOD-IDORDER(1)  NOT > ZERO                                        
096100        IF PROFORMA                                                       
096200           MOVE 'ORDERBEKRÄFTELSER FÖR PROFORMA SAKNAS'                   
096300                                 TO MOD-TEMFSFEL                          
096400           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
096500        ELSE                                                              
096600           MOVE INF-ORDBEK-SAKNAS TO MED-IDMFSFEL                         
096700           CALL WMEDKONV USING MED-WMEDAREA                               
096800           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
096900           PERFORM MFS-RENSA-ALLA-FAELT-UT                                
097000           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
097100        END-IF                                                            
097200     END-IF                                                               
097300                                                                          
097400     IF MOD-INDX > MAX-MOD-INDX                                           
097500        PERFORM GD-SPARA-NEXT-NYCKLAR                                     
097600     END-IF                                                               
097700     .                                                                    
097800     EJECT                                                                
097900                                                                          
098000 GA-LAES-FOERSTA-WDQ1-POSTEN SECTION.                                     
098100                                                                          
098200     MOVE NEJ                  TO WDQ1-SEGMENT-SW                         
098300     MOVE JA                   TO FIRST-TIME-WDQ1-SW                      
098400                                                                          
098500     IF NOT MFS-NEXT                                                      
098600        MOVE +1                TO CLAGER-INDX                             
098700     END-IF                                                               
098800                                                                          
098900     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
099000                                                                          
099100        IF PROFORMA                                                       
099200           MOVE PHUV-IDORDER   TO W-IDORDER-MIN                           
099300                                  W-IDORDER-MAX                           
099400        ELSE                                                              
099500           MOVE OHUV-IDORDER   TO W-IDORDER-MIN                           
099600                                  W-IDORDER-MAX                           
099700        END-IF                                                            
099800                                                                          
099900                                                                          
100000        PERFORM UNTIL WDQ1-SEGMENT-FINNS     OR                           
100100                      BASEN-SLUT             OR                           
100200                      SEGMENT-SAKNAS                                      
100300                                                                          
100400           MOVE JA TO FIRST-TIME-WDQ1-SW                                  
100500                                                                          
100600           PERFORM GAA-LAES-WDQ1                                          
100700        END-PERFORM                                                       
100800                                                                          
100900        IF WDQ1-SEGMENT-FINNS                                             
101000           MOVE 'GE' TO STATUS-WS                                         
101100        ELSE                                                              
101200           IF PROFORMA                                                    
101300              PERFORM IMS-08-GN-PROC-WDE801                               
101400           ELSE                                                           
101500              PERFORM IMS-02-GN-ORQI-WDQ201                               
101600           END-IF                                                         
101700           MOVE STATUS-WS  TO WDQ2-STATUS-WS                              
101800           IF SEGMENT-FINNS                                               
101900              IF PROFORMA                                                 
102000                 CONTINUE                                                 
102100              ELSE                                                        
102200                 MOVE OHUV-KDORDKL TO OHUV-KDORDKL-SPAR                   
102300              END-IF                                                      
102400           END-IF                                                         
102500        END-IF                                                            
102600                                                                          
102700     END-PERFORM                                                          
102800                                                                          
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200 GAA-LAES-WDQ1 SECTION.                                                   
103300                                                                          
103400     MOVE JA TO FIRST-TIME-WDQ1-SW                                        
103500     MOVE SPACE TO STATUS-WS                                              
103600     MOVE NEJ TO WDQ1-SEGMENT-SW                                          
103700                                                                          
103800     IF BLAEDDRING                                                        
103900        MOVE +1 TO MOD-INDX                                               
104000     END-IF                                                               
104100                                                                          
104200     PERFORM UNTIL WDQ1-SEGMENT-FINNS        OR                           
104300                   BASEN-SLUT                OR                           
104400                   SEGMENT-SAKNAS            OR                           
104500                   MOD-INDX > MAX-MOD-INDX                                
104600                                                                          
104700        IF PROFORMA                                                       
104800           IF FIRST-TIME-WDQ1                                             
104900              PERFORM IMS-03-GU-ORQM-WDQ101-PROF                          
105000              MOVE NEJ TO FIRST-TIME-WDQ1-SW                              
105100              IF SEGMENT-FINNS                                            
105200                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
105300              END-IF                                                      
105400           ELSE                                                           
105500              PERFORM IMS-04-GN-ORQM-WDQ101-PROF                          
105600              IF SEGMENT-FINNS                                            
105700                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
105800              END-IF                                                      
105900           END-IF                                                         
106000        ELSE                                                              
106100           IF FIRST-TIME-WDQ1                                             
106200              PERFORM IMS-05-GU-ORQM-WDQ101                               
106300              MOVE NEJ TO FIRST-TIME-WDQ1-SW                              
106400              IF SEGMENT-FINNS                                            
106500                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
106600              END-IF                                                      
106700           ELSE                                                           
106800              PERFORM IMS-06-GN-ORQM-WDQ101                               
106900              IF SEGMENT-FINNS                                            
107000                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
107100              END-IF                                                      
107200           END-IF                                                         
107300        END-IF                                                            
107400     END-PERFORM                                                          
107500                                                                          
107600     .                                                                    
107700     EJECT                                                                
107800 GAB-KOLLA-OM-DEN-SKALL-UT SECTION.                                       
107900                                                                          
108000     IF OBKR-BERADREF NOT = 'W480      '                                  
108100        PERFORM GC-REDIGERA-RAD                                           
108200     END-IF                                                               
108300     .                                                                    
108400     EJECT                                                                
108500                                                                          
108600 GB-LAES-VIDARE SECTION.                                                  
108700                                                                          
108800     IF LAES-EJ-WDQ2                                                      
108900        MOVE JA TO LAES-WDQ2-SW                                           
109000     ELSE                                                                 
109100        IF PROFORMA                                                       
109200           PERFORM IMS-08-GN-PROC-WDE801                                  
109300        ELSE                                                              
109400           PERFORM IMS-02-GN-ORQI-WDQ201                                  
109500        END-IF                                                            
109600        MOVE STATUS-WS     TO WDQ2-STATUS-WS                              
109700        IF SEGMENT-FINNS                                                  
109800           IF PROFORMA                                                    
109900              MOVE PHUV-IDORDER TO W-IDORDER-MIN                          
110000                                     W-IDORDER-MAX                        
110100           ELSE                                                           
110200              MOVE OHUV-KDORDKL TO OHUV-KDORDKL-SPAR                      
110300              MOVE OHUV-IDORDER TO W-IDORDER-MIN                          
110400                                     W-IDORDER-MAX                        
110500           END-IF                                                         
110600           MOVE JA         TO FIRST-TIME-WDQ1-SW                          
110700        END-IF                                                            
110800     END-IF                                                               
110900                                                                          
111000     MOVE NEJ TO WDQ1-SEGMENT-SW                                          
111100                                                                          
111200     PERFORM UNTIL SEGMENT-SAKNAS     OR                                  
111300                   BASEN-SLUT                                             
111400                                                                          
111500                                                                          
111600        MOVE JA TO FIRST-TIME-WDQ1-SW                                     
111700                                                                          
111800        PERFORM GBA-LAES-WDQ1                                             
111900        IF WDQ1-SEGMENT-FINNS                                             
112000           MOVE 'GE' TO STATUS-WS                                         
112100        END-IF                                                            
112200     END-PERFORM                                                          
112300     .                                                                    
112400     EJECT                                                                
112500 GBA-LAES-WDQ1 SECTION.                                                   
112600                                                                          
112700     MOVE JA TO FIRST-TIME-WDQ1-SW                                        
112800     MOVE '  ' TO STATUS-WS                                               
112900     MOVE NEJ TO WDQ1-SEGMENT-SW                                          
113000     PERFORM UNTIL SEGMENT-SAKNAS            OR                           
113100                   WDQ1-SEGMENT-FINNS        OR                           
113200                   BASEN-SLUT                OR                           
113300                   MOD-INDX > MAX-MOD-INDX                                
113400                                                                          
113500        IF PROFORMA                                                       
113600           IF FIRST-TIME-WDQ1                                             
113700              PERFORM IMS-03-GU-ORQM-WDQ101-PROF                          
113800              MOVE NEJ TO FIRST-TIME-WDQ1-SW                              
113900              IF SEGMENT-FINNS                                            
114000                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
114100              END-IF                                                      
114200           ELSE                                                           
114300              PERFORM IMS-04-GN-ORQM-WDQ101-PROF                          
114400              IF SEGMENT-FINNS                                            
114500                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
114600              END-IF                                                      
114700           END-IF                                                         
114800        ELSE                                                              
114900           IF FIRST-TIME-WDQ1                                             
115000              PERFORM IMS-05-GU-ORQM-WDQ101                               
115100              MOVE NEJ TO FIRST-TIME-WDQ1-SW                              
115200              IF SEGMENT-FINNS                                            
115300                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
115400              END-IF                                                      
115500           ELSE                                                           
115600              PERFORM IMS-06-GN-ORQM-WDQ101                               
115700              IF SEGMENT-FINNS                                            
115800                 PERFORM GAB-KOLLA-OM-DEN-SKALL-UT                        
115900              END-IF                                                      
116000           END-IF                                                         
116100        END-IF                                                            
116200     END-PERFORM                                                          
116300                                                                          
116400     .                                                                    
116500     EJECT                                                                
116600                                                                          
116700 GC-REDIGERA-RAD SECTION.                                                 
116800                                                                          
116900     IF KDFRAKT-IFYLLD                                                    
117000        IF NOT KDORDKL-IFYLLD                                             
117100          IF WS-KDFRAKT-NUM = OBKR-KDFRAKT                                
117200             PERFORM GCA-FLYTTA-UT-RAD                                    
117300          END-IF                                                          
117400        END-IF                                                            
117500     END-IF                                                               
117600                                                                          
117700     IF KDORDKL-IFYLLD                                                    
117800        IF NOT KDFRAKT-IFYLLD                                             
117900           IF WS-KDORDKL-NUM = OBKR-KDORDKL                               
118000              PERFORM GCA-FLYTTA-UT-RAD                                   
118100           END-IF                                                         
118200        END-IF                                                            
118300     END-IF                                                               
118400                                                                          
118500     IF KDORDKL-IFYLLD                                                    
118600        IF KDFRAKT-IFYLLD                                                 
118700           IF WS-KDORDKL-NUM = OBKR-KDORDKL AND                           
118800              WS-KDFRAKT-NUM = OBKR-KDFRAKT                               
118900              PERFORM GCA-FLYTTA-UT-RAD                                   
119000           END-IF                                                         
119100        END-IF                                                            
119200     END-IF                                                               
119300                                                                          
119400                                                                          
119500     IF NOT KDORDKL-IFYLLD                                                
119600        IF NOT KDFRAKT-IFYLLD                                             
119700           PERFORM GCA-FLYTTA-UT-RAD                                      
119800        END-IF                                                            
119900     END-IF                                                               
120000     .                                                                    
120100     EJECT                                                                
120200                                                                          
120300 GCA-FLYTTA-UT-RAD SECTION.                                               
120400                                                                          
120500     IF BLAEDDRING                                                        
120600        MOVE +14 TO MOD-INDX                                              
120700        MOVE JA TO SID-SW                                                 
120800        MOVE JA                  TO WDQ1-SEGMENT-SW                       
120900        MOVE MID-PROFORMA        TO MOD-PROFORMA                          
121000     END-IF                                                               
121100                                                                          
121200     IF MOD-INDX NOT > MAX-MOD-INDX                                       
121300        MOVE OBKR-IDKUNDNR     TO MOD-IDKUNDNR (MOD-INDX)                 
121400        IF PROFORMA                                                       
121500           MOVE PROC-IDORDNR7  TO MOD-IDORDNR7     (MOD-INDX)             
121600        ELSE                                                              
121700           MOVE ORQI-IDORDNR7  TO MOD-IDORDNR7     (MOD-INDX)             
121800        END-IF                                                            
121900        MOVE OBKR-BEKUNDRF     TO MOD-BEKUNDRF (MOD-INDX)                 
122000        MOVE OBKR-IDDC         TO MOD-IDDC     (MOD-INDX)                 
122100        MOVE OBKR-KDFRAKT      TO MOD-KDFRAKT (MOD-INDX)                  
122200        MOVE OHUV-KDORDKL-SPAR TO MOD-KDORDKL (MOD-INDX)                  
122300        MOVE OBKR-TIREGDAT     TO MOD-TIREGDAT (MOD-INDX)                 
122400        MOVE OBKR-IDORDER      TO MOD-IDORDER (MOD-INDX)                  
122500        MOVE OBKR-IDARTNR      TO MOD-IDARTNR (MOD-INDX)                  
122600        MOVE OBKR-KDORDBEK     TO MOD-KDORDBEK(MOD-INDX)                  
122700        MOVE JA                TO WDQ1-SEGMENT-SW                         
122800        ADD +1                 TO MOD-INDX                                
122900     END-IF                                                               
123000     .                                                                    
123100     EJECT                                                                
123200                                                                          
123300 GD-SPARA-NEXT-NYCKLAR SECTION.                                           
123400                                                                          
123500     IF PROFORMA                                                          
123600        PERFORM IMS-08-GN-PROC-WDE801                                     
123700     ELSE                                                                 
123800        PERFORM IMS-02-GN-ORQI-WDQ201                                     
123900     END-IF                                                               
124000     MOVE STATUS-WS        TO WDQ2-STATUS-WS                              
124100     IF SEGMENT-FINNS                                                     
124200        IF PROFORMA                                                       
124300           CONTINUE                                                       
124400        ELSE                                                              
124500           MOVE OHUV-KDORDKL TO OHUV-KDORDKL-SPAR                         
124600        END-IF                                                            
124700        MOVE JA TO BLAEDDRING-SW                                          
124800        PERFORM GA-LAES-FOERSTA-WDQ1-POSTEN                               
124900        IF MER-INFO-FINNS                                                 
125000           IF PROFORMA                                                    
125100              MOVE PROC-IDDISTR  TO MOD-IDDISTR-NEXT                      
125200              MOVE PROC-IDKUNDNR TO MOD-IDKUNDNR-NEXT                     
125300              MOVE PROC-IDORDNR7 TO MOD-IDORDNR7-NEXT                     
125400           ELSE                                                           
125500              MOVE ORQI-IDDISTR  TO MOD-IDDISTR-NEXT                      
125600              MOVE ORQI-IDKUNDNR TO MOD-IDKUNDNR-NEXT                     
125700              MOVE ORQI-IDORDNR7 TO MOD-IDORDNR7-NEXT                     
125800           END-IF                                                         
125900                                                                          
126000           IF NOT MFS-PRINT                                               
126100             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
126200             CALL WMEDKONV USING MED-WMEDAREA                             
126300             MOVE MED-MFSINF     TO MOD-TEMFSINF                          
126400           END-IF                                                         
126500        END-IF                                                            
126600     END-IF                                                               
126700     .                                                                    
126800     EJECT                                                                
126900                                                                          
127000 GE-KOLLA-AVSLUT-ELLER-FORTS  SECTION.                                    
127100                                                                          
127200     IF SEGMENT-FINNS                                                     
127300        IF PROFORMA                                                       
127400           MOVE PHUV-IDDISTR TO MOD-IDDISTR-ENTER                         
127500                                  MOD-IDDISTR-NEXT                        
127600           MOVE PHUV-IDKUNDNR TO MOD-IDKUNDNR-ENTER                       
127700                                  MOD-IDKUNDNR-NEXT                       
127800           MOVE PHUV-IDORDNR7 TO MOD-IDORDNR7-ENTER                       
127900                                  MOD-IDORDNR7-NEXT                       
128000           MOVE JA TO WDQ1-SEGMENT-SW                                     
128100        ELSE                                                              
128200           MOVE ORQI-IDDISTR TO MOD-IDDISTR-ENTER                         
128300                                     MOD-IDDISTR-NEXT                     
128400           MOVE ORQI-IDKUNDNR TO MOD-IDKUNDNR-ENTER                       
128500                                     MOD-IDKUNDNR-NEXT                    
128600           MOVE ORQI-IDORDNR7 TO MOD-IDORDNR7-ENTER                       
128700                                     MOD-IDORDNR7-NEXT                    
128800           MOVE JA TO WDQ1-SEGMENT-SW                                     
128900        END-IF                                                            
129000     ELSE                                                                 
129100        IF PROFORMA                                                       
129200           MOVE 'ORDERBEKRÄFTELSER FÖR PROFORMA SAKNAS'                   
129300                                 TO MOD-TEMFSFEL                          
129400        ELSE                                                              
129500           MOVE INF-ORDBEK-SAKNAS TO MED-IDMFSFEL                         
129600           CALL WMEDKONV USING MED-WMEDAREA                               
129700           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
129800           PERFORM MFS-RENSA-ALLA-FAELT-UT                                
129900           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
130000        END-IF                                                            
130100     END-IF                                                               
130200     .                                                                    
130300     EJECT                                                                
130400                                                                          
130500 MFS-ROER-EJ-FAELT-UT SECTION.                                            
130600*    --- ALLA UTDATA-FÄLT                                                 
130700     MOVE +1 TO HELP-INDX                                                 
130800     PERFORM UNTIL HELP-INDX > MAX-MOD-INDX                               
130900       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKUNDNR     (HELP-INDX)            
131000                                  MOD-IDORDNR7     (HELP-INDX)            
131100                                  MOD-BEKUNDRF     (HELP-INDX)            
131200                                  MOD-IDDC         (HELP-INDX)            
131300                                  MOD-KDFRAKT      (HELP-INDX)            
131400                                  MOD-KDORDKL      (HELP-INDX)            
131500                                  MOD-TIREGDAT     (HELP-INDX)            
131600                                  MOD-IDORDER      (HELP-INDX)            
131700                                  MOD-IDARTNR      (HELP-INDX)            
131800                                  MOD-KDORDBEK     (HELP-INDX)            
131900       ADD +1 TO HELP-INDX                                                
132000     END-PERFORM                                                          
132100     .                                                                    
132200     EJECT                                                                
132300                                                                          
132400 MFS-RENSA-ALLA-FAELT-UT SECTION.                                         
132500                                                                          
132600     MOVE MFS-RENSA-FAELT      TO MOD-KDPRT                               
132700     MOVE +1 TO HELP-INDX                                                 
132800     PERFORM UNTIL HELP-INDX > MAX-MOD-INDX                               
132900        MOVE MFS-RENSA-FAELT   TO MOD-KDCMD       (HELP-INDX)             
133000                                  MOD-IDKUNDNR    (HELP-INDX)             
133100                                  MOD-IDORDNR7    (HELP-INDX)             
133200                                  MOD-BEKUNDRF    (HELP-INDX)             
133300                                  MOD-IDDC        (HELP-INDX)             
133400                                  MOD-KDFRAKT     (HELP-INDX)             
133500                                  MOD-KDORDKL     (HELP-INDX)             
133600                                  MOD-TIREGDAT    (HELP-INDX)             
133700                                  MOD-IDORDER     (HELP-INDX)             
133800                                  MOD-IDARTNR     (HELP-INDX)             
133900                                  MOD-KDORDBEK    (HELP-INDX)             
134000        ADD +1 TO HELP-INDX                                               
134100     END-PERFORM                                                          
134200     .                                                                    
134300     EJECT                                                                
136700                                                                          
136800 MFS-RENSA-FAELT-UT SECTION.                                              
136900                                                                          
137000*    --- ALLA UTDATA-FÄLT                                                 
137100     MOVE +1 TO HELP-INDX                                                 
137200     PERFORM UNTIL HELP-INDX > MAX-MOD-INDX                               
137300       MOVE MFS-RENSA-FAELT    TO MOD-IDKUNDNR     (HELP-INDX)            
137400                                  MOD-IDORDNR7     (HELP-INDX)            
137500                                  MOD-BEKUNDRF     (HELP-INDX)            
137600                                  MOD-IDDC         (HELP-INDX)            
137700                                  MOD-KDFRAKT      (HELP-INDX)            
137800                                  MOD-KDORDKL      (HELP-INDX)            
137900                                  MOD-TIREGDAT     (HELP-INDX)            
138000                                  MOD-IDORDER      (HELP-INDX)            
138100                                  MOD-IDARTNR      (HELP-INDX)            
138200                                  MOD-KDORDBEK     (HELP-INDX)            
138300       ADD +1 TO HELP-INDX                                                
138400     END-PERFORM                                                          
138500     .                                                                    
138600     EJECT                                                                
138700                                                                          
141200                                                                          
141300 IMS-GET-MSG SECTION.                                                     
141400                                                                          
141500     MOVE '  QC'               TO GODK-STATUSKODER                        
141600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
141700     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000     SKIP3                                                                
142100 IMS-INSERT-MSG SECTION.                                                  
142200                                                                          
142300     IF ENGLISH-TEXT                                                      
142400       MOVE 'N'                TO MFS-KDHUVOMR                            
142500     END-IF                                                               
142600     MOVE LOW-VALUE            TO MSG-KDZ1 MSG-KDZ2                       
142700     MOVE SPACE                TO GODK-STATUSKODER                        
142800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
142900     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
143000     PERFORM IMS-STATUSKONTROLL                                           
143100     .                                                                    
143200     SKIP3                                                                
143300 IMS-INSERT-TRANS4294 SECTION.                                            
143400                                                                          
143500     MOVE LOW-VALUE            TO 4294-Z1                                 
143600                                  4294-Z2                                 
143700     MOVE SPACE                TO GODK-STATUSKODER                        
143800     CALL CBLTDLI USING ISRT 4294-PCB 4294-MSG-IO-AREA                    
143900     MOVE 4294-STATUS-CODE   TO STATUS-WS                                 
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     EJECT                                                                
144300                                                                          
144400 IMS-01-GU-ORQI-WDQ201 SECTION.                                           
144500                                                                          
144600     STRING 'WLORQI01(WDQ2CSEQ>=' W-IDGMTREF-MIN-X                        
144700                    '&WDQ2CSEQ<=' W-IDGMTREF-MAX-X ')'                    
144800          DELIMITED BY SIZE  INTO SSA1                                    
144900     MOVE '  GEGB'             TO GODK-STATUSKODER                        
145000     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-WDQ2 SSA1                 
145100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400     SKIP3                                                                
145500 IMS-02-GN-ORQI-WDQ201 SECTION.                                           
145600                                                                          
145700     STRING 'WLORQI01(WDQ2CSEQ>=' W-IDGMTREF-MIN-X                        
145800                    '&WDQ2CSEQ<=' W-IDGMTREF-MAX-X ')'                    
145900          DELIMITED BY SIZE  INTO SSA1                                    
146000     MOVE '  GEGB'             TO GODK-STATUSKODER                        
146100     CALL CBLTDLI USING GN ORQI-PCB DLI-IO-AREA-WDQ2 SSA1                 
146200     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
146300     PERFORM IMS-STATUSKONTROLL                                           
146400     .                                                                    
146500     EJECT                                                                
146600 IMS-03-GU-ORQM-WDQ101-PROF SECTION.                                      
146700                                                                          
146800     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
146900                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
147000                    '&IDDC     =' W-IDDC-X                                
147100                    '&IDSYSTEM =' W-IDSYSTEM-X                            
147200                    '&TIREGDAT =' W-TIREGDAT-X ')'                        
147300          DELIMITED BY SIZE  INTO SSA1                                    
147400     MOVE '  GEGB'             TO GODK-STATUSKODER                        
147500     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA-WDQ1 SSA1                 
147600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900     SKIP3                                                                
148000 IMS-04-GN-ORQM-WDQ101-PROF SECTION.                                      
148100                                                                          
148200     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
148300                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
148400                    '&IDDC     =' W-IDDC-X                                
148500                    '&IDSYSTEM =' W-IDSYSTEM-X                            
148600                    '&TIREGDAT =' W-TIREGDAT-X ')'                        
148700          DELIMITED BY SIZE  INTO SSA1                                    
148800     MOVE '  GEGB'             TO GODK-STATUSKODER                        
148900     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA-WDQ1 SSA1                 
149000     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300     EJECT                                                                
149400 IMS-05-GU-ORQM-WDQ101 SECTION.                                           
149500                                                                          
149600     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
149700                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
149800                    '&IDDC     =' W-IDDC-X                                
149900                    '&IDSYSTEMNE' W-IDSYSTEM-X                            
150000                    '&TIREGDAT =' W-TIREGDAT-X ')'                        
150100          DELIMITED BY SIZE  INTO SSA1                                    
150200     MOVE '  GEGB'             TO GODK-STATUSKODER                        
150300     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA-WDQ1 SSA1                 
150400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
150500     PERFORM IMS-STATUSKONTROLL                                           
150600     .                                                                    
150700     SKIP3                                                                
150800 IMS-06-GN-ORQM-WDQ101 SECTION.                                           
150900                                                                          
151000     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
151100                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
151200                    '&IDDC     =' W-IDDC-X                                
151300                    '&IDSYSTEMNE' W-IDSYSTEM-X                            
151400                    '&TIREGDAT =' W-TIREGDAT-X ')'                        
151500          DELIMITED BY SIZE  INTO SSA1                                    
151600     MOVE '  GEGB'             TO GODK-STATUSKODER                        
151700     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA-WDQ1 SSA1                 
151800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
151900     PERFORM IMS-STATUSKONTROLL                                           
152000     .                                                                    
152100     EJECT                                                                
152200 IMS-07-GU-PROC-WDE801 SECTION.                                           
152300                                                                          
152400     STRING 'WLPROC01(WDE801KY>=' W-WDE801KY-MIN-X                        
152500                    '&WDE801KY<=' W-WDE801KY-MAX-X ')'                    
152600          DELIMITED BY SIZE INTO SSA1                                     
152700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
152800     CALL CBLTDLI USING GU PROC-PCB DLI-IO-AREA-WDE8  SSA1                
152900     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
153000     PERFORM IMS-STATUSKONTROLL                                           
153100     .                                                                    
153200     EJECT                                                                
153300 IMS-08-GN-PROC-WDE801 SECTION.                                           
153400                                                                          
153500     STRING 'WLPROC01(WDE801KY>=' W-WDE801KY-MIN-X                        
153600                    '&WDE801KY<=' W-WDE801KY-MAX-X ')'                    
153700          DELIMITED BY SIZE INTO SSA1                                     
153800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
153900     CALL CBLTDLI USING GN PROC-PCB DLI-IO-AREA-WDE8  SSA1                
154000     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300     EJECT                                                                
154310                                                                          
154320 IMS-GU-WDB601    SECTION.                                                
154330     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
154340          DELIMITED BY SIZE INTO SSA1                                     
154350     MOVE '  GE' TO GODK-STATUSKODER                                      
154360     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
154370     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
154380     PERFORM IMS-STATUSKONTROLL                                           
154390     IF SEGMENT-SAKNAS                                                    
154391         MOVE SPACE TO DCS-KDDC                                           
154392     END-IF                                                               
154393     .                                                                    
154400 IMS-STATUSKONTROLL SECTION.                                              
154500                                                                          
154600     SET STATUS-IX TO 1                                                   
154700     SEARCH GODK-STATUS                                                   
154800       AT END CALL FELLOG                                                 
154900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
155000     END-SEARCH                                                           
155100     .                                                                    
