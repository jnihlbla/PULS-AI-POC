000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4051300.                                                
000300 AUTHOR.         LASSI OLGRENER.                                          
000400 DATE-WRITTEN.   JULI 1992.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET HANTERAR BILDEN                                       
000900*        FRÅGA PÅ DISTRIKT / STATUS                                       
001000*                                                                         
001100*        PROGRAMMET LÄSER   - WDQ2                                        
001200*                                                                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T513                                              
001600*        MID:         W4I51301                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O51301                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4051300'.            
002900 77  FELTEXT                     PIC X(32)   VALUE SPACE.                 
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003300 77  MOD-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
003400 77  MAX-MOD-INDX                PIC S9(4)  VALUE +14   COMP SYNC.        
003500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003600 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
003700                                                                          
003800 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
003900 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
004000 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
004100                                                                          
004200                                                                          
004300                                                                          
004400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004500 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
004600 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
004700 77  WS-KDORDKL                  PIC X(1)    VALUE SPACE.                 
004800 77  WS-KDORDKL-NUM              PIC 9(1)    VALUE ZERO.                  
004900 77  WS-KDORDSTA                 PIC X(1)    VALUE SPACE.                 
005000     EJECT                                                                
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 01  SW-ORDER-AKTUELL            PIC X       VALUE 'N'.                   
005600     88  ORDER-AKTUELL                       VALUE 'J'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '4513'.                
006000     88  GODK-MID                            VALUE '4511' '4512'          
006100                                                   '4513' '4514'          
006200                                                   '4515' '4517'.         
006300     88  HELP-MID                            VALUE '0551'.                
006400                                                                          
006500                                                                          
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007400*01 -COPY WMSGINIT                                                        
007500     EJECT                                                                
007600*   -COPY WMEDAREA                                                        
007700     SKIP3                                                                
007800 01  MESSAGE-CODES.                                                       
007900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008100     03  INF-NO-MORE-F6          PIC X(3)    VALUE '368'.                 
008200     03  INF-INFO-SAKNAS         PIC X(3)    VALUE '413'.                 
008300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008400     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
008500     EJECT                                                                
008600*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
008700   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
008800     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
008900                                                                          
009000 01  BILD-HOPP-AREOR.                                                     
009100                                                                          
009200   03    W-BILD               PIC X(4)    VALUE SPACE.                    
009300   03    W-HOPP-IDTRANS.                                                  
009400     05  FILLER               PIC X(1)    VALUE 'W'.                      
009500     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
009600     05  FILLER               PIC X(1)    VALUE 'T'.                      
009700     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
009800     05  FILLER               PIC X(2)    VALUE SPACE.                    
009900                                                                          
010000                                                                          
010100   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
010200   03      P-TO-P-SW.                                                     
010300                                                                          
010400     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
010500     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
010600     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
010700     05  P-TO-P-KDTRANS          PIC X(8).                                
010800     05  P-TO-P-IDTRANS          PIC X(4).                                
010900     05  P-TO-P-KDMFSFOR         PIC X(1).                                
011000     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
011100                                                                          
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011400     SKIP3                                                                
011500*01  MID -COPY W4I51301                                                   
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011800     SKIP3                                                                
011900*01  -COPY WMSGAREA                                                       
012000     EJECT                                                                
012100     03  MOD REDEFINES MSG-AREA.                                          
012200*      05  -COPY W4O51301                                                 
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012500                                                                          
012600*01  -COPY WMFSAREA                                                       
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
012900                                                                          
013000 01  SPAR-AREA.                                                           
013100     03  SPAR-IDTRANS            PIC X(4)    VALUE SPACE.                 
013200     03  PGNO                    PIC 9(2)    VALUE ZERO.                  
013300     03  FIRST-SW                PIC X       VALUE 'J'.                   
013400     03  SPAR-WDQ2CSEQ-ENTER OCCURS 20 PIC X(17) VALUE SPACE.             
013500     03  SPAR-WDQ2CSEQ-NEXT     PIC X(17)   VALUE SPACE.                  
013600     03  SPAR-KDORDSTA           PIC X(1)    VALUE SPACE.                 
013700     03  SPAR-KDORDKL            PIC X(1)    VALUE SPACE.                 
013800                                                                          
013900     EJECT                                                                
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100                                                                          
014200 01  NYCKLAR-TILL-DLI.                                                    
014300                                                                          
014400     03  W-WDQ2CSEQ-X.                                                    
014500         05  W-IDDISTR-SPAR  PIC S9(5)   VALUE ZERO COMP-3.               
014600         05  W-IDKUNDNR-SPAR PIC S9(7)   VALUE ZERO COMP-3.               
014700         05  W-IDORDNR7-SPAR PIC  9(7)   VALUE ZERO.                      
014800         05  FILLER          PIC  X(3)   VALUE SPACE.                     
014900                                                                          
015000                                                                          
015100     03  W-WDQ2CSEQ-MIN-X.                                                
015200         05  W-IDDISTR-MIN   PIC S9(5)   VALUE ZERO COMP-3.               
015300         05  W-IDKUNDNR-MIN  PIC S9(7)   VALUE ZERO COMP-3.               
015400         05  W-IDORDNR7-MIN  PIC  9(7)   VALUE ZERO.                      
015500         05  FILLER          PIC  X(3)   VALUE SPACE.                     
015600                                                                          
015700     03  W-WDQ2CSEQ-MAX-X.                                                
015800         05  W-IDDISTR-MAX   PIC S9(5)   VALUE ZERO COMP-3.               
015900         05  W-IDKUNDNR-MAX  PIC S9(7)   VALUE ZERO COMP-3.               
016000         05  W-IDORDNR7-MAX  PIC  9(7)   VALUE 9999999.                   
016100         05  FILLER          PIC  X(3)   VALUE SPACE.                     
016200                                                                          
016300     03  W-KDORDSTA-SOK-X.                                                
016400         05 W-KDORDSTA-SOK   PIC X(1)    VALUE SPACE.                     
016500                                                                          
016600     03  W-IDORDER-X.                                                     
016700         05 W-IDORDER        PIC S9(7)   VALUE ZERO COMP-3.               
016800                                                                          
016900     03  W-IDDC-X.                                                        
017000         05 W-IDDC           PIC  X(2)   VALUE SPACE.                     
017100                                                                          
017200     03  W-IDDC-B6-X.                                                     
017300         05 W-IDDC-B6                  PIC X(2).                          
017400     EJECT                                                                
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  STATUS-OK                           VALUE '  '.                  
017700     88  SEGMENT-FINNS                       VALUE '  '.                  
017800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017900     88  BASEN-SLUT                          VALUE 'GB'.                  
018000     88  TRANSKOD-FEL                        VALUE 'A1'.                  
018100     88  SECURITY-FEL                        VALUE 'A4'.                  
018200     SKIP2                                                                
018300 01  GODK-STATUSKODER.                                                    
018400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018500     SKIP3                                                                
018600 01  SSA1                        PIC X(600).                              
018600 01  SSA2                        PIC X(600).                              
018700     EJECT                                                                
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200 01  FILLER                 PIC X(16)   VALUE 'WDQ201-IO-AREA'.           
019300                                                                          
019400 01  DLI-IO-AREA-WDQ201.                                                  
019500*    03  -COPY WDQ201                                                     
019600     EJECT                                                                
019700 01  FILLER                 PIC X(16)   VALUE 'WDQ212-IO-AREA'.           
019800                                                                          
019900 01  DLI-IO-AREA-WDQ212.                                                  
020000*    03  -COPY WDQ212                                                     
020100                                                                          
020200 01  FILLER                 PIC X(16)   VALUE 'WDQ221-IO-AREA'.           
020300                                                                          
020400 01  DLI-IO-AREA-WDQ221.                                                  
020500*    03  -COPY WDQ221                                                     
020600                                                                          
020700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020800 01   DLI-IO-AREA-B601.                                                   
020900*     03  -COPY WDB601                                                    
021000     EJECT                                                                
021100 LINKAGE SECTION.                                                         
021200*01  -COPY W0009      -PRE MSG-                                           
021300                                                                          
021400*01  -COPY W0009      -PRE ALT-                                           
021500     EJECT                                                                
021600*01  -COPY W0008      -PRE USEA-                                          
021700     05  FILLER                  PIC X.                                   
021800                                                                          
021900*01  -COPY W0008      -PRE ORQI-                                          
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008      -PRE WDB6-                                          
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  USEA-PCB                     
022600                           ORQI-PCB WDB6-PCB.                             
022700 MAIN SECTION.                                                            
022800     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  USEA-PCB                     
022900                           ORQI-PCB WDB6-PCB.                             
023000                                                                          
023100     PERFORM IMS-GET-MSG                                                  
023200     IF SEGMENT-FINNS                                                     
023300       PERFORM A-INIT                                                     
023400       PERFORM B-KOLLA-NYCKLAR                                            
023500       IF NYCKLAR-OK                                                      
023600           IF MFS-FIRST                                                   
023700             PERFORM C-FOERSTA-SIDA                                       
023800           ELSE                                                           
023900             IF MFS-NEXT                                                  
024000               PERFORM D-NAESTA-SIDA                                      
024100             ELSE                                                         
024200               IF MFS-PREVIOUS                                            
024300                 PERFORM I-PREV-SIDA                                      
024400               ELSE                                                       
024500                 PERFORM E-SAMMA-SIDA                                     
024600               END-IF                                                     
024700             END-IF                                                       
024800           END-IF                                                         
024900           IF STARTA-ANNAN-BILD                                           
025000              CONTINUE                                                    
025100           ELSE                                                           
025200              PERFORM F-LAES-VISA-INFO                                    
025300           END-IF                                                         
025400       END-IF                                                             
025500       IF STARTA-ANNAN-BILD                                               
025600          CONTINUE                                                        
025700       ELSE                                                               
025800          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51301 + 4                   
025900          PERFORM IMS-INSERT-MSG                                          
026000       END-IF                                                             
026100     END-IF                                                               
026200                                                                          
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700                                                                          
026800 A-INIT SECTION.                                                          
026900                                                                          
027000     IF MSG-DUBBLA-TRANSKODER                                             
027100       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
027200                               TO MID-W4I51301                            
027300       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
027400       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
027500     ELSE                                                                 
027600       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
027700                               TO MID-W4I51301                            
027800       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
027900       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
028000     END-IF                                                               
028100                                                                          
028200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
028300     MOVE MSG-IDPFK            TO MFS-IDPFK                               
028400     MOVE MFS-IDTRANS          TO W-IDTRANS                               
028500                                                                          
028600     MOVE LOW-VALUE            TO MSG-AREA                                
028700     MOVE 'W4O513N1'           TO MFS-IDMOD                               
028800     MOVE '4513'               TO MOD-IDTRANS                             
028900     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
029000                                                                          
029100     IF NOT EGEN-MID                                                      
029200       MOVE SPACE              TO MFS-KDTRTYP                             
029300       MOVE '7'                TO MFS-IDPFK                               
029400     END-IF                                                               
029500                                                                          
029600     .                                                                    
029700     EJECT                                                                
029800                                                                          
029900 B-KOLLA-NYCKLAR SECTION.                                                 
030000                                                                          
030100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030200     MOVE '001'             TO MSGI-KDCALL                                
030300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030400     MOVE '4513'            TO MSGI-IDTRANS                               
030500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030600     IF EGEN-MID                                                          
030700        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
030800        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
030900     END-IF                                                               
031000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031100     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
031200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
031300                                                                          
031400     MOVE JA TO NYCKLAR-SW                                                
031500                                                                          
031600     PERFORM BA-KOLLA-DISTRIKT                                            
031700     PERFORM BB-KOLLA-KUND                                                
031800     PERFORM BC-KOLLA-DC                                                  
031900     PERFORM BD-KOLLA-ORDERKLASS                                          
032000     PERFORM BF-KOLLA-STATUS                                              
032100                                                                          
032200     IF NYCKLAR-FEL                                                       
032300        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
032400        CALL WMEDKONV USING MED-WMEDAREA                                  
032500        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
032600     END-IF                                                               
032700     PERFORM MFS-RENSA-BILD                                               
032800     .                                                                    
032900     EJECT                                                                
033000                                                                          
033100 BA-KOLLA-DISTRIKT SECTION.                                               
033200                                                                          
033300     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
033400                                                                          
033500     IF MID-IDDISTR-IN         NOT = ALL '+'                              
033600       MOVE '7'                TO MFS-IDPFK                               
033700       MOVE SPACE              TO MFS-KDTRTYP                             
033800     END-IF                                                               
033900     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
034000       MOVE MSGI-IDDISTR       TO W-IDDISTR-MIN                           
034100                                  W-IDDISTR-MAX                           
034200     ELSE                                                                 
034300       MOVE NEJ                TO NYCKLAR-SW                              
034400     END-IF                                                               
034500                                                                          
034600     MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                          
034700     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
034800     .                                                                    
034900     EJECT                                                                
035000                                                                          
035100 BB-KOLLA-KUND SECTION.                                                   
035200                                                                          
035300     MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR-IN                         
035400                                                                          
035500     IF MID-IDKUNDNR-IN        NOT = ALL '+'                              
035600       MOVE '7'                TO MFS-IDPFK                               
035700       MOVE SPACE              TO MFS-KDTRTYP                             
035800     END-IF                                                               
035900                                                                          
036000     IF MSGI-IDKUNDNR          = SPACE                                    
036100       MOVE +0                 TO W-IDKUNDNR-MIN                          
036200       MOVE +9999999           TO W-IDKUNDNR-MAX                          
036300     ELSE                                                                 
036400       IF MSGI-IDKUNDNR NUMERIC                                           
036500           MOVE MSGI-IDKUNDNR  TO W-IDKUNDNR-MIN                          
036600                                  W-IDKUNDNR-MAX                          
036700       ELSE                                                               
036800         MOVE NEJ              TO NYCKLAR-SW                              
036900       END-IF                                                             
037000     END-IF                                                               
037100                                                                          
037200     MOVE MSGI-IDKUNDNR          TO MOD-IDKUNDNR-UT                       
037300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
037400     IF MSGI-IDKUNDNR            = ZERO                                   
037500        MOVE '     0'            TO MOD-IDKUNDNR-UT                       
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000 BC-KOLLA-DC SECTION.                                                     
038100                                                                          
038200     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
038300                                                                          
038400     IF MID-IDDC-IN = ALL '+'                                             
038500        MOVE MID-IDDC-UT       TO W-IDDC-B6                               
038600     ELSE                                                                 
038700       MOVE MID-IDDC-IN        TO W-IDDC-B6                               
038800       MOVE '7'                TO MFS-IDPFK                               
038900       MOVE SPACE              TO MFS-KDTRTYP                             
039000     END-IF                                                               
039100     PERFORM IMS-GU-WDB601                                                
039200                                                                          
039300     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
039400        MOVE MSGI-IDDC    TO MOD-IDDC-UT                                  
039500                             W-IDDC                                       
039800     ELSE                                                                 
039900        MOVE DCS-IDDC     TO MOD-IDDC-UT                                  
040000                             W-IDDC                                       
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600                                                                          
040700 BD-KOLLA-ORDERKLASS SECTION.                                             
040800                                                                          
040900     MOVE MFS-RENSA-FAELT      TO MOD-KDORDKL-IN                          
041000                                                                          
041100     IF MID-KDORDKL-IN NOT = ALL '+'                                      
041200       MOVE '7'                TO MFS-IDPFK                               
041300       MOVE SPACE              TO MFS-KDTRTYP                             
041400       MOVE MID-KDORDKL-IN     TO WS-KDORDKL                              
041500     ELSE                                                                 
041600       IF MID-IDDISTR-IN = ALL '+' AND                                    
041700          MID-IDKUNDNR-IN = ALL '+' AND                                   
041800          MID-IDDC-IN     = ALL '+'                                       
041900          MOVE MID-KDORDKL-UT  TO WS-KDORDKL                              
042000       ELSE                                                               
042100          MOVE SPACE           TO WS-KDORDKL                              
042200       END-IF                                                             
042300                                                                          
042400       IF  NOT EGEN-MID                                                   
042500       AND (SPAR-KDORDKL = '0' OR '1' OR '2' OR                           
042600                           '3' OR '4' OR SPACE)                           
042700         MOVE SPAR-KDORDKL       TO WS-KDORDKL                            
042800       END-IF                                                             
042900     END-IF                                                               
043000     IF WS-KDORDKL = '0' OR '1' OR '2' OR '3' OR '4' OR SPACE             
043100        IF WS-KDORDKL NUMERIC                                             
043200          MOVE WS-KDORDKL      TO WS-KDORDKL-NUM                          
043300                                  SPAR-KDORDKL                            
043400        ELSE                                                              
043500          MOVE SPACE           TO SPAR-KDORDKL                            
043600        END-IF                                                            
043700     ELSE                                                                 
043800        MOVE SPACE             TO SPAR-KDORDKL                            
043900        MOVE NEJ               TO NYCKLAR-SW                              
044000     END-IF                                                               
044100                                                                          
044200     MOVE WS-KDORDKL           TO MOD-KDORDKL-UT                          
044300     .                                                                    
044400     EJECT                                                                
044500                                                                          
044600 BF-KOLLA-STATUS SECTION.                                                 
044700                                                                          
044800     MOVE MFS-RENSA-FAELT      TO MOD-KDORDSTA-IN                         
044900                                                                          
045000     IF MID-KDORDSTA-IN = ALL '+'                                         
045100       MOVE MID-KDORDSTA-UT    TO WS-KDORDSTA                             
045200     ELSE                                                                 
045300       MOVE MID-KDORDSTA-IN    TO WS-KDORDSTA                             
045400       MOVE '7'                TO MFS-IDPFK                               
045500       MOVE SPACE              TO MFS-KDTRTYP                             
045600     END-IF                                                               
045700                                                                          
045800     IF  NOT EGEN-MID                                                     
045900     AND (SPAR-KDORDSTA = 'B' OR 'C' OR 'E' OR 'R' OR                     
046000                          'U' OR 'P' OR 'F' OR 'L' OR 'S')                
046100         MOVE SPAR-KDORDSTA    TO WS-KDORDSTA                             
046200     END-IF                                                               
046300                                                                          
046400     IF WS-KDORDSTA = 'B' OR 'C' OR 'E' OR 'R' OR                         
046500                      'U' OR 'P' OR 'F' OR 'L' OR 'S'                     
046600        MOVE WS-KDORDSTA       TO W-KDORDSTA-SOK                          
046700                                  SPAR-KDORDSTA                           
046800     ELSE                                                                 
046900        MOVE SPACE             TO SPAR-KDORDSTA                           
047000        MOVE NEJ               TO NYCKLAR-SW                              
047100     END-IF                                                               
047200                                                                          
047300     MOVE WS-KDORDSTA          TO MOD-KDORDSTA-UT                         
047400     .                                                                    
047500     EJECT                                                                
047600 C-FOERSTA-SIDA SECTION.                                                  
047700     INITIALIZE SPAR-AREA                                                 
047800     MOVE 01                    TO PGNO                                   
047900     MOVE JA                    TO FIRST-SW                               
048000     MOVE INF-FIRST-PAGE        TO MED-IDMFSFEL                           
048100     CALL WMEDKONV           USING MED-WMEDAREA                           
048200     MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                           
048300                                                                          
048400     MOVE ZERO                  TO W-IDORDNR7-MIN                         
048500     .                                                                    
048600     SKIP3                                                                
048700 D-NAESTA-SIDA SECTION.                                                   
048800                                                                          
048900     IF SPAR-IDTRANS = '4513'                                             
049000        MOVE SPAR-WDQ2CSEQ-NEXT        TO W-WDQ2CSEQ-MIN-X                
049100        IF SPAR-WDQ2CSEQ-ENTER(PGNO) NOT = SPAR-WDQ2CSEQ-NEXT             
049200          IF PGNO = 20                                                    
049300            PERFORM VARYING PGNO FROM 1 BY 1                              
049400              UNTIL PGNO = 20                                             
049500              MOVE SPAR-WDQ2CSEQ-ENTER(PGNO + 1) TO                       
049600                                    SPAR-WDQ2CSEQ-ENTER(PGNO)             
049700            END-PERFORM                                                   
049800            MOVE NEJ             TO FIRST-SW                              
049900          ELSE                                                            
050000             ADD 1 TO PGNO                                                
050100          END-IF                                                          
050200        END-IF                                                            
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 I-PREV-SIDA SECTION.                                                     
050700                                                                          
050800     IF SPAR-IDTRANS = '4513'                                             
050900       IF PGNO > 1                                                        
051000         COMPUTE PGNO = PGNO - 1                                          
051100         MOVE SPAR-WDQ2CSEQ-ENTER(PGNO) TO W-WDQ2CSEQ-MIN-X               
051200       ELSE                                                               
051300          MOVE SPAR-WDQ2CSEQ-ENTER(1)   TO W-WDQ2CSEQ-MIN-X               
051400          IF FIRST-SW = JA                                                
051500             MOVE INF-FIRST-PAGE         TO MED-IDMFSFEL                  
051600             CALL WMEDKONV            USING MED-WMEDAREA                  
051700             MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                  
051800          ELSE                                                            
051900             MOVE INF-NO-MORE-F6         TO MED-IDMFSFEL                  
052000             CALL WMEDKONV            USING MED-WMEDAREA                  
052100             MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                  
052200          END-IF                                                          
052300       END-IF                                                             
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 E-SAMMA-SIDA SECTION.                                                    
052800     IF SPAR-IDTRANS = '4513'                                             
052900        MOVE SPAR-WDQ2CSEQ-ENTER(PGNO) TO W-WDQ2CSEQ-MIN-X                
053000                                                                          
053100       MOVE +1                   TO INDX                                  
053200       PERFORM UNTIL INDX        >  MAX-MOD-INDX                          
053300         IF MID-IDTRANS(INDX)    =  ALL '+'                               
053400            CONTINUE                                                      
053500         ELSE                                                             
053600            IF MID-IDTRANS(INDX) NUMERIC                                  
053700               PERFORM EA-STARTA-ANNAN-BILD                               
053800               MOVE JA            TO SW-STARTA-ANNAN-BILD                 
053900               MOVE MAX-MOD-INDX  TO INDX                                 
054000            END-IF                                                        
054100         END-IF                                                           
054200         ADD +1                   TO INDX                                 
054300       END-PERFORM                                                        
054400     ELSE                                                                 
054500        INITIALIZE SPAR-AREA                                              
054600        MOVE 1                   TO PGNO                                  
054700        MOVE JA                  TO FIRST-SW                              
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 EA-STARTA-ANNAN-BILD  SECTION.                                           
055200                                                                          
055300     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
055400     MOVE MID-IDKUNDNR(INDX)     TO MSGI-IDKUNDNR                         
055500     INSPECT MID-IDORDNR7(INDX) REPLACING LEADING SPACE BY ZERO           
055600     MOVE MID-IDORDNR7(INDX)     TO MSGI-IDKUNDRF(1:7)                    
055700     MOVE '001'                  TO MSGI-KDCALL                           
055800     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
055900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
056000                                                                          
056100     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
056200     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
056300     MOVE MID-IDTRANS(INDX) (1:1)  TO W-HOPP-IDTRANS-2                    
056400     MOVE MID-IDTRANS(INDX) (2:3)  TO W-HOPP-IDTRANS-4-6                  
056500     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
056600     MOVE '4513'                 TO P-TO-P-IDTRANS                        
056700     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
056800                                                                          
056900     PERFORM S01-INSERT-ALTMSG                                            
057000                                                                          
057100     MOVE '002'             TO MSGI-KDCALL                                
057200     MOVE '4513'            TO SPAR-IDTRANS                               
057300     MOVE SPAR-AREA         TO MSGI-SPAR-AREA                             
057400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
057500     .                                                                    
057600     EJECT                                                                
057700 F-LAES-VISA-INFO SECTION.                                                
057800                                                                          
057900     MOVE +0                   TO MOD-INDX                                
058000     PERFORM IMS-GN-WDQ201-CSEQ                                           
058100                                                                          
058200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                        
058300                   MOD-INDX > MAX-MOD-INDX                                
058400                                                                          
058500        PERFORM FC-VALIDATE-FIELDS                                        
058600                                                                          
058700        IF  ORDER-AKTUELL                                                 
058800            PERFORM FE-LAGG-UT-RADEN                                      
058900        END-IF                                                            
059000                                                                          
059100        PERFORM IMS-GN-WDQ201-CSEQ                                        
059200                                                                          
059300        IF  MOD-INDX = 14                                                 
059400        AND SEGMENT-FINNS                                                 
059500           MOVE 15             TO MOD-INDX                                
059600        END-IF                                                            
059700     END-PERFORM                                                          
059800                                                                          
059900     MOVE NEJ                  TO SW-ORDER-AKTUELL                        
060000                                                                          
060100     IF MOD-INDX = 15                                                     
060200        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                     
060300                      ORDER-AKTUELL                                       
060400                                                                          
060500           PERFORM FC-VALIDATE-FIELDS                                     
060600                                                                          
060700           IF NOT ORDER-AKTUELL                                           
060800              PERFORM IMS-GN-WDQ201-CSEQ                                  
060900           END-IF                                                         
061000        END-PERFORM                                                       
061100     ELSE                                                                 
061200        MOVE INF-LAST-PAGE-SHOWN       TO MED-IDMFSINF                    
061300        CALL WMEDKONV               USING MED-WMEDAREA                    
061400        MOVE MED-MFSINF                TO MOD-TEMFSINF                    
061500     END-IF                                                               
061600                                                                          
061700     PERFORM FF-FLER-SIDOR-ELLER-EJ                                       
061800                                                                          
061900     MOVE '002'             TO MSGI-KDCALL                                
062000     MOVE '4513'            TO SPAR-IDTRANS                               
062100     MOVE SPAR-AREA         TO MSGI-SPAR-AREA                             
062200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062300     .                                                                    
062400     EJECT                                                                
062500 FC-VALIDATE-FIELDS SECTION.                                              
062600*TO CHECK ORDER STATUS,ORDER KLASS WITH THE SCREEN VALUES &               
062700*TPOTYP SHOULD BE ZERO                                                    
062800                                                                          
062900     MOVE NEJ TO SW-ORDER-AKTUELL                                         
063000     IF  OHUV-KDTPOTYP      = ZERO  AND                                   
063100         (                                                                
063200          OHUV-KDORDKL            = WS-KDORDKL-NUM OR                     
063300          WS-KDORDKL              = SPACE                                 
063400         )                                                                
063500                                                                          
063600         PERFORM IMS-GNP-WDQ212-KDORDSTA                                  
063700         IF SEGMENT-FINNS                                                 
063800            IF WS-KDORDSTA = 'R' OR 'B' OR 'C'                            
063900               PERFORM IMS-GNP-WDQ221                                     
064000               IF SEGMENT-FINNS                                           
064100                  MOVE JA             TO SW-ORDER-AKTUELL                 
064200               END-IF                                                     
064300            ELSE                                                          
064400               MOVE JA                TO SW-ORDER-AKTUELL                 
064500            END-IF                                                        
064600         END-IF                                                           
064700                                                                          
064800     END-IF                                                               
064900     .                                                                    
065000 FE-LAGG-UT-RADEN SECTION.                                                
065100                                                                          
065200     ADD +1                      TO MOD-INDX                              
065300     IF MOD-INDX = +1                                                     
065400        MOVE OHUV-IDDISTR        TO W-IDDISTR-SPAR                        
065500        MOVE OHUV-IDKUNDNR       TO W-IDKUNDNR-SPAR                       
065600        MOVE OHUV-IDORDNR7       TO W-IDORDNR7-SPAR                       
065700        MOVE W-WDQ2CSEQ-X        TO SPAR-WDQ2CSEQ-ENTER(PGNO)             
065800     END-IF                                                               
065900                                                                          
066000     MOVE OHUV-IDKUNDNR          TO MOD-IDKUNDNR(MOD-INDX)                
066100     MOVE OHUV-IDORDNR7          TO MOD-IDORDNR7(MOD-INDX)                
066200     MOVE W-IDDC                 TO MOD-IDDC(MOD-INDX)                    
066300     MOVE OHUV-TIREGDAT          TO MOD-TIREGDAT(MOD-INDX)                
066400     MOVE OHUV-KDORDKL           TO MOD-KDORDKL(MOD-INDX)                 
066500     MOVE ARB-KDORDSTA           TO MOD-KDORDSTA(MOD-INDX)                
066600     .                                                                    
066700     EJECT                                                                
066800 FF-FLER-SIDOR-ELLER-EJ SECTION.                                          
066900                                                                          
067000     IF ORDER-AKTUELL                                                     
067100        MOVE OHUV-IDDISTR        TO W-IDDISTR-SPAR                        
067200        MOVE OHUV-IDKUNDNR       TO W-IDKUNDNR-SPAR                       
067300        MOVE OHUV-IDORDNR7       TO W-IDORDNR7-SPAR                       
067400        MOVE W-WDQ2CSEQ-X        TO SPAR-WDQ2CSEQ-NEXT                    
067500        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
067600        CALL WMEDKONV USING MED-WMEDAREA                                  
067700        MOVE MED-MFSINF          TO MOD-TEMFSINF                          
067800     ELSE                                                                 
067900        IF MOD-INDX = +0                                                  
068000           MOVE INF-INFO-SAKNAS  TO MED-IDMFSINF                          
068100           CALL WMEDKONV USING MED-WMEDAREA                               
068200           MOVE MED-MFSINF       TO MOD-TEMFSINF                          
068300           MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL                          
068400           MOVE W-WDQ2CSEQ-MIN-X TO SPAR-WDQ2CSEQ-NEXT                    
068500                                    SPAR-WDQ2CSEQ-ENTER(PGNO)             
068600        ELSE                                                              
068700           MOVE W-WDQ2CSEQ-X     TO SPAR-WDQ2CSEQ-NEXT                    
068800        END-IF                                                            
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 S01-INSERT-ALTMSG SECTION.                                               
069300                                                                          
069400     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
069500     PERFORM IMS-CHANGE-ALTMSG                                            
069600     IF STATUS-OK                                                         
069700       PERFORM IMS-INSERT-ALTMSG                                          
069800     ELSE                                                                 
069900       MOVE LOW-VALUE          TO MSG-AREA                                
070000       MOVE 'W4O51301'         TO MFS-IDMOD                               
070100       MOVE '4513'             TO MOD-IDTRANS                             
070200       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
070300       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
070400       IF SECURITY-FEL                                                    
070500         STRING 'NOT AUTHORIZED TO USE '                                  
070600                W-BILD                                                    
070700                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
070800       ELSE                                                               
070900         STRING 'WRONG PICTURE '                                          
071000                 W-BILD                                                   
071100                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
071200       END-IF                                                             
071300       PERFORM MFS-ROER-EJ-BILD                                           
071400       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51301 + 4                      
071500       PERFORM IMS-INSERT-MSG                                             
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 MFS-ROER-EJ-BILD SECTION.                                                
072000                                                                          
072100     MOVE +1 TO MOD-INDX                                                  
072200     PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                                
072300        MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR (MOD-INDX)                 
072400                                  MOD-IDORDNR7 (MOD-INDX)                 
072500                                  MOD-IDDC     (MOD-INDX)                 
072600                                  MOD-TIREGDAT (MOD-INDX)                 
072700                                  MOD-KDORDKL  (MOD-INDX)                 
072800                                  MOD-KDORDSTA (MOD-INDX)                 
072900        ADD +1 TO MOD-INDX                                                
073000     END-PERFORM                                                          
073100     .                                                                    
073200     EJECT                                                                
073300 MFS-RENSA-BILD SECTION.                                                  
073400                                                                          
073500     MOVE +1 TO MOD-INDX                                                  
073600     PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                                
073700        MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR (MOD-INDX)                   
073800                                MOD-IDORDNR7 (MOD-INDX)                   
073900                                MOD-IDDC     (MOD-INDX)                   
074000                                MOD-TIREGDAT (MOD-INDX)                   
074100                                MOD-KDORDKL  (MOD-INDX)                   
074200                                MOD-KDORDSTA (MOD-INDX)                   
074300        ADD +1 TO MOD-INDX                                                
074400     END-PERFORM                                                          
074500     .                                                                    
074600     EJECT                                                                
074700* --- IMS SEKTIONER ---                                                   
074800     SKIP3                                                                
074900 IMS-GET-MSG SECTION.                                                     
075000                                                                          
075100     MOVE '  QC'          TO GODK-STATUSKODER                             
075200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600     SKIP3                                                                
075700 IMS-INSERT-MSG SECTION.                                                  
075800                                                                          
075900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
076000       MOVE '0'           TO MFS-KDHUVOMR                                 
076100     END-IF                                                               
076200     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
076300     MOVE SPACE           TO GODK-STATUSKODER                             
076400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     EJECT                                                                
076900                                                                          
077000 IMS-CHANGE-ALTMSG SECTION.                                               
077100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
077200     MOVE '  A1A4' TO GODK-STATUSKODER                                    
077300     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
077400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     .                                                                    
077700     SKIP3                                                                
077800 IMS-INSERT-ALTMSG SECTION.                                               
077900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
078000     MOVE SPACE TO GODK-STATUSKODER                                       
078100     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
078200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
078300     PERFORM IMS-STATUSKONTROLL                                           
078400     .                                                                    
078500     EJECT                                                                
078600 IMS-GN-WDQ201-CSEQ  SECTION.                                             
078700                                                                          
078800     STRING 'WLORQI01(WDQ2CSEQ>=' W-WDQ2CSEQ-MIN-X                        
078900                    '&WDQ2CSEQ<=' W-WDQ2CSEQ-MAX-X ')'                    
079000          DELIMITED BY SIZE INTO SSA1                                     
079100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
079200     CALL CBLTDLI USING GN  ORQI-PCB DLI-IO-AREA-WDQ201 SSA1              
079300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
079400     PERFORM IMS-STATUSKONTROLL                                           
079500     .                                                                    
079600     SKIP3                                                                
079700 IMS-GNP-WDQ212-KDORDSTA SECTION.                                         
079800                                                                          
079900     STRING 'WLORQI12(IDDC     =' W-IDDC-X                                
080000                    '&KDORDST1 =' W-KDORDSTA-SOK-X ')'                    
080100          DELIMITED BY SIZE INTO SSA1                                     
080200     MOVE '  GE'              TO GODK-STATUSKODER                         
080300     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-WDQ212 SSA1              
080400     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
080500     PERFORM IMS-STATUSKONTROLL                                           
080600     .                                                                    
080700     EJECT                                                                
080800 IMS-GNP-WDQ221 SECTION.                                                  
080900                                                                          
081000     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
081100          DELIMITED BY SIZE INTO SSA1                                     
081200     MOVE   'WLORQI21'        TO SSA2                                     
081300     MOVE '  GE'              TO GODK-STATUSKODER                         
081400     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-WDQ221 SSA1 SSA2         
081500     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     EJECT                                                                
081900 IMS-GU-WDB601    SECTION.                                                
082000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
082100          DELIMITED BY SIZE INTO SSA1                                     
082200     MOVE '  GE' TO GODK-STATUSKODER                                      
082300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
082400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
082500     PERFORM IMS-STATUSKONTROLL                                           
082600     IF SEGMENT-SAKNAS                                                    
082700         MOVE SPACE TO DCS-KDDC                                           
082800     END-IF                                                               
082900     .                                                                    
083000 IMS-STATUSKONTROLL SECTION.                                              
083100                                                                          
083200     SET STATUS-IX TO 1                                                   
083300     SEARCH GODK-STATUS                                                   
083400       AT END                                                             
083500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
083600         DELIMITED BY SIZE INTO FELTEXT                                   
083700         CALL FELLOG                                                      
083800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
083900         CONTINUE                                                         
084000     END-SEARCH                                                           
084100     .                                                                    
