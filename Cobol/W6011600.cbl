000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011600.                                                
000300 AUTHOR.                                                                  
000400 DATE-WRITTEN.   LARS THELL.                                              
000500 DATE-COMPILED.  92/02/25.                                                
000600*                                                                         
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        MFS-HANTERING AV ERROR MSG FRÅN W6011610.                        
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W6T116                                              
001300*        MID:         W6I11601                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W6O11601                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300*    -- CHECKED BY WY2000                                                 
002400     SKIP3                                                                
002500 77  IDPGM                       PIC X(08)   VALUE 'W6011600'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FILLER                      PIC X(8)  VALUE 'ERRORTEX'.              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  YES                         PIC X       VALUE 'Y'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +901  COMP SYNC.        
004000 77  WS-RAD-IX                   PIC S9(9)  VALUE +0    COMP SYNC.        
004100 77  RAD-IX1                     PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  MAX-KVRADER                 PIC S9(4)  VALUE +36   COMP SYNC.        
004300 77  W-IDLEVNR                   PIC  X(5)  VALUE SPACE.                  
004400 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
004500 77  INDX-DISPLAY                PIC  9(9)  VALUE  0.                     
004600                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA VÄRDEN FRÅN SKÄRMEN                      
004800                                                                          
004900 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005000 77  WS-KDRT                     PIC X(2)    VALUE SPACE.                 
005100 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
005200 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
005300 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
005400 77  WS-IDFTG                    PIC X(2)    VALUE SPACE.                 
005500 77  WS-IDKONTO                  PIC X(10)   VALUE SPACE.                 
005600 77  WS-IDKONTO-NUM              PIC 9(10)   VALUE ZERO.                  
005700 77  WS-IDANALYS                 PIC X(12)   VALUE SPACE.                 
005800 77  WS-IDANALYS-NUM             PIC 9(12)   VALUE ZERO.                  
005900 77  WS-IDKST                    PIC X(10)   VALUE SPACE.                 
006000 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
006100 77  WS-FLGODK                   PIC X(1)    VALUE SPACE.                 
006200 77  FL-IDFS-OK                  PIC X       VALUE 'N'.                   
006300 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
006400 77  WS-KVAVIS                   PIC 9(06)   VALUE ZERO.                  
006500 77  WS-IDARTNR                  PIC 9(08)   VALUE ZERO.                  
006600                                                                          
006700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006800     88  EGEN-MID                            VALUE '6116'.                
006900     88  GODK-MID                            VALUE '6111' '6112'          
007000                                                   '6113' '6114'          
007100                                                   '6115' '6116'          
007200                                                   '6118' '6119'.         
007300     88  HELP-MID                            VALUE '0551'.                
007400       EJECT                                                              
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  W611REG                 PIC X(8)    VALUE 'W611REG'.             
008100     03  W411SAP                 PIC X(8)    VALUE 'W411SAP'.             
008200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008300     03  W6011610                PIC X(8)    VALUE 'W6011610'.            
008400     EJECT                                                                
008500*    --- PARAMETERS TO ABEND                                              
008600                                                                          
008700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009000     SKIP3                                                                
009100*01 -COPY WMSGINIT                                                        
009200     SKIP3                                                                
009300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009400*01 -COPY WMEDAREA                                                        
009500*                                                                         
009600*                                                                         
009900 01  MESSAGE-CODES.                                                       
010000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  ERR-IN-LINE-ONE         PIC X(3)    VALUE '184'.                 
010700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011000     03  ERR-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
011100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011300     03  ERR-IDFS-ALREADY-EXISTS PIC X(3)    VALUE '201'.                 
011400     03  INF-AVROP-SAKNAS        PIC X(3)    VALUE '285'.                 
011500     03  INF-PART-SUPERSEDED     PIC X(3)    VALUE '220'.                 
011600     03  SAP-ACCOUNT-MISSING-IN-R3    PIC X(3) VALUE '346'.               
011700     03  SAP-ANALYS-NO-MISSING-IN-R3  PIC X(3) VALUE '347'.               
011800     03  SAP-COSTCENTER-MISSING-IN-R3 PIC X(3) VALUE '348'.               
012000     03  SAP-ACCOUNT-MUST-BE-REG      PIC X(3) VALUE '350'.               
012100     03  SAP-COSTCENTER-NOT-ALLOWED   PIC X(3) VALUE '351'.               
012200     03  SAP-ANALYSIS-NO-NOT-ALLOWED  PIC X(3) VALUE '352'.               
012400     03  SAP-ANAL-CC-MUST-BE-ENTERED  PIC X(3) VALUE '353'.               
012500     03  ERR-PRICE-IS-MISSING         PIC X(3) VALUE '301'.               
012510     03  ERR-WRNG-CURRENCY            PIC X(3) VALUE '151'.               
012600     03  ERR-PART-MISSING             PIC X(3) VALUE '017'.               
012700     03  ERR-PART-OTHER-COMPANY       PIC X(3) VALUE '088'.               
012800                                                                          
012900 01 WS-IDMSG-ERROR               PIC X(3).                                
013000     88  R-ERR-EXEC-AND-NO-DATA                VALUE '014'.               
013100     88  R-CORR-HILITE-FLDS                    VALUE '020'.               
013200     88  R-ERR-WRONG-KEY                       VALUE '022'.               
013300     88  R-ERR-NOT-FOUND                       VALUE '025'.               
013400     88  R-ERR-IDFS-ALREADY-EXISTS             VALUE '030'.               
013500     88  R-ERR-AVROP-AVISERAT                  VALUE '285'.               
013600     88  R-ERR-IN-LINE-ONE                     VALUE '337'.               
013700     88  R-ERR-PART-SUPERSEDED                 VALUE '223'.               
013800     88  R-SAP-ACCOUNT-MISSING-IN-R3           VALUE '338'.               
013900     88  R-SAP-ACCOUNT-MUST-BE-REG             VALUE '339'.               
014000     88  R-SAP-COSTCENTER-MISSING-IN-R3        VALUE '341'.               
014100     88  R-SAP-COSTCENTER-NOT-ALLOWED          VALUE '340'.               
014200     88  R-SAP-ANALYSIS-NO-NOT-ALLOWED         VALUE '342'.               
014300     88  R-SAP-ANALYS-NO-MISSING-IN-R3         VALUE '343'.               
014400     88  R-SAP-ANAL-CC-MUST-BE-ENTERED         VALUE '344'.               
014500     88  R-ERR-UPDATE-NOT-DONE                 VALUE '004'.               
014600     88  R-ERR-PRICE-IS-MISSING                VALUE '260'.               
014610     88  R-ERR-WRNG-CURRENCY                   VALUE '426'.               
014700     88  R-ERR-PART-MISSING                    VALUE '041'.               
014800     88  R-ERR-PART-OTHER-COMPANY              VALUE '360'.               
014900                                                                          
015000 01 WS-IDMSG-INFO               PIC X(3).                                 
015100     88  R-INF-UPDATE-DONE                     VALUE '001'.               
015200     88  R-INF-AVROP-SAKNAS                    VALUE '336'.               
015400     88  R-INF-ALREADY-EXISTS                  VALUE '030'.               
015500     EJECT                                                                
015600*                                                                         
015700*                                                                         
015800     EJECT                                                                
015900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016000*                                                                         
016100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016200     SKIP3                                                                
016300*01  MID -COPY W6I11601                                                   
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016600     SKIP3                                                                
016700*01  -COPY WMSGAREA                                                       
016800     EJECT                                                                
016900     03  MOD REDEFINES MSG-AREA.                                          
017000*      05  -COPY W6O11601                                                 
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017300     SKIP3                                                                
017400*01  -COPY WMFSAREA                                                       
017500     EJECT                                                                
017600*    --- AREOR TILL W6011160 SUBPROGRAM                                   
017700*                                                                         
017800 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
017900 01  REQU-AREA.                                                           
018000*    03 -COPY WZ01REQU                                                    
018100*    03 -COPY W60116I1                                                    
018200     SKIP3                                                                
018300                                                                          
018400 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
018500 01  RESP-AREA.                                                           
018600*    03 -COPY WZ01RESP                                                    
018700*    03 -COPY W60116O1                                                    
018800     SKIP3                                                                
018900*                                                                         
019000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019100*                                                                         
019200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019300 01  SSA1                        PIC X(256).                              
019400                                                                          
019500 01  STATUS-WS                   PIC XX.                                  
019600     88 SEGMENT-FINNS                        VALUE '  '.                  
019700     88 SEGMENT-SAKNAS                       VALUE 'GE'.                  
019800     88 BASEN-SLUT                           VALUE 'GB'.                  
019900                                                                          
020000 01  GODK-STATUSKODER.                                                    
020100     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
020200                                                                          
020300     EJECT                                                                
020400*    --- IMS FUNKTIONSKODER                                               
020500*01  -COPY W0003                                                          
020600     EJECT                                                                
020700 LINKAGE SECTION.                                                         
020800                                                                          
020900 01 -COPY W0009     -PRE MSG-                                             
021000                                                                          
021100 01  USEA-PCB                    PIC X.                                   
021200                                                                          
021300*   PCB'ER FÖR SUB PGM W611610                                            
021400                                                                          
021500 01  WDB6-LEV-PCB                PIC X.                                   
021600                                                                          
021700 01  WDB6-PCB                    PIC X.                                   
021800                                                                          
021900*   PCB'ER FÖR SUB PGM W611REG                                            
022000                                                                          
022100 01  REG-INLA1-PCB               PIC X.                                   
022200                                                                          
022300 01  REG-INLA2-PCB               PIC X.                                   
022400                                                                          
022500 01  REG-INLA3-PCB               PIC X.                                   
022600                                                                          
022700 01  REG-LEVA-PCB                PIC X.                                   
022800                                                                          
022900 01  REG-ARTC-PCB                PIC X.                                   
023000                                                                          
023100 01  REG-BENA-PCB                PIC X.                                   
023200                                                                          
023300 01  REG-INLB-PCB                PIC X.                                   
023400                                                                          
023500 01  REG-ARTS-PCB                PIC X.                                   
023510                                                                          
023520 01  REG-WDK7-PCB                PIC X.                                   
023600                                                                          
023610 01  REG-WDB6-PCB                PIC X.                                   
023620                                                                          
023700*   PCB'ER FÖR SUB PGM W411SAP                                            
023800                                                                          
023900 01  SAP-SAPC-PCB              PIC X.                                     
024000                                                                          
024010*01    -COPY W0008     -PRE 9305-                                         
024020     05  FILLER                  PIC X(30).                               
024030                                                                          
024100     EJECT                                                                
024200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
024300                           WDB6-LEV-PCB   WDB6-PCB                        
024400                           REG-INLA1-PCB  REG-INLA2-PCB                   
024500                           REG-INLA3-PCB  REG-LEVA-PCB                    
024600                                          REG-ARTC-PCB                    
024700                           REG-BENA-PCB   REG-INLB-PCB                    
024800                           REG-ARTS-PCB   REG-WDK7-PCB                    
024810                           REG-WDB6-PCB                                   
024900                           SAP-SAPC-PCB                                   
024910                           9305-PCB.                                      
025000 MAIN SECTION.                                                            
025100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
025200                           WDB6-LEV-PCB   WDB6-PCB                        
025300                           REG-INLA1-PCB  REG-INLA2-PCB                   
025400                           REG-INLA3-PCB  REG-LEVA-PCB                    
025500                                          REG-ARTC-PCB                    
025600                           REG-BENA-PCB   REG-INLB-PCB                    
025700                           REG-ARTS-PCB   REG-WDK7-PCB                    
025710                           REG-WDB6-PCB                                   
025800                           SAP-SAPC-PCB                                   
025810                           9305-PCB.                                      
025900                                                                          
026000     PERFORM IMS-GET-MSG                                                  
026100     IF SEGMENT-FINNS                                                     
026200       PERFORM A-INIT                                                     
026300       PERFORM B-FLYTTA-NYCKLAR                                           
026400       PERFORM C-INIT-REQU                                                
026500*                                                                         
026600       IF MFS-ENTER AND EGEN-MID                                          
026700         PERFORM G-CALL-SUBROUTINE                                        
026800       ELSE                                                               
026900         IF HELP-MID                                                      
027000           PERFORM F-LAES-VISA-INFO                                       
027100         ELSE                                                             
027200           PERFORM MFS-RENSA-NYCKLAR                                      
027300           PERFORM MFS-RENSA-FAELT-IN                                     
027400         END-IF                                                           
027500       END-IF                                                             
027600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
027700       PERFORM IMS-INSERT-MSG                                             
027800     END-IF                                                               
027900                                                                          
028000     MOVE ZERO TO RETURN-CODE                                             
028100     GOBACK                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 A-INIT SECTION.                                                          
028500                                                                          
028600     IF MSG-DUBBLA-TRANSKODER                                             
028700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11601                 
028800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
028900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029000     ELSE                                                                 
029100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11601                  
029200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
029300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029400     END-IF                                                               
029500                                                                          
029600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
029700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
029800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
029900                                                                          
030000     MOVE LOW-VALUE TO MSG-AREA                                           
030100     MOVE 'W6O116N1' TO MFS-IDMOD                                         
030200     MOVE '6116' TO MOD-IDTRANS                                           
030300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
030400                                                                          
030500     MOVE SPACE           TO RESP-IDMSG-INFO                              
030600     MOVE SPACE           TO RESP-IDMSG-ERROR                             
030700     MOVE SPACE           TO MED-IDMFSINF MED-IDMFSFEL                    
030800                                                                          
030900     IF EGEN-MID OR HELP-MID                                              
031000       CONTINUE                                                           
031100      ELSE                                                                
031200       MOVE SPACE TO MFS-KDTRTYP                                          
031300       MOVE '7' TO MFS-IDPFK                                              
031400     END-IF                                                               
031500                                                                          
031600     PERFORM AA-INIT-NYCKLAR                                              
031700                                                                          
031800     IF MSGI-IDLAND-SPR = 'GB'                                            
031900       MOVE +2                 TO SPRAK-IX                                
032000       MOVE 'GB '              TO MED-IDSKYLT                             
032100     ELSE                                                                 
032200       MOVE +1                 TO SPRAK-IX                                
032300       MOVE 'S  '              TO MED-IDSKYLT                             
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700*----------------------------------------------------------------*        
032800 AA-INIT-NYCKLAR SECTION.                                                 
032900                                                                          
033000     MOVE ALL '+' TO MSGI-WMSGINIT                                        
033100     MOVE '013'                  TO MSGI-KDCALL                           
033200     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
033300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
033400     MOVE '6116'                 TO MSGI-IDTRANS                          
033500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033600     .                                                                    
033700     EJECT                                                                
033800 B-FLYTTA-NYCKLAR SECTION.                                                
033900                                                                          
034000     MOVE SPACE TO MED-IDMFSFEL                                           
034100                                                                          
034200     PERFORM BA-FLYTTA-IDLEVNR                                            
034300     PERFORM BB-FLYTTA-KDRT                                               
034400     PERFORM BC-FLYTTA-IDFS                                               
034500     PERFORM BD-FLYTTA-TIAVIDAT                                           
034600     PERFORM BE-FLYTTA-IDLBBET                                            
034700                                                                          
034800     PERFORM BF-FLYTTA-IDFTG                                              
034900     PERFORM BG-FLYTTA-IDKONTO                                            
035000     PERFORM BI-FLYTTA-IDANALYS                                           
035100     PERFORM BJ-FLYTTA-FLGODK                                             
035200     PERFORM BK-FLYTTA-ADINLOMR-PRT                                       
035300     PERFORM BL-FLYTTA-IDDC                                               
035400     PERFORM BN-FLYTTA-IDKST                                              
035500     PERFORM BM-FLYTTA-OEVRIGA-NYCKLAR                                    
035600                                                                          
035700     IF GODK-MID                                                          
035800       CONTINUE                                                           
035900     ELSE                                                                 
036000       PERFORM MFS-RENSA-NYCKLAR                                          
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 BA-FLYTTA-IDLEVNR SECTION.                                               
036600     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                          
036800     IF MID-IDLEVNR-IN         = ALL '+'                                  
036900         MOVE MID-IDLEVNR-UT   TO WS-IDLEVNR                              
037000     ELSE                                                                 
037100         MOVE MID-IDLEVNR-IN   TO WS-IDLEVNR                              
037200         MOVE     SPACE        TO MFS-KDTRTYP                             
037300     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 BB-FLYTTA-KDRT   SECTION.                                                
037900     MOVE MFS-RENSA-FAELT        TO MOD-KDRT-IN                           
038100     IF GODK-MID OR HELP-MID                                              
038200       IF MID-KDRT-IN          = ALL '+'                                  
038300         MOVE MID-KDRT-UT    TO WS-KDRT                                   
038400       ELSE                                                               
038500         MOVE MID-KDRT-IN    TO WS-KDRT                                   
038600         MOVE   SPACE        TO MFS-KDTRTYP                               
038700       END-IF                                                             
038800     ELSE                                                                 
038900       MOVE ZERO             TO WS-KDRT                                   
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 BC-FLYTTA-IDFS   SECTION.                                                
039600     MOVE MFS-RENSA-FAELT      TO MOD-IDFS-IN                             
039800     IF MID-IDFS-IN            = ALL '+'                                  
039900       MOVE MID-IDFS-UT      TO WS-IDFS                                   
040000     ELSE                                                                 
040100       MOVE MID-IDFS-IN      TO WS-IDFS                                   
040200       MOVE   SPACE          TO MFS-KDTRTYP                               
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600                                                                          
040700 BD-FLYTTA-TIAVIDAT SECTION.                                              
040900     MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT-IN                         
041100     IF MID-TIAVIDAT-IN        = ALL '+'                                  
041200       MOVE MID-TIAVIDAT-UT  TO WS-TIAVIDAT                               
041300     ELSE                                                                 
041400       MOVE MID-TIAVIDAT-IN  TO WS-TIAVIDAT                               
041500       MOVE   SPACE          TO MFS-KDTRTYP                               
041600     END-IF                                                               
041700                                                                          
041800     .                                                                    
041900     EJECT                                                                
042000                                                                          
042100 BE-FLYTTA-IDLBBET        SECTION.                                        
042300     MOVE MFS-RENSA-FAELT        TO MOD-IDLBBET-IN                        
042500     IF EGEN-MID OR HELP-MID                                              
042600       IF MID-IDLBBET-IN       =  ALL '+'                                 
042700         MOVE MID-IDLBBET-UT TO WS-IDLBBET                                
042800       ELSE                                                               
042900         MOVE MID-IDLBBET-IN TO WS-IDLBBET                                
043000         MOVE SPACE          TO MFS-KDTRTYP                               
043100       END-IF                                                             
043200     ELSE                                                                 
043300       MOVE SPACE            TO WS-IDLBBET                                
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 BF-FLYTTA-IDFTG          SECTION.                                        
043900     IF EGEN-MID                                                          
044000       IF MID-IDFTG-IN           = ALL '+'                                
044100         MOVE MID-IDFTG-UT       TO WS-IDFTG                              
044200         INSPECT WS-IDFTG REPLACING LEADING SPACE BY ZERO                 
044300       ELSE                                                               
044400         MOVE MFS-RENSA-FAELT     TO MOD-IDFTG-UT                         
044500         MOVE MID-IDFTG-IN        TO WS-IDFTG                             
044600         MOVE SPACE               TO MFS-KDTRTYP                          
044700       END-IF                                                             
044800     ELSE                                                                 
044900       MOVE ZERO                  TO WS-IDFTG                             
045000       MOVE MFS-RENSA-FAELT       TO MOD-IDFTG-IN                         
045100     END-IF                                                               
045200                                                                          
045300     .                                                                    
045400     EJECT                                                                
045500 BG-FLYTTA-IDKONTO          SECTION.                                      
045700     IF EGEN-MID                                                          
045800       IF MID-IDKONTO-IN           = ALL '+'                              
045900         MOVE MID-IDKONTO-UT       TO WS-IDKONTO                          
046000         MOVE MID-IDKONTO-UT       TO MOD-IDKONTO-UT                      
046100         INSPECT WS-IDKONTO REPLACING LEADING SPACE BY ZERO               
046200       ELSE                                                               
046300         MOVE MFS-RENSA-FAELT TO MOD-IDKONTO-UT                           
046400         MOVE MID-IDKONTO-IN      TO WS-IDKONTO                           
046500         MOVE SPACE               TO MFS-KDTRTYP                          
046600       END-IF                                                             
046700     ELSE                                                                 
046800       MOVE ZERO                 TO WS-IDKONTO                            
046900       MOVE MFS-RENSA-FAELT      TO MOD-IDKONTO-IN                        
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 BI-FLYTTA-IDANALYS       SECTION.                                        
047500     IF EGEN-MID                                                          
047600       IF MID-IDANALYS-IN             =  ALL '+'                          
047700         MOVE MID-IDANALYS-UT       TO WS-IDANALYS                        
047800         MOVE MID-IDANALYS-UT       TO MOD-IDANALYS-UT                    
047900         INSPECT WS-IDANALYS REPLACING LEADING SPACE BY ZERO              
048100       ELSE                                                               
048200         MOVE MFS-RENSA-FAELT       TO MOD-IDANALYS-UT                    
048300         MOVE MID-IDANALYS-IN       TO WS-IDANALYS                        
048400         MOVE SPACE                 TO MFS-KDTRTYP                        
048500       END-IF                                                             
048600     ELSE                                                                 
048700       MOVE ZERO                      TO WS-IDANALYS                      
048800       MOVE MFS-RENSA-FAELT           TO MOD-IDANALYS-IN                  
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 BJ-FLYTTA-FLGODK  SECTION.                                               
049400     MOVE MFS-RENSA-FAELT      TO MOD-FLGODK-IN                           
049600     IF EGEN-MID OR HELP-MID                                              
049700       IF MID-FLGODK-IN      = ALL '+'                                    
049800         MOVE MID-FLGODK-UT TO WS-FLGODK                                  
049900       ELSE                                                               
050000         MOVE MID-FLGODK-IN TO WS-FLGODK                                  
050100         MOVE SPACE         TO MFS-KDTRTYP                                
050200       END-IF                                                             
050300     ELSE                                                                 
050400       MOVE SPACE            TO WS-FLGODK                                 
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800                                                                          
050900 BK-FLYTTA-ADINLOMR-PRT  SECTION.                                         
051100     MOVE MFS-RENSA-FAELT          TO MOD-ADINLOMR-PRT-IN                 
051300     IF MID-ADINLOMR-PRT-IN        = ALL '+'                              
051400       MOVE MID-ADINLOMR-PRT-UT  TO WS-ADINLOMR-PRT                       
051500     ELSE                                                                 
051600       MOVE MID-ADINLOMR-PRT-IN  TO WS-ADINLOMR-PRT                       
051700       MOVE SPACE                TO MFS-KDTRTYP                           
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 BL-FLYTTA-IDDC   SECTION.                                                
052300     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
052400                                                                          
052500     IF EGEN-MID OR HELP-MID                                              
052600       IF MID-IDDC-IN = ALL '+'                                           
052700         MOVE MSGI-IDDC   TO WS-IDDC                                      
052800       ELSE                                                               
052900         MOVE MID-IDDC-IN TO WS-IDDC                                      
053000         MOVE SPACE       TO MFS-KDTRTYP                                  
053100       END-IF                                                             
053200     ELSE                                                                 
053300       IF GODK-MID                                                        
053400         MOVE MSGI-IDDC   TO WS-IDDC                                      
053500       ELSE                                                               
053600         MOVE SPACE       TO WS-IDDC                                      
053700       END-IF                                                             
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100                                                                          
054200 BM-FLYTTA-OEVRIGA-NYCKLAR  SECTION.                                      
054400     MOVE MFS-RENSA-FAELT      TO MOD-FLKLIVIS-IN                         
054500                                  MOD-IDLOPNRM-IN                         
054700     IF MID-FLKLIVIS-IN   = ALL '+'                                       
054800         MOVE MID-FLKLIVIS-UT  TO MOD-FLKLIVIS-UT                         
054900     ELSE                                                                 
055000         MOVE MID-FLKLIVIS-IN  TO MOD-FLKLIVIS-UT                         
055100     END-IF                                                               
055200                                                                          
055300     IF MID-IDLOPNRM-IN   = ALL '+'                                       
055400       MOVE MID-IDLOPNRM-UT  TO MOD-IDLOPNRM-UT                           
055500     ELSE                                                                 
055600       MOVE MID-IDLOPNRM-IN  TO MOD-IDLOPNRM-UT                           
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 BN-FLYTTA-IDKST  SECTION.                                                
056200     IF EGEN-MID                                                          
056300       IF MID-IDKST-IN           = ALL '+'                                
056400         MOVE MID-IDKST-UT       TO WS-IDKST                              
056600       ELSE                                                               
056700         MOVE MFS-RENSA-FAELT    TO MOD-IDKST-UT                          
056800         MOVE MID-IDKST-IN       TO WS-IDKST                              
056900         MOVE SPACE              TO MFS-KDTRTYP                           
057000       END-IF                                                             
057100     ELSE                                                                 
057200       MOVE SPACE                TO WS-IDKST                              
057300       MOVE MFS-RENSA-FAELT      TO MOD-IDKST-IN                          
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 C-INIT-REQU   SECTION.                                                   
058000     MOVE WS-IDLEVNR               TO REQU-IDLEVNR-KEY                    
058200     MOVE WS-IDFS                  TO REQU-IDFS-KEY                       
058300     MOVE WS-TIAVIDAT              TO REQU-TIAVIDAT-KEY                   
058400     MOVE WS-ADINLOMR-PRT          TO REQU-ADINLOMR-PRT-KEY               
058500     MOVE WS-IDDC                  TO REQU-IDDC-KEY                       
058600     MOVE WS-KDRT                  TO REQU-KDRT-KEY                       
058800     MOVE WS-IDLBBET               TO REQU-IDLBBET                        
058900     MOVE WS-IDFTG                 TO REQU-IDFTG                          
059000     MOVE WS-IDKONTO               TO REQU-IDKONTO                        
059100     MOVE WS-IDANALYS              TO REQU-IDANALYS                       
059300     MOVE WS-IDKST                 TO REQU-IDKST                          
059400     MOVE WS-FLGODK                TO REQU-FLGODK                         
059500     MOVE MAX-KVRADER              TO REQU-KVRADER                        
059600*                                                                         
059700     MOVE +1 TO RAD-IX1                                                   
059800     PERFORM UNTIL RAD-IX1    > MAX-KVRADER                               
059900       INSPECT MID-IDARTNR(RAD-IX1)                                       
060000                              REPLACING LEADING SPACE BY ZERO             
060100       MOVE MID-IDARTNR(RAD-IX1) TO REQU-IDARTNR-LINE(RAD-IX1)            
060200*                                                                         
060300       INSPECT MID-KVAVIS (RAD-IX1)                                       
060400                                 REPLACING LEADING SPACE BY ZERO          
060500       MOVE MID-KVAVIS (RAD-IX1) TO REQU-KVAVIS-LINE (RAD-IX1)            
061500       ADD +1                    TO RAD-IX1                               
061700     END-PERFORM                                                          
061800     .                                                                    
061900     EJECT                                                                
062000                                                                          
062100 F-LAES-VISA-INFO        SECTION.                                         
062200     IF MID-IDFTG-IN                =  ALL '+'                            
062300         IF MID-IDFTG-UT            =  SPACE                              
062400             MOVE MFS-RENSA-FAELT   TO MOD-IDFTG-UT                       
062500          ELSE                                                            
062600             MOVE MID-IDFTG-UT      TO MOD-IDFTG-UT                       
062700         END-IF                                                           
062800         MOVE MFS-RENSA-FAELT       TO MOD-IDFTG-IN                       
062900      ELSE                                                                
063000         MOVE MID-IDFTG-IN          TO MOD-IDFTG-IN                       
063100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFTG-IN-ATTR                  
063200     END-IF                                                               
063300                                                                          
063400     IF MID-IDKONTO-IN              =  ALL '+'                            
063500         IF MID-IDKONTO-UT          =  SPACE                              
063600             MOVE MFS-RENSA-FAELT   TO MOD-IDKONTO-UT                     
063700          ELSE                                                            
063800             MOVE MID-IDKONTO-UT    TO MOD-IDKONTO-UT                     
063900         END-IF                                                           
064000         MOVE MFS-RENSA-FAELT       TO MOD-IDKONTO-IN                     
064100      ELSE                                                                
064200         MOVE MID-IDKONTO-IN        TO MOD-IDKONTO-IN                     
064300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKONTO-IN-ATTR                
064400     END-IF                                                               
064500                                                                          
064600     IF MID-IDANALYS-IN             =  ALL '+'                            
064700         IF MID-IDANALYS-UT         =  SPACE                              
064800             MOVE MFS-RENSA-FAELT   TO MOD-IDANALYS-UT                    
064900          ELSE                                                            
065000             MOVE MID-IDANALYS-UT TO MOD-IDANALYS-UT                      
065100         END-IF                                                           
065200         MOVE MFS-RENSA-FAELT       TO MOD-IDANALYS-IN                    
065300      ELSE                                                                
065400         MOVE MID-IDANALYS-IN       TO MOD-IDANALYS-IN                    
065500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANALYS-IN-ATTR               
065600     END-IF                                                               
065700                                                                          
065800     IF MID-IDKST-IN              =  ALL '+'                              
065900         IF MID-IDKST-UT          =  SPACE                                
066000             MOVE MFS-RENSA-FAELT   TO MOD-IDKST-UT                       
066100          ELSE                                                            
066200             MOVE MID-IDKST-UT      TO MOD-IDKST-UT                       
066300         END-IF                                                           
066400         MOVE MFS-RENSA-FAELT       TO MOD-IDKST-IN                       
066500      ELSE                                                                
066600         MOVE MID-IDKST-IN          TO MOD-IDKST-IN                       
066700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKST-IN-ATTR                  
066800     END-IF                                                               
066900                                                                          
067000                                                                          
067100     IF MID-FLGODK-IN               = ALL '+'                             
067200         MOVE MFS-RENSA-FAELT       TO MOD-FLGODK-IN-ATTR                 
067300      ELSE                                                                
067400         MOVE MID-FLGODK-IN         TO MOD-FLGODK-IN                      
067500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLGODK-IN-ATTR                 
067600     END-IF                                                               
067700                                                                          
067800     PERFORM FA-FLYTTA-KOLUMN-FAELT                                       
067900     .                                                                    
068000     SKIP2                                                                
068100 FA-FLYTTA-KOLUMN-FAELT    SECTION.                                       
068200                                                                          
068300     MOVE +1                        TO RAD-IX1                            
068400     PERFORM UNTIL RAD-IX1          >  MAX-KVRADER                        
068500       IF MID-IDARTNR(RAD-IX1)     =  ALL '+'                             
068600         MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN(RAD-IX1)            
068700       ELSE                                                               
068800         MOVE MID-IDARTNR(RAD-IX1) TO MOD-IDARTNR-IN(RAD-IX1)             
068900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-IN-ATTR(RAD-IX1)         
069000       END-IF                                                             
069100                                                                          
069200       IF MID-KVAVIS(RAD-IX1)      =  ALL '+'                             
069300         MOVE MFS-RENSA-FAELT       TO MOD-KVAVIS-IN(RAD-IX1)             
069400       ELSE                                                               
069500         MOVE MID-KVAVIS(RAD-IX1) TO MOD-KVAVIS-IN(RAD-IX1)               
069600         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAVIS-IN-ATTR(RAD-IX1)        
069700       END-IF                                                             
069800       ADD +1                       TO RAD-IX1                            
069900     END-PERFORM                                                          
070000     .                                                                    
070100     EJECT                                                                
070200                                                                          
070300 G-CALL-SUBROUTINE       SECTION.                                         
070500     MOVE '001'                TO REQU-IDMSGVER                           
070600     MOVE 'E'                  TO REQU-KDPGMACT                           
070700     MOVE MSGI-IDUSER          TO REQU-IDUSER                             
070800                                                                          
070900     CALL W6011610 USING REQU-AREA     RESP-AREA     MAX-KVRADER          
071000                         MSG-PCB       WDB6-LEV-PCB  WDB6-PCB             
071100                         REG-INLA1-PCB REG-INLA2-PCB REG-INLA3-PCB        
071200                         REG-LEVA-PCB  REG-ARTC-PCB  REG-BENA-PCB         
071300                         REG-INLB-PCB  REG-ARTS-PCB  REG-WDK7-PCB         
071310                         REG-WDB6-PCB                                     
071400                         SAP-SAPC-PCB                                     
071500                         9305-PCB.                                        
071600     PERFORM GB-SET-MSG-AND-HILIGHT                                       
071700     PERFORM GC-MOVE-OUTDATA-TO-MOD-AREA                                  
071800     .                                                                    
071900     EJECT                                                                
072000                                                                          
072500 GB-SET-MSG-AND-HILIGHT   SECTION.                                        
072800     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
072900     MOVE RESP-IDMSG-INFO  TO WS-IDMSG-INFO                               
073100     MOVE SPACE            TO MED-IDMFSFEL                                
073200     MOVE SPACE            TO MED-IDMFSINF                                
073400*                                                                         
073500     IF R-ERR-WRONG-KEY                                                   
073600       MOVE ERR-WRONG-KEY          TO MED-IDMFSFEL                        
073800       CALL WMEDKONV USING MED-WMEDAREA                                   
073900       MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                                 
074100       PERFORM MFS-LAES-IN-IGEN                                           
074200       PERFORM MFS-RENSA-FAELT-IN                                         
074500     ELSE                                                                 
076800       IF R-ERR-PART-SUPERSEDED                                           
076900         MOVE INF-PART-SUPERSEDED TO MED-IDMFSFEL                         
077100         PERFORM MFS-STAENG-KOL-FAELT                                     
077200         MOVE MFS-OEPPNA-ALFA-FAELT   TO MOD-FLGODK-IN-ATTR               
077300       END-IF                                                             
077400*                                                                         
077500       IF R-CORR-HILITE-FLDS                                              
077600         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
077700       END-IF                                                             
077800*                                                                         
077900       IF R-ERR-AVROP-AVISERAT                                            
078000         MOVE INF-AVROP-SAKNAS        TO MED-IDMFSFEL                     
078100         MOVE MFS-OEPPNA-ALFA-FAELT   TO MOD-FLGODK-IN-ATTR               
078200         PERFORM MFS-STAENG-KOL-FAELT                                     
078300       END-IF                                                             
078400*                                                                         
079300       IF R-ERR-IN-LINE-ONE                                               
079400         MOVE ERR-IN-LINE-ONE         TO MED-IDMFSFEL                     
079500       END-IF                                                             
079600*                                                                         
080500       IF R-ERR-IDFS-ALREADY-EXISTS                                       
080600         MOVE ERR-IDFS-ALREADY-EXISTS TO MED-IDMFSFEL                     
080700       END-IF                                                             
080800*                                                                         
080900       IF R-INF-AVROP-SAKNAS                                              
081000         MOVE INF-AVROP-SAKNAS        TO MED-IDMFSFEL                     
081200         MOVE MFS-OEPPNA-ALFA-FAELT   TO MOD-FLGODK-IN-ATTR               
081300         PERFORM MFS-STAENG-KOL-FAELT                                     
081400       END-IF                                                             
081500                                                                          
081600       IF R-SAP-ACCOUNT-MISSING-IN-R3                                     
081800         MOVE SAP-ACCOUNT-MISSING-IN-R3    TO MED-IDMFSFEL                
082100       END-IF                                                             
082200                                                                          
083500       IF R-SAP-COSTCENTER-MISSING-IN-R3                                  
083700         MOVE SAP-COSTCENTER-MISSING-IN-R3 TO MED-IDMFSFEL                
084000       END-IF                                                             
084100                                                                          
085400       IF R-SAP-ANALYS-NO-MISSING-IN-R3                                   
085500         MOVE SAP-ANALYS-NO-MISSING-IN-R3  TO MED-IDMFSFEL                
085600       END-IF                                                             
085700                                                                          
085800       IF R-SAP-ANAL-CC-MUST-BE-ENTERED                                   
085900         MOVE SAP-ANAL-CC-MUST-BE-ENTERED  TO MED-IDMFSFEL                
086100       END-IF                                                             
086200                                                                          
086300       IF R-ERR-PRICE-IS-MISSING                                          
086400         MOVE ERR-PRICE-IS-MISSING      TO MED-IDMFSFEL                   
086500       END-IF                                                             
086600                                                                          
086610       IF R-ERR-WRNG-CURRENCY                                             
086620         MOVE ERR-WRNG-CURRENCY         TO MED-IDMFSFEL                   
086630       END-IF                                                             
086640                                                                          
086700       IF R-ERR-PART-MISSING                                              
086800         MOVE ERR-PART-MISSING          TO MED-IDMFSFEL                   
086900       END-IF                                                             
087000                                                                          
087100       IF R-ERR-PART-OTHER-COMPANY                                        
087200         MOVE ERR-PART-OTHER-COMPANY    TO MED-IDMFSFEL                   
087300       END-IF                                                             
087400                                                                          
087500       IF R-SAP-COSTCENTER-NOT-ALLOWED                                    
087600         MOVE SAP-COSTCENTER-NOT-ALLOWED TO MED-IDMFSFEL                  
087700       END-IF                                                             
087800                                                                          
087900       IF R-SAP-ANALYSIS-NO-NOT-ALLOWED                                   
088000         MOVE SAP-ANALYSIS-NO-NOT-ALLOWED TO MED-IDMFSFEL                 
088100       END-IF                                                             
088200                                                                          
088300       IF R-SAP-ACCOUNT-MUST-BE-REG                                       
088400         MOVE SAP-ACCOUNT-MUST-BE-REG     TO MED-IDMFSFEL                 
088500       END-IF                                                             
088600                                                                          
088700       IF R-INF-UPDATE-DONE                                               
088800         MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                       
088900       END-IF                                                             
089000*                                                                         
089100       IF R-ERR-UPDATE-NOT-DONE                                           
089200         MOVE ERR-UPDATE-NOT-DONE   TO MED-IDMFSINF                       
089300       END-IF                                                             
089400*                                                                         
089500       IF R-INF-ALREADY-EXISTS                                            
089600         MOVE ERR-IDFS-ALREADY-EXISTS TO MED-IDMFSINF                     
089700         MOVE MFS-OEPPNA-ALFA-FAELT   TO MOD-FLGODK-IN-ATTR               
089800         PERFORM MFS-STAENG-KOL-FAELT                                     
089900       END-IF                                                             
090000*                                                                         
091000       CALL WMEDKONV USING MED-WMEDAREA                                   
091100       MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                            
091200       MOVE MED-TEMFSINF       TO MOD-TEMFSINF                            
091300*                                                                         
091400     END-IF                                                               
095400     .                                                                    
095500     EJECT                                                                
095600                                                                          
095700 GC-MOVE-OUTDATA-TO-MOD-AREA    SECTION.                                  
096000     IF GODK-MID                                                          
096100       MOVE WS-IDLEVNR           TO MOD-IDLEVNR-UT                        
096200       MOVE WS-KDRT              TO MOD-KDRT-UT                           
096300       INSPECT MOD-KDRT-UT REPLACING LEADING ZERO BY SPACE                
096400       IF MOD-KDRT-UT  =  SPACE                                           
096500         MOVE ' 0'               TO MOD-KDRT-UT                           
096600       END-IF                                                             
096700       MOVE WS-IDFS              TO MOD-IDFS-UT                           
096800       MOVE WS-TIAVIDAT          TO MOD-TIAVIDAT-UT                       
096900       INSPECT MOD-TIAVIDAT-UT REPLACING LEADING ZERO BY SPACE            
097000       MOVE WS-ADINLOMR-PRT      TO MOD-ADINLOMR-PRT-UT                   
097100       MOVE WS-IDDC              TO MOD-IDDC-UT                           
097200       MOVE WS-IDLBBET           TO MOD-IDLBBET-UT                        
097300       MOVE WS-FLGODK            TO MOD-FLGODK-UT                         
097400*                                                                         
098600       IF RESP-IDFTG-IN-ATTR > SPACE                                      
098700         MOVE RESP-IDFTG-IN-ATTR TO MOD-IDFTG-IN-ATTR                     
098800       END-IF                                                             
099000       MOVE MFS-RENSA-FAELT      TO MOD-IDFTG-IN                          
099100       MOVE WS-IDFTG             TO MOD-IDFTG-UT                          
099200       INSPECT MOD-IDFTG-UT REPLACING LEADING ZERO BY SPACE               
099300*                                                                         
099400       IF RESP-IDKONTO-IN-ATTR > SPACE                                    
099500         MOVE RESP-IDKONTO-IN-ATTR TO MOD-IDKONTO-IN-ATTR                 
099600       END-IF                                                             
100300       MOVE MFS-RENSA-FAELT        TO MOD-IDKONTO-IN                      
100400       MOVE WS-IDKONTO             TO MOD-IDKONTO-UT                      
100500       INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE             
100800*                                                                         
100900       IF RESP-IDANALYS-IN-ATTR > SPACE                                   
101000         MOVE RESP-IDANALYS-IN-ATTR TO MOD-IDANALYS-IN-ATTR               
101100       END-IF                                                             
101900       MOVE MFS-RENSA-FAELT         TO MOD-IDANALYS-IN                    
102000       MOVE WS-IDANALYS             TO MOD-IDANALYS-UT                    
102100       INSPECT MOD-IDANALYS-UT REPLACING LEADING ZERO BY SPACE            
102400*                                                                         
102500       IF RESP-IDKST-IN-ATTR > SPACE                                      
102600         MOVE RESP-IDKST-IN-ATTR TO MOD-IDKST-IN-ATTR                     
102700       END-IF                                                             
103500       MOVE MFS-RENSA-FAELT    TO MOD-IDKST-IN                            
103600       MOVE WS-IDKST           TO MOD-IDKST-UT                            
103900*                                                                         
104000       IF RESP-FLGODK-IN-ATTR > SPACE                                     
104100         MOVE RESP-FLGODK-IN-ATTR   TO MOD-FLGODK-IN-ATTR                 
104200       END-IF                                                             
104300       MOVE MFS-RENSA-FAELT         TO MOD-FLGODK-IN                      
104400*                                                                         
104600       MOVE +1 TO RAD-IX1                                                 
104700       PERFORM UNTIL RAD-IX1 > MAX-KVRADER                                
104800         MOVE RESP-IDARTNR-LINE-ATTR(RAD-IX1)                             
104900                               TO MOD-IDARTNR-IN-ATTR(RAD-IX1)            
105000         IF RESP-IDARTNR-LINE(RAD-IX1) = SPACES                           
105100           MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-IN(RAD-IX1)            
105200         ELSE                                                             
105300           IF RESP-IDARTNR-LINE(RAD-IX1) = ALL '+'                        
105400             MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN(RAD-IX1)            
105500           ELSE                                                           
105600             MOVE RESP-IDARTNR-LINE(RAD-IX1)                              
105700                                    TO MOD-IDARTNR-IN(RAD-IX1)            
105800             INSPECT MOD-IDARTNR-IN(RAD-IX1)                              
105900                               REPLACING LEADING ZERO BY SPACE            
106000           END-IF                                                         
106100         END-IF                                                           
106200*                                                                         
106300         MOVE RESP-KVAVIS-LINE-ATTR(RAD-IX1)                              
106400                                    TO MOD-KVAVIS-IN-ATTR(RAD-IX1)        
106500         IF RESP-KVAVIS-LINE(RAD-IX1) = SPACES                            
106600           MOVE MFS-RENSA-FAELT     TO MOD-KVAVIS-IN(RAD-IX1)             
106700         ELSE                                                             
106800           IF RESP-KVAVIS-LINE(RAD-IX1) = ALL '+'                         
106900             MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVIS-IN(RAD-IX1)             
107000           ELSE                                                           
107100             MOVE RESP-KVAVIS-LINE(RAD-IX1)                               
107200                                    TO MOD-KVAVIS-IN(RAD-IX1)             
107300             INSPECT MOD-KVAVIS-IN(RAD-IX1)                               
107400                              REPLACING LEADING ZERO BY SPACE             
107500           END-IF                                                         
107600         END-IF                                                           
107700*                                                                         
107800         ADD +1                   TO RAD-IX1                              
107900       END-PERFORM                                                        
108000     END-IF                                                               
108100                                                                          
108200     IF R-INF-UPDATE-DONE                                                 
108400       PERFORM MFS-FORM-ATTR                                              
108500       PERFORM MFS-RENSA-FAELT-IN                                         
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900                                                                          
109000 MFS-RENSA-NYCKLAR  SECTION.                                              
109100                                                                          
109200     MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-UT                              
109300                              MOD-KDRT-UT                                 
109400                              MOD-IDFS-UT                                 
109500                              MOD-TIAVIDAT-UT                             
109600                              MOD-ADINLOMR-PRT-UT                         
109700                              MOD-IDLBBET-UT                              
109800                              MOD-FLGODK-UT                               
109900                              MOD-IDDC-UT                                 
110000*     + ÖVRIGA SPAR-NYCKLAR                                               
110100                              MOD-FLKLIVIS-UT                             
110200                              MOD-IDLOPNRM-UT                             
110300     .                                                                    
110400     SKIP2                                                                
110500                                                                          
110600                                                                          
110700 MFS-RENSA-FAELT-IN SECTION.                                              
110800*    --- ALLA UTDATA-FÄLT                                                 
110900     MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-IN                          
111000                                  MOD-IDFTG-IN                            
111100                                  MOD-IDKONTO-IN                          
111200                                  MOD-IDANALYS-IN                         
111300                                  MOD-IDKST-IN                            
111400                                  MOD-FLGODK-IN                           
111500                                                                          
111600     PERFORM MFS-RENSA-KOLUMN-FAELT-IN                                    
111700     .                                                                    
111800     EJECT                                                                
111900 MFS-RENSA-KOLUMN-FAELT-IN SECTION.                                       
112000     MOVE +1                   TO RAD-IX1                                 
112100     PERFORM UNTIL RAD-IX1     >  MAX-KVRADER                             
112200         MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR-IN(RAD-IX1)                 
112300                                  MOD-KVAVIS-IN (RAD-IX1)                 
112400         ADD +1                TO RAD-IX1                                 
112500     END-PERFORM                                                          
112600     .                                                                    
112700     SKIP2                                                                
112800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
112900     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDFTG-IN                            
113000                                  MOD-IDKONTO-IN                          
113100                                  MOD-IDANALYS-IN                         
113200                                  MOD-IDKST-IN                            
113300                                                                          
113400     PERFORM MFS-ROER-EJ-KOLUMN-FAELT-IN                                  
113500     .                                                                    
113600     EJECT                                                                
113700 MFS-ROER-EJ-KOLUMN-FAELT-IN SECTION.                                     
113800                                                                          
113900     MOVE +1                     TO RAD-IX1                               
114000     PERFORM UNTIL RAD-IX1       >  MAX-KVRADER                           
114100         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-IN(RAD-IX1)               
114200                                    MOD-KVAVIS-IN (RAD-IX1)               
114300         ADD +1                  TO RAD-IX1                               
114400     END-PERFORM                                                          
114500     .                                                                    
114600     SKIP2                                                                
114700 MFS-FORM-ATTR SECTION.                                                   
114800                                                                          
114900     MOVE MFS-FORMATETS-ATTR   TO MOD-IDLBBET-IN                          
115000                                  MOD-FLGODK-IN                           
115100                                                                          
115200     PERFORM MFS-FORM-ATTR-KOL-FAELT-IN                                   
115300     EJECT                                                                
115400     .                                                                    
115500 MFS-FORM-ATTR-KOL-FAELT-IN SECTION.                                      
115600                                                                          
115700     MOVE +1                     TO RAD-IX1                               
115800     PERFORM UNTIL RAD-IX1       >  MAX-KVRADER                           
115900         MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-IN-ATTR(RAD-IX1)          
116000                                    MOD-KVAVIS-IN-ATTR (RAD-IX1)          
116100         ADD +1                  TO RAD-IX1                               
116200     END-PERFORM                                                          
116300     .                                                                    
116400     SKIP2                                                                
116500 MFS-LAES-IN-IGEN SECTION.                                                
116600                                                                          
116700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFTG-IN-ATTR                      
116800                                   MOD-IDKONTO-IN-ATTR                    
116900                                   MOD-IDANALYS-IN-ATTR                   
117000                                   MOD-IDKST-IN-ATTR                      
117100                                                                          
117200     PERFORM MFS-LAES-IN-IGEN-KOL-FAELT                                   
117300     .                                                                    
117400     EJECT                                                                
117500 MFS-LAES-IN-IGEN-KOL-FAELT SECTION.                                      
117600                                                                          
117700     MOVE +1                     TO RAD-IX1                               
117800     PERFORM UNTIL RAD-IX1       >  MAX-KVRADER                           
117900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-IN-ATTR(RAD-IX1)         
118000                                     MOD-KVAVIS-IN-ATTR (RAD-IX1)         
118100       ADD +1                    TO RAD-IX1                               
118200     END-PERFORM                                                          
118300     .                                                                    
118400     SKIP2                                                                
118500 MFS-STAENG-IN-FAELT  SECTION.                                            
118600                                                                          
118700     MOVE MFS-STAENG-FAELT            TO MOD-IDFTG-IN-ATTR                
118800                                         MOD-IDKONTO-IN-ATTR              
118900                                         MOD-IDANALYS-IN-ATTR             
119000                                         MOD-IDKST-IN-ATTR                
119100                                                                          
119200     PERFORM MFS-STAENG-KOL-FAELT                                         
119300     .                                                                    
119400     SKIP3                                                                
119500 MFS-STAENG-KOL-FAELT SECTION.                                            
119600                                                                          
119700     MOVE +1                          TO RAD-IX1                          
119800     PERFORM UNTIL RAD-IX1            >  MAX-KVRADER                      
119900*      IF REG6-IDARTNR-OK (RAD-IX1) =    JA                               
120000                                                                          
120100       IF RESP-IDMSG-ERROR    > ZERO                                      
120200      AND RESP-IDELMT-ERROR = 'IDARTNR'                                   
120300         MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-IN-ATTR(RAD-IX1)            
120400       ELSE                                                               
120500         MOVE MFS-STAENG-FAELT-HI TO MOD-IDARTNR-IN-ATTR(RAD-IX1)         
120600       END-IF                                                             
120700       MOVE MFS-STAENG-FAELT TO MOD-KVAVIS-IN-ATTR(RAD-IX1)               
120800       ADD +1                  TO RAD-IX1                                 
120900     END-PERFORM                                                          
121000     .                                                                    
121100     EJECT                                                                
121200* --- IMS SEKTIONER ---                                                   
121300     SKIP3                                                                
121400 IMS-GET-MSG SECTION.                                                     
121500                                                                          
121600     MOVE '  QC' TO GODK-STATUSKODER                                      
121700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
121800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
121900     PERFORM IMS-STATUSKONTROLL                                           
122000     .                                                                    
122100     SKIP3                                                                
122200 IMS-INSERT-MSG SECTION.                                                  
122300                                                                          
122400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
122500       MOVE '0' TO MFS-KDHUVOMR                                           
122600     END-IF                                                               
122700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
122800     MOVE SPACE TO GODK-STATUSKODER                                       
122900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
123000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123100     PERFORM IMS-STATUSKONTROLL                                           
123200     .                                                                    
123300 IMS-STATUSKONTROLL SECTION.                                              
123400                                                                          
123500     SET STATUS-IX TO 1                                                   
123600     SEARCH GODK-STATUS                                                   
123700       AT END                                                             
123800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
123900         DELIMITED BY SIZE INTO FELTEXT                                   
124000         CALL FELLOG                                                      
124100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
124200         CONTINUE                                                         
124300     END-SEARCH                                                           
124400     .                                                                    
