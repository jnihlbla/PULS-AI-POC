000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011400.                                                
000300 AUTHOR.         LARS THELL.                                              
000400 DATE-WRITTEN.   92/02/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MPP FÖR ATT RÄTTA/ÄNDRA FÖLJESEDELINFO                           
000900*                                                                         
001000*        THIS IS A DRIVER PGM FOR TRANSACTIONS W6T114, W6T114U            
001100*                                                                         
001200*        IT TAKES CARE OF ALL TECHNICAL DETAILS RELATED TO WHELP          
001300*        AND 3270 FORMATS AND CALLS SUBPROGRAM W6011410 WHICH             
001400*        CONTAINS ALL BUSINESS LOGIC FOR THESE TRANSACTIONS.              
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM THE WEB            
001700*        EXISTS - W6W11400 (TRANSACTIONS W6T114, W6T114U)                 
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T114                                              
002100*        MID:         W6I11401                                            
002200*        REQU:        W60114I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W6O11401                                            
002600*        RESP:        W60114O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300     SKIP3                                                                
003400 77  IDPGM                       PIC X(08)   VALUE 'W6011400'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FILLER                      PIC X(8)  VALUE 'ERRORTEX'.              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  SPAR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  MAX-KVRADER                 PIC S9(4)  VALUE +8    COMP SYNC.        
004800 77  MAX-REQU-INDX               PIC S9(4)  VALUE +500  COMP SYNC.        
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  PRIS-INDX                   PIC S9(9)  VALUE +0    COMP SYNC.        
005100 77  PRIS-INDX-MAX               PIC S9(9)  VALUE +5    COMP SYNC.        
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +827  COMP SYNC.        
005300 77  WS-RAD-IX                   PIC  9(3)  VALUE  0.                     
005400                                                                          
005500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005700 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
005800 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
005900 77  WS-FLKLIVIS                 PIC X(1)    VALUE SPACE.                 
006000 77  WS-FLBORT                   PIC X(1)    VALUE SPACE.                 
006100 77  WS-TIPRLIST-NYCKEL          PIC S9(9)   VALUE ZERO.                  
006200 77  WS-TIPRLIST-DEL             PIC 9(9).                                
006300 77  WS-TIPRLIST                 PIC 9(6).                                
006400*77  WS-IDLOPNRM                 PIC 9(8).                                
006500 77  WS-KDRT                     PIC X(2)    VALUE SPACE.                 
006600 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
006700 77  WS-FLBORT-UPD               PIC X(01)   VALUE SPACE.                 
006800                                                                          
006900 01  WS-TIAVIDAT                 PIC X(6).                                
007000 01  WS-TIAVIDAT-N REDEFINES WS-TIAVIDAT PIC 9(6).                        
007100                                                                          
007200 01  WS-IDDC-LOCAL.                                                       
007300     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
007400     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
007500     03  FILLER                  PIC X(1)   VALUE SPACE.                  
007600                                                                          
007700                                                                          
007800 01  DAGENS-DATUM                PIC 9(6).                                
007900 01  FILLER REDEFINES DAGENS-DATUM.                                       
008000  05 DATUM-AA                    PIC 9(2).                                
008100  05 DATUM-MM                    PIC 9(2).                                
008200  05 DATUM-DD                    PIC 9(2).                                
008300                                                                          
008400 01  DAGENS-DATUM-LOCAL          PIC 9(6).                                
008500 01  FILLER REDEFINES DAGENS-DATUM-LOCAL.                                 
008600  05 DATUM-AA-LOCAL              PIC 9(2).                                
008700  05 DATUM-MM-LOCAL              PIC 9(2).                                
008800  05 DATUM-DD-LOCAL              PIC 9(2).                                
008900                                                                          
009000*     -- DAGENS-DATUM MED SEKEL-SIFFRA                                    
009100 01      WS-DAGENS-DATUM         PIC 9(8)    VALUE ZERO.                  
009200 01      FILLER REDEFINES WS-DAGENS-DATUM.                                
009300   03    WS-DAGENS-SEKEL         PIC 9(2).                                
009400   03    WS-IDAG                 PIC 9(6).                                
009500 EJECT                                                                    
009600*      --- VALID IDDC CODES                                               
009700*                                                                         
009800*01    -COPY WWDC99                                                       
009900       EJECT                                                              
010000 77  W-KVKOLLI                   PIC S9(7)  VALUE +0    COMP-3.           
010100 77  W-IDLEVNR-UPD               PIC  X(5)  VALUE SPACE.                  
010200 77  W-TIANKDAG-UPD              PIC  9(6)  VALUE ZERO.                   
010300 77  W-TIAVIDAT-UPD              PIC  9(6)  VALUE ZERO.                   
010400 77  W-TIAVIDAT                  PIC  9(6)  VALUE ZERO.                   
010500 77  W-ANT-ART                   PIC S9(3)  VALUE +0    COMP-3.           
010600 77  W-MAX-ART                   PIC S9(3)  VALUE +40   COMP-3.           
010700 77  SPAR-IDRADNR                PIC  9(3)  VALUE ZERO.                   
010800 77  SPAR-IDRADNR-INL            PIC  9(5)  VALUE ZERO.                   
010900 77  SPAR-IDARTNR-X              PIC  X(8)  VALUE ZERO.                   
011000 77  SPAR-KVAVIS                 PIC S9(7)  VALUE ZERO COMP-3.            
011100 77  SPAR-KVINLART               PIC S9(7)  VALUE ZERO COMP-3.            
011200 77  W-MOD-IDARTNR-RAD           PIC Z(7)9  VALUE ZERO.                   
011300 77  W-MOD-KVAVIS-RAD            PIC Z(5)9  VALUE ZERO.                   
011400 77  W-MOD-KDRT-RAD              PIC Z(1)9  VALUE ZERO.                   
011500 77  W-MOD-KVKOLLI-RAD           PIC Z(2)9  VALUE ZERO.                   
011600 77  W-MOD-IDOKOLLI-RAD          PIC Z(8)9  VALUE ZERO.                   
011700 77  W-MOD-KVINLART-RAD          PIC Z(5)9  VALUE ZERO.                   
011800                                                                          
011900 77  W-SPAR-IDLEVNR              PIC  X(5)   VALUE SPACE.                 
012000 77  W-SPAR-IDFS                 PIC  X(8)   VALUE SPACE.                 
012100 77  W-SPAR-TIAVIDAT             PIC S9(7)   VALUE ZERO COMP-3.           
012200                                                                          
012300 77  W-SPAR-INL-FLFEL            PIC  X(1)   VALUE SPACE.                 
012400 77  W-SPAR-TIANKDAG             PIC S9(7)   VALUE ZERO COMP-3.           
012500 77  W-FLFEL                     PIC  X(1)   VALUE SPACE.                 
012600 77  W-IDFTG                     PIC  9(2)   VALUE ZERO.                  
012700 77  W-INL-IDFTG                 PIC  9(2)   VALUE ZERO.                  
012800 77  WS-KVAVROP                  PIC S9(7)   COMP-3 VALUE ZERO.           
012900                                                                          
013000 EJECT                                                                    
013100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013200     88  INDATA-OK                           VALUE 'J'.                   
013300     88  INDATA-FEL                          VALUE 'N'.                   
013400                                                                          
013500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013600     88  NYCKLAR-OK                          VALUE 'J'.                   
013700     88  NYCKLAR-FEL                         VALUE 'N'.                   
013800                                                                          
013900 77  KONTO-LEV-SW                PIC X       VALUE 'J'.                   
014000     88  KONTO-LEV-FEL                       VALUE 'N'.                   
014100                                                                          
014200 77  RAD-OK-SW                   PIC X       VALUE 'J'.                   
014300     88  RAD-OK                              VALUE 'J'.                   
014400                                                                          
014500 77  LEVPLAN-SW                  PIC X       VALUE 'J'.                   
014600     88  LEVPLAN-OK                          VALUE 'J'.                   
014700     88  LEVPLAN-FEL                         VALUE 'N'.                   
014800                                                                          
014900 77  KONTO-UPPG-SW               PIC X       VALUE 'N'.                   
015000     88  KONTO-UPPG-AENDRAT                  VALUE 'J'.                   
015100                                                                          
015200 77  PRIS-FINNS-SW               PIC X       VALUE 'J'.                   
015300     88  PRIS-FINNS                          VALUE 'J'.                   
015400     88  PRIS-SAKNAS                         VALUE 'N'.                   
015500                                                                          
015600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015700     88  EGEN-MID                            VALUE '6114'.                
015800     88  GODK-MID                            VALUE '6111' '6112'          
015900                                                   '6113' '6114'          
016000                                                   '6115' '6116'          
016100                                                   '6118' '6119'.         
016200     88  HELP-MID                            VALUE '0551'.                
016300                                                                          
016400 01  W-PRL-DADAT                 PIC 9(8)    VALUE ZERO.                  
016500     EJECT                                                                
016600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016700 01  GENERELLA-SUBPROGRAM.                                                
016800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017300     03  W611REG                 PIC X(8)    VALUE 'W611REG '.            
017500     03  W411SAP                 PIC X(8)    VALUE 'W411SAP '.            
017600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017700     03  W6011410                PIC X(8)    VALUE 'W6011410'.            
017800     EJECT                                                                
017900*    --- PARAMETERS TO ABEND                                              
018000                                                                          
018100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
018300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
018400                                                                          
018500     SKIP3                                                                
018600*01 -COPY WMSGINIT                                                        
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018900*01 -COPY WMEDAREA                                                        
019000     EJECT                                                                
019100*01 -COPY WDATAREA                                                        
019200     EJECT                                                                
019300 01  MESSAGE-CODES.                                                       
019400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
019500     03  ERR-UPDATE-FORBIDD      PIC X(3)    VALUE '007'.                 
019600     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
019700     03  ERR-MISSING-IN-REG      PIC X(3)    VALUE '010'.                 
019800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
019900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020000     03  ERR-PART-SUPERSEDED     PIC X(3)    VALUE '220'.                 
020100     03  ERR-PRICE-IS-MISSING    PIC X(3)    VALUE '301'.                 
020200     03  ERR-WRONG-QUANTITY      PIC X(3)    VALUE '302'.                 
020300     03  ERR-WRONG-CMD-CODE      PIC X(3)    VALUE '304'.                 
020400     03  ERR-MESSAGE-MISSING     PIC X(3)    VALUE '999'.                 
020500     03  ERR-PART-IS-MISSING     PIC X(3)    VALUE '017'.                 
020600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020900     03  INF-AVROP-SAKNAS        PIC X(3)    VALUE '285'.                 
021000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
021100                                                                          
021200 01 WS-IDMSG-ERROR         PIC X(3).                                      
021300     88  PF11-AND-NO-DATA    VALUE '014'.                                 
021400     88  UPDATE-FORBIDD      VALUE '007'.                                 
021500     88  NUMERIC-CHECK       VALUE '024'.                                 
021600     88  IS-INVALID          VALUE '023'.                                 
021700     88  MISSING-IN-REG      VALUE '027'.                                 
021800     88  CORR-HILITE-FLDS    VALUE '020'.                                 
021900     88  WRONG-KEY           VALUE '022'.                                 
022000     88  PART-SUPERSEDED     VALUE '223'.                                 
022200     88  PRICE-IS-MISSING    VALUE '260'.                                 
022300     88  WRONG-QUANTITY      VALUE '330'.                                 
022400     88  WRONG-CMD-CODE      VALUE '345'.                                 
022500     88  MESSAGE-MISSING     VALUE '999'.                                 
022600     88  MISSING-PART        VALUE '041'.                                 
022700                                                                          
022800 01 WS-IDMSG-INFO          PIC X(3).                                      
022900     88  UPDATE-DONE         VALUE '001'.                                 
023000     88  MORE-INFO-EXISTS    VALUE '011'.                                 
023100     88  PRESS-PF11          VALUE '013'.                                 
023200     88  AVROP-SAKNAS        VALUE '336'.                                 
023300     88  FIRST-PAGE          VALUE '006'.                                 
023400                                                                          
023500     EJECT                                                                
023600 01  FILLER                      PIC X(16)   VALUE 'W611REG-AREA'.        
023700     SKIP3                                                                
023800*01  -COPY W611REG0                                                       
023900*01  FILLER -COPY W611REG4          -RED LAENK-W611REG0.                  
024000     EJECT                                                                
024300*01 -COPY W411SAP                                                         
024400     EJECT                                                                
024500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024600*                                                                         
024700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024800     SKIP3                                                                
024900*01  MID -COPY W6I11401                                                   
025000     EJECT                                                                
025100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025200     SKIP3                                                                
025300*01  -COPY WMSGAREA                                                       
025400     EJECT                                                                
025500     03  MOD REDEFINES MSG-AREA.                                          
025600*      05  -COPY W6O11401                                                 
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025900     SKIP3                                                                
026000*01  -COPY WMFSAREA                                                       
026100     EJECT                                                                
026200                                                                          
026300*    --- AREOR TILL W6011410 SUBPROGRAM                                   
026400*                                                                         
026500 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
026600 01  REQU-AREA.                                                           
026700*    03 -COPY WZ01REQU                                                    
026800*    03 -COPY W60114I1                                                    
026900     SKIP3                                                                
027000                                                                          
027100                                                                          
027200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
027300 01  RESP-AREA.                                                           
027400*    03 -COPY WZ01RESP                                                    
027500*    03 -COPY W60114O1                                                    
027600     SKIP3                                                                
027700*                                                                         
027800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027900*                                                                         
028000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028100*    --- STATUS-KOD FRÅN IMS                                              
028200 01  STATUS-WS                   PIC XX.                                  
028300     88  SEGMENT-FINNS                       VALUE '  '.                  
028400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
028700     SKIP2                                                                
028800 01  GODK-STATUSKODER.                                                    
028900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029000     SKIP3                                                                
029100 01  SSA1                        PIC X(96).                               
029200 01  SSA2                        PIC X(64).                               
029300 01  SSA3                        PIC X(64).                               
029400     EJECT                                                                
029500*    --- IMS FUNKTIONSKODER                                               
029600*01  -COPY W0003                                                          
029700     SKIP3                                                                
029800 LINKAGE SECTION.                                                         
029900                                                                          
030000 01  -COPY W0009   -PRE MSG-                                              
030100                                                                          
030200 01  USEA-PCB                    PIC X.                                   
030300                                                                          
030400 01  W6INLB-PCB                  PIC X.                                   
030500                                                                          
030600 01  INLA1-PCB                   PIC X.                                   
030700                                                                          
030800 01  INLA2-PCB                   PIC X.                                   
030900                                                                          
031000 01  LEVA-PCB                    PIC X.                                   
031100                                                                          
031200 01  ARTC-PCB                    PIC X.                                   
031300                                                                          
031400 01  WDD9-PCB                    PIC X.                                   
031500                                                                          
031600 01  ARTS-PCB                    PIC X.                                   
031700                                                                          
031800 01  WDB6-PCB                    PIC X.                                   
031900                                                                          
032000     EJECT                                                                
032100*   PCB'ER FÖR SUB PGM W611REG                                            
032200                                                                          
032300 01  REG-INLA1-PCB               PIC X.                                   
032400                                                                          
032500 01  REG-INLA2-PCB               PIC X.                                   
032600                                                                          
032700 01  REG-INLA3-PCB               PIC X.                                   
032800                                                                          
032900 01  REG-LEVA-PCB                PIC X.                                   
033000                                                                          
033100 01  REG-ARTC-PCB                PIC X.                                   
033200                                                                          
033300 01  REG-BENA-PCB                PIC X.                                   
033400                                                                          
033500 01  REG-INLB-PCB                PIC X.                                   
033600                                                                          
033700 01  REG-ARTS-PCB                PIC X.                                   
033710                                                                          
033720 01  REG-WDK7-PCB                PIC X.                                   
033800                                                                          
033810 01  REG-WDB6-PCB                PIC X.                                   
033820                                                                          
033900*   PCB'ER FÖR SUB PGM W411SAP                                            
034000                                                                          
034100 01  SAP-SAPC-PCB                PIC X.                                   
034200                                                                          
034500 01  LEVP-INLB-PCB               PIC X.                                   
034600     EJECT                                                                
034700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB W6INLB-PCB                    
034800                           INLA1-PCB INLA2-PCB LEVA-PCB                   
034900                           ARTC-PCB WDD9-PCB ARTS-PCB                     
035000                           WDB6-PCB                                       
035100                           REG-INLA1-PCB REG-INLA2-PCB                    
035200                           REG-INLA3-PCB REG-LEVA-PCB                     
035300                                         REG-ARTC-PCB                     
035400                           REG-BENA-PCB  REG-INLB-PCB                     
035500                           REG-ARTS-PCB  REG-WDK7-PCB                     
035510                           REG-WDB6-PCB                                   
035600                           SAP-SAPC-PCB                                   
035700                           LEVP-INLB-PCB.                                 
035800 MAIN SECTION.                                                            
035900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB W6INLB-PCB INLA1-PCB          
036000                           INLA2-PCB                                      
036100                           LEVA-PCB ARTC-PCB WDD9-PCB ARTS-PCB            
036200                           WDB6-PCB                                       
036300                           REG-INLA1-PCB REG-INLA2-PCB                    
036400                           REG-INLA3-PCB REG-LEVA-PCB                     
036500                                         REG-ARTC-PCB                     
036600                           REG-BENA-PCB  REG-INLB-PCB                     
036700                           REG-ARTS-PCB  REG-WDK7-PCB                     
036710                           REG-WDB6-PCB                                   
036800                           SAP-SAPC-PCB                                   
036900                           LEVP-INLB-PCB.                                 
037000                                                                          
037100     PERFORM IMS-GET-MSG                                                  
037200     IF SEGMENT-FINNS                                                     
037300       PERFORM A-INIT                                                     
037400       PERFORM B-MOVE-KEYS                                                
037500       PERFORM C-INIT-REQU                                                
037600                                                                          
037700       IF MFS-UPDATE                                                      
037800         SET REQU-UPDATE TO TRUE                                          
037900       ELSE                                                               
038000         IF MFS-FIRST                                                     
038100           SET REQU-FIRST TO TRUE                                         
038200         ELSE                                                             
038300           IF MFS-NEXT                                                    
038400             SET REQU-NEXT TO TRUE                                        
038500             MOVE MID-IDRADNR-NEXT     TO REQU-IDRADNR-START              
038600             MOVE MID-IDRADNR-INL-NEXT TO REQU-IDRADNR-INL-START          
038700           ELSE                                                           
038800             SET REQU-QUERY TO TRUE                                       
038900             MOVE MID-IDRADNR-ENTER     TO REQU-IDRADNR-START             
039000             MOVE MID-IDRADNR-INL-ENTER TO REQU-IDRADNR-INL-START         
039100           END-IF                                                         
039200         END-IF                                                           
039300       END-IF                                                             
039400                                                                          
039500       PERFORM F-CALL-BIZ-LOGIC-W6011410                                  
039600                                                                          
039700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
039800       PERFORM IMS-INSERT-MSG                                             
039900     END-IF                                                               
040000                                                                          
040100     MOVE ZERO TO RETURN-CODE                                             
040200     GOBACK                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 A-INIT SECTION.                                                          
040600                                                                          
040700     IF MSG-DUBBLA-TRANSKODER                                             
040800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11401                 
040900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
041000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
041100     ELSE                                                                 
041200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11401                  
041300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
041400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
041500     END-IF                                                               
041600                                                                          
041700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
041800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
041900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
042000                                                                          
042100     MOVE LOW-VALUE TO MSG-AREA                                           
042200     MOVE 'W6O114N1' TO MFS-IDMOD                                         
042300     MOVE '6114' TO MOD-IDTRANS                                           
042400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
042500                                                                          
042600     IF EGEN-MID OR HELP-MID                                              
042700       CONTINUE                                                           
042800      ELSE                                                                
042900       MOVE SPACE TO MFS-KDTRTYP                                          
043000       MOVE '7' TO MFS-IDPFK                                              
043100     END-IF                                                               
043200                                                                          
043300     MOVE SPACE                TO MED-IDMFSFEL                            
043400                                  MED-IDMFSINF                            
043500                                                                          
043600     ACCEPT DAGENS-DATUM       FROM DATE                                  
043700     PERFORM AA-INIT-NYCKLAR                                              
043800                                                                          
043900     IF MSGI-IDLAND-SPR = 'GB'                                            
044000       MOVE +2 TO SPRAK-IX                                                
044100       MOVE 'GB ' TO MED-IDSKYLT                                          
044200     ELSE                                                                 
044300       MOVE +1 TO SPRAK-IX                                                
044400       MOVE 'S  ' TO MED-IDSKYLT                                          
044500     END-IF                                                               
044600                                                                          
044700     .                                                                    
044800     EJECT                                                                
044900*----------------------------------------------------------------*        
045000 AA-INIT-NYCKLAR SECTION.                                                 
045100                                                                          
045200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
045300     MOVE '001'                  TO MSGI-KDCALL                           
045400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
045500     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
045600     MOVE '6114'                 TO MSGI-IDTRANS                          
045700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045800     .                                                                    
045900     EJECT                                                                
046000 B-MOVE-KEYS SECTION.                                                     
046100     MOVE JA TO NYCKLAR-SW                                                
046200                                                                          
046300     PERFORM BE-INIT-IDDC                                                 
046400     PERFORM BA-INIT-IDLEVNR                                              
046500     PERFORM BB-INIT-IDFS                                                 
046600     PERFORM BC-INIT-TIAVIDAT                                             
046700     PERFORM BD-INIT-FLKLIVIS                                             
046800                                                                          
046900     MOVE MFS-RENSA-FAELT        TO MOD-ADINLOMR-PRT-IN                   
047000     IF MID-ADINLOMR-PRT-IN = ALL '+'                                     
047100       MOVE MID-ADINLOMR-PRT-UT  TO WS-ADINLOMR-PRT                       
047200     ELSE                                                                 
047300       MOVE MID-ADINLOMR-PRT-IN  TO WS-ADINLOMR-PRT                       
047400       MOVE SPACE                TO MFS-KDTRTYP                           
047500     END-IF                                                               
047600                                                                          
047700     PERFORM BF-MOVE-OEVRIGA-NYCKLAR                                      
047800*                                                                         
047900     IF GODK-MID OR NYCKLAR-OK                                            
048000       MOVE WS-IDLEVNR         TO MOD-IDLEVNR-UT                          
048100       MOVE WS-IDFS            TO MOD-IDFS-UT                             
048200       MOVE WS-TIAVIDAT        TO MOD-TIAVIDAT-UT                         
048300       MOVE WS-ADINLOMR-PRT    TO MOD-ADINLOMR-PRT-UT                     
048400       MOVE WS-FLKLIVIS        TO MOD-FLKLIVIS-UT                         
048500       IF WS-FLKLIVIS = YES                                               
048600         MOVE JA               TO WS-FLKLIVIS                             
048700       END-IF                                                             
048800       MOVE WS-IDDC            TO MOD-IDDC-UT                             
048900     ELSE                                                                 
049000       MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-UT                          
049100                                MOD-IDFS-UT                               
049200                                MOD-TIAVIDAT-UT                           
049300                                MOD-FLKLIVIS-UT                           
049400                                MOD-ADINLOMR-PRT-UT                       
049500                                MOD-IDDC-UT                               
049600*      + ÖVRIGA SPAR-NYCKLAR                                              
049700                                MOD-KDRT-UT                               
049800                                MOD-IDLBBET-UT                            
049900                                MOD-IDLOPNRM-UT                           
050000     END-IF                                                               
050100*                                                                         
050200     .                                                                    
050300     EJECT                                                                
050400 BA-INIT-IDLEVNR SECTION.                                                 
050500     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                          
050600                                                                          
050700     IF MID-IDLEVNR-IN         = ALL '+'                                  
050800       MOVE MID-IDLEVNR-UT     TO WS-IDLEVNR                              
050900     ELSE                                                                 
051000       MOVE MID-IDLEVNR-IN     TO WS-IDLEVNR                              
051100       MOVE '7'                TO MFS-IDPFK                               
051200       MOVE SPACE              TO MFS-KDTRTYP                             
051300     END-IF                                                               
051400     .                                                                    
051500     EJECT                                                                
051600 BB-INIT-IDFS    SECTION.                                                 
051700     MOVE MFS-RENSA-FAELT      TO MOD-IDFS-IN                             
051800                                                                          
051900     IF MID-IDFS-IN            = ALL '+'                                  
052000       MOVE MID-IDFS-UT        TO WS-IDFS                                 
052100     ELSE                                                                 
052200       MOVE MID-IDFS-IN        TO WS-IDFS                                 
052300       MOVE '7'                TO MFS-IDPFK                               
052400       MOVE SPACE              TO MFS-KDTRTYP                             
052500     END-IF                                                               
052600     .                                                                    
052700     EJECT                                                                
052800 BC-INIT-TIAVIDAT SECTION.                                                
052900     MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT-IN                         
053000                                                                          
053100     IF MID-TIAVIDAT-IN        = ALL '+'                                  
053200       MOVE MID-TIAVIDAT-UT    TO WS-TIAVIDAT                             
053300       INSPECT WS-TIAVIDAT     REPLACING LEADING SPACE BY ZERO            
053400     ELSE                                                                 
053500       MOVE MID-TIAVIDAT-IN    TO WS-TIAVIDAT                             
053600       MOVE '7'                TO MFS-IDPFK                               
053700       MOVE SPACE              TO MFS-KDTRTYP                             
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 BD-INIT-FLKLIVIS    SECTION.                                             
054200     MOVE MFS-RENSA-FAELT      TO MOD-FLKLIVIS-IN                         
054300                                                                          
054400     IF EGEN-MID                                                          
054500       IF MID-FLKLIVIS-IN    = ALL '+'                                    
054600         MOVE MID-FLKLIVIS-UT TO WS-FLKLIVIS                              
054700       ELSE                                                               
054800         MOVE MID-FLKLIVIS-IN TO WS-FLKLIVIS                              
054900         MOVE '7'             TO MFS-IDPFK                                
055000         MOVE SPACE           TO MFS-KDTRTYP                              
055100       END-IF                                                             
055200     ELSE                                                                 
055300       MOVE SPACE             TO WS-FLKLIVIS                              
055400     END-IF                                                               
055500                                                                          
055600     IF WS-FLKLIVIS = '+'                                                 
055700       MOVE SPACE            TO  WS-FLKLIVIS                              
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 BE-INIT-IDDC    SECTION.                                                 
056200     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
056300     MOVE     SPACE       TO WS-IDDC                                      
056400                                                                          
056500     IF EGEN-MID OR HELP-MID                                              
056600       IF MID-IDDC-IN = ALL '+'                                           
056700         MOVE MSGI-IDDC   TO WS-IDDC                                      
056800       ELSE                                                               
056900         MOVE MID-IDDC-IN TO WS-IDDC                                      
057000         MOVE    SPACE    TO MFS-KDTRTYP                                  
057100       END-IF                                                             
057200     ELSE                                                                 
057300       IF GODK-MID                                                        
057400         MOVE MSGI-IDDC   TO WS-IDDC                                      
057500       END-IF                                                             
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 BF-MOVE-OEVRIGA-NYCKLAR    SECTION.                                      
058000     MOVE MFS-RENSA-FAELT      TO MOD-KDRT-IN                             
058100                                  MOD-IDLBBET-IN                          
058200                                  MOD-IDLOPNRM-IN                         
058300*                                                                         
058400     IF MID-KDRT-IN       = ALL '+'                                       
058500         MOVE MID-KDRT-UT      TO WS-KDRT                                 
058600     ELSE                                                                 
058700         MOVE MID-KDRT-IN      TO WS-KDRT                                 
058800     END-IF                                                               
058900     MOVE WS-KDRT              TO  MOD-KDRT-UT                            
059000*                                                                         
059100     IF MID-IDLBBET-IN    = ALL '+'                                       
059200         MOVE MID-IDLBBET-UT   TO WS-IDLBBET                              
059300     ELSE                                                                 
059400         MOVE MID-IDLBBET-IN   TO WS-IDLBBET                              
059500     END-IF                                                               
059600     MOVE WS-IDLBBET           TO MOD-IDLBBET-UT                          
059700*                                                                         
059800     IF MID-IDLOPNRM-IN   = ALL '+'                                       
059900         MOVE MID-IDLOPNRM-UT  TO MOD-IDLOPNRM-UT                         
060000     ELSE                                                                 
060100         MOVE MID-IDLOPNRM-IN  TO MOD-IDLOPNRM-UT                         
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 BG-HAEMTA-LOKAL-DATUM SECTION.                                           
060600     SKIP2                                                                
060700*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
060800*W005INIT. DETTA SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC               
060900*                                                                         
061000     MOVE ALL '+'         TO MSGI-WMSGINIT                                
061100     MOVE '001'           TO MSGI-KDCALL                                  
061200     MOVE WS-IDDC         TO WS-IDDC-LOCAL-DATE                           
061300                                                                          
061400     MOVE WS-IDDC-LOCAL   TO MSGI-IDUSER                                  
061500                                                                          
061600     CALL W005INIT  USING  MSGI-WMSGINIT USEA-PCB                         
061700     MOVE MSGI-TILOKDAT TO DAGENS-DATUM-LOCAL                             
061800     .                                                                    
061900     EJECT                                                                
062000                                                                          
062100 C-INIT-REQU          SECTION.                                            
062200     MOVE MAX-KVRADER          TO REQU-KVRADER                            
062300     MOVE '001'                TO REQU-IDMSGVER                           
062400     MOVE MSGI-IDUSER          TO REQU-IDUSER                             
062500*IN-KEYS                                                                  
062600     MOVE WS-IDLEVNR           TO REQU-IDLEVNR-KEY                        
062700     MOVE WS-IDFS              TO REQU-IDFS-KEY                           
062800     MOVE WS-TIAVIDAT          TO REQU-TIAVIDAT-KEY                       
062900     MOVE WS-ADINLOMR-PRT      TO REQU-ADINLOMR-PRT-KEY                   
063000     MOVE WS-IDDC              TO REQU-IDDC-KEY                           
063100     MOVE WS-KDRT              TO REQU-KDRT-KEY                           
063200     MOVE WS-IDLBBET           TO REQU-IDLBBET-KEY                        
063300     MOVE WS-FLKLIVIS          TO REQU-FLKLIVIS-KEY                       
063400*INPUT                                                                    
063500     MOVE MID-TIAVIDAT-UPD     TO REQU-TIAVIDAT-UPD                       
063600     MOVE MID-IDLBBET-UPD      TO REQU-IDLBBET-UPD                        
063700     MOVE MID-TIANKDAG-UPD     TO REQU-TIANKDAG-UPD                       
063800     MOVE MID-IDLEVNR-UPD      TO REQU-IDLEVNR-UPD                        
063900     MOVE MID-FLBORT-UPD       TO REQU-FLBORT-UPD                         
064000     MOVE MID-IDFTG-UPD        TO REQU-IDFTG-UPD                          
064100     MOVE MID-IDKONTO-UPD      TO REQU-IDKONTO-UPD                        
064200     MOVE MID-IDANALYS-UPD     TO REQU-IDANALYS-UPD                       
064300     MOVE MID-IDKST-UPD        TO REQU-IDKST-UPD                          
064400                                                                          
064500     MOVE MID-IDARTNR-UPD      TO REQU-IDARTNR-UPD                        
064600     MOVE MID-KVAVIS-UPD       TO REQU-KVAVIS-UPD                         
064700     MOVE MID-KDRT-UPD         TO REQU-KDRT-UPD                           
064800     MOVE +1 TO INDX                                                      
064900                                                                          
065000     PERFORM UNTIL INDX > MAX-KVRADER                                     
065100       MOVE MID-KDCMD-RAD(INDX)    TO REQU-KDCMD-LINE(INDX)               
065200       MOVE MID-IDARTNR-RAD(INDX)  TO REQU-IDARTNR-LINE(INDX)             
065300       MOVE MID-KVAVIS-RAD(INDX)   TO REQU-KVAVIS-LINE(INDX)              
065400       MOVE MID-KDRT-RAD(INDX)     TO REQU-KDRT-LINE(INDX)                
065500       MOVE MID-IDARTNR-SPAR(INDX) TO REQU-IDARTNR-SPAR-LINE(INDX)        
065600       MOVE MID-IDRADNR-SPAR(INDX) TO REQU-IDRADNR-SPAR-LINE(INDX)        
065700       MOVE MID-IDRADNR-INL-SPAR(INDX)                                    
065800                               TO REQU-IDRADNR-INL-SPAR-LINE(INDX)        
065900       ADD +1 TO INDX                                                     
066000     END-PERFORM                                                          
066100                                                                          
066200     PERFORM UNTIL INDX > MAX-REQU-INDX                                   
066300       MOVE ALL '+'        TO REQU-KDCMD-LINE(INDX)                       
066400                              REQU-IDARTNR-LINE(INDX)                     
066500                              REQU-KVAVIS-LINE(INDX)                      
066600                              REQU-KDRT-LINE(INDX)                        
066700       ADD +1 TO INDX                                                     
066800     END-PERFORM                                                          
066900                                                                          
067000     .                                                                    
067100     EJECT                                                                
067200 F-CALL-BIZ-LOGIC-W6011410  SECTION.                                      
067300     CALL W6011410 USING REQU-AREA  RESP-AREA  MAX-KVRADER                
067400                         MSG-PCB  W6INLB-PCB INLA1-PCB INLA2-PCB          
067500                         LEVA-PCB ARTC-PCB WDD9-PCB ARTS-PCB              
067600                         WDB6-PCB                                         
067700                         REG-INLA1-PCB REG-INLA2-PCB                      
067800                         REG-INLA3-PCB REG-LEVA-PCB                       
067900                         REG-ARTC-PCB  REG-BENA-PCB                       
068000                         REG-INLB-PCB  REG-ARTS-PCB                       
068010                         REG-WDK7-PCB  REG-WDB6-PCB                       
068100                         SAP-SAPC-PCB  LEVP-INLB-PCB                      
068200                                                                          
068300     PERFORM FA-SET-MSG-HIGHLIGHT                                         
068400     PERFORM FB-MOVE-RESP-TO-MOD                                          
068500     .                                                                    
068600     EJECT                                                                
068700 FA-SET-MSG-HIGHLIGHT     SECTION.                                        
068800                                                                          
068900     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
069000     MOVE RESP-IDMSG-INFO  TO WS-IDMSG-INFO                               
069100                                                                          
069200     IF PF11-AND-NO-DATA                                                  
069300       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
069400     END-IF                                                               
069500                                                                          
069600     IF WRONG-KEY                                                         
069700       MOVE NEJ                   TO INDATA-SW                            
069800       MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                         
069900     END-IF                                                               
070000                                                                          
070100     IF UPDATE-FORBIDD                                                    
070200       MOVE ERR-UPDATE-FORBIDD    TO MED-IDMFSFEL                         
070300     END-IF                                                               
070400                                                                          
070500     IF NUMERIC-CHECK                                                     
070600       MOVE ERR-NOT-NUMERIC       TO MED-IDMFSFEL                         
070700     END-IF                                                               
070800                                                                          
070900     IF MISSING-IN-REG                                                    
071000       MOVE ERR-MISSING-IN-REG    TO MED-IDMFSFEL                         
071100     END-IF                                                               
071200                                                                          
071300     IF CORR-HILITE-FLDS                                                  
071400       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
071500     END-IF                                                               
071600                                                                          
071700     IF PART-SUPERSEDED                                                   
071800       MOVE ERR-PART-SUPERSEDED   TO MED-IDMFSFEL                         
071900     END-IF                                                               
072000                                                                          
072100     IF PRICE-IS-MISSING                                                  
072200       MOVE ERR-PRICE-IS-MISSING  TO MED-IDMFSFEL                         
072300     END-IF                                                               
072400                                                                          
072500     IF WRONG-QUANTITY                                                    
072600       MOVE ERR-WRONG-QUANTITY    TO MED-IDMFSFEL                         
072700     END-IF                                                               
072800                                                                          
072900     IF WRONG-CMD-CODE                                                    
073000       MOVE ERR-WRONG-CMD-CODE    TO MED-IDMFSFEL                         
073100     END-IF                                                               
073200                                                                          
073300     IF MESSAGE-MISSING                                                   
073400       MOVE ERR-MESSAGE-MISSING   TO MED-IDMFSFEL                         
073500     END-IF                                                               
073600                                                                          
073700     IF MISSING-PART                                                      
073800       MOVE ERR-PART-IS-MISSING   TO MED-IDMFSFEL                         
073900     END-IF                                                               
074000                                                                          
074100     CALL WMEDKONV USING MED-WMEDAREA                                     
074200     MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                                 
074300     PERFORM MFS-ROER-EJ-FAELT-UT                                         
074400     PERFORM MFS-ROER-EJ-FAELT-IN                                         
074500*                                                                         
074600     IF FIRST-PAGE                                                        
074700       MOVE INF-FIRST-PAGE      TO MED-IDMFSINF                           
074800     END-IF                                                               
074900                                                                          
075000     IF MORE-INFO-EXISTS                                                  
075100       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
075200     END-IF                                                               
075300                                                                          
075400     IF PRESS-PF11                                                        
075500       MOVE INF-PRESS-PF11      TO MED-IDMFSINF                           
075600     END-IF                                                               
075700                                                                          
075800     IF AVROP-SAKNAS                                                      
075900       MOVE INF-AVROP-SAKNAS    TO MED-IDMFSINF                           
076000     END-IF                                                               
076100                                                                          
076200     IF UPDATE-DONE                                                       
076300       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
076400     END-IF                                                               
076500                                                                          
076600     CALL WMEDKONV USING MED-WMEDAREA                                     
076700     MOVE MED-TEMFSINF  TO MOD-TEMFSINF                                   
076800                                                                          
076900     IF INDATA-OK                                                         
077000       MOVE ALL '+'       TO MSGI-WMSGINIT                                
077100       MOVE '001'         TO MSGI-KDCALL                                  
077200       MOVE WS-IDDC       TO WS-IDDC-LOCAL-DATE                           
077300       MOVE WS-IDDC-LOCAL TO MSGI-IDUSER                                  
077400       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
077500     END-IF                                                               
077600     .                                                                    
077700     EJECT                                                                
077800                                                                          
077900 FB-MOVE-RESP-TO-MOD  SECTION.                                            
078000     IF GODK-MID                                                          
078100       MOVE WS-IDLEVNR                TO MOD-IDLEVNR-UT                   
078200       MOVE WS-IDFS                   TO MOD-IDFS-UT                      
078300                                                                          
078400       IF WS-TIAVIDAT-N > 0                                               
078500         MOVE WS-TIAVIDAT             TO MOD-TIAVIDAT-UT                  
078600       ELSE                                                               
078700         MOVE RESP-TIAVIDAT           TO MOD-TIAVIDAT-UT                  
078800         INSPECT MOD-TIAVIDAT-UT REPLACING                                
078900                                 LEADING ZERO BY SPACE                    
079000       END-IF                                                             
079100                                                                          
079200       MOVE WS-ADINLOMR-PRT           TO MOD-ADINLOMR-PRT-UT              
079300       MOVE WS-IDDC                   TO MOD-IDDC-UT                      
079400       MOVE WS-IDLBBET                TO MOD-IDLBBET-UT                   
079500       MOVE WS-KDRT                   TO MOD-KDRT-UT                      
079600*                                                                         
079700       INSPECT MOD-KDRT-UT REPLACING LEADING ZERO BY SPACE                
079800       IF MOD-KDRT-UT = SPACE                                             
079900         MOVE ' 0'           TO MOD-KDRT-UT                               
080000       END-IF                                                             
080100*                                                                         
080200       MOVE RESP-IDRADNR-START        TO MOD-IDRADNR-ENTER                
080300       MOVE RESP-IDRADNR-INL-START    TO MOD-IDRADNR-INL-ENTER            
080400       MOVE RESP-IDRADNR-NEXT         TO MOD-IDRADNR-NEXT                 
080500       MOVE RESP-IDRADNR-INL-NEXT     TO MOD-IDRADNR-INL-NEXT             
080600*                                                                         
080700       IF RESP-TIAVIDAT = ALL '+'                                         
080800         MOVE MFS-ROER-EJ-FAELT  TO MOD-TIAVIDAT                          
080900       ELSE                                                               
081000         IF RESP-TIAVIDAT = SPACE                                         
081100           MOVE MFS-RENSA-FAELT    TO MOD-TIAVIDAT                        
081200         ELSE                                                             
081300           MOVE RESP-TIAVIDAT      TO MOD-TIAVIDAT                        
081400         END-IF                                                           
081500       END-IF                                                             
081600*                                                                         
081700       IF RESP-IDLBBET  = ALL '+'                                         
081800         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLBBET                           
081900       ELSE                                                               
082000         IF RESP-IDLBBET  = SPACE                                         
082100           MOVE MFS-RENSA-FAELT    TO MOD-IDLBBET                         
082200         ELSE                                                             
082300           MOVE RESP-IDLBBET       TO MOD-IDLBBET                         
082400         END-IF                                                           
082500       END-IF                                                             
082600*                                                                         
082700       IF RESP-TIANKDAG = ALL '+'                                         
082800         MOVE MFS-ROER-EJ-FAELT  TO MOD-TIANKDAG                          
082900       ELSE                                                               
083000         IF RESP-TIANKDAG = SPACE                                         
083100           MOVE MFS-RENSA-FAELT    TO MOD-TIANKDAG                        
083200         ELSE                                                             
083300           MOVE RESP-TIANKDAG      TO MOD-TIANKDAG                        
083400         END-IF                                                           
083500       END-IF                                                             
083600*                                                                         
083700       IF RESP-IDLEVNR  = ALL '+'                                         
083800         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR                           
083900       ELSE                                                               
084000         IF RESP-IDLEVNR  = SPACE                                         
084100           MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR                         
084200         ELSE                                                             
084300           MOVE RESP-IDLEVNR       TO MOD-IDLEVNR                         
084400         END-IF                                                           
084500       END-IF                                                             
084600*                                                                         
084700       IF RESP-TIAVIDAT-UPD = SPACES                                      
084800         MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT-UPD                    
084900       ELSE                                                               
085000         IF RESP-TIAVIDAT-UPD = ALL '+'                                   
085100           MOVE MFS-ROER-EJ-FAELT  TO MOD-TIAVIDAT-UPD                    
085200         ELSE                                                             
085300           MOVE RESP-TIAVIDAT-UPD  TO MOD-TIAVIDAT-UPD                    
085400         END-IF                                                           
085500       END-IF                                                             
085600       MOVE RESP-TIAVIDAT-UPD-ATTR TO MOD-TIAVIDAT-UPD-ATTR               
085700*                                                                         
085800       MOVE RESP-IDLBBET-UPD-ATTR  TO MOD-IDLBBET-UPD-ATTR                
085900       IF RESP-IDLBBET-UPD = SPACES                                       
086000         MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-UPD                     
086100       ELSE                                                               
086200         IF RESP-IDLBBET-UPD = ALL '+'                                    
086300           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLBBET-UPD                     
086400         ELSE                                                             
086500           MOVE RESP-IDLBBET-UPD   TO MOD-IDLBBET-UPD                     
086600         END-IF                                                           
086700       END-IF                                                             
086800*                                                                         
086900       MOVE RESP-TIANKDAG-UPD-ATTR TO MOD-TIANKDAG-UPD-ATTR               
087000       IF RESP-TIANKDAG-UPD = SPACES                                      
087100         MOVE MFS-RENSA-FAELT      TO MOD-TIANKDAG-UPD                    
087200       ELSE                                                               
087300         IF RESP-TIANKDAG-UPD = ALL '+'                                   
087400           MOVE MFS-ROER-EJ-FAELT  TO MOD-TIANKDAG-UPD                    
087500         ELSE                                                             
087600           MOVE RESP-TIANKDAG-UPD  TO MOD-TIANKDAG-UPD                    
087700         END-IF                                                           
087800       END-IF                                                             
087900*                                                                         
088000       MOVE RESP-IDLEVNR-UPD-ATTR  TO MOD-IDLEVNR-UPD-ATTR                
088100       IF RESP-IDLEVNR-UPD = SPACES                                       
088200         MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-UPD                     
088300       ELSE                                                               
088400         IF RESP-IDLEVNR-UPD = ALL '+'                                    
088500           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-UPD                     
088600         ELSE                                                             
088700           MOVE RESP-IDLEVNR-UPD   TO MOD-IDLEVNR-UPD                     
088800         END-IF                                                           
088900       END-IF                                                             
089000*                                                                         
089100       MOVE RESP-FLBORT-UPD-ATTR   TO MOD-FLBORT-UPD-ATTR                 
089200       IF RESP-FLBORT-UPD = SPACES                                        
089300         MOVE MFS-RENSA-FAELT      TO MOD-FLBORT-UPD                      
089400       ELSE                                                               
089500         IF RESP-FLBORT-UPD = ALL '+'                                     
089600           MOVE MFS-ROER-EJ-FAELT  TO MOD-FLBORT-UPD                      
089700         ELSE                                                             
089800           MOVE RESP-FLBORT-UPD    TO MOD-FLBORT-UPD                      
089900         END-IF                                                           
090000       END-IF                                                             
090100*                                                                         
090200       MOVE RESP-IDFTG-UPD-ATTR    TO MOD-IDFTG-UPD-ATTR                  
090300       IF RESP-IDFTG-UPD = SPACES                                         
090400         MOVE MFS-RENSA-FAELT      TO MOD-IDFTG-UPD                       
090500       ELSE                                                               
090600         IF RESP-IDFTG-UPD = ALL '+'                                      
090700           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFTG-UPD                       
090800         ELSE                                                             
090900           MOVE RESP-IDFTG-UPD     TO MOD-IDFTG-UPD                       
091000         END-IF                                                           
091100       END-IF                                                             
091200*                                                                         
091300       MOVE RESP-IDKONTO-UPD-ATTR  TO MOD-IDKONTO-UPD-ATTR                
091400       IF RESP-IDKONTO-UPD = SPACES                                       
091500         MOVE MFS-RENSA-FAELT      TO MOD-IDKONTO-UPD                     
091600       ELSE                                                               
091700         IF RESP-IDKONTO-UPD = ALL '+'                                    
091800           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKONTO-UPD                     
091900         ELSE                                                             
092000           MOVE RESP-IDKONTO-UPD   TO MOD-IDKONTO-UPD                     
092100         END-IF                                                           
092200       END-IF                                                             
092300*                                                                         
092400       MOVE RESP-IDANALYS-UPD-ATTR TO MOD-IDANALYS-UPD-ATTR               
092500       IF RESP-IDANALYS-UPD = SPACES                                      
092600         MOVE MFS-RENSA-FAELT      TO MOD-IDANALYS-UPD                    
092700       ELSE                                                               
092800         IF RESP-IDANALYS-UPD = ALL '+'                                   
092900           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDANALYS-UPD                    
093000         ELSE                                                             
093100           MOVE RESP-IDANALYS-UPD  TO MOD-IDANALYS-UPD                    
093200         END-IF                                                           
093300       END-IF                                                             
093400*                                                                         
093500       MOVE RESP-IDKST-UPD-ATTR    TO MOD-IDKST-UPD-ATTR                  
093600       IF RESP-IDKST-UPD = SPACES                                         
093700         MOVE MFS-RENSA-FAELT      TO MOD-IDKST-UPD                       
093800       ELSE                                                               
093900         IF RESP-IDKST-UPD = ALL '+'                                      
094000           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKST-UPD                       
094100         ELSE                                                             
094200           MOVE RESP-IDKST-UPD     TO MOD-IDKST-UPD                       
094300         END-IF                                                           
094400       END-IF                                                             
094500*                                                                         
094600       MOVE RESP-IDARTNR-UPD-ATTR  TO MOD-IDARTNR-UPD-ATTR                
094700       IF RESP-IDARTNR-UPD  = SPACES                                      
094800         MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-UPD                     
094900       ELSE                                                               
095000         IF RESP-IDARTNR-UPD = ALL '+'                                    
095100           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-UPD                     
095200         ELSE                                                             
095300**         IF RESP-IDARTNR-UPD IS NUMERIC                                 
095400           MOVE RESP-IDARTNR-UPD   TO MOD-IDARTNR-UPD                     
095500**         END-IF                                                         
095600         END-IF                                                           
095700       END-IF                                                             
095800*                                                                         
095900       MOVE RESP-KDRT-UPD-ATTR    TO MOD-KDRT-UPD-ATTR                    
096000       IF RESP-KDRT-UPD = SPACES                                          
096100         MOVE MFS-RENSA-FAELT      TO MOD-KDRT-UPD                        
096200       ELSE                                                               
096300         IF RESP-KDRT-UPD    = ALL '+'                                    
096400           MOVE MFS-ROER-EJ-FAELT  TO MOD-KDRT-UPD                        
096500         ELSE                                                             
096600           MOVE RESP-KDRT-UPD      TO MOD-KDRT-UPD                        
096700         END-IF                                                           
096800       END-IF                                                             
096900*                                                                         
097000       MOVE RESP-KVAVIS-UPD-ATTR   TO MOD-KVAVIS-UPD-ATTR                 
097100       IF RESP-KVAVIS-UPD  = SPACES                                       
097200         MOVE MFS-RENSA-FAELT      TO MOD-KVAVIS-UPD                      
097300       ELSE                                                               
097400         IF RESP-KVAVIS-UPD = ALL '+'                                     
097500           MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS-UPD                      
097600         ELSE                                                             
097700           MOVE RESP-KVAVIS-UPD    TO MOD-KVAVIS-UPD                      
097800         END-IF                                                           
097900       END-IF                                                             
098000*                                                                         
098100       IF RESP-IDFTG = ALL '+'                                            
098200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFTG                             
098300       ELSE                                                               
098400         IF RESP-IDFTG = SPACE                                            
098500           MOVE MFS-RENSA-FAELT    TO MOD-IDFTG                           
098600         ELSE                                                             
098700           MOVE RESP-IDFTG         TO MOD-IDFTG                           
098800         END-IF                                                           
098900       END-IF                                                             
099000*                                                                         
099100       IF RESP-IDKONTO = ALL '+'                                          
099200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKONTO                           
099300       ELSE                                                               
099400         IF RESP-IDKONTO = SPACE                                          
099500           MOVE MFS-RENSA-FAELT    TO MOD-IDKONTO                         
099600         ELSE                                                             
099700           MOVE RESP-IDKONTO       TO MOD-IDKONTO                         
099800         END-IF                                                           
099900       END-IF                                                             
100000*                                                                         
100100       IF RESP-IDANALYS = ALL '+'                                         
100200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDANALYS                          
100300       ELSE                                                               
100400         IF RESP-IDANALYS = SPACE                                         
100500           MOVE MFS-RENSA-FAELT    TO MOD-IDANALYS                        
100600         ELSE                                                             
100700           MOVE RESP-IDANALYS      TO MOD-IDANALYS                        
100800         END-IF                                                           
100900       END-IF                                                             
101000*                                                                         
101100       IF RESP-IDKST = ALL '+'                                            
101200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKST                             
101300       ELSE                                                               
101400         IF RESP-IDKST = SPACE                                            
101500           MOVE MFS-RENSA-FAELT    TO MOD-IDKST                           
101600         ELSE                                                             
101700           MOVE RESP-IDKST         TO MOD-IDKST                           
101800         END-IF                                                           
101900       END-IF                                                             
102000*                                                                         
102100       MOVE +1 TO INDX                                                    
102200       PERFORM UNTIL INDX > RESP-KVRADER                                  
102300         MOVE RESP-KDCMD-LINE-ATTR(INDX) TO                               
102400                                      MOD-KDCMD-RAD-ATTR(INDX)            
102500         IF RESP-KDCMD-LINE(INDX) = SPACE                                 
102600           MOVE MFS-RENSA-FAELT         TO MOD-KDCMD-RAD(INDX)            
102700         ELSE                                                             
102800           IF RESP-KDCMD-LINE(INDX) = ALL '+'                             
102900             MOVE MFS-ROER-EJ-FAELT     TO MOD-KDCMD-RAD(INDX)            
103000           ELSE                                                           
103100             MOVE RESP-KDCMD-LINE(INDX) TO MOD-KDCMD-RAD(INDX)            
103200           END-IF                                                         
103300         END-IF                                                           
103400*                                                                         
103500         MOVE RESP-IDARTNR-LINE-ATTR(INDX)  TO                            
103600                                        MOD-IDARTNR-RAD-ATTR(INDX)        
103700         IF RESP-IDARTNR-LINE(INDX) = SPACE                               
103800           MOVE MFS-RENSA-FAELT           TO MOD-IDARTNR-RAD(INDX)        
103900         ELSE                                                             
104000           IF RESP-IDARTNR-LINE(INDX) = ALL '+'                           
104100             MOVE MFS-ROER-EJ-FAELT       TO MOD-IDARTNR-RAD(INDX)        
104200           ELSE                                                           
104300             MOVE RESP-IDARTNR-LINE(INDX) TO MOD-IDARTNR-RAD(INDX)        
104400           END-IF                                                         
104500         END-IF                                                           
104600*                                                                         
104700         MOVE RESP-KVAVIS-LINE-ATTR(INDX)  TO                             
104800                                        MOD-KVAVIS-RAD-ATTR(INDX)         
104900         IF RESP-KVAVIS-LINE(INDX)  = SPACE                               
105000           MOVE MFS-RENSA-FAELT          TO MOD-KVAVIS-RAD(INDX)          
105100         ELSE                                                             
105200           IF RESP-KVAVIS-LINE(INDX) = ALL '+'                            
105300             MOVE MFS-ROER-EJ-FAELT      TO MOD-KVAVIS-RAD(INDX)          
105400           ELSE                                                           
105500             MOVE RESP-KVAVIS-LINE(INDX) TO MOD-KVAVIS-RAD(INDX)          
105600           END-IF                                                         
105700         END-IF                                                           
105800*                                                                         
105900         MOVE RESP-KDRT-LINE-ATTR(INDX)  TO                               
106000                                        MOD-KDRT-RAD-ATTR(INDX)           
106100         IF RESP-KDRT-LINE(INDX)  = SPACE                                 
106200           MOVE MFS-RENSA-FAELT        TO MOD-KDRT-RAD(INDX)              
106300         ELSE                                                             
106400           IF RESP-KDRT-LINE(INDX) = ALL '+'                              
106500             MOVE MFS-ROER-EJ-FAELT    TO MOD-KDRT-RAD(INDX)              
106600           ELSE                                                           
106700             MOVE RESP-KDRT-LINE(INDX) TO MOD-KDRT-RAD(INDX)              
106800           END-IF                                                         
106900         END-IF                                                           
107000*                                                                         
107100         IF RESP-KVKOLLI-LINE(INDX) = ALL '+'                             
107200           MOVE MFS-ROER-EJ-FAELT      TO MOD-KVKOLLI-RAD(INDX)           
107300          ELSE                                                            
107400           IF RESP-KVKOLLI-LINE(INDX) = SPACE                             
107500             MOVE MFS-RENSA-FAELT         TO MOD-KVKOLLI-RAD(INDX)        
107600           ELSE                                                           
107700             MOVE RESP-KVKOLLI-LINE(INDX) TO MOD-KVKOLLI-RAD(INDX)        
107800           END-IF                                                         
107900         END-IF                                                           
108000*                                                                         
108100         IF RESP-IDOKOLLI-LINE(INDX) = ALL '+' OR SPACE                   
108500           MOVE MFS-RENSA-FAELT     TO MOD-IDOKOLLI-RAD(INDX)             
108600         ELSE                                                             
108700           MOVE RESP-IDOKOLLI-LINE(INDX) TO MOD-IDOKOLLI-RAD(INDX)        
108900         END-IF                                                           
109000                                                                          
109100         IF RESP-KVINLART-LINE(INDX) = ALL '+' OR SPACE                   
109500             MOVE MFS-RENSA-FAELT      TO MOD-KVINLART-RAD(INDX)          
109600         ELSE                                                             
109700             MOVE RESP-KVINLART-LINE(INDX)                                
109800                                       TO MOD-KVINLART-RAD(INDX)          
110000         END-IF                                                           
110100                                                                          
110200         IF RESP-IDARTNR-SPAR-LINE(INDX) = ALL '+'                        
110300           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-SPAR(INDX)              
110400         ELSE                                                             
110500           IF RESP-IDARTNR-SPAR-LINE(INDX) = SPACE                        
110600             MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR-SPAR(INDX)              
110700           ELSE                                                           
110800             MOVE RESP-IDARTNR-SPAR-LINE(INDX)                            
110900                                   TO MOD-IDARTNR-SPAR(INDX)              
111000           END-IF                                                         
111100         END-IF                                                           
111200                                                                          
111300         IF RESP-IDRADNR-SPAR-LINE(INDX) = ALL '+'                        
111400           MOVE MFS-ROER-EJ-FAELT    TO MOD-IDRADNR-SPAR(INDX)            
111500         ELSE                                                             
111600           IF RESP-IDRADNR-SPAR-LINE(INDX) = SPACE                        
111700             MOVE MFS-RENSA-FAELT    TO MOD-IDRADNR-SPAR(INDX)            
111800           ELSE                                                           
111900            MOVE RESP-IDRADNR-SPAR-LINE(INDX)                             
112000                                     TO MOD-IDRADNR-SPAR(INDX)            
112100           END-IF                                                         
112200         END-IF                                                           
112300                                                                          
112400         IF RESP-IDRADNR-INL-SPAR-LINE(INDX) = ALL '+'                    
112500           MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-INL-SPAR(INDX)           
112600         ELSE                                                             
112700           IF RESP-IDRADNR-INL-SPAR-LINE(INDX) = SPACE                    
112800             MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-INL-SPAR(INDX)           
112900           ELSE                                                           
113000             MOVE RESP-IDRADNR-INL-SPAR-LINE(INDX)                        
113100                                    TO MOD-IDRADNR-INL-SPAR(INDX)         
113200           END-IF                                                         
113300         END-IF                                                           
113400                                                                          
113500         ADD +1                 TO INDX                                   
113600       END-PERFORM                                                        
113700     END-IF                                                               
113800                                                                          
113900*                                                                         
114000* CLOSE/CLEAR THE REMAINING LINES ON THE SCREEN                           
114100     PERFORM UNTIL INDX    >  MAX-KVRADER                                 
114200       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
114300       MOVE MFS-STAENG-FAELT TO MOD-KDCMD-RAD-ATTR   (INDX)               
114400                                MOD-IDARTNR-RAD-ATTR (INDX)               
114500                                MOD-KVAVIS-RAD-ATTR  (INDX)               
114600                                MOD-KDRT-RAD-ATTR    (INDX)               
114700       MOVE ZERO             TO MOD-IDRADNR-INL-SPAR (INDX)               
114800                                MOD-IDARTNR-SPAR (INDX)                   
114900       ADD +1                 TO INDX                                     
115000     END-PERFORM                                                          
115100     .                                                                    
115200     EJECT                                                                
115300                                                                          
115400 MFS-RENSA-FAELT-UT SECTION.                                              
115500                                                                          
115600     MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT                            
115700                                  MOD-IDLBBET                             
115800                                  MOD-TIANKDAG                            
115900                                  MOD-IDLEVNR                             
116000                                  MOD-FLBORT                              
116100                                  MOD-IDFTG                               
116200                                  MOD-IDKONTO                             
116300                                  MOD-IDANALYS                            
116400                                  MOD-IDKST                               
116500                                  MOD-IDRADNR-ENTER                       
116600                                  MOD-IDRADNR-NEXT                        
116700                                  MOD-IDRADNR-INL-ENTER                   
116800                                  MOD-IDRADNR-INL-NEXT                    
116900                                                                          
117000     MOVE +1                   TO INDX                                    
117100     PERFORM UNTIL INDX        >  MAX-KVRADER                             
117200         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
117300         ADD +1                TO INDX                                    
117400     END-PERFORM                                                          
117500     .                                                                    
117600     SKIP2                                                                
117700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
117800                                                                          
117900     MOVE MFS-RENSA-FAELT      TO MOD-KDCMD-RAD      (INDX)               
118000                                  MOD-IDARTNR-RAD    (INDX)               
118100                                  MOD-KVAVIS-RAD     (INDX)               
118200                                  MOD-KDRT-RAD       (INDX)               
118300                                  MOD-KVKOLLI-RAD    (INDX)               
118400                                  MOD-IDOKOLLI-RAD   (INDX)               
118500                                  MOD-KVINLART-RAD   (INDX)               
118600                                  MOD-IDARTNR-SPAR (INDX)                 
118700                                  MOD-IDRADNR-INL-SPAR (INDX)             
118800                                  MOD-IDRADNR-SPAR   (INDX)               
118900     .                                                                    
119000     EJECT                                                                
119100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
119200                                                                          
119300     MOVE MFS-ROER-EJ-FAELT    TO MOD-TIAVIDAT                            
119400                                  MOD-IDLBBET                             
119500                                  MOD-TIANKDAG                            
119600                                  MOD-IDLEVNR                             
119700                                  MOD-FLBORT                              
119800                                  MOD-IDFTG                               
119900                                  MOD-IDKONTO                             
120000                                  MOD-IDANALYS                            
120100                                  MOD-IDKST                               
120200                                  MOD-IDRADNR-ENTER                       
120300                                  MOD-IDRADNR-NEXT                        
120400                                  MOD-IDRADNR-INL-ENTER                   
120500                                  MOD-IDRADNR-INL-NEXT                    
120600     MOVE +1                   TO INDX                                    
120700     PERFORM UNTIL INDX        >  MAX-KVRADER                             
120800         PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                 
120900         ADD +1                TO INDX                                    
121000     END-PERFORM                                                          
121100     .                                                                    
121200     SKIP2                                                                
121300 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
121400                                                                          
121500     MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMD-RAD      (INDX)               
121600                                  MOD-IDARTNR-RAD    (INDX)               
121700                                  MOD-KVAVIS-RAD     (INDX)               
121800                                  MOD-KDRT-RAD       (INDX)               
121900                                  MOD-KVKOLLI-RAD    (INDX)               
122000                                  MOD-IDOKOLLI-RAD   (INDX)               
122100                                  MOD-KVINLART-RAD   (INDX)               
122200                                  MOD-IDARTNR-SPAR (INDX)                 
122300                                  MOD-IDRADNR-INL-SPAR (INDX)             
122400                                  MOD-IDRADNR-SPAR   (INDX)               
122500     .                                                                    
122600     EJECT                                                                
122700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
122800                                                                          
122900     MOVE MFS-ROER-EJ-FAELT    TO MOD-TIAVIDAT-UPD                        
123000                                  MOD-IDLBBET-UPD                         
123100                                  MOD-TIANKDAG-UPD                        
123200                                  MOD-IDLEVNR-UPD                         
123300                                  MOD-FLBORT-UPD                          
123400                                  MOD-IDFTG-UPD                           
123500                                  MOD-IDKONTO-UPD                         
123600                                  MOD-IDANALYS-UPD                        
123700                                  MOD-IDKST-UPD                           
123800                                  MOD-IDARTNR-UPD                         
123900                                  MOD-KVAVIS-UPD                          
124000                                  MOD-KDRT-UPD                            
124100     .                                                                    
124200     SKIP2                                                                
124300 MFS-LAES-IN-IGEN SECTION.                                                
124400                                                                          
124500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIAVIDAT-UPD-ATTR                  
124600                                   MOD-IDLBBET-UPD-ATTR                   
124700                                   MOD-TIANKDAG-UPD-ATTR                  
124800                                   MOD-IDLEVNR-UPD-ATTR                   
124900                                   MOD-FLBORT-UPD-ATTR                    
125000                                   MOD-IDFTG-UPD-ATTR                     
125100                                   MOD-IDKONTO-UPD-ATTR                   
125200                                   MOD-IDANALYS-UPD-ATTR                  
125300                                   MOD-IDKST-UPD-ATTR                     
125400                                   MOD-IDARTNR-UPD-ATTR                   
125500                                   MOD-KVAVIS-UPD-ATTR                    
125600                                   MOD-KDRT-UPD-ATTR                      
125700     .                                                                    
125800     EJECT                                                                
125900* --- IMS SEKTIONER ---                                                   
126000     SKIP3                                                                
126100 IMS-GET-MSG SECTION.                                                     
126200                                                                          
126300     MOVE '  QC' TO GODK-STATUSKODER                                      
126400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
126500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     SKIP3                                                                
126900 IMS-INSERT-MSG SECTION.                                                  
127000                                                                          
127100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
127200       MOVE '0' TO MFS-KDHUVOMR                                           
127300     END-IF                                                               
127400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
127500     MOVE SPACE TO GODK-STATUSKODER                                       
127600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
127700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     .                                                                    
128000     EJECT                                                                
128100 IMS-STATUSKONTROLL SECTION.                                              
128200                                                                          
128300     SET STATUS-IX TO 1                                                   
128400     SEARCH GODK-STATUS                                                   
128500       AT END                                                             
128600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
128700         DELIMITED BY SIZE INTO FELTEXT                                   
128800         CALL FELLOG                                                      
128900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
129000         CONTINUE                                                         
129100     END-SEARCH                                                           
129200     .                                                                    
129300     EJECT                                                                
