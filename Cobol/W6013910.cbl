000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013910.                                                
000400*AUTHOR.         GERRY  CARMICHAEL.                                       
000500*DATE-WRITTEN.   92/09/02.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        MOTTAGNINGSKONTROLL INLEVERANS                                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR W6UPFA (W6L1)                              
001300*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001400*        PROGRAMMET UPPDATERAR W6KVAH (W6D2)                              
001500*        PROGRAMMET UPPDATERAR W6KVAE (W6H7)                              
001600*        PROGRAMMET LÄSER      WDF5                                       
001700*        PROGRAMMET LÄSER      WLLEVA (W6F1)                              
001800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001900*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002000*        PROGRAMMET LÄSER      W6INLC (W6D1B1)                            
002100*        PROGRAMMET LÄSER              WDB6                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        REQU:   W60139I1                                                 
002500*                                                                         
002600*    UTDATA.                                                              
002700*        RESP:   W60139O1                                                 
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6013910'.            
003700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003800 77  NUM-KDKVAULG                PIC 9(1).                                
003900 77  NUM-KDKVATYP                PIC 9(1).                                
004000 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
004100 77  WS-IDKR                     PIC X(5)    VALUE SPACE.                 
004200 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
004300 77  WS-ADINLOMR                 PIC X(4)    VALUE SPACE.                 
004400 77  WS-ADINLOMR-NXT             PIC X(4)    VALUE SPACE.                 
004500 77  WS-KDINLQ                   PIC X(1)    VALUE SPACE.                 
004600 77  WS-BEFT-FOM                 PIC X(2)    VALUE SPACE.                 
004700 77  WS-BEFT-TOM                 PIC X(2)    VALUE SPACE.                 
004800 77  WS-FLINLFB                  PIC X(1)    VALUE SPACE.                 
004900 77  WS-FLUPG                    PIC X(1)    VALUE SPACE.                 
005000 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
005100 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
005200 77  WS-ADKVAULG                 PIC X(2)    VALUE SPACE.                 
005300 77  WS-KDKVAKTL                 PIC X(4)    VALUE '0000'.                
005400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 77  WS-FLADM                    PIC X(1)    VALUE 'J'.                   
005700 77  WS-CP-UNICODE               PIC X(4)    VALUE 'UTF8'.                
005800 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
005900*                                                                         
006000                                                                          
006100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006300                                                                          
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  YES                         PIC X       VALUE 'Y'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700                                                                          
006800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
007000                                                                          
007100       EJECT                                                              
007200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007300                                                                          
007400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007500     88  INDATA-OK                           VALUE 'J'.                   
007600     88  INDATA-FEL                          VALUE 'N'.                   
007700                                                                          
007800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007900     88  NYCKLAR-OK                          VALUE 'J'.                   
008000     88  NYCKLAR-FEL                         VALUE 'N'.                   
008100                                                                          
008200 77  SEGMENT-SW                  PIC X       VALUE 'N'.                   
008300     88  NEXT-SEGMENT                        VALUE 'J'.                   
008400                                                                          
008500 77  NYUPPLAEGG-SW               PIC X       VALUE 'N'.                   
008600     88  NYUPPLAEGG                          VALUE 'J'.                   
008700                                                                          
008800 77  ETT-FLAGGA-NEJ-SW           PIC X       VALUE 'N'.                   
008900     88  ETT-FLAGGA-NEJ                      VALUE 'J'.                   
009000                                                                          
009100 77  6202-SW                     PIC X       VALUE 'N'.                   
009200     88  6202-TRANS                          VALUE 'J'.                   
009300                                                                          
009400 77  6202-UPDATE-SW              PIC X       VALUE 'J'.                   
009500     88  6202-UPDATE                         VALUE 'J'.                   
009600     88  6202-NO-UPDATE                      VALUE 'N'.                   
009700                                                                          
009800 77  HOPP-TILL-6202-SW           PIC X       VALUE 'N'.                   
009900     88  HOPP-TILL-6202                      VALUE 'J'.                   
010000                                                                          
010100 77  TRYCK-UPPDATERING-SW        PIC X       VALUE 'N'.                   
010200     88  TRYCK-UPPDATERING                   VALUE 'J'.                   
010300                                                                          
010400 77  WS-KDMFSFOR                 PIC X       VALUE SPACE.                 
010500     88  WS-SWEDISH-TEXT                     VALUE '1'.                   
010600     88  WS-ENGLISH-TEXT                     VALUE '2'.                   
010700                                                                          
010800 01  ALL-SPACE.                                                           
010900     03 FILLER                   PIC X(80)   VALUE SPACE.                 
011000 01  ALL-PLUS.                                                            
011100     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
011110 01  ALL-UTF8-SPACE.                                                      
011120     03 FILLER                   PIC X(50)   VALUE ALL X'20'.             
011130 01  ALL-UTF8-PLUS.                                                       
011140     03 FILLER                   PIC X(50)   VALUE ALL X'2B'.             
011150                                                                          
011200     EJECT                                                                
011300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011400 01  GENERELLA-SUBPROGRAM.                                                
011500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
011800     EJECT                                                                
011900*01 -COPY WMSGINIT                                                        
012000     SKIP3                                                                
012100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012200*01 -COPY WMEDAREA                                                        
012300     SKIP3                                                                
012400 01  TEST-IDARTNR                PIC S9(9)   COMP-3.                      
012500     SKIP3                                                                
012600*01  FILLER   -COPY WWBYT03  -RED TEST-IDARTNR.                           
012700     SKIP3                                                                
012800*01  FILLER   -COPY WWBYT07  -RED TEST-IDARTNR.                           
012900     SKIP3                                                                
013000*    --- UNDERLAG FÖR KVALITETSKONTROLL                                   
013100*    -COPY W426KTL                                                        
013200     EJECT                                                                
013300 01  MESSAGE-CODES.                                                       
013400     03  ERR-NO-UPDATE           PIC X(3)    VALUE '007'.                 
013500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
013600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
013700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
013800     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '025'.                 
013900     03  ERR-IST-MISSING         PIC X(3)    VALUE '363'.                 
014000                                                                          
014100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
014200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
014300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
014400     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
014500     03  INF-PRESS-PF23          PIC X(3)    VALUE '013'.                 
014600     03  INF-INFO-MISSING        PIC X(3)    VALUE '041'.                 
014700     03  INF-QTY-IR-EXISTS       PIC X(3)    VALUE '351'.                 
014800     03  SPLITT-PRESS-PF23       PIC X(3)    VALUE '353'.                 
014900     03  NO-IR-CREATED           PIC X(3)    VALUE '364'.                 
015000     EJECT                                                                
015100*                                                                         
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015400     SKIP3                                                                
015500*01  -COPY WMFSAREA                                                       
015600     EJECT                                                                
015610 01  ALT-MSG-AREA-IMS.                                                    
015630  03     ALT-LL-IMS              PIC S9(4) VALUE +182 COMP SYNC.          
015640  03     ALT-Z1-IMS              PIC X(1)  VALUE LOW-VALUE.               
015650  03     ALT-Z2-IMS              PIC X(1)  VALUE LOW-VALUE.               
015660  03     ALT-TRANSKOD-IMS        PIC X(8)  VALUE 'W6T202X '.              
015670  03     ALT-IDTRANS-IMS         PIC X(4)  VALUE '6139'.                  
015680  03     ALT-SPRAK-IMS           PIC X(1).                                
015690* 03     MID -COPY W6I20201   -PRE ALT-                                   
015691     EJECT                                                                
015700 01  ALT-MSG-IO-AREA.                                                     
015900  03     ALT-LL                  PIC S9(4) COMP.                          
016000  03     ALT-Z1                  PIC X(1)  VALUE LOW-VALUE.               
016100  03     ALT-Z2                  PIC X(1)  VALUE LOW-VALUE.               
016200  03     ALT-TRANSKOD            PIC X(8)  VALUE 'W6W202T '.              
016510  03     ALT-REQU-6202.                                                   
016520*      05  -COPY WZ01REQU     -PRE ALT-                                   
016530*      05  -COPY W60202I1     -PRE ALT-                                   
016600     EJECT                                                                
016610 01  FILLER                      PIC X(16)   VALUE 'DC CODES   '.         
016630*   -COPY WWDC99                                                          
016700 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
016800*01  -COPY WTRAUTF8                                                       
016900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017000*                                                                         
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-IDARTNR-X.                                                     
017600         05 W-IDARTNR            PIC S9(9)  VALUE ZERO COMP-3.            
017700     03  W-IDDC-X.                                                        
017800         05 W-IDDC               PIC  X(2)  VALUE SPACE.                  
017900     03  W-IDLEVNR-X.                                                     
018000         05  W-IDLEVNR           PIC  X(5)  VALUE SPACE.                  
018100     03  W-IDLOPNRM-X.                                                    
018200         05  W-IDLOPNRM          PIC S9(9)  VALUE ZERO COMP-3.            
018300     03  W-W6H7CSEQ-X.                                                    
018400         05  W-IDLOPNRM-CSEQ     PIC S9(9)  VALUE ZERO COMP-3.            
018500         05  W-DAAVSDAT-CSEQ     PIC  9(8)  VALUE ZERO.                   
018600     03  W-IDKR-X.                                                        
018700         05  W-IDKR              PIC  9(5)  VALUE ZERO.                   
018800     03  W-IDKVAINF-X.                                                    
018900         05  W-IDKVAINF          PIC  9(2)  VALUE ZERO.                   
019000     03  W-WDD3BSEQ-X.                                                    
019100         05  W-WDD3BSEQ          PIC S9(9)  VALUE ZERO COMP-3.            
019200     03  W-IDSKYLT-KEY-X.                                                 
019300         05 W-IDSKYLT-KEY        PIC X(3)   VALUE SPACE.                  
019400     03  W-W6D1BSEQ-X.                                                    
019500         05  W-BSEQ-IDLOPNRM     PIC S9(9)  VALUE ZERO COMP-3.            
019600                                                                          
019700     03  W-IDDC-B6-X.                                                     
019800         05 W-IDDC-B6            PIC X(2).                                
019900                                                                          
020000     SKIP2                                                                
020100*    --- STATUS-KOD FRÅN IMS                                              
020200 01  STATUS-WS                   PIC XX.                                  
020300     88  SEGMENT-FINNS                       VALUE '  '.                  
020400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020600     SKIP2                                                                
020700 01  GODK-STATUSKODER.                                                    
020800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020900     SKIP3                                                                
021000 01  SSA1                        PIC X(96).                               
021100 01  SSA2                        PIC X(64).                               
021200 01  SSA3                        PIC X(64).                               
021300     EJECT                                                                
021400*    --- IMS FUNKTIONSKODER                                               
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700*    ---  DLI INPUT-OUTPUT AREA                                           
021800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021900     SKIP3                                                                
022000 01  DLI-IO-AREA-UPFA01.                                                  
022100     03  W6UPFA01.                                                        
022200*        05  -COPY W6L101                                                 
022300     SKIP3                                                                
022400 01  DLI-IO-AREA-UPFA11.                                                  
022500     03  W6UPFA11.                                                        
022600*        05  -COPY W6L111                                                 
022700     SKIP3                                                                
022800 01  DLI-IO-AREA-UPFA12.                                                  
022900     03  W6UPFA12.                                                        
023000*        05  -COPY W6L112                                                 
023100     EJECT                                                                
023200 01  DLI-IO-AREA-BENA11.                                                  
023300     03  WLBENA11.                                                        
023400*        05  -COPY WDD311                                                 
023500     SKIP3                                                                
023600 01  DLI-IO-AREA-KVAH01.                                                  
023700     03  W6KVAH01.                                                        
023800*        05  -COPY W6D201  -PRE KVAH-                                     
023900     SKIP3                                                                
024000 01  DLI-IO-AREA-KVAH12.                                                  
024100     03  W6KVAH12.                                                        
024200*        05  -COPY W6D212  -PRE KVAH-                                     
024300     SKIP3                                                                
024400 01  DLI-IO-AREA-KVAH22.                                                  
024500     03  W6KVAH22.                                                        
024600*        05  -COPY W6D222  -PRE KVAH-                                     
024700     EJECT                                                                
024800 01  DLI-IO-AREA-INLA11.                                                  
024900     03  W6INLA11.                                                        
025000*        05  -COPY W6D111                                                 
025100     SKIP3                                                                
025200 01  DLI-IO-AREA-INLC01.                                                  
025300     03  W6INLC01.                                                        
025400*        05  -COPY W6D1B1  -PRE INLC-                                     
025500     EJECT                                                                
025600 01  DLI-IO-AREA-ARTC01.                                                  
025700     03  WLARTC01.                                                        
025800*        05  -COPY WDK601  -PRE ARTC-                                     
025900     SKIP3                                                                
026000 01  DLI-IO-AREA-WDF502.                                                  
026100     03  WDF502.                                                          
026200*        05  -COPY WDF502                                                 
026300     EJECT                                                                
026400 01  DLI-IO-AREA-KVAE01.                                                  
026500     03  W6KVAE01.                                                        
026600*        05  -COPY W6H701                                                 
026700                                                                          
026800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026900 01   DLI-IO-AREA-B601.                                                   
027000*     03  -COPY WDB601                                                    
027100     EJECT                                                                
027200 LINKAGE SECTION.                                                         
027300                                                                          
027400 01  REQU-AREA.                                                           
027500*    03 -COPY WZ01REQU                                                    
027600*    03 -COPY W60139I1                                                    
027700     EJECT                                                                
027800 01  RESP-AREA.                                                           
027900*    03 -COPY WZ01RESP                                                    
028000*    03 -COPY W60139O1                                                    
028100     EJECT                                                                
028200*01  -COPY W0009  -PRE ALT-                                               
028300     EJECT                                                                
028310*01  -COPY W0009  -PRE ALT-IMS-                                           
028320     EJECT                                                                
028400*01  -COPY W0008  -PRE UPFA-                                              
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700*01  -COPY W0008  -PRE INLA-                                              
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000*01  -COPY W0008  -PRE KVAH-                                              
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01  -COPY W0008  -PRE WDF5-                                              
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE LEVA-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE KVAE1-                                             
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE ARTC-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE BENA-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01  -COPY W0008  -PRE INLC-                                              
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008  -PRE KVAE2-                                             
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01  -COPY W0008  -PRE WDB6-                                              
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA                            
031800     ALT-PCB ALT-IMS-PCB UPFA-PCB INLA-PCB                                
031900     KVAH-PCB WDF5-PCB LEVA-PCB KVAE1-PCB ARTC-PCB BENA-PCB               
032000     INLC-PCB KVAE2-PCB WDB6-PCB.                                         
032100                                                                          
032200     PERFORM A-INIT                                                       
032300     PERFORM B-KOLLA-NYCKLAR                                              
032400     IF NYCKLAR-OK                                                        
032500       PERFORM IMS-GU-UPFA01                                              
032600       IF REQU-FLAGGA-KR-HOPP = (JA OR YES) AND SEGMENT-FINNS             
032700         PERFORM J-KOLLA-HOPP-TILL-6202                                   
032800       END-IF                                                             
032900       IF NOT HOPP-TILL-6202                                              
033000         IF REQU-UPDATE OR REQU-UPD-V                                     
033100           PERFORM G-KOLLA-INPUT                                          
033200           IF INDATA-OK                                                   
033300             PERFORM H-UPPDATERA                                          
033400           END-IF                                                         
033500         ELSE                                                             
033600           IF REQU-FIRST                                                  
033700             PERFORM C-FOERSTA-SIDA                                       
033800           ELSE                                                           
033900             IF REQU-NEXT                                                 
034000               PERFORM D-NAESTA-SIDA                                      
034100             ELSE                                                         
034200               PERFORM E-SAMMA-SIDA                                       
034300             END-IF                                                       
034400           END-IF                                                         
034500         END-IF                                                           
034600         IF INDATA-OK                                                     
034610           IF REQU-IDMSGVER = '001' AND                                   
034611              REQU-FLAGGA-KR-HOPP = JA OR YES                             
034620             CONTINUE                                                     
034630           ELSE                                                           
034700             PERFORM F-LAES-VISA-INFO                                     
034710           END-IF                                                         
034800         END-IF                                                           
034900       END-IF                                                             
035000     END-IF                                                               
035100     GOBACK                                                               
035200                                                                          
035300     .                                                                    
035400     EJECT                                                                
035500 A-INIT SECTION.                                                          
035600                                                                          
035700     MOVE ALL '+'       TO RESP-W60139O1                                  
035800     PERFORM MFS-FORM-ATTR                                                
035900                                                                          
036000     ACCEPT DAGENS-DATUM FROM DATE                                        
036100                                                                          
036200     MOVE 001          TO RESP-IDMSGVER                                   
036300     MOVE SPACE        TO RESP-IDMSG-ERROR                                
036400                          RESP-IDMSG-INFO                                 
036500                          RESP-IDELMT-ERROR                               
036600     .                                                                    
036700     EJECT                                                                
036800 B-KOLLA-NYCKLAR SECTION.                                                 
036900                                                                          
037000     MOVE JA                TO NYCKLAR-SW                                 
037100     MOVE NEJ               TO HOPP-TILL-6202-SW                          
037200                               NYUPPLAEGG-SW                              
037300                               ETT-FLAGGA-NEJ-SW                          
037400     EVALUATE REQU-IDSPRAK                                                
037500     WHEN 'SV'                                                            
037600        MOVE 'S  '          TO W-IDSKYLT-KEY                              
037700        MOVE WS-CP-EBCDIC   TO TRAUTF8-KDCP                               
037800        MOVE +1             TO SPRAK-IX                                   
037900*                              ALT-SPRAK                                  
038000                               WS-KDMFSFOR                                
038100     WHEN 'ZH'                                                            
038200        MOVE 'RCN'          TO W-IDSKYLT-KEY                              
038300        MOVE WS-CP-UNICODE  TO TRAUTF8-KDCP                               
038400        MOVE +2             TO SPRAK-IX                                   
038500*                              ALT-SPRAK                                  
038600                               WS-KDMFSFOR                                
038700     WHEN OTHER                                                           
038800        MOVE 'GB '          TO W-IDSKYLT-KEY                              
038900        MOVE WS-CP-EBCDIC   TO TRAUTF8-KDCP                               
039000        MOVE +2             TO SPRAK-IX                                   
039100*                              ALT-SPRAK                                  
039200                               WS-KDMFSFOR                                
039300     END-EVALUATE                                                         
039400                                                                          
039500     MOVE REQU-IDLOPNRM-KEY TO WS-IDLOPNRM                                
039600     IF WS-IDLOPNRM NUMERIC AND WS-IDLOPNRM  > ZERO                       
039700       MOVE WS-IDLOPNRM     TO W-IDLOPNRM                                 
039800                               W-BSEQ-IDLOPNRM                            
039900     ELSE                                                                 
040000       MOVE NEJ             TO NYCKLAR-SW                                 
040100     END-IF                                                               
040200                                                                          
040300     MOVE REQU-IDDC-KEY     TO W-IDDC-B6                                  
040400     PERFORM IMS-GU-WDB601                                                
040500                                                                          
040600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
040700       MOVE NEJ             TO NYCKLAR-SW                                 
040800     ELSE                                                                 
040900       MOVE DCS-IDDC        TO W-IDDC                                     
041000                               WS-IDDC                                    
041100     END-IF                                                               
041200                                                                          
041300     IF NYCKLAR-FEL                                                       
041400       MOVE ERR-WRONG-KEY   TO RESP-IDMSG-ERROR                           
041500       PERFORM MFS-RENSA-FAELT-IN                                         
041600       PERFORM MFS-RENSA-FAELT-UT                                         
041700     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042000 C-FOERSTA-SIDA SECTION.                                                  
042100                                                                          
042200     MOVE INF-FIRST-PAGE TO RESP-IDMSG-INFO                               
042300                                                                          
042400*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
042500     MOVE ZERO       TO RESP-IDKR-START                                   
042600                        RESP-IDKR-NEXT                                    
042700                        RESP-IDKVAINF-START                               
042800                        RESP-IDKVAINF-NEXT                                
042900     PERFORM MFS-RENSA-FAELT-IN                                           
043000     PERFORM MFS-RENSA-FAELT-UT                                           
043100     .                                                                    
043200     EJECT                                                                
043300 D-NAESTA-SIDA SECTION.                                                   
043400                                                                          
043500     IF REQU-IDKR-START > ZERO                                            
043600       MOVE REQU-IDKR-START     TO W-IDKR                                 
043700     END-IF                                                               
043800                                                                          
043900     IF REQU-IDKVAINF-START > ZERO                                        
044000       MOVE REQU-IDKVAINF-START TO W-IDKVAINF                             
044100     END-IF                                                               
044200                                                                          
044300     PERFORM MFS-RENSA-FAELT-IN                                           
044400     PERFORM MFS-RENSA-FAELT-UT                                           
044500     .                                                                    
044600     EJECT                                                                
044700 E-SAMMA-SIDA SECTION.                                                    
044800                                                                          
044900                                                                          
045000     IF REQU-INPUT = ALL '+'                                              
045100       PERFORM MFS-RENSA-FAELT-IN                                         
045200     ELSE                                                                 
045300       IF (REQU-BEANST NOT = ALL '+' AND REQU-BEANST NOT = SPACE)         
045310          OR (REQU-IDMSGVER = 101                                         
                    AND                                                         
045311              ((REQU-FLAGGA-GODK NOT = ALL '+' AND                        
045320                REQU-FLAGGA-GODK NOT = SPACE)  OR                         
045311               (REQU-IDUSER-APR  NOT = ALL '+' AND                        
045320                REQU-IDUSER-APR  NOT = SPACE)                             
                    )                                                           
                   )                                                            
045400         MOVE INF-PRESS-PF23 TO RESP-IDMSG-INFO                           
045500         MOVE 'PF23'         TO RESP-IDELMT-ERROR                         
045600       ELSE                                                               
045700         MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                           
045800         IF REQU-IDKR-START = ZERO AND REQU-IDKVAINF-START = ZERO         
045900           MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-GODK-ATTR           
045900           MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDUSER-APR-ATTR            
046000         ELSE                                                             
046100           MOVE MFS-STAENG-FAELT-NOMOD TO RESP-BEANST-ATTR                
046200         END-IF                                                           
046300       END-IF                                                             
046400       MOVE JA TO TRYCK-UPPDATERING-SW                                    
046500       PERFORM EA-REQU-INDATA-TILL-MOD                                    
046600     END-IF                                                               
046700     .                                                                    
046800     EJECT                                                                
046900 EA-REQU-INDATA-TILL-MOD SECTION.                                         
047000                                                                          
047100     IF REQU-FLAGGA-KR-HOPP NOT = ALL '+'                                 
047200       MOVE REQU-FLAGGA-KR-HOPP   TO RESP-FLAGGA-KR-HOPP                  
047300       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-KR-HOPP-ATTR             
047400     ELSE                                                                 
047500       MOVE ALL-SPACE             TO RESP-FLAGGA-KR-HOPP                  
047600     END-IF                                                               
047700                                                                          
047800     IF REQU-FLAGGA-PRI NOT = ALL '+'                                     
047900       MOVE REQU-FLAGGA-PRI       TO RESP-FLAGGA-PRI                      
048000       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-PRI-ATTR                 
048100     ELSE                                                                 
048200       MOVE ALL-SPACE             TO RESP-FLAGGA-PRI                      
048300     END-IF                                                               
048400                                                                          
048500     IF REQU-FLAGGA-ADM NOT = ALL '+'                                     
048600       MOVE REQU-FLAGGA-ADM       TO RESP-FLAGGA-ADM                      
048700       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-ADM-ATTR                 
048800     ELSE                                                                 
048900       MOVE ALL-SPACE             TO RESP-FLAGGA-ADM                      
049000     END-IF                                                               
049100                                                                          
049200     IF REQU-FLAGGA-SEK NOT = ALL '+'                                     
049300       MOVE REQU-FLAGGA-SEK       TO RESP-FLAGGA-SEK                      
049400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-SEK-ATTR                 
049500     ELSE                                                                 
049600       MOVE ALL-SPACE             TO RESP-FLAGGA-SEK                      
049700     END-IF                                                               
049800                                                                          
049900     IF REQU-FLAGGA-GODK NOT = ALL '+'                                    
050000       MOVE REQU-FLAGGA-GODK      TO RESP-FLAGGA-GODK                     
050100       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-GODK-ATTR                
050200     ELSE                                                                 
050300       MOVE ALL-SPACE             TO RESP-FLAGGA-GODK                     
050400     END-IF                                                               
050500                                                                          
                                                                                
           IF REQU-IDUSER-APR NOT = ALL '+'                                     
             MOVE REQU-IDUSER-APR       TO RESP-IDUSER-APR                      
             MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-APR-ATTR                 
           ELSE                                                                 
             MOVE ALL-SPACE             TO RESP-IDUSER-APR                      
           END-IF                                                               
                                                                                
050600     .                                                                    
050700     EJECT                                                                
050800 F-LAES-VISA-INFO SECTION.                                                
050900                                                                          
051000     PERFORM IMS-GU-UPFA01                                                
051100     IF SEGMENT-FINNS AND UPPF-IDDC = W-IDDC                              
051200                                                                          
051300       MOVE UPPF-IDARTNR         TO RESP-IDARTNR                          
051400                                    W-IDARTNR                             
051500       MOVE UPPF-IDLEVNR         TO RESP-IDLEVNR                          
051600                                    W-IDLEVNR                             
051700       MOVE UPPF-KVKVAPRIM       TO RESP-KVKVAPRIM                        
051800       MOVE UPPF-KVKVASEK        TO RESP-KVKVASEK                         
051900       IF TRYCK-UPPDATERING                                               
052000          PERFORM FB-FLYTTA-EV-MID                                        
052100       ELSE                                                               
052200         MOVE UPPF-IDUSER-PRI    TO RESP-IDUSER-PRI                       
052300         MOVE UPPF-IDUSER-ADM    TO RESP-IDUSER-ADM                       
052400         MOVE UPPF-IDUSER-SEK    TO RESP-IDUSER-SEK                       
052500         MOVE UPPF-BEANST        TO RESP-BEANST                           
052600       END-IF                                                             
052700                                                                          
052800       IF UPPF-KDKVASTA-PRI > ZERO OR                                     
052900          UPPF-KDKVASTA-ADM > ZERO OR                                     
053000          UPPF-KDKVASTA-SEK > ZERO                                        
053100                                                                          
053200          IF UPPF-KDKVASTA-PRI = 2                                        
053300            IF WS-ENGLISH-TEXT                                            
053400              MOVE YES           TO RESP-FLAGGA-PRI                       
053500            ELSE                                                          
053600              MOVE JA            TO RESP-FLAGGA-PRI                       
053700            END-IF                                                        
053800          END-IF                                                          
053900                                                                          
054000          IF UPPF-KDKVASTA-PRI = 3                                        
054100            MOVE NEJ             TO RESP-FLAGGA-PRI                       
054200          END-IF                                                          
054300                                                                          
054400          IF UPPF-KDKVASTA-ADM = 3                                        
054500            MOVE NEJ             TO RESP-FLAGGA-ADM                       
054600          END-IF                                                          
054700                                                                          
054800          IF UPPF-KDKVASTA-SEK = 2                                        
054900            IF WS-ENGLISH-TEXT                                            
055000              MOVE YES           TO RESP-FLAGGA-SEK                       
055100            ELSE                                                          
055200              MOVE JA            TO RESP-FLAGGA-SEK                       
055300            END-IF                                                        
055400          END-IF                                                          
055500                                                                          
055600          IF UPPF-KDKVASTA-SEK = 3                                        
055700            MOVE NEJ             TO RESP-FLAGGA-SEK                       
055800          END-IF                                                          
055900                                                                          
056000          IF UPPF-KDKVATYP > ZERO                                         
056100            MOVE UPPF-KDKVATYP   TO NUM-KDKVATYP                          
056200            MOVE KVA-KDKVATYP (SPRAK-IX NUM-KDKVATYP)                     
056300                                 TO RESP-KDKVATYP                         
056400          ELSE                                                            
056500            MOVE ALL-SPACE       TO RESP-KDKVATYP                         
056600          END-IF                                                          
056700                                                                          
056800          PERFORM IMS-GU-KVAH01                                           
056900          IF SEGMENT-FINNS                                                
057000            MOVE KVAH-ART-KDKVAULG   TO NUM-KDKVAULG                      
057100            MOVE KVAH-ART-ADKVAULG   TO RESP-ADKVAULG                     
057200          ELSE                                                            
057300            MOVE UPPF-KDKVAULG       TO NUM-KDKVAULG                      
057400            MOVE UPPF-ADKVAULG       TO RESP-ADKVAULG                     
057500          END-IF                                                          
057600          IF NUM-KDKVAULG > ZERO                                          
057700            MOVE KVA-BEKVAULG (SPRAK-IX NUM-KDKVAULG)                     
057800                                     TO RESP-UNDERLAG                     
057900          ELSE                                                            
058000            MOVE KVA-BEKVAULG (SPRAK-IX 10)                               
058100                                     TO RESP-UNDERLAG                     
058200          END-IF                                                          
058300                                                                          
058400       ELSE                                                               
058500          PERFORM IMS-GU-KVAH01                                           
058600          IF SEGMENT-FINNS                                                
058700            MOVE KVAH-ART-KDKVAULG   TO NUM-KDKVAULG                      
058800            MOVE KVAH-ART-ADKVAULG   TO RESP-ADKVAULG                     
058900            IF NUM-KDKVAULG > ZERO                                        
059000              MOVE KVA-BEKVAULG (SPRAK-IX NUM-KDKVAULG)                   
059100                                     TO RESP-UNDERLAG                     
059200            ELSE                                                          
059300              MOVE KVA-BEKVAULG (SPRAK-IX 10)                             
059400                                     TO RESP-UNDERLAG                     
059500            END-IF                                                        
059600          END-IF                                                          
059700                                                                          
059800       END-IF                                                             
059900                                                                          
060000       PERFORM FA-LAES-VISA-KONTROLL-TEXT                                 
060100                                                                          
060200       PERFORM IMS-GU-KVAH12                                              
060300       IF SEGMENT-FINNS                                                   
060400         IF KVAH-LEV-FLUPG = NEJ                                          
060500           MOVE ERR-IST-MISSING TO RESP-IDMSG-ERROR                       
060600           PERFORM S02-RENSA-JA-FLAGGOR                                   
060700         END-IF                                                           
060800       END-IF                                                             
060900                                                                          
061000       PERFORM IMS-GU-INLA11                                              
061100       IF SEGMENT-FINNS                                                   
061200          MOVE ART-KVAVIS       TO RESP-KVAVIS                            
061300       END-IF                                                             
                                                                                
             MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT-KEY                        
             IF DCS-UNICODE-IDSKYLT                                             
                MOVE 'UTF8'             TO TRAUTF8-KDCP                         
             ELSE                                                               
                MOVE '278 '             TO TRAUTF8-KDCP                         
             END-IF                                                             
061400                                                                          
061500       PERFORM IMS-GU-BENA-WLBENA11                                       
061600       IF SEGMENT-FINNS                                                   
061700          MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                       
061800       ELSE                                                               
061900          MOVE SPACE         TO TRAUTF8-TECONV-FROM                       
062000                                TEXT-BEART                                
062100          MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                              
062200       END-IF                                                             
             IF TRAUTF8-TECONV-FROM = SPACES                                    
              MOVE 'GB'  TO W-IDSKYLT-KEY                                       
              MOVE '278' TO TRAUTF8-KDCP                                        
              PERFORM IMS-GU-BENA-WLBENA11                                      
              MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                         
             END-IF                                                             
062300                                                                          
062400       IF REQU-IDMSGVER = '001'                                           
062500*      CALL FROM WEB AND NDC CHINA                                        
062600*      CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE         
062700          CALL WTRAUTF8 USING TRAUTF8-AREA                                
062800          MOVE TRAUTF8-TECONV-TO TO RESP-BEART                            
062900       ELSE                                                               
063000*      CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                       
063100          MOVE TEXT-BEART        TO RESP-BEART                            
063200       END-IF                                                             
063300                                                                          
063400       PERFORM IMS-GU-WDF502                                              
063500       IF SEGMENT-FINNS                                                   
063600          MOVE XLEV-BELEVART    TO RESP-BELEVART                          
063700       END-IF                                                             
063800                                                                          
063900     ELSE                                                                 
064000                                                                          
064100       PERFORM IMS-GU-INLA11                                              
064200       IF SEGMENT-FINNS                                                   
064300          MOVE ART-KVAVIS       TO RESP-KVAVIS                            
064400          MOVE ART-IDARTNR      TO RESP-IDARTNR                           
064500                                   W-IDARTNR                              
064600                                                                          
064700         PERFORM IMS-GU-INLC01                                            
064800         IF SEGMENT-FINNS                                                 
064900           MOVE INLC-SEQB-IDLEVNR TO W-IDLEVNR                            
065000                                     RESP-IDLEVNR                         
065100         END-IF                                                           
065200                                                                          
065300         PERFORM IMS-GU-BENA-WLBENA11                                     
065700         IF SEGMENT-FINNS                                                 
065800            MOVE TEXT-BEART     TO TRAUTF8-TECONV-FROM                    
065900         ELSE                                                             
066000            MOVE SPACE          TO TRAUTF8-TECONV-FROM                    
066100                                   TEXT-BEART                             
066200            MOVE WS-CP-EBCDIC   TO TRAUTF8-KDCP                           
066300         END-IF                                                           
066400                                                                          
066500         IF REQU-IDMSGVER = '001'                                         
066600* *      CALL FROM WEB AND NDC CHINA                                      
066700* *      CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPAC        
066800            CALL WTRAUTF8 USING TRAUTF8-AREA                              
066900            MOVE TRAUTF8-TECONV-TO TO RESP-BEART                          
067000         ELSE                                                             
067100* *      CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                     
067200            MOVE TEXT-BEART        TO RESP-BEART                          
067300         END-IF                                                           
067400                                                                          
067500         PERFORM IMS-GU-WDF502                                            
067600         IF SEGMENT-FINNS                                                 
067700            MOVE XLEV-BELEVART  TO RESP-BELEVART                          
067800         END-IF                                                           
067900                                                                          
068000         PERFORM IMS-GU-KVAH01                                            
068100         IF SEGMENT-FINNS                                                 
068200           MOVE KVAH-ART-KDKVAULG   TO NUM-KDKVAULG                       
068300           MOVE KVAH-ART-ADKVAULG   TO RESP-ADKVAULG                      
068400           IF NUM-KDKVAULG > ZERO                                         
068500             MOVE KVA-BEKVAULG (SPRAK-IX NUM-KDKVAULG)                    
068600                                    TO RESP-UNDERLAG                      
068700           ELSE                                                           
068800             MOVE KVA-BEKVAULG (SPRAK-IX 10)                              
068900                                    TO RESP-UNDERLAG                      
069000           END-IF                                                         
069100         END-IF                                                           
069200                                                                          
069300         PERFORM MFS-STAENG-ANDRA-FAELT                                   
069400                                                                          
069500       ELSE                                                               
069600         MOVE ERR-URVAL-SAKNAS TO RESP-IDMSG-ERROR                        
069700         MOVE 'KEY'            TO RESP-IDELMT-ERROR                       
069800         PERFORM MFS-RENSA-FAELT-IN                                       
069900         PERFORM MFS-RENSA-FAELT-UT                                       
070000         PERFORM MFS-STAENG-ANDRA-FAELT                                   
070100         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-PRI-ATTR              
070200         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-SEK-ATTR              
070300         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDUSER-PRI-ATTR              
070400         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDUSER-SEK-ATTR              
070500       END-IF                                                             
070600     END-IF                                                               
070700     .                                                                    
070800     EJECT                                                                
070900 FA-LAES-VISA-KONTROLL-TEXT SECTION.                                      
071000     MOVE NEJ                TO SEGMENT-SW                                
071100     IF REQU-NEXT                                                         
071200       IF REQU-IDKR-START > ZERO                                          
071300         MOVE REQU-IDKR-START TO W-IDKR                                   
071400         PERFORM IMS-GNP-UPFA11-KVAL                                      
071500       ELSE                                                               
071600         IF REQU-IDKVAINF-START > ZERO                                    
071700           MOVE REQU-IDKVAINF-START TO W-IDKVAINF                         
071800           PERFORM IMS-GNP-UPFA12-KVAL                                    
071900         ELSE                                                             
072000           PERFORM IMS-GNP-UPFA11                                         
072100           IF SEGMENT-FINNS                                               
072200             CONTINUE                                                     
072300           ELSE                                                           
072400             PERFORM IMS-GNP-UPFA12                                       
072500           END-IF                                                         
072600         END-IF                                                           
072700       END-IF                                                             
072800     ELSE                                                                 
072900       IF REQU-FIRST                                                      
073000         PERFORM IMS-GNP-UPFA11                                           
073100         IF SEGMENT-FINNS                                                 
073200           CONTINUE                                                       
073300         ELSE                                                             
073400           PERFORM IMS-GNP-UPFA12                                         
073500         END-IF                                                           
073600       ELSE                                                               
073700         IF REQU-IDKR-START > ZERO                                        
073800           MOVE REQU-IDKR-START TO W-IDKR                                 
073900           PERFORM IMS-GNP-UPFA11-KVAL                                    
074000         ELSE                                                             
074100           IF REQU-IDKVAINF-START > ZERO                                  
074200             MOVE REQU-IDKVAINF-START TO W-IDKVAINF                       
074300             PERFORM IMS-GNP-UPFA12-KVAL                                  
074400           ELSE                                                           
074500             PERFORM IMS-GNP-UPFA11                                       
074600             IF SEGMENT-FINNS                                             
074700               CONTINUE                                                   
074800             ELSE                                                         
074900               PERFORM IMS-GNP-UPFA12                                     
075000             END-IF                                                       
075100           END-IF                                                         
075200         END-IF                                                           
075300       END-IF                                                             
075400     END-IF                                                               
075500     IF SEGMENT-FINNS                                                     
075600       IF UPFA-SEG-NAME-FB = 'W6UPFA11'                                   
075700         MOVE RAPP-IDKR          TO RESP-IDKR                             
075800                                    RESP-IDKR-START                       
075900                                    RESP-IDKR-NEXT                        
076000         INSPECT RESP-IDKR REPLACING LEADING ZERO BY SPACE                
076100         MOVE ZERO               TO RESP-IDKVAINF-START                   
076200                                    RESP-IDKVAINF-NEXT                    
076300         MOVE KVA-KONTROLLTYP(SPRAK-IX 1) TO RESP-KONTROLL                
076400         IF RAPP-KDKVASTA-PRI  = 2                                        
076500           IF WS-ENGLISH-TEXT                                             
076600             MOVE YES              TO RESP-FLAGGA-GODK                    
076700           ELSE                                                           
076800             MOVE JA               TO RESP-FLAGGA-GODK                    
076900           END-IF                                                         
076910         END-IF                                                           
077000                                                                          
077100         IF RAPP-KDKVASTA-PRI  = 3                                        
077200           MOVE NEJ              TO RESP-FLAGGA-GODK                      
077300         END-IF                                                           
077400         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-BEANST-ATTR                  
077500         PERFORM UNTIL SEGMENT-SAKNAS OR NEXT-SEGMENT                     
077600           MOVE RAPP-BEKRFEL(1)  TO RESP-TEKRFEL (1)                      
077700           MOVE RAPP-BEKRFEL(2)  TO RESP-TEKRFEL (2)                      
077800           MOVE RAPP-BEKRFEL(3)  TO RESP-TEKRFEL (3)                      
077900           IF WS-ENGLISH-TEXT                                             
078000             MOVE 'OK IR    '    TO RESP-FLAGGA-TEXT                      
078100           ELSE                                                           
078200             MOVE 'Godk KR  '    TO RESP-FLAGGA-TEXT                      
078300           END-IF                                                         
                                                                                
                 IF WS-ENGLISH-TEXT                                             
                   MOVE 'Empl ID.'     TO RESP-EMPLID-TEXT                      
                 ELSE                                                           
                   MOVE 'Anstnr  '     TO RESP-EMPLID-TEXT                      
                 END-IF                                                         
                                                                                
                 MOVE RAPP-IDUSER      TO RESP-IDUSER-APR                       
                                                                                
078400           PERFORM IMS-GNP-UPFA11                                         
078500           IF SEGMENT-FINNS                                               
078600             MOVE JA TO SEGMENT-SW                                        
078700           ELSE                                                           
078800             PERFORM IMS-GNP-UPFA12                                       
078900             IF SEGMENT-FINNS                                             
079000               MOVE JA TO SEGMENT-SW                                      
079100             END-IF                                                       
079200           END-IF                                                         
079300         END-PERFORM                                                      
079400         IF SEGMENT-FINNS                                                 
079500           IF UPFA-SEG-NAME-FB = 'W6UPFA11'                               
079600             MOVE RAPP-IDKR      TO RESP-IDKR-NEXT                        
079700           ELSE                                                           
079800             MOVE SPEC-IDKVAINF  TO RESP-IDKVAINF-NEXT                    
079900             MOVE ZERO           TO RESP-IDKR-NEXT                        
080000           END-IF                                                         
080100           IF REQU-UPDATE OR REQU-UPD-V                                   
080200             CONTINUE                                                     
080300           ELSE                                                           
080400             MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                 
080500           END-IF                                                         
080600         END-IF                                                           
080700       ELSE                                                               
080800         IF UPFA-SEG-NAME-FB = 'W6UPFA12'                                 
080900           MOVE SPEC-IDKVAINF      TO RESP-IDKVAINF-START                 
081000                                      RESP-IDKVAINF-NEXT                  
081100                                      W-IDKVAINF                          
081200           MOVE ZERO               TO RESP-IDKR-START                     
081300                                      RESP-IDKR-NEXT                      
081400           MOVE MFS-STAENG-FAELT-NOMOD TO RESP-BEANST-ATTR                
081500           MOVE KVA-KONTROLLTYP(SPRAK-IX 2) TO RESP-KONTROLL              
081600           IF SPEC-KDKVASTA-PRI = 2                                       
081700             IF WS-ENGLISH-TEXT                                           
081800               MOVE YES            TO RESP-FLAGGA-GODK                    
081900             ELSE                                                         
082000               MOVE JA             TO RESP-FLAGGA-GODK                    
082100             END-IF                                                       
082200           END-IF                                                         
082300           IF SPEC-KDKVASTA-PRI = 3                                       
082400             MOVE NEJ            TO RESP-FLAGGA-GODK                      
082500           END-IF                                                         
082600           PERFORM UNTIL SEGMENT-SAKNAS OR NEXT-SEGMENT                   
082700             MOVE SPEC-TEKVAINF (1) TO RESP-TEKRFEL (1)                   
082800             MOVE SPEC-TEKVAINF (2) TO RESP-TEKRFEL (2)                   
082900             MOVE SPEC-TEKVAINF (3) TO RESP-TEKRFEL (3)                   
083000             IF WS-ENGLISH-TEXT                                           
083100               MOVE '**FOR MORE INFO SEE 6214**' TO                       
083200                                                  RESP-TEKRFEL(4)         
083300               MOVE 'OK SPEC  '                TO RESP-FLAGGA-TEXT        
083400             ELSE                                                         
083500               MOVE '**FÖR MER INFO SE 6214**' TO RESP-TEKRFEL(4)         
083600               MOVE 'GODK SPEC'                TO RESP-FLAGGA-TEXT        
083700             END-IF                                                       
                                                                                
                   IF WS-ENGLISH-TEXT                                           
                     MOVE 'Empl ID.'     TO RESP-EMPLID-TEXT                    
                   ELSE                                                         
                     MOVE 'Anstnr  '     TO RESP-EMPLID-TEXT                    
                   END-IF                                                       
                                                                                
                   MOVE SPEC-IDUSER      TO RESP-IDUSER-APR                     
                                                                                
083800             PERFORM IMS-GNP-UPFA12                                       
083900             IF SEGMENT-FINNS                                             
084000               MOVE JA TO SEGMENT-SW                                      
084100             END-IF                                                       
084200           END-PERFORM                                                    
084300           IF SEGMENT-FINNS                                               
084400             MOVE SPEC-IDKVAINF TO RESP-IDKVAINF-NEXT                     
084500             IF REQU-UPDATE OR REQU-UPD-V                                 
084600               CONTINUE                                                   
084700             ELSE                                                         
084800               MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO               
084900             END-IF                                                       
085000           END-IF                                                         
085100           PERFORM IMS-GU-KVAH22                                          
085200           IF SEGMENT-FINNS                                               
085300             MOVE KVAH-SPEC-BEANST TO RESP-SPEC-BEANST                    
085400             MOVE KVAH-SPEC-IDTFN  TO RESP-IDTFN                          
085500           END-IF                                                         
085600         END-IF                                                           
085700       END-IF                                                             
085800     ELSE                                                                 
085900       MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-GODK-ATTR               
085900       MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDUSER-APR-ATTR                
086000       MOVE ALL-SPACE            TO RESP-FLAGGA-TEXT                      
086100                                    RESP-FLAGGA-GODK                      
086200                                    RESP-TEKRFEL (1)                      
086300                                    RESP-TEKRFEL (2)                      
086400                                    RESP-TEKRFEL (3)                      
086500                                    RESP-TEKRFEL (4)                      
                                          RESP-EMPLID-TEXT                      
                                          RESP-IDUSER-APR                       
                                                                                
086600       MOVE ZERO                 TO RESP-IDKR-START                       
086700                                    RESP-IDKR-NEXT                        
086800                                    RESP-IDKVAINF-START                   
086900                                    RESP-IDKVAINF-NEXT                    
087000     END-IF                                                               
087100     .                                                                    
087200     EJECT                                                                
087300 FB-FLYTTA-EV-MID SECTION.                                                
087400                                                                          
087500     IF REQU-IDUSER-PRI = ALL '+'                                         
087600        MOVE UPPF-IDUSER-PRI      TO RESP-IDUSER-PRI                      
087700     ELSE                                                                 
087800       MOVE REQU-IDUSER-PRI       TO RESP-IDUSER-PRI                      
087900       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-PRI-ATTR                 
088000     END-IF                                                               
088100     IF REQU-IDUSER-ADM = ALL '+'                                         
088200        MOVE UPPF-IDUSER-ADM      TO RESP-IDUSER-ADM                      
088300     ELSE                                                                 
088400       MOVE REQU-IDUSER-ADM       TO RESP-IDUSER-ADM                      
088500       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-ADM-ATTR                 
088600     END-IF                                                               
088700     IF REQU-IDUSER-SEK = ALL '+'                                         
088800        MOVE UPPF-IDUSER-SEK      TO RESP-IDUSER-SEK                      
088900     ELSE                                                                 
089000       MOVE REQU-IDUSER-SEK       TO RESP-IDUSER-SEK                      
089100       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-SEK-ATTR                 
089200     END-IF                                                               
089300     IF REQU-BEANST    = ALL '+'                                          
089400        MOVE UPPF-BEANST          TO RESP-BEANST                          
089500     ELSE                                                                 
089600       MOVE REQU-BEANST           TO RESP-BEANST                          
089700       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-BEANST-ATTR                     
089800     END-IF                                                               
089900     .                                                                    
090000     EJECT                                                                
090100 G-KOLLA-INPUT SECTION.                                                   
090200                                                                          
090300     MOVE JA TO INDATA-SW                                                 
090400     IF REQU-INPUT = ALL '+'                                              
090500       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
090600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
090700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
090800       IF REQU-IDKR-START = ZERO AND REQU-IDKVAINF-START = ZERO           
090900          MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-GODK-ATTR            
090900          MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDUSER-APR-ATTR             
091000       ELSE                                                               
091100          MOVE MFS-STAENG-FAELT-NOMOD TO RESP-BEANST-ATTR                 
091200       END-IF                                                             
091300       IF SEGMENT-SAKNAS                                                  
091400         PERFORM MFS-STAENG-ANDRA-FAELT                                   
091500       END-IF                                                             
091600       MOVE NEJ TO INDATA-SW                                              
091700     ELSE                                                                 
091800                                                                          
091810       IF REQU-UPDATE AND                                                 
091900          ((REQU-BEANST NOT = ALL '+' AND REQU-BEANST NOT = SPACE)        
092010           OR                                                             
                 (REQU-IDMSGVER = 101                                           
                     AND                                                        
092020               ((REQU-FLAGGA-GODK NOT = ALL '+' AND                       
092030                 REQU-FLAGGA-GODK NOT = SPACE)                            
                       OR                                                       
092020                (REQU-IDUSER-APR  NOT = ALL '+' AND                       
092030                 REQU-IDUSER-APR  NOT = SPACE)                            
                     )                                                          
                 )                                                              
                )                                                               
092100         MOVE INF-PRESS-PF23 TO RESP-IDMSG-INFO                           
092200         MOVE 'PF23'         TO RESP-IDELMT-ERROR                         
092300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
092400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
092500         PERFORM MFS-LAES-IN-IGEN                                         
092600         IF REQU-IDKR-START = ZERO AND REQU-IDKVAINF-START = ZERO         
092700            MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-GODK-ATTR          
092700            MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDUSER-APR-ATTR           
092800         END-IF                                                           
092900         IF SEGMENT-SAKNAS                                                
093000           PERFORM MFS-STAENG-ANDRA-FAELT                                 
093100         END-IF                                                           
093200         MOVE NEJ TO INDATA-SW                                            
093300       ELSE                                                               
093400         IF (REQU-FLAGGA-PRI = NEJ OR REQU-FLAGGA-SEK = NEJ OR            
093500             REQU-FLAGGA-ADM = NEJ) AND REQU-UPDATE                       
093600           PERFORM IMS-GU-INLA11                                          
093700           IF SEGMENT-FINNS                                               
093800             IF ART-FLSPLPART = JA                                        
093900               MOVE SPLITT-PRESS-PF23 TO RESP-IDMSG-INFO                  
094000               PERFORM MFS-ROER-EJ-FAELT-IN                               
094100               PERFORM MFS-ROER-EJ-FAELT-UT                               
094200               PERFORM MFS-LAES-IN-IGEN                                   
094300               MOVE NEJ TO INDATA-SW                                      
094400             END-IF                                                       
094500           END-IF                                                         
094600         END-IF                                                           
094700       END-IF                                                             
094800     END-IF                                                               
094900                                                                          
095000     IF INDATA-OK                                                         
095100        PERFORM IMS-GU-UPFA01                                             
095200        IF SEGMENT-FINNS                                                  
095300          MOVE UPPF-IDARTNR     TO TEST-IDARTNR                           
095400                                   W-IDARTNR                              
095500          MOVE UPPF-IDLEVNR     TO W-IDLEVNR                              
095600          PERFORM IMS-GU-INLC01                                           
095700          IF SEGMENT-FINNS                                                
095800            MOVE INLC-SEQB-IDLOPNRM TO W-IDLOPNRM-CSEQ                    
095900            MOVE INLC-SEQB-TIAVIDAT TO W-DAAVSDAT-CSEQ                    
096000            IF INLC-SEQB-TIAVIDAT NOT = ZERO                              
096100              IF INLC-SEQB-TIAVIDAT < 500000                              
096200                MOVE 20             TO W-DAAVSDAT-CSEQ (1:2)              
096300              ELSE                                                        
096400                IF INLC-SEQB-TIAVIDAT < 999999                            
096500                  MOVE 19           TO W-DAAVSDAT-CSEQ (1:2)              
096600                ELSE                                                      
096700                  MOVE 99999999     TO W-DAAVSDAT-CSEQ                    
096800                END-IF                                                    
096900              END-IF                                                      
097000            END-IF                                                        
097100          END-IF                                                          
097200          PERFORM S01-KOLLA-UPPDAT-TILLATEN                               
097300          IF INDATA-OK                                                    
097400                                                                          
097500            IF REQU-BEANST NOT = ALL '+'                                  
097600              PERFORM IMS-GNP-UPFA11                                      
097700              IF SEGMENT-FINNS                                            
097800                MOVE MFS-ALFA-FAELT-FEL TO RESP-BEANST-ATTR               
097900                MOVE NEJ TO INDATA-SW                                     
098000              ELSE                                                        
098100                PERFORM IMS-GNP-UPFA12                                    
098200                IF SEGMENT-FINNS                                          
098300                  MOVE MFS-ALFA-FAELT-FEL TO RESP-BEANST-ATTR             
098400                  MOVE NEJ TO INDATA-SW                                   
098500                ELSE                                                      
098600                  MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEANST-ATTR           
098700                END-IF                                                    
098800              END-IF                                                      
098900              IF INDATA-OK                                                
099000                IF WS-FLUPG = NEJ                                         
099100                  MOVE NEJ TO INDATA-SW                                   
099200                  MOVE MFS-ALFA-FAELT-FEL TO RESP-BEANST-ATTR             
099300                ELSE                                                      
099400                  MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEANST-ATTR           
099500                END-IF                                                    
099600              END-IF                                                      
099700            END-IF                                                        
099800                                                                          
099900            PERFORM GB-KOLLA-PRI-ADM-SEK                                  
100000                                                                          
100100            IF WS-FLUPG = NEJ                                             
100200              PERFORM GA-KONTROLL-UTFALLSPROV                             
100300            END-IF                                                        
100400                                                                          
100500            IF REQU-FLAGGA-GODK NOT = ALL '+'                             
100600              IF REQU-FLAGGA-GODK = JA OR NEJ OR YES                      
100700                IF REQU-IDUSER-PRI NOT = ALL '+' OR                       
100800                   UPPF-IDUSER-PRI NOT = SPACE                            
100900                  MOVE MFS-ALFA-FAELT-RAETT TO                            
101000                                        RESP-FLAGGA-GODK-ATTR             
101100                ELSE                                                      
101200                   MOVE MFS-ALFA-FAELT-FEL TO                             
101300                                        RESP-FLAGGA-GODK-ATTR             
101400                   MOVE MFS-ALFA-FAELT-FEL TO                             
101500                                      RESP-IDUSER-PRI-ATTR                
101600                   MOVE NEJ TO INDATA-SW                                  
101700                END-IF                                                    
                                                                                
                      IF REQU-IDUSER-APR NOT = ALL '+' AND                      
                         REQU-IDUSER-APR NOT = SPACE                            
                                                                                
                         MOVE MFS-ALFA-FAELT-RAETT TO                           
                                              RESP-IDUSER-APR-ATTR              
                      ELSE                                                      
                         MOVE MFS-ALFA-FAELT-FEL TO                             
                                              RESP-IDUSER-APR-ATTR              
                         MOVE NEJ TO INDATA-SW                                  
                      END-IF                                                    
                                                                                
101800              ELSE                                                        
101900                MOVE MFS-ALFA-FAELT-FEL TO                                
102000                                        RESP-FLAGGA-GODK-ATTR             
102100                MOVE NEJ TO INDATA-SW                                     
102200              END-IF                                                      
102300            END-IF                                                        
                                                                                
                  IF REQU-IDUSER-APR NOT = ALL '+'                              
                     IF REQU-IDUSER-APR NOT = SPACE                             
                                                                                
                        MOVE MFS-ALFA-FAELT-RAETT TO                            
                                              RESP-IDUSER-APR-ATTR              
                     ELSE                                                       
                        MOVE MFS-ALFA-FAELT-FEL TO                              
                                          RESP-IDUSER-APR-ATTR                  
                        MOVE NEJ TO INDATA-SW                                   
                     END-IF                                                     
                  END-IF                                                        
                                                                                
102400                                                                          
102500            IF INDATA-FEL                                                 
102600              MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR               
102700              PERFORM MFS-ROER-EJ-FAELT-IN                                
102800              PERFORM MFS-ROER-EJ-FAELT-UT                                
102900              IF REQU-IDKR-START = ZERO AND                               
103000                                  REQU-IDKVAINF-START = ZERO              
103100                MOVE MFS-STAENG-FAELT-NOMOD TO                            
103200                                           RESP-FLAGGA-GODK-ATTR          
103100                MOVE MFS-STAENG-FAELT-NOMOD TO                            
103200                                           RESP-IDUSER-APR-ATTR           
103300              ELSE                                                        
103400                MOVE MFS-STAENG-FAELT-NOMOD TO                            
103500                                           RESP-BEANST-ATTR               
103600              END-IF                                                      
103700                                                                          
103800              IF KVAH-LEV-FLUPG = NEJ                                     
103900                MOVE ERR-IST-MISSING TO RESP-IDMSG-ERROR                  
104000                PERFORM S02-RENSA-JA-FLAGGOR                              
104100              END-IF                                                      
104200            END-IF                                                        
104300                                                                          
104400          END-IF                                                          
104500        ELSE                                                              
104600***OM EJ FINNS PÅ W6L1                                                    
104700          PERFORM IMS-GU-INLA11                                           
104800          IF SEGMENT-FINNS                                                
104900            MOVE ART-IDARTNR TO W-IDARTNR                                 
105000          END-IF                                                          
105100          IF REQU-IDUSER-PRI NOT = ALL '+'                                
105200                               OR REQU-FLAGGA-PRI NOT = ALL '+'           
105300             IF REQU-IDUSER-PRI = ALL '+'                                 
105400               MOVE MFS-ALFA-FAELT-FEL TO                                 
105500                                       RESP-IDUSER-PRI-ATTR               
105600               MOVE NEJ TO INDATA-SW                                      
105700             ELSE                                                         
105800               MOVE MFS-ALFA-FAELT-RAETT TO                               
105900                                       RESP-IDUSER-PRI-ATTR               
106000             END-IF                                                       
106100             IF REQU-FLAGGA-PRI = ALL '+'                                 
106200                MOVE MFS-ALFA-FAELT-FEL TO                                
106300                                     RESP-FLAGGA-PRI-ATTR                 
106400               MOVE NEJ TO INDATA-SW                                      
106500             ELSE                                                         
106600               MOVE MFS-ALFA-FAELT-RAETT TO                               
106700                                          RESP-FLAGGA-PRI-ATTR            
106800             END-IF                                                       
106900          END-IF                                                          
107000          IF REQU-IDUSER-ADM NOT = ALL '+'                                
107100                               OR REQU-FLAGGA-ADM NOT = ALL '+'           
107200             IF REQU-IDUSER-ADM = ALL '+'                                 
107300               MOVE MFS-ALFA-FAELT-FEL TO                                 
107400                                       RESP-IDUSER-ADM-ATTR               
107500               MOVE NEJ TO INDATA-SW                                      
107600             ELSE                                                         
107700               MOVE MFS-ALFA-FAELT-RAETT TO                               
107800                                          RESP-IDUSER-ADM-ATTR            
107900             END-IF                                                       
108000             IF REQU-FLAGGA-ADM = ALL '+'                                 
108100                MOVE MFS-ALFA-FAELT-FEL TO                                
108200                                        RESP-FLAGGA-ADM-ATTR              
108300               MOVE NEJ TO INDATA-SW                                      
108400             ELSE                                                         
108500               MOVE MFS-ALFA-FAELT-RAETT TO                               
108600                                          RESP-FLAGGA-ADM-ATTR            
108700             END-IF                                                       
108800          END-IF                                                          
108900          IF REQU-IDUSER-SEK NOT = ALL '+'                                
109000                               OR REQU-FLAGGA-SEK NOT = ALL '+'           
109100             IF REQU-IDUSER-SEK = ALL '+'                                 
109200               MOVE MFS-ALFA-FAELT-FEL TO                                 
109300                                         RESP-IDUSER-SEK-ATTR             
109400               MOVE NEJ TO INDATA-SW                                      
109500             ELSE                                                         
109600               MOVE MFS-ALFA-FAELT-RAETT TO                               
109700                                          RESP-IDUSER-SEK-ATTR            
109800             END-IF                                                       
109900             IF REQU-FLAGGA-SEK = ALL '+'                                 
110000                MOVE MFS-ALFA-FAELT-FEL TO                                
110100                                        RESP-FLAGGA-SEK-ATTR              
110200               MOVE NEJ TO INDATA-SW                                      
110300             ELSE                                                         
110400               MOVE MFS-ALFA-FAELT-RAETT TO                               
110500                                          RESP-FLAGGA-SEK-ATTR            
110600             END-IF                                                       
110700          END-IF                                                          
110800          IF INDATA-OK                                                    
110900            PERFORM GC-KOLLA-PRI-ADM-SEK                                  
111000          END-IF                                                          
111100                                                                          
111200          IF INDATA-FEL                                                   
111300            MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                 
111400            PERFORM MFS-ROER-EJ-FAELT-IN                                  
111500            PERFORM MFS-ROER-EJ-FAELT-UT                                  
111600            PERFORM MFS-STAENG-ANDRA-FAELT                                
111700          ELSE                                                            
111800            MOVE JA TO NYUPPLAEGG-SW                                      
111900          END-IF                                                          
112000        END-IF                                                            
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400 GA-KONTROLL-UTFALLSPROV SECTION.                                         
112500     IF UPPF-KDKVASTA-PRI = '3' OR                                        
112600        UPPF-KDKVASTA-SEK = '3' OR                                        
112700        REQU-FLAGGA-PRI = NEJ OR                                          
112800        REQU-FLAGGA-SEK = NEJ                                             
112900        CONTINUE                                                          
113000     ELSE                                                                 
113100       MOVE MFS-ALFA-FAELT-FEL TO                                         
113200                               RESP-FLAGGA-PRI-ATTR                       
113300       MOVE MFS-ALFA-FAELT-FEL TO                                         
113400                               RESP-FLAGGA-SEK-ATTR                       
113500       MOVE NEJ TO INDATA-SW                                              
113600     END-IF                                                               
113700     .                                                                    
113800     EJECT                                                                
113900 GB-KOLLA-PRI-ADM-SEK SECTION.                                            
114000                                                                          
114100     IF REQU-IDUSER-PRI NOT = ALL '+'                                     
114200       IF REQU-IDUSER-PRI NOT = SPACE                                     
114300         MOVE MFS-ALFA-FAELT-RAETT TO                                     
114400                                  RESP-IDUSER-PRI-ATTR                    
114500       ELSE                                                               
114600        MOVE MFS-ALFA-FAELT-FEL TO                                        
114700                                  RESP-IDUSER-PRI-ATTR                    
114800        MOVE NEJ TO INDATA-SW                                             
114900       END-IF                                                             
115000     END-IF                                                               
115100                                                                          
115200     IF REQU-IDUSER-ADM NOT = ALL '+'                                     
115300       IF REQU-IDUSER-ADM NOT = SPACE                                     
115400         MOVE MFS-ALFA-FAELT-RAETT TO                                     
115500                                  RESP-IDUSER-ADM-ATTR                    
115600       ELSE                                                               
115700        MOVE MFS-ALFA-FAELT-FEL TO                                        
115800                                  RESP-IDUSER-ADM-ATTR                    
115900        MOVE NEJ TO INDATA-SW                                             
116000       END-IF                                                             
116100     END-IF                                                               
116200                                                                          
116300     IF REQU-IDUSER-SEK NOT = ALL '+'                                     
116400       IF REQU-IDUSER-SEK NOT = SPACE                                     
116500         MOVE MFS-ALFA-FAELT-RAETT TO                                     
116600                                  RESP-IDUSER-SEK-ATTR                    
116700       ELSE                                                               
116800         MOVE MFS-ALFA-FAELT-FEL TO                                       
116900                                  RESP-IDUSER-SEK-ATTR                    
117000         MOVE NEJ TO INDATA-SW                                            
117100       END-IF                                                             
117200     END-IF                                                               
117300                                                                          
117400     IF REQU-FLAGGA-PRI NOT = ALL '+'                                     
117500       IF REQU-FLAGGA-PRI = JA OR NEJ OR YES                              
117600         IF REQU-IDUSER-PRI NOT = ALL '+' OR                              
117700            UPPF-IDUSER-PRI NOT = SPACE                                   
117800           MOVE MFS-ALFA-FAELT-RAETT TO                                   
117900                                 RESP-FLAGGA-PRI-ATTR                     
118000         ELSE                                                             
118100           MOVE MFS-ALFA-FAELT-FEL TO                                     
118200                                RESP-FLAGGA-PRI-ATTR                      
118300           MOVE MFS-ALFA-FAELT-FEL TO                                     
118400                               RESP-IDUSER-PRI-ATTR                       
118500           MOVE NEJ TO INDATA-SW                                          
118600         END-IF                                                           
118700       ELSE                                                               
118800         MOVE MFS-ALFA-FAELT-FEL TO                                       
118900                                 RESP-FLAGGA-PRI-ATTR                     
119000         MOVE NEJ TO INDATA-SW                                            
119100       END-IF                                                             
119200     END-IF                                                               
119300                                                                          
119400     IF REQU-FLAGGA-ADM NOT = ALL '+'                                     
119500       IF REQU-FLAGGA-ADM = JA OR NEJ OR YES                              
119600         IF REQU-IDUSER-ADM NOT = ALL '+' OR                              
119700            UPPF-IDUSER-ADM NOT = SPACE                                   
119800           MOVE MFS-ALFA-FAELT-RAETT TO                                   
119900                                 RESP-FLAGGA-ADM-ATTR                     
120000         ELSE                                                             
120100           MOVE MFS-ALFA-FAELT-FEL TO                                     
120200                                RESP-FLAGGA-ADM-ATTR                      
120300           MOVE MFS-ALFA-FAELT-FEL TO                                     
120400                               RESP-IDUSER-ADM-ATTR                       
120500           MOVE NEJ TO INDATA-SW                                          
120600         END-IF                                                           
120700       ELSE                                                               
120800         MOVE MFS-ALFA-FAELT-FEL TO                                       
120900                                 RESP-FLAGGA-ADM-ATTR                     
121000         MOVE NEJ TO INDATA-SW                                            
121100       END-IF                                                             
121200     END-IF                                                               
121300                                                                          
121400     IF REQU-FLAGGA-SEK NOT = ALL '+'                                     
121500       IF REQU-FLAGGA-SEK = JA OR NEJ OR YES                              
121600         IF REQU-IDUSER-SEK NOT = ALL '+' OR                              
121700            UPPF-IDUSER-SEK NOT = SPACE                                   
121800           MOVE MFS-ALFA-FAELT-RAETT TO                                   
121900                                 RESP-FLAGGA-SEK-ATTR                     
122000         ELSE                                                             
122100           MOVE MFS-ALFA-FAELT-FEL TO                                     
122200                                   RESP-FLAGGA-SEK-ATTR                   
122300           MOVE MFS-ALFA-FAELT-FEL TO                                     
122400                               RESP-IDUSER-SEK-ATTR                       
122500           MOVE NEJ TO INDATA-SW                                          
122600         END-IF                                                           
122700       ELSE                                                               
122800         MOVE MFS-ALFA-FAELT-FEL TO                                       
122900                                 RESP-FLAGGA-SEK-ATTR                     
123000         MOVE NEJ TO INDATA-SW                                            
123100       END-IF                                                             
123200     END-IF                                                               
123300     .                                                                    
123400     EJECT                                                                
123500 GC-KOLLA-PRI-ADM-SEK SECTION.                                            
123600                                                                          
123700     IF REQU-IDUSER-PRI NOT = ALL '+'                                     
123800       IF REQU-IDUSER-PRI NOT = SPACE                                     
123900         MOVE MFS-ALFA-FAELT-RAETT TO                                     
124000                                  RESP-IDUSER-PRI-ATTR                    
124100       ELSE                                                               
124200        MOVE MFS-ALFA-FAELT-FEL TO                                        
124300                                  RESP-IDUSER-PRI-ATTR                    
124400        MOVE NEJ TO INDATA-SW                                             
124500       END-IF                                                             
124600     END-IF                                                               
124700     IF REQU-FLAGGA-PRI NOT = ALL '+'                                     
124800       IF REQU-FLAGGA-PRI = NEJ                                           
124900         IF REQU-IDUSER-PRI NOT = ALL '+'                                 
125000           MOVE MFS-ALFA-FAELT-RAETT TO                                   
125100                                 RESP-FLAGGA-PRI-ATTR                     
125200         ELSE                                                             
125300           MOVE MFS-ALFA-FAELT-FEL TO                                     
125400                                RESP-FLAGGA-PRI-ATTR                      
125500           MOVE MFS-ALFA-FAELT-FEL TO                                     
125600                               RESP-IDUSER-PRI-ATTR                       
125700           MOVE NEJ TO INDATA-SW                                          
125800         END-IF                                                           
125900       ELSE                                                               
126000         MOVE MFS-ALFA-FAELT-FEL TO                                       
126100                                 RESP-FLAGGA-PRI-ATTR                     
126200         MOVE NEJ TO INDATA-SW                                            
126300       END-IF                                                             
126400     END-IF                                                               
126500     IF REQU-IDUSER-ADM NOT = ALL '+'                                     
126600       IF REQU-IDUSER-ADM NOT = SPACE                                     
126700         MOVE MFS-ALFA-FAELT-RAETT TO                                     
126800                                  RESP-IDUSER-ADM-ATTR                    
126900       ELSE                                                               
127000        MOVE MFS-ALFA-FAELT-FEL TO                                        
127100                                  RESP-IDUSER-ADM-ATTR                    
127200        MOVE NEJ TO INDATA-SW                                             
127300       END-IF                                                             
127400     END-IF                                                               
127500     IF REQU-FLAGGA-ADM NOT = ALL '+'                                     
127600       IF REQU-FLAGGA-ADM = NEJ                                           
127700         IF REQU-IDUSER-ADM NOT = ALL '+'                                 
127800           MOVE MFS-ALFA-FAELT-RAETT TO                                   
127900                                 RESP-FLAGGA-ADM-ATTR                     
128000         ELSE                                                             
128100           MOVE MFS-ALFA-FAELT-FEL TO                                     
128200                                RESP-FLAGGA-ADM-ATTR                      
128300           MOVE MFS-ALFA-FAELT-FEL TO                                     
128400                               RESP-IDUSER-ADM-ATTR                       
128500           MOVE NEJ TO INDATA-SW                                          
128600         END-IF                                                           
128700       ELSE                                                               
128800         MOVE MFS-ALFA-FAELT-FEL TO                                       
128900                                 RESP-FLAGGA-ADM-ATTR                     
129000         MOVE NEJ TO INDATA-SW                                            
129100       END-IF                                                             
129200     END-IF                                                               
129300     IF REQU-IDUSER-SEK NOT = ALL '+'                                     
129400       IF REQU-IDUSER-SEK NOT = SPACE                                     
129500         MOVE MFS-ALFA-FAELT-RAETT TO                                     
129600                                  RESP-IDUSER-SEK-ATTR                    
129700       ELSE                                                               
129800        MOVE MFS-ALFA-FAELT-FEL TO                                        
129900                                  RESP-IDUSER-SEK-ATTR                    
130000        MOVE NEJ TO INDATA-SW                                             
130100       END-IF                                                             
130200     END-IF                                                               
130300     IF REQU-FLAGGA-SEK NOT = ALL '+'                                     
130400       IF REQU-FLAGGA-SEK = NEJ                                           
130500         IF REQU-IDUSER-SEK NOT = ALL '+'                                 
130600           MOVE MFS-ALFA-FAELT-RAETT TO                                   
130700                                 RESP-FLAGGA-SEK-ATTR                     
130800         ELSE                                                             
130900           MOVE MFS-ALFA-FAELT-FEL TO                                     
131000                                RESP-FLAGGA-SEK-ATTR                      
131100           MOVE MFS-ALFA-FAELT-FEL TO                                     
131200                               RESP-IDUSER-SEK-ATTR                       
131300           MOVE NEJ TO INDATA-SW                                          
131400         END-IF                                                           
131500       ELSE                                                               
131600         MOVE MFS-ALFA-FAELT-FEL TO                                       
131700                                 RESP-FLAGGA-SEK-ATTR                     
131800         MOVE NEJ TO INDATA-SW                                            
131900       END-IF                                                             
132000     END-IF                                                               
132100     .                                                                    
132200     EJECT                                                                
132300 H-UPPDATERA SECTION.                                                     
132400                                                                          
132500     IF NYUPPLAEGG                                                        
132600       PERFORM HA-NYUPPLAEGG-UPFA01                                       
132700       IF INDATA-OK                                                       
132800         PERFORM HB-UPPDATERA-KR-EV                                       
132900       END-IF                                                             
133000     ELSE                                                                 
133100       IF REQU-FLAGGA-PRI NOT = ALL '+' OR                                
133200          REQU-FLAGGA-ADM NOT = ALL '+' OR                                
133300          REQU-FLAGGA-SEK NOT = ALL '+' OR                                
133400          REQU-FLAGGA-GODK NOT = ALL '+'                                  
133500          IF REQU-FLAGGA-PRI = NEJ OR                                     
133600             REQU-FLAGGA-ADM = NEJ OR                                     
133700             REQU-FLAGGA-SEK = NEJ OR                                     
133800             REQU-FLAGGA-GODK = NEJ                                       
133900             MOVE JA TO ETT-FLAGGA-NEJ-SW                                 
134000          END-IF                                                          
134100          PERFORM HB-UPPDATERA-KR-EV                                      
134200       END-IF                                                             
134300                                                                          
134400       IF REQU-FLAGGA-GODK NOT = ALL '+' OR                               
                REQU-IDUSER-APR  NOT = ALL '+'                                  
134500         IF REQU-IDKR-START > ZERO                                        
134600           PERFORM HC-UPPDATERA-KONTROLLRAPPORT                           
134700         ELSE                                                             
134800           IF REQU-IDKVAINF-START > ZERO                                  
134900             PERFORM HD-UPPDATERA-SPECIALRAPPORT                          
135000           ELSE                                                           
135100             CONTINUE                                                     
135200           END-IF                                                         
135300         END-IF                                                           
135400       END-IF                                                             
135500                                                                          
135600       IF REQU-IDUSER-PRI NOT = ALL '+' OR                                
135700          REQU-IDUSER-ADM NOT = ALL '+' OR                                
135800          REQU-IDUSER-SEK NOT = ALL '+' OR                                
135900          REQU-BEANST    NOT = ALL '+' OR                                 
136000          REQU-FLAGGA-PRI NOT = ALL '+' OR                                
136100          REQU-FLAGGA-ADM NOT = ALL '+' OR                                
136200          REQU-FLAGGA-SEK NOT = ALL '+'                                   
136300                                                                          
136400         PERFORM IMS-GHU-UPFA01                                           
136500                                                                          
136600         IF REQU-IDUSER-PRI NOT = ALL '+'                                 
136700           MOVE REQU-IDUSER-PRI TO UPPF-IDUSER-PRI                        
136800           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
136900                              RESP-IDUSER-PRI-ATTR                        
137000         END-IF                                                           
137100                                                                          
137200         IF REQU-IDUSER-ADM NOT = ALL '+'                                 
137300           IF REQU-FLAGGA-ADM = NEJ                                       
137400             MOVE REQU-IDUSER-ADM TO UPPF-IDUSER-ADM                      
137500           ELSE                                                           
137600             MOVE SPACE TO UPPF-IDUSER-ADM                                
137700           END-IF                                                         
137800           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
137900                              RESP-IDUSER-ADM-ATTR                        
138000         END-IF                                                           
138100                                                                          
138200         IF REQU-IDUSER-SEK NOT = ALL '+'                                 
138300           MOVE REQU-IDUSER-SEK TO UPPF-IDUSER-SEK                        
138400           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
138500                              RESP-IDUSER-SEK-ATTR                        
138600         END-IF                                                           
138700                                                                          
138800         IF REQU-BEANST NOT = ALL '+'                                     
138900           MOVE REQU-BEANST   TO UPPF-BEANST                              
139000           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
139100                              RESP-BEANST-ATTR                            
139200           MOVE NEJ           TO UPPF-FLKVAUTV-KVAL                       
139300         END-IF                                                           
139400                                                                          
139500         IF REQU-FLAGGA-PRI NOT = ALL '+'                                 
139600           IF REQU-FLAGGA-PRI = JA OR YES                                 
139700             MOVE 2           TO UPPF-KDKVASTA-PRI                        
139800           ELSE                                                           
139900             MOVE 3           TO UPPF-KDKVASTA-PRI                        
140000           END-IF                                                         
140100           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
140200                              RESP-FLAGGA-PRI-ATTR                        
140300         END-IF                                                           
140400                                                                          
140500         IF REQU-FLAGGA-ADM NOT = ALL '+'                                 
140600           IF REQU-FLAGGA-ADM = JA OR YES                                 
140700             MOVE 0           TO UPPF-KDKVASTA-ADM                        
140800           ELSE                                                           
140900             MOVE 3           TO UPPF-KDKVASTA-ADM                        
141000           END-IF                                                         
141100           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
141200                              RESP-FLAGGA-ADM-ATTR                        
141300         END-IF                                                           
141400                                                                          
141500         IF REQU-FLAGGA-SEK NOT = ALL '+'                                 
141600           IF REQU-FLAGGA-SEK = JA OR YES                                 
141700             MOVE 2           TO UPPF-KDKVASTA-SEK                        
141800           ELSE                                                           
141900             MOVE 3           TO UPPF-KDKVASTA-SEK                        
142000           END-IF                                                         
142100           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
142200                              RESP-FLAGGA-SEK-ATTR                        
142300         END-IF                                                           
142400                                                                          
142500         PERFORM IMS-REPL-UPFA01                                          
142600                                                                          
142700       END-IF                                                             
142800                                                                          
142900       IF NOT ETT-FLAGGA-NEJ                                              
143000         IF (UPPF-KDKVASTA-PRI = 2 OR  0 ) AND                            
143100            (UPPF-KDKVASTA-SEK = 2 OR  0 )                                
143200            PERFORM HE-KOLLA-OM-KR-SPEC-JA                                
143300            IF NOT ETT-FLAGGA-NEJ                                         
143400              PERFORM IMS-GHU-INLA11                                      
143500              IF SEGMENT-FINNS                                            
143600                MOVE NEJ TO ART-FLKVAKAR                                  
143700                PERFORM IMS-REPL-INLA11                                   
143800              END-IF                                                      
143900            ELSE                                                          
144000              CONTINUE                                                    
144100            END-IF                                                        
144200         ELSE                                                             
144300            CONTINUE                                                      
144400         END-IF                                                           
144500       END-IF                                                             
144600                                                                          
144700       IF WS-FLADM = JA                                                   
144800         IF 6202-NO-UPDATE AND                                            
144900           (REQU-FLAGGA-PRI = NEJ OR                                      
145000            REQU-FLAGGA-SEK = NEJ)                                        
145100           MOVE NO-IR-CREATED   TO RESP-IDMSG-INFO                        
145200         ELSE                                                             
145300           MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                        
145400         END-IF                                                           
145500         PERFORM MFS-RENSA-FAELT-IN                                       
145600       ELSE                                                               
145700         MOVE INF-QTY-IR-EXISTS TO RESP-IDMSG-INFO                        
145800       END-IF                                                             
145900     END-IF                                                               
146000     .                                                                    
146100     EJECT                                                                
146200                                                                          
146300 HA-NYUPPLAEGG-UPFA01 SECTION.                                            
146400     PERFORM IMS-GU-INLC01                                                
146500     IF SEGMENT-FINNS                                                     
146600       MOVE INLC-SEQB-IDLEVNR  TO W-IDLEVNR                               
146700       MOVE INLC-SEQB-IDLOPNRM TO W-IDLOPNRM-CSEQ                         
146800       MOVE INLC-SEQB-TIAVIDAT TO W-DAAVSDAT-CSEQ                         
146900       IF INLC-SEQB-TIAVIDAT NOT = ZERO                                   
147000         IF INLC-SEQB-TIAVIDAT < 500000                                   
147100           MOVE 20             TO W-DAAVSDAT-CSEQ (1:2)                   
147200         ELSE                                                             
147300           IF INLC-SEQB-TIAVIDAT < 999999                                 
147400             MOVE 19           TO W-DAAVSDAT-CSEQ (1:2)                   
147500           ELSE                                                           
147600             MOVE 99999999     TO W-DAAVSDAT-CSEQ                         
147700           END-IF                                                         
147800         END-IF                                                           
147900       END-IF                                                             
148000       PERFORM S01-KOLLA-UPPDAT-TILLATEN                                  
148100       IF INDATA-OK                                                       
148200         PERFORM IMS-GU-KVAH01                                            
148300         IF SEGMENT-FINNS                                                 
148400           MOVE KVAH-ART-ADKVAULG TO WS-ADKVAULG                          
148500           MOVE KVAH-ART-KDKVAKTL TO WS-KDKVAKTL                          
148600         END-IF                                                           
148700         MOVE W-IDLOPNRM       TO UPPF-IDLOPNRM                           
148800         MOVE W-IDDC           TO UPPF-IDDC                               
148900         MOVE WS-ADKVAULG      TO UPPF-ADKVAULG                           
149000         MOVE SPACE            TO UPPF-BEANST                             
149100         MOVE NEJ              TO UPPF-FLANNULL                           
149200         MOVE NEJ              TO UPPF-FLKVARED                           
149300         MOVE JA               TO UPPF-FLSKPSAK                           
149400         MOVE NEJ              TO UPPF-FLKVAUTV-ANT                       
149500         MOVE NEJ              TO UPPF-FLKVAUTV-KVAL                      
149600         MOVE W-IDARTNR        TO UPPF-IDARTNR                            
149700         MOVE W-IDLEVNR        TO UPPF-IDLEVNR                            
149800         IF REQU-IDUSER-PRI NOT = ALL '+'                                 
149900           MOVE REQU-IDUSER-PRI TO UPPF-IDUSER-PRI                        
150000         ELSE                                                             
150100           MOVE SPACE          TO UPPF-IDUSER-PRI                         
150200         END-IF                                                           
150300         IF REQU-IDUSER-ADM NOT = ALL '+'                                 
150400           IF REQU-FLAGGA-ADM = NEJ                                       
150500             MOVE REQU-IDUSER-ADM TO UPPF-IDUSER-ADM                      
150600           ELSE                                                           
150700             MOVE SPACE          TO UPPF-IDUSER-ADM                       
150800           END-IF                                                         
150900         ELSE                                                             
151000           MOVE SPACE          TO UPPF-IDUSER-ADM                         
151100         END-IF                                                           
151200         IF REQU-IDUSER-SEK NOT = ALL '+'                                 
151300           MOVE REQU-IDUSER-SEK TO UPPF-IDUSER-SEK                        
151400         ELSE                                                             
151500           MOVE SPACE          TO UPPF-IDUSER-SEK                         
151600         END-IF                                                           
151700         MOVE WS-KDKVAKTL      TO UPPF-KDKVAKTL                           
151800         MOVE '0'              TO UPPF-KDKVASTA-ANT                       
151900         IF REQU-FLAGGA-PRI NOT = ALL '+'                                 
152000           MOVE '3'            TO UPPF-KDKVASTA-PRI                       
152100         ELSE                                                             
152200           MOVE '0'            TO UPPF-KDKVASTA-PRI                       
152300         END-IF                                                           
152400         IF REQU-FLAGGA-ADM NOT = ALL '+'                                 
152500           MOVE '3'            TO UPPF-KDKVASTA-ADM                       
152600         ELSE                                                             
152700           MOVE '0'            TO UPPF-KDKVASTA-ADM                       
152800         END-IF                                                           
152900         IF REQU-FLAGGA-SEK NOT = ALL '+'                                 
153000           MOVE '3'            TO UPPF-KDKVASTA-SEK                       
153100         ELSE                                                             
153200           MOVE '0'            TO UPPF-KDKVASTA-SEK                       
153300         END-IF                                                           
153400         MOVE '0'              TO UPPF-KVKVAPRIM                          
153500         MOVE '0'              TO UPPF-KVKVASEK                           
153600         MOVE DAGENS-DATUM     TO UPPF-TIREGDAT                           
153700                                                                          
153800         PERFORM IMS-ISRT-UPFA01                                          
153900                                                                          
154000         PERFORM IMS-GHU-INLA11                                           
154100         IF SEGMENT-FINNS                                                 
154200*          MOVE JA             TO ART-FLKVAFEL                            
154300           PERFORM IMS-REPL-INLA11                                        
154400         END-IF                                                           
154500                                                                          
154600         MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                          
154700         PERFORM MFS-RENSA-FAELT-IN                                       
154800         PERFORM MFS-FORM-ATTR                                            
154900       END-IF                                                             
155000     ELSE                                                                 
155100       MOVE ERR-NO-UPDATE     TO RESP-IDMSG-ERROR                         
155200       PERFORM MFS-RENSA-FAELT-IN                                         
155300       MOVE NEJ               TO INDATA-SW                                
155400     END-IF                                                               
155500     .                                                                    
155600     EJECT                                                                
155700 HB-UPPDATERA-KR-EV SECTION.                                              
155800     MOVE JA TO 6202-UPDATE-SW                                            
155900                                                                          
156000     PERFORM IMS-GHU-INLA11                                               
156100     IF SEGMENT-FINNS                                                     
156200       IF NOT NYUPPLAEGG                                                  
156300         IF REQU-FLAGGA-PRI NOT = ALL '+'                                 
156400           IF ART-KVKVAPRIM-BER > +0                                      
156500             MOVE ART-KVKVAPRIM-BER TO ART-KVKVAPRIM-VER                  
156600           END-IF                                                         
156700         END-IF                                                           
156800         IF REQU-FLAGGA-SEK NOT = ALL '+'                                 
156900           IF ART-KVKVASEK-BER > +0                                       
157000             MOVE ART-KVKVASEK-BER TO ART-KVKVASEK-VER                    
157100           END-IF                                                         
157200         END-IF                                                           
157300       END-IF                                                             
157400       IF REQU-FLAGGA-PRI = NEJ OR                                        
157500          REQU-FLAGGA-ADM = NEJ OR                                        
157600          REQU-FLAGGA-SEK = NEJ OR                                        
157700          REQU-FLAGGA-GODK = NEJ OR NYUPPLAEGG                            
157800          MOVE JA TO ETT-FLAGGA-NEJ-SW                                    
157900       END-IF                                                             
158000       PERFORM IMS-REPL-INLA11                                            
158100     END-IF                                                               
158200                                                                          
158300     IF REQU-FLAGGA-PRI = NEJ OR                                          
158400        REQU-FLAGGA-SEK = NEJ OR                                          
158500        REQU-FLAGGA-GODK = NEJ                                            
158600        PERFORM IMS-GU-KVAE01-CSEQ                                        
158700        IF SEGMENT-SAKNAS                                                 
158800          CONTINUE                                                        
158900        ELSE                                                              
159000          PERFORM UNTIL SEGMENT-SAKNAS OR 6202-NO-UPDATE                  
159100            IF KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                          
159200              CONTINUE                                                    
159300            ELSE                                                          
159400              MOVE KR-IDKR TO W-IDKR                                      
159500              MOVE NEJ TO 6202-UPDATE-SW                                  
159600              PERFORM IMS-GU-UPFA01                                       
159700              PERFORM IMS-GHU-KVAE01                                      
159800              IF REQU-FLAGGA-PRI = NEJ                                    
159900                IF UPPF-KDKVASTA-SEK >= ZERO AND                          
160000                   UPPF-KDKVASTA-SEK <= '2'                               
160100                  MOVE '1' TO KR-KDKRUTF                                  
160200                ELSE                                                      
160300                  IF UPPF-KDKVASTA-SEK = '3'                              
160400                    MOVE '3' TO KR-KDKRUTF                                
160500                  END-IF                                                  
160600                END-IF                                                    
160700              END-IF                                                      
160800              IF REQU-FLAGGA-SEK = NEJ                                    
160900                IF (UPPF-KDKVASTA-PRI >= ZERO AND                         
161000                   UPPF-KDKVASTA-PRI <= '2')                              
161100                  MOVE '2' TO KR-KDKRUTF                                  
161200                ELSE                                                      
161300                  IF UPPF-KDKVASTA-PRI = '3'                              
161400                    MOVE '3' TO KR-KDKRUTF                                
161500                  END-IF                                                  
161600                END-IF                                                    
161700              END-IF                                                      
161800              PERFORM IMS-REPL-KVAE01                                     
161900            END-IF                                                        
162000            PERFORM IMS-GN-KVAE01-CSEQ                                    
162100          END-PERFORM                                                     
162200        END-IF                                                            
162300                                                                          
162400        IF 6202-UPDATE                                                    
162500          COMPUTE ALT-LL  = LENGTH OF ALT-REQU-6202 + 12                  
162510          SET ALT-REQU-UPD-X   TO TRUE                                    
162600          MOVE ZERO            TO ALT-REQU-IDKR-KEY                       
162700          MOVE REQU-IDDC-KEY   TO ALT-REQU-IDDC-KEY                       
162800          MOVE ALL '+'         TO ALT-REQU-INPUT                          
162900          MOVE ALL '+'         TO ALT-REQU-KDKRUTF-UPD                    
163000          MOVE W-IDLOPNRM-CSEQ TO ALT-REQU-IDLOPNRM-UPD                   
163100          MOVE W-DAAVSDAT-CSEQ (3:6) TO ALT-REQU-TIAVSDAT-UPD             
163200          IF  REQU-FLAGGA-PRI = NEJ AND                                   
163300              REQU-FLAGGA-SEK = NEJ                                       
163400             MOVE '3'          TO ALT-REQU-KDKRUTF-UPD                    
163500          ELSE                                                            
163600            IF REQU-FLAGGA-PRI = NEJ                                      
163700              MOVE '1'         TO ALT-REQU-KDKRUTF-UPD                    
163800            ELSE                                                          
163900              IF REQU-FLAGGA-SEK = NEJ                                    
164000                MOVE '2'       TO ALT-REQU-KDKRUTF-UPD                    
164100              END-IF                                                      
164200            END-IF                                                        
164300          END-IF                                                          
164400          PERFORM IMS-ISRT-ALT-MSG                                        
164500        END-IF                                                            
164600     END-IF                                                               
164700                                                                          
164800     IF REQU-FLAGGA-ADM = NEJ                                             
164900        PERFORM IMS-GU-KVAE01-CSEQ                                        
165000        IF SEGMENT-SAKNAS                                                 
165100          CONTINUE                                                        
165200        ELSE                                                              
165300          MOVE JA          TO 6202-UPDATE-SW                              
165400          PERFORM UNTIL SEGMENT-SAKNAS OR 6202-NO-UPDATE                  
165500            IF KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                          
165600              MOVE KR-IDKR TO W-IDKR                                      
165700              MOVE NEJ     TO 6202-UPDATE-SW                              
165800              MOVE NEJ     TO WS-FLADM                                    
165900            END-IF                                                        
166000            PERFORM IMS-GN-KVAE01-CSEQ                                    
166100          END-PERFORM                                                     
166200        END-IF                                                            
166300                                                                          
166400        IF 6202-UPDATE                                                    
166500          COMPUTE ALT-LL = LENGTH OF ALT-REQU-6202 + 12                   
166510          SET ALT-REQU-UPD-X   TO TRUE                                    
166600          MOVE ZERO            TO ALT-REQU-IDKR-KEY                       
166700          MOVE REQU-IDDC-KEY   TO ALT-REQU-IDDC-KEY                       
166800          MOVE ALL '+'         TO ALT-REQU-INPUT                          
166900          MOVE ALL '+'         TO ALT-REQU-KDKRUTF-UPD                    
167000          MOVE W-IDLOPNRM-CSEQ TO ALT-REQU-IDLOPNRM-UPD                   
167100          MOVE W-DAAVSDAT-CSEQ (3:6) TO ALT-REQU-TIAVSDAT-UPD             
167200          MOVE '1'             TO ALT-REQU-KDKRUTF-UPD                    
167300          MOVE 'K '            TO ALT-REQU-IDKRFEL-UPD                    
167400          PERFORM IMS-ISRT-ALT-MSG                                        
167500        END-IF                                                            
167600     END-IF                                                               
167700     .                                                                    
167800     EJECT                                                                
167900 HC-UPPDATERA-KONTROLLRAPPORT SECTION.                                    
168000     MOVE REQU-IDKR-START     TO W-IDKR                                   
168100     PERFORM IMS-GHU-UPFA11                                               
168200     IF SEGMENT-FINNS                                                     
168300       IF REQU-FLAGGA-GODK = JA OR YES                                    
168400         MOVE 2               TO RAPP-KDKVASTA-PRI                        
168500         IF WS-FLUPG = JA OR YES                                          
168600           PERFORM IMS-GHU-INLA11                                         
168700           IF SEGMENT-FINNS                                               
168800             MOVE NEJ TO ART-FLKVAKAR                                     
168900             PERFORM IMS-REPL-INLA11                                      
169000           END-IF                                                         
169100         END-IF                                                           
169200       ELSE                                                               
169300         MOVE 3               TO RAPP-KDKVASTA-PRI                        
169400       END-IF                                                             
                                                                                
             MOVE REQU-IDUSER-APR   TO RAPP-IDUSER                              
                                                                                
169500       PERFORM IMS-REPL-UPFA11                                            
169600                                                                          
169700       PERFORM IMS-GHU-INLA11                                             
169800       IF SEGMENT-FINNS                                                   
169900         IF ART-KVKVAPRIM-BER > +0                                        
170000           IF ART-KVKVAPRIM-BER NOT = ART-KVKVAPRIM-VER                   
170100             MOVE ART-KVKVAPRIM-BER TO ART-KVKVAPRIM-VER                  
170200             PERFORM IMS-REPL-INLA11                                      
170300           END-IF                                                         
170400         END-IF                                                           
170500       END-IF                                                             
170600                                                                          
170700       PERFORM IMS-GHU-KVAE01                                             
170800       IF SEGMENT-FINNS                                                   
170900         IF KR-KVKRKNTR > +0                                              
171000           COMPUTE KR-KVKRKNTR = KR-KVKRKNTR - 1                          
171100           PERFORM IMS-REPL-KVAE01                                        
171200         END-IF                                                           
171300       END-IF                                                             
171400       MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-FLAGGA-GODK-ATTR                
171400       MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDUSER-APR-ATTR                 
171500     END-IF                                                               
171600     .                                                                    
171700     EJECT                                                                
171800 HD-UPPDATERA-SPECIALRAPPORT SECTION.                                     
171900     MOVE REQU-IDKVAINF-START TO W-IDKVAINF                               
172000     PERFORM IMS-GHU-UPFA12                                               
172100     IF SEGMENT-FINNS                                                     
172200       IF REQU-FLAGGA-GODK = JA or yes                                    
172300         MOVE 2               TO SPEC-KDKVASTA-PRI                        
               MOVE REQU-IDUSER-APR TO SPEC-IDUSER                              
172400         PERFORM IMS-REPL-UPFA12                                          
172500       ELSE                                                               
172600         MOVE 3               TO SPEC-KDKVASTA-PRI                        
               MOVE REQU-IDUSER-APR TO SPEC-IDUSER                              
172700         PERFORM IMS-REPL-UPFA12                                          
172800       END-IF                                                             
172900                                                                          
173000       PERFORM IMS-GHU-INLA11                                             
173100       IF SEGMENT-FINNS                                                   
173200         IF ART-KVKVAPRIM-BER > +0                                        
173300           IF ART-KVKVAPRIM-BER NOT = ART-KVKVAPRIM-VER                   
173400             MOVE ART-KVKVAPRIM-BER TO ART-KVKVAPRIM-VER                  
173500             PERFORM IMS-REPL-INLA11                                      
173600           END-IF                                                         
173700         END-IF                                                           
173800       END-IF                                                             
173900                                                                          
174000       PERFORM IMS-GHU-KVAH22                                             
174100       IF SEGMENT-FINNS                                                   
174200         COMPUTE KVAH-SPEC-KVSKPLOT-PRI =                                 
174300                            KVAH-SPEC-KVSKPLOT-PRI - 1                    
174400         IF KVAH-SPEC-KVSKPLOT-PRI > +0                                   
174500           PERFORM IMS-REPL-KVAH22                                        
174600         ELSE                                                             
174700           PERFORM IMS-DLET-KVAH22                                        
174800         END-IF                                                           
174900       END-IF                                                             
175000       MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-FLAGGA-GODK-ATTR                
175000       MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDUSER-APR-ATTR                 
175100     END-IF                                                               
175200     .                                                                    
175300     EJECT                                                                
175400 HE-KOLLA-OM-KR-SPEC-JA SECTION.                                          
175500     PERFORM IMS-GU-UPFA01                                                
175600     IF SEGMENT-FINNS                                                     
175700       PERFORM IMS-GNP-UPFA11                                             
175800       IF SEGMENT-FINNS                                                   
175900         PERFORM UNTIL SEGMENT-SAKNAS OR ETT-FLAGGA-NEJ                   
176000           IF RAPP-KDKVASTA-PRI = 3                                       
176100             MOVE JA TO ETT-FLAGGA-NEJ-SW                                 
176200           END-IF                                                         
176300           PERFORM IMS-GNP-UPFA11                                         
176400         END-PERFORM                                                      
176500       END-IF                                                             
176600                                                                          
176700       IF NOT ETT-FLAGGA-NEJ                                              
176800         PERFORM IMS-GNP-UPFA12                                           
176900         IF SEGMENT-FINNS                                                 
177000           PERFORM UNTIL SEGMENT-SAKNAS OR ETT-FLAGGA-NEJ                 
177100             IF SPEC-KDKVASTA-PRI = 3                                     
177200               MOVE JA TO ETT-FLAGGA-NEJ-SW                               
177300             END-IF                                                       
177400             PERFORM IMS-GNP-UPFA12                                       
177500           END-PERFORM                                                    
177600         END-IF                                                           
177700       END-IF                                                             
177800     END-IF                                                               
177900     .                                                                    
178000     EJECT                                                                
178100 J-KOLLA-HOPP-TILL-6202 SECTION.                                          
178200     IF REQU-FLAGGA-KR-HOPP = JA OR YES                                   
178300       PERFORM IMS-GU-INLC01                                              
178400       IF SEGMENT-FINNS                                                   
178500         MOVE INLC-SEQB-IDLOPNRM TO W-IDLOPNRM-CSEQ                       
178600         MOVE INLC-SEQB-TIAVIDAT TO W-DAAVSDAT-CSEQ                       
178700         IF INLC-SEQB-TIAVIDAT NOT = ZERO                                 
178800           IF INLC-SEQB-TIAVIDAT < 500000                                 
178900             MOVE 20             TO W-DAAVSDAT-CSEQ (1:2)                 
179000           ELSE                                                           
179100             IF INLC-SEQB-TIAVIDAT < 999999                               
179200               MOVE 19           TO W-DAAVSDAT-CSEQ (1:2)                 
179300             ELSE                                                         
179400               MOVE 99999999     TO W-DAAVSDAT-CSEQ                       
179500             END-IF                                                       
179600           END-IF                                                         
179700         END-IF                                                           
179800       END-IF                                                             
179900       PERFORM IMS-GU-KVAE01-CSEQ                                         
180000       IF SEGMENT-SAKNAS                                                  
180001         IF REQU-IDMSGVER = '001'                                         
180010           MOVE ZERO             TO RESP-IDKR                             
180020         END-IF                                                           
180100         MOVE INF-INFO-MISSING TO RESP-IDMSG-INFO                         
180200         MOVE 'INF'            TO RESP-IDELMT-ERROR                       
180300       ELSE                                                               
180400         PERFORM UNTIL SEGMENT-SAKNAS OR HOPP-TILL-6202                   
180500           IF KR-IDKRFEL = 'PA' OR 'PB'                                   
180600             CONTINUE                                                     
180700           ELSE                                                           
180800             MOVE JA           TO HOPP-TILL-6202-SW                       
180900                                  RESP-FLAGGA-KR-HOPP                     
181000             MOVE KR-IDKR      TO WS-IDKR                                 
181100           END-IF                                                         
181200           PERFORM IMS-GN-KVAE01-CSEQ                                     
181300         END-PERFORM                                                      
181400         IF HOPP-TILL-6202                                                
181500           IF REQU-IDMSGVER = '001'                                       
181600             MOVE WS-IDKR       TO RESP-IDKR                              
181700           ELSE                                                           
181800             COMPUTE ALT-LL-IMS = LENGTH OF ALT-MID-W6I20201 + 17         
181900             MOVE WS-IDKR       TO ALT-MID-IDKR-IN                        
182000             MOVE SPACE         TO ALT-MID-IDKR-UT                        
182100             MOVE ALL '+'       TO ALT-MID-INPUT                          
182300             PERFORM IMS-ISRT-ALT-MSG-IMS                                 
182310           END-IF                                                         
182400         ELSE                                                             
182500           MOVE INF-INFO-MISSING TO RESP-IDMSG-INFO                       
182600           MOVE 'INF'            TO RESP-IDELMT-ERROR                     
182700         END-IF                                                           
182800       END-IF                                                             
182900     END-IF                                                               
183000     .                                                                    
183100     EJECT                                                                
183200 S01-KOLLA-UPPDAT-TILLATEN SECTION.                                       
183300     IF NOT BYT03-OBJEKT AND NOT BYT07-OBJEKT-RADIO                       
183400       PERFORM IMS-GU-ARTC01                                              
183500       IF SEGMENT-FINNS                                                   
183600**3324 CARPAC                                                             
183700         PERFORM IMS-GU-INLA11                                            
183800         IF SEGMENT-FINNS                                                 
183900           IF ART-KDRT NOT = +3 OR +6 OR +7 OR +77 OR +8                  
184000             IF ART-FLKLAR = JA                                           
184100                 MOVE NEJ TO INDATA-SW                                    
184200             ELSE                                                         
184300               CONTINUE                                                   
184400             END-IF                                                       
184500           ELSE                                                           
184600             MOVE NEJ TO INDATA-SW                                        
184700           END-IF                                                         
184800         ELSE                                                             
184900           MOVE NEJ TO INDATA-SW                                          
185000         END-IF                                                           
185100       END-IF                                                             
185200     ELSE                                                                 
185300       MOVE NEJ TO INDATA-SW                                              
185400     END-IF                                                               
185500                                                                          
185600     IF INDATA-OK                                                         
185700       PERFORM IMS-GU-KVAE01-CSEQ                                         
185800       IF SEGMENT-SAKNAS                                                  
185900         CONTINUE                                                         
186000       ELSE                                                               
186100         PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                       
186200           IF KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                           
186300             CONTINUE                                                     
186400           ELSE                                                           
186500             IF KR-KDKRSTA > 1                                            
186600               IF REQU-IDUSER-PRI NOT = ALL '+' OR                        
186700                  REQU-FLAGGA-PRI NOT = ALL '+' OR                        
186800                  REQU-BEANST NOT = ALL '+' OR                            
186900                  REQU-IDUSER-SEK NOT = ALL '+' OR                        
187000                  REQU-FLAGGA-SEK NOT = ALL '+' OR                        
187100                 (REQU-IDUSER-ADM NOT = ALL '+' AND                       
187200                  REQU-FLAGGA-ADM = ALL '+')   OR                         
187300                 (REQU-IDUSER-ADM = ALL '+' AND                           
187400                  REQU-FLAGGA-ADM NOT = ALL '+')   OR                     
187500                  REQU-FLAGGA-GODK NOT = ALL '+'                          
187600                  MOVE NEJ       TO INDATA-SW                             
187700               END-IF                                                     
187800             END-IF                                                       
187900           END-IF                                                         
188000           PERFORM IMS-GN-KVAE01-CSEQ                                     
188100         END-PERFORM                                                      
188200       END-IF                                                             
188300     END-IF                                                               
188400                                                                          
188500     PERFORM IMS-GU-KVAH12                                                
188600     IF SEGMENT-FINNS                                                     
188700       MOVE KVAH-LEV-FLUPG TO WS-FLUPG                                    
188800     END-IF                                                               
188900                                                                          
189000     IF INDATA-FEL                                                        
189100       MOVE ERR-NO-UPDATE TO RESP-IDMSG-ERROR                             
189200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
189300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
189400       IF REQU-IDKR-START = ZERO AND REQU-IDKVAINF-START = ZERO           
189500         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-GODK-ATTR             
189500         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDUSER-APR-ATTR              
189600       ELSE                                                               
189700         MOVE MFS-STAENG-FAELT-NOMOD TO RESP-BEANST-ATTR                  
189800       END-IF                                                             
189900     END-IF                                                               
190000     .                                                                    
190100     EJECT                                                                
190200 S02-RENSA-JA-FLAGGOR SECTION.                                            
190300                                                                          
190400     IF REQU-FLAGGA-PRI = 'J'                                             
190500        MOVE SPACE     TO  RESP-FLAGGA-PRI                                
190600     END-IF                                                               
190700     IF REQU-FLAGGA-SEK = 'J'                                             
190800        MOVE SPACE     TO  RESP-FLAGGA-SEK                                
190900     END-IF                                                               
191000     IF REQU-FLAGGA-ADM = 'J'                                             
191100        MOVE SPACE     TO  RESP-FLAGGA-ADM                                
191200     END-IF                                                               
191300     IF REQU-FLAGGA-GODK = 'J'                                            
191400        MOVE SPACE     TO  RESP-FLAGGA-GODK                               
191500     END-IF                                                               
191600     .                                                                    
191700     EJECT                                                                
191800 MFS-RENSA-FAELT-IN SECTION.                                              
191900                                                                          
192000*    --- ALLA INDATA-FÄLT                                                 
192100     MOVE ALL-SPACE       TO RESP-FLAGGA-KR-HOPP                          
192200                             RESP-IDUSER-PRI                              
192300                             RESP-IDUSER-ADM                              
192400                             RESP-BEANST                                  
192500                             RESP-IDUSER-SEK                              
192600                             RESP-FLAGGA-PRI                              
192700                             RESP-FLAGGA-ADM                              
192800                             RESP-FLAGGA-SEK                              
192900                             RESP-FLAGGA-GODK                             
                                   RESP-IDUSER-APR                              
193000     .                                                                    
193100     SKIP2                                                                
193200 MFS-RENSA-FAELT-UT SECTION.                                              
193300                                                                          
193400*    --- ALLA UTDATA-FÄLT                                                 
193500     MOVE ALL-SPACE       TO RESP-IDKR-START                              
193600                             RESP-IDKR-NEXT                               
193700                             RESP-IDKVAINF-START                          
193800                             RESP-IDKVAINF-NEXT                           
193900                             RESP-IDARTNR                                 
194100                             RESP-BELEVART                                
194200                             RESP-IDLEVNR                                 
194300                             RESP-KVAVIS                                  
194400                             RESP-KVKVAPRIM                               
194500                             RESP-KVKVASEK                                
194600                             RESP-KDKVATYP                                
194700                             RESP-IDKR                                    
194800                             RESP-UNDERLAG                                
194900                             RESP-SPEC-BEANST                             
195000                             RESP-ADKVAULG                                
195100                             RESP-KONTROLL                                
195200                             RESP-IDTFN                                   
195300                             RESP-TEKRFEL (1)                             
195400                             RESP-TEKRFEL (2)                             
195500                             RESP-TEKRFEL (3)                             
195600                             RESP-TEKRFEL (4)                             
                                   RESP-EMPLID-TEXT                             
                                                                                
195610     IF REQU-IDMSGVER = '001'                                             
195620       MOVE ALL-UTF8-SPACE TO RESP-BEART                                  
195630     END-IF                                                               
195640                                                                          
195700     .                                                                    
195800     SKIP2                                                                
195900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
196000                                                                          
196100     MOVE ALL-PLUS          TO RESP-FLAGGA-KR-HOPP                        
196200                               RESP-IDUSER-PRI                            
196300                               RESP-IDUSER-ADM                            
196400                               RESP-IDUSER-SEK                            
196500                               RESP-BEANST                                
196600                               RESP-FLAGGA-PRI                            
196700                               RESP-FLAGGA-ADM                            
196800                               RESP-FLAGGA-SEK                            
196900                               RESP-FLAGGA-GODK                           
                                     RESP-IDUSER-APR                            
197000                                                                          
197100     .                                                                    
197200     EJECT                                                                
197300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
197400     MOVE ALL-PLUS          TO RESP-IDKR-START                            
197500                               RESP-IDKR-NEXT                             
197600                               RESP-IDKVAINF-START                        
197700                               RESP-IDKVAINF-NEXT                         
197800                               RESP-IDARTNR                               
198000                               RESP-BELEVART                              
198100                               RESP-IDLEVNR                               
198200                               RESP-KVAVIS                                
198300                               RESP-KVKVAPRIM                             
198400                               RESP-KVKVASEK                              
198500                               RESP-KDKVATYP                              
198600                               RESP-IDKR                                  
198700                               RESP-FLAGGA-TEXT                           
198800                               RESP-UNDERLAG                              
198900                               RESP-SPEC-BEANST                           
199000                               RESP-ADKVAULG                              
199100                               RESP-KONTROLL                              
199200                               RESP-IDTFN                                 
199300                               RESP-TEKRFEL (1)                           
199400                               RESP-TEKRFEL (2)                           
199500                               RESP-TEKRFEL (3)                           
199600                               RESP-TEKRFEL (4)                           
                                     RESP-EMPLID-TEXT                           
199610     IF REQU-IDMSGVER = '001'                                             
199620       MOVE ALL-UTF8-PLUS  TO RESP-BEART                                  
199630     END-IF                                                               
199640                                                                          
199700     .                                                                    
199800     EJECT                                                                
199900 MFS-LAES-IN-IGEN SECTION.                                                
200000                                                                          
200100*    --- ALLA INDATA-FÄLT                                                 
200200                                                                          
200300     IF REQU-FLAGGA-KR-HOPP NOT = ALL '+'                                 
200400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-KR-HOPP-ATTR             
200500     END-IF                                                               
200600                                                                          
200700     IF REQU-IDUSER-PRI NOT = ALL '+'                                     
200800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-PRI-ATTR                 
200900     END-IF                                                               
201000                                                                          
201100     IF REQU-IDUSER-ADM NOT = ALL '+'                                     
201200       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-ADM-ATTR                 
201300     END-IF                                                               
201400                                                                          
201500     IF REQU-IDUSER-SEK NOT = ALL '+'                                     
201600       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-SEK-ATTR                 
201700     END-IF                                                               
201800                                                                          
201900     IF REQU-BEANST NOT = ALL '+'                                         
202000       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-BEANST-ATTR                     
202100     END-IF                                                               
202200                                                                          
202300     IF REQU-FLAGGA-PRI NOT = ALL '+'                                     
202400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-PRI-ATTR                 
202500     END-IF                                                               
202600                                                                          
202700     IF REQU-FLAGGA-ADM NOT = ALL '+'                                     
202800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-ADM-ATTR                 
202900     END-IF                                                               
203000                                                                          
203100     IF REQU-FLAGGA-SEK NOT = ALL '+'                                     
203200       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-SEK-ATTR                 
203300     END-IF                                                               
203400                                                                          
203500     IF REQU-FLAGGA-GODK NOT = ALL '+'                                    
203600       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLAGGA-GODK-ATTR                
203700     END-IF                                                               
203800                                                                          
           IF REQU-IDUSER-APR NOT = ALL '+'                                     
             MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDUSER-APR-ATTR                 
           END-IF                                                               
203900     .                                                                    
204000     EJECT                                                                
204100 MFS-FORM-ATTR SECTION.                                                   
204200                                                                          
204300*    --- ALLA INDATA-FÄLT                                                 
204400                                                                          
204500     MOVE MFS-FORMATETS-ATTR TO RESP-FLAGGA-KR-HOPP-ATTR                  
204600                                RESP-IDUSER-PRI-ATTR                      
204700                                RESP-IDUSER-ADM-ATTR                      
204800                                RESP-IDUSER-SEK-ATTR                      
204900                                RESP-BEANST-ATTR                          
205000                                RESP-FLAGGA-PRI-ATTR                      
205100                                RESP-FLAGGA-SEK-ATTR                      
205200                                RESP-FLAGGA-GODK-ATTR                     
                                      RESP-IDUSER-APR-ATTR                      
205300                                                                          
205400     .                                                                    
205500     EJECT                                                                
205600 MFS-STAENG-ANDRA-FAELT SECTION.                                          
205700*    --- ALLA INDATA-FÄLT                                                 
205800     MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-KR-HOPP-ATTR              
205900                                    RESP-BEANST-ATTR                      
206000                                    RESP-FLAGGA-GODK-ATTR                 
206000                                    RESP-IDUSER-APR-ATTR                  
206100     .                                                                    
206200     EJECT                                                                
206300* --- IMS SEKTIONER ---                                                   
206400 IMS-ISRT-ALT-MSG SECTION.                                                
206500     MOVE SPACE TO GODK-STATUSKODER                                       
206600     CALL CBLTDLI USING ISRT ALT-PCB ALT-MSG-IO-AREA                      
206700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
206800     PERFORM IMS-STATUSKONTROLL                                           
206900     .                                                                    
207000     EJECT                                                                
207010 IMS-ISRT-ALT-MSG-IMS SECTION.                                            
207020     MOVE SPACE TO GODK-STATUSKODER                                       
207030     CALL CBLTDLI USING ISRT ALT-IMS-PCB ALT-MSG-AREA-IMS                 
207040     MOVE ALT-IMS-STATUS-CODE TO STATUS-WS                                
207050     PERFORM IMS-STATUSKONTROLL                                           
207060     .                                                                    
207070     EJECT                                                                
207100 IMS-GU-BENA-WLBENA11 SECTION.                                            
207200                                                                          
207300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
207400     DELIMITED BY SIZE INTO SSA1                                          
207500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-KEY-X ')'                     
207600     DELIMITED BY SIZE INTO SSA2                                          
207700     MOVE '  GE' TO GODK-STATUSKODER                                      
207800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA11 SSA1 SSA2          
207900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
208000     PERFORM IMS-STATUSKONTROLL                                           
208100     .                                                                    
208200     SKIP3                                                                
208300 IMS-GU-UPFA01 SECTION.                                                   
208400     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
208500          DELIMITED BY SIZE INTO SSA1                                     
208600     MOVE '  GE' TO GODK-STATUSKODER                                      
208700     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
208800     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
208900     PERFORM IMS-STATUSKONTROLL                                           
209000     .                                                                    
209100     SKIP3                                                                
209200 IMS-GHU-UPFA01 SECTION.                                                  
209300     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
209400          DELIMITED BY SIZE INTO SSA1                                     
209500     MOVE '  GE' TO GODK-STATUSKODER                                      
209600     CALL CBLTDLI USING GHU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1              
209700     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
209800     PERFORM IMS-STATUSKONTROLL                                           
209900     .                                                                    
210000     EJECT                                                                
210100 IMS-GNP-UPFA11 SECTION.                                                  
210200                                                                          
210300     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
210400          DELIMITED BY SIZE INTO SSA1                                     
210500     MOVE 'W6UPFA11 ' TO SSA2                                             
210600     MOVE '  GE' TO GODK-STATUSKODER                                      
210700     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
210800     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100     SKIP3                                                                
211200 IMS-GNP-UPFA12 SECTION.                                                  
211300                                                                          
211400     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
211500          DELIMITED BY SIZE INTO SSA1                                     
211600     MOVE 'W6UPFA12 ' TO SSA2                                             
211700     MOVE '  GE' TO GODK-STATUSKODER                                      
211800     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
211900     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
212000     PERFORM IMS-STATUSKONTROLL                                           
212100     .                                                                    
212200     SKIP3                                                                
212300 IMS-GNP-UPFA11-KVAL SECTION.                                             
212400     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
212500          DELIMITED BY SIZE INTO SSA1                                     
212600     STRING 'W6UPFA11(IDKR     =' W-IDKR-X ')'                            
212700          DELIMITED BY SIZE INTO SSA2                                     
212800     MOVE '  GE' TO GODK-STATUSKODER                                      
212900     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
213000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
213100     PERFORM IMS-STATUSKONTROLL                                           
213200     .                                                                    
213300     EJECT                                                                
213400 IMS-GNP-UPFA12-KVAL SECTION.                                             
213500     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
213600          DELIMITED BY SIZE INTO SSA1                                     
213700     STRING 'W6UPFA12(IDKVAINF =' W-IDKVAINF-X ')'                        
213800          DELIMITED BY SIZE INTO SSA2                                     
213900     MOVE '  GE' TO GODK-STATUSKODER                                      
214000     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
214100     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
214200     PERFORM IMS-STATUSKONTROLL                                           
214300     .                                                                    
214400     SKIP3                                                                
214500 IMS-GHU-UPFA11 SECTION.                                                  
214600     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
214700          DELIMITED BY SIZE INTO SSA1                                     
214800     STRING 'W6UPFA11(IDKR     =' W-IDKR-X ')'                            
214900          DELIMITED BY SIZE INTO SSA2                                     
215000     MOVE '  GE' TO GODK-STATUSKODER                                      
215100     CALL CBLTDLI USING GHU UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
215200     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
215300     PERFORM IMS-STATUSKONTROLL                                           
215400     .                                                                    
215500     SKIP3                                                                
215600 IMS-GHU-UPFA12 SECTION.                                                  
215700     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
215800          DELIMITED BY SIZE INTO SSA1                                     
215900     STRING 'W6UPFA12(IDKVAINF =' W-IDKVAINF-X ')'                        
216000          DELIMITED BY SIZE INTO SSA2                                     
216100     MOVE '  GE' TO GODK-STATUSKODER                                      
216200     CALL CBLTDLI USING GHU UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
216300     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     EJECT                                                                
216700 IMS-REPL-UPFA01 SECTION.                                                 
216800                                                                          
216900     MOVE '  ' TO GODK-STATUSKODER                                        
217000     CALL CBLTDLI USING REPL UPFA-PCB DLI-IO-AREA-UPFA01                  
217100     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
217200     PERFORM IMS-STATUSKONTROLL                                           
217300     .                                                                    
217400     SKIP3                                                                
217500 IMS-REPL-UPFA11 SECTION.                                                 
217600                                                                          
217700     MOVE '  ' TO GODK-STATUSKODER                                        
217800     CALL CBLTDLI USING REPL UPFA-PCB DLI-IO-AREA-UPFA11                  
217900     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
218000     PERFORM IMS-STATUSKONTROLL                                           
218100     .                                                                    
218200     SKIP3                                                                
218300 IMS-REPL-UPFA12 SECTION.                                                 
218400                                                                          
218500     MOVE '  ' TO GODK-STATUSKODER                                        
218600     CALL CBLTDLI USING REPL UPFA-PCB DLI-IO-AREA-UPFA12                  
218700     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
218800     PERFORM IMS-STATUSKONTROLL                                           
218900     .                                                                    
219000     SKIP3                                                                
219100 IMS-ISRT-UPFA01 SECTION.                                                 
219200                                                                          
219300     MOVE 'W6UPFA01 ' TO SSA1                                             
219400     MOVE '  ' TO GODK-STATUSKODER                                        
219500     CALL CBLTDLI USING ISRT UPFA-PCB DLI-IO-AREA-UPFA01 SSA1             
219600     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
219700     PERFORM IMS-STATUSKONTROLL                                           
219800     .                                                                    
219900     EJECT                                                                
220000 IMS-GU-KVAH01 SECTION.                                                   
220100     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
220200          DELIMITED BY SIZE INTO SSA1                                     
220300     MOVE '  GE' TO GODK-STATUSKODER                                      
220400     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA-KVAH01 SSA1               
220500     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
220600     PERFORM IMS-STATUSKONTROLL                                           
220700     .                                                                    
220800     SKIP3                                                                
220900 IMS-GU-KVAH12 SECTION.                                                   
221000     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
221100          DELIMITED BY SIZE INTO SSA1                                     
221200     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
221300          DELIMITED BY SIZE INTO SSA2                                     
221400     MOVE '  GE' TO GODK-STATUSKODER                                      
221500     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA-KVAH12 SSA1 SSA2          
221600     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
221700     PERFORM IMS-STATUSKONTROLL                                           
221800     .                                                                    
221900     SKIP3                                                                
222000 IMS-GHU-KVAH22 SECTION.                                                  
222100     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
222200          DELIMITED BY SIZE INTO SSA1                                     
222300     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
222400          DELIMITED BY SIZE INTO SSA2                                     
222500     STRING 'W6KVAH22(IDKVAINF =' W-IDKVAINF-X ')'                        
222600          DELIMITED BY SIZE INTO SSA3                                     
222700     MOVE '  GE' TO GODK-STATUSKODER                                      
222800     CALL CBLTDLI USING GHU KVAH-PCB DLI-IO-AREA-KVAH22                   
222900                                     SSA1 SSA2 SSA3                       
223000     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
223100     PERFORM IMS-STATUSKONTROLL                                           
223200     .                                                                    
223300     SKIP3                                                                
223400 IMS-GU-KVAH22 SECTION.                                                   
223500     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
223600          DELIMITED BY SIZE INTO SSA1                                     
223700     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
223800          DELIMITED BY SIZE INTO SSA2                                     
223900     STRING 'W6KVAH22(IDKVAINF =' W-IDKVAINF-X ')'                        
224000          DELIMITED BY SIZE INTO SSA3                                     
224100     MOVE '  GE' TO GODK-STATUSKODER                                      
224200     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA-KVAH22                    
224300                                     SSA1 SSA2 SSA3                       
224400     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
224500     PERFORM IMS-STATUSKONTROLL                                           
224600     .                                                                    
224700     EJECT                                                                
224800 IMS-DLET-KVAH22 SECTION.                                                 
224900                                                                          
225000     MOVE '  ' TO GODK-STATUSKODER                                        
225100     CALL CBLTDLI USING DLET KVAH-PCB DLI-IO-AREA-KVAH22                  
225200     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
225300     PERFORM IMS-STATUSKONTROLL                                           
225400     .                                                                    
225500     SKIP3                                                                
225600 IMS-REPL-KVAH22 SECTION.                                                 
225700                                                                          
225800     MOVE '  ' TO GODK-STATUSKODER                                        
225900     CALL CBLTDLI USING REPL KVAH-PCB DLI-IO-AREA-KVAH22                  
226000     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
226100     PERFORM IMS-STATUSKONTROLL                                           
226200     .                                                                    
226300     SKIP3                                                                
226400 IMS-GU-ARTC01 SECTION.                                                   
226500                                                                          
226600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
226700            DELIMITED BY SIZE INTO SSA1                                   
226800     MOVE '  GE' TO GODK-STATUSKODER                                      
226900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC01 SSA1               
227000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
227100     PERFORM IMS-STATUSKONTROLL                                           
227200     .                                                                    
227300     SKIP3                                                                
227400 IMS-GU-INLA11 SECTION.                                                   
227500     STRING 'W6INLA11(W6D1BSEQ =' W-IDLOPNRM-X                            
227600                    '&IDDC     =' W-IDDC-X ')'                            
227700          DELIMITED BY SIZE INTO SSA1                                     
227800     MOVE '  GE' TO GODK-STATUSKODER                                      
227900     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-INLA11 SSA1               
228000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
228100     PERFORM IMS-STATUSKONTROLL                                           
228200     .                                                                    
228300     EJECT                                                                
228400 IMS-GU-INLC01 SECTION.                                                   
228500     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
228600          DELIMITED BY SIZE INTO SSA1                                     
228700     MOVE '  GE' TO GODK-STATUSKODER                                      
228800     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA-INLC01 SSA1               
228900     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
229000     PERFORM IMS-STATUSKONTROLL                                           
229100     .                                                                    
229200     SKIP3                                                                
229300 IMS-GHU-INLA11 SECTION.                                                  
229400     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
229500          DELIMITED BY SIZE INTO SSA1                                     
229600     MOVE '  GE' TO GODK-STATUSKODER                                      
229700     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA-INLA11 SSA1              
229800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
229900     PERFORM IMS-STATUSKONTROLL                                           
230000     .                                                                    
230100     SKIP3                                                                
230200 IMS-REPL-INLA11 SECTION.                                                 
230300                                                                          
230400     MOVE '  ' TO GODK-STATUSKODER                                        
230500     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA-INLA11                  
230600     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
230700     PERFORM IMS-STATUSKONTROLL                                           
230800     .                                                                    
230900     EJECT                                                                
231000 IMS-GU-WDF502 SECTION.                                                   
231100                                                                          
231200     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
231300            DELIMITED BY SIZE INTO SSA1                                   
231400     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
231500            DELIMITED BY SIZE INTO SSA2                                   
231600     MOVE '  GE' TO GODK-STATUSKODER                                      
231700     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA-WDF502 SSA1   SSA2        
231800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
231900     PERFORM IMS-STATUSKONTROLL                                           
232000     .                                                                    
232100     SKIP3                                                                
232200 IMS-GU-KVAE01-CSEQ SECTION.                                              
232300     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
232400          DELIMITED BY SIZE INTO SSA1                                     
232500     MOVE '  GE' TO GODK-STATUSKODER                                      
232600     CALL CBLTDLI USING GU KVAE1-PCB DLI-IO-AREA-KVAE01 SSA1              
232700     MOVE KVAE1-STATUS-CODE TO STATUS-WS                                  
232800     PERFORM IMS-STATUSKONTROLL                                           
232900     .                                                                    
233000     SKIP3                                                                
233100 IMS-GN-KVAE01-CSEQ SECTION.                                              
233200     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
233300          DELIMITED BY SIZE INTO SSA1                                     
233400     MOVE '  GE' TO GODK-STATUSKODER                                      
233500     CALL CBLTDLI USING GN KVAE1-PCB DLI-IO-AREA-KVAE01 SSA1              
233600     MOVE KVAE1-STATUS-CODE TO STATUS-WS                                  
233700     PERFORM IMS-STATUSKONTROLL                                           
233800     .                                                                    
233900     EJECT                                                                
234000 IMS-GHU-KVAE01 SECTION.                                                  
234100     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
234200          DELIMITED BY SIZE INTO SSA1                                     
234300     MOVE '  GE' TO GODK-STATUSKODER                                      
234400     CALL CBLTDLI USING GHU KVAE2-PCB DLI-IO-AREA-KVAE01 SSA1             
234500     MOVE KVAE2-STATUS-CODE TO STATUS-WS                                  
234600     PERFORM IMS-STATUSKONTROLL                                           
234700     .                                                                    
234800     SKIP3                                                                
234900 IMS-REPL-KVAE01 SECTION.                                                 
235000                                                                          
235100     MOVE '  ' TO GODK-STATUSKODER                                        
235200     CALL CBLTDLI USING REPL KVAE2-PCB DLI-IO-AREA-KVAE01                 
235300     MOVE KVAE2-STATUS-CODE TO STATUS-WS                                  
235400     PERFORM IMS-STATUSKONTROLL                                           
235500     .                                                                    
235600     SKIP3                                                                
235700 IMS-GU-WDB601    SECTION.                                                
235800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
235900          DELIMITED BY SIZE INTO SSA1                                     
236000     MOVE '  GE' TO GODK-STATUSKODER                                      
236100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
236200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
236300     PERFORM IMS-STATUSKONTROLL                                           
236400     IF SEGMENT-SAKNAS                                                    
236500         MOVE SPACE TO DCS-KDDC                                           
236600     END-IF                                                               
236700     .                                                                    
236800 IMS-STATUSKONTROLL SECTION.                                              
236900                                                                          
237000     SET STATUS-IX TO 1                                                   
237100     SEARCH GODK-STATUS                                                   
237200       AT END                                                             
237300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
237400         DELIMITED BY SIZE INTO FELTEXT                                   
237500         CALL FELLOG                                                      
237600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
237700         CONTINUE                                                         
237800     END-SEARCH                                                           
237900     .                                                                    
