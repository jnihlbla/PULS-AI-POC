000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011810.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/09/10 (REWRITTEN DEC 2011).                           
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    NOTE: THIS PROGRAM IS CALLED FROM "DRIVER" PROGRAMS                  
000800*          TAKING CARE OF DIFFERENT TECHNICAL DETAILS                     
000900*          DEPENDING ON HOW THE TRANSACTION WAS STARTED.                  
001000*          ONE DRIVER EXIST FOR "CLASSICAL" INVOCATION                    
001100*          VIA 3270 SCREEN - W6011800, AND ONE FOR INVOCATION             
001200*          FROM THE WEB - W6W11800.                                       
001300*                                                                         
001400*    FUNKTION:                                                            
001500*        BACKA DEL AV INLAGT PARTI                                        
001600*                                                                         
001700*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001800*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001900*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002000*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
002100*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
002200*        PROGRAMMET LOGGAR SALDO FÖRÄNDRINGAR PÅ WLLOGA01 (WDL9)          
002300*                                                                         
002400*    INDATA.                                                              
002500*        REQUEST:     W60118I1                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        RESPONSE     W60118O1                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(08)   VALUE 'W6011810'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
004400                                                                          
004500*    --- WORK FIELD FOR MOVING SPACE TO ANY KIND OF FIELD                 
004600 01  W-SPACE.                                                             
004700     03 FILLER                   PIC X(50)   VALUE SPACE.                 
004800                                                                          
004900*    --- WORK FIELD FOR MOVING ALL + TO ANY KIND OF FIELD                 
005000 01  W-PLUS.                                                              
005100     03 FILLER                   PIC X(50)   VALUE                        
005200        '++++++++++++++++++++++++++++++++++++++++++++++++++'.             
005300                                                                          
005400*01  W-UNICODE-PLUS.                                                      
005500*    03 FILLER                   PIC X(50)   VALUE                        
005600*    X'2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2        
005700*      B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B'.                        
005800                                                                          
005900 77  W-KVRADER                   PIC S9(4)  COMP.                         
006000                                                                          
006100 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
006200 77  P-TO-P-PREFIX-LNG           PIC S9(4)   VALUE +17  COMP SYNC.        
006300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006500 77  INDX-DISPLAY                PIC 9(4).                                
006600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006700 77  CD-IX                       PIC  9(1)  VALUE ZERO.                   
006800                                                                          
006900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007000 77  WS-IDLOPNRM                 PIC X(8)    VALUE SPACE.                 
007100                                                                          
007200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007300     88  INDATA-OK                           VALUE 'J'.                   
007400     88  INDATA-FEL                          VALUE 'N'.                   
007500                                                                          
007600 77  SW-ADINLOMR                 PIC X       VALUE 'J'.                   
007700     88  ADINLOMR-ANGETT                     VALUE 'J'.                   
007800     88  ADINLOMR-EJ-ANGETT                  VALUE 'N'.                   
007900                                                                          
008000 77  KOPPLINGS-SW                PIC X       VALUE 'J'.                   
008100     88  KOPPLING                            VALUE 'J'.                   
008200                                                                          
008300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008400     88  NYCKLAR-OK                          VALUE 'J'.                   
008500     88  NYCKLAR-FEL                         VALUE 'N'.                   
008600                                                                          
008700 77  W6202-SW                    PIC X       VALUE 'N'.                   
008800     88  W6202-TRANS                         VALUE 'J'.                   
008900                                                                          
009000 77  WS-PRAVCOST                 PIC S9(7)V9(2).                          
009100 77  WS-ARTC-KDVALISO            PIC X(3)    VALUE SPACE.                 
009200 77  WS-ARTC-PRARTBEL-PR         PIC S9(8)V9(5) VALUE ZERO COMP-3.        
009300 77  WS-ARTC-KDPSLLOC            PIC S9(3)   VALUE ZERO COMP-3.           
009400 77  WS-DAINLEV                  PIC 9(16)   VALUE ZERO.                  
009500 77  WS-DAINLEV2                 PIC S9(16)  VALUE ZERO COMP-3.           
009600 77  WS-DAINLEV3                 PIC 9(8)    VALUE ZERO COMP-3.           
009700 77  WS-DAINLEV4                 PIC 9(6)    VALUE ZERO COMP-3.           
009800 77  WS-IDLEVNR                  PIC  X(5)   VALUE SPACE.                 
009900     EJECT                                                                
010000*      --- VALID IDDC CODES                                               
010100*                                                                         
010200*01    -COPY WWDC99                                                       
010300       EJECT                                                              
010400*    ---DATUMFÄLT + TIDFÄLT                                               
010500                                                                          
010600 01   WLOGG-TID                  PIC S9(9)   VALUE ZERO.                  
010700 01   LOGG-DATUM                 PIC S9(8)   VALUE ZERO.                  
010800 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0.                 
010900 01  WS-PRARTKALKYL              PIC S9(7)V9(5) VALUE +0 COMP-3.          
011000 01  WS-PRKURS                   PIC S9(5)V9(5) VALUE +0   COMP-3.        
011100 01  WS-REVALUTA                 PIC S9(5)      VALUE +0   COMP-3.        
011200 01  W-REVALUTA                 PIC S9(5)      VALUE +0   COMP-3.         
011300 01  SPAR-PRKURS                 PIC S9(5)V9(5) VALUE +0   COMP-3.        
011400                                                                          
011500 01  WS-DAT.                                                              
011600*     -- DAAVIDAT TILL SAP                                                
011700  02     WS-DAAVIDAT             PIC 9(8)    VALUE ZERO.                  
011800  02     FILLER REDEFINES WS-DAAVIDAT.                                    
011900   03    WS-DAAVIDAT-SEKEL       PIC 9(2).                                
012000   03    WS-DAAVIDAT-YYMMDD      PIC 9(6).                                
012100                                                                          
012200   03  WS-DATE-YYMMDD            PIC 9(06).                               
012300   03  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                            
012400       05  WS-DATE-YYMM          PIC 9(04).                               
012500       05  WS-DATE-DD            PIC 9(02).                               
012600                                                                          
012700     EJECT                                                                
012800                                                                          
012900*    --- ALLMÄNNA ARBETSFÄLT                                              
013000 01      FILLER               PIC X(16) VALUE 'WS**************'.         
013100 01      WS.                                                              
013200                                                                          
013300  02     WS-BACK-KDINLSTA        PIC X(3)    VALUE SPACE.                 
013400   88    WS-BACK-KDINLSTA-OK                 VALUE                        
013500         'INL', 'VOR', 'ANT', 'AVV', 'KVA', 'RET', 'TRP'.                 
013600   88    WS-BACK-KDINLSTA-INLAGD             VALUE                        
013700         'INL', 'VOR'.                                                    
013800   88    WS-BACK-KDINLSTA-AVVIK              VALUE                        
013900         'ANT', 'AVV', 'KVA', 'RET' 'TRP'.                                
014000                                                                          
014100  02     WS-BACK-INL-SW          PIC X(1)    VALUE SPACE.                 
014200   88    WS-BACK-INL                         VALUE 'J'.                   
014300                                                                          
014400  02     WS-BACK-AVV-SW          PIC X(1)    VALUE SPACE.                 
014500   88    WS-BACK-AVV                         VALUE 'J'.                   
014600                                                                          
014700*        -- BACKAD KVANT PER RAD                                          
014800  02     WS-BACK-KVINLART        PIC S9(7)   VALUE ZERO COMP-3.           
014900*        -- BACKAD KVANT TOTALT                                           
015000  02     WS-BACK-TOT-KVINLART    PIC S9(7)   VALUE ZERO COMP-3.           
015100*        -- BACKAD KVANT SOM BOKAT SVS                                    
015200  02     WS-BACK-SVS-KVINLART    PIC S9(7)   VALUE ZERO COMP-3.           
015300*        -- BACKAD KVANT SOM BOKAT CD                                     
015400  02     WS-BACK-CD OCCURS 4.                                             
015500    03     WS-BACK-CD-KVINLART   PIC S9(7)   VALUE ZERO COMP-3.           
015600*        -- FRÅN INLA-RAD 1                                               
015700  02     WS-INLA-RAD1-ADINLOMR   PIC X(4)    VALUE SPACE.                 
015800  02     WS-INLA-RAD1-KVINLART   PIC S9(7)   VALUE ZERO COMP-3.           
015900*        -- FRÅN INLA-ART                                                 
016000  02     WS-INLA-ART-BEFT        PIC S9(3)   VALUE ZERO COMP-3.           
016100  02     WS-INLA-ART-FLKLAR      PIC X(1)    VALUE SPACE.                 
016200  02     WS-INLA-ART-KDRT        PIC S9(3)   VALUE ZERO COMP-3.           
016300  02     WS-INLA-ART-KVAVIS      PIC S9(7)   VALUE ZERO COMP-3.           
016400  02     WS-INLA-ART-ADTRDEST    PIC X(3)    VALUE SPACE.                 
016500*        -- KVRAPP I BILD-HUVUD                                           
016600  02     WS-KVRAPP               PIC S9(7)   VALUE ZERO COMP-3.           
016700*        -- UPPRÄKNINGSFÄLT FÖR KONTROLL MOT KR                           
016800  02     SPAR-KVINLART           PIC S9(7)   VALUE ZERO COMP-3.           
016900                                                                          
017000*        -- FÖR REDIGERING AV INDATA                                      
017100  02     WS-IDRADNR-X            PIC X(4)    VALUE SPACE.                 
017200  02     WS-IDRADNR-N            REDEFINES WS-IDRADNR-X                   
017300                                 PIC 9(4).                                
017400  02     USER-X.                                                          
017500    03   FILLER                  PIC X(2).                                
017600    03   USER                    PIC X(5).                                
017700    03   FILLER                  PIC X(1).                                
017800     SKIP3                                                                
017900 01      FILLER               PIC X(16) VALUE 'SW-SWITCHAR*****'.         
018000 01      SW-SWITCHAR.                                                     
018100                                                                          
018200  02     SW-INLA-RAD1-FINNS      PIC X(1)    VALUE SPACE.                 
018300     EJECT                                                                
018400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018500 01  GENERELLA-SUBPROGRAM.                                                
018600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018800     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
018900     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
019000     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
019100                                                                          
019200     SKIP3                                                                
019300 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
019400*01  -COPY WTRAUTF8                                                       
019500                                                                          
019600 01  WS-CP-UNICODE               PIC X(4)  VALUE 'UTF8'.                  
019700 01  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
019800                                                                          
019900     EJECT                                                                
020000*01  MESSAGE-CODES.                                                       
020100*    03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
020200*    03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020300*    03  ERR-007-OTILLATEN-UPPD  PIC X(3)    VALUE '007'.                 
020400*    03  ERR-010-NOT-IN-REG      PIC X(3)    VALUE '010'.                 
020500*    03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
020600*    03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020700*    03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
020800*    03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020900*    03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021000*    03  ERR-MAN-CHG-NEEDED      PIC X(3)    VALUE '232'.                 
021100*    03  ERR-ALREADY-REGISTRED   PIC X(3)    VALUE '165'.                 
021200*    03  ERR-REMAINS-ADM-REP     PIC X(3)    VALUE '293'.                 
021300                                                                          
021400 01  RESPONSE-CODES.                                                      
021500     03  WERR-CORR-MARKED-FLDS   PIC X(3)    VALUE '020'.                 
021600     03  WINF-PRESS-EXEC         PIC X(3)    VALUE '276'.                 
021700     03  WERR-INVALID-UPDATE     PIC X(3)    VALUE '007'.                 
021800     03  WERR-NOT-FOUND          PIC X(3)    VALUE '025'.                 
021900     03  WERR-EXEC-AND-NO-DATA   PIC X(3)    VALUE '014'.                 
022000     03  WINF-UPDATE-DONE        PIC X(3)    VALUE '001'.                 
022100     03  WINF-FIRST-PAGE         PIC X(3)    VALUE '010'.                 
022200     03  WINF-MORE-INFO-EXISTS   PIC X(3)    VALUE '011'.                 
022300     03  WERR-WRONG-KEY          PIC X(3)    VALUE '022'.                 
022400     03  WERR-MAN-CHG-NEEDED     PIC X(3)    VALUE '335'.                 
022500     03  WERR-ALREADY-REGISTRED  PIC X(3)    VALUE '030'.                 
022600     03  WERR-REMAINS-ADM-REP    PIC X(3)    VALUE '348'.                 
022700                                                                          
022800 01  W-IDMSG-ERROR               PIC X(3)    VALUE SPACE.                 
022900     EJECT                                                                
023000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023100 01  FILLER                      PIC X(24)   VALUE                        
023200                                 '6202-MID-W60202I1'.                     
023300 01  6202-AREA.                                                           
023400     SKIP2                                                                
023500     03 -COPY WREQUPRE -PRE 6202-                                         
023600     03 -COPY WZ01REQU -PRE 6202-                                         
023700     03 -COPY W60202I1 -PRE 6202-                                         
023800     EJECT                                                                
023900                                                                          
024000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024100     SKIP3                                                                
024200*01  -COPY WMFSAREA                                                       
024300 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
024400     EJECT                                                                
024500*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
024600     EJECT                                                                
024700*    --- AREA FÖR W510AVG                                                 
024800 01  FILLER                    PIC X(16) VALUE 'W510AVGAREA*****'.        
024900*01  -COPY W510AVG                                                        
025000*    --- AREA FÖR W510CURR                                                
025100*01  -COPY W510CURR                                                       
025200     EJECT                                                                
025300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025400*                                                                         
025500     SKIP3                                                                
025600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025700     SKIP3                                                                
025800 01  NYCKLAR-TILL-DLI.                                                    
025900     03  W-W6D1BSEQ-X.                                                    
026000         05  W-W6D1BSEQ-IDLOPNRM PIC S9(9)   VALUE ZERO COMP-3.           
026100                                                                          
026200     03  W-IDDC-X.                                                        
026300         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
026400                                                                          
026500     03  W-IDLOPNRM-X.                                                    
026600         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
026700                                                                          
026800     03  W-IDRADNR-X.                                                     
026900         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
027000                                                                          
027100     03  W-IDARTNR-X.                                                     
027200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027300                                                                          
027400     03  W-DAINLEV-X.                                                     
027500         05  W-DAINLEV           PIC 9(16)  VALUE ZERO.                   
027600                                                                          
027700     03  W-W6GXKEY-6005-X.                                                
027800         05  FILLER              PIC X(4)    VALUE '6005'.                
027900         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
028000         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
028100                                                                          
028200     03  W-W6GXKEY-6006-X.                                                
028300         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
028400         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
028500                                                                          
028600     03  W-W6H7CSEQ-X.                                                    
028700         05  W-IDLOPNRM-H7       PIC S9(9)   VALUE ZERO COMP-3.           
028800         05  W-DAAVSDAT-H7       PIC  9(8)   VALUE ZERO.                  
028900                                                                          
029000     03  W-IDSKYLT-X.                                                     
029100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
029200                                                                          
029300     03  W-DAPRLIST-K7-N.                                                 
029400         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
029500                                                                          
029600     03  W-IDLEVNR-PR-X.                                                  
029700         05  W-IDLEVNR-PR        PIC X(5)    VALUE ZERO.                  
029800                                                                          
029900     03  W-IDLEVNR-X.                                                     
030000         05  W-IDLEVNR           PIC  X(5)    VALUE SPACE.                
030100                                                                          
030200     03  W-IDLAND-X.                                                      
030300         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
030400                                                                          
030500     03  W-IDLANDX2-X.                                                    
030600         05    W-IDLANDX2        PIC X(2)    VALUE SPACE.                 
030700                                                                          
030800     03  W-WDGX9305-X.                                                    
030900         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
031000         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
031100         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
031200         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
031300     03  W-KDVALISO-X.                                                    
031400         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
031500     03  W-TISTADA9-X.                                                    
031600         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
031700                                                                          
031800 01  WS-IDSKYLT-SE               PIC X(3) VALUE 'S  '.                    
031900 01  WS-IDSKYLT-GB               PIC X(3) VALUE 'GB '.                    
032000 01  WS-IDSKYLT-CN               PIC X(3) VALUE 'RCN'.                    
032100     SKIP2                                                                
032200*    --- STATUS-KOD FRÅN IMS                                              
032300 01  STATUS-WS                   PIC XX.                                  
032400     88  SEGMENT-FINNS                       VALUE '  '.                  
032500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032700     SKIP2                                                                
032800 01  GODK-STATUSKODER.                                                    
032900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033000     SKIP3                                                                
033100 01  SSA1                        PIC X(96).                               
033200 01  SSA2                        PIC X(64).                               
033300 01  SSA3                        PIC X(64).                               
033400     EJECT                                                                
033500*    --- IMS FUNKTIONSKODER                                               
033600*01  -COPY W0003                                                          
033700     EJECT                                                                
033800*    ---  DLI INPUT-OUTPUT AREA                                           
033900 01  FILLER                   PIC X(16) VALUE 'DLI-IO-W6INLA11'.          
034000 01  DLI-IO-W6INLA11.                                                     
034100*    03  -COPY W6D111 -PRE INLA-                                          
034200     EJECT                                                                
034300                                                                          
034400 01  FILLER                   PIC X(16) VALUE 'DLI-IO-W6INLA21'.          
034500 01  DLI-IO-W6INLA21.                                                     
034600*    03  -COPY W6D121 -PRE INLA-                                          
034700     EJECT                                                                
034800                                                                          
034900 01  FILLER                PIC X(16) VALUE 'DLI-IO-W6PLAA11'.             
035000 01  DLI-IO-W6PLAA11.                                                     
035100*    03  -COPY W6GX6006 -PRE PLAA-                                        
035200     EJECT                                                                
035300                                                                          
035400 01  FILLER                PIC X(16) VALUE 'DLI-IO-WLARTC11'.             
035500 01  DLI-IO-WLARTC11.                                                     
035600*    03  -COPY WDK611  -PRE ARTC-                                         
035700     EJECT                                                                
035800                                                                          
035900 01  FILLER                PIC X(16) VALUE 'DLI-IO-WLARTS11'.             
036000 01  DLI-IO-WLARTS11.                                                     
036100*    03  -COPY WDK711  -PRE ARTS-                                         
036200     EJECT                                                                
036300                                                                          
036400 01  FILLER                PIC X(16) VALUE 'DLI-IO-WLINLE21'.             
036500 01  DLI-IO-WLINLE21.                                                     
036600*    03  -COPY WDL221  -PRE INLE-                                         
036700     EJECT                                                                
036800                                                                          
036900 01  FILLER                PIC X(16) VALUE 'DLI-IO-WLINLE31'.             
037000 01  DLI-IO-WLINLE31.                                                     
037100*    03  -COPY WDL231  -PRE INLE-                                         
037200     EJECT                                                                
037300                                                                          
037400 01  FILLER                PIC X(16) VALUE 'DLI-IO-W6KVAE01'.             
037500 01  DLI-IO-W6KVAE01.                                                     
037600*    03  -COPY W6H701  -PRE KVAE-                                         
037700     EJECT                                                                
037800                                                                          
037900     EJECT                                                                
038000 01  FILLER                PIC X(16) VALUE 'DLI-IO-W6INLC01'.             
038100 01  DLI-IO-W6INLC01.                                                     
038200*    03  -COPY W6D1B1                                                     
038300     EJECT                                                                
038400                                                                          
038500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDL601'.        
038600 01  DLI-IO-AREA-WDL601.                                                  
038700*    03  -COPY WDL601 -PRE INLC-                                          
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-INCL11'.        
039000 01  DLI-IO-AREA-WDL611.                                                  
039100*    03  -COPY WDL611 -PRE INLC-                                          
039200     EJECT                                                                
039300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-INCL11'.        
039400 01  DLI-IO-AREA-WDL621.                                                  
039500*    03  -COPY WDL621 -PRE INLC-                                          
039600     EJECT                                                                
039700 01  DLI-IO-AREA-UPFA01.                                                  
039800     03  W6UPFA01.                                                        
039900*        05  -COPY W6L101                                                 
040000     EJECT                                                                
040100 01  FILLER                      PIC X(16)  VALUE 'WLLOGA01'.             
040200*01  WLLOGA01 -COPY WDL901                                                
040300     EJECT                                                                
040400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD311'.         
040500 01  DLI-IO-WDD311.                                                       
040600*    03  -COPY WDD311                                                     
040700                                                                          
040800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK724'.         
040900 01  DLI-IO-WDK724.                                                       
041000*    03  -COPY WDK724                                                     
041100     EJECT                                                                
041200                                                                          
041300 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDB601  '.               
041400 01  DLI-IO-WDB601.                                                       
041500*    03  WDB601    -COPY WDB601                                           
041600     EJECT                                                                
041700                                                                          
041800 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
041900 01   DLI-IO-AREA-B617.                                                   
042000*     03  -COPY WDB617                                                    
042100     EJECT                                                                
042200                                                                          
042300 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
042400*01  WLLEVA01 -COPY WDF101                                                
042500     EJECT                                                                
042600                                                                          
042700 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
042800*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
042900     EJECT                                                                
043000                                                                          
043100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
043200 01  DLI-IO-WDGX9306.                                                     
043300*    03  -COPY WDGX9306                                                   
043400     EJECT                                                                
043500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
043600 01  DLI-IO-WDGX9308.                                                     
043700*    03  -COPY WDGX9308                                                   
043800                                                                          
043900 LINKAGE SECTION.                                                         
044000                                                                          
044100 01  REQU-AREA.                                                           
044200*    03 -COPY WZ01REQU                                                    
044300*    03 -COPY W60118I1                                                    
044400                                                                          
044500 01  RESP-AREA.                                                           
044600*    03 -COPY WZ01RESP                                                    
044700*    03 -COPY W60118O1                                                    
044800                                                                          
044900 01  MAX-KVRADER                PIC S9(4)   COMP.                         
045000                                                                          
045100*01  -COPY W0009  -PRE MSG-                                               
045200     EJECT                                                                
045300*01  -COPY W0009  -PRE 6202-                                              
045400     EJECT                                                                
045800*01  -COPY W0008  -PRE INLA-                                              
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008  -PRE SEQB-                                              
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400*01  -COPY W0008  -PRE PLAA-                                              
046500     05  FILLER                  PIC X.                                   
046600     EJECT                                                                
046700*01  -COPY W0008  -PRE ARTC-                                              
046800     05  FILLER                  PIC X.                                   
046900     EJECT                                                                
047000*01  -COPY W0008  -PRE INLE-                                              
047100     05  FILLER                  PIC X.                                   
047200     EJECT                                                                
047300*01  -COPY W0008  -PRE KVAE-                                              
047400     05  FILLER                  PIC X.                                   
047500     EJECT                                                                
047600*01  -COPY W0008  -PRE WDK7-                                              
047700     05  FILLER                  PIC X.                                   
047800*01  -COPY W0008  -PRE WDL6-                                              
047900     05  FILLER                  PIC X.                                   
048000     EJECT                                                                
048100*01  -COPY W0008  -PRE LOGA-                                              
048200     05  FILLER                  PIC X.                                   
048300     EJECT                                                                
048400*01  -COPY W0008  -PRE UPFA-                                              
048500     05  FILLER                  PIC X.                                   
048600     EJECT                                                                
048700*01  -COPY W0008  -PRE WDD3-                                              
048800     05  FILLER                  PIC X.                                   
048900     EJECT                                                                
049000*01  -COPY W0008  -PRE 9305-AVG-                                          
050000     05  FILLER                  PIC X.                                   
050100     EJECT                                                                
050200*01  -COPY W0008  -PRE AVG-WDB6-                                          
050300     05  FILLER                  PIC X.                                   
050400     EJECT                                                                
050500*01  -COPY W0008  -PRE WDB6-                                              
050600     05  FILLER                  PIC X.                                   
050700     EJECT                                                                
050800*01  -COPY W0008 -PRE LEV-                                                
050900     05  FILLER                  PIC X(5).                                
051000     EJECT                                                                
051100*01  -COPY W0008  -PRE 9305-                                              
051200     05  FILLER                  PIC X.                                   
051300     EJECT                                                                
051400 PROCEDURE DIVISION  USING REQU-AREA  RESP-AREA MAX-KVRADER               
051500                           MSG-PCB 6202-PCB          INLA-PCB             
051600                           SEQB-PCB PLAA-PCB                              
051700                           ARTC-PCB INLE-PCB KVAE-PCB WDK7-PCB            
051800                           WDL6-PCB LOGA-PCB UPFA-PCB                     
051900                           WDD3-PCB 9305-AVG-PCB                          
052000                           AVG-WDB6-PCB                                   
052100                           WDB6-PCB LEV-PCB 9305-PCB.                     
052200                                                                          
052300     PERFORM A-INIT                                                       
052400     PERFORM B-KOLLA-NYCKLAR                                              
052500     IF NYCKLAR-OK                                                        
052600       IF REQU-UPDATE                                                     
052700         PERFORM G-KOLLA-INPUT                                            
052800         IF INDATA-OK                                                     
052900           PERFORM H-UPPDATERA                                            
053000         END-IF                                                           
053100       ELSE                                                               
053200         IF REQU-FIRST                                                    
053300           PERFORM C-FOERSTA-SIDA                                         
053400         ELSE                                                             
053500           IF REQU-NEXT                                                   
053600             PERFORM D-NAESTA-SIDA                                        
053700           ELSE                                                           
053800             PERFORM E-SAMMA-SIDA                                         
053900           END-IF                                                         
054000         END-IF                                                           
054100       END-IF                                                             
054200                                                                          
054300       IF INDATA-OK                                                       
054400         PERFORM F-LAES-VISA-INFO                                         
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800     MOVE ZERO TO RETURN-CODE                                             
054900     GOBACK                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 A-INIT          SECTION.                                                 
055300                                                                          
055400     MOVE SPACE              TO RESP-WZ01RESP                             
055500     MOVE 001                TO RESP-IDMSGVER                             
055600     IF REQU-KVRADER NOT NUMERIC                                          
055700       MOVE ZERO             TO REQU-KVRADER                              
055800     END-IF                                                               
055900                                                                          
056000     MOVE ALL '+'            TO RESP-W60118O1                             
056100     MOVE REQU-IDRADNR-START TO RESP-IDRADNR-START                        
056200                                RESP-IDRADNR-NEXT                         
056300*    -- SET VALID ATTRIBUTE VALUES (DESTROYED ABOVE)                      
056400     MOVE MAX-KVRADER        TO RESP-KVRADER                              
056500     PERFORM RESP-FORM-ATTR                                               
056600*    -- PROBABLY BETTER WITH ZERO THAN MAX                                
056700     MOVE ZERO               TO RESP-KVRADER                              
056800     .                                                                    
056900                                                                          
057000     EJECT                                                                
057100 B-KOLLA-NYCKLAR SECTION.                                                 
057200                                                                          
057300     MOVE JA TO NYCKLAR-SW                                                
057400                                                                          
057500     PERFORM BA-KONTROLL-AV-IDLOPNRM                                      
057600     PERFORM BB-KONTROLL-AV-IDDC                                          
057700                                                                          
057800     IF NYCKLAR-OK                                                        
057900       MOVE WS-IDLOPNRM     TO W-IDLOPNRM                                 
058000       MOVE WS-IDDC         TO W-IDDC                                     
058100                               W-6005-IDDC                                
058200     ELSE                                                                 
058300       PERFORM RESP-RENSA-FAELT-UT                                        
058400       PERFORM RESP-RENSA-FAELT-IN                                        
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800 BA-KONTROLL-AV-IDLOPNRM   SECTION.                                       
058900                                                                          
059000     MOVE REQU-IDLOPNRM-KEY TO WS-IDLOPNRM                                
059100     IF REQU-IDLOPNRM-KEY = ALL '+'                                       
059200       MOVE ZERO TO WS-IDLOPNRM                                           
059300     END-IF                                                               
059400     INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                  
059500                                                                          
059600     IF WS-IDLOPNRM NUMERIC AND WS-IDLOPNRM > ZERO                        
059700       CONTINUE                                                           
059800     ELSE                                                                 
059900       MOVE NEJ                 TO NYCKLAR-SW                             
060000       MOVE WERR-WRONG-KEY      TO RESP-IDMSG-ERROR                       
060100       MOVE 'IDLOPNRM'          TO RESP-IDELMT-ERROR                      
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 BB-KONTROLL-AV-IDDC    SECTION.                                          
060600                                                                          
060700     MOVE REQU-IDDC-KEY   TO WS-IDDC                                      
060800                                                                          
060900*NDC                                                                      
061000     IF CDC OR NDC                                                        
061100       CONTINUE                                                           
061200     ELSE                                                                 
061300       MOVE WERR-WRONG-KEY TO RESP-IDMSG-ERROR                            
061400       MOVE 'IDDC'         TO RESP-IDELMT-ERROR                           
061500       MOVE    NEJ         TO NYCKLAR-SW                                  
061600       MOVE   SPACE        TO WS-IDDC                                     
061700     END-IF                                                               
061800     .                                                                    
061900     EJECT                                                                
062000 C-FOERSTA-SIDA SECTION.                                                  
062100                                                                          
062200     MOVE WINF-FIRST-PAGE TO RESP-IDMSG-INFO                              
062300                                                                          
062400*    --- USE START VALUE (ZERO) FROM CALLER                               
062500     MOVE REQU-IDRADNR-START TO W-IDRADNR                                 
062600*    --- CLEAR INPUT IN SCROLLABLE LINES                                  
062700     PERFORM RESP-RENSA-FAELT-IN                                          
062800                                                                          
062900     .                                                                    
063000     EJECT                                                                
063100 D-NAESTA-SIDA SECTION.                                                   
063200                                                                          
063300*    --- USE START VALUE FROM CALLER                                      
063400     MOVE REQU-IDRADNR-START TO W-IDRADNR                                 
063500     .                                                                    
063600     EJECT                                                                
063700 E-SAMMA-SIDA SECTION.                                                    
063800                                                                          
063900*    --- USE START VALUE FROM CALLER                                      
064000     MOVE REQU-IDRADNR-START TO W-IDRADNR                                 
064100                                                                          
064200     PERFORM S20-KOLLA-ADINLOMR                                           
064300                                                                          
064400     IF ADINLOMR-EJ-ANGETT                                                
064500       PERFORM RESP-RENSA-FAELT-IN                                        
064600     ELSE                                                                 
064700       MOVE WINF-PRESS-EXEC TO RESP-IDMSG-ERROR                           
064800       PERFORM EA-REQU-INDATA-TILL-RESP                                   
064900     END-IF                                                               
065000                                                                          
065100     MOVE REQU-KVRADER TO RESP-KVRADER                                    
065200     .                                                                    
065300     EJECT                                                                
065400 EA-REQU-INDATA-TILL-RESP SECTION.                                        
065500                                                                          
065600     MOVE +1   TO INDX                                                    
065700     PERFORM UNTIL INDX > REQU-KVRADER                                    
065800                                                                          
065900       IF REQU-ADINLOMR-UPD-LINE (INDX) NOT = ALL '+'                     
066000         MOVE MFS-ADD-LAES-IN-FAELT                                       
066100           TO RESP-ADINLOMR-UPD-LINE-ATTR (INDX)                          
066200       END-IF                                                             
066300       MOVE REQU-ADINLOMR-UPD-LINE (INDX)                                 
066400         TO RESP-ADINLOMR-UPD-LINE (INDX)                                 
066500                                                                          
066600       ADD +1  TO INDX                                                    
066700     END-PERFORM                                                          
066800     .                                                                    
066900     EJECT                                                                
067000 F-LAES-VISA-INFO SECTION.                                                
067100                                                                          
067200     PERFORM FA-LAES-GRUNDDATA                                            
067300                                                                          
067400     IF INDATA-OK                                                         
067500       MOVE +1 TO INDX                                                    
067600       PERFORM IMS-GNP-INLA-RAD-F-GQ                                      
067700       IF SEGMENT-FINNS                                                   
067800         MOVE INLA-RAD-IDRADNR TO RESP-IDRADNR-START                      
067900       ELSE                                                               
068000         MOVE ZERO             TO RESP-IDRADNR-START                      
068100       END-IF                                                             
068200                                                                          
068300       PERFORM UNTIL INDX > MAX-KVRADER OR SEGMENT-SAKNAS                 
068400         PERFORM FC-VISA-RADDATA                                          
068500         MOVE INDX TO RESP-KVRADER                                        
068600                                                                          
068700         ADD 1 TO INDX                                                    
068800         PERFORM IMS-GNP-INLA-RAD                                         
068900       END-PERFORM                                                        
069000                                                                          
069100       IF SEGMENT-FINNS                                                   
069200         MOVE INLA-RAD-IDRADNR      TO RESP-IDRADNR-NEXT                  
069300         MOVE WINF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                    
069400       ELSE                                                               
069500         MOVE ZERO                  TO RESP-IDRADNR-NEXT                  
069600       END-IF                                                             
069700                                                                          
069800     ELSE                                                                 
069900*        -- CLEAR ANY "FIRST LINE SHOWN" MESSAGE                          
070000         MOVE SPACE            TO RESP-IDMSG-INFO                         
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 FA-LAES-GRUNDDATA SECTION.                                               
070500                                                                          
070600     PERFORM S10-KTRL-INLA-ART                                            
070700     .                                                                    
070800     EJECT                                                                
070900 FC-VISA-RADDATA SECTION.                                                 
071000                                                                          
071100     MOVE SPACE TO RESP-ADINLOMR-UPD-LINE (INDX)                          
071200                                                                          
071300     MOVE INLA-RAD-KDINLSTA      TO WS-BACK-KDINLSTA                      
071400     IF WS-BACK-KDINLSTA-OK                                               
071500       CONTINUE                                                           
071600     ELSE                                                                 
071700       MOVE MFS-STAENG-FAELT                                              
071800            TO RESP-ADINLOMR-UPD-LINE-ATTR (INDX)                         
071900     END-IF                                                               
072000                                                                          
072100     MOVE INLA-RAD-IDRADNR       TO RESP-IDRADNR-LINE (INDX)              
072200                                                                          
072300     IF WS-BACK-KDINLSTA-AVVIK                                            
072400       COMPUTE RESP-KVINLART-LINE (INDX) = INLA-RAD-KVINLART * -1         
072500     ELSE                                                                 
072600       MOVE INLA-RAD-KVINLART    TO RESP-KVINLART-LINE (INDX)             
072700     END-IF                                                               
072800                                                                          
072900     MOVE INLA-RAD-ADINLOMR      TO RESP-ADINLOMR-LINE (INDX)             
073000     MOVE INLA-RAD-KDINLSTA      TO RESP-KDINLSTA-LINE (INDX)             
073100     MOVE INLA-RAD-IDLEVNR-KOLLI                                          
073200                            TO RESP-IDLEVNR-KOLLI-LINE (INDX)             
073300     MOVE INLA-RAD-IDOKOLLI      TO RESP-IDOKOLLI-LINE(INDX)              
073400                                                                          
073500     IF REQU-IDSPRAK NOT = 'SV'                                           
073600       EVALUATE RESP-KDINLSTA-LINE (INDX)                                 
073700         WHEN 'SAK'  MOVE 'Case missing             '                     
073800                                TO RESP-STATUS-TEXT-LINE(INDX)            
073900                     MOVE 'MIS' TO RESP-KDINLSTA-LINE (INDX)              
074000         WHEN 'KVA'  MOVE 'Quality deviation        '                     
074100                                TO RESP-STATUS-TEXT-LINE (INDX)           
074200                     MOVE 'Q-D' TO RESP-KDINLSTA-LINE (INDX)              
074300         WHEN 'ANT'  MOVE 'Qantity deviation        '                     
074400                                TO RESP-STATUS-TEXT-LINE (INDX)           
074500                     MOVE 'DEV' TO RESP-KDINLSTA-LINE (INDX)              
074600         WHEN 'AVV'  MOVE 'Quantity deviation       '                     
074700                                TO RESP-STATUS-TEXT-LINE (INDX)           
074800                     MOVE 'DEV' TO RESP-KDINLSTA-LINE (INDX)              
074900         WHEN 'FPK'  MOVE 'Packaged                 '                     
075000                                TO RESP-STATUS-TEXT-LINE (INDX)           
075100                     MOVE 'PP ' TO RESP-KDINLSTA-LINE (INDX)              
075200         WHEN 'VOR'  MOVE 'Binned (VOR)             '                     
075300                                TO RESP-STATUS-TEXT-LINE (INDX)           
075400                     MOVE 'VOR' TO RESP-KDINLSTA-LINE (INDX)              
075500         WHEN 'INL'  MOVE 'Binned                   '                     
075600                                TO RESP-STATUS-TEXT-LINE (INDX)           
075700                     MOVE 'BIN' TO RESP-KDINLSTA-LINE (INDX)              
075800         WHEN 'RET'  MOVE 'Returned to supplier     '                     
075900                                TO RESP-STATUS-TEXT-LINE (INDX)           
076000                     MOVE 'RET' TO RESP-KDINLSTA-LINE (INDX)              
076100         WHEN 'FRD'  MOVE 'Split                    '                     
076200                                TO RESP-STATUS-TEXT-LINE (INDX)           
076300                     MOVE 'TRP' TO RESP-KDINLSTA-LINE (INDX)              
076400         WHEN 'MAK'  MOVE 'Cancelled                '                     
076500                                TO RESP-STATUS-TEXT-LINE (INDX)           
076600                     MOVE 'CAN' TO RESP-KDINLSTA-LINE (INDX)              
076700         WHEN 'TRP'  MOVE 'Transport deviation      '                     
076800                                TO RESP-STATUS-TEXT-LINE (INDX)           
076900                     MOVE 'TRP' TO RESP-KDINLSTA-LINE (INDX)              
077000         WHEN OTHER  MOVE SPACE TO RESP-STATUS-TEXT-LINE (INDX)           
077100       END-EVALUATE                                                       
077200     ELSE                                                                 
077300         MOVE SPACE TO RESP-STATUS-TEXT-LINE (INDX)                       
077400     END-IF                                                               
077500     .                                                                    
077600     EJECT                                                                
077700 G-KOLLA-INPUT SECTION.                                                   
077800                                                                          
077900     MOVE SPACE TO RESP-IDMSG-ERROR                                       
078000     MOVE REQU-KVRADER TO RESP-KVRADER                                    
078100                                                                          
078200     PERFORM S20-KOLLA-ADINLOMR                                           
078300     IF ADINLOMR-EJ-ANGETT                                                
078400       MOVE WERR-EXEC-AND-NO-DATA TO RESP-IDMSG-ERROR                     
078500       PERFORM RESP-ROER-EJ-FAELT-UT                                      
078600       PERFORM RESP-ROER-EJ-FAELT-IN                                      
078700       MOVE NEJ TO INDATA-SW                                              
078800     ELSE                                                                 
078900       PERFORM S10-KTRL-INLA-ART                                          
079000                                                                          
079100       IF INDATA-OK                                                       
079200         MOVE +1 TO W-IDRADNR                                             
079300         PERFORM IMS-GNP-INLA-RAD-F-Q                                     
079400         IF SEGMENT-FINNS                                                 
079500           MOVE JA TO SW-INLA-RAD1-FINNS                                  
079600           MOVE INLA-RAD-ADINLOMR TO WS-INLA-RAD1-ADINLOMR                
079700           MOVE INLA-RAD-KVINLART TO WS-INLA-RAD1-KVINLART                
079800         ELSE                                                             
079900           MOVE NEJ TO SW-INLA-RAD1-FINNS                                 
080000         END-IF                                                           
080100                                                                          
080200         MOVE +1 TO INDX                                                  
080300         PERFORM UNTIL INDX > REQU-KVRADER                                
080400           PERFORM GA-KTRL-RAD                                            
080500           ADD +1 TO INDX                                                 
080600         END-PERFORM                                                      
080700                                                                          
080800         IF INDATA-FEL                                                    
080900           PERFORM RESP-ROER-EJ-FAELT-UT                                  
081000         END-IF                                                           
081100       END-IF                                                             
081200     END-IF                                                               
081300                                                                          
081400     IF INDATA-FEL                                                        
081500       PERFORM GB-SKRIVSKYDDA-RADER                                       
081600     END-IF                                                               
081700     .                                                                    
081800     EJECT                                                                
081900 GA-KTRL-RAD SECTION.                                                     
082000                                                                          
082100     IF  REQU-ADINLOMR-UPD-LINE (INDX) NOT = ALL '+'                      
082200     AND REQU-ADINLOMR-UPD-LINE (INDX) NOT = SPACE                        
082300                                                                          
082400       MOVE REQU-ADINLOMR-UPD-LINE (INDX) TO W-6006-ADINLOMR              
082500       PERFORM IMS-GU-PLAA-6006                                           
082600                                                                          
082700       IF SEGMENT-FINNS                                                   
082800         MOVE REQU-IDRADNR-LINE (INDX) TO WS-IDRADNR-X                    
082900         INSPECT WS-IDRADNR-X REPLACING LEADING SPACE BY ZERO             
083000         MOVE WS-IDRADNR-N        TO W-IDRADNR                            
083100         PERFORM IMS-GNP-INLA-RAD-F-Q                                     
083200                                                                          
083300         IF SEGMENT-FINNS                                                 
083400          MOVE INLA-RAD-KDINLSTA TO WS-BACK-KDINLSTA                      
083500          IF (INLA-RAD-KDINLSTA = 'INL' OR 'VOR') AND CDC-TR              
083600            MOVE WERR-CORR-MARKED-FLDS TO W-IDMSG-ERROR                   
083700            PERFORM GAA-ADINLOMR-UPD-FEL                                  
083800          ELSE                                                            
083900           IF WS-BACK-KDINLSTA-OK                                         
084000                                                                          
084100             IF  ((INLA-RAD-KDINLSTA = 'INL'                              
084200                 AND INLA-RAD-IDOKOLLI = ZERO)                            
084300              OR  INLA-RAD-KDINLSTA = 'VOR' OR 'ANT' OR 'AVV' OR          
084400                                      'KVA' OR 'RET' OR 'TRP')            
084500             AND SW-INLA-RAD1-FINNS = JA                                  
084600             AND WS-INLA-RAD1-ADINLOMR NOT = SPACE                        
084700             AND WS-INLA-RAD1-ADINLOMR                                    
084800                            NOT = REQU-ADINLOMR-UPD-LINE (INDX)           
084900                                                                          
085000*              * ADINLOMR <> INLA-RAD 1 FÖR INL U KOLLI EL VOR            
085100               MOVE WERR-CORR-MARKED-FLDS TO W-IDMSG-ERROR                
085200               PERFORM GAA-ADINLOMR-UPD-FEL                               
085300             ELSE                                                         
085400               IF INLA-RAD-KDINLSTA = 'AVV' OR 'ANT' OR 'TRP'             
085500                 IF INLA-RAD-KVINLART < +0                    AND         
085600                    INLA-RAD-KVINLART * -1 > WS-INLA-RAD1-KVINLART        
085700                   MOVE WERR-CORR-MARKED-FLDS TO W-IDMSG-ERROR            
085800                   PERFORM GAA-ADINLOMR-UPD-FEL                           
085900                 ELSE                                                     
086000*                        * FÄLTET ÄR HELT KORREKT !!!!                    
086100                   MOVE MFS-ALFA-FAELT-RAETT                              
086200                            TO RESP-ADINLOMR-UPD-LINE-ATTR (INDX)         
086300                 END-IF                                                   
086400               ELSE                                                       
086500*                       * FÄLTET ÄR HELT KORREKT !!!!                     
086600                   MOVE MFS-ALFA-FAELT-RAETT                              
086700                            TO RESP-ADINLOMR-UPD-LINE-ATTR (INDX)         
086800               END-IF                                                     
086900             END-IF                                                       
087000           ELSE                                                           
087100*            * EJ BACKNINGSBAR STATUS                                     
087200             MOVE WERR-INVALID-UPDATE TO W-IDMSG-ERROR                    
087300             PERFORM GAA-ADINLOMR-UPD-FEL                                 
087400           END-IF                                                         
087500          END-IF                                                          
087600         ELSE                                                             
087700*          * INLA-RAD SAKNAS                                              
087800             MOVE MFS-ALFA-FAELT-FEL                                      
087900                  TO RESP-ADINLOMR-UPD-LINE-ATTR (INDX)                   
088000             MOVE NEJ TO INDATA-SW                                        
088100             IF RESP-IDMSG-ERROR = SPACE                                  
088200               MOVE WERR-NOT-FOUND TO RESP-IDMSG-ERROR                    
088300               MOVE 'IDRADNR'      TO RESP-IDELMT-ERROR                   
088400             END-IF                                                       
088500         END-IF                                                           
088600       ELSE                                                               
088700*        * ADINLOMR FINNS EJ I PLAA                                       
088800         MOVE WERR-NOT-FOUND   TO W-IDMSG-ERROR                           
088900         PERFORM GAA-ADINLOMR-UPD-FEL                                     
089000       END-IF                                                             
089100     END-IF                                                               
089200     .                                                                    
089300                                                                          
089400 GAA-ADINLOMR-UPD-FEL SECTION.                                            
089500                                                                          
089600     MOVE MFS-ALFA-FAELT-FEL                                              
089700                          TO RESP-ADINLOMR-UPD-LINE-ATTR (INDX)           
089800     MOVE NEJ TO INDATA-SW                                                
089900                                                                          
090000     IF RESP-IDMSG-ERROR = SPACE                                          
090100       MOVE W-IDMSG-ERROR TO RESP-IDMSG-ERROR                             
090200       MOVE INDX          TO INDX-DISPLAY                                 
090300       MOVE SPACE         TO RESP-IDELMT-ERROR                            
090400       STRING 'ADINLOMR-U*' INDX-DISPLAY                                  
090500       DELIMITED BY SIZE  INTO RESP-IDELMT-ERROR                          
090600     END-IF                                                               
090700     .                                                                    
090800     EJECT                                                                
090900 GB-SKRIVSKYDDA-RADER SECTION.                                            
091000                                                                          
091100     MOVE +1 TO INDX                                                      
091200     PERFORM UNTIL INDX > REQU-KVRADER                                    
091300                                                                          
091400       MOVE REQU-KDINLSTA-LINE (INDX)  TO WS-BACK-KDINLSTA                
091500       IF WS-BACK-KDINLSTA-OK                                             
091600         CONTINUE                                                         
091700       ELSE                                                               
091800         MOVE MFS-STAENG-FAELT                                            
091900                      TO RESP-ADINLOMR-UPD-LINE-ATTR(INDX)                
092000       END-IF                                                             
092100                                                                          
092200      ADD +1                     TO INDX                                  
092300     END-PERFORM                                                          
092400     .                                                                    
092500     EJECT                                                                
092600 H-UPPDATERA SECTION.                                                     
092700                                                                          
092800     MOVE ZERO                   TO WS-BACK-TOT-KVINLART                  
092900                                    WS-BACK-SVS-KVINLART                  
093000                                    WS-BACK-CD-KVINLART(1)                
093100                                    WS-BACK-CD-KVINLART(2)                
093200                                    WS-BACK-CD-KVINLART(3)                
093300                                    WS-BACK-CD-KVINLART(4)                
093400                                                                          
093500     MOVE +1 TO INDX                                                      
093600     PERFORM UNTIL INDX > REQU-KVRADER                                    
093700                                                                          
093800       IF  REQU-ADINLOMR-UPD-LINE (INDX) NOT = ALL '+'                    
093900       AND REQU-ADINLOMR-UPD-LINE (INDX) NOT = SPACE                      
094000         PERFORM HA-UPPDAT-RAD                                            
094100       END-IF                                                             
094200                                                                          
094300       ADD +1 TO INDX                                                     
094400     END-PERFORM                                                          
094500                                                                          
094600     IF KOPPLING AND WS-BACK-INL                                          
094700       PERFORM HB-BACKA-KOPPL-TOT                                         
094800     END-IF                                                               
094900                                                                          
095000     IF WS-BACK-AVV                                                       
095100       IF WS-BACK-KDINLSTA = 'TRP'                                        
095200         CONTINUE                                                         
095300       ELSE                                                               
095400*NDC  DET GÖRS INGA KR VID INLEVERANS I NDC:ER. DÄRFÖR SKA INTE           
095500*     KONTROLL GÖRAS MOT KR-BASEN.                                        
095600         IF NDC                                                           
095700           CONTINUE                                                       
095800         ELSE                                                             
095900           PERFORM S12-KOLLA-OM-KR-UPPDATERING                            
096000         END-IF                                                           
096100       END-IF                                                             
096200     END-IF                                                               
096300                                                                          
096400     MOVE WINF-UPDATE-DONE TO RESP-IDMSG-INFO                             
096500     PERFORM RESP-FORM-ATTR                                               
096600     PERFORM RESP-RENSA-FAELT-IN                                          
096700* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
096800     .                                                                    
096900     EJECT                                                                
097000 HA-UPPDAT-RAD SECTION.                                                   
097100                                                                          
097200     MOVE REQU-IDRADNR-LINE (INDX)   TO WS-IDRADNR-X                      
097300     INSPECT WS-IDRADNR-X REPLACING LEADING SPACE BY ZERO                 
097400     MOVE WS-IDRADNR-N               TO W-IDRADNR                         
097500     PERFORM IMS-GHNP-INLA-RAD-F-Q                                        
097600                                                                          
097700     MOVE INLA-RAD-KDINLSTA      TO WS-BACK-KDINLSTA                      
097800     IF WS-BACK-KDINLSTA-INLAGD                                           
097900       MOVE JA TO WS-BACK-INL-SW                                          
098000     ELSE                                                                 
098100       IF WS-BACK-KDINLSTA = 'AVV' OR 'ANT' OR 'TRP'                      
098200         MOVE JA TO WS-BACK-AVV-SW                                        
098300       END-IF                                                             
098400     END-IF                                                               
098500     MOVE INLA-RAD-KVINLART      TO WS-BACK-KVINLART                      
098600                                                                          
098700     IF INLA-RAD-KDINLSTA = 'INL' OR 'VOR'                                
098800       ADD INLA-RAD-KVINLART       TO WS-BACK-TOT-KVINLART                
098900       IF INLA-RAD-FLSVSLS = JA                                           
099000         ADD INLA-RAD-KVINLART     TO WS-BACK-SVS-KVINLART                
099100       END-IF                                                             
099200       IF WS-INLA-ART-ADTRDEST(1:2) = 'CD'                                
099300         MOVE WS-INLA-ART-ADTRDEST(3:1) TO CD-IX                          
099400         ADD INLA-RAD-KVINLART     TO WS-BACK-CD-KVINLART(CD-IX)          
099500       END-IF                                                             
099600     END-IF                                                               
099700                                                                          
099800     IF (INLA-RAD-KDINLSTA = 'INL' OR 'TRP')                              
099900     AND INLA-RAD-IDOKOLLI > ZERO                                         
100000*      -- BACKA INOM AKTUELL RAD                                          
100100                                                                          
100200       MOVE ZERO                 TO INLA-RAD-TIUPPDAT                     
100300       MOVE REQU-ADINLOMR-UPD-LINE (INDX) TO INLA-RAD-ADINLOMR            
100400       MOVE SPACE                TO INLA-RAD-ADINLOMR-NXT                 
100500                                                                          
100600       IF WS-INLA-ART-BEFT > ZERO                                         
100700         IF WS-INLA-ART-BEFT > +69 AND WS-INLA-ART-BEFT < +80             
100800***LEVERANTÖRSFÖRPACKAT                                                   
100900           MOVE SPACE              TO INLA-RAD-KDINLSTA                   
101000         ELSE                                                             
101100           MOVE 'FPK'              TO INLA-RAD-KDINLSTA                   
101200         END-IF                                                           
101300       ELSE                                                               
101400         MOVE SPACE              TO INLA-RAD-KDINLSTA                     
101500       END-IF                                                             
101600                                                                          
101700       PERFORM IMS-REPL-INLA                                              
101800                                                                          
101900     ELSE                                                                 
102000*      -- BACKA TILL RAD 1                                                
102100                                                                          
102200       PERFORM IMS-DLET-INLA                                              
102300                                                                          
102400       IF SW-INLA-RAD1-FINNS = JA                                         
102500                                                                          
102600         MOVE +1                 TO W-IDRADNR                             
102700         PERFORM IMS-GHNP-INLA-RAD-F-Q                                    
102800                                                                          
102900         ADD WS-BACK-KVINLART    TO INLA-RAD-KVINLART                     
103000         MOVE REQU-ADINLOMR-UPD-LINE (INDX) TO INLA-RAD-ADINLOMR          
103100                                                                          
103200         IF INLA-RAD-KVINLART = +0                                        
103300           PERFORM IMS-DLET-INLA                                          
103400           MOVE NEJ TO SW-INLA-RAD1-FINNS                                 
103500         ELSE                                                             
103600           PERFORM IMS-REPL-INLA                                          
103700         END-IF                                                           
103800                                                                          
103900       ELSE                                                               
104000         PERFORM HAA-NYUPPL-RAD-1                                         
104100         MOVE JA TO SW-INLA-RAD1-FINNS                                    
104200       END-IF                                                             
104300                                                                          
104400       MOVE WS-BACK-KVINLART TO SPAR-KVINLART                             
104500                                                                          
104600     END-IF                                                               
104700                                                                          
104800     MOVE INLA-RAD-IDRADNR       TO W-IDRADNR                             
104900                                                                          
105000*NDC                                                                      
105100     IF CDC                                                               
105200       PERFORM HAB-BACKA-KOPPL-RAD                                        
105300     ELSE                                                                 
105400       PERFORM HAC-BACKA-WDL6-RAD                                         
105500     END-IF                                                               
105600     .                                                                    
105700     EJECT                                                                
105800 HAA-NYUPPL-RAD-1 SECTION.                                                
105900                                                                          
106000*    -- RAD 1 NYUPPLÄGGES.                                                
106100*       KVAR I IO-AREA FRÅN BORTTAGEN RAD:                                
106200*         FLKVAANT,FLPRIO,FLSATS,KDINLPRIO & KVINLART                     
106300                                                                          
106400     MOVE +1                     TO INLA-RAD-IDRADNR                      
106500     MOVE REQU-ADINLOMR-UPD-LINE (INDX)                                   
106600                                 TO INLA-RAD-ADINLOMR                     
106700     MOVE SPACE                  TO INLA-RAD-ADINLOMR-NXT                 
106800     MOVE NEJ                    TO INLA-RAD-FLDIVKLI                     
106900     MOVE NEJ                    TO INLA-RAD-FLINLFP                      
107000     MOVE NEJ                    TO INLA-RAD-FLINLFB                      
107100     MOVE ZERO                   TO INLA-RAD-IDANSTNR                     
107200     MOVE ZERO                   TO INLA-RAD-IDILIRAD                     
107300     MOVE ZERO                   TO INLA-RAD-IDILIST                      
107400     MOVE ZERO                   TO INLA-RAD-IDINLVGN                     
107500     MOVE SPACE                  TO INLA-RAD-IDLEVNR-KOLLI                
107600     MOVE ZERO                   TO INLA-RAD-IDOKOLLI                     
107700                                                                          
107800     IF WS-INLA-ART-BEFT > ZERO                                           
107900       IF WS-INLA-ART-BEFT > +69 AND WS-INLA-ART-BEFT < +80               
108000***LEVERANTÖRSFÖRPACKAT                                                   
108100         MOVE SPACE              TO INLA-RAD-KDINLSTA                     
108200       ELSE                                                               
108300         MOVE 'FPK'              TO INLA-RAD-KDINLSTA                     
108400       END-IF                                                             
108500     ELSE                                                                 
108600       MOVE SPACE                TO INLA-RAD-KDINLSTA                     
108700     END-IF                                                               
108800                                                                          
108900     MOVE ZERO                   TO INLA-RAD-TIUPPDAT                     
109000                                                                          
109100     PERFORM IMS-ISRT-INLA-RAD                                            
109200     .                                                                    
109300     EJECT                                                                
109400 HAB-BACKA-KOPPL-RAD SECTION.                                             
109500                                                                          
109600     PERFORM HABA-BACKA-INLE-RAD                                          
109700     .                                                                    
109800     EJECT                                                                
109900 HABA-BACKA-INLE-RAD SECTION.                                             
110000     IF WS-BACK-KDINLSTA-INLAGD                                           
110100     PERFORM IMS-GHU-INLE-MOT                                             
110200       IF SEGMENT-FINNS                                                   
110300         PERFORM IMS-GHNP-INLE-DEL                                        
110400                                                                          
110500         PERFORM UNTIL INLE-DEL-KVRAPP = WS-BACK-KVINLART                 
110600           PERFORM IMS-GHNP-INLE-DEL                                      
110700         END-PERFORM                                                      
110800                                                                          
110900         PERFORM IMS-DLET-INLE                                            
111000       ELSE                                                               
111100         MOVE NEJ TO KOPPLINGS-SW                                         
111200       END-IF                                                             
111300     END-IF                                                               
111400     .                                                                    
111500     EJECT                                                                
111600 HAC-BACKA-WDL6-RAD SECTION..                                             
111700     IF WS-BACK-KDINLSTA-INLAGD                                           
111800       PERFORM IMS-GU-WDL601                                              
111900       PERFORM IMS-GNP-WDL611                                             
112000                                                                          
112100       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
112200                     INLC-INL-IDLOPNRM = W-IDLOPNRM                       
112300         PERFORM IMS-GNP-WDL611                                           
112400       END-PERFORM                                                        
112500*                                                                         
112600       IF SEGMENT-FINNS                                                   
112700         MOVE INLC-INL-DAINLEV  TO W-DAINLEV                              
112800         PERFORM IMS-GHNP-WDL621                                          
112900                                                                          
113000         PERFORM UNTIL INLC-NDEL-KVRAPP = WS-BACK-KVINLART                
113100           PERFORM IMS-GHNP-WDL621                                        
113200         END-PERFORM                                                      
113300                                                                          
113400         PERFORM IMS-DLET-WDL621                                          
113500       ELSE                                                               
113600         MOVE NEJ TO KOPPLINGS-SW                                         
113700       END-IF                                                             
113800     END-IF                                                               
113900     .                                                                    
114000     EJECT                                                                
114100 HB-BACKA-KOPPL-TOT SECTION.                                              
114200                                                                          
114300     IF CDC                                                               
114400       PERFORM HBB-BACKA-INLE-TOT                                         
114500     ELSE                                                                 
114600       IF NDC OR GOOD-DDC                                                 
114700         PERFORM HBD-BACKA-WDL6-TOT                                       
114800       END-IF                                                             
114900     END-IF                                                               
115000                                                                          
115100     IF CDC                                                               
115200       PERFORM HBA-BACKA-ARTC-TOT                                         
115300     ELSE                                                                 
115400       IF NDC                                                             
115500         PERFORM HBC-BACKA-ARTS-TOT                                       
115600       END-IF                                                             
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000 HBA-BACKA-ARTC-TOT SECTION.                                              
116100                                                                          
116200     IF  WS-INLA-ART-KDRT        NOT = 77                                 
116300                                                                          
116400       IF CDC-SE                                                          
116500         PERFORM IMS-GHU-ARTC11                                           
116600                                                                          
116700         ADD WS-BACK-TOT-KVINLART  TO ARTC-CLAG-KVAKS-CDC                 
116800         MOVE '+' TO LOGG-IDTECKEN-KVAKS                                  
116900         SUBTRACT WS-BACK-TOT-KVINLART FROM ARTC-CLAG-KVLS                
117000         IF WS-BACK-SVS-KVINLART > ARTC-CLAG-KVLS-SVS                     
117100           CONTINUE                                                       
117200         ELSE                                                             
117300           SUBTRACT WS-BACK-SVS-KVINLART FROM ARTC-CLAG-KVLS-SVS          
117400         END-IF                                                           
117500                                                                          
117600         MOVE 1 TO CD-IX                                                  
117700         PERFORM UNTIL CD-IX > 4                                          
117800           IF WS-BACK-CD-KVINLART(CD-IX) >                                
117900                                       ARTC-CLAG-KVLS-CD(CD-IX)           
118000             CONTINUE                                                     
118100           ELSE                                                           
118200             SUBTRACT WS-BACK-CD-KVINLART(CD-IX) FROM                     
118300                                         ARTC-CLAG-KVLS-CD(CD-IX)         
118400           END-IF                                                         
118500           ADD 1 TO CD-IX                                                 
118600         END-PERFORM                                                      
118700         MOVE '-' TO LOGG-IDTECKEN-KVLS                                   
118800                                                                          
118900         PERFORM IMS-REPL-ARTC                                            
119000         PERFORM HBE-SKAPA-SALDOLOGG-WDK6                                 
119100                                                                          
119200       ELSE                                                               
119300         ADD WS-BACK-TOT-KVINLART  TO ARTC-CLAG-KVAKS-T                   
119400                                                                          
119500         PERFORM IMS-REPL-ARTC                                            
119600         PERFORM IMS-GHU-ARTS11                                           
119700         SUBTRACT WS-BACK-TOT-KVINLART FROM ARTS-SLAG-KVLS                
119800         PERFORM IMS-REPL-ARTS                                            
119900         PERFORM HBF-SKAPA-SALDOLOGG-WDK7                                 
120000                                                                          
120100       END-IF                                                             
120200     END-IF                                                               
120300     .                                                                    
120400     EJECT                                                                
120500 HBB-BACKA-INLE-TOT SECTION.                                              
120600                                                                          
120700     PERFORM IMS-GHU-INLE-MOT                                             
120800                                                                          
120900     SUBTRACT WS-BACK-TOT-KVINLART FROM INLE-MOT-KVANTMOT                 
121000                                                                          
121100     PERFORM IMS-REPL-INLE                                                
121200     .                                                                    
121300     EJECT                                                                
121400 HBC-BACKA-ARTS-TOT SECTION.                                              
121500                                                                          
121600     PERFORM IMS-GU-ARTS11                                                
121700     MOVE ARTS-SLAG-PRAVCOST       TO   WS-PRAVCOST                       
121800                                                                          
121900     IF SEGMENT-FINNS                                                     
122000       IF NDC-CN                                                          
122100       OR NDC-US                                                          
122200         IF NDC-CN                                                        
122300           MOVE 081                 TO AVG-KDCALL                         
122400         END-IF                                                           
122500         IF NDC-US                                                        
122600           MOVE 080                 TO AVG-KDCALL                         
122700         END-IF                                                           
122800         MOVE +0                    TO AVG-PRARTBEL                       
122900         MOVE +0                    TO AVG-REMARKUP                       
123000****   KINAS AVERAGE COST SKALL TA HÄNSYN TILL EFR                        
123100         COMPUTE AVG-KVLS-OLD = ARTS-SLAG-KVLS +                          
123200                 ARTS-SLAG-KVEFRS                                         
123300                                                                          
123400         MOVE WS-DAINLEV(3:6)         TO WS-DAAVIDAT-YYMMDD               
123500         MOVE WS-IDLEVNR TO W-IDLEVNR                                     
123600         PERFORM IMS-GU-WLLEVA01                                          
123700         IF SEGMENT-SAKNAS                                                
123800           MOVE ZERO TO W-RETULF                                          
123900         ELSE                                                             
124000           PERFORM IMS-GU-WDB601                                          
124100           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
124200           PERFORM IMS-GNP-WLLEVA11                                       
124300           IF SEGMENT-FINNS                                               
124400             IF LEV-TULL-TITULF < WS-DAAVIDAT-YYMMDD                      
124500               MOVE LEV-TULL-RETULF-1 TO W-RETULF                         
124600             ELSE                                                         
124700               MOVE LEV-TULL-RETULF-2 TO W-RETULF                         
124800             END-IF                                                       
124900           END-IF                                                         
125000         END-IF                                                           
125100         MOVE W-RETULF                TO AVG-REMARKUP                     
125200                                                                          
125300         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                 
125400         MOVE 01                      TO W-DATE-AAMM(3:2)                 
125500         MOVE 'SEK'                   TO CURR-KDVALISO-HUV                
125600         IF NDC-CN                                                        
125700           MOVE 'CNY'                 TO CURR-KDVALISO-ROW                
125800         END-IF                                                           
125900         IF NDC-US                                                        
126000           MOVE 'USD'                 TO CURR-KDVALISO-ROW                
127000         END-IF                                                           
127100         MOVE W-DATE-AAMM             TO CURR-TIAAMM                      
127200         MOVE 'A'                     TO CURR-KDVALTYP                    
127300         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
127400         IF CURR-KDSVAR = ' '                                             
127500           MOVE CURR-PRKURS-NEW     TO WS-PRKURS                          
127600           MOVE CURR-REVALUTA-TO    TO WS-REVALUTA                        
127700         ELSE                                                             
127800           MOVE 1                   TO WS-PRKURS                          
127900           MOVE 1                   TO WS-REVALUTA                        
128000         END-IF                                                           
128100                                                                          
128200         PERFORM IMS-GU-WDB601                                            
128300         MOVE DCS-IDLANDX2            TO W-IDLANDX2                       
128400         IF SEGMENT-FINNS                                                 
128500           PERFORM IMS-GNP-WDB617                                         
128600           IF SEGMENT-FINNS                                               
128700             PERFORM IMS-GU-ARTC11                                        
128800             IF SEGMENT-FINNS                                             
128900               COMPUTE WS-PRARTKALKYL          ROUNDED =                  
129000                    (ARTC-CLAG-PRDIRLON *                                 
129100                     PROC-REDIRLON * WS-REVALUTA / WS-PRKURS) +           
129200                    (ARTC-CLAG-PRDMTRL *                                  
129300                     PROC-REDMTRL * WS-REVALUTA / WS-PRKURS)              
129400             ELSE                                                         
129500               MOVE ZERO TO WS-PRARTKALKYL                                
129600             END-IF                                                       
129700           END-IF                                                         
129800         END-IF                                                           
129900         MOVE WS-PRARTKALKYL          TO AVG-PRARTNTO                     
130000         PERFORM S06-GET-PRARTBEL                                         
130100         PERFORM DDAB-GET-CURRENCY-RATE                                   
130200                                                                          
130300         MOVE WS-BACK-TOT-KVINLART  TO AVG-KVANTMOT                       
130400         MOVE ARTS-SLAG-PRAVCOST    TO AVG-PRAVCOST-OLD                   
130500         MOVE SPAR-PRKURS           TO AVG-PRKURS                         
130600         MOVE +0                    TO AVG-KVLEVART                       
130700         MOVE WS-ARTC-KDVALISO      TO AVG-KDVALISO                       
130800         MOVE WS-ARTC-PRARTBEL-PR   TO AVG-PRARTBEL                       
130900         MOVE ZERO                  TO AVG-KDPSLLOC                       
131000         MOVE ZERO                  TO AVG-KDPRODSL                       
131100         MOVE ZERO                  TO AVG-IDFKNGRP                       
131200         MOVE W-IDDC                TO AVG-IDDC                           
131300         MOVE +0                    TO AVG-PRAVCOST-NEW                   
131400         MOVE SPACE                 TO AVG-KDSVAR                         
131500         MOVE WS-DAINLEV(3:2)       TO AVG-TIAA                           
131600         MOVE WS-DAINLEV(5:2)       TO AVG-TIMM                           
131700                                                                          
131800         CALL W510AVG USING AVG-W510AVG 9305-AVG-PCB                      
131900                            AVG-WDB6-PCB                                  
132000         IF AVG-KDSVAR = SPACE                                            
132100           MOVE AVG-PRAVCOST-NEW TO WS-PRAVCOST                           
132200         ELSE                                                             
132300           IF AVG-KDSVAR = '4'                                            
132400             MOVE ARTS-SLAG-PRAVCOST  TO WS-PRAVCOST                      
132500           ELSE                                                           
132600             STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                        
132700              DELIMITED BY SIZE INTO FELTEXT                              
132800               CALL FELLOG                                                
132900           END-IF                                                         
133000         END-IF                                                           
133100       END-IF                                                             
133200                                                                          
133300       PERFORM IMS-GHU-ARTS11                                             
133400       MOVE     WS-PRAVCOST          TO   ARTS-SLAG-PRAVCOST              
133500       ADD WS-BACK-TOT-KVINLART  TO ARTS-SLAG-KVAKS-SDC                   
133600       SUBTRACT WS-BACK-TOT-KVINLART FROM ARTS-SLAG-KVLS                  
133700       PERFORM IMS-REPL-ARTS                                              
133800       PERFORM HBF-SKAPA-SALDOLOGG-WDK7                                   
133900     END-IF                                                               
134000     .                                                                    
134100     EJECT                                                                
134200 HBD-BACKA-WDL6-TOT SECTION.                                              
134300                                                                          
134400     PERFORM IMS-GU-WDL601                                                
134500                                                                          
134600     PERFORM IMS-GHNP-WDL611                                              
134700                                                                          
134800     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
134900                   INLC-INL-IDLOPNRM = W-IDLOPNRM                         
135000       PERFORM IMS-GHNP-WDL611                                            
135100     END-PERFORM                                                          
135200                                                                          
135300     IF SEGMENT-FINNS                                                     
135400       IF INLC-INL-IDLOPNRM = W-IDLOPNRM                                  
135500         SUBTRACT WS-BACK-TOT-KVINLART FROM INLC-INL-KVANTMOT             
135600         MOVE INLC-INL-KDVALISO   TO WS-ARTC-KDVALISO                     
135700         COMPUTE WS-DAINLEV2 = 9999999999999999 -                         
135800                              INLC-INL-DAINLEV                            
135900         MOVE WS-DAINLEV2         TO WS-DAINLEV                           
136000         MOVE WS-DAINLEV(1:8)     TO WS-DAINLEV3                          
136100         IF INLC-INL-TIAVIDAT > ZERO                                      
136200           MOVE INLC-INL-TIAVIDAT TO WS-DAINLEV4                          
136300           MOVE WS-DAINLEV4       TO WS-DAINLEV(3:6)                      
136400           MOVE WS-DAINLEV(1:8)   TO WS-DAINLEV3                          
136500         END-IF                                                           
136600         MOVE INLC-INL-PRARTNTO   TO WS-ARTC-PRARTBEL-PR                  
136700         MOVE INLC-INL-IDLEVNR    TO WS-IDLEVNR                           
136800       END-IF                                                             
136900     END-IF                                                               
137000                                                                          
137100     PERFORM IMS-REPL-WDL611                                              
137200     .                                                                    
137300     EJECT                                                                
137400 HBE-SKAPA-SALDOLOGG-WDK6 SECTION.                                        
137500     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
137600     MOVE 9                         TO LOGG-IDSEKVNR                      
137700     MOVE WS-IDDC                   TO LOGG-IDDC                          
137800     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
137900     MOVE 'R32'                     TO LOGG-IDSUBTYP                      
138000     MOVE 'W6011800'                TO LOGG-IDPGM                         
138100     MOVE '6118'                    TO LOGG-IDTRANS                       
138200     MOVE REQU-IDUSER               TO LOGG-IDUSER                        
138300     MOVE SPACE                     TO LOGG-REF                           
138400     MOVE SPACE                     TO LOGG-IDLEVNR                       
138500*    -- HIDDEN IDFS/IDLBBET FROM MID REMOVED (CHINA WH)                   
138600*    -- NOT SIGNIFICANT FOR THIS LOGGING.    (CHINA WH)                   
138700     MOVE SPACE                     TO LOGG-IDFS                          
138800     MOVE SPACE                     TO LOGG-IDLBBET                       
138900                                                                          
139000     MOVE REQU-IDLOPNRM-KEY         TO LOGG-IDLOPNRM                      
139100     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
139200     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
139300     MOVE WS-BACK-TOT-KVINLART      TO LOGG-KVART-SALDO                   
139400*                                                                         
139500     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                           
139600                          ARTC-CLAG-KVAKS-T                               
139700     MOVE ARTC-CLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                     
139800     MOVE ARTC-CLAG-KVEFRS          TO LOGG-KVEFRS                        
139900     MOVE ARTC-CLAG-KVLS            TO LOGG-KVLS                          
140000     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
140100     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
140200     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
140300     ACCEPT WLOGG-TID FROM TIME                                           
140400     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
140500                                                                          
140600     PERFORM IMS-ISRT-WDL9                                                
140700     IF SEGMENT-FINNS-REDAN                                               
140800       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
140900          ADD -1 TO LOGG-IDSEKVNR                                         
141000          PERFORM IMS-ISRT-WDL9                                           
141100       END-PERFORM                                                        
141200     END-IF                                                               
141300     .                                                                    
141400     EJECT                                                                
141500                                                                          
141600 HBF-SKAPA-SALDOLOGG-WDK7 SECTION.                                        
141700     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
141800     MOVE 9                         TO LOGG-IDSEKVNR                      
141900     MOVE WS-IDDC                   TO LOGG-IDDC                          
142000     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
142100     MOVE 'R32'                     TO LOGG-IDSUBTYP                      
142200     MOVE 'W6011800'                TO LOGG-IDPGM                         
142300     MOVE '6118'                    TO LOGG-IDTRANS                       
142400     MOVE REQU-IDUSER               TO LOGG-IDUSER                        
142500     MOVE SPACE                     TO LOGG-REF                           
142600     MOVE SPACE                     TO LOGG-IDLEVNR                       
142700     MOVE SPACE                     TO LOGG-IDFS                          
142800     MOVE SPACE                     TO LOGG-IDLBBET                       
142900     MOVE REQU-IDLOPNRM-KEY         TO LOGG-IDLOPNRM                      
143000     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
143100     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
143200     MOVE WS-BACK-TOT-KVINLART      TO LOGG-KVART-SALDO                   
143300     MOVE ARTS-SLAG-KVAKS-SDC       TO LOGG-KVAKS                         
143400     MOVE ARTS-SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                     
143500     MOVE ARTS-SLAG-KVEFRS          TO LOGG-KVEFRS                        
143600     MOVE ARTS-SLAG-KVLS            TO LOGG-KVLS                          
143700     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
143800                                                                          
143900     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
144000     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
144100     ACCEPT WLOGG-TID FROM TIME                                           
144200     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
144300                                                                          
144400     PERFORM IMS-ISRT-WDL9                                                
144500     IF SEGMENT-FINNS-REDAN                                               
144600       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
144700          ADD -1 TO LOGG-IDSEKVNR                                         
144800          PERFORM IMS-ISRT-WDL9                                           
144900       END-PERFORM                                                        
145000     END-IF                                                               
145100     .                                                                    
145200     EJECT                                                                
145300                                                                          
145400 S06-GET-PRARTBEL SECTION.                                                
145500     MOVE ZERO        TO WS-ARTC-PRARTBEL-PR                              
145600                                                                          
145700*    -- WDK711                                                            
145800     PERFORM IMS-GU-ARTS11                                                
145900     IF SEGMENT-FINNS                                                     
146000                                                                          
146100*    -- WDK724                                                            
146200       MOVE WS-IDLEVNR           TO W-IDLEVNR-PR                          
146300       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAINLEV3                     
146400       PERFORM IMS-GNP-WDK724                                             
146500                                                                          
146600       IF SEGMENT-FINNS                                                   
146700         MOVE SPRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-PR                   
146800         MOVE SPRL-KDVALISO      TO WS-ARTC-KDVALISO                      
146900       END-IF                                                             
147000     END-IF                                                               
147100     .                                                                    
147200     EJECT                                                                
147300                                                                          
147400 DDAB-GET-CURRENCY-RATE SECTION.                                          
147500     MOVE WS-IDDC         TO W-IDDC                                       
147600     PERFORM IMS-GU-WDB601                                                
147700     MOVE DCS-KDVALISO      TO W-KDVALISO-HUV                             
147800     IF W-KDVALISO-HUV =  WS-ARTC-KDVALISO                                
147900       MOVE 1 TO SPAR-PRKURS                                              
148000       MOVE 1 TO W-REVALUTA                                               
148100     ELSE                                                                 
148200       MOVE WS-ARTC-KDVALISO TO W-KDVALISO-ROW                            
148300       PERFORM IMS-GU-WDGX9306                                            
148400       IF SEGMENT-SAKNAS                                                  
148500         MOVE 1               TO SPAR-PRKURS                              
148600         MOVE 1               TO W-REVALUTA                               
148700       ELSE                                                               
148800         COMPUTE W-TISTADAT-9KOMPL =                                      
148900                 9999999 - WS-DATE-YYMMDD                                 
149000         PERFORM IMS-GNP-WDGX9308                                         
149100         IF SEGMENT-SAKNAS                                                
149200           PERFORM IMS-GNP-WDGX9308-FIRST                                 
149300           IF SEGMENT-SAKNAS                                              
149400             MOVE 1               TO SPAR-PRKURS                          
149500             MOVE 1               TO W-REVALUTA                           
149600           ELSE                                                           
149700             MOVE 9308-PRKURS   TO SPAR-PRKURS                            
149800             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
149900           END-IF                                                         
150000         ELSE                                                             
150100           MOVE 9308-PRKURS   TO SPAR-PRKURS                              
150200           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
150300         END-IF                                                           
150400       END-IF                                                             
150500     END-IF                                                               
150600     .                                                                    
150700     EJECT                                                                
150800                                                                          
150900 S10-KTRL-INLA-ART SECTION.                                               
151000     MOVE W-IDLOPNRM             TO W-W6D1BSEQ-IDLOPNRM                   
151100                                                                          
151200     PERFORM IMS-GU-INLA-ART                                              
151300     IF  SEGMENT-FINNS                                                    
151400       MOVE INLA-ART-IDARTNR   TO W-IDARTNR                               
151500       MOVE INLA-ART-BEFT      TO WS-INLA-ART-BEFT                        
151600       MOVE INLA-ART-FLKLAR    TO WS-INLA-ART-FLKLAR                      
151700       MOVE INLA-ART-KDRT      TO WS-INLA-ART-KDRT                        
151800       MOVE INLA-ART-KVAVIS    TO WS-INLA-ART-KVAVIS                      
151900       MOVE INLA-ART-ADTRDEST  TO WS-INLA-ART-ADTRDEST                    
152000                                                                          
152100       PERFORM S11-RED-OUTPUT-HUV                                         
152200                                                                          
152300       IF WS-INLA-ART-FLKLAR = JA                                         
152400         MOVE WERR-INVALID-UPDATE TO RESP-IDMSG-ERROR                     
152500         IF REQU-UPDATE                                                   
152600           PERFORM RESP-ROER-EJ-FAELT-UT                                  
152700           PERFORM RESP-ROER-EJ-FAELT-IN                                  
152800         ELSE                                                             
152900           PERFORM RESP-RENSA-FAELT-UT-BLAD                               
153000           PERFORM RESP-RENSA-FAELT-UT-RADER                              
153100           PERFORM RESP-RENSA-FAELT-IN                                    
153200         END-IF                                                           
153300         MOVE NEJ            TO INDATA-SW                                 
153400       END-IF                                                             
153500     ELSE                                                                 
153600       MOVE WERR-NOT-FOUND   TO RESP-IDMSG-ERROR                          
153700       MOVE 'IDLOPNRM'       TO RESP-IDELMT-ERROR                         
153800       MOVE ZERO TO RESP-KVRADER                                          
153900       PERFORM RESP-RENSA-FAELT-UT                                        
154000       PERFORM RESP-RENSA-FAELT-IN                                        
154100       MOVE NEJ              TO INDATA-SW                                 
154200     END-IF                                                               
154300     .                                                                    
154400     EJECT                                                                
154500 S11-RED-OUTPUT-HUV SECTION.                                              
154600                                                                          
154700     MOVE INLA-ART-IDARTNR       TO RESP-IDARTNR                          
154800                                    W-IDARTNR                             
154900     MOVE INLA-ART-KVAVIS        TO RESP-KVAVIS                           
155000                                                                          
155100*    MOVE INLA-ART-BEART         TO RESP-BEART                            
155200*    -- FETCH BETTER FLAVOR OF DESCRIPTION THAN IN INLA                   
155300     PERFORM S11A-FETCH-DESCRIPTION                                       
155400                                                                          
155500     MOVE INLA-ART-KDSORT        TO RESP-KDSORT                           
155600     MOVE INLA-ART-BEFT          TO RESP-BEFT                             
155700                                                                          
155800     EVALUATE INLA-ART-KDFARLIG ALSO REQU-IDSPRAK                         
155900                                                                          
156000                                                                          
156100       WHEN +4 ALSO 'SV'                                                  
156200         MOVE 'Ja            '       TO RESP-BEFARLIG                     
156300       WHEN +4 ALSO NOT 'SV'                                              
156400         MOVE 'Yes           '       TO RESP-BEFARLIG                     
156500       WHEN +5 ALSO 'SV'                                                  
156600         MOVE 'Ja, asbest    '       TO RESP-BEFARLIG                     
156700       WHEN +5 ALSO NOT 'SV'                                              
156800         move 'Yes, asbestos '       to resp-befarlig                     
156900       WHEN +6 ALSO 'SV'                                                  
157000         MOVE 'Kemikalier    '       TO RESP-BEFARLIG                     
157100       WHEN +6 ALSO NOT 'SV'                                              
157200         MOVE 'Chemicals     '       TO RESP-BEFARLIG                     
157300       WHEN +7 ALSO 'SV'                                                  
157400         MOVE 'Ja            '       TO RESP-BEFARLIG                     
157500       WHEN +7 ALSO NOT 'SV'                                              
157600         MOVE 'Yes           '       TO RESP-BEFARLIG                     
157700       WHEN OTHER                                                         
157800         MOVE SPACE                  TO RESP-BEFARLIG                     
157900     END-EVALUATE                                                         
158000                                                                          
158100     MOVE ZERO                   TO WS-KVRAPP                             
158200     PERFORM IMS-GNP-INLA-RAD                                             
158300     PERFORM UNTIL SEGMENT-SAKNAS                                         
158400       IF  (INLA-RAD-KDINLSTA = 'INL' OR 'VOR')                           
158500       OR  INLA-RAD-FLSATS = JA                                           
158600         ADD INLA-RAD-KVINLART   TO WS-KVRAPP                             
158700       END-IF                                                             
158800       PERFORM IMS-GNP-INLA-RAD                                           
158900     END-PERFORM                                                          
159000     MOVE WS-KVRAPP              TO RESP-KVRAPP                           
159100     .                                                                    
159200     EJECT                                                                
159300 S11A-FETCH-DESCRIPTION SECTION.                                          
159400                                                                          
159500     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
159600                                                                          
159700*    -- SELECT LANGUAGE TO FETCH AND CORRESPONDING CODE-PAGE              
159800     EVALUATE REQU-IDSPRAK                                                
159900       WHEN 'SV'                                                          
160000        MOVE WS-IDSKYLT-SE TO W-IDSKYLT-X                                 
160100        MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                
160200                                                                          
160300       WHEN 'ZH'                                                          
160400        MOVE WS-IDSKYLT-CN TO W-IDSKYLT-X                                 
160500        MOVE WS-CP-UNICODE TO TRAUTF8-KDCP                                
160600                                                                          
160700       WHEN OTHER                                                         
160800        MOVE WS-IDSKYLT-GB TO W-IDSKYLT-X                                 
160900        MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                
161000     END-EVALUATE                                                         
161100                                                                          
161200     PERFORM IMS-GU-WDD311                                                
161300     IF SEGMENT-FINNS                                                     
161400       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
161500     ELSE                                                                 
161600       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
161700                             TEXT-BEART                                   
161800       MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                 
161900     END-IF                                                               
161910     IF TRAUTF8-TECONV-FROM = SPACES                                      
161920      MOVE 'GB'  TO W-IDSKYLT                                             
161930      MOVE '278' TO TRAUTF8-KDCP                                          
161940      PERFORM IMS-GU-WDD311                                               
161950      MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
161960     END-IF                                                               
162000                                                                          
162100     IF MAX-KVRADER > 20                                                  
162200*      CALL FROM WEB                                                      
162300*      CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE         
162400       CALL WTRAUTF8 USING TRAUTF8-AREA                                   
162500       MOVE TRAUTF8-TECONV-TO TO RESP-BEART                               
162600     ELSE                                                                 
162700*      CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                       
162800       MOVE TEXT-BEART        TO RESP-BEART                               
162900     END-IF                                                               
163000     .                                                                    
163100     EJECT                                                                
163200 S12-KOLLA-OM-KR-UPPDATERING SECTION.                                     
163300                                                                          
163400     MOVE JA TO W6202-SW                                                  
163500                                                                          
163600     MOVE W-IDLOPNRM TO W-IDLOPNRM-H7                                     
163700     PERFORM IMS-GU-SEQB-W6D1B1                                           
163800     MOVE SEQB-TIAVIDAT TO W-DAAVSDAT-H7                                  
163900     IF SEQB-TIAVIDAT NOT = ZERO                                          
164000       IF SEQB-TIAVIDAT < 500000                                          
164100         MOVE 20        TO W-DAAVSDAT-H7 (1:2)                            
164200       ELSE                                                               
164300         IF SEQB-TIAVIDAT < 999999                                        
164400           MOVE 19      TO W-DAAVSDAT-H7 (1:2)                            
164500         ELSE                                                             
164600           MOVE 99999999 TO W-DAAVSDAT-H7                                 
164700         END-IF                                                           
164800       END-IF                                                             
164900     END-IF                                                               
165000     IF WS-INLA-ART-KDRT     = 6 OR 7 OR 77 OR 8 OR                       
165100        WS-INLA-ART-KVAVIS   = +0                                         
165200       MOVE NEJ TO W6202-SW                                               
165300     ELSE                                                                 
165400       PERFORM IMS-GHU-KVAE-W6H701                                        
165500       IF SEGMENT-FINNS                                                   
165600           IF KVAE-KR-IDKRFEL          = 'PA' OR 'PB'                     
165700             IF (KVAE-KR-KVANTMOT + SPAR-KVINLART) =                      
165800                 KVAE-KR-KVAVIS                                           
165900               IF (KVAE-KR-KDKRSTA = '0' OR '1')                          
166000                 IF KVAE-KR-FLANNULL = NEJ                                
166100** OM ANTALSAVVIKELSE TAS TILLBAKA FÅR KR ENDAST ANNULERAS OM             
166200** INTE ADMINISTRATIVT FEL ÄR ANGIVET PÅ BILD 6139                        
166300                  PERFORM IMS-GU-UPFA01                                   
166400                   IF SEGMENT-FINNS                                       
166500                     IF UPPF-KDKVASTA-ADM = '3'                           
166600                       MOVE 'K ' TO KVAE-KR-IDKRFEL                       
166700                       MOVE ZERO TO KVAE-KR-KVANTMOT                      
166800                                    KVAE-KR-KVART-AAVV                    
166900                                    KVAE-KR-KVART-EJ-GODK                 
167000                                    KVAE-KR-KVART-KJUST                   
167100                                    KVAE-KR-KVART-KONTR                   
167200                                    KVAE-KR-KVART-RET                     
167300                                    KVAE-KR-KVART-SJUST                   
167400                                    KVAE-KR-KVART-SKROT                   
167500                                    KVAE-KR-KDDISP                        
167600                                    KVAE-KR-KDHANDCO                      
167700                       MOVE WERR-REMAINS-ADM-REP                          
167800                            TO RESP-IDMSG-ERROR                           
167900                     ELSE                                                 
168000                       MOVE JA TO KVAE-KR-FLANNULL                        
168100                       MOVE REQU-IDUSER TO USER-X                         
168200                       MOVE USER TO KVAE-KR-BEKRANS                       
168300                       MOVE SPACE TO KVAE-KR-IDKRATLF                     
168400                     END-IF                                               
168500                   ELSE                                                   
168600                     MOVE JA TO KVAE-KR-FLANNULL                          
168700                     MOVE REQU-IDUSER TO USER-X                           
168800                     MOVE USER TO KVAE-KR-BEKRANS                         
168900                     MOVE SPACE TO KVAE-KR-IDKRATLF                       
169000                   END-IF                                                 
169100                   PERFORM IMS-REPL-KVAE-W6H7                             
169200                   MOVE NEJ TO W6202-SW                                   
169300                 ELSE                                                     
169400                   MOVE NEJ TO W6202-SW                                   
169500                 END-IF                                                   
169600               ELSE                                                       
169700                 MOVE WERR-MAN-CHG-NEEDED TO RESP-IDMSG-ERROR             
169800                 MOVE NEJ                 TO W6202-SW                     
169900               END-IF                                                     
170000             ELSE                                                         
170100               IF (KVAE-KR-KDKRSTA = '0' OR '1') OR                       
170200                  (KVAE-KR-KDKRSTA > '1'        AND                       
170300                   KVAE-KR-FLKRGODK = NEJ )                               
170400                  IF KVAE-KR-FLANNULL = JA                                
170500                    MOVE NEJ TO KVAE-KR-FLANNULL                          
170600                    PERFORM IMS-REPL-KVAE-W6H7                            
170700                  END-IF                                                  
170800               ELSE                                                       
170900                 MOVE NEJ                    TO W6202-SW                  
171000                 MOVE WERR-ALREADY-REGISTRED TO RESP-IDMSG-ERROR          
171100               END-IF                                                     
171200             END-IF                                                       
171300           ELSE                                                           
171400               PERFORM IMS-GHN-KVAE-W6H701                                
171500               IF SEGMENT-FINNS                                           
171600                 IF KVAE-KR-IDKRFEL          = 'PA' OR 'PB'               
171700                   IF (KVAE-KR-KVANTMOT + SPAR-KVINLART) =                
171800                       KVAE-KR-KVAVIS                                     
171900                       IF (KVAE-KR-KDKRSTA = '0' OR '1')                  
172000                         IF KVAE-KR-FLANNULL = NEJ                        
172100                           MOVE JA TO KVAE-KR-FLANNULL                    
172200                           MOVE REQU-IDUSER TO USER-X                     
172300                           MOVE USER TO KVAE-KR-BEKRANS                   
172400                           MOVE SPACE TO KVAE-KR-IDKRATLF                 
172500                           PERFORM IMS-REPL-KVAE-W6H7                     
172600                           MOVE NEJ                TO W6202-SW            
172700                         ELSE                                             
172800                           MOVE NEJ                TO W6202-SW            
172900                         END-IF                                           
173000                       ELSE                                               
173100                         MOVE NEJ                TO W6202-SW              
173200                         MOVE WERR-MAN-CHG-NEEDED                         
173300                              TO RESP-IDMSG-ERROR                         
173400                       END-IF                                             
173500                   ELSE                                                   
173600                     IF (KVAE-KR-KDKRSTA = '0' OR '1') OR                 
173700                        (KVAE-KR-KDKRSTA > '1'        AND                 
173800                         KVAE-KR-FLKRGODK = NEJ )                         
173900                       IF KVAE-KR-FLANNULL = JA                           
174000                         MOVE NEJ TO KVAE-KR-FLANNULL                     
174100                         PERFORM IMS-REPL-KVAE-W6H7                       
174200                       END-IF                                             
174300                     ELSE                                                 
174400                       MOVE NEJ                TO W6202-SW                
174500                       MOVE 'IDKR'  TO RESP-IDELMT-ERROR                  
174600                       MOVE WERR-ALREADY-REGISTRED                        
174700                            TO RESP-IDMSG-ERROR                           
174800                     END-IF                                               
174900                   END-IF                                                 
175000                 END-IF                                                   
175100             END-IF                                                       
175200           END-IF                                                         
175300       END-IF                                                             
175400     END-IF                                                               
175500                                                                          
175600     IF W6202-TRANS                                                       
175700        PERFORM S12A-STARTA-W6202                                         
175800     END-IF                                                               
175900     .                                                                    
176000     EJECT                                                                
176100 S12A-STARTA-W6202     SECTION.                                           
176200                                                                          
176300     MOVE ALL '+'              TO 6202-REQU-W60202I1                      
176400     SET 6202-REQU-UPD-X       TO TRUE                                    
176500     MOVE ZERO                 TO 6202-REQU-IDKR-KEY                      
176600     MOVE REQU-IDDC-KEY        TO 6202-REQU-IDDC-KEY                      
176700     MOVE W-IDLOPNRM           TO 6202-REQU-IDLOPNRM-UPD                  
176800     MOVE SEQB-TIAVIDAT        TO 6202-REQU-TIAVSDAT-UPD                  
176900                                                                          
177000     COMPUTE 6202-REQU-KVANTMOT-UPD =                                     
177100             KVAE-KR-KVANTMOT + SPAR-KVINLART                             
177200                                                                          
177300*                                                                         
177400     COMPUTE 6202-REQU-KVLL    = LENGTH OF 6202-AREA                      
177500     MOVE LOW-VALUE            TO 6202-REQU-KDZ1 6202-REQU-KDZ2           
177600     SET 6202-REQU-UPD-X       TO TRUE                                    
177700     MOVE 'W6W202T '           TO 6202-REQU-KDTRANS                       
177800     MOVE 101                  TO 6202-REQU-IDMSGVER                      
177900     MOVE REQU-IDUSER          TO 6202-REQU-IDUSER                        
178000                                                                          
178100     PERFORM IMS-ISRT-6202-MSG                                            
178200     .                                                                    
178300                                                                          
178400     EJECT                                                                
178500 S20-KOLLA-ADINLOMR  SECTION.                                             
178600                                                                          
178700*    --- CHECK IF AREA HAS BEEN SPECIFIED IN ANY LINE OR NOT              
178800     MOVE NEJ TO SW-ADINLOMR                                              
178900                                                                          
179000     MOVE +1  TO INDX                                                     
179100     PERFORM UNTIL INDX > REQU-KVRADER                                    
179200       IF  REQU-ADINLOMR-UPD-LINE (INDX) NOT = ALL '+'                    
179300       AND REQU-ADINLOMR-UPD-LINE (INDX) NOT = SPACE                      
179400         MOVE JA TO SW-ADINLOMR                                           
179500         MOVE MAX-KVRADER TO INDX                                         
179600       END-IF                                                             
179700       ADD 1 TO INDX                                                      
179800     END-PERFORM                                                          
179900     .                                                                    
180000                                                                          
180100     EJECT                                                                
180200 RESP-RENSA-FAELT-UT SECTION.                                             
180300                                                                          
180400*    --- ALLA UTDATA-FÄLT                                                 
180500     PERFORM RESP-RENSA-FAELT-UT-BLAD                                     
180600     PERFORM RESP-RENSA-FAELT-UT-HUV                                      
180700     PERFORM RESP-RENSA-FAELT-UT-RADER                                    
180800     .                                                                    
180900     SKIP2                                                                
181000 RESP-RENSA-FAELT-UT-BLAD SECTION.                                        
181100                                                                          
181200*    --- BLÄDDRINGSNYCKLAR                                                
181300     MOVE ZERO        TO RESP-IDRADNR-START                               
181400                         RESP-IDRADNR-NEXT                                
181500     .                                                                    
181600     SKIP2                                                                
181700 RESP-RENSA-FAELT-UT-HUV SECTION.                                         
181800                                                                          
181900*    --- UTDATA-FÄLT I BILD-HUVUD                                         
182000     MOVE W-SPACE         TO RESP-IDARTNR                                 
182100                             RESP-KVAVIS                                  
182200                             RESP-BEART                                   
182300                             RESP-KDSORT                                  
182400                             RESP-BEFT                                    
182500                             RESP-KVRAPP                                  
182600                             RESP-BEFARLIG                                
182700     IF MAX-KVRADER > 20                                                  
182800*       -- UNICODE SPACE IF CALLED FROM WEB                               
182900        MOVE ALL X'20'    TO RESP-BEART                                   
183000     END-IF                                                               
183100     .                                                                    
183200     SKIP2                                                                
183300 RESP-RENSA-FAELT-UT-RADER SECTION.                                       
183400                                                                          
183500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
183600     MOVE +1 TO INDX                                                      
183700     MOVE RESP-KVRADER TO W-KVRADER                                       
183800     PERFORM UNTIL INDX > W-KVRADER                                       
183900       PERFORM RESP-RENSA-RAD-FAELT-UT                                    
184000       ADD +1 TO INDX                                                     
184100     END-PERFORM                                                          
184200     .                                                                    
184300     SKIP2                                                                
184400 RESP-RENSA-RAD-FAELT-UT SECTION.                                         
184500                                                                          
184600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
184700     MOVE W-SPACE         TO RESP-IDRADNR-LINE (INDX)                     
184800                             RESP-KVINLART-LINE (INDX)                    
184900                             RESP-ADINLOMR-LINE (INDX)                    
185000                             RESP-KDINLSTA-LINE (INDX)                    
185100                             RESP-IDLEVNR-KOLLI-LINE (INDX)               
185200                             RESP-IDOKOLLI-LINE (INDX)                    
185300                             RESP-STATUS-TEXT-LINE (INDX)                 
185400     .                                                                    
185500     SKIP2                                                                
185600 RESP-RENSA-FAELT-IN SECTION.                                             
185700                                                                          
185800*    --- ALLA INDATA-FÄLT                                                 
185900     MOVE +1 TO INDX                                                      
186000     MOVE RESP-KVRADER TO W-KVRADER                                       
186100     PERFORM UNTIL INDX > W-KVRADER                                       
186200       MOVE SPACE TO RESP-ADINLOMR-UPD-LINE (INDX)                        
186300       ADD +1 TO INDX                                                     
186400     END-PERFORM                                                          
186500     .                                                                    
186600     EJECT                                                                
186700 RESP-ROER-EJ-FAELT-UT SECTION.                                           
186800                                                                          
186900*    --- ALLA UTDATA-FÄLT                                                 
187000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
187100     MOVE W-PLUS            TO RESP-IDRADNR-START                         
187200                               RESP-IDRADNR-NEXT                          
187300                               RESP-IDARTNR                               
187400                               RESP-KVAVIS                                
187500                               RESP-BEART                                 
187600                               RESP-KDSORT                                
187700                               RESP-BEFT                                  
187800                               RESP-KVRAPP                                
187900                               RESP-BEFARLIG                              
188000     IF MAX-KVRADER > 20                                                  
188100*       -- UNICODE PLUS IF CALLED FROM WEB                                
188200        MOVE ALL X'2B'      TO RESP-BEART                                 
188300     END-IF                                                               
188400                                                                          
188500     MOVE +1 TO INDX                                                      
188600     MOVE RESP-KVRADER TO W-KVRADER                                       
188700     PERFORM UNTIL INDX > W-KVRADER                                       
188800       PERFORM RESP-ROER-EJ-RAD-FAELT-UT                                  
188900       ADD +1 TO INDX                                                     
189000     END-PERFORM                                                          
189100     .                                                                    
189200     SKIP2                                                                
189300 RESP-ROER-EJ-RAD-FAELT-UT SECTION.                                       
189400                                                                          
189500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
189600     MOVE W-PLUS          TO RESP-IDRADNR-LINE (INDX)                     
189700                             RESP-KVINLART-LINE (INDX)                    
189800                             RESP-ADINLOMR-LINE (INDX)                    
189900                             RESP-KDINLSTA-LINE (INDX)                    
190000                             RESP-IDLEVNR-KOLLI-LINE (INDX)               
190100                             RESP-IDOKOLLI-LINE (INDX)                    
190200                             RESP-STATUS-TEXT-LINE (INDX)                 
190300     .                                                                    
190400     SKIP2                                                                
190500 RESP-ROER-EJ-FAELT-IN SECTION.                                           
190600                                                                          
190700*    --- ALLA INDATA-FÄLT                                                 
190800     MOVE +1 TO INDX                                                      
190900     MOVE RESP-KVRADER TO W-KVRADER                                       
191000     PERFORM UNTIL INDX > W-KVRADER                                       
191100       MOVE ALL '+'   TO RESP-ADINLOMR-UPD-LINE (INDX)                    
191200       ADD +1 TO INDX                                                     
191300     END-PERFORM                                                          
191400     .                                                                    
191500     EJECT                                                                
191600 RESP-FORM-ATTR SECTION.                                                  
191700                                                                          
191800*    --- ALLA INDATA-FÄLT                                                 
191900     MOVE +1 TO INDX                                                      
192000     MOVE RESP-KVRADER TO W-KVRADER                                       
192100     PERFORM UNTIL INDX > W-KVRADER                                       
192200       MOVE MFS-FORMATETS-ATTR                                            
192300                    TO RESP-ADINLOMR-UPD-LINE-ATTR (INDX)                 
192400       ADD +1 TO INDX                                                     
192500     END-PERFORM                                                          
192600     .                                                                    
192700                                                                          
192800     EJECT                                                                
192900* --- IMS SEKTIONER ---                                                   
193000                                                                          
193100 IMS-ISRT-6202-MSG SECTION.                                               
193200     MOVE SPACE TO GODK-STATUSKODER                                       
193300     CALL CBLTDLI USING ISRT 6202-PCB 6202-AREA                           
193400     MOVE 6202-STATUS-CODE TO STATUS-WS                                   
193500     PERFORM IMS-STATUSKONTROLL                                           
193600     .                                                                    
193700     EJECT                                                                
193800 IMS-GU-INLA-ART SECTION.                                                 
193900     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
194000                    '&IDDC     =' W-IDDC-X         ')'                    
194100          DELIMITED BY SIZE INTO SSA1                                     
194200     MOVE '  GE' TO GODK-STATUSKODER                                      
194300     CALL CBLTDLI USING GU INLA-PCB DLI-IO-W6INLA11  SSA1                 
194400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
194500     PERFORM IMS-STATUSKONTROLL                                           
194600     .                                                                    
194700     SKIP3                                                                
194800 IMS-GNP-INLA-RAD SECTION.                                                
194900     MOVE 'W6INLA21 ' TO SSA1                                             
195000     MOVE '  GE' TO GODK-STATUSKODER                                      
195100     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-W6INLA21 SSA1                 
195200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
195300     PERFORM IMS-STATUSKONTROLL                                           
195400     .                                                                    
195500     SKIP3                                                                
195600 IMS-GNP-INLA-RAD-F-GQ SECTION.                                           
195700     STRING 'W6INLA21*F(IDRADNR =>' W-IDRADNR-X ')'                       
195800          DELIMITED BY SIZE INTO SSA1                                     
195900     MOVE '  GE' TO GODK-STATUSKODER                                      
196000     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-W6INLA21  SSA1                
196100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400     EJECT                                                                
196500 IMS-GNP-INLA-RAD-F-Q SECTION.                                            
196600     STRING 'W6INLA21*F(IDRADNR  =' W-IDRADNR-X ')'                       
196700          DELIMITED BY SIZE INTO SSA1                                     
196800     MOVE '  GE' TO GODK-STATUSKODER                                      
196900     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-W6INLA21  SSA1                
197000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
197100     PERFORM IMS-STATUSKONTROLL                                           
197200     .                                                                    
197300     SKIP3                                                                
197400 IMS-GHNP-INLA-RAD-F-Q SECTION.                                           
197500     STRING 'W6INLA21*F(IDRADNR  =' W-IDRADNR-X ')'                       
197600          DELIMITED BY SIZE INTO SSA1                                     
197700     MOVE '  ' TO GODK-STATUSKODER                                        
197800     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-W6INLA21  SSA1               
197900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
198000     PERFORM IMS-STATUSKONTROLL                                           
198100     .                                                                    
198200     EJECT                                                                
198300 IMS-REPL-INLA SECTION.                                                   
198400                                                                          
198500     MOVE '  ' TO GODK-STATUSKODER                                        
198600     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-W6INLA21                     
198700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
198800     PERFORM IMS-STATUSKONTROLL                                           
198900     .                                                                    
199000     SKIP3                                                                
199100 IMS-DLET-INLA SECTION.                                                   
199200                                                                          
199300     MOVE '  ' TO GODK-STATUSKODER                                        
199400     CALL CBLTDLI USING DLET INLA-PCB DLI-IO-W6INLA21                     
199500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     .                                                                    
199800     SKIP3                                                                
199900 IMS-ISRT-INLA-RAD SECTION.                                               
200000     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
200100          DELIMITED BY SIZE INTO SSA1                                     
200200     MOVE 'W6INLA21 ' TO SSA2                                             
200300     MOVE '  ' TO GODK-STATUSKODER                                        
200400     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-W6INLA21                     
200500                             SSA1 SSA2                                    
200600     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
200700     PERFORM IMS-STATUSKONTROLL                                           
200800     .                                                                    
200900     EJECT                                                                
201000 IMS-GU-SEQB-W6D1B1 SECTION.                                              
201100     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
201200          DELIMITED BY SIZE INTO SSA1                                     
201300     MOVE '  ' TO GODK-STATUSKODER                                        
201400     CALL CBLTDLI USING GU   SEQB-PCB DLI-IO-W6INLC01 SSA1                
201500     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
201600     PERFORM IMS-STATUSKONTROLL                                           
201700     .                                                                    
201800     EJECT                                                                
201900 IMS-GHU-KVAE-W6H701 SECTION.                                             
202000     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
202100          DELIMITED BY SIZE INTO SSA1                                     
202200     MOVE '  GE' TO GODK-STATUSKODER                                      
202300     CALL CBLTDLI USING GHU KVAE-PCB DLI-IO-W6KVAE01  SSA1                
202400     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
202500     PERFORM IMS-STATUSKONTROLL                                           
202600     .                                                                    
202700     SKIP3                                                                
202800 IMS-GHN-KVAE-W6H701 SECTION.                                             
202900     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
203000          DELIMITED BY SIZE INTO SSA1                                     
203100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
203200     CALL CBLTDLI USING GHN KVAE-PCB DLI-IO-W6KVAE01 SSA1                 
203300     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
203400     PERFORM IMS-STATUSKONTROLL                                           
203500     .                                                                    
203600     SKIP3                                                                
203700 IMS-REPL-KVAE-W6H7 SECTION.                                              
203800                                                                          
203900     MOVE '  ' TO GODK-STATUSKODER                                        
204000     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-W6KVAE01                     
204100     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
204200     PERFORM IMS-STATUSKONTROLL                                           
204300     .                                                                    
204400     EJECT                                                                
204500 IMS-GU-PLAA-6006 SECTION.                                                
204600     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
204700          DELIMITED BY SIZE INTO SSA1                                     
204800     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
204900          DELIMITED BY SIZE INTO SSA2                                     
205000     MOVE '  GE' TO GODK-STATUSKODER                                      
205100     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-W6PLAA11 SSA1 SSA2             
205200     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
205300     PERFORM IMS-STATUSKONTROLL                                           
205400     .                                                                    
205500     EJECT                                                                
205600 IMS-GHU-ARTC11 SECTION.                                                  
205700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
205800          DELIMITED BY SIZE INTO SSA1                                     
205900     MOVE 'WLARTC11 ' TO SSA2                                             
206000     MOVE '  ' TO GODK-STATUSKODER                                        
206100     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2            
206200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
206300     PERFORM IMS-STATUSKONTROLL                                           
206400     .                                                                    
206500     SKIP3                                                                
206600 IMS-GU-ARTC11 SECTION.                                                   
206700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
206800          DELIMITED BY SIZE INTO SSA1                                     
206900     MOVE 'WLARTC11 ' TO SSA2                                             
207000     MOVE '  ' TO GODK-STATUSKODER                                        
207100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
207200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
207300     PERFORM IMS-STATUSKONTROLL                                           
207400     .                                                                    
207500     SKIP3                                                                
207600 IMS-REPL-ARTC SECTION.                                                   
207700                                                                          
207800     MOVE '  ' TO GODK-STATUSKODER                                        
207900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
208000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
208100     PERFORM IMS-STATUSKONTROLL                                           
208200     .                                                                    
208300     EJECT                                                                
208400 IMS-GU-ARTS11 SECTION.                                                   
208500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
208600          DELIMITED BY SIZE INTO SSA1                                     
208700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
208800          DELIMITED BY SIZE INTO SSA2                                     
208900     MOVE '  ' TO GODK-STATUSKODER                                        
209000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WLARTS11 SSA1 SSA2             
209100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
209200     PERFORM IMS-STATUSKONTROLL                                           
209300     .                                                                    
209400     SKIP3                                                                
209500 IMS-GHU-ARTS11 SECTION.                                                  
209600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
209700          DELIMITED BY SIZE INTO SSA1                                     
209800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
209900          DELIMITED BY SIZE INTO SSA2                                     
210000     MOVE '  ' TO GODK-STATUSKODER                                        
210100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WLARTS11 SSA1 SSA2            
210200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
210300     PERFORM IMS-STATUSKONTROLL                                           
210400     .                                                                    
210500     SKIP3                                                                
210600 IMS-REPL-ARTS SECTION.                                                   
210700                                                                          
210800     MOVE '  ' TO GODK-STATUSKODER                                        
210900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WLARTS11                     
211000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
211100     PERFORM IMS-STATUSKONTROLL                                           
211200     .                                                                    
211300     EJECT                                                                
211400 IMS-GHU-INLE-MOT SECTION.                                                
211500     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
211600          DELIMITED BY SIZE INTO SSA1                                     
211700     MOVE 'WLINLE11 ' TO SSA2                                             
211800     STRING 'WLINLE21(IDLOPNRM =' W-IDLOPNRM-X ')'                        
211900          DELIMITED BY SIZE INTO SSA3                                     
212000     MOVE '  ' TO GODK-STATUSKODER                                        
212100     CALL CBLTDLI USING GHU INLE-PCB DLI-IO-WLINLE21                      
212200                                     SSA1 SSA2 SSA3                       
212300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
212400     PERFORM IMS-STATUSKONTROLL                                           
212500     .                                                                    
212600     SKIP3                                                                
212700 IMS-GHNP-INLE-DEL SECTION.                                               
212800     MOVE 'WLINLE31 ' TO SSA1                                             
212900     MOVE '  ' TO GODK-STATUSKODER                                        
213000     CALL CBLTDLI USING GHNP INLE-PCB DLI-IO-WLINLE31 SSA1                
213100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
213200     PERFORM IMS-STATUSKONTROLL                                           
213300     .                                                                    
213400     SKIP3                                                                
213500 IMS-DLET-INLE SECTION.                                                   
213600     MOVE '  ' TO GODK-STATUSKODER                                        
213700     CALL CBLTDLI USING DLET INLE-PCB DLI-IO-WLINLE31                     
213800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
213900     PERFORM IMS-STATUSKONTROLL                                           
214000     .                                                                    
214100     EJECT                                                                
214200 IMS-REPL-INLE SECTION.                                                   
214300     MOVE '  ' TO GODK-STATUSKODER                                        
214400     CALL CBLTDLI USING REPL INLE-PCB DLI-IO-WLINLE21                     
214500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
214600     PERFORM IMS-STATUSKONTROLL                                           
214700     .                                                                    
214800     SKIP3                                                                
214900 IMS-GU-WDL601 SECTION.                                                   
215000     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
215100          DELIMITED BY SIZE INTO SSA1                                     
215200     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-WDL601 SSA1               
215300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     SKIP3                                                                
215700 IMS-GHNP-WDL611 SECTION.                                                 
215800     MOVE 'WDL611   ' TO SSA1                                             
215900     MOVE '  GE' TO GODK-STATUSKODER                                      
216000     CALL CBLTDLI USING GHNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1             
216100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
216200     PERFORM IMS-STATUSKONTROLL                                           
216300     .                                                                    
216400     EJECT                                                                
216500 IMS-GNP-WDL611 SECTION.                                                  
216600     MOVE 'WDL611   ' TO SSA1                                             
216700     MOVE '  GE' TO GODK-STATUSKODER                                      
216800     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1              
216900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
217000     PERFORM IMS-STATUSKONTROLL                                           
217100     .                                                                    
217200     EJECT                                                                
217300 IMS-GHNP-WDL621   SECTION.                                               
217400     STRING 'WDL611  (DAINLEV =>' W-DAINLEV-X ')'                         
217500          DELIMITED BY SIZE INTO SSA1                                     
217600     MOVE 'WDL621   ' TO SSA2                                             
217700     MOVE '  ' TO GODK-STATUSKODER                                        
217800     CALL CBLTDLI USING GHNP WDL6-PCB DLI-IO-AREA-WDL621 SSA1 SSA2        
217900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
218000     PERFORM IMS-STATUSKONTROLL                                           
218100     .                                                                    
218200     SKIP3                                                                
218300 IMS-DLET-WDL621 SECTION.                                                 
218400     MOVE '  ' TO GODK-STATUSKODER                                        
218500     CALL CBLTDLI USING DLET WDL6-PCB DLI-IO-AREA-WDL621                  
218600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
218700     PERFORM IMS-STATUSKONTROLL                                           
218800     .                                                                    
218900     EJECT                                                                
219000 IMS-REPL-WDL611 SECTION.                                                 
219100     MOVE '  ' TO GODK-STATUSKODER                                        
219200     CALL CBLTDLI USING REPL WDL6-PCB DLI-IO-AREA-WDL611                  
219300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
219400     PERFORM IMS-STATUSKONTROLL                                           
219500     .                                                                    
219600     SKIP3                                                                
219700 IMS-ISRT-WDL9 SECTION.                                                   
219800     MOVE 'WLLOGA01 ' TO SSA1                                             
219900     MOVE '  II' TO GODK-STATUSKODER                                      
220000     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
220100     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
220200     PERFORM IMS-STATUSKONTROLL                                           
220300     .                                                                    
220400     SKIP3                                                                
220500 IMS-GU-UPFA01 SECTION.                                                   
220600     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
220700          DELIMITED BY SIZE INTO SSA1                                     
220800     MOVE '  GE' TO GODK-STATUSKODER                                      
220900     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
221000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
221100     PERFORM IMS-STATUSKONTROLL                                           
221200     .                                                                    
221300     SKIP3                                                                
221400 IMS-GU-WDD311 SECTION.                                                   
221500     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
221600             DELIMITED BY SIZE INTO SSA1                                  
221700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
221800              DELIMITED BY SIZE INTO SSA2                                 
221900     MOVE '  GE' TO GODK-STATUSKODER                                      
222000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
222100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
222200     PERFORM IMS-STATUSKONTROLL                                           
222300     .                                                                    
222400     SKIP3                                                                
222500 IMS-GNP-WDK724 SECTION.                                                  
222600     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
222700                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
222800          DELIMITED BY SIZE INTO SSA1                                     
222900     MOVE '  GE' TO GODK-STATUSKODER                                      
223000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
223100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
223200     PERFORM IMS-STATUSKONTROLL                                           
223300     .                                                                    
223400     EJECT                                                                
223500                                                                          
223600 IMS-GU-WLLEVA01 SECTION.                                                 
223700     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
223800     DELIMITED BY SIZE INTO SSA1                                          
223900     MOVE '  GE' TO GODK-STATUSKODER                                      
224000     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
224100     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
224200     PERFORM IMS-STATUSKONTROLL                                           
224300     .                                                                    
224400     SKIP3                                                                
224500                                                                          
224600 IMS-GNP-WLLEVA11 SECTION.                                                
224700     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
224800     DELIMITED BY SIZE INTO SSA1                                          
224900     MOVE '  GE' TO GODK-STATUSKODER                                      
225000     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
225100     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
225200     PERFORM IMS-STATUSKONTROLL                                           
225300     .                                                                    
225400     EJECT                                                                
225500                                                                          
225600 IMS-GU-WDB601 SECTION.                                                   
225700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
225800          DELIMITED BY SIZE INTO SSA1                                     
225900     MOVE '  GE'            TO GODK-STATUSKODER                           
226000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
226100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400     EJECT                                                                
226500                                                                          
226600 IMS-GNP-WDB617    SECTION.                                               
226700     MOVE 'WDB617   ' TO SSA1                                             
226800     MOVE '  GE' TO GODK-STATUSKODER                                      
226900     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
227000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
227100     PERFORM IMS-STATUSKONTROLL                                           
227200     .                                                                    
227300                                                                          
227400 IMS-GU-WDGX9306 SECTION.                                                 
227500     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
227600             DELIMITED BY SIZE INTO SSA1                                  
227700     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
227800             DELIMITED BY SIZE INTO SSA2                                  
227900     MOVE '  GE'   TO GODK-STATUSKODER                                    
228000     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
228100     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
228200     PERFORM IMS-STATUSKONTROLL                                           
228300     .                                                                    
228400     SKIP3                                                                
228500                                                                          
228600 IMS-GNP-WDGX9308 SECTION.                                                
228700     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
228800             DELIMITED BY SIZE INTO SSA1                                  
228900     MOVE '  GE'   TO GODK-STATUSKODER                                    
229000     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
229100     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
229200     PERFORM IMS-STATUSKONTROLL                                           
229300     .                                                                    
229400     SKIP3                                                                
229500                                                                          
229600 IMS-GNP-WDGX9308-FIRST SECTION.                                          
229700     MOVE 'WDGX9308*F' TO SSA1                                            
229800     MOVE '  GE'   TO GODK-STATUSKODER                                    
229900     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
230000     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
230100     PERFORM IMS-STATUSKONTROLL                                           
230200     .                                                                    
230300     SKIP3                                                                
230400                                                                          
230500 IMS-STATUSKONTROLL SECTION.                                              
230600                                                                          
230700     SET STATUS-IX TO 1                                                   
230800     SEARCH GODK-STATUS                                                   
230900       AT END                                                             
231000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
231100         DELIMITED BY SIZE INTO FELTEXT                                   
231200         CALL FELLOG                                                      
231300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
231400         CONTINUE                                                         
231500     END-SEARCH                                                           
231600     .                                                                    
