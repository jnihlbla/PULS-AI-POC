001300 ID DIVISION.                                                             
001400                                                                          
001500 PROGRAM-ID.     W4079300.                                                
001600 AUTHOR.         LARS CALAIS.                                             
001700 DATE-WRITTEN.   95/08/30.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        MPP RECIEVES TRANSACTION WITH DISCREPANCY-IDENTITY AND           
002200*        WRITES A DISCREPANCY LIST.                                       
002300*                                                                         
002410*        THE PROGRAM READS     WLKREE (WDA2)                              
002420*                              WLGMTA (WDB2)                              
002421*                              WLBENA (WDD3)                              
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: W4T793                                              
002800*        MID:         W4I79301                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        MOD:         W4O79301                                            
003200                                                                          
003300                                                                          
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003701*    -- CHECKED BY WY2000                                                 
003710     SKIP3                                                                
003800 77  IDPGM                       PIC X(08)   VALUE 'W4079300'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                             PIC X(80) VALUE SPACE.        
004200                                                                          
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004410 77  WS-KVRADER                  PIC 9(3)    VALUE ZERO.                  
004420 77  WS-KVSIDOR                  PIC 9(3)    VALUE ZERO.                  
004430 77  WS-TODAYS-DATE              PIC 9(6)    VALUE ZERO.                  
004440 77  WS-VALD-PRINTER             PIC X(8)    VALUE '4LAOFF  '.            
004441 77  WS-INTERNUPP                PIC X(22)   VALUE                        
004442                                 '** INTERNUPPACKNING **'.                
004450 77  WS-DUMMY                    PIC X(2)    VALUE SPACE.                 
004500                                                                          
004700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005100                                                                          
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'Y'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  OWN-MID                             VALUE '4793'.                
005800     88  GOOD-MID                            VALUE '4712'.                
006400     EJECT                                                                
006402                                                                          
006403 01  FILLER                      PIC X(16)   VALUE 'HEADLINE-00'.         
006404 01  HLINE00.                                                             
006406     03 FILLER                   PIC X(10)   VALUE 'W40793-001'.          
006407     03 FILLER                   PIC X(4)    VALUE SPACE.                 
006408     03 FILLER                   PIC X(38)   VALUE SPACE.                 
006409     03 FILLER               PIC X(18) VALUE 'LEVERANSANMÄRKNING'.        
006410     03 FILLER                   PIC X(5)    VALUE SPACE.                 
006411     03 HLINE0-INTERNUPP         PIC X(22)   VALUE SPACE.                 
006412     03 FILLER                   PIC X(8)    VALUE SPACE.                 
006413     03 FILLER                   PIC X(5)    VALUE 'SIDA '.               
006414     03 FILLER                   PIC X       VALUE SPACE.                 
006415     03 HLINE0-IDSIDA            PIC Z(3)    VALUE ZERO.                  
006416     03 FILLER                   PIC X(18)   VALUE SPACE.                 
006417                                                                          
006418 01  FILLER                      PIC X(16)   VALUE 'HEADLINE-0'.          
006419 01  HLINE0.                                                              
006420     03 FILLER                   PIC X(5)    VALUE SPACE.                 
006421     03 FILLER                   PIC X(6)    VALUE 'DEALER'.              
006422     03 FILLER                   PIC X(29)   VALUE SPACE.                 
006423     03 FILLER                   PIC X(5)    VALUE 'DISTR'.               
006424     03 FILLER                   PIC X(4)    VALUE SPACE.                 
006425     03 FILLER                   PIC X(6)    VALUE 'KUNDNR'.              
006426     03 FILLER                   PIC X       VALUE SPACE.                 
006427     03 FILLER                   PIC X(9)    VALUE 'RAPPORTNR'.           
006428     03 FILLER                   PIC X(67)   VALUE SPACE.                 
006429                                                                          
006430 01  FILLER                      PIC X(16)   VALUE 'HEADLINE-1'.          
006431 01  HLINE1.                                                              
006432     03 FILLER                   PIC X(5)    VALUE SPACE.                 
006433     03 HLINE1-BEGODSM-1         PIC X(34)   VALUE SPACE.                 
006434     03 FILLER                   PIC X(1)    VALUE SPACE.                 
006435     03 HLINE1-IDDISTR           PIC Z(5).                                
006436     03 FILLER                   PIC X(3)    VALUE SPACE.                 
006437     03 HLINE1-IDKUNDNR          PIC Z(7).                                
006438     03 FILLER                   PIC X(3)    VALUE SPACE.                 
006439     03 HLINE1-IDRAPPNR          PIC Z(6)9.                               
006440     03 FILLER                   PIC X(67)   VALUE SPACE.                 
006441     EJECT                                                                
006442 01  FILLER                      PIC X(16)   VALUE 'HEADLINE-2'.          
006443 01  HLINE2.                                                              
006444     03 FILLER                   PIC X(5)    VALUE SPACE.                 
006445     03 HLINE2-BEGODSM-2         PIC X(35)   VALUE SPACE.                 
006446     03 FILLER                   PIC X(92)  VALUE SPACE.                  
006447     EJECT                                                                
006448 01  FILLER                      PIC X(16)   VALUE 'HEADLINE-3'.          
006449                                                                          
006450 01  HLINE3.                                                              
006451     03 FILLER                   PIC X(5)    VALUE SPACE.                 
006452     03 HLINE3-ADGODSMK-1        PIC X(32)   VALUE SPACE.                 
006453     03 FILLER                   PIC X(1)    VALUE SPACE.                 
006454     03 FILLER                   PIC X(7)    VALUE 'ANM.DAT'.             
006455     03 FILLER                   PIC X(5)    VALUE SPACE.                 
006456     03 FILLER                   PIC X(5)    VALUE 'UTSKR'.               
006457     03 FILLER                   PIC X(77)   VALUE SPACE.                 
006458     EJECT                                                                
006459 01  FILLER                      PIC X(16)   VALUE 'HEADLINE-4'.          
006460                                                                          
006461 01  HLINE4.                                                              
006462     03 FILLER                   PIC X(5)    VALUE SPACE.                 
006463     03 HLINE4-ADGODSMK-2        PIC X(33)   VALUE SPACE.                 
006464     03 FILLER                   PIC X(1)    VALUE SPACE.                 
006465     03 HLINE4-TILEVANM          PIC 9(6).                                
006466     03 FILLER                   PIC X(4)    VALUE SPACE.                 
006467     03 HLINE4-TIAAMMDD          PIC 9(6).                                
006468     03 FILLER                   PIC X(87)   VALUE SPACE.                 
006469     EJECT                                                                
006470                                                                          
006471 01  LLINE-HEADER.                                                        
006472     03 FILLER                   PIC X(3)    VALUE SPACE.                 
006473     03 FILLER                   PIC X(5)    VALUE 'ARTNR'.               
006480     03 FILLER                   PIC X(2)    VALUE SPACE.                 
006490     03 FILLER                   PIC X(5)    VALUE 'RADNR'.               
006491     03 FILLER                   PIC X       VALUE SPACE.                 
006492     03 FILLER                   PIC X(5)    VALUE 'KOLLI'.               
006493     03 FILLER                   PIC X       VALUE SPACE.                 
006494     03 FILLER                   PIC X(5)    VALUE 'ORDNR'.               
006495     03 FILLER                   PIC X       VALUE SPACE.                 
006496     03 FILLER                   PIC X(9)    VALUE 'BENÄMNING'.           
006497     03 FILLER                   PIC X(19)   VALUE SPACE.                 
006498     03 FILLER                   PIC X(5)    VALUE 'ANTAL'.               
006499     03 FILLER                   PIC X       VALUE SPACE.                 
006500     03 FILLER                   PIC X(3)    VALUE 'KOD'.                 
006501     03 FILLER                   PIC X(8)    VALUE SPACE.                 
006502     03 FILLER                   PIC X(4)    VALUE 'PRIS'.                
006503     03 FILLER                   PIC X       VALUE SPACE.                 
006504     03 FILLER                   PIC X(7)    VALUE 'FAKT.NR'.             
006505     03 FILLER                   PIC X       VALUE SPACE.                 
006506     03 FILLER                   PIC X(8)    VALUE 'FAKT.DAT'.            
006507     03 FILLER                   PIC X       VALUE SPACE.                 
006508     03 FILLER                   PIC X(2)    VALUE 'DL'.                  
006509     03 FILLER                   PIC X       VALUE SPACE.                 
006510     03 FILLER                   PIC X(3)    VALUE 'FTG'.                 
006511     03 FILLER                   PIC X       VALUE SPACE.                 
006513     03 FILLER                   PIC X(12)   VALUE 'ANALYS.NR'.           
006514     03 FILLER                   PIC X       VALUE SPACE.                 
006515     03 FILLER                   PIC X(2)    VALUE 'DC'.                  
006516     03 FILLER                   PIC X(15)   VALUE SPACE.                 
006517     EJECT                                                                
006518                                                                          
006519 01  LLINE.                                                               
006520     03 LLINE-IDARTNR            PIC Z(7)9.                               
006521     03 FILLER                   PIC X(2)    VALUE SPACE.                 
006522     03 LLINE-IDRADNR            PIC Z(4)9.                               
006523     03 FILLER                   PIC X       VALUE SPACE.                 
006524     03 LLINE-IDKOLLI            PIC Z(5).                                
006525     03 FILLER                   PIC X       VALUE SPACE.                 
006526     03 LLINE-IDORDNR5           PIC Z(5).                                
006527     03 FILLER                   PIC X       VALUE SPACE.                 
006528     03 LLINE-BEART              PIC X(25).                               
006529     03 FILLER                   PIC X       VALUE SPACE.                 
006530     03 LLINE-KVLEVANM           PIC Z(6)9.                               
006531     03 FILLER                   PIC X       VALUE SPACE.                 
006532     03 LLINE-KDANMORS           PIC X(3).                                
006533     03 FILLER                   PIC X       VALUE SPACE.                 
006534     03 LLINE-PRARTBTO           PIC Z(7)9.99.                            
006535     03 FILLER                   PIC X       VALUE SPACE.                 
006536     03 LLINE-IDFAKT             PIC Z(6)9.                               
006537     03 FILLER                   PIC X(3)    VALUE SPACE.                 
006538     03 LLINE-TIFAKT             PIC Z(6).                                
006539     03 FILLER                   PIC X       VALUE SPACE.                 
006540     03 LLINE-FLDIRLEV           PIC X.                                   
006541     03 FILLER                   PIC X(3)    VALUE SPACE.                 
006542     03 LLINE-IDFTG              PIC Z(2).                                
006543     03 FILLER                   PIC X       VALUE SPACE.                 
006548     03 LLINE-IDANALYS           PIC X(12).                               
006549     03 FILLER                   PIC X       VALUE SPACE.                 
006550     03 LLINE-IDDC               PIC X(2).                                
006551     03 FILLER                   PIC X(15)   VALUE SPACE.                 
006552     EJECT                                                                
006553                                                                          
006560*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERAL-SUBPROGRAM.                                                  
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006710     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007300*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007400*01 -COPY WMEDAREA                                                        
007500                                                                          
007600 01  MESSAGE-CODES.                                                       
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     EJECT                                                                
008200*    --- PARAMETERS FOR SUB PROGRAM W006PRS1                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'W006PRAR'.            
008500                                                                          
008600*01 -COPY W006PRAR                                                        
008610     EJECT                                                                
008620*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008630*                                                                         
008640 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008650                                                                          
008660*01 -COPY WMSGINIT                                                        
008700                                                                          
008800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100                                                                          
009200*01  MID -COPY W4I79301                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500                                                                          
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200                                                                          
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900                                                                          
011300 01  KEYS-TO-DLI.                                                         
011400                                                                          
011401     03  W-IDLEVANM-X.                                                    
011402         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
011403         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
011404         05  W-IDRAPPNR          PIC 9(7)    VALUE ZERO.                  
011405                                                                          
011422     03  W-IDARTNR-X.                                                     
011430         05  W-IDARTNR-BENA      PIC S9(9)   VALUE ZERO COMP-3.           
011431                                                                          
011440     03  W-IDSKYLT-X.                                                     
011450         05  W-IDSKYLT           PIC  X(3)   VALUE 'S  '.                 
011451                                                                          
011460     03  W-IDGMT-X.                                                       
011470         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
011480         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
011500                                                                          
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FOUND                       VALUE '  '.                  
012000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012010     88  SEGMENT-END                         VALUE 'GB'.                  
012100                                                                          
012200 01  GOOD-STATUSCODES.                                                    
012300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400                                                                          
012500 01  SSA1                        PIC X(64).                               
012600 01  SSA2                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNCTION CODES                                               
012900*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013400                                                                          
013500 01  DLI-IO-AREA.                                                         
013600     03  IO-AREA                 PIC X(600)  VALUE SPACE.                 
013701                                                                          
013702     03  WLKREE01 REDEFINES IO-AREA.                                      
013703*        05  -COPY WDA201                                                 
013704     EJECT                                                                
013705     03  WLKREE11 REDEFINES IO-AREA.                                      
013710*        05  -COPY WDA211                                                 
014030     EJECT                                                                
014040     03  WLBENA01 REDEFINES IO-AREA.                                      
014050*        05  -COPY WDD311                                                 
014051     EJECT                                                                
014052 01  DLI-IO-AREA-WDB2.                                                    
014053     03  WLGMTA01.                                                        
014054*        05  -COPY WDB201                                                 
014060     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200                                                                          
014300*01  -COPY W0009   -PRE MSG-                                              
014310*01  -COPY W0009   -PRE ALT-                                              
014400*01  -COPY W0008   -PRE USEA-                                             
014500     05  FILLER                  PIC X.                                   
014601     EJECT                                                                
014602*01  -COPY W0008  -PRE KREE-                                              
014610     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800*01  -COPY W0008  -PRE GMTA-                                              
014801     05  FILLER                  PIC X.                                   
014802     EJECT                                                                
014803*01  -COPY W0008  -PRE BENA-                                              
014804     05  FILLER                  PIC X.                                   
014805     EJECT                                                                
014806 PROCEDURE DIVISION  USING MSG-PCB                                        
014807                           ALT-PCB                                        
014808                           USEA-PCB                                       
014809                           KREE-PCB                                       
014810                           GMTA-PCB                                       
014811                           BENA-PCB.                                      
014812     ENTRY 'DLITCBL' USING MSG-PCB                                        
014813                           ALT-PCB                                        
014820                           USEA-PCB                                       
014830                           KREE-PCB                                       
014840                           GMTA-PCB                                       
014850                           BENA-PCB.                                      
014900                                                                          
015100     PERFORM IMS-GET-MSG                                                  
015200     IF SEGMENT-FOUND                                                     
015300       PERFORM A-INIT                                                     
015400       PERFORM B-CHECK-KEYS                                               
015500       IF KEYS-OK                                                         
016000         PERFORM F-READ-PRINT-INFO                                        
016100       END-IF                                                             
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO               TO RETURN-CODE                               
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT                      SECTION.                                     
017200                                                                          
017300     IF MSG-DOUBLE-TRANSACTIONS                                           
017400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I79301                 
017500       MOVE MSG-IDTRANS-2    TO MFS-IDTRANS                               
017600       MOVE MSG-KDMFSFOR-2   TO MFS-KDMFSFOR                              
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I79301                  
017900       MOVE MSG-IDTRANS-1    TO MFS-IDTRANS                               
018000       MOVE MSG-KDMFSFOR-1   TO MFS-KDMFSFOR                              
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP        TO MFS-KDTRTYP                               
018400     MOVE MSG-IDPFK          TO MFS-IDPFK                                 
018500     MOVE MFS-IDTRANS        TO W-IDTRANS                                 
018600                                                                          
018700     MOVE LOW-VALUE          TO MSG-AREA                                  
018800     MOVE 'W4O79301'         TO MFS-IDMOD                                 
019100                                                                          
019110     ACCEPT WS-TODAYS-DATE   FROM DATE                                    
019200                                                                          
019600     IF OWN-MID                                                           
019700       CONTINUE                                                           
019800     ELSE                                                                 
019900       MOVE SPACE            TO MFS-KDTRTYP                               
020000       MOVE '7'              TO MFS-IDPFK                                 
020100     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 B-CHECK-KEYS                SECTION.                                     
020700                                                                          
020800     MOVE ALL '+'            TO MSGI-WMSGINIT                             
020900     MOVE '001'              TO MSGI-KDCALL                               
021000     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
021010     MOVE '4793'               TO MSGI-IDTRANS                            
021020     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
021100     IF GOOD-MID                                                          
021200       MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                              
021210       MOVE MID-IDKUNDNR-IN  TO MSGI-IDKUNDNR                             
021220       MOVE MID-IDRAPPNR-IN  TO MSGI-IDRAPPNR                             
021300     END-IF                                                               
021400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021500                                                                          
021600     MOVE YES TO KEYS-SW                                                  
021700                                                                          
021800     PERFORM BA-CONTROLL-IDDISTR                                          
021900     PERFORM BB-CONTROLL-IDKUNDNR                                         
021910     PERFORM BC-CONTROLL-IDRAPPNR                                         
022000                                                                          
022800     .                                                                    
023000     EJECT                                                                
023100 BA-CONTROLL-IDDISTR         SECTION.                                     
023110                                                                          
023190     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
023191       MOVE MSGI-IDDISTR     TO W-IDDISTR                                 
023194     ELSE                                                                 
023195       MOVE NOO              TO KEYS-SW                                   
023196     END-IF                                                               
023197                                                                          
023198     .                                                                    
023199     EJECT                                                                
023200                                                                          
023201 BB-CONTROLL-IDKUNDNR        SECTION.                                     
023202                                                                          
023210     IF MSGI-IDKUNDNR           NUMERIC                                   
023211       MOVE MSGI-IDKUNDNR    TO W-IDKUNDNR                                
023214     ELSE                                                                 
023215       MOVE NOO              TO KEYS-SW                                   
023216     END-IF                                                               
023217                                                                          
023218     .                                                                    
023219     EJECT                                                                
023220 BC-CONTROLL-IDRAPPNR        SECTION.                                     
023221                                                                          
023229     IF MSGI-IDRAPPNR           =  SPACE                                  
023230       MOVE NOO              TO KEYS-SW                                   
023231     ELSE                                                                 
023232       MOVE MSGI-IDRAPPNR    TO W-IDRAPPNR                                
023233     END-IF                                                               
023235                                                                          
023236     .                                                                    
023237     EJECT                                                                
023240 F-READ-PRINT-INFO           SECTION.                                     
023300                                                                          
023500     MOVE SPACE              TO WS-DUMMY                                  
023600                                                                          
023700     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
023800                         PRT-OPEN                                         
023900                         WS-VALD-PRINTER                                  
024000                         ALT-PCB                                          
024100                         WS-DUMMY                                         
024300                                                                          
024800     PERFORM IMS-GET-KREE01                                               
024900     IF SEGMENT-FOUND                                                     
024910       MOVE +1               TO WS-KVSIDOR                                
025000       PERFORM FA-GET-BASIC-INFO                                          
025100                                                                          
025101       PERFORM IMS-GNP-KREE11                                             
025102       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                       
025103         PERFORM FB-WRITE-PAGE-INFO                                       
025104         ADD  +1             TO WS-KVSIDOR                                
025105       END-PERFORM                                                        
025106     END-IF                                                               
025107     PERFORM FC-CLOSE-PRINTER                                             
025108     .                                                                    
025109     EJECT                                                                
025110                                                                          
025111 FA-GET-BASIC-INFO           SECTION.                                     
025120                                                                          
025125     MOVE ANM-IDDISTR        TO HLINE1-IDDISTR                            
025126     MOVE ANM-IDKUNDNR       TO HLINE1-IDKUNDNR                           
025127     MOVE ANM-IDRAPPNR       TO HLINE1-IDRAPPNR                           
025129                                                                          
025130     MOVE ANM-DALEVANM (3:6) TO HLINE4-TILEVANM                           
025131     MOVE WS-TODAYS-DATE     TO HLINE4-TIAAMMDD                           
025132                                                                          
025133     MOVE W-IDDISTR          TO W-IDDISTR-B2                              
025134     MOVE W-IDKUNDNR         TO W-IDKUNDNR-B2                             
025135                                                                          
025136     PERFORM IMS-GET-GMTA01                                               
025137     IF SEGMENT-FOUND                                                     
025138        MOVE GMT-BEGMT-RAD1  TO HLINE1-BEGODSM-1                          
025139        MOVE GMT-BEGMT-RAD2  TO HLINE2-BEGODSM-2                          
025140        MOVE GMT-ADGMT-GATA  TO HLINE3-ADGODSMK-1                         
025141        MOVE GMT-ADGMT-PADR  TO HLINE4-ADGODSMK-2                         
025142*       MOVE GMT-ADGMT-LAND  TO HLINE5-ADGODSMK-3                         
025143     ELSE                                                                 
025144        MOVE SPACE           TO HLINE1-BEGODSM-1                          
025145                                HLINE2-BEGODSM-2                          
025146                                HLINE3-ADGODSMK-1                         
025147                                HLINE4-ADGODSMK-2                         
025148*FINNS INTE MEN BÖR LÄGGAS TILL HLINE5-ADGODSMK-3                         
025149     END-IF                                                               
025150     .                                                                    
025151     EJECT                                                                
025152                                                                          
025153 FB-WRITE-PAGE-INFO          SECTION.                                     
025154                                                                          
025155     MOVE WS-KVSIDOR         TO HLINE0-IDSIDA                             
025156     IF LEV-KDANMORS = '97'                                               
025157       MOVE WS-INTERNUPP     TO HLINE0-INTERNUPP                          
025158     ELSE                                                                 
025159       MOVE SPACE            TO HLINE0-INTERNUPP                          
025160     END-IF                                                               
025161                                                                          
025162     PERFORM FBA-WRITE-HEADING                                            
025163                                                                          
025164     PERFORM FBB-WRITE-LINE-HEADER                                        
025165     MOVE +1                 TO WS-KVRADER                                
025166     PERFORM UNTIL SEGMENT-MISSING OR WS-KVRADER > 13                     
025167       PERFORM FBC-WRITE-LINE-INFO                                        
025168                                                                          
025169       PERFORM IMS-GNP-KREE11                                             
025170       ADD  +1               TO WS-KVRADER                                
025180     END-PERFORM                                                          
025200     .                                                                    
025500     EJECT                                                                
025510 FBA-WRITE-HEADING           SECTION.                                     
025511                                                                          
025512     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025513                         PRT-WRITE                                        
025514                         WS-VALD-PRINTER                                  
025515                         ALT-PCB                                          
025516                         PRT-NYSIDA-RAD4                                  
025517                         HLINE00                                          
025518                                                                          
025519     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025520                         PRT-WRITE                                        
025521                         WS-VALD-PRINTER                                  
025522                         ALT-PCB                                          
025523                         PRT-AFTER-2                                      
025524                         HLINE0                                           
025525                                                                          
025526     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025527                         PRT-WRITE                                        
025528                         WS-VALD-PRINTER                                  
025529                         ALT-PCB                                          
025530                         PRT-AFTER-2                                      
025531                         HLINE1                                           
025532                                                                          
025533     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025534                         PRT-WRITE                                        
025535                         WS-VALD-PRINTER                                  
025536                         ALT-PCB                                          
025537                         PRT-AFTER-2                                      
025538                         HLINE2                                           
025539                                                                          
025540     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025541                         PRT-WRITE                                        
025542                         WS-VALD-PRINTER                                  
025543                         ALT-PCB                                          
025544                         PRT-AFTER-2                                      
025545                         HLINE3                                           
025546                                                                          
025547     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025548                         PRT-WRITE                                        
025549                         WS-VALD-PRINTER                                  
025550                         ALT-PCB                                          
025551                         PRT-AFTER-2                                      
025552                         HLINE4                                           
025553                                                                          
025554     .                                                                    
025555     EJECT                                                                
025556 FBB-WRITE-LINE-HEADER       SECTION.                                     
025557                                                                          
025558     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025559                         PRT-WRITE                                        
025560                         WS-VALD-PRINTER                                  
025561                         ALT-PCB                                          
025562                         PRT-AFTER-2                                      
025563                         LLINE-HEADER                                     
025564                                                                          
025565     .                                                                    
025566     EJECT                                                                
025570 FBC-WRITE-LINE-INFO         SECTION.                                     
025588                                                                          
025596                                                                          
025597     MOVE LEV-IDARTNR        TO LLINE-IDARTNR                             
025598                                W-IDARTNR-BENA                            
025599     MOVE LEV-FLDIRLEV       TO LLINE-FLDIRLEV                            
025601     MOVE LEV-IDANALYS       TO LLINE-IDANALYS                            
025602     MOVE LEV-IDDC           TO LLINE-IDDC                                
025603     MOVE LEV-IDFAKT         TO LLINE-IDFAKT                              
025604     MOVE LEV-IDFTG          TO LLINE-IDFTG                               
025605     MOVE LEV-IDKOLLI        TO LLINE-IDKOLLI                             
025606     MOVE LEV-IDORDNR7       TO LLINE-IDORDNR5                            
025607     MOVE LEV-IDRADNR        TO LLINE-IDRADNR                             
025608     MOVE LEV-KDANMORS       TO LLINE-KDANMORS                            
025609     MOVE LEV-KVLEVANM-BEKR  TO LLINE-KVLEVANM                            
025610     MOVE LEV-PRARTBTO       TO LLINE-PRARTBTO                            
025611     MOVE LEV-TIFAKT         TO LLINE-TIFAKT                              
025612     PERFORM IMS-GET-BENA01-SEQ                                           
025613     PERFORM IMS-GNP-BENA11-SEQ                                           
025614     IF SEGMENT-FOUND                                                     
025615       MOVE TEXT-BEART       TO LLINE-BEART                               
025616     ELSE                                                                 
025617       MOVE SPACE            TO LLINE-BEART                               
025618     END-IF                                                               
025619                                                                          
025620     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
025621                         PRT-WRITE                                        
025622                         WS-VALD-PRINTER                                  
025623                         ALT-PCB                                          
025624                         PRT-AFTER-2                                      
025625                         LLINE                                            
025626                                                                          
025630     .                                                                    
025700     EJECT                                                                
025800 FC-CLOSE-PRINTER            SECTION.                                     
026700                                                                          
026800     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
026900                         PRT-CLOSE                                        
027000                         WS-VALD-PRINTER                                  
027100                         ALT-PCB                                          
027200                         WS-DUMMY                                         
027400     .                                                                    
027500     EJECT                                                                
030400* --- IMS SECTIONS ---                                                    
030500                                                                          
030600 IMS-GET-MSG                 SECTION.                                     
030700                                                                          
030800     MOVE '  QC' TO GOOD-STATUSCODES                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSCHECK                                              
031200     .                                                                    
031300                                                                          
032502 IMS-GET-KREE01              SECTION.                                     
032503                                                                          
032504     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
032505          DELIMITED BY SIZE INTO SSA1                                     
032506     MOVE '  GE' TO GOOD-STATUSCODES                                      
032507     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
032508     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
032509     PERFORM IMS-STATUSCHECK                                              
032510     .                                                                    
032511     EJECT                                                                
032512 IMS-GNP-KREE11              SECTION.                                     
032513                                                                          
032514     MOVE 'WLKREE11 '           TO SSA1                                   
032516     MOVE '  GE' TO GOOD-STATUSCODES                                      
032517     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
032518     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
032519     PERFORM IMS-STATUSCHECK                                              
032520     .                                                                    
032600     EJECT                                                                
032610 IMS-GET-BENA01-SEQ          SECTION.                                     
032620                                                                          
032630     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
032640             DELIMITED BY SIZE INTO SSA1                                  
032650     MOVE '    ' TO GOOD-STATUSCODES                                      
032660     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
032670     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
032680     PERFORM IMS-STATUSCHECK                                              
032690     .                                                                    
032691 IMS-GNP-BENA11-SEQ          SECTION.                                     
032692                                                                          
032693     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
032694             DELIMITED BY SIZE INTO SSA1                                  
032695     MOVE '  GE' TO GOOD-STATUSCODES                                      
032696     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
032697     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
032698     PERFORM IMS-STATUSCHECK                                              
032699     .                                                                    
032700     EJECT                                                                
032701 IMS-GET-GMTA01              SECTION.                                     
032702                                                                          
032703     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
032705          DELIMITED BY SIZE INTO SSA1                                     
032709     MOVE '  GE'              TO GOOD-STATUSCODES                         
032711     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-WDB2 SSA1                 
032712     MOVE GMTA-STATUS-CODE    TO STATUS-WS                                
032713     PERFORM IMS-STATUSCHECK                                              
032714     .                                                                    
032715     EJECT                                                                
032720 IMS-STATUSCHECK             SECTION.                                     
032800                                                                          
032900     SET STATUS-IX TO 1                                                   
033000     SEARCH GOOD-STATUS                                                   
033100       AT END                                                             
033200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033300         DELIMITED BY SIZE INTO ERROR-TEXT                                
033400         CALL FELLOG                                                      
033500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
