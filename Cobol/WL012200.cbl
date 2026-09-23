000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL012200.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/06/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*1640522      STEER THE X-DOCK GOODS TO THEEN NET                         
000800*    NAME:       'CARPARTS.LDC.CASEREPORTING2'                            
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        KOLLIVIS PACKNING - RAPPORTERING AV VAD SOM LIGGER               
001200*        WL012200 PROGRAM IS A REPLICA OF W4031500 PROGRAM                
001300*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: WL0122T                                             
001800*        REQUEST:     WL0122I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    WL0122O1                                            
002200*                                                                         
002300* ETRACKER: 2360246 AVOID LOCKING OF ORDERS                               
002400* STORY 3046195: REMOVE IDSYSTEM FROM WDGX4142 OF WDR2                    
002500* STORY 3143960 BUG:PRICE MISSING IN PACK REPORTS                         
002600* STORY 3118644 SOLUTION FOR SENDING NEW DATA IN PACK REPORTS             
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WL012200'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600 77    FELTEXT                   PIC X(80)   VALUE SPACE.                 
004700 77    JA                        PIC X       VALUE 'J'.                   
004800 77    YES                       PIC X       VALUE 'Y'.                   
004900 77    NEJ                       PIC X       VALUE 'N'.                   
005000 77    RAETT                     PIC X       VALUE 'R'.                   
005100 77    FEL                       PIC X       VALUE 'F'.                   
005200 77    SAKNAS                    PIC X       VALUE 'S'.                   
005300 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
005400 77    DATUM-SW                  PIC X       VALUE 'N'.                   
005500 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
005600 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77    INX                       PIC  9(9)   VALUE  0.                    
005900 77    DX                        PIC  9(3)   VALUE  0.                    
006000 77    PLATSIX                   PIC S9(9)   VALUE +0.                    
006100 77    PLATSIX-MAX               PIC S9(9)   VALUE +100.                  
006200 77    PLATSIX-MAX-OEVR          PIC S9(9)   VALUE +20.                   
006300 77    ACK-KOLLI                 PIC S9(9)   VALUE +0.                    
006400 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006500 77    REST-INX                  PIC S9(9)   VALUE +0   COMP SYNC.        
006600 77    BILD-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.        
006700 77    FG-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006800 77    FG-MAX-INDX               PIC S9(9)   VALUE +10  COMP SYNC.        
006900 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
007000 77    KDRC-DISP                 PIC 9(4)    VALUE ZERO.                  
007100 77    WS-IDCOM                  PIC S9(9)   VALUE ZERO COMP-3.           
007200 77    W-IDSHIPM                 PIC 9(7)    VALUE ZERO.                  
007300 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
007400 77    WS-CDC-SE                 PIC X(2)    VALUE '11'.                  
007500 77    WS-TOT-ANT-RADER          PIC S9(3)   VALUE +0    COMP-3.          
007600 77    WS-ANT-RADER-INT          PIC S9(3)   VALUE +0    COMP-3.          
007700 77    WS-ANT-RAD-I-BILD         PIC S9(3)   VALUE +0    COMP-3.          
007800 77    WS-MAX-ANT-RAD-I-BILD     PIC S9(3)   VALUE +200  COMP-3.          
007900 77    WS-RAD-FOM                PIC S9(4)   VALUE +0.                    
008000 77    WS-RAD-TOM                PIC S9(4)   VALUE +0.                    
008100 77    WS-KVLOCK                 PIC S9(3)   VALUE +0    COMP-3.          
008200 77    WS-KVRAM                  PIC S9(3)   VALUE +0    COMP-3.          
008300 77    WS-KVPALL                 PIC S9(3)   VALUE +0    COMP-3.          
008400 77    WS-KDORDKL                PIC S9(1)   VALUE +0    COMP-3.          
008500 77    WS-KORD-IDORDER           PIC S9(7)   VALUE +0    COMP-3.          
008600 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
008700 77    WS-KDFRAKT                PIC 9(2)   VALUE ZERO.                   
008800 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
008900 77    WS-EMBPROF                PIC X(1)   VALUE SPACE.                  
009000 77    WS-FLAUTFAK               PIC X(1)   VALUE SPACE.                  
009100 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
009200 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
009300 77    WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                   
009400 77    WS-TISKPTID               PIC 9(6)   VALUE ZERO.                   
009500 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
009600 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
009700 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
009800 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
009900 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
010000 77    WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.            
010100 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
010200 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
010300 77    WS-IDKOLLI-LR             PIC 9(5)   VALUE ZERO.                   
010400 77    WS-IDKOLLI-NUM            PIC 9(5)   VALUE ZERO.                   
010500 77    WS-IDKOLLI-FOM            PIC 9(5)   VALUE ZERO.                   
010600 77    WS-IDKOLLI-TOM            PIC 9(5)   VALUE ZERO.                   
010700 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
010800 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
010900 77    WS-IDPURAD                PIC 9(4)   VALUE ZERO.                   
011000 77    WS-START-RAD              PIC 9(4)   VALUE ZERO.                   
011100 77    WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                   
011200 77    WS-AKTUELL-RAD            PIC 9(4)   VALUE ZERO.                   
011300 77    WS-KVLEVART               PIC 9(6)   VALUE ZERO.                   
011400 77    WS-ORAD-KVLEVART          PIC 9(6)   VALUE ZERO.                   
011500 77    WS-KDKOLLI                PIC X(8)   VALUE SPACE.                  
011600 77    FILLER                    PIC X(8)    VALUE 'FFFFFFFF'.            
011700 77    WS-EMB-VKTARA-ONE-CASE    PIC S9(6)V9 VALUE ZERO  COMP-3.          
011800 77    WS-EMB-VKTARA-TOT-ORDER   PIC S9(6)V9 VALUE ZERO  COMP-3.          
011900 77    WS-EMB-VKTARA-NUM         PIC  9(6)   VALUE ZERO.                  
012000 77    WS-KOLLI-VKORDNTO         PIC S9(6)V9 VALUE ZERO  COMP-3.          
012100 77    WS-MOD-VKORDBTO           PIC 9(6)V9 VALUE ZERO.                   
012200 77    WS-VLORDBTO               PIC 9(4)V9(3)  VALUE ZERO.               
012300 77    WS-KDEMBTYP               PIC 9(2)   VALUE ZERO.                   
012400 77    WS-DIKOLLIL               PIC 9(4)   VALUE ZERO.                   
012500 77    WS-MOD-DIKOLLIL           PIC 9(4)   VALUE ZERO.                   
012600 77    WS-DIKOLLIB               PIC 9(3)   VALUE ZERO.                   
012700 77    WS-MOD-DIKOLLIB           PIC 9(3)   VALUE ZERO.                   
012800 77    WS-DIKOLLIH               PIC 9(3)   VALUE ZERO.                   
012900 77    WS-MOD-DIKOLLIH           PIC 9(3)   VALUE ZERO.                   
013000 77    WS-KDKOLLID               PIC X(1)   VALUE 'L'.                    
013100 77    WS-ADFLGEO                PIC X(3)   VALUE SPACE.                  
013200 77    WS-ADFLOMR                PIC 9(3)   VALUE ZERO.                   
013300 77    WS-ADRUTNIV               PIC 9(3)   VALUE ZERO.                   
013400 77    WS-REST                   PIC 9(4)   VALUE ZERO.                   
013500 77    FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.            
013600 77    WS-ODEL-IDTRP             PIC X(5)   VALUE SPACE.                  
013700 77    WS-TRAEFF-PACKARE         PIC X(01).                               
013800 77    WS-TRAEFF-RAD             PIC X(01).                               
013900 77    WS-RADER-OK               PIC X(01)  VALUE 'N'.                    
014000 77    WS-RADER-SAKNAS           PIC X(01).                               
014100 77    WS-RADER-RAPPORTERADE     PIC X(01).                               
014200 77    MAX-RAD-ANTAL-PLUS-1      PIC S9(3)  VALUE +200 COMP-3.            
014300 77    WS-KVPTID-MIN             PIC S9(7)  VALUE ZERO COMP-3.            
014400 77    WS-KVPTID-TIM             PIC S9(3)  VALUE ZERO COMP-3.            
014500 77    WS-PRT-KDSVAR-ADRESSFL    PIC X(1)   VALUE SPACE.                  
014600 77    WS-PRT-KDSVAR-FOLJEFL     PIC X(1)   VALUE SPACE.                  
014700 77    WS-PACKARES-ODEL-REDAN-KLARA PIC X.                                
014800 77    WS-TIORDREG-NUM6          PIC 9(6)   VALUE ZERO.                   
014900 77    WS-DARFS                  PIC 9(12) VALUE ZERO.                    
015000*                                        ANTAL FÄRDIGPACKADE RADER        
015100*                                        I ETT RAD-INTERVALL.             
015200 77    WS-RINT-ANT-FPACK-ORAD    PIC S9(5)  VALUE ZERO COMP-3.            
015300 77    FILLER                    PIC  X(08) VALUE 'HHHHHHHH'.             
015400 77    WS-SPAR-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
015500 77    WS-SPAR-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
015600 77    WS-SPAR-IDDC              PIC  X(02) VALUE ZERO.                   
015700 77    WS-SPAR-BEART             PIC  X(25) VALUE SPACE.                  
015800 77    WS-SPAR-KVBEART           PIC S9(07) VALUE ZERO COMP-3.            
015900 77    WS-SPAR-FLTILLK           PIC  X(01) VALUE SPACE.                  
016000 77    WS-SPAR-IDKUNDRF-RO       PIC  X(10) VALUE SPACE.                  
016100 77    WS-SPAR-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
016200*                                        ANTAL FÄRDIGPACKADE RADER        
016300*                                        I ETT RAD-INTERVALL.             
016400 77    FILLER                    PIC X(8)    VALUE 'IIIIIIII'.            
016500                                                                          
016600 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
016700 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
016800 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
016900 77  TEMFSINF1                   PIC X(61).                               
017000                                                                          
017100 77  WS-IDELMT-ERROR             PIC X(16).                               
017200 77  WS-IDMSG-ERROR              PIC X(03).                               
017300 77  WS-IDMSG-INFO               PIC X(03).                               
017400                                                                          
017500 77    WS-IDLEVNR                PIC X(5)   VALUE SPACES.                 
017600 77    WS-ODEL-IDDC-EXP          PIC X(2)   VALUE SPACES.                 
017700                                                                          
017800 01  TRANSFER-KUND               PIC 9(7).                                
017900     88 TRANSFER-KUNDNR          VALUE 0000511                            
018000                                       0000512                            
018100                                       0000513.                           
018200     88  RETUR-KUNDNR            VALUE 0000051.                           
018300*                                                                         
018400 01  WS-KDMATT                   PIC X.                                   
018500     88 US-MEASUREMENT           VALUE 'U'.                               
018600     88 SIS-MEASUREMENT          VALUE 'S'.                               
018700*                                                                         
018800 01    WS-VKORDBTO-RED         PIC 9(6).9  VALUE ZERO.                    
018900 01    FILLER           REDEFINES WS-VKORDBTO-RED.                        
019000       05  WS-VKORDBTO-KG      PIC 9(6).                                  
019100       05  WS-VKORDBTO-PUNKT   PIC X.                                     
019200       05  WS-VKORDBTO-DEC     PIC 9.                                     
019300                                                                          
019400 01  WS-IDPRTLST.                                                         
019500     03 WS-SYSTDEL               PIC X(1).                                
019600     03 WS-LISTTYP               PIC X(2).                                
019700     03 WS-DC                    PIC X(2).                                
019800     03 WS-KDPRT                 PIC X(3).                                
019900 01  WS-IDPRTJAP REDEFINES WS-IDPRTLST.                                   
020000     03 WS-LASER-BLANKETT        PIC X(6).                                
020100     03 WS-NDC-JAP-KDPRT         PIC X(2).                                
020200                                                                          
020300 77    ABEND-MED1                PIC X(80)  VALUE                         
020400      'ABEND-ORSAK:FEL I TRANS FRÅN ORDVIS,STARTA OM 4315 & 0605'.        
020500     SKIP2                                                                
020600                                                                          
020700     SKIP2                                                                
020800 77    WS-INDATA-TEST            PIC X(01).                               
020900   88  WS-INDATA-FEL                        VALUE 'F'.                    
021000   88  WS-INDATA-RATT                       VALUE 'R'.                    
021100     SKIP2                                                                
021200 77    WS-BEHANDLING-TEST        PIC X(01).                               
021300   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
021400   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
021500     SKIP2                                                                
021600 77    FL-RAD-INTERVALL          PIC X(01).                               
021700   88  INTERVALL-RAD                        VALUE 'J'.                    
021800   88  AVVIKELSE-RAD                        VALUE 'N'.                    
021900     SKIP2                                                                
022000 77    FL-KOLLI-INTERVALL        PIC X(01).                               
022100   88  KOLLI-INTERVALL                      VALUE 'J'.                    
022200   88  EJ-KOLLI-INTERVALL                   VALUE 'N'.                    
022300     SKIP2                                                                
022400 77    FL-RAD-INOM-INTERVALL     PIC X(01).                               
022500   88  RAD-FINNS-I-INTERVALL                VALUE 'J'.                    
022600     SKIP2                                                                
022700 77    FL-SLINGA-KLAR            PIC X(01).                               
022800   88  SLINGA-KLAR                          VALUE 'J'.                    
022900                                                                          
023000 77    SW-KOLLI-HITTAT           PIC X(01).                               
023100   88  KOLLI-HITTAT                         VALUE 'J'.                    
023200                                                                          
023300 77    SW-TIKLAR-UPPDATERAD      PIC X(01).                               
023400                                                                          
023500 77    DIRLEV-KOLLI-SW           PIC X(01).                               
023600   88  DIRLEV-KOLLI                         VALUE 'J'.                    
023700                                                                          
023800 77    NYA-NYCKLAR-SW            PIC X(01).                               
023900   88  NYA-NYCKLAR                          VALUE 'J'.                    
024000                                                                          
024100 77    FILLER                    PIC X(8)    VALUE 'JJJJJJJJ'.            
024200 01     WS-TEMFSINF              PIC X(61)  VALUE SPACE.                  
024300     SKIP2                                                                
024400 01     WS-IDKUNDRF.                                                      
024500   03   WS-IDORDNR              PIC X(5).                                 
024600   03   FILLER                  PIC X(5)    VALUE SPACE.                  
024700 01     WS-JFR-IDANSTNR.                                                  
024800   03   FILLER                    PIC X(3).                               
024900   03   WS-JFR-IDANSTNR-5         PIC X(5).                               
025000     SKIP3                                                                
025100 01     WS-SUPTID-PRAPP         PIC 9(3)V99.                              
025200 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
025300   03   WS-SUPTID-TIM           PIC 9(3).                                 
025400   03   WS-SUPTID-MIN           PIC 9(2).                                 
025500     SKIP3                                                                
025600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
025700 01  FILLER REDEFINES DAGENS-DATUM.                                       
025800     03  DAGENS-AA               PIC 9(2).                                
025900     03  DAGENS-MM               PIC 9(2).                                
026000     03  DAGENS-DD               PIC 9(2).                                
026100                                                                          
026200 01  WS-IDDC-LOCAL.                                                       
026300     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
026400     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
026500     03  FILLER                  PIC X(1)   VALUE SPACE.                  
026600                                                                          
026700 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
026800 01  FILLER REDEFINES DAGENS-TID.                                         
026900     03  DAGENS-TID-HHMMSS       PIC 9(6).                                
027000     03  FILLER                  PIC 9(2).                                
027100                                                                          
027200 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
027300 01    ALL-PLUS.                                                          
027400   03 FILLER                     PIC X(30)  VALUE                         
027500        '++++++++++++++++++++++++++++++'.                                 
027600                                                                          
027700     EJECT                                                                
027800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
027900 01  GENERAL-SUBPROGRAMS.                                                 
028000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
028200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
028300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
028500     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
028600     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
028700     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
028800     03  W403TMS1                PIC X(8)    VALUE 'W403TMS1'.            
028900     SKIP3                                                                
029000*    --- PARAMETERS TO ABEND                                              
029100                                                                          
029200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
029300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
029400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
029500     EJECT                                                                
029600*                                                                         
029700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
029800     SKIP3                                                                
029900*01  -COPY WZ01SUB                                                        
030000     EJECT                                                                
030100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
030200     SKIP3                                                                
030300 01  REQU-AREA.                                                           
030400*    03  -COPY WZ01REQU                                                   
030500*    03  -COPY WL0122I1                                                   
030600     EJECT                                                                
030700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
030800     SKIP3                                                                
030900 01  RESP-AREA.                                                           
031000*    03  -COPY WZ01RESP                                                   
031100*    03  -COPY WL0122O1                                                   
031200     EJECT                                                                
031300                                                                          
031400*01  -COPY WL01TIDZ                                                       
031500     EJECT                                                                
031600*                                                                         
031700*TMS PACKNING INFO ECOM                                                   
031800 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
031900*    -COPY W403TMS1                                                       
032000*                                                                         
032100 01  GEMENSAMMA-SUBPROGRAM.                                               
032200     03  W411DNOT               PIC X(8)    VALUE 'W411DNOT'.             
032300*                                                                         
032400*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
032500*                                                                         
032600 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
032700     EJECT                                                                
032800 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
032900*01 -COPY WDECAREA                                                        
033000     EJECT                                                                
033100 01  FILLER                     PIC X(16)   VALUE 'W403PLAT '.            
033200*   -COPY W403PLAT                                                        
033300     EJECT                                                                
033400 01  FILLER                     PIC X(16)   VALUE 'W411DNOT '.            
033500*   -COPY W411DNOT                                                        
033600     EJECT                                                                
033700 01  FILLER                     PIC X(16)   VALUE 'WWOMVAND '.            
033800*   -COPY WWOMVAND                                                        
033900     SKIP3                                                                
034000 01  FILLER                     PIC X(16)   VALUE 'WSTAB-PLATS'.          
034100 01  WSTAB-PLATS.                                                         
034200     05 WS-INGANG  OCCURS 100.                                            
034300        10 WSTAB-IDTRPTNR       PIC S9(3)   COMP-3.                       
034400        10 WSTAB-ADFLGEO        PIC X(3).                                 
034500        10 WSTAB-ADFLOMR        PIC S9(3)   COMP-3.                       
034600        10 WSTAB-ADRUTNIV       PIC S9(3)   COMP-3.                       
034700        10 WSTAB-DIHMODUL       PIC S9(3)   COMP-3.                       
034800        10 WSTAB-DIDMODUL       PIC S9(3)   COMP-3.                       
034900        10 WSTAB-ADVMODUL       PIC S9(3)   COMP-3.                       
035000        10 WSTAB-ADHMODUL       PIC S9(3)   COMP-3.                       
035100        10 WSTAB-FLUTLAST       PIC X(1).                                 
035200        10 WSTAB-IDDC-CROSS     PIC X(2).                                 
035300*                                                                         
035400 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
035500 01     HJALP-ODEL-DARFS        PIC 9(12).                                
035600 01     FILLER                  REDEFINES HJALP-ODEL-DARFS.               
035700   03   FILLER                  PIC  9(2).                                
035800   03   HJALP-ODEL-DARFS-6      PIC  9(6).                                
035900   03   FILLER                  PIC  9(4).                                
036000     SKIP2                                                                
036100 01     HJALP-4472-TIRFS        PIC 9(11).                                
036200 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
036300   03   FILLER                  PIC  9(1).                                
036400   03   HJALP-4472-TIRFS-6      PIC  9(6).                                
036500   03   FILLER                  PIC  9(4).                                
036600     EJECT                                                                
036700 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREOR'.           
036800 01     SPAR-AREOR.                                                       
036900   03   SPAR1-INTERVALL-AREA.                                             
037000     05 SPAR1-IDRADNR-FOM       PIC  9(5).                                
037100     05 SPAR1-IDRADNR-TOM       PIC  9(5).                                
037200     05 SPAR1-KVORAPP           PIC  9(7).                                
037300     05 SPAR1-FLNOLLJ           PIC  X(1).                                
037400*                                                                         
037500   03   SPAR2-INTERVALL-AREA.                                             
037600     05 SPAR2-IDRADNR-FOM       PIC  9(5).                                
037700     05 SPAR2-IDRADNR-TOM       PIC  9(5).                                
037800     05 SPAR2-KVORAPP           PIC  9(7).                                
037900     05 SPAR2-FLNOLLJ           PIC  X(1).                                
038000*                                                                         
038100   03   SPAR-PRAD-UPPG-AREA.                                              
038200     05 SPAR-PRAD-VKARTNTO      PIC  9(6)V9(3)    VALUE ZERO.             
038300     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
038400     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
038500     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
038600     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
038700     05 SPAR-PRAD-PRAVCOST      PIC  S9(9)V9(2)   VALUE ZERO.             
038800     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
038900     05 SPAR-PRAD-PRARTNTO-LOCPREL  PIC  S9(9)V9(2)   VALUE ZERO.         
039000     05 SPAR-PRAD-KDVALISO      PIC X(3)          VALUE SPACE.            
039100     05 SPAR-PRAD-KDVALISO-EXP  PIC X(3)          VALUE SPACE.            
039200*                                                                         
039300   03   SPAR-FARLIGT-GODS-DATA.                                           
039400     05 SPAR-IDPSN              PIC  9(3)                VALUE 0.         
039500     05 SPAR-VKART-FG           PIC  S9(7)        COMP-3 VALUE 0.         
039600     05 SPAR-VLFG               PIC  S9(4)V9(3)   COMP-3 VALUE 0.         
039700     05 SPAR-SUEQFG             PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
039800*                                                                         
039900     05 TOTAL-SUEQFG            PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
040000     EJECT                                                                
040100 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
040200 01     ARBETSAREOR.                                                      
040300   03   ARB-AREA-RAD.                                                     
040400     05 ARB-RAD-FOM             PIC  9(4).                                
040500     05 ARB-RAD-TOM             PIC  9(4).                                
040600     05 ARB-RAD-AKTUELL         PIC  9(4).                                
040700     05 ARB-KVLEVART            PIC  9(6).                                
040800     SKIP2                                                                
040900   03   ARB-KOLLI-UPPG-AREA.                                              
041000     05 ARB-KOLLI-VKORDNTO       PIC  9(6)V9(3)   VALUE ZERO.             
041100     05 ARB-KOLLI-KVFLAMP        PIC  S9(2)V9(1)  VALUE ZERO.             
041200     05 ARB-KOLLI-KDFARLIG       PIC  S9          VALUE ZERO.             
041300     05 ARB-KOLLI-KVORDRAD       PIC  S9(5)       VALUE ZERO.             
041400     05 ARB-KOLLI-KVFALRAD       PIC  S9(5)       VALUE ZERO.             
041500     05 ARB-KOLLI-SUORDV         PIC  S9(9)V9(2)  VALUE ZERO.             
041600     05 ARB-KOLLI-SUORDV-EXP     PIC  S9(9)V9(2)  VALUE ZERO.             
041700     05 ARB-KOLLI-SUORDV-LOC     PIC  S9(9)V9(2)  VALUE ZERO.             
041800     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)  VALUE ZERO.             
041900     05 WS-ORAD-VLORDNTO         PIC  9(8)        VALUE ZERO.             
042000     05 WS-ORAD-VLORDNTO-SUM     PIC  9(8)        VALUE ZERO.             
042100     05 WS-KOLLI-VLORDBTO        PIC  9(8)        VALUE ZERO.             
042200     05 FILLER                   PIC X(8)    VALUE 'TTTTTTTT'.            
042300     05 ARB-KOLLI-KDVALISO       PIC X(3)    VALUE SPACE.                 
042400     05 ARB-KOLLI-KDVALISO-EXP   PIC X(3)    VALUE SPACE.                 
042500     SKIP2                                                                
042600   03   ARB-ADRESS.                                                       
042700     05 ARB-ADFLGEO             PIC  X(3)   VALUE SPACE.                  
042800     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
042900     05 ARB-ADFLOMR             PIC  9(3)   VALUE ZERO.                   
043000     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
043100     05 ARB-ADRUTNIV            PIC  9(3)   VALUE ZERO.                   
043200     SKIP2                                                                
043300   03   ARB-ANTAL-KOLLI-PLUS-1   PIC 9(5)    VALUE ZERO.                  
043400   03   ARB-ANTAL-KOLLI          PIC 9(5)    VALUE ZERO.                  
043500     SKIP2                                                                
043600 01     WS-TIDPUNKT-RED.                                                  
043700   03   WS-HHMMSS               PIC  9(6).                                
043800   03   WS-DD                   PIC  9(2).                                
043900     EJECT                                                                
044000*01   -COPY WWDC04                                                        
044100                                                                          
044200 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
044300 01     FILLER REDEFINES TEST-IDDISTR.                                    
044400*  03   -COPY WWDIST03.                                                   
044500     SKIP2                                                                
044600 01     FILLER REDEFINES TEST-IDDISTR.                                    
044700*  03   -COPY WWDIST07.                                                   
044800     SKIP2                                                                
044900 01     FILLER REDEFINES TEST-IDDISTR.                                    
045000*  03   -COPY WWDIST08.                                                   
045100     SKIP2                                                                
045200 01     FILLER REDEFINES TEST-IDDISTR.                                    
045300*  03   -COPY WWDIST18.                                                   
045400     SKIP2                                                                
045500 01     FILLER REDEFINES TEST-IDDISTR.                                    
045600*  03   -COPY WWDIST19.                                                   
045700     SKIP2                                                                
045800 01     FILLER REDEFINES TEST-IDDISTR.                                    
045900*  03   -COPY WWDIST21.                                                   
046000     SKIP2                                                                
046100 01     FILLER REDEFINES TEST-IDDISTR.                                    
046200*  03   -COPY WWDIST35.                                                   
046300     SKIP2                                                                
046400 01     FILLER REDEFINES TEST-IDDISTR.                                    
046500*  03   -COPY WWDIST79.                                                   
046600     SKIP2                                                                
046700 01     FILLER REDEFINES TEST-IDDISTR.                                    
046800*  03   -COPY WWDIST85.                                                   
046900 01     FILLER REDEFINES TEST-IDDISTR.                                    
047000*  03   -COPY WWDIST44.                                                   
047100*    ----DISTR-DEALER-PRICE----                                           
047200     SKIP2                                                                
047300*01    FILLER  -COPY WWDIS128      -RED TEST-IDDISTR.                     
047400     EJECT                                                                
047500 01    TEST-IDKUNDNR             PIC 9(7)         COMP-3.                 
047600 01  FILLER                    PIC X(08) VALUE 'FRAK-010'.                
047700*01    FILLER  -COPY WWFRAKT1                                             
047800     SKIP2                                                                
047900 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
048000 01    NYCKLAR-TILL-DLI.                                                  
048100*                                                                         
048200   03    W-WDE4A1-KUNDORDER-X.                                            
048300     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
048400     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
048500     05    W-4A1-IDKUNDRF.                                                
048600       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
048700       07  FILLER                PIC X(05)   VALUE SPACE.                 
048800*                                                                         
048900   03    W-WDE401-KUNDORDER-X.                                            
049000     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
049100     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
049200     05    W-401-IDKUNDRF.                                                
049300       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
049400       07  FILLER                PIC X(05)   VALUE SPACE.                 
049500     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
049600     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
049700*                                                                         
049800   03    W-WDE4B-KEYSEQ-MIN-X.                                            
049900     05    W-420-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
050000     05    W-420-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
050100*                                                                         
050200   03    W-WDE4B-KEYSEQ-MAX-X.                                            
050300     05    W-420-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
050400     05    W-420-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
050500*                                                                         
050600   03    W-WDE4B-KEYSEQ-X.                                                
050700     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
050800     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
050900*                                                                         
051000   03    W-WDE411-IDPURAD-X.                                              
051100     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
051200*                                                                         
051300   03    W-WDE421-IDKOLLI-X.                                              
051400     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
051500     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
051600*                                                                         
051700   03    W-WDE601-IDPRODNR-X.                                             
051800     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
051900*                                                                         
052000   03    W-WDE611-IDKOLLI-X.                                              
052100     05    W-611-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
052200*                                                                         
052300   03    W-4726-WDGXKEY-ROT-X.                                            
052400     05    W-4726-IDHTYP         PIC X(4)    VALUE '4726'.                
052500     05    W-4726-FLBATCH        PIC X(1)    VALUE SPACE.                 
052600     05    W-4726-LOWVALUE       PIC X(25)   VALUE LOW-VALUE.             
052700*                                                                         
052800   03    W-4726-WDGXKEY-UNDSEG-X.                                         
052900     05    W-4726-IDDISTR        PIC S9(5)   COMP-3.                      
053000     05    W-4726-IDKUNDNR       PIC S9(7)   COMP-3.                      
053100     05    W-4726-IDDC           PIC X(2).                                
053200     05    W-4726-KDFAKTYP       PIC X.                                   
053300*                                                                         
053400   03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
053500*                                                                         
053600   03    W-4321-IDHTYP-X.                                                 
053700         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
053800         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
053900*                                                                         
054000   03    W-WDQ201-X.                                                      
054100     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
054200*                                                                         
054300   03    W-IDDC-X.                                                        
054400     05    W-IDDC                PIC X(2).                                
054500*                                                                         
054600   03    W-WDQ2CSEQ-X.                                                    
054700     05    W-WDQ2C-IDGMTREF-X.                                            
054800       07    W-WDQ2C-IDDISTR     PIC S9(5) COMP-3 VALUE +0.               
054900       07    W-WDQ2C-IDKUNDNR    PIC S9(7) COMP-3 VALUE +0.               
055000       07    W-WDQ2C-IDKUNDRF.                                            
055100         09    FILLER            PIC  9(2)        VALUE ZERO.             
055200         09    W-WDQ2C-IDORDNR5                                           
055300                                 PIC  9(5)        VALUE ZERO.             
055400         09    FILLER            PIC  X(3)        VALUE SPACE.            
055500*                                                                         
055600   03    W-WDQ301-ORDERDEL-X.                                             
055700     05    W-301-IDORDER         PIC S9(7) COMP-3.                        
055800     05    W-301-IDDC            PIC X(2).                                
055900     05    W-301-IDPRODNR        PIC S9(7) COMP-3.                        
056000     05    W-301-IDPLKLST        PIC S9(3) COMP-3.                        
056100*                                                                         
056200   03    W-4471-WDGXKEY-X.                                                
056300     05    W-4471-IDHTYP         PIC X(4)  VALUE '4471'.                  
056400     05    W-4471-IDDC           PIC X(2).                                
056500     05    W-4471-IDPRC.                                                  
056600       07    W-4471-IDPRCBAS     PIC X(3).                                
056700       07    W-4471-IDPRCVAR     PIC X(1).                                
056800     05    FILLER                PIC X(20) VALUE LOW-VALUE.               
056900*                                                                         
057000   03    W-4472-KDSEGKEY-X.                                               
057100     05    W-4472-KDSEGKEY       PIC X(1)  VALUE '1'.                     
057200*                                                                         
057300   03    W-4477-WDGXKEY-X.                                                
057400     05    W-4477-IDHTYP         PIC X(4)  VALUE '4477'.                  
057500     05    W-4477-IDDC           PIC X(2).                                
057600     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
057700*                                                                         
057800   03    W-4478-WDGXKEY-X.                                                
057900     05    W-4478-IDSHIFT        PIC X(1).                                
058000     05    W-4478-IDUSER         PIC X(8).                                
058100     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
058200*                                                                         
058300     03  W-IDDC-B6-X.                                                     
058400         05 W-IDDC-B6                  PIC X(2).                          
058500                                                                          
058600   03    W-WDA601KY-MIN-X.                                                
058700     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
058800     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
058900     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
059000     05    FILLER                    PIC X(03) VALUE SPACE.               
059100     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
059200     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
059300     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
059400     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
059500     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
059600     SKIP2                                                                
059700   03    W-WDA601KY-MAX-X.                                                
059800     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
059900     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
060000     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
060100     05    FILLER                    PIC X(03) VALUE SPACE.               
060200     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
060300     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
060400     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
060500     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
060600     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
060700*                                                                         
060800   03    W-IDARTNR-X.                                                     
060900     05    W-IDARTNR                 PIC S9(9) VALUE ZERO COMP-3.         
061000   03    W-KDSEGKEY-X.                                                    
061100     05    W-KDSEGKEY                PIC X(1)  VALUE '1'.                 
061200     EJECT                                                                
061300*                                                                         
061400     EJECT                                                                
061500     SKIP3                                                                
061600 01    MEDDELANDE.                                                        
061700    03 FEL-827.                                                           
061800       05  FILLER       PIC X(4)  VALUE 'MAX.'.                           
061900       05  FEL-PLATSIX-C2  PIC 9(3).                                      
062000       05  FILLER       PIC X(9) VALUE 'CASES/INT'.                       
062100**********************************************                            
062200   03   FEL-XX-MISSING            PIC  X(03)  VALUE '041'.                
062300   03   FEL-INVALID-KEY-XX        PIC  X(03)  VALUE '022'.                
062400   03   FEL-CASE-AND-INTERVAL     PIC  X(03)  VALUE '136'.                
062500   03   FEL-SYSTEM-ERROR          PIC  X(03)  VALUE '099'.                
062600   03   FEL-CASE-INTERVAL         PIC  X(03)  VALUE '137'.                
062700   03   FEL-MAX-100-CASES         PIC  X(03)  VALUE '138'.                
062800   03   FEL-MAX-50-CASES          PIC  X(03)  VALUE '139'.                
062900   03   FEL-MORE-CASE-INFO-NEEDED PIC  X(03)  VALUE '140'.                
063000   03   FEL-ENTER-INTERVAL        PIC  X(03)  VALUE '141'.                
063100   03   FEL-WRONG-INTERVAL        PIC  X(03)  VALUE '142'.                
063200   03   FEL-TOO-MANY-LINES        PIC  X(03)  VALUE '129'.                
063300   03   FEL-ONLY-ONE-LINE         PIC  X(03)  VALUE '144'.                
063400   03   FEL-MORE-THAN-ONE-INT     PIC  X(03)  VALUE '145'.                
063500   03   FEL-ORDER-REPORTED        PIC  X(03)  VALUE '146'.                
063600   03   FEL-CASE-INT-REPORTED     PIC  X(03)  VALUE '147'.                
063700   03   FEL-PACK-NOT-ALLOWED      PIC  X(03)  VALUE '148'.                
063800   03   FEL-CASE-REPORTED         PIC  X(03)  VALUE '149'.                
063900   03   FEL-PACKER-ORDER          PIC  X(03)  VALUE '150'.                
064000   03   FEL-ORDER-PART-READY      PIC  X(03)  VALUE '151'.                
064100   03   FEL-DEV-IN-PROGRESS       PIC  X(03)  VALUE '152'.                
064200   03   FEL-GEN-ADRESS            PIC  X(03)  VALUE '154'.                
064300   03   FEL-MIX-CASE-OTHER-TRP    PIC  X(03)  VALUE '156'.                
064400   03   FEL-MIX-CASE-NO-TRP       PIC  X(03)  VALUE '157'.                
064500   03   FEL-XT                    PIC  X(03)  VALUE '158'.                
064600   03   FEL-DISTR-NOT-MIXED       PIC  X(03)  VALUE '159'.                
064700   03   FEL-INT-NO-PACKER         PIC  X(03)  VALUE '160'.                
064800   03   FEL-ORDER-NOT-SPLIT       PIC  X(03)  VALUE '161'.                
064900   03   FEL-INTERVAL-REPORTED     PIC  X(03)  VALUE '162'.                
065000   03   FEL-LINE-ZEROED           PIC  X(03)  VALUE '163'.                
065100   03   FEL-NO-DANGEROUS          PIC  X(03)  VALUE '164'.                
065200   03   FEL-LARGE-QUANT           PIC  X(03)  VALUE '165'.                
065300   03   FEL-LINE-QUANT            PIC  X(03)  VALUE '166'.                
065400*                                                                         
065500   03   FEL-LINE-VOLUME           PIC  X(03)  VALUE '166'.                
065600   03   UPDATE-DONE               PIC  X(03)  VALUE '001'.                
065700   03   FEL-ZERO-NOT-ALLOWED      PIC  X(03)  VALUE '168'.                
065800   03   FEL-DC-NOT-MATCH          PIC  X(03)  VALUE '289'.                
065900   03   FEL-USESCREEN-L0197-ORL0199 PIC X(03) VALUE '192'.                
066000   03   ERR-ORDER-HAS-WRONG-STATUS  PIC X(03) VALUE '273'.                
066100   03   ERR-NOT-CASE-INTERVALL-ECOM PIC X(03) VALUE '425'.                
066200*                                                                         
066300     EJECT                                                                
066400 01    FILLER                 PIC X(16) VALUE 'EMB-TABELL'.               
066500     SKIP3                                                                
066600 01    EMB-TABELL.                                                        
066700   03    EMB-TAB-X.                                                       
066800     05  KLASS        OCCURS 3   INDEXED BY KL-INDX.                      
066900         07  TYP      OCCURS 5   INDEXED BY TYP-INDX                      
067000                                         PIC S9(5)  COMP-3.               
067100   03    EMB-TAB   REDEFINES  EMB-TAB-X.                                  
067200     05  FILLER.                                                          
067300         07  PALLAR   OCCURS 5           PIC S9(5)  COMP-3.               
067400     05  FILLER.                                                          
067500         07  KRAGAR   OCCURS 5           PIC S9(5)  COMP-3.               
067600     05  FILLER.                                                          
067700         07  EMB-LOCK OCCURS 5           PIC S9(5)  COMP-3.               
067800     EJECT                                                                
067900 01    FILLER                 PIC X(16) VALUE 'FG-TABELL'.                
068000                                                                          
068100 01    FG-TABELL.                                                         
068200   03    TAB-POST OCCURS 10.                                              
068300                                                                          
068400     05  TAB-IDPSN            PIC 9(3)              VALUE ZERO.           
068500                                                                          
068600     05  TAB-VKART-FG         PIC S9(7)      COMP-3 VALUE ZERO.           
068700                                                                          
068800     05  TAB-VLFG             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
068900     EJECT                                                                
069000*01  XXJK  -COPY WDGX4322    -PRE XXJK-                                   
069100     EJECT                                                                
069200******************************************************************        
069300*                                                                         
069400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
069500*                                                                         
069600 01    IMS-WS.                                                            
069700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
069800     SKIP3                                                                
069900*                        **** STATUS-KOD FRÅN IMS                         
070000   03    STATUS-KUNDORDER-SEK-WS PIC X(02).                               
070100     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
070200     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
070300   03    STATUS-WS               PIC XX.                                  
070400     88    SEGMENT-FINNS                     VALUE '  '.                  
070500     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
070600     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
070700     88    END-OF-DATABASE                   VALUE 'GB'.                  
070800     SKIP3                                                                
070900   03    GODK-STATUSKODER.                                                
071000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
071100     SKIP3                                                                
071200 01    SSA1                      PIC X(128).                              
071300 01    SSA2                      PIC X(96).                               
071400 01    SSA3                      PIC X(96).                               
071500     EJECT                                                                
071600*                            IMS FUNKTIONSKODER                           
071700*01    -COPY W0003                                                        
071800     EJECT                                                                
071900 01    FILLER               PIC X(16)  VALUE 'IO-K501'.                   
072000 01    DLI-IO-K501.                                                       
072100*  03    EMBB01   -COPY WDK501                                            
072200     EJECT                                                                
072300 01    FILLER               PIC X(16)  VALUE 'IO-E401'.                   
072400 01    DLI-IO-E401.                                                       
072500*  03    WDE401 -COPY WDE401                                              
072600     EJECT                                                                
072700 01    FILLER               PIC X(16)  VALUE 'IO-E411'.                   
072800 01    DLI-IO-E411.                                                       
072900*  03    WDE411 -COPY WDE411                                              
073000     EJECT                                                                
073100 01    FILLER               PIC X(16)  VALUE 'IO-E421'.                   
073200 01    DLI-IO-E421.                                                       
073300*  03    WDE421 -COPY WDE421                                              
073400     EJECT                                                                
073500 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA2'.               
073600 01    DLI-IO-AREA2.                                                      
073700   03    IO-AREA2                PIC X(400)  VALUE SPACE.                 
073800     SKIP3                                                                
073900*  03    WDE601   -COPY WDE601             -RED IO-AREA2.                 
074000     EJECT                                                                
074100*  03    WDE611   -COPY WDE611             -RED IO-AREA2.                 
074200     EJECT                                                                
074300 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA3'.               
074400 01    DLI-IO-AREA3.                                                      
074500   03    IO-AREA3                PIC X(25)   VALUE SPACE.                 
074600     SKIP3                                                                
074700*  03    WLXXDV11 -COPY WDGX4726           -RED IO-AREA3.                 
074800     EJECT                                                                
074900*  03    WLXXDV21 -COPY WDGX4727           -RED IO-AREA3.                 
075000     EJECT                                                                
075100 01  DLI-IO-AREA4.                                                        
075200     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
075300*                                                                         
075400*    03  WLXXJK01  -COPY WDGX01      -PRE 4321-  -RED IO-AREA4            
075500*    03  WLXXJK11  -COPY WDGX4322    -RED IO-AREA4                        
075600     EJECT                                                                
075700 01  DLI-IO-AREA5.                                                        
075800     03  IO-AREA5                PIC X(256)  VALUE SPACE.                 
075900*                                                                         
076000*    03  WLORQA01  -COPY WDQ301      -RED IO-AREA5                        
076100     EJECT                                                                
076200 01  DLI-IO-AREA6.                                                        
076300     03  IO-AREA6                PIC X(1000) VALUE SPACE.                 
076400*                                                                         
076500*    03  WLXXKW11  -COPY WDGX4472    -RED IO-AREA6                        
076600     EJECT                                                                
076700*    03  WLXXLB11  -COPY WDGX4478    -RED IO-AREA6                        
076800     EJECT                                                                
076900 01    DLI-IO-AREA7.                                                      
077000   03    IO-AREA7                PIC X(150)  VALUE SPACE.                 
077100     SKIP3                                                                
077200*  03  WDGZ01     -COPY WDGZ01  -PRE LOGG-   -RED IO-AREA7.               
077300     EJECT                                                                
077400 01  FILLER               PIC X(16)   VALUE 'WDQ201 AREA'.                
077500 01   DLI-IO-AREA-Q201.                                                   
077600*     03  -COPY WDQ201                                                    
077700                                                                          
077800 01  FILLER               PIC X(16)   VALUE 'WDQ212 AREA'.                
077900 01   DLI-IO-AREA-Q212.                                                   
078000*     03  -COPY WDQ212                                                    
078100     EJECT                                                                
078200                                                                          
078300*01      WDGZRYK  -COPY WDGZRYK.                                          
078400     SKIP2                                                                
078500 01    FILLER                    PIC X(16) VALUE 'WDE4A-AREA'.            
078600 01    WDE4A-IO-AREA.                                                     
078700   03    WDE4A-AREA              PIC X(100)  VALUE SPACE.                 
078800*  03    WDE4A1    -COPY WDE4A1                                           
078900                                                                          
079000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
079100 01   DLI-IO-AREA-B601.                                                   
079200*     03  -COPY WDB601                                                    
079300                                                                          
079400 01  FILLER               PIC X(16)   VALUE 'WDA601 AREA'.                
079500 01   DLI-IO-WDA601.                                                      
079600*     03  -COPY WDA601                                                    
079700     EJECT                                                                
079800 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK601'.              
079900 01    DLI-IO-WDK601.                                                     
080000*      03  -COPY WDK601                                                   
080100     EJECT                                                                
080200 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK611'.              
080300 01    DLI-IO-WDK611.                                                     
080400*      03  -COPY WDK611                                                   
080500     EJECT                                                                
080600 LINKAGE SECTION.                                                         
080700*01    -COPY W0009     -PRE MSG-                                          
080800     EJECT                                                                
080900 01  TMS-CRE-PCB                 PIC X.                                   
081000 01  TMS-DEL-PCB                 PIC X.                                   
081100 01  ATAB-PCB                    PIC X.                                   
081200     EJECT                                                                
081300*01    -COPY W0008     -PRE WDE41-                                        
081400     05  FILLER                  PIC X.                                   
081500     EJECT                                                                
081600*01    -COPY W0008     -PRE WDE4A-                                        
081700     05  FILLER                  PIC X.                                   
081800     EJECT                                                                
081900*01    -COPY W0008     -PRE WDE4-                                         
082000     05  FILLER                  PIC X.                                   
082100     EJECT                                                                
082200*01    -COPY W0008     -PRE WDE42-                                        
082300     05  FILLER                  PIC X.                                   
082400     EJECT                                                                
082500*01    -COPY W0008     -PRE WDE6-                                         
082600     05  FILLER                  PIC X.                                   
082700     EJECT                                                                
082800*01    -COPY W0008     -PRE EMBB-                                         
082900     05  FILLER                  PIC X.                                   
083000     EJECT                                                                
083100*01    -COPY W0008     -PRE XXDV-                                         
083200     05  FILLER                  PIC X.                                   
083300     EJECT                                                                
083400*01    -COPY W0008     -PRE ORQA-                                         
083500     05  FILLER                  PIC X.                                   
083600     EJECT                                                                
083700*01    -COPY W0008     -PRE XXKW-                                         
083800     05  FILLER                  PIC X.                                   
083900     EJECT                                                                
084000*01    -COPY W0008     -PRE XXLB-                                         
084100     05  FILLER                  PIC X.                                   
084200     EJECT                                                                
084300*01    -COPY W0008     -PRE XXJK-                                         
084400     05  FILLER                  PIC X.                                   
084500     EJECT                                                                
084600*01    -COPY W0008     -PRE ZZAC-                                         
084700     05  FILLER                  PIC X.                                   
084800     EJECT                                                                
084900*01    -COPY W0008     -PRE ORQI-                                         
085000     05  FILLER                  PIC X.                                   
085100     EJECT                                                                
085200*01  -COPY W0008       -PRE ORQL-                                         
085300     05  FILLER                  PIC X.                                   
085400     EJECT                                                                
085500*01    -COPY W0008     -PRE WDE62-                                        
085600     05  FILLER                  PIC X.                                   
085700     EJECT                                                                
085800*01  -COPY W0008       -PRE PLATS-DM-                                     
085900     05  FILLER                  PIC X.                                   
086000     EJECT                                                                
086100*01  -COPY W0008       -PRE PLATS-DN-                                     
086200     05  FILLER                  PIC X.                                   
086300     EJECT                                                                
086400*01  -COPY W0008       -PRE PLATS-DP-                                     
086500     05  FILLER                  PIC X.                                   
086600     EJECT                                                                
086700*01  -COPY W0008       -PRE PLATS-DO-                                     
086800     05  FILLER                  PIC X.                                   
086900     EJECT                                                                
087000*01  -COPY W0008       -PRE PLATS-WDE6C-                                  
087100     05  FILLER                  PIC X.                                   
087200     EJECT                                                                
087300*01  -COPY W0008       -PRE PLATS-GMTC-                                   
087400     05  FILLER                  PIC X.                                   
087500     EJECT                                                                
087600*01  -COPY W0008       -PRE PLATS-WDB6-                                   
087700     05  FILLER                  PIC X.                                   
087800     EJECT                                                                
087900*01  -COPY W0008       -PRE WDA6B-                                        
088000     05  FILLER                  PIC X.                                   
088100     EJECT                                                                
088200*01  -COPY W0008       -PRE WDB6-                                         
088300     05  FILLER                  PIC X.                                   
088400     EJECT                                                                
088500*01  -COPY W0008       -PRE WDK6-                                         
088600     05  FILLER                  PIC X.                                   
088700     EJECT                                                                
088800 01  DNOT-ORQP-PCB               PIC X.                                   
088900 01  DNOT-ORQP2-PCB              PIC X.                                   
089000 01  DNOT-ORQP3-PCB              PIC X.                                   
089100 01  DNOT-4013-PCB               PIC X.                                   
089200 01  DNOT-BENA-PCB               PIC X.                                   
089300 01  DNOT-WDB6-PCB               PIC X.                                   
089400 01  TMS-1165-PCB                PIC X.                                   
089500 01  TMS-4141-PCB                PIC X.                                   
089600 01  TMS-WDB2-PCB                PIC X.                                   
089700 01  TMS-WDB6-PCB                PIC X.                                   
089800 01  TMS-WDD3-PCB                PIC X.                                   
089900 01  TMS-WDB1-PCB                PIC X.                                   
090000 01  TMS-WDE4A-PCB               PIC X.                                   
090100 01  TMS-WDE4F-PCB               PIC X.                                   
090200 01  TMS-WDQ2-PCB                PIC X.                                   
090300 01  TMS-WDQ3-PCB                PIC X.                                   
090400 01  TMS-WDK6-PCB                PIC X.                                   
090500 01  TMS-WDE6-PCB                PIC X.                                   
090600 01  TMS-WDK5-PCB                PIC X.                                   
090700 01  TMS-WDQ2C-PCB               PIC X.                                   
090800     EJECT                                                                
090900 PROCEDURE DIVISION USING MSG-PCB TMS-CRE-PCB TMS-DEL-PCB                 
091000                           ATAB-PCB WDE41-PCB WDE4A-PCB                   
091100                           WDE4-PCB WDE42-PCB                             
091200                           WDE6-PCB EMBB-PCB  XXDV-PCB ORQA-PCB           
091300                           XXKW-PCB XXLB-PCB  XXJK-PCB ZZAC-PCB           
091400                           ORQI-PCB ORQL-PCB WDE62-PCB                    
091500                           PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB         
091600                           PLATS-DO-PCB PLATS-WDE6C-PCB                   
091700                           PLATS-GMTC-PCB PLATS-WDB6-PCB                  
091800                           WDA6B-PCB WDB6-PCB WDK6-PCB                    
091900                           DNOT-ORQP-PCB                                  
092000                           DNOT-ORQP2-PCB                                 
092100                           DNOT-ORQP3-PCB                                 
092200                           DNOT-4013-PCB                                  
092300                           DNOT-BENA-PCB                                  
092400                           DNOT-WDB6-PCB                                  
092500                           TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB         
092600                           TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB         
092700                         TMS-WDE4A-PCB TMS-WDE4F-PCB                      
092800                         TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB           
092900                         TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.         
093000 MAIN SECTION.                                                            
093100     ENTRY 'DLITCBL' USING MSG-PCB TMS-CRE-PCB TMS-DEL-PCB                
093200                           ATAB-PCB WDE41-PCB WDE4A-PCB                   
093300                           WDE4-PCB WDE42-PCB                             
093400                           WDE6-PCB EMBB-PCB  XXDV-PCB ORQA-PCB           
093500                           XXKW-PCB XXLB-PCB  XXJK-PCB ZZAC-PCB           
093600                           ORQI-PCB ORQL-PCB WDE62-PCB                    
093700                           PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB         
093800                           PLATS-DO-PCB PLATS-WDE6C-PCB                   
093900                           PLATS-GMTC-PCB PLATS-WDB6-PCB                  
094000                           WDA6B-PCB WDB6-PCB WDK6-PCB                    
094100                           DNOT-ORQP-PCB                                  
094200                           DNOT-ORQP2-PCB                                 
094300                           DNOT-ORQP3-PCB                                 
094400                           DNOT-4013-PCB                                  
094500                           DNOT-BENA-PCB                                  
094600                           DNOT-WDB6-PCB                                  
094700                           TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB         
094800                           TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB         
094900                         TMS-WDE4A-PCB TMS-WDE4F-PCB                      
095000                         TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB           
095100                         TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.         
095200                                                                          
095300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
095400                                                                          
095500     IF SUB-KDRC = 0                                                      
095600        IF REQU-KDPGMACT = 'E'                                            
095700          PERFORM A-INIT                                                  
095800          PERFORM B-GENERELL-KONTROLL                                     
095900          IF WS-INDATA-RATT                                               
096000            PERFORM C-RELATIONSKONTROLL                                   
096100            IF WS-INDATA-RATT                                             
096200              PERFORM D-LAGG-UPP-KOLLI-SEG                                
096300                                                                          
096400              MOVE +1 TO INX                                              
096500              PERFORM UNTIL INX NOT <                                     
096600                            MAX-RAD-ANTAL-PLUS-1                          
096700                 PERFORM F-BEHANDLA-RADER                                 
096800                 ADD +1 TO INX                                            
096900              END-PERFORM                                                 
097000              PERFORM L-HAMTA-ADRESS                                      
097100              IF WS-BEHANDLING-RATT                                       
097200                 PERFORM G-UPPDATERA-KOLLIREG                             
097300                 IF WS-FLAUTFAK  = JA                                     
097400                    IF DIST03-SVERIGE-2 OR                                
097500                       DIST18-SKROT                                       
097600                       PERFORM K-UPPDAT-4726-4727                         
097700                    END-IF                                                
097800                 END-IF                                                   
097900                 PERFORM S02-CLEAR-RESPONSE-FIELDS                        
098000              END-IF                                                      
098100            ELSE                                                          
098200              PERFORM S04-MOVE-REQU-FIELDS-TO-RESP                        
098300            END-IF                                                        
098400          END-IF                                                          
098500                                                                          
098600          IF WS-INDATA-RATT                                               
098700            PERFORM H-AVSLUT                                              
098800          END-IF                                                          
098900        ELSE                                                              
099000          MOVE FEL-SYSTEM-ERROR TO RESP-IDMSG-ERROR                       
099100        END-IF                                                            
099200                                                                          
099300        MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                          
099400        MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                         
099500        MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                        
099600        IF WS-IDMSG-ERROR NOT = SPACE                                     
099700           MOVE ALL '+' TO RESP-WL0122O1(1:34)                            
099800           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
099900           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
100000           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
100100           MOVE 001              TO RESP-IDMSGVER                         
100200           PERFORM S03-SPARA-RADINF                                       
100300        END-IF                                                            
100400        PERFORM S02-RETURN-RESPONSE                                       
100500     END-IF                                                               
100600                                                                          
100700     MOVE ZERO TO RETURN-CODE                                             
100800     GOBACK                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 A-INIT SECTION.                                                          
101200     SKIP3                                                                
101300     MOVE RAETT                           TO WS-INDATA-TEST               
101400                                             WS-BEHANDLING-TEST           
101500     MOVE ALL '+'                         TO RESP-AREA                    
101600     MOVE 001                             TO RESP-IDMSGVER                
101700     MOVE SPACE                           TO RESP-IDMSG-ERROR             
101800                                             RESP-IDMSG-INFO              
101900                                             RESP-IDELMT-ERROR            
102000                                         RESP-WL0122O1(89:1888)           
102100     PERFORM AA-FLYTTA-NYCKLAR                                            
102200     MOVE NEJ                   TO DIRLEV-KOLLI-SW                        
102300                                   NYA-NYCKLAR-SW                         
102400                                                                          
102500     INITIALIZE TMS-W403TMS1                                              
102600     ACCEPT DAGENS-DATUM FROM DATE                                        
102700     ACCEPT DAGENS-TID   FROM TIME                                        
102800                                                                          
102900*    DISPLAY ' WL012200 '                                                 
103000*    DISPLAY ' DAGENS-DATUM ' DAGENS-DATUM                                
103100*    DISPLAY ' DAGENS-TID   ' DAGENS-TID                                  
103200                                                                          
103300     MOVE +1                        TO PLATSIX                            
103400     PERFORM UNTIL PLATSIX NOT < PLATSIX-MAX + 1                          
103500         MOVE SPACE                 TO WSTAB-ADFLGEO    (PLATSIX)         
103600                                       WSTAB-FLUTLAST   (PLATSIX)         
103700                                       WSTAB-IDDC-CROSS (PLATSIX)         
103800         MOVE ZERO                  TO WSTAB-IDTRPTNR   (PLATSIX)         
103900                                       WSTAB-ADFLOMR    (PLATSIX)         
104000                                       WSTAB-ADRUTNIV   (PLATSIX)         
104100                                       WSTAB-DIHMODUL   (PLATSIX)         
104200                                       WSTAB-DIDMODUL   (PLATSIX)         
104300                                       WSTAB-ADVMODUL   (PLATSIX)         
104400                                       WSTAB-ADHMODUL   (PLATSIX)         
104500         ADD +1                     TO PLATSIX                            
104600     END-PERFORM                                                          
104700                                                                          
104800     MOVE ZERO                      TO PALLAR (1)                         
104900                                       PALLAR (2)                         
105000                                       PALLAR (3)                         
105100                                       PALLAR (4)                         
105200                                       PALLAR (5)                         
105300                                                                          
105400     MOVE ZERO                      TO KRAGAR (1)                         
105500                                       KRAGAR (2)                         
105600                                       KRAGAR (3)                         
105700                                       KRAGAR (4)                         
105800                                       KRAGAR (5)                         
105900                                                                          
106000     MOVE ZERO                      TO EMB-LOCK (1)                       
106100                                       EMB-LOCK (2)                       
106200                                       EMB-LOCK (3)                       
106300                                       EMB-LOCK (4)                       
106400                                       EMB-LOCK (5)                       
106500                                       LOGG-IDLOGLOP                      
106600     .                                                                    
106700     EJECT                                                                
106800 AA-FLYTTA-NYCKLAR  SECTION.                                              
106900                                                                          
107000     MOVE REQU-KDMATT       TO WS-KDMATT                                  
107100                                                                          
107200     IF REQU-IDANSTNR-KEY = ALL '+'                                       
107300         MOVE ZERO                        TO   WS-IDANSTNR                
107400     ELSE                                                                 
107500         MOVE JA                          TO   NYA-NYCKLAR-SW             
107600         MOVE REQU-IDANSTNR-KEY           TO   WS-IDANSTNR                
107700     END-IF                                                               
107800     SKIP2                                                                
107900         MOVE JA                          TO   NYA-NYCKLAR-SW             
108000         IF REQU-IDDISTR-KEY = ALL '+'                                    
108100           MOVE ZERO                      TO   WS-IDDISTR                 
108200         ELSE                                                             
108300           MOVE REQU-IDDISTR-KEY          TO   WS-IDDISTR                 
108400         END-IF                                                           
108500                                                                          
108600     IF REQU-IDKUNDNR-KEY = ALL '+'                                       
108700         MOVE ZERO                        TO   WS-IDKUNDNR                
108800     ELSE                                                                 
108900         MOVE JA                          TO   NYA-NYCKLAR-SW             
109000         MOVE REQU-IDKUNDNR-KEY           TO   WS-IDKUNDNR                
109100     END-IF                                                               
109200                                                                          
109300     IF REQU-IDORDNR-KEY = ALL '+'                                        
109400*       MOVE ZERO                         TO   WS-IDORDNR                 
109500        MOVE '00000'                      TO   WS-IDORDNR                 
109600     ELSE                                                                 
109700         MOVE JA                          TO   NYA-NYCKLAR-SW             
109800         MOVE REQU-IDORDNR-KEY            TO   WS-IDORDNR                 
109900     END-IF                                                               
110000                                                                          
110100     IF REQU-IDKOLLI-KEY = ALL '+'                                        
110200         IF REQU-IDKOLLI-FOM = ALL '+'                                    
110300             MOVE ZERO                    TO   WS-IDKOLLI                 
110400         ELSE                                                             
110500             MOVE ZERO                    TO   WS-IDKOLLI                 
110600         END-IF                                                           
110700     ELSE                                                                 
110800         MOVE REQU-IDKOLLI-KEY            TO   WS-IDKOLLI                 
110900         INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO               
111000     END-IF                                                               
111100                                                                          
111200     IF REQU-IDPRODNR-KEY = ALL '+'                                       
111300       IF NYA-NYCKLAR                                                     
111400         MOVE ZERO                        TO   WS-IDPRODNR                
111500       ELSE                                                               
111600         MOVE REQU-IDPRODNR-KEY           TO   WS-IDPRODNR                
111700         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
111800       END-IF                                                             
111900     ELSE                                                                 
112000         MOVE REQU-IDPRODNR-KEY           TO   WS-IDPRODNR                
112100     END-IF                                                               
112200                                                                          
112300     IF WS-IDANSTNR NUMERIC                                               
112400     MOVE WS-IDANSTNR                     TO   RESP-IDANSTNR-KEY          
112500     INSPECT RESP-IDANSTNR-KEY REPLACING LEADING ZERO BY SPACE            
112600     END-IF                                                               
112700                                                                          
112800     IF WS-IDDISTR NUMERIC                                                
112900     MOVE WS-IDDISTR                      TO   RESP-IDDISTR-KEY           
113000     INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE            
113100     END-IF                                                               
113200                                                                          
113300     IF WS-IDKUNDNR NUMERIC                                               
113400     MOVE WS-IDKUNDNR                     TO   RESP-IDKUNDNR-KEY          
113500     INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE            
113600     END-IF                                                               
113700                                                                          
113800     IF WS-IDORDNR NUMERIC                                                
113900     MOVE WS-IDORDNR                      TO   RESP-IDORDNR-KEY           
114000     INSPECT RESP-IDORDNR-KEY  REPLACING LEADING ZERO BY SPACE            
114100     END-IF                                                               
114200                                                                          
114300     IF WS-IDKOLLI NUMERIC                                                
114400     MOVE WS-IDKOLLI                      TO   RESP-IDKOLLI-KEY           
114500     INSPECT RESP-IDKOLLI-KEY  REPLACING LEADING ZERO BY SPACE            
114600     END-IF                                                               
114700                                                                          
114800     IF WS-IDPRODNR NUMERIC                                               
114900     MOVE WS-IDPRODNR                     TO   RESP-IDPRODNR-KEY          
115000     INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE            
115100     END-IF                                                               
115200                                                                          
115300     MOVE REQU-IDDC-KEY                   TO   RESP-IDDC-KEY              
115400     INSPECT RESP-IDDC-KEY REPLACING LEADING ZERO BY SPACE                
115500     .                                                                    
115600     EJECT                                                                
115700 B-GENERELL-KONTROLL  SECTION.                                            
115800                                                                          
115900     IF WS-IDANSTNR NOT NUMERIC                                           
116000         MOVE FEL                       TO   WS-INDATA-TEST               
116100         MOVE FEL-INVALID-KEY-XX        TO   RESP-IDMSG-ERROR             
116200         MOVE 'IDANSTNR'                TO RESP-IDELMT-ERROR              
116300     END-IF                                                               
116400                                                                          
116500     IF WS-IDDISTR  NOT NUMERIC                                           
116600         MOVE FEL                       TO   WS-INDATA-TEST               
116700         MOVE FEL-INVALID-KEY-XX        TO   RESP-IDMSG-ERROR             
116800         MOVE 'IDDISTR'                 TO RESP-IDELMT-ERROR              
116900     ELSE                                                                 
117000         MOVE WS-IDDISTR                TO   WS-IDDISTR-NUM               
117100         MOVE WS-IDDISTR-NUM            TO   TEST-IDDISTR                 
117200* * * SATSORDER EJ TILLÅTNA ANNAT ÄN FRÅN BILD 4303 * * *                 
117300         IF DIST19-SATS                                                   
117400           MOVE FEL                     TO   WS-INDATA-TEST               
117500           MOVE FEL-INVALID-KEY-XX      TO   RESP-IDMSG-ERROR             
117600           MOVE 'IDDISTR'               TO RESP-IDELMT-ERROR              
117700         ELSE                                                             
117800           MOVE WS-IDDISTR              TO   W-4A1-IDDISTR                
117900         END-IF                                                           
118000* * * * * * * * * * * * * * * * *                                         
118100     END-IF                                                               
118200                                                                          
118300     IF WS-IDKUNDNR NOT NUMERIC                                           
118400         MOVE FEL                       TO   WS-INDATA-TEST               
118500         MOVE FEL-INVALID-KEY-XX        TO   RESP-IDMSG-ERROR             
118600         MOVE 'IDKUNDNR'                TO RESP-IDELMT-ERROR              
118700     ELSE                                                                 
118800         MOVE WS-IDKUNDNR               TO   W-4A1-IDKUNDNR               
118900     END-IF                                                               
119000                                                                          
119100*    IF WS-IDORDNR  NOT NUMERIC                                           
119200*        MOVE FEL                       TO   WS-INDATA-TEST               
119300*        MOVE FEL-INVALID-KEY-XX        TO   RESP-IDMSG-ERROR             
119400*        MOVE 'IDORDNR'                 TO RESP-IDELMT-ERROR              
119500*    ELSE                                                                 
119600*        MOVE WS-IDORDNR                TO   W-4A1-IDORDNR                
119700*    END-IF                                                               
119800     MOVE ZERO                      TO   W-4A1-IDORDNR                    
119900                                                                          
120000     IF WS-IDKOLLI NOT NUMERIC                                            
120100         MOVE FEL                    TO   WS-INDATA-TEST                  
120200         MOVE FEL-INVALID-KEY-XX     TO   RESP-IDMSG-ERROR                
120300         MOVE 'IDKOLLI'                 TO RESP-IDELMT-ERROR              
120400     ELSE                                                                 
120500         PERFORM BA-KONTROLLERA-IDKOLLI                                   
120600     END-IF                                                               
120700                                                                          
120800     IF WS-IDPRODNR NOT NUMERIC                                           
120900         MOVE FEL                       TO   WS-INDATA-TEST               
121000         MOVE FEL-INVALID-KEY-XX        TO   RESP-IDMSG-ERROR             
121100         MOVE 'IDPRODNR'                TO RESP-IDELMT-ERROR              
121200     END-IF                                                               
121300                                                                          
121400         PERFORM BB-KONTROLLERA-KOLLIKOD                                  
121500                                                                          
121600     MOVE +1                        TO INX                                
121700     MOVE ZERO                      TO RAD-INX                            
121800     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL-PLUS-1                         
121900         PERFORM BD-KONTROLLERA-RAD                                       
122000         ADD +1                     TO INX                                
122100     END-PERFORM                                                          
122200     COMPUTE MAX-RAD-ANTAL-PLUS-1 = RAD-INX + 1                           
122300                                                                          
122400     IF WS-INDATA-RATT                                                    
122500        IF  WS-ANT-RAD-I-BILD > WS-MAX-ANT-RAD-I-BILD                     
122600            MOVE FEL                    TO WS-INDATA-TEST                 
122700            MOVE FEL-TOO-MANY-LINES     TO RESP-IDMSG-ERROR               
122800        END-IF                                                            
122900     END-IF                                                               
123000                                                                          
123100     IF REQU-FLSISTAK = 'N' OR 'Y'  OR 'J'                                
123200        CONTINUE                                                          
123300     ELSE                                                                 
123400        IF WS-INDATA-RATT                                                 
123500           MOVE 'FLSISTAK IS WRG'    TO RESP-IDELMT-ERROR                 
123600           MOVE '252'                TO RESP-IDMSG-ERROR                  
123700           MOVE FEL                  TO WS-INDATA-TEST                    
123800        END-IF                                                            
123900     END-IF                                                               
124000******** ADAPT DATE AND TIME FOR TIMEZONES                                
124100         MOVE REQU-IDDC-KEY TO W-IDDC-B6                                  
124200         PERFORM IMS-GU-WDB601                                            
124300                                                                          
124400         MOVE '011'                TO MSGI-KDCALL                         
124500         MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                       
124510         MOVE DCS-IDDC             TO MSGI-IDDC                           
124600         MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                       
124700         MOVE DAGENS-TID           TO MSGI-TILOKTID                       
124800         CALL WL01TIDZ USING          MSGI-WL01TIDZ                       
124900           MOVE MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                        
125000           MOVE MSGI-TILOKTID(1:4) TO DAGENS-TID(1:4)                     
125100     MOVE FUNCTION CURRENT-DATE(13:2)  TO DAGENS-TID(5:2)                 
125200********                                                                  
125300     .                                                                    
125400                                                                          
125500     EJECT                                                                
125600 BA-KONTROLLERA-IDKOLLI SECTION.                                          
125700                                                                          
125800     IF REQU-IDKOLLI-FOM = ALL '+'                                        
125900         IF WS-IDKOLLI = ZERO                                             
126000             MOVE FEL                   TO WS-INDATA-TEST                 
126100             MOVE 'IDKOLLI'             TO RESP-IDELMT-ERROR              
126200             MOVE FEL-XX-MISSING        TO RESP-IDMSG-ERROR               
126300         ELSE                                                             
126400             IF REQU-IDKOLLI-TOM NOT = ALL '+'                            
126500                 MOVE FEL               TO WS-INDATA-TEST                 
126600                 MOVE FEL-CASE-AND-INTERVAL                               
126700                                        TO RESP-IDMSG-ERROR               
126800             END-IF                                                       
126900         END-IF                                                           
127000     ELSE                                                                 
127100         IF REQU-IDKOLLI-KEY NOT = ALL '+'                                
127200             MOVE FEL                   TO WS-INDATA-TEST                 
127300             MOVE FEL-CASE-AND-INTERVAL TO RESP-IDMSG-ERROR               
127400         ELSE                                                             
127500             EVALUATE TRUE                                                
127600             WHEN REQU-IDKOLLI-FOM NOT NUMERIC                            
127700                 MOVE 'IDKOLLI-FOM'     TO RESP-IDELMT-ERROR              
127800                 MOVE '024'             TO RESP-IDMSG-ERROR               
127900                 MOVE FEL               TO WS-INDATA-TEST                 
128000             WHEN REQU-IDKOLLI-TOM = ALL '+'                              
128100                 MOVE FEL               TO WS-INDATA-TEST                 
128200                 MOVE FEL-CASE-AND-INTERVAL                               
128300                                        TO RESP-IDMSG-ERROR               
128400             WHEN REQU-IDKOLLI-TOM NOT NUMERIC                            
128500                 MOVE 'IDKOLLI-TOM'     TO RESP-IDELMT-ERROR              
128600                 MOVE '024'             TO RESP-IDMSG-ERROR               
128700                 MOVE FEL               TO WS-INDATA-TEST                 
128800             WHEN REQU-IDKOLLI-TOM < REQU-IDKOLLI-FOM OR                  
128900                  REQU-IDKOLLI-TOM  = REQU-IDKOLLI-FOM                    
129000                 MOVE FEL               TO WS-INDATA-TEST                 
129100                 MOVE FEL-CASE-INTERVAL TO RESP-IDMSG-ERROR               
129200             WHEN OTHER                                                   
129300                 MOVE REQU-IDKOLLI-FOM   TO WS-IDKOLLI-FOM                
129400                 MOVE REQU-IDKOLLI-TOM   TO WS-IDKOLLI-TOM                
129500                                                                          
129600               IF (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM > 100 AND              
129700                 REQU-FLSKRIV-DELNOTE NOT = 'N') OR                       
129800                  (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM > 100 AND              
129900                 REQU-FLSKRIV-CLABEL  NOT = 'N')                          
130000                                                                          
130100                 MOVE FEL               TO WS-INDATA-TEST                 
130200                 MOVE FEL-MAX-100-CASES TO RESP-IDMSG-ERROR               
130300               ELSE                                                       
130400                                                                          
130500                 IF (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM > 50 AND             
130600                   REQU-FLSKRIV-DELNOTE NOT = 'N') AND                    
130700                    (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM > 50 AND             
130800                   REQU-FLSKRIV-CLABEL  NOT = 'N')                        
130900                                                                          
131000                   MOVE FEL               TO WS-INDATA-TEST               
131100                   MOVE FEL-MAX-50-CASES  TO RESP-IDMSG-ERROR             
131200                 ELSE                                                     
131300                   IF  WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1 >              
131400                       PLATSIX-MAX                                        
131500                      MOVE FEL           TO WS-INDATA-TEST                
131600                      MOVE PLATSIX-MAX TO FEL-PLATSIX-C2                  
131700                      MOVE FEL-827     TO RESP-IDELMT-ERROR               
131800                      MOVE '251'             TO RESP-IDMSG-ERROR          
131900                   ELSE                                                   
132000                     MOVE JA             TO FL-KOLLI-INTERVALL            
132100                   END-IF                                                 
132200                 END-IF                                                   
132300               END-IF                                                     
132400             END-EVALUATE                                                 
132500         END-IF                                                           
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900 BB-KONTROLLERA-KOLLIKOD    SECTION.                                      
133000                                                                          
133100     IF REQU-KDKOLLI NOT = ALL '+'                                        
133200         MOVE REQU-KDKOLLI              TO WS-KDKOLLI                     
133300     ELSE                                                                 
133400         MOVE SPACE                     TO WS-KDKOLLI                     
133500*----------------------------------------OM EJ KOLLIKOD ANGIVEN           
133600*----------------------------------------SKALL EMBTYP,LÄNGD,HÖJD          
133700*----------------------------------------OCH BREDD ANGES                  
133800         IF  WS-IDDISTR NUMERIC                                           
133900         AND WS-IDDISTR < '0800'                                          
134000         AND WS-IDKOLLI NUMERIC                                           
134100             IF REQU-KDEMBTYP = ALL '+'                                   
134200                MOVE FEL                   TO WS-INDATA-TEST              
134300                MOVE FEL-MORE-CASE-INFO-NEEDED                            
134400                                            TO RESP-IDMSG-ERROR           
134500             ELSE                                                         
134600                IF  REQU-DIKOLLIL = (ALL '+' OR ZERO)                     
134700                OR  REQU-DIKOLLIB = (ALL '+' OR ZERO)                     
134800                OR  REQU-DIKOLLIH = (ALL '+' OR ZERO)                     
134900                    MOVE FEL               TO WS-INDATA-TEST              
135000                    MOVE FEL-MORE-CASE-INFO-NEEDED                        
135100                                           TO RESP-IDMSG-ERROR            
135200                END-IF                                                    
135300             END-IF                                                       
135400         END-IF                                                           
135500     END-IF                                                               
135600                                                                          
135700     PERFORM BBA-KONTROLLERA-UPPG                                         
135800     .                                                                    
135900     EJECT                                                                
136000 BBA-KONTROLLERA-UPPG   SECTION.                                          
136100                                                                          
136200     IF REQU-KDEMBTYP NOT = ALL '+'                                       
136300         IF REQU-KDEMBTYP NUMERIC                                         
136400             MOVE REQU-KDEMBTYP         TO WS-KDEMBTYP                    
136500         ELSE                                                             
136600           IF WS-INDATA-RATT                                              
136700             MOVE '024'                 TO RESP-IDMSG-ERROR               
136800             MOVE 'KDEMBTYP'            TO RESP-IDELMT-ERROR              
136900             MOVE FEL                   TO WS-INDATA-TEST                 
137000           END-IF                                                         
137100         END-IF                                                           
137200     ELSE                                                                 
137300         MOVE ZERO                      TO WS-KDEMBTYP                    
137400     END-IF                                                               
137500     IF REQU-DIKOLLIL NOT = ALL '+'                                       
137600         IF REQU-DIKOLLIL NUMERIC                                         
137700         AND REQU-DIKOLLIL  > ZERO                                        
137800             MOVE REQU-DIKOLLIL         TO WS-DIKOLLIL                    
137900             IF US-MEASUREMENT                                            
138000               COMPUTE WS-DIKOLLIL ROUNDED =                              
138100                       WS-DIKOLLIL * CONV-IN-TO-CM                        
138200               END-COMPUTE                                                
138300             END-IF                                                       
138400         ELSE                                                             
138500           IF WS-INDATA-RATT                                              
138600             MOVE 'DIKOLLIL'        TO RESP-IDELMT-ERROR                  
138700             MOVE '024'             TO RESP-IDMSG-ERROR                   
138800             MOVE FEL                   TO WS-INDATA-TEST                 
138900           END-IF                                                         
139000         END-IF                                                           
139100     END-IF                                                               
139200                                                                          
139300     IF REQU-DIKOLLIH NOT = ALL '+'                                       
139400         IF REQU-DIKOLLIH NUMERIC                                         
139500         AND REQU-DIKOLLIH  > ZERO                                        
139600             MOVE REQU-DIKOLLIH         TO WS-DIKOLLIH                    
139700             IF US-MEASUREMENT                                            
139800               COMPUTE WS-DIKOLLIH ROUNDED =                              
139900                       WS-DIKOLLIH * CONV-IN-TO-CM                        
140000               END-COMPUTE                                                
140100             END-IF                                                       
140200         ELSE                                                             
140300           IF WS-INDATA-RATT                                              
140400             MOVE 'DIKOLLIH'        TO RESP-IDELMT-ERROR                  
140500             MOVE '024'             TO RESP-IDMSG-ERROR                   
140600             MOVE FEL                   TO WS-INDATA-TEST                 
140700           END-IF                                                         
140800         END-IF                                                           
140900     END-IF                                                               
141000                                                                          
141100     IF REQU-DIKOLLIB NOT = ALL '+'                                       
141200         IF REQU-DIKOLLIB NUMERIC                                         
141300         AND REQU-DIKOLLIB  > ZERO                                        
141400             MOVE REQU-DIKOLLIB         TO WS-DIKOLLIB                    
141500             IF US-MEASUREMENT                                            
141600               COMPUTE WS-DIKOLLIB ROUNDED =                              
141700                       WS-DIKOLLIB * CONV-IN-TO-CM                        
141800               END-COMPUTE                                                
141900             END-IF                                                       
142000         ELSE                                                             
142100           IF WS-INDATA-RATT                                              
142200             MOVE 'DIKOLLIB'        TO RESP-IDELMT-ERROR                  
142300             MOVE '024'             TO RESP-IDMSG-ERROR                   
142400             MOVE FEL                   TO WS-INDATA-TEST                 
142500           END-IF                                                         
142600         END-IF                                                           
142700     END-IF                                                               
142800     .                                                                    
142900     EJECT                                                                
143000 BD-KONTROLLERA-RAD     SECTION.                                          
143100     SKIP3                                                                
143200     IF REQU-IDRADNR-FOM (INX) = ALL '+'                                  
143300         IF REQU-IDRADNR-TOM (INX) = ALL '+'                              
143400             IF INX = +1                                                  
143500                 MOVE FEL-ENTER-INTERVAL                                  
143600                                  TO  RESP-IDMSG-ERROR                    
143700                                      RESP-IDMSG-ERROR-LINE (INX)         
143800                 MOVE FEL         TO  WS-INDATA-TEST                      
143900             END-IF                                                       
144000             MOVE +201            TO  INX                                 
144100         ELSE                                                             
144200             MOVE FEL             TO  WS-INDATA-TEST                      
144300             MOVE FEL-WRONG-INTERVAL                                      
144400                                  TO RESP-IDMSG-ERROR                     
144500                                     RESP-IDMSG-ERROR-LINE (INX)          
144600         END-IF                                                           
144700     ELSE                                                                 
144800         IF REQU-IDRADNR-FOM (INX) NUMERIC                                
144900             IF REQU-IDRADNR-FOM (INX) > ZERO                             
145000              CONTINUE                                                    
145100             ELSE                                                         
145200                 MOVE FEL       TO WS-INDATA-TEST                         
145300                 MOVE FEL-WRONG-INTERVAL                                  
145400                                TO RESP-IDMSG-ERROR                       
145500                                   RESP-IDMSG-ERROR-LINE (INX)            
145600             END-IF                                                       
145700             IF REQU-IDRADNR-TOM (INX) = ALL '+'                          
145800                 MOVE ZERO              TO REQU-IDRADNR-TOM (INX)         
145900                 ADD +1                TO WS-ANT-RAD-I-BILD               
146000             ELSE                                                         
146100                 IF REQU-IDRADNR-TOM (INX) NUMERIC                        
146200                     IF REQU-IDRADNR-FOM (INX)                            
146300                                         < REQU-IDRADNR-TOM (INX)         
146400                         MOVE REQU-IDRADNR-FOM (INX) TO WS-RAD-FOM        
146500                         MOVE REQU-IDRADNR-TOM (INX) TO WS-RAD-TOM        
146600                         COMPUTE WS-ANT-RAD-I-BILD =                      
146700                                 WS-ANT-RAD-I-BILD +                      
146800                                 WS-RAD-TOM        -                      
146900                                 WS-RAD-FOM        + 1                    
147000                     ELSE                                                 
147100                         MOVE FEL   TO  WS-INDATA-TEST                    
147200                         MOVE FEL-WRONG-INTERVAL                          
147300                                    TO RESP-IDMSG-ERROR                   
147400                                       RESP-IDMSG-ERROR-LINE (INX)        
147500                     END-IF                                               
147600                 ELSE                                                     
147700                     MOVE FEL       TO  WS-INDATA-TEST                    
147800                     MOVE '024'     TO RESP-IDMSG-ERROR                   
147900                                       RESP-IDMSG-ERROR-LINE (INX)        
148000                     MOVE 'IDRADNR-TOM'                                   
148100                                    TO RESP-IDELMT-ERROR                  
148200                 END-IF                                                   
148300             END-IF                                                       
148400         ELSE                                                             
148500             MOVE FEL              TO  WS-INDATA-TEST                     
148600             MOVE '024'            TO RESP-IDMSG-ERROR                    
148700                                      RESP-IDMSG-ERROR-LINE (INX)         
148800             MOVE 'IDRADNR-FOM'    TO  RESP-IDELMT-ERROR                  
148900         END-IF                                                           
149000         ADD +1  TO  RAD-INX                                              
149100     END-IF                                                               
149200     SKIP2                                                                
149300     IF INX = 201                                                         
149400         ADD RAD-INX +1                   GIVING REST-INX                 
149500         PERFORM UNTIL REST-INX NOT < MAX-RAD-ANTAL-PLUS-1                
149600             IF  REQU-IDRADNR-FOM (REST-INX) NOT = ALL '+'                
149700                 MOVE FEL   TO WS-INDATA-TEST                             
149800                 MOVE '033' TO RESP-IDMSG-ERROR                           
149900                               RESP-IDMSG-ERROR-LINE (REST-INX)           
150000                 MOVE 'IDRADNR-FOM'     TO RESP-IDELMT-ERROR              
150100             END-IF                                                       
150200             IF  REQU-IDRADNR-TOM (REST-INX) NOT = ALL '+'                
150300                 MOVE FEL           TO WS-INDATA-TEST                     
150400                 MOVE '033'         TO RESP-IDMSG-ERROR                   
150500                                   RESP-IDMSG-ERROR-LINE(REST-INX)        
150600                 MOVE 'IDRADNR-TOM' TO RESP-IDELMT-ERROR                  
150700             END-IF                                                       
150800             IF  REQU-KVLEVART (REST-INX) NOT = ALL '+'                   
150900                 MOVE FEL          TO WS-INDATA-TEST                      
151000                 MOVE '033'        TO RESP-IDMSG-ERROR                    
151100                                   RESP-IDMSG-ERROR-LINE(REST-INX)        
151200                 MOVE 'KVLEVART'   TO RESP-IDELMT-ERROR                   
151300             END-IF                                                       
151400             ADD +1                       TO REST-INX                     
151500         END-PERFORM                                                      
151600     ELSE                                                                 
151700         IF REQU-KVLEVART (INX) = ALL '+'                                 
151800            CONTINUE                                                      
151900         ELSE                                                             
152000             IF REQU-KVLEVART (INX) NUMERIC                               
152100                 IF REQU-KVLEVART (INX) = ZERO                            
152200*----------------------------------------------NOLLADE AVVIKELSER         
152300*----------------------------------------------GODKÄNNAS SOM              
152400*----------------------------------------------ORAPPORTERAD RAD           
152500*----------------------------------------------PÅ ANNAN BILD              
152600                     MOVE FEL    TO  WS-INDATA-TEST                       
152700                     MOVE FEL-ZERO-NOT-ALLOWED                            
152800                                 TO RESP-IDMSG-ERROR                      
152900                                    RESP-IDMSG-ERROR-LINE (INX)           
153000                 ELSE                                                     
153100                     IF  REQU-IDRADNR-TOM (INX) NOT = ALL '+'             
153200                     AND REQU-IDRADNR-TOM (INX) NOT = ZERO                
153300                         MOVE FEL         TO  WS-INDATA-TEST              
153400                         MOVE '033'                                       
153500                                    TO RESP-IDMSG-ERROR                   
153600                                       RESP-IDMSG-ERROR-LINE (INX)        
153700                         MOVE 'IDRADNR-TOM'                               
153800                                    TO RESP-IDELMT-ERROR                  
153900                     END-IF                                               
154000                 END-IF                                                   
154100             ELSE                                                         
154200                 MOVE FEL           TO  WS-INDATA-TEST                    
154300                 MOVE '024'         TO RESP-IDMSG-ERROR                   
154400                                       RESP-IDMSG-ERROR-LINE (INX)        
154500                 MOVE 'KVLEVART'    TO RESP-IDELMT-ERROR                  
154600             END-IF                                                       
154700         END-IF                                                           
154800     END-IF                                                               
154900     .                                                                    
155000     EJECT                                                                
155100 C-RELATIONSKONTROLL  SECTION.                                            
155200                                                                          
155300     MOVE WS-IDKUNDNR           TO  WS-IDKUNDNR-NUM                       
155400                                                                          
155500         PERFORM CA-KONTROLLERA-KUNDORDNR                                 
155600         IF WS-INDATA-RATT                                                
155700             PERFORM CB-KONTROLLERA-IDKOLLI                               
155800             IF WS-INDATA-RATT                                            
155900                 PERFORM CC-KONTROLLERA-PACKARE                           
156000             END-IF                                                       
156100             IF WS-INDATA-RATT                                            
156200                 PERFORM CD-KONTROLLERA-KOLLIUPPG                         
156300             END-IF                                                       
156400             IF WS-INDATA-RATT                                            
156500                 PERFORM CG-OTHER-CONTROL                                 
156600             END-IF                                                       
156700             IF WS-INDATA-RATT                                            
156800                                                                          
156900                IF KOLLI-INTERVALL                                        
157000                   MOVE +1        TO INX                                  
157100                   MOVE WS-IDKOLLI-FOM TO WS-IDKOLLI-NUM                  
157200                   PERFORM UNTIL INX NOT < ARB-ANTAL-KOLLI-PLUS-1         
157300                       MOVE INX        TO PLATSIX                         
157400                       PERFORM CE-KOLLA-PLATSSATTNING                     
157500                       ADD 1           TO WS-IDKOLLI-NUM                  
157600                                          INX                             
157700                   END-PERFORM                                            
157800                ELSE                                                      
157900                   MOVE +1             TO PLATSIX                         
158000                   PERFORM CE-KOLLA-PLATSSATTNING                         
158100                END-IF                                                    
158200             END-IF                                                       
158300         END-IF                                                           
158400                                                                          
158500     .                                                                    
158600     SKIP3                                                                
158700 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
158800                                                                          
158900     IF REQU-IDPRODNR-KEY = ALL '+'                                       
159000         IF REQU-IDDISTR-KEY  = ALL '+'                                   
159100           AND REQU-IDKUNDNR-KEY = ALL '+'                                
159200           AND REQU-IDORDNR-KEY  = ALL '+'                                
159300             IF WS-IDPRODNR > ZERO                                        
159400*                << ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT >>              
159500                 MOVE JA              TO  SOEK-VIA-PRODNR                 
159600             ELSE                                                         
159700                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
159800             END-IF                                                       
159900         ELSE                                                             
160000             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
160100         END-IF                                                           
160200     ELSE                                                                 
160300         MOVE JA                       TO SOEK-VIA-PRODNR                 
160400     END-IF                                                               
160500                                                                          
160600     IF WS-INDATA-RATT                                                    
160700         MOVE WS-IDPRODNR               TO   W-601-IDPRODNR               
160800         PERFORM CAB-KOLLA-MOT-WDE411-BSEQ                                
160900     END-IF                                                               
161000     .                                                                    
161100     SKIP2                                                                
161200 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
161300                                                                          
161400     MOVE WS-IDDISTR-NUM   TO W-4A1-IDDISTR                               
161500     MOVE WS-IDKUNDNR-NUM  TO W-4A1-IDKUNDNR                              
161600     MOVE WS-IDORDNR       TO W-4A1-IDORDNR                               
161700                                                                          
161800     PERFORM IMS-GU-KUNDORDER-SEK                                         
161900                                                                          
162000     IF KUNDORDER-SEK-FINNS                                               
162100       PERFORM UNTIL (KORD-IDDC = REQU-IDDC-KEY AND                       
162200         KORD-KVORDRAD-LEVPL = ZERO) OR KUNDORDER-SEK-SAKNAS              
162300          PERFORM IMS-GN-SEQA-WDE4A1                                      
162400       END-PERFORM                                                        
162500                                                                          
162600       IF KUNDORDER-SEK-FINNS                                             
162700         MOVE KORD-IDPRODNR TO W-601-IDPRODNR                             
162800         PERFORM IMS-GU-WDE601                                            
162900         IF SEGMENT-FINNS                                                 
163000           MOVE VORD-IDPRODNR TO WS-IDPRODNR                              
163100         ELSE                                                             
163200           MOVE FEL                       TO   WS-INDATA-TEST             
163300           MOVE 'IDORDNR'                 TO RESP-IDELMT-ERROR            
163400           MOVE FEL-XX-MISSING            TO RESP-IDMSG-ERROR             
163500         END-IF                                                           
163600       ELSE                                                               
163700         MOVE FEL                          TO   WS-INDATA-TEST            
163800         MOVE 'IDORDNR'                    TO RESP-IDELMT-ERROR           
163900         MOVE FEL-XX-MISSING               TO RESP-IDMSG-ERROR            
164000       END-IF                                                             
164100     ELSE                                                                 
164200       MOVE FEL                          TO   WS-INDATA-TEST              
164300       MOVE 'IDORDNR'                    TO RESP-IDELMT-ERROR             
164400       MOVE FEL-XX-MISSING               TO RESP-IDMSG-ERROR              
164500     END-IF                                                               
164600     .                                                                    
164700     EJECT                                                                
164800 CAB-KOLLA-MOT-WDE411-BSEQ  SECTION.                                      
164900                                                                          
165000     MOVE W-601-IDPRODNR                TO W-420-IDPRODNR-MIN             
165100                                           W-420-IDPRODNR-MAX             
165200                                           W-420-IDPRODNR                 
165300     MOVE 1                             TO W-420-IDPURAD-MIN              
165400                                           W-420-IDPURAD                  
165500     MOVE 99999                         TO W-420-IDPURAD-MAX              
165600     PERFORM IMS-GU-KUNDORDER-SEK-INV                                     
165700                                                                          
165800     IF SEGMENT-FINNS                                                     
165900        MOVE KORD-IDDISTR               TO WS-IDDISTR-NUM                 
166000        MOVE WS-IDDISTR-NUM             TO RESP-IDDISTR-KEY               
166100        INSPECT RESP-IDDISTR-KEY REPLACING                                
166200                LEADING ZERO BY SPACE                                     
166300        MOVE KORD-IDKUNDNR              TO WS-IDKUNDNR-NUM                
166400        MOVE WS-IDKUNDNR-NUM            TO RESP-IDKUNDNR-KEY              
166500        INSPECT RESP-IDKUNDNR-KEY REPLACING                               
166600                LEADING ZERO BY SPACE                                     
166700        MOVE KORD-IDKUNDRF              TO WS-IDKUNDRF                    
166800        MOVE WS-IDORDNR                 TO RESP-IDORDNR-KEY               
166900        INSPECT RESP-IDORDNR-KEY  REPLACING                               
167000                LEADING ZERO BY SPACE                                     
167100        MOVE KORD-KDFRAKT               TO PLATS-KDFRAKT                  
167200        MOVE KORD-KDORDKL               TO PLATS-KDORDKLX                 
167300                                           WS-KDORDKL                     
167400        MOVE KORD-IDORDER               TO WS-KORD-IDORDER                
167500        MOVE KORD-IDORDER               TO W-201-IDORDER                  
167600*-----------------------------------------------ÄR ORDERN                 
167700*-----------------------------------------------FÄRDIGRAPPORTERAD         
167800     ELSE                                                                 
167900         MOVE FEL                       TO WS-INDATA-TEST                 
168000         MOVE 'IDORDNR'                 TO RESP-IDELMT-ERROR              
168100         MOVE FEL-XX-MISSING            TO RESP-IDMSG-ERROR               
168200     END-IF                                                               
168300     .                                                                    
168400     EJECT                                                                
168500 CB-KONTROLLERA-IDKOLLI SECTION.                                          
168600                                                                          
168700     IF KOLLI-INTERVALL                                                   
168800         MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                              
168900         IF DIST03-SVERIGE                                                
169000           OR (WS-KDORDKL = 0 OR 1 OR 2 OR 3)                             
169100           CONTINUE                                                       
169200         ELSE                                                             
169300             IF  WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1 >                    
169400             PLATSIX-MAX-OEVR                                             
169500                                                                          
169600               IF WS-INDATA-RATT                                          
169700                 MOVE FEL               TO WS-INDATA-TEST                 
169800                 MOVE PLATSIX-MAX-OEVR  TO FEL-PLATSIX-C2                 
169900                 MOVE FEL-827           TO RESP-IDELMT-ERROR              
170000               END-IF                                                     
170100             END-IF                                                       
170200         END-IF                                                           
170300         IF REQU-IDRADNR-TOM (1) NOT = ZERO                               
170400           IF WS-INDATA-RATT                                              
170500             MOVE FEL                    TO   WS-INDATA-TEST              
170600             MOVE FEL-ONLY-ONE-LINE      TO  RESP-IDMSG-ERROR             
170700           END-IF                                                         
170800         END-IF                                                           
170900         IF REQU-IDRADNR-FOM (2) NOT = ALL '+'                            
171000           IF WS-INDATA-RATT                                              
171100             MOVE FEL                    TO   WS-INDATA-TEST              
171200             MOVE FEL-MORE-THAN-ONE-INT  TO   RESP-IDMSG-ERROR            
171300           END-IF                                                         
171400         END-IF                                                           
171500         IF WS-INDATA-RATT                                                
171600             IF    WS-IDKOLLI-FOM       = ZERO                            
171700               OR  WS-IDKOLLI-TOM       = ZERO                            
171800                 MOVE FEL                TO   WS-INDATA-TEST              
171900                MOVE 'IDKOLLI-FOM-TOM'   TO RESP-IDELMT-ERROR             
172000                 MOVE FEL-INVALID-KEY-XX TO   RESP-IDMSG-ERROR            
172100             END-IF                                                       
172200             COMPUTE ARB-ANTAL-KOLLI =                                    
172300                         WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1              
172400             COMPUTE ARB-ANTAL-KOLLI-PLUS-1 =                             
172500                         ARB-ANTAL-KOLLI + 1                              
172600             MOVE 1                      TO   ACK-KOLLI                   
172700             PERFORM IMS-GHU-KOLLIREG                                     
172800                                                                          
172900             IF VORD-IDDC NOT = REQU-IDDC-KEY                             
173000                MOVE FEL                 TO   WS-INDATA-TEST              
173100                MOVE FEL-XX-MISSING      TO RESP-IDMSG-ERROR              
173200                MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR             
173300             END-IF                                                       
173400                                                                          
173500             IF VORD-KDORDSTA > 2                                         
173600                MOVE FEL                 TO   WS-INDATA-TEST              
173700                MOVE FEL-ORDER-REPORTED  TO   RESP-IDMSG-ERROR            
173800             END-IF                                                       
173900                                                                          
174000             IF VORD-KDMETOD = +3                                         
174100                MOVE FEL                  TO WS-INDATA-TEST               
174200*               MOVE FEL-USESCREEN-L0197-ORL0199                          
174300                MOVE ERR-ORDER-HAS-WRONG-STATUS                           
174400                  TO RESP-IDMSG-ERROR                                     
174500             END-IF                                                       
174600                                                                          
174700             MOVE VORD-KDFRAKT           TO   PLATS-KDFRAKT               
174800             MOVE VORD-KDORDKL           TO   PLATS-KDORDKLX              
174900                                              WS-KDORDKL                  
175000             MOVE VORD-IDDC              TO   PLATS-IDDC                  
175100             MOVE VORD-IDDISTR           TO   PLATS-IDDISTR               
175200             MOVE VORD-IDKUNDNR          TO   PLATS-IDKUNDNR              
175300             MOVE VORD-FLAUTFAK          TO   WS-FLAUTFAK                 
175400             MOVE VORD-KDFAKTYP          TO   WS-KDFAKTYP                 
175500                                                                          
175600             IF   VORD-KDORDKL = 4                                        
175700               IF  VORD-IDDISTR = +00878                                  
175800               AND VORD-IDKUNDNR > +006000                                
175900                  MOVE 2                 TO   PLATS-KDCALL                
176000               ELSE                                                       
176100                  MOVE 1                 TO   PLATS-KDCALL                
176200               END-IF                                                     
176300             ELSE                                                         
176400                  MOVE 2                 TO   PLATS-KDCALL                
176500             END-IF                                                       
176600                                                                          
176700             MOVE WS-IDKOLLI-FOM         TO   WS-IDKOLLI-NUM              
176800             PERFORM UNTIL ACK-KOLLI NOT < ARB-ANTAL-KOLLI-PLUS-1         
176900                 MOVE WS-IDKOLLI-NUM     TO   W-611-IDKOLLI               
177000                 PERFORM IMS-GHNP-KOLLI                                   
177100                 IF SEGMENT-FINNS                                         
177200                     MOVE FEL            TO   WS-INDATA-TEST              
177300                     MOVE FEL-CASE-INT-REPORTED                           
177400                                         TO RESP-IDMSG-ERROR              
177500                 END-IF                                                   
177600                 ADD 1                   TO   ACK-KOLLI                   
177700                                              WS-IDKOLLI-NUM              
177800             END-PERFORM                                                  
177900         END-IF                                                           
178000     ELSE                                                                 
178100                                                                          
178200         IF WS-IDKOLLI                   =    ZERO                        
178300             MOVE FEL                    TO   WS-INDATA-TEST              
178400             MOVE FEL-INVALID-KEY-XX     TO   RESP-IDMSG-ERROR            
178500             MOVE 'IDKOLLI'              TO RESP-IDELMT-ERROR             
178600         END-IF                                                           
178700         IF WS-INDATA-RATT                                                
178800             MOVE WS-IDPRODNR            TO   W-601-IDPRODNR              
178900             MOVE WS-IDKOLLI             TO   W-421-IDKOLLI               
179000                                              W-611-IDKOLLI               
179100             PERFORM IMS-GHU-KOLLIREG                                     
179200             IF VORD-IDDC = REQU-IDDC-KEY                                 
179300               IF VORD-KDMETOD = +3                                       
179400                 MOVE FEL                TO WS-INDATA-TEST                
179500*                MOVE FEL-USESCREEN-L0197-ORL0199                         
179600                 MOVE ERR-ORDER-HAS-WRONG-STATUS                          
179700                   TO RESP-IDMSG-ERROR                                    
179800               ELSE                                                       
179900                 MOVE VORD-KDFRAKT       TO   PLATS-KDFRAKT               
180000                 MOVE VORD-KDORDKL       TO   PLATS-KDORDKLX              
180100                                                WS-KDORDKL                
180200                 MOVE VORD-IDDC          TO   PLATS-IDDC                  
180300                 MOVE VORD-IDDISTR       TO   PLATS-IDDISTR               
180400                 MOVE VORD-IDKUNDNR      TO   PLATS-IDKUNDNR              
180500                 MOVE VORD-FLAUTFAK      TO   WS-FLAUTFAK                 
180600                 MOVE VORD-KDFAKTYP      TO   WS-KDFAKTYP                 
180700                                                                          
180800                 IF WS-KDORDKL = 4                                        
180900                   IF VORD-IDDISTR = +00878                               
181000                   AND VORD-IDKUNDNR > +006000                            
181100                      MOVE 2             TO   PLATS-KDCALL                
181200                   ELSE                                                   
181300                      MOVE 1             TO   PLATS-KDCALL                
181400                   END-IF                                                 
181500                 ELSE                                                     
181600                      MOVE 2             TO   PLATS-KDCALL                
181700                 END-IF                                                   
181800                                                                          
181900                 PERFORM IMS-GHNP-KOLLI                                   
182000                 IF SEGMENT-FINNS                                         
182100                     MOVE FEL             TO   WS-INDATA-TEST             
182200                     MOVE FEL-CASE-REPORTED TO RESP-IDMSG-ERROR           
182300                 ELSE                                                     
182400                     MOVE 1               TO   ARB-ANTAL-KOLLI            
182500                 END-IF                                                   
182600               END-IF                                                     
182700             ELSE                                                         
182800*---------------------------------------------FELAKTIGT                   
182900*---------------------------------------------DC-LAGER                    
183000*                                                                         
183100               MOVE FEL                  TO WS-INDATA-TEST                
183200               MOVE 'IDPRODNR'           TO RESP-IDELMT-ERROR             
183300               MOVE FEL-DC-NOT-MATCH     TO RESP-IDMSG-ERROR              
183400             END-IF                                                       
183500         END-IF                                                           
183600     END-IF                                                               
183700     .                                                                    
183800     EJECT                                                                
183900 CC-KONTROLLERA-PACKARE SECTION.                                          
184000                                                                          
184100     MOVE NEJ                     TO WS-TRAEFF-PACKARE                    
184200     MOVE JA                      TO WS-PACKARES-ODEL-REDAN-KLARA         
184300     MOVE WS-IDDISTR-NUM                TO W-4A1-IDDISTR                  
184400     MOVE WS-IDKUNDNR-NUM               TO W-4A1-IDKUNDNR                 
184500     MOVE WS-IDORDNR                    TO W-4A1-IDORDNR                  
184600     PERFORM IMS-GU-KUNDORDER-SEK                                         
184700     IF KUNDORDER-SEK-FINNS                                               
184800                                                                          
184900        PERFORM UNTIL KUNDORDER-SEK-SAKNAS                                
185000          MOVE KORD-IDDISTR             TO W-401-IDDISTR                  
185100          MOVE KORD-IDKUNDNR            TO W-401-IDKUNDNR                 
185200          MOVE KORD-IDORDNR5            TO W-401-IDORDNR                  
185300          MOVE KORD-IDPRODNR            TO W-401-IDPRODNR                 
185400          MOVE KORD-IDPLKLST            TO W-401-IDPLKLST                 
185500          PERFORM IMS-GU-WDE401                                           
185600          MOVE KORD-IDPRODNR         TO   WS-JFR-IDPRODNR                 
185700          MOVE KORD-IDUSER           TO   WS-JFR-IDANSTNR                 
185800                                                                          
185900          IF WS-IDPRODNR = WS-JFR-IDPRODNR AND                            
186000             WS-IDANSTNR = WS-JFR-IDANSTNR-5                              
186100             MOVE 'J'                TO   WS-TRAEFF-PACKARE               
186200             PERFORM CCA-KONTOLLERA-PLOCKLISTA                            
186300          END-IF                                                          
186400          PERFORM IMS-GN-SEQA-WDE4A1                                      
186500        END-PERFORM                                                       
186600     END-IF                                                               
186700                                                                          
186800     IF WS-TRAEFF-PACKARE  = 'N'                                          
186900*----------------------------------------------SAKNAS ANGIVEN             
187000*----------------------------------------------PACKARE PÅ ORDERN          
187100        MOVE FEL                        TO   WS-INDATA-TEST               
187200        MOVE FEL-PACKER-ORDER           TO   RESP-IDMSG-ERROR             
187300*----------------------------------------------ÄR ANGIVEN PACKARES        
187400*----------------------------------------------ORDERDEL REDAN KLAW        
187500     ELSE                                                                 
187600       IF WS-INDATA-RATT   AND                                            
187700          WS-PACKARES-ODEL-REDAN-KLARA = JA                               
187800         MOVE FEL               TO   WS-INDATA-TEST                       
187900         MOVE FEL-ORDER-PART-READY                                        
188000                                TO RESP-IDMSG-ERROR                       
188100       END-IF                                                             
188200     END-IF                                                               
188300     .                                                                    
188400     EJECT                                                                
188500 CCA-KONTOLLERA-PLOCKLISTA               SECTION.                         
188600                                                                          
188700     IF KORD-KDPAKOLL NOT = ZERO                                          
188800*-----------------------------------------FÅR MAN EJ RAPPORTERA DÅ        
188900*-----------------------------------------AVVIKELSEKONTROLL PÅGÅR         
189000        MOVE FEL                 TO   WS-INDATA-TEST                      
189100        MOVE FEL-DEV-IN-PROGRESS TO RESP-IDMSG-ERROR                      
189200     ELSE                                                                 
189300                                                                          
189400       IF ( (KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD)       OR             
189500            (KORD-KVORDRAD-LEVPL    > 0                    AND            
189600             KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD-LEVPL) )              
189700         MOVE NEJ             TO WS-PACKARES-ODEL-REDAN-KLARA             
189800       END-IF                                                             
189900     END-IF                                                               
190000     .                                                                    
190100     EJECT                                                                
190200 CD-KONTROLLERA-KOLLIUPPG  SECTION.                                       
190300                                                                          
190400     PERFORM CDA-KONTROLLERA-VIKT                                         
190500     IF WS-KDKOLLI NOT = SPACE                                            
190600         MOVE WS-KDKOLLI TO W-KDKOLLI-WDK5                                
190700         PERFORM IMS-GET-EMBB                                             
190800         IF SEGMENT-SAKNAS                                                
190900             MOVE FEL                   TO WS-INDATA-TEST                 
191000             MOVE 'KDKOLLI'             TO RESP-IDELMT-ERROR              
191100             MOVE '041'                 TO RESP-IDMSG-ERROR               
191200         ELSE                                                             
191300*------------------------------------------VISSA KOLLIKODER GER           
191400*------------------------------------------EJ ALLA MÅTT. DÅ SKALL         
191500*------------------------------------------DESSA KOMPLETTERAS.            
191600             MOVE EMB-KDKOLLID          TO WS-KDKOLLID                    
191700             MOVE EMB-VKTARA            TO WS-EMB-VKTARA-ONE-CASE         
191800                                                                          
191900             IF  EMB-EMBPROF NOT = SPACE                                  
192000                 MOVE EMB-EMBPROF       TO WS-EMBPROF                     
192100                 MOVE EMB-KVPALL        TO WS-KVPALL                      
192200                 MOVE EMB-KVLOCK        TO WS-KVLOCK                      
192300                 MOVE EMB-KVRAM         TO WS-KVRAM                       
192400             END-IF                                                       
192500                                                                          
192600             IF WS-KDEMBTYP > ZERO                                        
192700                 MOVE '023'             TO RESP-IDMSG-ERROR               
192800                 MOVE 'KDEMBTYP'        TO RESP-IDELMT-ERROR              
192900                 MOVE FEL               TO WS-INDATA-TEST                 
193000             ELSE                                                         
193100                 MOVE EMB-KDEMBTYP      TO WS-KDEMBTYP                    
193200             END-IF                                                       
193300             IF WS-DIKOLLIL = ZERO                                        
193400                 IF EMB-DIKOLLIL = ZERO                                   
193500                   IF  DIST03-SVERIGE                                     
193600                   CONTINUE                                               
193700                   ELSE                                                   
193800                     MOVE FEL           TO WS-INDATA-TEST                 
193900                     MOVE FEL-MORE-CASE-INFO-NEEDED                       
194000                                        TO RESP-IDMSG-ERROR               
194100                   END-IF                                                 
194200                 ELSE                                                     
194300                     MOVE EMB-DIKOLLIL   TO WS-DIKOLLIL                   
194400                 END-IF                                                   
194500             END-IF                                                       
194600             IF WS-DIKOLLIH = ZERO                                        
194700                 IF EMB-DIKOLLIH = ZERO                                   
194800                   IF  DIST03-SVERIGE                                     
194900                   CONTINUE                                               
195000                   ELSE                                                   
195100                     IF WS-INDATA-RATT                                    
195200                       MOVE FEL           TO WS-INDATA-TEST               
195300                       MOVE FEL-MORE-CASE-INFO-NEEDED                     
195400                                          TO RESP-IDMSG-ERROR             
195500                     END-IF                                               
195600                   END-IF                                                 
195700                 ELSE                                                     
195800                     MOVE EMB-DIKOLLIH   TO WS-DIKOLLIH                   
195900                 END-IF                                                   
196000             END-IF                                                       
196100             IF WS-DIKOLLIB = ZERO                                        
196200                 IF EMB-DIKOLLIB = ZERO                                   
196300                   IF  DIST03-SVERIGE                                     
196400                   CONTINUE                                               
196500                   ELSE                                                   
196600                     IF WS-INDATA-RATT                                    
196700                       MOVE FEL           TO WS-INDATA-TEST               
196800                       MOVE FEL-MORE-CASE-INFO-NEEDED                     
196900                                          TO RESP-IDMSG-ERROR             
197000                     END-IF                                               
197100                   END-IF                                                 
197200                 ELSE                                                     
197300                     MOVE EMB-DIKOLLIB  TO WS-DIKOLLIB                    
197400                 END-IF                                                   
197500             END-IF                                                       
197600         END-IF                                                           
197700     END-IF                                                               
197800                                                                          
197900     IF WS-DIKOLLIL > ZERO                                                
198000       IF US-MEASUREMENT                                                  
198100         COMPUTE WS-MOD-DIKOLLIL ROUNDED =                                
198200                 WS-DIKOLLIL * CONV-CM-TO-IN                              
198300         END-COMPUTE                                                      
198400         MOVE WS-MOD-DIKOLLIL           TO  RESP-DIKOLLIL                 
198500       ELSE                                                               
198600         MOVE WS-DIKOLLIL               TO  RESP-DIKOLLIL                 
198700       END-IF                                                             
198800     END-IF                                                               
198900                                                                          
199000     IF WS-DIKOLLIH > ZERO                                                
199100       IF US-MEASUREMENT                                                  
199200         COMPUTE WS-MOD-DIKOLLIH ROUNDED =                                
199300                 WS-DIKOLLIH * CONV-CM-TO-IN                              
199400         END-COMPUTE                                                      
199500         MOVE WS-MOD-DIKOLLIH           TO  RESP-DIKOLLIH                 
199600       ELSE                                                               
199700         MOVE WS-DIKOLLIH               TO  RESP-DIKOLLIH                 
199800       END-IF                                                             
199900     END-IF                                                               
200000                                                                          
200100     IF WS-DIKOLLIB > ZERO                                                
200200       IF US-MEASUREMENT                                                  
200300         COMPUTE WS-MOD-DIKOLLIB ROUNDED =                                
200400                 WS-DIKOLLIB * CONV-CM-TO-IN                              
200500         END-COMPUTE                                                      
200600         MOVE WS-MOD-DIKOLLIB           TO  RESP-DIKOLLIB                 
200700       ELSE                                                               
200800         MOVE WS-DIKOLLIB               TO  RESP-DIKOLLIB                 
200900       END-IF                                                             
201000     END-IF                                                               
201100                                                                          
201200     COMPUTE WS-KOLLI-VLORDBTO =                                          
201300                WS-DIKOLLIL * WS-DIKOLLIB * WS-DIKOLLIH                   
201400     END-COMPUTE                                                          
201500                                                                          
201600     .                                                                    
201700     EJECT                                                                
201800 CDA-KONTROLLERA-VIKT    SECTION.                                         
201900                                                                          
202000     MOVE REQU-VKORDBTO-KOLLI        TO DEC-IDFRIDATA                     
202100     MOVE 5                          TO DEC-KVHELTAL                      
202200     MOVE 1                          TO DEC-KVDECIMAL                     
202300     CALL WDECEDIT USING DEC-WDECAREA                                     
202400                                                                          
202500     MOVE WS-IDDISTR-NUM                TO TEST-IDDISTR                   
202600*--------OM DISTR = SVERIGE BEHÖVER BRUTTOVIKT EJ ANGES                   
202700     IF  ( DIST03-SVERIGE                    AND                          
202800          REQU-VKORDBTO-KOLLI = ALL '+' )                                 
202900     OR  ( DIST03-SVERIGE                    AND                          
203000         REQU-VKORDBTO-KOLLI NUMERIC        AND                           
203100          REQU-VKORDBTO-KOLLI = ZERO )                                    
203200     OR  ( DIST03-SVERIGE                    AND                          
203300           DEC-KDSVAR-OK )                                                
203400                                                                          
203500         IF DIST03-SVERIGE                                                
203600           IF DEC-KDSVAR-OK  AND DEC-IDEDITDATA > ZERO                    
203700               MOVE DEC-IDEDITDATA      TO WS-MOD-VKORDBTO                
203800               IF US-MEASUREMENT                                          
203900                 PERFORM S19-CONVERT-LB-TO-KG                             
204000               END-IF                                                     
204100               MOVE WS-MOD-VKORDBTO     TO PLATS-VKORDNTO-KOLLI           
204200           END-IF                                                         
204300         ELSE                                                             
204400           MOVE ZERO                    TO WS-MOD-VKORDBTO                
204500                                           PLATS-VKORDNTO-KOLLI           
204600         END-IF                                                           
204700     ELSE                                                                 
204800     IF REQU-VKORDBTO-KOLLI = ALL '+'                                     
204900        CONTINUE                                                          
205000     ELSE                                                                 
205100         IF DEC-KDSVAR-OK                                                 
205200         AND DEC-IDEDITDATA > ZERO                                        
205300             IF  DIST03-SVERIGE                                           
205400               CONTINUE                                                   
205500             ELSE                                                         
205600               MOVE DEC-IDEDITDATA      TO WS-MOD-VKORDBTO                
205700                                           PLATS-VKORDNTO-KOLLI           
205800               IF US-MEASUREMENT                                          
205900                 PERFORM S19-CONVERT-LB-TO-KG                             
206000               END-IF                                                     
206100               MOVE WS-MOD-VKORDBTO     TO PLATS-VKORDNTO-KOLLI           
206200             END-IF                                                       
206300*----------------------------------------ATT VI LADDAR NETTO-VIKT         
206400*----------------------------------------MED BRUTTO-VIKT ÄR OK            
206500         ELSE                                                             
206600           IF   DEC-KDSVAR-OK                                             
206700           AND  DEC-IDEDITDATA = ZERO                                     
206800           AND  DIST03-SVERIGE                                            
206900                MOVE DEC-IDEDITDATA      TO WS-MOD-VKORDBTO               
207000                                             PLATS-VKORDNTO-KOLLI         
207100           END-IF                                                         
207200         END-IF                                                           
207300     END-IF                                                               
207400     END-IF                                                               
207500     .                                                                    
207600     EJECT                                                                
207700 CE-KOLLA-PLATSSATTNING SECTION.                                          
207800     SKIP3                                                                
207900     MOVE ZERO                          TO PLATS-ADVMODUL                 
208000                                           PLATS-ADHMODUL                 
208100                                           PLATS-ADFLOMR                  
208200                                           PLATS-ADRUTNIV                 
208300                                           PLATS-IDTRPTNR                 
208400                                           PLATS-DIHMODUL                 
208500     MOVE SPACE                         TO PLATS-ADFLGEO                  
208600                                           PLATS-FLUTLAST                 
208700                                           PLATS-IDDC-CROSS               
208800     MOVE REQU-IDDC-KEY                 TO PLATS-IDDC                     
208900     MOVE WS-IDORDNR                    TO PLATS-IDORDNR                  
209000     MOVE WS-DIKOLLIL                   TO PLATS-DIKOLLIL                 
209100     MOVE WS-DIKOLLIB                   TO PLATS-DIKOLLIB                 
209200     MOVE WS-DIKOLLIH                   TO PLATS-DIKOLLIH                 
209300     MOVE WS-KDKOLLID                   TO PLATS-KDKOLLID                 
209400                                                                          
209500     IF WS-KDORDKL = +4                                                   
209600        IF VORD-IDDISTR = +00878                                          
209700        AND VORD-IDKUNDNR > +006000                                       
209800           MOVE 2                       TO PLATS-KDCALL                   
209900        ELSE                                                              
210000           MOVE 1                       TO PLATS-KDCALL                   
210100        END-IF                                                            
210200     ELSE                                                                 
210300        MOVE +2                         TO PLATS-KDCALL                   
210400     END-IF                                                               
210500                                                                          
210600     IF REQU-ADFLGEO NOT = ALL '+'                                        
210700        MOVE REQU-ADFLGEO                TO PLATS-ADFLGEO                 
210800                                           WS-ADFLGEO                     
210900     END-IF                                                               
211000     IF REQU-ADFLOMR NOT = ALL '+'                                        
211100        MOVE REQU-ADFLOMR                TO PLATS-ADFLOMR                 
211200                                           WS-ADFLOMR                     
211300     END-IF                                                               
211400     IF REQU-ADRUTNIV NOT = ALL '+'                                       
211500        MOVE REQU-ADRUTNIV               TO PLATS-ADRUTNIV                
211600                                           WS-ADRUTNIV                    
211700     END-IF                                                               
211800                                                                          
211900     CALL W403PLAT USING PLATS-W403PLAT                                   
212000                         PLATS-DM-PCB                                     
212100                         PLATS-DN-PCB                                     
212200                         PLATS-DP-PCB                                     
212300                         PLATS-DO-PCB                                     
212400                         PLATS-WDE6C-PCB                                  
212500                         PLATS-GMTC-PCB                                   
212600                         PLATS-WDB6-PCB                                   
212700                                                                          
212800     IF PLATS-KDSVAR = SPACE                                              
212900        MOVE PLATS-IDTRPTNR   TO WSTAB-IDTRPTNR   (PLATSIX)               
213000                                                                          
213100        MOVE PLATS-ADFLGEO    TO WSTAB-ADFLGEO    (PLATSIX)               
213200                                 ARB-ADFLGEO                              
213300                                 WS-ADFLGEO                               
213400        MOVE PLATS-ADFLOMR    TO WSTAB-ADFLOMR    (PLATSIX)               
213500                                 ARB-ADFLOMR                              
213600                                 WS-ADFLOMR                               
213700        MOVE PLATS-ADRUTNIV   TO WSTAB-ADRUTNIV   (PLATSIX)               
213800                                 ARB-ADRUTNIV                             
213900                                 WS-ADRUTNIV                              
214000        MOVE PLATS-DIHMODUL   TO WSTAB-DIHMODUL   (PLATSIX)               
214100        MOVE PLATS-DIDMODUL   TO WSTAB-DIDMODUL   (PLATSIX)               
214200        MOVE PLATS-ADVMODUL   TO WSTAB-ADVMODUL   (PLATSIX)               
214300        MOVE PLATS-ADHMODUL   TO WSTAB-ADHMODUL   (PLATSIX)               
214400        MOVE PLATS-FLUTLAST   TO WSTAB-FLUTLAST   (PLATSIX)               
214500        MOVE PLATS-IDDC-CROSS TO WSTAB-IDDC-CROSS (PLATSIX)               
214600                                                                          
214700*       MOVE '001'              TO  RESP-IDMSG-INFO                       
214800     ELSE                                                                 
214900        IF    WS-ADFLOMR  = ZERO                                          
215000           OR WS-ADRUTNIV = ZERO                                          
215100           OR WS-ADFLGEO  = SPACE                                         
215200           MOVE FEL               TO WS-INDATA-TEST                       
215300           MOVE FEL-GEN-ADRESS    TO RESP-IDMSG-ERROR                     
215400        ELSE                                                              
215500           MOVE FEL               TO WS-INDATA-TEST                       
215600           MOVE 'ADFLGEOGR'       TO RESP-IDELMT-ERROR                    
215700           MOVE '023'             TO RESP-IDMSG-ERROR                     
215800        END-IF                                                            
215900     END-IF                                                               
216000     .                                                                    
216100     EJECT                                                                
216200 CG-OTHER-CONTROL       SECTION.                                          
216300*DIRLEV DC11                                                              
216400     IF VORD-FLDIRLEV = NEJ OR                                            
216500        VORD-IDDC     = WS-CDC-SE                                         
216600       CONTINUE                                                           
216700     ELSE                                                                 
216800       IF WS-INDATA-RATT                                                  
216900         MOVE FEL-PACK-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
217000         MOVE FEL                  TO WS-INDATA-TEST                      
217100       END-IF                                                             
217200     END-IF                                                               
217300                                                                          
217400     PERFORM  IMS-GU-ORQI01                                               
217500     .                                                                    
217600     SKIP3                                                                
217700                                                                          
217800 D-LAGG-UPP-KOLLI-SEG   SECTION.                                          
217900                                                                          
218000     IF KOLLI-INTERVALL                                                   
218100                                                                          
218200         MOVE +1                       TO INX                             
218300         MOVE WS-IDKOLLI-FOM           TO WS-IDKOLLI-NUM                  
218400         PERFORM UNTIL INX NOT < ARB-ANTAL-KOLLI-PLUS-1                   
218500             PERFORM S09-SKAPA-KOLLI-SEGMENT                              
218600                                                                          
218700             PERFORM IMS-ISRT-KOLLI                                       
218800             IF SEGMENT-FINNS-REDAN                                       
218900                 MOVE FEL    TO WS-BEHANDLING-TEST                        
219000                 MOVE FEL-CASE-REPORTED                                   
219100                             TO RESP-IDMSG-ERROR                          
219200                                RESP-IDMSG-ERROR-LINE(INX)                
219300             END-IF                                                       
219400                                                                          
219500             ADD 1                     TO WS-IDKOLLI-NUM                  
219600                                          INX                             
219700         END-PERFORM                                                      
219800     ELSE                                                                 
219900         MOVE WS-IDKOLLI               TO WS-IDKOLLI-NUM                  
220000         PERFORM S09-SKAPA-KOLLI-SEGMENT                                  
220100                                                                          
220200         PERFORM IMS-ISRT-KOLLI                                           
220300                                                                          
220400         IF SEGMENT-FINNS-REDAN                                           
220500             MOVE FEL        TO WS-BEHANDLING-TEST                        
220600             MOVE FEL-CASE-REPORTED                                       
220700                             TO RESP-IDMSG-ERROR                          
220800                                RESP-IDMSG-ERROR-LINE(INX)                
220900         END-IF                                                           
221000         MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                              
221100     END-IF                                                               
221200     .                                                                    
221300     EJECT                                                                
221400 F-BEHANDLA-RADER     SECTION.                                            
221500                                                                          
221600     MOVE REQU-IDRADNR-FOM (INX)         TO   ARB-RAD-FOM                 
221700     MOVE REQU-IDRADNR-TOM (INX)         TO   ARB-RAD-TOM                 
221800                                                                          
221900     IF ARB-RAD-TOM                     =    ZERO                         
222000*-------------------------------------------ÄR DET EN DELAD RAD           
222100*-------------------------------------------ELLER EN AVVIKELSERAD         
222200         MOVE NEJ                       TO  FL-RAD-INTERVALL              
222300         MOVE ARB-RAD-FOM               TO  WS-START-RAD                  
222400                                            WS-SISTA-RAD                  
222500     ELSE                                                                 
222600         MOVE JA                        TO  FL-RAD-INTERVALL              
222700         COMPUTE WS-ANT-RADER-INT = ARB-RAD-TOM - ARB-RAD-FOM + 1         
222800         MOVE ARB-RAD-TOM               TO  WS-SISTA-RAD                  
222900         MOVE ARB-RAD-FOM               TO  WS-START-RAD                  
223000     END-IF                                                               
223100                                                                          
223200         MOVE 'N'                       TO WS-RADER-OK                    
223300         MOVE 'N'                       TO WS-TRAEFF-RAD                  
223400         MOVE WS-IDDISTR-NUM            TO W-4A1-IDDISTR                  
223500         MOVE WS-IDKUNDNR-NUM           TO W-4A1-IDKUNDNR                 
223600         MOVE WS-IDORDNR                TO W-4A1-IDORDNR                  
223700         PERFORM IMS-GU-KUNDORDER-SEK                                     
223800                                                                          
223900         IF KUNDORDER-SEK-FINNS                                           
224000                                                                          
224100            PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                         
224200                          WS-RADER-OK = 'J'                               
224300              MOVE KORD-IDDISTR           TO W-401-IDDISTR                
224400              MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR               
224500              MOVE KORD-IDORDNR5          TO W-401-IDORDNR                
224600              MOVE KORD-IDPRODNR          TO W-401-IDPRODNR               
224700              MOVE KORD-IDPLKLST          TO W-401-IDPLKLST               
224800                                             WS-IDPLKLST                  
224900              PERFORM IMS-GU-WDE401                                       
225000              MOVE KORD-IDPRODNR          TO WS-JFR-IDPRODNR              
225100              MOVE KORD-IDUSER            TO WS-JFR-IDANSTNR              
225200                                                                          
225300              MOVE KORD-IDORDER           TO WS-SPAR-IDORDER              
225400              MOVE KORD-IDDC              TO WS-SPAR-IDDC                 
225500                                                                          
225600              IF WS-IDPRODNR = WS-JFR-IDPRODNR                            
225700                 MOVE KORD-IDDC   TO TMS-IDDC                             
225800                 IF REQU-KVLEVART (INX) NOT NUMERIC                       
225900                    MOVE ZERO          TO   ARB-KVLEVART                  
226000                                            WS-KVLEVART                   
226100                 ELSE                                                     
226200                    MOVE REQU-KVLEVART (INX) TO  ARB-KVLEVART             
226300                                                 WS-KVLEVART              
226400                 END-IF                                                   
226500                                                                          
226600                 PERFORM FA-KONTROLLERA-RADER                             
226700                                                                          
226800                 IF KOLLI-INTERVALL AND INX = +1 AND                      
226900                    WS-TRAEFF-RAD = JA                                    
227000                    PERFORM FC-KOLLA-ANTAL-PER-KOLLI                      
227100                 END-IF                                                   
227200              END-IF                                                      
227300              PERFORM IMS-GN-SEQA-WDE4A1                                  
227400            END-PERFORM                                                   
227500                                                                          
227600            IF WS-TRAEFF-RAD = 'N'                                        
227700              MOVE FEL            TO WS-BEHANDLING-TEST                   
227800              MOVE FEL-INT-NO-PACKER                                      
227900                                  TO RESP-IDMSG-ERROR                     
228000                                     RESP-IDMSG-ERROR-LINE (INX)          
228100                                                                          
228200            END-IF                                                        
228300         END-IF                                                           
228400                                                                          
228500     IF WS-BEHANDLING-RATT                                                
228600       PERFORM FB-BEHANDLA-RAD-INOM-INTERVALL                             
228700                                                                          
228800                                                                          
228900       IF WS-BEHANDLING-RATT                                              
229000          MOVE WS-MOD-VKORDBTO        TO PLATS-VKORDNTO-KOLLI             
229100          MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                             
229200          IF NOT DIST19-SATS                                              
229300             PERFORM FD-UPPDATERA-PRODTAB                                 
229400          END-IF                                                          
229500       END-IF                                                             
229600     END-IF                                                               
229700     .                                                                    
229800     EJECT                                                                
229900 FA-KONTROLLERA-RADER SECTION.                                            
230000                                                                          
230100     SKIP3                                                                
230200     MOVE 'N'                         TO WS-RADER-OK                      
230300     MOVE 'N'                         TO FL-SLINGA-KLAR                   
230400     MOVE ARB-RAD-FOM                 TO ARB-RAD-AKTUELL                  
230500     MOVE ARB-RAD-AKTUELL             TO W-420-IDPURAD2                   
230600                                                                          
230700     IF WS-IDANSTNR = WS-JFR-IDANSTNR-5                                   
230800        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
230900                      SLINGA-KLAR                                         
231000          PERFORM IMS-GU-RAD                                              
231100          IF SEGMENT-SAKNAS                                               
231200            MOVE 'J'               TO FL-SLINGA-KLAR                      
231300          ELSE                                                            
231400            MOVE 'J'                  TO WS-TRAEFF-RAD                    
231500            IF WS-JFR-IDANSTNR-5 = '00000'                                
231600               MOVE 'J'         TO FL-SLINGA-KLAR                         
231700               MOVE FEL         TO WS-BEHANDLING-TEST                     
231800               MOVE FEL-ORDER-NOT-SPLIT                                   
231900                                TO RESP-IDMSG-ERROR                       
232000                                   RESP-IDMSG-ERROR-LINE (INX)            
232100            ELSE                                                          
232200              IF ORAD-KDRADSTA > 3                                        
232300                 MOVE 'J'       TO FL-SLINGA-KLAR                         
232400                 MOVE FEL       TO WS-BEHANDLING-TEST                     
232500                 MOVE FEL-INTERVAL-REPORTED                               
232600                                TO RESP-IDMSG-ERROR                       
232700                                   RESP-IDMSG-ERROR-LINE (INX)            
232800              ELSE                                                        
232900                IF ORAD-FLNOLLJ = JA                                      
233000                   MOVE 'J'     TO FL-SLINGA-KLAR                         
233100                   MOVE FEL     TO WS-BEHANDLING-TEST                     
233200                   MOVE FEL-LINE-ZEROED                                   
233300                                TO RESP-IDMSG-ERROR                       
233400                                   RESP-IDMSG-ERROR-LINE (INX)            
233500                END-IF                                                    
233600              END-IF                                                      
233700            END-IF                                                        
233800          END-IF                                                          
233900                                                                          
234000          IF SEGMENT-FINNS AND                                            
234100             WS-TRAEFF-RAD = 'J'                                          
234200             COMPUTE ARB-RAD-AKTUELL = ARB-RAD-AKTUELL + 1                
234300             END-COMPUTE                                                  
234400             IF ARB-RAD-AKTUELL > ARB-RAD-TOM                             
234500                MOVE 'J' TO FL-SLINGA-KLAR                                
234600                MOVE 'J' TO WS-RADER-OK                                   
234700             ELSE                                                         
234800                MOVE ARB-RAD-AKTUELL TO W-420-IDPURAD2                    
234900             END-IF                                                       
235000          END-IF                                                          
235100                                                                          
235200          IF SEGMENT-SAKNAS AND                                           
235300             WS-TRAEFF-RAD = 'J'                                          
235400             IF ARB-RAD-AKTUELL NOT > ARB-RAD-TOM                         
235500                MOVE 'J'    TO FL-SLINGA-KLAR                             
235600                MOVE FEL    TO WS-BEHANDLING-TEST                         
235700                MOVE FEL-WRONG-INTERVAL                                   
235800                            TO RESP-IDMSG-ERROR                           
235900                               RESP-IDMSG-ERROR-LINE (INX)                
236000                MOVE SPACE  TO TEMFSINF1                                  
236100             END-IF                                                       
236200          END-IF                                                          
236300        END-PERFORM                                                       
236400     END-IF                                                               
236500     .                                                                    
236600     EJECT                                                                
236700 FB-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
236800                                                                          
236900     SKIP3                                                                
237000     MOVE ZERO                   TO  WS-RINT-ANT-FPACK-ORAD               
237100                                                                          
237200     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR-MIN                   
237300                                     W-420-IDPRODNR-MAX                   
237400                                     W-420-IDPRODNR                       
237500     MOVE WS-START-RAD           TO  W-420-IDPURAD-MIN                    
237600                                     W-420-IDPURAD                        
237700                                     WS-AKTUELL-RAD                       
237800     MOVE WS-SISTA-RAD           TO  W-420-IDPURAD-MAX                    
237900     MOVE JA                     TO  FL-RAD-INOM-INTERVALL                
238000                                                                          
238100     PERFORM IMS-GHU-RAD-SEK                                              
238200                                                                          
238300     PERFORM UNTIL (NOT RAD-FINNS-I-INTERVALL)                            
238400               OR  (NOT WS-BEHANDLING-RATT)                               
238500                                                                          
238600      PERFORM FBA-SPARA-RAD-INFO                                          
238700      IF WS-BEHANDLING-RATT                                               
238800                                                                          
238900       PERFORM FBB-UPPDATERA-RAD                                          
239000       IF WS-BEHANDLING-RATT                                              
239100         IF KOLLI-INTERVALL                                               
239200             MOVE 1                      TO   ACK-KOLLI                   
239300             MOVE WS-IDKOLLI-FOM         TO   WS-IDKOLLI-NUM              
239400             PERFORM UNTIL ACK-KOLLI NOT < ARB-ANTAL-KOLLI-PLUS-1         
239500                 MOVE WS-IDKOLLI-NUM     TO   WS-IDKOLLI                  
239600                 PERFORM FBC-LAGG-UPP-KOLLI-KOPPL                         
239700                 ADD 1                   TO   ACK-KOLLI                   
239800                                              WS-IDKOLLI-NUM              
239900             END-PERFORM                                                  
240000             COMPUTE ARB-KOLLI-KVORDRAD                                   
240100                  =  ARB-KOLLI-KVORDRAD                                   
240200                  / (ARB-ANTAL-KOLLI-PLUS-1 - 1)                          
240300                                                                          
240400             COMPUTE ARB-KOLLI-KVFALRAD                                   
240500                  =  ARB-KOLLI-KVFALRAD                                   
240600                  / (ARB-ANTAL-KOLLI-PLUS-1 - 1)                          
240700                                                                          
240800         ELSE                                                             
240900             PERFORM FBC-LAGG-UPP-KOLLI-KOPPL                             
241000         END-IF                                                           
241100         ADD 1 TO WS-AKTUELL-RAD                                          
241200         IF WS-AKTUELL-RAD > WS-SISTA-RAD                                 
241300             MOVE NEJ TO FL-RAD-INOM-INTERVALL                            
241400         ELSE                                                             
241500             PERFORM IMS-GHN-RAD-SEK                                      
241600         END-IF                                                           
241700       END-IF                                                             
241800      END-IF                                                              
241900     END-PERFORM                                                          
242000     .                                                                    
242100     EJECT                                                                
242200 FBA-SPARA-RAD-INFO        SECTION.                                       
242300                                                                          
242400     MOVE ORAD-VKARTNTO         TO SPAR-PRAD-VKARTNTO                     
242500     MOVE ORAD-KVFLAMP          TO SPAR-PRAD-KVFLAMP                      
242600     MOVE ORAD-KDFARLIG         TO SPAR-PRAD-KDFARLIG                     
242700     MOVE ORAD-PRARTNTO         TO SPAR-PRAD-PRARTNTO                     
242800     MOVE ORAD-PRAVCOST         TO SPAR-PRAD-PRAVCOST                     
242900     MOVE ORAD-PRARTNTO-LOC     TO SPAR-PRAD-PRARTNTO-LOC                 
243000     MOVE ORAD-PRARTNTO-LOCPREL TO SPAR-PRAD-PRARTNTO-LOCPREL             
243100     MOVE ORAD-KDVALISO         TO SPAR-PRAD-KDVALISO                     
243200     MOVE ORAD-KDVALISO-EXP     TO SPAR-PRAD-KDVALISO-EXP                 
243300                                                                          
243400     MOVE ORAD-IDPSN            TO SPAR-IDPSN                             
243500     MOVE ORAD-VKART-FG         TO SPAR-VKART-FG                          
243600     MOVE ORAD-VLFG             TO SPAR-VLFG                              
243700     MOVE ORAD-SUEQFG           TO SPAR-SUEQFG                            
243800                                                                          
243900     MOVE ORAD-IDARTNR          TO WS-SPAR-IDARTNR                        
244000     MOVE ORAD-BEART            TO WS-SPAR-BEART                          
244100     MOVE ORAD-KVBEART          TO WS-SPAR-KVBEART                        
244200     MOVE ORAD-FLTILLK          TO WS-SPAR-FLTILLK                        
244300     MOVE ORAD-IDKUNDRF-RO      TO WS-SPAR-IDKUNDRF-RO                    
244400     MOVE ORAD-IDPURAD          TO WS-SPAR-IDPURAD                        
244500                                                                          
244600     IF WS-KVLEVART             >   ORAD-KVAVBART                         
244700         MOVE FEL       TO WS-BEHANDLING-TEST                             
244800         MOVE FEL-LARGE-QUANT                                             
244900                        TO RESP-IDMSG-ERROR                               
245000                           RESP-IDMSG-ERROR-LINE (INX)                    
245100     ELSE                                                                 
245200         IF KOLLI-INTERVALL                                               
245300             MOVE ARB-KVLEVART  TO  SPAR-PRAD-KVLEVART                    
245400         ELSE                                                             
245500             IF WS-KVLEVART     =   ZERO                                  
245600               IF ORAD-KVLEVART > ZERO                                    
245700                 COMPUTE SPAR-PRAD-KVLEVART                               
245800                   = ORAD-KVAVBART - ORAD-KVLEVART                        
245900                 END-COMPUTE                                              
246000*PGA SATSORDERPROBLEM 031008                                              
246100                 IF  SPAR-PRAD-KVLEVART = ZERO                            
246200                 AND DIST19-SATS                                          
246300                 AND ORAD-KVAVBART = ORAD-KVLEVART                        
246400                   MOVE ORAD-KVLEVART TO SPAR-PRAD-KVLEVART               
246500                 END-IF                                                   
246600*PGA SATSORDERPROBLEM                                                     
246700               ELSE                                                       
246800                 MOVE ORAD-KVAVBART TO SPAR-PRAD-KVLEVART                 
246900               END-IF                                                     
247000             ELSE                                                         
247100                 MOVE WS-KVLEVART TO SPAR-PRAD-KVLEVART                   
247200             END-IF                                                       
247300         END-IF                                                           
247400*                                                                         
247500*    OBS OBS OBS                                                          
247600*    ÖPPNA DENNA IF-SATS VID TESTER I BTS OCH                             
247700*    STÄNG MOTSV. I STYR-SECTION                                          
247800*   (BTS KLARAR EJ AV ROLL-BACK)                                          
247900*        PERFORM D-LAGG-UPP-KOLLI-SEG                                     
248000*    SLUT BTS-SPECIAL                                                     
248100         PERFORM S10-UPPD-SPAR-KOLLI                                      
248200     END-IF                                                               
248300     .                                                                    
248400     EJECT                                                                
248500 FBB-UPPDATERA-RAD         SECTION.                                       
248600                                                                          
248700     IF ORAD-IDLEVNR NOT = SPACE                                          
248800       MOVE JA                  TO DIRLEV-KOLLI-SW                        
248900     END-IF                                                               
249000                                                                          
249100     IF  KOLLI-INTERVALL                                                  
249200         COMPUTE ORAD-KVLEVART = ORAD-KVLEVART                            
249300                               + SPAR-PRAD-KVLEVART                       
249400                               * (ARB-ANTAL-KOLLI-PLUS-1 - 1)             
249500                                                                          
249600     ELSE                                                                 
249700       IF ORAD-KVLEVART + SPAR-PRAD-KVLEVART > ORAD-KVAVBART              
249800                                                                          
249900         IF  DIST19-SATS                                                  
250000           MOVE ORAD-KVAVBART TO ORAD-KVLEVART                            
250100         ELSE                                                             
250200           MOVE FEL    TO WS-BEHANDLING-TEST                              
250300           MOVE FEL-LARGE-QUANT                                           
250400                       TO RESP-IDMSG-ERROR                                
250500                          RESP-IDMSG-ERROR-LINE (INX)                     
250600         END-IF                                                           
250700       ELSE                                                               
250800         COMPUTE ORAD-KVLEVART = ORAD-KVLEVART                            
250900                               + SPAR-PRAD-KVLEVART                       
251000                                                                          
251100                                                                          
251200       END-IF                                                             
251300     END-IF                                                               
251400                                                                          
251500*LK** VOLUME COMTROL                                                      
251600     IF SEGMENT-FINNS AND WS-TRAEFF-RAD = 'J'                             
251700       IF NOT DIST44-SCRAP-DIST                                           
251800         MOVE  REQU-IDDC-KEY    TO VOL-IDDC                               
251900         IF CONTROL-OF-CASE-NET-VOLUME                                    
252000           COMPUTE WS-ORAD-VLORDNTO =                                     
252100              SPAR-PRAD-KVLEVART * ORAD-VLARTNTO                          
252200           END-COMPUTE                                                    
252300           ADD WS-ORAD-VLORDNTO    TO WS-ORAD-VLORDNTO-SUM                
252400           IF WS-ORAD-VLORDNTO-SUM > WS-KOLLI-VLORDBTO                    
252500             MOVE 'J'         TO FL-SLINGA-KLAR                           
252600             MOVE FEL         TO WS-BEHANDLING-TEST                       
252700                                                                          
252800             MOVE '117'       TO RESP-IDMSG-ERROR                         
252900             MOVE 'VLART'     TO RESP-IDELMT-ERROR                        
253000             MOVE 'VOL'       TO RESP-IDMSG-ERROR-LINE (INX)              
253100           END-IF                                                         
253200           MOVE ZERO          TO WS-ORAD-VLORDNTO                         
253300         END-IF                                                           
253400       END-IF                                                             
253500     END-IF                                                               
253600                                                                          
253700     IF WS-BEHANDLING-RATT                                                
253800       IF ORAD-KVLEVART = ORAD-KVAVBART                                   
253900          MOVE +4           TO ORAD-KDRADSTA                              
254000          MOVE 'Y'          TO DATUM-SW                                   
254100                                                                          
254200          ADD 1 TO WS-TOT-ANT-RADER                                       
254300          ADD 1 TO  WS-RINT-ANT-FPACK-ORAD                                
254400          IF KORD-KDORDKL = +0                                            
254500             PERFORM FBBA-UPPDATERA-VOR-TIKLAR                            
254600          END-IF                                                          
254700       END-IF                                                             
254800                                                                          
254900       PERFORM IMS-REPL-BEHANDLAD-RAD                                     
255000       IF DATUM-SW = 'Y'                                                  
255100          PERFORM IMS-GHNP-RAD-E401                                       
255200             IF KORD-TIBEGPAC NOT = DAGENS-DATUM                          
255300                MOVE DAGENS-DATUM TO KORD-TIBEGPAC                        
255400                PERFORM IMS-REPL-BEHANDLAD-E401                           
255500             END-IF                                                       
255600          MOVE 'N'          TO DATUM-SW                                   
255700       END-IF                                                             
255800       PERFORM FBBB-EV-SKAPA-RYK-TRANS                                    
255900     END-IF                                                               
256000     .                                                                    
256100     EJECT                                                                
256200 FBBA-UPPDATERA-VOR-TIKLAR SECTION.                                       
256300                                                                          
256400     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
256500     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
256600     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
256700                                    W-A601KY-MAX-IDDISTR                  
256800     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
256900                                    W-A601KY-MAX-IDKUNDNR                 
257000     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
257100                                    W-A601KY-MAX-IDORDNR                  
257200                                                                          
257300     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
257400                                                                          
257500     PERFORM IMS-GHN-WDA6B                                                
257600     PERFORM UNTIL SEGMENT-SAKNAS                                         
257700                OR END-OF-DATABASE                                        
257800                OR SW-TIKLAR-UPPDATERAD = JA                              
257900                                                                          
258000         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
258100         AND VOR-TIKLAR = +0                                              
258200                                                                          
258300             MOVE DAGENS-DATUM      TO VOR-TIKLAR                         
258400             MOVE DAGENS-TID-HHMMSS TO VOR-TIKLATID                       
258500             PERFORM IMS-REPL-WDA6B                                       
258600             MOVE JA                TO SW-TIKLAR-UPPDATERAD               
258700         END-IF                                                           
258800                                                                          
258900         PERFORM IMS-GHN-WDA6B                                            
259000     END-PERFORM                                                          
259100     .                                                                    
259200 FBBB-EV-SKAPA-RYK-TRANS SECTION.                                         
259300                                                                          
259400     IF ORAD-IDKUNDRF-RO NOT = '00000     ' AND                           
259500        ORAD-TIRODAT         > ZERO                                       
259600                                                                          
259700       IF LOGG-IDLOGLOP = 9                                               
259800         MOVE ZERO                TO   LOGG-IDLOGLOP                      
259900       END-IF                                                             
260000                                                                          
260100       ACCEPT  LOGG-TIAAMMDD      FROM DATE                               
260200       ACCEPT  LOGG-TIKLOCK       FROM TIME                               
260300       ADD     +1                 TO   LOGG-IDLOGLOP                      
260400                                                                          
260500       MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                        
260600                                       W-WDQ2C-IDDISTR                    
260700       MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                       
260800                                       W-WDQ2C-IDKUNDNR                   
260900       MOVE    ORAD-IDKUNDRF-RO(1:5)                                      
261000                                  TO   W-WDQ2C-IDORDNR5                   
261100                                                                          
261200*****  FIX-START-DEL1 930303 FÖR ATT TA HAND OM EN ORDER SOM              
261300*      SAKNAR ORDERHUVUD (WDQ2) OBS, VID ANVÄNDANDE ÖPPNA OCKSÅ           
261400*      FIX-DEL2 I SECTION S13.                                            
261500       PERFORM IMS-GET-ORQI01-CSEQ                                        
261600       IF SEGMENT-FINNS                                                   
261700          MOVE OHUV-IDORDER       TO   RYK-IDORDER                        
261800       ELSE                                                               
261900          MOVE +0                 TO   RYK-IDORDER                        
262000       END-IF                                                             
262100*****  FIX-END-DEL1 930303                                                
262200       MOVE    'RYK'              TO   RYK-IDPTYP                         
262300                                       LOGG-IDPTYP                        
262400                                                                          
262500       MOVE    ORAD-IDARTNR       TO   RYK-IDARTNR                        
262600       MOVE    LOGG-TIAAMMDD      TO   RYK-TIRODAT                        
262700       MOVE    ORAD-KVLEVART      TO   RYK-KVLEVART                       
262800       MOVE    ORAD-KVBEART       TO   RYK-KVBEART-Q                      
262900       MOVE    ORAD-KDORDKL       TO   RYK-KDORDKL                        
263000       MOVE    ORAD-KDPRODSL      TO   RYK-KDPRODSL                       
263100       MOVE    ZERO               TO   RYK-KDORDBEK                       
263200                                                                          
263300       MOVE    SPACE              TO   LOGG-SORTPOST                      
263400       MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                      
263500                                                                          
263600       PERFORM IMS-ISRT-ZZAC01                                            
263700                                                                          
263800       PERFORM UNTIL SEGMENT-FINNS                                        
263900         IF LOGG-IDLOGLOP = 9                                             
264000           MOVE ZERO              TO LOGG-IDLOGLOP                        
264100           ACCEPT  LOGG-TIKLOCK   FROM TIME                               
264200         END-IF                                                           
264300         ADD +1                   TO LOGG-IDLOGLOP                        
264400         PERFORM IMS-ISRT-ZZAC01                                          
264500       END-PERFORM                                                        
264600     END-IF                                                               
264700     .                                                                    
264800     EJECT                                                                
264900 FBC-LAGG-UPP-KOLLI-KOPPL  SECTION.                                       
265000     SKIP3                                                                
265100     MOVE WS-IDPRODNR        TO KKOLLI-IDPRODNR                           
265200     MOVE WS-IDKOLLI         TO KKOLLI-IDKOLLI                            
265300     MOVE SPAR-PRAD-KVLEVART TO KKOLLI-KVLEVART                           
265400     PERFORM IMS-ISRT-KOLLI-KOPPL                                         
265500     IF SEGMENT-FINNS-REDAN                                               
265600         MOVE WS-IDPRODNR    TO  W-421-IDPRODNR                           
265700         MOVE WS-IDKOLLI     TO  W-421-IDKOLLI                            
265800         PERFORM IMS-GHNP-KOLLI-KOPPL                                     
265900         ADD SPAR-PRAD-KVLEVART  TO KKOLLI-KVLEVART                       
266000         PERFORM IMS-REPL-KOLLI-KOPPL                                     
266100     ELSE                                                                 
266200         ADD +1                  TO ARB-KOLLI-KVORDRAD                    
266300                                                                          
266400         IF SPAR-PRAD-KDFARLIG = +4 OR SPAR-PRAD-KDFARLIG = +7            
266500             ADD +1              TO ARB-KOLLI-KVFALRAD                    
266600         END-IF                                                           
266700     END-IF                                                               
266800*                                                                         
266900     IF REQU-IDDC-KEY  NOT  = W-IDDC-B6                                   
267000        MOVE REQU-IDDC-KEY TO W-IDDC-B6                                   
267100        PERFORM IMS-GU-WDB601                                             
267200     END-IF                                                               
267300     IF DCS-NDC-NA                                                        
267400        PERFORM S20-DATA-TILL-DEL-NOTE                                    
267500     END-IF                                                               
267600*                                                                         
267700                                                                          
267800     IF SPAR-IDPSN > ZERO                                                 
267900       PERFORM FBCA-SPARA-FG-DATA                                         
268000     END-IF                                                               
268100     .                                                                    
268200     EJECT                                                                
268300 FBCA-SPARA-FG-DATA SECTION.                                              
268400     SKIP3                                                                
268500     MOVE +1 TO FG-INDX                                                   
268600     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
268700                                                                          
268800       IF TAB-IDPSN(FG-INDX) = ZERO                                       
268900         MOVE SPAR-IDPSN TO TAB-IDPSN(FG-INDX)                            
269000         PERFORM S17-BERAEKNA-FG-FAELT                                    
269100                                                                          
269200       ELSE                                                               
269300         IF SPAR-IDPSN = TAB-IDPSN(FG-INDX)                               
269400           PERFORM S17-BERAEKNA-FG-FAELT                                  
269500         END-IF                                                           
269600       END-IF                                                             
269700                                                                          
269800       ADD +1 TO FG-INDX                                                  
269900     END-PERFORM                                                          
270000                                                                          
270100     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
270200                            (SPAR-SUEQFG      *                           
270300                             KKOLLI-KVLEVART)                             
270400     .                                                                    
270500     EJECT                                                                
270600 FC-KOLLA-ANTAL-PER-KOLLI  SECTION.                                       
270700                                                                          
270800     PERFORM S06-KOLLA-AVBOKAT-ANTAL                                      
270900     SKIP2                                                                
271000*-------------------------------------------ARB-KVLEVART ANVÄNDS          
271100*-------------------------------------------FÖR ANTAL/KOLLI MEDAN         
271200*-------------------------------------------WS-KVLEVART AVSER DET         
271300*-------------------------------------------TOTALA ANTALET/RAD            
271400     MOVE WS-KVLEVART                  TO   ARB-KVLEVART                  
271500     DIVIDE ARB-KVLEVART BY ARB-ANTAL-KOLLI                               
271600                               GIVING ARB-KVLEVART                        
271700                               REMAINDER WS-REST                          
271800     IF WS-REST > ZERO                                                    
271900*-------------------------------------------ÄR INTE ANTALET JÄMNT         
272000*-------------------------------------------DELBART I KOLLIINTERV.        
272100         MOVE FEL    TO WS-BEHANDLING-TEST                                
272200         MOVE FEL-LINE-QUANT                                              
272300                     TO RESP-IDMSG-ERROR                                  
272400                        RESP-IDMSG-ERROR-LINE (INX)                       
272500     END-IF                                                               
272600     .                                                                    
272700     EJECT                                                                
272800 FD-UPPDATERA-PRODTAB      SECTION.                                       
272900     SKIP3                                                                
273000                                                                          
273100     MOVE WS-IDPRODNR            TO W-420-IDPRODNR                        
273200     MOVE WS-IDPRODNR            TO W-420-IDPRODNR                        
273300     PERFORM IMS-GU-WDE42-KORD-BSEQ                                       
273400                                                                          
273500     MOVE KORD-IDORDER           TO W-301-IDORDER                         
273600     MOVE KORD-IDDC              TO W-301-IDDC                            
273700     MOVE KORD-IDPRODNR          TO W-301-IDPRODNR                        
273800     MOVE KORD-IDPLKLST          TO W-301-IDPLKLST                        
273900     PERFORM IMS-GU-ORQA01                                                
274000     IF  SEGMENT-FINNS                                                    
274100       MOVE ODEL-IDTRP           TO WS-ODEL-IDTRP                         
274200       MOVE ODEL-IDLEVNR         TO WS-IDLEVNR                            
274300       MOVE ODEL-IDDC-EXP        TO WS-ODEL-IDDC-EXP                      
274400     END-IF                                                               
274500                                                                          
274600     IF  WS-RINT-ANT-FPACK-ORAD > ZERO                                    
274700*      * RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                         
274800                                                                          
274900       PERFORM FDA-LAES-SHIFTTAB                                          
275000                                                                          
275100       IF  ODEL-KDPRODKL = 'B'                                            
275200       OR  ODEL-KDPRODKL = 'C'                                            
275300*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
275400                                                                          
275500         MOVE KORD-IDDC          TO W-4471-IDDC                           
275600         MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                       
275700         MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                       
275800         PERFORM IMS-GHU-XXKW11                                           
275900                                                                          
276000         IF  SEGMENT-FINNS                                                
276100*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
276200                                                                          
276300           MOVE 1                TO IND1                                  
276400           MOVE W-4478-IDSHIFT   TO IND2                                  
276500           MOVE ODEL-DARFS       TO HJALP-ODEL-DARFS                      
276600           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
276700                                                                          
276800           PERFORM UNTIL IND1 = 30 OR                                     
276900                         4472-TIRFS (IND1) = ZERO OR                      
277000                         HJALP-ODEL-DARFS-6 = HJALP-4472-TIRFS-6          
277100             ADD 1                TO IND1                                 
277200             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
277300           END-PERFORM                                                    
277400                                                                          
277500           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
277600           MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)              
277700           ADD WS-RINT-ANT-FPACK-ORAD                                     
277800                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
277900           PERFORM FDB-ADDERA-TOTAL-PRODTID                               
278000           PERFORM IMS-REPL-XXKW11                                        
278100         END-IF                                                           
278200       END-IF                                                             
278300     END-IF                                                               
278400     .                                                                    
278500     EJECT                                                                
278600 FDA-LAES-SHIFTTAB         SECTION.                                       
278700                                                                          
278800     MOVE KORD-IDDC         TO W-4477-IDDC                                
278900     MOVE '1'               TO W-4478-IDSHIFT                             
279000     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
279100     PERFORM IMS-GU-XXLB                                                  
279200                                                                          
279300     IF SEGMENT-SAKNAS                                                    
279400        MOVE '2'            TO W-4478-IDSHIFT                             
279500        PERFORM IMS-GU-XXLB                                               
279600                                                                          
279700        IF SEGMENT-SAKNAS                                                 
279800           MOVE '3'         TO W-4478-IDSHIFT                             
279900           PERFORM IMS-GU-XXLB                                            
280000                                                                          
280100           IF SEGMENT-SAKNAS                                              
280200              MOVE '1'      TO W-4478-IDSHIFT                             
280300           END-IF                                                         
280400        END-IF                                                            
280500     END-IF                                                               
280600     .                                                                    
280700     EJECT                                                                
280800 FDB-ADDERA-TOTAL-PRODTID             SECTION.                            
280900                                                                          
281000     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
281100                                                                          
281200     COMPUTE WS-KVPTID-MIN ROUNDED = WS-RINT-ANT-FPACK-ORAD *             
281300                                     ODEL-KVPTID                          
281400     END-COMPUTE                                                          
281500                                                                          
281600     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
281700     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
281800     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
281900                            (WS-KVPTID-TIM * 60)                          
282000                                                                          
282100     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
282200     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
282300     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
282400     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
282500                            (WS-KVPTID-TIM * 60)                          
282600     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
282700                                                                          
282800     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
282900     .                                                                    
283000     EJECT                                                                
283100 G-UPPDATERA-KOLLIREG      SECTION.                                       
283200                                                                          
283300     PERFORM IMS-GHU-KOLLIREG                                             
283400     MOVE VORD-IDDISTR       TO  TEST-IDDISTR                             
283500                                 DIS128-IDDISTR                           
283600     MOVE VORD-IDKUNDNR      TO  TEST-IDKUNDNR                            
283700     MOVE VORD-KDORDKL       TO  WS-KDORDKL                               
283800     MOVE VORD-FLAUTFAK      TO  WS-FLAUTFAK                              
283900     MOVE VORD-DARFS         TO  WS-DARFS                                 
284000     IF VORD-KDORDSTA        =   1                                        
284100         MOVE 2              TO  VORD-KDORDSTA                            
284200     END-IF                                                               
284300     COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC + ARB-ANTAL-KOLLI              
284400* FIX-START FÖR ATT TA HAND OM EN W4T315-TRANS SOM GÅTT FRÅN              
284500* 0605 EFTER DET ATT R4T397/98-TRANS HAR GÅTT.                            
284600     COMPUTE VORD-KVORDRAD-PACK                                           
284700                         = VORD-KVORDRAD-PACK + WS-TOT-ANT-RADER          
284800* FIX-SLUT                                                                
284900                                                                          
285000         ADD ARB-ANTAL-KOLLI      TO  VORD-KVKOLLI                        
285100         MOVE    DAGENS-DATUM     TO  VORD-TIPACKN-SK                     
285200                                                                          
285300         IF REQU-VKORDBTO-KOLLI = ALL '+'                                 
285400           COMPUTE VORD-VKORDBTO ROUNDED                                  
285500                 = VORD-VKORDBTO +                                        
285600                   ARB-KOLLI-VKORDNTO * ARB-ANTAL-KOLLI                   
285700           END-COMPUTE                                                    
285800* FOR DIST03-SVERIGE GROSS WEIGHT WILL BE REPORTED TO                     
285900             COMPUTE WS-EMB-VKTARA-TOT-ORDER ROUNDED                      
286000                   = WS-EMB-VKTARA-ONE-CASE * ARB-ANTAL-KOLLI             
286100             END-COMPUTE                                                  
286200             ADD WS-EMB-VKTARA-TOT-ORDER  TO VORD-VKORDBTO                
286300         ELSE                                                             
286400           COMPUTE VORD-VKORDBTO ROUNDED                                  
286500                 = VORD-VKORDBTO  +                                       
286600                   WS-MOD-VKORDBTO * ARB-ANTAL-KOLLI                      
286700           END-COMPUTE                                                    
286800         END-IF                                                           
286900                                                                          
287000         COMPUTE VORD-VLORDBTO ROUNDED                                    
287100               = VORD-VLORDBTO + WS-VLORDBTO * ARB-ANTAL-KOLLI            
287200         END-COMPUTE                                                      
287300                                                                          
287400         COMPUTE VORD-SUORDV-PACK-LOC ROUNDED                             
287500               = VORD-SUORDV-PACK-LOC + ARB-KOLLI-SUORDV-LOC              
287600                                         * ARB-ANTAL-KOLLI                
287700         END-COMPUTE                                                      
287800                                                                          
287900         COMPUTE VORD-SUORDV-PACK-LOCPREL ROUNDED                         
288000               = VORD-SUORDV-PACK-LOCPREL +                               
288100               ARB-KOLLI-SUORDV-LOCPREL * ARB-ANTAL-KOLLI                 
288200         END-COMPUTE                                                      
288300                                                                          
288400         COMPUTE VORD-SUORDV-PACK ROUNDED                                 
288500             = VORD-SUORDV-PACK + ARB-KOLLI-SUORDV                        
288600                                 * ARB-ANTAL-KOLLI                        
288700         END-COMPUTE                                                      
288800         MOVE ARB-KOLLI-KDVALISO     TO VORD-KDVALISO                     
288900         MOVE ARB-KOLLI-KDVALISO-EXP TO VORD-KDVALISO-EXP                 
289000                                                                          
289100        IF  VORD-KDORDSTA < +3                                            
289200        AND VORD-KVKOLLI  > +0                                            
289300        AND (DIST03-SVERIGE OR DIST35-CDC-LDC-REFILL)                     
289400        AND (DCS-CDC OR (DCS-SDC AND DCS-SWEDEN))                         
289500                                                                          
289600          MOVE VORD-KDFRAKT   TO WS-KDFRAKT                               
289700          MOVE WS-KDFRAKT     TO FRAK01-KDFRAKT                           
289800          MOVE WS-IDKOLLI     TO WS-IDKOLLI-LR                            
289900                                                                          
290000          IF  FRAK01-SVERIGE2                                             
290100          OR  FRAK01-NORDEN                                               
290200          OR  FRAK01-KDFRAKT21                                            
290300          OR  FRAK01-KDFRAKT62                                            
290400          OR (WS-IDKOLLI-LR > 149 AND WS-IDKOLLI-LR < 200)                
290500          OR (WS-IDKOLLI-LR > 349 AND WS-IDKOLLI-LR < 400)                
290600                                                                          
290700             IF   VORD-FLDIRLEV = NEJ                                     
290800             AND  VORD-KDFRAKT  NOT = +17                                 
290900               IF NOT DIS128-FRAKTS                                       
291000                 MOVE JA          TO VORD-FLFRAKTS                        
291100               END-IF                                                     
291200             END-IF                                                       
291300           END-IF                                                         
291400        END-IF                                                            
291500                                                                          
291600     PERFORM IMS-REPL-KOLLIREG                                            
291700     SKIP2                                                                
291800     IF KOLLI-INTERVALL                                                   
291900         PERFORM GA-BEH-KOLLI-INTERVALL                                   
292000     ELSE                                                                 
292100         PERFORM GB-BEH-ENKELT-KOLLI                                      
292200     END-IF                                                               
292300     .                                                                    
292400     EJECT                                                                
292500 GA-BEH-KOLLI-INTERVALL   SECTION.                                        
292600                                                                          
292700     MOVE +1                      TO  INX                                 
292800     MOVE WS-IDKOLLI-FOM          TO  WS-IDKOLLI-NUM                      
292900     PERFORM UNTIL INX NOT < ARB-ANTAL-KOLLI-PLUS-1                       
293000         MOVE WS-IDKOLLI-NUM      TO  W-611-IDKOLLI                       
293100         PERFORM IMS-GHU-KOLLI                                            
293200         MOVE INX                 TO  PLATSIX                             
293300         PERFORM S11-UPPD-KOLLI-FRAN-ARB                                  
293400                                                                          
293500             IF KOLLI-KDKOLSTA    =   ZERO                                
293600                 IF INX > 15                                              
293700                   CONTINUE                                               
293800                 ELSE                                                     
293900                   PERFORM S18-SKAPA-FOLJESEDEL-TRANS                     
294000                 END-IF                                                   
294100                 MOVE 1               TO KOLLI-KDKOLSTA                   
294200                 MOVE    DAGENS-DATUM TO KOLLI-TIPACKN                    
294300                 MOVE DAGENS-TID(1:6) TO KOLLI-TIPACTID                   
294400                 MOVE WS-DARFS        TO KOLLI-DARFS                      
294500                 PERFORM S12-SKAPA-4322                                   
294600                 PERFORM S13-UPPDAT-KDORDSTA                              
294700                 MOVE KOLLI-IDKOLLI   TO TMS-IDKOLLI(INX)                 
294800             END-IF                                                       
294900         PERFORM S16-UPPD-FARLIGT-GODS-DATA                               
295000                                                                          
295100         PERFORM IMS-REPL-KOLLI                                           
295200         ADD +1 TO WS-IDKOLLI-NUM                                         
295300                   INX                                                    
295400     END-PERFORM                                                          
295500                                                                          
295600*LK TMS PACKING INFO                                                      
295700     MOVE KORD-IDDISTR       TO TMS-IDDISTR                               
295800     MOVE KORD-IDKUNDNR      TO TMS-IDKUNDNR                              
295900     MOVE KORD-IDORDNR5      TO TMS-IDORDNR7                              
296000     CALL W403TMS1 USING TMS-W403TMS1                                     
296100          TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                                
296200          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                          
296300          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                          
296400          TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                        
296500          TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                          
296600          TMS-WDK5-PCB TMS-WDQ2C-PCB                                      
296700                                                                          
296800     PERFORM S14-EV-SEND-PRINTTRANS                                       
296900     MOVE WS-IDKOLLI-FOM          TO  WS-IDKOLLI-NUM                      
297000                                                                          
297100     IF WS-IDKOLLI-TOM NOT = ZERO                                         
297200         MOVE WS-IDKOLLI-TOM           TO RESP-IDKOLLI-KEY                
297300     END-IF                                                               
297400     .                                                                    
297500     EJECT                                                                
297600 GB-BEH-ENKELT-KOLLI      SECTION.                                        
297700                                                                          
297800     MOVE WS-IDKOLLI              TO  W-611-IDKOLLI                       
297900     PERFORM IMS-GHU-KOLLI                                                
298000     MOVE +1                      TO  PLATSIX                             
298100     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
298200         IF KOLLI-KDKOLSTA        =   ZERO                                
298300               PERFORM S14-EV-SEND-PRINTTRANS                             
298400                 MOVE +1             TO  INX                              
298500                 PERFORM S18-SKAPA-FOLJESEDEL-TRANS                       
298600             MOVE 1               TO KOLLI-KDKOLSTA                       
298700             MOVE    DAGENS-DATUM TO KOLLI-TIPACKN                        
298800             MOVE DAGENS-TID(1:6) TO KOLLI-TIPACTID                       
298900             MOVE WS-DARFS        TO KOLLI-DARFS                          
299000             PERFORM S12-SKAPA-4322                                       
299100             PERFORM S13-UPPDAT-KDORDSTA                                  
299200*LK TMS PACKING INFO                                                      
299300             MOVE KORD-IDDISTR    TO TMS-IDDISTR                          
299400             MOVE KORD-IDKUNDNR   TO TMS-IDKUNDNR                         
299500             MOVE KORD-IDORDNR5   TO TMS-IDORDNR7                         
299600             MOVE KOLLI-IDKOLLI   TO TMS-IDKOLLI(1)                       
299700             CALL W403TMS1 USING TMS-W403TMS1                             
299800                 TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                         
299900                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
300000                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
300100                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
300200                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
300300                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
300400                                                                          
300500         END-IF                                                           
300600                                                                          
300700     PERFORM S16-UPPD-FARLIGT-GODS-DATA                                   
300800     PERFORM IMS-REPL-KOLLI                                               
300900     .                                                                    
301000     EJECT                                                                
301100 H-AVSLUT             SECTION.                                            
301200                                                                          
301300     IF WS-BEHANDLING-RATT                                                
301400       IF  TEMFSINF1 = SPACE                                              
301500       OR  TEMFSINF1 = LOW-VALUE                                          
301600           MOVE UPDATE-DONE              TO RESP-IDMSG-INFO               
301700       END-IF                                                             
301800                                                                          
301900       MOVE WS-IDKUNDNR-NUM              TO TRANSFER-KUND                 
302000       IF REQU-FLSISTAK               = 'Y'                               
302100           PERFORM HC-LADDA-4318-AREA                                     
302200       ELSE                                                               
302300        IF REQU-FLSISTAK               = 'N'                              
302400           PERFORM HC-LADDA-4318-PLUS                                     
302500        END-IF                                                            
302600       END-IF                                                             
302700       PERFORM HA-EV-LADDA-L198                                           
302800                                                                          
302900     ELSE                                                                 
303000       PERFORM IMS-ROLLBACK                                               
303100     END-IF                                                               
303200                                                                          
303300     .                                                                    
303400     EJECT                                                                
303500                                                                          
303600 HA-EV-LADDA-L198  SECTION.                                               
303700                                                                          
303800*    VILLKOR FÖR LOAD HÄMTADE UR W4031400                                 
303900*    EV BEHÖVS INGA VILLKOR UTAN FLYTTA ALLTID?                           
304000                                                                          
304100**   MOVE WS-IDKUNDNR-NUM      TO TRANSFER-KUND                           
304200**                                                                        
304300**   IF ((DCS-CDC AND (DIST08-URSP-RAPP                                   
304400**                  OR DIST08-URSP-RAPP-CDC                               
304500**                  OR DIST08-URSP-SPX))                                  
304600**        OR                                                              
304700**        (DCS-NDC-NA AND ((TRANSFER-KUNDNR AND                           
304800**                         DIST08-URSP-TRANSFER-NDC)                      
304900**                       OR (RETUR-KUNDNR AND                             
305000**                         DIST08-URSP-RETUR-NDC)))                       
305100**        OR                                                              
305200**        (DCS-NDC-NA AND DCS-IDLANDX2 = 'CA'                             
305300**                    AND DIST08-URSP-RAPP-CDC))                          
305400                                                                          
305500         MOVE WS-IDANSTNR      TO RESP-L198-IDANSTNR-KEY                  
305600         MOVE WS-IDDISTR-NUM   TO RESP-L198-IDDISTR-KEY                   
305700         MOVE WS-IDKUNDNR-NUM  TO RESP-L198-IDKUNDNR-KEY                  
305800         MOVE WS-IDORDNR       TO RESP-L198-IDORDNR-KEY                   
305900         MOVE WS-IDPRODNR      TO RESP-L198-IDPRODNR-KEY                  
306000         MOVE REQU-IDDC-KEY    TO RESP-L198-IDDC-KEY                      
306100                                                                          
306200         IF WS-IDKOLLI-TOM NOT = ZERO                                     
306300           MOVE WS-IDKOLLI-TOM TO RESP-L198-IDKOLLI-KEY                   
306400         ELSE                                                             
306500           MOVE WS-IDKOLLI     TO RESP-L198-IDKOLLI-KEY                   
306600         END-IF                                                           
306700                                                                          
306800         MOVE +1               TO INDX                                    
306900         PERFORM UNTIL INDX > +15                                         
307000           MOVE ALL-PLUS       TO RESP-L198-RAD (INDX)                    
307100           ADD +1              TO INDX                                    
307200         END-PERFORM                                                      
307300**   END-IF                                                               
307400                                                                          
307500     .                                                                    
307600 HC-LADDA-4318-AREA    SECTION.                                           
307700                                                                          
307800     MOVE WS-IDANSTNR              TO RESP-L123-IDANSTNR-KEY              
307900     MOVE WS-IDDISTR-NUM           TO RESP-L123-IDDISTR-KEY               
308000     MOVE WS-IDKUNDNR-NUM          TO RESP-L123-IDKUNDNR-KEY              
308100     MOVE WS-IDORDNR               TO RESP-L123-IDORDNR-KEY               
308200     MOVE WS-IDPRODNR              TO RESP-L123-IDPRODNR-KEY              
308300     MOVE REQU-IDDC-KEY            TO RESP-L123-IDDC-KEY                  
308400     IF WS-IDKOLLI-TOM NOT = ZERO                                         
308500         MOVE WS-IDKOLLI-TOM           TO RESP-L123-IDKOLLI-KEY           
308600     ELSE                                                                 
308700         MOVE WS-IDKOLLI               TO RESP-L123-IDKOLLI-KEY           
308800     END-IF                                                               
308900     MOVE '+'                      TO RESP-L123-FLSVAR                    
309000     .                                                                    
309100     EJECT                                                                
309200 HC-LADDA-4318-PLUS    SECTION.                                           
309300                                                                          
309400     MOVE ALL '+'                  TO RESP-WL0123I1                       
309500     .                                                                    
309600     EJECT                                                                
309700 K-UPPDAT-4726-4727 SECTION.                                              
309800     SKIP2                                                                
309900                                                                          
310000       MOVE '4726'                  TO W-4726-IDHTYP                      
310100*      IF  DIST03-SVERIGE                                                 
310200*      OR  DIST18-SKROT                                                   
310300*          MOVE JA                  TO W-4726-FLBATCH                     
310400*      ELSE                                                               
310500           MOVE NEJ                 TO W-4726-FLBATCH                     
310600*      END-IF                                                             
310700       MOVE LOW-VALUE               TO W-4726-LOWVALUE                    
310800                                                                          
310900       PERFORM IMS-GU-4726-ROT-KVAL                                       
311000                                                                          
311100       MOVE WS-IDDISTR-NUM          TO W-4726-IDDISTR                     
311200       MOVE WS-IDKUNDNR-NUM         TO W-4726-IDKUNDNR                    
311300       MOVE REQU-IDDC-KEY           TO W-4726-IDDC                        
311400       MOVE WS-KDFAKTYP             TO W-4726-KDFAKTYP                    
311500                                                                          
311600       PERFORM IMS-GNP-4726-UNDERSEG-KVAL                                 
311700                                                                          
311800       IF  SEGMENT-SAKNAS                                                 
311900           MOVE WS-IDDISTR-NUM      TO AUTFAKT-IDDISTR                    
312000           MOVE WS-IDKUNDNR-NUM     TO AUTFAKT-IDKUNDNR                   
312100           MOVE REQU-IDDC-KEY       TO AUTFAKT-IDDC                       
312200           MOVE WS-KDFAKTYP         TO AUTFAKT-KDFAKTYP                   
312300                                                                          
312400           PERFORM IMS-INSERT-4726-UNDERSEG                               
312500       END-IF                                                             
312600       MOVE WS-IDPRODNR             TO AUTFAKT-IDPRODNR                   
312700       MOVE ZERO                    TO AUTFAKT-IDSKEPPN                   
312800                                       AUTFAKT-PRFRAKT                    
312900                                                                          
313000       IF  DIST03-SVERIGE                                                 
313100           MOVE NEJ                 TO AUTFAKT-FLLASTA                    
313200       ELSE                                                               
313300           MOVE JA                  TO AUTFAKT-FLLASTA                    
313400       END-IF                                                             
313500                                                                          
313600       PERFORM IMS-INSERT-4727                                            
313700                                                                          
313800                                                                          
313900     .                                                                    
314000     EJECT                                                                
314100 L-HAMTA-ADRESS SECTION.                                                  
314200                                                                          
314300     MOVE WS-IDDISTR-NUM  TO W-4A1-IDDISTR                                
314400                             TEST-IDDISTR                                 
314500     MOVE WS-IDKUNDNR-NUM TO W-4A1-IDKUNDNR                               
314600     MOVE WS-IDORDNR      TO W-4A1-IDORDNR                                
314700     PERFORM IMS-GU-KUNDORDER-SEK                                         
314800     MOVE KORD-IDDISTR    TO W-401-IDDISTR                                
314900     MOVE KORD-IDKUNDNR   TO W-401-IDKUNDNR                               
315000     MOVE KORD-IDORDNR5   TO W-401-IDORDNR                                
315100     MOVE KORD-IDPRODNR   TO W-401-IDPRODNR                               
315200     MOVE KORD-IDPLKLST   TO W-401-IDPLKLST                               
315300     MOVE KORD-IDORDER    TO W-201-IDORDER                                
315400     .                                                                    
315500     EJECT                                                                
315600*    --- DISPATCHER SECTIONS                                              
315700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
315800                                                                          
315900     MOVE 'GETARG'               TO SUB-KDFUNC                            
316000     MOVE 'CARPARTS.LDC.CASEREPORTING2'     TO SUB-ADDISPABS              
316100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
316200                                                                          
316300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
316400                                                                          
316500     IF SUB-KDRC > 0                                                      
316600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
316700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
316800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
316900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
317000     END-IF                                                               
317100     .                                                                    
317200     SKIP3                                                                
317300 S02-RETURN-RESPONSE SECTION.                                             
317400                                                                          
317500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
317600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
317700                                                                          
317800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
317900                                                                          
318000     IF SUB-KDRC > 0                                                      
318100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
318200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
318300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
318400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
318500     END-IF                                                               
318600     .                                                                    
318700     EJECT                                                                
318800 S02-CLEAR-RESPONSE-FIELDS  SECTION.                                      
318900                                                                          
319000     MOVE SPACE                    TO RESP-KDKOLLI                        
319100     MOVE ZERO                     TO RESP-VKORDBTO-KOLLI                 
319200     MOVE REQU-FLSISTAK            TO RESP-FLSISTAK                       
319300     MOVE ZERO                     TO RESP-KDEMBTYP                       
319400     MOVE ZERO                     TO RESP-DIKOLLIL                       
319500     MOVE ZERO                     TO RESP-DIKOLLIB                       
319600     MOVE ZERO                     TO RESP-DIKOLLIH                       
319700     MOVE SPACE                    TO RESP-ADFLGEO                        
319800     MOVE SPACE                    TO RESP-ADFLOMR                        
319900     MOVE SPACE                    TO RESP-ADRUTNIV                       
320000     MOVE ZERO                     TO RESP-IDKOLLI-FOM                    
320100     MOVE ZERO                     TO RESP-IDKOLLI-TOM                    
320200     MOVE REQU-FLSKRIV-CLABEL      TO RESP-FLSKRIV-CLABEL                 
320300     MOVE REQU-FLSKRIV-DELNOTE     TO RESP-FLSKRIV-DELNOTE                
320400     MOVE ZERO                     TO RESP-IDKOLLI-SAMP                   
320500     EJECT                                                                
320600     .                                                                    
320700 S03-SPARA-RADINF SECTION.                                                
320800     IF RESP-IDMSG-ERROR NOT = SPACE                                      
320900       MOVE +1 TO DX                                                      
321000       PERFORM UNTIL DX > 200                                             
321100         IF  REQU-IDRADNR-FOM (DX) NOT = ALL '+'                          
321200           IF  REQU-IDRADNR-FOM (DX) NUMERIC                              
321300             MOVE REQU-IDRADNR-FOM (DX)  TO RESP-IDRADNR-FOM(DX)          
321400           END-IF                                                         
321500         END-IF                                                           
321600         IF  REQU-IDRADNR-TOM (DX) NOT = ALL '+'                          
321700           IF  REQU-IDRADNR-TOM(DX) NUMERIC                               
321800             MOVE REQU-IDRADNR-TOM (DX)  TO RESP-IDRADNR-TOM(DX)          
321900           END-IF                                                         
322000         END-IF                                                           
322100         IF REQU-KVLEVART (DX) NOT = ALL '+'                              
322200           IF REQU-KVLEVART(DX) NUMERIC                                   
322300             MOVE REQU-KVLEVART (DX)     TO RESP-KVLEVART(DX)             
322400           END-IF                                                         
322500         END-IF                                                           
322600         ADD +1 TO DX                                                     
322700                                                                          
322800       END-PERFORM                                                        
322900     END-IF                                                               
323000     EJECT                                                                
323100     .                                                                    
323200 S04-MOVE-REQU-FIELDS-TO-RESP SECTION.                                    
323300                                                                          
323400     MOVE REQU-KDKOLLI         TO RESP-KDKOLLI                            
323500     IF REQU-VKORDBTO-KOLLI NUMERIC                                       
323600       MOVE REQU-VKORDBTO-KOLLI  TO RESP-VKORDBTO-KOLLI                   
323700     END-IF                                                               
323800     MOVE REQU-FLSISTAK        TO RESP-FLSISTAK                           
323900     IF REQU-KDEMBTYP NUMERIC                                             
324000     MOVE REQU-KDEMBTYP        TO RESP-KDEMBTYP                           
324100     END-IF                                                               
324200     IF REQU-DIKOLLIL  NUMERIC                                            
324300       MOVE REQU-DIKOLLIL        TO RESP-DIKOLLIL                         
324400     END-IF                                                               
324500     IF REQU-DIKOLLIB  NUMERIC                                            
324600       MOVE REQU-DIKOLLIB      TO RESP-DIKOLLIB                           
324700     END-IF                                                               
324800     IF REQU-DIKOLLIH  NUMERIC                                            
324900       MOVE REQU-DIKOLLIH      TO RESP-DIKOLLIH                           
325000     END-IF                                                               
325100     MOVE REQU-ADFLGEO         TO RESP-ADFLGEO                            
325200     MOVE REQU-ADFLOMR         TO RESP-ADFLOMR                            
325300     MOVE REQU-ADRUTNIV        TO RESP-ADRUTNIV                           
325400     IF  REQU-IDKOLLI-FOM NUMERIC                                         
325500       MOVE REQU-IDKOLLI-FOM     TO RESP-IDKOLLI-FOM                      
325600     END-IF                                                               
325700     IF  REQU-IDKOLLI-TOM NUMERIC                                         
325800       MOVE REQU-IDKOLLI-TOM     TO RESP-IDKOLLI-TOM                      
325900     END-IF                                                               
326000     MOVE REQU-FLSKRIV-CLABEL  TO RESP-FLSKRIV-CLABEL                     
326100     MOVE REQU-FLSKRIV-DELNOTE TO RESP-FLSKRIV-DELNOTE                    
326200     IF RESP-IDKOLLI-SAMP NUMERIC                                         
326300       MOVE REQU-IDKOLLI-SAMP    TO RESP-IDKOLLI-SAMP                     
326400     END-IF                                                               
326500     MOVE REQU-RAD(RAD-INX)      TO RESP-RAD(RAD-INX)                     
326600     .                                                                    
326700     EJECT                                                                
326800 S06-KOLLA-AVBOKAT-ANTAL    SECTION.                                      
326900     SKIP3                                                                
327000     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR                       
327100     MOVE ARB-RAD-FOM            TO  W-420-IDPURAD                        
327200     SKIP2                                                                
327300     PERFORM IMS-GHU-RAD-SEK                                              
327400     SKIP2                                                                
327500     IF WS-KVLEVART = ZERO                                                
327600         COMPUTE WS-KVLEVART = ORAD-KVAVBART - ORAD-KVLEVART              
327700     END-IF                                                               
327800     SKIP2                                                                
327900     IF WS-KVLEVART > (ORAD-KVAVBART - ORAD-KVLEVART)                     
328000         MOVE FEL                TO  WS-BEHANDLING-TEST                   
328100         MOVE FEL-LARGE-QUANT    TO RESP-IDMSG-ERROR                      
328200     END-IF                                                               
328300     .                                                                    
328400     EJECT                                                                
328500 S09-SKAPA-KOLLI-SEGMENT    SECTION.                                      
328600                                                                          
328700     MOVE WS-IDDISTR-NUM    TO   TEST-IDDISTR                             
328800     IF DIST19-SATS                                                       
328900       MOVE SPACE           TO  KOLLI-IDTRP                               
329000     ELSE                                                                 
329100       MOVE WS-ODEL-IDTRP   TO  KOLLI-IDTRP                               
329200     END-IF                                                               
329300                                                                          
329400     MOVE WS-IDKOLLI-NUM    TO  KOLLI-IDKOLLI                             
329500     MOVE WS-IDANSTNR       TO  KOLLI-IDPLOCK                             
329600     MOVE WS-IDDISTR-NUM    TO  KOLLI-IDDISTR                             
329700     MOVE WS-IDKUNDNR-NUM   TO  KOLLI-IDKUNDNR                            
329800     MOVE WS-ADFLOMR        TO  KOLLI-ADFLOMR                             
329900     MOVE WS-ADRUTNIV       TO  KOLLI-ADRUTNIV                            
330000     MOVE WS-DIKOLLIL       TO  KOLLI-DIKOLLIL                            
330100     MOVE WS-DIKOLLIB       TO  KOLLI-DIKOLLIB                            
330200     MOVE WS-DIKOLLIH       TO  KOLLI-DIKOLLIH                            
330300     MOVE WS-KDEMBTYP       TO  KOLLI-KDEMBTYP                            
330400     MOVE WS-MOD-VKORDBTO   TO  KOLLI-VKORDBTO-KOLLI                      
330500     MOVE REQU-IDDC-KEY     TO  KOLLI-IDDC                                
330600     MOVE WS-ADFLGEO        TO  KOLLI-ADFLGEO                             
330700     MOVE WS-KDKOLLI        TO  KOLLI-KDKOLLI                             
330800     MOVE ZERO              TO  KOLLI-IDKOLLI-SAMP                        
330900     MOVE NEJ               TO  KOLLI-FLBANDST                            
331000                                KOLLI-FLFRSUTS                            
331100     MOVE ZERO              TO  KOLLI-IDKOLLI-FLER                        
331200                                KOLLI-IDTRPTNR                            
331300                                KOLLI-ADVMODUL                            
331400                                KOLLI-ADHMODUL                            
331500                                KOLLI-IDFAKLOP                            
331600                                KOLLI-IDFAKT                              
331700                                KOLLI-IDFAKT-EXP                          
331800                                KOLLI-DIDMODUL                            
331900                                KOLLI-DIHMODUL                            
332000                                KOLLI-KVFALRAD                            
332100                                KOLLI-KDKOLSTA                            
332200                                KOLLI-KDORDKL                             
332300                                KOLLI-TIFAKT                              
332400                                KOLLI-TIFAKT-EXP                          
332500                                KOLLI-TIFAKTID                            
332600                                KOLLI-TIFAKTID-EXP                        
332700                                KOLLI-TILASTN                             
332800                                KOLLI-TILASTID                            
332900                                KOLLI-TIPACKN                             
333000                                KOLLI-TIPACTID                            
333100                                KOLLI-TIPACTID                            
333200                                KOLLI-VKORDNTO-KOLLI                      
333300                                KOLLI-SUORDV-KOLLI                        
333400                                KOLLI-SUORDV-KLI-EXP                      
333500                                KOLLI-SUORDV-LOC                          
333600                                KOLLI-SUORDV-LOCPREL                      
333700                                KOLLI-KDARTURS-KOLLI                      
333800                                KOLLI-KVORDRAD                            
333900                                KOLLI-TIAAVVD-PATR                        
334000                                KOLLI-KDFARLIG-KOLLI                      
334100                                KOLLI-KVFLAMP-KOLLI                       
334200                                KOLLI-IDLASTN                             
334300* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
334400* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
334500                                KOLLI-DASUPREF                            
334600                                KOLLI-TISUPTID                            
334700                                KOLLI-KDVIA                               
334800                                KOLLI-IDTULLNR                            
334900                                KOLLI-RETULKS                             
335000                                KOLLI-IDSHIPM                             
335100     MOVE SPACE             TO  KOLLI-IDLEVNR                             
335200                                KOLLI-KDVALISO                            
335300                                KOLLI-KDVALISO-EXP                        
335400                                KOLLI-FILLERX2                            
335500                                                                          
335600     MOVE +1 TO FG-INDX                                                   
335700     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
335800       MOVE ZERO            TO KOLLI-IDPSN(FG-INDX)                       
335900                               KOLLI-VKART-FG(FG-INDX)                    
336000                               KOLLI-VLFG(FG-INDX)                        
336100       ADD +1 TO FG-INDX                                                  
336200     END-PERFORM                                                          
336300     MOVE ZERO              TO KOLLI-SUEQFG                               
336400                               KOLLI-DARFS                                
336500     MOVE SPACE             TO  KOLLI-FLUTLAST                            
336600                                KOLLI-IDSUPREF                            
336700                                KOLLI-FLAUTFAK                            
336800                                KOLLI-FLTULLG                             
336900                                KOLLI-IDLBBET                             
337000                                KOLLI-IDTULFTG                            
337100                                KOLLI-KDSTASKLI                           
337200                                                                          
337300     COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                               
337400       KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000         
337500*--------------------------------------- KOLLI BREDD, HÖJD OCH            
337600*--------------------------------------- LÄNGD ANGIVNA I CM MEDAN         
337700*--------------------------------------- BRUTTOVOLYM I KUBIK M.           
337800     MOVE KOLLI-VLORDBTO-KOLLI TO WS-VLORDBTO                             
337900     .                                                                    
338000     EJECT                                                                
338100 S10-UPPD-SPAR-KOLLI    SECTION.                                          
338200                                                                          
338300     MOVE WS-IDDISTR-NUM     TO TEST-IDDISTR                              
338400                                                                          
338500     COMPUTE ARB-KOLLI-VKORDNTO ROUNDED = ARB-KOLLI-VKORDNTO +            
338600                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
338700                                                                          
338800     IF DIST79-DEALER-PRICE                                               
338900      IF  ORAD-PRARTNTO-LOCPREL > ZERO                                    
339000       COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL        
339100           + SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART              
339200      ELSE                                                                
339300       COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC                
339400           + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                  
339500      END-IF                                                              
339600     ELSE                                                                 
339800       IF DIST79-ECOM-PRICE                                               
339900         COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC              
340000             + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                
340100       ELSE                                                               
340200         COMPUTE ARB-KOLLI-SUORDV-EXP = ARB-KOLLI-SUORDV-EXP +            
340300          SPAR-PRAD-PRAVCOST * SPAR-PRAD-KVLEVART                         
340400         COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                    
340500          SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                         
340600       END-IF                                                             
340700     END-IF                                                               
340800     MOVE SPAR-PRAD-KDVALISO     TO ARB-KOLLI-KDVALISO                    
340900     MOVE SPAR-PRAD-KDVALISO-EXP TO ARB-KOLLI-KDVALISO-EXP                
341000                                                                          
341100     IF   SPAR-PRAD-KVFLAMP   >  ZERO                                     
341200     AND (SPAR-PRAD-KVFLAMP   <  ARB-KOLLI-KVFLAMP                        
341300     OR   ARB-KOLLI-KVFLAMP   =  ZERO)                                    
341400       MOVE SPAR-PRAD-KVFLAMP  TO ARB-KOLLI-KVFLAMP                       
341500     END-IF                                                               
341600                                                                          
341700     IF  SPAR-PRAD-KDFARLIG  =  +2 OR +3 OR +4 OR +7                      
341800     AND SPAR-PRAD-KDFARLIG  >  ARB-KOLLI-KDFARLIG                        
341900       MOVE SPAR-PRAD-KDFARLIG TO ARB-KOLLI-KDFARLIG                      
342000     END-IF                                                               
342100                                                                          
342200     .                                                                    
342300     EJECT                                                                
342400 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
342500                                                                          
342600     ADD ARB-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                        
342700     ADD ARB-KOLLI-KVORDRAD      TO KOLLI-KVORDRAD                        
342800     ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDNTO-KOLLI                  
342900     ADD ARB-KOLLI-SUORDV-LOC    TO KOLLI-SUORDV-LOC                      
343000     ADD ARB-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                 
343100     ADD ARB-KOLLI-SUORDV        TO KOLLI-SUORDV-KOLLI                    
343200     ADD ARB-KOLLI-SUORDV-EXP    TO KOLLI-SUORDV-KLI-EXP                  
343300     MOVE ARB-KOLLI-KDVALISO     TO KOLLI-KDVALISO                        
343400     MOVE ARB-KOLLI-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                    
343500                                                                          
343600     IF      ARB-KOLLI-KVFLAMP   >  ZERO                                  
343700        AND (ARB-KOLLI-KVFLAMP   <  KOLLI-KVFLAMP-KOLLI                   
343800        OR   KOLLI-KVFLAMP-KOLLI =  ZERO)                                 
343900         MOVE ARB-KOLLI-KVFLAMP  TO KOLLI-KVFLAMP-KOLLI                   
344000     END-IF                                                               
344100                                                                          
344200     IF ARB-KOLLI-KDFARLIG       >  KOLLI-KDFARLIG-KOLLI                  
344300         MOVE ARB-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
344400     END-IF                                                               
344500     SKIP2                                                                
344600     MOVE WS-KDKOLLI             TO KOLLI-KDKOLLI                         
344700     MOVE WS-KDORDKL             TO KOLLI-KDORDKL                         
344800     MOVE WS-FLAUTFAK            TO KOLLI-FLAUTFAK                        
344900     MOVE WS-DIKOLLIL            TO KOLLI-DIKOLLIL                        
345000     MOVE WS-DIKOLLIH            TO KOLLI-DIKOLLIH                        
345100     MOVE WS-DIKOLLIB            TO KOLLI-DIKOLLIB                        
345200     MOVE WS-KDEMBTYP            TO KOLLI-KDEMBTYP                        
345300*WEIGHT                                                                   
345400     IF REQU-VKORDBTO-KOLLI = ALL '+'                                     
345500* FOR DIST03-SVERIGE GROSS WEIGHT WILL BE REPORTED TO                     
345600         COMPUTE KOLLI-VKORDBTO-KOLLI ROUNDED =                           
345700                 ARB-KOLLI-VKORDNTO + WS-EMB-VKTARA-ONE-CASE              
345800         END-COMPUTE                                                      
345900     ELSE                                                                 
346000       MOVE WS-MOD-VKORDBTO           TO KOLLI-VKORDBTO-KOLLI             
346100     END-IF                                                               
346200                                                                          
346300     IF   KOLLI-VKORDNTO-KOLLI > KOLLI-VKORDBTO-KOLLI                     
346400          MOVE KOLLI-VKORDBTO-KOLLI   TO KOLLI-VKORDNTO-KOLLI             
346500     END-IF                                                               
346600*                                                                         
346700     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
346800       ADD 0.1                        TO KOLLI-VKORDBTO-KOLLI             
346900     END-IF                                                               
347000                                                                          
347100     MOVE WSTAB-IDTRPTNR   (PLATSIX)  TO KOLLI-IDTRPTNR                   
347200     MOVE WSTAB-ADFLGEO    (PLATSIX)  TO KOLLI-ADFLGEO                    
347300     MOVE WSTAB-ADFLOMR    (PLATSIX)  TO KOLLI-ADFLOMR                    
347400     MOVE WSTAB-ADRUTNIV   (PLATSIX)  TO KOLLI-ADRUTNIV                   
347500     MOVE WSTAB-DIHMODUL   (PLATSIX)  TO KOLLI-DIHMODUL                   
347600     MOVE WSTAB-DIDMODUL   (PLATSIX)  TO KOLLI-DIDMODUL                   
347700     MOVE WSTAB-ADVMODUL   (PLATSIX)  TO KOLLI-ADVMODUL                   
347800     MOVE WSTAB-ADHMODUL   (PLATSIX)  TO KOLLI-ADHMODUL                   
347900     MOVE SPACE                       TO KOLLI-FILLERX2                   
348000     IF KOLLI-IDKOLLI-SAMP          >  ZERO                               
348100        MOVE NEJ                    TO KOLLI-FLUTLAST                     
348200     ELSE                                                                 
348300        MOVE WSTAB-FLUTLAST (PLATSIX) TO KOLLI-FLUTLAST                   
348400        IF KOLLI-FLAUTFAK = JA AND (DIST03-SVERIGE-2                      
348500           OR DIST18-SKROT)                                               
348600           MOVE NEJ                 TO KOLLI-FLUTLAST                     
348700        END-IF                                                            
348800     END-IF                                                               
348900                                                                          
349000     IF KOLLI-KDFARLIG-KOLLI = +4 OR KOLLI-KDFARLIG-KOLLI = +7            
349100       MOVE +950                    TO KOLLI-ADFLOMR                      
349200     END-IF                                                               
349300     .                                                                    
349400     EJECT                                                                
349500 S12-SKAPA-4322 SECTION.                                                  
349600                                                                          
349700*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
349800     IF (DIST03-SVERIGE-100-799                                           
349900     OR DIST03-NORGE                                                      
350000     OR DIST03-DANMARK-900                                                
350100     OR DIST85-PU-VIA-VR                                                  
350200     OR DIST21-TYRE)                                                      
350300        MOVE WS-IDPRODNR TO XXJK-4322-IDPRODNR                            
350400        MOVE KOLLI-IDKOLLI  TO XXJK-4322-IDKOLLI                          
350500        MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                          
350600        PERFORM IMS-ISRT-4322-SEGM                                        
350700     END-IF                                                               
350800     .                                                                    
350900     EJECT                                                                
351000 S13-UPPDAT-KDORDSTA SECTION.                                             
351100     MOVE WS-IDDISTR-NUM            TO   TEST-IDDISTR                     
351200     IF DIST19-SATS                                                       
351300       CONTINUE                                                           
351400     ELSE                                                                 
351500       PERFORM IMS-GU-ORQI01                                              
351600       MOVE REQU-IDDC-KEY     TO  W-IDDC                                  
351700       PERFORM IMS-GHNP-ORQI12                                            
351800       IF ARB-KDORDSTA = 'U '                                             
351900          MOVE 'U*' TO ARB-KDORDSTA                                       
352000          PERFORM IMS-REPL-ORQI12                                         
352100       END-IF                                                             
352200     END-IF                                                               
352300     .                                                                    
352400     SKIP2                                                                
352500 S14-EV-SEND-PRINTTRANS SECTION.                                          
352600                                                                          
352700     MOVE REQU-FLSKRIV-CLABEL  TO RESP-L128-CLABEL(1)                     
352800     IF REQU-FLSKRIV-CLABEL = 'Y'                                         
352900        MOVE 1       TO RESP-L128-KVRADER                                 
353000        MOVE 'Y'     TO RESP-L128-FLBG                                    
353100        PERFORM S15-SEND-PRINTTRANS                                       
353200     ELSE                                                                 
353300        IF REQU-FLSKRIV-CLABEL = 'N'                                      
353400           MOVE ZERO  TO RESP-L128-KVRADER                                
353500           MOVE 'N'   TO RESP-L128-FLBG                                   
353600        END-IF                                                            
353700     END-IF                                                               
353800     .                                                                    
353900     SKIP2                                                                
354000 S15-SEND-PRINTTRANS SECTION.                                             
354100                                                                          
354200     MOVE WS-IDDISTR-NUM           TO RESP-L128-IDDISTR-KEY(1)            
354300     MOVE WS-IDKUNDNR-NUM          TO RESP-L128-IDKUNDNR-KEY(1)           
354400     MOVE WS-IDORDNR               TO RESP-L128-IDORDNR-KEY(1)            
354500     IF KOLLI-INTERVALL                                                   
354600       MOVE WS-IDKOLLI-FOM         TO RESP-L128-IDKOLLI-KEY(1)            
354700       MOVE WS-IDKOLLI-TOM         TO RESP-L128-IDKOLLI-TOM(1)            
354800     ELSE                                                                 
354900       MOVE WS-IDKOLLI             TO RESP-L128-IDKOLLI-KEY(1)            
355000       MOVE ZERO                    TO RESP-L128-IDKOLLI-TOM(1)           
355100     END-IF                                                               
355200     MOVE REQU-IDDC-KEY            TO RESP-L128-IDDC-KEY(1)               
355300                                                                          
355400     IF DIRLEV-KOLLI                                                      
355500       MOVE WS-IDPRODNR            TO RESP-L128-IDPRODNR-KEY(1)           
355600     ELSE                                                                 
355700       MOVE ZERO                   TO RESP-L128-IDPRODNR-KEY(1)           
355800     END-IF                                                               
355900                                                                          
356000     .                                                                    
356100     EJECT                                                                
356200 S16-UPPD-FARLIGT-GODS-DATA SECTION.                                      
356300     SKIP3                                                                
356400     MOVE +1 TO FG-INDX                                                   
356500     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
356600                                                                          
356700       IF TAB-IDPSN(FG-INDX) > ZERO                                       
356800         MOVE TAB-IDPSN(FG-INDX)    TO KOLLI-IDPSN(FG-INDX)               
356900         COMPUTE KOLLI-VKART-FG(FG-INDX) =                                
357000                                       TAB-VKART-FG(FG-INDX) /            
357100                                       ARB-ANTAL-KOLLI                    
357200         COMPUTE KOLLI-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) /                
357300                                       ARB-ANTAL-KOLLI                    
357400                                                                          
357500       ELSE                                                               
357600         MOVE ZERO                  TO KOLLI-IDPSN(FG-INDX)               
357700                                       KOLLI-VKART-FG(FG-INDX)            
357800                                       KOLLI-VLFG(FG-INDX)                
357900       END-IF                                                             
358000                                                                          
358100       ADD +1 TO FG-INDX                                                  
358200     END-PERFORM                                                          
358300                                                                          
358400     IF TAB-IDPSN(1) > ZERO                                               
358500       COMPUTE KOLLI-SUEQFG = TOTAL-SUEQFG / ARB-ANTAL-KOLLI              
358600     ELSE                                                                 
358700       MOVE ZERO TO KOLLI-SUEQFG                                          
358800     END-IF                                                               
358900     .                                                                    
359000     EJECT                                                                
359100 S17-BERAEKNA-FG-FAELT SECTION.                                           
359200     SKIP3                                                                
359300     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) +                      
359400                                 (SPAR-VLFG        *                      
359500                                  KKOLLI-KVLEVART)                        
359600     IF SPAR-IDPSN = 10 OR 11                                             
359700       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
359800                                       (SPAR-VKART-FG        *            
359900                                        KKOLLI-KVLEVART)                  
360000     ELSE                                                                 
360100       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
360200     END-IF                                                               
360300                                                                          
360400     MOVE 10 TO FG-INDX                                                   
360500     .                                                                    
360600     EJECT                                                                
360700 S18-SKAPA-FOLJESEDEL-TRANS SECTION.                                      
360800                                                                          
360900         IF REQU-FLSKRIV-DELNOTE = 'Y'                                    
361000                                                                          
361100           MOVE 1                  TO RESP-L129-KVRADER                   
361200           MOVE 'Y'                TO RESP-L129-FLBG                      
361300           MOVE WS-IDDISTR-NUM     TO  RESP-L129-IDDISTR-KEY(INX)         
361400           MOVE WS-IDKUNDNR-NUM    TO  RESP-L129-IDKUNDNR-KEY(INX)        
361500           MOVE WS-IDORDNR         TO  RESP-L129-IDORDNR-KEY(INX)         
361600           IF  KOLLI-INTERVALL                                            
361700               MOVE WS-IDKOLLI-NUM TO RESP-L129-IDKOLLI-KEY(INX)          
361800               MOVE WS-IDKOLLI-TOM TO RESP-L129-IDKOLLI-TOM(INX)          
361900           ELSE                                                           
362000               MOVE WS-IDKOLLI     TO RESP-L129-IDKOLLI-KEY(INX)          
362100               MOVE ZERO           TO RESP-L129-IDKOLLI-TOM(INX)          
362200           END-IF                                                         
362300           MOVE REQU-FLSKRIV-DELNOTE TO                                   
362400                                   RESP-L129-FLSKRIV-DELNOTE(INX)         
362500           MOVE REQU-IDDC-KEY      TO RESP-L129-IDDC-KEY(INX)             
362600                                                                          
362700                                                                          
362800                                                                          
362900         ELSE                                                             
363000           IF REQU-FLSKRIV-DELNOTE = 'N'                                  
363100           MOVE 0                  TO RESP-L129-KVRADER                   
363200           MOVE 'N'                TO RESP-L129-FLBG                      
363300           END-IF                                                         
363400         END-IF                                                           
363500     SKIP2                                                                
363600     .                                                                    
363700 S19-CONVERT-LB-TO-KG                     SECTION.                        
363800                                                                          
363900     COMPUTE WS-MOD-VKORDBTO ROUNDED =                                    
364000             WS-MOD-VKORDBTO * CONV-LB-TO-KG                              
364100     END-COMPUTE                                                          
364200     .                                                                    
364300     SKIP2                                                                
364400                                                                          
364500 S20-DATA-TILL-DEL-NOTE SECTION.                                          
364600                                                                          
364700     MOVE WS-IDDISTR-NUM           TO TEST-IDDISTR                        
364800     IF DIST07-USA-RETAILER-DNOTE                                         
364900     OR DIST07-CAN-RETAILER                                               
365000        INITIALIZE DNOT-ORDER-INFO                                        
365100                                                                          
365200        MOVE IDPGM                    TO DNOT-IDPGM                       
365300        MOVE WS-SPAR-IDORDER          TO DNOT-IDORDER                     
365400        MOVE WS-SPAR-IDARTNR          TO DNOT-IDARTNR                     
365500        MOVE WS-SPAR-IDDC             TO DNOT-IDDC                        
365600        MOVE WS-SPAR-BEART            TO DNOT-BEART-USA                   
365700        MOVE WS-SPAR-KVBEART          TO DNOT-KVBEART                     
365800        MOVE WS-SPAR-FLTILLK          TO DNOT-FLTILLK                     
365900        MOVE WS-SPAR-IDKUNDRF-RO      TO DNOT-IDKUNDRF-RO                 
366000        MOVE WS-SPAR-IDPURAD          TO DNOT-IDPURAD                     
366100        MOVE KKOLLI-IDKOLLI           TO DNOT-IDKOLLI                     
366200        MOVE KKOLLI-IDPRODNR          TO DNOT-IDPRODNR                    
366300        MOVE KKOLLI-KVLEVART          TO DNOT-KVLEVART                    
366400                                                                          
366500        CALL W411DNOT USING DNOT-W411DNOT                                 
366600                            DNOT-ORQP-PCB                                 
366700                            DNOT-ORQP2-PCB                                
366800                            DNOT-ORQP3-PCB                                
366900                            DNOT-4013-PCB                                 
367000                            DNOT-BENA-PCB                                 
367100     END-IF                                                               
367200     .                                                                    
367300                                                                          
367400     EJECT                                                                
367500                                                                          
367600* IMS SEKTIONER                                                           
367700                                                                          
367800 IMS-GU-WDE401 SECTION.                                                   
367900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
368000            DELIMITED BY SIZE INTO SSA1                                   
368100     MOVE '    ' TO GODK-STATUSKODER                                      
368200     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E401 SSA1                 
368300     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
368400     PERFORM IMS-STATUSKONTROLL                                           
368500     SKIP3                                                                
368600     .                                                                    
368700 IMS-GU-WDE601    SECTION.                                                
368800                                                                          
368900     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
369000            DELIMITED BY SIZE INTO SSA1                                   
369100     MOVE '  GE' TO GODK-STATUSKODER                                      
369200     CALL CBLTDLI USING GU    WDE62-PCB DLI-IO-AREA2 SSA1                 
369300     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
369400     PERFORM IMS-STATUSKONTROLL                                           
369500     SKIP3                                                                
369600     .                                                                    
369700 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
369800     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
369900                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
370000            DELIMITED BY SIZE INTO SSA1                                   
370100     MOVE 'WDE401   ' TO SSA2                                             
370200     MOVE '  GE' TO GODK-STATUSKODER                                      
370300     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
370400     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
370500     PERFORM IMS-STATUSKONTROLL                                           
370600     .                                                                    
370700     SKIP3                                                                
370800 IMS-GU-WDE42-KORD-BSEQ SECTION.                                          
370900     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
371000            DELIMITED BY SIZE INTO SSA1                                   
371100     MOVE 'WDE401   ' TO SSA2                                             
371200     MOVE '  ' TO GODK-STATUSKODER                                        
371300     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
371400     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
371500     PERFORM IMS-STATUSKONTROLL                                           
371600     .                                                                    
371700     SKIP3                                                                
371800 IMS-GHU-RAD-SEK  SECTION.                                                
371900     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
372000            DELIMITED BY SIZE INTO SSA1                                   
372100     MOVE '    ' TO GODK-STATUSKODER                                      
372200     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-E411 SSA1                  
372300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
372400     PERFORM IMS-STATUSKONTROLL                                           
372500     SKIP3                                                                
372600     .                                                                    
372700 IMS-GHNP-RAD-E401 SECTION.                                               
372800*    STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
372900*           DELIMITED BY SIZE INTO SSA1                                   
373000     MOVE 'WDE401   ' TO SSA1                                             
373100     MOVE '    ' TO GODK-STATUSKODER                                      
373200     CALL CBLTDLI USING GHNP   WDE4-PCB DLI-IO-E401 SSA1                  
373300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
373400     PERFORM IMS-STATUSKONTROLL                                           
373500     SKIP3                                                                
373600     .                                                                    
373700 IMS-GHN-RAD-SEK  SECTION.                                                
373800     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
373900                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
374000            DELIMITED BY SIZE INTO SSA1                                   
374100     MOVE '  ' TO GODK-STATUSKODER                                        
374200     CALL CBLTDLI USING GHN    WDE4-PCB DLI-IO-E411 SSA1                  
374300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
374400     PERFORM IMS-STATUSKONTROLL                                           
374500     .                                                                    
374600     SKIP2                                                                
374700 IMS-REPL-BEHANDLAD-RAD SECTION.                                          
374800     MOVE '    ' TO GODK-STATUSKODER                                      
374900     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
375000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
375100     PERFORM IMS-STATUSKONTROLL                                           
375200     SKIP3                                                                
375300     .                                                                    
375400 IMS-REPL-BEHANDLAD-E401 SECTION.                                         
375500     MOVE '    ' TO GODK-STATUSKODER                                      
375600     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E401                         
375700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
375800     PERFORM IMS-STATUSKONTROLL                                           
375900     SKIP3                                                                
376000     .                                                                    
376100 IMS-GHNP-KOLLI-KOPPL    SECTION.                                         
376200     STRING 'WDE421  *F(WDE421KY =' W-WDE421-IDKOLLI-X ')'                
376300            DELIMITED BY SIZE INTO SSA1                                   
376400     MOVE '  ' TO GODK-STATUSKODER                                        
376500     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E421 SSA1                    
376600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
376700     PERFORM IMS-STATUSKONTROLL                                           
376800     SKIP3                                                                
376900     .                                                                    
377000 IMS-REPL-KOLLI-KOPPL  SECTION.                                           
377100     MOVE '  '   TO GODK-STATUSKODER                                      
377200     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E421                         
377300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
377400     PERFORM IMS-STATUSKONTROLL                                           
377500     SKIP2                                                                
377600     .                                                                    
377700 IMS-ISRT-KOLLI-KOPPL  SECTION.                                           
377800     MOVE   'WDE421   '       TO   SSA1                                   
377900     MOVE '  II' TO GODK-STATUSKODER                                      
378000     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-E421 SSA1                    
378100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
378200     PERFORM IMS-STATUSKONTROLL                                           
378300     .                                                                    
378400     SKIP2                                                                
378500 IMS-GHU-KOLLIREG SECTION.                                                
378600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
378700            DELIMITED BY SIZE INTO SSA1                                   
378800     MOVE '    ' TO GODK-STATUSKODER                                      
378900     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA2 SSA1                 
379000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
379100     PERFORM IMS-STATUSKONTROLL                                           
379200     .                                                                    
379300     SKIP2                                                                
379400 IMS-REPL-KOLLIREG SECTION.                                               
379500     MOVE '    ' TO GODK-STATUSKODER                                      
379600     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA2                        
379700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
379800     PERFORM IMS-STATUSKONTROLL                                           
379900     SKIP3                                                                
380000     .                                                                    
380100 IMS-GHU-KOLLI    SECTION.                                                
380200     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
380300            DELIMITED BY SIZE INTO SSA1                                   
380400     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
380500            DELIMITED BY SIZE INTO SSA2                                   
380600     MOVE '  GE' TO GODK-STATUSKODER                                      
380700     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA2 SSA1 SSA2            
380800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
380900     PERFORM IMS-STATUSKONTROLL                                           
381000     SKIP3                                                                
381100     .                                                                    
381200 IMS-GHNP-KOLLI    SECTION.                                               
381300     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
381400            DELIMITED BY SIZE INTO SSA1                                   
381500     MOVE '  GE' TO GODK-STATUSKODER                                      
381600     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-AREA2 SSA1                   
381700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
381800     PERFORM IMS-STATUSKONTROLL                                           
381900     SKIP3                                                                
382000     .                                                                    
382100 IMS-REPL-KOLLI    SECTION.                                               
382200     MOVE '    ' TO GODK-STATUSKODER                                      
382300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA2                        
382400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
382500     PERFORM IMS-STATUSKONTROLL                                           
382600     SKIP3                                                                
382700     .                                                                    
382800 IMS-ISRT-KOLLI   SECTION.                                                
382900     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
383000            DELIMITED BY SIZE INTO SSA1                                   
383100     MOVE   'WDE611   '       TO   SSA2                                   
383200     MOVE '  II' TO GODK-STATUSKODER                                      
383300     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-AREA2 SSA1 SSA2              
383400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
383500     PERFORM IMS-STATUSKONTROLL                                           
383600     .                                                                    
383700     SKIP2                                                                
383800 IMS-GET-EMBB     SECTION.                                                
383900     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-WDK5 ')'                      
384000            DELIMITED BY SIZE INTO SSA1                                   
384100     MOVE '  GE' TO GODK-STATUSKODER                                      
384200     CALL CBLTDLI USING GHU    EMBB-PCB DLI-IO-K501 SSA1                  
384300     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
384400     PERFORM IMS-STATUSKONTROLL                                           
384500     .                                                                    
384600     SKIP2                                                                
384700 IMS-GU-KUNDORDER-SEK SECTION.                                            
384800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
384900            DELIMITED BY SIZE INTO SSA1                                   
385000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
385100     CALL CBLTDLI USING GU   WDE4A-PCB DLI-IO-E401 SSA1                   
385200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
385300                               STATUS-KUNDORDER-SEK-WS                    
385400     PERFORM IMS-STATUSKONTROLL                                           
385500     SKIP2                                                                
385600     .                                                                    
385700 IMS-GN-SEQA-WDE4A1 SECTION.                                              
385800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
385900            DELIMITED BY SIZE INTO SSA1                                   
386000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
386100     CALL CBLTDLI USING GN   WDE4A-PCB DLI-IO-E401 SSA1                   
386200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
386300                               STATUS-KUNDORDER-SEK-WS                    
386400     PERFORM IMS-STATUSKONTROLL                                           
386500     SKIP2                                                                
386600     .                                                                    
386700 IMS-GU-RAD         SECTION.                                              
386800     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
386900            DELIMITED BY SIZE INTO SSA1                                   
387000     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
387100            DELIMITED BY SIZE INTO SSA2                                   
387200     MOVE '  GE' TO GODK-STATUSKODER                                      
387300     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E411 SSA1 SSA2            
387400     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
387500     PERFORM IMS-STATUSKONTROLL                                           
387600     .                                                                    
387700     SKIP2                                                                
387800 IMS-GU-4726-ROT-KVAL SECTION.                                            
387900     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
388000            DELIMITED BY SIZE INTO SSA1                                   
388100     MOVE '  ' TO GODK-STATUSKODER                                        
388200     CALL CBLTDLI USING GU     XXDV-PCB DLI-IO-AREA3 SSA1                 
388300     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
388400     PERFORM IMS-STATUSKONTROLL                                           
388500     SKIP2                                                                
388600     .                                                                    
388700 IMS-GNP-4726-UNDERSEG-KVAL SECTION.                                      
388800     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
388900            DELIMITED BY SIZE INTO SSA1                                   
389000     MOVE '  GE' TO GODK-STATUSKODER                                      
389100     CALL CBLTDLI USING GNP    XXDV-PCB DLI-IO-AREA3 SSA1                 
389200     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
389300     PERFORM IMS-STATUSKONTROLL                                           
389400     SKIP2                                                                
389500     .                                                                    
389600 IMS-INSERT-4726-UNDERSEG SECTION.                                        
389700     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
389800            DELIMITED BY SIZE INTO SSA1                                   
389900     MOVE 'WLXXDV11 ' TO SSA2                                             
390000     MOVE '  ' TO GODK-STATUSKODER                                        
390100     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3 SSA1 SSA2              
390200     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
390300     PERFORM IMS-STATUSKONTROLL                                           
390400     SKIP2                                                                
390500     .                                                                    
390600 IMS-INSERT-4727 SECTION.                                                 
390700     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
390800            DELIMITED BY SIZE INTO SSA1                                   
390900     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
391000            DELIMITED BY SIZE INTO SSA2                                   
391100     MOVE 'WLXXDV21 ' TO SSA3                                             
391200     MOVE '  II' TO GODK-STATUSKODER                                      
391300     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3                        
391400                               SSA1 SSA2 SSA3                             
391500     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
391600     PERFORM IMS-STATUSKONTROLL                                           
391700     .                                                                    
391800     SKIP2                                                                
391900 IMS-ISRT-4322-SEGM SECTION.                                              
392000     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
392100            DELIMITED BY SIZE INTO SSA1                                   
392200     MOVE 'WLXXJK11*L' TO SSA2                                            
392300     MOVE '  ' TO GODK-STATUSKODER                                        
392400     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA4 SSA1 SSA2              
392500     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
392600     PERFORM IMS-STATUSKONTROLL                                           
392700     .                                                                    
392800     SKIP2                                                                
392900 IMS-GU-ORQA01    SECTION.                                                
393000     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
393100            DELIMITED BY SIZE INTO SSA1                                   
393200     MOVE '  ' TO GODK-STATUSKODER                                        
393300     CALL CBLTDLI USING GU    ORQA-PCB DLI-IO-AREA5 SSA1                  
393400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
393500     PERFORM IMS-STATUSKONTROLL                                           
393600     .                                                                    
393700     SKIP2                                                                
393800 IMS-GU-ORQI01    SECTION.                                                
393900     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
394000            DELIMITED BY SIZE INTO SSA1                                   
394100     MOVE '  GE' TO GODK-STATUSKODER                                      
394200     CALL CBLTDLI USING GU   ORQI-PCB DLI-IO-AREA-Q201 SSA1               
394300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
394400     PERFORM IMS-STATUSKONTROLL                                           
394500     .                                                                    
394600 IMS-GHNP-ORQI12    SECTION.                                              
394700     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
394800            DELIMITED BY SIZE INTO SSA1                                   
394900     MOVE '    ' TO GODK-STATUSKODER                                      
395000     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-Q212 SSA1               
395100     MOVE ORQI-STATUS-CODE       TO STATUS-WS                             
395200     PERFORM IMS-STATUSKONTROLL                                           
395300     .                                                                    
395400     SKIP2                                                                
395500 IMS-REPL-ORQI12      SECTION.                                            
395600     MOVE '  ' TO GODK-STATUSKODER                                        
395700     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-Q212                    
395800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
395900     PERFORM IMS-STATUSKONTROLL                                           
396000     .                                                                    
396100     SKIP2                                                                
396200 IMS-GHU-XXKW11       SECTION.                                            
396300     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY-X ')'                    
396400            DELIMITED BY SIZE INTO SSA1                                   
396500     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY-X ')'                   
396600            DELIMITED BY SIZE INTO SSA2                                   
396700     MOVE '  GE' TO GODK-STATUSKODER                                      
396800     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA6 SSA1 SSA2            
396900     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
397000     PERFORM IMS-STATUSKONTROLL                                           
397100     .                                                                    
397200 IMS-REPL-XXKW11    SECTION.                                              
397300     MOVE '  '   TO GODK-STATUSKODER                                      
397400     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA6                        
397500     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
397600     PERFORM IMS-STATUSKONTROLL                                           
397700     .                                                                    
397800     SKIP2                                                                
397900 IMS-GU-XXLB         SECTION.                                             
398000     STRING 'WLXXLB01(WDGXKEY  =' W-4477-WDGXKEY-X ')'                    
398100            DELIMITED BY SIZE INTO SSA1                                   
398200     STRING 'WLXXLB11(WDGXKEY  =' W-4478-WDGXKEY-X ')'                    
398300            DELIMITED BY SIZE INTO SSA2                                   
398400     MOVE '  GE' TO GODK-STATUSKODER                                      
398500     CALL CBLTDLI USING GU    XXLB-PCB DLI-IO-AREA6 SSA1 SSA2             
398600     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
398700     PERFORM IMS-STATUSKONTROLL                                           
398800     .                                                                    
398900     SKIP2                                                                
399000 IMS-ISRT-ZZAC01 SECTION.                                                 
399100     MOVE 'WLZZAC01' TO SSA1                                              
399200     MOVE '  II'     TO GODK-STATUSKODER                                  
399300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
399400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
399500     PERFORM IMS-STATUSKONTROLL                                           
399600     .                                                                    
399700     SKIP2                                                                
399800                                                                          
399900 IMS-GET-ORQI01-CSEQ SECTION.                                             
400000                                                                          
400100     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
400200             DELIMITED BY SIZE INTO    SSA1                               
400300     MOVE    '  GE'              TO    GODK-STATUSKODER                   
400400     CALL    CBLTDLI             USING GU   ORQL-PCB                      
400500                                            DLI-IO-AREA-Q201              
400600                                            SSA1                          
400700     MOVE    ORQL-STATUS-CODE    TO    STATUS-WS                          
400800     PERFORM IMS-STATUSKONTROLL                                           
400900     .                                                                    
401000     SKIP2                                                                
401100 IMS-GU-WDB601    SECTION.                                                
401200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
401300          DELIMITED BY SIZE INTO SSA1                                     
401400     MOVE '  ' TO GODK-STATUSKODER                                        
401500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
401600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
401700     PERFORM IMS-STATUSKONTROLL                                           
401800     .                                                                    
401900     SKIP3                                                                
402000                                                                          
402100 IMS-GHN-WDA6B SECTION.                                                   
402200                                                                          
402300     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
402400                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
402500            DELIMITED BY SIZE INTO SSA1                                   
402600     MOVE '  GEGB'               TO GODK-STATUSKODER                      
402700     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-WDA601 SSA1              
402800     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
402900     PERFORM IMS-STATUSKONTROLL                                           
403000     .                                                                    
403100                                                                          
403200                                                                          
403300 IMS-REPL-WDA6B SECTION.                                                  
403400                                                                          
403500     MOVE 'WDA601  '           TO SSA1                                    
403600     MOVE '    '               TO GODK-STATUSKODER                        
403700     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-WDA601 SSA1               
403800     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
403900     PERFORM IMS-STATUSKONTROLL                                           
404000     .                                                                    
404100                                                                          
404200 IMS-GU-WDK601  SECTION.                                                  
404300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
404400          DELIMITED BY SIZE INTO SSA1                                     
404500     MOVE '  GE' TO GODK-STATUSKODER                                      
404600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
404700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
404800     PERFORM IMS-STATUSKONTROLL                                           
404900     .                                                                    
405000     SKIP3                                                                
405100 IMS-GNP-WDK611  SECTION.                                                 
405200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
405300          DELIMITED BY SIZE INTO SSA1                                     
405400     MOVE '  GE' TO GODK-STATUSKODER                                      
405500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
405600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
405700     PERFORM IMS-STATUSKONTROLL                                           
405800     .                                                                    
405900     SKIP3                                                                
406000                                                                          
406100 IMS-ROLLBACK    SECTION.                                                 
406200     SKIP2                                                                
406300     MOVE '  ' TO GODK-STATUSKODER                                        
406400     CALL CBLTDLI USING ROLB    MSG-PCB                                   
406500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
406600     PERFORM IMS-STATUSKONTROLL                                           
406700     SKIP2                                                                
406800     .                                                                    
406900 IMS-STATUSKONTROLL SECTION.                                              
407000     SET STATUS-IX TO 1                                                   
407100     SEARCH GODK-STATUS AT END CALL FELLOG                                
407200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
407300     END-SEARCH                                                           
407400     CONTINUE                                                             
407500     .                                                                    
